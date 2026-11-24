(uiop:define-package :org-latex-preview/organ
  (:use :cl)
  (:import-from :org-latex-preview/webview
                :cell-height-px
                :ensure-image-rendering
                :font-size-px
                :image-rendering-ready-p
                :install-drawing-support
                :refresh-display)
  (:documentation "Latex previews inside organ-mode buffers.

organ-mode already carries the whole preview feature -- it finds the fragments
in the parse tree, compiles them through cltpt/latex-previews, keeps the images
in overlays and resyncs them as the buffer changes -- but it stays switched off
in this Lem because three frontend interface names it asks about were stubbed
out to NIL (they do not exist here) and because the webview frontend cannot
draw images.  org-latex-preview/webview closes the second gap; this package
answers the first and turns the feature on.

The faces, concealment and link work in organ-extras.lisp are independent of
this: previews replace a fragment's source with its typeset image while the
cursor is outside it, and give the source back when the cursor is inside, which
is org-mode's behaviour.")
  (:export
   :*auto-preview*
   :*preview-foreground*
   :*preview-cache-directory*
   :setup
   :theme-foreground-color))

(in-package :org-latex-preview/organ)

(defvar *auto-preview* t
  "When true, every organ-mode buffer previews its latex as it changes.")

(defvar *preview-foreground* :theme
  "Colour previews are typeset in.  :theme follows the colour theme's
foreground (and keeps following it), a \"#rrggbb\" string fixes a colour, and
:none leaves latex's own black.")

(defvar *preview-cache-directory* ".cache/lem/latex-previews/"
  "Where compiled previews are kept, relative to the home directory.
cltpt's own default is a subdirectory of the temporary directory, which the
system empties: previews would be recompiled after every reboot.")

(defvar *refresh-timer* nil)

(defun theme-foreground-color ()
  "The colour of ordinary text, as \"#rrggbb\"."
  (or (let ((colour (ignore-errors (lem:foreground-color))))
        (and (stringp colour) colour))
      ;; base16 themes name their default foreground :base05 and leave the
      ;; theme's own :foreground unset, which is also why the webview client
      ;; falls back to its built-in colour instead of one the theme named.
      (ignore-errors (lem-core::base-color :base05))
      "#cccccc"))

;;; The interface names organ-mode's preview module asks about.  init.lisp
;;; creates them as stubs that answer NIL when this Lem does not have them, so
;;; they are looked up at run time rather than read as package-qualified
;;; symbols; a build that has the real thing is answered the same way.

(defun define-interface-function (package-designator symbol-name function)
  (let ((package (find-package package-designator)))
    (when package
      (let ((symbol (find-symbol symbol-name package)))
        (when symbol
          #+sbcl (sb-ext:unlock-package package)
          (unwind-protect
               (progn (setf (symbol-function symbol) function) t)
            #+sbcl (sb-ext:lock-package package)))))))

(defun install-interface-answers ()
  (define-interface-function
   "LEM" "IMAGE-SUPPORT-P"
   (lambda (implementation)
     (declare (ignore implementation))
     (image-rendering-ready-p)))
  (define-interface-function
   "LEM-INTERFACE" "FONT-EM-PIXELS"
   (lambda (implementation)
     (declare (ignore implementation))
     (font-size-px)))
  (define-interface-function
   "LEM-INTERFACE" "CELL-HEIGHT"
   (lambda (implementation)
     (declare (ignore implementation))
     (cell-height-px))))

;;; Displaying the image.

(defun install-preview-rendering ()
  "Put the image in one virtual run, which is what this Lem can draw.

organ-mode writes a list of runs to :after-string -- a row break, the image, and
another row break for a focused display fragment -- but this Lem's display reads
one run per overlay, and a list of runs arrives as a single string that is not a
string.  One run is enough for the image itself: it replaces the fragment while
the cursor is elsewhere, and follows the source on the same line while the
cursor is inside it."
  (setf (symbol-function 'organ/organ-mode::render-preview)
        (lambda (preview)
          (let ((overlay (organ/organ-mode::preview-overlay preview))
                (image (organ/organ-mode::preview-image preview)))
            (when overlay
              (lem:overlay-put overlay :invisible
                               (and image
                                    (not (organ/organ-mode::preview-focused-p preview))))
              (lem:overlay-put overlay :after-string
                               (when image
                                 (list organ/organ-mode::*organ-latex-preview-placeholder*
                                       (organ/organ-mode::preview-image-attribute preview))))))))
  t)

;;; organ-mode's own default for the preview colour is the theme foreground,
;;; read through LEM:FOREGROUND-COLOR, which answers NIL here: a theme without
;;; a :foreground entry.  Replacing the reader keeps previews visible, and
;;; keeps them following the theme.

(defun organ/organ-mode::preview-foreground-color ()
  (cond ((stringp organ/organ-mode::*organ-latex-preview-foreground*)
         organ/organ-mode::*organ-latex-preview-foreground*)
        ((eq organ/organ-mode::*organ-latex-preview-foreground* :none) nil)
        (t (theme-foreground-color))))

;;; Alignment.  A fragment that is not inline -- display math, whatever
;;; delimiters it was written with -- stands on a line of its own and belongs
;;; in the middle of that line, where latex would centre it on the page.  An
;;; inline fragment stays where its sentence put it, and a focused fragment
;;; shows its image after its source on the source's line, which is no place
;;; to move it.  The decision is recorded on the image attribute, which the
;;; frontend reads: see INSTALL-LINE-RENDERING in webview-images.lisp.

(defvar *original-preview-image-attribute* nil)

(defun install-preview-alignment ()
  (unless *original-preview-image-attribute*
    (setf *original-preview-image-attribute*
          (symbol-function 'organ/organ-mode::preview-image-attribute)))
  (let ((original *original-preview-image-attribute*))
    (setf (symbol-function 'organ/organ-mode::preview-image-attribute)
          (lambda (preview)
            (let ((attribute (funcall original preview)))
              (when (and attribute
                         (not (organ/organ-mode::preview-inline-p preview))
                         (not (organ/organ-mode::preview-focused-p preview)))
                (setf (lem-core::attribute-value attribute :center) t))
              attribute))))
  ;; The attributes of the previews already on screen were built by the
  ;; previous definition, so they are rebuilt here: previews are otherwise only
  ;; re-rendered when their fragment is recompiled.
  (dolist (buffer (lem:buffer-list))
    (when (organ-buffer-p buffer)
      (lem:with-current-buffer buffer
        (dolist (preview (organ/organ-mode::buffer-previews buffer))
          (organ/organ-mode::render-preview preview))))))

(defun install-preview-defaults ()
  (setf organ/organ-mode::*organ-latex-preview-auto* *auto-preview*
        organ/organ-mode::*organ-latex-preview-foreground*
        (if (eq *preview-foreground* :theme) nil *preview-foreground*)))

(defun install-compiler-directories ()
  (let* ((cache (merge-pathnames *preview-cache-directory*
                                 (user-homedir-pathname)))
         (temporary (merge-pathnames "tmp/" cache)))
    (setf cltpt/latex-previews:*latex-previews-cache-directory*
          (uiop:native-namestring cache)
          cltpt/latex-previews:*latex-previews-tmp-directory*
          (uiop:native-namestring temporary))))

;;; An image reaches the client a frame after the compile finishes, so a redraw
;;; is wanted after the images land, not only when the overlay changes.  The
;;; client asks for one itself when an image decodes (see webview-images.lisp);
;;; this is the same request from the compiling side, once per batch.

(defun request-refresh ()
  (unless *refresh-timer*
    (setf *refresh-timer*
          (lem:start-timer
           (lem:make-timer
            (lambda ()
              (setf *refresh-timer* nil)
              (lem:send-event #'refresh-display))
            :name "org-latex-preview-refresh")
           0.05
           :repeat nil))))

(defvar *original-set-preview-compiled* nil)

(defun install-refresh-after-compile ()
  (unless *original-set-preview-compiled*
    (setf *original-set-preview-compiled*
          (symbol-function 'organ/organ-mode::set-preview-compiled))
    (let ((original *original-set-preview-compiled*))
      (setf (symbol-function 'organ/organ-mode::set-preview-compiled)
            (lambda (preview result)
              (prog1 (funcall original preview result)
                (request-refresh)))))))

;;; Commands.

(lem:define-command organ-latex-preview-toggle () ()
  "Show or hide the latex previews in the current buffer."
  (let ((buffer (lem:current-buffer))
        (mode 'organ/organ-mode:organ-latex-preview-mode))
    (if (lem:mode-active-p buffer mode)
        (progn (lem:disable-minor-mode mode)
               (lem:message "latex previews off"))
        (if (image-rendering-ready-p)
            (progn (lem:enable-minor-mode mode)
                   (lem:message "latex previews on"))
            (lem:message "latex previews need the webview patch; try again in a moment")))))

(defun install-keybinding ()
  ;; C-c C-x l is organ-mode's own preview dispatch (m toggles, r regenerates);
  ;; C-c C-x C-l is the same toggle under the key org itself uses for previews.
  (handler-case
      (lem:define-key organ/organ-mode:*organ-mode-keymap*
                      "C-c C-x C-l"
                      'organ-latex-preview-toggle)
    (error () nil)))

;;; Startup.  The webview client connects after init.lisp has run, so the first
;;; patch attempt is usually lost; it is repeated until the client answers, and
;;; again on every buffer switch until it does.  Previews placed before the
;;; patch exists are not lost: enabling the mode compiles and installs them.

(defun maybe-enable-preview (buffer)
  (when (and (image-rendering-ready-p)
             (organ-buffer-p buffer)
             (not (lem:mode-active-p buffer 'organ/organ-mode:organ-latex-preview-mode)))
    (lem:with-current-buffer buffer
      (lem:enable-minor-mode 'organ/organ-mode:organ-latex-preview-mode))))

(defun organ-buffer-p (buffer)
  (eq (ignore-errors (lem:buffer-major-mode buffer))
      'organ/organ-mode:organ-mode))

(defun on-buffer-switch (buffer)
  (unless (image-rendering-ready-p)
    (ensure-image-rendering))
  (maybe-enable-preview buffer))

(defun setup ()
  "Install everything: server-side image drawing, interface answers, defaults."
  (install-drawing-support)
  (install-preview-rendering)
  (install-preview-alignment)
  (install-interface-answers)
  (install-preview-defaults)
  (install-compiler-directories)
  (install-refresh-after-compile)
  (install-keybinding)
  ;; add-hook keeps a hook from being added twice
  (lem:add-hook lem:*switch-to-buffer-hook* 'on-buffer-switch)
  (ensure-image-rendering))

(setup)
