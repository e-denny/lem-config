;;; organ-mode extras: emphasis/code/comment/math faces, and a Return that
;;; follows the link under point.
;;;
;;; organ-mode's own highlighting covers structural elements only -- headings,
;;; timestamps, links, tables, lists, blocks, drawers, keywords, latex
;;; environments -- and its Return special-cases tables and lists, leaving
;;; [[file:...]] links to C-c C-o.  Both gaps are handled here rather than in
;;; the extensions/organ-mode checkout, so that checkout stays close to
;;; upstream and this file is versioned with the rest of the configuration.
;;;
;;; Everything below is keyed to organ-mode's internals (its package uses
;;; single colons for the public names it defines, :: for the rest), and it is
;;; loaded from init.lisp after (asdf:load-system "organ-mode"), so the
;;; packages and the TEXT-OBJECT-OVERLAYS generic exist when this file is
;;; read.
;;;
;;; The webview frontend draws foreground, background, bold, reverse and
;;; underline but not italic or strikethrough, so /italic/ is given a colour
;;; instead of a slant: on this frontend an italic-only face would be
;;; invisible.
;;;
;;; The markup itself is hidden rather than merely coloured: emphasis markers
;;; and the [[target][...]] wrapper around a link are covered by :invisible
;;; overlays, which Lem splices out of the drawn line.  They come back while
;;; the cursor is inside them, so they stay editable.

(in-package :lem-user)

;;;; link at point

(defun organ-link-at-point ()
  "The link object under the cursor, or NIL."
  (let ((tree (organ/organ-mode:current-tree)))
    (when tree
      (let ((object (cltpt/base:child-at-pos tree (organ/utils:current-pos))))
        (and (typep object 'cltpt/base::text-link) object)))))

;;;; faces
;;;
;;; The names are the base16 keys of the colour theme this editor runs
;;; (lem-default defines :base00 through :base0f), so they follow a theme
;;; change instead of pinning hex values.  organ-mode's own faces use the same
;;; keys.

(lem:define-attribute organ-emph-attribute
  (t :bold t))

(lem:define-attribute organ-italic-attribute
  (t :foreground :base0c))

(lem:define-attribute organ-inline-code-attribute
  (t :background :base01 :foreground :base0b))

(lem:define-attribute organ-comment-attribute
  (t :foreground :base03))

;; organ-mode colours links :base0d and stops there.  Underlining is the one
;; other thing this frontend can draw that a link wants (it has no italic and
;; no images), and with the target and brackets concealed the description has
;; nothing else left to mark it as a link.
(lem:define-attribute organ-link-underlined-attribute
  (t :foreground :base0d :underline t))

(lem:define-attribute organ-math-attribute
  (t :foreground :base0a))

(defun organ-object-face-overlay (object buffer attribute)
  "An overlay covering OBJECT's span in BUFFER, drawn with ATTRIBUTE."
  (lem:make-overlay
   (organ/utils:char-offset-to-point buffer
                                     (organ/organ-mode::begin-in-root object))
   (organ/utils:char-offset-to-point buffer
                                     (organ/organ-mode::end-in-root object))
   attribute))

(defvar *organ-conceal-markup* t
  "When true, emphasis markers and link wrappers are hidden from the drawn line.
Set it to NIL to read and edit the raw markup again; the change takes effect
on the next redraw of the buffer (organ-mode rebuilds its overlays after every
change), so tag a character and undo it if a file looks wrong.")

(defun organ-conceal-overlay (buffer begin end attribute)
  "An overlay hiding the characters in [BEGIN, END) of BUFFER, or NIL when
*ORGAN-CONCEAL-MARKUP* is NIL.
Lem treats :invisible overlays as a splice, so the characters leave the drawn
line instead of being painted over in the background colour.  The enter/leave
hooks are the ones Lem installs for folds: driving the cursor into the hidden
characters brings them back, which keeps the markup editable.  ATTRIBUTE is
only seen once the text is revealed, so it is the face of the thing it hides
(an overlay must carry one: LEM:MAKE-OVERLAY rejects a NIL attribute).  NIL for
an empty range, so callers can pass the result straight into a list."
  (when (and *organ-conceal-markup* (< begin end))
    (let ((overlay (lem:make-overlay
                    (organ/utils:char-offset-to-point buffer begin)
                    (organ/utils:char-offset-to-point buffer end)
                    attribute)))
      (lem:overlay-put overlay :invisible t)
      (lem:overlay-put overlay :cursor-enter-functions
                       '(lem-core::reveal-overlay-on-cursor-enter))
      (lem:overlay-put overlay :cursor-leave-functions
                       '(lem-core::hide-overlay-on-cursor-leave))
      overlay)))

(defun organ-emphasis-overlays (object buffer attribute)
  "The emphasis face over OBJECT, with its one-character markers hidden.
`*bold*' draws as bold text and `/italic/' as the italic face, while the
markers stay in the buffer one cursor move away."
  (let ((begin (organ/organ-mode::begin-in-root object))
        (end (organ/organ-mode::end-in-root object)))
    (remove nil
            (list (organ-object-face-overlay object buffer attribute)
                  (organ-conceal-overlay buffer begin (1+ begin) attribute)
                  (organ-conceal-overlay buffer (1- end) end attribute)))))

(defun organ-link-overlays (object buffer)
  "The link face over OBJECT, with the markup around a description hidden.
`[[file:notes.org][the notes]]' draws as underlined `the notes'.  A link
without a description keeps its target visible and loses only the brackets:
what the link points at is the only thing such a link has to show."
  (let* ((begin (organ/organ-mode::begin-in-root object))
         (end (organ/organ-mode::end-in-root object))
         (text (cltpt/base:text-object-text object))
         (separator (search "][" text)))
    (remove nil
            (if separator
                (list (organ-object-face-overlay
                       object buffer 'organ-link-underlined-attribute)
                      ;; everything up to the description, and the closing ]]
                      (organ-conceal-overlay buffer begin (+ begin separator 2)
                                             'organ-link-underlined-attribute)
                      (organ-conceal-overlay buffer (- end 2) end
                                             'organ-link-underlined-attribute))
                (list (organ-object-face-overlay
                       object buffer 'organ-link-underlined-attribute)
                      (organ-conceal-overlay buffer begin (+ begin 2)
                                             'organ-link-underlined-attribute)
                      (organ-conceal-overlay buffer (- end 2) end
                                             'organ-link-underlined-attribute))))))

(macrolet ((define-object-face (type attribute)
             `(defmethod organ/organ-mode::text-object-overlays ((object ,type) buffer)
                (list (organ-object-face-overlay object buffer ',attribute))))
           (define-emphasis-face (type attribute)
             `(defmethod organ/organ-mode::text-object-overlays ((object ,type) buffer)
                (organ-emphasis-overlays object buffer ',attribute))))
  ;; inline emphasis: *bold*, /italic/, ~code~, _underline_, +strike-through+
  (define-emphasis-face cltpt/org-mode::org-emph organ-emph-attribute)
  (define-emphasis-face cltpt/org-mode::org-italic organ-italic-attribute)
  (define-emphasis-face cltpt/org-mode::org-inline-code organ-inline-code-attribute)
  (define-emphasis-face cltpt/org-mode::org-underline organ/organ-mode::organ-underline-attribute)
  (define-emphasis-face cltpt/org-mode::org-strike-through organ/organ-mode::organ-strike-through-attribute)
  ;; a line starting "# ": the leading marker is part of the comment syntax
  (define-object-face cltpt/org-mode::org-comment organ-comment-attribute)
  ;; $inline$ and $$display$$ math
  (define-object-face cltpt/latex:inline-math organ-math-attribute)
  (define-object-face cltpt/latex:display-math organ-math-attribute))

;; links are handled apart from the macro above because the hidden range
;; depends on where the description starts, not on a fixed marker width.
(defmethod organ/organ-mode::text-object-overlays ((object cltpt/org-mode:org-link) buffer)
  (organ-link-overlays object buffer))

;;;; the key hints popup
;;;
;;; organ-mode answers MODE-TRANSIENT-KEYMAP for its major mode, and the
;;; transient extension consults that on every completed key sequence: after
;;; any command in an org buffer it resolves the organ keymap and puts its key
;;; hints on screen.  Since every lookup resolves to the same keymap, the panel
;;; is shown again straight after it is drawn, which reads as permanent.  The
;;; hints are useful when wanted, so they start off and a key toggles them.

(defvar *organ-transient-hints* nil
  "When true, organ-mode's key hints popup follows the mode keymap.
Off by default, because it comes back after every command; C-c t in an org
buffer toggles it (ORGAN-TOGGLE-TRANSIENT-HINTS).")

(defun organ-transient-keymaps (keymap)
  "KEYMAP together with every keymap its prefixes lead to."
  (cons keymap
        (loop :for prefix :in (lem:keymap-prefixes keymap)
              :for suffix := (lem:prefix-suffix prefix)
              :when (typep suffix 'lem:keymap)
                :append (organ-transient-keymaps suffix))))

(defun organ-mark-transient-keymaps (show-p)
  "Mark the organ-mode keymaps as popup candidates or not.
The transient extension shows a keymap only when its SHOW-P flag is set, and
marking the whole tree -- the C-c sub-keymaps too -- is what keeps the popup
gone when the hints are off: an intermediate prefix would otherwise bring a
smaller panel back the moment C-c is pressed."
  (dolist (keymap (organ-transient-keymaps organ/organ-mode:*organ-mode-keymap*))
    (setf (lem/transient::keymap-show-p keymap) show-p)))

(defmethod lem/transient:mode-transient-keymap ((mode organ/organ-mode:organ-mode))
  "The organ-mode keymap, but only while the hints are switched on."
  (when *organ-transient-hints*
    organ/organ-mode:*organ-mode-keymap*))

(lem:define-command organ-toggle-transient-hints () ()
  "Show or hide organ-mode's key hints popup."
  (setf *organ-transient-hints* (not *organ-transient-hints*))
  (organ-mark-transient-keymaps *organ-transient-hints*)
  (if *organ-transient-hints*
      (lem:keymap-activate organ/organ-mode:*organ-mode-keymap*)
      ;; with the hints off the root keymap resolves to nothing, and a keymap
      ;; that resolves to nothing is what hides the popup.
      (lem:keymap-activate lem:*root-keymap*))
  (lem:message "organ-mode key hints ~:[off~;on~]" *organ-transient-hints*))

;; start out off, and reachable from the org keymap itself so that the popup is
;; not needed to find the key that brings it back.
(organ-mark-transient-keymaps *organ-transient-hints*)

(lem:define-key organ/organ-mode:*organ-mode-keymap* "C-c t" 'organ-toggle-transient-hints)

;;;; Return follows the link under point

(defun organ-link-path (source-buffer path)
  "PATH as a pathname string, resolved against SOURCE-BUFFER's directory.
Organ links are relative to the file that carries them, which is also what
`probe-file' in ORGAN-OPEN-AT-POINT must look at: it resolves against the
editor's working directory otherwise, and a relative link then never opens."
  (let* ((namestring (princ-to-string path))
         (pathname (uiop:ensure-pathname namestring))
         (directory (let ((file (lem:buffer-filename source-buffer)))
                      (and file (uiop:pathname-directory-pathname file)))))
    (cond
      ;; a URL, not a filesystem path
      ((search "://" namestring) namestring)
      ((uiop:absolute-pathname-p pathname) namestring)
      ((null directory) namestring)
      (t (namestring (merge-pathnames pathname directory))))))

;; The command is redefined rather than wrapped: the mode keymap binds
;; ORGAN-OPEN-AT-POINT and the dwim Return calls it, so one definition serves
;; both paths.  DEFUN and not DEFINE-COMMAND: the latter refuses to redefine a
;; command that another file defines (an interactive CERROR), and the command
;; object is already registered -- its EXECUTE method calls this function by
;; name -- so only the body needs replacing.
(defun organ/organ-mode::organ-open-at-point ()
  "Open the link at point, resolving relative links against this file."
  (let ((object (organ-link-at-point)))
    (unless object
      (lem:editor-error "not on a link."))
    (let* ((source-buffer (lem:current-buffer))
           (resolved (let ((cltpt/roam:*roam-parse-data*
                             (list :roamer (organ/roam:current-roamer))))
                       (cltpt/base:text-link-resolve object)))
           (dest (cltpt/base:link-dest (cltpt/base:text-link-link object)))
           (path (organ-link-path
                  source-buffer
                  (typecase resolved
                    (pathname (cltpt/file-utils:ensure-filepath-string resolved))
                    (cltpt/roam:node (cltpt/roam:node-file resolved))
                    (t dest)))))
      (cond
        ;; a URL (http:, https:, ...): hand it to the desktop, not to find-file
        ((search "://" path) (lem:open-external-file path))
        ((probe-file path) (lem:find-file path))
        (t (lem:editor-error "file ~A doesnt exist" path))))))

(defmethod lem:prefix-active-p ((prefix (eql organ/organ-mode::*return-prefix*)))
  "RET acts on the element at point: a link, a table row, or a list item."
  (or (organ/organ-mode::current-text-obj-ignore-newline 'cltpt/org-mode:org-table)
      (organ-link-at-point)
      (let ((list-object (organ/organ-mode::current-text-obj-ignore-newline
                          'cltpt/org-mode:org-list)))
        (and list-object (lem:end-line-p (lem:current-point))))))

(defun organ/organ-mode::organ-dwim-return ()
  "Context-sensitive Return: follow a link, or advance in a table or list."
  (let ((table-found (organ/organ-mode::current-text-obj-ignore-newline
                      'cltpt/org-mode:org-table))
        (list-found (organ/organ-mode::current-text-obj-ignore-newline
                     'cltpt/org-mode:org-list)))
    (cond
      ((organ-link-at-point) (organ/organ-mode::organ-open-at-point))
      (table-found (organ/organ-mode::org-table-navigate table-found 0 1))
      (list-found (organ/organ-mode::org-list-newline)))))
