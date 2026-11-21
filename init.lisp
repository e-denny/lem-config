(in-package :lem-user)

;;; Turn off the blinking cursor in the webview / browser frontend.
;;;
;;; The frontend stylesheet (frontends/server/frontend/editor.css) animates
;;; the `.lem-cursor` overlay with `animation: lem-cursor-blink 1s step-end
;;; infinite`. Appending one more rule with the same specificity through
;;; LOAD-CSS (see frontends/server/how-to-customize.md) makes it win by
;;; source order, so the cursor stays solidly visible.
;;;
;;; LOAD-CSS only exists when a server-based frontend (webview/server) is
;;; loaded, hence the soft lookup so this file still loads under ncurses/SDL2.
(let ((load-css (uiop:find-symbol* :load-css :lem-server nil)))
  (when load-css
    (funcall load-css ".lem-cursor { animation: none; }")))

;;; Flat frame around floating windows, no drop shadow.
;;;
;;; Popup windows created with a border -- the M-x prompt and its completion
;;; list, along with the other popup windows -- get the wrapper class
;;; `lem-editor__floating-window--bordered`.  Its rule in editor.css carries
;;; `box-shadow: 4px 4px 16px black, 0 0 0 1px rgba(128, 128, 128, 0.2)`: the
;;; first shadow is the soft black halo, the second the faint grey hairline.
;;; One spread-only shadow replaces both, so the halo is gone and a single 1px
;;; white line is all that is left.  It is a box-shadow rather than a `border`
;;; on purpose: the wrapper is sized in JavaScript from the character grid, and
;;; a real border would eat 1px of the padding box on every side and nudge the
;;; popup's text out of alignment with the buffer behind it.  Same specificity
;;; as the editor.css rule, but loaded later, so source order lets it win.
(let ((load-css (uiop:find-symbol* :load-css :lem-server nil)))
  (when load-css
    (funcall load-css
             ".lem-editor__floating-window--bordered { box-shadow: 0 0 0 1px #ffffff; }")))

;;; Font: the one Emacs uses.
;;;
;;; Emacs' default face is Adwaita Mono (frame font "...Adwaita Mono...-15-"),
;;; i.e. 15px, which lays out as a 9x20 cell (C-x = says 9x20 too).  This
;;; frontend takes the row height from the font's vertical metrics as Chromium
;;; reports them, which is tighter than what Xft reports to Emacs: at 15px the
;;; cell is 9x18 and the grid no longer matches Emacs' line rhythm.  16px is
;;; the size whose metrics round to exactly 9x20 (measured with
;;; ctx.measureText("W").fontBoundingBox* in the running webview), so the two
;;; editors break lines and scroll in the same shape.  The advance is 9.6px at
;;; that size and the frontend floors it to 9, which condenses glyphs by ~4%.
;;;
;;; SET-FONT exists for every frontend, but only the server/webview one takes a
;;; CSS family name, so this is guarded the same way as the cursor rule.
(let ((set-font (uiop:find-symbol* :set-font :lem nil))
      (server-frontend-p (uiop:find-symbol* :load-css :lem-server nil)))
  (when (and set-font server-frontend-p)
    (funcall set-font :name "Adwaita Mono" :size 16)))

;;; Vertico-style completion.
;;;
;;; Lem's prompt is a floating window: where it sits, how wide it is, whether
;;; the candidate list opens by itself, and which end of that list the first
;;; match lands on are all controlled by these variables. Defaults in parens.

;; Prompt at the bottom of the frame (default :top-display).
(setf lem-core::*default-prompt-gravity* :bottom-display)

;; Prompt as wide as the display instead of 2/3 (default nil).
(setf lem/prompt-window::*fill-width* t)

;; Candidate list directly above the prompt, not to its right
;; (default :horizontally-adjacent-window).
(setf lem/prompt-window::*prompt-completion-window-gravity*
      :horizontally-above-window)

;; Open the candidate list as soon as the prompt appears, so TAB is not needed
;; to see it (default nil). This is the switch that *prompt-after-activate-hook*
;; tricks are usually used for; prefer it.
(setf lem:*automatic-tab-completion* t)

;; Reverse the list so the best match sits next to the prompt line and
;; navigation starts from there (default nil).
(setf lem/completion-mode::*completion-reverse* t)

;;; Completion popup size.
;;;
;;; lem-if:display-popup-menu defaults to 20 visible items
;;; (src/ext/popup-menu.lisp:114) and completion-mode never passes
;;; :max-display-items, so the popup can be raised well past the space under the
;;; prompt. These :around methods only act when the caller did not choose a
;;; limit itself -- i.e. for completion popups -- and they also force the window
;;; height to equal the number of candidate lines, so the focused entry (the
;;; last one, because *completion-reverse* is on) always sits on the popup's
;;; bottom row instead of leaving blank rows underneath.

(defvar *completion-popup-max-rows* 10
  "Upper bound on the number of candidate rows shown by the completion popup.")

(defun completion-popup-height (popup-menu)
  (let ((lines (lem:buffer-nlines (lem/popup-menu::popup-menu-buffer popup-menu))))
    (max 1 (min *completion-popup-max-rows* lines))))

(defun fit-completion-popup (popup-menu)
  (when popup-menu
    (let* ((window (lem/popup-menu::popup-menu-window popup-menu))
           (height (completion-popup-height popup-menu)))
      (unless (= height (lem:window-height window))
        (lem/popup-window::update-popup-window
         :source-window (lem/popup-window::popup-window-source-window window)
         :width (lem:window-width window)
         :height height
         :destination-window window)))
    (align-popup-selection popup-menu)))

(defmethod lem-if:display-popup-menu :around ((implementation lem:implementation)
                                              items &rest args)
  (let ((popup-menu (apply #'call-next-method implementation items args)))
    (when (null (getf args :max-display-items))
      (fit-completion-popup popup-menu))
    popup-menu))

(defmethod lem-if:popup-menu-update :around ((implementation lem:implementation)
                                             popup-menu items &rest args)
  (prog1 (apply #'call-next-method implementation popup-menu items args)
    (when (null (getf args :max-display-items))
      (fit-completion-popup popup-menu))))

;;; Keep the selected candidate on the popup's bottom row.
;;;
;;; Popup menus move the focus with WINDOW-SEE, which honours the global
;;; *scroll-recenter-p* (t) and therefore calls WINDOW-RECENTER: the focused
;;; line is put in the *middle* of the popup. When the focus is the last
;;; candidate there is nothing left to draw below it, so the rows under it are
;;; cleared and you get blank rows at the bottom -- exactly the "6 candidates
;;; and 4 blank rows in a 10-row popup" case. ALIGN-POPUP-SELECTION undoes that
;;; by scrolling the view until the focused row is the window's last row;
;;; MOVE-FOCUS is advised (with *scroll-recenter-p* bound to NIL) so it runs on
;;; every focus move. MOVE-FOCUS lives in the locked LEM/POPUP-MENU package,
;;; hence the unlock (SBCL-only; other implementations ignore that block).

(defun align-popup-selection (popup-menu)
  "Scroll POPUP-MENU's window so the focused row is the bottom row."
  (when popup-menu
    (let* ((window (lem/popup-menu::popup-menu-window popup-menu))
           (buffer (lem/popup-menu::popup-menu-buffer popup-menu))
           (focus (lem/popup-menu::focus-point popup-menu))
           (row (lem:window-cursor-y window))
           (last-row (1- (lem:window-height window))))
      (when (and focus
                 (< row last-row)
                 (= (lem:line-number-at-point focus)
                    (lem:buffer-nlines buffer)))
        (lem:window-scroll window (- row last-row))))))

(defvar *original-move-focus* nil
  "The unadvised LEM/POPUP-MENU::MOVE-FOCUS, captured the first time this file
is loaded so that reloading init.lisp replaces the advice instead of stacking
another copy on top of it.")

#+sbcl
(sb-ext:without-package-locks
  (progn
    (setf *original-move-focus*
          (or *original-move-focus*
              (symbol-function 'lem/popup-menu::move-focus)))
    (setf (symbol-function 'lem/popup-menu::move-focus)
          (lambda (popup-menu function)
            (let ((lem-core::*scroll-recenter-p* nil))
              (funcall *original-move-focus* popup-menu function))
            (align-popup-selection popup-menu)))))

;;; ---------------------------------------------------------------------------
;;; Orderless-style matching
;;; ---------------------------------------------------------------------------
;;;
;;; Emacs' orderless: the input "foo bar" matches every candidate that contains
;;; "foo" AND "bar", anywhere and in any order, best matches first. Lem asks a
;;; completion function for candidates on each keystroke, so orderless is
;;; implemented by replacing the *filtering* of those functions; the popup,
;;; its size, bottom alignment and reversed order all stay as they are.
;;;
;;; Only inputs containing a space/tab change behaviour. A single token keeps
;;; lem's own matching -- prefix/substring/fuzzy for names, path completion for
;;; files -- so typing src/buffer/ in a file prompt still walks directories.

(defvar *orderless-input-p* nil
  "T when the last completion input was orderless (contained a space).
Read by the NARROWING-DOWN advice below: inserting the candidates' common
prefix makes no sense once the tokens match in arbitrary positions.")

(defun orderless-input-p (input)
  (and (find-if (lambda (c) (member c '(#\Space #\Tab))) input) t))

(defun orderless-tokens (input)
  (remove "" (uiop:split-string input :separator '(#\Space #\Tab))
          :test #'string=))

(defun orderless-ranges (string tokens)
  "Return the ((START . END) ...) ranges of each of TOKENS inside STRING.
Case-insensitive; NIL as soon as one token is missing."
  (let ((ranges '()))
    (dolist (token tokens (nreverse ranges))
      (let ((start (search token string :test #'char-equal)))
        (unless start (return nil))
        (push (cons start (+ start (length token))) ranges)))))

(defun orderless-sort-key (label tokens ranges)
  "Ranking key: prefix match of the first token first, then the tightest match
span, then the shortest candidate, then alphabetical."
  (let ((first-start (or (search (first tokens) label :test #'char-equal) 0)))
    (list (if (zerop first-start) 0 1)
          (- (reduce #'max (mapcar #'cdr ranges))
             (reduce #'min (mapcar #'car ranges)))
          (length label)
          label)))

(defun orderless-key-lessp (key1 key2)
  (loop :for a :in key1
        :for b :in key2
        :do (cond ((and (numberp a) (numberp b))
                   (cond ((< a b) (return t))
                         ((> a b) (return nil))))
                  ((string< a b) (return t))
                  ((string< b a) (return nil)))
        :finally (return nil)))

(defun orderless-filter (input elements &key key)
  "Filter ELEMENTS by the space-separated tokens of INPUT, best match first."
  (let* ((tokens (orderless-tokens input))
         (key (or key #'identity))
         (entries '()))
    (dolist (element elements)
      (let* ((string (funcall key element))
             (ranges (orderless-ranges (string string) tokens)))
        (when ranges
          (push (cons (orderless-sort-key (string string) tokens ranges) element)
                entries))))
    (mapcar #'cdr (sort entries
                        (lambda (a b)
                          (orderless-key-lessp (car a) (car b)))))))

;;; Commands (M-x).  Built on PROMPT-COMMAND-COMPLETION with an empty input, so
;;; the keybinding details and the candidate list come from lem itself; this
;;; only keeps the matches, highlights them (:chunks) and ranks them.

(defun orderless-command-matches (string &key candidates)
  "Candidate items for STRING: lem's own matching for one token, orderless
(space separated tokens, any order) otherwise."
  (cond ((not (orderless-input-p string))
         (setf *orderless-input-p* nil)
         (lem/prompt-window::prompt-command-completion string
                                                       :candidates candidates))
        (t
         (setf *orderless-input-p* t)
         (let ((tokens (orderless-tokens string))
               (entries '()))
           (dolist (item (lem/prompt-window::prompt-command-completion
                          "" :candidates candidates))
             (let* ((label (lem/completion-mode:completion-item-label item))
                    (ranges (orderless-ranges label tokens)))
               (when ranges
                 (push (list (orderless-sort-key label tokens ranges)
                             (lem/completion-mode:make-completion-item
                              :label label
                              :detail (lem/completion-mode:completion-item-detail item)
                              :chunks ranges))
                       entries))))
           (mapcar #'second
                   (sort entries
                         (lambda (a b)
                           (orderless-key-lessp (first a) (first b)))))))))

(defun orderless-command-completion (string &key candidates)
  "M-x candidates for STRING, each annotated with the command's description.
The matching lives in ORDERLESS-COMMAND-MATCHES; the annotation is described in
the marginalia section below, which is why it is applied to the *matched* set
(the description column is sized from the labels that will really be shown)."
  (marginalia-annotate-command-items
   (orderless-command-matches string :candidates candidates)))

;;; Buffers.  A token may match the buffer name or its file name, so "src buf"
;;; finds a buffer whose file lives under src.  Only matches inside the name are
;;; highlighted, since the name is what gets printed.

(defun orderless-buffers (input buffers)
  "Buffers whose name or file name contains every token of INPUT, best first."
  (let ((tokens (orderless-tokens input))
        (entries '()))
    (dolist (buffer buffers)
      (let* ((name (lem:buffer-name buffer))
             (filename (and (lem:buffer-filename buffer)
                            (namestring (lem:buffer-filename buffer))))
             (ranges (or (orderless-ranges name tokens)
                         (and filename (orderless-ranges filename tokens)))))
        (when ranges
          (push (cons (orderless-sort-key name tokens ranges) buffer) entries))))
    (mapcar #'cdr
            (sort entries
                  (lambda (a b) (orderless-key-lessp (car a) (car b)))))))

(defun orderless-buffer-completion (string)
  (cond ((not (orderless-input-p string))
         (setf *orderless-input-p* nil)
         (lem/prompt-window::prompt-buffer-completion string))
        (t
         (setf *orderless-input-p* t)
         (let ((tokens (orderless-tokens string)))
           (loop :for buffer :in (orderless-buffers string (lem:completion-buffer ""))
                 :collect (with-point ((s (lem/prompt-window::current-prompt-start-point))
                                       (e (lem/prompt-window::current-prompt-start-point)))
                            (lem/completion-mode:make-completion-item
                             :label (lem:buffer-name buffer)
                             :detail (let ((filename (lem:buffer-filename buffer)))
                                       (if filename
                                           (or (ignore-errors
                                                (enough-namestring filename
                                                                   (probe-file "./")))
                                               "")
                                           ""))
                             :chunks (orderless-ranges (lem:buffer-name buffer) tokens)
                             :start s
                             :end (line-end e))))))))

;;; Files.  With a space in the input every token must match the path of an
;;; entry of the directory being completed, e.g. "bu ini" matches
;;; "buffer/init.lisp"; picking a candidate replaces the whole input with that
;;; relative path.  Without a space nothing changes, so "src/buff" still walks
;;; directories and ~/ expands as usual.  This mirrors what orderless does in
;;; Emacs' find-file prompt: it searches the listing you are looking at, not the
;;; whole tree (for that, use find-file-recursively, which completion-files
;;; makes orderless below).

(defun orderless-file-completion (string directory &key directory-only)
  (cond ((not (orderless-input-p string))
         (setf *orderless-input-p* nil)
         (lem/prompt-window::prompt-file-completion string directory
                                                    :directory-only directory-only))
        (t
         (setf *orderless-input-p* t)
         (let* ((tokens (orderless-tokens string))
                (default-directory (uiop:ensure-directory-pathname
                                    (or directory (uiop:getcwd))))
                ;; The first token carries the path being typed, so the listing
                ;; to search is the directory it points into: "/p/src/bu" means
                ;; "list /p/src/".  Tokens are then matched against each
                ;; candidate's absolute path, which is also what Emacs does, so
                ;; a leading path token keeps matching and gets replaced along
                ;; with the rest when a candidate is picked.
                (base (let ((expanded (ignore-errors
                                        (uiop:ensure-pathname
                                         (expand-file-name (first tokens)
                                                           default-directory)))))
                        (cond ((and expanded (probe-file expanded))
                               (uiop:ensure-directory-pathname expanded))
                              ((and expanded
                                    (probe-file (uiop:pathname-directory-pathname expanded)))
                               (uiop:pathname-directory-pathname expanded))
                              (t default-directory))))
                (entries '()))
           (dolist (pathname (ignore-errors
                              (lem/buffer/file-utils:list-directory
                               base :directory-only directory-only)))
             (let ((label (or (ignore-errors (namestring pathname))
                              (princ-to-string pathname))))
               (when (orderless-ranges label tokens)
                 (push (list (orderless-sort-key label tokens
                                                 (orderless-ranges label tokens))
                             label)
                       entries))))
           (loop :for entry :in (sort entries
                                      (lambda (a b)
                                        (orderless-key-lessp (first a) (first b))))
                 :for label = (second entry)
                 :collect (with-point ((s (lem/prompt-window::current-prompt-start-point))
                                       (e (lem/prompt-window::current-prompt-start-point)))
                            (lem/completion-mode:make-completion-item
                             :label label
                             :start s
                             :end (line-end e))))))))

;;; A space has to separate two tokens instead of ending the completion: the
;;; default binding in the completion keymap is COMPLETION-INSERT-SPACE-AND-CANCEL
;;; (src/ext/completion-mode.lisp:116), which would close the popup the moment
;;; you start the second token.  Insert the space and refresh instead.

(define-command completion-insert-space-and-continue () ()
  "Insert a space and keep narrowing the completion."
  (insert-character (current-point) #\space)
  (lem/completion-mode:completion-refresh))

(define-key lem/completion-mode::*completion-mode-keymap*
  "Space" 'completion-insert-space-and-continue)

(setf lem:*prompt-command-completion-function* 'orderless-command-completion)
(setf lem:*prompt-buffer-completion-function* 'orderless-buffer-completion)
(setf lem:*prompt-file-completion-function* 'orderless-file-completion)

;;; The three helpers below are what other prompts use when they build their own
;;; candidate list instead of going through the *prompt-*-completion-function*
;;; variables: find-recent-file uses COMPLETION-STRINGS,
;;; find-file-recursively uses COMPLETION-FILES, and COMPLETION-BUFFER is used
;;; by anything that lists buffers.  Patching them makes those orderless too.
;;; They live in the locked LEM-CORE package, hence the unlock, and the original
;;; function is captured once so that reloading this file does not stack
;;; wrappers.

(defvar *original-completion-strings* nil)
(defvar *original-completion-files* nil)
(defvar *original-completion-buffer* nil)
(defvar *original-narrowing-down* nil)

#+sbcl
(sb-ext:without-package-locks
  (progn
    (setf *original-completion-strings*
          (or *original-completion-strings*
              (symbol-function 'lem:completion-strings)))
    (setf (symbol-function 'lem:completion-strings)
          (lambda (string strings &key key)
            (cond ((not (orderless-input-p string))
                   (setf *orderless-input-p* nil)
                   (funcall *original-completion-strings* string strings :key key))
                  (t
                   (setf *orderless-input-p* t)
                   (orderless-filter string strings :key key)))))

    (setf *original-completion-files*
          (or *original-completion-files*
              (symbol-function 'lem:completion-files)))
    (setf (symbol-function 'lem:completion-files)
          (lambda (string strings &key key)
            (cond ((not (orderless-input-p string))
                   (setf *orderless-input-p* nil)
                   (funcall *original-completion-files* string strings :key key))
                  (t
                   (setf *orderless-input-p* t)
                   (orderless-filter string strings :key key)))))

    (setf *original-completion-buffer*
          (or *original-completion-buffer*
              (symbol-function 'lem:completion-buffer)))
    (setf (symbol-function 'lem:completion-buffer)
          (lambda (string &optional buffers)
            (cond ((not (orderless-input-p string))
                   (setf *orderless-input-p* nil)
                   (if buffers
                       (funcall *original-completion-buffer* string buffers)
                       (funcall *original-completion-buffer* string)))
                  (t
                   (setf *orderless-input-p* t)
                   (orderless-buffers string
                                      (or buffers
                                          (funcall *original-completion-buffer* "")))))))

    ;; NARROWING-DOWN inserts the candidates' common prefix into the prompt.
    ;; With orderless tokens that prefix has nothing to do with the input, so
    ;; skip it whenever the last completion was orderless (TAB then just moves
    ;; the selection, as it should).
    (setf *original-narrowing-down*
          (or *original-narrowing-down*
              (symbol-function 'lem/completion-mode::narrowing-down)))
    (setf (symbol-function 'lem/completion-mode::narrowing-down)
          (lambda (context last-items)
            (unless *orderless-input-p*
              (funcall *original-narrowing-down* context last-items))))))

;;; ---------------------------------------------------------------------------
;;; Marginalia-style annotations in M-x
;;; ---------------------------------------------------------------------------
;;;
;;; Emacs' marginalia shows the first line of each command's docstring next to
;;; the command name in M-x -- the same text M-x describe-function prints.
;;; lem already has the plumbing: a completion item carries a :detail string
;;; (src/ext/completion-mode.lisp:52-56) which lem's own
;;; PROMPT-COMMAND-COMPLETION fills with the command's keybindings
;;; (src/ext/prompt-window.lisp:486-518); apply-print-spec then prints it after
;;; the label column in the dim DETAIL-ATTRIBUTE
;;; (src/ext/completion-mode.lisp:152-160).
;;;
;;; :detail is display-only: selecting a candidate inserts its :label, and
;;; narrowing-down takes the common prefix of :labels, so annotating items
;;; cannot change which command runs.  The docstring is read exactly the way
;;; describe-command reads it (src/commands/help.lisp:169-177):
;;; (documentation (command-name command) 'function).
;;;
;;; About 650 of the 1081 commands in a fully loaded image have no docstring
;;; (mostly extensions), so those keep their keybinding column and show nothing
;;; after it -- marginalia does the same for undocumented functions.
;;;
;;; The annotation is applied by ORDERLESS-COMMAND-COMPLETION, which is what
;;; *prompt-command-completion-function* points at, so every M-x path goes
;;; through it (single token, orderless tokens, and the :candidates lists that
;;; other commands pass to prompt-for-command).  It runs after matching, on the
;;; candidates that will really be shown, because the column widths are
;;; computed from them; annotating all 1081 commands would size the description
;;; from the longest command name in the image and leave ~20 usable columns.

(defvar *marginalia-key-column* 12
  "Minimum width of the right-aligned keybinding column that precedes the
command description. Widened when the current candidates carry longer ones.")

(defvar *marginalia-description-width* 60
  "Longest description, in characters. Shortened when the candidates have long
command names, so that no row can push the popup past the frame edge (the
popup is as wide as the prompt, which fills the frame).")

(defun marginalia-collapse-whitespace (string)
  "STRING with each run of spaces/tabs turned into one space."
  (with-output-to-string (s)
    (let ((pending nil))
      (loop :for c :across string
            :do (if (member c '(#\Space #\Tab))
                    (setf pending t)
                    (progn (when pending (write-char #\Space s))
                           (setf pending nil)
                           (write-char c s)))))))

(defun marginalia-truncate (string width)
  "STRING cut at a word boundary to at most WIDTH characters, with an ellipsis."
  (if (<= (length string) width)
      string
      (let* ((cut (subseq string 0 (1- width)))
             (space (position #\Space cut :from-end t)))
        (concatenate 'string
                     (if (and space (> space (floor width 2)))
                         (subseq cut 0 space)
                         (string-right-trim " " cut))
                     "…"))))

(defun marginalia-docstring (name)
  "First non-empty line of the docstring of the command named NAME, or NIL."
  (let* ((command (lem:find-command name))
         (doc (and command
                   (documentation (lem:command-name command) 'function))))
    (when doc
      (loop :for line :in (uiop:split-string doc :separator '(#\Newline))
            :for text := (marginalia-collapse-whitespace
                          (string-trim '(#\Space #\Tab #\Return) line))
            :unless (string= text "")
              :do (return text)))))

(defun marginalia-first-keybinding (detail)
  "First binding of a keybinding list DETAIL (\"C-x C-f, F5\"), or \"\"."
  (let ((end (search ", " detail)))
    (if end (subseq detail 0 end) detail)))

(defun marginalia-copy-item (item detail)
  "ITEM with its :detail replaced by DETAIL (the other slots preserved)."
  (lem/completion-mode:make-completion-item
   :label (lem/completion-mode:completion-item-label item)
   :detail detail
   :chunks (lem/completion-mode::completion-item-chunks item)
   :start (lem/completion-mode::completion-item-start item)
   :end (lem/completion-mode::completion-item-end item)
   :focus-action (lem/completion-mode::completion-item-focus-action item)))

(defun marginalia-annotate-shown-command-items (items)
  "ITEMS with each command's docstring appended after its keybindings.
The keybinding column is right-aligned and the description column starts at
the same screen column on every row, the way marginalia aligns its fields."
  (let* ((max-label
           (reduce (lambda (width item)
                     (max width (length (lem/completion-mode:completion-item-label item))))
                   items :initial-value 0))
         (key-column
           (reduce (lambda (width item)
                     (max width (length (marginalia-first-keybinding
                                         (lem/completion-mode:completion-item-detail item)))))
                   items :initial-value *marginalia-key-column*))
         ;; apply-print-spec puts one space, the label padded to max-label + 1,
         ;; two spaces, the detail and one trailing space on the row, so the
         ;; frame width minus those fixed columns is what is left for the
         ;; description (the -2 keeps the popup inside the frame border).
         (description-width
           (max 20 (min *marginalia-description-width*
                        (- (lem:display-width) 8 max-label key-column)))))
    (mapcar (lambda (item)
              (alexandria:if-let (description
                                  (marginalia-docstring
                                   (lem/completion-mode:completion-item-label item)))
                (marginalia-copy-item
                 item
                 (format nil "~va  ~a"
                         key-column
                         (marginalia-first-keybinding
                          (lem/completion-mode:completion-item-detail item))
                         (marginalia-truncate description description-width)))
                item))
            items)))

;;; Only the candidates that will be on screen matter for the column widths,
;;; and completion-mode keeps just the first *limit-number-of-items* of them
;;; (src/ext/completion-mode.lisp:302-312, applied before *completion-reverse*
;;; flips the list).  Annotating that prefix keeps the description column as
;;; wide as the popup can afford instead of sizing it from all 1081 commands.

(defun marginalia-annotate-command-items (items)
  "ITEMS with each command's docstring appended after its keybindings."
  (let* ((limit lem/completion-mode::*limit-number-of-items*)
         (shown (if (and limit (< limit (length items)))
                    (subseq items 0 limit)
                    items))
         (annotated (marginalia-annotate-shown-command-items shown)))
    (if (eq shown items)
        annotated
        (append annotated (nthcdr (length shown) items)))))


;;; Ghostty-backed terminal (user extension).
;;;
;;; The libghostty-vt terminal now lives outside the Lem tree, as a user
;;; extension under ~/.config/lem/extensions/ghostty-terminal/.  It is
;;; loaded here at startup instead of being compiled into the image, so
;;; the stock Lem binary is left untouched.  ASDF finds its .asd through
;;; the central-registry entry below; the system sources and its vendored
;;; libghostty-vt build are resolved relative to that .asd.

;;; lem-terminal draws its terminal with libvterm.  This extension replaces
;;; that backend with libghostty-vt, the standalone VT engine from Ghostty:
;;; it handles modern keyboard/mouse protocols, bracketed paste, 256/24-bit
;;; colour and grapheme clusters, and keeps scrollback in native code.  The
;;; PTY comes from SBCL (:pty), the cells are drawn with CFFI, and there is
;;; no native helper to build for this extension itself.
;;;
;;; Requirements: SBCL 2.6+, libghostty-vt 1.0+ (a linux/x64 build is
;;; vendored in the extension's lib/), and cffi-libffi from
;;; Quicklisp.  M-x ghostty-terminal opens a shell in the current
;;; directory; inside the terminal C-c goes to the program, so open it from
;;; another buffer with C-c t or from anywhere with M-x.  M-[ enters copy
;;; mode and Escape returns to the live terminal.
;;;
;;; The library path can be overridden with LEM_GHOSTTY_VT_LIBRARY, and
;;; other platforms can build it with the extension's build-ghostty-vt.lisp.
(let ((source (merge-pathnames "extensions/ghostty-terminal/" (lem-home))))
  (pushnew source asdf:*central-registry* :test #'equal))
(handler-case
    (progn
      (asdf:load-system :lem-ghostty-terminal)
      ;; Look the symbol up at run time: if the load failed, the package
      ;; does not exist and a package-qualified symbol here would signal a
      ;; reader error before HANDLER-CASE could report the real cause.
      (let* ((package (find-package :lem-ghostty-terminal))
             (setup (and package (find-symbol "SETUP" package))))
        (if setup
            (funcall setup :key "C-c t")
            (error "package LEM-GHOSTTY-TERMINAL was not created"))))
  (error (condition)
    (format *error-output*
            "~&lem-ghostty-terminal unavailable: ~A~%" condition)))

;;; MCP server, started automatically at launch.
;;;
;;; The MCP server exposes editor_send_keys and the buffer/command tools over
;;; HTTP JSON-RPC on 127.0.0.1:7890 (extensions/mcp-server).  Starting it can
;;; fail -- for example when the port is already held by a previous Lem -- and
;;; that must not abort the rest of init, so it is caught here.
;;;
;;; The symbol is looked up at run time because a package-qualified
;;; LEM-MCP-SERVER:MCP-SERVER-START would signal a reader error if the system
;;; were absent, before any handler could report the real cause.
(let ((start (find-symbol "MCP-SERVER-START" :lem-mcp-server)))
  (cond
    ((null start)
     (format *error-output* "~&lem-mcp-server not loaded; MCP server disabled~%"))
    (t
     (handler-case
         (funcall start nil nil)
       (error (condition)
         (format *error-output* "~&lem-mcp-server failed to start: ~A~%" condition))))))
