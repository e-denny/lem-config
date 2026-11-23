(uiop:define-package :org-latex-preview/fragments
  (:use :cl)
  (:documentation "The latex fragments organ-mode cannot see, and the way they compile.

cltpt reads latex as \\(...\\), \\[...\\] and \\begin{env}.  Org files, and this
user's notes, write inline math as $...$ and display math as $$...$$, so those
two forms are added as text objects here -- subclasses of cltpt's own, which is
what makes organ-mode's preview list pick them up.

Display fragments are also compiled differently.  preview.sty crops a page to the
box it captured, and a display fragment is a line of the full text width even
when the ink in it is narrow, so \\[...\\], $$...$$ and \\begin{equation} all
came out as images as wide as the page.  Typesetting them as one inline display
($\\displaystyle ...$) keeps the display layout and gives the crop something of
the fragment's own size to work with.")
  (:export :install
           :*math-environments*
           :dollar-inline-math
           :dollar-display-math))

(in-package :org-latex-preview/fragments)

;;; $...$ and $$...$$

;; the double dollar form is registered before the single dollar one, so that a
;; "$$" is read as one display fragment rather than an empty inline one.
(cltpt/base:define-text-object dollar-display-math
  :superclass cltpt/latex:display-math
  :rule `(:pattern
          (cltpt/combinator:pair
           (cltpt/combinator:unescaped (cltpt/combinator:literal "$$"))
           (cltpt/combinator:unescaped (cltpt/combinator:literal "$$"))
           :rules-for-content ,(cltpt/base:macro-text-object-types)
           :allow-empty nil)
          :on-char #\$)
  :documentation "org-mode display math written $$...$$.")

(cltpt/base:define-text-object dollar-inline-math
  :superclass cltpt/latex:inline-math
  :rule `(:pattern
          (cltpt/combinator:pair
           (cltpt/combinator:unescaped (cltpt/combinator:literal "$"))
           (cltpt/combinator:unescaped (cltpt/combinator:literal "$"))
           :rules-for-content ,(cltpt/base:macro-text-object-types)
           :allow-multiline nil
           :allow-empty nil)
          :on-char #\$)
  :documentation "org-mode inline math written $...$.")

(defmethod cltpt/base:text-format-text-object-types :around ((fmt (eql :org-mode)))
  (append (call-next-method)
          '(dollar-display-math dollar-inline-math)))

(defvar *original-inline-types* nil)

(defun install-inline-types ()
  "Let the two dollar forms appear inside other inline content, such as a list
item, by adding them to the rule set nested content is parsed with."
  (unless *original-inline-types*
    (setf *original-inline-types*
          (symbol-function 'cltpt/org-mode::org-mode-inline-text-object-types))
    (let ((original *original-inline-types*))
      (setf (symbol-function 'cltpt/org-mode::org-mode-inline-text-object-types)
            (lambda ()
              (append (funcall original)
                      '(dollar-display-math dollar-inline-math)))))))

;;; Display fragments as tight previews.

(defvar *math-environments*
  '("equation" "equation*" "align" "align*" "aligned" "gather" "gather*"
    "multline" "multline*" "eqnarray" "eqnarray*" "displaymath" "math"
    "matrix" "pmatrix" "bmatrix" "Bmatrix" "vmatrix" "Vmatrix" "smallmatrix"
    "cases" "split")
  "Environments whose body is display math, and so can be previewed as one
inline display.  Every other environment is left alone: a tabular or a figure
is not math, and the preview module's own conversion of it is as good a guess
as any.")

(defun latex-environment-name (object)
  "The environment OBJECT opens, or NIL."
  (let* ((text (cltpt/base:text-object-text object))
         (start (search "\\begin{" text)))
    (when start
      (let ((from (+ start (length "\\begin{")))
            (to (position #\} text :start (+ start (length "\\begin{")))))
        (when to
          (subseq text from to))))))

(defun strip-delimiters (text open close)
  "TEXT without a leading OPEN and a trailing CLOSE."
  (let ((end (- (length text) (length close))))
    (if (and (<= (length open) end)
             (eql 0 (search open text))
             (string= close text :start2 end :end2 (length text)))
        (subseq text (length open) end)
        text)))

(defun fragment-contents (object)
  "OBJECT's text without its delimiters.

Only an environment records its contents region; the pair-based fragments carry
their delimiters in the text itself."
  (let ((text (cltpt/base:text-object-text object)))
    (cond
      ((typep object 'dollar-display-math) (strip-delimiters text "$$" "$$"))
      ((typep object 'dollar-inline-math) (strip-delimiters text "$" "$"))
      ((typep object 'cltpt/latex:display-math) (strip-delimiters text "\\[" "\\]"))
      ((typep object 'cltpt/latex:inline-math) (strip-delimiters text "\\(" "\\)"))
      ;; an environment: drop its own open and close tags
      (t (let ((name (latex-environment-name object)))
           (if name
               (strip-delimiters text
                                 (format nil "\\begin{~A}" name)
                                 (format nil "\\end{~A}" name))
               text))))))

(defun drop-command (command text)
  "TEXT without any COMMAND{...} occurrence, COMMAND ending in its brace."
  (loop :with result = text
        :for start = (search command result)
        :while start
        :do (let ((end (position #\} result :start start)))
              (setf result
                    (if end
                        (concatenate 'string (subseq result 0 start)
                                     (subseq result (1+ end)))
                        (subseq result 0 start))))
        :finally (return result)))

(defun drop-numbering (text)
  "TEXT without \\label{...} and \\tag{...}: a preview carries no counter."
  (drop-command "\\tag{" (drop-command "\\label{" text)))

(defun display-source (contents)
  "CONTENTS typeset as a display whose box is the fragment's own size.

Rows of a cases or matrix environment are already aligned by that environment,
so only bare rows are put in an aligned block."
  (let* ((display (drop-numbering contents))
         (rows-p (and (not (search "\\begin{" display))
                      (or (search "&" display)
                          (search "\\\\" display)))))
    (format nil "$\\displaystyle ~A$"
            (if rows-p
                (format nil "\\begin{aligned}~A\\end{aligned}" display)
                display))))

(defvar *original-expanded-latex* nil)

(defun install-display-conversion ()
  (unless *original-expanded-latex*
    (setf *original-expanded-latex*
          (symbol-function 'cltpt/latex:expanded-latex))
    (let ((original *original-expanded-latex*))
      (setf (symbol-function 'cltpt/latex:expanded-latex)
            (lambda (object)
              (cond
                ((typep object 'cltpt/latex:display-math)
                 (display-source (fragment-contents object)))
                ((and (typep object 'cltpt/latex:latex-env)
                      (member (latex-environment-name object)
                              *math-environments*
                              :test #'equal))
                 (display-source (fragment-contents object)))
                (t (funcall original object))))))))

;;; The fragment, delimiters included, is what the image stands in for.

(defun install-fragment-region ()
  "Preview the whole fragment rather than its contents.

organ-mode previews only the contents of a fragment, which is right for an
environment (\\begin{equation} is not math) but leaves the dollars of $...$ on
screen around the image.  Hiding the delimiters too is what org-mode does: the
image is the fragment."
  (setf (symbol-function 'organ/organ-mode::latex-object-region)
        (lambda (object)
          (let ((begin (cltpt/base:text-object-begin-in-root object)))
            (values begin
                    (+ begin (length (cltpt/base:text-object-text object))))))))

(defun install ()
  (install-inline-types)
  (install-display-conversion)
  (install-fragment-region)
  ;; rules embed the rules of the types defined before them, so they are
  ;; re-derived once the new types are in place.
  (cltpt/base:rebuild-text-object-rules))

(install)
