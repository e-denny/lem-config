(uiop:define-package :org-latex-preview/webview
  (:use :cl)
  (:documentation "Image drawing for Lem's webview frontend.

The frontend draws character cells only: it has no image path at all, so a
drawing object that stands for an image (an attribute carrying :image, :width,
:height and :ascent) reaches the client as its placeholder text and is never
painted.  Everything here exists to close that gap without modifying the Lem
tree: the client is patched at run time with a small script evaluated through
the frontend's own js-eval call, and the server side of the jsonrpc protocol --
which already builds image objects -- is taught to send them.

Nothing here is specific to latex previews; any buffer that puts an image in an
attribute benefits.  The image source is a path on this machine, served to the
client by lem-server's /local/ route.")
  (:export
   :*cell-width-px*
   :*cell-height-px*
   :*font-size-px*
   :cell-width-px
   :cell-height-px
   :font-size-px
   :image-rendering-ready-p
   :install-drawing-support
   :ensure-image-rendering
   :mark-windows-dirty
   :refresh-display))

(in-package :org-latex-preview/webview)

;;; Cell metrics, reported by the client once the patch is in place.  They are
;;; what turns an image's pixel size into the number of character cells it
;;; occupies, so they default to the values the strip below the editor uses
;;; (18px font in a 9x20 cell) until the client answers.

(defvar *cell-width-px* 9)
(defvar *cell-height-px* 20)
(defvar *font-size-px* 16)

(defvar *ready-p* nil
  "True once the client has installed the patch and reported its metrics.")

(defvar *retry-timer* nil)
(defvar *retry-count* 0)
(defconstant +retry-limit+ 120
  "Give up patching after this many attempts, one every half second.")

(defparameter *patch-source*
  (uiop:read-file-string
   (asdf:system-relative-pathname "org-latex-preview" "src/webview-image-patch.js"))
  "the script injected into the client, see webview-image-patch.js.
DEFPARAMETER rather than DEFVAR: the file is read when this system is compiled,
and a reloaded system has to send the script it was just compiled from.")

(defvar *original-ensure-attribute* nil
  "lem-server's own attribute encoder, kept so the image fields can be added
on top of it instead of replacing it.")

(defun cell-width-px () *cell-width-px*)
(defun cell-height-px () *cell-height-px*)
(defun font-size-px () *font-size-px*)
(defun image-rendering-ready-p () *ready-p*)

(defun webview-implementation-p ()
  "Whether the running Lem is the webview (jsonrpc) frontend."
  (handler-case
      (let ((class (find-class 'lem-server:jsonrpc nil)))
        (and class (typep (lem:implementation) class)))
    (error () nil)))

(defun current-view ()
  (let ((window (and (fboundp 'lem:current-window) (lem:current-window))))
    (and window (lem:window-view window))))

(defun send-patch ()
  "Evaluate the patch in the client.  Returns true when it was handed over.
The call is a notification: it does not wait for the client, which reports
back through the metrics method instead."
  (let ((view (current-view)))
    (when (and view (webview-implementation-p))
      (handler-case
          (progn (lem-if:js-eval (lem:implementation) view *patch-source*) t)
        (error () nil)))))

(defun retire-retry-timer ()
  (when *retry-timer*
    (ignore-errors (lem:stop-timer *retry-timer*))
    (setf *retry-timer* nil)))

(defun retry-patch ()
  (cond
    (*ready-p* (retire-retry-timer))
    ((>= *retry-count* +retry-limit+) (retire-retry-timer))
    (t
     (incf *retry-count*)
     (send-patch))))

(defun ensure-image-rendering ()
  "Patch the client if it is not patched yet, retrying until it answers."
  (when (webview-implementation-p)
    (send-patch)
    (unless (or *ready-p* *retry-timer*)
      (setf *retry-timer*
            (lem:start-timer
             (lem:make-timer #'retry-patch :name "org-latex-preview-patch")
             0.5
             :repeat t)))))

;;; Server side.

(defun install-metrics-method ()
  (lem-server:register-method
   "org-latex-preview/metrics"
   (lambda (arguments)
     (labels ((field (name)
                (let ((value (gethash name arguments)))
                  (and (numberp value) (plusp value) value))))
       (let ((width (field "cellWidth"))
             (height (field "cellHeight"))
             (size (field "fontSize")))
         (when width (setf *cell-width-px* width))
         (when height (setf *cell-height-px* height))
         (when size (setf *font-size-px* size)))
       (setf *ready-p* t
             *retry-count* 0)
       (retire-retry-timer)
       nil))))

(defun mark-windows-dirty ()
  "Mark every window as needing a redraw.
lem-server declares itself :NO-FORCE-NEEDED, which makes REDRAW-DISPLAY drop its
FORCE argument, so a line whose fingerprint is unchanged is skipped -- which is
exactly the case when an image has simply finished loading."
  (let ((function (or (find-symbol "NEED-TO-REDRAW" :lem)
                      (find-symbol "NEED-TO-REDRAW" :lem-core))))
    (when (and function (fboundp function))
      (dolist (window (lem:window-list))
        (funcall function window))
      (ignore-errors
       (dolist (window (lem:frame-floating-windows (lem:current-frame)))
         (funcall function window)))
      t)))

(defun refresh-display ()
  "Redraw every window, even the ones whose content fingerprint is unchanged."
  (mark-windows-dirty)
  (lem:redraw-display :force t))

(defun install-redraw-method ()
  ;; the client asks for another frame when an image finishes decoding; this
  ;; arrives on the websocket thread, and drawing belongs to the editor thread.
  (lem-server:register-method
   "org-latex-preview/redraw"
   (lambda (arguments)
     (declare (ignore arguments))
     (lem:send-event #'refresh-display)
     nil)))

(defun install-attribute-encoding ()
  "Add the image fields to what lem-server sends for an attribute.

The client needs the image source and the pixel box to draw in; the ascent is
the part of that box above the text baseline, as a percentage of its height."
  (unless *original-ensure-attribute*
    (setf *original-ensure-attribute* (symbol-function 'lem-server::ensure-attribute))
    (let ((original *original-ensure-attribute*))
      ;; every drawing object goes through this, so the added fields are
      ;; computed defensively: an error here would blank the whole display.
      (setf (symbol-function 'lem-server::ensure-attribute)
            (lambda (attribute)
              (let ((encoded (funcall original attribute)))
                (handler-case
                    (let ((object (and encoded (lem:ensure-attribute attribute nil))))
                      (when object
                        (let ((image (lem:attribute-image object)))
                          (when image
                            (setf (gethash "image" encoded)
                                  (if (stringp image)
                                      image
                                      (uiop:native-namestring image))
                                  (gethash "imageWidth" encoded)
                                  (lem:attribute-width object)
                                  (gethash "imageHeight" encoded)
                                  (lem:attribute-height object)
                                  (gethash "imageAscent" encoded)
                                  (lem-core::attribute-value object :ascent))))))
                  (error () nil))
                encoded))))))

(defun install-image-object-width ()
  "Report how many character columns an image covers, so that the text after it
is placed beyond it rather than on top of it.

LEM-CORE:OBJECT-WIDTH caches what it measures in the WIDTH slot of the drawing
object, and only measures when that slot is empty.  An image object is created
with its pixel width in that same slot -- IMAGE-OBJECT's WIDTH is
DRAWING-OBJECT's WIDTH, given an initarg -- so the plain image object reports
its pixels as columns: a 438px equation laid out as 438 columns, far wider
than the window, which wrapped the line and pushed the text after an inline
preview a screen's width away.  Emptying the slot as the object is built makes
it measure through OBJECT-WIDTH below, and measure once."
  (defmethod initialize-instance :after ((object lem-core/display:image-object)
                                          &key &allow-other-keys)
    (when (webview-implementation-p)
      (setf (lem-core::drawing-object-width object) nil)))
  (defmethod lem-server::object-width ((object lem-core/display:image-object))
    (multiple-value-bind (width height) (image-pixels object)
      (declare (ignore height))
      (max 1 (ceiling (max 1 width) (max 1 *cell-width-px*))))))

(defun image-pixels (object)
  "The pixel width and height of the image OBJECT.

They are read from the attribute the image was made from rather than from the
object's own WIDTH and HEIGHT slots.  Those slots are the ones
LEM-CORE:OBJECT-WIDTH and the size comparisons use, and an image object is
built with its pixel width in the first of them, which this frontend counts in
character columns; see INSTALL-IMAGE-OBJECT-WIDTH."
  (let ((attribute (ignore-errors (lem-core::image-object-attribute object))))
    (values (or (and attribute (lem:attribute-width attribute))
                (ignore-errors (lem-core/display:image-object-width object))
                0)
            (or (and attribute (lem:attribute-height attribute))
                (ignore-errors (lem-core/display:image-object-height object))
                0))))

(defun image-rows (object)
  "How many character rows an image covers, at the client's cell height."
  (multiple-value-bind (width height) (image-pixels object)
    (declare (ignore width))
    (max 1 (ceiling height (max 1 *cell-height-px*)))))

(defun install-image-object-height ()
  "Reserve the rows an image covers, not the single row of every other object.

This server frontend lays a window out in character rows, not pixels: the
client multiplies the row index it is sent by its cell height.  So the height
of a drawing object is a number of rows here, and every object used to answer
1 -- including images, which are measured in pixels.  An image taller than one
row was therefore laid out with the next lines inside it: the client paints
rows in order, each after filling it with its background, so the line below an
image painted over the image's lower part.  A fraction's numerator and bar
survived and its denominator was cut off by the first line that followed it.
Reporting the rows an image really needs makes the rows below it start below
it.  It is also what the client's clamp of the image into its block assumes."
  (defmethod lem-if:object-height ((implementation lem-server:jsonrpc)
                                   (object lem-core/display:image-object))
    (declare (ignore implementation))
    (image-rows object)))

(defvar *original-render-line* nil)

(defun centered-attribute-p (attribute)
  "Whether ATTRIBUTE asks for the image it carries to be centred."
  (and attribute
       (ignore-errors (lem-core::attribute-value attribute :center))))

(defun centered-line-p (objects)
  "Whether OBJECTS are a display fragment standing alone on its line.

That is: an image that asks to be centred, and nothing but images that do, the
spaces between them, and the cursor or the background that fills to the end of
a line with the cursor on it.  A line of that shape is what a display fragment
is reduced to once its source is concealed, and the whole line is what gets
centred -- centring each image on its own would stack the fragments of a line
that holds two of them, as organ-mode puts the two halves of a balance-sheet
identity on one line."
  (and (loop :for object :in objects
             :always (typecase object
                       (lem-core/display:image-object
                        (centered-attribute-p
                         (lem-core::image-object-attribute object)))
                       (lem-core/display:text-object
                        (every (lambda (character) (char= character #\Space))
                               (lem-core/display:text-object-string object)))
                       ((or lem-core/display:eol-cursor-object
                            lem-core/display:extend-to-eol-object)
                        t)
                       (t nil)))
       (some (lambda (object) (typep object 'lem-core/display:image-object)) objects)))

(defun install-line-rendering ()
  "Clear every row of a line, not only the row it starts at, and centre the
lines that hold an isolated display fragment.

The client clears one row per rendered line, so the rows an image adds would
keep whatever an earlier frame drew there -- the lower half of a wider
preview, or of one whose image now sits elsewhere on the line.  Those rows
belong to the image's line alone: no other line draws in them, and clearing
them whole is what lets the image be moved or shrunk without leaving a ghost
beside it.  The modeline is not this, so it is left alone.

Centring shifts where the line is drawn, not the line itself: the column the
line starts at is what is moved, so everything on the line moves together and
the buffer keeps the layout it computed."
  ;; the original is kept from the first install, and the wrapper is rebuilt
  ;; every time, so a reloaded file replaces the previous wrapper rather than
  ;; nesting one inside it -- or, worse, leaving it in place because the
  ;; original was already saved.
  (unless *original-render-line*
    (setf *original-render-line* (symbol-function 'lem-server::render-line)))
  (let ((original *original-render-line*))
    (setf (symbol-function 'lem-server::render-line)
          (lambda (jsonrpc view x y objects)
            (when (eq (symbol-value 'lem-server::*put-target*) :edit-area)
              (let ((rows (loop :for object :in objects
                                :maximize (lem-if:object-height
                                           (lem:implementation) object))))
                (loop :for row :from (1+ y) :below (+ y rows)
                      :do (lem-server::notify*
                           jsonrpc
                           "clear-eol"
                           (lem-server/utils:hash
                            "viewInfo" (lem-server::view-id-hash view)
                            "x" 0
                            "y" row)))
                (when (centered-line-p objects)
                  (let* ((implementation (lem:implementation))
                         (width (loop :for object :in objects
                                      :sum (lem-if:object-width implementation object)))
                         (view-width (lem-if:view-width implementation view)))
                    (when (< width view-width)
                      (setf x (+ x (floor (- view-width width) 2))))))))
            (funcall original jsonrpc view x y objects)))))

(defun install-image-object-drawing ()
  "Send an image object to the client as a run of placeholder cells carrying
the image attribute.  The patched client paints the image over those cells."
  (defmethod lem-server::draw-object ((jsonrpc lem-server:jsonrpc)
                                      (object lem-core/display:image-object)
                                      x y view)
    (let* ((attribute (lem-core::image-object-attribute object))
           (cells (lem-server::object-width object)))
      (lem-server::put jsonrpc view x y
                       (make-string cells :initial-element #\Space)
                       attribute
                       :text-width cells))))

(defun install-drawing-support ()
  "Teach this Lem to draw images in the webview frontend.  Idempotent."
  (handler-case
      (progn
        (install-metrics-method)
        (install-redraw-method)
        (install-attribute-encoding)
        (install-image-object-width)
        (install-image-object-height)
        (install-line-rendering)
        (install-image-object-drawing)
        t)
    (error (condition)
      (warn "org-latex-preview: image drawing unavailable: ~A" condition)
      nil)))
