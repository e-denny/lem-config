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

(defvar *patch-source*
  (uiop:read-file-string
   (asdf:system-relative-pathname "org-latex-preview" "src/webview-image-patch.js"))
  "the script injected into the client, see webview-image-patch.js.")

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
  "Report how many character cells an image covers, so that the text after it
is placed beyond it rather than on top of it."
  (defmethod lem-server::object-width ((object lem-core/display:image-object))
    (let ((pixels (or (ignore-errors (lem-core/display:image-object-width object)) 0)))
      (max 1 (ceiling (max 1 pixels) (max 1 *cell-width-px*))))))

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
        (install-image-object-drawing)
        t)
    (error (condition)
      (warn "org-latex-preview: image drawing unavailable: ~A" condition)
      nil)))
