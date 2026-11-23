(defsystem "org-latex-preview"
  :description "Latex previews in organ-mode buffers, drawn by the webview frontend."
  :version "0.1"
  :author "Edgar Denny"
  :depends-on ("lem/core" "lem-server" "organ-mode")
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "webview-images")
     (:file "organ-preview")
     (:file "latex-fragments")))))
