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

;;; Flat frame around floating windows, no drop shadow, no rounded corners.
;;;
;;; Popup windows created with a border -- the M-x prompt and its completion
;;; list, along with the other popup windows -- get the wrapper class
;;; `lem-editor__floating-window--bordered`.  Its rule in editor.css carries
;;; `box-shadow: 4px 4px 16px black, 0 0 0 1px rgba(128, 128, 128, 0.2)`: the
;;; first shadow is the soft black halo, the second the faint grey hairline.
;;; One spread-only shadow replaces both, so the halo is gone and a single 1px
;;; white line is all that is left.  The 8px radius that editor.css puts on the
;;; same rule is switched off too, so the prompt box and the candidate list are
;;; plain rectangles.  It is a box-shadow rather than a `border`
;;; on purpose: the wrapper is sized in JavaScript from the character grid, and
;;; a real border would eat 1px of the padding box on every side and nudge the
;;; popup's text out of alignment with the buffer behind it.  Same specificity
;;; as the editor.css rule, but loaded later, so source order lets it win.
(let ((load-css (uiop:find-symbol* :load-css :lem-server nil)))
  (when load-css
    (funcall load-css
             ".lem-editor__floating-window--bordered { box-shadow: 0 0 0 1px #ffffff; border-radius: 0; }")))

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
;;; The second family is not decoration.  The frontend draws buffer text on a
;;; canvas with `ctx.font` set to this string, and WebKitGTK's canvas text does
;;; not run fontconfig fallback for Private Use Area codepoints: with only
;;; "Adwaita Mono" in the stack, every Nerd Font icon (eza --icons, nerd font
;;; glyphs from any program) is drawn as Adwaita Mono's .notdef box.  Normal
;;; scripts such as CJK do fall back, which is why only icons are affected.
;;; Naming the Nerd Font explicitly resolves them.  SauceCodePro Nerd Font
;;; Mono is patched from Source Code Pro, so its advance is the same 9.6px and
;;; the grid does not move; the ASCII range still comes from Adwaita Mono.
;;
;;; SET-FONT exists for every frontend, but only the server/webview one takes a
;;; CSS family name, so this is guarded the same way as the cursor rule.
(let ((set-font (uiop:find-symbol* :set-font :lem nil))
      (server-frontend-p (uiop:find-symbol* :load-css :lem-server nil)))
  (when (and set-font server-frontend-p)
    (funcall set-font
             :name "Adwaita Mono, \"SauceCodePro Nerd Font Mono\""
             :size 16)))

;;; Vertico-style completion (user extensions).
;;;
;;; The prompt and completion setup that used to live here is three systems
;;; under extensions/, each loaded from its own directory:
;;;
;;;   vertico     bottom-of-frame prompt, width cap and centering, candidate
;;;               popup size and bottom-row alignment, Return selecting and
;;;               submitting in one keystroke, and keeping the completion alive
;;;               when nothing matches.
;;;   orderless   space-separated tokens that match anywhere and in any order,
;;;               for commands, buffers and files, plus the patches that make
;;;               lem's own completion helpers orderless.
;;;   marginalia  command docstrings next to the command name in M-x.
;;;
;;; Loading a system applies it: the settings, keymap bindings and function
;;; patches run at load, so the order below matters only in that marginalia
;;; wraps orderless's completion function.  See each extension's README for its
;;; knobs.  Failures must not abort the rest of init, hence the handler.

(dolist (extension '("vertico" "orderless" "marginalia"))
  (pushnew (merge-pathnames (format nil "extensions/~a/" extension) (lem-home))
           asdf:*central-registry* :test #'equal))

(handler-case
    (dolist (system '("lem-vertico" "lem-orderless" "lem-marginalia"))
      (asdf:load-system system))
  (error (condition)
    (format *error-output* "~&completion extensions unavailable: ~A~%" condition)))


;;; Which-key: show the keys that follow a prefix.
;;;
;;; Lem ships a transient popup (extensions/transient): a bottomside window that
;;; lists a keymap's bindings, shown by KEYMAP-ACTIVATE while a prefix key is
;;; being read, after *transient-popup-delay* milliseconds.  Out of the box it
;;; only reacts to keymaps made with DEFINE-TRANSIENT -- so the built-in prefixes
;;; such as C-x and C-c show nothing -- or to *transient-always-show*, which also
;;; leaves the root keymap on screen permanently.
;;;
;;; RESOLVE-TRANSIENT-KEYMAP decides what KEYMAP-ACTIVATE displays.  The advice
;;; accepts any pending keymap, so every prefix opens the popup, and leaves the
;;; original's answers alone for the cases it already handles (explicit
;;; transients and mode-level transient keymaps).  When a sequence finishes or is
;;; cancelled, KEYMAP-ACTIVATE is called with *root-keymap*, which the advice
;;; ignores, so the popup disappears again -- which-key behaviour rather than a
;;; permanent cheat sheet.  The delay is the extension's: at 500ms a sequence
;;; typed at speed never draws anything.
;;;
;;; M-x which-key-mode turns it off and on.  Symbols are looked up at run time
;;; because a package-qualified LEM/TRANSIENT::RESOLVE-TRANSIENT-KEYMAP would be
;;; a reader error if the extension were missing, before the handler could
;;; report the real cause.

(defvar *original-resolve-transient-keymap* nil
  "The unadvised RESOLVE-TRANSIENT-KEYMAP, captured on the first load.")

(defvar *which-key-mode* t
  "T when a prefix key opens the transient popup, i.e. which-key behaviour.")

(define-command which-key-mode () ()
  "Toggle which-key: show the keybindings that follow a prefix key."
  (setf *which-key-mode* (not *which-key-mode*))
  (message "which-key ~:[off~;on~]" *which-key-mode*))

(handler-case
    (progn
      (asdf:load-system :lem-transient)
      (let ((resolve (find-symbol "RESOLVE-TRANSIENT-KEYMAP" :lem/transient))
            (root (find-symbol "*ROOT-KEYMAP*" :lem)))
        (when (and resolve root)
          (setf *original-resolve-transient-keymap*
                (or *original-resolve-transient-keymap*
                    (symbol-function resolve)))
          (setf (symbol-function resolve)
                (lambda (&optional keymap)
                  (or (funcall *original-resolve-transient-keymap* keymap)
                      (and *which-key-mode*
                           keymap
                           (not (eq keymap (symbol-value root)))
                           keymap)))))))
  (error (condition)
    (format *error-output* "~&which-key unavailable: ~A~%" condition)))

;;; organ-mode (user extension): org-mode-style notes, agenda and roam.
;;;
;;; organ-mode lives outside the Lem tree, in extensions/organ-mode/, next to
;;; cltpt (extensions/cltpt), the text parser it is built on; both are found
;;; through the same central-registry entry as the extensions above.  Loading
;;; the system is the whole installation: it binds C-c r to the organ keymap,
;;; registers the major mode for *.org files, and hooks the latex-preview
;;; module into the mode.  The agenda and roam commands additionally need
;;; *ORGAN-FILES* (set below); without it they only say to configure it.
;;;
;;; Its Lisp dependencies (local-time, ironclad, clingon, fiveam,
;;; bordeaux-threads) come from the Quicklisp dist this Lem was built against
;;; (~/lem/.qlot) and were installed when organ-mode was, so none of them is
;;; fetched at startup.
;;;
;;; Compatibility shims for the latex-preview module.  It targets a Lem whose
;;; frontend interface reports image metrics and image support
;;; (LEM-IF:FONT-EM-PIXELS, LEM-IF:CELL-HEIGHT, LEM:IMAGE-SUPPORT-P); this
;;; build has none of the three, and a package-qualified symbol that does not
;;; exist is a reader error, so the file would not even load.  Each symbol is
;;; added only when missing and answers NIL, which is the answer the module
;;; already provides for: the preview commands then report that the frontend
;;; cannot draw images, and every other part of organ-mode is unaffected.  A
;;; future Lem that does have these interfaces keeps them untouched.  Lem
;;; locks its packages, hence the unlock/lock around the interning.

(defun ensure-interface-stub (package-designator symbol-name)
  "Export SYMBOL-NAME from PACKAGE-DESIGNATOR as a function returning NIL.
Leaves the symbol alone when it is already fbound, so a Lem that has the real
interface keeps it.  The package is unlocked for the interning and locked
again afterwards, which is what the build expects of its own packages."
  (let* ((package (find-package package-designator))
         (symbol (and package (find-symbol symbol-name package))))
    (cond
      ((null package) nil)
      ((and symbol (fboundp symbol)) symbol)
      (t
       #+sbcl (sb-ext:unlock-package package)
       (unwind-protect
            (let ((symbol (intern symbol-name package)))
              (export symbol package)
              (setf (symbol-function symbol)
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      nil))
              symbol)
         #+sbcl (sb-ext:lock-package package))))))

;;; Popup headers underline themselves in a darkened foreground colour, and
;;; this theme has no foreground to darken: lem-default's specs say
;;; (:foreground nil), the palette preferring :base00..:base0f, so
;;; (LEM-CORE:FOREGROUND-COLOR) answers NIL.  The darkening helper,
;;; LEM/MULTI-COLUMN-LIST::DARKEN-COLOR, hands that to RGB-TO-HSV, which signals
;;; "The value NIL is not of type LEM/COMMON/COLOR:COLOR" as soon as a popup
;;; with a column header is drawn.  C-x C-b is one: the buffer list died on its
;;; "Buffer File" header.  The wrapper falls back to the colour the frontend
;;; reports for foreground text, which is what the theme's missing entry would
;;; have named, and gives up on the underline (rather than erroring) when even
;;; that is absent.
;;;
;;; The function is patched rather than the theme because the theme is not
;;; ours; Lem locks its packages, hence the unlock/lock, and the saved original
;;; makes re-evaluating this file replace the wrapper instead of stacking a
;;; second one on it.

(defvar *unpatched-darken-color*
  (symbol-function 'lem/multi-column-list::darken-color)
  "DARKEN-COLOR as Lem defines it, saved before the wrapper below.")

#+sbcl (sb-ext:unlock-package :lem/multi-column-list)
(unwind-protect
     (setf (symbol-function 'lem/multi-column-list::darken-color)
           (lambda (color &key (factor 0.5))
             (let ((color (or color
                              (let ((foreground (lem-if:get-foreground-color
                                                 (implementation))))
                                (and foreground
                                     (lem/common/color:color-to-hex-string foreground))))))
               (when color
                 (funcall *unpatched-darken-color* color :factor factor)))))
  #+sbcl (sb-ext:lock-package :lem/multi-column-list))

;;; organ-mode and cltpt are checkouts in extensions/, each with its own
;;; repository; org-latex-preview is configuration, so it lives here.
(dolist (extension '("extensions/organ-mode/" "extensions/cltpt/" "org-latex-preview/"))
  (pushnew (merge-pathnames extension (lem-home))
           asdf:*central-registry* :test #'equal))

(handler-case
    (progn
      (dolist (spec '(("LEM-INTERFACE" "FONT-EM-PIXELS")
                      ("LEM-INTERFACE" "CELL-HEIGHT")
                      ("LEM" "IMAGE-SUPPORT-P")))
        (ensure-interface-stub (first spec) (second spec)))
      (asdf:load-system "organ-mode"))
  (error (condition)
    (format *error-output* "~&organ-mode unavailable: ~A~%" condition)))

;;; organ-extras.lisp adds what organ-mode leaves out: faces for emphasis, code
;;; spans, comments and math, and a Return that follows the link under point.
;;; Both are keyed to organ-mode's internals, so the file is read here, after
;;; the system is loaded and its packages exist, and a failure of its own does
;;; not take organ-mode down with it.
(handler-case
    (load (merge-pathnames "organ-extras.lisp" (lem-home)))
  (error (condition)
    (format *error-output* "~&organ-mode extras unavailable: ~A~%" condition)))

;;; org-latex-preview renders latex fragments in an organ buffer as the images
;;; latex typesets, the way org-mode's own preview does.  It is a system rather
;;; than a file because it adds text objects to cltpt, whose rules are rebuilt
;;; as one set, and it is part of this repository because it is configuration,
;;; not a checkout of someone else's work.  See its sources for what it
;;; changes: the webview frontend cannot draw images at all, so the frontend is
;;; patched at run time and the server side of the jsonrpc protocol is taught
;;; to send them.
(handler-case
    (asdf:load-system "org-latex-preview")
  (error (condition)
    (format *error-output* "~&org-latex-preview unavailable: ~A~%" condition)))

;;; What the agenda and roam commands scan.  The older Emacs org tree under
;;; ~/org is indexed, recursively because its notes sit in agenda/ and roam/.
;;; The variable is reached at run time for the same reason as the symbols
;;; above, and this is skipped when the system failed to load.
(let ((organ-files (find-symbol "*ORGAN-FILES*" :organ)))
  (when organ-files
    (setf (symbol-value organ-files)
          '((:path ("/home/edgar/org/")
             :glob "*.org"
             :recurse t
             :format "org-mode")))))

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
;;; directory.  The default semi-char input mode reserves C-c for terminal
;;; control (C-c C-c sends an interrupt) and leaves C-x to Lem, so C-x C-c
;;; quits; C-c M-d switches to char mode, where every key, including C-c
;;; and C-x, goes to the program.  M-[ enters copy mode and Escape returns
;;; to the live terminal.
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
             (setup (and package (find-symbol "SETUP" package)))
             (osc52 (and package
                         (find-symbol "GHOSTTY-TERMINAL-ENABLE-OSC52"
                                      package))))
        (if setup
            (progn
              (funcall setup :key "C-c t")
              ;; Let programs (tmux, neovim, a remote session) set the
              ;; clipboard with OSC 52.  The extension leaves this off by
              ;; default so that no program can silently overwrite it.
              (when osc52
                (setf (variable-value osc52 :global) t)))
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
