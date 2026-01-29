;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load (expand-file-name "libs.el" doom-user-dir))


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; corfu
(use-package! corfu
  :config
  ;; M-TAB to move to minibuffer
  (map! :map corfu-map  "M-<TAB>" #'+corfu/move-to-minibuffer)
  (map! "M-<TAB>" #'+corfu/move-to-minibuffer))

;;; vertico
(use-package! vertico
  :config
  (map! :map 'embark-org-heading-map "n" 'org-tree-to-indirect-buffer)
  (map! :map 'vertico-map "M-TAB" 'embark-act))
;;; god i fucking hate smartparens
(smartparens-global-mode -1)
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
;;; org
(use-package! org
  :init
  ;; NOTE By default, TAB does not cycle the subtree, just the current one. Why, doom, why...
  (setq org-directory "~/Dropbox/org")

  (setq org-cycle-max-level 2)


  (setq org-highlight-latex-and-related '(latex))

  (setq org-use-fast-todo-selection nil)
  (setq org-agenda-files (mapcar (lambda (str) (f-join org-directory str))
                                 '("habit.org" "todo.org" "Inbox.org")))
  :config
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  (setq org-cycle-emulate-tab t)

  (setq org-link-file-path-type 'relative)

  (setq org-highlight-latex-and-related '(latex))

  (setq
   org-cycle-emulate-tab nil

   org-cycle-inline-images-display t
   org-startup-with-inline-images t
   org-image-max-width 480
   org-image-align 'left

   org-startup-indented t))

;;;; bindings
(map! :mode org-mode
      :nv "]h" #'org-forward-heading-same-level
      :nv "[h" #'org-backward-heading-same-level)


;;;; org-attach
(after! org-attach
  (setq
   org-attach-dir-relative t
   org-attach-directory (f-join org-directory "data/")
   org-attach-store-link-p 'file))
;;;; org-download
(use-package! org-download
  :init
  (setq-default org-download-image-dir "./images")
  (map! :map org-mode-map
        :localleader
        :desc "Rename image at point" "a C" #'org-download-rename-at-point
        :desc "Download file from clipboard" "a p" #'org-download-clipboard)
  :config
  (setq org-download-method 'directory
        org-download-image-latex-width 0
        org-download-timestamp "%Y-%m-%d-%H%M%S"
        org-download-annotate-function (lambda (&rest _) "")
        org-download-screenshot-basename "sc.png"
        org-download-link-format "[[file:%s]]\n"
        org-download-image-attr-list nil
        org-download-heading-lvl nil))
;;;; cdlatex
(after! org
  (add-hook 'org-tab-first-hook #'org-try-cdlatex-tab))

;;;; org-latex-preview
(map! :mode org
      :leader "cl" (λ! (org-latex-preview (or current-prefix-arg '(16)))))
;;;; This is necssary cuz doom is gay
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  (setq org-cycle-emulate-tab nil))
;;;; Bind to quickly traverse up headings
(after! org
  (map! :mode org-mode
      :nv "zU" (lambda ()
                 (interactive)
                 (evil-org-top))
      :nv "U" (lambda (arg)
                (interactive "pGo up bitch")
                (org-up-heading-all arg))))

;;;; org bindings idk
(map! :mode org-mode
      :nv "zv" #'org-reveal)

;;;; focus.el in org-mode
(use-package! focus
  :config
  (mapcar (lambda (mode) (add-hook mode 'focus-mode))
            '(org-mode-hook emacs-lisp-mode-hook LaTeX-mode-hook)))
;;;; org-agenda
;;;;; keywords
(after! org
  (setq org-todo-keywords '((type "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (type "KILL(k)"))))

;;;;; bindings
(map! :map 'evil-org-agenda-mode-map
      :mnv "j" #'org-agenda-next-item
      :mnv "k" #'org-agenda-previous-item)
;;; Aesthetic Stuff
;;;; Mixed-pitch
(use-package! mixed-pitch
  :config
  (add-hook 'mixed-pitch-mode 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'mixed-pitch-mode)
  (pushnew! mixed-pitch-fixed-pitch-faces
              'org-date 'font-lock-comment-face
              'org-list-dt))
;;; custom functions
;;;; zd or z-d hides drawers
(map! :mode org-mode
      :nv "zd" #'org-fold-hide-drawer-all)

(advice-add 'org-tree-to-indirect-buffer :after
            (lambda (&rest _)
              (org-fold-hide-drawer-all)))

;;;; custom function to clean org dir
(defun my/clean-notes ()
  (interactive)
  (dolist
      (f
       (directory-files org-directory t
                        "^.*\\.\\(png\\|pdf\\|tex\\|aux\\|bbl\\|log\\|out\\|fls\\|fdb_latexmk\\|synctex\\.gz\\|organice-bak\\)$"))
    (delete-file f)))

(map! :map doom-leader-notes-map
      "w" #'org-save-all-org-buffers
      "x" #'my/clean-notes)

;;; evil
(use-package! evil
  :init
  ;; NOTE annoyingly it's not default to have C-h in insert mode as help. No idea why
  (define-key! :keymaps 'evil-insert-state-map (kbd "C-h") 'help-command)
  (map! :map 'override :i "C-h" 'help-command)
  :config
  (add-hook 'evil-mode-hook
            (lambda () (setq-local evil-scroll-count 18) ))

  (cl-pushnew 'org-agenda-mode evil-motion-state-modes))
