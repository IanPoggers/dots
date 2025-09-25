;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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

(setq! debug-on-error t)

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 18))
(setq! doom-variable-pitch-font (font-spec :family "Atkinson Hyperlegible" :size 20))
;;(setq! doom-font (font-spec :family "Atkinson Hyperlegible" :size 16))

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
;;(setq! doom-theme 'catppuccin)
;;(setq! catpuccin-flavor 'latte)
;;(load-theme 'catppuccin t t)
;;(catppuccin-reload)

(setq! doom-theme 'doom-opera-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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


(setq! org-timeblock-span 7)
(setq! org-timeblock-show-future-repeats t)

(setq! org-roam-directory "~/Sync/Roam/")


;;(setq! evil-respect-visual-line-mode t)
(setq! visual-fill-column-center-text t)

(+global-word-wrap-mode 1)

(map! :after org
      :map org-mode-map
      :n "gj" #'evil-next-visual-line
      :n "gk" #'evil-previous-visual-line)

(setq! fill-column 90)
(setq! org-hide-leading-stars nil)

(add-hook! 'org-mode-hook (lambda()
                            (setq! visual-fill-column-center-text t)
                            (setq! fill-column 90)
                            (setq! org-hide-leading-stars nil)))




(after! org-timeblock
  (add-to-list 'evil-emacs-state-modes 'org-timeblock-mode))

;; (use-package! org-typst-preview)

;;(load! "~/Downloads/org-typst-preview.el")
(use-package! org-timeblock)


(use-package! org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :init
  (setq org-recur-finish-done t
        org-recur-finish-archive t)
  :bind (:map org-recur-mode-map
         ("C-c d" . org-recur-finish)
         :map org-recur-agenda-mode-map
         ("d" . org-recur-finish)
         ("C-c d" . org-recur-finish)))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Timeblock" "t" #'org-timeblock))


(use-package! org-download)

(use-package! mixed-pitch)

(use-package! org-noter
  :init
  (setq org-noter-hide-other nil
        org-noter-doc-split-fraction '(0.65 . 0.35))
  :ensure t
  :after org)

(setq! org-noter-highlight-selected-text t)


(add-to-list 'display-buffer-alist
             '("\\*Org Agenda\\*"
               (display-buffer-in-side-window)

               (side . right)           ;; or 'left, 'bottom, 'top
               (window-width . 0.4)     ;; or use window-height for top/bottom
               (slot . 0)
               (window-parameters . ((no-delete-other-windows . t)))))

(use-package! howm)

(use-package! ox-typst
  :after org
  :ensure t)

(use-package! ox-latex
  :after org
  :ensure t)


;;(setq! pop-up-frames t)
(setq! nov-text-width 85)


(use-package! org-remark
  :after org
  :ensure t)
(with-eval-after-load 'nov
  (org-remark-nov-mode +1))


(global-auto-revert-mode 1)
(setq! global-auto-revert-non-file-buffers t
       auto-revert-verbose nil)

(use-package! mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))


(smartparens-global-mode nil)

(after! eww
  (org-remark-eww-mode +1))

(after! nov
  (org-remark-nov-mode +1))

(after! info
  (org-remark-info-mode +1))

(setq! shr-max-width 90)

(setq! pdf-view-resize-factor 1.10)


(use-package! auto-dim-other-buffers)

(setq! evil-snipe-scope 'whole-buffer)

(after! pdf-view
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (visual-line-fill-column-mode -1)
              (visual-fill-column-mode -1))))

(after! pdf-view
  :hook
  (add-to-list 'pdf-view-incompatible-modes '(visual-fill-column-mode visual-line-fill-column-mode)))

(setq! pdf-view-display-size 'fit-width)


(setq! org-hide-leading-stars nil)
(setq! org-hide-leading-stars-before-indent-mode nil) ;; if you use indent mode

(use-package! org-modern
  :ensure t)

(after! org-modern
  (setq org-modern-table t)
  (setq org-modern-table-vertical 2)
  (setq org-modern-block-fringe 8)
  (setq org-modern-block-name nil)
  (setq org-modern-keyword "‣"))

(after! org
                                        ;(setq! org-hide-leading-stars-before-indent-mode nil) ;; if you use indent mode
                                        ;(setq org-hide-leading-stars nil)
  (global-org-modern-mode 1))

(after! ispell
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US"))

(map! :nv "G" (lambda (count)           ; Rebinds "G" to "Gz-"
                (interactive "P")
                (evil-goto-line count)
                (evil-scroll-line-to-bottom-first-non-blank nil)))
