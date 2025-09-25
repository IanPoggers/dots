;;; General Emacs
(global-auto-revert-mode 1)
(smartparens-global-mode 1)

(setq! global-auto-revert-non-file-buffers t
       auto-revert-verbose nil)

(setq! ispell-dictionary "en_US")
;;;; PDF
(setq! pdf-view-resize-factor 1.05
       pdf-view-display-size 'fit-width
       pdf-view-continuous nil)

(after! pdf-view
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (visual-line-fill-column-mode -1)
              (visual-fill-column-mode -1)
              (add-to-list 'pdf-view-incompatible-modes '(visual-fill-column-mode visual-line-fill-column-mode)))))

;;;; Doom Stuff
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq! user-full-name "James Ian Pogue"
       user-mail-address "jamesipogue@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face

;;;; Fonts and Themes
(setq! doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 18))
(setq! doom-variable-pitch-font (font-spec :family "Atkinson Hyperlegible" :size 20))
;;(setq! doom-theme 'catppuccin)
;;(setq! catpuccin-flavor 'latte)
;;(load-theme 'catppuccin t t)
;;(catppuccin-reload)

(setq! doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Disable Title Bar
(add-to-list 'default-frame-alist '(undecorated . t))


;;;;; rapping and Mixed Pitch
(use-package! auto-dim-other-buffers)
(use-package! visual-fill-column
  :hook
  (org-mode . visual-fill-column-mode)
  (doom-docs-mode . (lambda () (visual-fill-column-mode -1))))


(global-visual-line-mode 1)

(setq! fill-column 80
       visual-fill-column-width 80)


;;(add-hook! 'visual-line-mode-hook
;;  visual-fill-column-mode)

(use-package! mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

(global-visual-fill-column-mode -1)
(setq! visual-fill-column-center-text t)
(setq! fill-column 80)

;;; Calendar https://github.com/kiwanami/emacs-calfw
(setq! cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap
       cfw:display-calendar-holidays nil)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Light Grey"))))  ; org-agenda source
                                        ;(cfw:howm-create-source "Blue")  ; howm source
                                        ;(cfw:cal-create-source "Orange") ; diary source
                                        ;(cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
                                        ;(cfw:ical-create-source "gcal"
                                        ;"https://bc.instructure.com/feeds/calendars/user_lAcbipQjOdEkynqm9tpSh6WgdZPIv89v8qusdQIF.ics"
                                        ;"Orange")))) ; google calendar ICS

;;(map! :leader
;;(:prefix ("o" . "open")
;;:desc "Calendar" "c" #'my-open-calendar)))))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Calendar" "c" #'+calendar/open-calendar))

;;; Org
(setq! org-directory "~/org/")
(setq! org-roam-directory "~/Sync/Roam/")

(after! org
  (setq
   org-startup-folded 'fold
   org-timeblock-span 7
   org-timeblock-show-future-repeats t
   org-hide-leading-stars nil)
  (set-face-attribute 'org-quote nil
                      :family "Crimson Pro"
                      :height 1.1)
  (add-hook 'org-mode-hook #'visual-fill-column-mode))

;;;; Journal
(after! org-journal
  (setq
   org-journal-dir "~/org/journal/"
   org-journal-file-type 'yearly))

;;;; zotero
(defun my-org-zotero-open (path _)
  (call-process "xdg-open" nil nil nil (concat "zotero:" path)))
(after! org
  (org-link-set-parameters "zotero" :follow #'my-org-zotero-open))

;;;; Visual
(setq! org-hide-leading-stars nil)
(setq! org-hide-leading-stars-before-indent-mode nil) ;; if you use indent mode

;;;;; Modern
(use-package! org-modern
  :after org)

(after! org-modern
  (setq org-modern-table t)
  (setq org-modern-table-vertical 2)
  (setq org-modern-block-fringe 8)
  (setq org-modern-block-name nil)
  (setq org-modern-keyword "‣")
  (global-org-modern-mode 1))

;;;; Timeblock
(use-package! org-timeblock)
(after! org-timeblock
  (add-to-list 'evil-emacs-state-modes 'org-timeblock-mode))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Timeblock" "t" #'org-timeblock))

;;;; Noter
(use-package! org-noter
  :after org)

(after! org-noter
  (setq org-noter-hide-other t
        org-noter-max-short-selected-text-length 200 ;; Determines how long until it substitutes with 'notes for..'
        org-noter-highlight-selected-text t
        org-noter-use-indirect-buffer t
        org-noter-always-create-frame nil
        org-noter-closest-tipping-point 0.2
        org-noter-doc-split-fraction '(0.6 . 0.4)))

(setq! org-noter-highlight-selected-text t) ; idk if this is needed but idc

;; zotxt and zotero integration
;;(use-package! zotxt
;;  :init
;;  (require 'org-zotxt-noter))
;;
;;(setq! zotxt-default-bibliography-style "chicago-notes-bibliography")

;;;; Agenda
(add-to-list 'display-buffer-alist
             '("\\*Org Agenda\\*"
               (display-buffer-in-side-window)

               (side . right)           ;; or 'left, 'bottom, 'top
               (window-width . 0.4)     ;; or use window-height for top/bottom
               (slot . 0)
               (window-parameters . ((no-delete-other-windows . t)))))


;; Hide some tags such as ATTATCH
(after! org-agenda
  (setq org-agenda-hide-tags-regexp (concat org-agenda-hide-tags-regexp "\\|ATTACH")
        org-agenda-files (append org-agenda-files '("~/org/"))
        org-agenda-follow-indirect nil ;; TODO What's the best value for this to not be confusing
        org-agenda-start-with-follow-mode nil))


;;;; ox-*
(use-package! ox-typst
  :after org)

(setq! org-pandoc-menu-entry
       (append '(
                 (?, "to typst." org-pandoc-export-to-typst)
                 (?, "to typst and open." org-pandoc-export-to-typst-and-open)
                 (?, "as typst." org-pandoc-export-as-typst)
                 (?< "to typst-pdf." org-pandoc-export-to-typst-pdf)
                 (?< "to typst-pdf and open." org-pandoc-export-to-typst-pdf-and-open))
               org-pandoc-menu-entry))
(plist-put org-format-latex-options :background "Transparent")




(after! ox-typst
  (setq org-typst-default-header "
#import \"@local/handout:0.0.1\": *
#show: template
#maketitle()

#let myquote(block: true, quotes: auto,  body) = {
        if body.has(\"children\") {
                let children = body.children.filter(it => it != [ ])
                        .map(it => {
                                if it.func() == list(list.item([])).func() {
                                        it.children.first()
                                } else {it}
                        })

                if children.last().func() == list.item([]).func() {
                        let attr = children.pop()

                        quote(attribution: attr.body,
                                 block: block,
                                 quotes: quotes,
                                 [].func()(children))
                } else {
                        quote(block: block, quotes: quotes, body)
                }
        } else {
                quote(block: block, quotes: quotes, body)
        }
}
#let quote = myquote
"
        org-typst-from-latex-environment #'org-typst-from-latex-with-naive
        org-typst-from-latex-fragment #'org-typst-from-latex-with-naive
        org-typst-heading-numbering 'none))

(use-package! ox-latex
  :after org
  :ensure t)

(after! ox-latex
  (add-to-list 'org-latex-classes
               '("tufte-handout"
                 "\\documentclass{tufte-handout}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;;; Bindings
(map! :after org
      :map org-mode-map
      :n "gj" #'evil-next-visual-line
      :n "gk" #'evil-previous-visual-line)

(setq! evil-snipe-scope 'whole-buffer)

;;; Evil
;;;; Bindings
