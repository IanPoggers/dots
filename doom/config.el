;;; config.el -*- lexical-binding: t; -*-

;;; General Emacs
(global-auto-revert-mode 1)
(smartparens-global-mode 1)

(setq! global-auto-revert-non-file-buffers t
       auto-revert-verbose nil)

(setq! ispell-dictionary "en_US")
;;;; Bindings
(map! :g  "<f5>" 'revert-buffer)

(after! which-key
  (setq which-key-idle-delay .18))

;;;; PDF
(setq! pdf-view-resize-factor 1.1
       pdf-view-display-size 'fit-width
       ;;pdf-view-use-scaling t
       pdf-view-continuous nil)

(after! pdf-view
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (visual-line-fill-column-mode -1)
              (visual-fill-column-mode -1)
              (pdf-view-auto-slice-minor-mode 1)
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
(setq! doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 16))
(setq! doom-variable-pitch-font (font-spec :family "Atkinson Hyperlegible" :size 17))
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

;; Define my own calendar function
;; I have decided to not use this funciton for now, but this could be changed in the future
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
(setq! org-directory "~/org/"
       org-roam-directory "~/Sync/Roam/"
       org-archive-location "~/org/archive/%s_archive::")

(after! org
  (setq
   org-startup-folded t ;; Apparently 'fold does not work for some reason
   org-hide-drawer-startup t
   org-cycle-hide-drawer-startup t
   org-cycle-hide-block-startup t
   org-hide-leading-stars nil)
  (set-face-attribute 'org-quote nil
                      :family "Crimson Pro"
                      :height 1.1)

  (add-hook 'org-mode-hook (lambda ()
                             (setq
                              org-startup-folded t
                              org-hide-drawer-startup t
                              org-cycle-hide-drawer-startup t
                              org-cycle-hide-block-startup t
                              org-hide-leading-stars nil)
                             (visual-fill-column-mode 1))))


;;;; latex preview

(after! org
  (plist-put org-format-latex-options :scale 0.54))


;;;; Timeblock
(after! org-timeblock
  (setq
   org-timeblock-span 7
   org-timeblock-show-future-repeats t))
;;;; Pomo
(after! org-pomodoro
  (setq
   org-pomodoro-start-sound-p t))
;;;; capture TODO
;;(after! org
(after! org
  (setq org-capture-templates
        (append org-capture-templates
                '(("c" "Templates for classes")))))
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
        org-noter-closest-tipping-point 0.4
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
        org-agenda-files (append org-agenda-files '("~/org/" "~/org/journal/"))
        org-agenda-follow-indirect nil ;; TODO What's the best value for this to not be confusing
        org-agenda-skip-scheduled-if-done t
        org-agenda-window-setup 'current-window
        org-agenda-window-frame-fractions '(0.5 . 0.8)
        org-agenda-skip-deadline-if-done t
        org-agenda-start-with-follow-mode nil)

  (set-popup-rule! "^\\*Org Agenda" :ignore t)
  (setq org-agenda-window-setup 'current-window))

(add-hook! 'org-agenda-mode-hook :append
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c"))))


;;;; export
;;;; ox-*
(use-package! ox-typst
  :after org)

(setq! org-pandoc-menu-entry
       '(
         ;;(?0 "to jats." org-pandoc-export-to-jats)
         ;;(?0 "to jats and open." org-pandoc-export-to-jats-and-open)
         ;;(?  "as jats." org-pandoc-export-as-jats)
         ;;(?1 "to epub2 and open." org-pandoc-export-to-epub2-and-open)
         ;;(?! "to epub2." org-pandoc-export-to-epub2)
         ;;(?2 "to tei." org-pandoc-export-to-tei)
         ;;(?2 "to tei and open." org-pandoc-export-to-tei-and-open)
         ;;(?" "as tei." org-pandoc-export-as-tei)
         ;;(?3 "to markdown_mmd." org-pandoc-export-to-markdown_mmd)
         (?3 "to markdown_mmd and open." org-pandoc-export-to-markdown_mmd-and-open)
         (?# "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
         (?4 "to html5." org-pandoc-export-to-html5)
         (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
         (?$ "as html5." org-pandoc-export-as-html5)
         (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
         (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
         ;;(?6 "to markdown_phpextra." org-pandoc-export-to-markdown_phpextra)
         ;;(?6 "to markdown_phpextra and open." org-pandoc-export-to-markdown_phpextra-and-open)
         ;;(?& "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
         ;;(?7 "to markdown_strict." org-pandoc-export-to-markdown_strict)
         ;;(?7 "to markdown_strict and open." org-pandoc-export-to-markdown_strict-and-open)
         ;;(?' "as markdown_strict." org-pandoc-export-as-markdown_strict)
         ;;(?8 "to opendocument." org-pandoc-export-to-opendocument)
         ;;(?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
         ;;(?( "as opendocument." org-pandoc-export-as-opendocument)
         ;;(?9 "to opml." org-pandoc-export-to-opml)
         ;;(?9 "to opml and open." org-pandoc-export-to-opml-and-open)
         ;;(?) "as opml." org-pandoc-export-as-opml)
         ;;(?: "to rst." org-pandoc-export-to-rst)
         ;;(?: "to rst and open." org-pandoc-export-to-rst-and-open)
         ;;(?* "as rst." org-pandoc-export-as-rst)
         ;;(?< "to slideous." org-pandoc-export-to-slideous)
         ;; (?\[ "to jira." org-pandoc-export-to-jira)
         ;; (?\[ "as jira." org-pandoc-export-as-jira)
         ;; (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
         ;; (?, "as slideous." org-pandoc-export-as-slideous)
         (?= "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
         (?- "to ms-pdf." org-pandoc-export-to-ms-pdf)
         ;;(?> "to textile." org-pandoc-export-to-textile)
         ;;(?> "to textile and open." org-pandoc-export-to-textile-and-open)
         ;;(?. "as textile." org-pandoc-export-as-textile)
         ;;(?a "to asciidoc." org-pandoc-export-to-asciidoc)
         ;;(?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
         ;;(?A "as asciidoc." org-pandoc-export-as-asciidoc)
         (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
         (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
         ;; (?c "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
         ;; (?C "to context-pdf." org-pandoc-export-to-context-pdf)
         ;;(?d "to docbook5." org-pandoc-export-to-docbook5)
         (?d "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
         (?D "as docbook5." org-pandoc-export-as-docbook5)
         ;; (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
         ;; (?E "to epub3." org-pandoc-export-to-epub3)
         ;;(?f "to fb2." org-pandoc-export-to-fb2)
         ;;(?f "to fb2 and open." org-pandoc-export-to-fb2-and-open)
         ;;(?F "as fb2." org-pandoc-export-as-fb2)
         ;;(?g "to gfm." org-pandoc-export-to-gfm)
         (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
         (?G "as gfm." org-pandoc-export-as-gfm)
         ;;(?h "to html4." org-pandoc-export-to-html4)
         (?h "to html4 and open." org-pandoc-export-to-html4-and-open)
         (?H "as html4." org-pandoc-export-as-html4)
         ;;(?i "to icml." org-pandoc-export-to-icml)
         ;; (?i "to icml and open." org-pandoc-export-to-icml-and-open)
         ;; (?I "as icml." org-pandoc-export-as-icml)
         ;;(?j "to json." org-pandoc-export-to-json)
         (?j "to json and open." org-pandoc-export-to-json-and-open)
         (?J "as json." org-pandoc-export-as-json)
         ;;(?k "to markdown." org-pandoc-export-to-markdown)
         ;;(?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
         ;;(?K "as markdown." org-pandoc-export-as-markdown)
         (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
         (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
         ;;(?m "to man." org-pandoc-export-to-man)
         (?m "to man and open." org-pandoc-export-to-man-and-open)
         (?M "as man." org-pandoc-export-as-man)
         ;;(?n "to native." org-pandoc-export-to-native)
         (?n "to native and open." org-pandoc-export-to-native-and-open)
         (?N "as native." org-pandoc-export-as-native)
         (?o "to odt and open." org-pandoc-export-to-odt-and-open)
         (?O "to odt." org-pandoc-export-to-odt)
         (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
         (?P "to pptx." org-pandoc-export-to-pptx)
         ;;(?q "to commonmark." org-pandoc-export-to-commonmark)
         ;;(?q "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
         ;;(?Q "as commonmark." org-pandoc-export-as-commonmark)
         ;;(?r "to rtf." org-pandoc-export-to-rtf)
         (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
         (?R "as rtf." org-pandoc-export-as-rtf)
         ;;(?s "to s5." org-pandoc-export-to-s5)
         ;;(?s "to s5 and open." org-pandoc-export-to-s5-and-open)
         ;;(?S "as s5." org-pandoc-export-as-s5)
         ;;(?t "to texinfo." org-pandoc-export-to-texinfo)
         ;;(?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
         ;;(?T "as texinfo." org-pandoc-export-as-texinfo)
	 ;; (?, "to typst." org-pandoc-export-to-typst)
         (?, "to typst and open." org-pandoc-export-to-typst-and-open)
         ;; (?, "as typst." org-pandoc-export-as-typst)
         ;; (?< "to typst-pdf." org-pandoc-export-to-typst-pdf)
         (?< "to typst-pdf and open." org-pandoc-export-to-typst-pdf-and-open)
         ;;(?u "to dokuwiki." org-pandoc-export-to-dokuwiki)
         ;; (?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
         ;; (?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
         ;;(?v "to revealjs." org-pandoc-export-to-revealjs)
         ;; (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
         ;; (?V "as revealjs." org-pandoc-export-as-revealjs)
         ;;(?w "to mediawiki." org-pandoc-export-to-mediawiki)
         ;; (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
         ;; (?W "as mediawiki." org-pandoc-export-as-mediawiki)
         (?x "to docx and open." org-pandoc-export-to-docx-and-open)
         (?X "to docx." org-pandoc-export-to-docx)
         ;;(?y "to slidy." org-pandoc-export-to-slidy)
         ;; (?y "to slidy and open." org-pandoc-export-to-slidy-and-open)
         ;; (?Y "as slidy." org-pandoc-export-as-slidy)
         ;;(?z "to dzslides." org-pandoc-export-to-dzslides)
         ;; (?z "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
         ;; (?Z "as dzslides." org-pandoc-export-as-dzslides)
         ;;(?{ "to muse." org-pandoc-export-to-muse)
         ;;(?{ "to muse and open." org-pandoc-export-to-muse-and-open)
         ;;(?[ "as muse." org-pandoc-export-as-muse)
         ;;(?} "to zimwiki." org-pandoc-export-to-zimwiki)
         ;;(?} "to zimwiki and open." org-pandoc-export-to-zimwiki-and-open)
         ;;(?] "as zimwiki." org-pandoc-export-as-zimwiki)
         ;;(?~ "to haddock." org-pandoc-export-to-haddock)
         ;;(?~ "to haddock and open." org-pandoc-export-to-haddock-and-open)
         ;;(?^ "as haddock." org-pandoc-export-as-haddock)
         ))
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
      :n "gk" #'evil-previous-visual-line
      :nv "zD" #'org-fold-hide-drawer-all
      :nv "zq" #'org-fold-hide-block-all)
(setq! evil-snipe-scope 'whole-buffer)

;;; Evil
;;;; Modes that evil is disabled
;;;; Bindings
(map! :map evil-window-map
      :g "O" #'delete-other-windows)

;;; Denote
;; Remember that the website version of this manual shows the latest
;; developments, which may not be available in the package you are
;; using.  Instead of copying from the web site, refer to the version
;; of the documentation that comes with your package.  Evaluate:
;;
;;     (info "(denote) Sample configuration")
(use-package denote
  :ensure t
  :hook
  ( ;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
         ("C-c n n" . denote)
         ("C-c n d" . denote-dired)
         ("C-c n g" . denote-grep)
         ;; If you intend to use Denote with a variety of file types, it is
         ;; easier to bind the link-related commands to the `global-map', as
         ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
         ;; `markdown-mode-map', and/or `text-mode-map'.
         ("C-c n l" . denote-link)
         ("C-c n L" . denote-add-links)
         ("C-c n b" . denote-backlinks)
         ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
         ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
         ;; Note that `denote-rename-file' can work from any context, not just
         ;; Dired bufffers.  That is why we bind it here to the `global-map'.
         ("C-c n r" . denote-rename-file)
         ("C-c n R" . denote-rename-file-using-front-matter)

         ;; Key bindings specifically for Dired.
         :map dired-mode-map
         ("C-c C-d C-i" . denote-dired-link-marked-notes)
         ("C-c C-d C-r" . denote-dired-rename-files)
         ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
         ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/doc/notes/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))
