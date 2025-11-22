;;; config.el -*- lexical-binding: t; eval: (+fold/close-all) -*-

;;; General Emacs
(global-auto-revert-mode 1)
(smartparens-global-mode -1)
(setq-default search-invisible nil)
(after! smartparens (setq smartparens-global-mode nil))


;; Make initial major mode org-mode (for scratch buffer)
(setq! doom-scratch-initial-major-mode 'org-mode)

(setq! global-auto-revert-non-file-buffers t
       auto-revert-verbose nil)

(setq! ispell-dictionary "en_US")
(add-to-list 'load-path (expand-file-name "lisp/" doom-user-dir))
(map!
 :nv "gk" #'outline-backward-same-level
 :nv "zu" #'outline-up-heading
 :nv "gj" #'outline-forward-same-level)
;;;; lookup provider
(add-to-list '+lookup-provider-url-alist '("Goodreads" "https://www.goodreads.com/search?q=%s"))
(add-to-list '+lookup-provider-url-alist '("Story Graph" "https://app.thestorygraph.com/browse?search_term=%s"))
(add-to-list '+lookup-provider-url-alist '("Anna's Archive" "https://annas-archive.org/search?q=%s"))
;;;; Relative line numbers
(after! display-line-numbers
  (setq display-line-numbers-type 'visual))
(setq! display-line-numbers-type 'visual)
;;;; Text mode line spacing
(add-hook! 'text-mode-hook
  (lambda ()
    (setq-local line-spacing 3)))
;;;; Bindings
(map! :g  "<f5>" 'revert-buffer)

(after! which-key
  (setq which-key-idle-delay .28))

;;;; PDF
(setq! pdf-view-resize-factor 1.1
       pdf-view-display-size 'fit-page
       ;;pdf-view-use-scaling t
       pdf-view-continuous nil)

(after! pdf-view
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (pdf-view-auto-slice-minor-mode 1)
              (add-to-list 'pdf-view-incompatible-modes '(visual-fill-column-mode visual-line-fill-column-mode)))))

;;;; Doom Stuff
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(defun my-weebery-is-always-greater ()
  (let* ((banner '("______ _____ ____ ___ ___"
                   "`  _  V  _  V  _ \\|  V  ´"
                   "| | | | | | | | | |     |"
                   "| | | | | | | | | | . . |"
                   "| |/ / \\ \\| | |/ /\\ |V| |"
                   "|   /   \\__/ \\__/  \\| | |"
                   "|  /                ' | |"
                   "| /     E M A C S     \\ |"
                   "´´                     ``"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'my-weebery-is-always-greater)
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
(setq! doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 17))
(setq! doom-variable-pitch-font (font-spec :family "Atkinson Hyperlegible" :size 20))
;;(setq! doom-theme 'catppuccin)
;;(setq! catpuccin-flavor 'latte)
;;(load-theme 'catppuccin t t)
;;(catppuccin-reload)

(setq! doom-theme 'doom-nord)

(after! org
  (after! org-indent
    (set-face-attribute 'org-block nil            :foreground nil :inherit 'fixed-pitch :height 0.85)
    (set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
    (set-face-attribute 'org-indent nil           :inherit '(org-hide fixed-pitch) :height 0.85)
    (set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
    (set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;; Disable Title Bar
(add-to-list 'default-frame-alist '(undecorated . t))


;;;;; wrapping and Mixed Pitch
(use-package! auto-dim-other-buffers)


(global-visual-line-mode 1)
(global-visual-wrap-prefix-mode 1)

(setq! fill-column 80)


;;(add-hook! 'visual-line-mode-hook
;;  visual-fill-column-mode)

(use-package! mixed-pitch
  :hook (text-mode . mixed-pitch-mode))

(setq! fill-column 80)

;;;; whisper.el
(use-package! whisper
  :bind ("C-H-r" . whisper-run)
  :config
  (setq whisper-model "medium"
        whisper-language "en"
        whisper-translate nil))

;;; calendar [calfw](https://github.com/kiwanami/emacs-calfw)
(setq! cfw:render-line-breaker 'cfw:render-line-breaker-none
       cfw:org-agenda-schedule-args '(:deadline :scheduled*)
       cfw:org-overwrite-default-keybinding t
       cfw:display-calendar-holidays nil)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :view 'day
   :contents-sources
   (list
    (cfw:org-create-source "light blue")  ; org-agenda source
    (cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/jamesipogue%40gmail.com/private-405fed4d44887c1fb303de8d0e6c7a0d/basic.ics" "IndianRed") ; google calendar ICS
    (cfw:ical-create-source "canvas" "https://bc.instructure.com/feeds/calendars/user_lAcbipQjOdEkynqm9tpSh6WgdZPIv89v8qusdQIF.ics" "Orange")))) ; google calendar ICS

(defun my-minimal-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :view 'day
   :contents-sources
   (list
                                        ;(cfw:org-create-source "light blue")  ; org-agenda source
    (cfw:ical-create-source "canvas" "https://bc.instructure.com/feeds/calendars/user_lAcbipQjOdEkynqm9tpSh6WgdZPIv89v8qusdQIF.ics" "Orange")
    (cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/jamesipogue%40gmail.com/private-405fed4d44887c1fb303de8d0e6c7a0d/basic.ics" "IndianRed")))
  (cfw:change-view-two-weeks)) ; google calendar ICS

(map! :leader
      (:prefix ("o" . "open")
       :desc "Calendar" "v" #'my-open-calendar))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Calendar" "c" #'my-minimal-calendar))
;; Add an org-capture template
(setq! cfw:org-capture-template
       '("c" "calfw2org" entry (file "cal.org")  "* %?
 %(cfw:org-capture-day)"))

;;; Org
(setq! org-directory "~/org/"
       org-roam-directory "~/doc/roam/"
       org-archive-location "~/org/.archive/%s_archive::")

;; Disable org-element-use-cache in an indirect buffer cuz it seems to break things.
(add-hook! 'clone-indirect-buffer (lambda ()
                                    (setq-local org-element-use-cache nil)))

(after! org
  (setq
   ;;org-indirect-buffer-display 'new-frame ; TODO should this be set to something else?
   org-display-internal-link-with-indirect-buffer t
   org-link-use-indirect-buffer-for-internals t ;; why are these two separate variables??
   org-export-with-tags nil
   org-element-use-cache nil ; TODO how to get this to only apply in indirect buffers
   org-export-with-todo-keywords nil
   org-export-initial-scope 'subtree
   org-pretty-entities t
   org-custom-properties '("AUTHOR")
   org-pretty-entities-include-sub-superscripts t
   org-todo-keywords '((sequence "TODO(t)"  "NEXT(n)" "WAIT(w)" "SOMEDAY(S)" "|" "DONE(d)" "KILL(k)")
                       (sequence "TOREAD(r)" "READING(g)" "|" "READ(R)")
                       (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](x)"))
   org-use-sub-superscripts t
   org-export-with-section-numbers nil
   org-export-with-toc nil
   org-startup-folded 'fold ;; Apparently 'fold does not work for some reason
   org-hide-drawer-startup t
   org-image-align 'center
   org-startup-with-latex-preview t
   org-startup-with-inline-images nil
   org-indent-indentation-per-level 0 ;; disable org-indent
   org-cycle-hide-drawer-startup t
   org-cycle-inline-images-display t
   org-image-max-width 440 ; TODO what's the best value for this??
   org-cycle-hide-block-startup t
   org-hide-leading-stars t)
  (set-face-attribute 'org-quote nil
                      :family "Crimson Pro"
                      :height 1.1)
  (set-face-attribute 'org-drawer nil
                      :family "Iosevka Nerd Font Mono"
                      :height 0.95)
  (set-face-attribute 'org-property-value nil
                      :family "Iosevka Nerd Font Mono"
                      :height 0.95)

  (add-hook 'org-mode-hook (lambda ()
                             (setq
                              org-hide-drawer-startup t
                              org-cycle-hide-drawer-startup t
                              display-line-numbers-type 'visual
                              org-cycle-hide-block-startup t
                              org-hide-leading-stars t)
                             (org-num-mode -1)
                             (cdlatex-mode -1)

                             (dolist (face '((org-level-1 . 1.30)
                                             (org-level-2 . 1.25)
                                             (org-level-3 . 1.2)
                                             (org-level-4 . 1.1)
                                             (org-level-5 . 1.1)
                                             (org-level-6 . 1.1)
                                             (org-level-7 . 1.1)
                                             (org-level-8 . 1.1)))
                               (set-face-attribute (car face) nil :font "Atkinson Hyperlegible" :weight 'semibold :height (cdr face)))

                             ;; Make the document title a bit bigger
                             (set-face-attribute 'org-document-title nil :font "Atkinson Hyperlegible" :weight
                                                 'semibold :height 1.8)

                             (ultra-scroll-mode -1)
                             (org-modern-mode -1)
                             (org-superstar-mode 1)
                             (org-latex-preview-center-mode 1)
                             (visual-line-mode 1)
                             ;;(visual-fill-column-mode 1)
                             (org-cdlatex-mode -1))))
;; Resize Org headings


(map! :leader "n/" #'consult-org-agenda)

;;;; org-cdlatex
(after! org-cdlatex
  (setq cdlatex-takeover-subsuperscript t
        cdlatex-simplify-sub-super-scripts t))
;;;; org-superstar
(use-package! org-superstar
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("DONE" . 9744)
                                          ("NEXT" . 9744)
                                          ("WAIT" . 9744)
                                          ("SOMEDAY" . 9744)
                                          ("READING" . 9744)
                                          ("TOREAD" . 9744)
                                          ("WAIT" . 9744))))


;;;; org-alert
(use-package! org-alert
  :config
  (setq org-alert-interval 300
        org-alert-notify-cutoff 10
        org-alert-notify-after-event-cutoff 10)
  (org-alert-enable))


;;;; org-download
(use-package! org-download
  :defer t
  :init
  (setq-default org-download-image-dir "images")
  (map! :map org-mode-map
        :localleader
        :desc "Rename image at point" "a C" #'org-download-rename-at-point)
  :config
  (setq org-download-method 'directory
        org-download-image-latex-width 0
        org-download-link-format "[[file:images/%s]]\n"
        org-download-image-attr-list '("#+attr_latex :width 0.65\\linewidth")
        org-download-heading-lvl nil))
;;;; org-latex
(after! org
  (setq org-latex-image-default-width "\\linewidth"
        org-latex-default-figure-position "H"
        org-latex-precompile nil
        org-latex-default-class "article")

  (dolist (pkg '("amsmath" "amssymb" "amsthm" "mathtools" "mathrsfs" "pgfplots" "float"))
    (add-to-list 'org-latex-packages-alist `("" ,pkg t))))
;;;; org-modern
(after! org-modern
  (add-hook! 'org-agenda-finalize-hook #'org-modern-agenda)
  (setq!
   org-modern-checkbox nil
   org-modern-keyword nil))
;;;; latex preview
;; code for centering LaTeX previews -- a terrible idea
;; TODO enable latex previews in org-roam and latex buffers (use xenops)

;; TODO enable latex previews in org-roam and latex buffers (use xenops)

;; modified from https://abode.karthinks.com/org-latex-preview/

;; TODO enable latex previews in org-roam and latex buffers (use xenops)

;; modified from https://abode.karthinks.com/org-latex-preview/
(use-package! org-latex-preview
  :after org
  :hook (org-mode . org-latex-preview-mode)
  :init
  (setq org-startup-with-latex-preview t)



  :config
  (plist-put org-latex-preview-appearance-options
             ;; :page-width 0.8)
             :page-width 0.8)
  (plist-put org-latex-preview-appearance-options
             :scale .5)
  (plist-put org-latex-preview-appearance-options
             :zoom 1.1)

  (setq org-latex-preview-preamble "\\documentclass{article}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\usepackage{xcolor}\\newtheorem{theorem}{Theorem}")

  (setq         org-latex-preview-numbered t
                org-latex-preview-process-precompile nil
                org-latex-preview-mode-display-live t
                org-latex-preview-process-alist '((dvipng :programs ("latex" "dvipng") :description "dvi > png" :message
                                                   "you need to install the programs: latex and dvipng." :image-input-type
                                                   "dvi" :image-output-type "png" :latex-compiler
                                                   ("%l -interaction nonstopmode -output-directory %o %f")
                                                   :latex-precompiler
                                                   ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
                                                   :image-converter
                                                   ("dvipng --follow -D %D -T tight --depth --height -o %B-%%09d.png %f")
                                                   :transparent-image-converter
                                                   ("dvipng --follow -D %D -T tight -bg Transparent --depth --height -o %B-%%09d.png %f"))
                                                  (dvisvgm :programs ("latex" "dvisvgm") :description "dvi > svg" :message
                                                           "you need to install the programs: latex and dvisvgm."
                                                           :image-input-type "dvi" :image-output-type "svg" :latex-compiler
                                                           ("%l -interaction nonstopmode -output-directory %o %f")
                                                           :latex-precompiler
                                                           ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
                                                           :image-converter
                                                           ("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts -v3 --message='processing page {?pageno}: output written to {?svgpath}' --bbox=min -o %B-%%9p.svg %f"))
                                                  (imagemagick :programs ("pdflatex" "convert") :description "pdf > png" :message
                                                               "you need to install the programs: latex and imagemagick."
                                                               :image-input-type "pdf" :image-output-type "png" :latex-compiler
                                                               ("pdflatex -interaction nonstopmode -output-directory %o %f")
                                                               :latex-precompiler
                                                               ("pdftex -output-directory %o -ini -jobname=%b \"&pdflatex\" mylatexformat.ltx %f")
                                                               :image-converter
                                                               ("convert -density %D -trim -antialias %f -quality 100 %B-%%09d.png")))
                org-latex-preview-mode-update-delay 0.20)
  ;; org-latex-preview-cache 'temp) ;; HACK fix `org-persist' issue: https://discord.com/channels/406534637242810369/1056621127188881439/1392466785168785540

  ;; code for centering LaTeX previews -- a terrible idea
  (defun my/org-latex-preview-uncenter (ov)
    (overlay-put ov 'before-string nil))
  (defun my/org-latex-preview-recenter (ov)
    (overlay-put ov 'before-string (overlay-get ov 'justify)))
  (defun my/org-latex-preview-center (ov)
    (save-excursion
      (goto-char (overlay-start ov))
      (when-let* ((elem (org-element-context))
                  ((or (eq (org-element-type elem) 'latex-environment)
                       (string-match-p "^\\\\\\[" (org-element-property :value elem))))
                  (img (overlay-get ov 'display))
                  (prop `(space :align-to (- center (0.55 . ,img))))
                  (justify (propertize " " 'display prop 'face 'default)))
        (overlay-put ov 'justify justify)
        (overlay-put ov 'before-string (overlay-get ov 'justify)))))
  (define-minor-mode org-latex-preview-center-mode
    "Center equations previewed with `org-latex-preview'."
    :global nil
    (if org-latex-preview-center-mode
        (progn
          (add-hook 'org-latex-preview-overlay-open-functions
                    #'my/org-latex-preview-uncenter nil :local)
          (add-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter nil :local)
          (add-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center nil :local))
      (remove-hook 'org-latex-preview-overlay-close-functions
                   #'my/org-latex-preview-recenter)
      (remove-hook 'org-latex-preview-overlay-update-functions
                   #'my/org-latex-preview-center)
      (remove-hook 'org-latex-preview-overlay-open-functions
                   #'my/org-latex-preview-uncenter))))

(after! org-latex-preview-mode
  (setq org-latex-preview-mode-hook
        (lambda () (org-latex-preview-center-mode 1))))


;; TODO No need after using new org version??????
;;(after! org
;;(plist-put org-format-latex-options :scale .60))
;; TODO enable latex previews in org-roam and latex buffers (use xenops)

;; modified from https://abode.karthinks.com/org-latex-preview/

;;;; Timeblock
(after! org-timeblock
  (setq
   org-timeblock-span 7
   org-timeblock-show-future-repeats t))
;;;; org-pomodoro and org-clock
(after! org-pomodoro
  (setq
   org-clock-sound "~/.config/doom/assets/bell.wav"
   org-pomodoro-start-sound "~/.config/doom/assets/bell.wav"
   org-pomodoro-clock-break t
   org-pomodoro-keep-killed-pomodoro-time t
   org-pomodoro-short-break-length 10
   org-pomodoro-manual-break t
   org-pomodoro-length 30
   org-pomodoro-start-sound-p t))
(after! org-clock
  (setq org-clock-sound "~/.config/doom/assets/bell.wav"))

(map! :leader
      "tT" #'org-timer-set-timer)
;;;; org-capture TODO
(after! org
  (setq org-capture-templates
        '(("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n" :prepend t)
          ("h" "Personal habit" entry (file+headline +org-capture-todo-file "Habits")
           "* TODO %?\n:PROPERTIES:\n:STYLE: habit\n:END:\n%i\n" :prepend t)
          ("T" "Linked todo" entry (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox")
           "* %?\n%i\n" :prepend t)
                                        ;("p" "Templates for projects")
                                        ;("pt" "Project-local todo" entry
                                        ;(file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n%i\n%a" :prepend
                                        ;t)
                                        ;("pn" "Project-local notes" entry
                                        ;(file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n%i\n%a" :prepend
                                        ;t)
                                        ;("pc" "Project-local changelog" entry
                                        ;(file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?\n%i\n%a"
                                        ;:prepend t)
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry #'+org-capture-central-project-todo-file
           "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry #'+org-capture-central-project-notes-file
           "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file
           "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
          ("c" "Calc Inbox" entry (file+headline "Calc2.org" "Inbox")
           "* %?\n%i\n" :prepend t)
          ("C" "Calc Todo" entry (file+headline "Calc2.org" "Inbox")
           "* TODO %?\n%i\n" :prepend t)
          ("p" "Physics Inbox" entry (file+headline "Phys3.org" "Inbox")
           "* %?\n%i\n" :prepend t)
          ("P" "Physics Inbox" entry (file+headline "Phys3.org" "Inbox")
           "* TODO %?\n%i\n" :prepend t)
          ("r" "Reading Inbox" entry (file+headline "reading.org" "Inbox")
           "* TOREAD %?\n%^{AUTHOR}p\n%i\n" :prepend t)
          )))

;;;; Journal
(after! org-journal
  (setq
   org-journal-file-format "%Y.org"
   org-journal-time-format "%R "
   org-journal-dir "~/org/journal/"
   org-journal-file-type 'yearly))

(map! :leader "njo" (lambda () (interactive) (org-journal-open-current-journal-file) (read-only-mode))
      :leader "j" (lambda () (interactive) (org-journal-open-current-journal-file) (read-only-mode)))
;;;; zotero
(defun my-org-zotero-open (path _)
  (call-process "xdg-open" nil nil nil (concat "zotero:" path)))
(after! org
  (org-link-set-parameters "zotero" :follow #'my-org-zotero-open))

;;;; Timeblock
(use-package! org-timeblock)
(after! org-timeblock
  (add-to-list 'evil-emacs-state-modes 'org-timeblock-mode))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Timeblock" "t" #'org-timeblock))

;;;; Noter
(use-package! org-noter
  :after org
  :config
  (setq org-noter-hide-other t
        org-noter-max-short-selected-text-length 300 ;; Determines how long until it substitutes with 'notes for..'
        org-noter-highlight-selected-text t
        org-noter-use-indirect-buffer nil
        org-noter-always-create-frame nil
        org-noter-closest-tipping-point 0.4
        org-noter-doc-split-fraction '(0.6 . 0.4)))


;; zotxt and zotero integration
;;(use-package! zotxt
;;  :init
;;  (require 'org-zotxt-noter))
;;
;;(setq! zotxt-default-bibliography-style "chicago-notes-bibliography")

;;;; org-agenda https://github.com/alphapapa/org-super-agenda
;;(add-to-list 'display-buffer-alist
;;'("\\*Org Agenda\\*"
;;(side . right)           ;; or 'left, 'bottom, 'top
;;(window-width . 0.7)     ;; or use window-height for top/bottom
;;(slot . 0)
;;(window-parameters . ((no-delete-other-windows . t)))))

(map! :map 'org-agenda-mode-map
      :e "'" #'org-agenda
      :e "C-t" #'org-pomodoro)

(map! :nv "G"
      (lambda ()
        (interactive)
        (evil-goto-line)
        (evil-scroll-line-to-bottom-first-non-blank nil)))

;; Hide some tags such as ATTATCH
(evil-set-initial-state 'org-agenda-mode 'emacs)
(add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
(after! org-agenda
  (after! evil (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
    (evil-set-initial-state 'org-agenda-mode 'emacs))
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (custom-set-faces!
    '(org-agenda-date-today :foreground "goldenrod3" :inverse t :height 1.1 :underline t :box t)
    '(org-agenda-date  :foreground "white" :box t)
    '(org-agenda-date-weekend :foreground "grey68" :box t))
  (setq! org-agenda-hide-tags-regexp nil
         org-agenda-remove-tags t ;; TODO until i fix tag alignment, this will be t
         org-agenda-files (append org-agenda-files '("~/org/" "~/org/journal/"))
         org-agenda-follow-indirect nil ;; TODO What's the best value for this to not be confusing
         org-agenda-skip-scheduled-if-done t
         org-habit-show-all-today nil
         org-habit-show-done-always-green t
         org-agenda-skip-scheduled-delay-if-deadline t
         org-agenda-skip-scheduled-if-deadline-is-shown t
         org-agenda-todo-ignore-scheduled t
         org-habit-following-days 4
         org-agenda-todo-ignore-deadlines t
         org-agenda-skip-deadline-prewarning-if-scheduled nil
         org-agenda-span 7
         org-agenda-sorting-strategy
         '((agenda time-up deadline-up priority-down)
           (todo urgency-down ) (tags urgency-down ))
         org-priority-default  67
         org-priority-lowest 68
         org-fold-show-context-detail '((agenda . tree)
                                        (org-goto . ancestors)
                                        (bookmark-jump . lineage)
                                        (isearch . lineage)
                                        (default . ancestors))
         org-priority-start-cycle-with-default nil
         org-agenda-start-on-weekday 1
         org-agenda-start-day nil
         org-agenda-window-setup 'only-window
         org-agenda-restore-windows-after-quit t
         org-deadline-warning-days 7 ;; TODO Change once i stop using so many deadlines
         org-agenda-todo-list-sublevels nil
         org-agenda-window-frame-fractions '(0.5 . 0.8)
         org-agenda-skip-deadline-if-done t
         org-startup-shrink-all-tables t
         org-super-agenda-groups nil
         org-agenda-start-with-follow-mode nil)

  (setq
   org-agenda-custom-commands
   '(("w" "Weekly Super Agenda" agenda ""
      ((org-agenda-time-grid '((daily require-timed remove-match) (800 1000 1200 1400 1600 1800) "......" "----------------"))
       (org-deadline-warning-days 0)
       (org-scheduled-past-days 0)
       (org-habit-show-habits nil)
       (org-super-agenda-groups
        '((:name "" :time-grid t)
          (:name "Class" :tag "class")
          (:name "Habits" :habit t)))))
     ("W" "Weekly Super Agenda" agenda ""
      ((org-agenda-window-setup 'current-window)
       (org-deadline-warning-days 0)
       (org-scheduled-past-days 0)
       (org-agenda-time-grid '((daily require-timed remove-match) (800 1000 1200 1400 1600 1800) "......" "----------------"))
       (org-super-agenda-groups
        '((:name "" :time-grid t)
          (:name "Class" :tag "class")
          (:name "Habits" :habit t)))))
     ("h" "Habits" agenda ""
      ((org-agenda-span 1)
       (org-habit-show-habits t)
       (org-super-agenda-groups
        '((:discard (:not (:habit t)))
          (:habit t)))))
     ("d" "Daily Super Agenda"
      ((agenda ""
               ((org-agenda-skip-scheduled-if-deadline-is-shown t)
                (org-agenda-span 1)
                (org-deadline-warning-days 7)
                (org-agenda-time-grid '((today require-timed remove-match) (800 1000 1200 1400 1600 1800) "......" "----------------"))
                (org-super-agenda-groups
                 '((:name "" :time-grid t)
                   (:name "" :and (:deadline nil :scheduled nil))
                   (:name "Class Today"
                    :and (:tag "class" :deadline past :not (:habit t))
                    :and (:tag "class" :date today :not (:habit t))
                    :and (:tag "class" :scheduled past :not (:deadline future :habit t)))
                   (:name "Misc Today"
                    :and (:deadline past :not (:habit t))
                    :and (:date today :not (:habit t))
                    :and (:scheduled past :not (:deadline future :habit t)))
                   (:name "Habits" :habit t)
                   (:name "Class Upcoming" :tag "class")
                   ))))))
     ("D" "Daily Super Agenda (fullscreen)" agenda ""
      ((org-agenda-window-setup 'current-window)
       (org-agenda-skip-scheduled-if-deadline-is-shown t)
       (org-agenda-span 1)
       (org-deadline-warning-days 7)
       (org-agenda-time-grid '((daily require-timed remove-match) (800 1000 1200 1400 1600 1800 2000) "......" "----------------"))
       (org-super-agenda-groups
        '((:name "" :time-grid t)
          (:name "" :and (:deadline nil :scheduled nil))
          (:name "Class Today"
           :and (:tag "class" :deadline past)
           :and (:tag "class" :date today :not (:habit t))
           :and (:tag "class" :scheduled past :not (:deadline future)))
          (:name "Class Upcoming" :and (:tag "class" :not (:habit t)))
          (:name "Habits" :habit t)))))
     ("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
     ("t" "Unscheduled TODOs"
      todo ""
      ((org-agenda-skip-function
        '(org-agenda-skip-entry-if 'scheduled 'deadline 'todo '("READING" "TOREAD")))
       (org-super-agenda-groups '((:auto-category t)))
       (org-agenda-overriding-header "Unscheduled TODOs")))
     ("r" "Reading & To-Read" ((todo "READING") (todo "TOREAD") (todo "DNF"))
      ((org-agenda-view-columns-initially nil)
       (org-agenda-prefix-format '((todo . " %(format \"%-20s\" (or (org-entry-get nil \"AUTHOR\" t) \"\")) %i %?-12t %s"))))
      (org-agenda-overriding-header ""))
     ("c" "Classes" agenda ""
      (;(org-super-agenda-groups '(())) TODO
       (org-deadline-warning-days 0)
       (org-scheduled-past-days 0)
       (org-agenda-tag-filter-preset '("+class"))))
     ("p" "Personal" agenda ""
      ((org-deadline-warning-days 0)
       (org-habit-show-habits nil)
       (org-scheduled-past-days 0)
       (org-agenda-tag-filter-preset '("-class"))))))

  (set-popup-rule! "^\\*Org Agenda" :ignore t)


  (add-hook! 'org-agenda-mode-hook :append
    (visual-line-mode -1)
    (setq org-agenda-prefix-format
          '((agenda . " %i %-12:c%?-12t% s")
            (todo . " %i %-12:c")
            (tags . " %i %-12:c")
            (search . " %i %-12:c"))))

  (defun my/org-agenda-adjust-text-size ()
    (if (= text-scale-mode-amount 0)
        (text-scale-set -0.8)))

  (add-hook! 'org-agenda-finalize-hook #'my/org-agenda-adjust-text-size)
  (add-hook! 'org-agenda-finalize-hook #'beginning-of-buffer))

(after! org-agenda
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (evil-set-initial-state 'evil-org-agenda-mode 'emacs)
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode))

;;;;; org-super-agenda
(after! evil-org-agenda
  (use-package! org-super-agenda
    :after (evil evil-org)
    :init (org-super-agenda-mode 1)
    :config
    (set-face-attribute 'org-super-agenda-header nil :underline t )
    (setq org-super-agenda-header-separator ""
          org-super-agenda-unmatched-name "Other"
          org-super-agenda-header-prefix " ")))

;;;; org-books
(use-package! org-books
  :config
  (map! :leader
        "a" #'org-books-add-book
        "A" #'org-books-add-url)
  (setq org-books-file "~/org/reading.org"
        org-books-file-depth 1))
;;;; export


;;;; ox-*
(use-package! ox-typst
  :after org)

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
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("koma-article"
                 "\\documentclass[letterpaper]{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("twocolumn"
                 "\\documentclass[letterpaper, twocolumn]{scrartcl}"
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
      :nv "zq" #'org-fold-hide-block-all
      :leader "tn" #'org-num-mode)



(setq! evil-snipe-scope 'whole-buffer)

(map! :map 'override
      :leader
      "'" #'org-agenda
      "x" #'+org-capture/open-frame
      "X" #'doom/open-scratch-buffer)

(map! :after org
      :map 'org-agenda-mode-map
      "k" #'+org-capture/open-frame)
;;; Evil
;; Normally evil doens't respect visual-line-mode. Hopefully this fixes it??
(after! evil
  (setq evil-respect-visual-line-mode nil)) ; TODO apparently this has to do with commands like $ and _. Do i want it??
(map!
 :nv "j" #'evil-next-visual-line
 :nv "k" #'evil-previous-visual-line)

;;;; evil-scroll
(after! evil
  (setq! evil-scroll-count 20))
;;;; Bindings
(map! :v "zn" #'doom/narrow-buffer-indirectly)
(map! :map evil-window-map
      :g "O" #'delete-other-windows)
(map! :leader
      "tq" #'olivetti-mode)
;;; Denote
;; Remember that the website version of this manual shows the latest
;; developments, which may not be available in the package you are
;; using.  Instead of copying from the web site, refer to the version
;; of the documentation that comes with your package.  Evaluate:
;;
;;     (info "(denote) Sample configuration")
(use-package denote
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

(map! :leader
      (:prefix ("d" . "Denote")
       :nv     "n" #'denote
       :nv     "d" #'denote-dired
       :nv     "g" #'denote-grep
                                        ; If you intend to use Denote with a variety of file types, it is
                                        ; easier to bind the link-related commands to the `global-map', as
                                        ; shown here. Otherwise follow the same pattern for `org-mode-map',
                                        ; `markdown-mode-map', and/or `text-mode-map'.
       :nv     "l" #'denote-link
       :nv     "L" #'denote-add-links
       :nv     "b" #'denote-backlinks
       :nv     "q c" #'denote-query-contents-link ; create link that triggers a grep
       :nv     "q f" #'denote-query-filenames-link ; create link that triggers a dired
                                        ; Note that `denote-rename-file' can work from any context, not just
                                        ; Dired bufffers#' That is why we bind it here to the `global-map'.
       :nv     "r" #'denote-rename-file
       :nv     "R" #'denote-rename-file-using-front-matter))
;;; Roam
(after! org-roam
  (setq! org-roam-directory "~/sync/misc/Roam/"))
;;; typst
;;;; typst-ts
(use-package! typst-ts-mode
  :mode "\\.typ\\'"
  :hook (typst-ts-mode . eglot-ensure)
  :init (setq
         typst-ts-watch-options nil
         typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory)
         typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (map! :map typst-ts-mode-map
        :nv "]]" #'outline-next-heading
        :nv "[[" #'outline-previous-heading)
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))

(with-eval-after-load 'eglot
  (with-eval-after-load 'typst-ts-mode
    (add-to-list 'eglot-server-programs
                 `((typst-ts-mode) .
                   ,(eglot-alternatives `(,typst-ts-lsp-download-path
                                          "tinymist"
                                          "typst-lsp"))))))

;;;; typst-preview
(use-package! typst-preview
  :init
  (setq typst-preview-autostart t
        typst-preview-open-browser-automatically t)
  :config
  (setq typst-preview-browser "capp.sh"))
;;; ticktick.el
(use-package! ticktick
  :config (setq ticktick-client-id "your-client-id"
                ticktick-sync-file "~/org/ticktick.org"
                ticktick-client-secret "your-client-secret"))

;;; Olivetti mode
(after! olivetti-mode
  (setq olivetti-body-width 80))
