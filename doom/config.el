;;; config.el -*- lexical-binding: t; -*-
;;; defining global variables
;; These are variables that will be used later
(setq org-directory (expand-file-name "~/org/")
      org-roam-directory (expand-file-name "~/org/Roam/"))
(setq
 my/notes-home (expand-file-name "Index.org" org-directory))

(setq
 my/bullet "Iosevka Nerd Font Mono"
 my/header "Iosevka Nerd Font Mono"
 my/monospace "Iosevka Nerd Font Mono"
 my/variable "Inter"
 my/pretty "Inter")
;;; Terminal Emacs configuation
;; NOTE I don't THINK this will have any impact
;; on anything in GUI windows, only terminals.
(unless (display-graphic-p)
  (xterm-mouse-mode 1))
;;; Don't display output of async cmds with no output.
(setq async-shell-command-display-buffer nil)
;;; Make sure the f.el lib is avaliable
(use-package! f)
;;; General Emacs
(global-auto-revert-mode 1)
(smartparens-global-mode -1)
(setq-default search-invisible nil)
(after! smartparens (setq smartparens-global-mode nil))
(setq-default tab-width 2)
(setq! tab-width 2)

(setq! select-enable-clipboard t)


;; Make initial major mode org-mode (for scratch buffer)

(setq! ispell-dictionary "en_US")
(add-to-list 'load-path (expand-file-name "lisp/" doom-user-dir))
(map!
 :nv "gk" #'outline-backward-same-level
 :nv "zu" #'outline-up-heading
 :nv "gj" #'outline-forward-same-level)
(global-hl-line-mode -1)
(auto-save-visited-mode)

(defun my/inbox-has-headings-p ()
  ;; Returns t if grep finds a match (exit code 0), nil otherwise
  (interactive)
  (eq 0 (call-process "grep" nil nil nil
                      "-q"           ;; Silent mode (just return exit code)
                      "^\\*\\+ "     ;; Regex: Start of line, 1+ stars, space
                      (expand-file-name "Inbox.org" org-directory))))

(defun my/inbox-has-headings-p ()
  ;; Returns t if grep finds a match (exit code 0), nil otherwise
  (interactive)
  (eq 0 (call-process "grep" nil nil nil
                      "-q"           ;; Silent mode (just return exit code)
                      "^\\*\\+ "     ;; Regex: Start of line, 1+ stars, space
                      (expand-file-name "Inbox.org" org-directory))))

(defun my/open-inbox-or-todo ()
  (interactive)
  (if (my/inbox-has-headings-p)
      (find-file (f-join org-directory "Inbox.org"))
    (find-file (f-join org-directory "todo.org"))))

(setq
 inhibit-startup-screen t
 initial-buffer-choice (f-join org-directory "Index.org"))

(map! :leader "l" #'my/open-inbox-or-todo)

;;;; Tab bar above emacs
(tab-bar-mode 1)
(map! :leader
      :desc "Tabs" "TAB" tab-prefix-map)

(map! :leader
      (:prefix
       ("TAB")
       "TAB" #'tab-previous
       "d" #'tab-close
       "q" #'tab-close
       ))

;;;; Default file modes
(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . json-mode))
;;;; links in source comments
(global-goto-address-mode 1)
;;;; isearch
(after! isearch
  (setq isearch-wrap-pause 'no-ding))
;;;; search/lookup provider
(add-to-list '+lookup-provider-url-alist '("Goodreads (Books)" "https://www.goodreads.com/search?q=%s"))
(add-to-list '+lookup-provider-url-alist '("Story Graph (Books)" "https://app.thestorygraph.com/browse?search_term=%s"))
(add-to-list '+lookup-provider-url-alist '("Anna's Archive (Books)" "https://annas-archive.org/search?q=%s"))
(add-to-list '+lookup-provider-url-alist '("Rotton Tomatoes (Movies)" "https://www.rottentomatoes.com/search?search=%s"))
(add-to-list '+lookup-provider-url-alist '("IMDb (Movies)" "https://www.imdb.com/find/?q=%s&s=tt"))
;;;; Line numbers in the margin
;; display-line-numbers seems to cause an enormous amount of drag so i am finding another package
(setq display-line-numbers-width 2)
(setq-default
 display-line-numbers-width 2
 display-line-numbers-widen nil)

(setq
 display-line-numbers-width 2
 display-line-numbers-current-absolute nil
 display-line-numbers-type 'visual
 display-line-numbers-widen nil)

(add-hook 'prog-mode-hook (lambda ()
                            (setq display-line-numebrs-width 2)
                            (display-line-numbers-mode 'visual)))
(add-hook 'doom-docs-mode-hook
          (λ! (display-line-numbers-mode -1)))

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;;;; Text mode line spacing
(setq-default line-spacing 0.0)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local line-spacing 0.0)))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local line-spacing 0.1)))

;;;; scratch buffer
(setq! doom-scratch-initial-major-mode 'emacs-lisp-mode)
;;;; Bindings
(map! :g  "<f5>" 'revert-buffer)

(setq! evil-snipe-scope 'whole-buffer)

(map! :map 'override
      :leader
      "'" #'org-agenda
      "X" #'doom/switch-to-scratch-buffer)

(after! which-key
  (setq which-key-idle-delay .15))

;;;; PDF

(use-package! pdf-view
  :config
  (setq pdf-view-resize-factor 1.1
        pdf-view-display-size 'fit-page
        ;;pdf-view-use-scaling t
        pdf-view-continuous nil)

  (add-hook 'pdf-view-mode-hook  (lambda () (pdf-view-midnight-minor-mode 1)))

  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (interactive)
              (visual-fill-column-mode -1)
              (visual-line-fill-column-mode -1)
              (pdf-view-set-slice-from-bounding-box)
              (pdf-view-auto-slice-minor-mode 1)
              )))

(after! pdf-view
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))


(add-hook! 'pdf-view-mode-hook #'pdf-view-auto-slice-minor-mode)
;;(add-hook! 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)


;;; calendar https://github.com/kiwanami/emacs-calfw
(use-package calfw
  :config
  (setq! calfw-render-line-breaker 'calfw-render-line-breaker-wordwrap
         calfw-org-agenda-schedule-args '(:deadline :scheduled)
         calfw-org-overwrite-default-keybinding t
         calfw-display-calendar-holidays nil)

  (defun my/open-calendar ()
    (interactive)
    ;; NOTE this is necessary cuz olivetti fucks with it
    (set-window-margins nil 0 0)
    (calfw-open-calendar-buffer
     :view 'two-weeks
     :sorter (lambda (a b)
               (let ((a-is-agenda (string-match-p "TODO" a))
                     (b-is-agenda (string-match-p "TODO" b)))
                 (cond
                  ((and a-is-agenda (not b-is-agenda)) t)
                  ((and b-is-agenda (not a-is-agenda)) nil)
                  (t (string-lessp a b)))))
     :contents-sources
     (list
      (calfw-org-create-source nil "org" "Orange")  ; org-agenda source
      (calfw-ical-create-source
       "https://calendar.google.com/calendar/ical/jamesipogue%40gmail.com/private-405fed4d44887c1fb303de8d0e6c7a0d/basic.ics"
       "gcal"
       "IndianRed")  ; ICS source1
      (calfw-ical-create-source
       "https://bc.instructure.com/feeds/calendars/user_lAcbipQjOdEkynqm9tpSh6WgdZPIv89v8qusdQIF.ics"
       "canvas"
       "LightBlue") ; google calendar ICS
      )))

  (defun my/minimal-calendar ()
    (interactive)
    ;; NOTE this is necessary cuz olivetti fucks with it
    (set-window-margins nil 0 0)
    (cfw:open-calendar-buffer
     :view 'week
     :contents-sources
     (list
      (cfw:ical-create-source "canvas" "https://bc.instructure.com/feeds/calendars/user_lAcbipQjOdEkynqm9tpSh6WgdZPIv89v8qusdQIF.ics" "Orange")
      (cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/jamesipogue%40gmail.com/private-405fed4d44887c1fb303de8d0e6c7a0d/basic.ics" "IndianRed")))
    (cfw:change-view-two-weeks)) ; google calendar ICS

  (map! :leader
        (:prefix ("o" . "open")
         :desc "Calendar" "c" #'my/open-calendar))

  (map! :leader
        (:prefix ("o" . "open")
         :desc "Calendar" "v" #'my/minimal-calendar)))
;; Add an org-capture template
(setq! cfw:org-capture-template
       '("c" "calfw2org" entry (file "cal.org")  "* %?
 %(cfw:org-capture-day)"))

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

(setq!
 +doom-dashboard-ascii-banner-fn #'my-weebery-is-always-greater)

(defun my/bruh () (interactive) (org-agenda nil "d"))

(setq!
 +doom-dashboard-menu-sections
 '(
   ("Open Today's Agenda" :icon
    (nerd-icons-octicon "nf-oct-calendar" :face 'doom-dashboard-menu-title) :when
    (fboundp 'org-agenda) :action my/bruh)
   ("Recently opened files" :icon
    (nerd-icons-faicon "nf-fa-file_text" :face 'doom-dashboard-menu-title) :action
    recentf-open-files)
   ("Reload last session" :icon
    (nerd-icons-octicon "nf-oct-history" :face 'doom-dashboard-menu-title) :when
    (cond
     ((modulep! :ui workspaces)
      (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
     ((require 'desktop nil t) (file-exists-p (desktop-full-file-name))))
    :action doom/quickload-session)
   ("Open project" :icon
    (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
    :action projectile-switch-project)
   ("Jump to bookmark" :icon
    (nerd-icons-octicon "nf-oct-bookmark" :face 'doom-dashboard-menu-title)
    :action bookmark-jump)
   ("Open private configuration" :icon
    (nerd-icons-octicon "nf-oct-tools" :face 'doom-dashboard-menu-title) :when
    (file-directory-p doom-user-dir) :action doom/open-private-config)
   ("Open documentation" :icon
    (nerd-icons-octicon "nf-oct-book" :face 'doom-dashboard-menu-title) :action
    doom/help)))

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
(setq!
 doom-font (font-spec :family my/monospace :size 21)
 doom-variable-pitch-font (font-spec :family my/variable :size 21))


;;(setq! doom-theme 'catppuccin)
;;(setq! catpuccin-flavor 'latte)
;;(load-theme 'catppuccin t t)
;;(catppuccin-reload)

(setq! doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;; Disable Title Bar
(add-to-list 'default-frame-alist '(undecorated . t))


;;;; mixed-pitch
;; modified from https://discourse.doomemacs.org/t/cant-size-doom-variable-pitch-font/4572/2

(use-package! mixed-pitch
  :config
  (setq mixed-pitch-set-height t
        variable-pitch-serif-font doom-variable-pitch-font)

  ;; TODO should this be here????
  (add-hook 'mixed-pitch-mode-hook #'variable-pitch-mode)

  (add-hook 'org-mode-hook #'mixed-pitch-mode)

  (pushnew! mixed-pitch-fixed-pitch-faces
            'org-superstar-leading 'org-date
            'org-list-dt 'org-document-info
            'warning 'org-property-value 'org-special-keyword
            'org-drawer 'org-cite-key  'org-hide
            'corfu-default 'font-latex-math-face))

;;;; Remove certain faces from mixed-pitch cuz its fucky
(after! mixed-pitch
  ;; Make sure the cl-lib library is loaded for set-difference
  (require 'cl-lib)

  ;; Define the faces you want to REMOVE from the fixed-pitch list
  (let ((faces-to-remove '(org-todo)))
    (setq mixed-pitch-fixed-pitch-faces
          (cl-set-difference mixed-pitch-fixed-pitch-faces faces-to-remove))))

;;;; doom-docs
(use-package! elisp-mode)


;; disable the arrow that appears
;; when line wrapping
(after! visual-line-mode
  (setq visual-line-fringe-indicators '(nil nil))
  (setf (cdr (assq 'truncation fringe-indicator-alist)) '(nil nil)))
;;;; prettifying emacs
(set-face-attribute 'hl-line nil
                    :extend t)

(set-face-attribute 'calfw-sanitized-face nil
                    :extend t)

;;; adaptive wrap
(add-hook 'prog-mode-hook #'adaptive-wrap-prefix-mode)
(add-hook 'prog-mode-hook #'visual-line-mode)

;; Make sure that the wrapped lines are clearly indented.
(setq adaptive-wrap-extra-indent 2)

(add-hook 'conf-unix-mode-hook #'adaptive-wrap-prefix-mode)
;;; rust-mode
(use-package! rust-mode
  :hook (rust-mode . eglot)
  :config
  (setq rust-format-on-save t
        rust-mode-treesitter-derive t)
                                        ;(add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))
;;; whisper.el
(use-package! whisper
  :config
  (setq! whisper-model "medium"
         whisper-insert-text-at-point nil
         whisper-language "en"
         whisper-use-threads 10
         whisper-translate nil)
  (map!
   (:prefix ("r" . "record voice")
    :leader
    "r" #'whisper-run
    "s" (lambda () (interactive) (let ((whisper-model "small"))
                                   (whisper-run)))
    "l" (lambda () (let ((whisper-model "large-v3-turbo"))
                     (whisper-run)))))
  )


;;; org-mode
(setq
 +fold-ellipsis " ·" ;; includes non-breaking space
 org-ellipsis " ·" ;; includes non-breaking space
 org-archive-location "~/org/.archive/%s_archive::")

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  (setq org-cycle-emulate-tab nil))

(after! org
  (add-hook! 'org-follow-link-hook #'org-reveal)

  (setq
   org-ascii-headline-spacing '(0 . 1)
   shr-inhibit-images '0 ;; for some reason this speeds scrolling when images are on screen.
   org-element-use-cache nil ; NOTE with this on, indirect buffers break
   org-indirect-buffer-display 'dedicated-frame ; TODO should this be set to something else?
   org-link-file-path-type 'relative
   org-habit-show-habits t
   org-habit-scheduled-past-days nil
   org-hide-emphasis-markers t
   org-export-with-smart-quotes nil
   org-export-with-special-strings nil
   org-display-internal-link-with-indirect-buffer nil
   org-link-use-indirect-buffer-for-internals t ;; why are these two separate variables??
   org-log-into-drawer t
   org-startup-indented t
   org-export-with-tags nil
   org-export-with-todo-keywords nil
   org-export-initial-scope 'subtree
   org-pretty-entities t
   org-custom-properties '("AUTHOR" "EXPORT_LATEX_CLASS" "EXPORT_LATEX_CLASS_OPTIONS" )
   org-pretty-entities-include-sub-superscripts t
   org-todo-keywords '((sequence  "TODO(t)" "WAIT(w)" "NEXT(n)" "|" "DONE(d)" "KILL(k)")
                                        ;(sequence "READING(g)" "TOREAD(r)" "|" "DNF" "READ(R)")
                       )
   org-use-sub-superscripts t
   org-export-with-section-numbers t
   org-export-with-toc nil
   org-startup-folded 'nofold ;; Apparently 'fold does not work for some reason
   org-hide-drawer-startup t
   org-image-align 'center
   org-startup-with-inline-images t
   org-cycle-inline-images-display t
   org-cycle-hide-drawer-startup t
   org-image-max-width 440 ; TODO what's the best value for this??
   org-cycle-hide-block-startup nil)

  (add-hook 'org-mode-hook (lambda ()
                             ;;(org-latex-preview-mode 1) NOTE The auto-previews get annoying when scrolling.
                             (org-latex-preview-center-mode 1)
                             ;;(display-line-numbers-mode -1)
                             (auto-save-visited-mode 1)

                             (ultra-scroll-mode 1)
                             (hl-line-mode -1)
                             (visual-line-mode 1)
                             )))
;; Resize Org headings
(map! :leader
      "zo" #'org-fold-show-subtree
      "n/" #'org-ql-find-in-agenda
      "nS" #'org-ql-find-path)

;;;; prettifying org-mode
(setq my/grey "#323232"
      my/emphasis-color "#88C0D0")

;; Spacing before headings in file
(after! org
  (setq
   org-blank-before-new-entry '((heading . auto) (plain-list-item . nil))
   org-indent-indentation-per-level 1
   )
  )

;; hedings are larger
(after! (:and org color)

  (dolist (face
           '((org-level-1 . 1.1)
             (org-level-2 . 1.1)
             (org-level-3 . 1.1)
             (org-level-4 . 1.1)
             (org-level-5 . 1.1)
             (org-level-6 . 1.1)
             (org-level-7 . 1.1)
             (org-level-8 . 1.1)))
    ;; Decided to not make them larger, for now. But idk i may change my mind.
    ;;'((org-level-1 . 1.35)
    ;;  (org-level-2 . 1.30)
    ;;  (org-level-3 . 1.25)
    ;;  (org-level-4 . 1.1)
    ;;  (org-level-5 . 1.1)
    ;;  (org-level-6 . 1.1)
    ;;  (org-level-7 . 1.1)
    ;;  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil
                        :height (cdr face)
                        :family my/monospace
                        :weight 'semi-bold
                        :extend nil
                        :slant 'italic
                        :overline nil
                        ))

  (set-face-attribute 'org-level-1 nil
                      :weight 'bold
                      :underline nil
                      :slant 'normal
                      :extend nil
                      :overline nil)

  (set-face-attribute 'org-link nil
                      :family my/monospace
                      :extend nil)

  (set-face-attribute 'org-list-dt nil
                      :slant 'italic)

  (set-face-attribute 'org-done nil
                      :family my/monospace
                      :foreground "sea green")

  (set-face-attribute 'org-ellipsis nil
                      :foreground "grey59"
                      :family my/monospace
                      :extend t
                      :height 0.8)

  (set-face-attribute 'bold nil
                      :foreground my/emphasis-color)
  (set-face-attribute 'italic nil
                      :foreground (color-desaturate-name my/emphasis-color 10))

  (set-face-attribute 'org-todo nil
                      :weight 'bold
                      :slant 'normal
                      :family my/monospace)

  (set-face-attribute 'org-priority nil
                      :weight 'bold
                      :slant 'normal
                      :family my/monospace)

  (set-face-attribute 'error nil
                      :family my/monospace
                      :weight 'bold)

  (set-face-attribute 'underline nil
                      :underline my/emphasis-color)

  (set-face-attribute 'org-property-value nil
                      :family my/monospace)

  (set-face-attribute 'org-drawer nil
                      :extend t
                      :height 0.8)

  (set-face-attribute 'org-special-keyword nil
                      :height 1.0
                      :family my/monospace)

  (set-face-attribute 'org-block nil
                      :extend t)

  (set-face-attribute 'org-block-begin-line nil
                      :extend t)

  (set-face-attribute 'org-block-end-line nil
                      :extend t)

  (set-face-attribute 'org-document-title nil
                      :family my/monospace
                      :weight 'bold
                      :slant 'italic)

  ;; change org-quote face to be mixed-pitch
  (set-face-attribute 'org-quote nil
                      :extend t
                      :font my/monospace
                      :slant 'italic
                      :height 1.0)
  )


;;;;; prettify-symbols-mode
(after! prettify-symbols
  (setq prettify-symbols-unprettify-at-point t))
(defun my/math/load-prettify-symbols ()
  "Load custom prettify symbols for latex/org."
  (interactive)
  (setq prettify-symbols-alist
        (append
         '(("\\blank"      . ?—)
           ("\\otimes"     . ?⨂)
           ("\\text"       . ?❝)
           ("\\defeq"      . ?≔)
           ("\\partial"    . ?∂)
           ("\\sqrt"       . ?√)
           ("\\left"       . ?ₗ)
           ("\\right"      . ?ᵣ)
           ("\\mathcal{C}" . ?𝓒)
           ("\\cat{C}"     . ?𝓒))
         prettify-symbols-alist))
  (prettify-symbols-mode 1))

(after! org
  (add-hook 'org-mode-hook #'my/math/load-prettify-symbols)
  (add-hook 'TeX-mode-hook #'my/math/load-prettify-symbols))

(my/math/load-prettify-symbols)

;;;; org-mouse
(use-package! org-mouse)
;;;; fuck middle click paste
(map! :map global-map "<mouse-2>" nil
      :map evil-insert-state-map "<mouse-2>" nil
      :map evil-normal-state-map "<mouse-2>" nil
      :map evil-replace-state-map "<mouse-2>" nil)

;;;;; jit-lock
(setq!
 jit-lock-chunk-size 6000
 jit-lock-defer-time 0 ; NOTE 0 --> Defer when pending input.
 )

;;;; org-appear
(use-package! org-appear
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq
   ;;if non-nil and org-hide-emphasis-markers is on, toggle emphasis markers
   org-appear-autoemphasis t
   ;;if non-nil and org-link-descriptive is on, toggle links
   org-appear-autolinks nil
   ;;if non-nil and org-pretty-entities is on, toggle subscripts and superscripts
   org-appear-autosubmarkers t
   ;;if non-nil and org-pretty-entities is on, toggle Org entitites
   org-appear-autoentities t
   ;;if non-nil and org-hidden-keywords is on, toggle keywords in org-hidden-keywords
   org-appear-autokeywords t
   ;;if non-nil, toggle entities and sub/superscripts in LaTeX fragments
   org-appear-inside-latex t
   ;;seconds of delay before toggling
                                        ;org-appear-delay
   ;;when to toggle elements
                                        ;org-appear-trigger
   ))
;;;; custom binds
(map! :leader
      :nv "SPC" (lambda () (interactive) (find-file my/notes-home))
      :nv "I" (lambda () (interactive) (find-file (expand-file-name "Inbox.org" org-directory)))
      )
;;;; How to follow file links
(after! org
  (setf (alist-get 'file org-link-frame-setup) 'find-file))
;;;; org-links
(after! org
  (setq org-id-link-to-org-use-id 'create-if-interactive))
;;;; custom function to convert links when refiling to new path.
(defun my/convert-links (oldpath)
  "When refiling, if the new path is in a different parent directory from the OLDPATH, relative links will break. This was written to automatically fix this."
  (interactive "DPath from which this file was moved: ")
  (goto-char (point-min))
  (while (search-forward-regexp "\\[\\[file:\\(.*?\\)\\]\\]")
    (let ((path (match-string 1)))
      (unless (f-absolute-p path)
        (replace-match
         (concat "[[file:" (f-join (f-relative oldpath) path) "]]"))))))
;;;; TODO hierarchical tags in org-mode
(after! org
  (setq
   org-tag-alist nil)
  )

;;;; org-src
(after! org
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0))
;;;; org-cdlatex
(after! cdlatex
  (setq! cdlatex-takeover-subsuperscript t
         TeX-electric-sub-and-superscript t
         cdlatex-simplify-sub-super-scripts nil))

(map! :after cdlatex
      :mode cdlatex-mode
      "<TAB>" #'cdlatex-tab)

;;;; org latex math block prettification

;; make =\frac{a}{b}= look pretty
(with-eval-after-load 'tex-fold
  (add-to-list 'TeX-fold-macro-spec-list '("{1}/{2}" ("frac"))))

(use-package! tex-fold
  :config
  ;;(add-hook 'org-mode-hook (lambda ()
  ;;                           (interactive)
  ;;                           (tex-fold-mode 1)
  ;;                           (TeX-fold-buffer)) t)
  (setq TeX-fold-ellipsis ".."))

;;;; org-superstar
(use-package! org-superstar
  :config
  (add-hook 'org-mode-hook #'org-superstar-mode)

  (set-face-attribute 'org-superstar-header-bullet nil
                      :slant 'normal
                      :weight 'ultra-bold
                      :family my/bullet)

  (setq org-superstar-leading-bullet "·"
        org-superstar-remove-leading-stars nil

        org-hide-leading-stars nil

        org-superstar-headline-bullets-list '("⁖" "◈" "◇" "◉" "○" "⚬")
        org-superstar-special-todo-items nil ;; Makes TODO header bullets into boxes
        org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("DONE" . 9744)
                                          ("NEXT" . 9744)
                                          ("WAIT" . 9744)
                                          ("SOMEDAY" . 9744)
                                          ("READING" . 9744)
                                          ("TOREAD" . 9744)
                                          ("WAIT" . 9744))
        )

  )


;;;; org-alert
(use-package! org-alert
  :config
  (setq org-alert-interval 300
        org-alert-notify-cutoff 10
        org-alert-notify-after-event-cutoff 10)
  (org-alert-enable))


;;;; org-download
(use-package! org-download
  :init
  (setq-default org-download-image-dir "./images")
  (map! :map org-mode-map
        :localleader
        :desc "Rename image at point" "a C" #'org-download-rename-at-point)
  :config
  (setq org-download-method 'directory
        org-download-image-latex-width 0
        org-download-timestamp "%Y-%m-%d-%H%M%S"
        org-download-screenshot-basename "sc.png"
        org-download-link-format "[[file:images/%s]]\n"
        org-download-image-attr-list nil
        org-download-heading-lvl nil))
;;;; org-attach
(after! org-attach
  (setq
   org-attach-dir-relative t
   org-attach-directory "~/org/data"
   org-attach-store-link-p 'file))
;;;; org-latex
(after! org
  (setq org-latex-image-default-width "0.90\\linewidth"
        ;; TODO make figure stay in spot in articles
        org-latex-default-figure-position "htbp"
        org-export-with-broken-links t
        org-latex-precompile t
        org-latex-default-class "article")

  (dolist (pkg '("placeins" "amsmath" "amssymb" "amsthm" "mathtools" "mathrsfs" "pgfplots" "float" "siunitx" "xfrac" "cancel"))
    (add-to-list 'org-latex-packages-alist `("" ,pkg t))))
;;;; org-modern
(use-package! org-modern
  :config
  (add-hook! 'org-agenda-finalize-hook #'org-modern-agenda)
  (setq!
   org-fold-catch-invisible-edits 'show-and-error

   org-modern-hide-stars nil
   org-modern-timestamp t
   org-modern-checkbox nil
   org-modern-keyword nil
   org-modern-star nil

   org-modern-tag nil
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil

   org-agenda-block-separator ?─
   )
  )
(global-org-modern-mode -1)
;;;; org-latex-preview
;; code for centering LaTeX previews -- a terrible idea
;; TODO enable latex previews in org-roam and latex buffers (use xenops)

;; TODO enable latex previews in org-roam and latex buffers (use xenops)

;; modified from https://abode.karthinks.com/org-latex-preview/

;; TODO enable latex previews in org-roam and latex buffers (use xenops)


;; modified from https://abode.karthinks.com/org-latex-preview/
;; code for centering LaTeX previews -- a terrible idea
(use-package! org-latex-preview
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.6)
  (plist-put org-latex-preview-appearance-options
             :zoom 1.10)
  (plist-put org-latex-preview-appearance-options
             :scale 1.00)

  ;; ;; Use dvisvgm to generate previews
  ;; ;; You don't need this, it's the default:
  ;; (setq org-latex-preview-process-default 'dvisvgm)

  ;; Turn on `org-latex-preview-mode', it's built into Org and much faster/more
  ;; featured than org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  ;; (add-hook 'org-mode-hook 'org-latex-preview-mode) NOTE This causes annoying previews on scrolling. I prefer the manual behavior.

  ;; Block C-n, C-p etc from opening up previews when using `org-latex-preview-mode'
  (setq org-latex-preview-mode-ignored-commands
        '(next-line previous-line mwheel-scroll
          scroll-up-command scroll-down-command
          wheel-up wheel-down wheel-down
          triple-wheel-down triple-wheel-up
          double-wheel-down double-wheel-up))
  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered nil)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-mode-display-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-mode-update-delay 0.25)

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
                  (prop `(space :align-to (- center (0.52 . ,img))))
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
                   #'my/org-latex-preview-uncenter)))
  (add-hook! 'org-latex-preview-mode-hook (lambda () (org-latex-preview 'buffer)))
  (add-hook! 'org-mode-hook (lambda () (org-latex-preview 'buffer))))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (org-latex-preview 'buffer))))

;;;;; My custom stuff
(setq org-latex-preview-process-alist '((dvipng :programs ("latex" "dvipng") :description "dvi > png" :message
                                         "you need to install the programs: latex and dvipng." :image-input-type
                                         "dvi" :image-output-type "png" :latex-compiler
                                         ("%l -interaction nonstopmode -output-directory %o %f") :latex-precompiler
                                         ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
                                         :image-converter
                                         ("dvipng --follow -D %D -T tight --depth --height -o %B-%%09d.png %f")
                                         :transparent-image-converter
                                         ("dvipng --follow -D %D -T tight -bg Transparent --depth --height -o %B-%%09d.png %f"))
                                        (dvisvgm :programs ("latex" "dvisvgm") :description "dvi > svg" :message
                                                 "you need to install the programs: latex and dvisvgm." :image-input-type
                                                 "dvi" :image-output-type "svg" :latex-compiler
                                                 ("%l -interaction nonstopmode -output-directory %o %f") :latex-precompiler
                                                 ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
                                                 :image-converter
                                                 ("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts  -v3 --message='processing page {?pageno}: output written to {?svgpath}' --bbox=preview -o %B-%%9p.svg %f"))
                                        (imagemagick :programs ("pdflatex" "convert") :description "pdf > png" :message
                                                     "you need to install the programs: latex and imagemagick."
                                                     :image-input-type "pdf" :image-output-type "png" :latex-compiler
                                                     ("pdflatex -interaction nonstopmode -output-directory %o %f")
                                                     :latex-precompiler
                                                     ("pdftex -output-directory %o -ini -jobname=%b \"&pdflatex\" mylatexformat.ltx %f")
                                                     :image-converter
                                                     ("convert -density %D -trim -antialias %f -quality 100 %B-%%09d.png"))))

;; TODO No need after using new org version??????
;;(after! org
;;(plist-put org-format-latex-options :scale .60))
;; TODO enable latex previews in org-roam and latex buffers (use xenops)


(map! :map 'org-mode-map
      :nvi "C-M-S-<return>" #'org-insert-todo-subheading)

;; modified from https://abode.karthinks.com/org-latex-preview/

;;;; Timeblock
(after! org-timeblock
  (setq
   org-timeblock-span 7
   org-timeblock-show-future-repeats t))
;;;; org-pomodoro and org-clock
(use-package! org-pomodoro
  :config
  (setq
   org-clock-sound "~/.config/doom/assets/chime.wav"
   org-pomodoro-format "Pomo~%s"
   org-clock-persist t

   org-pomodoro-start-sound-p t
   org-pomodoro-start-sound "~/.config/doom/assets/chime.wav"
   org-pomodoro-finished-sound "~/.config/doom/assets/chime.wav"
   org-pomodoro-long-break-sound "~/.config/doom/assets/chime.wav"
   org-pomodoro-short-break-sound nil
   org-pomodoro-short-break-format "Break~%s"
   org-pomodoro-clock-break nil
   org-pomodoro-keep-killed-pomodoro-time t

   org-pomodoro-ticking-sound-states '(:pomodoro)

   org-pomodoro-short-break-length 7
   org-pomodoro-manual-break t
   org-pomodoro-length 30

   org-pomodoro-ticking-sound-p t

   ;; I just find it annoying that the stop sound and the tick
   ;; play at the same time when the pomo length is an integer
   ;; multiple of the tick time, so the frequency is 5 min + 1 second
   ;; so that this does not happen. The tick will come 6 seconds late
   ;; after 30 minutes: (/ 30 5) => 6
   org-pomodoro-ticking-frequency (+ (* 60 5) 2)
   org-pomodoro-ticking-sound "~/.config/doom/assets/gong.wav")

  (map! :map 'org-agenda-mode-map
        :e "P" #'org-pomodoro)
  (map! :leader
        "tp" #'org-pomodoro)
  )

(use-package! org-clock
  :config
  (setq org-clock-sound "~/.config/doom/assets/bell.wav"))

(map! :leader
      "tT" #'org-timer-set-timer)
;;;; org-capture TODO
(after! org
  (setq org-capture-templates
        '(("u" "Unscheduled todo" entry (file "Inbox.org")
           "* TODO %?\n%i\n" :prepend t)
          ("U" "Unscheduled Linked todo" entry (file "Inbox.org")
           "* TODO %?%i\n%a" :prepend t)
          ("t" "Personal todo" entry (file "Inbox.org")
           "* TODO %?\nSCHEDULED: %t\n%i\n" :prepend t)
          ("T" "Linked todo" entry (file "Inbox.org")
           "* TODO %?%i\nSCHEDULED: %t\n%a" :prepend t)
          ("h" "Personal habit" entry (file "habit.org")
           "* TODO [#D] %?\nSCHEDULED: %t\n:PROPERTIES:\n:STYLE: habit\n:END:\n%i\n" :prepend t)
          ("n" "Personal notes" entry (file "Inbox.org")
           "* %?\n%i\n" :prepend t :jump-to-captured t)
          ("N" "Linked note" entry (file "Inbox.org")
           "* %?\n%i\n%a" :prepend t :jump-to-captured t)
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
          ("c" "Class Todo" entry (file+headline "todo.org" "Class")
           "* TODO %?\nSCHEDULED: %t\n%i\n" :prepend t)
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry #'+org-capture-central-project-todo-file
           "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry #'+org-capture-central-project-notes-file
           "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file
           "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
          ("r" "Reading Inbox" entry (file+headline "reading.org" "Inbox")
           "* TOREAD %?\n%^{AUTHOR}p\n%i\n" :prepend t)
          )))

;;;; org-journal
(after! org
  (setq
   org-journal-file-format "%Y.org"
   org-journal-time-format "" ; NOTE 3-char weekday: "%a"
   org-journal-date-format "%b %e"
   org-journal-dir (expand-file-name "~/org/journal/")
   org-journal-file-type 'yearly)
  )

(map! :leader "njo" #'org-journal-open-current-journal-file)
;;;; zotero
(defun my-org-zotero-open (path _)
  (start-process "zotero-open" nil "xdg-open" (concat "zotero:" path)))
(after! org
  (org-link-set-parameters "zotero" :follow #'my-org-zotero-open))

;;;; Timeblock
(use-package! org-timeblock)
(after! org-timeblock
  (add-to-list 'evil-emacs-state-modes 'org-timeblock-mode))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Timeblock" "t" #'org-timeblock))

;;;; org-ql https://github.com/alphapapa/org-ql
(use-package! org-ql
  :config
  )
;;;; org-noter
(use-package! org-noter
  :after org
  :config
  (setq org-noter-hide-other t
        org-noter-max-short-selected-text-length 300 ;; Determines how long until it substitutes with 'notes for..'
        org-noter-highlight-selected-text t
        org-noter-separate-notes-from-heading nil
        org-noter-use-indirect-buffer nil
        org-noter-always-create-frame nil
        org-noter-disable-narrowing t ;; NOTE This is because fontification gets fucky in large indirect buffers.
        org-noter-closest-tipping-point 0.8
        org-noter-doc-split-fraction '(0.55 . 0.45))
  )


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
(defun my/switch-to-del-other-window ()
  (interactive)
  (org-agenda-switch-to t))
(map! :map 'org-super-agenda-header-map
      :e "n" #'org-agenda-next-item
      :e "p" #'org-agenda-previous-item)

(map! :map 'org-agenda-mode-map
      :e "n" #'org-agenda-next-item
      :e "p" #'org-agenda-previous-item
      :e "S" #'org-agenda-schedule
      :e "D" #'org-agenda-deadline
      :e "'" #'org-agenda
      :e "c" #'org-agenda-clock-goto
      ;; Make S-return fill the whole current frame with the selected heading
      :e "<return>" #'my/switch-to-del-other-window ; optional argument DELETE-OTHER-WINDOWS = t
      :e "S-<return>" #'org-agenda-switch-to
      :e "C-t" #'org-pomodoro)

;; Hide some tags such as ATTATCH
(evil-set-initial-state 'org-agenda-mode 'emacs)
(add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
(after! org-agenda
  (after! evil (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
    (evil-set-initial-state 'org-agenda-mode 'emacs))
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)

  (custom-set-faces!
    '(org-agenda-date-today :slant normal :weight ultra-bold)
    '(org-agenda-date  :slant normal :weight bold)
    '(org-agenda-date-weekend :slant normal :weight bold))

  (setq!
   org-agenda-use-time-grid nil
   org-agenda-overriding-header ""
   org-agenda-remove-tags nil
   org-enforce-todo-checkbox-dependencies t
   org-enforce-todo-dependencies t
   org-agenda-dim-blocked-tasks nil
   org-agenda-scheduled-leaders '("Schd." "S.%2dx")
   org-agenda-deadline-leaders '("Dedl." "In%2dd" "D.%2dx")
   org-habit-show-habits t
   org-agenda-format-date "%b %d ─── %a"
   org-agenda-timerange-leaders '("" "     ")
   org-agenda-prefix-format
   '((agenda . " %-8:c %t %s ")
     (todo . " %-8:c")
     (tags . " %-8:c")
     (search . " %-8:c"))
   org-agenda-hide-tags-regexp nil
   org-agenda-files
   (seq-filter (lambda (f)
                 (and
                  (not (string-prefix-p ".trash" (file-name-nondirectory f)))
                  (not (string-match-p "\\.sync-conflict" (file-name-nondirectory f)))))
               (append org-agenda-files
                       (directory-files org-directory t "^.*\\.org$")
                       (directory-files (f-join org-directory "class/") t "^.*\\.org$")
                       ;; NOTE 3 dots to escape "/." and "/.."
                       (directory-files org-journal-dir t "...")))

   org-agenda-follow-indirect nil ;; TODO What's the best value for this to not be confusing
   org-agenda-skip-scheduled-if-done t
   org-habit-show-all-today nil
   org-habit-show-done-always-green t
   org-agenda-skip-scheduled-repeats-after-deadline t
   org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
   org-agenda-todo-ignore-scheduled 'future
   org-agenda-todo-ignore-deadlines 'future
   org-agenda-todo-ignore-timestamp 'future
   org-habit-following-days 2
   +org-habit-graph-padding 0
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

  (setq org-agenda-sorting-strategy
        '((agenda
           habit-down
           priority-down
           timestamp-up
           deadline-up
           time-up
           priority-down)
          (todo urgency-down)
          (tags urgency-down)))

  (setq
   org-priority-default  67
   org-priority-lowest 68
   org-fold-show-context-detail '((agenda . tree)
                                  (org-goto . ancestors)
                                  (bookmark-jump . lineage)
                                  (isearch . lineage)
                                  (default . ancestors))
   org-priority-start-cycle-with-default nil
   org-agenda-start-day nil
   org-agenda-window-setup 'only-window
   org-agenda-restore-windows-after-quit t
   org-deadline-warning-days 14 ;; TODO Change once i stop using so many deadlines
   
   ;; TODO setting this to 0 somehow removes all non-past scheduled from agenda. no idea why:
   org-scheduled-delay-days 0
   org-agenda-todo-list-sublevels nil
   org-agenda-window-frame-fractions '(0.5 . 0.8)
   org-agenda-skip-deadline-if-done t
   org-startup-shrink-all-tables t
   org-agenda-span 7
   org-agenda-start-on-weekday nil ;; NOTE 1 for Monday
   org-agenda-start-with-follow-mode nil)


  (setq
   org-agenda-custom-commands
   '(("'" "Default Agenda IDK" agenda "" ((org-deadline-warning-days 7)
                                          (org-agenda-span 1)))
     ("w" "Weekly Super Agenda"
      ((agenda ""
               ((org-agenda-overriding-header "")
                (org-agenda-skip-scheduled-if-deadline-is-shown t)
                (org-agenda-span 7)
                (org-habit-graph-column 70)
                (org-agenda-time-grid '((today require-timed remove-match) (800 1000 1200 1400 1600 1800) "......" "----------------"))
                (org-super-agenda-groups
                 '((:name "" :time-grid t)
                   (:name "" :and (:deadline nil :scheduled nil))
                   (:name "" :todo "NEXT")
                   (:name "Personal"
                    :and (:todo "NEXT" :not (:habit t))
                    :tag "class"
                    :not(:habit t))
                   (:name "Habits" :habit t)
                   ))))
       (todo "READING"
             ((org-agenda-hide-tags-regexp "reading")
              (org-agenda-overriding-header "Currently Reading:")
              (org-agenda-view-columns-initially nil)
              (org-agenda-prefix-format '((todo . " %(format \"%-18s\" (or (org-entry-get nil \"AUTHOR\" t) \"\")) %i %?-12t %s")))))))
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
       (org-agenda-sorting-strategy
        '((agenda habit-up)))
       (org-habit-show-habits-only-for-today t)
       (org-habit-show-all-today t)
       (org-habit-show-habits t)
       (org-super-agenda-groups
        '((:discard (:not (:habit t)))
          (:habit t)))))
     ("d" "Daily Super Agenda" agenda ""
      ((org-agenda-span 1)))
     ("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
     ("r" "Reading & To-Read" ((todo "READING") (todo "TOREAD") (todo "DNF"))
      ((org-agenda-view-columns-initially nil)
       (org-agenda-prefix-format '((todo . " %(format \"%-18s\" (or (org-entry-get nil \"AUTHOR\" t) \"\")) %i %?-12t %s"))))
      (org-agenda-overriding-header ""))
     ;; lowercase c displays todays class agenda grouped by heading.
     ("c" "Classes" agenda ""
      ((org-super-agenda-groups
        '(
          (:discard (:not (:tag "class")))
          (:auto-parent t)
          ))
       )
      )))

  (set-popup-rule! "^\\*Org Agenda" :ignore t)


  (add-hook! 'org-agenda-mode-hook :append
    (visual-line-mode -1)
    (ultra-scroll-mode 1)
    )
  )


(after! org-agenda
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (evil-set-initial-state 'evil-org-agenda-mode 'emacs)
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode))

;;;;; org-super-agenda groups
(after! org
  (setq
   org-super-agenda-groups '((:name "" :time-grid t)
                             ;; Events are not scheduled or deadlines.
                             (:name "" :and
                                    (:deadline nil
                                     :scheduled nil))
                             ;; Not a habit and not in the future
                             (:name "\nToday"
                              :and (:not (:deadline future
                                          :scheduled future)
                                         :not (:habit t)))
                             (:name "\nHabits" :habit t)
                             (:name "\nUpcoming" :deadline future)
                             (:name "" :anything t)))
  )


;;;;; org-agend entry text
(after! org-agenda
  (setq org-agenda-entry-text-maxlines 10
        org-agenda-start-with-entry-text-mode nil)
  )
;;;; Make check boxes under a heading re-set when the heading is done
(after! org
  (add-hook 'org-todo-repeat-hook #'org-reset-checkbox-state-subtree))
;;;;; org-super-agenda
(after! evil-org-agenda
  (use-package! org-super-agenda
    :after (evil evil-org)
    :init (org-super-agenda-mode 1)
    :config
    (set-face-attribute 'org-super-agenda-header nil :slant 'italic )
    (setq org-super-agenda-header-separator ""
          org-super-agenda-unmatched-name "Other"
          org-super-agenda-header-prefix " ")))

;;;; org-anki
(use-package! org-anki)
;;;; consult
(after! consult
  (map! :leader "nh" #'consult-org-agenda))
;;;; ox-*
(use-package! ox-typst
  :after org)

(plist-put org-format-latex-options :background "Transparent")
(plist-put org-format-latex-options :font "IBMPlexSans")

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

;;;; ox-latex
(use-package! ox-latex
  :after org
  :config
  (add-to-list 'org-latex-classes
               '("tufte-handout"
                 "\\documentclass[letterpaper,justified]{tufte-handout}\n\\usepackage{marginfix}" ;; NOTE latex header goes here
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("tufte-book"
                 "\\documentclass[letterpaper,justified]{tufte-book}\n\\usepackage{marginfix}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
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

;;;; ox-tufte
(use-package! ox-tufte)
;;; denote
;;     (info "(denote) Sample configuration")
(use-package! denote
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
  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/Documents/denote/")
        denote-use-directory (expand-file-name "~/Documents/denote/")
        denote-save-buffers t
        denote-org-store-link-to-heading t
        denote-known-keywords '("emacs" "philosophy" "politics" "economics")
        denote-infer-keywords t
        denote-sort-keywords t
        denote-excluded-directories-regexp nil
        denote-excluded-keywords-regexp nil
        denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Since doom emacs automatically inserts a =#+title: ...= to the of the file, it's not needed here
  (setq
   denote-org-front-matter
   "#+title: %s\n#+date: %s\n#+filetags:   %s\n#+identifier: %s\n#+signature:  %s\n#+CATEGORY:   Denote\n\n")

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1)
  )

(map! :leader
      :prefix ("d" . "Denote")
      :nv    "n" #'denote
      :nv     "N" #'denote-link-after-creating
      :nv     "y" #'denote-region
      :nv     "d" #'denote-dired
      :nv     "s" #'denote-subdirectory
      :nv     "g" #'denote-grep
      :nv     "k" #'denote-rename-file-keywords
                                        ; If you intend to use Denote with a variety of file types, it is
                                        ; easier to bind the link-related commands to the `global-map', as
                                        ; shown here. Otherwise follow the same pattern for `org-mode-map',
                                        ; `markdown-mode-map', and/or `text-mode-map'.

      :nv     "o" #'denote-open-or-create

      :nv     "l" #'denote-link-or-create
      :nv     "L" #'denote-add-links
      :nv     "b" #'denote-backlinks
      :nv     "q c" #'denote-query-contents-link ; create link that triggers a grep
      :nv     "q f" #'denote-query-filenames-link ; create link that triggers a dired
                                        ; Note that `denote-rename-file' can work from any context, not just
                                        ; Dired bufffers#' That is why we bind it here to the `global-map'.
      :nv     "r" #'denote-rename-file
      :nv     "R" #'denote-rename-file-using-front-matter)
;;;; consult-notes
(use-package! consult-notes
  :config
  (setq consult-notes-file-dir-sources nil)
                                        ;(setq consult-notes-file-dir-sources
                                        ;      `(;("Dentoe"  ?d  ,denote-directory)
                                        ;        ("Org"     ?o  ,org-directory)) ;; Set notes dir(s), see below
                                        ;      )
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
                                        ;(setq consult-notes-org-headings-files org-agenda-files)

  (consult-notes-org-headings-mode)
  ;;(consult-notes-denote-mode -1)
  (consult-notes-org-roam-mode)
  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function
        (lambda () (denote-directory-files nil t t))))

(map! :map doom-leader-notes-map
      "s" #'consult-notes-search-in-all-notes
      "'" #'consult-notes)

(map! :leader
      "d'" #'consult-notes)

;;;; denote-org
(use-package! denote-org)
;;;; denote-menu
(use-package! denote-menu
  :config
  (setq!
   denote-menu-show-file-type nil))
;;;; TODO Disable wrapping in denote-dired..
(global-visual-line-mode -1)
(add-hook! 'denote-dired-mode-hook
  (lambda ()
    (visual-line-mode -1))
  )
(add-hook! 'dired-mode-hook
  (lambda ()
    (visual-line-mode -1))
  )
;;; spacious-padding https://protesilaos.com/emacs/spacious-padding
(use-package! spacious-padding
  :hook (text-mode . spacious-padding-mode)
  :config
  ;; These are the default values, but I keep them here for visibility.
  (setq spacious-padding-widths
        '( :internal-border-width 0
           :header-line-width 0
           :mode-line-width 0
           :tab-width 1
           :right-divider-width 4
           :scroll-bar-width 8
           :fringe-width 12))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-frame-lines
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))

  ;; Set a key binding if you need to toggle spacious padding.
  (define-key global-map (kbd "<f8>") #'spacious-padding-mode)
  (spacious-padding-mode)
  )
;;; Roam
(after! org-roam
  (map! :mode org-mode
        :leader
        :nv "mrN" #'org-roam-extract-subtree)
  (setq! org-roam-directory (expand-file-name "~/org/Roam/")))
;;; typst-ts
(use-package! typst-ts-mode
  :mode "\\.typ\\'"
  :hook (typst-ts-mode . eglot-ensure)
  :config
  (setq
   typst-ts-watch-options nil
   typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory)
   typst-ts-mode-enable-raw-blocks-highlight t)
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

;;; typst-preview
(use-package! typst-preview
  :init
  (setq typst-preview-autostart t
        typst-preview-open-browser-automatically t)
  :config
  (setq typst-preview-browser "capp.sh"))
;;;;; Tpyst-ts-faces
(after! typst-ts-mode
  (set-face-attribute 'typst-ts-markup-header-face nil
                      :height 1.5
                      :weight 'bold)
  (set-face-attribute 'typst-ts-markup-emphasis-face nil
                      :height 1))
;;; Latex
(after! font-latex
  (setq font-latex-fontify-sectioning 1.6))
;;; ticktick.el
(use-package! ticktick
  :config (setq ticktick-client-id "your-client-id"
                ticktick-sync-file "~/org/ticktick.org"
                ticktick-client-secret "your-client-secret"))

;;; Olivetti mode
(use-package! olivetti
  :config
  (setq-default olivetti-body-width 80)
  (add-hook 'org-mode-hook
            (λ! (olivetti-mode 1))))

(setq! fill-column 80)


;;;; No Olivetti in scratch buffers
;;; ultra-scroll
(use-package! ultra-scroll
  :config
  (setq-default scroll-conservatively 80)
  (setq scroll-conservatively 80 ; or whatever value you prefer, since v0.4
        ultra-scroll-hide-cursor 0.5
        ;; NOTE important: scroll-margin>0 not yet supported
        scroll-margin 0)
  )
;;; custom bindings
;;;
(defun my/preview () (interactive) (org-latex-preview 'buffer))
(add-hook! 'org-mode #'my-preview)
(map! :after evil-org
      :mode org-mode
      :leader "cl" #'my/preview)
;;;; TODO advice for opening subtree idk
(advice-add 'org-fold-show-subtree :after
            (λ! (org-cycle-inline-images-display 'subtree)))
;;;; Convert region to org from md
(evil-define-operator evil-pandoc-md-to-org (beg end)
  "Convert region from Markdown to Org using pandoc, and remove soft hyphens."
  :move-point nil
  (let* ((input (buffer-substring-no-properties beg end))
         ;; call pandoc with input from region
         (converted (with-temp-buffer
                      (insert input)
                      (call-process-region (point-min) (point-max)
                                           "pandoc" t t nil "-f" "markdown" "-t" "org" "--wrap=none")
                      (buffer-string)))
         ;; remove soft hyphens
         (cleaned (replace-regexp-in-string "\u00AD" "" converted)))
    ;; replace region with cleaned output
    (delete-region beg end)
    (insert cleaned)))

(map! :after org
      :mode org-mode
      :leader
      "cm" #'evil-pandoc-md-to-org
      "co" #'evil-pandoc-md-to-org)


;;;; my/clean-org
(defun my/clean-notes ()
  (interactive)
  (dolist
      (f
       (directory-files org-directory t
                        "^.*\\.\\(png\\|pdf\\|tex\\|aux\\|bbl\\|log\\|out\\|fls\\|fdb_latexmk\\|synctex\\.gz\\)$"))
    (delete-file f))
  (dolist
      (f
       (directory-files denote-directory t
                        "^.*\\.\\(tex\\|aux\\|bbl\\|log\\|out\\|fls\\|fdb_latexmk\\|synctex\\.gz\\)$"))
    (delete-file f)))

(map! :map doom-leader-notes-map
      "w" #'org-save-all-org-buffers
      "x" #'my/clean-notes)
;;;; nf and nF
(map! :after evil
      :leader
      "nf" #'+default/browse-notes
      "nF" #'+default/find-in-notes)
;;;; M-[ and M-]
(defun my/org-show-parent-entry-and-return ()
  "Show the parent heading's entry, then return to point."
  (interactive)
  (let ((pos (point)))
    (org-back-to-heading)
    (org-up-heading-safe)
    (outline-show-entry)
    (goto-char pos)))

(defun my/open-subtree-only ()
  (interactive)
  (outline-hide-other)
  (org-reveal)
  (org-fold-show-subtree)
  (org-cycle-inline-images-display 'subtree))

(defun my/open-only ()
  (interactive)
  (outline-hide-other)
  (org-reveal)
  )

(defun close-and-open-next ()
  (interactive)
  (condition-case nil
      (outline-show-children)
    (error nil))
  (outline-next-heading)
  (outline-hide-body)
  (+org/open-fold)
  (+org/open-fold)
  (org-cycle-inline-images-display 'subtree)
  )

(defun close-and-open-prev ()
  (interactive)
  (outline-previous-heading)
  (outline-hide-body)
  (+org/open-fold)
  (+org/open-fold)
  (org-cycle-inline-images-display 'subtree)
  )

(defun close-and-Open-next ()
  (interactive)
  (condition-case nil
      (+org/close-fold)
    (error nil))
  (org-forward-heading-same-level 1)
  (+org/open-fold)
  (org-cycle-inline-images-display 'subtree)
  )

(defun close-and-Open-prev ()
  (interactive)
  (condition-case nil
      (+org/close-fold)
    (error nil))
  (org-backward-heading-same-level 1)
  (+org/open-fold)
  (org-cycle-inline-images-display 'subtree)
  )

(defun my/next-and-open ()
  (interactive)
  (outline-next-heading)
  (org-fold-show-subtree)
  (org-cycle-inline-images-display 'subtree))

(defun my/prev-and-open ()
  (interactive)
  (outline-previous-heading)
  (org-fold-show-subtree)
  (org-cycle-inline-images-display 'subtree))

(map! :map 'org-mode-map
      ;;:nv "M-[" #'close-and-Open-prev
      ;;:nv "M-]" #'close-and-Open-next
      :nv "M-{" #'my/prev-and-open
      :nv "M-}" #'my/next-and-open
      :nv "M-o" #'my/open-subtree-only
      :nv "zp"  #'my/open-subtree-only
      :nv "M-c" #'+org/close-fold
      :nv "M-<tab>" #'outline-show-subtree
      )

(map! :mode org-mode
      :nv "z O" #'my/open-subtree-only)

;;;; z-H moves to top
(map! :mode org-mode
      :nv "zU" (lambda ()
                 (interactive)
                 (evil-org-top))
      :nv "U" (lambda (arg)
                (interactive "P")
                (org-up-heading (or arg 1))))
;;;; z-d does in org
(map! :mode org-mode
      :nv "zd" #'org-fold-hide-drawer-all)
;;;; z-y resets to show2levels
;; NOTE Gemini wrote this but it works so IDC
(map! :mode org-mode
      :nv "zy" (lambda (arg)
                 (interactive "P")
                 (org-content (or arg 2))))
;;;; org block templates
(map! :after org
      :mode org-mode
      :g "C-c C-x t" #'org-insert-block-template)
;;;; Denote refile subtree
(map! :mode org
      :leader "mrd" #'denote-org-extract-org-subtree)
;;;; custom reveal idk
(map! :mode org-mode
      :nv "zv" #'org-reveal ; mnemonic for reVeal idk...
      :nv "z <SPC>" #'org-reveal)
;;; yasnippet
(after! yasnippet
  (map! :map yas-keymap
        :i "<tab>" nil)
  (map! :map yas-minor-mode-map
        :i "<tab>" nil))

;;; TODO Run something at midnight idk
(use-package! midnight
  :config )
;;; Surround latex environments on export with "$$"
(defun my/surround-$$ (str backend channel)
  (if (eq backend 'md)
      (concat "$$\n" (string-trim str) "\n$$\n")
    nil))

(defun my/org-latex-figure-end-filter (str backend info)
  "Append \\setfloatalignment{b} before \\end{figure} in LaTeX export."
  (when (eq backend 'latex)
    (replace-regexp-in-string
     "\\\\end{figure}"
     "\\\\setfloatalignment{b}\n\\\\end{figure}"
     str)))

(defun my/FloatBarrier-on-headline (str backend info)
  (when (eq backend 'latex)
    (concat "\\FloatBarrier\n" str)))

(setq
 org-export-filter-final-output-functions '(my/org-latex-figure-end-filter)
 org-export-filter-latex-environment-functions '(my/surround-$$)
 org-export-filter-headline-functions '(my/FloatBarrier-on-headline))
;;; Evil
;; Normally evil doens't respect visual-line-mode. Hopefully this fixes it??
(after! evil
  (setq evil-respect-visual-line-mode nil)) ; TODO apparently this has to do with commands like $ and _. Do i want it??
;; I think this is actually rather annoying
                                        ;(map!
                                        ; :nv "j" #'evil-next-visual-line
                                        ; :nv "k" #'evil-previous-visual-line)

;;;; evil-scroll
(after! evil
  (setq! evil-scroll-count 0))
;;;; Bindings
(map! :v "zn" #'doom/narrow-buffer-indirectly)
(map! :map evil-window-map
      :g "O" #'delete-other-windows)
(map! :leader
      "tq" #'olivetti-mode)
(map! :nv "C-'" #'org-cycle-agenda-files)
;;; kdl-mode
(use-package! kdl-mode)
;;; rainbow-delimiters
(use-package! rainbow-delimiters
  :config
  (add-hook! 'prog-mode-hook #'rainbow-delimiters-mode))

;;; This will stop annoying errors that this function doesn't exist
(defun org--latex-preview-region (arg arga)
  (org-latex-preview 'section)
  )

;;; This warning is really fucking annoying
(add-to-list 'warning-suppress-types '(org-element))
;;;; emacs-lisp-mode config
(add-hook 'emacs-lisp-mode-hook #'show-paren-mode)



