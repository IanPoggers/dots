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
;;

;;; custom vars
(setq my/monospace "Fira Code Nerd Font Mono"
      my/variable "Inter")

(setq! doom-font (font-spec :family my/monospace :size 17)
       doom-variable-pitch-font (font-spec :family my/variable :size 17))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual
      display-line-numbers-current-absolute nil)
(setq-default display-line-numbers-width 2)

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

;;;; set corfu delay
(after! corfu
  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq corfu-auto-delay 0.03
                    corfu-popupinfo-delay '(0.05 . 0.1)))))

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

  ;; NOTE DOOM WHY DO YOU DO THIS TO ME
  (require 'org-habit)

  (setq org-habit-show-habits-only-for-today t
        ;; I don't like the consistentcy graph, and don't want to see it.
        +org-habit-graph-window-ratio 0
        +org-habit-graph-padding 0)

  (cl-pushnew 'habit org-modules)

  (setq org-indirect-buffer-display 'current-window)

  (setq org-cycle-max-level nil)

  (setq org-indent-indentation-per-level 1)

  (setq org-use-fast-todo-selection nil)

  (setq org-agenda-files (cl-remove-duplicates (mapcar 'f-expand
                                                       (cl-union org-agenda-files
                                                                 (directory-files org-directory t ".org$")))))

  ;;(setq org-agenda-files (mapcar
  ;;                        (lambda (str) (f-join org-directory str))
  ;;                        '("habit.org" "todo.org" "Inbox.org" "reading.org"
  ;;                          "notes.org" )))

  :config
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  (setq org-cycle-emulate-tab t)

  (setq org-link-file-path-type 'relative)

  (setq org-highlight-latex-and-related '(native))

  (setq org-hide-emphasis-markers t)

  (setq org-agenda-time-grid nil)

  ;;(setq org-agenda-time-grid
  ;;      '((today require-timed remove-match)
  ;;        (0800 1200  1600 2000)
  ;;        "......" "----------------"))


  (setq
   org-cycle-emulate-tab nil

   org-cycle-inline-images-display t
   org-startup-with-inline-images t
   org-image-max-width 480
   org-image-align 'left

   org-startup-indented t))



;;;; org-export
(after! org
  (setq org-export-with-toc nil
        org-export-with-tags nil))
;;;; org-agenda
(after! org
  (setq
   org-agenda-start-day "today")

  (setq org-timestamp-custom-formats
        '("%m/%d %a" . "%m/%d %a %H:%M")
        org-display-custom-times t)

  (add-hook 'org-mode-hook 'org-toggle-timestamp-overlays)

  (setq org-agenda-todo-keyword-format "%1.1s "
        org-agenda-scheduled-leaders '("   " "%2dx")
        org-agenda-deadline-leaders '("Due" "%2dd" "%2dx")
        org-agenda-format-date " %b %d ─── %^a"
        org-agenda-prefix-format
        '((agenda . "%?-12t%s ")
          (todo . " %-8 c")
          (tags . " %-8 c")
          (search . " %-8 c")))

  (setq org-habit-show-habits t
        org-log-into-drawer t)

  (setq org-agenda-window-setup 'reorganize-frame)

  (map! :map 'evil-org-mode-map
        :nvi "C-M-S-<return>" #'org-insert-todo-subheading)

  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up deadline-up priority-down  category-keep)
          (todo urgency-down category-keep)
          (tags urgency-down category-keep)
          (search category-keep)))

  (setq org-agenda-dim-blocked-tasks nil)

  (setq org-agenda-overriding-header "")

  (setq org-agenda-skip-scheduled-repeats-after-deadline t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-prewarning-if-scheduled nil ;'pre-scheduled
        )

  (setq org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done nil)

  (setq org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t))
;;;;; org-super-agenda-groups
(setq
 my/class-selector '(:tag ("class" "LA" "DE" "CAL"))
 my/today-group
 '(:not (:and (:scheduled future
               :deadline future)
              :and (:deadline future
                    :scheduled nil)))
 my/today-nohabit `(:and (,@my/today-group :not (:habit t))))

(use-package! org-super-agenda
  :config
  (setq org-super-agenda-header-prefix "")

  (set-face-attribute 'org-super-agenda-header nil
                      :weight 'ultra-bold
                      :slant 'italic)

  (require 'evil-org-agenda)
  (setq org-super-agenda-header-map evil-org-agenda-mode-map)

  (org-super-agenda-mode)
  (setq org-agenda-span 1)
  (setq org-super-agenda-groups
        `(;; Events are not scheduled or deadlines.

          ;; Discard TOREAD enties that have no
          (:time-grid t)
          (:discard (:and (:todo "TOREAD"
                           :not (:scheduled t :deadline t))))
          (:name "Inbox" :category "Inbox"
           :order 0)
          ;;(:name "Class Deadlines"
          ;; :and (,@my/class-selector
          ;;       :deadline future
          ;;       :not (:scheduled past :scheduled today))
          ;; :order 3)
          (:name "\nDeadlines"
           :and (:deadline future
                 :not (:scheduled past :scheduled today))
           :order 4)
          (:name "" :auto-parent t
           :order 1)
          (:name "\nHabits" :take (8 (:habit t)) :order 2))))

;;;;; org-agenda commands
(after! org-agenda
  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
          ("'" agenda "")
          ("w" "Week View" agenda ""
           ((org-agenda-span 7)
            (org-deadline-warning-days 0)
            (org-agenda-start-on-weekday 1)
            (org-habit-show-habits nil)
            (org-super-agenda-groups nil))
           ("h" "Habits" agenda ""
            ((org-agenda-span 1)
             (org-agenda-overriding-header "")
             (org-agenda-sorting-strategy
              '((agenda priority-down timestamp-up)))
             (org-habit-show-habits-only-for-today nil)
             (org-habit-show-all-today nil)
             (org-habit-show-habits t)
             (org-super-agenda-groups
              `((:discard (:not (:habit t)))
                (:name ""
                 :and (,@my/today-group
                       :not (:scheduled future)))
                (:discard (:habit t))))))))))
;;;;; keywords
(after! org
  (setq org-todo-keywords '((type "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (type "KILL(k)"))))

;;;; prettify-symbols
(after! org
  (setq org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil)

  (add-hook 'org-mode-hook #'prettify-symbols-mode))
;;;;; evil-org-agenda-mode key mappings
(after! evil-org-agenda
  (map! :mode evil-org-agenda-mode
        :mvi "{" (λ! (evil-backward-paragraph 1)
                     (org-agenda-previous-item 1))
        :mvi "}" (λ! (evil-forward-paragraph 1)
                     (org-agenda-next-item 1))
        :mvi "q" nil
        :mvi "E" #'org-agenda-set-effort
        :mvi "'" #'org-agenda
        :mvi "w" #'org-save-all-org-buffers
        :mvi "S" #'org-agenda-schedule
        :mvi "R" #'org-agenda-refile
        ;; since the previous one overwrites the binding for
        :mvi "s\\" #'org-agenda-filter-remove-all
        :mvi "D" #'org-agenda-deadline
        :mvi "S-<return>" (λ! (org-agenda-switch-to t))
        :mvi "j" #'org-agenda-next-item
        :mvi "k" #'org-agenda-previous-item
        :mvi "<down>" #'org-agenda-next-item
        :mvi "<up>" #'org-agenda-previous-item
        :mvi "s/" #'org-agenda-filter
        :leader "k" #'org-habit-toggle-habits)

  ;; really disable "q" to quit agenda... i keep killing it on accident lol
  (map! :map 'org-agenda-keymap "q" nil
        :map 'org-agenda-mode-map "q" nil))



;;;; bindings
(defun my/org-tree-to-indirect-buffer-default-C-u-prefix (arg)
  (interactive "P")
  (org-tree-to-indirect-buffer (or arg '(4))))

(after! org
  (map! :map 'override
        :leader
        "'" #'org-agenda)

  (map! :map 'evil-org-mode-map
        :i "TAB" #'cdlatex-tab
        :nv "]h" #'org-forward-heading-same-level
        :nv "[h" #'org-backward-heading-same-level
        :nv "zn" #'my/org-tree-to-indirect-buffer-default-C-u-prefix)

  (map! :leader
        "n/" #'consult-org-agenda
        "nh" #'+default/org-notes-headlines))

;;;; disable corfu in org-mode
(after! org
  (add-hook 'org-mode-hook (λ! (corfu-mode -1))))

;;;; org-capture-templates
(after! org
  (setq org-capture-templates
        '(("t" "Unscheduled todo" entry (file "Inbox.org")
           "* TODO %?\n%i\n" :prepend t)
          ("T" "Unscheduled Linked todo" entry (file "Inbox.org")
           "* TODO %? %A\n%i" :prepend t)
          ("s" "Scheduled todo" entry (file "Inbox.org")
           "* TODO %?\nSCHEDULED: %^t\n%i\n" :prepend t)
          ("S" "Linked Scheduled todo" entry (file "Inbox.org")
           "* TODO %? %A\nSCHEDULED: %^t\n%i" :prepend t)
          ("h" "Personal habit" entry (file "habit.org")
           "* TODO [#D] %?\nSCHEDULED: <%<%Y-%m-%d %a> .+1d>\n:PROPERTIES:\n:STYLE: habit\n:END:\n%i\n" :prepend t)
          ("n" "Personal notes" entry (file "Inbox.org")
           "* %?\n%i\n" :prepend t)
          ("N" "Linked note" entry (file "Inbox.org")
           "* %? %A\n%i" :prepend t)
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
          ("l" "Linear Algebra Todo" entry (file "Inbox.org")
           "* TODO Lin Alg: %? :LA:\n%i\n" :prepend t)
          ("c" "Calc Todo" entry (file "Inbox.org")
           "* TODO Calculus: %? :calc:\n%i\n" :prepend t)
          ("d" "Diff Eqns Todo" entry (file "Inbox.org")
           "* TODO Diff Eqn: %? :DE:\n%i\n" :prepend t)
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry #'+org-capture-central-project-todo-file
           "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry #'+org-capture-central-project-notes-file
           "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file
           "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
          ("r" "Reading Inbox" entry (file+headline "reading.org" "Inbox")
           "* TOREAD %?\n%^{AUTHOR}p\n%i\n" :prepend t)
          ("m" "Movie Inbox" entry (file "movies.org")
           "* TODO %?\n%^{Tomato}p\n%^{IMDb}p" :prepend t))))

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

(use-package! cdlatex
  :config
  (setq cdlatex-simplify-sub-super-scripts nil)

  ;; holy cow i hate how it inserts brackets kill me
  (map! :map 'org-cdlatex-mode-map
        "^" nil
        "_" nil))

;;;; org-latex-preview
(map! :mode org
      :leader "cl" (λ! (org-latex-preview (or current-prefix-arg '(16)))))
;;;; This is necssary cuz doom is gay
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  (setq org-cycle-emulate-tab nil))
;;;; Bind to quickly traverse up headings
(map! :mode org-mode
      :nv "zU" (lambda ()
                 (interactive)
                 (evil-org-top))
      :nv "U" (λ! (org-up-heading-or-point-min)))
;;;; org bindings idk
(map! :mode org-mode
      :nv "zv" #'org-reveal)

;;;;; ox-latex
(use-package! ox-latex
  :after org
  :config
  (dolist (pkg '("placeins" "amsmath" "amssymb" "amsthm" "mathtools" "mathrsfs" "pgfplots" "float" "siunitx" "xfrac" "cancel"))
    (add-to-list 'org-latex-packages-alist `("" ,pkg t)))

  (add-to-list 'org-latex-classes
               '("tufte-handout"
                 ;; NOTE latex header goes here:
                 "\\documentclass[letterpaper]{tufte-handout}
\\usepackage{marginfix}
\\setlength{\\marginparpush}{1.8em}
\\usepackage{geometry}
\\geometry{
  left=0.8in,
  textheight=9in,
  marginparsep=0.25in, % gap between text and notes
  marginparwidth=2.4in % width of the sidenote area
}
\\pagestyle{empty}"
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

;;;; focus.el in org-mode
(use-package! focus
  :config
  ;;  (mapcar (lambda (mode) (add-hook mode 'focus-mode))
  ;;          '(org-mode-hook emacs-lisp-mode-hook LaTeX-mode-hook))


  ;; TODO customize this so that stuff is visible but clearly not focused
  (set-face-attribute 'focus-unfocused nil))
;;;; text should be a little bigger in org-mode
;; Since I am going to be spending a considerable amount of time reading
;; prose in this mode, I'd like it if the text is larger on the screen
(after! org
  (setq my/org-text-height 1.10)
  (add-hook 'org-mode-hook (λ! (face-remap-add-relative 'default :height my/org-text-height)))
  (add-hook 'prog-mode-hook (λ! (face-remap-add-relative 'default :height 1.05)))
  (add-hook 'TeX-mode-hook (λ! (face-remap-add-relative 'default :height 1.1)))
  )
;;;;; custom function to sort entries under toplevel headings
(defun my/custom-org-entry-sort-algo ()
  (interactive)
  (ignore-errors (org-sort-entries nil ?t))
  (ignore-errors (org-sort-entries nil ?p))
  (ignore-errors (org-sort-entries nil ?o))
  ;; Put NEXT entries to the top, even if they
  ;; aren't the first in the keyword series.
  (ignore-errors (org-sort-entries nil ?f
                                   (lambda ()
                                     (if (string= (org-get-todo-state) "NEXT")
                                         0 1))
                                   '<)))

(defun my/sort-top-level ()
  (interactive)
  (org-map-entries 'my/custom-org-entry-sort-algo
                   "LEVEL=1")
  (org-cycle '(16))
  (org-map-entries (λ! (unless (org-invisible-p)
                     (outline-show-entry)))
                   "LEVEL=2"))

(map! :map 'org-mode-map
      :g "C-c s" #'my/sort-top-level)

;;; Aesthetic Stuff
;;;; Mixed-pitch
(use-package! mixed-pitch
  :config
  ;;(add-hook 'org-mode-hook 'mixed-pitch-mode)
  (pushnew! mixed-pitch-fixed-pitch-faces
            'org-superstar-leading 'org-date
            'font-lock-comment-face
            'org-list-dt 'org-document-info
            'org-property-value 'org-special-keyword
            'org-drawer 'org-cite-key  'org-hide
            'org-link
            'corfu-default)

  (setq mixed-pitch-set-height nil))

(after! org
  (dolist (face '(org-todo error warning org-link))
    (set-face-attribute face nil :family my/monospace))

  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4
                  org-level-5 org-level-6 org-level-7 org-level-8
                  org-document-title))
    (set-face-attribute face nil
                        :height 1.00
                        :family my/monospace)))




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

;;; INBOX
;;;; olivetti-mode
(use-package! olivetti
  :config
  (add-hook 'org-mode-hook #'olivetti-mode)

  (setq-default olivetti-body-width 100)

  (map! :leader "tq" #'olivetti-mode))
;;;; disable projectile caching cuz it makes phantom files show up in find-file
(setq projectile-enable-caching nil)
;;; Don't display output of async cmds with no output.
(setq async-shell-command-display-buffer nil)

;;;; org-pomodoro and org-clock
(use-package! org-pomodoro
  :config
  (setq
   org-clock-sound "~/.config/doom/assets/chime.wav"
   org-pomodoro-format "Pomo~%s"
   org-clock-persist t

   org-pomodoro-start-sound-p t
   org-pomodoro-start-sound "~/.config/doom/assets/gong.mp3"
   org-pomodoro-finished-sound "~/.config/doom/assets/gong.mp3"
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
   org-pomodoro-tick-hook nil

   ;; I just find it annoying that the stop sound and the tick
   ;; play at the same time when the pomo length is an integer
   ;; multiple of the tick time, so the frequency is 5 min + 1 second
   ;; so that this does not happen. The tick will come 6 seconds late
   ;; after 30 minutes: (/ 30 5) => 6
   org-pomodoro-ticking-frequency (+ (* 60 5) 2)
   org-pomodoro-ticking-sound "~/.config/doom/assets/tick.wav")

  (map! :map 'org-agenda-mode-map
        :e "P" #'org-pomodoro)
  (map! :leader
        "tp" #'org-pomodoro))

(use-package! org-clock
  :config
  (setq org-clock-sound "~/.config/doom/assets/bell.wav"))

(map! :leader
      "tT" #'org-timer-set-timer)

;;;; TODO idk
(add-hook 'org-after-sorting-entries-or-items-hook
          (λ!
           (+org/close-fold)
           (+org/open-fold)))
;;;; custom bindings
(map! :map 'evil-window-map
      "O" #'delete-other-windows)

;;; org-roam
(use-package! org-roam
  :init
  (setq org-roam-directory (f-join org-directory "Roam")))

;;; consult-notes
(use-package! consult-notes
  :config
  (org-roam-db-sync)

  (setq consult-notes-file-dir-sources
        `(("Org" ?o ,org-directory)))

  (consult-notes-org-headings-mode)
  (consult-notes-org-roam-mode)

  (map! :leader
        "ns" #'consult-notes-search-in-all-notes
        "n'" #'consult-notes))
;;; detached
(use-package! detached
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach nil)
           (detached-show-output-command nil)
           (detached-terminal-data-command system-type)))



;;; focus-mode
(use-package! focus
  :config
  (map! :nv "zf" #'focus-mode))

;;; hl-line
(add-hook 'org-mode-hook (λ! (hl-line-mode -1)))


;;; org-anki
(use-package! org-anki)

;;; org-latex-prview (copied from old config)
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
  (add-hook! 'org-mode-hook (lambda () (org-latex-preview 'buffer)))
  (add-hook 'org-mode-hook 'org-latex-preview-mode))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (org-latex-preview 'buffer))))


(after! org-latex-preview
  (add-hook 'org-mode-hook 'org-latex-preview-center-mode)
  (add-hook 'org-mode-hook 'org-latex-preview-mode))

;;; org-ql
(use-package! org-ql
  :config
  (add-to-list 'warning-suppress-types '(org-ql)))

;;; TODO
(setq org-id-link-to-org-use-id t)
;;; theming org-mode
(after! org
  (set-face-attribute 'org-block nil
                      :extend t)
  (set-face-attribute 'org-quote nil
                      :extend t))

;;; TODO This will stop annoying errors that this function doesn't exist
(defun org--latex-preview-region (arg arga)
  (org-latex-preview 'section))

;;; org-babel-rust
(use-package! rustic-babel
  :config
  (setq rustic-babel-format-src-block t))

;;; TODO org-ellipsis should have non-breaking space
(after! org
  (setq org-ellipsis " [...]"
        +fold-ellipsis " [...]"))
