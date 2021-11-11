(setq inhibit-startup-message t)
(setq create-lockfiles nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

;;Size of the window
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 160))

;;Visible bell
(setq visible-bell t)

;;Font
(set-face-attribute 'default nil :font "Iosevka Curly" :height 160)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Add russian layout in emacs, independent from system
(setq default-input-method "russian-computer")
(setq org-roam-v2-ack t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Add my own directory with packages
(add-to-list 'load-path "~/projects/emacs/wc-mode/")
(require 'wc-mode)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; Search and complete engine Ivy
(use-package ivy
  :init (ivy-mode 1) ;; this will enable the mode on loading. What is in :config happens only when mode is first started.
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill)))

;; Icons
;; run all-the-icons-install-fonts first time when it is loaded
(use-package all-the-icons)

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)
	   (doom-modeline-enable-word-count t)))

;; Doom themes
(use-package doom-themes
  :init (load-theme 'doom-flatwhite t))

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		org-agenda-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which key
(use-package which-key
  :init (which-key-mode)
  :diminish which-kew-mode
  :config
  (setq which-key-idle-delay 0.3))

;; ivy rich
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c c" . counsel-org-capture)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)
	 ("M-RET" . 'counsel-org-tag-action))
  :config
  (setq ivy-initial-inputs-alist nil))

;; Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; General
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer kostia/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (kostia/leader-keys
    "o" '(:ignore t :which-key "Org functions")
    "os" '(org-save-all-org-buffers :which-key "Save all org buffers")
    "b" '(:ignore t :which-key "Buffer functions")
    "bk" '(kill-buffer-and-window :which-key "Kill buffer and window")
    "bs" '(save-buffer :which-key "Save buffer")
    "bl" '(counsel-ibuffer :which-key "List buffers")
    "f" '(:ignore t :which-key "File functions")
    "ff" '(counsel-find-file :which-key "Find file")
    "c" '(counsel-org-capture :which-key "capture something")
    "n" '(:ignore t :which-key "Roam")
    "ni" '(org-roam-node-insert :which-key "Insert node")
    "nf" '(org-roam-node-find :which-key "Find node")
    "nc" '(org-roam-capture :which-key "Capture")
    "nt" '(org-roam-buffer-toggle :which-key "Toggle buffer")
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(general-define-key
 "C-M-j" 'counsel-switch-buffer)
(general-define-key
 "C-c a" 'org-agenda)

;; Evil mode
(defun kostia/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (evil-mode 1)
  :hook (evil-mode . kostia/evil-hook)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Hydra
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(kostia/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Forge
(use-package forge)

;; Org settings
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))
(setq org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((tags "PRIORITY=\"A\""
	     ((org-agenda-skip-function
	       '(org-agenda-skip-entry-if 'todo 'done))
	      (org-agenda-overriding-header "High-priority unfinished tasks:")))
       (agenda ""
	       ((org-agenda-span 2)))
       (alltodo ""
		((org-agenda-sorting-strategy '(todo-state-up))
		 (org-agenda-skip-function
		  '(or
		    (air-org-skip-subtree-if-priority 65)
		    (org-agenda-skip-if nil
					'(scheduled deadline))))))))))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-agenda-files (directory-files-recursively "~/org/" "\.org$"))
;;(setq org-agenda-files
;;      '("~/org/misc.org"
;;	"~/org/inbox.org"
;;	"~/org/journal.org"
;;	"~/org/periodic.org"
;;	"~/org/habits.org"
;;	"~/org/ripe.org"
;;	"~/org/bike.org"
;;	"~/org/language.org"
;;	"~/org/food.org"
;;	"~/org/health.org"
;;	"~/org/blogging.org"
;;	"~/org/home.org"))

(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)
(setq org-capture-templates
   '(("t" "Todo" entry
      (file "~/org/inbox.org")
      "* TODO %?" :kill-buffer t)
     ("j" "Journal" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?
Entered on %U" :kill-buffer t :created t)
     ("n" "Note" entry
      (file+headline "~/org/misc.org" "Notes")
      "* %?
Entered on %U" :jump-to-captured t :kill-buffer t)))

(setq org-id-link-to-org-use-id 'create-if-interactive)
(setq org-id-method 'uuid)
(setq org-refile-targets '((org-agenda-files :maxlevel . 9)
			   ("archive.org" :maxlevel . 3)))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("HOLD" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)
	("MEETING" :foreground "forest green" :weight bold)
	("PHONE" :foreground "forest green" :weight bold)))
(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
(defun kostia/org-mode-setup ()
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)

;; For saving files after inserting links
;;  (defun save-after-link-store ()
;;    (interactive)
;;    (call-interactively 'org-store-link)
;;    (save-buffer))
;;  (global-set-key (kbd C-c l") 'save-after-link-store)
;;
;;  (define-key org-mode-map (kbd "C-c C-l") nil)
;;  (defun save-after-insert-link ()
;;    (interactive)
;;    (call-interactively 'org-insert-link)
;;    (save-buffer))
;;  (global-set-key (kbd "C-c C-l") 'save-after-insert-link)
)

(use-package org
  :hook (org-mode . kostia/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (add-to-list 'org-modules 'org-habit t)
  :bind (
	 :map org-mode-map
	 ("C-c C-q" . counsel-org-tag)))

;; Use wc-mode in org
(add-hook 'org-mode-hook 'wc-mode)

;; Evil mode in org
(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Installing and setting org-roam
(use-package org-roam
  :ensure t
  :after org
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (file-truename "~/org"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      (file "~/org/templates/book_notes.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :book:книга:")
      :unnarrowed t)
     )
   )
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-dailies)
  (setq org-roam-link-use-custom-faces t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
	 :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
	 ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map))

(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                ))
  (select-frame-by-name "remember")
  (org-capture))

;; For YAML editing
(use-package yaml-mode)

;; For sml-nj, coursera course on programming languages, part A
(use-package sml-mode
  :config
  (setenv "PATH" (concat "/usr/local/smlnj/bin:" (getenv "PATH")))
  (setq exec-path (cons "/usr/local/smlnj/bin" exec-path)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/Users/kpetrov/org/Zoom.org" "/Users/kpetrov/org/archive.org" "/Users/kpetrov/org/bike.org" "/Users/kpetrov/org/blogging.org" "/Users/kpetrov/org/checklist.org" "/Users/kpetrov/org/documents.org" "/Users/kpetrov/org/food.org" "/Users/kpetrov/org/habits.org" "/Users/kpetrov/org/health.org" "/Users/kpetrov/org/home.org" "/Users/kpetrov/org/inbox.org" "/Users/kpetrov/org/init.org" "/Users/kpetrov/org/journal.org" "/Users/kpetrov/org/language.org" "/Users/kpetrov/org/misc.org" "/Users/kpetrov/org/my_family.org" "/Users/kpetrov/org/periodic.org" "/Users/kpetrov/org/ripe.org"))
 '(package-selected-packages
   '(evil-surround auto-package-update sml-mode evil-magit yaml-mode which-key visual-fill-column use-package rainbow-delimiters org-bullets ivy-rich hydra helpful general forge evil-org evil-collection doom-themes doom-modeline counsel-projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
