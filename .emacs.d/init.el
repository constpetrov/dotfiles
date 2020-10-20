(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

;;Visible bell
(setq visible-bell t)

;;Font
(set-face-attribute 'default nil :font "Iosevka SS08" :height 160)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Add russian layout in emacs, independent from system
(setq default-input-method "russian-computer")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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
  :custom ((doom-modeline-height 10)))

;; Doom themes
(use-package doom-themes
  :init (load-theme 'doom-tomorrow-night t))

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
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
   "c" '(counsel-org-capture :which-key "capture something")
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

(use-package evil-magit
  :after magit)

;; Forge
(use-package forge)

;; Org settings
(defun kostia/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (add-to-list 'org-modules 'org-habit t)
  (defun air-org-skip-subtree-if-priority (priority))

(defun kostia/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Iosevka SS08" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . kostia/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (kostia/org-font-setup)
  :bind (
	 :map org-mode-map
	 ("C-c C-q" . counsel-org-tag)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun kostia/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . kostia/org-mode-visual-fill))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((tags "PRIORITY=\"A\""
	     ((org-agenda-skip-function
	       '(org-agenda-skip-entry-if 'todo 'done))
	      (org-agenda-overriding-header "High-priority unfinished tasks:")))
       (agenda ""
	       ((org-agenda-span 2)))
       (alltodo ""
		((org-agenda-skip-function
		  '(or
		    (air-org-skip-subtree-if-priority 65)
		    (org-agenda-skip-if nil
					'(scheduled deadline))))))))))

(setq org-agenda-files
      '("~/org/misc.org"
	"~/org/inbox.org"
	"~/org/journal.org"
	"~/org/periodic.org"
	"~/org/habits.org"
	"~/org/ripe.org"
	"~/org/bike.org"
	"~/org/language.org"
	"~/org/food.org"
	"~/org/home.org"))

(setq org-capture-templates
   '(("t" "Todo" entry
      (file "~/org/inbox.org")
      "* TODO %?" :kill-buffer t)
     ("j" "Journal" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?
Entered on %U" :kill-buffer t)
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
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
(use-package yaml-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode visual-fill-column org-bullets org forge evil-magit magit-evil magit counsel-projectile projectile hydra evil-collection evil general doom-themes helpful which-key rainbow-delimiters use-package ivy-rich ivy-omni-org doom-modeline counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
