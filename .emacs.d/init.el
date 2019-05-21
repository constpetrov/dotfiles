(setq org-directory "/Users/kpetrov/own/org")

(eval-after-load "org" '(progn
			      (define-key org-mode-map (kbd "M-S-<RET>") 'org-insert-todo-heading)
			      ))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
