
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(require 'org-id)
(setq org-directory "/Users/kpetrov/org")

(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(setq refile-settings-file "~/.emacs.d/refile.el")
(load refile-settings-file t)
