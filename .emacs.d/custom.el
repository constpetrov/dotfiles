(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/misc.org" "~/org/inbox.org")))
 '(package-selected-packages (quote (org))))
(setq org-capture-templates
      '(("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?%^g\nEntered on %U")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
