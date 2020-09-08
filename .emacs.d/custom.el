(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (light-blue)))
 '(default-input-method "russian-computer")
 '(org-agenda-files
   (quote
    ("~/org/misc.org" "~/org/inbox.org" "~/org/journal.org" "~/org/periodic.org" "~/org/habits.org" "~/org/ripe.org")))
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file "~/org/inbox.org")
      "* TODO %?" :kill-buffer t)
     ("j" "Journal" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?%^g
Entered on %U" :kill-buffer t)
     ("n" "Note" entry
      (file+headline "~/org/misc.org" "Notes")
      "* %?%^g
Entered on %U" :jump-to-captured t :kill-buffer t))))
 '(org-id-link-to-org-use-id (quote create-if-interactive))
 '(org-id-method (quote uuid))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-refile-targets (quote ((org-agenda-files :tag . ""))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO" "NEXT" "INACTIVE" "WAITING" "CANCELLED" "DONE"))))
 '(package-selected-packages (quote (org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
