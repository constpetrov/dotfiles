(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes '(light-blue))
 '(default-input-method "russian-computer")
 
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((tags "PRIORITY=\"A\""
	     ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
	      (org-agenda-overriding-header "High-priority unfinished tasks:")))
       (agenda ""
	       ((org-agenda-span 'day)))
       (alltodo ""
		((org-agenda-skip-function
		  '(or (air-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))))))))
 '(org-agenda-files
   '("~/org/misc.org" "~/org/inbox.org" "~/org/journal.org" "~/org/periodic.org" "~/org/habits.org" "~/org/ripe.org"))
 '(org-capture-templates
   '(("t" "Todo" entry
      (file "~/org/inbox.org")
      "* TODO %?" :kill-buffer t)
     ("j" "Journal" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?%^g
Entered on %U" :kill-buffer t)
     ("n" "Note" entry
      (file+headline "~/org/misc.org" "Notes")
      "* %?%^g
Entered on %U" :jump-to-captured t :kill-buffer t)))
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
