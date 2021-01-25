(use-package diminish)

;; load configuration variables
(require 'init-config-variables)

;; use evil key bindings
(require 'init-evil)

;; use helm for navigation
(require 'init-helm)

;; initialize custom key bindings
(require 'init-key-bindings)

;; use treemacs
(use-package treemacs)

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

;; org mode
(with-eval-after-load 'org
    (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                   (file+headline ,(concat lotsohuggin-org-dir "/inbox.org") "Tasks")
                                   "* TODO %i%?")
                                  ("n" "Note" entry
                                   (file+headline ,(concat lotsohuggin-org-dir "/inbox.org") "Notes")
                                   "* %i%?")
                                  ("j" "Journal" entry
                                   (file+datetree ,(concat lotsohuggin-org-dir "/journal.org"))
                                   "* %?"
                                   :empty-lines 1)))

    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c c") 'org-capture)

    (setq org-todo-keywords
              (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                      (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                      (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))

    (setq org-agenda-files `(,(concat lotsohuggin-org-dir "/work.org")
                             ,(concat lotsohuggin-org-dir "/life.org")
                             ,(concat lotsohuggin-org-dir "/notes")))

    (setq org-archive-location "%s_archive::* Archive"))

;; evil
(with-eval-after-load 'evil
    (setq-default evil-escape-key-sequence "jk")
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))

(provide 'init-local)