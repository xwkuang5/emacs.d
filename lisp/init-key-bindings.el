;;; init-key-bindings.el --- Attempt my own spacemacs-like key bindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-config-variables)
(require 'spacemacs-utils)
(require 'lotsohuggin-utils)

;;----------------------------------------------------------------------------
;; The following show how spacemacs uses bind-map to bind a leader key to a
;; keymap and define command sequences on the keymap
;;----------------------------------------------------------------------------
;; (defvar lotsohuggin-default-map (make-sparse-keymap)
;;   "Base keymap for all leader key commands.")

;; (defun lotsohuggin/set-leader-keys (key def &rest bindings)
;;   "Add KEY and DEF as key bindings"
;;   (while key
;;     (define-key spacemacs-default-map (kbd key) def)
;;     (setq key (pop bindings) def (pop bindings))))

;; (defun lotsohuggin/declare-prefix (prefix name &optional long-name)
;;   (let* ((command name)
;;          (full-prefix (concat lotsohuggin-leader-key " " prefix)))
;;     ;; define the prefix command only if it does not already exist
;;     (unless long-name
;;       (setq long-name name))
;;     (which-key-declare-prefixes full-prefix (cons name long-name))))

;; ;; use which-key for shortcut menu
;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode)
;;   (setq which-key-idle-delay lotsohuggin-which-key-delay)
;;   (setq prefixes '(("f" "file")
;;                    ("s" "search")
;;                    ("b" "buffer")
;;                    ("p" "projectile")
;;                    ("g" "git")
;;                    ("j" "jump")
;;                    ("w" "window")))
;;   (mapc (lambda (x)
;;           (apply 'lotsohuggin/declare-prefix x)) prefixes))

;; (use-package bind-map
;;   :config
;;   (bind-map lotsohuggin-default-map
;;     :evil-keys (lotsohuggin-leader-key)
;;     :evil-states (normal motion visual)))

;;----------------------------------------------------------------------------
;; The same thing can be accomplished with general
;;----------------------------------------------------------------------------

;; keymaps where the leader bindings are enabled
(setq leader-binding-states '(normal visual motion))

(defun lotsohuggin/init-key-bindings()

  (general-define-key
   :prefix lotsohuggin-leader-key
   :keymaps leader-binding-states
   "" '(nil :wk "my lieutenant general prefix")
   "f" '(:ignore t :wk "files")
   "b" '(:ignore t :wk "buffers")
   "s" '(:ignore t :wk "search")
   "t" '(:ignore t :wk "tag")
   "w" '(:ignore t :wk "window")
   "c" '(:ignore t :wk "compile/comment")
   "p" '(:keymap projectile-command-map :wk "projectile" :package projectile))

  (general-create-definer lotsohuggin-leader-def
    :prefix lotsohuggin-leader-key :non-normal-prefix "M-SPC"))

(defun lotsohuggin/init-buffer-key-bindings()
  (lotsohuggin-leader-def
    :states leader-binding-states
    "bb" 'helm-mini
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "bd" 'spacemacs/kill-this-buffer
    "bc" 'lotsohuggin/new-empty-frame
    "bk" 'kill-buffer))

(defun lotsohuggin/init-window-key-bindings()
  "window key bindings")

(defun lotsohuggin/init-compile-or-comment-key-bindings()
  "compile or comment key bindings"
  (lotsohuggin-leader-def
    :states leader-binding-states
    "cl" 'evilnc-comment-or-uncomment-lines))

(defun lotsohuggin/init-tag-key-bindings()
  (lotsohuggin-leader-def
    :states leader-binding-states
    "tb" 'helm-semantic-or-imenu
    "td" 'helm-gtags-find-tag
    "tr" 'helm-gtags-find-rtag
    "tn" 'helm-gtags-next-history
    "tp" 'helm-gtags-previous-history
    "t`" 'helm-gtags-pop-stack
    "tc" 'helm-gtags-create-tags
    "tu" 'helm-gtags-update-tags))

(defun lotsohuggin/helm-do-ag-project-root (&optional pattern)
  (interactive)
  ;; use the word at point
  (unless pattern
    (setq pattern (thing-at-point 'word 'no-properties)))
  (helm-do-ag-project-root pattern))

(defun lotsohuggin/init-search-key-bindings()
  (lotsohuggin-leader-def
   :states leader-binding-states
   "s`" 'helm-ag-pop-stack
   "sp" 'lotsohuggin/helm-do-ag-project-root
   "sb" 'helm-do-ag-buffers
   "sd" 'helm-do-ag
   "ss" 'helm-swoop))

(defun lotsohuggin/init-file-key-bindings()
  (lotsohuggin-leader-def
   :states leader-binding-states
   "fs" 'save-buffer
   "ft" 'treemacs
   "fr" 'helm-recentf
   "fR" 'spacemacs/rename-current-buffer-file))

(defun lotsohuggin/init-evil-key-bindings()
  (general-imap "j"
                ;; this approach can be used to intercept key dispatch
                (general-key-dispatch 'self-insert-command
                                      :timeout 0.25
                                      "k" 'evil-normal-state)))

(use-package which-key
  :diminish which-key-mode
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay lotsohuggin-which-key-delay))

;; use general for key bindings
(use-package general
  :ensure t
  :config
  ;; auto unbind to avoid prefix key error
  (general-auto-unbind-keys)
  (general-evil-setup))

(lotsohuggin/init-key-bindings)
(lotsohuggin/init-buffer-key-bindings)
(lotsohuggin/init-tag-key-bindings)
(lotsohuggin/init-search-key-bindings)
(lotsohuggin/init-file-key-bindings)
(lotsohuggin/init-evil-key-bindings)
(lotsohuggin/init-compile-or-comment-key-bindings)

(provide 'init-key-bindings)
