;;; init-helm.el --- Use helm for narrowing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-config-variables)

(defun helm-init-keybindings ()
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (with-eval-after-load 'helm-files (dolist (keymap (list helm-find-files-map helm-read-file-map))
                                      (define-key keymap (kbd "C-l")
                                        'helm-execute-persistent-action)
                                      (define-key keymap (kbd "C-h")
                                        'helm-find-files-up-one-level))))

(defvar spacemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
(defvar spacemacs-helm-display-buffer-regexp
  `("*.*helm.**"
    (display-buffer-in-side-window)
    (inhibit-same-window . t)
    (side . ,lotsohuggin-helm-position)
    (window-width . 0.6)
    (window-height . 0.4)))

(defun spacemacs//display-helm-window (buffer &optional resume)
  "Display the Helm window respecting `lotsohuggin-helm-position-helm-position'."
  (let ((display-buffer-alist
         (list spacemacs-helm-display-help-buffer-regexp
               ;; this or any specialized case of Helm buffer must be
               ;; added AFTER `spacemacs-helm-display-buffer-regexp'.
               ;; Otherwise, `spacemacs-helm-display-buffer-regexp' will
               ;; be used before
               ;; `spacemacs-helm-display-help-buffer-regexp' and display
               ;; configuration for normal Helm buffer is applied for helm
               ;; help buffer, making the help buffer unable to be
               ;; displayed.
               spacemacs-helm-display-buffer-regexp)))
    (helm-default-display-buffer buffer)))

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(use-package
  helm
  :diminish 'helm-mode
  :config
  (require 'helm-config)
  (helm-mode 1)
  ;; disable autoresize otherwise interactive search is hard to use
  (helm-autoresize-mode 0))

(setq helm-prevent-escaping-from-minibuffer t
      helm-bookmark-show-location 'bottom
      helm-display-header-line nil
      helm-split-window-in-side-p t
      helm-always-two-windows t
      helm-echo-input-in-header-line t
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-imenu-execute-action-at-once-if-one nil
      helm-display-function 'spacemacs//display-helm-window)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(helm-init-keybindings)

(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(use-package helm-projectile
  :after (helm projectile)
  :config
  (add-hook 'after-init-hook 'helm-projectile-on))

(use-package helm-swoop
  :config
  (setq helm-swoop-move-to-line-cycle t))

(use-package helm-ag
  :after ag
  :init
  ;; use M-x customize to set helm-follow-mode-persistent instead
  ;; enable follow mode to get preview of files automatically
  ;; (custom-set-variables
  ;;  '(helm-follow-mode-persistent t))
  :config
  ;; ;; to change backend search tool
  ;; (custom-set-variables
  ;;   '(helm-ag-base-command "rg --no-heading"))
  )

;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'emacs-lisp-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-use-input-at-cursor t)
 '(helm-gtags-auto-update t))

(use-package helm-gtags
  :diminish 'helm-gtags-mode
  :config
  (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))

;; settings for helm semantics and imenu
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(provide 'init-helm)
;;; init-helm.el ends here
