;;; init-evil.el --- Use evil-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun evil-init()
  ;; use <C-u> for scrolling up instead of universal argument
  (when evil-want-C-u-scroll
    (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode))

(provide 'init-evil)
;;; init-evil.el ends here
