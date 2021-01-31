;;; lotsohuggin-utils.el --- Some utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun lotsohuggin/new-empty-frame ()
  "Opens a new frame with a buffer named 'untitled'."
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled")))

(provide 'lotsohuggin-utils)
