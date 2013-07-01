(require 'linum)
(require 'hl-line)

(defun kill-to-line (target-line)
  (if (or (< target-line 1) (> target-line (line-number-at-pos (point-max))))
      (message (concat "Line number " (number-to-string target-line)
                       " out of range: 1, "
                       (number-to-string (line-number-at-pos (point-max)))))
    (let ((range (- target-line (line-number-at-pos))))
      (kill-whole-line (if (> range 0) (1+ range) (1- range))))))

(defun kill-lines ()
  (interactive)
  (let ((already-numbered-p linum-overlays)
        (already-hl-line-p hl-line-mode))
    (narrow-to-region (window-start) (window-end))
    (unless already-numbered-p (linum-mode 1))
    (unless already-hl-line-p (hl-line-mode 1))
    (let ((inhibit-quit t))
      (unless (with-local-quit
                (kill-to-line (read-number "Kill until which line? "))
                t)
        (progn
          (setq quit-flag nil))))
    (unless already-hl-line-p (hl-line-mode -1))
    (unless already-numbered-p (linum-mode -1))
    (widen)))

(provide 'kill-lines)
