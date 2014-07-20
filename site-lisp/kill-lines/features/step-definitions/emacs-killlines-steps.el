;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I kill until line \\(.+\\)$"
     (lambda (line)
       (kill-to-line (string-to-number line))))

(When "^I turn off linum-mode$"
      (lambda ()
        (linum-mode -1)))

(When "^I turn off hl-line-mode$"
      (lambda ()
        (hl-line-mode -1)))

(Then "^linum should be \"\\(.+\\)\"$"
      (lambda (flag)
        (if (string= flag "on")
            (assert linum-overlays)
          (assert (eq nil linum-overlays)))))

(Then "^hl-line should be \"\\(.+\\)\"$"
      (lambda (flag)
        (if (string= flag "on")
            (assert hl-line-mode)
          (assert (eq nil hl-line-mode)))))
