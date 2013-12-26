;;; Compiled snippets and support files for `udev-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'udev-mode
                     '(("env" "ENV{$1}$0" "ENV" nil nil nil nil nil nil)
                       ("goto" "GOTO=\"$1\"\n$0\n\nLABEL=\"$1\"" "GOTO" nil nil nil nil nil nil)
                       ("ker" "KERNEL!=\"$0\"" "KERNEL" nil nil nil nil nil nil)
                       ("add" "ACTION==\"add\", $0" "add" nil nil nil nil nil nil)
                       ("$" "$env{$1} $0" "env$" nil nil nil nil nil nil)
                       ("run" "RUN+=\"$0\"" "run" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Dec 26 11:33:14 2013
