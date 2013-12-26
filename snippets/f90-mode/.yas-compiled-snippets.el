;;; Compiled snippets and support files for `f90-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'f90-mode
                     '(("au" "automatic $0 \n" "automatic" nil nil nil nil nil nil)
                       ("bd" "block data $0\n" "block data" nil nil nil nil nil nil)
                       ("c" "continue $0\n" "continue" nil nil nil nil nil nil)
                       ("ch" "character $0\n" "character" nil nil nil nil nil nil)
                       ("cx" "complex $0\n" "complex" nil nil nil nil nil nil)
                       ("dc" "double complex $0\n" "double complex" nil nil nil nil nil nil)
                       ("do" "do while (${1:condition})\n   $0\nend do\n" "do while (...) end do" nil nil nil nil nil nil)
                       ("dp" "double precision $0\n" "double precision" nil nil nil nil nil nil)
                       ("eq" "equivalence $0\n" "equivalence" nil nil nil nil nil nil)
                       ("ib" "implicit byte $0\n" "implicit byte" nil nil nil nil nil nil)
                       ("ic" "implicit complex $0\n" "implicit complex" nil nil nil nil nil nil)
                       ("ich" "implicit character $0\n" "implicit character" nil nil nil nil nil nil)
                       ("if" "if ( ${1:condition} ) then\n   $0\nend if\n" "if then end if" nil nil nil nil nil nil)
                       ("ii" "implicit integer $0\n" "implicit integer " nil nil nil nil nil nil)
                       ("il" "implicit logical $0\n" "implicit logical" nil nil nil nil nil nil)
                       ("in" "implicit none\n" "implicit none" nil nil nil nil nil nil)
                       ("inc" "include $0\n" "include" nil nil nil nil nil nil)
                       ("intr" "intrinsic $0\n" "intrinsic" nil nil nil nil nil nil)
                       ("ir" "implicit real $0\n" "implicit real" nil nil nil nil nil nil)
                       ("l" "logical $0\n" "logical" nil nil nil nil nil nil)
                       ("pa" "parameter $0\n" "parameter" nil nil nil nil nil nil)
                       ("pr" "program ${1:name}\n  $0\nend program ${1:name}\n" "program ... end program ..." nil nil nil nil nil nil)
                       ("re" "read (${1:*},${2:*}) $0\n" "read (*,*)" nil nil nil nil nil nil)
                       ("st" "structure $0\n" "structure" nil nil nil nil nil nil)
                       ("su" "subroutine $0\n" "subroutine" nil nil nil nil nil nil)
                       ("wr" "write (${1:*},${2:*}) $0\n" "write (*,*)" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Dec 26 11:33:13 2013
