;;; Compiled snippets and support files for `nsis-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'nsis-mode
                     '(("def" "!define ${1:CONSTANT} ${2:value}" "define" nil nil nil nil nil nil)
                       ("fun" "Function ${1:Name}\n         $0\nFunctionEnd" "function" nil nil nil nil nil nil)
                       ("if" "${IF} ${1:cond}\n      $0\n${ElseIf} ${2:else_cond}\n\n${EndIf}" "if" nil nil nil nil nil nil)
                       ("inc" "!include \"${Library.nsh}\"" "include" nil nil nil nil nil nil)
                       ("im" "!insermacro ${1:Name} ${2:\"args\"}" "insert_macro" nil nil nil nil nil nil)
                       ("$" "$INSTDIR" "instdir" nil nil nil nil nil nil)
                       ("macro" "!macro ${1:Name} UN\n$0\n\n!macroend" "macro" nil nil nil nil nil nil)
                       ("msg" "MessageBox MB_OK \"${1:hello}\"" "message" nil nil nil nil nil nil)
                       ("$" "$OUTDIR" "outdir" nil nil nil nil nil nil)
                       ("out" "outFile \"${1:setup}.exe\"" "outfile" nil nil nil nil nil nil)
                       ("sec" "Section \"${1:Program}\"\n        $0\nSectionEnd" "section" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Dec 26 11:33:14 2013
