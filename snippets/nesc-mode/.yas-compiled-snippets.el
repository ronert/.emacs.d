;;; Compiled snippets and support files for `nesc-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'nesc-mode
                     '(("tossim" "#ifndef TOSSIM\n        $0\n#endif" "TOSSIM" nil nil nil nil nil nil)
                       ("author" "" "author" nil nil nil nil nil nil)
                       ("command" "command ${1:void} ${2:naMe}($3) {\n\n}" "command" nil nil nil nil nil nil)
                       ("dbg" "dbg(\"${1:Module}\", \"${2:message}\"${3:, ${4:var list}});" "dbg" nil nil nil nil nil nil)
                       ("event" "event ${1:void} ${2:On.Event}($3) {\n      $0\n}" "event" nil nil nil nil nil nil)
                       ("ifdef" "#ifdef ${1:Macro}\n $2\n${3:#else}\n $4\n#endif" "ifdef" nil nil nil nil nil nil)
                       ("int" "interface ${1:Interface} {\n          $0\n}" "interface" nil nil nil nil nil nil)
                       ("mod" "module ${1:Module} {\n       ${2:uses interface ${3:Packet}};\n       $0\n}" "module" nil nil nil nil nil nil)
                       ("nx" "nx_uint${1:8}_t ${2:var};\n$0" "nx" nil nil nil nil nil nil)
                       ("provides" "provides interface ${1:Interface};" "provides" nil nil nil nil nil nil)
                       ("sim" "#ifdef TOSSIM\n       $0\n#endif" "sim" nil nil nil nil nil nil)
                       ("u8" "uint8_t ${1:var};\n$0" "uint8_t" nil nil nil nil nil nil)
                       ("uses" "uses interface ${1:Interface}${2: as ${3:alias}};\n$0" "uses" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Dec 26 11:33:14 2013
