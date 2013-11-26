;;; Compiled snippets and support files for `haskell-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'haskell-mode
                     '(("case" "case ${1:var} of\n     ${2:cond} -> ${3:value}\n     $0\n     otherwise -> ${4:other}" "case" nil nil nil nil nil nil)
                       ("da" "data ${1:Type} = $2" "data" nil nil nil nil nil nil)
                       ("d" "{-\n  $0\n-}" "doc" nil nil nil nil nil nil)
                       ("fun" "${1:function-name} :: ${2:type}\n$1 ${3:arguments} $0" "fun" nil nil nil nil nil nil)
                       ("import" "import${1: qualified} ${2:Module${3:(symbols)}}${4: as ${5:alias}}" "import" nil nil nil nil nil nil)
                       ("instance" "instance ${1:${2:(Show a)} => }${3:Ord} ${4:DataType} where\n$0\n" "instance" nil nil nil nil nil nil)
                       ("main" "main = do $0" "main" nil nil nil nil nil nil)
                       ("mod" "module ${1:Module} where\n$0" "module" nil nil nil nil nil nil)
                       ("class" "class ${1:Class Name} where\n      $0" "new class" nil nil nil nil nil nil)
                       ("{" "{-# ${1:PRAGMA} #-}" "pragma" nil nil nil nil nil nil)
                       ("pr" "print $0" "print" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue Nov 12 11:12:04 2013
