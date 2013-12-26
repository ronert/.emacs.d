;;; Compiled snippets and support files for `ned-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ned-mode
                     '(("chan" "channel Channel extends ${1:ned.DelayChannel} {\n        $0\n}\n" "chan" nil nil nil nil nil nil)
                       ("conn" "connections${1: allowunconnected}:\n                $0" "connections" nil nil nil nil nil nil)
                       ("for" "for ${1:i}=${2:0}..${3:sizeof(port)-1} {\n    $0\n}" "for" nil nil nil nil nil nil)
                       ("imp" "import ned.${1:Package};" "import" nil nil nil nil nil nil)
                       ("net" "network ${1:Name}\n{\n        submodules:\n           $2\n        connections:\n           $3\n}" "network" nil nil nil nil nil nil)
                       ("simple" "simple ${1:Component}${2: extends ${3:Component}}\n{\n    $0\n}" "simple" nil nil nil nil nil nil)
                       ("sub" "submodules:\n   $0" "submodules" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Dec 26 11:33:14 2013
