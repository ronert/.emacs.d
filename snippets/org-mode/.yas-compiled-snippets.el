;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("align" "\\begin{align}\n${1:equations}\n\\end{align}\n$0" "align" nil
                        ("TeX")
                        nil nil nil nil)
                       ("ascii" "#+begin_src r :results output org :session ascii\n$0\nascii(${1:object})\n#+end_src" "use ascii package output from r-source" nil
                        ("R")
                        nil nil nil nil)
                       ("author.yasnippet" "#+AUTHOR: ${1:`user-full-name`}\n" "Author" nil nil nil nil nil nil)
                       ("block.yasnippet" "#+begin_$1 $2\n  $0\n#+end_$1\n" "#+begin_...#+end_" nil nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        nil nil nil)
                       ("elisp" "#+begin_src emacs-lisp :tangle yes\n$0\n#+end_src" "elisp" nil nil nil nil nil nil)
                       ("email.yasnippet" "#+EMAIL: ${1:`user-mail-address`}\n" "Email" nil nil nil nil nil nil)
                       ("english" "#+TITLE: \\color{statblue}${1:title}\n#+AUTHOR: \\color{statblue}Ronert Obst\n#+DATE: \\color{statblue}\\today\n#+LATEX_CMD: xelatex\n#+LaTeX_CLASS: xelatexcalibri\n#+FILETAGS: ${2:tags}\n\n$0" "english latex preamble" nil
                        ("LaTeX")
                        nil nil nil nil)
                       ("englishnotoc" "#+TITLE: \\color{statblue}${1:title}\n#+AUTHOR: \\color{statblue}Ronert Obst\n#+DATE: \\color{statblue}\\today\n#+LATEX_CMD: xelatex\n#+LaTeX_CLASS: xelatexcalibri\n#+OPTIONS: toc:nil :num nil\n#+FILETAGS: ${2:tags}\n\n$0" "english latex preamble without toc" nil
                        ("LaTeX")
                        nil nil nil nil)
                       ("figure.yasnippet" "#+CAPTION: $1\n#+LABEL: $2\n#+attr_latex: width=$3\\textwidth\n[[file:$4.pdf]]\n$0\n" "figure" nil nil nil nil nil nil)
                       ("german" "#+TITLE: \\color{statblue}${1:title}\n#+AUTHOR: \\color{statblue}Ronert Obst\n#+DATE: \\color{statblue}\\today\n#+LATEX_CMD: xelatex\n#+LaTeX_CLASS: xelatexcalibrigerman\n#+FILETAGS: ${2:tags}\n\n$0" "german latex preamble" nil
                        ("LaTeX")
                        nil nil nil nil)
                       ("germannotoc" "#+TITLE: \\color{statblue}${1:title}\n#+AUTHOR: \\color{statblue}Ronert Obst\n#+DATE: \\color{statblue}\\today\n#+LATEX_CMD: xelatex\n#+LaTeX_CLASS: xelatexcalibrigerman\n#+OPTIONS: toc:nil :num nil\n#+FILETAGS: ${2:tags}\n\n$0" "german latex preamble without toc" nil
                        ("LaTeX")
                        nil nil nil nil)
                       ("inc.yasnippet" "#+INCLUDE: \"${1:file}\" ${2:src-example-quote} ${3:mode}\n\n" "Author" nil nil nil nil nil nil)
                       ("latex" "#+begin_latex\n$0\n#+end_latex\n" "LaTeX" nil nil nil nil nil nil)
                       ("linkfest" "#+TITLE: Weekend Linkfest $1.$2.$3\n#+DATE: $3-$2-$1\n#+SETUPFILE: ~/Dropbox/octopress/setupfile.org\n#+JEKYLL_LAYOUT: post\n#+JEKYLL_CATEGORIES: Linkfest\n#+JEKYLL_PUBLISHED: true\n\n* Statistics and Machine Learning\n- $0\n* R Package of the Week\n* Paper of the Week\n* Programming\n* Elsewhere\n" "Linkfest" nil nil nil nil nil nil)
                       ("rfunc" "#+begin_src r :session :exports code :tangle ./library/${1:functionName}.R :mkdirp yes :cache yes\n$1 <- function($2) {\n$0\n}\n#+end_src\n" "r function definition" nil
                        ("R")
                        nil nil nil nil)
                       ("rtangle" "#+begin_src r :session :results output :exports both :tangle ./${1:filename}.R :mkdirp yes :cache yes\n$0\n#+end_src\n" "tangle r-code block" nil
                        ("R")
                        nil nil nil nil)
                       ("sb.yasnippet" "#+source: ${1:name}\n#+begin_src ${2:language}\n  $3\n#+end_src\n" "#+srcname:..#+begin_src...#+end_src" nil nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        nil nil nil)
                       ("src" "#+begin_src $1\n$0\n#+end_src\n" "Source Code Block" nil nil nil nil nil nil)
                       ("srce" "#+begin_src emacs-lisp\n$0\n#+end_src" "elisp src" nil
                        ("elisp")
                        nil nil nil nil)
                       ("srcr" "#+begin_src r\n$0\n#+end_src\n" "Source Code Block" nil nil nil nil nil nil)
                       ("name" "#+srcname: $0" "srcname" nil nil nil nil nil nil)
                       ("srcrs" "#+begin_src r :exports code\n$0\n#+end_src" "R source for slides     " nil nil nil nil nil nil)
                       ("title.yasnippet" "#+TITLE: ${1:title}\n" "Title Block" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Dec 26 11:33:14 2013
