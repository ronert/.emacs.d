;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
                     '(("abs" "\\begin{abstract}\n$0\n\\end{abstract}" "\\abstract" nil
                        ("sections")
                        nil nil nil nil)
                       ("ac" "\\newacronym{${1:label}}{${1:$(upcase text)}}{${2:Name}}" "acronym" nil nil nil nil nil nil)
                       ("align" "\\begin{align}\n${1:equations}\n\\end{align}\n$0" "align" nil
                        ("TeX")
                        nil nil nil nil)
                       ("aligns" "\\begin{align*}\n${1:equations}\n\\end{align*}\n$0" "aligns" nil
                        ("TeX")
                        nil nil nil nil)
                       ("article" "\\documentclass[11pt]{article}\n\n\\usepackage{graphicx,amsmath,amssymb,subfigure,url,xspace}\n\\newcommand{\\eg}{e.g.,\\xspace}\n\\newcommand{\\bigeg}{E.g.,\\xspace}\n\\newcommand{\\etal}{\\textit{et~al.\\xspace}}\n\\newcommand{\\etc}{etc.\\@\\xspace}\n\\newcommand{\\ie}{i.e.,\\xspace}\n\\newcommand{\\bigie}{I.e.,\\xspace}\n\n\\title{${1:title}}\n\\author{${2:Author Name}}\n\n\\begin{document}\n\\maketitle\n\n\n\\bibliographystyle{${3:plain}}\n\\bibliography{${4:literature.bib}}\n\n\\end{document}\n" "\\documentclass{article} ..." nil
                        ("skeleton")
                        nil nil nil nil)
                       ("beamer" "\\documentclass[xcolor=dvipsnames]{beamer}\n\n\\usepackage{graphicx,subfigure,url}\n\n% example themes\n\\usetheme{Frankfurt}\n\\usecolortheme{seahorse}\n\\usecolortheme{rose}\n\n% put page numbers\n% \\setbeamertemplate{footline}[frame number]{}\n% remove navigation symbols\n% \\setbeamertemplate{navigation symbols}{}\n\n\\title{${1:Presentation Title}}\n\\author{${2:Author Name}}\n\n\\begin{document}\n	\n\\frame[plain]{\\titlepage}\n	\n\\begin{frame}[plain]{Outline}\n	\\tableofcontents\n\\end{frame}\n	\n\\section{${3:Example Section}}\n\\begin{frame}{${4:Frame Title}}\n\n\\end{frame}\n\n\\end{document}\n" "\\documentclass{beamer} ..." nil
                        ("skeleton")
                        nil nil nil nil)
                       ("begin" "\\begin{$1}\n  $0\n\\end{$1}\n" "\\begin{...} ... \\end{...}" nil nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        nil nil nil)
                       ("bib" "\\bibliographystyle{plain}\n\\bibliography{$1}$0" "\\bibliography" nil
                        ("misc")
                        nil nil nil nil)
                       ("big" "\\\\${1:$$(yas/choose-value '(\"big\" \"Big\" \"bigg\" \"Bigg\"))}l( $0  \\\\$1r)" "\\bigl( ... \\bigr)" nil
                        ("math")
                        nil nil nil nil)
                       ("bigop" "\\\\big${1:$$(yas/choose-value '(\"oplus\" \"otimes\" \"odot\" \"cup\" \"cap\" \"uplus\" \"sqcup\" \"vee\" \"wedge\"))}_{$2}^{$3}$0\n" "\\bigop_{n}^{}" nil
                        ("math")
                        nil nil nil nil)
                       ("binom" "\\binom{${1:n}}{${2:k}}\n" "\\binom{n}{k}" nil
                        ("math")
                        nil nil nil nil)
                       ("block" "\\begin{${1:$$(yas/choose-value '(\"block\" \"exampleblock\" \"alertblock\"))}}{${2:Block Title}}\n\n\\end{$1}\n" "\\begin{*block} ... \\end{*block}" nil
                        ("environments")
                        nil nil nil nil)
                       ("mbf" "\\mathbf{${1:bold.symbol}}$0" "mathbf" nil
                        ("TeX")
                        nil nil nil nil)
                       ("ca" "\\caption{$0}" "caption" nil nil nil nil nil nil)
                       ("case" "\\begin{cases}\n$0 \\\\\\\\\n\\end{cases}\n" "\\begin{cases} ... \\end{cases}" nil
                        ("math")
                        nil nil nil nil)
                       ("cases" "\\begin{dcases}\n${1:cases}\n\\end{dcases}\n$0" "cases" nil
                        ("TeX")
                        nil nil nil nil)
                       ("cha" "\\chapter{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\chapter" nil
                        ("sections")
                        nil nil nil nil)
                       ("cha*" "\\chapter*{${1:name}}\n$0" "\\chapter*" nil
                        ("sections")
                        nil nil nil nil)
                       ("cite" "\\cite{${1:label$(unless yas/modified-p (reftex-citation nil 'dont-insert))}}$0\n" "\\cite" nil
                        ("references")
                        nil nil nil nil)
                       ("desc" "\\begin{description}\n\\item[$0]\n\\end{description}\n" "\\begin{description} ... \\end{description}" nil
                        ("environments")
                        nil nil nil nil)
                       ("enum" "\\begin{enumerate}\n\\item $0\n\\end{enumerate}\n" "\\begin{enumerate} ... \\end{enumerate}" nil
                        ("environments")
                        nil nil nil nil)
                       ("eq" "\\begin{equation}\n\\label{${1:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0\n\\end{equation}\n" "\\begin{equation} ... \\end{equation}" nil
                        ("math")
                        nil nil nil nil)
                       ("eqs" "\\begin{${1:$$(yas/choose-value '(\"align\" \"align*\" \"multline\" \"gather\" \"subequations\"))}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0\n\\end{$1}\n" "\\begin{align} ... \\end{align}" nil
                        ("math")
                        nil nil nil nil)
                       ("fig" "\\begin{figure}[!htb]\n\\centering{%\n\\input{${1:file}}\n\\caption{${2:caption}}\n\\label{${3:label}}}\n\\end{figure}\n$0" "insert figure" nil
                        ("Figures")
                        nil nil nil nil)
                       ("frac" "\\frac{${1:numerator}}{${2:denominator}}$0" "\\frac{numerator}{denominator}" nil
                        ("math")
                        nil nil nil nil)
                       ("frame" "\\begin{frame}{${1:Frame Title}}\n\n\\end{frame}\n" "\\begin{frame} ... \\end{frame}" nil
                        ("environments")
                        nil nil nil nil)
                       ("graphics" "\\includegraphics[width=${1:\\linewidth}]{${2:file}}" "\\includegraphics" nil nil nil nil nil nil)
                       ("hat" "\\hat{${1:variable}}$0" "hat" nil
                        ("TeX")
                        nil nil nil nil)
                       ("href" "\\href{${1:url}}{${2:text}}$0" "\\href{url}{text}" nil
                        ("environments")
                        nil nil nil nil)
                       ("int" "\\Int{${1:integrand}}{${2:x,-∞,∞}}$0" "integral" nil
                        ("math")
                        nil nil nil nil)
                       ("itm" "\\item $0" "\\item" nil
                        ("environments")
                        nil nil nil nil)
                       ("item" "\\begin{itemize}\n\\item $0\n\\end{itemize}\n" "\\begin{itemize} ... \\end{itemize}" nil
                        ("environments")
                        nil nil nil nil)
                       ("itemize" "\\begin{itemize}\n$0\n\\end{itemize}" "itemize" nil nil nil nil nil nil)
                       ("lab" "\\label{${1:label$(unless yas/modified-p (reftex-label nil 'dont-insert))}}$0\n" "\\label" nil
                        ("references")
                        nil nil nil nil)
                       ("letter" "\\documentclass[german]{rletter}\n\\begin{document}\n\\firsthead{\\begin{flushright}\\vspace{10pt}\n    \\sffamily{\\textbf{\\usekomavar{fromname}}\\\\\\ \\usekomavar{fromaddress}\\\\\\ \\usekomavar{fromphone}\\\\\\ \\usekomavar{fromemail}}\n  \\end{flushright}}\n\n%%%%%%%%%%%%%%%%%%%%%%%%%%% Brief %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\\setkomavar{subject}{\\bfseries{${1:Betreff}}}\n\\begin{letter}{\\sffamily{%\n    ${2:Adressat} \\\\\\\n    ${3:Adresse} \\\\\\\n    ${4:Ort und PLZ}}\n  }\n\n  \\opening{${5:Sehr geehrte Damen und Herren},}\n\n  ${6:Text}\n  $0\n  \\closing{${7:Mit freundlichen Grüßen}\\vspace{16pt}}\n\n\\end{letter}\n\\end{document}\n" "rletter" nil
                        ("classes")
                        nil nil nil nil)
                       ("lim" "\\lim_{$1}$0\n" "\\lim_{n}" nil
                        ("math")
                        nil nil nil nil)
                       ("math" "\\[\n$1\n\\]\n" "displaymath \\[ ... \\]" nil
                        ("math")
                        nil nil nil nil)
                       ("cal" "\\mathcal{${1:letter}}$0" "mathcal" nil
                        ("TeX")
                        nil nil nil nil)
                       ("matrix" "\\left \\(\n\\begin{array}{${1:ccc}}\n${2:v1 & v2} \\\\\n$0\n\\end{array}\n\\right \\)" "matrix" nil nil nil nil nil nil)
                       ("mdf" "\\begin{mdframed}[hidealllines=true,backgroundcolor=blue!20]\n$1\n\\end{mdframed}\n$0" "set question environment" nil
                        ("latex")
                        nil nil nil nil)
                       ("minipage" "\\begin{minipage}[${1:htbp}]{${2:1.0}${3:\\linewidth}}\n  $0\n\\end{minipage}" "\\begin{minipage}[position][width] ... \\end{minipage}" nil
                        ("environments")
                        nil nil nil nil)
                       ("mm" "\\[\n${1:equations}\n\\]\n$0" "mm" nil
                        ("TeX")
                        nil nil nil nil)
                       ("cmd" "\\newcommand{\\\\${1:name}}${2:[${3:0}]}{$0}" "newcommand" nil nil nil nil nil nil)
                       ("other" "\\begin{otherlanguage*}{${1:language}}\n$0\n\\end{otherlanguage*}" "Other Language" nil
                        ("babel")
                        nil nil nil nil)
                       ("par" "\\paragraph{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\paragraph" nil
                        ("sections")
                        nil nil nil nil)
                       ("prod" "\\Prod{$1}{$2}$0\n" "Product" nil
                        ("math")
                        nil nil nil nil)
                       ("quote" "\\begin{quote}\\textit{%\n${1:text}}\n\\end{quote}\n$0" "quote" nil nil nil nil nil nil)
                       ("ref" "\\ref{${1:label$(unless yas/modified-p (reftex-reference nil 'dont-insert))}}$0" "\\ref" nil
                        ("references")
                        nil nil nil nil)
                       ("sec" "\\section{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\section" nil
                        ("sections")
                        nil nil nil nil)
                       ("sec*" "\\section*{${1:name}}\n$0" "\\section*" nil
                        ("sections")
                        nil nil nil nil)
                       ("ssub" "\\subsubsection{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\subsubsection" nil
                        ("sections")
                        nil nil nil nil)
                       ("ssub*" "\\subsubsection*{${1:name}}\n$0" "\\subsubsection*" nil
                        ("sections")
                        nil nil nil nil)
                       ("sub" "\\subsection{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\subsection" nil
                        ("sections")
                        nil nil nil nil)
                       ("subfig" "\\subfigure[${1:caption}]{\n  \\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n  $0\n}\n" "\\subfigure" nil
                        ("environments")
                        nil nil nil nil)
                       ("sub*" "\\subsection*{${1:name}}\n$0" "\\subsection*" nil
                        ("sections")
                        nil nil nil nil)
                       ("sum" "\\Sum{${1:X_i}}{${2:i,1,n}}$0\n" "sum" nil
                        ("math")
                        nil nil nil nil)
                       ("tab.yasnippet" "\\\\begin{${1:t}${1/(t)$|(a)$|(.*)/(?1:abular)(?2:rray)/}}{${2:c}}\n$0${2/((?<=.)c|l|r)|./(?1: & )/g}\n\\\\end{${1:t}${1/(t)$|(a)$|(.*)/(?1:abular)(?2:rray)/}}" "tab.yasnippet" nil nil nil nil nil nil)
                       ("table" "\\begin{table}[htbp]\n  \\centering\n  \\begin{tabular}{${3:format}}\n    $0\n  \\end{tabular}\n  \\caption{${1:caption}}\n  \\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n\\end{table}\n" "\\begin{table} ... \\end{table}" nil
                        ("environments")
                        nil nil nil nil)
                       ("bf" "\\textbf{$1}$0" "textbf" nil nil nil nil nil nil)
                       ("mcal" "$\\mathcal{${1:letter}}$$0" "mathcal in text" nil
                        ("TeX")
                        nil nil nil nil)
                       ("it" "\\textit{$1}$0" "textit" nil nil nil nil nil nil)
                       ("sc" "\\textsc{$1}$0" "textsc" nil nil nil nil nil nil)
                       ("tt" "\\texttt{$1}$0" "texttt" nil nil nil nil nil nil)
                       ("tilde" "\\tilde{${1:variable}}$0" "tilde" nil
                        ("TeX")
                        nil nil nil nil)
                       ("url" "\\url{${1:$$(yas/choose-value '(\"http\" \"ftp\"))}://${2:address}}$0" "\\url" nil
                        ("environments")
                        nil nil nil nil)
                       ("use" "\\usepackage[$2]{$1}$0" "\\usepackage" nil
                        ("misc")
                        nil nil nil nil)
                       ("verb" "\\begin{verbatim}\n$0\n\\end{verbatim}\n" "\\begin{verbatim} ... \\end{verbatim}" nil
                        ("environments")
                        nil nil nil nil)))


;;; Do not edit! File generated at Wed Apr  3 17:55:22 2013
