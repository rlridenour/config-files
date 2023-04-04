;; -*- mode: lisp -*-
;; see: https://github.com/minad/tempel#template-syntax
;; see: https://www.emacswiki.org/emacs/tempo-c-cpp.el
;; see: https://www.emacswiki.org/emacs/tempo-c-cpp.el
;;
;; "string"                      Inserts a string literal.
;; p                             Inserts an unnamed placeholder field.
;; n                             Inserts a newline.
;; >                             Indents with indent-according-to-mode.
;; r                             Inserts the current region.
;; r>                            The region, but indented.
;; n>                            Inserts a newline and indents.
;; &                             Insert newline if there is only whitespace between line start and point.
;; %                             Insert newline if there is only whitespace between point and line end.
;; o                             Like % but leaves the point before newline.
;; (s NAME)                      Inserts a named field.
;; (p PROMPT <NAME> <NONINS>)    Insert an optionally named field with a prompt.
;;                               The PROMPT is displayed directly in the buffer
;;                               as default value. If NOINSERT is non-nil, no
;;                               field is inserted. Then the minibuffer is used
;;                               for prompting and the value is bound to NAME.
;; (r PROMPT <NAME> <NOINSERT>)  Insert region or act like (p ...).
;; (r> PROMPT <NAME> <NOINSERT>) Act like (r ...), but indent region.
;; (p FORM <NAME> <NONINS>)      Like p described above, but FORM is evaluated.
;; (FORM ...)                    Other Lisp forms are evaluated. Named fields are lexically bound.

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))
(ydate (format-time-string "%Y%m%d"))
(ddate (format-time-string "%B %e, %Y"))
(xds (format-time-string "%Y%m%d"))
(xtime (current-time-string))


org-mode

(article "#+TITLE: " p n "drawer:" n "#+AUTHOR: Randy Ridenour" n "#+DATE: "(format-time-string "%B %d, %Y") n "#+LANGUAGE: en-us" n "#+LaTeX_CLASS: org-article" n "#+LaTeX_CLASS_OPTIONS: [11pt]" n "#+LaTeX_HEADER: % arara: latexmk: { engine: pdflatex }" n "#+LaTeX_HEADER: \\usepackage{amsmath,amssymb}" n "#+LaTeX_HEADER: \\usepackage{csquotes}" n "#+LaTeX_HEADER: \\usepackage{graphicx}" n "#+LaTeX_HEADER: \\usepackage{longtable}" n "#+LaTeX_HEADER: \\usepackage{wrapfig}" n "#+LaTeX_HEADER: \\usepackage{rotating}" n "#+LaTeX_HEADER: \\usepackage{url}" n "#+LaTeX_HEADER: \\usepackage[normalem]{ulem}" n "#+LaTeX_HEADER: \\usepackage{microtype}" n "#+LaTeX_HEADER: \\usepackage[american]{babel}" n "#+LaTeX_HEADER: \\usepackage[letterpaper,centering]{geometry}" n "#+LaTeX_HEADER: \\usepackage[sf,sc]{titlesec}" n "#+LaTeX_HEADER: \\usepackage[parfill]{parskip} % Line between paragraphs" n "#+LaTeX_HEADER: % \\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate to notes if desired." n "#+LaTeX_HEADER: % \\addbibresource{/Users/rlridenour/Dropbox/bibtex/rlr.bib}" n "#+LaTeX_HEADER: \\clubpenalty = 10000 % Reduce orphans and widows" n "#+LaTeX_HEADER: \\widowpenalty = 10000" n "#+LaTeX_HEADER: \\usepackage{enumitem}" n "#+LaTeX_HEADER: \\setlist{nosep}" n "#+LaTeX_HEADER: \\usepackage{libertinus-type1}" n "#+LaTeX_HEADER: \\usepackage{libertinust1math}" n "#+LaTeX_HEADER: \\usepackage[T1]{fontenc}" n "#+LaTeX_HEADER: \\usepackage{hyperref}" n "#+OPTIONS: toc:nil" n "end:" n n)


latex-mode

(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(frac "\\frac{" p "}{" q "}")
(enumerate "\\begin{enumerate}\n  \\item " p n  "\\end{enumerate}")
(itemize "\\begin{itemize}\n  \\item " r> n> "\\end{itemize}")
(lcommands "\\renewcommand{\\land}{\\&}" n "\\newcommand{\\lif}{\\supset}" n "\\newcommand{\\liff}{\\equiv}" n "\\renewcommand{\\land}{\\mathbin{\\&}}" n "\\newcommand{\\lneg}{\\neg}" n "\\renewcommand\\familydefault\\sfdefault" n "\\usepackage[LGRgreek]{mathastext}" n "\\renewcommand\\familydefault\\rmdefault" n)
(fpf "\\(" n "\\begin{nd}" n> "\\hypo {" p "} {" p "}" n> q n "\\end{nd}" n "\\)")
(fpr "\\hypo {" p "} {" q "}")
(fln "\have {" p "} {" p "} \\ " p "{" p "}")
(exam "% arara: pdflatex: { interaction: nonstopmode, synctex: yes }" n
 n
"\\documentclass{examdesign}" n
"\\Fullpages" n
"\\NumberOfVersions{1}" n
"\\class{" p "}" n
"\\examname{" q "}" n
 n
"\\usepackage{enumitem}" n
"\\setlist{nosep}" n
"\\usepackage{libertinus-type1}" n
"\\usepackage{libertinust1math}" n
"\\usepackage[T1]{fontenc}" n
 n
"\\begin{document}" n
 n
"\\begin{examtop}" n
"\\noindent \\textbf{\\classdata} \\hfill  \\textbf{Name:} \\rule{2.5in}{.4pt}\\\\" n
"\\textbf{\\examtype,  Version \\Alph{version}} \\\\" n
"\\textbf{Oklahoma Baptist University} \\\\" n
"\\textbf{Dr. Ridenour}\\hfill \\textbf{Date:} \\rule{2.5in}{.4pt} \\\\" n
"\\bigskip" n
"\\end{examtop}" n



"\\begin{truefalse}[title={True or False (2 points each)},resetcounter=yes]" n
"	Write ``textbf{T}'' or ``textbf{F}'' in the blank provided." n
 n
 n
 n
"\\end{truefalse}" n
 n
"\\begin{multiplechoice}[title={Multiple Choice (2 points each)},resetcounter=yes]" n
"	Circle the correct answer." n
 n
 n
 n
"\\end{multiplechoice}" n
 n
 n
"\\begin{shortanswer}[title={Short Answer (10 pts each)}]" n
 n
 n
 n
 n
"\\end{shortanswer}" n
 n
 n
 n
"\\end{document}"
)
