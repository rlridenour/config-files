
;; ~/.config/emacs/templates

;; Tempel snippet syntax
;; “string” Inserts a string literal.
;; p Inserts an unnamed placeholder field.
;; n Inserts a newline.
;; > Indents with indent-according-to-mode.
;; r Inserts the current region. If no region is active, quits the containing template when jumped to.
;; r> Acts like r, but indent region.
;; n> Inserts a newline and indents.
;; & Insert newline unless there is only whitespace between line start and point.
;; % Insert newline unless there is only whitespace between point and line end.
;; o Like % but leaves the point before newline.
;; (s NAME) Inserts a named field.
;; (p PROMPT <NAME> <NOINSERT>) Insert an optionally named field with a prompt. The PROMPT is displayed directly in the buffer as default value. If NOINSERT is non-nil, no field is inserted. Then the minibuffer is used for prompting and the value is bound to NAME.
;; (r PROMPT <NAME> <NOINSERT>) Insert region or act like (p ...).
;; (r> PROMPT <NAME> <NOINSERT>) Act like (r ...), but indent region.
;; Furthermore Tempel supports syntax extensions:
;; (p FORM <NAME> <NOINSERT>) Like p described above, but FORM is evaluated.
;; (FORM ...) Other Lisp forms are evaluated. Named fields are lexically bound.
;; q Quits the containing template when jumped to.

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))
(ddate (format-time-string "%B %e, %Y") " ")
(xtime (current-time-string))
(datelisp "(insert (format-time-string \"%Y%m%d\")))")
(xds (format-time-string "%Y%m%d") "-")
(email (replace-regexp-in-string "@" "@NOSPAM." user-mail-address))

latex-mode

(> "\\lif " q)

(verb "\\begin{verbatim}" n q n "\\end{verbatim}" n)

(bf "{\\bf " p "}" q)

(exam "% arara: pdflatex: { interaction: nonstopmode, synctex: yes }" n
      n
      "\\documentclass{examdesign}" n
      "\\Fullpages" n
      "\\NumberOfVersions{1}" n
      "\\class{" (p "Course n ame") "}" n
      "\\examname{" (p "Exam Name") "}" n
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
      "\\noindent \\textbf{\\classdata} \\hfill  \\textbf{Name:} \\rule{2.5in}{.4pt}\\\" n
 "    \\textbf{\\examtype,  Version \\Alph{version}} \\\" n
      "\\textbf{Oklahoma Baptist University} \\\" n
 " \\textbf{Dr. Ridenour}\\hfill \\textbf{Date:} \\rule{2.5in}{.4pt} \\\" n
      "\\bigskip" n
      "\\end{examtop}" n
      n
      n
      n
      "\\begin{truefalse}[title={True or False (2 points each)},resetcounter=yes]" n
      "	Write \\`\`\\textbf{T}\'\' or \\`\`\\textbf{F}\'\' in the blank provided." n
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
      "% \\pagebreak" n
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
      "\\end{document}" n
      )

(article "\\documentclass[11pt]{article}" n
	 n
	 "\\usepackage{graphicx,epstopdf,amsmath,amssymb,url,tabularray}" n
	 "\\UseTblrLibrary{booktabs}" n
	 "%\\usepackage{mathastext} % Make math use roman type." n
	 "\\usepackage[normalem]{ulem}" n
	 "\\usepackage{microtype,todonotes}" n
	 "\\usepackage[american]{babel}" n
	 "\\usepackage[autostyle]{csquotes}" n
	 "\\usepackage[letterpaper,centering]{geometry}" n
	 "\\usepackage[sf,sc]{titlesec}" n
	 "\\usepackage[parfill]{parskip} % Line between paragraphs" n
	 "% \\usepackage{tikz}" n
	 "% \\usetikzlibrary{shapes,backgrounds}" n
	 "% \\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate to notes if desired." n
	 "% \\addbibresource{/Users/rlridenour/Dropbox/bibtex/rlr.bib}" n
	 "\\clubpenalty = 10000 % Reduce orphans and widows" n
	 "\\widowpenalty = 10000" n
	 n
	 "\\usepackage{enumitem}" n
	 "\\setlist{nosep}" n
	 "\\usepackage{libertinus-type1}" n
	 "\\usepackage{libertinust1math}" n
	 "\\usepackage[T1]{fontenc}" n
	 "\\usepackage{hyperref}" n
	 n
	 "\\title{" (p "Title") "}" n
	 "\\author{Randy Ridenour}" n
	 "%\\date{}  % Activate to display a given date or n
o date" n
	 n
	 "\\begin{document}" n
	 "\\maketitle" n
	 n
	 q n
	 n
	 "%\\printbibliography" n
	 n
	 "\\end{document}" n
	 )

(sub "\\subsection{" (p "name") "}" n
     n
     q)

(xvenn n
       "% Add to preamble" n
       "% \\usepackage{tikz}" n
       "% \\usetikzlibrary{shapes,backgrounds}" n
       n
       "\\def\\sub{(0,0) circle (1.5cm)}" n
       "\\def\\mid{(-60:2cm) circle (1.5cm)}" n
       "\\def\\pred{(0:2cm) circle (1.5cm)}" n
       n
       n
       "\\begin{tikzpicture}[thick,scale=1, every node/.style={transform shape}]" n
       "  \\begin{scope}" n
       "    % A-Sentences" n
       n
       "    % \\begin{scope}[even odd rule]% Shade S without P" n
       "    %   \\clip \\pred (-1.5,-1.5) rectangle (1.5,1.5);" n
       "    %   \\fill[gray] \\sub;" n
       "    % \\end{scope}" n
       n
       "    % \\begin{scope}[even odd rule]% Shade S without M" n
       "    %   \\clip \\mid (-1.5,-1.5) rectangle (1.5,1.5);" n
       "    %   \\fill[gray] \\sub;" n
       "    % \\end{scope}" n
       n
       "    % \\begin{scope}[even odd rule]% Shade M without S" n
       "    %   \\clip \\sub (-0.5,-3.3) rectangle (2.5,0);" n
       "    %   \\fill[gray] \\mid;" n
       "    % \\end{scope}" n
       n
       "    % \\begin{scope}[even odd rule]% Shade M without P" n
       "    %   \\clip \\pred (-0.5,-3.3) rectangle (2.5,0);" n
       "    %   \\fill[gray] \\mid;" n
       "    % \\end{scope}" n
       n
       "    % \\begin{scope}[even odd rule]% Shade P without M" n
       "    %   \\clip \\mid (-0,-1.5) rectangle (3.5,1.5);" n
       "    %   \\fill[gray] \\pred;" n
       "    % \\end{scope}" n
       n
       "    % \\begin{scope}[even odd rule]% Shade P without S" n
       "    %   \\clip \\sub (0,-1.5) rectangle (3.5,1.5);" n
       "    %   \\fill[gray] \\pred;" n
       "    % \\end{scope}" n
       n
       "    % E Sentences" n
       n
       "    % \\begin{scope} %Shade intersection of S and P" n
       "    %   \\clip \\pred;" n
       "    %   \\fill[gray] \\sub;" n
       "    % \\end{scope}" n
       n
       "    % \\begin{scope} %Shade intersection of S and M" n
       "    %   \\clip \\mid;" n
       "    %   \\fill[gray] \\sub;" n
       "    % \\end{scope}" n
       n
       "    % \\begin{scope} %Shade intersection of M and P" n
       "    %   \\clip \\pred;" n
       "    %   \\fill[gray] \\mid;" n
       "    % \\end{scope}" n
       n
       "    % Mark I and O Sentences" n
       n
       "    % \\draw (-0.5,0.3) n
ode {1};" n
       "    % \\draw (1,0.3) node {2};" n
       "    % \\draw (2.5,0.3) node {3};" n
       "    % \\draw (0.2,-0.9) node {4};" n
       "    % \\draw (1,-0.6) node {5};" n
       "    % \\draw (1.8,-0.9) node {6};" n
       "    % \\draw (1,-2) node {7};" n
       n
       "    % Label Circles" n
       n
       "    \\draw (-2,-0) node {" "$S" "$};" n
       "    \\draw (1,-4) node {" "$M" "$};" n
       "    \\draw (4,0) node {" "$P" "$};" n
       n
       "    % Draw Circles" n
       n
       "    \\draw \\sub;" n
       "    \\draw \\pred;" n
       "    \\draw \\mid;" n
       "  \\end{scope}" n
       n
       "\\end{tikzpicture}" n
       n
       q)

(nd n
    "\\(" n
    "\\begin{nd}" n
    "  " q n
    "\\end{nd}" n
    "\\)" n
    )

(tft n
     "\\begin{question}" n
     "	\\answer{True} " p n
     "\\end{question}" n
     n
     p n
     )

(luaarticle "% arara: latexmk: { engine: lualatex }" n
	    n
	    "\\documentclass[11pt]{article}" n
	    n
	    "\\usepackage{graphicx,epstopdf,amsmath,amssymb,url}" n
	    "\\usepackage{microtype,todonotes}" n
	    "\\usepackage[american]{babel}" n
	    "\\usepackage[letterpaper,centering]{geometry}" n
	    "\\usepackage[sf,sc]{titlesec}" n
	    "\\usepackage[parfill]{parskip} % Line between paragraphs" n
	    "% \\usepackage{tikz}" n
	    "% \\usetikzlibrary{shapes,backgrounds}" n
	    "\\usepackage{enumitem}" n
	    "\\setlist[itemize]{noitemsep} % Comment out for wider separation in lists." n
	    "\\setlist[enumerate]{noitemsep}" n
	    "\\usepackage[autostyle]{csquotes}" n
	    "\\clubpenalty = 10000 % Reduce orphans and widows" n
	    "\\widowpenalty = 10000" n
	    n
	    "\\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate to notes if desired." n
	    "\\addbibresource{/Users/rlridenour/Dropbox/bibtex/rlr.bib}" n
	    n
	    n
	    "\\usepackage{lualatex-math,luatextra}" n
	    "\\usepackage{libertinus}" n
	    "\\usepackage{unicode-math}" n
	    "\\usepackage[unicode=true]{hyperref}" n
	    n
	    "\\title{" (p "Title") "}" n
	    "\\author{Randy Ridenour}" n
	    "%\\date{}  % Activate to display a given date or no date" n
	    n
	    "\\begin{document}" n
	    "\\maketitle" n
	    n
	    q n
	    n
	    "%\\printbibliography" n
	    n
	    "\\end{document}" n
	    )

(it "\\item " q)

(saq n
     "\\begin{question}" n
     "	" p n
     n
     "	\\examvspace*{0in}" n
     "	\\begin{answer}" n
     "		" p "		" n
     "	\\end{answer}" n
     "\\end{question}" n
     n
     q n
     )

(v "\\lor " q)

(tff n
     "\\begin{question}" n
     "	\\answer{False} " p n
     "\\end{question}" n
     n
     p n
     )

(qtt "\\enquote*{" p "} " q)

(handout "% arara: lualatex: { interaction: nonstopmode, synctex: yes }" n
	 n
	 "\\documentclass[11pt]{rlrhandout}" n
	 n
	 n
	 "\\title{" (p "Title") "}" n
	 "\\author{Randy Ridenour}" n
	 "%\\date{}  % Activate to display a given date or no date" n
	 n
	 "\\begin{document}" n
	 "\\maketitle" n
	 "\\thispagestyle{empty}" n
	 "\\RaggedRight" n
	 n
	 q n
	 n
	 "%\\printbibliography" n
	 n
	 "\\end{document}" n
	 )

( "\\chapter{" (p "name") "}" n
  n
  q)

(frac "\\frac{" (p "numerator") "}{" (p "denominator") "}" q)

(enum "\\begin{enumerate}" n
      "\\item " p n
      "\\end{enumerate}" n
      n
      q)

(aarg n
      "\\begin{enumerate}" n
      "	\\item " (p "First Premise") n
      "	\\item \\underline{" (p "Last Premise") "}" n
      "	\\item [" "$\\therefore" "$] " (p "Conclusion") n
      "\\end{enumerate}" n
      n
      q)

(ssub "\\subsubsection{" (p "name") "}" n
      n
      q)

(noi "\\noindent " q)

( "\\begin{description}" n
  "\\item[" (p "label") "] " p n
  "\\end{description}" n
  n
  q n
  )

(myletter "% arara: pdflatex: { interaction: nonstopmode }" n
	  "\\documentclass[12pt]{obuletter} % For OBU letterhead" n
	  "% \\documentclass[12pt]{myletter} % For personal letterhead" n
	  n
	  n
	  "% Customize variables --- for the entire list, see obuletter.cls and myletter.cls." n
	  "\\newcommand{\\obuDept}{Department of Philosophy} % For personal letterhead, use n
ame here." n
	  "\\newcommand{\\Sender}{Randy Ridenour, Ph.D.}" n
	  "\\newcommand{\\obuTitle}{Professor of Philosophy}" n
	  "\\newcommand{\\obuCollege}{Hobbs School of Theology and Ministry} % For personal letterhead, use title here." n
	  "\\newcommand{\\obuPhone}{405.585.4432}" n
	  "\\newcommand{\\obuFax}{405.878.2401}" n
	  "\\newcommand{\\obuBox}{61273}" n
	  "\\newcommand{\\closing}{Sincerely,}" n
	  "\\newcommand{\\toName}{" (p "Recipient") "}" n
	  "\\newcommand{\\toAddress}{" (p "Street Address") "\\\\" (p "City") ", " (p "State") " " (p "ZIP") "}" n
	  n
	  "% \\date{} % Uncomment to add a particular date." n
	  n
	  "\\begin{document}" n
	  n
	  "\\heading" n
	  n
	  "Dear " (p "Recipient") "," n
	  n
	  "% Body of letter goes here." n
	  q n
	  n
	  "\\signature" n
	  n
	  n
	  "\\end{document}" n
	  )

(~ "\\lnet " q)

(ttree n
       "\\begin{tableau}" n
       "  {}" n
       "  " q n
       "\\end{tableau}" n
       )

(tt "{\\tt " p "}" q)

(lcommands "\\renewcommand{\\land}{\\&}" n
	   "\\newcommand{\\lif}{\\supset}" n
	   "\\newcommand{\\liff}{\\equiv}" n
	   "\\renewcommand{\\land}{\\mathbin{\\&}}" n
	   "\\newcommand{\\lneg}{\\neg}" n
	   "\\renewcommand\\familydefault\\sfdefault" n
	   "\\usepackage[LGRgreek]{mathastext}" n
	   "\\renewcommand\\familydefault\\rmdefault" n
	   n
	   q)

(arara "% arara: latexmk: { engine: " (p "pdf") "latex }" q n
       )

( "\\section{" (p "name") "}" n
  n
  q)

(itd "\\item[" (p "label") "] " q)

(fpr n
     "\\hypo {" p "} {" p "}" q n
     )

(mcq n
     "\\begin{question}" n
     "	" p n
     "	\\choice {" p "}" n
     "	\\choice {" p "}" n
     "	\\choice {" p "}" n
     "	\\choice {" p "}" n
     "\\end{question}" n
     n
     p n
     )

(em "\\emph{" p "} " q)

(fpf n
     "\\(" n
     "\\begin{nd}" n
     "\\hypo {" p "} {" p "}" n
     "\\end{nd}" n
     "\\)" n
     n
     q)

(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")

(<> "\\liff " q)

(venn2 "\\begin{venndiagram2sets}" n
       "  [" n
       "  tikzoptions={scale=1,thick}," n
       "  showframe=false," n
       "  labelA=S," n
       "  labelB=P," n
       "  labelOnlyA={x}," n
       "  labelAB={x}" n
       "  ]" n
       "  \\fillANotB" n
       "\\end{venndiagram2sets}" n
       n
       q n
       )

(pdfhandout "% arara: pdflatex: { interaction: n
onstopmode, synctex: yes }" n
n
"\\documentclass[11pt]{article}" n
"\\usepackage{sidenotes,ragged2e,authoraftertitle} % Use \\sidenote{sidenote text}" n
n
"% Reduce font size in sidenotes." n
"\\makeatletter" n
"\\RenewDocumentCommand\\sidenotetext{ o o +m }{%      " n
"    \\IfNoValueOrEmptyTF{#1}{%" n
"        \\@sidenotes@placemarginal{#2}{\\textsuperscript{\\thesidenote}{}~\\footnotesize#3}%" n
"        \\refstepcounter{sidenote}%" n
"    }{%" n
"        \\@sidenotes@placemarginal{#2}{\\textsuperscript{#1}~#3}%" n
"    }%" n
"}" n
"\\makeatother" n
n
"\\usepackage{fancyhdr}" n
"\\setlength{\\headheight}{15.2pt}" n
"\\pagestyle{fancy}" n
n
"\\usepackage{graphicx,epstopdf,amsmath,amssymb,url}" n
"\\usepackage[normalem]{ulem}" n
"\\usepackage{microtype,todonotes}" n
"\\usepackage[american]{babel}" n
"\\usepackage[autostyle]{csquotes}" n
"\\usepackage[letterpaper,left=1in,top=1in,headsep=2\\baselineskip,textwidth=26pc,marginparsep=2pc,marginparwidth=12pc,textheight=44\\baselineskip,headheight=\\baselineskip]{geometry}" n
n
"\\usepackage[sf,sc]{titlesec}" n
"\\usepackage[parfill]{parskip} % Line between paragraphs" n
"% \\usepackage{tikz}" n
"% \\usetikzlibrary{shapes,backgrounds}" n
"% \\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate to notes if desired." n
"% \\addbibresource{/Users/rlridenour/Dropbox/bibtex/rlr.bib}" n
"\\clubpenalty = 10000 % Reduce orphans and widows" n
"\\widowpenalty = 10000" n
n
"\\usepackage{enumitem}" n
"\\setlist[itemize]{noitemsep} % Comment out for wider separation in lists." n
"\\setlist[enumerate]{noitemsep}" n
"\\usepackage{libertinus-type1}" n
"\\usepackage{libertinust1math}" n
"\\usepackage[T1]{fontenc}" n
"\\usepackage{hyperref}" n
n
"\\makeatletter" n
"\\renewcommand\\maketitle" n
"{\\noindent" n
"  {\\LARGE\\scshape\\sffamily\\@title}\\\%" n
"  \\noindent" n
"  {\\large\\scshape\\sffamily\\@author}\\\%" n
"  \\noindent" n
"  {\\scshape\\sffamily\\@date}%" n
"  \\bigskip\\par\\noindent" n
"}" n
"\\makeatother" n
n
"\\fancyheadoffset[R]{\\marginparsep+\\marginparwidth}" n
" \\fancyhead[L,C]{}" n
" \\fancyhead[R]{\\textbf{\\textsf{\\textsc{\\MyTitle \\phantom{X} \\thepage}}}}" n
"\\fancyfoot[L,C,R]{}" n
" " n
"\\title{" (p "Title") "}" n
"\\author{Randy Ridenour}" n
"%\\date{}  % Activate to display a given date or n
o date" n
n
"\\begin{document}" n
"\\maketitle" n
"\\thispagestyle{empty}" n
"\\RaggedRight" n
n
q n
n
"%\\printbibliography" n
n
"\\end{document}" n
)

(ph n
    "\\phantom{XXXXX} " q n
    )

(minipage "\\begin{minipage}[" (p "htbp") "]{" (p "1.0") (p "\linewidth") "}" n
	  "  " q n
	  "\\end{minipage}")

(tuftehandout "\\documentclass[nobib]{tufte-handout}" n
	      "\\usepackage{geometry}                % See geometry.pdf to learn the layout options. There are lots." n
	      "\\geometry{letterpaper}                   % ... or a4paper or a5paper or ... " n
	      "%\\geometry{landscape}                % Activate for for rotated page geometry" n
	      "%\\usepackage[parfill]{parskip}    % Activate to begin paragraphs with an empty line rather than an indent" n
	      "\\usepackage{graphicx}" n
	      "\\usepackage{amssymb}" n
	      "\\usepackage{epstopdf}" n
	      "\\DeclareGraphicsRule{.tif}{png}{.png}{\\`convert #1 \\`dirname #1\\`/\\`basename #1 .tif\\`.png}" n
	      "\\usepackage{hyphenat}" n
	      "\\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate to notes if desired." n
	      "\\addbibresource{/Users/rlridenour/Dropbox/bibtex/randybib.bib}" n
	      n
	      n
	      "\\title{" (p "Title") "}" n
	      "\\author{Dr. Randy Ridenour}" n
	      "%\\date{}  % Activate to display a given date or n
o date" n
	      n
	      "\\begin{document}" n
	      "\\maketitle" n
	      n
	      q n
	      n
	      "%\\printbibliography" n
	      n
	      "\\end{document}" n
	      )

(biber "% arara: biber" n
       "% arara: lualatex: { interaction: n
onstopmode, synctex: yes }" n
       "% arara: lualatex: { interaction: n
onstopmode, synctex: yes }" n
       n
       )

(blank "\\underline{\\phantom{xxxxxxxxxx}}")

(luahandout "% arara: lualatex: { interaction: n
onstopmode, synctex: yes }" n
n
"\\documentclass[11pt]{article}" n
"\\usepackage{sidenotes,ragged2e,authoraftertitle} % Use \\sidenote{sidenote text}" n
n
"% Reduce font size in sidenotes." n
"\\makeatletter" n
"\\RenewDocumentCommand\\sidenotetext{ o o +m }{%      " n
"    \\IfNoValueOrEmptyTF{#1}{%" n
"        \\@sidenotes@placemarginal{#2}{\\textsuperscript{\\thesidenote}{}~\\footnotesize#3}%" n
"        \\refstepcounter{sidenote}%" n
"    }{%" n
"        \\@sidenotes@placemarginal{#2}{\\textsuperscript{#1}~#3}%" n
"    }%" n
"}" n
"\\makeatother" n
n
"\\usepackage{fancyhdr}" n
"\\setlength{\\headheight}{15.2pt}" n
"\\pagestyle{fancy}" n
n
"\\usepackage{graphicx,epstopdf,amsmath,amssymb,url}" n
"\\usepackage[normalem]{ulem}" n
"\\usepackage{microtype,todonotes}" n
"\\usepackage[american]{babel}" n
"\\usepackage[autostyle]{csquotes}" n
"\\usepackage[letterpaper,left=1in,top=1in,headsep=2\\baselineskip,textwidth=26pc,marginparsep=2pc,marginparwidth=12pc,textheight=44\\baselineskip,headheight=\\baselineskip]{geometry}" n
n
"\\usepackage[sf,sc]{titlesec}" n
"\\usepackage[parfill]{parskip} % Line between paragraphs" n
"% \\usepackage{tikz}" n
"% \\usetikzlibrary{shapes,backgrounds}" n
"% \\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate to notes if desired." n
"% \\addbibresource{/Users/rlridenour/Dropbox/bibtex/rlr.bib}" n
"\\clubpenalty = 10000 % Reduce orphans and widows" n
"\\widowpenalty = 10000" n
n
"\\usepackage{enumitem}" n
"\\setlist[itemize]{noitemsep} % Comment out for wider separation in lists." n
"\\setlist[enumerate]{noitemsep}" n
n
"\\usepackage{lualatex-math,luatextra}" n
"\\usepackage{libertinus}" n
"\\usepackage{unicode-math}" n
"\\usepackage[unicode=true]{hyperref}" n
n
"\\makeatletter" n
"\\renewcommand\\maketitle" n
"{\\noindent" n
"  {\\LARGE\\scshape\\sffamily\\@title}\\\%" n
"  \\noindent" n
"  {\\large\\scshape\\sffamily\\@author}\\\%" n
"  \\noindent" n
"  {\\scshape\\sffamily\\@date}%" n
"  \\bigskip\\par\\noindent" n
"}" n
"\\makeatother" n
n
"\\fancyheadoffset[R]{\\marginparsep+\\marginparwidth}" n
" \\fancyhead[L,C]{}" n
" \\fancyhead[R]{\\textbf{\\textsf{\\textsc{\\MyTitle \\phantom{X} \\thepage}}}}" n
"\\fancyfoot[L,C,R]{}" n
" " n
"\\title{" (p "Title") "}" n
"\\author{Randy Ridenour}" n
"%\\date{}  % Activate to display a given date or n
o date" n
n
"\\begin{document}" n
"\\maketitle" n
"\\thispagestyle{empty}" n
"\\RaggedRight" n
n
q n
n
"%\\printbibliography" n
n
"\\end{document}" n
)

(frame "\\begin{frame}{" (p "Frame Title") "}" n
       q n
       "\\end{frame}" n
       )

(bs "\\bigskip" n
    n
    q)

( "\\begin{itemize}" n
  "\\item " p n
  "\\end{itemize}" n
  n
  q)

(prooftree n
	   "\\begin{prooftree}" n
	   "  {}" n
	   "  [" q "]" n
	   "\\end{prooftree}" n
	   )

(qt "\\enquote{" p "} " q)

(venn3 "\\begin{venndiagram3sets}" n
       "  [" n
       "  tikzoptions={scale=1,thick}," n
       "  showframe=false," n
       "  labelA=S," n
       "  labelB=P," n
       "  labelC=M," n
       "  labelOnlyA={x}," n
       "  labelOnlyBC={x}," n
       "  labelABC={x}" n
       "  ]" n
       "  \\fillANotB" n
       "  \\fillBCapC" n
       "  % To put an x on the lines, uncomment the following:" n
       "  % \\setpostvennhook" n
       "  % {" n
       "  % \\node[above = 1.6cm of labelC] {X};" n
       "  % \\node[above left = 1.4cm and .25cm of labelC, rotate=45] {X};" n
       "  % }" n
       "\\end{venndiagram3sets}" n
       n
       q n
       )

(_ "\\land " q)

(fln n
     "\\have {" p "} {" p "} \\" p "{" p "}" q n
     )





org-mode


(mm "\\(" q "\\)")

(kb "(global-set-key (kbd \"" p "\") \'" p ")" n
 q n
 n
)

(article "#+TITLE: " (p "Title") n
 ":drawer:" n
 "#+AUTHOR: Randy Ridenour" n
 "#+DATE: " (format-time-string "%B %d, %Y") n
 "#+LANGUAGE: en-us" n
 "#+LATEX_COMPILER: pdflatex" n
 "#+LaTeX_CLASS: org-article" n
 "#+LaTeX_CLASS_OPTIONS: [11pt]" n
 "#+LaTeX_HEADER: \\usepackage{amsmath,amssymb}" n
 "#+LaTeX_HEADER: \\usepackage{csquotes}" n
 "#+LaTeX_HEADER: \\usepackage{graphicx}" n
 "#+LaTeX_HEADER: \\usepackage{longtable}" n
 "#+LaTeX_HEADER: \\usepackage{wrapfig}" n
 "#+LaTeX_HEADER: \\usepackage{rotating}" n
 "#+LaTeX_HEADER: \\usepackage{url}" n
 "#+LaTeX_HEADER: \\usepackage[normalem]{ulem}" n
 "#+LaTeX_HEADER: \\usepackage{microtype}" n
 "#+LaTeX_HEADER: \\usepackage[american]{babel}" n
 "#+LaTeX_HEADER: \\usepackage[letterpaper,centering]{geometry}" n
 "#+LaTeX_HEADER: \\usepackage[sf,sc]{titlesec}" n
 "#+LaTeX_HEADER: \\usepackage[parfill]{parskip} % Line between paragraphs" n
 "#+LaTeX_HEADER: % \\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate tonotes if desired." n
 "#+LaTeX_HEADER: % \\addbibresource{/Users/rlridenour/Dropbox/bibtex/rlr.bib}" n
 "#+LaTeX_HEADER: \\clubpenalty = 10000 % Reduce orphans and widows" n
 "#+LaTeX_HEADER: \\widowpenalty = 10000" n
 "#+LaTeX_HEADER: \\usepackage{enumitem}" n
 "#+LaTeX_HEADER: \\setlist{nosep}" n
 "#+LaTeX_HEADER: \\usepackage{libertinus-type1}" n
 "#+LaTeX_HEADER: \\usepackage{libertinust1math}" n
 "#+LaTeX_HEADER: \\usepackage[T1]{fontenc}" n
 "#+LaTeX_HEADER: \\usepackage{hyperref}" n
 "#+OPTIONS: toc:nil" n
 ":end:" n
 n
 q)

(snote "****  :B_note:" n
 ":PROPERTIES:" n
 ":BEAMER_ENV: n
ote" n
 ":END:" n
 n
 q)

(srcel "#+begin_src emacs-lisp" n
 q n
 "#+end_src" n
 n
)

(bshrink n
 ":PROPERTIES:" n
 ":BEAMER_opt: shrink=10" n
 ":END:" n
)

(handout "#+TITLE: " (p "Title") n
 ":drawer:" n
 "#+AUTHOR: Dr. Ridenour" n
 "#+LaTeX_CLASS: org-handout" n
 "#+LaTeX_CLASS_OPTIONS: [11pt]" n
 "#+LaTeX_HEADER: % arara: latexmk: { engine: pdflatex }" n
 "#+LaTeX_HEADER: % \\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate tonotes if desired." n
 "#+LaTeX_HEADER: % \\addbibresource{/Users/rlridenour/Dropbox/bibtex/rlr.bib}" n
 "#+OPTIONS: toc:nil" n
 "#+OPTIONS: n
um:1" n
 "#+LATEX: \\thispagestyle{empty}" n
 "#+LATEX: \\RaggedRight" n
 ":end:" n
 n
 q)

(blog "#+TITLE: " (p "Title") n
 "#+DATE: <" (format-time-string "%Y-%m-%d %a") ">" n
 n
 "#+BEGIN_PREVIEW" n
 n
 "#+END_PREVIEW" n
 n
 q)

(block "#+BEGIN_" p " " p n
 "  " q n
 "#+END_" p n
)

(rlink "[[lid:" (p "id") "][")

(biberpdf "#+LaTeX_HEADER: % arara: biber" n
 "#+LaTeX_HEADER: % arara: pdflatex: { interaction: n
onstopmode, synctex: yes }" n
 "#+LaTeX_HEADER: % arara: pdflatex: { interaction: n
onstopmode, synctex: yes }")

(2col n
 "** " (p "Slide title") n
 n
 "*** A       :BMCOL:" n
 ":PROPERTIES:" n
 ":BEAMER_col: 0.5" n
 ":BEAMER_opt: [t]" n
 ":END:" n
 n
 q n
 n
 "*** B       :BMCOL:" n
 ":PROPERTIES:" n
 ":BEAMER_col: 0.5" n
 ":BEAMER_opt: [t]" n
 ":END:" n
)

(canvashtml ":drawer:" n
 "#+TITLE: " p n
 "#+AUTHOR: Randy Ridenour" n
 "#+LANGUAGE: en-us" n
 "#+EXPORT_FILE_NAME: canvas.html" n
 "#+HTML_DOCTYPE: html5" n
 "#+OPTIONS: toc:nil" n
 "#+OPTIONS: title:nil" n
 "#+OPTIONS: html-style:nil" n
 "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"https://randyridenour.net/css/canvas.css\"/>" n
 ":end:" n
 n
 q n
)

(myletter "#+LaTeX_CLASS: rlr-personal-letter" n
 "#+OPTIONS: toc:nil" n
 "#+OPTIONS: title:nil" n
 "#+TITLE: " (p "Title") n
 "#+AUTHOR: Randy Ridenour" n
 "#+DATE: " (format-time-string "%B %e, %Y") " " n
 n
 n
 "\\renewcommand{\\toName}{" (p "Recipient") "}" n
 "\\renewcommand{\\toAddress}{" (p "Street Address") "\\\\" (p "City") ", " (p "State") " " (p "ZIP") "}" n
 n
 "\\heading" n
 n
 q n
 n
 n
 "\\signature")

(bnote "**** :B_note:" n
 ":PROPERTIES:" n
 ":BEAMER_ENV: n
ote" n
 ":END:" n
 n
 p n
 n
 "*** n
otes :B_ignoreheading:" n
 ":PROPERTIES:" n
 ":BEAMER_env: ignoreheading" n
 ":END:" n
 n
 p n
 n
 q)

(blogimage "#+attr_html: alt: " (p "Alternative text") n
 "#+attr_html: title: " (p "Image title") n
 "#+caption: " (p "Caption text") n
 "#+attr_html: :width 450px" n
 q)

(anote "*** n
otes :B_ignoreheading:" n
 ":PROPERTIES:" n
 ":BEAMER_env: ignoreheading" n
 ":END:" n
 n
 q)

(qt* "\\enquote*{" p "} " q)

(harg n
 "#+ATTR_HTML: :class arg" n
 "1. " q)

(barticle "#+TITLE: Sample Presentation notes" n
	  "#+AUTHOR: Randy Ridenour" n
	  "#+LaTeX_CLASS: article" n
	  "#+LaTeX_CLASS_OPTIONS: [11pt]" n
	  "#+LaTeX_HEADER: \\usepackage{beamerarticle}" n
	  "#+LaTeX_HEADER: \\usepackage{amssymb}" n
	  "#+LaTeX_HEADER: \\usepackage{url}" n
	  "#+LaTeX_HEADER: \\usepackage[normalem]{ulem}" n
	  "#+LaTeX_HEADER: \\usepackage{microtype}" n
	  "#+LaTeX_HEADER: \\usepackage[american]{babel}" n
	  "#+LaTeX_HEADER: \\usepackage[letterpaper,centering]{geometry}" n
	  "#+LaTeX_HEADER: \\usepackage[sf,sc]{titlesec}" n
	  "#+LaTeX_HEADER: \\usepackage[parfill]{parskip} % Line between paragraphs" n
	  "#+LaTeX_HEADER: \\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate tonotes if desired." n
	  "#+LaTeX_HEADER: \\addbibresource{/Users/rlridenour/Dropbox/randybib.bib}" n
	  "#+LaTeX_HEADER: \\clubpenalty = 10000 % Reduce orphans and widows" n
	  "#+LaTeX_HEADER: \\widowpenalty = 10000" n
	  "#+LaTeX_HEADER: " n
	  "#+LaTeX_HEADER: \\usepackage{enumitem}" n
	  "#+LaTeX_HEADER: \\setlist[itemize]{noitemsep} % Comment out for wider separation in lists." n
	  "#+LaTeX_HEADER: \\setlist[enumerate]{noitemsep}" n
	  "#+LaTeX_HEADER: " n
	  "#+LaTeX_HEADER: \\usepackage{libertine}" n
	  "#+LaTeX_HEADER: \\usepackage[libertine]{newtxmath}" n
	  "#+LaTeX_HEADER: \\usepackage[scaled=0.96]{zi4}" n
	  "#+LaTeX_HEADER: \\usepackage[T1]{fontenc}" n
	  "#+OPTIONS: toc:nil" n
	  n
	  "#+include: \"contents.org\" :minlevel 1" n
)

(MM "\\[" n
 q n
 "\\]")

(biberlua "#+LaTeX_HEADER: % arara: biber" n
 "#+LaTeX_HEADER: % arara: lualatex: { interaction: n
onstopmode, synctex: yes }" n
 "#+LaTeX_HEADER: % arara: lualatex: { interaction: n
onstopmode, synctex: yes }" n
)

( "#+BEGIN_EXPORT html" n
 "<ol class=\"arg\">" n
 (let ((text (yas-selected-text))) (when text (replace-regexp-in-string "^\\(.*\\)$" "<li>\\1</li>" text t))) " " q n
 "</ol>" n
 "#+END_EXPORT" n
)

(bquote "#+begin_quote" n
 p n
 "#+end_quote" n
 n
 "#+LaTeX: \\begin{raggedleft}" n
 "--- " p n
 "#+LaTeX: \\par\\end{raggedleft}" n
 n
 n
 q)

(syllabuslua "#+TITLE: " (p "Title") n
 ":drawer:" n
 "#+AUTHOR: Randy Ridenour" n
 "#+LaTeX_CLASS: org-article" n
 "#+LaTeX_CLASS_OPTIONS: [11pt]" n
 "% arara: latexmk: { engine: lualatex }" n
 "#+LaTeX_HEADER: \\usepackage{amssymb}" n
 "#+LaTeX_HEADER: \\usepackage{url}" n
 "#+LaTeX_HEADER: \\usepackage[normalem]{ulem}" n
 "#+LaTeX_HEADER: \\usepackage{microtype}" n
 "#+LaTeX_HEADER: \\usepackage[american]{babel}" n
 "#+LaTeX_HEADER: \\usepackage{booktabs,longtable,multirow,pdfpages}" n
 "#+LaTeX_HEADER: \\usepackage[letterpaper,centering]{geometry}" n
 "#+LaTeX_HEADER: \\usepackage[sf,sc]{titlesec}" n
 "#+LaTeX_HEADER: \\usepackage[parfill]{parskip} % Line between paragraphs" n
 "#+LaTeX_HEADER: \\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate tonotes if desired." n
 "#+LaTeX_HEADER: \\addbibresource{/Users/rlridenour/Dropbox/bibtex/randybib.bib}" n
 "#+LaTeX_HEADER: \\clubpenalty = 10000 % Reduce orphans and widows" n
 "#+LaTeX_HEADER: \\widowpenalty = 10000" n
 "#+LaTeX_HEADER:" n
 "#+LaTeX_HEADER: \\usepackage{enumitem}" n
 "#+LaTeX_HEADER: \\setlist[itemize]{noitemsep} % Comment out for wider separation in lists." n
 "#+LaTeX_HEADER: \\setlist[enumerate]{noitemsep}" n
 "#+LaTeX_HEADER:" n
 "#+LaTeX_HEADER: \\usepackage{lualatex-math,luatextra}" n
 "#+LaTeX_HEADER: \\usepackage{libertine}" n
 "#+LaTeX_HEADER: \\usepackage{unicode-math}          " n
 "#+LaTeX_HEADER: \\setmathfont[Scale=MatchUppercase]{libertinusmath-regular.otf}" n
 "#+LaTeX_HEADER:\\usepackage[unicode=true]{hyperref}" n
 "#+OPTIONS: title:nil" n
 "#+OPTIONS: toc:nil" n
 n
 n
 "#+LATEX: \\newcommand{\\coursenumber}{PHIL " (p "XXXX") "}" n
 "#+LATEX: \\newcommand{\\coursetitle}{" (p "Title") "}" n
 "#+LATEX: \\newcommand{\\coursesection}{" (p "A") "}" n
 "#+LATEX: \\newcommand{\\days}{" (p "MW") "}" n
 "#+LATEX: \\newcommand{\\coursetime}{" (p "Start") "--" (p "End") "}" n
 "#+LATEX: \\newcommand{\\room}{" (p "BABC") " " (p "105") "}" n
 "#+LATEX: \\newcommand{\\semester}{" (p "Fall") " " (p "Year") "}" n
 "#+LATEX: \\newcommand{\\officehours}{" (p "MTWR") ", " (p "2:00") "--" (p "4:00") "}" n
 n
 n
 "#+BEGIN_EXPORT latex" n
 "\\begin{center}" n
 "\\begin{tabular}{ll}" n
 "\\multirow{3}{*}{\\includegraphics[height=1.55in]{/Users/rlridenour/Dropbox/images/obu-logo}}" n
 "&\\textsf{\\textbf{\\coursenumber{}: \\coursetitle{}}}\\\\" n
 "&\\textsf{\\textbf{\\coursesection{}: \\days{}, \\coursetime{}, \\room{}}}\\\\" n
 "& \\textsf{\\textbf{\\semester{}}}\\\\\\\\" n
 "& \\textsf{\\textbf{Dr.\\ Randy Ridenour}}\\\\" n
 "& \\textsf{\\textbf{Office: Montgomery Hall 210}}\\\\" n
 "& \\textsf{\\textbf{Office Hours: \\officehours{} or by appointment}}\\\\" n
 "& \\textsf{\\textbf{(W) 585-4432, (C) 613-7516, randy.ridenour@okbu.edu}}\\\\\\\\" n
 "\\end{tabular}" n
 "\\end{center}" n
 "#+END_EXPORT" n
 ":end:" n
 n
 "* OBU Mission" n
 n
 "As a Christian liberal arts university, OBU transforms lives by equipping students to pursue academic excellence, integrate faith with all areas of knowledge, engage a diverse world, and live worthy of the high calling of God in Christ." n
 n
 n
 q n
 n
 "/The instructor reserves the right to amend this syllabus as required./" n
 n
 "\\includepdf[pages=-]{syllabus-attachment.pdf}" n
 n
)

(fullscreen "#+attr_latex: :width \\textwidth :height 0.98\\textheight :options keepaspectratio" n
 q n
 n
 n
)

(beamerwhite "#+TITLE: " (p "Title") n
 "#+AUTHOR: " (p "Dr.") " Ridenour" n
 "#+DATE: " (format-time-string "%B %e, %Y") n
 "#+OPTIONS:   H:3" n
 "#+LaTeX_CLASS: beamer" n
 "#+BEAMER_THEME: white" n
 "#+LaTeX_CLASS_options: [aspectratio=169,12pt]" n
 "#+LATEX_HEADER: \\usepackage{fancyvrb}" n
 "#+BEAMER_HEADER:      \\institute[OBU]{Oklahoma Baptist University}" n
 n
 q n
 n
 n
)

(barg n
 "#+BEGIN_EXPORT latex" n
 "\\begin{enumerate}" n
 "\\item " p n
 "\\item \\underline{" p "}" n
 "\\item [] " p n
 "\\end{enumerate}" n
 "#+END_EXPORT" n
 n
 "#+BEGIN_EXPORT html" n
 "<ol class=\"arg\">" n
 "  <li>" p "</li>" n
 "  <li>" p "</li>" n
 "  <li>" p "</li>" n
 "</ol>" n
 "#+END_EXPORT" n
)

(beamer "#+TITLE: " (p "Title") n
	":drawer:" n
 "#+AUTHOR: " (p "Dr.") " Ridenour" n
 "#+DATE: " (format-time-string "%B %d, %Y") n
 "#+SUBTITLE: " p n
 "#+BEAMER_HEADER: \\institute{Department of Philosophy}" n
 "#+LANGUAGE: en-us" n
 "#+LaTeX_CLASS: beamer" n
 "#+LaTeX_CLASS_options: [aspectratio=169,12pt,ignorenonframetext]" n
 "#+BEAMER_THEME: basicwhite[sections]" n
 "# Use univ or school options for title page logos." n
 "#+OPTIONS: H:3" n
 "#+LATEX_HEADER: \\usepackage{fancyvrb, graphicx, amsmath, amssymb, bm, here}" n
 "# #+LATEX_HEADER: \\setbeameroption{show n
otes on second screen}" n
 "#+LATEX_HEADER: \\setbeamerfont{note page}{size=\\footnotesize}" n
 "#+LATEX_HEADER: \\addtobeamertemplate{note page}{\\setbeamerfont{itemize/enumerate subbody}{size=\\footnotesize}}{}" n
 "#+LATEX_HEADER: \\setbeamercolor{alerted text}{fg=black}" n
 "#+LATEX_HEADER: \\setbeamerfont{alerted text}{series=\\bfseries}" n
 "#+LATEX_HEADER: \\mode<article>{\\setbeamertemplate{alerted text begin}{\\bfseries\\upshape} \\setbeamertemplate{alerted text end}{}}" n
 "#+LATEX_HEADER: \\usepackage{hyperref}" n
 "#+LATEX_HEADER: \\hypersetup{pdfkeywords={SP-Right}}" n
 "#+LATEX_COMPILER: pdflatex" n
 "#+OPTIONS: toc:nil" n
 "#+startup: beamer " n
 ":end:" n
 n
 "*** Title :B_fullframe:" n
 ":PROPERTIES:" n
 ":BEAMER_env: fullframe" n
 ":END:" n
 n
 "\\maketitle" n
 n
 n
 "# *** Table of Contents :B_fullframe:" n
 "# :PROPERTIES:" n
 "# :BEAMER_env: fullframe" n
 "# :END:" n
 n
 "# \\tableofcontents" n
 n
 q n
)

(syllabus "#+TITLE: " (p "Title") n
 ":drawer:" n
 "#+AUTHOR: Randy Ridenour" n
 "#+LaTeX_CLASS: org-article" n
 "#+LaTeX_CLASS_OPTIONS: [11pt]" n
 "% arara: latexmk: { engine: pdflatex }" n
 "#+LaTeX_HEADER: \\usepackage{amssymb}" n
 "#+LaTeX_HEADER: \\usepackage{url}" n
 "#+LaTeX_HEADER: \\usepackage[normalem]{ulem}" n
 "#+LaTeX_HEADER: \\usepackage{microtype}" n
 "#+LaTeX_HEADER: \\usepackage[american]{babel}" n
 "#+LaTeX_HEADER: \\usepackage{booktabs,longtable,multirow,pdfpages}" n
 "#+LaTeX_HEADER: \\usepackage[letterpaper,centering]{geometry}" n
 "#+LaTeX_HEADER: \\usepackage[sf,sc]{titlesec}" n
 "#+LaTeX_HEADER: \\usepackage[parfill]{parskip} % Line between paragraphs" n
 "#+LaTeX_HEADER: %\\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate tonotes if desired." n
 "#+LaTeX_HEADER: %\\addbibresource{/Users/rlridenour/Dropbox/bibtex/randybib.bib}" n
 "#+LaTeX_HEADER: \\clubpenalty = 10000 % Reduce orphans and widows" n
 "#+LaTeX_HEADER: \\widowpenalty = 10000" n
 "#+LaTeX_HEADER:" n
 "#+LaTeX_HEADER: \\usepackage{enumitem}" n
 "#+LaTeX_HEADER: \\setlist[itemize]{noitemsep} % Comment out for wider separation in lists." n
 "#+LaTeX_HEADER: \\setlist[enumerate]{noitemsep}" n
 "#+LaTeX_HEADER:" n
 "#+LaTeX_HEADER: \\usepackage{libertinus-type1}" n
 "#+LaTeX_HEADER: \\usepackage[libertinust1math}" n
 "#+LaTeX_HEADER: \\usepackage[T1]{fontenc}" n
 n
 "#+LaTeX_HEADER:\\usepackage[unicode=true]{hyperref}" n
 "#+OPTIONS: title:nil" n
 "#+OPTIONS: toc:nil" n
 n
 n
 "#+LATEX: \\newcommand{\\coursenumber}{PHIL " (p "XXXX") "}" n
 "#+LATEX: \\newcommand{\\coursetitle}{" (p "Title") "}" n
 "#+LATEX: \\newcommand{\\coursesection}{" (p "A") "}" n
 "#+LATEX: \\newcommand{\\days}{" (p "MW") "}" n
 "#+LATEX: \\newcommand{\\coursetime}{" (p "Start") "--" (p "End") "}" n
 "#+LATEX: \\newcommand{\\room}{" (p "BABC") " " (p "105") "}" n
 "#+LATEX: \\newcommand{\\semester}{" (p "Fall") " " (p "Year") "}" n
 "#+LATEX: \\newcommand{\\officehours}{" (p "MTWR") ", " (p "2:00") "--" (p "4:00") "}" n
 n
 n
 "#+BEGIN_EXPORT latex" n
 "\\begin{center}" n
 "\\begin{tabular}{ll}" n
 "\\multirow{3}{*}{\\includegraphics[height=1.55in]{/Users/rlridenour/Dropbox/images/obu-logo}}" n
 "&\\textsf{\\textbf{\\coursenumber{}: \\coursetitle{}}}\\\\" n
 "&\\textsf{\\textbf{\\coursesection{}: \\days{}, \\coursetime{}, \\room{}}}\\\\" n
 "& \\textsf{\\textbf{\\semester{}}}\\\\\\\\" n
 "& \\textsf{\\textbf{Dr.\\ Randy Ridenour}}\\\\" n
 "& \\textsf{\\textbf{Office: Montgomery Hall 210}}\\\\" n
 "& \\textsf{\\textbf{Office Hours: \\officehours{} or by appointment}}\\\\" n
 "& \\textsf{\\textbf{(W) 585-4432, (C) 613-7516, randy.ridenour@okbu.edu}}\\\\\\\\" n
 "\\end{tabular}" n
 "\\end{center}" n
 "#+END_EXPORT" n
 ":end:" n
 n
 "* OBU Mission" n
 n
 "As a Christian liberal arts university, OBU transforms lives by equipping students to pursue academic excellence, integrate faith with all areas of knowledge, engage a diverse world, and live worthy of the high calling of God in Christ." n
 n
 n
 q n
 n
 "/The instructor reserves the right to amend this syllabus as required./" n
 n
 "\\includepdf[pages=-]{syllabus-attachment.pdf}" n
 n
)

(arara "#+LaTeX_HEADER: % arara: latexmk: { engine: " (p "pdf") "latex }" q n
)

(org-plot-histogram-image "#+PLOT: title:\"" (p "Line Example") "\"" n
 "#+PLOT: set:\"term " (p "$$(yas-choose-value \'(\"svg\" \"jpeg\" \"png\"))") " size 846, 594 font \'Futura,12\'\"" n
 "#+PLOT: type:2d" n
 "#+PLOT: set:\"style histogram " (p "$$(yas-choose-value \'(\"clustered\" \"rowstacked\"))") (p "$(when (equal yas-text \"clustered\") \" gap 1\")") "\"" n
 "#+PLOT: set:\"style fill solid 1.0 border -1\"" n
 "#+PLOT: set:\"key right top\"" n
 "#+PLOT: set:\"xlabel \'" (p "x label") "\'\"" n
 "#+PLOT: set:\"ylabel \'" (p "y label") "\'\"" n
 "#+PLOT: set:\"xtics font \',10\'\"" n
 "#+PLOT: set:\"ytics font \',10\'\"" n
 "#+PLOT: set:\"yrange [0:]\"" n
 "#+PLOT: with:histograms" n
 "#+PLOT: ind:" (p "1") " deps:(" (p "2") ")" n
 "#+PLOT: file:\"" (p "image") "." (p "$(cond ((equal yas-text \"svg\") \"svg\") ((equal yas-text \"jpeg\") \"jpg\") ((equal yas-text \"png\") \"png\"))") "\"" n
 q)

(fullframe n
 "*** " (p "Title") " :B_fullframe:" n
 ":PROPERTIES:" n
 ":BEAMER_env: fullframe" n
 ":END:" n
 n
 q)

(imagesize n
 "#+ATTR_HTML: width=\"100px\"" n
 "#+ATTR_LATEX: :width 100px" n
 "#+ATTR_ORG: :width 100" n
 q)

(obuletter "#+LaTeX_CLASS: rlr-obu-letter" n
 "#+OPTIONS: toc:nil" n
 "#+OPTIONS: title:nil" n
 "#+TITLE: " (p "Title") n
 "#+AUTHOR: Randy Ridenour" n
 "#+DATE: " (format-time-string "%B %e, %Y") " " n
 n
 n
 "\\renewcommand{\\toName}{" (p "Recipient") "}" n
 "\\renewcommand{\\toAddress}{" (p "Street Address") "\\\\" (p "City") ", " (p "State") " " (p "ZIP") "}" n
 n
 "\\heading" n
 n
 q n
 n
 n
 "\\signature")

(bpres "#+TITLE: Beamer Presentations with Org Mode" n
 "#+AUTHOR: Dr. Ridenour" n
 "#+DATE: May  4, 2020" n
 "#+OPTIONS: H:3" n
 "#+LaTeX_CLASS: beamer" n
 "#+BEAMER_THEME: white" n
 "#+LaTeX_CLASS_options: [aspectratio=169,12pt,ignorenonframetext]" n
 "#+LATEX_HEADER: \\usepackage{fancyvrb}" n
 "#+BEAMER_HEADER:      \\institute[OBU]{Oklahoma Baptist University}" n
 "#+OPTIONS: toc:nil" n
 n
 "*** " n
 "\\maketitle" n
 n
 "*** " n
 "\\tableofcontents" n
 n
 "#+include: \"contents.org\" :minlevel 1" n
)

(mainpoint "#+begin_center" n
 "\\Huge{" p "}" n
 "#+end_center" n
 n
 q)

(bp "#+ATTR_BEAMER: :overlay +-")

(luaarticle n
 "#+TITLE: " (p "Title") n
 ":drawer:" n
 "#+AUTHOR: Randy Ridenour" n
 "#+DATE: " (format-time-string "%B %d, %Y") n
 "#+LANGUAGE: en-us" n
 "#+LATEX_COMPILER: lualatex" n
 "#+LaTeX_CLASS: org-article" n
 "#+LaTeX_CLASS_OPTIONS: [11pt]" n
 "#+LaTeX_HEADER: \\usepackage{amsmath,amssymb}" n
 "#+LaTeX_HEADER: \\usepackage{graphicx}" n
 "#+LaTeX_HEADER: \\usepackage{longtable}" n
 "#+LaTeX_HEADER: \\usepackage{wrapfig}" n
 "#+LaTeX_HEADER: \\usepackage{rotating}" n
 "#+LaTeX_HEADER: \\usepackage{url}" n
 "#+LaTeX_HEADER: \\usepackage{csquotes}" n
 "#+LaTeX_HEADER: \\usepackage[normalem]{ulem}" n
 "#+LaTeX_HEADER: \\usepackage{microtype}" n
 "#+LaTeX_HEADER: \\usepackage[american]{babel}" n
 "#+LaTeX_HEADER: \\usepackage[letterpaper,centering]{geometry}" n
 "#+LaTeX_HEADER: \\usepackage[sf,sc]{titlesec}" n
 "#+LaTeX_HEADER: \\usepackage[parfill]{parskip} % Line between paragraphs" n
 "#+LaTeX_HEADER: % \\usepackage[authordate,url=false,isbn=false,backend=biber]{biblatex-chicago} %Change authordate tonotes if desired." n
 "#+LaTeX_HEADER: % \\addbibresource{/Users/rlridenour/Dropbox/bibtex/rlr.bib}" n
 "#+LaTeX_HEADER: \\clubpenalty = 10000 % Reduce orphans and widows" n
 "#+LaTeX_HEADER: \\widowpenalty = 10000" n
 "#+LaTeX_HEADER: " n
 "#+LaTeX_HEADER: \\usepackage{enumitem}" n
 "#+LaTeX_HEADER: \\setlist[itemize]{noitemsep} % Comment out for wider separation in lists." n
 "#+LaTeX_HEADER: \\setlist[enumerate]{noitemsep}" n
 "#+LaTeX_HEADER: \\usepackage{lualatex-math,luatextra}" n
 "#+LaTeX_HEADER: \\usepackage{libertine}" n
 "#+LaTeX_HEADER: \\usepackage{unicode-math}          " n
 "#+LaTeX_HEADER: \\setmathfont[Scale=MatchUppercase]{libertinusmath-regular.otf}" n
 "#+LaTeX_HEADER:\\usepackage[unicode=true]{hyperref}" n
 "#+LaTeX_HEADER: " n
 "#+OPTIONS: toc:nil" n
 ":end:" n
 n
 q)

( "#+BEGIN_EXPORT latex" n
 "\\begin{enumerate}" n
 (let ((text (yas-selected-text))) (when text (replace-regexp-in-string "^" item-string text))) q n
 "\\end{enumerate}" n
 "#+END_EXPORT" n
)

(qt "\\enquote{" p "} " q)

(bbreak n
 "*** " (p "Frame Title") n
 ":PROPERTIES:" n
 ":BEAMER_opt: allowframebreaks=0.8, label=" n
 ":END:" n
 n
 q)

(larg n
      "#+BEGIN_EXPORT latex" n
      "\\begin{enumerate}" n
      "% \\tightlist" n
      "	\\item " (p "First Premise") n
      "	\\item \\underline{" (p "Last Premise") "}" n
      "	\\item [" "$\\therefore" "$] " (p "Conclusion") n
      "\\end{enumerate}" n
      "#+END_EXPORT" n
      q)


context-mode

(article n "\\setupinteraction[state=start,  % make hyperlinks active, define metadata" n "  title={" (p "Title") "}," n "  % subtitle={" (p "Subtitle") "}," n "  author={" (p "Randy Ridenour") "}," n "  keyword={" (p "Keywords") "}]" n "  " n "\\setuppapersize[letter]" n "\\setuppagenumbering[location={footer,middle}]" n "\\setupwhitespace[medium] % For space between paragraphs" n n "\\definefontfamily [linux] [serif] [Linux Libertine O]" n "\\definefontfamily [linux] [sans]  [Linux Biolinum O]" n "\\definefontfamily [linux] [mono]  [Linux Libertine O]" n "\\definefontfamily [linux] [math]  [Linux Libertine O]" n "\\setupbodyfont[linux]" n n "\\setuphead[section][style={\\tfb\\sc\\ss}]" n "\\setuphead[subsection][style={\\tfa\\sc\\ss}]" n "\\setuphead[subsubsection][style={\\tf\\sc\\ss}]" n n "\\setuplayout[" n "    backspace=91pt," n "    width=430pt," n "    margin=44pt," n "    topspace=81pt," n "    header=12pt," n "    headerdistance=25pt," n "    footer=12pt," n "    footerdistance=18pt," n "    height=623pt]" n "    " n "\\starttext" n n "\\startalignment[center]" n "  \\blank[2*big]" n "  {\\tfc " p "}" n "  \\blank[3*medium]" n "  {\\tfa " p "}" n "  \\blank[2*medium]" n "  {\\tfa \\currentdate}" n "  \\blank[3*medium]" n "\\stopalignment" n n q n n "\\stoptext")
(handout n "\\setupinteraction[state=start,  % make hyperlinks active, define metadata" n "  title={" (p "Title") "}," n "  % subtitle={" (p "Subtitle") "}," n "  author={" (p "Randy Ridenour") "}," n "  keyword={" (p "Keywords") "}]" n "  " n "\\setuppapersize[letter]" n "\\setuppagenumbering[location={footer,middle}]" n "\\setupwhitespace[medium] % For space between paragraphs" n n "\\definefontfamily [linux] [serif] [Linux Libertine O]" n "\\definefontfamily [linux] [sans]  [Linux Biolinum O]" n "\\definefontfamily [linux] [mono]  [Linux Libertine O]" n "\\definefontfamily [linux] [math]  [Linux Libertine O]" n "\\setupbodyfont[linux]" n n "\\setuphead[section][style={\\tfa\\em}]" n "\\setuphead[subsection][style={\\tf\\em}]" n n n "\\setuplayout[" n "    backspace=72pt," n "    width=312pt," n "    leftmargin=30pt," n "    rightmargindistance=24pt," n "    rightmargin=144pt," n "    topspace=30pt," n "    header=14pt," n "    headerdistance=28pt," n "    footer=12pt," n "    footerdistance=18pt," n "    height=688pt]" n n n n "\\starttext" n n "{\\tfc \\em " p "}" n n "{\\tfa \\em Dr. Randy Ridenour}" n n "{\\tfa \\em \\currentdate}" n n q n n "\\stoptext")


html-mode

(frag n "<!-- .element: class=\"fragment\" -->" q)
(aarg n "<ol>" n "	<li>" n "		" (p "First Premise") n "	</li>" n "	<li>" n "		<u>" (p "Last Premise") "</u>" n "	</li>" n "	<li style=\"list-style:none;\">" n "		" (p "Conclusion") n "	</li>" n "</ol>" n)
(smd n "<section data-markdown>" n "	<textarea data-template>" n q n "</textarea>" n "</section>" n)



;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
