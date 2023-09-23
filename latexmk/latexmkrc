# LaTeXmk configuration file

# Usage example
# latexmk file.tex

# Main command line options
# -pdflatex : compile with pdflatex
# -lualatex : compile with luaflatex
# -pv       : run file previewer
# -pvc      : run file previewer and continually recompile on change
# -c        :
# -C        :clean up auxiliary and output files

# Generate pdf using pdflatex (-pdf)
$pdf_mode = 1;

# Use bibtex if a .bib file exists
$bibtex_use = 1;

# Define commands to compile with pdfsync support and nonstopmode
$pdflatex = 'pdflatex -synctex=1 -interaction=nonstopmode %O %S';
# $pdflatex = 'lualatex -synctex=1 -interaction=nonstopmode %O %S';

$silent = 1;

# Use default pdf viewer (Skim)
$pdf_previewer = 'open';

# Also remove pdfsync files on clean
$clean_ext = '%R.synctex.gz';
