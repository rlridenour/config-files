function handout
    pandoc --filter pandoc-citeproc --biblatex --template="handout.tex" $argv[1] -o $argv[2]
end