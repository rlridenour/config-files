function article
    pandoc --filter pandoc-citeproc --biblatex --template="article.tex" $argv[1] -o $argv[2]
end