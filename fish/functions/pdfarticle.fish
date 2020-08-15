function pdfarticle
    pandoc --filter pandoc-citeproc --biblatex --template="pdfarticle.tex" $argv[1] -o $argv[2]
end