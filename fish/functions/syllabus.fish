function syllabus
    pandoc --filter pandoc-citeproc --biblatex --template="syllabus.tex" $argv[1] -o $argv[2]
end