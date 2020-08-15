function obuletter
    pandoc --template="obuletter.tex" $argv[1] -o $argv[2]
end