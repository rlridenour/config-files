function beamer
    pandoc -t beamer --slide-level=3 --template="beamer.tex" $argv[1] -o $argv[2]
end