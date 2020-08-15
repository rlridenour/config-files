function slides
   pandoc -t beamer --latex-engine=xelatex -V theme:metropolis $argv[1] -o $argv[2]
end