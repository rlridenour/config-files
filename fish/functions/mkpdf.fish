function mkpdf
    latexmk -pdf -interaction=nonstopmode -synctex=1 -quiet $argv
end
