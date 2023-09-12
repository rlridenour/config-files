function mkpdfc
    latexmk -pdf -interaction=nonstopmode -synctex=1 -quiet -pvc $argv
end
