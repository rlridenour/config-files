function mkpvc
    latexmk -pdf -pvc -interaction=nonstopmode -synctex=1 -quiet $argv
end
