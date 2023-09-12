function mkluac
    latexmk -lualatex -pvc -interaction=nonstopmode -synctex=1 -quiet -pvc $argv
end
