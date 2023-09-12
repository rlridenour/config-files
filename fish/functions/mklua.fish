function mklua
    latexmk -lualatex -pvc -interaction=nonstopmode -synctex=1 -quiet -pv $argv
end
