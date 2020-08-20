function mkpvc
    latexmk -pdf -pvc -halt-on-error $argv
end
