function convert
    pandoc -s --bibliography=/Users/rlridenour/zotero.bib $argv[1] -o $argv[2]
end