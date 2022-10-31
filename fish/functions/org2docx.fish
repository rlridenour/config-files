function org2docx --description 'Generate a DOCX file using a custom reference document'
    set -l refdoc "$PATH_TO_REFERENCE_DOCX"
    set -l base (basename -s .md -s .org $argv)
    echo Generating $base.docx ...
    pandoc --reference-doc $refdoc -o $base.docx $argv
end
