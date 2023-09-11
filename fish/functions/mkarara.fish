function mkarara
    # mkpdf $argv
    arara $argv
    set output_file (string replace -r tex\$ pdf $argv)
    open -g $output_file
end
