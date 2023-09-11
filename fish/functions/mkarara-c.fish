function mkarara-c
    arara $argv
    set output_file (string replace -r tex\$ pdf $argv)
    open -g $output_file
    fswatch -o $argv | xargs -n1 -I{} arara $argv
end
