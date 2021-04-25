function mkt
    fswatch -o $argv | xargs -n1 -I{} arara $argv
end