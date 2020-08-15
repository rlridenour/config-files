function dired
    emacsclient -e "(dired \"$PWD\")"
    open -a emacs
end
