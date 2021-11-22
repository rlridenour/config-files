function e
    # emacsclient -c -F "((width . 100) (height . 80))" -a '' $argv
    emacsclient -c -F "((width  . (text-pixels . 940)) (height . (text-pixels . 1200)))" -n -a '' $argv
		open -a Finder
    open -a emacs
end
