function handout
    mkdir {$argv}
    cd {$argv}
    touch {$argv}-data.org
    echo -e '#+TITLE: '\n'#+AUTHOR: Dr. Randy Ridenour'\n'#+DATE: '>>{$argv}-data.org
    touch {$argv}-handout.org
    cat /Users/rlridenour/Dropbox/emacs/handout/handout.org >{$argv}-handout.org
    echo -e '#+include: "'{$argv}'-data.org" :minlevel 1' >>{$argv}-handout.org
    cp /Users/rlridenour/Dropbox/emacs/handout/canvas.org canvas.org
    dired
    open -a emacs
end
