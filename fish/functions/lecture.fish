function lecture
    mkdir {$argv}
    cd {$argv}
    touch {$argv}-data.org
    echo -e '#+startup: beamer' \n'#+TITLE: ' \n'#+AUTHOR: Dr. Ridenour' \n'#+DATE: ddate' >>{$argv}-data.org
    touch {$argv}-slides.org
    cat /Users/rlridenour/Dropbox/emacs/beamer/lecture-beamer.org >{$argv}-slides.org
    echo -e '#+include: "'{$argv}'-data.org" :minlevel 1' >>{$argv}-slides.org
    touch {$argv}-notes.org
    cat /Users/rlridenour/Dropbox/emacs/beamer/lecture-notes.org >{$argv}-notes.org
    echo -e '#+include: "'{$argv}'-data.org" :minlevel 1' >>{$argv}-notes.org
    dired
    open -a emacs
end
