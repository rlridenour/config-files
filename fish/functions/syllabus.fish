function syllabus
    mkdir {$argv}
    cd {$argv}
    touch {$argv}-data.org
    echo -e 'syllabus'>>{$argv}-data.org
    touch {$argv}-syllabus.org
    cat /Users/rlridenour/Dropbox/emacs/syllabus/syllabus.org >{$argv}-syllabus.org
    echo -e '#+include: "'{$argv}'-data.org" :minlevel 1' >>{$argv}-syllabus.org
    cp /Users/rlridenour/Dropbox/emacs/syllabus/canvas.org canvas.org
    dired
    open -a emacs
end
