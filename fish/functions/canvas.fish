function canvas
    juice canvas.html canvas.html
    find . -type f -name "canvas.html" -print0 | xargs -0 sed -i '' -e 's/body>/div>/g'
    pbcopy < "canvas.html"
    # rm canvas.html
end


