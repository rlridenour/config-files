function canvas-notes
    cp *-data.org canvas-data.org
    find . -type f -name "canvas-data.org" -print0 | xargs -0 sed -i '' -e 's/B_ignoreheading/ignore/g'
    find . -type f -name "canvas-data.org" -print0 | xargs -0 sed -i '' -e 's/B_note/noexport/g'
end
