#!/usr/bin/env fish

function present
         open -R *-slides.pdf
         osascript -e 'tell application "Keyboard Maestro Engine" to do script "4B048CB1-406F-47F9-B1E8-762910295AAF"'
end
