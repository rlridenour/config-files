function mknotes
		for file in *-notes.tex
				arara $file
		end
		open -g *-notes.pdf
end
