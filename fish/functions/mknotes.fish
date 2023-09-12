function mknotes
		for file in *-notes.tex
		    # arara $file
		    mkpdf $file 
		end
		open -g *-notes.pdf
end
