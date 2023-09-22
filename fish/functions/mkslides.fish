function mkslides
		for file in *-slides.tex
		    # arara $file
		    mkpdf $file
		end
		open -g *-slides.pdf
end
