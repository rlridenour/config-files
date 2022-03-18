function mkslides
		for file in *-slides.tex
				arara $file
		end
		open -g *-slides.pdf
end
