function mkall
		for file in *.tex
				mkpdf $file
		end
		open -g *.pdf
end
				
