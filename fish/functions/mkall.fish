function mkall
		for file in *.tex
				arara $file
		end
		open -g *.pdf
end
				
