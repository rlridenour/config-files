function panclean
    touch foo.tex 
    latexmk -c
	rm foo.tex
    trash -f *html *.tex *.docx *.pdf
	touch foo.rel
	rm *.rel
end
