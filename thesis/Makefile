all: clean
	pdflatex -shell-escape thesis.tex
	bibtex thesis
	pdflatex -shell-escape thesis.tex
	pdflatex -shell-escape thesis.tex

clean:
	rm -f thesis.aux thesis.bbl thesis.blg thesis.log thesis.out thesis.toc thesis.pdf
