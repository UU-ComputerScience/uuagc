all: hcar-uuagc.pdf

hcar-uuagc.pdf: hcar.tex hcar.sty UUAG-JU.tex
	pdflatex hcar.tex
	mv hcar.pdf hcar-uuagc.pdf

view: hcar-uuagc.pdf
	evince hcar-uuagc.pdf

clean:
	rm -f hcar.aux hcar.log
