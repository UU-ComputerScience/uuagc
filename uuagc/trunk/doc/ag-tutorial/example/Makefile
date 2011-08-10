all: html

html: Html.hs
	ghc -O2 --make -o html Html.hs

Html.hs: Html.ag
	uuagc -dcfsrv --visit --bangpats --strictdata --strictwrap -o Html.hs Html.ag

clean:
	rm -f html Html.hs Html.o Html.hi test.html

test: test.html

test.html: test.doc html
	./html test.doc test.html
