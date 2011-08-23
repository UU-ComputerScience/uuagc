PROJ_A = Html0HS
PROJ_B = HtmlHS
PROJ_C = Html

# choose the default build
PROJ = $(PROJ_A)
#PROJ = HtmlHS
#PROJ = Html

PROJ_ALL = $(PROJ_A) $(PROJ_B) $(PROJ_C)

default: html

html: $(PROJ).hs
	ghc --make -o html $(PROJ).hs

Html.hs: Html.ag Makefile
	uuagc -dcfsrv -o Html.hs Html.ag
	#uuagc -dcfsrv --visit --bangpats --strictdata --strictwrap -o Html.hs Html.ag

HtmlHS.hs: HtmlHS.ag Makefile
	uuagc -dcfsrv --haskellsyntax -o HtmlHS.hs HtmlHS.ag

Html0HS.hs: Html0HS.ag Makefile
	uuagc -dcfsrv --haskellsyntax -o Html0HS.hs Html0HS.ag

clean:
	rm -f html \
		Html.hs Html.o Html.hi \
		HtmlHS.hs HtmlHS.o HtmlHS.hi \
		Html0HS.hs Html0HS.o Html0HS.hi \
		test.html test0.html

test: test.doc html Makefile
	./html test.doc test.html

test0: test0.doc html Makefile
	./html test0.doc test0.html

