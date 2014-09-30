UTILS=build/uulogo.pdf build/beamerthemeuucs.sty build/uueduc.jpg
LHS2TEX=lhs2TeX
PICTGEN=./gen/dist/build/pictgen/pictgen
DIAGRAMS=build/cons_html.tikz build/section_html.tikz build/cons_level.tikz build/section_level.tikz build/cons_count.tikz build/section_count.tikz build/cons_count_full.tikz build/cons_toc.tikz build/section_toc.tikz build/cons_left.tikz build/section_left.tikz build/cons_left.tikz build/cons_right.tikz build/section_right.tikz


all: ag-tutorial.pdf

clean:
	rm build/*

ag-tutorial.pdf: build/ag-tutorial.pdf
	cp build/ag-tutorial.pdf ag-tutorial.pdf

build/ag-tutorial.pdf: build/ag-tutorial.tex $(UTILS) $(DIAGRAMS)
	cd build; pdflatex ag-tutorial.tex

build/ag-tutorial.tex: AgTutorial.ltx
	cd build; $(LHS2TEX) --path=:.. --poly ../AgTutorial.ltx -o ag-tutorial.tex

build/uulogo.pdf: uulogo.pdf
	cp uulogo.pdf build/uulogo.pdf

build/beamerthemeuucs.sty: beamerthemeuucs.sty
	cp beamerthemeuucs.sty build/beamerthemeuucs.sty

build/uueduc.jpg: uueduc.jpg
	cp uueduc.jpg build/uueduc.jpg

build/cons_html.tikz: diagrams/cons_html.ppd
	$(PICTGEN) diagrams/cons_html.ppd build/cons_html.tikz

build/section_html.tikz: diagrams/section_html.ppd
	$(PICTGEN) diagrams/section_html.ppd build/section_html.tikz

build/cons_level.tikz: diagrams/cons_level.ppd
	$(PICTGEN) diagrams/cons_level.ppd build/cons_level.tikz

build/section_level.tikz: diagrams/section_level.ppd
	$(PICTGEN) diagrams/section_level.ppd build/section_level.tikz

build/cons_count.tikz: diagrams/cons_count.ppd
	$(PICTGEN) diagrams/cons_count.ppd build/cons_count.tikz

build/section_count.tikz: diagrams/section_count.ppd
	$(PICTGEN) diagrams/section_count.ppd build/section_count.tikz

build/cons_count_full.tikz: diagrams/cons_count_full.ppd
	$(PICTGEN) diagrams/cons_count_full.ppd build/cons_count_full.tikz

build/cons_toc.tikz: diagrams/cons_toc.ppd
	$(PICTGEN) diagrams/cons_toc.ppd build/cons_toc.tikz

build/section_toc.tikz: diagrams/section_toc.ppd
	$(PICTGEN) diagrams/section_toc.ppd build/section_toc.tikz

build/cons_left.tikz: diagrams/cons_left.ppd
	$(PICTGEN) diagrams/cons_left.ppd build/cons_left.tikz

build/section_left.tikz: diagrams/section_left.ppd
	$(PICTGEN) diagrams/section_left.ppd build/section_left.tikz

build/cons_right.tikz: diagrams/cons_right.ppd
	$(PICTGEN) diagrams/cons_right.ppd build/cons_right.tikz

build/section_right.tikz: diagrams/section_right.ppd
	$(PICTGEN) diagrams/section_right.ppd build/section_right.tikz
