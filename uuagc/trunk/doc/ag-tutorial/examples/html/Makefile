# base
PROJ_A 	:= Html0HS
# with level, counting
PROJ_B 	:= Html1HS
# with TOC
PROJ_C 	:= Html2HS
# with lift/right links (and non implemented index etc)
PROJ_D 	:= Html3HS
# non haskell syntax variant
PROJ_E 	:= Html3

# choose the default build
PROJ 	:= $(PROJ_A)
#PROJ 	:= $(PROJ_B)
#PROJ 	:= $(PROJ_C)
#PROJ 	:= $(PROJ_D)
#PROJ 	:= $(PROJ_E)

PROJ_HS_SYN 	:= $(PROJ_A) $(PROJ_B) $(PROJ_C) $(PROJ_D)
PROJ_OLD_SYN 	:= $(PROJ_E)
PROJ_ALL 		:= $(PROJ_HS_SYN) $(PROJ_OLD_SYN)

default: html

tst:
	echo $(patsubst %,%.hs,$(PROJ_HS_SYN))

html: $(PROJ).hs
	ghc --make -o $@ $<

$(patsubst %,%.hs,$(PROJ_OLD_SYN)): %.hs: %.ag Makefile
	uuagc -dcfsrv -o $@ $<

$(patsubst %,%.hs,$(PROJ_HS_SYN)): %.hs: %.ag Makefile
	uuagc -dcfsrv --haskellsyntax -o $@ $<

clean:
	rm -f html \
		$(patsubst %,%.hs,$(PROJ_ALL)) \
		$(patsubst %,%.hi,$(PROJ_ALL)) \
		$(patsubst %,%.o ,$(PROJ_ALL)) \
		test.html test0.html

test3: test3.doc html Makefile
	./html test3.doc test3.html

test2: test2.doc html Makefile
	./html test2.doc test2.html

test0: test0.doc html Makefile
	./html test0.doc test0.html

