# default : block sixpass
default : fixpoint fixpointf

block : Block.ag
	../dist/build/uuagc/uuagc -a --visit --seq Block.ag
	ghc -c Block.hs

sixpass : SixPass.ag
	../dist/build/uuagc/uuagc -a --visit --seq SixPass.ag
	ghc -c SixPass.hs

fixpoint : TestFP1.ag
	rm -f TestFP1.hs
	../dist/build/uuagc/uuagc -a --dumpgrammar $<
	ghc -c TestFP1.hs

fixpointf : TestFPf1.ag
	../dist/build/uuagc/uuagc -a --dumpgrammar $<
	ghc -c TestFPf1.hs

