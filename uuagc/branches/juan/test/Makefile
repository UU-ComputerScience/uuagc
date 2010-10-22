# default : block sixpass
default : fixpoint fixpointf

block : Block.ag
	../dist/build/src/uuagc -a --visit --seq Block.ag
	ghc -c Block.hs

sixpass : SixPass.ag
	../dist/build/src/uuagc -a --visit --seq SixPass.ag
	ghc -c SixPass.hs

fixpoint : TestFP1.ag
	../dist/build/uuagc/uuagc -a $<
	ghc -c TestFP1.hs

fixpointf : TestFPf1.ag
	../dist/build/uuagc/uuagc -a $<
	ghc -c TestFPf1.hs

