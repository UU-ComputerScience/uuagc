default : block sixpass

block : Block.ag
	../dist/build/src/uuagc -a --visit --seq Block.ag
	ghc -c Block.hs

sixpass : SixPass.ag
	../dist/build/src/uuagc -a --visit --seq SixPass.ag
	ghc -c SixPass.hs

