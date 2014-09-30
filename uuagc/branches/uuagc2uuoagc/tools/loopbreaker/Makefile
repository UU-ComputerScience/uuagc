all:
	rm -rf results
	rm -f execution.log intermediate.attrs
	mkdir results
	ghc -O --make LoopBreaker.hs -o breaker
	./breaker
clean:
	rm -f *.o *.hi breaker intermediate.attrs execution.log
	rm -rf results test
