all:
	bnfc --haskell CPP.cf
	happy -gca ParCPP.y
	alex -g LexCPP.x
	ghc --make TestCPP.hs -o TestCPP

run1:
	./TestCPP Lab1/1-hello.cc

run2:
	./TestCPP Lab1/2-greet.cc

run3:
	./TestCPP Lab1/3-med.cc

run4:
	./TestCPP Lab1/4-grade.cc

run5:
	./TestCPP Lab1/5-palin.cc

run6:
	./TestCPP Lab1/6-grammar.cc

run: run1 run2 run3 run4 run5 run6

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* ComposOp.* CPP.dtd XMLCPP.* Makefile*
	

