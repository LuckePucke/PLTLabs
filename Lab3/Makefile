# Makefile for PLT lab 3

lab3: lab3.hs TypeChecker.hs Compiler.hs CPP/Test

	ghc --make lab3.hs -o lab3

CPP/Test.hs CPP/Lex.x CPP/Layout.hs CPP/Par.y : CPP.cf
	bnfc --haskell -d $<

CPP/Par.hs: CPP/Par.y
	happy -gcai $<

CPP/Lex.hs: CPP/Lex.x
	alex -g $<

CPP/Test: CPP/Test.hs CPP/Par.hs CPP/Lex.hs
	ghc --make $< -o $@

testfile: lab3
	./lab3 testfile.cc

lab3-testsuite/progs-test-lab3.hs:
	cd lab3-testsuite; make

test: lab3 lab3-testsuite/progs-test-lab3
	cd lab3-testsuite; ./progs-test-lab3 ../

clean:
	-rm -f CPP/*.log CPP/*.aux CPP/*.hi CPP/*.o CPP/*.dvi

distclean: clean
	-rm -f CPP/Doc.* CPP/Lex.* CPP/Par.* CPP/Layout.* CPP/Skel.* CPP/Print.* CPP/Test.* CPP/Abs.* CPP/Test CPP/ErrM.* CPP/SharedString.* CPP/ComposOp.* CPP/CPP.dtd CPP/XML.*
	-rmdir -p CPP/

# EOF
