# Makefile for PLT lab 4 in Haskell

## Variables
###########################################################################

# Files of solution.  Add more files as needed.  'lab4.hs'  has to be first!
files=lab4.hs hgrammar.cf Makefile Interpreter.hs

###########################################################################

# List of goals not corresponding to file names.
.PHONY : sdist clean distclean

# 'lab4' is default goal and needs to remain first goal in this file!
# The extra dependency Hgrammar/Test ensures that the parser is built from hgrammar.cf

lab4 : $(files) Hgrammar/Test
	ghc --make lab4.hs -o lab4

run: 
	cd lab4-testsuite; make; ./progs-test-lab4 ../
# Rules to build the parser:

Hgrammar/Test.hs Hgrammar/Lex.x Hgrammar/Par.y : hgrammar.cf
	bnfc --haskell -d $<

%.hs : %.y
	happy -gcai $<
#	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

Hgrammar/Test : Hgrammar/Test.hs Hgrammar/Par.hs Hgrammar/Lex.hs
	ghc --make $< -o $@

# Rules for shipping the solution:

sdist : lab4.tar.gz

tmpdir := $(shell mktemp -d)
lab4.tar.gz : $(files)
	mkdir $(tmpdir)/lab4
	cp $^ $(tmpdir)/lab4/
	tar -C $(tmpdir) -czhf $@ lab4

# Rules for cleaning generated files:

clean :
	-rm -f Hgrammar/*.log Hgrammar/*.aux Hgrammar/*.hi Hgrammar/*.o Hgrammar/*.dvi *.hi *.o

distclean : clean
	-rm -f Hgrammar/Doc.* Hgrammar/Lex.* Hgrammar/Par.* Hgrammar/Layout.* Hgrammar/Skel.* Hgrammar/Print.* Hgrammar/Test.* Hgrammar/Abs.* Hgrammar/Test Hgrammar/ErrM.* Hgrammar/SharedString.* Hgrammar/ComposOp.* Hgrammar/Fun.dtd Hgrammar/XML.* Hgrammar/*.bak
	-rmdir -p Hgrammar/
	-rm -f lab4.tar.gz lab4

# EOF
