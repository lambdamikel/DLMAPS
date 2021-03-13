Lisp = lisp

all		: compile manual test


compile		: 
	echo "(load \"compile.lsp\")" | $(Lisp)


test		:
	echo "(load \"run-tests.lsp\")" | $(Lisp)
	./plot-data
	cd Results/Latex; \
	latex results; \
	latex results; \
	latex results; \
	dvips -o results.ps results.dvi; \
	cd ../..


manual		:
	cd Manual; \
	latex manual; \
	makeindex manual; \
	latex manual; \
	latex manual; \
	dvips -o manual.ps manual.dvi; \
	cd ..


clean		: 
	rm -f Results/*.text Results/*.data Results/*.eps
	cd Manual; \
	rm -f *.aux *.bbl *.blg *.dvi *.idx *.ilg *.ind *.log *.toc *~ ; \
	cd ..
	cd Results/Latex; \
	rm -f *.aux *.bbl *.blg *.dvi *.idx *.ilg *.ind *.log *.toc *~ ; \
	cd ..
	rm -f *.fasl *.o
