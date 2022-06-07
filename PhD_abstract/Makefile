TEX = pdflatex -shell-escape -interaction=nonstopmode -file-line-error --output-directory=build
BIB = bibtex
BDIR =./build
main:
	$(TEX) glugluz.tex
bib:
	$(BIB) glugluz
clean:
	rm -rf  $(BDIR)/*.log  $(BDIR)/*.toc  $(BDIR)/*.aux  $(BDIR)/*.gz  $(BDIR)/*.nav  $(BDIR)/*.out  $(BDIR)/*.snm  $(BDIR)/*.blg
view:
	open ./build/glugluz.pdf
feyn:
	@for f in $(BDIR)/*.mp ; do mpost --output-directory=$(BDIR) $$f ; done #not used now
cleanall:
	rm -rf  $(BDIR)/*.*
