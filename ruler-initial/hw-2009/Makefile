LHS2TEX=/home/arie/lhs2tex-install/bin/lhs2TeX

all: paper.pdf

paper.tex: paper.lhs
	$(LHS2TEX) --poly paper.lhs -o paper.tex

paper.pdf: paper.tex
	pdflatex paper.tex
	bibtex paper
	pdflatex paper.tex

remake:
	pdflatex paper.tex
