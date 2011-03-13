lns=~/latex-namespace/trunk/latex-namespace
lhs=~/lhs2tex-install/bin/lhs2TeX

all: paper.pdf
paper.pdf: paper.tex
	pdflatex paper.tex

paper.tex: defer-generic.tex
	$(lns) -s defer-generic.tex -o paper.tex

defer-generic.tex: defer-generic.lhs
	$(lhs) --poly defer-generic.lhs -o defer-generic.tex
