UUAGC=/home/arie/uuagc/dist/build/uuagc/uuagc --haskellsyntax -cfsrmd

all: run


run: Experiment.hs
	ghci Experiment.hs

Experiment.hs: Experiment.ag
	$(UUAGC) Experiment.ag -o Experiment.hs
