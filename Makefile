.PHONY: test, doc, readmes, all, cleanup

README_SOURCES = README.tex grammar.tex

all: readmes

readmes: README.pdf README.md

README.pdf: $(README_SOURCES)
	-rm README.pdf
	# LaTeX packages required: parskip, xcolor, hyperref and courier.
	pdflatex -interaction=nonstopmode -output-format=pdf README.tex

README.md: $(README_SOURCES)
	-rm README.md
	pandoc -f latex -t commonmark --strip-comments --standalone README.tex \
	| sed -re 's/<span class="smallcaps">/<span style="font-variant: small-caps">/g' \
	> README.md

grammar.tex: grammar.abnf abnf2tex.awk
	./abnf2tex.awk grammar.abnf > grammar.tex

test:
	cabal test

doc:
	cabal haddock

cleanup:
	cabal clean
	-rm README.{aux,log,md,out,pdf}
	-rm grammar.tex
	find . \( -name \*\~ -o -name \#\*\# -o -iname \*.sw\? \) -delete
