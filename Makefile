# !/usr/bin/make

# Required tools: awk, sed, find, pandoc, pdflatex and of course Cabal and GHC.

.PHONY: test, doc, readmes, all, cleanup

.ONESHELL: README.md

README_SOURCES = README.tex grammar.tex

all: readmes

readmes: README.pdf README.md

README.pdf: $(README_SOURCES)
	# LaTeX packages required: parskip, xcolor, hyperref and courier.
	pdflatex -interaction=nonstopmode -output-format=pdf README.tex

README.md: $(README_SOURCES)
	# Remove
	sed -re 's|^[[:blank:]]*\\input\{\./grammar.tex\}.*$$|-=grammar.abnf=-|' README.tex \
	| pandoc -f latex -t gfm --strip-comments --standalone \
	| sed -re '/-=grammar\.abnf=-/ {
	s/^.*$$/``` abnf/
	rgrammar.abnf
	a```
	}' \
	-re 's/<span class="smallcaps">/<span style="font-variant: small-caps">/g' \
	-re 's/<span class="roman">/<span style="font-family: serif">/g' \
	> README.md

grammar.tex: grammar.abnf abnf2tex.awk
	./abnf2tex.awk grammar.abnf > grammar.tex

test:
	cabal test

doc:
	cabal haddock

cleanup:
	cabal clean
	-@rm README.{aux,log,md,out,pdf}
	-@rm grammar.tex
	find . \( -name \*\~ -o -name \#\*\# -o -iname \*.sw\? \) -delete

