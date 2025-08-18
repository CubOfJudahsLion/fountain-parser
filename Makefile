.PHONY: test, doc, readmes, all, cleanup

all: readmes

readmes: README.pdf README.md

README.pdf: README.tex
	@pdflatex -interaction=nonstopmode -output-format=pdf README.tex

README.md: README.tex
	@pandoc -f latex -t commonmark --strip-comments --standalone README.tex \
	| sed -re 's/<span class="roman">/<span style="font-family: serif">/g' \
	> README.md

test:
	cabal test

doc:
	cabal haddock

cleanup:
	cabal clean
	rm README.{aux,log,md,out,pdf}

