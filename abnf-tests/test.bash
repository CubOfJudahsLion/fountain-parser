#! /usr/bin/bash
TEXFILE=${1/%.abnf/.tex}
cat latex-prolog > $TEXFILE
../abnf2latex.hs $1 >> $TEXFILE
cat latex-epilog >> $TEXFILE
pdflatex $TEXFILE

