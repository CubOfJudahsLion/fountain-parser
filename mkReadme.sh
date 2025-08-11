#! /usr/bin/sh

pdflatex -interaction=nonstopmode -output-format=pdf README.tex
pandoc -f latex -t commonmark --strip-comments --standalone README.tex \
  | sed -re 's/<span class="roman">/<span style="font-family: serif">/g' \
        -re 's:</?u>::g' \
  > README.md
