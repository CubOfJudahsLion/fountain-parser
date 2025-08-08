!# /usr/bin/bash

pdflatex -interaction=errorstopmode README.tex 2> /dev/null
pandoc -f latex -t markdown --strip-comments README.tex | sed -re 's/\{\.[a-z-]+\}//gi' > README.md
