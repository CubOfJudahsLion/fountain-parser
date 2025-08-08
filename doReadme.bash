#! /usr/bin/bash

pdflatex README.tex
pandoc -f latex -t commonmark --strip-comments -o README.md README.tex # \
#| sed -r \
#    -e 's/\[([^]]+)\]\{\.[a-z-]+\}/\1/g' \
#    -e 's/\[(\[[^\]+\]\([^\]+\))\]\{\.[a-z-]+\}/\1/g' \
#    -e 's/\s+\{\#[a-z-]+\s+\.[a-z-]+\}//g' \
#    > README.md
	# the three expressions remove anchors and styling syntax:
	# - [text]{.style}
	# - [[text](link)]{.style}
	# - {#section-anchor .style}
