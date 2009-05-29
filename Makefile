.PHONY: all

all: index.htm tutorial.html flp.html

%.htm: %.md
	pandoc --standalone --css=style.css --output=$@ $<

%.html: %.lhs
	pandoc --standalone --toc --css=lhs.css --output=$@ $<
