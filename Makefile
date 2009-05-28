.PHONY: all

all: index.htm tutorial.html flp.html

%.htm: %.md
	pandoc --standalone --css=style.css --output=$@ $<

%.html: %.lhs lhs.style
	pandoc --standalone --toc --include-in-header=lhs.style --output=$@ $<
