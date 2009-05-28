.PHONY: all

all: index.htm tutorial.html

%.htm: %.md
	pandoc --standalone --css=style.css --output=$@ $<

%.html: %.lhs lhs.style
	pandoc --standalone --include-in-header=lhs.style --output=$@ $<
