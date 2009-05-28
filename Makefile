.PHONY: all

all: index.html

%.html: %.md
	pandoc --standalone --css=style.css --output=$@ $<

