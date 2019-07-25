demos: build demo-sine demo-sine-wait demo-sine-wait-change demo-sines-forever

demo-sine:
	stack exec DemoSine > DemoSine.txt

# I could make this by-file, but I don't know how how to tell it whether build has done something or not
demo-sines-forever:
	stack exec DemoSinesForever > DemoSinesForever.txt

demo-sine-wait:
	stack exec DemoSineWait > DemoSineWait.txt

demo-sine-wait-change:
	stack exec DemoSineWaitChange > DemoSineWaitChange.txt

speedtest: build
	time stack exec SpeedTest

build:
	stack build

article: demos latex

latex: latex-article latex-appendix

latex-article:
	cd article && pdflatex -shell-escape -interact nonstopmode EssenceOfLiveCoding.lhs

latex-appendix:
	cd article && pdflatex -shell-escape -interact nonstopmode EssenceOfLiveCodingAppendix.lhs

bibtex:
	cd article && bibtex EssenceOfLiveCoding && bibtex EssenceOfLiveCodingAppendix

presentation:
	cd article && pandoc -s EssenceOfLiveCodingPresentation.md -t revealjs -V theme=serif -i -o EssenceOfLiveCodingPresentation.html

symlinks: essenceoflivecoding-gloss-example/.ghci

essenceoflivecoding-gloss-example/.ghci:
	ln -s essenceoflivecoding-gloss/.ghci essenceoflivecoding-gloss-example/.ghci

gloss-example: symlinks
	cd essenceoflivecoding-gloss-example && stack ghci

gears: symlinks
	cd essenceoflivecoding-gloss-example && stack ghci essenceoflivecoding-gloss-example:exe:gears
