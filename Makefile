demos: demos/DemoSine.txt demos/DemoSinesForever.txt demos/DemoSineWait.txt demos/DemoSineWaitChange.txt

demos/%.txt:
	cd demos && stack build --exec $* > $*.txt

speedtest: build
	cd essenceoflivecoding-speedtest-yampa && stack build && time stack exec SpeedTest

build:
	stack build

article: demos latex

latex: latex-article latex-appendix

latex-article:
	cd article && pdflatex -shell-escape -interact nonstopmode EssenceOfLiveCoding.lhs

latex-abstract:
	cd article && pdflatex -shell-escape -interact nonstopmode EssenceOfLiveCodingAbstract.lhs

latex-appendix:
	cd article && pdflatex -shell-escape -interact nonstopmode EssenceOfLiveCodingAppendix.lhs

bibtex:
	cd article && bibtex EssenceOfLiveCoding && bibtex EssenceOfLiveCodingAppendix && bibtex EssenceOfLiveCodingAbstract

article/%.png: article/%.tex
	cd article && pdflatex -shell-escape $*

pictures: article/CategoryId.png article/CategoryCompose.png article/ArrowArr.png article/ArrowCompose.png

git-submodule:
	git submodule init && git submodule update --checkout

revealjs: git-submodule

presentation: revealjs pictures
	cd article && pandoc -s EssenceOfLiveCodingPresentation.md -t revealjs -V theme=serif -i -o EssenceOfLiveCodingPresentation.html

symlinks: essenceoflivecoding-gloss-example/.ghci

essenceoflivecoding-gloss-example/.ghci:
	echo "Won't copy symlink, file exists" #ln -s ../essenceoflivecoding-gloss/.ghci essenceoflivecoding-gloss-example/.ghci

gloss-example: symlinks
	cd essenceoflivecoding-gloss-example && stack ghci

gears: symlinks
	cd essenceoflivecoding-gloss-example && stack ghci essenceoflivecoding-gloss-example:exe:gears
