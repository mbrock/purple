# Use lhs2TeX to generate a pretty PDF from literate Haskell.
FROM ubuntu:16.10
RUN apt-get update && apt-get install -y lhs2tex latex-xcolor cm-super
RUN apt-get update && apt-get install -y texlive-latex-extra
WORKDIR /result
CMD cp -r /src/*.lhs /src/Maker /src/*.pdf /src/*.tex . && lhs2TeX *.lhs > x.tex && \
pdflatex -halt-on-error x.tex && \
pdflatex -halt-on-error x.tex && \
pdflatex -halt-on-error x.tex && \
cp x.pdf /doc/MakerDao900.pdf
