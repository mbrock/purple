# Use lhs2TeX to generate a pretty PDF from literate Haskell.
FROM ubuntu:16.10
RUN apt-get update && apt-get install -y lhs2tex latex-xcolor
WORKDIR /result
CMD cp /src/*.lhs /src/*.pdf /src/*.tex . && lhs2TeX *.lhs > x.tex && \
pdflatex -halt-on-error x.tex && \
pdflatex -halt-on-error x.tex && \
cp x.pdf /doc/MakerDao900.pdf
