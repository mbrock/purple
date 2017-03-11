# Use lhs2TeX to generate a pretty PDF from literate Haskell.
FROM ubuntu:16.10
RUN apt-get update && apt-get install -y lhs2tex latex-xcolor cm-super
RUN apt-get update && apt-get install -y texlive-latex-extra
RUN apt-get update && apt-get install -y texlive-fonts-extra
RUN apt-get update && apt-get install -y fonts-fantasque-sans
RUN apt-get update && apt-get install -y texlive-xetex
RUN rm -rf /usr/share/fonts/woff
WORKDIR /result
CMD cp -r /src/*.lhs /src/*.fmt /src/Maker /src/*.pdf /src/*.tex . && lhs2TeX *.lhs > x.tex && \
xelatex -halt-on-error x.tex && \
xelatex -halt-on-error x.tex && \
xelatex -halt-on-error x.tex && \
cp x.pdf /doc/MakerDao900.pdf
