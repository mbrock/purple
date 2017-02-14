FROM haskell:8.0.1
WORKDIR /src
COPY ./mkrfuzz.cabal /src
RUN apt-get update && apt-get install -y libbz2-dev
RUN cabal update && cabal install --only-dependencies
COPY . /src
RUN cabal install
CMD ["mkrfuzz"]
