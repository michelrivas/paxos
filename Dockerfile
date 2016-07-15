FROM haskell:7.10

RUN mkdir paxos

RUN cabal update

COPY paxos.cabal /paxos/paxos.cabal

RUN cd paxos && cabal install --only-dependencies -j4

COPY dist/build/paxos/paxos /paxos/paxos

ENTRYPOINT ["/paxos/paxos"]
