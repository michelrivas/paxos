FROM haskell:7.10

RUN mkdir paxos

COPY dist/build/paxos/paxos /paxos/paxos

ENTRYPOINT ["/paxos/paxos"]
