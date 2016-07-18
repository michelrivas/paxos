FROM haskell:7.10

# Creating directory in container
RUN mkdir paxos

# Updating cabal in the container
RUN cabal update

# Adding .cabal file to capture dependencies
COPY paxos.cabal /paxos/paxos.cabal

# Installing dependencies separately to 
# use docker layer cache 
RUN cd paxos && cabal install --only-dependencies -j4

# Adding and installing executable
COPY dist/build/paxos/paxos /paxos/paxos

#Setting up the application as the entry point of the container
ENTRYPOINT ["/paxos/paxos"]
