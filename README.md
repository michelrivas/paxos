Paxos in Haskell
================

Implementation of Paxos in Haskell


Instructions
-------------------

### Install dependencies and compile

```bash
$ cabal update && cabal install --only-dependencies --enable-tests
$ cabal build
```

### Command line

Run multiple instances of the program, specifying IP address and port of the other instances

#### Unix
```bash
$ dist/build/paxos/paxos localhost:5555 localhost:6666 localhost:7777
$ dist/build/paxos/paxos localhost:6666 localhost:7777
$ dist/build/paxos/paxos localhost:7777
```

#### Windows

```bash
$ start dist/build/paxos/paxos.exe localhost:5555 localhost:6666 localhost:7777
$ start dist/build/paxos/paxos.exe localhost:6666 localhost:7777
$ start dist/build/paxos/paxos.exe localhost:7777
```
or

```bash
$ run.bat
```

### Docker

```bash
$ docker-compose up
```


### Instances

To start a round of the algorithm you can enter a number in any console where the program is running

### Tests

```bash
$ runhaskell -isrc -itest test/Spec.hs
```
