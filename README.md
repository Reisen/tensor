Tensor Chain
--------------------------------------------------------------------------------

This mini-project is a place to experiment with blockchain ideas. In
particular, to play around with some of the more complex unanswered questions
that many projects are currently trying to solve. **To get caught up**, here is
a metric ton of resources. They can be read in the order of listing:

### On Consensus Itself

- (Blog) [How Does Distributed Consensus Work?][source1]  
  _An overview of key breakthroughs in blockchain technology — and why Nakamoto
  Consensus is such a big deal._

  This is the best first introduction article I have come accross. It also is
  written with a lot of respet to why PoW is an incredible innovation, rather
  than just plugging PoS. It covers history, but also doesn't shy away from
  introducing the hardcore underlying papers and theories.

- (Blog) [Satoshi’s Most Misleading Paragraph][source2]

  This article focuses on one of Satoshi's notes regarding voting. It can be
  skipped but it is one of several great articles that show that Bitcoin's
  Nakamoto Consensus is still inherently around voting and not necessarily
  about PoW or hashing at all.

- (Blog) [Consensus, Two Phase and Three Phase Commits][source3]

  Almost all current Proof of Stake implementations are actually
  implementations of Three Phase Commit ([3PC][]). Even though it doesn't
  specifically talk about consensus this article gets to the core of why
  consensus protocols themselves are built on top of either [2PC][] (Raft) and
  3PC (Tendermint, found in the form of Prevote, Precommit, Commit).

- (Blog) [Blockchain Proof-of-Work Is a Decentralized Clock][source4]

  In a similar vein to the article above about Satoshi's Most Misleading
  Paragraph; this article points out that most don't fundamentally understand
  the problem that Nakamoto attempted to solve. Most importantly, blockchain is
  not just a solution to decentralized consensus, it is specifically
  distributed consensus about _ordering_ and _time_.

  This is the first article that pushes in the direction that Solana is trying
  to solve, and implication that consensus is fundamentally about agreement on
  ordering (Proof of History).

- (Paper) Lamports Papers

  Lamport is basically the godfather of consensus. His papers aren't too bad to
  read, they take some time to process but they are not impenetrable. All his
  papers are not only on Microsoft Research, but have commentary added by him
  along with each paper that I recommend reading just as much as the papers
  themselves. As such all these links link to the MS Research pages:

  - [The Implementation of Reliable Distributed Multiprocess Systems][source5]

  This was how Lamport originally started with the problem, in an attempt to
  design an algorithm on a multiprocessor PC. It doesn't consider any kinds of
  failures at all.

  - [Reaching Agreement in the Presence of Faults][source6]

  This was the first tiem Lamport considered failures. In particular he was
  still only considering a single PC with multiple processors, and how a
  multi-core algorithm using message passing could handle failures. This was
  the first time the figures `2n+1` and `3n+1` for handling failures are
  presented.

  This is the paper that won Lamport the Djikstra Award.

  - [The Byzantine Generals Problem][source7]

  This is where Lamport finally extracted the concept of purely arbitrary, or
  even attacker behaviour in a system. And how to approach it. It's the first
  time the problem is presented in the context of intentional attack (hence the
  reformulation of the problem with soldiers).

  - [Synchronizing Clocks in the Presence of Faults][source8]

  One of the conclusions from the previous paper is that an actual solution to
  the byzantine problem requires synchronization. This paper covers how
  multiple systems can synchronize clocks in a fault-tolerant way.

  - [Time, Clocks and the Ordering of Events in a Distributed System][source9]

  The previous paper gives us clock synchronization among our systems. This
  leads to one of Lamports most amazing papers. It describes how, once you do
  have a synchronized clock, it is possible to define distributed fault
  tolerant state machines. This is literally the definition of pretty much
  _any_ blockchain you can name. They are all distributed fault tolerant state
  machines.

  > The basic message of this paper should have been pretty obvious: the state
  > machine approach, introduced in [27], allows us to turn any consensus
  > algorithm into a general method for implementing distributed systems.

  This paper won award after award, most influential paper, another Djikstra
  prize, as well as the ACM SIGOPS Hall of Fame award. All the previous papers
  lead up to this, probably the most important paper.

  - [Using Time Instead of Timeout for Fault-Tolerant Distributed Systems][source10]

  This paper is incredible. The previous papers show how with synchronized
  clocks we can build these powerful fault tolerant state machines. The problem
  then is, how do we synchronize clocks among thousands of computers of which
  we do not control? (Ala Bitcoin) This takes us back to the statement above
  which is that Bitcoin is a solution to the distributed clock problem.

  This paper is a literal description of Solana, with a magical synchronized
  clock it becomes possible to eliminate timeouts from consensus algorithms.

- (Paper) [The Blockchain Consensus Layer and BFT][source11]

  With all the above under your belt, this paper will take all of it and
  compare Nakamoto Consensus and PBFT to extract and compare the underlying
  similarities in such a way that the core concepts of consensus are in view.
  It also covers history of bitcoin-ng, ByzCoin, and Solidus.

- (Paper) [The Bitcoin Backbone Protocol:Analysis and Applications][source12]

  This paper is the first (and only) in depth analysis of Nakamoto Consensus in
  terms of its safety guarantees versus Byzantine Agreement. It's pretty hairy
  but a good test of understanding.

[source1]: https://medium.com/s/story/lets-take-a-crack-at-understanding-distributed-consensus-dad23d0dc95
[source2]: https://medium.com/@elombrozo/satoshis-most-misleading-paragraph-c3d7f8989e6f
[source3]: https://medium.com/@balrajasubbiah/consensus-two-phase-and-three-phase-commits-4e35c1a435ac
[source4]: https://grisha.org/blog/2018/01/23/explaining-proof-of-work/
[source5]: https://www.microsoft.com/en-us/research/publication/implementation-reliable-distributed-multiprocess-systems/
[source6]: https://www.microsoft.com/en-us/research/publication/reaching-agreement-presence-faults/
[source7]: https://www.microsoft.com/en-us/research/publication/byzantine-generals-problem/
[source8]: https://www.microsoft.com/en-us/research/publication/synchronizing-clocks-presence-faults/
[source9]: https://www.microsoft.com/en-us/research/publication/time-clocks-ordering-events-distributed-system/
[source10]: https://www.microsoft.com/en-us/research/publication/using-time-instead-timeout-fault-tolerant-distributed-systems/
[source11]: https://dahliamalkhi.files.wordpress.com/2016/08/blockchainbft-beatcs2017.pdf
[source12]: https://eprint.iacr.org/2014/765.pdf

[2PC]: https://en.wikipedia.org/wiki/Two-phase_commit_protocol
[3PC]: https://en.wikipedia.org/wiki/Three-phase_commit_protocol


This Repository
--------------------------------------------------------------------------------

Two very interesting areas of blockchain research that I want to tackle in this
repository are:

* Synchrony in PBFT style chains. (Tendermint)
* Efficient Leader Election in Fork Selection style chains. (Bitcoin)


### Synchrony

Solving synchrony with PBFT style chains comes down to something known as [The
impossibility result][1]. This is the discovery that it is impossible to solve
the consensus problem in a fully distributed, fully asynchronous system. You
only need _one_ participant to fail for the entire system to break down. This
comes with a couple of caveats in that _it is possible_ if:

1. You enforce synchrony. (Duh)
2. You accept non-determinism in the consensus algorithm

Most chains currently solve this by enforcing partial-synchrony (fake
synchrony) by adding timeouts to the system. This comes with its own downsides.
To truly solve the problem, the above two conditions are of interest. Breaking
down two real solutions as of today:

#### Enforcing Synchrony (1)
Solana achieves this by using a repeated hash function to create a shared
stream of hashes that everyone agrees on. A hash at some point in the chain is
then treated as a timestamp. The benefit from doing this is that the hash
stream can be treated as a shared clock. Any two parties can agree that at hash
`N` some event happened, and its ordering with respect to other hashes acts the
same way that a timestamp with a real clock does relative to other timestamps.
This is known as [Proof of History][2]

#### Allowing Non-Determinism (2)
The other solution is to allow non-determinism, this is what HashGraph does. I
can't write about it yet because I don't fully grasp it. Article of intrest found
[here][3].


### Voting Style

There are two styles of blockchain consensus at the moment in general:

1. Voting and Round Consensus (Tendermint)
2. Fork Selection Conensus (Bitcoin)

The first gives strong finality but weak liveness, the second gives strong
liveness but weak (in fact, probabilistic) finality. The current paper I am
reading regarding this which heavily compares these can be found [here][4].

In this area, I am only interested in (2). PBFT has a thousand people working
on new systems, but very few seem to care about improving (2).

In particular interest, Bitcoin's PoW system can be viewed as an oracle with
some interesting properties:

1. It is completely random.
2. It allows only one block to be proposed at once (important).
3. It is permissionless and will choose any participant.

By reducing the problem to these statements, we can consider alternative
systems to PoW hashing that have the same properties. Of particular interest
are VDF/VRF combinations which are currently being considered for Ethereum 2.0.


The Goal
--------------------------------------------------------------------------------

This repository is a place to play with these ideas, especially interesting is
seeing how to combine them. Nirvana is a PoW style chain with 6 figure
transactions per second characteristics, all while maintaining the security of
a permissioned system with a byzantine failure requirement of 51%

Probably impossible.

[1]: https://groups.csail.mit.edu/tds/papers/Lynch/jacm85.pdf
[2]: https://medium.com/solana-labs/proof-of-history-a-clock-for-blockchain-cf47a61a9274
[3]: https://hackernoon.com/demystifying-hashgraph-benefits-and-challenges-d605e5c0cee5
[4]: https://dahliamalkhi.files.wordpress.com/2016/08/blockchainbft-beatcs2017.pdf
