Tensor Chain
--------------------------------------------------------------------------------

This mini-project is a place to experiment with blockchain ideas. In
particular, to play around with some of the more complex unanswered questions
that many projects are currently trying to solve. In particular, two very interesting
areas of blockchain research are:

* Synchrony in PBFT style chains. (Tendermint)
* Efficient Leader Election in Fork Selection style chains. (Bitcoin)


## Synchrony

Solving synchrony with PBFT style chains comes down to something known as [The
impossibility result][1]. This is the discovery that it is impossible to solve
the consensus problem in a fully distributed, fully asynchronous system. You
only need _one_ participant to fail for the entire system to break down. This
comes with a couple of caveats, it _is_ possible if:

1. You enforce synchrony. (Duh)
2. You accept non-determinism in the consensus algorithm

Most chains currently solve this by enforcing partial-synchrony (fake
synchrony) by adding timeouts to the system. This comes with its own downsides.
To truly solve the problem, the two above caveats are of interest:

### Enforcing Synchrony (1)
Solana achieves this by using a repeated hash function to create a shared
stream of hashes that everyone agrees on. A hash at some point in the chain is
then treated as a timestamp. The benefit from doing this is that the hash
stream can be treated as a shared clock. Any two parties can agree that at hash
`N` some event happened, and its ordering with respect to other hashes acts the
same way that a timestamp with a real clock does relative to other timestamps.
This is known as [Proof of History][2]

### Allowing Non-Determinism (2)
The other solution is to allow non-determinism, this is what HashGraph does. I
can't write about it yet because I don't fully grasp it. Article of intrest found
[here][3].


## Voting Style

There are two styles of blockchain consensus at the moment.

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

This Repository
--------------------------------------------------------------------------------

This repository is a place to play with these ideas, especially interesting is
seeing how to combine them. Nirvana is a PoW style chain with 6 figure
transactions per second characteristics, all while maintaining the security of
a permissioned system and the security requirement of 51%

Probably impossible.

[1]: https://groups.csail.mit.edu/tds/papers/Lynch/jacm85.pdf
[2]: https://medium.com/solana-labs/proof-of-history-a-clock-for-blockchain-cf47a61a9274
[3]: https://hackernoon.com/demystifying-hashgraph-benefits-and-challenges-d605e5c0cee5
[4]: https://dahliamalkhi.files.wordpress.com/2016/08/blockchainbft-beatcs2017.pdf
