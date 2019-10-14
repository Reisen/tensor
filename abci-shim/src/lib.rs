//! This module is a shim around rust-abci. Why? Well there is a Haskell ABCI implementation but
//! it's currently unmaintained. We COULD update it or write our own, but It's more trouble than
//! It's worth: it entails dealing with protobuf, translating all the types sent of the wire and
//! worst of all constantly keeping up with development of Tendermint.
//!
//! Fuck that.
//!
//! Instead, this module will hook into tendermint's official rust-abci library, and provide a more
//! stable interface that Haskell can call into using basic C FFI calls.
