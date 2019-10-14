//! This module is a shim around rust-abci. Why? Well there is a Haskell ABCI implementation but
//! it's currently unmaintained. We COULD update it or write our own, but It's more trouble than
//! It's worth: it entails dealing with protobuf, translating all the types sent over the wire, and
//! worst of all: constantly keeping up with development of Tendermint.
//!
//! Fuck that.
//!
//! Instead, this module will hook into tendermint's official rust-abci library, and provide a more
//! stable interface that Haskell can call into using basic C FFI calls.
//!
// -----------------------------------------------------------------------------

struct Context {
    callback: Callback,
}

fn empty_callback() {}

/// Define a Global Context in the library. This is quite an anti-pattern but we are specifically
/// designing the library to be linked against an ABCI implementation. As such this saves us
/// managing the lifetimes of the context and eliminates allocation/de-allocation pains for the
/// consumer.
static mut CONTEXT: Context = Context {
    callback: empty_callback,
};

// -----------------------------------------------------------------------------
// Define ABCI Application

struct Tensor {
    context: &'static Context,
}

impl abci::Application for Tensor {
    // Implement CheckTX and do nothing.
    fn check_tx(&mut self, _req: &abci::RequestCheckTx) -> abci::ResponseCheckTx {
        let response = abci::ResponseCheckTx::new();
        response
    }

    // Implement DeliveryTx and do nothing.
    fn deliver_tx(&mut self, _req: &abci::RequestDeliverTx) -> abci::ResponseDeliverTx {
        abci::ResponseDeliverTx::new()
    }

    // Implement commit and do nothing.
    fn commit(&mut self, _req: &abci::RequestCommit) -> abci::ResponseCommit {
        let response = abci::ResponseCommit::new();
        response
    }
}

// -----------------------------------------------------------------------------
// Define C Interface

#[no_mangle]
unsafe extern "C" fn get_abci_context() -> *mut Context {
    return &mut CONTEXT as *mut Context;
}

type Callback = fn();

#[no_mangle]
extern "C" fn register_abci_callback(_ctx: &'static mut Context, callback: Callback) {
    _ctx.callback = callback;
    let address = "127.0.0.1:26658".parse().unwrap();
    abci::run(address, Tensor { context: _ctx });
}
