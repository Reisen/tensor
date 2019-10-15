#![feature(custom_inner_attributes)]
#![rustfmt::skip]

//! This module is a shim around rust-abci. Why? Well there is a Haskell ABCI
//! implementation but it's currently unmaintained. We COULD update it or write
//! our own, but It's more trouble than It's worth: it entails dealing with
//! protobuf, translating all the types sent over the wire, and worst of all:
//! constantly keeping up with development of Tendermint.
//!
//! Fuck that.
//!
//! Instead, this module will hook into tendermint's official rust-abci library,
//! and provide a more stable interface that Haskell can call into using basic C
//! FFI calls.
// -----------------------------------------------------------------------------

type CheckTxCallback   = fn();
type DeliverTxCallback = fn();
type CommitCallback    = fn();

struct Context {
    check_tx:   CheckTxCallback,
    deliver_tx: DeliverTxCallback,
    commit:     CommitCallback,
}

fn empty_callback() {
}

/// Define a Global Context in the library. This is quite an anti-pattern but we
/// are specifically designing the library to be linked against an ABCI
/// implementation. As such this saves us managing the lifetimes of the context
/// and eliminates allocation/de-allocation pains for the consumer.
static mut CONTEXT: Context = Context {
    check_tx:   empty_callback,
    deliver_tx: empty_callback,
    commit:     empty_callback,
};

// -----------------------------------------------------------------------------
// Define ABCI Application

struct Tensor {
    context: &'static Context,
}

impl Tensor {
    unsafe fn initialize() -> Self {
        Self { context: &CONTEXT }
    }
}

impl abci::Application for Tensor {
    // Implement CheckTX and do nothing.
    fn check_tx(&mut self, _req: &abci::RequestCheckTx) -> abci::ResponseCheckTx {
        println!("[tensor-shim] check_tx called");
        (self.context.check_tx)();
        let response = abci::ResponseCheckTx::new();
        response
    }

    // Implement DeliveryTx and do nothing.
    fn deliver_tx(&mut self, _req: &abci::RequestDeliverTx) -> abci::ResponseDeliverTx {
        println!("[tensor-shim] deliver_tx called");
        (self.context.deliver_tx)();
        abci::ResponseDeliverTx::new()
    }

    // Implement commit and do nothing.
    fn commit(&mut self, _req: &abci::RequestCommit) -> abci::ResponseCommit {
        println!("[tensor-shim] commit called");
        (self.context.commit)();
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

#[no_mangle]
extern "C" fn register_abci_callback(
    _ctx:       &'static mut Context,
    check_tx:   CheckTxCallback,
    deliver_tx: DeliverTxCallback,
    commit:     CommitCallback,
) {
    // Populate Context
    println!("[tensor-shim] Registering Context Callbacks");
    _ctx.check_tx   = check_tx;
    _ctx.deliver_tx = deliver_tx;
    _ctx.commit     = commit;

    // Construct Tensor
    println!("[tensor-shim] Construct Tensor ABCI Application");
    let tensor  = unsafe { Tensor::initialize() };
    let address = "127.0.0.1:26658".parse().unwrap();

    println!("[tensor-shim] Start Tendermint...");
    abci::run(address, tensor);
}
