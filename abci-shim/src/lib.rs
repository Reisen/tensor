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



// These types match the callbacks passed in from Haskell land.

type CheckTxCallback   = fn(*const u8, usize);
type DeliverTxCallback = fn(*const u8, usize);
type CommitCallback    = fn();

// Our context carries pointers into Haskell land to handle the various ABCI
// messages coming over the wire.

struct Context {
    check_tx:   CheckTxCallback,
    deliver_tx: DeliverTxCallback,
    commit:     CommitCallback,
}

/// Define a Global Context in the library. This is quite an anti-pattern but we
/// are specifically designing the library to be linked against an ABCI
/// implementation. As such this saves us managing the lifetimes of the context
/// and eliminates allocation/de-allocation pains for the consumer.

static mut CONTEXT: Context = Context {
    check_tx:   |_, _| {},
    deliver_tx: |_, _| {},
    commit:     || {},
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

// Implement ABCI handler. All of these functions simply call forward into Haskell
// land where we'll implement all the block characteristics.

impl abci::Application for Tensor {
    fn check_tx(&mut self, _req: &abci::RequestCheckTx) -> abci::ResponseCheckTx {
        println!("[tensor-shim] check_tx called");

        let request_bytes = _req.get_tx();
        let raw_bytes     = request_bytes.as_ptr();
        let raw_length    = request_bytes.len();
        (self.context.check_tx)(raw_bytes, raw_length);
        abci::ResponseCheckTx::new()
    }

    fn deliver_tx(&mut self, _req: &abci::RequestDeliverTx) -> abci::ResponseDeliverTx {
        println!("[tensor-shim] deliver_tx called");

        let request_bytes = _req.get_tx();
        let raw_bytes     = request_bytes.as_ptr();
        let raw_length    = request_bytes.len();
        (self.context.deliver_tx)(raw_bytes, raw_length);
        abci::ResponseDeliverTx::new()
    }

    fn commit(&mut self, _req: &abci::RequestCommit) -> abci::ResponseCommit {
        println!("[tensor-shim] commit called");

        (self.context.commit)();
        abci::ResponseCommit::new()
    }
}



// -----------------------------------------------------------------------------
// Define C Interface



#[no_mangle]
unsafe extern "C" fn get_abci_context() -> *mut Context {
    println!("[tensor-shim] Getting Context");
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

static WASM: &'static [u8] = &[
    // The module above compiled to bytecode goes here.
    0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x06, 0x01, 0x60,
    0x01, 0x7f, 0x01, 0x7f, 0x03, 0x02, 0x01, 0x00, 0x07, 0x0b, 0x01, 0x07,
    0x61, 0x64, 0x64, 0x5f, 0x6f, 0x6e, 0x65, 0x00, 0x00, 0x0a, 0x09, 0x01,
    0x07, 0x00, 0x20, 0x00, 0x41, 0x01, 0x6a, 0x0b, 0x00, 0x1a, 0x04, 0x6e,
    0x61, 0x6d, 0x65, 0x01, 0x0a, 0x01, 0x00, 0x07, 0x61, 0x64, 0x64, 0x5f,
    0x6f, 0x6e, 0x65, 0x02, 0x07, 0x01, 0x00, 0x01, 0x00, 0x02, 0x70, 0x30,
];

#[no_mangle]
extern "C" fn execute_wasm(script: &[u8]) {
    use wasmer_runtime::{
        error,
        imports,
        instantiate,
        Value,
    };

    fn wasm_runner(script: &[u8]) -> error::Result<()> {
        // We're not importing anything, so make an empty import object.
        let import_object = imports! {};
        let instance      = instantiate(script, &import_object)?;
        let values        = instance.dyn_func("add_one")?.call(&[Value::I32(42)])?;

        assert_eq!(
            values[0],
            Value::I32(43)
        );

        Ok(())
    }

    match wasm_runner(script) {
        Err(e) => println!("WASM Execution Failed:  {:?}", e),
        Ok(()) => println!("WASM Execution Success!"),
    }
}
