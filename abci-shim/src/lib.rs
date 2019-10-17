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

#[no_mangle]
extern "C" fn execute_wasm(script: *const u8, len: usize) {
    use wasmer_runtime::{
        error,
        imports,
        instantiate,
        Value,
    };

    let wasm = unsafe {
        std::slice::from_raw_parts(
            script,
            len,
        )
    };

    fn wasm_runner(script: &[u8]) -> error::Result<bool> {
        // We're not importing anything, so make an empty import object.
        let import_object = imports! {};
        let instance      = instantiate(script, &import_object)?;
        let values        = instance.dyn_func("add_one")?.call(&[Value::I32(100)])?;

        if values[0] == Value::I32(101) {
            println!("[wasm] Ran N + 1: 100 + 1 = {:?}", values[0]);
            Ok(true)
        } else {
            println!("[wasm] Ran N + 1: 100 + 1 = {:?}", values[0]);
            Ok(false)
        }
    }

    match wasm_runner(wasm) {
        Err(e)    => println!("[wasm] WASM Execution Failed:  {:?}", e),
        Ok(true)  => println!("[wasm] WASM Execution Success!"),
        Ok(false) => println!("[wasm] WASM Execution Logic Failed!"),
    }
}
