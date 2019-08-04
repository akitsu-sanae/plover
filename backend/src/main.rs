#[macro_use]
extern crate lambda_runtime as lambda;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate log;
extern crate simple_logger;

use lambda::error::HandlerError;
use std::error::Error;

#[derive(Deserialize, Clone)]
struct CustomEvent {
    #[serde(rename = "smt_source")]
    smt_source: String,
}

#[derive(Serialize, Clone)]
struct CustomOutput {
    exit: i32,
    stdout: String,
    stderr: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    simple_logger::init_with_level(log::Level::Info)?;
    lambda!(my_handler);

    Ok(())
}

fn my_handler(e: CustomEvent, _c: lambda::Context) -> Result<CustomOutput, HandlerError> {
    {
        use std::fs::File;
        use std::io::Write;
        let mut tmp = File::create("/tmp/source.smt2").unwrap();
        tmp.write_all(e.smt_source.as_bytes()).unwrap();
    }
    use std::process::Command;
    let output = Command::new("./z3")
        .arg("/tmp/source.smt2")
        .output()
        .expect("failed to execute z3");
    Ok(CustomOutput {
        exit: output.status.code().unwrap(),
        stdout: String::from_utf8(output.stdout).unwrap(),
        stderr: String::from_utf8(output.stderr).unwrap(),
    })
}
