#[macro_use]
extern crate lambda_runtime as lambda;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate log;
extern crate simple_logger;

mod z3;

use lambda::error::HandlerError;
use std::error::Error;

#[derive(Deserialize, Clone)]
enum Argments {
    Z3(z3::Argments),
}

impl Argments {
    fn to_commandline(&self) -> Vec<String> {
        use Argments::Z3;
        match self {
            Z3(z3_argments) => z3_argments.to_commandline(),
        }
    }
}

#[derive(Deserialize, Clone)]
struct Event {
    #[serde(rename = "src")]
    src: String,
    #[serde(rename = "argments")]
    argments: Argments,
}

#[derive(Serialize, Clone)]
struct Output {
    exit: i32,
    stdout: String,
    stderr: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    simple_logger::init_with_level(log::Level::Info)?;
    lambda!(handler);

    Ok(())
}

fn handler(e: Event, _c: lambda::Context) -> Result<Output, HandlerError> {
    {
        use std::fs::File;
        use std::io::Write;
        let mut tmp = File::create("/tmp/source.smt2").unwrap();
        tmp.write_all(e.src.as_bytes()).unwrap();
    }

    let args = e.argments.to_commandline();
    let args: Vec<&str> = args.iter().map(|arg| arg.as_str()).collect();

    use std::process::Command;
    let output = Command::new("./z3")
        .args(args.into_iter())
        .arg("/tmp/source.smt2")
        .output()
        .expect("failed to execute z3");
    Ok(Output {
        exit: output.status.code().unwrap(),
        stdout: String::from_utf8(output.stdout).unwrap(),
        stderr: String::from_utf8(output.stderr).unwrap(),
    })
}
