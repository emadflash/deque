use std::{env, error::Error, fs};
use deque::env::{Env, eval};

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().skip(1).collect::<Vec<_>>();

    if args.is_empty() {
        eprintln!("Usage: deque SOURCE_FILE");
        return Ok(());
    }

    let mut env = Env::new();
    let src = fs::read_to_string(&args[0])?;

    eval(&mut env, &src).unwrap();
    eprintln!("{:?}", env.deque);

    Ok(())
}
