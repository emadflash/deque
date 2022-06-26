use deque::env::Env;
use deque::eval::Eval;
use std::{env, error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().skip(1).collect::<Vec<_>>();

    if args.is_empty() {
        eprintln!("Usage: deque SOURCE_FILE");
        return Ok(());
    }

    let mut env = Env::new();
    let mut eval = Eval::new(&mut env);

    let src = fs::read_to_string(&args[0])?;
    eval.eval(&src).unwrap();
    eprintln!("{:?}", env.deque);

    Ok(())
}
