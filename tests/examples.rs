use assert_cmd::Command;
use predicates::prelude::*;

type TestResult = Result<(), Box<dyn std::error::Error>>;

#[test]
fn starter() -> TestResult {
    Command::cargo_bin("deque")?
        .assert()
        .failure()
        .stderr(predicate::str::contains("error: The following required arguments were not provided:
    <SOURCE_FILE>...

USAGE:
    deque [FLAGS] <SOURCE_FILE>...

For more information try --help"));
    Ok(())
}

#[test]
fn fibonacci() -> TestResult {
    Command::cargo_bin("deque")?
        .arg("examples/fibonacci.deque")
        .assert()
        .success()
        .stdout(predicate::str::contains("0
1
1
2
3
5
8
13
21
34
55
89"));
    Ok(())
}

#[test]
fn prime() -> TestResult {
    Command::cargo_bin("deque")?
        .arg("examples/prime.deque")
        .assert()
        .success()
        .stdout(predicate::str::contains("0
1
2
3
5
7"));
    Ok(())
}

#[test]
fn fizzbuzz() -> TestResult {
    Command::cargo_bin("deque")?
        .arg("examples/fizzbuzz.deque")
        .assert()
        .success()
        .stdout(predicate::str::contains("1
2
fizz
4
buzz
fizz
7
8
fizz
buzz
11
fizz
13
14
fizzbuzz"));
    Ok(())
}
