mod lex;

use std::fs;

#[test]
#[should_panic(expected = "always false")]
fn it_works() {
  assert!(false, "always false");
}

#[test]
fn test_lex() {
    let raw = "if end = hoge ) +";
    let raw_chars: Vec<char> = raw.chars().collect();
    let tokens = match lex::lex(&raw_chars){
        Ok(tokens) => tokens,
        Err(msg) => panic!("{}", msg),
    };

    assert!(tokens[0].value == "if")

}
