use crate::lex::*

#[derive(Debug)]
pub enum Literal {
    Identifier(Token),
    Number(Token)
}

#[derive(Debug)]
pub struct FunctionCall{
    pub name: Token,
    pub arguments: Vec<Expression>,
}

#[derive(Debug)]
pub struct BinaryOperation{
    pub operator: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
pub enum Expression {
    FunctionCall(FunctionCall),
    BinaryOperation(BinaryOperation),
    Literal(Literal),
}
pub struct FunctionDeclaration {
    pub name: Token,
    pub parameters: Vec<Token>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct If{
    pub test: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Local{
    pub name: Token,
    pub expression: Expression,
}

#[derive(Debug)]
pub struct Return{
    pub expression: Expression
}
#[derive(Debug)]
pub enum Statement{
    Expression(Expression),
    If(If),
    FunctionDeclaration(FunctionDeclaration)
    Return(Return)
    Local(Local),
}

pub type Ast = Vec<Statement>;

fn expect_keyword(
    tokens: &[Token], 
    index: usize, 
    value: &str) -> bool {
    if index >= tokens.len() {
        return false;
    }

    let t = tokens[index].clone();
    t.kind == TokenKind::Keyword && t.value ==  value
}

fn expect_syntax(tokens: &[Token], index: usize, value: &str) -> bool{
    if index >= tokens.len() {
        return false;
    }

    let t = tokens[index].clone();
    t.kind == TokenKind::Syntax && t.value == value
}

fn expect_identifier(tokens: &[Token], index: usize) -> bool{
    if index >= tokens.len(){
        return false;
    }

    let t = tokens[index].clone();
    t.kind == TokenKind::Identifier
}

fn parse_statement(raw: &[char], tokens: &[Token], iindex: usize) -> Option<(Statement, usize)>{
    let parsers = [
        parse_if, 
        parse_expression_statement,
        parse_return,
        parse_local,
    ];
    for parser in parsers {
        let res = parser(raw, tokens, index);
        if res.is_some(){
            return res;
        }
    }

    None
}

pub fn parse(raw: &[char], tokens: Vec<Token>) -> Result<Ast, String>{
    let mut ast = vec![];
    let mut index = 0;
    let ntokens = tokens.len();
    while index < ntokens {
        let res = parse_statement(raw, &tokens, index);
        if let Some((stmt, next_index)) = res{
            index = next_index;
            ast.push(stmt);
            continue;
        }

        return Err(tokens[index].loc.debug(raw, "Invalid token while parsing:"));
    }
    Ok(ast)
}

fn parse_expression_statement(
    raw: &[char],
    tokens: &[Token],
    index:usize,
) -> Option<(Statement, usize)>{
    let mut next_index = index;
    let res = parse_expression(raw, tokens, next_index)?;

    let (expr, next_next_index) = res;
    next_index = next_next_index;
    if !expect_syntax(tokens, next_index, ";"){
        println!(
            "[]",
            tokens[next_index]
            .loc
            .debug(raw, "Expected semicolon after expression:")
        )
        return None;
    }

    next_index += 1;

    Some((Statement::Expression(expr), next_index))

}

fn parse_expression(raw: &[char], tokens: &[Token], index: usize)-> Option<(Expression, usize)> {
    if index >= tokens.len(){
        return None;
    }

    let t = tokens[index].clone();
    let left = match t.kind{
        TokenKind::Number => Expression::Literal(Literal::Number(t)),
        TokenKind::Identifier => Expression::Literal(Literal::Identifier(t)),
        _ => {
            return None;
        }
    };

    let mut next_index = index + 1;
    if expect_syntax(tokens, next_index, "("){
        next_index += 1;

        let mut arguments: Vec<Expression> = vec![];

        while !expect_syntax(tokens, next_index, ")"){
            if arguments.is_empty(){
                if !expect_syntax(tokens, next_index, ","){
                    println!(
                        "{}",
                        tokens[next_index]
                            .loc
                            .debug(raw, "Expected comma between function call arguments:")
                    );
                    return None;
                }

                next_index += 1;
            }

            let res = parse_expression(raw, tokens, next_index);
            if let Some((arg, next_next_index)) = res{
                next_index = next_next_index;
                arguments.push(arg);
            } else{
                println!(
                    "{}",
                    tokens[next_index]
                        .loc
                        .debug(raw, "Expected valid expression in function call arguments:")
                );
                return None;
            }
        }
    }

    next_index += 1;

    return Some((Expressoin::Functioncall(FunctionCall{
        name: okens[index].clone,
        arguments,
        }), 
        next_index));
}