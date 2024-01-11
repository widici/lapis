use ast::{Expression, ExpressionEnum, Statement, StatementEnum};
use error::ErrorKind::{Unexpected, UnexpectedExpected, DuplicateParam};
use error::{impl_error_handling, Error, ErrorLocation};
use lexer::token::{Op, Token, TokenType};
use span::Span;
use std::cmp::Ordering;

pub struct Parser {
    tokens: Vec<Token>,
    current_pos: usize,
    current_token: Token,
    start_stack: Vec<usize>,
    current_expr_id: usize,
    errors: Vec<Error>,
}

impl_error_handling!(Parser, ErrorLocation::Parser);

impl Parser {
    #[must_use]
    pub fn new(tokens: Vec<Token>) -> Parser {
        let current_token = Token::new(TokenType::EOF, Span::new(0, 0));
        let mut parser = Parser {
            tokens,
            current_pos: 0,
            current_token,
            start_stack: Vec::new(),
            current_expr_id: 0,
            errors: Vec::new(),
        };
        parser.current_token = parser.get_token();
        parser
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let result = self.parse_inner(false);
        self.report_errors();
        result
    }

    fn parse_inner(&mut self, is_inner: bool) -> Vec<Statement> {
        let mut stmts: Vec<Statement> = Vec::new();
        loop {
            match self.current_token.tt.clone() {
                TokenType::EOF => break,
                TokenType::RCurly => {
                    //self.advance(); // Consumes the rcurly
                    if is_inner {
                        break;
                    }
                    self.add_error(Unexpected {
                        found: Box::new(self.current_token.clone()),
                    });
                    // TODO: Maybe add report here?
                }
                _ => {
                    stmts.push(self.parse_stmt());
                    //self.advance();
                }
            }
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Statement {
        self.add_start(self.current_token.span.start);
        let stmt = match &self.current_token.tt {
            TokenType::Var => self.parse_var_decl(),
            TokenType::LCurly => self.parse_block(),
            TokenType::Ident(_) => {
                if self.peek_next_op().is_some() {
                    self.parse_var_assign()
                } else if let Some(token) = self.peek_token() {
                    if token.tt == TokenType::LParen {
                        StatementEnum::Expression(self.parse_call())
                    } else {
                        self.add_error(UnexpectedExpected {
                            expected: format!("{:?}", TokenType::LParen),
                            found: Box::new(token),
                        });
                        self.report_errors();
                        unreachable!()
                    }
                } else {
                    unimplemented!()
                }
            }
            TokenType::Return => {
                self.advance();
                StatementEnum::Return {
                    expr: self.parse_expr(),
                }
            }
            TokenType::Fn => self.parse_fn_decl(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::Break => StatementEnum::Break,
            TokenType::Continue => StatementEnum::Continue,
            _ => StatementEnum::Expression(self.parse_expr()),
        };
        let result = self.construct_stmt(stmt);

        self.advance();
        result
    }

    fn peek_next_op(&self) -> Option<Op> {
        let mut count = 0;
        loop {
            match self.tokens[self.current_pos + count].tt {
                //TokenType::ILLEGAL { .. } => {}
                TokenType::EOF | TokenType::Comma | TokenType::RParen | TokenType::RCurly => {
                    return None
                }
                TokenType::Op(op) => return Some(op),
                _ => count += 1,
            }
        }
    }

    fn peek_token(&self) -> Option<Token> {
        if self.current_pos == self.tokens.len() - 1 {
            None
        } else {
            Some(self.tokens[self.current_pos + 1].clone())
        }
    }

    fn get_token(&self) -> Token {
        self.tokens[self.current_pos].clone()
    }

    fn advance(&mut self) {
        self.current_pos += 1;
        self.current_token = self.get_token()
    }

    fn parse_ident(&mut self) -> Token {
        let ident = match &self.current_token.tt {
            TokenType::Ident(_) => self.current_token.clone(),
            _ => {
                self.add_error(UnexpectedExpected { expected: String::from("TokenType::Ident"), found: Box::new(self.current_token.clone()) });
                self.report_errors();
                unreachable!()
            },
        };
        self.advance();
        ident
    }

    fn parse_var_decl(&mut self) -> StatementEnum {
        self.advance(); // Consumes the var-kw
        let ident = self.parse_ident();

        assert_eq!(&self.current_token.tt, &TokenType::Op(Op::Eq));
        self.advance();

        let expr = self.parse_expr();
        StatementEnum::VarDeclaration { ident, expr }
    }

    fn parse_var_assign(&mut self) -> StatementEnum {
        self.add_start(self.current_token.span.start);
        let ident = self.parse_ident();

        let operator = match self.current_token.tt {
            TokenType::Op(op) => match op {
                Op::Eq => self.get_token(),
                _ => {
                    let token = self.get_token();
                    self.advance(); // Consumes the eq
                    assert_eq!(self.current_token.tt, TokenType::Op(Op::Eq));
                    token
                }
            },
            _ => {
                self.add_error(UnexpectedExpected { expected: String::from("TokenType::Op"), found: Box::new(self.current_token.clone()) });
                self.report_errors();
                unreachable!()
            }
        };
        self.advance();

        let left = self.parse_expr();
        // x op= 10 -> x = x op 10
        let right = match operator.tt {
            TokenType::Op(Op::Eq) => left,
            _ => {
                // Temporary solution
                self.add_start(left.span.start); // Push for right
                self.add_start(left.span.start); // Push for expr
                let right = self.construct_expr(ExpressionEnum::Var {
                    ident: ident.clone(),
                });
                let expr = ExpressionEnum::BinOp {
                    left,
                    operator,
                    right,
                };
                self.construct_expr(expr)
            }
        };
        StatementEnum::Expression(self.construct_expr(ExpressionEnum::Assignment { ident, right }))
    }

    fn parse_block(&mut self) -> StatementEnum {
        assert_eq!(self.current_token.tt, TokenType::LCurly);
        self.advance(); // Consumes the lcurly
        StatementEnum::Block {
            stmts: self.parse_inner(true),
        }
    }

    fn parse_fn_decl(&mut self) -> StatementEnum {
        self.advance(); // Consumes the fn-kw
        let ident = self.parse_ident();

        assert_eq!(self.current_token.tt, TokenType::LParen);
        self.advance(); // Consumes the lparen

        let mut params: Vec<Token> = Vec::new();
        while ![TokenType::RParen, TokenType::EOF].contains(&self.current_token.tt) {
            let ident = self.parse_ident();

            match &self.current_token.tt {
                TokenType::Comma => self.advance(),
                TokenType::RParen => {}
                _ => {
                    self.add_error(UnexpectedExpected { expected: format!("{:?} or {:?}", TokenType::Comma, TokenType::RParen), found: Box::new(self.current_token.clone()) })
                },
            }

            if params.contains(&ident) {
                self.add_error(DuplicateParam { param: Box::new(ident.clone()) })
            }

            params.push(ident)
        }
        self.report_errors();
        self.advance(); // Consume the rparen

        self.add_start(self.current_token.span.start);
        self.advance(); // Consumes the lparen
        let stmts = self.parse_inner(true);
        //info!("{:?}", block);
        StatementEnum::FnDeclaration {
            ident,
            params,
            stmts,
        }
    }

    fn parse_if(&mut self) -> StatementEnum {
        let if_branch = self.parse_cond_branch();

        let mut elif_branches: Vec<(Expression, Statement)> = Vec::new();
        while self.peek_token().unwrap().tt == TokenType::Elif {
            self.advance(); // Consumes the rcurly
            elif_branches.push(self.parse_cond_branch());
        }

        let else_branch: Option<Statement> = if self.peek_token().unwrap().tt == TokenType::Else {
            self.advance(); // Consumes the rcurly
            self.advance(); // Consumes the else-kw
            self.add_start(self.current_token.span.start);
            let block = self.parse_block();
            Some(self.construct_stmt(block))
        } else {
            None
        };

        StatementEnum::If {
            if_branch,
            elif_branches,
            else_branch,
        }
    }

    fn parse_cond_branch(&mut self) -> (Expression, Statement) {
        self.advance();
        let condition = self.parse_expr();
        self.advance();
        self.add_start(self.current_token.span.start);
        let block = self.parse_block();
        let block = self.construct_stmt(block);
        //self.advance();
        (condition, block)
    }

    fn parse_while(&mut self) -> StatementEnum {
        self.advance(); // Consumes the while-kw
        let condition = self.parse_expr();
        self.advance();
        self.add_start(self.current_token.span.start);
        let block = self.parse_block();
        let block = self.construct_stmt(block);
        StatementEnum::While { condition, block }
    }

    fn parse_call(&mut self) -> Expression {
        self.add_start(self.current_token.span.start);
        let ident = self.parse_ident();
        assert_eq!(self.current_token.tt, TokenType::LParen);
        self.advance();

        let mut params: Vec<Expression> = Vec::new();
        while ![TokenType::RParen, TokenType::EOF].contains(&self.current_token.tt) {
            params.push(self.parse_expr());
            self.advance();
            if self.current_token.tt != TokenType::RParen {
                self.advance()
            }
        }

        self.construct_expr(ExpressionEnum::Call { ident, params })
    }

    fn parse_operand(&mut self) -> Expression {
        match &self.current_token.tt {
            TokenType::Literal(_) => {
                let literal = self.get_token();
                self.add_start(self.get_token().span.start);
                self.construct_expr(ExpressionEnum::Literal(literal))
            }
            TokenType::LParen => {
                self.advance(); // Consumes the lparen
                let expr_result = self.parse_expr();
                self.advance();
                expr_result
            }
            TokenType::Op(op) => match op {
                Op::Sub | Op::Not => {
                    let operator = self.get_token();
                    self.add_start(self.current_token.span.start);
                    self.advance(); // Consume the sub/not
                    let expr = self.parse_operand();
                    self.construct_expr(ExpressionEnum::UnaryOp { operator, expr })
                }
                _ => {
                    self.add_error(UnexpectedExpected { expected: format!("{:?} or {:?}", Op::Sub, Op::Not), found: Box::new(self.current_token.clone()) });
                    self.report_errors();
                    unreachable!()
                },
            },
            TokenType::Ident(_) => {
                if let Some(Token {
                    tt: TokenType::LParen,
                    ..
                }) = self.peek_token()
                {
                    self.parse_call()
                } else {
                    let ident = self.get_token();
                    self.add_start(self.current_token.span.start);
                    self.construct_expr(ExpressionEnum::Var { ident })
                }
            }
            other => panic!("{:?}", other),
        }
    }

    fn parse_expr(&mut self) -> Expression {
        self.parse_expr_inner(false)
    }

    fn parse_expr_inner(&mut self, is_inner: bool) -> Expression {
        let mut left = self.parse_operand();
        if let Some(token) = self.peek_token() {
            if let TokenType::Op(..) = token.tt {
                self.advance()
            }
        }

        while let TokenType::Op(operator) = self.current_token.tt {
            let token_op = self.get_token();
            self.add_start(left.span.start);
            self.advance(); // Consumes the operator
            let next_op = self.peek_next_op().unwrap_or(operator);

            left = match next_op.partial_cmp(&operator) {
                Some(Ordering::Greater) => self.parse_greater(token_op, left),
                Some(Ordering::Equal) => self.parse_equal(token_op, left),
                Some(Ordering::Less) => {
                    let result = self.parse_equal(token_op, left);
                    match is_inner {
                        true => return result,
                        false => result,
                    }
                }
                None => unimplemented!(),
            };

            // Validates next token and checks if it's on a new line
            if let Some(mut token) = self.peek_token() {
                match token.tt {
                    TokenType::Literal(..)
                    | TokenType::LParen
                    | TokenType::Op(..)
                    | TokenType::Ident(..) => {
                        if !(token.span.comp_line_col(&mut self.current_token.span)) {
                            return left;
                        }
                    }
                    _ => return left,
                }
            }

            self.advance();
        }

        left // Returns on either eof or rparen
    }

    fn parse_greater(&mut self, operator: Token, left: Expression) -> Expression {
        let right = self.parse_expr_inner(true);
        self.construct_expr(ExpressionEnum::BinOp {
            left,
            operator,
            right,
        })
    }

    fn parse_equal(&mut self, operator: Token, left: Expression) -> Expression {
        let right = self.parse_operand();
        self.construct_expr(ExpressionEnum::BinOp {
            left,
            operator,
            right,
        })
    }

    fn construct_expr(&mut self, expr: ExpressionEnum) -> Expression {
        self.current_expr_id += 1;
        Expression {
            expr_enum: Box::new(expr),
            span: self.get_span(),
            id: self.current_expr_id,
        }
    }

    fn construct_stmt(&mut self, stmt: StatementEnum) -> Statement {
        Statement {
            stmt_enum: Box::new(stmt),
            span: self.get_span(),
        }
    }

    fn get_span(&mut self) -> Span {
        (self.get_start(), self.current_token.span.end).into()
    }

    fn add_start(&mut self, start: usize) {
        self.start_stack.push(start);
    }

    fn get_start(&mut self) -> usize {
        match self.start_stack.pop() {
            Some(start) => start,
            None => unreachable!(),
        }
    }
}

/*
#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;

    #[test]
    fn test_expr_parsing() {
        let test_cases = ["1 + 2 * 3", "1 * 2 + 3"];

        for case in test_cases {
            let chars: Vec<char> = case.chars().collect();
            let mut lexer = Lexer::new(chars);
            let tokens = lexer.get_tokens();
            let mut parser = Parser::new(tokens);
            let result = parser.parse_expr();
        }
    }
}
*/

// 3

//   +  1 + | 2 * 3 |
//  / \
//  1  *
//    / \
//    2  3

// 1 + 2 * 3 Greater Scenario
// push 1
// push 2
// pop eq
// pop gt

// 1 * 2 * 3 or 1 * 2 + 3 Equal/Lesser Scenario
// push 1
// pop eq
// push left (1)
// pop eq
