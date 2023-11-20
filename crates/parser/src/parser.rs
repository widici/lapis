use std::cmp::Ordering::Greater;
use std::cmp::Ordering;
use ast::{Expression, Statement, ExpressionEnum, StatementEnum};
use lexer::token::{Op, TokenType, Literal::{Float, Int}, Token};
use error::span::Span;

pub struct Parser {
    tokens: Vec<Token>,
    current_pos: usize,
    current_token: Token,
    start_stack: Vec<(usize, usize)>
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        let current_token = Token::new(TokenType::EOF, Span::new((0,0), (0, 0)));
        let mut parser = Parser { tokens, current_pos: 0, current_token, start_stack: Vec::new() };
        parser.current_token = parser.get_token();
        return parser
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        self.parse_inner(false)
    }

    fn parse_inner(&mut self, is_inner: bool) -> Vec<Statement> {
        let mut stmts: Vec<Statement> = Vec::new();
        loop {
            match self.current_token.tt {
                TokenType::EOF => break,
                TokenType::RCurly => {
                    //self.advance(); // Consumes the rcurly
                    if is_inner {
                        break
                    } else {
                        unimplemented!()
                    }
                },
                _ => { 
                    stmts.push(self.parse_stmt());
                    //self.advance();
                },
            }
        }
        //println!("Returns: {:?}", self.current_token);
        return stmts
    }

    fn parse_stmt(&mut self) -> Statement {
        //println!("Parsing stmt on: {:?}", self.current_token);
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
                    } else { unimplemented!() }
                } else { unimplemented!() }
            },
            TokenType::Return => {
                self.advance();
                StatementEnum::Return { expr: self.parse_expr() }
            }
            TokenType::Fn => self.parse_fn_decl(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::Break => StatementEnum::Break,
            TokenType::Continue => StatementEnum::Continue,
            _ => {
                StatementEnum::Expression(self.parse_expr())
            }
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
                TokenType::EOF | TokenType::Comma | TokenType::RParen | TokenType::RCurly => return None,
                TokenType::Op(op) => return Some(op),
                _ => count += 1,
            }
        }
    }

    fn peek_token(&self) -> Option<Token> {
        return if self.current_pos == self.tokens.len() - 1 {
            None
        } else {
            Some(self.tokens[self.current_pos + 1].clone())
        }
    }

    fn get_token(&self) -> Token {
        return self.tokens[self.current_pos].clone()
    }

    fn advance(&mut self) {
        self.current_pos += 1;
        self.current_token = self.get_token()
    }

    fn parse_ident(&mut self) -> String {
        let ident = match self.get_token().tt {
            TokenType::Ident(ident) => ident,
            other => panic!("{:?}", other)
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

        let operator = match self.get_token().tt {
            TokenType::Op(op) => match op {
                Op::Eq => Op::Eq,
                other => {
                    self.advance(); // Consumes the eq
                    assert_eq!(self.current_token.tt, TokenType::Op(Op::Eq));
                    other
                }
            },
            _ => unimplemented!()
        };
        self.advance();

        let left = self.parse_expr();
        // x op= 10 -> x = x op 10
        let expr = match operator {
            Op::Eq => left,
            _ => {
                // Temporary solution
                self.add_start(left.span.start); // Push for right
                self.add_start(left.span.start); // Push for expr
                let right = self.construct_expr(ExpressionEnum::Var { ident: ident.clone() });
                let expr = ExpressionEnum::BinOp { left, operator, right };
                self.construct_expr(expr)
            }
        };
        StatementEnum::Expression(self.construct_expr(ExpressionEnum::Assignment { ident, expr }))
    }

    fn parse_block(&mut self) -> StatementEnum {
        assert_eq!(self.current_token.tt, TokenType::LCurly);
        self.advance(); // Consumes the lcurly
        StatementEnum::Block { stmts: self.parse_inner(true) }
    }

    fn parse_fn_decl(&mut self) -> StatementEnum {
        self.advance(); // Consumes the fn-kw
        let ident = self.parse_ident();

        assert_eq!(self.current_token.tt, TokenType::LParen);
        self.advance(); // Consumes the lparen

        let mut params: Vec<String> = Vec::new();
        while ![TokenType::RParen, TokenType::EOF].contains(&self.current_token.tt) {
            let ident = self.parse_ident();
            
            match &self.current_token.tt {
                TokenType::Comma => self.advance(),
                TokenType::RParen => {},
                _ => unimplemented!()
            }

            if params.contains(&ident) {
                unimplemented!()
            }

            params.push(ident)
        }
        self.advance(); // Consume the rparen
        
        self.add_start(self.current_token.span.start);
        let block = self.parse_block();
        let block = self.construct_stmt(block);
        //info!("{:?}", block);
        StatementEnum::FnDeclaration { ident, params, block }
    }

    fn parse_if(&mut self) -> StatementEnum {
        let if_branch = self.parse_cond_branch();

        let mut elif_branches: Vec<(Expression, Statement)> = Vec::new();
        while self.peek_token().unwrap().tt == TokenType::Elif {
            self.advance(); // Consumes the rcurly
            elif_branches.push(self.parse_cond_branch());
        }

        let mut else_branch: Option<Statement> = None;
        if self.peek_token().unwrap().tt == TokenType::Else {
            self.advance(); // Consumes the rcurly
            self.advance(); // Consumes the else-kw
            self.add_start(self.current_token.span.start);
            let block = self.parse_block();
            else_branch = Some(self.construct_stmt(block));
        }
        StatementEnum::If { if_branch, elif_branches, else_branch }
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
            if self.current_token.tt != TokenType::RParen {
                self.advance()
            }
        }

        self.construct_expr(ExpressionEnum::Call { ident, params })
    }

    fn parse_operand(&mut self) -> Expression {
        return match &self.get_token().tt {
            TokenType::Literal(literal) => {
                self.add_start(self.current_token.span.start);
                let result = self.construct_expr(ExpressionEnum::Literal(literal.to_owned()));
                result
            },
            TokenType::LParen => {
                self.advance(); // Consumes the lparen
                let expr_result = self.parse_expr();
                self.advance();
                return expr_result
            },
            TokenType::Op(op) => match op {
                Op::Sub | Op::Not => {
                    self.add_start(self.current_token.span.start);
                    self.advance(); // Consume the sub/not
                    let expr = self.parse_operand();
                    self.construct_expr(ExpressionEnum::UnaryOp { operator: op.clone(), expr })
                }
                _ => unimplemented!()
            }    

            TokenType::Ident(name) => {
                match self.peek_token() {
                    Some(Token { tt: TokenType::LParen, .. }) => return self.parse_call(),
                    _ => {
                        self.add_start(self.current_token.span.start);
                        let result = self.construct_expr(ExpressionEnum::Var { ident: name.to_owned() });
                        result
                    }
                }
            },
            other => panic!("{:?}", other)
        };
    }

    fn parse_expr(&mut self) -> Expression { 
        let mut left = self.parse_operand();
        if let Some(token) = self.peek_token() { 
            match token.tt {
                TokenType::Op(..) => self.advance(), // Consumes the operand
                _ => {}
            }
        }

        while let TokenType::Op(operator) = self.current_token.tt {
            self.add_start(left.span.start);
            self.advance(); // Consumes the operator
            let next_op = self.peek_next_op().unwrap_or(operator);

            let comparison = next_op.partial_cmp(&operator);
            left = match next_op.partial_cmp(&operator) {
                Some(Ordering::Greater) => self.parse_greater(operator, left),
                Some(Ordering::Equal) | Some(Ordering::Less) => self.parse_equal(operator, left),
                None => unimplemented!()
            };

            if let Some(token) = self.peek_token() {
                match token.tt {
                    TokenType::Literal(..) | TokenType::LParen | TokenType::Op(..) | TokenType::Ident(..) => {
                        if token.span.start.0 != self.current_token.span.end.0 { return left }
                    },
                    _ => return left
                }
            } 

            if comparison != Some(Greater) { 
                self.advance() 
            }
        }

        left // Returns on either eof or rparen
    }

    fn parse_greater(&mut self, operator: Op, left: Expression) -> Expression {
        let right = self.parse_expr();
        self.construct_expr(ExpressionEnum::BinOp { left: left.clone(), operator, right })
    }

    fn parse_equal(&mut self, operator: Op, left: Expression) -> Expression {
        let right = self.parse_operand();
        self.construct_expr(ExpressionEnum::BinOp { left: left.clone(), operator, right })
    }

    fn construct_expr(&mut self, expr: ExpressionEnum) -> Expression {
        Expression { expr: Box::new(expr), span: self.get_span() }
    }

    fn construct_stmt(&mut self, stmt: StatementEnum) -> Statement {
        Statement { stmt: Box::new(stmt), span: self.get_span() }
    }

    fn get_span(&mut self) -> Span {
        Span::new(self.get_start(), self.current_token.span.end)
    }

    fn add_start(&mut self, start: (usize, usize)) { 
        self.start_stack.push(start);
     }
    
    fn get_start(&mut self) -> (usize, usize) {
        match self.start_stack.pop() {
            Some(start) => start,
            None => unreachable!(),
        }
    }
}

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
