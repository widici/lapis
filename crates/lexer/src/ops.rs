use std::ops::{Add, Sub, Mul, Div, Rem, Neg, Not};
use crate::token::Literal;

macro_rules! impl_arth_op {
    ($trait:ident, $fn:ident, $op:tt) => {
        impl $trait for Literal {
            type Output = Option<Self>;

            fn $fn(self, rhs: Self) -> Self::Output {
                Some(match (self, rhs) {
                    (Self::Int(lhs), Self::Int(rhs)) => {
                        Self::Int(lhs $op rhs)
                    },
                    (Self::Int(lhs), Self::Float(rhs)) => {
                        Self::Float(lhs as f64 $op rhs)
                    },
                    (Self::Float(lhs), Self::Int(rhs)) => {
                        Self::Float(lhs $op rhs as f64)
                    },
                    (Self::Float(lhs), Self::Float(rhs)) => {
                        Self::Float(lhs $op rhs)
                    },
                    _ => return None
                })
            }
        }
    }
}

impl_arth_op!(Add, add, +);
impl_arth_op!(Sub, sub, -);
impl_arth_op!(Mul, mul, *);
impl_arth_op!(Div, div, /);
impl_arth_op!(Rem, rem, %);

trait Pow<Rhs = Self> {
    type Output;
    fn pow(self, rhs: Rhs) -> Self::Output;
}

impl Pow for Literal {
    type Output = Option<Self>;

    fn pow(self, rhs: Self) -> Self::Output {
        Some(match (self, rhs) {
            (Self::Int(lhs), Self::Int(rhs)) => {
                Self::Int(lhs.pow(u32::try_from(rhs).unwrap()))
            },
            (Self::Float(lhs), Self::Int(rhs)) => {
                Self::Float(lhs.powf(rhs as f64))
            },
            (Self::Int(lhs), Self::Float(rhs)) => {
                Self::Float((lhs as f64).powf(rhs))
            },
            (Self::Float(lhs), Self::Float(rhs)) => {
                Self::Float(lhs.powf(rhs))
            },
            _ => return None
        })
    }
}

impl Neg for Literal {
    type Output = Option<Self>;

    fn neg(self) -> Self::Output {
        Some(match self {
            Self::Int(int) => {
                Self::Int(-int)
            },
            Self::Float(float) => {
                Self::Float(-float)
            },
            _ => return None
        })
    }
}

impl Not for Literal {
    type Output = Option<Self>;
    
    fn not(self) -> Self::Output {
        Some(match self {
            Self::Bool(bool) => {
                Self::Bool(!bool)
            },
            _ => return None
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{token::Literal, ops::Pow};

    #[test]
    fn test_impl_arth_op() {
        for case in [
            (Literal::Int(1) + Literal::Int(2), Some(Literal::Int(4))),
            (Literal::Float(0.99) - Literal::Int(3), Some(Literal::Float(0.33))),
            (Literal::Int(2) * Literal::Float(0.1), Some(Literal::Float(0.2))),
            (Literal::Float(0.1337) / Literal::Int(2), Some(Literal::Float(0.06685))),
            (Literal::Int(22) % Literal::Float(0.25), Some(Literal::Float(0.0))),
            (Literal::Bool(true) * Literal::Bool(false), None),
        ] {
            assert_eq!(case.0, case.1)
        }
    }

    #[test]
    fn test_neg_op() {
        assert_eq!(
            -Literal::Int(100),
            Some(Literal::Int(-100))
        );
        assert_eq!(
            -Literal::Float(44.44),
            Some(Literal::Float(-44.44))
        );
    }

    #[test]
    fn test_not_op() {
        assert_eq!(
            !Literal::Bool(true),
            Some(Literal::Bool(false))
        );
        assert_eq!(
            !Literal::Bool(false),
            Some(Literal::Bool(true))
        );
    }

    #[test]
    fn test_pow_op() {
        for case in [
            (Literal::Int(3).pow(Literal::Int(3)), Literal::Int(27)),
            (Literal::Float(2.5).pow(Literal::Float(2.0)), Literal::Float(6.25)),
        ] {
            assert_eq!(case.0, Some(case.1))
        }
    }
}