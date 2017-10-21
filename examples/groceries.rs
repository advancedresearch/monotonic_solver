extern crate monotonic_solver;

use monotonic_solver::search;

use std::collections::HashSet;

use Expr::*;
use Fruit::*;
use Taste::*;
use Person::*;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Person {
    Hannah,
    Peter,
    Clara,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Taste {
    Sweet,
    Sour,
    Bitter,
    NonSour,
}

impl Taste {
    fn likes(&self, fruit: Fruit) -> bool {
        *self == Sweet && fruit.is_sweet() ||
        *self == Sour && fruit.is_sour() ||
        *self == Bitter && fruit.is_bitter() ||
        *self == NonSour && !fruit.is_sour()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Fruit {
    Apple,
    Grape,
    Lemon,
    Orange,
}

impl Fruit {
    fn is_sweet(&self) -> bool {
        match *self {Orange | Apple => true, Grape | Lemon => false}
    }

    fn is_sour(&self) -> bool {
        match *self {Lemon | Orange => true, Apple | Grape => false}
    }

    fn is_bitter(&self) -> bool {
        match *self {Grape | Lemon => true, Apple | Orange => false}
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Expr {
    ForSale(Fruit),
    Preference(Person, Taste, Taste),
    Buy(Person, Fruit),
}

fn infer(cache: &HashSet<Expr>, filter_cache: &HashSet<Expr>, story: &[Expr]) -> Option<Expr> {
    let can_add = |new_expr: &Expr| {
        !cache.contains(new_expr) &&
        !filter_cache.contains(new_expr)
    };
    for expr in story {
        if let &Preference(x, taste1, taste2) = expr {
            for expr2 in story {
                if let &ForSale(y) = expr2 {
                    // Both tastes must be satisfied for the fruit.
                    if taste1.likes(y) && taste2.likes(y) {
                        let new_expr = Buy(x, y);
                        if can_add(&new_expr) {return Some(new_expr)};
                    }
                }
            }
        }
    }
    None
}

fn main() {
    let start = vec![
        ForSale(Orange),
        ForSale(Grape),
        ForSale(Apple),
        ForSale(Lemon),
        Preference(Hannah, Sour, Bitter),
        Preference(Peter, Sour, Sweet),
        Preference(Peter, NonSour, Bitter),
        Preference(Clara, NonSour, Sweet),
    ];
    let order_constraints = vec![
        // Peter likes grape better than orange.
        (Buy(Peter, Grape), Buy(Peter, Orange)),
    ];

    // Look up what this person will buy.
    let person = Peter;

    let res = search(
        &start,
        |expr| if let &Buy(x, y) = expr {if x == person {Some(y)} else {None}} else {None},
        1000, // max proof size.
        &[],
        &order_constraints,
        infer,
    );
    match res {
        Ok(ref res) | Err(ref res) => {
            for r in res {
                println!("{:?}", r);
            }
        }
    }
}
