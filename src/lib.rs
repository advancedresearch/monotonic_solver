#![deny(missing_docs)]

//! # Monotonic-Solver
//! A monotonic solver designed to be easy to use with Rust enum expressions
//!
//! This can be used to:
//!
//! - Research modeling of common sense for artificial intelligence
//! - Test inference rules when studying logic and languages
//! - Generate story plots
//! - Search and extract data
//!
//! Used in [Avalog](https://github.com/advancedresearch/avalog),
//! an experimental implementation of Avatar Logic with a Prolog-like syntax.
//!
//! Blog posts:
//!
//! - [2017-07-25 New Library for Automated Monotonic Theorem Proving](https://github.com/advancedresearch/advancedresearch.github.io/blob/master/blog/2017-07-25-new-library-for-automated-monotonic-theorem-proving.md)
//!
//! The advantage of this library design is the ease-of-use for prototyping.
//! In a few hours, one can test a new idea for modeling common sense.
//!
//! Here is an example of program output (from "examples/drama.rs"):
//!
//! ```text
//! Bob murdered Alice with a gun
//! Bob shot Alice with a gun
//! Bob pulled the trigger of the gun
//! Bob aimed the gun at Alice
//! ```
//!
//! This is a program that generates drama story plots.
//! The solver starts with the ending and work backwards to the beginning.
//!
//! - Start: "Bob murdered Alice with a gun"
//! - Goal: "Bob aimed the gun at Alice".
//!
//! You can follow the reasoning step-by-step,
//! printed out as sentences in natural language or code.
//!
//!
//! When using this story plot for writing, you might do something like this:
//!
//! ```text
//! Bob picked up the gun and aimed it Alice.
//! "I hate you!" he cried.
//! "Wait, I can explain..." Alice raised her hands.
//! A loud bang.
//! Bob realized in the same moment what he did.
//! Something he never would believe if anyone had told him as a child.
//! He was now a murderer.
//! ```
//!
//! This particular program reasons backwards in time to take advantage of monotonic logic.
//! It helps to avoid explosive combinatorics of possible worlds.
//!
//! Technically, this solver can also be used when multiple contradicting facts lead
//! to the same goal.
//! The alternative histories, that do not lead to a goal, are erased when
//! the solver reduces the proof after finding a solution.
//!
//! ### Example
//!
//! Here is the full source code of a "examples/groceries.rs" that figures out which fruits
//! a person will buy from the available food and taste preferences.
//!
//! ```rust
//! extern crate monotonic_solver;
//!
//! use monotonic_solver::search;
//!
//! use std::collections::HashSet;
//!
//! use Expr::*;
//! use Fruit::*;
//! use Taste::*;
//! use Person::*;
//!
//! #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
//! pub enum Person {
//!     Hannah,
//!     Peter,
//!     Clara,
//! }
//!
//! #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
//! pub enum Taste {
//!     Sweet,
//!     Sour,
//!     Bitter,
//!     NonSour,
//! }
//!
//! impl Taste {
//!     fn likes(&self, fruit: Fruit) -> bool {
//!         *self == Sweet && fruit.is_sweet() ||
//!         *self == Sour && fruit.is_sour() ||
//!         *self == Bitter && fruit.is_bitter() ||
//!         *self == NonSour && !fruit.is_sour()
//!     }
//! }
//!
//! #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
//! pub enum Fruit {
//!     Apple,
//!     Grape,
//!     Lemon,
//!     Orange,
//! }
//!
//! impl Fruit {
//!     fn is_sweet(&self) -> bool {
//!         match *self {Orange | Apple => true, Grape | Lemon => false}
//!     }
//!
//!     fn is_sour(&self) -> bool {
//!         match *self {Lemon | Orange => true, Apple | Grape => false}
//!     }
//!
//!     fn is_bitter(&self) -> bool {
//!         match *self {Grape | Lemon => true, Apple | Orange => false}
//!     }
//! }
//!
//! #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
//! pub enum Expr {
//!     ForSale(Fruit),
//!     Preference(Person, Taste, Taste),
//!     Buy(Person, Fruit),
//! }
//!
//! fn infer(cache: &HashSet<Expr>, filter_cache: &HashSet<Expr>, story: &[Expr]) -> Option<Expr> {
//!     let can_add = |new_expr: &Expr| {
//!         !cache.contains(new_expr) &&
//!         !filter_cache.contains(new_expr)
//!     };
//!     for expr in story {
//!         if let &Preference(x, taste1, taste2) = expr {
//!             for expr2 in story {
//!                 if let &ForSale(y) = expr2 {
//!                     // Both tastes must be satisfied for the fruit.
//!                     if taste1.likes(y) && taste2.likes(y) {
//!                         let new_expr = Buy(x, y);
//!                         if can_add(&new_expr) {return Some(new_expr)};
//!                     }
//!                 }
//!             }
//!         }
//!     }
//!     None
//! }
//!
//! fn main() {
//!     let start = vec![
//!         ForSale(Orange),
//!         ForSale(Grape),
//!         ForSale(Apple),
//!         ForSale(Lemon),
//!         Preference(Hannah, Sour, Bitter),
//!         Preference(Peter, Sour, Sweet),
//!         Preference(Peter, NonSour, Bitter),
//!         Preference(Clara, NonSour, Sweet),
//!     ];
//!     let order_constraints = vec![
//!         // Peter likes grape better than orange.
//!         (Buy(Peter, Grape), Buy(Peter, Orange)),
//!     ];
//!
//!     // Look up what this person will buy.
//!     let person = Peter;
//!
//!     let (res, _) = search(
//!         &start,
//!         |expr| if let &Buy(x, y) = expr {if x == person {Some(y)} else {None}} else {None},
//!         Some(1000), // max proof size.
//!         &[],
//!         &order_constraints,
//!         infer,
//!     );
//!     println!("{:?} will buy:", person);
//!     for r in res {
//!         println!("- {:?}", r);
//!     }
//! }
//! ```
//!
//! When you run this program, it will output:
//!
//! ```text
//! Peter will buy:
//! - Grape
//! - Orange
//! ```
//!
//! This is what Peter will buy.
//!
//! Notice the following kinds of constraints:
//!
//! - People prefer some fruits above others
//! - A fruit can give multiple tasting experiences
//! - All tasting experiences must be satisfied for people to buy the fruit
//! - Not all kinds of fruits are available all the time
//! - People's preferences are combinations of tasting experiences
//! - People might change preferences over time
//!
//! When you start to code a new idea, you might only know vaguely
//! what the solver should do. Experiment!
//!
//! ### Design
//!
//! A monotonic solver is an automatic theorem prover that finds proofs using
//! forward-only search. The word "monotonic" means additional facts do not cancel
//! the truth value of previously added facts.
//!
//! This theorem prover is designed to work on AST (Abstract Syntax Tree)
//! described with Rust enums.
//! The API is low level to allow precise control over performance,
//! by taking advantage of `HashSet` cache for inferred facts and filtering.
//!
//! - `solve_and_reduce` is most commonly used, because it first finds a proof
//! and then removes all facts that are inferred but irrelevant.
//! - `solve` is used to show exhaustive search for facts, for e.g. debugging.
//!
//! The API is able to simplify the proof without knowing anything explicit
//! about the rules, because it reasons counter-factually afterwards by modifying the filter.
//! After finding a solution, it tests each fact one by one, by re-solving the problem, starting with the latest added facts and moving to the beginning, to solve the implicit dependencies.
//! All steps in the new solution must exist in the old solution.
//! Since this can happen many times, it is important to take advantage of the cache.
//!
//! Each fact can only be added once to the solution.
//! It is therefore not necessary a good algorithm to use on long chains of events.
//! A more applicable area is modeling of common sense for short activities.
//!
//! This is the recommended way of using this library:
//!
//! 1. Model common sense for a restricted domain of reasoning
//! 2. Wrap the solver and constraints in an understandable programming interface
//!
//! The purpose is use a handful of facts to infer a few additional facts.
//! In many applications, such additional facts can be critical,
//! because they might seem so obvious to the user that they are not even mentioned.
//!
//! It can also be used to speed up productivity when serial thinking is required.
//! Human brains are not that particularly good at this kind of reasoning, at least not compared to a computer.
//!
//!
//! The challenge is to encode the rules required to make the computer an efficient reasoner.
//! This is why this library focuses on ease-of-use in a way that is familiar to Rust programmers, so multiple designs can be tested and compared with short iteration cycles.
//!
//! ### Usage
//!
//! There are two modes supported by this library: Solving and searching.
//!
//! - In solving mode, you specify a goal and the solver tries to find a proof.
//! - In searching mode, you specify a pattern and extract some data.
//!
//! The solver requires 5 things:
//!
//! 1. A list of start facts.
//! 2. A list of goal facts.
//! 3. A list of filtered facts.
//! 4. A list of order-constraints.
//! 5. A function pointer to the inference algorithm.
//!
//! Start facts are the initial conditions that trigger the search through rules.
//!
//! Goal facts decides when the search terminates.
//!
//! Filtered facts are blocked from being added to the solution.
//! This can be used as feedback to the algorithm when a wrong assumption is made.
//!
//! Order-constraints are used when facts represents events.
//! It is a list of tuples of the form `(A, B)` which controls the ordering of events.
//! The event `B` is added to the internal filter temporarily until event `A`
//! has happened.
//!
//! The search requires 6 things (similar to solver except no goal is required):
//!
//! 1. A list of start facts.
//! 2. A matching pattern to extract data.
//! 3. A maximum size of proof to avoid running out of memory.
//! 4. A list of filtered facts.
//! 5. A list of order-constraints.
//! 6. A function pointer to the inference algorithm.
//!
//! It is common to set up the inference algorithm in this pattern:
//!
//! ```ignore
//! fn infer(cache: &HashSet<Expr>, filter_cache: &HashSet<Expr>, story: &[Expr]) -> Option<Expr> {
//!     let can_add = |new_expr: &Expr| {
//!         !cache.contains(new_expr) &&
//!         !filter_cache.contains(new_expr)
//!     };
//!
//!     let places = &[
//!         University, CoffeeBar
//!     ];
//!
//!     for expr in story {
//!         if let &HadChild {father, mother, ..} = expr {
//!             let new_expr = Married {man: father, woman: mother};
//!             if can_add(&new_expr) {return Some(new_expr);}
//!         }
//!
//!         if let &Married {man, woman} = expr {
//!             let new_expr = FellInLove {man, woman};
//!             if can_add(&new_expr) {return Some(new_expr);}
//!         }
//!
//!         ...
//!     }
//!     None
//! }
//! ```
//!
//! The `can_add` closure checks whether the fact is already inferred.
//! It is also common to create lists of items to iterate over,
//! and use it in combination with the cache to improve performance of lookups.

use std::hash::Hash;
use std::collections::HashSet;

/// Stores solver error.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Error {
    /// Failed to reach the goal.
    Failure,
    /// Reached maximum proof size limit.
    MaxSize,
}

/// Solves without reducing.
pub fn solve<T: Clone + PartialEq + Eq + Hash>(
    start: &[T],
    goal: &[T],
    max_size: Option<usize>,
    filter: &[T],
    order_constraints: &[(T, T)],
    infer: fn(cache: &HashSet<T>, filter_cache: &HashSet<T>, story: &[T]) -> Option<T>
) -> (Vec<T>, Result<(), Error>) {
    let mut cache = HashSet::new();
    for s in start {
        cache.insert(s.clone());
    }
    let mut filter_cache: HashSet<T> = HashSet::new();
    for f in filter {
        filter_cache.insert(f.clone());
    }
    let mut res: Vec<T> = start.into();
    loop {
        if goal.iter().all(|e| res.iter().any(|f| e == f)) {
            break;
        }
        if let Some(n) = max_size {
            if res.len() >= n {return (res, Err(Error::MaxSize))};
        }

        // Modify filter to prevent violation of order-constraints.
        let mut added_to_filter = vec![];
        for (i, &(ref a, ref b)) in order_constraints.iter().enumerate() {
            if !cache.contains(a) && !filter_cache.contains(b) {
                added_to_filter.push(i);
            }
        }
        for &i in &added_to_filter {
            filter_cache.insert(order_constraints[i].1.clone());
        }

        let expr = if let Some(expr) = infer(&cache, &filter_cache, &res) {
            expr
        } else {
            return (res, Err(Error::Failure));
        };
        res.push(expr.clone());
        cache.insert(expr);

        // Revert filter.
        for &i in &added_to_filter {
            filter_cache.remove(&order_constraints[i].1);
        }
    }
    (res, Ok(()))
}

/// Solves and reduces the proof to those steps that are necessary.
pub fn solve_and_reduce<T: Clone + PartialEq + Eq + Hash>(
    start: &[T],
    goal: &[T],
    mut max_size: Option<usize>,
    filter: &[T],
    order_constraints: &[(T, T)],
    infer: fn(cache: &HashSet<T>, filter_cache: &HashSet<T>, story: &[T]) -> Option<T>
) -> (Vec<T>, Result<(), Error>) {
    let (mut res, status) = solve(start, goal, max_size, filter, order_constraints, infer);
    if status.is_err() {return (res, status)};

    // Check that every step is necessary.
    max_size = Some(res.len());
    let mut new_filter: Vec<T> = filter.into();
    loop {
        let old_len = res.len();
        for i in (0..res.len()).rev() {
            if goal.iter().any(|e| e == &res[i]) {continue;}

            new_filter.push(res[i].clone());

            if let (solution, Ok(())) = solve(start, goal, max_size, &new_filter, order_constraints, infer) {
                if solution.len() < res.len() &&
                   solution.iter().all(|e| res.iter().any(|f| e == f))
                {
                    max_size = Some(solution.len());
                    res = solution;
                    break;
                }
            }

            new_filter.pop();
        }

        if res.len() == old_len {break;}
    }

    (res, Ok(()))
}

/// Searches for matches by a pattern.
///
/// - `pat` specifies the map and acceptance criteria
/// - `max_size` specifies the maximum size of proof
///
/// Returns `Ok` if all rules where exausted.
/// Returns `Err` if the maximum size of proof was exceeded.
pub fn search<T, F, U>(
    start: &[T],
    pat: F,
    max_size: Option<usize>,
    filter: &[T],
    order_constraints: &[(T, T)],
    infer: fn(cache: &HashSet<T>, filter_cache: &HashSet<T>, story: &[T]) -> Option<T>
) -> (Vec<U>, Result<(), Error>)
    where T: Clone + PartialEq + Eq + Hash,
          F: Fn(&T) -> Option<U>
{
    let mut cache = HashSet::new();
    for s in start {
        cache.insert(s.clone());
    }
    let mut filter_cache: HashSet<T> = HashSet::new();
    for f in filter {
        filter_cache.insert(f.clone());
    }
    let mut res: Vec<T> = start.into();
    let mut matches: Vec<U> = vec![];

    for expr in start {
        if let Some(val) = (pat)(expr) {
            matches.push(val);
        }
    }

    loop {
        if let Some(n) = max_size {
            if res.len() >= n {break};
        }

        // Modify filter to prevent violating of order-constraints.
        let mut added_to_filter = vec![];
        for (i, &(ref a, ref b)) in order_constraints.iter().enumerate() {
            if !cache.contains(a) && !filter_cache.contains(b) {
                added_to_filter.push(i);
            }
        }
        for &i in &added_to_filter {
            filter_cache.insert(order_constraints[i].1.clone());
        }

        let expr = if let Some(expr) = infer(&cache, &filter_cache, &res) {
            expr
        } else {
            return (matches, Ok(()));
        };
        res.push(expr.clone());

        if let Some(val) = (pat)(&expr) {
            matches.push(val);
        }

        cache.insert(expr);

        // Revert filter.
        for &i in &added_to_filter {
            filter_cache.remove(&order_constraints[i].1);
        }

    }
    (matches, Err(Error::MaxSize))
}
