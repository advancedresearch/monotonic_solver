#![deny(missing_docs)]

//! A monotonic solver designed to be easy to use with Rust enums.
//! 
//! This can be used to:
//!
//! - Research modeling of common sense for artificial intelligence
//! - Test inference rules when studying logic and languages
//! - Generate story plots
//!
//! The advantage of this library design is the ease-of-use for prototyping.
//! In a few hours, one can test a new idea for modeling common sense.
//!
//! Here is an example of program output (from "examples/drama.rs"):
//!
//! ```
//! Bob murdered Alice with a gun
//! Bob shot Alice with a gun
//! Bob pulled the trigger of the gun
//! Bob aimed the gun at Alice
//! ```
//!
//! - Start: "Bob murdered Alice with a gun"
//! - Goal: "Bob aimed the gun at Alice".
//!
//! You can follow the reasoning step-by-step,
//! printed out as sentences in natural language or code.
//!
//! This particular program reasons backwards in time to take advantage of monotonic logic.
//! It helps to avoid explosive combinatorics of possible worlds.
//!
//! Technically, this solver can also be used when multiple contradicting facts lead
//! to the same goal.
//! The alternative histories, that do not lead to a goal, are erased when
//! the solver reduces the proof after finding a solution.
//!
//! ### Design
//!
//! A monotonic solver is an automatic theorem prover that finds proofs using
//! forward-only search. The word "monotonic" means additional facts do not cancel
//! the truth value of previously added facts.
//!
//! This theorem prover is designed to work AST (Abstract Syntax Tree)
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
//! The solver requires 5 things:
//!
//! 1. A list of start facts.
//! 2. A list of goal facts.
//! 3. A list of filtered facts.
//! 4. A list of after-constraints.
//! 5. A function pointer to the inference algorithm.
//!
//! Start facts are the initial conditions that trigger the search through rules.
//!
//! Goal facts decides when the search terminates.
//!
//! Filtered facts are blocked from being added to the solution.
//! This can be used as feedback to the algorithm when a wrong assumption is made.
//!
//! After-constraints is used when facts represents events.
//! It is a list of tuples of the form `(A, B)` which controls the ordering of events.
//! The event `B` is added to the internal filter temporarily until event `A`
//! has happened.
//!
//! It is common to set up the inference algorithm in this pattern:
//!
//! ```rust
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

/// Solves without reducing.
pub fn solve<T: Clone + PartialEq + Eq + Hash>(
    start: &[T],
    goal: &[T],
    filter: &[T],
    after_constraints: &[(T, T)],
    infer: fn(cache: &HashSet<T>, filter_cache: &HashSet<T>, story: &[T]) -> Option<T>
) -> Result<Vec<T>, Vec<T>> {
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

        // Modify filter to prevent violating of after-constraints.
        let mut added_to_filter = vec![];
        for (i, &(ref a, ref b)) in after_constraints.iter().enumerate() {
            if !cache.contains(a) && !filter_cache.contains(b) {
                added_to_filter.push(i);
            }
        }
        for &i in &added_to_filter {
            filter_cache.insert(after_constraints[i].1.clone());
        }

        let expr = if let Some(expr) = infer(&cache, &filter_cache, &res) {
            expr
        } else {
            return Err(res);
        };
        res.push(expr.clone());
        cache.insert(expr);

        // Revert filter.
        for &i in &added_to_filter {
            filter_cache.remove(&after_constraints[i].1);
        }
    }
    Ok(res)
}

/// Solves and reduces the proof to those steps that are necessary.
pub fn solve_and_reduce<T: Clone + PartialEq + Eq + Hash>(
    start: &[T],
    goal: &[T],
    filter: &[T],
    after_constraints: &[(T, T)],
    infer: fn(cache: &HashSet<T>, filter_cache: &HashSet<T>, story: &[T]) -> Option<T>
) -> Result<Vec<T>, Vec<T>> {
    let mut res = solve(start, goal, filter, after_constraints, infer)?;

    // Check that every step is necessary.
    let mut new_filter: Vec<T> = filter.into();
    loop {
        let old_len = res.len();
        for i in (0..res.len()).rev() {
            if goal.iter().any(|e| e == &res[i]) {continue;}

            new_filter.push(res[i].clone());

            if let Ok(solution) = solve(start, goal, &new_filter, after_constraints, infer) {
                if solution.len() < res.len() &&
                   solution.iter().all(|e| res.iter().any(|f| e == f))
                {
                    res = solution;
                    break;
                }
            }

            new_filter.pop();
        }

        if res.len() == old_len {break;}
    }

    Ok(res)
}
