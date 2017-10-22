# monotonic_solver
A monotonic solver designed to be easy to use with Rust enum expressions

This can be used to:

- Research modeling of common sense for artificial intelligence
- Test inference rules when studying logic and languages
- Generate story plots
- Search and extract data

The advantage of this library design is the ease-of-use for prototyping.
In a few hours, one can test a new idea for modeling common sense.

Here is an example of program output (from "examples/drama.rs"):

```ignore
Bob murdered Alice with a gun
Bob shot Alice with a gun
Bob pulled the trigger of the gun
Bob aimed the gun at Alice
```

This is a program that generates drama story plots.
The solver starts with the ending and work backwards to the beginning.

- Start: "Bob murdered Alice with a gun"
- Goal: "Bob aimed the gun at Alice".

You can follow the reasoning step-by-step,
printed out as sentences in natural language or code.

When using this story plot for writing, you might do something like this:

```ignore
Bob picked up the gun and aimed it Alice.
"I hate you!" he cried.
"Wait, I can explain..." Alice raised her hands.
A loud bang.
Bob realized in the same moment what he did.
Something he never would believe if anyone had told as a child.
He was now a murderer.
```

This particular program reasons backwards in time to take advantage of monotonic logic.
It helps to avoid explosive combinatorics of possible worlds.

Technically, this solver can also be used when multiple contradicting facts lead
to the same goal.
The alternative histories, that do not lead to a goal, are erased when
the solver reduces the proof after finding a solution.

### Example

Here is the full source code of a "examples/groceries.rs" that figures out which fruits
a person will buy from the available food and taste preferences.

```rust
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
```

When you run this program, it will output:

```
Peter will buy:
- Grape
- Orange
```

Notice the following kinds of constraints:

- People prefer some fruits above others
- A fruit can give multiple tasting experiences
- All tasting experiences must be satisfied for people to buy the fruit
- Not all kinds of fruits are available all the time
- People's preferences are combinations of tasting experiences
- People might change preferences over time

When you start to code a new idea, you might only know vaguely
what the solver should do. Experiment!

### Design

A monotonic solver is an automatic theorem prover that finds proofs using
forward-only search. The word "monotonic" means additional facts do not cancel
the truth value of previously added facts.

This theorem prover is designed to work on AST (Abstract Syntax Tree)
described with Rust enums.
The API is low level to allow precise control over performance,
by taking advantage of `HashSet` cache for inferred facts and filtering.

- `solve_and_reduce` is most commonly used, because it first finds a proof
and then removes all facts that are inferred but irrelevant.
- `solve` is used to show exhaustive search for facts, for e.g. debugging.

The API is able to simplify the proof without knowing anything explicit
about the rules, because it reasons counter-factually afterwards by modifying the filter.
After finding a solution, it tests each fact one by one, by re-solving the problem, starting with the latest added facts and moving to the beginning, to solve the implicit dependencies.
All steps in the new solution must exist in the old solution.
Since this can happen many times, it is important to take advantage of the cache.

Each fact can only be added once to the solution.
It is therefore not necessary a good algorithm to use on long chains of events.
A more applicable area is modeling of common sense for short activities.

This is the recommended way of using this library:

1. Model common sense for a restricted domain of reasoning
2. Wrap the solver and constraints in an understandable programming interface

The purpose is use a handful of facts to infer a few additional facts.
In many applications, such additional facts can be critical,
because they might seem so obvious to the user that they are not even mentioned.

It can also be used to speed up productivity when serial thinking is required.
Human brains are not that particularly good at this kind of reasoning, at least not compared to a computer.


The challenge is to encode the rules required to make the computer an efficient reasoner.
This is why this library focuses on ease-of-use in a way that is familiar to Rust programmers, so multiple designs can be tested and compared with short iteration cycles.

### Usage

There are two modes supported by this library: Solving and searching.

- In solving mode, you specify a goal and the solver tries to find a proof.
- In searching mode, you specify a pattern and extract some data.

The solver requires 5 things:

1. A list of start facts.
2. A list of goal facts.
3. A list of filtered facts.
4. A list of order-constraints.
5. A function pointer to the inference algorithm.

Start facts are the initial conditions that trigger the search through rules.

Goal facts decides when the search terminates.

Filtered facts are blocked from being added to the solution.
This can be used as feedback to the algorithm when a wrong assumption is made.

Order-constraints are used when facts represents events.
It is a list of tuples of the form `(A, B)` which controls the ordering of events.
The event `B` is added to the internal filter temporarily until event `A`
has happened.

The search requires 6 things (similar to solver except no goal is required):

1. A list of start facts.
2. A matching pattern to extract data.
3. A maximum size of proof to avoid running out of memory.
4. A list of filtered facts.
5. A list of order-constraints.
6. A function pointer to the inference algorithm.

It is common to set up the inference algorithm in this pattern:

```rust
fn infer(cache: &HashSet<Expr>, filter_cache: &HashSet<Expr>, story: &[Expr]) -> Option<Expr> {
    let can_add = |new_expr: &Expr| {
        !cache.contains(new_expr) &&
        !filter_cache.contains(new_expr)
    };

    let places = &[
        University, CoffeeBar
    ];

    for expr in story {
        if let &HadChild {father, mother, ..} = expr {
            let new_expr = Married {man: father, woman: mother};
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &Married {man, woman} = expr {
            let new_expr = FellInLove {man, woman};
            if can_add(&new_expr) {return Some(new_expr);}
        }

        ...
    }
    None
}
```

The `can_add` closure checks whether the fact is already inferred.
It is also common to create lists of items to iterate over,
and use it in combination with the cache to improve performance of lookups.
