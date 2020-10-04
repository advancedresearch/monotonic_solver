/*

This example shows how to use combine a monotonic solver
with a mutable world.

- 3 people can wear 3 hats
- The same hat can not be worn by two people at the same time
- Two people can swap hats
- The hat worn by a person is tracked

Some expressions are used as instructions to manipulate the world.
If any instruction is invalid, no new inferences are made to stop the solver.
New inferences are made from the world after execution.

The solver can only reason about a single timeline.
This is because it requires N worlds to reason about N timelines.

The world is constructed from scratch and checked for each new inference.
Not an ideal design, but for simple worlds this is fast enough.
In a real world application, e.g. game programming, one would use a such
solver to test the game logic, which does not need optimization.

*/

extern crate monotonic_solver;

use monotonic_solver::{solve, solve_and_reduce};

use std::collections::HashSet;

use Expr::*;
use Person::*;

/// List of people.
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Person {
    Alice = 0,
    Bob = 1,
    Carl = 2,
}

impl Person {
    /// Converts from a number.
    pub fn from(val: u8) -> Option<Person> {
        match val {
            0 => Some(Alice),
            1 => Some(Bob),
            2 => Some(Carl),
            _ => None,
        }
    }
}

/// List of hats.
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Hat {
    Blue,
    Green,
    Yellow,
}

impl Hat {
    pub fn from(val: u8) -> Option<Hat> {
        match val {
            0 => Some(Hat::Blue),
            1 => Some(Hat::Green),
            2 => Some(Hat::Yellow),
            _ => None,
        }
    }
}

pub struct World {
    pub wears: [Option<Hat>; 3],
    pub worn: [[bool; 3]; 3],
}

impl World {
    pub fn new() -> World {
        World {
            wears: [None; 3],
            worn: [[false; 3]; 3],
        }
    }

    /// Returns whether a hat is weared by anyone.
    pub fn is_hat_free(&self, hat: Hat) -> bool {
        for i in 0..self.wears.len() {
            if self.wears[i] == Some(hat) {return false};
        }
        return true;
    }

    pub fn update_worn(&mut self) {
        for i in 0..self.wears.len() {
            if let Some(hat) = self.wears[i] {
                self.worn[i][hat as usize] = true;
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Expr {
    TakesOn(Person, Hat),
    TakesOff(Person, Hat),
    TakesOffHat(Person),
    SwapHats(Person, Person),
    TakeAllOff,
    /// The story works out.
    Sound,
    Wears(Person, Hat),
    WearsNoHat(Person),
    HaveWorn(Person, Hat),
}

fn infer(cache: &HashSet<Expr>, filter_cache: &HashSet<Expr>, story: &[Expr]) -> Option<Expr> {
    let can_add = |new_expr: &Expr| {
        !cache.contains(new_expr) &&
        !filter_cache.contains(new_expr)
    };

    let ref mut world = World::new();

    // Execute expressions on world.
    for expr in story {
        if let TakesOn(a, hat) = *expr {
            if !world.is_hat_free(hat) {return None};

            if world.wears[a as usize].is_some() {
                return None;
            } else {
                world.wears[a as usize] = Some(hat);
            }
        }

        if let TakesOff(a, hat) = *expr {
            if world.wears[a as usize].is_none() {
                return None;
            } else if world.wears[a as usize].unwrap() != hat {
                return None;
            } else {
                world.wears[a as usize] = None;
            }
        }

        if let TakesOffHat(a) = *expr {
            world.wears[a as usize] = None;
        }

        if let SwapHats(a, b) = *expr {
            world.wears.swap(a as usize, b as usize);
        }

        if let TakeAllOff = *expr {
            for i in 0..world.wears.len() {
                world.wears[i] = None;
            }
        }

        world.update_worn();
    }

    for i in 0..world.wears.len() {
        if let Some(person) = Person::from(i as u8) {
            if let Some(hat) = world.wears[i] {
                let new_expr = Wears(person, hat);
                if can_add(&new_expr) {return Some(new_expr)};
            } else {
                let new_expr = WearsNoHat(person);
                if can_add(&new_expr) {return Some(new_expr)};
            }
        }
    }

    for i in 0..world.worn.len() {
        for j in 0..world.worn[i].len() {
            if world.worn[i][j] {
                if let Some(person) = Person::from(i as u8) {
                    if let Some(hat) = Hat::from(j as u8) {
                        let new_expr = HaveWorn(person, hat);
                        if can_add(&new_expr) {return Some(new_expr)};
                    }
                }
            }
        }
    }

    Some(Sound)
}

pub fn trivial() -> (Vec<Expr>, Vec<Expr>) {
    (
        vec![
            TakesOn(Alice, Hat::Blue),
        ],
        vec![
            Sound,
        ]
    )
}

pub fn hats_colliding() -> (Vec<Expr>, Vec<Expr>) {
    (
        vec![
            TakesOn(Alice, Hat::Blue),
            TakesOn(Alice, Hat::Green),
        ],
        vec![
            Sound,
        ]
    )
}

pub fn take_on_off() -> (Vec<Expr>, Vec<Expr>) {
    (
        vec![
            TakesOn(Alice, Hat::Blue),
            TakesOff(Alice, Hat::Blue),
        ],
        vec![
            Sound,
        ]
    )
}

pub fn take_off_wrong() -> (Vec<Expr>, Vec<Expr>) {
    (
        vec![
            TakesOn(Alice, Hat::Blue),
            TakesOff(Alice, Hat::Green),
        ],
        vec![
            Sound,
        ]
    )
}

pub fn take_off_empty() -> (Vec<Expr>, Vec<Expr>) {
    (
        vec![
            TakesOff(Alice, Hat::Green),
        ],
        vec![
            Sound,
        ]
    )
}

pub fn bob_and_alice_wears_different_hats() -> (Vec<Expr>, Vec<Expr>) {
    (
        vec![
            TakesOn(Alice, Hat::Green),
            TakesOn(Bob, Hat::Blue),
        ],
        vec![
            Sound,
        ]
    )
}

pub fn alice_and_bob_tries_to_put_on_same_hat() -> (Vec<Expr>, Vec<Expr>) {
    (
        vec![
            TakesOn(Alice, Hat::Green),
            TakesOn(Bob, Hat::Green),
        ],
        vec![
            Sound,
        ]
    )
}

/// Checks a list of tests.
pub fn check(fs: &[(fn() -> (Vec<Expr>, Vec<Expr>), bool)]) {
    for (i, &(f, ok)) in fs.iter().enumerate() {
        let (start, goal) = f();
        let order_constraints = vec![];

        // Use `solve` because it's faster than reduction.
        let res = solve(
            &start,
            &goal,
            None,
            &[],
            &order_constraints,
            infer,
        );
        if res.is_ok() != ok {
            panic!("Failed check `{}`", i);
        }
    }
}

pub fn test() -> (Vec<Expr>, Vec<Expr>) {
    (
        vec![
            TakesOn(Alice, Hat::Green),
            TakesOn(Bob, Hat::Blue),
            TakesOn(Carl, Hat::Yellow),
            SwapHats(Alice, Carl),
            TakesOffHat(Alice),
        ],
        vec![
            WearsNoHat(Alice),
            Wears(Bob, Hat::Blue),
            Wears(Carl, Hat::Green),
            HaveWorn(Alice, Hat::Green),
            HaveWorn(Alice, Hat::Yellow),
            Sound,
        ]
    )
}

fn main() {
    check(&[
            (trivial, true),
            (hats_colliding, false),
            (take_on_off, true),
            (take_off_wrong, false),
            (take_off_empty, false),
            (bob_and_alice_wears_different_hats, true),
            (alice_and_bob_tries_to_put_on_same_hat, false),
        ]);

    let (start, goal) = test();
    let order_constraints = vec![
    ];

    let res = solve_and_reduce(
        &start,
        &goal,
        None,
        &[],
        &order_constraints,
        infer,
    );
    if res.is_ok() {
        println!("OK");
    } else {
        println!("ERROR");
    }
    match res {
        Ok(ref res) | Err(ref res) => {
            for r in res {
                println!("{:?}", r);
            }
        }
    }
}
