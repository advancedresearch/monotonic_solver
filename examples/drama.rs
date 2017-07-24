#![feature(box_patterns)]

extern crate monotonic_solver;

use std::fmt;
use std::collections::HashSet;

use monotonic_solver::solve_and_reduce;

use Person::*;
use Expr::*;
use Place::*;
use Weapon::*;

/// People in the story.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Person {
    Alice,
    Bob,
    Cecil,
    Dan,
    Erica,
    Filip,
    Gaia,
    Hans,
    Ida,
    Joel,
    Kitty,
    Lamar,
    Monica,
    Nils,
}

impl Person {
    pub fn male(&self) -> bool {
        match *self {
            Alice | Cecil | Erica | Gaia | Ida | Kitty | Monica => false,
            Bob | Dan | Filip | Hans | Joel | Lamar | Nils => true
        }
    }
}

impl fmt::Display for Person {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}", self)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Place {
    University,
    CoffeeBar,
}

impl fmt::Display for Place {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", match *self {
            University => "university",
            CoffeeBar => "coffee bar",
        })
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Weapon {
    Knife,
    Gun,
}

impl Weapon {
    fn shoots(&self) -> bool {
        match *self {
            Knife => false,
            Gun => true,
        }
    }
}

impl fmt::Display for Weapon {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", match *self {
            Knife => "knife",
            Gun => "gun",
        })
    }
}

/// Event expressions in the story.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Expr {
    HadChild {father: Person, mother: Person, child: Person},
    Married {man: Person, woman: Person},
    FellInLove {man: Person, woman: Person},
    RomanticDinner {man: Person, woman: Person},
    MetEachOther {man: Person, woman: Person, at: Place},
    WasWorking {person: Person, place: Place},
    GotJob {person: Person, place: Place},
    AppliedForJob {person: Person, place: Place},
    DiedOfCancer(Person),
    HadToStayInBedBecauseOfIllness(Person),
    GotCancer(Person),
    GotSick(Person),
    FeltIll(Person),
    DiedInCarAccident(Person),
    DroveCarToWork(Person),
    OwnedCar(Person),
    PurchasedCar(Person),
    SawAffordableCar(Person),
    WasLookingForCar(Person),
    NeededCar(Person),
    Murdered {murderer: Person, victim: Person, weapon: Weapon},
    Shot {attacker: Person, target: Person, weapon: Weapon},
    PulledTrigger {person: Person, weapon: Weapon},
    Aimed {aimer: Person, target: Person, weapon: Weapon},
}

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            HadChild {father, mother, child} =>
                write!(fmt, "{} and {} gave birth to {}", father, mother, child)?,
            Married {man, woman} => write!(fmt, "{} married {}", man, woman)?,
            FellInLove {man, woman} => write!(fmt, "{} and {} fell in love", man, woman)?,
            RomanticDinner {man, woman} =>
                write!(fmt, "{} took {} out for a romantic dinner", man, woman)?,
            MetEachOther {man, woman, at} =>
                write!(fmt, "{} and {} met each other at the {}", man, woman, at)?,
            WasWorking {person, place} =>
                write!(fmt, "{} was working at the {}", person, place)?,
            GotJob {person, place} =>
                write!(fmt, "{} got job at the {}", person, place)?,
            AppliedForJob {person, place} =>
                write!(fmt, "{} applied for job at the {}", person, place)?,
            DiedOfCancer(person) => write!(fmt, "{} died of cancer", person)?,
            HadToStayInBedBecauseOfIllness(person) =>
                write!(fmt, "{} has to stay in bed because of illness", person)?,
            GotCancer(person) => write!(fmt, "{} got cancer", person)?,
            GotSick(person) => write!(fmt, "{} got sick", person)?,
            FeltIll(person) => write!(fmt, "{} felt ill", person)?,
            DiedInCarAccident(person) => write!(fmt, "{} died in car accident", person)?,
            DroveCarToWork(person) => write!(fmt, "{} drove the car to work", person)?,
            OwnedCar(person) => write!(fmt, "{} owned a car", person)?,
            PurchasedCar(person) => write!(fmt, "{} purchased a car", person)?,
            SawAffordableCar(person) => {
                write!(fmt, "{} saw a car {} could affort", person,
                       if person.male() {"he"} else {"she"})?
            }
            WasLookingForCar(person) => write!(fmt, "{} was looking for a car", person)?,
            NeededCar(person) => write!(fmt, "{} needed a car", person)?,
            Murdered {murderer, victim, weapon} =>
                write!(fmt, "{} murdered {} with a {}", murderer, victim, weapon)?,
            Shot {attacker, target, weapon} =>
                write!(fmt, "{} shot {} with a {}", attacker, target, weapon)?,
            Aimed {aimer, target, weapon} =>
                write!(fmt, "{} aimed the {} at {}", aimer, weapon, target)?,
            PulledTrigger {person, weapon} =>
                write!(fmt, "{} pulled the trigger of the {}", person, weapon)?,
        }
        Ok(())
    }
}

fn infer(cache: &HashSet<Expr>, filter_cache: &HashSet<Expr>, story: &[Expr]) -> Option<Expr> {
    let can_add = |new_expr: &Expr| {
        !cache.contains(new_expr) &&
        !filter_cache.contains(new_expr)
    };

    /*
    let people = &[
        Alice, Bob, Cecil, Dan, Erica, Filip,
        Gaia, Hans, Ida, Joel, Kitty, Lamar,
        Monica, Nils
    ];
    */
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

        if let &FellInLove {man, woman} = expr {
            let new_expr = RomanticDinner {man, woman};
            if can_add(&new_expr) {return Some(new_expr);}

            for place in places {
                if cache.contains(&WasWorking {person: man, place: *place}) ||
                   cache.contains(&WasWorking {person: woman, place: *place}) {
                    let new_expr = MetEachOther {man, woman, at: *place};
                    if can_add(&new_expr) {return Some(new_expr);}
                }
            }
        }

        if let &WasWorking {person, place} = expr {
            let new_expr = GotJob {person, place};
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &GotJob {person, place} = expr {
            let new_expr = AppliedForJob {person, place};
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &DiedOfCancer(person) = expr {
            let new_expr = GotCancer(person);
            if can_add(&new_expr) {return Some(new_expr);}

            let new_expr = HadToStayInBedBecauseOfIllness(person);
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &GotCancer(person) = expr {
            let new_expr = GotSick(person);
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &HadToStayInBedBecauseOfIllness(person) = expr {
            let new_expr = GotSick(person);
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &GotSick(person) = expr {
            let new_expr = FeltIll(person);
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &WasWorking {person, ..} = expr {
            if cache.contains(&DiedInCarAccident(person)) {
                let new_expr = DroveCarToWork(person);
                if can_add(&new_expr) {return Some(new_expr);}
            }
        }

        if let &DroveCarToWork(person) = expr {
            let new_expr = OwnedCar(person);
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &OwnedCar(person) = expr {
            let new_expr = PurchasedCar(person);
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &PurchasedCar(person) = expr {
            let new_expr = SawAffordableCar(person);
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &SawAffordableCar(person) = expr {
            let new_expr = WasLookingForCar(person);
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &WasLookingForCar(person) = expr {
            let new_expr = NeededCar(person);
            if can_add(&new_expr) {return Some(new_expr);}
        }

        if let &Murdered {murderer, victim, weapon} = expr {
            if weapon.shoots() {
                let new_expr = Shot {attacker: murderer, target: victim, weapon};
                if can_add(&new_expr) {return Some(new_expr);}
            }
        }

        if let &Shot {attacker, target, weapon} = expr {
            let new_expr = PulledTrigger {person: attacker, weapon};
            if can_add(&new_expr) {return Some(new_expr);}

            if cache.contains(&PulledTrigger {person: attacker, weapon}) {
                let new_expr = Aimed {aimer: attacker, target, weapon};
                if can_add(&new_expr) {return Some(new_expr);}
            }
        }
    }
    None
}

fn main() {
    // Events are reversed to take advantage of monotonic logic.
    let start = vec![
        // DiedInCarAccident(Bob),
        // HadChild {father: Bob, mother: Alice, child: Dan},
        // WasWorking {person: Bob, place: University},
        Murdered {murderer: Bob, victim: Alice, weapon: Gun},
    ];
    let goal = vec![
        // MetEachOther {man: Bob, woman: Alice, at: University},
        // AppliedForJob {person: Bob, place: University},
        // HadToStayInBedBecauseOfIllness(Bob),
        // WasLookingForCar(Bob),
        // Shot {attacker: Bob, target: Alice, weapon: Gun},
        Aimed {aimer: Bob, target: Alice, weapon: Gun},
    ];
    let filter = vec![];
    let after_constraints = vec![];
    match solve_and_reduce(&start, &goal, &filter, &after_constraints, infer) {
        Ok(solution) => {
            // for expr in solution.iter().rev() {
            for expr in solution {
                println!("{}", expr);
            }
        }
        Err(solution) => {
            // for expr in solution.iter().rev() {
            for expr in solution {
                println!("{}", expr);
            }
            println!("Could not solve");
        }
    }
}
