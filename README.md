# Default Variant
Procedural attribute macro to specify default values for enums. Given
the argument provided to the `default` attribute, a corresponding
`Default` implemenntation is derived.

Using `#[default(...)]`, an enum's default implementation no longer
requires the common boilerplate which often consists of a simple
(often trivial) expression.

# Examples

## Unit variants
The simplest use falls with unit (i.e., fieldless) variants, where the
name of the variant is passed as an argument to the `default` attribute.
```rust
use default_variant::default;

#[derive(PartialEq, Debug)]
#[default(Zero)]
//        ^^^^ expands to `Self::Zero`
pub enum Nat {
    Zero,
    Succ(Box<Self>)
}

assert_eq!(Nat::default(), Nat::Zero);
```

## Associated constants
Providing an identifier doesn't restrict the default value to be a
variant. If there's an associated constant defined (with the same type as
the enum, of course), then that too is fair game.

Suppose we have an `Expr` type (parametrized by some identifier type `Id`) modelling expressions in an AST, and we
want to use a representation for `()` as our default expression, where
we represent `()` as an empty `Expr::Tuple` variant.
```rust
use default_variant::default;

#[default(UNIT)]
//        ^^^^ expands to `Self::UNIT`
pub enum Expr<Id> {
    Var(Id),
    Tuple(Vec<Self>),
    /* other variants */
}

impl<Id> Expr<Id> {
    pub const UNIT: Self = Self::Tuple(vec![]);
    /* other fun but totally irrelevant stuff  */
}

// We'll (arbitrarily) parametrize `Expr` by `&'static str`
// here since the compiler can't infer `Id` strictly from
// this context due to its lack of appearance, as well as
// use the `matches` macro to pattern match for equality
// since we won't make assumptions about `Id` for this
// simple example
matches!(Expr::default(), Expr::Tuple(elems) if elems.is_empty())
```

## Tuple variants
Tuple variants may be used in the same way their expressions would be
formed without the enum name qualified.

Note however that the values passed in have the same scope as the body
of the `Default::default()` method in an `impl`-block.
```rust
use default_variant::default;

#[default(Coord(0, 0))]
//        ^^^^^^^^^^^ expands to `Self::Coord`
enum Position {
    Coord(usize, usize),
    /* some other variants */
}

assert_eq!(Position::default(), Position::Coord(0, 0));
```
## Associated methods
Tuple variants aren't the only "callable" expressions that `default`
accepts. Any associated method defined within an `impl`-block for the
given enum (that doesn't take a receiver, though arguments are fine)
is also a valid input.

```rust
use default_variant::default;

#[default(my_method())]
//        ^^^^^^^^^^^ expands to `Self::my_method()`
enum MyEnum {
    /* some variants */
}

impl MyEnum {
    fn my_method() -> Self {
        /* some logic */
    }
}
```

## Struct variants
To use struct variants as default values, simply pass in the relevant
struct variant (excluding the enum as a qualifier).
```rust
use default_variant::default;

#[derive(PartialEq, Debug)]
#[default(Foo { one: 1, two: 2 })]
//        ^^^ expands to `Self::Foo { one: 1, two: 2 }
pub enum FooBarBaz {
    Foo { one: u8, two: u8 },
    Bar(bool),
    Baz(char)
}

assert_eq!(FooBarBaz::default(), FooBarBaz::Foo { one: 1, two: 2 });
```
## `where`-clauses
We can also include `where`-clauses in the argument to `default`. The
predicates allow for generic uses of default methods without needing
to add the bounds in the enum's definition. The `where`-clauses passed
to `default` are then promptly added to the `where`-clause in the
derived `Default` implementation.
```rust
use default_variant::default;

#[default(
    Two(Default::default(), Default::default())
    where
        A: Default,
        B: Default
)]
#[derive(Debug, PartialEq)]
pub enum Pairs<A, B> {
    Two(A, B),
    /* other variants */
}

// we'll arbitrarily parametrize `Pairs` with primitives
// that have known defaults for demonstrative purposes
assert_eq!(Pairs::<bool, usize>::default(), Pairs::Two(false, 0));
```