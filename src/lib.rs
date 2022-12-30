use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    token, ExprStruct, Ident, ItemEnum, ItemImpl, WhereClause,
};

type CommaSep<T> = syn::punctuated::Punctuated<T, token::Comma>;
type Bounds<'a> = (syn::ImplGenerics<'a>, syn::TypeGenerics<'a>, Preds<'a>);

/// Corresponds to a value computed by calling an enum tuple variant
/// with a given number of arguments.
///
/// The "function" is the name of the variant, and the arguments are
/// expressions computed in the scope provided when deriving `Default`.
///
/// For example, the attribute
///
///         #[default(Foo(a, b, c))]
///               ^^^^^^^^^^^^
/// expands to
///
///         Self::Foo(a, b, c)
///
/// within the body of the `default` method in an impl block for the
/// enum following the given attribute.
///
/// Unlike regular call expressions, the "function" *must* be the name
/// of the enum variant returned, and cannot be a lambda, as in the
/// following (erroneous) example
///
///     #[default((|x| if GLOBAL_STATE { Foo } else { Bar })(a, b, c))]
///
/// where `GLOBAL_STATE`, `a`, `b`, and `c` are arbitrary expressions.
struct Call {
    name: Ident,
    paren: token::Paren,
    args: CommaSep<syn::Expr>,
}

impl Parse for Call {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = Ident::parse(input)?;
        let content;
        let paren = syn::parenthesized!(content in input);
        let args = content.parse_terminated(syn::Expr::parse)?;
        Ok(Self { name, paren, args })
    }
}

impl ToTokens for Call {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.name.to_tokens(tokens);
        self.paren
            .surround(tokens, |tokens| self.args.to_tokens(tokens))
    }
}

/// The argument of the `default` attribute. Corresponds to the value
/// returned by the derived `Default` implementation.
///
///         #[default(...)]
///                   ^^^
///
/// Consists of either a single identifier, an identifier followed by
/// a tuple (emulating a call-expression and most commonly
/// representing tuple variant values), or a struct expression
/// corresponding to a struct variant.
///
/// When the identifier is followed by parentheses, the restriction
/// that the identifier is a variant is lifted, allowing for
/// associated methods (that *don't* take a receiver, but *do* return
/// a value of type `Self`!) to be called.
///
/// Moreover, note that the `Self` type name is not included, as the
/// `VariantValue` is expanded after `Self::` in the body of the
/// implementation of `Default::default()`.
///
/// # Example
///
/// ## `VariantValue::Unit`
///
/// The attribute `#[default(Zero)]` in the following enum definition
///
///     #[default(Zero)]
///     enum Nat {
///         Zero,
///         Succ(Box<Self>)
///     }
///
/// expands to the following `Default` implementation
///
///     impl Default for Nat {
///         fn default() -> Self {
///             Self::Zero
///         }
///     }
///
/// ## `VariantValue::Tuple`
///
/// The attribute `#[default(Foo(true))]` in the following enum
/// definition
///
///     #[default(Foo(true))]
///     enum FooBar {
///         Foo(bool),
///         Bar
///     }
///
/// expands to the following `Default` implementation
///
///     impl Default for FooBar {
///         fn default() -> Self {
///             Self::Foo(true)
///         }
///     }
///
/// which uses the variant `Foo`, while the attribute
/// `#[default(cfg())]`, which calls `cfg`, an identifier that does
/// not exist as the name of any (tuple) variants (but is a defined
/// associated method) in the following enum definition of `OsTag`
///
///     #[default(cfg())]
///     enum OsTag {
///         Linux,
///         MacOs,
///         Windows,
///         Other
///     }
///
///     impl OsTag {
///         fn cfg() -> OsTag {
///             if cfg!(linux) {
///                 Self::Linux
///             } else if cfg!(macos) {
///                 Self::MacOs
///             } else if cfg!(windows) {
///                 Self::Windows
///             } else {
///                 Self::Other
///             }
///         }
///     }
///
/// expands to the following `Default` implementation
///
///     impl Default for OsTag {
///         fn default() -> Self {
///             Self::cfg()
///         }
///     }
///
/// ## `VariantValue::Struct`
///
/// The attribute `#[default(Quux { one: 1, two: 2 })]` in the
/// following enum definition
///
///     #[default(Baz { one: 1, two: 2 })]
///     enum MyEnum {
///         Foo,
///         Bar,
///         Baz { one: usize, two: usize }
///     }
///
/// expands to the following `Default` implementation
///
///     impl Default for MyEnum {
///         fn default() -> Self {
///             Self::Baz { one: 1, two: 2 }
///         }
///     }
///
enum VariantValue {
    /// A value from an enum variant with no fields, such as `None`
    /// from `enum Option<A> { None, Some(A) }`.
    Unit(Ident),
    /// A value from an enum tuple variant with unnamed, ordered
    /// fields, such as `Some(true)` from `enum Option<A> { None,
    /// Some(A) }` instantiated with `A = bool`.
    ///
    /// Alternatively, this value may represent a call to some
    /// associated method (with no receiver) on the defined enum type.
    Tuple(Call),
    Struct(ExprStruct),
}

impl Parse for VariantValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) {
            if input.peek2(token::Paren) {
                Call::parse(input).map(Self::Tuple)
            } else if input.peek2(token::Brace) {
                ExprStruct::parse(input).map(Self::Struct)
            } else {
                Ident::parse(input).map(Self::Unit)
            }
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for VariantValue {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            VariantValue::Unit(id) => id.to_tokens(tokens),
            VariantValue::Tuple(tup) => tup.to_tokens(tokens),
            VariantValue::Struct(st) => st.to_tokens(tokens),
        }
    }
}

/// Represents the arguments given to the `default` attribute, which
/// are expanded to follow `Self::` in the generated `Default`
/// implementation block.
///
///         // `Self::MyVariant`
///         #[default(MyVariant)]
///                   ^^^^^^^^^
///
///         // `Self::MyPair(a, b)` -- where `a` and `b` are in scope
///         #[default(MyPair(a, b))]
///                   ^^^^^^^^^^^^
///
///         // `Self::MyStruct { id: 0, ch: 'a' }`
///         #[default(MyStruct { id: 0, ch: 'a' })]
///                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
///
///         // `Self::my_method()`
///         #[default(my_method())]
///                   ^^^^^^^^^^^
///
///         // `Self::MyGenerics(T)`
///         #[default(MyGenerics(Default::default() where T: Default)]
///                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
///
/// # Example use
///
///     #[default(Foo)]
///     enum MyEnum {
///         Foo,
///         Bar,
///         Baz,
///     }
///
/// ## Including `where`-clauses
///
/// Arguments to `default` attributes may include `where`-clauses,
/// which allow for use of trait methods without requiring such bounds
/// in the definition of the enum and are effectively positioned in
/// the `where`-clause position of the expanded `Default`
/// implementation block.
///
///     #[default(
///         Coord {
///             x: Default::default(),
///             y: Default::default()
///         } where X: Default, Y: Default
///     )]
///     enum Point<X, Y> {
///         Coord { x: X, y: Y },
///         Top(X),
///         Bottom(X),
///         Left(Y),
///         Right(Y)
///     }
///
struct EnumDefault {
    value: VariantValue,
    preds: Option<WhereClause>,
}

impl Parse for EnumDefault {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let arg = VariantValue::parse(input)?;
        let preds = if input.peek(syn::Token![where]) {
            WhereClause::parse(input).map(Some)?
        } else {
            None
        };
        Ok(EnumDefault { value: arg, preds })
    }
}

impl EnumDefault {
    fn split_for_impl<'a>(&'a self, item: &'a ItemEnum) -> Bounds<'a> {
        let (pre, post, preds) = item.generics.split_for_impl();
        (pre, post, Preds(preds, self.preds.as_ref()))
    }

    fn item_impl(&self, item: &ItemEnum) -> ItemImpl {
        let name = &item.ident;
        let arg = &self.value;
        let (pre, post, preds) = self.split_for_impl(item);

        syn::parse_quote! {
            impl #pre Default for #name #post #preds {
                fn default() -> Self {
                    Self::#arg
                }
            }
        }
    }
}

struct Preds<'a>(Option<&'a WhereClause>, Option<&'a WhereClause>);

impl quote::ToTokens for Preds<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self(None, None) => (),
            Self(Some(prs), None) | Self(None, Some(prs)) => prs.to_tokens(tokens),
            Self(Some(prs1), Some(prs2)) => {
                prs1.where_token.to_tokens(tokens);
                prs1.predicates.to_tokens(tokens);
                prs2.predicates.to_tokens(tokens);
            }
        }
    }
}

/// Procedural attribute macro allowing for specification of a value
/// to use in a generated implementation for `std::default::Default`.
///
/// Placed above the definition of an enum, this attribute take as its
/// argument an expression beginning with an identifier. The
/// identifier should be the name of one of the enum's variants, and
/// must correspond to either a unit (fieldless) variant *OR* an
/// associated constant of the same type.
///
/// Alternatively, tuple and struct variants may be specified with
/// given values, as long as the identifiers exist as variant names.
///
/// The arguments to the `default` attribute are effectively prefixed
/// with `Self::` in the body of the generated implementation for
/// `Default::default()`.
///
/// Additionally, a `where`-clause may be provided within the
/// argument, and the included predicates will be appended to the
/// `where`-clause bounds in the generated implementation for
/// `Default`.
///
/// # Examples
///
/// ## Unit variants and associated constants
///
///     #[default(Zero)]
///     //        ^^^^ `Self::Zero`
///     enum Nat {
///         Zero,
///         Succ(Box<Self>)
///     }
///
/// The single identifier may also refer to an associated constant of
/// the same type. Below, we use `Self::MY_CONST` as the value used
/// when generating defaults (notice that it must be defined in an
/// `impl`-block as an associated constant!).
///
///     #[default(MY_CONST)]
///     //        ^^^^^^^^ `MyEnum::MY_CONST: MyEnum` must be defined
///     enum MyEnum {
///         /* variants */
///     }
///
///     impl MyEnum {
///         pub const MY_CONST: Self = /* constant expression */;
///     }
///
/// ## Tuple variants
///
///     #[default(Bar(true, 0))]
///     //        ^^^ `FooBar::Bar` is a tuple variant
///     enum FooBar {
///         Foo,
///         Bar(bool, u8)
///     }
///
/// ## Struct variants
///
///     #[default(Baz { foo: Some(0), bar: true })]
///     //        ^^^ `BarBaz::Baz` is a struct variant
///     enum BarBaz {
///         Bar(usize),
///         Baz { foo: Option<usize>, bar: bool }
///     }
///
/// ## `where`-clauses
///
///     #[default(One(Default::default()) where A: Default)]
///     enum NonEmpty<A> {
///         One(A),
///         Many(A, Box<Self>)
///     }
///
///     #[default(
///         Quux {
///             a: A::default(),
///             b: B::default(),
///             letter: 'c'
///         } where A: Default, B: Default
///     )]
///     enum Thing<A, B> {
///         One(A),
///         Two(A, B),
///         Quux { a: A, b: C, letter: char }
///     }
///
///
#[proc_macro_attribute]
pub fn default(macro_arg: TokenStream, input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as ItemEnum);
    let enum_default = syn::parse_macro_input!(macro_arg as EnumDefault);
    let output = enum_default.item_impl(&input);
    TokenStream::from(quote::quote!(#input #output))
}
