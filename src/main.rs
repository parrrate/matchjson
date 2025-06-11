use std::{fmt::Display, marker::PhantomData};

use serde_json::json;

trait Exclusion {
    const EXCLUDE: &'static phf::Set<&'static str>;
}

struct Exclude<'a, E: Exclusion> {
    object: &'a serde_json::Map<String, serde_json::Value>,
    _exclude: PhantomData<E>,
}

impl<'a, E: Exclusion> Exclude<'a, E> {
    fn iter(&self) -> ExcludeIter<'a, E> {
        ExcludeIter {
            iter: self.object.iter(),
            _exclude: PhantomData,
        }
    }
}

struct ExcludeIter<'a, E: Exclusion> {
    iter: serde_json::map::Iter<'a>,
    _exclude: PhantomData<E>,
}

impl<'a, E: Exclusion> Iterator for ExcludeIter<'a, E> {
    type Item = (&'a String, &'a serde_json::Value);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (k, v) = self.iter.next()?;
            if !E::EXCLUDE.contains(k) {
                break Some((k, v));
            }
        }
    }
}

impl<E: Exclusion> Display for Exclude<'_, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sep = "";
        write!(f, "{{")?;
        for (k, v) in self.iter() {
            write!(f, "{sep}")?;
            write!(f, "{}:{v}", serde_json::Value::String(k.clone()))?;
            sep = ",";
        }
        write!(f, "}}")?;
        Ok(())
    }
}

macro_rules! ifletarray {
    ($x:expr, [], $b:expr, $f:expr $(,)?) => {
        match $x {
            [] => $b,
            _ => $f,
        }
    };
    ($x:expr, [$p:tt $(,)?], $b:expr, $f:expr $(,)?) => {
        match $x {
            [only] => ifletjson!(only, $b, $f, $p),
            _ => $f,
        }
    };
    ($x:expr, [$($e:ident @ )? .. $(,)?] $(rev [])?, $b:expr, $f:expr $(,)?) => {{
        $(let $e = $x;)?
        let _ = $x;
        $b
    }};
    ($x:expr, [$($e:ident @)? .. $(,)?] rev [$p1:tt $(,$p2:tt)* $(,)?], $b:expr, $f:expr $(,)?) => {
        match $x {
            [rest @ .., last] => ifletjson!(last, ifletarray!(rest, [$($e @ )? ..] rev [$($p2)*], $b, $f), $f, $p1),
            _ => $f,
        }
    };
    ($x:expr, [$($e:ident @ )? .., $p1:tt $(, $p2:tt)* $(,)?] rev [$($p3:tt)*], $b:expr, $f:expr $(,)?) => {
        ifletarray!($x, [$($e @ )? .., $($p2)*] rev [$p1, $($p3)*], $b, $f)
    };
    ($x:expr, [$($e:ident @ )? .., $p1:tt $(, $p2:tt)* $(,)?], $b:expr, $f:expr $(,)?) => {
        ifletarray!($x, [$($e @ )? .., $($p2)*] rev [$p1], $b, $f)
    };
    ($x:expr, [$p1:tt, $($p2:tt)+], $b:expr, $f:expr $(,)?) => {
        match $x {
            [first, rest @ ..] => ifletjson!(first, ifletarray!(rest, [$($p2)*], $b, $f), $f, $p1),
            _ => $f,
        }
    };
}

macro_rules! ifletjson {
    ($x:expr, $b:expr, $f:expr, | $p1:tt | $($p2:tt)*) => {
        ifletjson!($x, $b, ifletjson!($x, $b, $f, $($p2)*), $p1)
    };
    ($x:expr, $b:expr, $f:expr, & $p1:tt & $($p2:tt)*) => {
        ifletjson!($x, ifletjson!($x, $b, $f, $($p2)*), $f, $p1)
    };
    ($x:expr, $b:expr, $f:expr, $p1:tt | $($p2:tt)*) => {
        ifletjson!($x, $b, $f, | $p1 | $($p2)*)
    };
    ($x:expr, $b:expr, $f:expr, $p1:tt & $($p2:tt)*) => {
        ifletjson!($x, $b, $f, & $p1 & $($p2)*)
    };
    ($x:expr, $b:expr, $f:expr, ($($p:tt)*) $(,)?) => {
        ifletjson!($x, $b, $f, $($p)*)
    };
    ($x:expr, $b:expr, $f:expr, _ $(,)?) => {$b};
    ($x:expr, $b:expr, $f:expr, $p:ident $(,)?) => {{
        let $p = $x;
        $b
    }};
    ($x:expr, $b:expr, $f:expr, $p1:ident @ $($p2:tt)*) => {{
        let $p1 = $x;
        ifletjson!($x, $b, $f, $($p2)*)
    }};
    ($x:expr, $b:expr, $f:expr, $p:literal $(,)?) => {{ if $x == $p { $b } else { $f } }};
    ($x:expr, $b:expr, $f:expr, null $(,)?) => {{ if $x.is_null() { $b } else { $f } }};
    ($x:expr, $b:expr, $f:expr, $p:ident: $(&)? str $(,)?) => {{
        if let Some($p) = $x.as_str() { $b } else { $f }
    }};
    ($x:expr, $b:expr, $f:expr, $p:ident: bool $(,)?) => {{
        if let Some($p) = $x.as_bool() { $b } else { $f }
    }};
    ($x:expr, $b:expr, $f:expr, $p:ident: i64 $(,)?) => {{
        if let Some($p) = $x.as_i64() { $b } else { $f }
    }};
    ($x:expr, $b:expr, $f:expr, $p:ident: u64 $(,)?) => {{
        if let Some($p) = $x.as_u64() { $b } else { $f }
    }};
    ($x:expr, $b:expr, $f:expr, $p:ident: f64 $(,)?) => {{
        if let Some($p) = $x.as_f64() { $b } else { $f }
    }};
    ($x:expr, $b:expr, $f:expr, { $k:literal: $v:tt $(,)? } $(,)?) => {{
        if let Some(v) = $x.get($k) {
            ifletjson!(v, $b, $f, $v)
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, { $k:literal: $v:tt, $( $kp:literal: $vp:tt ),+ $(,)? } $(,)?) => {{
        ifletjson!($x, ifletjson!($x, $b, $f, { $( $kp: $vp ),+ }), $f, {$k: $v})
    }};
    ($x:expr, $b:expr, $f:expr, { $( $kp:literal: $vp:tt, )+ ..$e:ident } $(,)?) => {{
        ifletjson!($x, {
            if let Some(object) = $x.as_object() {
                #[derive(Clone, Copy)]
                struct E;
                impl $crate::Exclusion for E {
                    const EXCLUDE: &'static phf::Set<&'static str> = &phf::phf_set!{ $($kp),+ };
                }
                let $e = $crate::Exclude {
                    object,
                    _exclude: PhantomData::<E>,
                };
                $b
            } else {
                $f
            }
        }, $f, { $( $kp: $vp ),+ })
    }};
    ($x:expr, $b:expr, $f:expr, {..$e:ident}) => {
        if let Some($e) = $x.as_object() {
            $b
        } else {
            $f
        }
    };
    ($x:expr, $b:expr, $f:expr, {}) => {
        if $x.is_object() {
            $b
        } else {
            $f
        }
    };
    ($x:expr, $b:expr, $f:expr, [$($p:tt)*] $(,)?) => {{
        if let Some(array) = $x.as_array() {
            ifletarray!(array.as_slice(), [$($p)*], $b, $f)
        } else {
            $f
        }
    }}
}

macro_rules! matchjson {
    ($x:expr, _ => $b:expr $(,)?) => {
        $b
    };
    ($x:expr, $p:ident => $b:expr $(,)?) => {{
        let $p = $x;
        $b
    }};
    ($x:expr, $p1:tt => $b:expr, $($p2:tt => $fp:expr),+ $(,)?) => {
        ifletjson!($x, $b, matchjson!($x, $($p2 => $fp),+), $p1)
    };
}

fn main() {
    matchjson!(
        json!({"a": 1, "b": "2", "c": [1, 2, 3, 4, 5, 6, 7, 8, 9], "d": 4}),
        {"a": a, "b": (b: str), "c": (e @ [1, 2, c @ .., 8, 9]), ..d} => println!("{a} {b} {c:?} {d} {e}"),
        x => println!("{x}"),
    );
    matchjson!(
        json!(["1"]),
        (
            | {"value": (x: i64)}
            | [(x: str)]
        ) => println!("{x}"),
        _ => println!("err"),
    );
    matchjson!(
        json!({"value": 1}),
        (
            | {"value": (x: i64)}
            | [(x: str)]
        ) => println!("{x}"),
        _ => println!("err"),
    );
}
