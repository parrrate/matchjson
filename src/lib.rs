#![doc(
    html_favicon_url = "https://media.githubusercontent.com/media/parrrate/matchjson/refs/heads/main/assets/images/logo_fullsize.png"
)]
#![doc(
    html_logo_url = "https://media.githubusercontent.com/media/parrrate/matchjson/refs/heads/main/assets/images/logo_fullsize.png"
)]

use std::fmt::{Debug, Display};

#[doc(hidden)]
pub use phf;
use serde::Serialize;
#[doc(hidden)]
pub use serde_json;

#[doc(hidden)]
pub trait Exclusion: Debug + Copy {
    #[doc(hidden)]
    const EXCLUDE: &'static phf::Set<&'static str>;
}

#[doc(hidden)]
#[derive(Clone, Copy)]
#[ghost::phantom]
pub struct Excluded<E: Exclusion>;

impl<E: Exclusion> Debug for Excluded<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Excluded")
            .field("EXCLUDE", &E::EXCLUDE)
            .finish()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Exclude<'a, E: Exclusion> {
    #[doc(hidden)]
    pub object: &'a serde_json::Map<String, serde_json::Value>,
    #[doc(hidden)]
    pub _exclude: Excluded<E>,
}

impl<E: Exclusion> Serialize for Exclude<'_, E> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;
        let mut map = serializer.serialize_map(Some(self.len()))?;
        for (k, v) in self {
            map.serialize_entry(k, v)?;
        }
        map.end()
    }
}

impl<'a, E: Exclusion> IntoIterator for &Exclude<'a, E> {
    type Item = (&'a String, &'a serde_json::Value);
    type IntoIter = ExcludeIter<'a, E>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, E: Exclusion> Exclude<'a, E> {
    pub fn iter(&self) -> ExcludeIter<'a, E> {
        ExcludeIter {
            iter: self.object.iter(),
            _exclude: Excluded,
        }
    }

    pub fn contains_key(&self, key: &str) -> bool {
        !E::EXCLUDE.contains(key) && self.object.contains_key(key)
    }

    pub fn get(&self, key: &str) -> Option<&'a serde_json::Value> {
        (!E::EXCLUDE.contains(key))
            .then(|| self.object.get(key))
            .flatten()
    }

    pub fn get_key_value(&self, key: &str) -> Option<(&'a String, &'a serde_json::Value)> {
        (!E::EXCLUDE.contains(key))
            .then(|| self.object.get_key_value(key))
            .flatten()
    }

    pub fn len(&self) -> usize {
        self.object
            .len()
            .checked_sub(E::EXCLUDE.len())
            .expect("all excluded keys must be initially present")
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

pub struct ExcludeIter<'a, E: Exclusion> {
    iter: serde_json::map::Iter<'a>,
    _exclude: Excluded<E>,
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
        // taken from `serde_json::Value`
        struct WriterFormatter<'a, 'b: 'a> {
            inner: &'a mut std::fmt::Formatter<'b>,
        }
        impl<'a, 'b> std::io::Write for WriterFormatter<'a, 'b> {
            fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                let s = str::from_utf8(buf).expect("serde_json only outputs utf8");
                self.inner.write_str(s).map_err(io_error)?;
                Ok(buf.len())
            }
            fn flush(&mut self) -> std::io::Result<()> {
                Ok(())
            }
        }
        fn io_error(_: std::fmt::Error) -> std::io::Error {
            std::io::Error::other("fmt error")
        }
        let alternate = f.alternate();
        let mut wr = WriterFormatter { inner: f };
        if alternate {
            serde_json::ser::to_writer_pretty(&mut wr, self).map_err(|_| std::fmt::Error)
        } else {
            serde_json::ser::to_writer(&mut wr, self).map_err(|_| std::fmt::Error)
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! ifletarray {
    ($x:expr, [], $b:expr, $f:expr $(,)?) => {
        match $x {
            [] => $b,
            _ => $f,
        }
    };
    ($x:expr, [$p:tt $(,)?], $b:expr, $f:expr $(,)?) => {'bail: {
        match $x {
            [only] => $crate::ifletjson!(only, break 'bail $b, {}, $p),
            _ => {},
        }
        $f
    }};
    ($x:expr, [$($e:ident @ )? .. $(,)?] $(rev [])?, $b:expr, $f:expr $(,)?) => {{
        $($e = $x;)?
        $b
    }};
    ($x:expr, [$($e:ident @)? .. $(,)?] rev [$p1:tt $(,$p2:tt)* $(,)?], $b:expr, $f:expr $(,)?) => {'bail: {
        match $x {
            [rest @ .., last] => $crate::ifletjson!(
                last,
                $crate::ifletarray!(rest, [$($e @ )? ..] rev [$($p2)*], break 'bail $b, {}),
                {},
                $p1,
            ),
            _ => {},
        }
        $f
    }};
    ($x:expr, [$($e:ident @ )? .., $p1:tt $(, $p2:tt)* $(,)?] rev [$($p3:tt)*], $b:expr, $f:expr $(,)?) => {
        $crate::ifletarray!($x, [$($e @ )? .., $($p2)*] rev [$p1, $($p3)*], $b, $f)
    };
    ($x:expr, [$($e:ident @ )? .., $p1:tt $(, $p2:tt)* $(,)?], $b:expr, $f:expr $(,)?) => {
        $crate::ifletarray!($x, [$($e @ )? .., $($p2)*] rev [$p1], $b, $f)
    };
    ($x:expr, [$p1:tt, $($p2:tt)+], $b:expr, $f:expr $(,)?) => {'bail: {
        match $x {
            [first, rest @ ..] => $crate::ifletjson!(
                first,
                $crate::ifletarray!(rest, [$($p2)*], break 'bail $b, {}),
                {},
                $p1,
            ),
            _ => {},
        }
        $f
    }};
}

#[macro_export]
#[doc(hidden)]
macro_rules! ifletjson {
    ($x:expr, $b:expr, $f:expr, | $p1:tt | $($p2:tt)*) => {'bail: {
        $crate::ifletjson!($x, {}, $crate::ifletjson!($x, {}, break 'bail $f, $($p2)*), $p1);
        $b
    }};
    ($x:expr, $b:expr, $f:expr, & $p1:tt & $($p2:tt)*) => {'bail: {
        $crate::ifletjson!($x, $crate::ifletjson!($x, break 'bail $b, {}, $($p2)*), {}, $p1);
        $f
    }};
    ($x:expr, $b:expr, $f:expr, $p1:tt | $($p2:tt)*) => {
        $crate::ifletjson!($x, $b, $f, | $p1 | $($p2)*)
    };
    ($x:expr, $b:expr, $f:expr, $p1:tt & $($p2:tt)*) => {
        $crate::ifletjson!($x, $b, $f, & $p1 & $($p2)*)
    };
    ($x:expr, $b:expr, $f:expr, ($($p:tt)*) $(,)?) => {
        $crate::ifletjson!($x, $b, $f, $($p)*)
    };
    ($x:expr, $b:expr, $f:expr, _ $(,)?) => {$b};
    ($x:expr, $b:expr, $f:expr, $p:ident $(,)?) => {{
        $p = $x;
        $b
    }};
    ($x:expr, $b:expr, $f:expr, $p1:ident @ $($p2:tt)*) => {{
        $p1 = $x;
        $crate::ifletjson!($x, $b, $f, $($p2)*)
    }};
    ($x:expr, $b:expr, $f:expr, $p:literal $(,)?) => {{ if $x == $p { $b } else { $f } }};
    ($x:expr, $b:expr, $f:expr, null $(,)?) => {{ if $x.is_null() { $b } else { $f } }};
    ($x:expr, $b:expr, $f:expr, $p:ident: $(&)? str $(,)?) => {{
        if let Some(s) = $x.as_str() {
            $p = s;
            $b
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, _: $(&)? str $(,)?) => {{
        if $x.is_string() {
            $b
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, $p:ident: bool $(,)?) => {{
        if let Some(s) = $x.as_bool() {
            $p = s;
            $b
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, _: $(&)? bool $(,)?) => {{
        if $x.is_bool() {
            $b
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, $p:ident: i64 $(,)?) => {{
        if let Some(s) = $x.as_i64() {
            $p = s;
            $b
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, _: $(&)? i64 $(,)?) => {{
        if $x.is_i64() {
            $b
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, $p:ident: u64 $(,)?) => {{
        if let Some(s) = $x.as_u64() {
            $p = s;
            $b
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, _: $(&)? u64 $(,)?) => {{
        if $x.is_u64() {
            $b
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, $p:ident: f64 $(,)?) => {{
        if let Some(s) = $x.as_f64() {
            $p = s;
            $b
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, _: $(&)? f64 $(,)?) => {{
        if $x.is_f64() {
            $b
        } else {
            $f
        }
    }};
    ($x:expr, $b:expr, $f:expr, { $k:literal: $v:tt $(,)? } $(,)?) => {'bail: {
        if let Some(v) = $x.get($k) {
            $crate::ifletjson!(v, break 'bail $b, {}, $v)
        }
        $f
    }};
    ($x:expr, $b:expr, $f:expr, { $k:literal: $v:tt, $( $kp:literal: $vp:tt ),+ $(,)? } $(,)?) => {'bail: {
        $crate::ifletjson!($x, $crate::ifletjson!($x, break 'bail $b, {}, { $( $kp: $vp ),+ }), {}, {$k: $v});
        $f
    }};
    ($x:expr, $b:expr, $f:expr, { $( $kp:literal: $vp:tt, )+ ..$e:ident } $(,)?) => {'bail: {
        $crate::ifletjson!($x, {
            if let Some(object) = $x.as_object() {
                $e = {
                    #[derive(Clone, Copy)]
                    struct E;
                    mod phf {
                        pub use $crate::phf::*;
                    }
                    impl $crate::Exclusion for E {
                        const EXCLUDE: &'static $crate::phf::Set<&'static str> =
                            &$crate::phf::phf_set!{ $($kp),+ };
                    }
                    impl ::core::fmt::Debug for E {
                        fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                            $crate::Excluded::<E>.fmt(f)
                        }
                    }
                    $crate::Exclude {
                        object,
                        _exclude: $crate::Excluded::<E>,
                    }
                };
                break 'bail $b
            }
        }, {}, { $( $kp: $vp ),+ });
        $f
    }};
    ($x:expr, $b:expr, $f:expr, {..$e:ident} $(,)?) => {
        if let Some(s) = $x.as_object() {
            $e = s;
            $b
        } else {
            $f
        }
    };
    ($x:expr, $b:expr, $f:expr, {} $(,)?) => {
        if $x.is_object() {
            $b
        } else {
            $f
        }
    };
    ($x:expr, $b:expr, $f:expr, [$($p:tt)*] $(,)?) => {'bail: {
        if let Some(array) = $x.as_array() {
            $crate::ifletarray!(array.as_slice(), [$($p)*], break 'bail $b, {})
        }
        $f
    }}
}

#[macro_export]
#[doc(hidden)]
macro_rules! varsarray {
    ($c:expr, []) => {
        $c
    };
    ($c:expr, [$p:tt $(,)?]) => {
        $crate::varsjson!($c, $p)
    };
    ($c:expr, [$($e:ident @ )? .. $(,)?] $(rev [])?) => {
        $crate::varsjson!($c, $($e)?)
    };
    ($c:expr, [$($e:ident @)? .. $(,)?] rev [$p1:tt $(,$p2:tt)* $(,)?]) => {
        $crate::varsarray!($crate::varsjson!($c, $p1), [$($e @ )? ..] rev [$($p2)*])
    };
    ($c:expr, [$($e:ident @ )? .., $p1:tt $(, $p2:tt)* $(,)?] rev [$($p3:tt)*]) => {
        $crate::varsarray!($c, [$($e @ )? .., $($p2)*] rev [$p1, $($p3)*])
    };
    ($c:expr, [$($e:ident @ )? .., $p1:tt $(, $p2:tt)* $(,)?]) => {
        $crate::varsarray!($c, [$($e @ )? .., $($p2)*] rev [$p1])
    };
    ($c:expr, [$p1:tt, $($p2:tt)+]) => {
        $crate::varsarray!($crate::varsjson!($c, $p1), [$($p2)*])
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! varsjson {
    ($c:expr,) => {
        $c
    };
    ($c:expr, | $p1:tt | $($p2:tt)*) => {
        $crate::varsjson!($crate::varsjson!($c, $p1), $($p2)*)
    };
    ($c:expr, & $p1:tt & $($p2:tt)*) => {
        $crate::varsjson!($crate::varsjson!($c, $p1), $($p2)*)
    };
    ($c:expr, $p1:tt | $($p2:tt)*) => {
        $crate::varsjson!($c, | $p1 | $($p2)*)
    };
    ($c:expr, $p1:tt & $($p2:tt)*) => {
        $crate::varsjson!($c, & $p1 & $($p2)*)
    };
    ($c:expr, ($($p:tt)*) $(,)?) => {
        $crate::varsjson!($c, $($p)*)
    };
    ($c:expr, _ $(,)?) => {
        $c
    };
    ($c:expr, $p:ident $(,)?) => {{
        #[allow(unused)]
        let mut $p;
        #[allow(unused)]
        #[allow(unreachable_code)]
        #[allow(clippy::diverging_sub_expression)]
        if false {
            $p = ::core::unreachable!();
        }
        $c
    }};
    ($c:expr, $p1:ident @ $($p2:tt)*) => {
        $crate::varsjson!($crate::varsjson!($c, $p1), $($p2)*)
    };
    ($c:expr, $p:literal $(,)?) => {
        $c
    };
    ($c:expr, null $(,)?) => {
        $c
    };
    ($c:expr, $p:ident: $(&)? str $(,)?) => {
        $crate::varsjson!($c, $p)
    };
    ($c:expr, _: $(&)? str $(,)?) => {
        $c
    };
    ($c:expr, $p:ident: $(&)? bool $(,)?) => {
        $crate::varsjson!($c, $p)
    };
    ($c:expr, _: $(&)? bool $(,)?) => {
        $c
    };
    ($c:expr, $p:ident: $(&)? i64 $(,)?) => {
        $crate::varsjson!($c, $p)
    };
    ($c:expr, _: $(&)? i64 $(,)?) => {
        $c
    };
    ($c:expr, $p:ident: $(&)? u64 $(,)?) => {
        $crate::varsjson!($c, $p)
    };
    ($c:expr, _: $(&)? u64 $(,)?) => {
        $c
    };
    ($c:expr, $p:ident: $(&)? f64 $(,)?) => {
        $crate::varsjson!($c, $p)
    };
    ($c:expr, _: $(&)? f64 $(,)?) => {
        $c
    };
    ($c:expr, { $k:literal: $v:tt $(,)? } $(,)?) => {
        $crate::varsjson!($c, $v)
    };
    ($c:expr, { $k:literal: $v:tt, $( $kp:literal: $vp:tt ),+ $(,)? } $(,)?) => {
        $crate::varsjson!($crate::varsjson!($c, {$k: $v}), { $( $kp: $vp ),+ })
    };
    ($c:expr, { $( $kp:literal: $vp:tt, )+ ..$e:ident } $(,)?) => {
        $crate::varsjson!($crate::varsjson!($c, $e), { $( $kp: $vp ),+ })
    };
    ($c:expr, {..$e:ident} $(,)?) => {
        $crate::varsjson!($c, $e)
    };
    ($c:expr, {} $(,)?) => {
        $c
    };
    ($c:expr, [$($p:tt)*] $(,)?) => {
        $crate::varsarray!($c, [$($p)*])
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! freezearray {
    ($c:expr, []) => {
        $c
    };
    ($c:expr, [$p:tt $(,)?]) => {
        $crate::freezejson!($c, $p)
    };
    ($c:expr, [$($e:ident @ )? .. $(,)?] $(rev [])?) => {
        $crate::freezejson!($c, $($e)?)
    };
    ($c:expr, [$($e:ident @)? .. $(,)?] rev [$p1:tt $(,$p2:tt)* $(,)?]) => {
        $crate::freezearray!($crate::freezejson!($c, $p1), [$($e @ )? ..] rev [$($p2)*])
    };
    ($c:expr, [$($e:ident @ )? .., $p1:tt $(, $p2:tt)* $(,)?] rev [$($p3:tt)*]) => {
        $crate::freezearray!($c, [$($e @ )? .., $($p2)*] rev [$p1, $($p3)*])
    };
    ($c:expr, [$($e:ident @ )? .., $p1:tt $(, $p2:tt)* $(,)?]) => {
        $crate::freezearray!($c, [$($e @ )? .., $($p2)*] rev [$p1])
    };
    ($c:expr, [$p1:tt, $($p2:tt)+]) => {
        $crate::freezearray!($crate::freezejson!($c, $p1), [$($p2)*])
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! freezejson {
    ($c:expr,) => {
        $c
    };
    ($c:expr, | $p1:tt | $($p2:tt)*) => {
        $crate::freezejson!($crate::freezejson!($c, $p1), $($p2)*)
    };
    ($c:expr, & $p1:tt & $($p2:tt)*) => {
        $crate::freezejson!($crate::freezejson!($c, $p1), $($p2)*)
    };
    ($c:expr, $p1:tt | $($p2:tt)*) => {
        $crate::freezejson!($c, | $p1 | $($p2)*)
    };
    ($c:expr, $p1:tt & $($p2:tt)*) => {
        $crate::freezejson!($c, & $p1 & $($p2)*)
    };
    ($c:expr, ($($p:tt)*) $(,)?) => {
        $crate::freezejson!($c, $($p)*)
    };
    ($c:expr, _ $(,)?) => {
        $c
    };
    ($c:expr, $p:ident $(,)?) => {{
        let $p = $p;
        $c
    }};
    ($c:expr, $p1:ident @ $($p2:tt)*) => {
        $crate::freezejson!($crate::freezejson!($c, $p1), $($p2)*)
    };
    ($c:expr, $p:literal $(,)?) => {
        $c
    };
    ($c:expr, null $(,)?) => {
        $c
    };
    ($c:expr, $p:ident: $(&)? str $(,)?) => {
        $crate::freezejson!($c, $p)
    };
    ($c:expr, _: $(&)? str $(,)?) => {
        $c
    };
    ($c:expr, $p:ident: $(&)? bool $(,)?) => {
        $crate::freezejson!($c, $p)
    };
    ($c:expr, _: $(&)? bool $(,)?) => {
        $c
    };
    ($c:expr, $p:ident: $(&)? i64 $(,)?) => {
        $crate::freezejson!($c, $p)
    };
    ($c:expr, _: $(&)? i64 $(,)?) => {
        $c
    };
    ($c:expr, $p:ident: $(&)? u64 $(,)?) => {
        $crate::freezejson!($c, $p)
    };
    ($c:expr, _: $(&)? u64 $(,)?) => {
        $c
    };
    ($c:expr, $p:ident: $(&)? f64 $(,)?) => {
        $crate::freezejson!($c, $p)
    };
    ($c:expr, _: $(&)? f64 $(,)?) => {
        $c
    };
    ($c:expr, { $k:literal: $v:tt $(,)? } $(,)?) => {
        $crate::freezejson!($c, $v)
    };
    ($c:expr, { $k:literal: $v:tt, $( $kp:literal: $vp:tt ),+ $(,)? } $(,)?) => {
        $crate::freezejson!($crate::freezejson!($c, {$k: $v}), { $( $kp: $vp ),+ })
    };
    ($c:expr, { $( $kp:literal: $vp:tt, )+ ..$e:ident } $(,)?) => {
        $crate::freezejson!($crate::freezejson!($c, $e), { $( $kp: $vp ),+ })
    };
    ($c:expr, {..$e:ident} $(,)?) => {
        $crate::freezejson!($c, $e)
    };
    ($c:expr, {} $(,)?) => {
        $c
    };
    ($c:expr, [$($p:tt)*] $(,)?) => {
        $crate::freezearray!($c, [$($p)*])
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! matchjson_raw {
    ($x:expr, _ => $b:expr $(,)?) => {
        $b
    };
    ($x:expr, $p:ident => $b:expr $(,)?) => {{
        let $p = $x;
        $b
    }};
    ($x:expr, $p1:tt => $b:expr, $($p2:tt => $fp:expr),+ $(,)?) => {
        #[allow(unreachable_code)]
        'bail: {
            $crate::varsjson!(
                {
                    $crate::ifletjson!(
                        $x,
                        {},
                        break 'bail $crate::matchjson_raw!($x, $($p2 => $fp),+),
                        $p1,
                    );
                    $crate::freezejson!(
                        $b,
                        $p1,
                    )
                },
                $p1,
            )
        }
    };
}

#[macro_export]
macro_rules! matchjson {
    ($x:expr, $($t:tt)*) => {{
        let s: &$crate::serde_json::Value = &$x;
        $crate::matchjson_raw!(s, $($t)*)
    }};
}

#[macro_export]
macro_rules! jsonmatches {
    ($x:expr, $($p:tt)*) => {
        $crate::ifletjson!($x, true, false, $($p)*)
    };
}
