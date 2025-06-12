use matchjson::{jsonmatches, matchjson};
use serde_json::json;

fn main() {
    matchjson!(
        json!("123"),
        (x: str) => println!("{x}"),
        _ => {}
    );
    matchjson!(
        json!({"a": 1, "b": "2", "c": [1, 2, 3, 4, 5, 6, 7, 8, 9], "d": 4}),
        {"a": a, "b": (b: str), "c": (e @ [1, 2, c @ .., 8, 9]), ..d} => println!("{a} {b} {c:?} {d} {e}"),
        x => println!("{x}"),
    );
    matchjson!(
        json!(["1"]),
        (
            | {"value": (y @ _: i64)}
            | [(y @ _: str)]
        ) => println!("{y}"),
        _ => println!("err"),
    );
    matchjson!(
        json!([1]),
        (
            | {"value": (x: i64)}
            | [(x: i64)]
        ) => println!("{x}"),
        _ => println!("err"),
    );
    let v = json!([1]);
    let x = matchjson!(
        v,
        (
            | {"value": (y @ x: i64)}
            | [(y @ x: i64)]
        ) => &y,
        _ => &serde_json::Value::Null,
    );
    println!("{x}");
    assert!(jsonmatches!(json!({"a": "b"}), {"a": "b"}));
}
