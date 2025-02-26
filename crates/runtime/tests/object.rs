use runtime::object::{LArray, LString, LTable, Object};

/// &str -> LString -> Object
fn s(str: &str) -> Object {
    LString::from(str).into()
}

/// i64 -> Object
fn i(i: i64) -> Object {
    i.into()
}

/// f64 -> Object
fn f(f: f64) -> Object {
    f.into()
}

/// bool -> Object
fn b(b: bool) -> Object {
    b.into()
}

#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

#[test]
fn array_construct_empty() {
    let array = LArray::new();
    assert_eq!(array.version(), 0);
    assert_eq!(array.len(), 0);
    assert!(array.is_empty());
    assert_eq!(array.get(0), None);
    assert_eq!(array, LArray::default());
}

#[test]
fn array_set_sync() {
    let mut array1 = LArray::from([i(1), s("tow")]);
    let v = array1.version();
    array1.set(0, s("one"));
    assert_ne!(array1.version(), v);
    assert_eq!(array1.get(0), Some(&s("one")));
    let mut array2 = array1.clone();
    let v = array1.version();
    array2.set(1, i(2));
    assert_ne!(array1.version(), v);
    assert_eq!(array1.get(1), Some(&i(2)));
}

#[test]
fn array_push_sync() {
    let mut array1 = LArray::new();
    let v = array1.version();
    array1.push(i(100));
    assert_ne!(array1.version(), v);
    assert_eq!(array1.get(0), Some(&i(100)));
    let mut array2 = array1.clone();
    let v = array1.version();
    array2.push(s("hello"));
    assert_ne!(array1.version(), v);
    assert_eq!(array1.get(1), Some(&s("hello")));
}

#[test]
fn array_pop_sync() {
    let mut array1 = LArray::from([b(true), f(1.23)]);
    let v = array1.version();
    assert_eq!(array1.pop(), Some(f(1.23)));
    assert_ne!(array1.version(), v);
    let mut array2 = array1.clone();
    let v = array1.version();
    assert_eq!(array2.pop(), Some(b(true)));
    assert_ne!(array1.version(), v);
    assert_eq!(array1.pop(), None);
}

#[test]
fn array_insert_sync() {
    let mut array1 = LArray::from([i(1), s("tow")]);
    let v = array1.version();
    array1.insert(1, s("one"));
    assert_ne!(array1.version(), v);
    assert_eq!(array1, LArray::from([i(1), s("one"), s("tow")]));
    let mut array2 = array1.clone();
    let v = array1.version();
    array2.insert(0, i(0));
    assert_ne!(array1.version(), v);
    assert_eq!(array1, LArray::from([i(0), i(1), s("one"), s("tow")]));
}

#[test]
fn array_remove_sync() {
    let mut array1 = LArray::from([i(1), s("tow")]);
    let v = array1.version();
    assert_eq!(array1.remove(0), i(1));
    assert_ne!(array1.version(), v);
    assert_eq!(array1, LArray::from([s("tow")]));
    let mut array2 = array1.clone();
    let v = array1.version();
    assert_eq!(array2.remove(0), s("tow"));
    assert_ne!(array1.version(), v);
    assert_eq!(array1, LArray::new());
}

#[test]
fn array_clear_sync() {
    let mut array1 = LArray::from([i(1), s("tow")]);
    assert!(!array1.is_empty());
    let array2 = array1.clone();
    let v = array1.version();
    array1.clear();
    assert_ne!(array1.version(), v);
    assert_eq!(array1, LArray::new());
    assert_eq!(array2, LArray::new());
    assert!(array1.is_empty());
}

#[test]
fn array_contains() {
    let array = LArray::from([i(1), s("tow")]);
    assert!(array.contains(&s("tow")));
    let v = array.version();
    assert!(!array.contains(&i(3)));
    assert_eq!(array.version(), v);
}

#[test]
fn table_construct_empty() {
    let table = LTable::new();
    assert_eq!(table.len(), 0);
    assert!(table.is_empty());
    assert_eq!(table.get("key"), None);
    assert_eq!(table, LTable::default());
}

#[test]
fn table_insert_sync() {
    let mut table1 = LTable::new();
    table1.insert("key".into(), i(100));
    assert_eq!(table1.get("key"), Some(&i(100)));
    let mut table2 = table1.clone();
    table2.insert("key".into(), s("hello"));
    assert_eq!(table1.get("key"), Some(&s("hello")));
    table1.insert("foo".into(), b(true));
    assert_eq!(table2, LTable::from([("key".into(), s("hello")), ("foo".into(), b(true))]));
}

#[test]
fn table_remove_sync() {
    let mut table1 = LTable::from([("key".into(), f(1.23)), ("foo".into(), b(true))]);
    assert_eq!(table1.remove("key"), Some(f(1.23)));
    assert_eq!(table1, LTable::from([("foo".into(), b(true))]));
    let mut table2 = table1.clone();
    assert_eq!(table2.remove("foo"), Some(b(true)));
    assert_eq!(table1, LTable::new());
}

#[test]
fn table_clear_sync() {
    let mut table1 = LTable::from([("key".into(), f(1.23)), ("foo".into(), b(true))]);
    assert!(!table1.is_empty());
    let table2 = table1.clone();
    table1.clear();
    assert_eq!(table1, LTable::new());
    assert_eq!(table2, LTable::new());
    assert!(table1.is_empty());
}

#[test]
fn table_contains_key() {
    let table = LTable::from([("foo".into(), b(true)), ("bar".into(), f(1.23))]);
    assert!(table.contains_key("foo"));
    assert!(!table.contains_key("baz"));
}

#[test]
fn table_equal() {
    let table1 = LTable::from([("1".into(), i(1)), ("tbl".into(), LTable::new().into())]);
    let table2 = LTable::from([("1".into(), i(1)), ("tbl".into(), LTable::new().into())]);
    assert_eq!(table1, table2);
}

#[test]
fn table_equal_for_nan() {
    let table1 = LTable::from([("key".into(), f64::NAN.into())]);
    let table2 = table1.clone();
    assert_ne!(table1, table2);
}
