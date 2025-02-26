use runtime::{
    il::{ExecUnit, Instr, Opcode},
    object::{LArray, LFCapture, LFunction, LString, LTable, Object},
};

#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

macro_rules! noleak {
    ($body:block) => {
        $body
        let stats = dhat::HeapStats::get();
        dhat::assert_eq!(stats.curr_blocks, 0);
        dhat::assert_eq!(stats.curr_bytes, 0);
    };
}

fn mk_profiler() -> dhat::Profiler {
    dhat::Profiler::builder().testing().build()
}

#[test]
fn empty() {
    let _profiler = mk_profiler();
    noleak!({
        let array = LArray::new();
        assert!(array.is_empty());
    });
    noleak!({
        let table = LTable::new();
        assert!(table.is_empty());
    });
}

#[test]
fn self_cycle() {
    let _profiler = mk_profiler();
    noleak!({
        let mut array = LArray::new();
        array.push(Object::Int(10));
        array.push(Object::Array(array.clone()));
    });
    noleak!({
        let mut table = LTable::new();
        table.insert("str".into(), Object::Float(1.23));
        table.insert("self".into(), Object::Table(table.clone()));
    });
    noleak!({
        let exe = ExecUnit::new(Box::new([]), Box::new([]));
        let mut cap = LFCapture::new(Object::Nil);
        let func = LFunction::new(0, exe, Box::new([cap.clone()]));
        cap.set(Object::Function(func.clone()));
    });
}

#[test]
fn contains_heap_allocated_string() {
    let _profiler = mk_profiler();
    let long_str = "abcdefghijklmnopqrstuvwxyz0123456789";
    noleak!({
        let string = LString::from(long_str);
        assert!(string.is_heap_allocated());
        let mut array = LArray::new();
        array.push(Object::String(string.clone()));
        assert!(array.contains(&Object::String(string)));
    });
    noleak!({
        let string = LString::from(long_str);
        assert!(string.is_heap_allocated());
        let mut table = LTable::new();
        table.insert(string.clone(), Object::Nil);
        table.insert("key".into(), Object::String(string));
    });
    noleak!({
        let string = LString::from(long_str);
        assert!(string.is_heap_allocated());
        let exe = ExecUnit::new(Box::new([]), Box::new([]));
        let cap = LFCapture::new(Object::String(string));
        let func = LFunction::new(0, exe, Box::new([cap.clone()]));
        assert!(func.captures().contains(&cap));
    });
}

#[test]
fn same_type_k2() {
    let _profiler = mk_profiler();
    noleak!({
        let mut array1 = LArray::new();
        let mut array2 = LArray::new();
        array1.push(Object::Array(array2.clone()));
        array2.push(Object::Array(array1.clone()));
    });
    noleak!({
        let mut table1 = LTable::new();
        let mut table2 = LTable::new();
        table1.insert("table".into(), Object::Table(table2.clone()));
        table2.insert("table".into(), Object::Table(table1.clone()));
    });
    noleak!({
        let exe = ExecUnit::new(Box::new([]), Box::new([]));
        let mut cap1 = LFCapture::new(Object::Nil);
        let mut cap2 = LFCapture::new(Object::Nil);
        let func1 = LFunction::new(0, exe.clone(), Box::new([cap1.clone()]));
        let func2 = LFunction::new(0, exe.clone(), Box::new([cap2.clone()]));
        cap1.set(Object::Function(func2.clone()));
        cap2.set(Object::Function(func1.clone()));
    });
}

#[test]
fn same_type_k3() {
    let _profiler = mk_profiler();
    noleak!({
        let mut array1 = LArray::new();
        let mut array2 = LArray::new();
        let mut array3 = LArray::new();
        array1.push(Object::Array(array2.clone()));
        array1.push(Object::Array(array3.clone()));
        array2.push(Object::Array(array1.clone()));
        array2.push(Object::Array(array3.clone()));
        array3.push(Object::Array(array1.clone()));
        array3.push(Object::Array(array2.clone()));
    });
    noleak!({
        let mut table1 = LTable::new();
        let mut table2 = LTable::new();
        let mut table3 = LTable::new();
        table1.insert("table2".into(), Object::Table(table2.clone()));
        table1.insert("table3".into(), Object::Table(table3.clone()));
        table2.insert("table1".into(), Object::Table(table1.clone()));
        table2.insert("table3".into(), Object::Table(table3.clone()));
        table3.insert("table1".into(), Object::Table(table1.clone()));
        table3.insert("table2".into(), Object::Table(table2.clone()));
    });
    noleak!({
        let exe = ExecUnit::new(Box::new([]), Box::new([]));
        let mut cap1 = LFCapture::new(Object::Nil);
        let mut cap2 = LFCapture::new(Object::Nil);
        let mut cap3 = LFCapture::new(Object::Nil);
        let func1 = LFunction::new(0, exe.clone(), Box::new([cap1.clone()]));
        let func2 = LFunction::new(0, exe.clone(), Box::new([cap2.clone()]));
        let func3 = LFunction::new(0, exe.clone(), Box::new([cap3.clone()]));
        cap1.set(Object::Function(func2.clone()));
        cap2.set(Object::Function(func3.clone()));
        cap3.set(Object::Function(func1.clone()));
    });
}

#[test]
fn array_access_check() {
    let _profiler = mk_profiler();
    noleak!({
        let mut array = LArray::new();
        {
            let mut array2 = LArray::new();
            array2.push(Object::Int(3));
            array.push(Object::Array(array2.clone()));
            drop(array2);
        }
        {
            let mut table = LTable::new();
            table.insert("value".into(), Object::Int(5));
            array.push(Object::Table(table.clone()));
            drop(table);
        }
        {
            let exe = ExecUnit::new(Box::new(["a".into()]), Box::new([Instr::new(Opcode::Nop)]));
            let cap = LFCapture::new(Object::Int(7));
            let func = LFunction::new(0, exe, Box::new([cap.clone()]));
            array.push(Object::Function(func.clone()));
            drop(cap);
            drop(func);
        }
        match array.get(0).unwrap() {
            Object::Array(array2) => {
                assert_eq!(array2.get(0), Some(&Object::Int(3)));
            }
            _ => unreachable!(),
        }
        match array.get(1).unwrap() {
            Object::Table(table) => {
                assert_eq!(table.get("value").unwrap(), &Object::Int(5));
            }
            _ => unreachable!(),
        }
        match array.get(2).unwrap() {
            Object::Function(func) => {
                assert_eq!(func.captures().first().map(|cap| cap.get()), Some(&Object::Int(7)));
                assert_eq!(func.exec_unit().code().first(), Some(&Instr::new(Opcode::Nop)));
                assert_eq!(func.exec_unit().data().first(), Some(&"a".into()));
            }
            _ => unreachable!(),
        }
    });
}

#[test]
fn table_access_check() {
    let _profiler = mk_profiler();
    noleak!({
        let mut table = LTable::new();
        {
            let mut array = LArray::new();
            array.push(Object::Int(3));
            table.insert("array".into(), Object::Array(array.clone()));
            drop(array);
        }
        {
            let mut table2 = LTable::new();
            table2.insert("value".into(), Object::Int(5));
            table.insert("table".into(), Object::Table(table2.clone()));
            drop(table2);
        }
        {
            let exe = ExecUnit::new(Box::new(["a".into()]), Box::new([Instr::new(Opcode::Nop)]));
            let cap = LFCapture::new(Object::Int(7));
            let func = LFunction::new(0, exe, Box::new([cap.clone()]));
            table.insert("func".into(), Object::Function(func.clone()));
            drop(cap);
            drop(func);
        }
        match table.get("array").unwrap() {
            Object::Array(array) => {
                assert_eq!(array.get(0), Some(&Object::Int(3)));
            }
            _ => unreachable!(),
        }
        match table.get("table").unwrap() {
            Object::Table(table2) => {
                assert_eq!(table2.get("value").unwrap(), &Object::Int(5));
            }
            _ => unreachable!(),
        }
        match table.get("func").unwrap() {
            Object::Function(func) => {
                assert_eq!(func.captures().first().map(|cap| cap.get()), Some(&Object::Int(7)));
                assert_eq!(func.exec_unit().code().first(), Some(&Instr::new(Opcode::Nop)));
                assert_eq!(func.exec_unit().data().first(), Some(&"a".into()));
            }
            _ => unreachable!(),
        }
    });
}
