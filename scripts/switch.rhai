let arr = [42, 123.456, "hello", true, "hey", 'x', 999, 1, 2, 3, 4];

for item in arr {
    switch item {
        42 => print("The Answer!"),
        123.456 => print(`Floating point... ${item}`),
        "hello" => print(`${item} world!`),
        999 => print(`Got 999: ${item}`),
        0..100 if item % 2 == 0 => print(`A small even number: ${item}`),
        0..100 => print(`A small odd number: ${item}`),
        _ => print(`Something else: <${item}> is ${type_of(item)}`)
    }
}
