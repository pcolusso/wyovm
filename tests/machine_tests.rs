use wyovm::Machine;


#[test]
fn test_hello_world() {
    let f = std::fs::File::open("samples/hello_world.obj").expect("hello world sample lost");
    let mut m = Machine::new();
    m.load_image(f);
}
