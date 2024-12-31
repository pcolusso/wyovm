use wyovm::Machine;


#[test]
fn test_hello_world() {
    let f = std::fs::File::open("samples/hello_world.obj").expect("hello world sample lost");
    let buf = Vec::new();
    {
        let mut m = Machine::new(Box::new(buf.clone()));
        m.load_image(f);
        m.run();
    }
    let out = String::from_utf8_lossy(&buf);
    assert_eq!(out, "Hello World!\n");
}
