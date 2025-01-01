use std::io::Write;
use std::rc::Rc;
use std::cell::RefCell;
use wyovm::Machine;

struct WritableBuffer(Rc<RefCell<Vec<u8>>>);

impl Write for WritableBuffer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.borrow_mut().flush()
    }
}

#[test]
fn test_hello_world() {
    let f = std::fs::File::open("samples/hello_world.obj").expect("hello world sample lost");
    // I feel all this ceremony is not necessary...
    let buf = Rc::new(RefCell::new(Vec::new()));
    {
        let mut m = Machine::new(std::io::stdin(), Box::new(WritableBuffer(buf.clone())));
        m.load_image(f);
        m.run();
    }
    let x = buf.borrow();
    let out = String::from_utf8_lossy(&x);
    assert_eq!(out, "Hello, World!");
}
