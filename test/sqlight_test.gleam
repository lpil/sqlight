import sqlight
import gleeunit

pub fn main() {
  gleeunit.main()
}

pub fn open_test() {
  assert Ok(conn) = sqlight.open("file:open_test?mode=memory")
  assert Ok(Nil) = sqlight.close(conn)
}
