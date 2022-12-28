pub external type Connection

pub type Error {
  No
}

// TODO: document
// TODO: test
pub external fn open(String) -> Result(Connection, Error) =
  "sqlight_ffi" "open"

pub external fn close(Connection) -> Result(Nil, Error) =
  "sqlight_ffi" "close"
