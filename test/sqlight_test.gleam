import sqlight
import gleeunit
import gleeunit/should
import gleam/dynamic
import gleam/option
import gleam/list

pub fn main() {
  gleeunit.main()
}

/// All valid SQLite error codes.
const codes = [
  4, 23, 5, 14, 19, 11, 101, 16, 1, 24, 13, 2, 9, 10, 6, 20, 21, 22, 7, 26, 12,
  27, 0, 3, 15, 25, 8, 100, 17, 18, 28, 516, 279, 261, 517, 773, 1038, 1294, 782,
  526, 270, 1550, 275, 531, 3091, 787, 1043, 1299, 2835, 1555, 2579, 1811, 2067,
  2323, 779, 523, 267, 257, 513, 769, 3338, 7178, 7434, 2826, 3594, 4106, 7690,
  6666, 8458, 8202, 2570, 5898, 4362, 1290, 1802, 1034, 6410, 3850, 6154, 3082,
  2314,
]

// Test helper that asserts the database closes after use
fn connect(
  f: fn(sqlight.Connection) -> Result(a, sqlight.Error),
) -> Result(a, sqlight.Error) {
  assert Ok(_) = sqlight.with_connection(":memory:", f)
}

pub fn errorcode_roundtrip_test() {
  use code <- list.each(codes)
  code
  |> sqlight.code_to_error
  |> sqlight.error_to_code
  |> should.equal(code)
}

pub fn open_test() {
  assert Ok(conn) = sqlight.open("file:open_test?mode=memory")
  assert Ok(Nil) = sqlight.close(conn)

  // Closing multiple times is OK.
  assert Ok(Nil) = sqlight.close(conn)
}

pub fn open_fail_test() {
  assert Error(sqlight.GenericError) =
    sqlight.open("file:open_fail_test?mode=wibble")
}

pub fn with_connection_test() {
  assert Ok(123) = {
    use _conn <-
      sqlight.with_connection("file:with_connection_test?mode=memory")
    Ok(123)
  }
}

pub fn status_test() {
  let status = sqlight.status()
  assert True = status.memory_used.used > -1
  assert True = status.memory_used.highwater > -1
  assert True = status.pagecache_used.used > -1
  assert True = status.pagecache_used.highwater > -1
  assert True = status.pagecache_overflow.used > -1
  assert True = status.pagecache_overflow.highwater > -1
  assert True = status.malloc_size.used > -1
  assert True = status.malloc_size.highwater > -1
  assert True = status.parser_stack.used > -1
  assert True = status.parser_stack.highwater > -1
  assert True = status.pagecache_size.used > -1
  assert True = status.pagecache_size.highwater > -1
  assert True = status.malloc_count.used > -1
  assert True = status.malloc_count.highwater > -1
}

pub fn query_1_test() {
  use conn <- connect()
  assert Ok([#(1, 2, 3), #(4, 5, 6)]) =
    sqlight.query(
      "select 1, 2, 3 union all select 4, 5, 6",
      conn,
      [],
      dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int),
    )
}

pub fn query_2_test() {
  use conn <- connect()
  assert Ok([1337]) =
    sqlight.query("select 1337", conn, [], dynamic.element(0, dynamic.int))
}

// bind_int/3	
// bind_float/3	
// bind_text/3	
// bind_blob/3	
// bind_null/2

pub fn bind_int_test() {
  use conn <- connect()
  assert Ok([12345]) =
    sqlight.query(
      "select ?",
      conn,
      [sqlight.int(12_345)],
      dynamic.element(0, dynamic.int),
    )
}

pub fn bind_float_test() {
  use conn <- connect()
  assert Ok([12345.6789]) =
    sqlight.query(
      "select ?",
      conn,
      [sqlight.float(12_345.6789)],
      dynamic.element(0, dynamic.float),
    )
}

pub fn bind_text_test() {
  use conn <- connect()
  assert Ok(["hello"]) =
    sqlight.query(
      "select ?",
      conn,
      [sqlight.text("hello")],
      dynamic.element(0, dynamic.string),
    )
}

pub fn bind_blob_test() {
  use conn <- connect()
  assert Ok([<<123, 0>>]) =
    sqlight.query(
      "select ?",
      conn,
      [sqlight.blob(<<123, 0>>)],
      dynamic.element(0, dynamic.bit_string),
    )
}

pub fn bind_null_test() {
  use conn <- connect()
  assert Ok([option.None]) =
    sqlight.query(
      "select ?",
      conn,
      [sqlight.null()],
      dynamic.element(0, dynamic.optional(dynamic.int)),
    )
}

pub fn bind_bool_test() {
  use conn <- connect()
  assert Ok([True]) =
    sqlight.query(
      "select ?",
      conn,
      [sqlight.bool(True)],
      dynamic.element(0, sqlight.decode_bool),
    )
}

pub fn exec_test() {
  use conn <- connect()
  assert Ok(Nil) = sqlight.exec("create table cats (name text)", conn)
  assert Ok(Nil) = sqlight.exec("insert into cats (name) values ('Tim')", conn)
  assert Ok(["Tim"]) =
    sqlight.query(
      "select name from cats",
      conn,
      [],
      dynamic.element(0, dynamic.string),
    )
}

pub fn exec_fail_test() {
  use conn <- connect()
  assert Error(sqlight.GenericError) =
    sqlight.exec("create table cats (name text", conn)
  Ok(Nil)
}

pub fn readme_example_test() {
  use conn <- sqlight.with_connection(":memory:")
  let cat_decoder = dynamic.tuple2(dynamic.string, dynamic.int)

  let sql =
    "
  create table cats (name text, age int);

  insert into cats (name, age) values 
  ('Nubi', 4),
  ('Biffy', 10),
  ('Ginny', 6);
  "
  assert Ok(Nil) = sqlight.exec(sql, conn)

  let sql =
    "
  select name, age from cats
  where age < ?
  "
  assert Ok([#("Nubi", 4), #("Ginny", 6)]) =
    sqlight.query(sql, on: conn, with: [sqlight.int(7)], expecting: cat_decoder)
}
