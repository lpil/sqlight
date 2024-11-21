import gleam/dynamic
import gleam/list
import gleam/option
import gleeunit
import gleeunit/should
import sqlight.{SqlightError}

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

fn connect(f: fn(sqlight.Connection) -> a) -> a {
  sqlight.with_connection(":memory:", f)
}

pub fn errorcode_roundtrip_test() {
  use code <- list.each(codes)
  code
  |> sqlight.error_code_from_int
  |> sqlight.error_code_to_int
  |> should.equal(code)
}

pub fn open_test() {
  let assert Ok(conn) = sqlight.open("")
  let assert Ok(Nil) = sqlight.close(conn)

  let assert // Closing multiple times is OK.
  Ok(Nil) = sqlight.close(conn)
}

@target(erlang)
pub fn open_fail_test() {
  let assert Error(SqlightError(sqlight.Cantopen, "", -1)) = sqlight.open("tmp")
}

pub fn with_connection_test() {
  let assert Ok(123) = {
    use _conn <- sqlight.with_connection("")
    Ok(123)
  }
}

fn prepare_and_query_multiple_times(
  sql: String,
  on connection: sqlight.Connection,
  with arguments: List(sqlight.Value),
  expecting decoder: dynamic.Decoder(t),
) -> Result(List(t), sqlight.Error) {
  case sqlight.prepare(sql, connection, decoder) {
    Ok(prepared) -> {
      let result = sqlight.query_prepared(prepared, arguments)

      sqlight.query(sql, connection, arguments, decoder)
      |> should.equal(result)

      sqlight.query_prepared(prepared, arguments)
      |> should.equal(result)

      result
    }
    Error(error) -> {
      let result = Error(error)
      sqlight.query(sql, connection, arguments, decoder)
      |> should.equal(result)

      result
    }
  }
}

pub fn query_1_test() {
  use conn <- connect()
  let assert Ok([#(1, 2, 3), #(4, 5, 6)]) =
    prepare_and_query_multiple_times(
      "select 1, 2, 3 union all select 4, 5, 6",
      conn,
      [],
      dynamic.tuple3(dynamic.int, dynamic.int, dynamic.int),
    )
}

pub fn query_2_test() {
  use conn <- connect()
  let assert Ok([1337]) =
    prepare_and_query_multiple_times(
      "select 1337",
      conn,
      [],
      dynamic.element(0, dynamic.int),
    )
}

pub fn bind_int_test() {
  use conn <- connect()
  let assert Ok([12_345]) =
    prepare_and_query_multiple_times(
      "select ?",
      conn,
      [sqlight.int(12_345)],
      dynamic.element(0, dynamic.int),
    )
}

pub fn bind_float_test() {
  use conn <- connect()
  let assert Ok([12_345.6789]) =
    prepare_and_query_multiple_times(
      "select ?",
      conn,
      [sqlight.float(12_345.6789)],
      dynamic.element(0, dynamic.float),
    )
}

pub fn bind_text_test() {
  use conn <- connect()
  let assert Ok(["hello"]) =
    prepare_and_query_multiple_times(
      "select ?",
      conn,
      [sqlight.text("hello")],
      dynamic.element(0, dynamic.string),
    )
}

pub fn bind_blob_test() {
  use conn <- connect()
  let assert Ok([#(<<123, 0>>, "blob")]) =
    prepare_and_query_multiple_times(
      "select ?1, typeof(?1)",
      conn,
      [sqlight.blob(<<123, 0>>)],
      dynamic.tuple2(dynamic.bit_array, dynamic.string),
    )
}

pub fn bind_null_test() {
  use conn <- connect()
  let assert Ok([option.None]) =
    prepare_and_query_multiple_times(
      "select ?",
      conn,
      [sqlight.null()],
      dynamic.element(0, dynamic.optional(dynamic.int)),
    )
}

pub fn bind_bool_test() {
  use conn <- connect()
  let assert Ok([True]) =
    prepare_and_query_multiple_times(
      "select ?",
      conn,
      [sqlight.bool(True)],
      dynamic.element(0, sqlight.decode_bool),
    )
}

pub fn exec_test() {
  use conn <- connect()
  let assert Ok(Nil) = sqlight.exec("create table cats (name text)", conn)
  let assert Ok(Nil) =
    sqlight.exec("insert into cats (name) values ('Tim')", conn)
  let assert Ok(["Tim"]) =
    prepare_and_query_multiple_times(
      "select name from cats",
      conn,
      [],
      dynamic.element(0, dynamic.string),
    )
}

pub fn exec_fail_test() {
  use conn <- connect()
  let assert Error(SqlightError(sqlight.GenericError, "incomplete input", -1)) =
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
  let assert Ok(Nil) = sqlight.exec(sql, conn)

  let sql =
    "
  select name, age from cats
  where age < ?
  "
  let assert Ok([#("Nubi", 4), #("Ginny", 6)]) =
    prepare_and_query_multiple_times(
      sql,
      on: conn,
      with: [sqlight.int(7)],
      expecting: cat_decoder,
    )
}

pub fn error_syntax_error_test() {
  use conn <- sqlight.with_connection(":memory:")
  let assert Error(SqlightError(sqlight.GenericError, "incomplete input", -1)) =
    sqlight.exec("create table cats (name text", conn)
}

pub fn error_info_foreign_key_test() {
  use conn <- sqlight.with_connection(":memory:")
  let assert Ok(_) =
    sqlight.exec(
      "
  pragma foreign_keys = on;

  create table users (
    id integer primary key autoincrement
  ) strict;

  create table posts (
    id integer primary key autoincrement,
    user_id integer not null,

    foreign key (user_id) references users(id)
  ) strict;
  ",
      conn,
    )
  let assert Error(SqlightError(code, "FOREIGN KEY constraint failed", -1)) =
    sqlight.exec("insert into posts (user_id) values (1)", conn)

  let assert // On Deno we do not have extended error codes so the information here is less precise.
  True = code == sqlight.ConstraintForeignkey || code == sqlight.Constraint
}

pub fn error_info_null_constraint_test() {
  use conn <- sqlight.with_connection(":memory:")
  let assert Ok(_) =
    sqlight.exec(
      "
  pragma foreign_keys = on;

  create table users (
    id integer primary key autoincrement,
    name text not null
  ) strict;
  ",
      conn,
    )
  let assert Error(SqlightError(
    code,
    "NOT NULL constraint failed: users.name",
    -1,
  )) = sqlight.exec("insert into users default values;", conn)

  let assert // On Deno we do not have extended error codes so the information here is less precise.
  True = sqlight.ConstraintNotnull == code || sqlight.Constraint == code
}

pub fn decode_error_test() {
  use conn <- sqlight.with_connection(":memory:")
  let assert Error(SqlightError(
    sqlight.GenericError,
    "Decoder failed, expected String, got Int in 0",
    -1,
  )) =
    prepare_and_query_multiple_times(
      "select 1",
      conn,
      [],
      dynamic.element(0, dynamic.string),
    )
}

pub fn query_error_test() {
  use conn <- sqlight.with_connection(":memory:")

  let assert Error(SqlightError(sqlight.GenericError, _, _)) =
    prepare_and_query_multiple_times(
      "this isn't a valid query",
      conn,
      [],
      dynamic.element(0, dynamic.int),
    )
}

pub fn bind_nullable_test() {
  use conn <- connect()

  let assert Ok([option.Some(12_345)]) =
    prepare_and_query_multiple_times(
      "select ?",
      conn,
      [sqlight.nullable(sqlight.int, option.Some(12_345))],
      dynamic.element(0, dynamic.optional(dynamic.int)),
    )

  let assert Ok([option.None]) =
    prepare_and_query_multiple_times(
      "select ?",
      conn,
      [sqlight.nullable(sqlight.int, option.None)],
      dynamic.element(0, dynamic.optional(dynamic.int)),
    )
}
