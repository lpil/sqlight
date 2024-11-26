# sqlight

[![Package Version](https://img.shields.io/hexpm/v/sqlight)](https://hex.pm/packages/sqlight)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/sqlight/)

Use [SQLite](https://www.sqlite.org/index.html) from Gleam!

Works on Erlang or JavaScript running on Deno.

```sh
gleam add sqlight
```

```gleam
import gleam/dynamic
import sqlight

pub fn main() {
  use conn <- sqlight.with_connection(":memory:")
  let cat_decoder = dynamic.tuple2(dynamic.string, dynamic.int)

  let sql = "
  create table cats (name text, age int);

  insert into cats (name, age) values 
  ('Nubi', 4),
  ('Biffy', 10),
  ('Ginny', 6);
  "
  let assert Ok(Nil) = sqlight.exec(sql, conn)

  let sql = "
  select name, age from cats
  where age < ?
  "
  let assert Ok([#("Nubi", 4), #("Ginny", 6)]) =
    sqlight.query(sql, on: conn, with: [sqlight.int(7)], expecting: cat_decoder)

  let assert Ok(prepared) =
    sqlight.prepare(sql, on: conn, expecting: cat_decoder)
  let assert Ok([#("Nubi", 4), #("Ginny", 6)]) =
    sqlight.query_prepared(prepared, with: [sqlight.int(7)])
}
```

Documentation can be found at <https://hexdocs.pm/sqlight>.

## Why SQLite?

SQLite is a implementation of SQL as a library. This means that you don't run a
separate SQL server that your program communicates with, but you embed the SQL
implementation directly in your program. SQLite stores its data in a single
file. The file format is portable between different machine architectures. It
supports atomic transactions and it is possible to access the file by multiple
processes and different programs.

You can also use in-memory databases with SQLite, which may be useful for testing.

## Implementation

When running on Erlang is library wrapper around the excellent Erlang library
[esqlite](https://hex.pm/packages/esqlite), which in turn is a wrapper around
the SQLite C library. It is implemented as a NIF, which means that the SQLite
database engine is linked to the erlang virtual machine.

When running on Deno it is a wrapper around the excellent
[x/sqlite](https://deno.land/x/sqlite@v3.7.0) library, which in turn is a
wrapper around the SQLite C library compiled to WASM.

## On using Bool with SQLite

SQLite does not have a native boolean type. Instead, it uses ints, where 0 is
False and 1 is True. Because of this the Gleam stdlib decoder for bools will not
work, instead the `sqlight.decode_bool` function should be used as it supports
both ints and bools.
