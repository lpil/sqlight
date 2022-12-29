# sqlight

[![Package Version](https://img.shields.io/hexpm/v/sqlight)](https://hex.pm/packages/sqlight)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/sqlight/)

Use SQLite from Gleam!

This library is a Gleam wrapper around the excellent Erlang library
[esqlite](https://hex.pm/packages/esqlite), which in turn is a wrapper around
the SQLite C library. It is implemented as a NIF, which means that the sqlite
database engine is linked to the erlang virtual machine.

## Why Sqlite?

Sqlite is a implementation of SQL as a library. This means that you don't run a
separate SQL server that your program communicates with, but you embed the SQL
implementation directly in your program. Sqlite stores its data in a single
file. The file format is portable between different machine architectures. It
supports atomic transactions and it is possible to access the file by multiple
processes and different programs.

You can also use in-memory databases with Sqlite, which may be useful for testing.

## Usage

Add SQLight to your Gleam project:

```sh
gleam add sqlight
```

And start making queries:

```gleam
import sqlight

pub fn main() -> Result(Int, sqlight.Error) {
  use conn <- sqlight.with_connection("mydb.sqlite3")
  // TODO: Use the connection
}
```

Further documentation can be found at <https://hexdocs.pm/sqlight>.
