import gleam/list
import gleam/string
import gleam/result
import gleam/dynamic.{Decoder, Dynamic}

pub external type Connection

/// A value that can be sent to SQLite as one of the arguments to a
/// parameterised SQL query.
pub external type Value

pub type Stats {
  Stats(used: Int, highwater: Int)
}

pub type Error {
  SqlightError(
    code: ErrorCode,
    message: String,
    /// If the most recent error references a specific token in the input SQL,
    /// this is the byte offset of the start of that token. 
    /// If the most recent error does not reference a specific token, this is -1.
    offset: Int,
  )
}

/// The errors that SQLite can return.
///
/// See the SQLite documentation for further details.
/// <https://sqlite.org/rescode.html>
///
/// When running on JavaScript with Deno only a less-detailed subset of these
/// will be used as the underlying library does not expose extended error codes.
///
pub type ErrorCode {
  Abort
  Auth
  Busy
  Cantopen
  Constraint
  Corrupt
  Done
  Empty
  GenericError
  Format
  Full
  Internal
  Interrupt
  Ioerr
  Locked
  Mismatch
  Misuse
  Nolfs
  Nomem
  Notadb
  Notfound
  Notice
  GenericOk
  Perm
  Protocol
  Range
  Readonly
  Row
  Schema
  Toobig
  Warning
  AbortRollback
  AuthUser
  BusyRecovery
  BusySnapshot
  BusyTimeout
  CantopenConvpath
  CantopenDirtywal
  CantopenFullpath
  CantopenIsdir
  CantopenNotempdir
  CantopenSymlink
  ConstraintCheck
  ConstraintCommithook
  ConstraintDatatype
  ConstraintForeignkey
  ConstraintFunction
  ConstraintNotnull
  ConstraintPinned
  ConstraintPrimarykey
  ConstraintRowid
  ConstraintTrigger
  ConstraintUnique
  ConstraintVtab
  CorruptIndex
  CorruptSequence
  CorruptVtab
  ErrorMissingCollseq
  ErrorRetry
  ErrorSnapshot
  IoerrAccess
  IoerrAuth
  IoerrBeginAtomic
  IoerrBlocked
  IoerrCheckreservedlock
  IoerrClose
  IoerrCommitAtomic
  IoerrConvpath
  IoerrCorruptfs
  IoerrData
  IoerrDelete
  IoerrDeleteNoent
  IoerrDirClose
  IoerrDirFsync
  IoerrFstat
  IoerrFsync
  IoerrGettemppath
  IoerrLock
  IoerrMmap
  IoerrNomem
  IoerrRdlock
}

/// Convert an `Error` to an error code int.
///
/// See the SQLite documentation for the full list of error codes.
/// <https://sqlite.org/rescode.html>
///
pub fn error_code_to_int(error: ErrorCode) -> Int {
  case error {
    GenericError -> 1
    Abort -> 4
    Auth -> 23
    Busy -> 5
    Cantopen -> 14
    Constraint -> 19
    Corrupt -> 11
    Done -> 101
    Empty -> 16
    Format -> 24
    Full -> 13
    Internal -> 2
    Interrupt -> 9
    Ioerr -> 10
    Locked -> 6
    Mismatch -> 20
    Misuse -> 21
    Nolfs -> 22
    Nomem -> 7
    Notadb -> 26
    Notfound -> 12
    Notice -> 27
    GenericOk -> 0
    Perm -> 3
    Protocol -> 15
    Range -> 25
    Readonly -> 8
    Row -> 100
    Schema -> 17
    Toobig -> 18
    Warning -> 28
    AbortRollback -> 516
    AuthUser -> 279
    BusyRecovery -> 261
    BusySnapshot -> 517
    BusyTimeout -> 773
    CantopenConvpath -> 1038
    CantopenDirtywal -> 1294
    CantopenFullpath -> 782
    CantopenIsdir -> 526
    CantopenNotempdir -> 270
    CantopenSymlink -> 1550
    ConstraintCheck -> 275
    ConstraintCommithook -> 531
    ConstraintDatatype -> 3091
    ConstraintForeignkey -> 787
    ConstraintFunction -> 1043
    ConstraintNotnull -> 1299
    ConstraintPinned -> 2835
    ConstraintPrimarykey -> 1555
    ConstraintRowid -> 2579
    ConstraintTrigger -> 1811
    ConstraintUnique -> 2067
    ConstraintVtab -> 2323
    CorruptIndex -> 779
    CorruptSequence -> 523
    CorruptVtab -> 267
    ErrorMissingCollseq -> 257
    ErrorRetry -> 513
    ErrorSnapshot -> 769
    IoerrAccess -> 3338
    IoerrAuth -> 7178
    IoerrBeginAtomic -> 7434
    IoerrBlocked -> 2826
    IoerrCheckreservedlock -> 3594
    IoerrClose -> 4106
    IoerrCommitAtomic -> 7690
    IoerrConvpath -> 6666
    IoerrCorruptfs -> 8458
    IoerrData -> 8202
    IoerrDelete -> 2570
    IoerrDeleteNoent -> 5898
    IoerrDirClose -> 4362
    IoerrDirFsync -> 1290
    IoerrFstat -> 1802
    IoerrFsync -> 1034
    IoerrGettemppath -> 6410
    IoerrLock -> 3850
    IoerrMmap -> 6154
    IoerrNomem -> 3082
    IoerrRdlock -> 2314
  }
}

/// Convert an error code int to an `Error`.
///
/// If the code is not a known error code, `GenericError` is returned.
///
pub fn error_code_from_int(code: Int) -> ErrorCode {
  case code {
    4 -> Abort
    23 -> Auth
    5 -> Busy
    14 -> Cantopen
    19 -> Constraint
    11 -> Corrupt
    101 -> Done
    16 -> Empty
    1 -> GenericError
    24 -> Format
    13 -> Full
    2 -> Internal
    9 -> Interrupt
    10 -> Ioerr
    6 -> Locked
    20 -> Mismatch
    21 -> Misuse
    22 -> Nolfs
    7 -> Nomem
    26 -> Notadb
    12 -> Notfound
    27 -> Notice
    0 -> GenericOk
    3 -> Perm
    15 -> Protocol
    25 -> Range
    8 -> Readonly
    100 -> Row
    17 -> Schema
    18 -> Toobig
    28 -> Warning
    516 -> AbortRollback
    279 -> AuthUser
    261 -> BusyRecovery
    517 -> BusySnapshot
    773 -> BusyTimeout
    1038 -> CantopenConvpath
    1294 -> CantopenDirtywal
    782 -> CantopenFullpath
    526 -> CantopenIsdir
    270 -> CantopenNotempdir
    1550 -> CantopenSymlink
    275 -> ConstraintCheck
    531 -> ConstraintCommithook
    3091 -> ConstraintDatatype
    787 -> ConstraintForeignkey
    1043 -> ConstraintFunction
    1299 -> ConstraintNotnull
    2835 -> ConstraintPinned
    1555 -> ConstraintPrimarykey
    2579 -> ConstraintRowid
    1811 -> ConstraintTrigger
    2067 -> ConstraintUnique
    2323 -> ConstraintVtab
    779 -> CorruptIndex
    523 -> CorruptSequence
    267 -> CorruptVtab
    257 -> ErrorMissingCollseq
    513 -> ErrorRetry
    769 -> ErrorSnapshot
    3338 -> IoerrAccess
    7178 -> IoerrAuth
    7434 -> IoerrBeginAtomic
    2826 -> IoerrBlocked
    3594 -> IoerrCheckreservedlock
    4106 -> IoerrClose
    7690 -> IoerrCommitAtomic
    6666 -> IoerrConvpath
    8458 -> IoerrCorruptfs
    8202 -> IoerrData
    2570 -> IoerrDelete
    5898 -> IoerrDeleteNoent
    4362 -> IoerrDirClose
    1290 -> IoerrDirFsync
    1802 -> IoerrFstat
    1034 -> IoerrFsync
    6410 -> IoerrGettemppath
    3850 -> IoerrLock
    6154 -> IoerrMmap
    3082 -> IoerrNomem
    2314 -> IoerrRdlock
    _ -> GenericError
  }
}

if erlang {
  external fn open_(String) -> Result(Connection, Error) =
    "sqlight_ffi" "open"

  external fn close_(Connection) -> Result(Nil, Error) =
    "sqlight_ffi" "close"
}

if javascript {
  external fn open_(String) -> Result(Connection, Error) =
    "./sqlight_ffi.js" "open"

  external fn close_(Connection) -> Result(Nil, Error) =
    "./sqlight_ffi.js" "close"
}

/// Open a connection to a SQLite database.
///
/// URI filenames are supported by SQLite, making it possible to open read-only
/// databases, in memory databases, and more. Further information about this can
/// be found in the SQLite documentation: <https://sqlite.org/uri.html>.
///
/// # Examples
///
/// ## Open "data.db" in the current working directory
///
/// ```gleam
/// assert Ok(conn) = open("file:data.sqlite3")
/// ```
/// 
/// ## Opens "data.db" in read only mode with a private cache
/// 
/// ```gleam
/// assert Ok(conn) = open("file:data.db?mode=ro&cache=private")
/// ```
/// 
/// Opens a shared memory database named memdb1 with a shared cache. 
/// 
/// ```gleam
/// assert Ok(conn) = open("file:memdb1?mode=memory&cache=shared")
/// ```
///
pub fn open(path: String) -> Result(Connection, Error) {
  open_(path)
}

/// Close a connection to a SQLite database.
///
/// Ideally applications should finallise all prepared statements and other open
/// resources before closing a connection. See the SQLite documentation for more
/// information: <https://www.sqlite.org/c3ref/close.html>.
///
pub fn close(connection: Connection) -> Result(Nil, Error) {
  close_(connection)
}

/// Open a connection to a SQLite database and execute a function with it, then
/// close the connection.
///
/// This function works well with a `use` expression to automatically close the
/// connection at the end of a block.
///
/// # Crashes
///
/// This function crashes if the connection cannot be opened or closed.
///
/// # Examples
///
/// ```gleam
/// use conn = with_connection("file:mydb?mode=memory")
/// // Use the connection here...
/// ```
///
pub fn with_connection(path: String, f: fn(Connection) -> a) -> a {
  assert Ok(connection) = open(path)
  let value = f(connection)
  assert Ok(_) = close(connection)
  value
}

pub fn exec(sql: String, on connection: Connection) -> Result(Nil, Error) {
  exec_(sql, connection)
}

pub fn query(
  sql: String,
  on connection: Connection,
  with arguments: List(Value),
  expecting decoder: Decoder(t),
) -> Result(List(t), Error) {
  use rows <- result.then(run_query(sql, connection, arguments))
  use rows <- result.then(
    list.try_map(over: rows, with: decoder)
    |> result.map_error(decode_error),
  )
  Ok(rows)
}

if erlang {
  external fn run_query(
    String,
    Connection,
    List(Value),
  ) -> Result(List(Dynamic), Error) =
    "sqlight_ffi" "query"
}

if javascript {
  external fn run_query(
    String,
    Connection,
    List(Value),
  ) -> Result(List(Dynamic), Error) =
    "./sqlight_ffi.js" "query"
}

if erlang {
  external fn coerce_value(a) -> Value =
    "sqlight_ffi" "coerce_value"
}

if javascript {
  external fn coerce_value(a) -> Value =
    "./sqlight_ffi.js" "coerce_value"
}

if erlang {
  external fn exec_(String, Connection) -> Result(Nil, Error) =
    "sqlight_ffi" "exec"
}

if javascript {
  external fn exec_(String, Connection) -> Result(Nil, Error) =
    "./sqlight_ffi.js" "exec"
}

/// Convert a Gleam `Int` to an SQLite int, to be used an argument to a
/// query.
///
pub fn int(value: Int) -> Value {
  coerce_value(value)
}

/// Convert a Gleam `Float` to an SQLite float, to be used an argument to a
/// query.
///
pub fn float(value: Float) -> Value {
  coerce_value(value)
}

/// Convert a Gleam `String` to an SQLite text, to be used an argument to a
/// query.
///
pub fn text(value: String) -> Value {
  coerce_value(value)
}

/// Convert a Gleam `BitString` to an SQLite blob, to be used an argument to a
/// query.
///
pub fn blob(value: BitString) -> Value {
  coerce_blob(value)
}

if erlang {
  external fn coerce_blob(BitString) -> Value =
    "sqlight_ffi" "coerce_value"
}

if javascript {
  external fn coerce_blob(BitString) -> Value =
    "./sqlight_ffi.js" "coerce_blob"
}

/// Convert a Gleam `Bool` to an SQLite int, to be used an argument to a
/// query.
///
/// SQLite does not have a native boolean type. Instead, it uses ints, where 0
/// is False and 1 is True. Because of this the Gleam stdlib decoder for bools
/// will not work, instead the `decode_bool` function should be used as it
/// supports both ints and bools.
///
pub fn bool(value: Bool) -> Value {
  int(case value {
    True -> 1
    False -> 0
  })
}

if erlang {
  /// Construct an SQLite null, to be used an argument to a query.
  ///
  pub external fn null() -> Value =
    "sqlight_ffi" "null"
}

if javascript {
  /// Construct an SQLite null, to be used an argument to a query.
  ///
  pub external fn null() -> Value =
    "./sqlight_ffi.js" "null_"
}

pub fn decode_bool(value: Dynamic) -> Result(Bool, List(dynamic.DecodeError)) {
  case dynamic.int(value) {
    Ok(0) -> Ok(False)
    Ok(_) -> Ok(True)
    Error(e) -> Error(e)
  }
}

fn decode_error(errors: List(dynamic.DecodeError)) -> Error {
  assert [dynamic.DecodeError(expected, actual, path), ..] = errors
  let path = string.join(path, ".")
  let message =
    "Decoder failed, expected " <> expected <> ", got " <> actual <> " in " <> path
  SqlightError(code: GenericError, message: message, offset: -1)
}
