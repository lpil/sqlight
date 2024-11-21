import { List, Ok, Error as GlError } from "./gleam.mjs";
import { SqlightError, error_code_from_int } from "./sqlight.mjs";
import { DB } from "https://deno.land/x/sqlite@v3.7.0/mod.ts";

function wrapDB(db) {
  return {
    db: db,
    statements: [],
  };
}

export function open(path) {
  return new Ok(wrapDB(new DB(path)));
}

export function close(connection) {
  for(let statement of connection.statements) {
    statement.finalize();
  }
  connection.statements = [];
  connection.db.close();
  return new Ok(undefined);
}

export function coerce_value(value) {
  return value;
}

export function coerce_blob(value) {
  console.log(value);
  return value.buffer;
}

export function status(connection) {
  throw new Error("status");
}

export function exec(sql, connection) {
  try {
    connection.db.execute(sql);
    return new Ok(undefined);
  } catch (error) {
    return convert_error(error);
  }
}

export function query(sql, connection, parameters) {
  let rows;
  try {
    rows = connection.db.query(sql, parameters.toArray());
  } catch (error) {
    return convert_error(error);
  }
  return new Ok(List.fromArray(rows));
}

export function prepare(sql, connection) {
  let statement;
  try {
    statement = connection.db.prepareQuery(sql);
    connection.statements.push(statement);
  } catch (error) {
    return convert_error(error);
  }
  return new Ok(statement);
}

export function query_prepared(statement, _connection, parameters) {
  let rows;
  try {
    rows = statement.all(parameters.toArray());
  } catch (error) {
    return convert_error(error);
  }
  return new Ok(List.fromArray(rows));
}

export function null_() {
  return undefined;
}

function convert_error(error) {
  return new GlError(
    new SqlightError(
      error_code_from_int(error.code),
      error.message,
      error.offset || -1
    )
  );
}
