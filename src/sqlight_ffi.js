import { List, Ok, Error as GlError } from "./gleam.mjs";
import { SqlightError, error_code_from_int } from "./sqlight.mjs";
import { DB } from "https://deno.land/x/sqlite@v3.9.1/mod.ts";

export function open(path) {
  return new Ok(new DB(path));
}

export function close(connection) {
  connection.close();
  return new Ok(undefined);
}

export function coerce_value(value) {
  return value;
}

export function coerce_blob(value) {
  return value.buffer;
}

export function status(connection) {
  throw new Error("status");
}

export function exec(sql, connection) {
  try {
    connection.execute(sql);
    return new Ok(undefined);
  } catch (error) {
    return convert_error(error);
  }
}

export function query(sql, connection, parameters) {
  let rows;
  try {
    rows = connection.query(sql, parameters.toArray());
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
