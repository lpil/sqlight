# Changelog

## v1.0.3 - 2025-10-21

- Remove version specifier for Deno `x/sqlite` import.

## v1.0.2 - 2025-07-01

- Updated for latest `gleam_stdlib`.

## v1.0.1 - 2025-02-02

- Removed a debug statement.

## v1.0.0 - 2025-01-04

- Updated to use `gleam/dynamic/decode`.

## v0.9.1 - 2024-08-19

- Fixed a bug where bit arrays could bind to the incorrect SQLite type.

## v0.9.0 - 2023-11-06

- Updated for v0.32.0.

## v0.8.0 - 2023-09-22

- The `nullable` function has been added.

## v0.7.0 - 2023-08-03

- Updated syntax for Gleam v0.30.0.

## v0.6.0 - 2023-03-23

- Updated syntax for Gleam v0.27.0.

## v0.6.0 - 2023-01-15

- Fixed a bug where `query` could crash when constructing an error when a query
  was invalid or otherwise failed.

## v0.5.0 - 2023-01-08

- This library now works on JavaScript with the Deno runtime.
- The `status` function has been removed.

## v0.4.0 - 2023-01-02

- The error type now contains more detail.

## v0.3.0 - 2023-01-02

- the `with_connection` function no longer returns a result.

## v0.2.0 - 2022-12-31

- The `bool` and `decode_bool` functions have been added.

## v0.1.0 - 2022-12-31

- Initial release.
