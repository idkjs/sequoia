open Printf;

type t('t, 'a) = ..;
type t('t, 'a) +=
  | Bool(string, Table.t('t)): t('t, bool)
  | Int(string, Table.t('t)): t('t, int)
  | Float(string, Table.t('t)): t('t, float)
  | String(string, Table.t('t)): t('t, string)
  | Blob(string, Table.t('t)): t('t, bytes);

module Null = {
  type t('t, 'a) +=
    | Bool(string, Table.t('t)): t('t, option(bool))
    | Int(string, Table.t('t)): t('t, option(int))
    | Float(string, Table.t('t)): t('t, option(float))
    | String(string, Table.t('t)): t('t, option(string))
    | Blob(string, Table.t('t)): t('t, option(bytes));

  let bool = (table, name) => [@implicit_arity] Bool(name, table);
  let int = (table, name) => [@implicit_arity] Int(name, table);
  let float = (table, name) => [@implicit_arity] Float(name, table);
  let string = (table, name) => [@implicit_arity] String(name, table);
  let blob = (table, name) => [@implicit_arity] Blob(name, table);
};

type foreign_key('t1, 't2) = (t('t1, int), t('t2, int));

let name: type a. t('t, a) => string =
  fun
  | [@implicit_arity] Bool(n, _) => n
  | [@implicit_arity] Int(n, _) => n
  | [@implicit_arity] Float(n, _) => n
  | [@implicit_arity] String(n, _) => n
  | [@implicit_arity] Blob(n, _) => n
  | [@implicit_arity] Null.Bool(n, _) => n
  | [@implicit_arity] Null.Int(n, _) => n
  | [@implicit_arity] Null.Float(n, _) => n
  | [@implicit_arity] Null.String(n, _) => n
  | [@implicit_arity] Null.Blob(n, _) => n
  | _ => assert(false);

let table: type u a. t(u, a) => Table.t(u) =
  fun
  | [@implicit_arity] Bool(_, t) => t
  | [@implicit_arity] Int(_, t) => t
  | [@implicit_arity] Float(_, t) => t
  | [@implicit_arity] String(_, t) => t
  | [@implicit_arity] Blob(_, t) => t
  | [@implicit_arity] Null.Bool(_, t) => t
  | [@implicit_arity] Null.Int(_, t) => t
  | [@implicit_arity] Null.Float(_, t) => t
  | [@implicit_arity] Null.String(_, t) => t
  | [@implicit_arity] Null.Blob(_, t) => t
  | _ => assert(false);

let to_string = fld => {
  let t = table(fld);
  sprintf("%s.%s", Table.name(t), name(fld));
};

let foreign_key = (table, name, ~references) => (
  [@implicit_arity] Int(name, table),
  references,
);

let bool = (table, name) => [@implicit_arity] Bool(name, table);
let int = (table, name) => [@implicit_arity] Int(name, table);
let float = (table, name) => [@implicit_arity] Float(name, table);
let string = (table, name) => [@implicit_arity] String(name, table);
let blob = (table, name) => [@implicit_arity] Blob(name, table);
