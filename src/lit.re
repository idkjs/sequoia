open Common;

type t('a) = ..;

type t('a) +=
  | Bool(bool): t(bool)
  | Int(int): t(int)
  | Float(float): t(float)
  | String(string): t(string)
  | Blob(bytes): t(bytes);

let bool = b => Bool(b);
let int = i => Int(i);
let float = x => Float(x);
let string = s => String(s);
let blob = b => Blob(b);

module Null = {
  type t('a) +=
    | Bool(bool): t(option(bool))
    | Int(int): t(option(int))
    | Float(float): t(option(float))
    | String(string): t(option(string))
    | Blob(bytes): t(option(bytes));

  let bool = b => Bool(b);
  let int = i => Int(i);
  let float = x => Float(x);
  let string = s => String(s);
  let blob = b => Blob(b);
};

let to_param: type a. t(a) => Param.t =
  fun
  | Bool(b) => Param.Bool(b)
  | Int(i) => Param.Int(i)
  | Float(x) => Param.Float(x)
  | String(s) => Param.String(s)
  | Blob(b) => Param.Blob(b)
  | Null.Bool(b) => Param.Bool(b)
  | Null.Int(i) => Param.Int(i)
  | Null.Float(x) => Param.Float(x)
  | Null.String(s) => Param.String(s)
  | Null.Blob(b) => Param.Blob(b)
  | _ => assert(false);

let build:
  type a. (~placeholder: int => string, build_step, t(a)) => build_step =
  (~placeholder, st, lit) => build_param(placeholder, st, to_param(lit));

type lit('a, 'b) = t('b);

module Vector =
  Vector.Make({
    type elem('a, 'b) = lit('a, 'b);
  });
