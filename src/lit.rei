/** SQL literal values. */

/** The type of literals. Can be extended by drivers. */

type t('a) = ..;

type t('a) +=
  | Bool(bool): t(bool)
  | Int(int): t(int)
  | Float(float): t(float)
  | String(string): t(string)
  | /** Basic SQL literals. */ Blob(bytes): t(bytes);

/** {3 Literal creation}

    The functions below create literal values of the appropriate types. */

let bool: bool => t(bool);
let int: int => t(int);
let float: float => t(float);
let string: string => t(string);
let blob: bytes => t(bytes);

/** Literals for nullable fields. */

module Null: {
  type t('a) +=
    | Bool(bool): t(option(bool))
    | Int(int): t(option(int))
    | Float(float): t(option(float))
    | String(string): t(option(string))
    | /** Basic SQL literals for nullable fields. */
      Blob(bytes): t(option(bytes));

  /** {3 Literal creation}

      The functions below create literal values of the appropriate types. */

  let bool: bool => t(option(bool));
  let int: int => t(option(int));
  let float: float => t(option(float));
  let string: string => t(option(string));
  let blob: bytes => t(option(bytes));
};

type lit('a, 'b) = t('b);

/** Vectors of SQL literals. */

module Vector: Vector.S with type elem('a, 'b) := lit('a, 'b);

/** {2 Functions useful for driver writers} */

/** Build a literal value. */

let build:
  (~placeholder: int => string, Common.build_step, t('a)) => Common.build_step;

/** Convert a literal value to a query parameter. */

let to_param: t('a) => Param.t;
