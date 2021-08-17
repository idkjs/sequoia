/** SQL field definitions. */

/** The type of SQL fields. Can be extended by driver writers. */

type t('t, 'a) = ..;

type t('t, 'a) +=
  | Bool(string, Table.t('t)): t('t, bool)
  | Int(string, Table.t('t)): t('t, int)
  | Float(string, Table.t('t)): t('t, float)
  | String(string, Table.t('t)): t('t, string)
  | /** The basic SQL field types. */
    Blob(string, Table.t('t)): t('t, bytes);

/** The type of foreign keys. They are a different type to allow
      enforcing of field relationships. */

type foreign_key('t1, 't2) = (t('t1, int), t('t2, int));

/** The name of a field. */

let name: t('t, 'a) => string;

/** The name of a field. */
/** The table a field belongs to. */

let table: t('t, 'a) => Table.t('t);

/** The table a field belongs to. */
/** The string representation of a field, i.e. [table].[name]. */

let to_string: t('t, 'a) => string;

/** A boolean field. */

let bool: (Table.t('t), string) => t('t, bool);

/** A boolean field. */
/** An integer field. */

let int: (Table.t('t), string) => t('t, int);

/** An integer field. */
/** A float field. */

let float: (Table.t('t), string) => t('t, float);

/** A float field. */
/** A string field. */

let string: (Table.t('t), string) => t('t, string);

/** A string field. */
/** A blob field. */

let blob: (Table.t('t), string) => t('t, bytes);

/** A foreign key referencing a field in another table. */

let foreign_key:
  (Table.t('t1), string, ~references: t('t2, int)) => foreign_key('t1, 't2);

module Null: {
  type t('t, 'a) +=
    | Bool(string, Table.t('t)): t('t, option(bool))
    | Int(string, Table.t('t)): t('t, option(int))
    | Float(string, Table.t('t)): t('t, option(float))
    | String(string, Table.t('t)): t('t, option(string))
    | /** The type of nullable fields. */
      Blob(string, Table.t('t)): t('t, option(bytes));

  /** A nullable boolean field. */

  let bool: (Table.t('t), string) => t('t, option(bool));

  /** A nullable boolean field. */
  /** A nullable int field. */

  let int: (Table.t('t), string) => t('t, option(int));

  /** A nullable int field. */
  /** A nullable float field. */

  let float: (Table.t('t), string) => t('t, option(float));

  /** A nullable float field. */
  /** A nullable string field. */

  let string: (Table.t('t), string) => t('t, option(string));

  /** A nullable string field. */
  /** A nullable blob field. */

  let blob: (Table.t('t), string) => t('t, option(bytes));
};
