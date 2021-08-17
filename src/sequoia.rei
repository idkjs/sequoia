/** Sequoia is a type-safe query builder for OCaml.*/

module type NAMED = {
  type t;
  let name: string;
};

module Common: (module type of Common);

/** The Sequoia query builder interface, output by the [Make] functor below. */

module type S = {
  /** {2 Query builder interface} */

  /** Query parameters. */

  module Param: (module type of Param);

  /** Query parameters. */
  /** Literal values. */

  module Lit: (module type of Lit);

  /** Literal values. */
  /** Expressions. */

  module Expr: (module type of Expr);

  /** Expressions. */
  /** SQL tables. */

  module Table: (module type of Table);

  /** SQL tables. */
  /** Field definitions. */

  module Field: (module type of Field);

  /** Field definitions. */
  /** Heterogeneous lists with type-encoded length. */

  module Vector: (module type of Vector);

  /** [SELECT] queries. */

  module Select: Select.S;

  /** [SELECT] queries. */
  /** [INSERT] queries. */

  module Insert: Insert.S;

  /** [INSERT] queries. */
  /** [REPLACE] queries. */

  module Replace: Replace.S;

  /** [REPLACE] queries. */
  /** [UPDATE] queries. */

  module Update: Update.S;

  /** [UPDATE] queries. */
  /** [DELETE] queries. */

  module Delete: Delete.S;

  /** Field definitions for a given table. */

  module type FIELD = {
    type table;
    type t('a) = Field.t(table, 'a);
    type foreign_key('t) = Field.foreign_key(table, 't);

    /** {3 Field definition functions}

		    These functions define a field of the respective type that
        corresponds to the SQL field given by the string parameter. */

    let bool: string => t(bool);
    let int: string => t(int);
    let float: string => t(float);
    let string: string => t(string);
    let blob: string => t(bytes);

    let foreign_key:
      (string, ~references: Field.t('t, int)) => foreign_key('t);
  };

  /** Nullable field definitions for a given table. */

  module type NULL_FIELD = {
    type t('a);

    /** {3 Field definition functions}

		    These functions define a field of the respective type that
        corresponds to the nullable SQL field given by the string parameter. */

    let bool: string => t(option(bool));
    let int: string => t(option(int));
    let float: string => t(option(float));
    let string: string => t(option(string));
    let blob: string => t(option(bytes));
  };

  module type TABLE = {
    type t;
    let table: Table.t(t);

    module Field: {
      include FIELD with type table = t;
      module Null: NULL_FIELD with type t('a) := t('a);
    };
  };

  /** [table name] returns a module that can be used to define SQL table
				fields. The intended use is for this result to be included in an
			  OCaml module that will hold the field definitions. */

  let table: string => (module TABLE);

  /** Functor that generates a table module to be used in SQL table
        defintions. Should only be useful for driver writers. */

  module MakeTable: (T: NAMED) => TABLE;
};

/* Functor that outputs a query builder from  */
module Make:
  (D: Driver.S) =>

    S with
      type Table.t('t) = Table.t('t) and
      type Field.t('t, 'a) = Field.t('t, 'a) and
      type Field.foreign_key('t1, 't2) = Field.foreign_key('t1, 't2) and
      type Lit.t('a) = Lit.t('a) and
      type Expr.t('a) = Expr.t('a) and
      type Expr.cast('a) = Expr.cast('a) and
      type Expr.handover = Expr.handover and
      type Param.t = Param.t and
      module Lit.Vector = Lit.Vector;
