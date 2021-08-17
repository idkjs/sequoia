open Printf;
open Common;

module type NAMED = {
  type t;
  let name: string;
};

module Common = Common;

module type S = {
  module Param: (module type of Param);
  module Lit: (module type of Lit);
  module Expr: (module type of Expr);
  module Table: (module type of Table);
  module Field: (module type of Field);
  module Vector: (module type of Vector);

  module Select: Select.S;
  module Insert: Insert.S;
  module Replace: Replace.S;
  module Update: Update.S;
  module Delete: Delete.S;

  module type NULL_FIELD = {
    type t('a);
    let bool: string => t(option(bool));
    let int: string => t(option(int));
    let float: string => t(option(float));
    let string: string => t(option(string));
    let blob: string => t(option(bytes));
  };

  module type FIELD = {
    type table;
    type t('a) = Field.t(table, 'a);
    type foreign_key('t) = Field.foreign_key(table, 't);

    let bool: string => t(bool);
    let int: string => t(int);
    let float: string => t(float);
    let string: string => t(string);
    let blob: string => t(bytes);

    let foreign_key:
      (string, ~references: Field.t('t, int)) => foreign_key('t);
  };

  module type TABLE = {
    type t;
    let table: Table.t(t);

    module Field: {
      include FIELD with type table = t;
      module Null: NULL_FIELD with type t('a) := t('a);
    };
  };

  let table: string => (module TABLE);

  module MakeTable: (T: NAMED) => TABLE;
};

module Make =
       (D: Driver.S)

         : (
           S with
             type Table.t('t) = Table.t('t) and
             type Field.t('t, 'a) = Field.t('t, 'a) and
             type Field.foreign_key('t1, 't2) =
               Field.foreign_key('t1, 't2) and
             type Lit.t('a) = Lit.t('a) and
             type Expr.t('a) = Expr.t('a) and
             type Expr.cast('a) = Expr.cast('a) and
             type Expr.handover = Expr.handover and
             type Param.t = Param.t and
             module Lit.Vector = Lit.Vector
       ) => {
  module Param = Param;
  module Lit = Lit;
  module Expr = Expr;
  module Table = Table;
  module Field = Field;
  module Vector = Vector;

  module type NULL_FIELD = {
    type t('a);
    let bool: string => t(option(bool));
    let int: string => t(option(int));
    let float: string => t(option(float));
    let string: string => t(option(string));
    let blob: string => t(option(bytes));
  };

  module type FIELD = {
    type table;
    type t('a) = Field.t(table, 'a);
    type foreign_key('t) = Field.foreign_key(table, 't);

    let bool: string => t(bool);
    let int: string => t(int);
    let float: string => t(float);
    let string: string => t(string);
    let blob: string => t(bytes);

    let foreign_key:
      (string, ~references: Field.t('t, int)) => foreign_key('t);
  };

  module type TABLE = {
    type t;
    let table: Table.t(t);

    module Field: {
      include FIELD with type table = t;
      module Null: NULL_FIELD with type t('a) := t('a);
    };
  };

  module MakeTable = (T: NAMED) : TABLE => {
    type t = T.t;

    let table = Table.called(T.name);

    module Field = {
      type table = t;
      type t('a) = Field.t(table, 'a);
      type foreign_key('t) = Field.foreign_key(table, 't);

      let bool = Field.bool(table);
      let int = Field.int(table);
      let float = Field.float(table);
      let string = Field.string(table);
      let blob = Field.blob(table);

      let foreign_key = (name, ~references) =>
        Field.foreign_key(table, name, ~references);

      module Null = {
        let bool = Field.Null.bool(table);
        let int = Field.Null.int(table);
        let float = Field.Null.float(table);
        let string = Field.Null.string(table);
        let blob = Field.Null.blob(table);
      };
    };
  };

  let table = (name): (module TABLE) =>
    (module
     MakeTable({
       type t;
       let name = name;
     }));

  module Select = Select.Make(D);
  module Insert = Insert.Make(D);
  module Replace = Replace.Make(D);
  module Update = Update.Make(D);
  module Delete = Delete.Make(D);
};
