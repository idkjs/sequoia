/** Definitions useful in multiple query types. */

/** Build a [WHERE] expression. */

let build_where:
  (
    ~placeholder: int => string,
    ~handover: Expr.handover,
    Common.build_step,
    option(Expr.t('a))
  ) =>
  Common.build_step;

/** Build a [LIMIT] expression. */

let build_limit:
  (int => string, Common.build_step, option((int, int))) => Common.build_step;

/** Expressions to be used in [UPDATE] or [DELETE] queries. */

module UpdateDeleteExpr: {
  type Expr.t(_) +=
    | Field(Field.t('t, 'a), Table.t('t)): Expr.t('a)
    | /** Extend the basic expressions with fields and foreign keys. */
      Foreign(Field.foreign_key('t, 'u), Table.t('t)): Expr.t('a);

  /** A table field expression. */

  let field: (Field.t('t, 'a), Table.t('t)) => Expr.t('a);

  /** A table foreign key expression. */

  let foreign_key:
    (Field.foreign_key('t1, 't2), Table.t('t1)) => Expr.t('a);

  /** {3 Functions useful for driver writers} */

  let build:
    (
      ~placeholder: int => string,
      ~handover: Expr.handover,
      Common.build_step,
      Expr.t('a)
    ) =>
    Common.build_step;
};

/** Types and values used in [INSERT] and [REPLACE] queries. They're not
    supposed to be called directly and therefore are not documented here. */

module InsertReplace: {
  module Vector: Vector.S with type elem('t, 'a) := Field.t('t, 'a);

  type t(_);

  let create:
    (
      ~into: Table.t('t),
      ~fields: Vector.t('t, 'a, Nat.s('n)),
      ~values: Lit.Vector.matrix('u, 'a, Nat.s('m), Nat.s('n))
    ) =>
    t('t);

  let seal:
    (~placeholder: int => string, ~query: string, t('t)) =>
    (string, list(Param.t));
};
