/** [SELECT] query builder. */

open Common;

module type S = {
  /** The type of [SELECT] queries. */

  type t(_);

  type join_steps('s1, 't1, 't2, 's2) =
    | There: join_steps('t2 => _ as 's1, 't1, 't2, 't1 => 's1)
    | /** This type allows the representation of SQL joins in the OCaml type
        system. */
      Skip(
        join_steps('s1, 't1, 't2, 's2),
      )
      : join_steps('a => 's1, 't1, 't2, 'a => 's2);

  /** The type of SQL joins. */

  type join;

  type source('s) =
    | From(Table.t('t)): source('t => unit)
    | /** Data sources for [SELECT] queries: tables specified in the the [FROM]
        or [JOIN] clause. Joins are only allowed on fields declared as
        foreign keys. The [join_steps] type joins the tables in the OCaml
        type system sense. */
      Join(
        join,
        Field.foreign_key('t1, 't2),
        source('s1),
        join_steps('s1, 't1, 't2, 's2),
      )
      : source('s2);

  type steps('s1, 't1, 't2, 's2) =
    | There: steps('t2 => _ as 's1, 't1, 't2, 's1)
    | /** This is analogous to [join_steps] but instead of building the set of
        data sources (i.e. the tables being referenced in the query), it is
        used to simply reference them. */
      Skip(
        steps('s1, 't1, 't2, 's2),
      )
      : steps('a => 's1, 't1, 't2, 'a => 's2);

  /** Mark the end of a [SELECT] query. */

  let seal: (~handover: Expr.handover, t('s)) => (string, list(Param.t));

  /** Expressions that are valid in [SELECT] queries. */

  module Expr: {
    type select('s) = t('s);

    type Expr.t('a) +=
      | Field(Field.t('t, 'a), source('s1), steps('s1, 't1, 't, 's2)): Expr.t(
                                                                    'a,
                                                                    )
      | Foreign(
                 Field.foreign_key('t, 't2),
                 source('s1),
                 steps('s1, 't1, 't, 's2),
               ): Expr.t('a)
      | /** Extend basic expressions with field and foreign key references and
          subqueries. */
        Select(select('s)): Expr.t('a);

    type t('a) = Expr.t('a);

    /** Define an alias for an expression (i.e. the SQL [AS] keyword). */

    let (-->): (source('s) => t('a), string, source('s)) => t('a);

    /** Reference a previously defined alias. Note: alias references are
          not type-safe! */

    let alias: (string, source('s)) => t('a);

    /** A field expression. */

    let field:
      (Field.t('t, 'a), steps('b, 'c, 't, 'd), source('b)) => t('a);

    /** A foreign key expression. */

    let foreign_key:
      (Field.foreign_key('t1, 't2), steps('a, 'b, 't1, 'c), source('a)) =>
      t('d);

    /** A subquery expression. */

    let subquery: (select('s), source('t)) => t('c);

    /** Convert a nullable field to an expression of the non-null type. */

    let unwrap:
      (Field.t('t, option('a)), steps('b, 'c, 't, 'd), source('b)) =>
      t('a);

    /** An expression builder, i.e. a function that returns an expression
          when given a [source]. */

    type mk('s, 'a) = source('s) => t('a);

    /** Vectors of expression builders. */

    module Vector: Vector.S with type elem('s, 'a) := mk('s, 'a);

    /** {3 Functions useful for driver writers} */

    /** Build an expression. */

    let build: (~handover: Expr.handover, build_step, t('a)) => build_step;

    /** Convert a vector of expression builders into a vector of expressions
          when given a [source]. */

    let vectormk_to_vector:
      (source('s), Vector.t('s, 'a, 'n)) => Expr.Vector.t('s, 'a, 'n);
  };

  /** Define the selected expressions in a [SELECT] query. */

  let select:
    (~distinct: bool=?, Expr.Vector.t('s, 'a, Nat.s('n)), source('s)) =>
    t('s);

  /** Starts a [SELECT] query by specifying the initial data source. */

  let from: Table.t('t) => source('t => unit);

  /** Specifies a [LEFT JOIN] adding the table containing the given foreign
        key as a data source. */

  let left_join:
    (
      (Field.foreign_key('t1, 't2), join_steps('s1, 't1, 't2, 's2)),
      source('s1)
    ) =>
    source('s2);

  /** Specifies a [RIGHT JOIN] adding the table containing the given foreign
        key as a data source. */

  let right_join:
    (
      (Field.foreign_key('t1, 't2), join_steps('s1, 't1, 't2, 's2)),
      source('s1)
    ) =>
    source('s2);

  /** Specifies an [INNER JOIN] adding the table containing the given foreign
        key as a data source. */

  let inner_join:
    (
      (Field.foreign_key('t1, 't2), join_steps('s1, 't1, 't2, 's2)),
      source('s1)
    ) =>
    source('s2);

  /** Allows self-joins, i.e. joins that use fields belonging to a single
        table. */

  let self:
    (Field.t('t, int), Field.t('t, int), join_steps('s1, 't1, 't, 's2)) =>
    (Field.foreign_key('t, 't), join_steps('s1, 't1, 't, 's2));

  /** Call this on a foreign key when you want to define a join with the
        table that contains the foreign key. */

  let this:
    (Field.foreign_key('t1, 't2), join_steps('s1, 't1, 't2, 's2)) =>
    (Field.foreign_key('t1, 't2), join_steps('s1, 't1, 't2, 's2));

  /** Call this on a foreign key when you want to define a join with the
        table that contains the field that the foreign key references. */

  let that:
    (Field.foreign_key('t1, 't2), join_steps('s1, 't2, 't1, 's2)) =>
    (Field.foreign_key('t2, 't1), join_steps('s1, 't2, 't1, 's2));

  /** Defines a [WHERE] clause for a [SELECT] query. */

  let where: (source('a) => Expr.t(bool), t('a)) => t('a);

  /** Defines a [GROUP BY] clause for a [SELECT] query. */

  let group_by:
    (
      ~having: source('s) => Expr.t(bool)=?,
      Expr.Vector.t('s, 'a, Nat.s('n)),
      t('s)
    ) =>
    t('s);

  /** Definitions for [ORDER BY] clauses. */

  module OrderBy: {
    /** The order type, i.e. ascending or descending. */

    type order;

    /** Expressions in [ORDER BY] clauses must specify an order. */

    type expr('s, 'a) = (Expr.t('a), order);

    module Expr: {
      /** Type of [ORDER BY] expressions builders. */

      type mk('s, 'a) = source('s) => (Expr.t('a), order);

      /** Vector of [ORDER BY] expressions builders. */

      module Vector: Vector.S with type elem('s, 'a) := mk('s, 'a);

      /** Order by the given expression ascendingly. */

      let asc: (source('s) => Expr.t('a), source('s)) => expr('s, 'a);

      /** Order by the given expression descendingly. */

      let desc: (source('s) => Expr.t('a), source('s)) => expr('s, 'a);
    };

    /** Vector of [ORDER BY] expressions. */

    module Vector: Vector.S with type elem('s, 'a) := expr('s, 'a);

    /** {3 Functions useful for driver writers} */

    /** Convert a vector of expression builders into a vector of expressions
          when given a [source]. */

    let vectormk_to_vector:
      (source('s), Expr.Vector.t('s, 'a, 'n)) => Vector.t('s, 'a, 'n);
  };

  /** Defines an [ORDER BY] clause in a [SELECT] query. */

  let order_by: (OrderBy.Expr.Vector.t('s, 'a, Nat.s('n)), t('s)) => t('s);

  /** Defines a [LIMIT] clause in a [SELECT] query. */

  let limit: (~offset: int=?, int, t('a)) => t('a);
};

/** Functor that generates a [SELECT] query builder for the given driver. */

module Make: (D: Driver.S) => S;
