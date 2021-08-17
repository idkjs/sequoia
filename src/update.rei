/** [UPDATE] query builder. */

module type S = {
  /** The type of [UPDATE] queries. */

  type t(_);

  /** Mark the end of a [UPDATE] query. */

  let seal: (~handover: Expr.handover, t('t)) => (string, list(Param.t));

  /** Definitions for [ORDER BY] clauses. */

  module OrderBy: {
    /** The order type, i.e. ascending or descending. */

    type order;

    /** Expressions in [ORDER BY] clauses must specify an order. */

    type expr('t, 'a) = (Expr.t('a), order);

    module Expr: {
      /** Type of [ORDER BY] expressions builders. */

      type mk('t, 'a) = Table.t('t) => (Expr.t('a), order);

      /** Vector of [ORDER BY] expressions builders. */

      module Vector: Vector.S with type elem('t, 'a) := mk('t, 'a);

      /** Order by the given expression ascendingly. */

      let asc: (Table.t('t) => Expr.t('a), Table.t('t)) => expr('t, 'a);

      /** Order by the given expression descendingly. */

      let desc: (Table.t('t) => Expr.t('a), Table.t('t)) => expr('t, 'a);
    };

    /** Vector of [ORDER BY] expressions. */

    module Vector: Vector.S with type elem('t, 'a) := expr('t, 'a);

    /** {3 Functions useful for driver writers} */

    /** Convert a vector of expression builders into a vector of expressions
          when given a [source]. */

    let vectormk_to_vector:
      (Table.t('t), Expr.Vector.t('t, 'a, 'n)) => Vector.t('t, 'a, 'n);
  };

  /** The type of [UPDATE] builders. */

  type mk('t, 'a) = (Field.t('t, 'a), Table.t('t) => Expr.expr('t, 'a));

  /** Defines a [WHERE] clause for an [UPDATE] query. */

  let where: (Table.t('t) => Expr.t('a), t('t)) => t('t);

  /** Defines an [ORDER BY] clause for an [UPDATE] query. */

  let order_by: (OrderBy.Expr.Vector.t('t, 'a, Nat.s('n)), t('t)) => t('t);

  /** Defines a [LIMIT] clause for an [UPDATE] query. */

  let limit: (~offset: int=?, int, t('t)) => t('t);

  /** [UPDATE] query expressions. */

  module Expr: {
    include (module type of Query_common.UpdateDeleteExpr);

    /** The type of [UPDATE] expression builders. */

    type mk('t, 'a) = Table.t('t) => Expr.expr('t, 'a);

    /** Vector of [UPDATE] expression builders. */

    module Vector: Vector.S with type elem('t, 'a) := mk('t, 'a);
  };

  /** Vector of [UPDATE] builders. */

  module Vector: Vector.S with type elem('t, 'a) := mk('t, 'a);

  /** Defines an [UPDATE] query for the given field/expression pairs. */

  let update: (Table.t('t), ~set: Vector.t('t, 'a, Nat.s('n))) => t('t);
};

/** Functor that generates an [UPDATE] query builder for the given driver. */

module Make: (D: Driver.S) => S;
