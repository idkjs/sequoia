/** [DELETE] query builder. */

module type S = {
  /** The type of [DELETE] queries. */

  type t(_);

  /** Define a [DELETE] query on the table given by [from] and conditions
        specified by [where]. */

  let delete:
    (~where: Table.t('t) => Expr.t(bool)=?, ~from: Table.t('t)) => t('t);

  /** Mark the end of a [DELETE] query. */

  let seal: (~handover: Expr.handover, t('t)) => (string, list(Param.t));

  /** Valid expressions for use in [DELETE] query. */

  module Expr: (module type of Query_common.UpdateDeleteExpr);
};

/** Functor that generates a [DELETE] query builder for the given driver. */

module Make: (D: Driver.S) => S;
