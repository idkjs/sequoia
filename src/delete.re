open Printf;
open Common;

module type S = {
  type t(_);

  let delete:
    (~where: Table.t('t) => Expr.t(bool)=?, ~from: Table.t('t)) => t('t);
  let seal: (~handover: Expr.handover, t('t)) => (string, list(Param.t));

  module Expr: (module type of Query_common.UpdateDeleteExpr);
};

module Make = (D: Driver.S) : S => {
  open Query_common;

  type t('t) =
    | D({
        table: Table.t('t),
        where: option(Expr.t('a)),
      })
      : t('t);

  let delete = (~where=?, ~from) => {
    let where =
      switch (where) {
      | Some(expr) => Some(expr(from))
      | None => None
      };
    D({table: from, where});
  };

  let seal = (~handover, D({table, where})) => {
    let st = {...blank_step, pos: 1};
    let st = build_where(~placeholder=D.placeholder, ~handover, st, where);
    let s = sprintf("DELETE FROM %s", Table.name(table));
    let repr = join_lines([s, st.repr]);
    (repr, st.params);
  };

  module Expr = UpdateDeleteExpr;
};
