open Printf;
open Common;

module type S = {
  type t(_);

  let seal: (~handover: Expr.handover, t('t)) => (string, list(Param.t));

  type mk('t, 'a) = (Field.t('t, 'a), Table.t('t) => Expr.expr('t, 'a));

  module OrderBy: {
    type order;

    type expr('t, 'a) = (Expr.t('a), order);

    module Expr: {
      type mk('t, 'a) = Table.t('t) => (Expr.t('a), order);

      module Vector: Vector.S with type elem('t, 'a) := mk('t, 'a);

      let asc: (Table.t('t) => Expr.t('a), Table.t('t)) => expr('t, 'a);
      let desc: (Table.t('t) => Expr.t('a), Table.t('t)) => expr('t, 'a);
    };

    module Vector: Vector.S with type elem('t, 'a) := expr('t, 'a);

    let vectormk_to_vector:
      (Table.t('t), Expr.Vector.t('t, 'a, 'n)) => Vector.t('t, 'a, 'n);
  };

  let where: (Table.t('t) => Expr.t('a), t('t)) => t('t);
  let order_by: (OrderBy.Expr.Vector.t('t, 'a, Nat.s('n)), t('t)) => t('t);
  let limit: (~offset: int=?, int, t('t)) => t('t);

  module Expr: {
    include (module type of Query_common.UpdateDeleteExpr);

    type mk('t, 'a) = Table.t('t) => Expr.expr('t, 'a);

    module Vector: Vector.S with type elem('t, 'a) := mk('t, 'a);
  };

  module Vector: Vector.S with type elem('t, 'a) := mk('t, 'a);

  let update: (Table.t('t), ~set: Vector.t('t, 'a, Nat.s('n))) => t('t);
};

module Make = (D: Driver.S) : S => {
  open Query_common;

  module OrderBy = {
    type order =
      | Asc
      | Desc;

    type expr('s, 'a) = (Expr.t('a), order);

    module Expr = {
      type mk('t, 'a) = Table.t('t) => (Expr.t('a), order);

      module Vector =
        Vector.Make({
          type elem('t, 'a) = mk('t, 'a);
        });

      let asc = (f, src) => (f(src), Asc);
      let desc = (f, src) => (f(src), Desc);
    };

    module Vector =
      Vector.Make({
        type elem('t, 'a) = expr('t, 'a);
      });

    let rec vectormk_to_vector:
      type a n. (Table.t('t), Expr.Vector.t('t, a, n)) => Vector.t('t, a, n) =
      (src, vec) =>
        Expr.Vector.(
          switch (vec) {
          | [] => Vector.[]
          | [f, ...fs] => Vector.[f(src), ...vectormk_to_vector(src, fs)]
          }
        );
  };

  module E = {
    include UpdateDeleteExpr;

    module Vec =
      Vector.Make({
        type elem('t, 'a) = Expr.expr('t, 'a);
      });

    type mk('t, 'a) = Table.t('t) => Expr.expr('t, 'a);

    module Vector =
      Vector.Make({
        type elem('t, 'a) = mk('t, 'a);
      });

    let rec vectormk_to_vector:
      type a n. (Table.t('t), Vector.t('t, a, n)) => Vec.t('t, a, n) =
      (table, vec) =>
        Vector.(
          switch (vec) {
          | [] => Vec.[]
          | [f, ...rest] =>
            Vec.[f(table), ...vectormk_to_vector(table, rest)]
          }
        );
  };

  module UpdateVec =
    Vector.Make({
      type elem('t, 'a) = (Field.t('t, 'a), Expr.expr('t, 'a));
    });

  type mk('t, 'a) = (Field.t('t, 'a), Table.t('t) => Expr.expr('t, 'a));

  module Vector =
    Vector.Make({
      type elem('t, 'a) = mk('t, 'a);
    });

  type t('t) =
    | U({
        table: Table.t('t),
        updates: UpdateVec.t('t, _, Nat.s('n)),
        where: option(Expr.t(_)),
        order_by: option(OrderBy.Vector.t(_, _, Nat.s('m))),
        limit: option((int, int)),
      })
      : t('t);

  let rec vectormk_to_vector:
    type a n. (Table.t('t), Vector.t('t, a, n)) => UpdateVec.t('t, a, n) =
    (table, vec) =>
      Vector.(
        switch (vec) {
        | [] => UpdateVec.[]
        | [(fld, f), ...rest] =>
          UpdateVec.[(fld, f(table)), ...vectormk_to_vector(table, rest)]
        }
      );

  let update = (table, ~set) =>
    U({
      table,
      updates: vectormk_to_vector(table, set),
      where: None,
      limit: None,
      order_by: None,
    });

  let where = (expr, U(stmt)) =>
    U({...stmt, where: Some(expr(stmt.table))});

  let limit = (~offset=0, n, U(stmt)) =>
    U({...stmt, limit: Some((offset, n))});

  let order_by:
    type a u n. (OrderBy.Expr.Vector.t(u, a, Nat.s(n)), t(u)) => t(u) =
    (bl, U(stmt)) => {
      let vec = OrderBy.vectormk_to_vector(stmt.table, bl);
      U({...stmt, order_by: Some(vec)});
    };

  let build_updates = (~handover, st, table, updates) => {
    let fold:
      type t a b.
        ((build_step, int), (Field.t(t, a), Expr.expr(b, a))) =>
        (build_step, int) =
      ((st, i), (fld, expr)) => {
        let fld = Field.to_string(fld);
        let st' = E.build(~placeholder=D.placeholder, ~handover, st, expr);
        if (i == 0) {
          (st', i);
        } else {
          let suf =
            if (i == 1) {
              "";
            } else {
              ",";
            };
          let st = {
            ...st',
            repr: sprintf("%s\n%s = %s%s", st.repr, fld, st'.repr, suf),
            params: st.params @ st'.params,
            pos: st'.pos,
          };
          (st, i - 1);
        };
      };
    let len = UpdateVec.vector_length(updates);
    fst @@
    UpdateVec.vector_fold_left(
      {UpdateVec.f: fold},
      ({...blank_step, pos: st.pos, aliases: st.aliases}, len),
      updates,
    );
  };

  let order_to_string =
    fun
    | OrderBy.Asc => "ASC"
    | OrderBy.Desc => "DESC";

  let join_order_by_exprs = (~handover, st) =>
    OrderBy.Vector.vector_fold_left(
      {
        OrderBy.Vector.f: ((st, i), (e, ord)) => {
          let st' = E.build(~placeholder=D.placeholder, ~handover, st, e);
          let st' = {...st', repr: st'.repr ++ " " ++ order_to_string(ord)};
          if (i == 0) {
            (st', 1);
          } else {
            let st = {
              ...st',
              repr: st.repr ++ ", " ++ st'.repr,
              params: st.params @ st'.params,
              pos: st'.pos,
            };
            (st, i + 1);
          };
        },
      },
      ({...blank_step, pos: st.pos, aliases: st.aliases}, 0),
    );

  let build_order_by = (~handover, st) =>
    fun
    | Some(exprs) => {
        let st = fst(join_order_by_exprs(~handover, st, exprs));
        {...st, repr: "ORDER BY " ++ st.repr};
      }
    | None => {...blank_step, pos: st.pos, aliases: st.aliases};

  let seal = (~handover, U({table, updates, where, limit, order_by})) => {
    let st = {...blank_step, pos: 1};
    let updates_st = build_updates(~handover, st, table, updates);
    let where_st =
      build_where(~placeholder=D.placeholder, ~handover, updates_st, where);
    let order_by_st = build_order_by(~handover, where_st, order_by);
    let limit_st = build_limit(D.placeholder, order_by_st, limit);
    let s = sprintf("UPDATE %s SET%s", Table.name(table), updates_st.repr);
    let params =
      updates_st.params
      @ where_st.params
      @ order_by_st.params
      @ limit_st.params;
    let repr =
      join_lines([s, where_st.repr, limit_st.repr, order_by_st.repr]);
    (repr, params);
  };

  module Expr = E;
};
