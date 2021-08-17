open Printf;
open Common;

module type S = {
  type t(_);

  type join_steps('s1, 't1, 't2, 's2) =
    | There: join_steps('t2 => _ as 's1, 't1, 't2, 't1 => 's1)
    | Skip(join_steps('s1, 't1, 't2, 's2))
      : join_steps('a => 's1, 't1, 't2, 'a => 's2);

  type join;

  type source('s) =
    | From(Table.t('t)): source('t => unit)
    | Join(
        join,
        Field.foreign_key('t1, 't2),
        source('s1),
        join_steps('s1, 't1, 't2, 's2),
      )
      : source('s2);

  type steps('s1, 't1, 't2, 's2) =
    | There: steps('t2 => _ as 's1, 't1, 't2, 's1)
    | Skip(steps('s1, 't1, 't2, 's2))
      : steps('a => 's1, 't1, 't2, 'a => 's2);

  let seal: (~handover: Expr.handover, t('s)) => (string, list(Param.t));

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
      | Select(select('s)): Expr.t('a);

    type t('a) = Expr.t('a);

    let (-->): (source('s) => t('a), string, source('s)) => t('a);
    let alias: (string, source('s)) => t('a);
    let field:
      (Field.t('t, 'a), steps('b, 'c, 't, 'd), source('b)) => t('a);
    let foreign_key:
      (Field.foreign_key('t1, 't2), steps('a, 'b, 't1, 'c), source('a)) =>
      t('d);

    let subquery: (select('s), source('t)) => t('c);

    let unwrap:
      (Field.t('t, option('a)), steps('b, 'c, 't, 'd), source('b)) =>
      t('a);

    type mk('s, 'a) = source('s) => t('a);

    module Vector: Vector.S with type elem('s, 'a) := mk('s, 'a);

    let build: (~handover: Expr.handover, build_step, t('a)) => build_step;

    let vectormk_to_vector:
      (source('s), Vector.t('s, 'a, 'n)) => Expr.Vector.t('s, 'a, 'n);
  };

  let select:
    (~distinct: bool=?, Expr.Vector.t('s, 'a, Nat.s('n)), source('s)) =>
    t('s);

  let from: Table.t('t) => source('t => unit);

  let left_join:
    (
      (Field.foreign_key('t1, 't2), join_steps('s1, 't1, 't2, 's2)),
      source('s1)
    ) =>
    source('s2);
  let right_join:
    (
      (Field.foreign_key('t1, 't2), join_steps('s1, 't1, 't2, 's2)),
      source('s1)
    ) =>
    source('s2);
  let inner_join:
    (
      (Field.foreign_key('t1, 't2), join_steps('s1, 't1, 't2, 's2)),
      source('s1)
    ) =>
    source('s2);

  let self:
    (Field.t('t, int), Field.t('t, int), join_steps('s1, 't1, 't, 's2)) =>
    (Field.foreign_key('t, 't), join_steps('s1, 't1, 't, 's2));

  let this:
    (Field.foreign_key('t1, 't2), join_steps('s1, 't1, 't2, 's2)) =>
    (Field.foreign_key('t1, 't2), join_steps('s1, 't1, 't2, 's2));
  let that:
    (Field.foreign_key('t1, 't2), join_steps('s1, 't2, 't1, 's2)) =>
    (Field.foreign_key('t2, 't1), join_steps('s1, 't2, 't1, 's2));

  let where: (source('a) => Expr.t(bool), t('a)) => t('a);
  let group_by:
    (
      ~having: source('s) => Expr.t(bool)=?,
      Expr.Vector.t('s, 'a, Nat.s('n)),
      t('s)
    ) =>
    t('s);

  module OrderBy: {
    type order;

    type expr('s, 'a) = (Expr.t('a), order);

    module Expr: {
      type mk('s, 'a) = source('s) => (Expr.t('a), order);

      module Vector: Vector.S with type elem('s, 'a) := mk('s, 'a);

      let asc: (source('s) => Expr.t('a), source('s)) => expr('s, 'a);
      let desc: (source('s) => Expr.t('a), source('s)) => expr('s, 'a);
    };

    module Vector: Vector.S with type elem('s, 'a) := expr('s, 'a);

    let vectormk_to_vector:
      (source('s), Expr.Vector.t('s, 'a, 'n)) => Vector.t('s, 'a, 'n);
  };

  let order_by: (OrderBy.Expr.Vector.t('s, 'a, Nat.s('n)), t('s)) => t('s);
  let limit: (~offset: int=?, int, t('a)) => t('a);
};

module Make = (D: Driver.S) : S => {
  type join_steps('s1, 't1, 't2, 's2) =
    | There: join_steps('t2 => _ as 's1, 't1, 't2, 't1 => 's1)
    | Skip(join_steps('s1, 't1, 't2, 's2))
      : join_steps('a => 's1, 't1, 't2, 'a => 's2);

  type join =
    | Left
    | Right
    | Inner;

  type source('s) =
    | From(Table.t('t)): source('t => unit)
    | Join(
        join,
        Field.foreign_key('t1, 't2),
        source('s1),
        join_steps('s1, 't1, 't2, 's2),
      )
      : source('s2);

  type steps('s1, 't1, 't2, 's2) =
    | There: steps('t2 => _ as 's1, 't1, 't2, 's1)
    | Skip(steps('s1, 't1, 't2, 's2))
      : steps('a => 's1, 't1, 't2, 'a => 's2);

  module OrderBy = {
    type order =
      | Asc
      | Desc;

    type expr('s, 'a) = (Expr.t('a), order);

    module Expr = {
      type mk('s, 'a) = source('s) => (Expr.t('a), order);

      module Vector =
        Vector.Make({
          type elem('s, 'a) = mk('s, 'a);
        });

      let asc = (f, src) => (f(src), Asc);
      let desc = (f, src) => (f(src), Desc);
    };

    module Vector =
      Vector.Make({
        type elem('s, 'a) = expr('s, 'a);
      });

    let rec vectormk_to_vector:
      type a s n. (source(s), Expr.Vector.t(s, a, n)) => Vector.t(s, a, n) =
      (src, vec) =>
        Expr.Vector.(
          switch (vec) {
          | [] => Vector.[]
          | [f, ...fs] => Vector.[f(src), ...vectormk_to_vector(src, fs)]
          }
        );
  };

  module rec SelectExpr: {
    type select('s) = T.t('s);

    type Expr.t('a) +=
      | As(Expr.t('a), string): Expr.t('a)
      | Alias(string): Expr.t('a)
      | Field(Field.t('t, 'a), source('s1), steps('s1, 't1, 't, 's2)): Expr.t(
                                                                    'a,
                                                                    )
      | Foreign(
                 Field.foreign_key('t, 't2),
                 source('s1),
                 steps('s1, 't1, 't, 's2),
               ): Expr.t('a)
      | Select(T.t('s)): Expr.t('a);

    type t('a) = Expr.t('a);

    let build: (~handover: Expr.handover, build_step, t('a)) => build_step;

    let (-->):
      (source('s) => Expr.t('a), string, source('s)) => Expr.t('a);
    let alias: (string, source('s)) => Expr.t('a);
    let field:
      (Field.t('t, 'a), steps('b, 'c, 't, 'd), source('b)) => t('a);
    let foreign_key:
      (Field.foreign_key('t1, 't2), steps('a, 'b, 't1, 'c), source('a)) =>
      t('d);

    let subquery: (T.t('s), source('t)) => t('c);

    let unwrap:
      (Field.t('t, option('a)), steps('b, 'c, 't, 'd), source('b)) =>
      t('a);

    type mk('s, 'a) = source('s) => t('a);

    module Vector: Vector.S with type elem('s, 'a) := mk('s, 'a);

    let vectormk_to_vector:
      (source('s), Vector.t('s, 'a, 'n)) => Expr.Vector.t('s, 'a, 'n);
  } = {
    type select('s) = T.t('s);

    type Expr.t('a) +=
      | As(Expr.t('a), string): Expr.t('a)
      | Alias(string): Expr.t('a)
      | Field(Field.t('t, 'a), source('s1), steps('s1, 't1, 't, 's2)): Expr.t(
                                                                    'a,
                                                                    )
      | Foreign(
                 Field.foreign_key('t, 't2),
                 source('s1),
                 steps('s1, 't1, 't, 's2),
               ): Expr.t('a)
      | Select(T.t('s)): Expr.t('a);

    type t('a) = Expr.t('a);

    let (-->) = (expr, alias, src) =>
      [@implicit_arity] As(expr(src), alias);
    let alias = (name, src) => Alias(name);
    let field = (fld, steps, src) =>
      [@implicit_arity] Field(fld, src, steps);
    let foreign_key = (fk, steps, src) =>
      [@implicit_arity] Foreign(fk, src, steps);
    let subquery = (sel, src) => Select(sel);

    /* XXX is there a better name for this? */
    let unwrap:
      type a.
        (Field.t('t, option(a)), steps('b, 'c, 't, 'd), source('b)) =>
        Expr.t(a) =
      (fld, steps, src) =>
        switch (fld) {
        | [@implicit_arity] Field.Null.Bool(n, t) =>
          [@implicit_arity]
          Field([@implicit_arity] Field.Bool(n, t), src, steps)
        | [@implicit_arity] Field.Null.Int(n, t) =>
          [@implicit_arity]
          Field([@implicit_arity] Field.Int(n, t), src, steps)
        | [@implicit_arity] Field.Null.Float(n, t) =>
          [@implicit_arity]
          Field([@implicit_arity] Field.Float(n, t), src, steps)
        | [@implicit_arity] Field.Null.String(n, t) =>
          [@implicit_arity]
          Field([@implicit_arity] Field.String(n, t), src, steps)
        | [@implicit_arity] Field.Null.Blob(n, t) =>
          [@implicit_arity]
          Field([@implicit_arity] Field.Blob(n, t), src, steps)
        | _ => assert(false)
        };

    let rec build:
      type a. (~handover: Expr.handover, build_step, Expr.t(a)) => build_step =
      (~handover, st, e) =>
        switch (e) {
        | [@implicit_arity] As(expr, alias) =>
          let st = build(~handover, st, expr);
          {
            ...st,
            repr: sprintf("%s AS %s", st.repr, alias),
            aliases: AliasSet.add(alias, st.aliases),
          };
        | Alias(name) =>
          if (AliasSet.mem(name, st.aliases)) {
            {...st, repr: name, params: []};
          } else {
            failwith(sprintf("unknown alias '%s'", name));
          }
        | [@implicit_arity] Field(Field.Bool(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | [@implicit_arity] Field(Field.Int(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | [@implicit_arity] Field(Field.Float(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | [@implicit_arity] Field(Field.String(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | [@implicit_arity] Field(Field.Blob(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | [@implicit_arity] Field(Field.Null.Bool(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | [@implicit_arity] Field(Field.Null.Int(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | [@implicit_arity] Field(Field.Null.Float(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | [@implicit_arity] Field(Field.Null.String(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | [@implicit_arity] Field(Field.Null.Blob(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | [@implicit_arity] Foreign((fld, _), _, _) => {
            ...st,
            repr: Field.to_string(fld),
            params: [],
          }
        | Select(s) =>
          let (repr, params) = T.seal(~handover, s);
          {...st, repr: "(" ++ repr ++ ")", params};
        | e => Expr.build(~placeholder=D.placeholder, ~handover, st, e)
        };

    type mk('s, 'a) = source('s) => Expr.t('a);

    module Vector =
      Vector.Make({
        type elem('s, 'a) = mk('s, 'a);
      });

    let rec vectormk_to_vector:
      type a s n. (source(s), Vector.t(s, a, n)) => Expr.Vector.t(s, a, n) =
      (src, vec) =>
        Vector.(
          switch (vec) {
          | [] => Expr.Vector.[]
          | [f, ...fs] =>
            Expr.Vector.[f(src), ...vectormk_to_vector(src, fs)]
          }
        );
  }

  and T: {
    type t(_) =
      | S({
          source: source('s),
          distinct: bool,
          select: Expr.Vector.t(_, _, Nat.s('n)),
          where: option(Expr.t(_)),
          group_by:
            option(
              (Expr.Vector.t(_, _, Nat.s('m)), option(Expr.t(bool))),
            ),
          order_by: option(OrderBy.Vector.t(_, _, Nat.s('k))),
          limit: option((int, int)),
        })
        : t('s);

    let seal: (~handover: Expr.handover, t('s)) => (string, list(Param.t));
  } = {
    module E = Expr;
    module Expr = SelectExpr;

    type t(_) =
      | S({
          source: source('s),
          distinct: bool,
          select: E.Vector.t(_, _, Nat.s('n)),
          where: option(Expr.t(_)),
          group_by:
            option((E.Vector.t(_, _, Nat.s('m)), option(Expr.t(bool)))),
          order_by: option(OrderBy.Vector.t(_, _, Nat.s('k))),
          limit: option((int, int)),
        })
        : t('s);

    let join_to_string =
      fun
      | Left => "LEFT"
      | Right => "RIGHT"
      | Inner => "INNER";

    let join_exprs = (~handover, st) =>
      E.Vector.vector_fold_left(
        {
          E.Vector.f: ((st, i), e) => {
            let st' = Expr.build(~handover, st, e);
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

    let build_select = (~handover, st, exprs) =>
      fst(join_exprs(~handover, st, exprs));

    let rec build_source:
      type s.
        (
          ~distinct: bool=?,
          ~handover: E.handover,
          build_step,
          E.Vector.t('a, _, 'n),
          source(s)
        ) =>
        build_step =
      (~distinct=false, ~handover, st, exprs) =>
        fun
        | From(t) => {
            let st = build_select(~handover, st, exprs);
            let repr =
              sprintf(
                "SELECT%s%s\nFROM %s",
                if (distinct) {" DISTINCT "} else {" "},
                st.repr,
                Table.name(t),
              );
            {...st, repr, params: st.params, pos: st.pos};
          }
        | [@implicit_arity] Join(join, (a, b), src, _) => {
            let st = build_source(~distinct, ~handover, st, exprs, src);
            let repr =
              st.repr
              ++ "\n"
              ++ sprintf(
                   "%s JOIN %s ON %s = %s",
                   join_to_string(join),
                   Table.name(Field.table(a)),
                   Field.to_string(a),
                   Field.to_string(b),
                 );
            {...st, repr, params: st.params, pos: st.pos};
          };

    let build_group_by:
      type t a n.
        (
          ~handover: E.handover,
          build_step,
          option((E.Vector.t(t, a, Nat.s(n)), option(Expr.t(bool))))
        ) =>
        build_step =
      (~handover, st) =>
        fun
        | Some((flds, having)) => {
            let st = fst(join_exprs(~handover, st, flds));
            switch (having) {
            | Some(expr) =>
              let st' = Expr.build(~handover, st, expr);
              let repr =
                sprintf("GROUP BY (%s) HAVING (%s)", st.repr, st'.repr);
              {...st', repr};
            | None => {...st, repr: sprintf("GROUP BY (%s)", st.repr)}
            };
          }
        | None => {...blank_step, pos: st.pos, aliases: st.aliases};

    let order_to_string =
      fun
      | OrderBy.Asc => "ASC"
      | OrderBy.Desc => "DESC";

    let join_order_by_exprs = (~handover, st) =>
      OrderBy.Vector.vector_fold_left(
        {
          OrderBy.Vector.f: ((st, i), (e, ord)) => {
            let st' = Expr.build(~handover, st, e);
            let st' = {
              ...st',
              repr: st'.repr ++ " " ++ order_to_string(ord),
            };
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
          {...st, repr: sprintf("ORDER BY %s", st.repr)};
        }
      | None => {...blank_step, pos: st.pos, aliases: st.aliases};

    open Query_common;

    let seal:
      type s. (~handover: E.handover, t(s)) => (string, list(Param.t)) =
      (~handover, S(stmt)) => {
        let st = {...blank_step, pos: 1};
        let src_st =
          build_source(
            ~distinct=stmt.distinct,
            ~handover,
            st,
            stmt.select,
            stmt.source,
          );
        let where_st =
          build_where(
            ~placeholder=D.placeholder,
            ~handover,
            src_st,
            stmt.where,
          );
        let group_by_st = build_group_by(~handover, where_st, stmt.group_by);
        let order_by_st =
          build_order_by(~handover, group_by_st, stmt.order_by);
        let limit_st = build_limit(D.placeholder, order_by_st, stmt.limit);
        let params =
          src_st.params
          @ where_st.params
          @ group_by_st.params
          @ order_by_st.params
          @ limit_st.params;
        let repr =
          join_lines([
            src_st.repr,
            where_st.repr,
            group_by_st.repr,
            order_by_st.repr,
            limit_st.repr,
          ]);
        (repr, params);
      };
  };

  include T;
  module Expr = SelectExpr;

  let select:
    type s a.
      (~distinct: bool=?, Expr.Vector.t(s, a, Nat.s('n)), source(s)) =>
      t(s) =
    (~distinct=false, bl, src) =>
      S({
        source: src,
        distinct,
        select: Expr.vectormk_to_vector(src, bl),
        where: None,
        group_by: None,
        order_by: None,
        limit: None,
      });

  let from = t => From(t);

  let join = (kind, rel, steps, src) =>
    [@implicit_arity] Join(kind, rel, src, steps);

  let left_join = ((rel, steps), src) => join(Left, rel, steps, src);
  let right_join = ((rel, steps), src) => join(Right, rel, steps, src);
  let inner_join = ((rel, steps), src) => join(Inner, rel, steps, src);

  let self = (fld1, fld2, steps) => ((fld1, fld2), steps);
  let this = (rel, steps) => (rel, steps);
  let that = ((fk, pk), steps) => ((pk, fk), steps);

  let where = (expr, S(stmt)) =>
    S({...stmt, where: Some(expr(stmt.source))});

  let group_by:
    type a s n.
      (
        ~having: source(s) => Expr.t(bool)=?,
        Expr.Vector.t(s, a, Nat.s(n)),
        t(s)
      ) =>
      t(s) =
    (~having=?, flds, S(stmt)) => {
      let flds = Expr.vectormk_to_vector(stmt.source, flds);
      let having =
        switch (having) {
        | Some(f) => Some(f(stmt.source))
        | None => None
        };
      S({...stmt, group_by: Some((flds, having))});
    };

  let order_by:
    type a s n. (OrderBy.Expr.Vector.t(s, a, Nat.s(n)), t(s)) => t(s) =
    (bl, S(stmt)) => {
      let vec = OrderBy.vectormk_to_vector(stmt.source, bl);
      S({...stmt, order_by: Some(vec)});
    };

  let limit = (~offset=0, n, S(stmt)) =>
    S({...stmt, limit: Some((offset, n))});
};
