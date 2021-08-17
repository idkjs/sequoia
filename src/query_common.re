open Printf;
open Common;

let build_where:
  type a.
    (
      ~placeholder: int => string,
      ~handover: Expr.handover,
      build_step,
      option(Expr.t(a))
    ) =>
    build_step =
  (~placeholder, ~handover, st) =>
    fun
    | Some(expr) => {
        let st = Expr.build(~placeholder, ~handover, st, expr);
        {...st, repr: sprintf("WHERE (%s)", st.repr)};
      }
    | None => {...blank_step, pos: st.pos, aliases: st.aliases};

let build_limit = (placeholder, st) =>
  fun
  | Some((0, lim)) => {
      ...st,
      repr: sprintf("LIMIT %s", placeholder(st.pos)),
      params: [Param.Int(lim)],
      pos: st.pos + 1,
    }
  | Some((off, lim)) => {
      let repr =
        sprintf(
          "LIMIT %s, %s",
          placeholder(st.pos),
          placeholder(st.pos + 1),
        );
      {
        ...st,
        repr,
        params: [Param.Int(off), Param.Int(lim)],
        pos: st.pos + 2,
      };
    }
  | None => {...blank_step, pos: st.pos, aliases: st.aliases};

module UpdateDeleteExpr = {
  type Expr.t(_) +=
    | Field(Field.t('t, 'a), Table.t('t)): Expr.t('a)
    | Foreign(Field.foreign_key('t, 'u), Table.t('t)): Expr.t('a);

  let field = (fld, table) => [@implicit_arity] Field(fld, table);
  let foreign_key = (fk, table) => [@implicit_arity] Foreign(fk, table);

  let rec build:
    type a.
      (
        ~placeholder: int => string,
        ~handover: Expr.handover,
        build_step,
        Expr.t(a)
      ) =>
      build_step =
    (~placeholder, ~handover, st, e) =>
      switch (e) {
      | [@implicit_arity] Field(Field.Bool(_) as fld, _) => {
          ...st,
          repr: Field.to_string(fld),
          pos: st.pos,
        }
      | [@implicit_arity] Field(Field.Int(_) as fld, _) => {
          ...st,
          repr: Field.to_string(fld),
          pos: st.pos,
        }
      | [@implicit_arity] Field(Field.Float(_) as fld, _) => {
          ...st,
          repr: Field.to_string(fld),
          pos: st.pos,
        }
      | [@implicit_arity] Field(Field.String(_) as fld, _) => {
          ...st,
          repr: Field.to_string(fld),
          pos: st.pos,
        }
      | [@implicit_arity] Field(Field.Blob(_) as fld, _) => {
          ...st,
          repr: Field.to_string(fld),
          pos: st.pos,
        }
      | [@implicit_arity] Foreign((fld, _), _) => {
          ...st,
          repr: Field.to_string(fld),
          pos: st.pos,
        }
      | e => Expr.build(~placeholder, ~handover, st, e)
      };
};

module InsertReplace = {
  module Vector =
    Vector.Make({
      type elem('t, 'a) = Field.t('t, 'a);
    });

  type t(_) =
    | I({
        table: Table.t('t),
        fields: Vector.t('t, 'a, Nat.s('n)),
        values: Lit.Vector.matrix('u, 'a, Nat.s('m), Nat.s('n)),
      })
      : t('t);

  let create = (~into, ~fields, ~values) => I({table: into, fields, values});

  let rec join_fields: type t a n. Vector.t(t, a, n) => string =
    flds =>
      Vector.(
        switch (flds) {
        | [] => assert(false)
        | [f] => Field.name(f)
        | [f, ...fs] => Field.name(f) ++ ", " ++ join_fields(fs)
        }
      );

  let expr_placeholders = (~placeholder, i, vs) => {
    open Lit;
    let rec eps: type t a n. (int, Lit.Vector.t(t, a, n)) => list(string) =
      (i, exprs) =>
        Lit.Vector.(
          switch (exprs) {
          | [] => []
          | [_, ...es] => [placeholder(i), ...eps(i + 1, es)]
          }
        );
    eps(i, vs);
  };

  let placeholders:
    type a m.
      (~placeholder: int => string, Lit.Vector.matrix('v, a, m, 'n)) => string =
    (~placeholder, values) => {
      let rec pss: type a m. (int, Lit.Vector.matrix('v, a, m, 'n)) => string =
        (i, vals) =>
          Lit.Vector.(
            switch (vals) {
            | [] => ""
            | [v] =>
              let ps = expr_placeholders(~placeholder, i, v);
              sprintf("(%s)", String.concat(", ", ps));
            | [v, ...vs] =>
              let ps = expr_placeholders(~placeholder, i, v);
              let n = i + List.length(ps);
              sprintf("(%s)\n%s", String.concat(", ", ps), pss(n, vs));
            }
          );
      pss(1, values);
    };

  let params_of_values:
    type a. Lit.Vector.matrix(a, 'b, 'm, 'n) => list(Param.t) =
    values =>
      List.rev @@
      Lit.Vector.matrix_fold_left(
        {Lit.Vector.f: (acc, e) => [Lit.to_param(e), ...acc]},
        [],
        values,
      );

  let seal = (~placeholder, ~query, I({table, fields, values})) => {
    let s =
      sprintf(
        "%s INTO %s (%s) VALUES\n%s",
        query,
        Table.name(table),
        join_fields(fields),
        placeholders(~placeholder, values),
      );
    (s, params_of_values(values));
  };
};
