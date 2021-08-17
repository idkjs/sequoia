/** Sequoia SQLite driver */;

open Printf;
open Sequoia.Common;

module D = {
  let placeholder = _ => "?";
};
module M = Sequoia.Make(D);

include (
          M:
             (module type of M) with
              module Lit := M.Lit and
              module Expr := M.Expr and
              module Select := M.Select and
              module Update := M.Update and
              module Delete := M.Delete and
              module Field := M.Field
        );

module Expr = {
  include M.Expr;
  include Base;

  type t('a) +=
    /* Core functions */
    | Abs(t(int)): t(int)
    | Changes: t(int)
    | Char(list(t(int))): t(string)
    | Coalesce(list(t('a))): t('a)
    | Glob(string, t(string)): t(bool)
    | Hex(t(bytes)): t(string)
    | Ifnull(t('a), t('a)): t('a)
    | Instr(t(string), string): t(int)
    | Length(t(string)): t(int)
    | Likelihood(t(bool), float): t(bool)
    | Likely(t(bool)): t(bool)
    | Lower(t(string)): t(string)
    | Ltrim(t(string), option(string)): t(string)
    | Max(list(t('a))): t('a)
    | Min(list(t('a))): t('a)
    | Nullif(t('a), t('a)): t('a)
    | Random: t(int)
    | Randomblob(t(int)): t(bytes)
    | Replace(t(string), string, string): t(string)
    | Round(t(float), option(int)): t(float)
    | Rtrim(t(string), option(string)): t(string)
    | Substr(t(string), int, option(int)): t(string)
    | Trim(t(string), option(string)): t(string)
    | Typeof(t('a)): t(string)
    | Unicode(t(string)): t(int)
    | Unlikely(t(bool)): t(bool)
    | Upper(t(string)): t(string)
    | Zeroblob(t(int)): t(bytes)
    /* Date and time functions */
    | Date(t(string), list(string)): t(string)
    | Time(t(string), list(string)): t(string)
    | Julianday(t(string), list(string)): t(string)
    | Strftime(string, t(string), list(string)): t(string)
    /* Aggregate functions */
    | Avg(t(float)): t(float)
    | Count(option(t('a))): t(int)
    | Group_concat(t(string), option(string)): t(string)
    | MaxAgg(t('a)): t('a)
    | MinAgg(t('a)): t('a)
    | Sum(t(int)): t(int)
    | Total(t(int)): t(int);

  let avg = (f, src) => Avg(f(src));
  let changes = ((), src) => Changes;
  let char = (f, src) => Char(f(src));
  let coalesce = (f, src) => Coalesce(f(src));
  let glob = (pat, f, src) => [@implicit_arity] Glob(pat, f(src));
  let hex = (f, src) => Hex(f(src));
  let ifnull = (f, g, src) => [@implicit_arity] Ifnull(f(src), g(src));
  let instr = (f, s, src) => [@implicit_arity] Instr(f(src), s);
  let length = (f, src) => Length(f(src));
  let likelihood = (f, p, src) => [@implicit_arity] Likelihood(f(src), p);
  let likely = (f, src) => Likely(f(src));
  let lower = (f, src) => Lower(f(src));
  let ltrim = (f, ~chars=?, src) => [@implicit_arity] Ltrim(f(src), chars);
  let max = (f, src) => Max(f(src));
  let min = (f, src) => Min(f(src));
  let nullif = (f, g, src) => [@implicit_arity] Nullif(f(src), g(src));
  let random = ((), src) => Random;
  let randomblob = (f, src) => Randomblob(f(src));
  let replace = (f, s1, s2, src) =>
    [@implicit_arity] Replace(f(src), s1, s2);
  let round = (f, ~digits=?, src) =>
    [@implicit_arity] Round(f(src), digits);
  let rtrim = (f, ~chars=?, src) => [@implicit_arity] Rtrim(f(src), chars);
  let substr = (f, pos, ~len=?, src) =>
    [@implicit_arity] Substr(f(src), pos, len);
  let trim = (f, ~chars=?, src) => [@implicit_arity] Trim(f(src), chars);
  let typeof = (f, src) => Typeof(f(src));
  let unicode = (f, src) => Unicode(f(src));
  let unlikely = (f, src) => Unlikely(f(src));
  let upper = (f, src) => Upper(f(src));
  let zeroblob = (f, src) => Zeroblob(f(src));
  let date = (f, l, src) => [@implicit_arity] Date(f(src), l);
  let time = (f, l, src) => [@implicit_arity] Time(f(src), l);
  let julianday = (f, l, src) => [@implicit_arity] Julianday(f(src), l);
  let strftime = (fmt, f, l, src) =>
    [@implicit_arity] Strftime(fmt, f(src), l);
  let avg = (f, src) => Avg(f(src));
  let count = (f, src) => Count(f(src));
  let group_concat = (f, ~sep=?, src) =>
    [@implicit_arity] Group_concat(f(src), sep);
  let max_agg = (f, src) => MaxAgg(f(src));
  let min_agg = (f, src) => MinAgg(f(src));
  let sum = (f, src) => Sum(f(src));
  let total = (f, src) => Total(f(src));

  let rec build: type a. (~handover: handover, build_step, t(a)) => build_step =
    (~handover, st, e) => {
      let fn = (~st=st) =>
        M.Expr.build_function(~placeholder=D.placeholder, ~handover, st);
      switch (e) {
      /* Core functions */
      | Abs(e) => fn("ABS(", [e], ")")
      | Changes => fn("CHANGES(", [], ")")
      | Char(l) => fn("CHAR(", l, ")")
      | Coalesce(l) => fn("COALESCE(", l, ")")
      | [@implicit_arity] Glob(pat, e) =>
        fn(sprintf("GLOB(%s, ", pat), [e], ")")
      | Hex(e) => fn("HEX(", [e], ")")
      | [@implicit_arity] Ifnull(e1, e2) => fn("IFNULL(", [e1, e2], ")")
      | [@implicit_arity] Instr(e, s) =>
        fn("INSTR(", [e], sprintf(", %s)", s))
      | Length(e) => fn("LENGTH(", [e], ")")
      | [@implicit_arity] Likelihood(e, p) =>
        fn("LIKELYHOOD(", [e], sprintf(", %f)", p))
      | Likely(e) => fn("LIKELY(", [e], ")")
      | Lower(e) => fn("LOWER(", [e], ")")
      | [@implicit_arity] Ltrim(e, Some(s)) =>
        fn("LTRIM(", [e], sprintf(", %s)", s))
      | [@implicit_arity] Ltrim(e, None) => fn("LTRIM(", [e], ")")
      | Max(l) => fn("MAX(", l, ")")
      | Min(l) => fn("MIN(", l, ")")
      | [@implicit_arity] Nullif(e1, e2) => fn("NULLIF(", [e1, e2], ")")
      | Random => fn("RANDOM(", [], ")")
      | Randomblob(e) => fn("RANDOMBLOB(", [e], ")")
      | [@implicit_arity] Replace(e, s1, s2) =>
        fn("REPLACE(", [e], sprintf(", %s, %s)", s1, s2))
      | [@implicit_arity] Round(e, Some(i)) =>
        fn("ROUND(", [e], sprintf(", %d)", i))
      | [@implicit_arity] Round(e, None) => fn("ROUND(", [e], ")")
      | [@implicit_arity] Rtrim(e, Some(s)) =>
        fn("RTRIM(", [e], sprintf(", %s)", s))
      | [@implicit_arity] Rtrim(e, None) => fn("RTRIM(", [e], ")")
      | [@implicit_arity] Substr(e, pos, Some(len)) =>
        fn("SUBSTR(", [e], sprintf(", %d, %d)", pos, len))
      | [@implicit_arity] Substr(e, pos, None) =>
        fn("SUBSTR(", [e], sprintf(", %d)", pos))
      | [@implicit_arity] Trim(e, Some(s)) =>
        fn("TRIM(", [e], sprintf(", %s)", s))
      | [@implicit_arity] Trim(e, None) => fn("TRIM(", [e], ")")
      | Typeof(e) => fn("TYPEOF(", [e], ")")
      | Unicode(e) => fn("UNICODE(", [e], ")")
      | Unlikely(e) => fn("UNLIKELY(", [e], ")")
      | Upper(e) => fn("UPPER(", [e], ")")
      | Zeroblob(e) => fn("ZEROBLOB(", [e], ")")
      /* Date and time functions */
      | [@implicit_arity] Date(e, l) =>
        fn("DATE(", [e], sprintf("%s)", String.concat(", ", l)))
      | [@implicit_arity] Time(e, l) =>
        fn("TIME(", [e], sprintf("%s)", String.concat(", ", l)))
      | [@implicit_arity] Julianday(e, l) =>
        fn("JULIANDAY(", [e], sprintf("%s)", String.concat(", ", l)))
      | [@implicit_arity] Strftime(fmt, e, l) =>
        fn(
          sprintf("STRFTIME(%s, ", fmt),
          [e],
          sprintf("%s)", String.concat(", ", l)),
        )
      /* Aggregate functions */
      | Avg(e) => fn("AVG(", [e], ")")
      | Count(None) => fn("COUNT(*", [], ")")
      | Count(Some(e)) => fn("COUNT(", [e], ")")
      | [@implicit_arity] Group_concat(e, Some(sep)) =>
        fn("GROUP_CONCAT(", [e], sprintf(", %s)", sep))
      | [@implicit_arity] Group_concat(e, None) =>
        fn("GROUP_CONCAT(", [e], ")")
      | MaxAgg(e) => fn("MAX(", [e], ")")
      | MinAgg(e) => fn("MIN(", [e], ")")
      | Sum(e) => fn("SUM(", [e], ")")
      | Total(e) => fn("TOTAL(", [e], ")")
      /* Handover to other expressions */
      | e => handover.expr(st, e)
      };
    };
};

module Select = {
  let expr_build = (st, e) => Expr.build(st, e);
  let cast_handover = Expr.string_of_cast;

  include M.Select;

  let seal = stmt => {
    let rec expr_handover: type a. (build_step, M.Expr.t(a)) => build_step =
      (st, e) => {
        let build = (st, e) =>
          Expr.build(
            ~handover={M.Expr.expr: expr_handover, cast: cast_handover},
            st,
            e,
          );
        expr_build(
          ~handover={M.Expr.expr: build, cast: cast_handover},
          st,
          e,
        );
      };
    seal(~handover={M.Expr.expr: expr_handover, cast: cast_handover}, stmt);
  };
};

module Update = {
  let expr_build = (st, e) => Expr.build(st, e);
  let cast_handover = Expr.string_of_cast;

  include M.Update;

  let seal = stmt => {
    let rec expr_handover: type a. (build_step, M.Expr.t(a)) => build_step =
      (st, e) => {
        let build = (st, e) =>
          Expr.build(
            ~placeholder=D.placeholder,
            ~handover={M.Expr.expr: expr_handover, cast: cast_handover},
            st,
            e,
          );
        expr_build(
          ~handover={M.Expr.expr: build, cast: cast_handover},
          st,
          e,
        );
      };
    seal(~handover={M.Expr.expr: expr_handover, cast: cast_handover}, stmt);
  };
};

module Delete = {
  let expr_build = (st, e) => Expr.build(st, e);
  let cast_handover = Expr.string_of_cast;

  include M.Delete;

  let seal = stmt => {
    let rec expr_handover: type a. (build_step, M.Expr.t(a)) => build_step =
      (st, e) => {
        let build = (st, e) =>
          Expr.build(
            ~placeholder=D.placeholder,
            ~handover={M.Expr.expr: expr_handover, cast: cast_handover},
            st,
            e,
          );
        expr_build(
          ~handover={M.Expr.expr: build, cast: cast_handover},
          st,
          e,
        );
      };
    seal(~handover={M.Expr.expr: expr_handover, cast: cast_handover}, stmt);
  };
};
