/** Sequoia PostgreSQL driver */;

open Printf;
open Sequoia.Common;

module D = {
  let placeholder = n => Printf.sprintf("$%d", n);
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
              module Field := M.Field and
              module Param := M.Param
        );

type time_kind = [ | `Time | `Timestamp | `Date | `Datetime];
type time_fields = {
  year: int,
  month: int,
  day: int,
  hour: int,
  minute: int,
  second: int,
};
type time('k) = time_fields constraint 'k = [< time_kind];

let base_time = {year: 0, month: 0, day: 0, hour: 0, minute: 0, second: 0};

module Enum = {
  module type Instance = {
    type t;
    let to_string: string;
  };

  module type S = {
    type t;
    let instance: t => (module Instance);
  };
};

module Field = {
  include (
            M.Field:  (module type of M.Field) with module Null := M.Field.Null
          );

  type t('t, 'a) +=
    | Time(string, M.Table.t('t)): t('t, time([ | `Time]))
    | Timestamp(string, M.Table.t('t)): t('t, time([ | `Timestamp]))
    | Date(string, M.Table.t('t)): t('t, time([ | `Date]))
    | Datetime(string, M.Table.t('t)): t('t, time([ | `Datetime]))
    | Enum(string, M.Table.t('t), (module Enum.Instance)): t(
                                                               't,
                                                               module Enum.Instance,
                                                             );

  module Null = {
    include M.Field.Null;

    type t('t, 'a) +=
      | Time(string, M.Table.t('t)): t('t, option(time([ | `Time])))
      | Timestamp(string, M.Table.t('t)): t(
                                              't,
                                              option(time([ | `Timestamp])),
                                            )
      | Date(string, M.Table.t('t)): t('t, option(time([ | `Date])))
      | Datetime(string, M.Table.t('t)): t(
                                             't,
                                             option(time([ | `Datetime])),
                                           )
      | Enum(string, M.Table.t('t), (module Enum.Instance)): t(
                                                                 't,
                                                                 option(
                                                                   module Enum.Instance,
                                                                 ),
                                                               );

    let time = (table, name) => [@implicit_arity] Time(name, table);
    let timestamp = (table, name) =>
      [@implicit_arity] Timestamp(name, table);
    let date = (table, name) => [@implicit_arity] Date(name, table);
    let datetime = (table, name) => [@implicit_arity] Datetime(name, table);

    let enum = (table, module E: Enum.S, name) =>
      [@implicit_arity]
      Enum(
        name,
        table,
        (module
         {
           type t = E.t;
           let to_string = "";
         } /* dummy */): (module Enum.Instance),
      );
  };

  let name: type a. t('t, a) => string =
    fun
    | [@implicit_arity] Time(n, _) => n
    | [@implicit_arity] Timestamp(n, _) => n
    | [@implicit_arity] Date(n, _) => n
    | [@implicit_arity] Datetime(n, _) => n
    | [@implicit_arity] Enum(n, _, _) => n
    | [@implicit_arity] Null.Time(n, _) => n
    | [@implicit_arity] Null.Timestamp(n, _) => n
    | [@implicit_arity] Null.Date(n, _) => n
    | [@implicit_arity] Null.Datetime(n, _) => n
    | [@implicit_arity] Null.Enum(n, _, _) => n
    | fld => name(fld);

  let table: type u a. t(u, a) => Table.t(u) =
    fun
    | [@implicit_arity] Time(_, t) => t
    | [@implicit_arity] Timestamp(_, t) => t
    | [@implicit_arity] Date(_, t) => t
    | [@implicit_arity] Datetime(_, t) => t
    | [@implicit_arity] Enum(_, t, _) => t
    | [@implicit_arity] Null.Time(_, t) => t
    | [@implicit_arity] Null.Timestamp(_, t) => t
    | [@implicit_arity] Null.Date(_, t) => t
    | [@implicit_arity] Null.Datetime(_, t) => t
    | [@implicit_arity] Null.Enum(_, t, _) => t
    | fld => table(fld);

  let to_string: type a b. t(a, b) => string =
    fun
    | [@implicit_arity] Time(name, table) =>
      sprintf("%s.%s", Table.name(table), name)
    | [@implicit_arity] Timestamp(name, table) =>
      sprintf("%s.%s", Table.name(table), name)
    | [@implicit_arity] Date(name, table) =>
      sprintf("%s.%s", Table.name(table), name)
    | [@implicit_arity] Datetime(name, table) =>
      sprintf("%s.%s", Table.name(table), name)
    | [@implicit_arity] Enum(name, table, _) =>
      sprintf("%s.%s", Table.name(table), name)
    | [@implicit_arity] Null.Time(name, table) =>
      sprintf("%s.%s", Table.name(table), name)
    | [@implicit_arity] Null.Timestamp(name, table) =>
      sprintf("%s.%s", Table.name(table), name)
    | [@implicit_arity] Null.Date(name, table) =>
      sprintf("%s.%s", Table.name(table), name)
    | [@implicit_arity] Null.Datetime(name, table) =>
      sprintf("%s.%s", Table.name(table), name)
    | [@implicit_arity] Null.Enum(name, table, _) =>
      sprintf("%s.%s", Table.name(table), name)
    | other => to_string(other);

  let to_string = fld => {
    let t = table(fld);
    sprintf("%s.%s", Table.name(t), name(fld));
  };

  let time = (table, name) => [@implicit_arity] Time(name, table);
  let timestamp = (table, name) => [@implicit_arity] Timestamp(name, table);
  let date = (table, name) => [@implicit_arity] Date(name, table);
  let datetime = (table, name) => [@implicit_arity] Datetime(name, table);

  let enum = (table, module E: Enum.S, name) =>
    [@implicit_arity]
    Enum(
      name,
      table,
      (module
       {
         type t = E.t;
         let to_string = "";
       } /* dummy */): (module Enum.Instance),
    );
};

module type POSTGRESQL_NULL_FIELD = {
  include NULL_FIELD;

  let time: string => t(option(time([ | `Time])));
  let timestamp: string => t(option(time([ | `Timestamp])));
  let date: string => t(option(time([ | `Date])));
  let datetime: string => t(option(time([ | `Datetime])));
  let enum: ((module Enum.S), string) => t(option(module Enum.Instance));
};

module type POSTGRESQL_FIELD = {
  include FIELD;

  let time: string => t(time([ | `Time]));
  let timestamp: string => t(time([ | `Timestamp]));
  let date: string => t(time([ | `Date]));
  let datetime: string => t(time([ | `Datetime]));
  let enum: ((module Enum.S), string) => t(module Enum.Instance);
};

module type POSTGRESQL_TABLE = {
  type t;
  let table: Table.t(t);

  module Field: {
    include POSTGRESQL_FIELD with type table = t;
    module Null: POSTGRESQL_NULL_FIELD with type t('a) := t('a);
  };
};

module MakePostgresqlTable = (T: Sequoia.NAMED) : POSTGRESQL_TABLE => {
  module Base = MakeTable(T);
  include (Base: TABLE with type t := Base.t and module Field := Base.Field);
  type t = Base.t;

  module Field = {
    include (
              Base.Field:
                 (module type of Base.Field) with
                  module Null := Base.Field.Null
            );

    let time = Field.time(table);
    let timestamp = Field.timestamp(table);
    let date = Field.date(table);
    let datetime = Field.datetime(table);
    let enum = Field.enum(table);

    module Null = {
      include Base.Field.Null;

      let time = Field.Null.time(table);
      let timestamp = Field.Null.timestamp(table);
      let date = Field.Null.date(table);
      let datetime = Field.Null.datetime(table);
      let enum = Field.Null.enum(table);
    };
  };
};

let table = (name): (module POSTGRESQL_TABLE) =>
  (module
   {
     include MakePostgresqlTable({
       type t;
       let name = name;
     });
   });

type time_unit =
  | Microseconds
  | Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
  | Months
  | Quarters
  | Years;

let string_of_time_unit =
  fun
  | Microseconds => "MICROSECOND"
  | Seconds => "SECOND"
  | Minutes => "MINUTE"
  | Hours => "HOUR"
  | Days => "DAY"
  | Weeks => "WEEK"
  | Months => "MONTH"
  | Quarters => "QUARTER"
  | Years => "YEAR";

module Param = {
  include M.Param;

  type t +=
    | Time(time([ | `Time]))
    | Timestamp(time([ | `Timestamp]))
    | Date(time([ | `Date]))
    | Datetime(time([ | `Datetime]))
    | Enum((module Enum.Instance));
};

module Lit = {
  include M.Lit;

  type t('a) +=
    | Time(time([ | `Time])): t(time([ | `Time]))
    | Timestamp(time([ | `Timestamp])): t(time([ | `Timestamp]))
    | Date(time([ | `Date])): t(time([ | `Date]))
    | Datetime(time([ | `Datetime])): t(time([ | `Datetime]))
    | Enum((module Enum.Instance)): t(module Enum.Instance);

  let to_param: type a. t(a) => Param.t =
    fun
    | Time(t) => Param.Time(t)
    | Timestamp(t) => Param.Timestamp(t)
    | Date(d) => Param.Date(d)
    | Datetime(d) => Param.Datetime(d)
    | Enum(e) => Param.Enum(e)
    | lit => to_param(lit);

  let build: type a. (build_step, t(a)) => build_step =
    (st, lit) => build_param(D.placeholder, st, to_param(lit));
};

module Expr = {
  include M.Expr;
  include Base;

  type cast('a) +=
    | Time: cast(time([ | `Time]))
    | Timestamp: cast(time([ | `Timestamp]))
    | Date: cast(time([ | `Date]))
    | Datetime: cast(time([ | `Datetime]));

  let string_of_cast: type a. cast(a) => string =
    fun
    | Time => "TIME"
    | Timestamp => "TIMESTAMP"
    | Date => "DATE"
    | Datetime => "DATETIME"
    | c => string_of_cast(c);

  type t('a) +=
    | Abs(t(int)): t(int)
    | Acos(t(float)): t(float)
    | Ascii(t(string)): t(string)
    | Asin(t(float)): t(float)
    | Atan(t(float)): t(float)
    | Atan2(t(float), t(float)): t(float)
    | Avg(t(float)): t(float)
    | Between(t('a), t('a)): t(bool)
    | Bin(t(int)): t(string)
    | Bit_and(t(int)): t(int)
    | Bit_count(t(int)): t(int)
    | Bit_length(t(string)): t(int)
    | Bit_or(t(int)): t(int)
    | Bit_xor(t(int)): t(int)
    | Ceil(t(float)): t(int)
    | Char(list(t(int))): t(string)
    | Char_length(t(string)): t(int)
    | Charset(t(string)): t(string)
    | Coalesce(list(t('a))): t('a)
    | Collation(t(string)): t(string)
    | Compress(t(string)): t(string)
    | Concat(list(t(string))): t(string)
    | Concat_ws(string, list(t(string))): t(string)
    | Connection_id: t(int)
    | Conv(t(int), int, int): t(string)
    | Cos(t(float)): t(float)
    | Cot(t(float)): t(float)
    | Count(t('a), bool): t(int)
    | Crc32(t(string)): t(int)
    | Current_date: t(time([ | `Date]))
    | Current_time: t(time([ | `Time]))
    | Current_timestamp: t(time([ | `Timestamp]))
    | Current_user: t(string)
    | Database: t(string)
    | DateFn(t(time([< | `Date | `Datetime]))): t(time([ | `Date]))
    | Date_add(t(time('k)), int, time_unit): t('k)
    | Date_format(t(time('k)), string): t(string)
    | Date_sub(t(time('k)), int, time_unit): t(time('k))
    | Datediff(t(time('k)), t(time('k))): t(int)
    | Dayname(t('k)): t(string)
    | Dayofmonth(t('k)): t(int)
    | Dayofweek(t('k)): t(int)
    | Dayofyear(t('k)): t(int)
    | Degrees(t(float)): t(int)
    | Expr(t(float)): t(float)
    | Floor(t(float)): t(int)
    | From_days(t(int)): t(time([ | `Date]))
    | From_unixtime(t(int)): t(time([ | `Datetime]))
    | If(t(bool), t('a), t('a)): t('a)
    | Ifnull(t(option('a)), t('a)): t('a)
    | Last_day(t(time([< | `Date | `Datetime]))): t(time([ | `Date]))
    | Length(t(string)): t(int)
    | Localtime: t(time([ | `Time]))
    | Log(t(float)): t(float)
    | Log10(t(float)): t(float)
    | Log2(t(float)): t(float)
    | Lower(t(string)): t(string)
    | Lpad(t(string)): t(string)
    | Ltrim(t(string)): t(string)
    | Max(t('a)): t('a)
    | Md5(t(string)): t(string)
    | Minute(t(string)): t(string)
    | Month(t(time('k))): t(int)
    | Monthname(t(time('k))): t(string)
    | Now: t(time([ | `Time]))
    | Nullif(t('a), t('a)): t(option('a))
    | Ord(t(string)): t(int)
    | Pow(t(int)): t(int)
    | Radians(t(float)): t(float)
    | Rand(option(t(int))): t(float)
    | Repeat(t(string), int): t(string)
    | Replace(t(string), string, string): t(string)
    | Reverse(t(string)): t(string)
    | Rlike(t('a), string): t(bool)
    | Round(t(float)): t(int)
    | Rpad(t(string)): t(string)
    | Rtrim(t(string)): t(string)
    | Sec_to_time(t(int)): t(string)
    | Second(t(string)): t(int)
    | Sha1(t(string)): t(string)
    | Sha2(t(string)): t(string)
    | Sign(t(int)): t(int)
    | Sin(t(float)): t(float)
    | Sqrt(t(float)): t(float)
    | Stddev(t(float)): t(float)
    | Substring(t(string), int, option(int)): t(string)
    | Substring_index(t(string), string, int): t(string)
    | Sum(t(int), bool): t(int)
    | Tan(t(float)): t(float)
    | TimeFn(t(time([< | `Time | `Datetime]))): t(time([ | `Time]))
    | Timestampdiff(time_unit, t(time('k)), t(time('k))): t(int)
    | Trim(t(string)): t(string)
    | Uncompress(t(string)): t(string)
    | Utc_date: t(string)
    | Utc_time: t(string)
    | Utc_timestamp: t(string)
    | Uuid: t(string)
    | Uuid_short: t(int)
    | Week: t(int)
    | Weekday: t(int)
    | Weekofyear: t(int)
    | Upper(t(string)): t(string)
    | Year: t(int);

  let time = (~hour=0, ~minute=0, ~second=0, _) =>
    Lit(Lit.Time({...base_time, hour, minute, second}));
  let timestamp = (t, _) => {
    let tm = Unix.localtime(t);
    let ts =
      Lit.Timestamp({
        year: tm.Unix.tm_year,
        month: tm.Unix.tm_mon,
        day: tm.Unix.tm_mday,
        hour: tm.Unix.tm_hour,
        minute: tm.Unix.tm_min,
        second: tm.Unix.tm_sec,
      });
    Lit(ts);
  };
  let date = (~year=0, ~month=0, ~day=0, _) =>
    Lit(Lit.Date({...base_time, year, month, day}));
  let datetime = (~year=0, ~month=0, ~day=0, ~hour=0, ~minute=0, ~second=0, _) =>
    Lit(Lit.Datetime({...base_time, year, month, day, minute, second}));

  let enum = (inst, _) => Lit(Lit.Enum(inst));

  let acos = (f, src) => Acos(f(src));
  let ascii = (f, src) => Ascii(f(src));
  let asin = (f, src) => Asin(f(src));
  let atan = (f, src) => Atan(f(src));
  let atan2 = (f, g, src) => [@implicit_arity] Atan2(f(src), g(src));
  let avg = (f, src) => Avg(f(src));
  let between = (f, g, src) => [@implicit_arity] Between(f(src), g(src));
  let bin = (f, src) => Bin(f(src));
  let bit_and = (f, src) => Bit_and(f(src));
  let bit_count = (f, src) => Bit_count(f(src));
  let bit_length = (f, src) => Bit_length(f(src));
  let bit_or = (f, src) => Bit_or(f(src));
  let bit_xor = (f, src) => Bit_xor(f(src));
  let ceil = (f, src) => Ceil(f(src));
  let char = (f, src) => Char(f(src));
  let char_length = (f, src) => Char_length(f(src));
  let charset = (f, src) => Charset(f(src));
  let coalesce = (f, src) => Coalesce(f(src));
  let collation = (f, src) => Collation(f(src));
  let compress = (f, src) => Compress(f(src));
  let concat = (f, src) => Concat(f(src));
  let concat_ws = (sep, l, src) =>
    [@implicit_arity] Concat_ws(sep, List.map(f => f(src), l));
  let connection_id = ((), src) => Connection_id;
  let conv = (f, i, j, src) => [@implicit_arity] Conv(f(src), i, j);
  let cos = (f, src) => Cos(f(src));
  let cot = (f, src) => Cot(f(src));
  let count = (~distinct=false, f, src) =>
    [@implicit_arity] Count(f(src), distinct);
  let crc32 = (f, src) => Crc32(f(src));
  let current_date = ((), src) => Current_date;
  let current_time = ((), src) => Current_time;
  let current_timestamp = ((), src) => Current_timestamp;
  let current_user = ((), src) => Current_user;
  let database = ((), src) => Database;
  let date_of:
    (
      M.Select.source('s) => t(time([< | `Date | `Datetime])),
      M.Select.source('s)
    ) =>
    t(time([ | `Date])) = (
    (f, src) => DateFn(f(src)):
      (
        M.Select.source('s) => t(time([< | `Date | `Datetime])),
        M.Select.source('s)
      ) =>
      t(time([ | `Date]))
  );
  let date_add = (f, i, u, src) => [@implicit_arity] Date_add(f(src), i, u);
  let date_format = (f, fmt, src) =>
    [@implicit_arity] Date_format(f(src), fmt);
  let date_sub = (f, i, u, src) => [@implicit_arity] Date_sub(f(src), i, u);
  let datediff = (f, g, src) => [@implicit_arity] Datediff(f(src), g(src));
  let dayname = (f, src) => Dayname(f(src));
  let dayofmonth = (f, src) => Dayofmonth(f(src));
  let dayofweek = (f, src) => Dayofweek(f(src));
  let dayofyear = (f, src) => Dayofyear(f(src));
  let degrees = (f, src) => Degrees(f(src));
  let expr = (f, src) => Expr(f(src));
  let floor = (f, src) => Floor(f(src));
  let from_days = (f, src) => From_days(f(src));
  let from_unixtime = (f, src) => From_unixtime(f(src));
  let if_ = (f, g, h, src) =>
    [@implicit_arity] If(f(src), g(src), h(src));
  let ifnull = (f, g, src) => [@implicit_arity] Ifnull(f(src), g(src));
  let last_day = (f, src) => Last_day(f(src));
  let length = (f, src) => Length(f(src));
  let localtime = ((), src) => Localtime;
  let log = (f, src) => Log(f(src));
  let log10 = (f, src) => Log10(f(src));
  let log2 = (f, src) => Log2(f(src));
  let lower = (f, src) => Lower(f(src));
  let lpad = (f, src) => Lpad(f(src));
  let ltrim = (f, src) => Ltrim(f(src));
  let max = (f, src) => Max(f(src));
  let md5 = (f, src) => Md5(f(src));
  let minute = (f, src) => Minute(f(src));
  let month = (f, src) => Month(f(src));
  let monthname = (f, src) => Monthname(f(src));
  let now = ((), src) => Now;
  let nullif = (f, g, src) => [@implicit_arity] Nullif(f(src), g(src));
  let ord = (f, src) => Ord(f(src));
  let pow = (f, src) => Pow(f(src));
  let radians = (f, src) => Radians(f(src));
  let rand = (f, src) => Rand(f(src));
  let repeat = (f, n, src) => [@implicit_arity] Repeat(f(src), n);
  let replace = (f, s1, s2, src) =>
    [@implicit_arity] Replace(f(src), s1, s2);
  let reverse = (f, src) => Reverse(f(src));
  let (=~) = (f, re, src) => [@implicit_arity] Rlike(f(src), re);
  let round = (f, src) => Round(f(src));
  let rpad = (f, src) => Rpad(f(src));
  let rtrim = (f, src) => Rtrim(f(src));
  let sec_to_time = (f, src) => Sec_to_time(f(src));
  let second = (f, src) => Second(f(src));
  let sha1 = (f, src) => Sha1(f(src));
  let sha2 = (f, src) => Sha2(f(src));
  let sign = (f, src) => Sign(f(src));
  let sin = (f, src) => Sin(f(src));
  let sqrt = (f, src) => Sqrt(f(src));
  let stddev = (f, src) => Stddev(f(src));
  let substring = (f, pos, len, src) =>
    [@implicit_arity] Substring(f(src), pos, len);
  let substring_index = (f, d, n, src) =>
    [@implicit_arity] Substring_index(f(src), d, n);
  let sum = (f, d, src) => [@implicit_arity] Sum(f(src), d);
  let tan = (f, src) => Tan(f(src));
  let time_of:
    (
      M.Select.source('s) => t(time([< | `Time | `Datetime])),
      M.Select.source('s)
    ) =>
    t(time([ | `Time])) = (
    (f, src) => TimeFn(f(src)):
      (
        M.Select.source('s) => t(time([< | `Time | `Datetime])),
        M.Select.source('s)
      ) =>
      t(time([ | `Time]))
  );
  let timestampdiff = (u, f, g, src) =>
    [@implicit_arity] Timestampdiff(u, f(src), g(src));
  let trim = (f, src) => Trim(f(src));
  let uncompress = (f, src) => Uncompress(f(src));
  let utc_date = ((), src) => Utc_date;
  let utc_time = ((), src) => Utc_time;
  let utc_timestamp = ((), src) => Utc_timestamp;
  let uuid = ((), src) => Uuid;
  let uuid_short = ((), src) => Uuid_short;
  let week = ((), src) => Week;
  let weekday = ((), src) => Weekday;
  let weekofyear = ((), src) => Weekofyear;
  let upper = (f, src) => Upper(f(src));
  let year = ((), src) => Year;

  let as_time = (f, src) => [@implicit_arity] Cast(f(src), Time);
  let as_timestamp = (f, src) => [@implicit_arity] Cast(f(src), Timestamp);
  let as_date = (f, src) => [@implicit_arity] Cast(f(src), Date);
  let as_datetime = (f, src) => [@implicit_arity] Cast(f(src), Datetime);

  let rec build: type a. (~handover: handover, build_step, t(a)) => build_step =
    (~handover, st, e) => {
      let build_param = build_param(D.placeholder);
      let fn = (~st=st) =>
        M.Expr.build_function(~placeholder=D.placeholder, ~handover, st);
      Base.(
        switch (e) {
        /* Data types */
        | Lit(Lit.Time(t)) => build_param(st, Param.Time(t))
        | Lit(Lit.Timestamp(t)) => build_param(st, Param.Timestamp(t))
        | Lit(Lit.Date(d)) => build_param(st, Param.Date(d))
        | Lit(Lit.Datetime(d)) => build_param(st, Param.Datetime(d))
        | Lit(Lit.Enum(e)) => build_param(st, Param.Enum(e))
        /* Functions */
        | Abs(e) => fn("ABS(", [e], ")")
        | Acos(e) => fn("ACOS(", [e], ")")
        | Ascii(e) => fn("ASCII(", [e], ")")
        | Asin(e) => fn("ASIN(", [e], ")")
        | Atan(e) => fn("ATAN(", [e], ")")
        | [@implicit_arity] Atan2(e1, e2) => fn("ATAN2(", [e1, e2], ")")
        | Avg(e) => fn("AVG(", [e], ")")
        | [@implicit_arity] Between(e1, e2) => fn("BETWEEN(", [e1, e2], ")")
        | Bin(e) => fn("BIN(", [e], ")")
        | Bit_and(e) => fn("BIT_AND", [e], ")")
        | Bit_count(e) => fn("BIT_COUNT", [e], ")")
        | Bit_length(e) => fn("BIT_LENGTH", [e], ")")
        | Bit_or(e) => fn("BIT_OR", [e], ")")
        | Bit_xor(e) => fn("BIT_XOR", [e], ")")
        | Ceil(e) => fn("CEIL(", [e], ")")
        | Char(l) => fn("CHAR(", l, ")")
        | Char_length(e) => fn("CHAR_LENGTH", [e], ")")
        | Charset(e) => fn("CHARSET(", [e], ")")
        | Coalesce(l) => fn("COALESCE(", l, ")")
        | Collation(e) => fn("COLLATION(", [e], ")")
        | Compress(e) => fn("COMPRESS(", [e], ")")
        | Concat(l) => fn("CONCAT(", l, ")")
        | [@implicit_arity] Concat_ws(s, l) => fn("CONCAT_WS", l, ")")
        | Connection_id => fn("CONNECTION_ID", [], ")")
        | [@implicit_arity] Conv(e1, i, j) =>
          fn("CONV(", [e1], sprintf(", %d, %d)", i, j))
        | Cos(e) => fn("COS(", [e], ")")
        | Cot(e) => fn("COT(", [e], ")")
        | [@implicit_arity] Count(e, true) =>
          fn("COUNT(DISTINCT ", [e], ")")
        | [@implicit_arity] Count(e, false) => fn("COUNT(", [e], ")")
        | Crc32(e) => fn("CRC32", [e], ")")
        | Current_date => fn("CURRENT_DATE", [], ")")
        | Current_time => fn("CURRENT_TIME", [], ")")
        | Current_timestamp => fn("CURRENT_TIMESTAMP", [], ")")
        | Current_user => fn("CURRENT_USER", [], ")")
        | Database => fn("DATABASE(", [], ")")
        | DateFn(e) => fn("DATE(", [e], ")")
        | [@implicit_arity] Date_add(e, i, u) =>
          let s = string_of_time_unit(u);
          fn("DATE_ADD(", [e], sprintf(", INTERVAL %d %s)", i, s));
        | [@implicit_arity] Date_format(e, fmt) =>
          fn("DATE_FORMAT(", [e], sprintf(", %s)", fmt))
        | [@implicit_arity] Date_sub(e, i, u) =>
          let s = string_of_time_unit(u);
          fn("DATE_SUB(", [e], sprintf(", INTERVAL %d %s)", i, s));
        | [@implicit_arity] Datediff(e1, e2) =>
          fn("DATEDIFF(", [e1, e2], ")")
        | Dayname(e) => fn("TO_CHAR(", [e], ", 'DAY')")
        | Dayofmonth(e) => fn("EXTRACT(DAY FROM ", [e], ")")
        | Dayofweek(e) => fn("EXTRACT(ISODOW FROM ", [e], ")")
        | Dayofyear(e) => fn("EXTRACT(DOY FROM ", [e], ")")
        | Degrees(e) => fn("DEGREES(", [e], ")")
        | Expr(e) => fn("EXPR(", [e], ")")
        | Floor(e) => fn("FLOOR(", [e], ")")
        /* No IF() function in PgSQL - use CASE construction */
        | [@implicit_arity] If(b, t, f) =>
          let st1 = fn("CASE WHEN ", [b], " THEN ");
          let st2 = fn(~st=st1, st1.repr ++ " ELSE ", [t, f], " END ");
          Sequoia.{...st2, params: st1.params @ st2.params};
        | From_days(e) => fn("FROM_DAYS(", [e], ")")
        | From_unixtime(e) => fn("FROM_UNIXTIME(", [e], ")")
        | [@implicit_arity] Ifnull(e1, e2) =>
          let st1 = fn("IFNULL(", [e1], "");
          let st2 = fn(~st=st1, st1.repr ++ ", ", [e2], ")");
          Sequoia.{...st2, params: st1.params @ st2.params};
        | Last_day(e) => fn("LAST_DAY(", [e], ")")
        | Length(e) => fn("LENGTH(", [e], ")")
        | Localtime => fn("LOCALTIME(", [], ")")
        | Log(e) => fn("LOG(", [e], ")")
        | Log10(e) => fn("LOG10", [e], ")")
        | Log2(e) => fn("LOG2", [e], ")")
        | Lower(e) => fn("LOWER(", [e], ")")
        | Lpad(e) => fn("LPAD(", [e], ")")
        | Ltrim(e) => fn("LTRIM(", [e], ")")
        | Max(e) => fn("MAX(", [e], ")")
        | Md5(e) => fn("MD5", [e], ")")
        | Minute(e) => fn("MINUTE(", [e], ")")
        | Month(e) => fn("MONTH(", [e], ")")
        | Monthname(e) => fn("MONTHNAME(", [e], ")")
        | Now => fn("NOW(", [], ")")
        | [@implicit_arity] Nullif(e1, e2) => fn("NULLIF(", [e1, e2], ")")
        | Ord(e) => fn("ORD(", [e], ")")
        | Pow(e) => fn("POW(", [e], ")")
        | Radians(e) => fn("RADIANS(", [e], ")")
        | Rand(Some(seed)) => fn("RAND(", [seed], ")")
        | Rand(None) => fn("RAND(", [], ")")
        | [@implicit_arity] Repeat(e, n) =>
          fn("REPEAT(", [e], sprintf(", %d)", n))
        | [@implicit_arity] Replace(e, s1, s2) =>
          fn("REPLACE(", [e], sprintf(", %s, %s)", s1, s2))
        | Reverse(e) => fn("REVERSE(", [e], ")")
        | [@implicit_arity] Rlike(e, re) =>
          fn("RLIKE(", [e], sprintf(", %s)", re))
        | Round(e) => fn("ROUND(", [e], ")")
        | Rpad(e) => fn("RPAD(", [e], ")")
        | Rtrim(e) => fn("RTRIM(", [e], ")")
        | Sec_to_time(e) => fn("SEC_TO_TIME", [e], ")")
        | Second(e) => fn("SECOND(", [e], ")")
        | Sha1(e) => fn("SHA1", [e], ")")
        | Sha2(e) => fn("SHA2", [e], ")")
        | Sign(e) => fn("SIGN(", [e], ")")
        | Sin(e) => fn("SIN(", [e], ")")
        | Sqrt(e) => fn("SQRT(", [e], ")")
        | Stddev(e) => fn("STDDEV(", [e], ")")
        | [@implicit_arity] Substring(e, pos, Some(len)) =>
          fn("SUBSTRING(", [e], sprintf(" FROM %d FOR %d)", pos, len))
        | [@implicit_arity] Substring(e, pos, None) =>
          fn("SUBSTRING(", [e], sprintf(" FROM %d)", pos))
        | [@implicit_arity] Substring_index(e, delim, n) =>
          fn("SUBSTRING_INDEX(", [e], sprintf(", %s, %d)", delim, n))
        | [@implicit_arity] Sum(e, true) => fn("SUM(DISTINCT ", [e], ")")
        | [@implicit_arity] Sum(e, false) => fn("SUM(", [e], ")")
        | Tan(e) => fn("TAN(", [e], ")")
        | TimeFn(e) => fn("TIME(", [e], ")")
        | [@implicit_arity] Timestampdiff(u, e1, e2) =>
          let s = string_of_time_unit(u);
          fn(sprintf("TIMESTAMPDIFF(%s, ", s), [e1, e2], ")");
        | Trim(e) => fn("TRIM(", [e], ")")
        | Uncompress(e) => fn("UNCOMPRESS(", [e], ")")
        | [@implicit_arity] Cast(e, Time) =>
          fn("CAST(", [e], sprintf(" AS %s)", string_of_cast(Time)))
        | [@implicit_arity] Cast(e, Timestamp) =>
          fn("CAST(", [e], sprintf(" AS %s)", string_of_cast(Timestamp)))
        | [@implicit_arity] Cast(e, Date) =>
          fn("CAST(", [e], sprintf(" AS %s)", string_of_cast(Date)))
        | [@implicit_arity] Cast(e, Datetime) =>
          fn("CAST(", [e], sprintf(" AS %s)", string_of_cast(Datetime)))
        | e => handover.expr(st, e)
        }
      );
    };
};

module Select = {
  include (
            M.Select:
               (module type of M.Select) with module Expr := M.Select.Expr
          );

  let expr_build = (st, e) => Expr.build(st, e);
  let cast_handover = Expr.string_of_cast;

  module Expr = {
    include M.Select.Expr;

    let unwrap:
      type a.
        (
          Field.t('t, option(a)),
          M.Select.steps('b, 'c, 't, 'd),
          M.Select.source('b)
        ) =>
        t(a) =
      (fld, sts, src) =>
        switch (fld) {
        | [@implicit_arity] Field.Null.Time(n, t) =>
          [@implicit_arity]
          Field([@implicit_arity] Field.Time(n, t), src, sts)
        | [@implicit_arity] Field.Null.Timestamp(n, t) =>
          [@implicit_arity]
          Field([@implicit_arity] Field.Timestamp(n, t), src, sts)
        | [@implicit_arity] Field.Null.Date(n, t) =>
          [@implicit_arity]
          Field([@implicit_arity] Field.Date(n, t), src, sts)
        | [@implicit_arity] Field.Null.Datetime(n, t) =>
          [@implicit_arity]
          Field([@implicit_arity] Field.Datetime(n, t), src, sts)
        | [@implicit_arity] Field.Null.Enum(n, t, e) =>
          [@implicit_arity]
          Field([@implicit_arity] Field.Enum(n, t, e), src, sts)
        | _ => unwrap(fld, sts, src)
        };

    let build:
      type a. (~handover: Expr.handover, build_step, Expr.t(a)) => build_step =
      (~handover, st) =>
        fun
        /* Fields */
        | [@implicit_arity] Field(Field.Time(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            pos: st.pos,
            params: [],
          }
        | [@implicit_arity] Field(Field.Timestamp(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            pos: st.pos,
            params: [],
          }
        | [@implicit_arity] Field(Field.Date(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            pos: st.pos,
            params: [],
          }
        | [@implicit_arity] Field(Field.Datetime(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            pos: st.pos,
            params: [],
          }
        | [@implicit_arity] Field(Field.Enum(_) as fld, _, _) => {
            ...st,
            repr: Field.to_string(fld),
            pos: st.pos,
            params: [],
          }
        /* Handover */
        | e => build(~handover, st, e);
  };

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
