let q2 = {
  let (query, params) = {
    open Mysql;
    open Expr;
    open Select;
    open Expr;
    open OrderBy.Expr;
    open Vector;
    let src = from(TeamUser.table);
    let src = left_join(that(TeamUser.user, There), src);
    let src = left_join(that(TeamUser.team, Skip(There)), src);
    let sel =
      select(
        ~distinct=true,
        Expr.(
          Vector.[field(User.name, There), field(Team.name, Skip(There))]
        ),
        src,
      );
    let stmt = where(Expr.(field(Team.name, Skip(There)) =% "foo%"), sel);
    let stmt = order_by([asc(field(Team.name, Skip(There)))], stmt);
    seal(stmt);
  };
  (query, params);
};
