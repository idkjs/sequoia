let q3 = {
  let%sql (query, params) = {
    open Mysql;
    open Expr;
    open Select;
    let src = from(Team.table);
    let src = inner_join(that(Team.owner, There), src);
    let src = inner_join(this(Project.leader, There), src);
    let sel =
      select(
        Expr.(
          Vector.[
            field(Team.name, Skip(Skip(There))),
            field(Project.title, There),
          ]
        ),
        src,
      );
    let stmt = where(Expr.(field(Project.title, There) =% "P%"), sel);
    seal(stmt);
  };
  (query, params);
};
