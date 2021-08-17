let q4 = {
  let (query, params) = {
    open Mysql;
    open Expr;
    open Select;
    let src = from(User.table);
    let src = right_join(this(Team.owner, There), src);
    let src = right_join(this(Project.leader, Skip(There)), src);
    let sel =
      select(
        Expr.(
          Vector.[
            field(Team.id, There),
            field(Team.name, There),
            field(User.id, Skip(Skip(There))),
            field(User.name, Skip(Skip(There))),
          ]
        ),
        src,
      );
    let stmt =
      where(Expr.(field(User.id, Skip(Skip(There))) > int(42)), sel);
    seal(stmt);
  };
  (query, params);
};
