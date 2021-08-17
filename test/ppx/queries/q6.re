let q6 = {
  let (query, params) =
    Mysql.(
      Lit.(
        Vector.(
          Insert.(
            Vector.(
              insert(
                ~into=User.table,
                ~fields=[User.id, User.name, User.site],
                ~values=[
                  [int(1), string("a"), Null.string("a.com")],
                  [int(2), string("b"), Null.string("b.com")],
                ],
              )
              |> seal
            )
          )
        )
      )
    );
  (query, params);
};
