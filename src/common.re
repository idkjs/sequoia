module AliasSet =
  Set.Make({
    type t = string;
    let compare = compare;
  });

type build_step = {
  repr: string,
  params: list(Param.t),
  pos: int,
  aliases: AliasSet.t,
};

let blank_step = {repr: "", params: [], pos: 0, aliases: AliasSet.empty};

let build_param = (f, st, p) => {
  ...st,
  repr: f(st.pos),
  params: [p],
  pos: st.pos + 1,
};

let join_lines =
  List.fold_left(
    (acc, s) =>
      if (s == "") {
        acc;
      } else if (acc == "") {
        s;
      } else {
        acc ++ "\n" ++ s;
      },
    "",
  );
