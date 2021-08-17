open Query_common;

module type S = {
  module Vector: Vector.S with type elem('t, 'a) := Field.t('t, 'a);

  type t(_);

  let replace:
    (
      ~into: Table.t('t),
      ~fields: Vector.t('t, 'a, Nat.s('n)),
      ~values: Lit.Vector.matrix('u, 'a, Nat.s('m), Nat.s('n))
    ) =>
    t('t);

  let seal: t('t) => (string, list(Param.t));
};

module Make = (D: Driver.S) : S => {
  include InsertReplace;

  let replace = create;
  let seal = stmt => seal(~placeholder=D.placeholder, ~query="REPLACE", stmt);
};
