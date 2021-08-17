/** [REPLACE] query builder */

module type S = {
  /** Vector of fields. */

  module Vector: Vector.S with type elem('t, 'a) := Field.t('t, 'a);

  /** The type of [REPLACE] queries. */

  type t(_);

  /** Create a [REPLACE] query for table [into], setting the given [fields]
        to the respective [values]. */

  let replace:
    (
      ~into: Table.t('t),
      ~fields: Vector.t('t, 'a, Nat.s('n)),
      ~values: Lit.Vector.matrix('u, 'a, Nat.s('m), Nat.s('n))
    ) =>
    t('t);

  /** Mark the end of a [REPLACE] query. */

  let seal: t('t) => (string, list(Param.t));
};

/** Functor that generates a [REPLACE] query builder for the given driver. */

module Make: (D: Driver.S) => S;
