/** [INSERT] query builder. */

module type S = {
  /** Vector of fields. */

  module Vector: Vector.S with type elem('t, 'a) := Field.t('t, 'a);

  /** The type of [INSERT] queries. */

  type t(_);

  /** Insert the given [values] (given as a vector of vectors) corresponding
        to the appropriate [fields] into the table given by [into]. */

  let insert:
    (
      ~into: Table.t('t),
      ~fields: Vector.t('t, 'a, Nat.s('n)),
      ~values: Lit.Vector.matrix('u, 'a, Nat.s('m), Nat.s('n))
    ) =>
    t('t);

  /** Mark the end of an [INSERT] query. */

  let seal: t('t) => (string, list(Param.t));
};

/** Functor that generates an [INSERT] query builder for the given driver. */

module Make: (D: Driver.S) => S;
