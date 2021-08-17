module type Elem = {type elem('a, 'b);};

module type S = {
  type elem('a, 'b);

  type t(_, _, _) =
    | []: t('a, 'b, Nat.z)
    | ::(elem('a, 'b), t('a, 'c, 'n)): t('a, ('b, 'c), Nat.s('n));

  type folder('z) = {f: 'a 'b. ('z, elem('a, 'b)) => 'z};

  let vector_fold_left: (folder('z), 'z, t('a, 'b, 'n)) => 'z;
  let vector_length: t('a, 'b, 'n) => int;

  type matrix('a, 'b, 'm, 'n) =
    | []: matrix('a, 'b, Nat.z, 'n)
    | ::(t('a, 'b, 'n), matrix('a, 'b, 'm, 'n))
      : matrix('a, 'b, Nat.s('m), 'n);

  let matrix_fold_left: (folder('z), 'z, matrix('a, 'b, 'm, 'n)) => 'z;
};

module Make = (E: Elem) : (S with type elem('a, 'b) := E.elem('a, 'b)) => {
  type t(_, _, _) =
    | []: t('a, 'b, Nat.z)
    | ::(E.elem('a, 'b), t('a, 'c, 'n)): t('a, ('b, 'c), Nat.s('n));

  type folder('z) = {f: 'a 'b. ('z, E.elem('a, 'b)) => 'z};

  let rec vector_fold_left: type a b n. (folder('z), 'z, t(a, b, n)) => 'z =
    (f, z) =>
      fun
      | [] => z
      | [e, ...es] => vector_fold_left(f, f.f(z, e), es);

  let rec vector_length: type a b n. t(a, b, n) => int =
    fun
    | [] => 0
    | [_, ...xs] => 1 + vector_length(xs);

  type matrix('a, 'b, 'm, 'n) =
    | []: matrix('a, 'b, Nat.z, 'n)
    | ::(t('a, 'b, 'n), matrix('a, 'b, 'm, 'n))
      : matrix('a, 'b, Nat.s('m), 'n);

  type matrix_folder('z) = {mf: 'a 'b. ('z, E.elem('a, 'b)) => 'z};

  let rec matrix_fold_left:
    type a b m n. (folder('z), 'z, matrix(a, b, m, n)) => 'z =
    (f, z) =>
      fun
      | [] => z
      | [v, ...vs] => {
          let z = vector_fold_left(f, z, v);
          matrix_fold_left(f, z, vs);
        };
};
