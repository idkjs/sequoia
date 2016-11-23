module type S = sig
  type _ t

  val seal : handover:Sequoia_expr.handover -> 't t
          -> string * Sequoia_param.t list

  type ('t, 'a) mk = ('t, 'a) Sequoia_field.t
                   * ('t Sequoia_table.t -> ('t, 'a) Sequoia_expr.expr)

  module OrderBy : sig
    type order

    type ('t, 'a) expr = 'a Sequoia_expr.t * order

    module Expr : sig
      type ('t, 'a) mk = 't Sequoia_table.t -> 'a Sequoia_expr.t * order

      module Vector : Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) mk

      val asc : ('t Sequoia_table.t -> 'a Sequoia_expr.t)
             -> 't Sequoia_table.t
             -> ('t, 'a) expr

      val desc : ('t Sequoia_table.t -> 'a Sequoia_expr.t)
              -> 't Sequoia_table.t
              -> ('t, 'a) expr
    end

    module Vector : Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) expr

    val vectormk_to_vector : 't Sequoia_table.t
                          -> ('t, 'a, 'n) Expr.Vector.t
                          -> ('t, 'a, 'n) Vector.t
  end

  module Expr : sig
    include module type of Sequoia_query_common.UpdateDeleteExpr

    type ('t, 'a) mk = 't Sequoia_table.t -> ('t, 'a) Sequoia_expr.expr

    module Vector : Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) mk
  end

  module Vector : Sequoia_vector.S with type ('t, 'a) elem := ('t, 'a) mk

  val update : 't Sequoia_table.t
            -> set:('t, 'a, 'n Sequoia_vector.Nat.s) Vector.t
            -> 't t

  val where : ('t Sequoia_table.t -> 'a Sequoia_expr.t) -> 't t -> 't t

  val order_by : ('t, 'a, 'n Sequoia_vector.Nat.s) OrderBy.Expr.Vector.t
              -> 't t
              -> 't t

  val limit : ?offset:int -> int -> 't t -> 't t
end

module Make (D : Sequoia_driver.S) : S
