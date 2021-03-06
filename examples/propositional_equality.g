# The formation rule for propositional equality
(eq : (a : type) -> (x : a) -> (y : a) -> type) =>

# The introduction rule for propositional equality
(refl : (a : type) -> (x : a) -> eq a x x) =>

# The elimination rule for propositional equality
(eq_ind : (a : type) ->
          (x : a) ->
          (p : a -> type) ->
          p x ->
          (y : a) ->
          eq a x y ->
          p y) =>

# A proof that propositional equality is symmetric
eq_symm : (
  (a : type) ->
  (x : a) ->
  (y : a) ->
  eq a x y ->
  eq a y x
) =
  a =>
  (x : a) =>
  (y : a) =>
  (x_equals_y : eq a x y) =>
    motive = (z : a) => eq a z x
    x_equals_x = refl a x
    eq_ind a x motive x_equals_x y x_equals_y

# A proof that propositional equality is transitive
eq_trans : (
  (a : type) ->
  (x : a) ->
  (y : a) ->
  (z : a) ->
  eq a x y ->
  eq a y z ->
  eq a x z
) =
  a =>
  (x : a) =>
  (y : a) =>
  (z : a) =>
  (x_equals_y : eq a x y) =>
  (y_equals_z : eq a y z) =>
    motive = (w : a) => eq a w z
    y_equals_x = eq_symm a x y x_equals_y
    eq_ind a y motive y_equals_z x y_equals_x

type
