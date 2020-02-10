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

# A proof that equality is symmetric
(
  (
    eq_symm : (a : type) ->
              (x : a) ->
              (y : a) ->
              eq a x y ->
              eq a y x
  ) => type
) (
  (a : type) =>
  (x : a) =>
  (y : a) =>
  (x_equals_y : eq a x y) =>
    motive = (z : a) => eq a z x;
    motive_holds_for_x = refl a x;
    eq_ind a x motive motive_holds_for_x y x_equals_y
)
