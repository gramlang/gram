#!/usr/bin/env gram

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
  (proof : eq a x y) =>
  eq_ind a x ((p : a) => eq a p x) (refl a x) y proof
)
