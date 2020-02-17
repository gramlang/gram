# This program is known as Girard's paradox. It loops forever, demonstrating
# the inconsistency of a pure type system with type : type. The formulation
# below is from this paper:
#   Antonius J. C. Hurkens. 1995. A Simplification of Girard's Paradox. In
#   Proceedings of the Second International Conference on Typed Lambda Calculi
#   and Applications (TLCA ’95). Springer-Verlag, Berlin, Heidelberg, 266–278.

# The cast of characters in our story
false = (p : type) -> p;
negate = (phi : type) => phi -> false;
power = (S : type) => S -> type;
U = (X : type) -> ((power (power X) -> X) -> power (power X));
tau =
  (t : power (power U)) =>
    (X : type) =>
      (f : power (power X) -> X) =>
        (p : power X) =>
          t ((x : U) => p (f (x X f)));
sigma = (s : U) => s U ((t : power (power U)) => tau t);
Delta = (y : U) => negate ((p : power U) -> sigma y p -> p (tau (sigma y)));
Omega = tau ((p : power U) => (x : U) -> sigma x p -> p x);

# Let the fun begin!
(
  (zero : (p : power U) -> ((x : U) -> sigma x p -> p x) -> p Omega) =>
    (
      zero
      Delta
      (
        (x : U) =>
          (two : sigma x Delta) =>
            (three : (p : power U) -> sigma x p -> p (tau (sigma x))) =>
              three Delta two ((p : power U) => three ((y : U) => p (tau (sigma y))))
      )
    )
    ((p : power U) => zero ((y : U) => p (tau (sigma y))))
)
(
  (p : power U) =>
    (one : (x : U) -> sigma x p -> p x) =>
      one Omega ((x : U) => one (tau (sigma x)))
)
