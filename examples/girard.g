# This program is known as Girard's paradox. It loops forever, demonstrating
# the inconsistency of a pure type system with type : type. The formulation
# below is from this paper:
#   Antonius J. C. Hurkens. 1995. A Simplification of Girard's Paradox. In
#   Proceedings of the Second International Conference on Typed Lambda Calculi
#   and Applications (TLCA ’95). Springer-Verlag, Berlin, Heidelberg, 266–278.

# The cast of characters in our story
exfalso = (p : type) -> p
negate = (phi : type) => phi -> exfalso
power = (S : type) => S -> type
universe = (X : type) -> ((power (power X) -> X) -> power (power X))
tau =
  (t : power (power universe)) =>
    (X : type) =>
      (f : power (power X) -> X) =>
        (p : power X) =>
          t ((x : universe) => p (f (x X f)))
sigma = (s : universe) => s universe ((t : power (power universe)) => tau t)
delta = (y : universe) => negate ((p : power universe) -> sigma y p -> p (tau (sigma y)))
omega = tau ((p : power universe) => (x : universe) -> sigma x p -> p x)

# Let the fun begin!
(
  (zero : (p : power universe) -> ((x : universe) -> sigma x p -> p x) -> p omega) =>
    (
      zero delta (
        (x : universe) =>
          (two : sigma x delta) =>
            (three : (p : power universe) -> sigma x p -> p (tau (sigma x))) =>
              three delta two ((p : power universe) => three ((y : universe) => p (tau (sigma y))))
      )
    ) ((p : power universe) => zero ((y : universe) => p (tau (sigma y))))
) (
  (p : power universe) =>
    (one : (x : universe) -> sigma x p -> p x) =>
      one omega ((x : universe) => one (tau (sigma x)))
)
