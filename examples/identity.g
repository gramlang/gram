#!/usr/bin/env gram
(a : type) => (b : type) =>
  (f : (_ : a) -> b) => (x : a) =>
    f x