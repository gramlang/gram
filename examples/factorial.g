factorial : (int -> int) = (x : int) =>
  if x == 0
  then 1
  else x * factorial (x - 1)

factorial 50