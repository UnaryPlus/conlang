module SoundChange where

type Pattern a = [[a]]

data Environment a
  =

x > x' / e
  1. check e
  2. check x
  3. replace with x'

x, y > x' / e
  1. check e
  2. check x or y
  3. replace with x'

x > x', y > y' / e
  1. check e
  2. check x (replace with x')
  3. check y (replace with y')

x > (x1 / e1, x2 / e2)
  1. check x
  2.
