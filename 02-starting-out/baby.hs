doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
						then x
						else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

a = if True then "something" else "nothing"
