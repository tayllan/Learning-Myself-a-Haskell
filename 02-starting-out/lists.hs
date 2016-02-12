boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeUpperCase xs = [x | x <- xs, not (elem x ['A'..'Z'])]
