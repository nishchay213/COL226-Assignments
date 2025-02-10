open A2

let%test "type_test1" = type_of (Add (ConstS 1., ConstS 2.)) = Scalar

let%test "type_test2" =
  type_of (Add (ConstV [ 1.; 2. ], ConstV [ 3.; 4. ])) = Vector 2

let%test "type_test3" = type_of T = Bool
let%test "type_test4" = type_of F = Bool
let%test "type_test5" = type_of (ConstS 1.) = Scalar
let%test "type_test6" = type_of (ConstV [ 1.; 2.; 3.; 4.; 5. ]) = Vector 5
let%test "type_test7" = type_of (Add (T, F)) = Bool
let%test "type_test8" = type_of (Add (T, T)) = Bool
let%test "type_test9" = type_of (Add (F, F)) = Bool
let%test "type_test10" = type_of (Add (F, T)) = Bool

let%test "type_test11" =
  try
    let _ = type_of (Add (T, ConstS 1.)) in
    false
  with
  | Wrong e -> e = Add (T, ConstS 1.)
  | _ -> false

let%test "type_test12" =
  try
    let _ = type_of (Add (ConstS 2., ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = Add (ConstS 2., ConstV [ 1.; 2. ])
  | _ -> false

let%test "type_test13" =
  try
    let _ = type_of (Add (ConstV [ 1.; 2. ], T)) in
    false
  with
  | Wrong e -> e = Add (ConstV [ 1.; 2. ], T)
  | _ -> false

let%test "type_test14" = type_of (Inv (ConstS 1.)) = Scalar
let%test "type_test15" = type_of (Inv (ConstV [ 1.; 2. ])) = Vector 2
let%test "type_test16" = type_of (Inv T) = Bool
let%test "type_test17" = type_of (Inv F) = Bool
let%test "type_test18" = type_of (Inv (Add (T, F))) = Bool
let%test "type_test19" = type_of (Inv (Add (T, T))) = Bool
let%test "type_test20" = type_of (Inv (Add (F, F))) = Bool
let%test "type_test21" = type_of (Inv (Add (F, T))) = Bool
let%test "type_test22" = type_of (Inv (Inv (ConstS 1.))) = Scalar
let%test "type_test23" = type_of (Inv (Inv (ConstV [ 1.; 2. ]))) = Vector 2
let%test "type_test24" = type_of (Inv (Inv T)) = Bool
let%test "type_test25" = type_of (ScalProd (T, F)) = Bool
let%test "type_test26" = type_of (ScalProd (T, T)) = Bool
let%test "type_test27" = type_of (ScalProd (F, F)) = Bool
let%test "type_test28" = type_of (ScalProd (F, T)) = Bool

let%test "type_test29" =
  type_of (ScalProd (Inv (ConstS 1.), ConstS 1.)) = Scalar

let%test "type_test30" =
  type_of (ScalProd (Inv (ConstV [ 1.; 2. ]), ConstS 2.)) = Vector 2

let%test "type_test31" =
  type_of (ScalProd (ConstV [ 1.; 2. ], ConstS 1.)) = Vector 2

let%test "type_test32" =
  try
    let _ = type_of (ScalProd (T, ConstS 1.)) in
    false
  with
  | Wrong e -> e = ScalProd (T, ConstS 1.)
  | _ -> false

let%test "type_test33" =
  try
    let _ = type_of (ScalProd (T, ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = ScalProd (T, ConstV [ 1.; 2. ])
  | _ -> false

let%test "type_test34" =
  try
    let _ = type_of (ScalProd (ConstV [ 1.; 3. ], ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = ScalProd (ConstV [ 1.; 3. ], ConstV [ 1.; 2. ])
  | _ -> false

let%test "type_test35" =
  type_of (DotProd (ConstV [ 1.; 2. ], ConstV [ 3.; 4. ])) = Scalar

let%test "type_test36" =
  type_of (DotProd (ConstV [ 1.; 2.; 3.; 4. ], ConstV [ 3.; 4.; 5.; 6. ]))
  = Scalar

let%test "type_test37" =
  type_of
    (DotProd (ConstV [ 1.; 2.; 3.; 4.; 5. ], ConstV [ 3.; 4.; 5.; 6.; 7. ]))
  = Scalar

let%test "type_test38" =
  try
    let _ = type_of (DotProd (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])) in
    false
  with
  | Wrong e -> e = DotProd (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])
  | _ -> false

let%test "type_test39" =
  try
    let _ = type_of (DotProd (ConstV [ 1.; 2. ], ConstS 1.)) in
    false
  with
  | Wrong e -> e = DotProd (ConstV [ 1.; 2. ], ConstS 1.)
  | _ -> false

let%test "type_test40" =
  try
    let _ = type_of (DotProd (T, ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = DotProd (T, ConstV [ 1.; 2. ])
  | _ -> false

let%test "type_test41" = type_of (Mag (ConstV [ 1.; 2. ])) = Scalar
let%test "type_test42" = type_of (Mag (ConstV [ 1.; 2.; 3.; 4. ])) = Scalar
let%test "type_test43" = type_of (Mag (ConstV [ 1.; 2.; 3.; 4.; 5. ])) = Scalar
let%test "type_test44" = type_of (Mag (ConstS 1.)) = Scalar

let%test "type_test45" =
  try
    let _ = type_of (Mag T) in
    false
  with
  | Wrong e -> e = Mag T
  | _ -> false

let%test "type_test46" =
  try
    let _ = type_of (Mag F) in
    false
  with
  | Wrong e -> e = Mag F
  | _ -> false

let%test "type_test47" =
  type_of (Angle (ConstV [ 1.; 2. ], ConstV [ 3.; 4. ])) = Scalar

let%test "type_test48" =
  type_of (Angle (ConstV [ 1.; 2.; 3.; 4. ], ConstV [ 3.; 4.; 5.; 6. ]))
  = Scalar

let%test "type_test49" =
  type_of (Angle (ConstV [ 1.; 2.; 3.; 4.; 5. ], ConstV [ 3.; 4.; 5.; 6.; 7. ]))
  = Scalar

let%test "type_test50" =
  try
    let _ = type_of (Angle (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])) in
    false
  with
  | Wrong e -> e = Angle (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])
  | _ -> false

let%test "type_test51" =
  try
    let _ = type_of (Angle (ConstV [ 1.; 2. ], ConstS 1.)) in
    false
  with
  | Wrong e -> e = Angle (ConstV [ 1.; 2. ], ConstS 1.)
  | _ -> false

let%test "type_test52" =
  try
    let _ = type_of (Angle (T, ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = Angle (T, ConstV [ 1.; 2. ])
  | _ -> false

let%test "type_test53" = type_of (IsZero (ConstV [ 1.; 2. ])) = Bool
let%test "type_test54" = type_of (IsZero (ConstV [ 1.; 2.; 3.; 4. ])) = Bool
let%test "type_test55" = type_of (IsZero (ConstV [ 1.; 2.; 3.; 4.; 5. ])) = Bool
let%test "type_test56" = type_of (IsZero (ConstV [ 0.; 0.; 0.; 0.; 0. ])) = Bool
let%test "type_test57" = type_of (IsZero (ConstS 1.)) = Bool
let%test "type_test58" = type_of (IsZero (ConstS 0.)) = Bool
let%test "type_test59" = type_of (IsZero F) = Bool
let%test "type_test60" = type_of (IsZero T) = Bool
let%test "type_test61" = type_of (Cond (T, T, T)) = Bool
let%test "type_test62" = type_of (Cond (T, F, F)) = Bool
let%test "type_test63" = type_of (Cond (T, ConstS 1., ConstS 1.)) = Scalar

let%test "type_test64" =
  type_of (Cond (T, ConstV [ 1.; 2. ], ConstV [ 1.; 2. ])) = Vector 2

let%test "type_test65" =
  type_of (Cond (T, ConstV [ 1.; 2.; 3.; 4. ], ConstV [ 1.; 2.; 3.; 4. ]))
  = Vector 4

let%test "type_test66" =
  try
    let _ = type_of (Cond (T, ConstV [ 1.; 2. ], ConstS 1.)) in
    false
  with
  | Wrong e -> e = Cond (T, ConstV [ 1.; 2. ], ConstS 1.)
  | _ -> false

let%test "type_test67" =
  try
    let _ = type_of (Cond (T, ConstS 1., ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = Cond (T, ConstS 1., ConstV [ 1.; 2. ])
  | _ -> false

let%test "type_test68" =
  try
    let _ = type_of (Cond (T, ConstV [ 1.; 2. ], ConstV [ 1.; 2.; 3. ])) in
    false
  with
  | Wrong e -> e = Cond (T, ConstV [ 1.; 2. ], ConstV [ 1.; 2.; 3. ])
  | _ -> false

let%test "type_test69" =
  try
    let _ = type_of (Cond (ConstS 1., T, T)) in
    false
  with
  | Wrong e -> e = Cond (ConstS 1., T, T)
  | _ -> false

let%test "type_test70" = type_of (UnitVector (ConstV [ 1.; 2. ])) = Vector 2

let%test "type_test71" =
  type_of (UnitVector (ConstV [ 1.; 2.; 3.; 4. ])) = Vector 4

let%test "type_test72" =
  type_of (UnitVector (ConstV [ 1.; 2.; 3.; 4.; 5. ])) = Vector 5

let%test "type_test73" =
  try
    let _ = type_of (UnitVector (ConstS 1.)) in
    false
  with
  | Wrong e -> e = UnitVector (ConstS 1.)
  | _ -> false

let%test "type_test74" =
  try
    let _ = type_of (UnitVector T) in
    false
  with
  | Wrong e -> e = UnitVector T
  | _ -> false

let%test "type_test75" =
  try
    let _ = type_of (UnitVector F) in
    false
  with
  | Wrong e -> e = UnitVector F
  | _ -> false

let%test "type_test76" = type_of (ZeroVector (ConstV [ 1.; 2. ])) = Vector 2

let%test "type_test77" =
  type_of (ZeroVector (ConstV [ 1.; 2.; 3.; 4. ])) = Vector 4

let%test "type_test78" =
  type_of (ZeroVector (ConstV [ 1.; 2.; 3.; 4.; 5. ])) = Vector 5

let%test "type_test79" =
  try
    let _ = type_of (ZeroVector (ConstS 1.)) in
    false
  with
  | Wrong e -> e = ZeroVector (ConstS 1.)
  | _ -> false

let%test "type_test80" =
  try
    let _ = type_of (ZeroVector T) in
    false
  with
  | Wrong e -> e = ZeroVector T
  | _ -> false

let%test "type_test81" =
  try
    let _ = type_of (ZeroVector F) in
    false
  with
  | Wrong e -> e = ZeroVector F
  | _ -> false

let%test "type_test82" = type_of (Sub (ConstS 1., ConstS 2.)) = Scalar

let%test "type_test83" =
  type_of (Sub (ConstV [ 1.; 2. ], ConstV [ 3.; 4. ])) = Vector 2

let%test "type_test84" =
  type_of (Sub (ConstV [ 1.; 2.; 3.; 4. ], ConstV [ 3.; 4.; 5.; 6. ]))
  = Vector 4

let%test "type_test85" =
  try
    let _ = type_of (Sub (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])) in
    false
  with
  | Wrong e -> e = Sub (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])
  | _ -> false

let%test "type_test86" =
  try
    let _ = type_of (Sub (ConstV [ 1.; 2. ], ConstS 1.)) in
    false
  with
  | Wrong e -> e = Sub (ConstV [ 1.; 2. ], ConstS 1.)
  | _ -> false

let%test "type_test87" =
  try
    let _ = type_of (Sub (T, ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = Sub (T, ConstV [ 1.; 2. ])
  | _ -> false

let%test "type_test88" =
  try
    let _ = type_of (ConstV []) in
    false
  with
  | Wrong e -> e = ConstV []
  | _ -> false

let%test "eval_test1" = eval (Add (ConstS 1., ConstS 2.)) = S 3.

let%test "eval_test2" =
  eval (Add (ConstV [ 1.; 2. ], ConstV [ 3.; 4. ])) = V [ 4.; 6. ]

let%test "eval_test3" = eval T = B true
let%test "eval_test4" = eval F = B false
let%test "eval_test5" = eval (ConstS 1.) = S 1.

let%test "eval_test6" =
  eval (ConstV [ 1.; 2.; 3.; 4.; 5. ]) = V [ 1.; 2.; 3.; 4.; 5. ]

let%test "eval_test7" = eval (Add (T, F)) = B true
let%test "eval_test8" = eval (Add (T, T)) = B true
let%test "eval_test9" = eval (Add (F, F)) = B false
let%test "eval_test10" = eval (Add (F, T)) = B true

let%test "eval_test11" =
  try
    let _ = eval (Add (T, ConstS 1.)) in
    false
  with
  | Wrong e -> e = Add (T, ConstS 1.)
  | _ -> false

let%test "eval_test12" =
  try
    let _ = eval (Add (ConstS 2., ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = Add (ConstS 2., ConstV [ 1.; 2. ])
  | _ -> false

let%test "eval_test13" =
  try
    let _ = eval (Add (ConstV [ 1.; 2. ], T)) in
    false
  with
  | Wrong e -> e = Add (ConstV [ 1.; 2. ], T)
  | _ -> false

let%test "eval_test14" = eval (Inv (ConstS 1.)) = S (-1.)
let%test "eval_test15" = eval (Inv (ConstV [ 1.; 2. ])) = V [ -1.; -2. ]
let%test "eval_test16" = eval (Inv T) = B false
let%test "eval_test17" = eval (Inv F) = B true
let%test "eval_test18" = eval (Inv (Add (T, F))) = B false
let%test "eval_test19" = eval (Inv (Add (T, T))) = B false
let%test "eval_test20" = eval (Inv (Add (F, F))) = B true
let%test "eval_test21" = eval (Inv (Add (F, T))) = B false
let%test "eval_test22" = eval (Inv (Inv (ConstS 1.))) = S 1.
let%test "eval_test23" = eval (Inv (Inv (ConstV [ 1.; 2. ]))) = V [ 1.; 2. ]
let%test "eval_test24" = eval (Inv (Inv T)) = B true
let%test "eval_test25" = eval (ScalProd (T, F)) = B false
let%test "eval_test26" = eval (ScalProd (T, T)) = B true
let%test "eval_test27" = eval (ScalProd (F, F)) = B false
let%test "eval_test28" = eval (ScalProd (F, T)) = B false
let%test "eval_test29" = eval (ScalProd (Inv (ConstS 1.), ConstS 1.)) = S (-1.)

let%test "eval_test30" =
  eval (ScalProd (Inv (ConstV [ 1.; 2. ]), ConstS 2.)) = V [ -2.; -4. ]

let%test "eval_test31" =
  eval (ScalProd (ConstV [ 1.; 2. ], ConstS 1.)) = V [ 1.; 2. ]

let%test "eval_test32" =
  try
    let _ = eval (ScalProd (T, ConstS 1.)) in
    false
  with
  | Wrong e -> e = ScalProd (T, ConstS 1.)
  | _ -> false

let%test "eval_test33" =
  try
    let _ = eval (ScalProd (T, ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = ScalProd (T, ConstV [ 1.; 2. ])
  | _ -> false

let%test "eval_test34" =
  try
    let _ = eval (ScalProd (ConstV [ 1.; 3. ], ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = ScalProd (ConstV [ 1.; 3. ], ConstV [ 1.; 2. ])
  | _ -> false

let%test "eval_test35" =
  eval (DotProd (ConstV [ 1.; 2. ], ConstV [ 3.; 4. ])) = S 11.

let%test "eval_test36" =
  eval (DotProd (ConstV [ 1.; 2.; 3.; 4. ], ConstV [ 3.; 4.; 5.; 6. ])) = S 50.

let%test "eval_test37" =
  eval (DotProd (ConstV [ 1.; 2.; 3.; 4.; 5. ], ConstV [ 3.; 4.; 5.; 6.; 7. ]))
  = S 85.

let%test "eval_test38" =
  try
    let _ = eval (DotProd (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])) in
    false
  with
  | Wrong e -> e = DotProd (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])
  | _ -> false

let%test "eval_test39" =
  try
    let _ = eval (DotProd (ConstV [ 1.; 2. ], ConstS 1.)) in
    false
  with
  | Wrong e -> e = DotProd (ConstV [ 1.; 2. ], ConstS 1.)
  | _ -> false

let%test "eval_test40" =
  try
    let _ = eval (DotProd (T, ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = DotProd (T, ConstV [ 1.; 2. ])
  | _ -> false

let%test "eval_test41" = eval (Mag (ConstV [ 3.; 4. ])) = S 5.
let%test "eval_test42" = eval (Mag (ConstV [ 3.; 4.; 5.; 5.; 3.; 4. ])) = S 10.
let%test "eval_test43" = eval (Mag (ConstV [ 10.; 10.; 10.; 10. ])) = S 20.
let%test "eval_test44" = eval (Mag (ConstS 1.)) = S 1.

let%test "eval_test45" =
  try
    let _ = eval (Mag T) in
    false
  with
  | Wrong e -> e = Mag T
  | _ -> false

let%test "eval_test46" =
  try
    let _ = eval (Mag F) in
    false
  with
  | Wrong e -> e = Mag F
  | _ -> false

let%test "eval_test47" =
  eval (Angle (ConstV [ 1.; 2. ], ConstV [ 3.; 4. ]))
  = S (acos (11. /. (sqrt 25. *. sqrt 5.)))

let%test "eval_test48" =
  eval (Angle (ConstV [ 1.; 2.; 3.; 4. ], ConstV [ 3.; 4.; 5.; 6. ]))
  = S (acos (50. /. (sqrt 30. *. sqrt 86.)))

let%test "eval_test49" =
  eval (Angle (ConstV [ 1.; 2.; 3.; 4.; 5. ], ConstV [ 3.; 4.; 5.; 6.; 7. ]))
  = S (acos (85. /. (sqrt 55. *. sqrt 135.)))

let%test "eval_test50" =
  try
    let _ = eval (Angle (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])) in
    false
  with
  | Wrong e -> e = Angle (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])
  | _ -> false

let%test "eval_test51" =
  try
    let _ = eval (Angle (ConstV [ 1.; 2. ], ConstS 1.)) in
    false
  with
  | Wrong e -> e = Angle (ConstV [ 1.; 2. ], ConstS 1.)
  | _ -> false

let%test "eval_test52" =
  try
    let _ = eval (Angle (T, ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = Angle (T, ConstV [ 1.; 2. ])
  | _ -> false

let%test "eval_test53" = eval (IsZero (ConstV [ 1.; 2. ])) = B false
let%test "eval_test54" = eval (IsZero (ConstV [ 1.; 2.; 3.; 4. ])) = B false
let%test "eval_test55" = eval (IsZero (ConstV [ 1.; 2.; 3.; 4.; 5. ])) = B false
let%test "eval_test56" = eval (IsZero (ConstV [ 0.; 0.; 0.; 0.; 0. ])) = B true
let%test "eval_test57" = eval (IsZero (ConstS 1.)) = B false
let%test "eval_test58" = eval (IsZero (ConstS 0.)) = B true
let%test "eval_test59" = eval (IsZero F) = B true
let%test "eval_test60" = eval (IsZero T) = B false
let%test "eval_test61" = eval (Cond (T, T, T)) = B true
let%test "eval_test62" = eval (Cond (T, F, F)) = B false
let%test "eval_test63" = eval (Cond (T, ConstS 1., ConstS 1.)) = S 1.

let%test "eval_test64" =
  eval (Cond (T, ConstV [ 1.; 2. ], ConstV [ 1.; 2. ])) = V [ 1.; 2. ]

let%test "eval_test65" =
  eval (Cond (T, ConstV [ 1.; 2.; 3.; 4. ], ConstV [ 1.; 2.; 3.; 4. ]))
  = V [ 1.; 2.; 3.; 4. ]

let%test "eval_test66" =
  try
    let _ = eval (Cond (T, ConstV [ 1.; 2. ], ConstS 1.)) in
    false
  with
  | Wrong e -> e = Cond (T, ConstV [ 1.; 2. ], ConstS 1.)
  | _ -> false

let%test "eval_test67" =
  try
    let _ = eval (Cond (T, ConstS 1., ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = Cond (T, ConstS 1., ConstV [ 1.; 2. ])
  | _ -> false

let%test "eval_test68" =
  try
    let _ = eval (Cond (T, ConstV [ 1.; 2. ], ConstV [ 1.; 2.; 3. ])) in
    false
  with
  | Wrong e -> e = Cond (T, ConstV [ 1.; 2. ], ConstV [ 1.; 2.; 3. ])
  | _ -> false

let%test "eval_test69" =
  try
    let _ = eval (Cond (ConstS 1., T, T)) in
    false
  with
  | Wrong e -> e = Cond (ConstS 1., T, T)
  | _ -> false

let%test "eval_test70" =
  eval (UnitVector (ConstV [ 1.; 2. ])) = V [ 1. /. sqrt 5.; 2. /. sqrt 5. ]

let%test "eval_test71" =
  eval (UnitVector (ConstV [ 1.; 2.; 3.; 4. ]))
  = V [ 1. /. sqrt 30.; 2. /. sqrt 30.; 3. /. sqrt 30.; 4. /. sqrt 30. ]

let%test "eval_test72" =
  eval (UnitVector (ConstV [ 1.; 2.; 3.; 4.; 5. ]))
  = V
      [
        1. *. (1. /. sqrt 55.);
        2. *. (1. /. sqrt 55.);
        3. *. (1. /. sqrt 55.);
        4. *. (1. /. sqrt 55.);
        5. *. (1. /. sqrt 55.);
      ]

let%test "eval_test73" =
  try
    let _ = eval (UnitVector (ConstS 1.)) in
    false
  with
  | Wrong e -> e = UnitVector (ConstS 1.)
  | _ -> false

let%test "eval_test74" =
  try
    let _ = eval (UnitVector T) in
    false
  with
  | Wrong e -> e = UnitVector T
  | _ -> false

let%test "eval_test75" =
  try
    let _ = eval (UnitVector F) in
    false
  with
  | Wrong e -> e = UnitVector F
  | _ -> false

let%test "eval_test76" = eval (ZeroVector (ConstV [ 1.; 2. ])) = V [ 0.; 0. ]

let%test "eval_test77" =
  eval (ZeroVector (ConstV [ 1.; 2.; 3.; 4. ])) = V [ 0.; 0.; 0.; 0. ]

let%test "eval_test78" =
  eval (ZeroVector (ConstV [ 1.; 2.; 3.; 4.; 5. ])) = V [ 0.; 0.; 0.; 0.; 0. ]

let%test "eval_test79" =
  try
    let _ = eval (ZeroVector (ConstS 1.)) in
    false
  with
  | Wrong e -> e = ZeroVector (ConstS 1.)
  | _ -> false

let%test "eval_test80" =
  try
    let _ = eval (ZeroVector T) in
    false
  with
  | Wrong e -> e = ZeroVector T
  | _ -> false

let%test "eval_test81" =
  try
    let _ = eval (ZeroVector F) in
    false
  with
  | Wrong e -> e = ZeroVector F
  | _ -> false

let%test "eval_test82" = eval (Sub (ConstS 1., ConstS 2.)) = S (-1.)

let%test "eval_test83" =
  eval (Sub (ConstV [ 1.; 2. ], ConstV [ 3.; 4. ])) = V [ -2.; -2. ]

let%test "eval_test84" =
  eval (Sub (ConstV [ 1.; 2.; 3.; 4. ], ConstV [ 3.; 4.; 5.; 6. ]))
  = V [ -2.; -2.; -2.; -2. ]

let%test "eval_test85" =
  try
    let _ = eval (Sub (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])) in
    false
  with
  | Wrong e -> e = Sub (ConstV [ 1.; 2. ], ConstV [ 3.; 4.; 5. ])
  | _ -> false

let%test "eval_test86" =
  try
    let _ = eval (Sub (ConstV [ 1.; 2. ], ConstS 1.)) in
    false
  with
  | Wrong e -> e = Sub (ConstV [ 1.; 2. ], ConstS 1.)
  | _ -> false

let%test "eval_test87" =
  try
    let _ = eval (Sub (T, ConstV [ 1.; 2. ])) in
    false
  with
  | Wrong e -> e = Sub (T, ConstV [ 1.; 2. ])
  | _ -> false

let%test "eval_test88" =
  try
    let _ = eval (Angle (ConstV [ 1.; 2. ], ConstV [ 0.; 0. ])) in
    false
  with
  | Wrong e -> e = Angle (ConstV [ 1.; 2. ], ConstV [ 0.; 0. ])
  | _ -> 
    false

let fin () = print_endline "All tests passed"
