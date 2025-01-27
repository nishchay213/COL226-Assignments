open Assignment1.Vector

let test_create () = 
  if testcase1_create ()&& testcase2_create() && testcase3_create() && testcase4_create() && testcase5_create() && testcase6_create() && testcase7_create() then
    print_endline "test_create passed"
  else
    print_endline "test_create failed";;

let test_dim () =
  if testcase1_dim() && testcase2_dim() && testcase3_dim() && testcase4_dim() && testcase5_dim() then
    print_endline "test_dim passed"
  else
    print_endline "test_dim failed";;

let test_is_zero () =
  if testcase1_is_zero() && testcase2_is_zero() && testcase3_is_zero() && testcase4_is_zero() && testcase5_is_zero() then
    print_endline "test_is_zero passed"
  else
    print_endline "test_is_zero failed";;

let test_unit () =
  if testcase1_unit() && testcase2_unit() && testcase3_unit() && testcase4_unit() && testcase5_unit() && testcase6_unit() && testcase7_unit() && testcase8_unit() then
    print_endline "test_unit passed"
  else
    print_endline "test_unit failed";;

let test_scale () =
  if testcase1_scale() && testcase2_scale() && testcase3_scale() && testcase4_scale() && testcase5_scale() then
    print_endline "test_scale passed"
  else
    print_endline "test_scale failed";;

let test_addv () =
  if testcase1_addv() && testcase2_addv() && testcase3_addv() && testcase4_addv() && testcase5_addv()  then
    print_endline "test_addv passed"
  else
    print_endline "test_addv failed";;

let test_dot_prod () =
  if testcase1_dot_prod() && testcase2_dot_prod() && testcase3_dot_prod()&& testcase4_dot_prod() && testcase5_dot_prod() then
    print_endline "test_dot_prod passed"
  else
    print_endline "test_dot_prod failed";;

let test_inv () =
  if testcase1_inv() && testcase2_inv() && testcase3_inv() && testcase4_inv() && testcase5_inv() then
    print_endline "test_inv passed"
  else
    print_endline "test_inv failed";;

let test_length () =
  if testcase1_length() && testcase2_length() && testcase3_length() && testcase4_length() && testcase5_length() then
    print_endline "test_length passed"
  else
    print_endline "test_length failed";;

let test_angle () =
  if testcase1_angle() && testcase2_angle() && testcase3_angle() && testcase4_angle() && testcase5_angle() then
    print_endline "test_angle passed"
  else
    print_endline "test_angle failed";;

let () =
  test_create ();
  test_dim ();
  test_is_zero ();
  test_unit ();
  test_scale ();
  test_addv ();
  test_dot_prod ();
  test_inv ();
  test_length ();
  test_angle ();
  ;;  
  



