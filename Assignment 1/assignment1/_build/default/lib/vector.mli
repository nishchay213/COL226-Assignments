
type vector = float list
val create : int -> float -> vector
val dim : vector -> int
val is_zero : vector -> bool
val unit : int -> int -> vector
val scale : float -> vector -> vector
val addv : vector -> vector -> vector
val dot_prod : vector -> vector -> float
val inv : vector -> vector
val length : vector -> float
val angle : vector -> vector -> float

val testcase1_create : unit -> bool
val testcase2_create : unit -> bool
val testcase3_create : unit -> bool
val testcase4_create : unit -> bool
val testcase5_create : unit -> bool
val testcase6_create : unit -> bool
val testcase7_create : unit -> bool

val testcase1_dim : unit -> bool
val testcase2_dim : unit -> bool
val testcase3_dim : unit -> bool
val testcase4_dim : unit -> bool
val testcase5_dim : unit -> bool

val testcase1_is_zero : unit -> bool
val testcase2_is_zero : unit -> bool
val testcase3_is_zero : unit -> bool
val testcase4_is_zero : unit -> bool
val testcase5_is_zero : unit -> bool

val testcase1_unit : unit -> bool
val testcase2_unit : unit -> bool
val testcase3_unit : unit -> bool
val testcase4_unit : unit -> bool
val testcase5_unit : unit -> bool
val testcase6_unit : unit -> bool
val testcase7_unit : unit -> bool
val testcase8_unit : unit -> bool

val testcase1_scale : unit -> bool
val testcase2_scale : unit -> bool
val testcase3_scale : unit -> bool
val testcase4_scale : unit -> bool
val testcase5_scale : unit -> bool

val testcase1_addv : unit -> bool
val testcase2_addv : unit -> bool
val testcase3_addv : unit -> bool
val testcase4_addv : unit -> bool
val testcase5_addv : unit -> bool

val testcase1_dot_prod : unit -> bool
val testcase2_dot_prod : unit -> bool
val testcase3_dot_prod : unit -> bool
val testcase4_dot_prod : unit -> bool
val testcase5_dot_prod : unit -> bool

val testcase1_inv : unit -> bool
val testcase2_inv : unit -> bool
val testcase3_inv : unit -> bool
val testcase4_inv : unit -> bool
val testcase5_inv : unit -> bool

val testcase1_length : unit -> bool
val testcase2_length : unit -> bool
val testcase3_length : unit -> bool
val testcase4_length : unit -> bool
val testcase5_length : unit -> bool

val testcase1_angle : unit -> bool
val testcase2_angle : unit -> bool
val testcase3_angle : unit -> bool
val testcase4_angle : unit -> bool
val testcase5_angle : unit -> bool

