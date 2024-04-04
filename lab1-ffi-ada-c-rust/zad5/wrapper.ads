with Common; use Common;
with Optional; use Optional;
with Interfaces.C; use Interfaces.C;

package Wrapper is
    subtype short is Interfaces.C.short;
    subtype int is Interfaces.C.int;
    subtype long is Interfaces.C.long;

    function C_Lin_Factorial(N: in short) return C_Opt_I64
    with
        Import        => True,
        Convention    => C,
        External_Name => "iter_factorial";

    function C_Lin_Gcd(A_Arg: in int; B_Arg: in int) return C_Opt_I32
    with
        Import        => True,
        Convention    => C,
        External_Name => "iter_gcd";

    procedure C_Lin_Solve(C_Dio_Eq: in C_Diophantine_Eq; C_Dio_Sol: in out C_Diophantine_Sol)
    with
        Import        => True,
        Convention    => C,
        External_Name => "iter_dio_solve";

    function C_Rec_Factorial(N: in short) return C_Opt_I64
    with
        Import        => True,
        Convention    => C,
        External_Name => "rec_factorial";

    function C_Rec_Gcd(A_Arg: in int; B_Arg: in int) return C_Opt_I32
    with
        Import        => True,
        Convention    => C,
        External_Name => "rec_gcd";

    procedure C_Rec_Solve(C_Dio_Eq: in C_Diophantine_Eq; C_Dio_Sol: in out C_Diophantine_Sol)
    with
        Import        => True,
        Convention    => C,
        External_Name => "rec_dio_solve";
end Wrapper;