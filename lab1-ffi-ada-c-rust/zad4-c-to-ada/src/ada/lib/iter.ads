with Common; use Common;
with Optional; use Optional;
with Interfaces.C; use Interfaces.C;

package Iter is
    subtype short is Interfaces.C.short;
    subtype int is Interfaces.C.int;
    subtype long is Interfaces.C.long;
    
    function Iter_Factorial(N_Arg: in Short_Integer) return Opt_I64;

    function C_Iter_Factorial(N_Arg: in short) return C_Opt_I64
    with
        Export        => True,
        Convention    => C,
        External_Name => "iter_factorial";

    function Iter_Gcd(A_Arg: in Integer; B_Arg: in Integer) return Opt_I32;

    function C_Iter_Gcd(A_Arg: in int; B_Arg: in int) return C_Opt_I32
    with
        Export        => True,
        Convention    => C,
        External_Name => "iter_gcd";

    procedure Iter_Dio_Solve(Eq: in Diophantine_Eq; Sol: in out Diophantine_Sol);

    procedure C_Iter_Dio_Solve(C_Eq: in C_Diophantine_Eq; C_Sol: in out C_Diophantine_Sol)
    with
        Export        => True,
        Convention    => C,
        External_Name => "iter_dio_solve";
end Iter;