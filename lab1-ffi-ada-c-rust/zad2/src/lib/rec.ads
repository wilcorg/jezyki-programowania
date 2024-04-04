with Common; use Common;
with Optional; use Optional;

package Rec is
    function Rec_Factorial(N_Arg: in Short_Integer) return Opt_I64;
    function Rec_Gcd(A_Arg: in Integer; B_Arg: in Integer) return Opt_I32;
    procedure Rec_Dio_Solve(Eq: in Diophantine_Eq; Sol: in out Diophantine_Sol);
end Rec;