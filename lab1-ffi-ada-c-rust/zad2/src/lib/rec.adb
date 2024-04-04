with Hidden; use Hidden;

package body Rec is
    type Integer_Access is access all Integer;
    
    function Rec_Factorial_Impl (N_Arg: in Short_Integer) return Long_Integer is
    begin
        if (N_Arg = 0) then
            return 1;
        end if;

        return Long_Integer(N_Arg) * Rec_Factorial_Impl(N_Arg - 1);
    end Rec_Factorial_Impl;

    function Rec_Factorial (N_Arg: in Short_Integer) return Opt_I64 is
        Result: Opt_I64 := (Value => Standard.Long_Integer'First, Is_Present => False);
    begin
        if (N_Arg < 0 or N_Arg > 20) then
            return Result;
        end if;

        Result.Value := Rec_Factorial_Impl(N_Arg);
        Result.Is_Present := True;
        return Result;
    end Rec_Factorial;


    function Rec_Gcd_Impl (A_Arg: in Integer; B_Arg: in Integer) return Integer is
    begin
        if (A_Arg = 0) then
            return B_Arg;
        end if;

        return Rec_Gcd_Impl(B_Arg mod A_Arg, A_Arg);
    end Rec_Gcd_Impl;

    function Rec_Gcd (A_Arg: in Integer; B_Arg: in Integer) return Opt_I32 is
        Result : Opt_I32 := (Is_Present => False, Value => Standard.Integer'First);
        A : Integer := abs(A_Arg);
        B : Integer := abs(B_Arg);
        X : Integer := 0;
        Y : Integer := 0;
    begin
        if (A = 0 or B = 0) then
            return Result;
        end if;

        Result.Value := Rec_Gcd_Impl(A, B);
        Result.Is_Present := True;
        return Result;
    end Rec_Gcd;

    function Rec_Ext_Euclid_Impl(A: Integer; B: Integer; X: in out Integer; Y: in out Integer) return Integer is
        X1 : Integer := 0;
        Y1 : Integer := 0;
        Gcd : Integer;
    begin
        if (a = 0) then
            X := 0;
            Y := 1;
            return B;
        end if;

        Gcd := Rec_Ext_Euclid_Impl(B rem A, A, X1, Y1);
        X := Y1 - (B / A) * X1;
        Y := X1;
        return Gcd;
    end Rec_Ext_Euclid_Impl;


    function Rec_Ext_Euclid (Dio_Eq : in Diophantine_Eq) return Euklidean_Sol_Access is
        X : Integer := 0;
        Y : Integer := 0;
        Gcd : Integer;
        D : Euklidean_Sol_Access;
    begin
        Gcd := Rec_Ext_Euclid_Impl(Dio_Eq.X_K, Dio_Eq.Y_K, X, Y);
        D := new Euklidean_Sol'(X_K => X * Gcd, Y_K => Y * Gcd);
        return D;
    end Rec_Ext_Euclid;


    procedure Rec_Dio_Solve(Eq: in Diophantine_Eq; Sol: in out Diophantine_Sol) is
        Gcd : Opt_I32;
        Eq_Copy : Diophantine_Eq;
        Eu_Sol_Access : Euklidean_Sol_Access;
    begin
        Sol.X_A := Integer'First;
        Sol.X_K := Integer'First;
        Sol.Y_A := Integer'First;
        Sol.Y_K := Integer'First;
        Sol.Is_Present := False;
        Gcd := Rec_Gcd(Rec_Gcd(Eq.X_K, Eq.Y_K).Value, Eq.C);
        Eq_Copy := Eq;

        if ((not Gcd.Is_Present) or Eq.C mod Gcd.Value /= 0) then return;
        end if;

        Eq_Copy.X_K := Eq.X_K / Gcd.Value;
        Eq_Copy.Y_K := Eq.Y_K / Gcd.Value;
        Eq_Copy.C := Eq.C / Gcd.Value;
        Eu_Sol_Access := Rec_Ext_Euclid(Eq_Copy);

        Sol.X_K := Eq_Copy.Y_K;
        Sol.X_A := Eu_Sol_Access.X_K * Eq_Copy.C;
        Sol.Y_K := -Eq_Copy.X_K;
        Sol.Y_A := Eu_Sol_Access.Y_K * Eq_Copy.C;
        Sol.Is_Present := True;
    end Rec_Dio_Solve;
end Rec;