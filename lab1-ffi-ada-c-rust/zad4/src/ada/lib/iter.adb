with Hidden; use Hidden;
with Interfaces.C; use Interfaces.C;

package body Iter is
    function Iter_Factorial (N_Arg: in Short_Integer) return Opt_I64 is
        Result: Opt_I64 := (Value => Standard.Long_Integer'First, Is_Present => False);
    begin
        if (N_Arg < 0 or N_Arg > 20) then
            return Result;
        elsif (N_Arg = 0) then
            Result.Value := 1;
            Result.Is_Present := True;
            return Result;
        end if;

        Result.Value := 1;
        for I in 1 .. Long_Integer (N_Arg) loop
            Result.Value := Result.Value * I;
        end loop;
        Result.Is_Present := True;
        return Result;
    end Iter_Factorial;

    function Iter_Gcd (A_Arg : in Integer; B_Arg : in Integer) return Opt_I32 is
        Result : Opt_I32 :=
           (Is_Present => False, Value => Standard.Integer'First);
        A      : Integer := abs (A_Arg);
        B      : Integer := abs (B_Arg);
        Temp   : Integer := B;
    begin
        if (A = 0 or B = 0) then
            return Result;
        end if;

        while B /= 0 loop
            Temp := B;
            B    := A mod B;
            A    := Temp;
        end loop;
        Result.Value      := A;
        Result.Is_Present := True;
        return Result;
    end Iter_Gcd;

    function Iter_Ext_Euclid(Eq_Arg : in Diophantine_Eq) return Euklidean_Sol_Access is
        Old_R    : Integer := Eq_Arg.X_K;
        R        : Integer := Eq_Arg.Y_K;
        Old_S    : Integer := 1;
        S        : Integer := 0;
        Old_T    : Integer := 0;
        T        : Integer := 1;
        Temp     : Integer := R;
        Quotient : Integer := Old_R / R;
        D        : Euklidean_Sol_Access;
    begin
        while R /= 0 loop
            Quotient := Old_R / R;

            Temp  := R;
            R     := Old_R - Quotient * Temp;
            Old_R := Temp;

            Temp  := S;
            S     := Old_S - Quotient * Temp;
            Old_S := Temp;

            Temp  := T;
            T     := Old_T - Quotient * Temp;
            Old_T := Temp;
        end loop;

        D := new Euklidean_Sol'(X_K => Old_S * Old_R, Y_K => Old_T * Old_R);
        return D;
    end Iter_Ext_Euclid;

    procedure Iter_Dio_Solve(Eq: in Diophantine_Eq; Sol: in out Diophantine_Sol) is
        Gcd : Opt_I32;
        Eq_Copy : Diophantine_Eq;
        Eu_Sol_Access : Euklidean_Sol_Access;
    begin
        Sol.X_A := Integer'First;
        Sol.X_K := Integer'First;
        Sol.Y_A := Integer'First;
        Sol.Y_K := Integer'First;
        Sol.Is_Present := False;
        Gcd := Iter_Gcd(Iter_Gcd(Eq.X_K, Eq.Y_K).Value, Eq.C);
        Eq_Copy := Eq;

        if ((not Gcd.Is_Present) or Eq.C mod Gcd.Value /= 0) then return;
        end if;

        Eq_Copy.X_K := Eq.X_K / Gcd.Value;
        Eq_Copy.Y_K := Eq.Y_K / Gcd.Value;
        Eq_Copy.C := Eq.C / Gcd.Value;
        Eu_Sol_Access := Iter_Ext_Euclid(Eq_Copy);

        Sol.X_K := Eq_Copy.Y_K;
        Sol.X_A := Eu_Sol_Access.X_K * Eq_Copy.C;
        Sol.Y_K := -Eq_Copy.X_K;
        Sol.Y_A := Eu_Sol_Access.Y_K * Eq_Copy.C;
        Sol.Is_Present := True;
    end Iter_Dio_Solve;

    function C_Iter_Factorial(N_Arg: in short) return C_Opt_I64 is
        Input  : Short_Integer := Short_Integer(N_Arg);
        Output : Opt_I64 := Iter_Factorial(Input);
        Result : C_Opt_I64 := (Value => long(Output.Value), Is_Present => Output.Is_Present);
    begin
        return Result;
    end C_Iter_Factorial;

    function C_Iter_Gcd(A_Arg: in int; B_Arg: in int) return C_Opt_I32 is
        Input_A  : Integer := Integer(A_Arg);
        Input_B  : Integer := Integer(B_Arg);
        Output : Opt_I32 := Iter_Gcd(Input_A, Input_B);
        Result : C_Opt_I32 := (Value => int(Output.Value), Is_Present => Output.Is_Present);
    begin
        return Result;
    end C_Iter_Gcd;

    procedure C_Iter_Dio_Solve(C_Eq: in C_Diophantine_Eq; C_Sol: in out C_Diophantine_Sol) is
        Input : Diophantine_Eq := (x_k => Integer(C_Eq.x_k), y_k => Integer(C_Eq.y_k), c => Integer(C_Eq.c));
        Output : Diophantine_Sol;
    begin
        Iter_Dio_Solve(Input, Output);
        C_Sol.x_k := int(Output.x_k);
        C_Sol.x_a := int(Output.x_a);
        C_Sol.y_k := int(Output.y_k);
        C_Sol.y_a := int(Output.y_a);
        C_Sol.is_present := Output.is_present;
    end;
end Iter;