with Interfaces.C; use Interfaces.C;

package Optional is
    type C_Opt_I32 is record
        Value : Interfaces.C.int;
        Is_Present : Boolean;
    end record;
    pragma Convention (C, C_Opt_I32);
    pragma Pack (C_Opt_I32);

    type C_Opt_I64 is record
        Value : Interfaces.C.long;
        Is_Present : Boolean;
    end record;
    pragma Convention (C, C_Opt_I64);
    pragma Pack (C_Opt_I64);
end Optional;