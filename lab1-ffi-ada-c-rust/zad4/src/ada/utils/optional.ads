with Interfaces.C; use Interfaces.C;

package Optional is
    subtype int is Interfaces.C.int;
    subtype long is Interfaces.C.long;

    type C_Opt_I32 is record
        Value : int;
        Is_Present : Boolean;
    end record;
    pragma Convention (C, C_Opt_I32);
    pragma Pack (C_Opt_I32);

    type C_Opt_I64 is record
        Value : long;
        Is_Present : Boolean;
    end record;
    pragma Convention (C, C_Opt_I64);
    pragma Pack (C_Opt_I64);

    type Opt_I32 is record
        Value : Integer;
        Is_Present : Boolean;
    end record;

    type Opt_I64 is record
        Value : Long_Integer;
        Is_Present : Boolean;
    end record;
end Optional;