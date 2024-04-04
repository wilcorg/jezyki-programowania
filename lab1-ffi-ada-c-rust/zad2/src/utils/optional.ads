package Optional is
    type Opt_I32 is record
        Value : Integer;
        Is_Present : Boolean;
    end record;

    type Opt_I64 is record
        Value : Long_Integer;
        Is_Present : Boolean;
    end record;
end Optional;