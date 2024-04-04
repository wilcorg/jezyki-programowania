package Common is
    type Diophantine_Eq is record
        X_K : Integer;
        Y_K : Integer;
        C   : Integer;
    end record;

    type Diophantine_Sol is record
        X_K : Integer;
        X_A : Integer;
        Y_K : Integer;
        Y_A : Integer;
        Is_Present : Boolean;
    end record;

    type Diophantine_Sol_Access is access all Diophantine_Sol;
    type Diophantine_Eq_Access is access all Diophantine_Eq;
end Common;