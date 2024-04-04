with Interfaces.C;

package Common is
    subtype int is Interfaces.C.int;

    type C_Diophantine_Eq is record
        x_k : int;
        y_k : int;
        c   : int;
    end record;
    pragma Convention (C, C_Diophantine_Eq);
    pragma Pack (C_Diophantine_Eq);

    type C_Diophantine_Sol is record
        x_k : int;
        x_a : int;
        y_k : int;
        y_a : int;
        is_present : Boolean;
    end record;
    pragma Convention (C, C_Diophantine_Sol);
    pragma Pack (C_Diophantine_Sol);

    type Diophantine_Eq is record
        x_k : Integer;
        y_k : Integer;
        c   : Integer;
    end record;

    type Diophantine_Sol is record
        x_k : Integer;
        x_a : Integer;
        y_k : Integer;
        y_a : Integer;
        is_present : Boolean;
    end record;

    type Diophantine_Sol_Access is access all Diophantine_Sol;
    type Diophantine_Eq_Access is access all Diophantine_Eq;
end Common;