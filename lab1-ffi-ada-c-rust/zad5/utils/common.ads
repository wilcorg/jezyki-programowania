with Interfaces.C;

package Common is
    type C_Diophantine_Eq is record
        x_k : Interfaces.C.int;
        y_k : Interfaces.C.int;
        c   : Interfaces.C.int;
    end record;
    pragma Convention (C, C_Diophantine_Eq);
    pragma Pack (C_Diophantine_Eq);

    type C_Diophantine_Sol is record
        x_k : Interfaces.C.int;
        x_a : Interfaces.C.int;
        y_k : Interfaces.C.int;
        y_a : Interfaces.C.int;
        is_present : Boolean;
    end record;
    pragma Convention (C, C_Diophantine_Sol);
    pragma Pack (C_Diophantine_Sol);
end Common;