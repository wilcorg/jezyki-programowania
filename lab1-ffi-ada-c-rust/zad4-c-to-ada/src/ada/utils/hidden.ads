with Common; use Common;

package Hidden is
    type Euklidean_Sol is record
        x_k : Integer;
        y_k : Integer;
    end record;
    pragma Convention (C, Euklidean_Sol);
    pragma Pack (Euklidean_Sol);

    type Euklidean_Sol_Access is access all Euklidean_Sol;
end Hidden;