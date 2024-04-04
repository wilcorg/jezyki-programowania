package Hidden is
    type Euklidean_Sol is record
        X_K : Integer;
        Y_K : Integer;
    end record;

    type Euklidean_Sol_Access is access all Euklidean_Sol;
end Hidden;