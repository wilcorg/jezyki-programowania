with Common; use Common;
with Optional; use Optional;
with Wrapper; use Wrapper;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
    Eq : C_Diophantine_Eq := (x_k => int(87), y_k => int(-64), c => int(3));
    Sol : C_Diophantine_Sol;
begin
    Ada.Text_IO.Put_Line(long'Image(C_Rec_Factorial(short(10)).Value));
    Ada.Text_IO.Put_Line(int'Image(C_Rec_Gcd(int(10), int(20)).Value));

    C_Lin_Solve(Eq, Sol);
    Ada.Text_IO.Put_Line(int'Image(Sol.x_k));
    Ada.Text_IO.Put_Line(int'Image(Sol.x_a));
    Ada.Text_IO.Put_Line(int'Image(Sol.y_k));
    Ada.Text_IO.Put_Line(int'Image(Sol.y_a));
end Main;