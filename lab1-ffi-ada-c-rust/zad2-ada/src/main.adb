with Ada.Text_IO;
with Common; use Common;
with Iter; use Iter;
with Rec; use Rec;

procedure Main is
   A : Integer := 25;
   B : Integer := 10;
   Eq : Diophantine_Eq := (X_K => 87, Y_K => -64, C => 3);
   Sol : Diophantine_Sol;
begin
   Rec_Dio_Solve(Eq, Sol);

   Ada.Text_IO.Put_Line(Long_Integer'Image(Rec_Factorial(20).Value));
   Ada.Text_IO.Put_Line(Integer'Image(Rec_Gcd(25, 10).Value));
   Ada.Text_IO.Put_Line ("X_K: " & Integer'Image(Sol.X_K));
   Ada.Text_IO.Put_Line ("X_A: " & Integer'Image(Sol.X_A));
   Ada.Text_IO.Put_Line ("Y_K: " & Integer'Image(Sol.Y_K));
   Ada.Text_IO.Put_Line ("Y_A: " & Integer'Image(Sol.Y_A));
end Main;