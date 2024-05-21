with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_IO;               use Ada.Text_IO;

procedure Dining_Philosophers is
    protected type Fork is
        entry Lock;
        procedure Unlock;
    private
        Occupied : Boolean := False;
    end Fork;

    protected body Fork is
        entry Lock when not Occupied is
        begin
            Occupied := True;
        end Lock;
        procedure Unlock is
        begin
            Occupied := False;
        end Unlock;
    end Fork;

    Dish_Count : constant := 3;
    Philosopher_Count : constant := 5;

    task type Person(ID: Natural; Left_Fork, Right_Fork : access Fork);
    task body Person is
        Uniform : Generator;
    begin
        Reset (Uniform);
        for I in 1..Dish_Count loop
            Put_Line ("Philosopher #" & Integer'Image(ID) & " is thinking");
            delay Duration (Random (Uniform) * 1.0);
            Put_Line ("Philosopher #" & Integer'Image(ID) & " is hungry");
            Left_Fork.Lock;
            Right_Fork.Lock;
            Put_Line ("Philosopher #" & Integer'Image(ID) & " is eating");
            delay Duration (Random (Uniform) * 1.0);
            Right_Fork.Unlock;
            Left_Fork.Unlock;
        end loop;
            Put_Line ("Philosopher #" & Integer'Image(ID) & " is leaving");
    end Person;

    Forks : array (1..Philosopher_Count) of aliased Fork;
    type Philosopher_Access_Array is array (Natural range <>) of access Person;
    Philosophers : Philosopher_Access_Array(1..Philosopher_Count);
begin
    for I in 1..Philosopher_Count loop
        Philosophers(I) := new Person(I, Forks(I)'Access, Forks(I mod Philosopher_Count + 1)'Access);
    end loop;
end Dining_Philosophers;