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
    Lock_Timeout_Sec : constant := 1.0;

    task type Person(ID: Natural; Left_Fork, Right_Fork : access Fork);
    task body Person is
        Uniform : Generator;
        Right_Fork_Locked : Boolean := False;
        I : Natural := 1; 
    begin
        Reset (Uniform);
        while I <= Dish_Count loop
            Put_Line ("Philosopher #" & Integer'Image(ID) & " is thinking");
            delay Duration (Random (Uniform) * 1.0);

            Put_Line ("Philosopher #" & Integer'Image(ID) & " is hungry");
            Left_Fork.Lock;

            -- select-or is similar to try-catch clause: if right fork didn't
            -- lock in timeout seconds, run code below or keyword
            -- unlike Go/Rust, Ada doesn't lock/unlock mutex in statements automatically
            select
                Right_Fork.Lock;
                Right_Fork_Locked := True;
            or
                delay Lock_Timeout_Sec;
                Right_Fork_Locked := False;
                delay 0.1;
            end select;

            if Right_Fork_Locked then
                Put_Line ("Philosopher #" & Integer'Image(ID) & " is eating");
                delay Duration (Random (Uniform) * 1.0);

                I := I + 1;
                Right_Fork.Unlock;
                Left_Fork.Unlock;
                Right_Fork_Locked := False;
            else
                Put_Line ("Philosopher #" & Integer'Image(ID) & " forgot why they took a fork and put it back");
                Left_Fork.Unlock;
            end if;

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
