with Blake3;
with Ada.Text_IO;

procedure Blake3Hello is
	package Output_Formatter is new Ada.Text_IO.Integer_IO(Integer);
 	Ctx: Blake3.Hasher := Blake3.Init;
begin
 	Ctx.Update("Hello world.");
	declare
		Result: constant String := Ctx.Final;
		Hex:    String          := "16#00#";
	begin
		for I in Result'Range loop
			Output_Formatter.Put(Hex, Character'Pos(Result(I)), 16);
			Ada.Text_IO.Put(if Hex(1) = ' ' then ("0" & Hex(5 .. 5))
							else Hex(4 .. 5));
		end loop;
	end;
end Blake3Hello;
