with Ada.Assertions;
use  Ada.Assertions;
with Ada.Text_IO;
use  Ada.Text_IO;
with Interfaces;
use  Interfaces;

with Blake3;
use  Blake3;

--with Ada.Text_IO.Text_Streams;
--with Ada.Streams;

-- ./blake3test < ../test_vectors/test_vectors.csv

procedure Blake3Test is

	pragma Assertion_Policy(Pre => Check, Post => Check);

	Key: constant String := "whats the Elvish word for friend";

	function Shift_Right(Value: in U8; Amount: in Natural) return U8 is
		(U8(Interfaces.Shift_Right(Interfaces.Unsigned_8(Value),
		Amount)));

	function Bin_To_Hex(Bin: in Octets) return String is
		Result: String(1 .. Bin'Length * 2);
		Output_Index: Natural := Result'First;
		Lower_Nibble: Integer;
		Upper_Nibble: Integer;
		Nibble_To_Hex: constant String := "0123456789abcdef";
	begin
		for I in Bin'Range loop
			Lower_Nibble := Integer(Bin(I) and 16#0f#);
			Upper_Nibble := Integer(
					Shift_Right(Bin(I) and 16#f0#, 4));
			Result(Output_Index) := Nibble_To_Hex(Upper_Nibble +
							Nibble_To_Hex'First);
			Output_Index := Output_Index + 1;
			Result(Output_Index) := Nibble_To_Hex(Lower_Nibble +
							Nibble_To_Hex'First);
			Output_Index := Output_Index + 1;
		end loop;
		return Result;
	end Bin_To_Hex;

	procedure Process_Test_Vector(Length: Integer; Hash: in Octets;
							Keyed: in Octets) is
		Regular_Ctx: Blake3.Hasher := Blake3.Init;
		Keyed_Ctx:   Blake3.Hasher := Blake3.Init(Key);

		Result_Regular: Octets(0 .. Hash'Length - 1) := (others => 0);
		Result_Keyed:   Octets(0 .. Keyed'Length - 1) := (others => 0);
	begin
		if Length /= 0 then
			declare
				Data: Octets(0 .. U32(Length - 1));
			begin
				for I in Data'Range loop
					Data(I) := U8(I mod 251);
				end loop;

				Regular_Ctx.Update(Data);
				-- TODO DEBUG ONLY
				--Keyed_Ctx.Update(Data);
			end;
		end if;

		Regular_Ctx.Final(Result_Regular);
		--Keyed_Ctx.Final(Result_Keyed);

		Put_Line("TEST COMPLETED W LENGTH = " & Integer'Image(Length));

		for I in Hash'Range loop
			if Hash(I) /= Result_Regular(I) then
				Put_Line("REGULAR MISMATCH @I = " & U32'Image(I));
			else
				Put_Line("              ok @I = " & U32'Image(I));
			end if;
		end loop;

		if Result_Regular = Hash and Result_Keyed = Keyed then
			Put_Line("[ OK ] Len = " & Integer'Image(Length));
		else
			Put_Line("[FAIL] Len = " & Integer'Image(Length) & " Expected=(" & Bin_To_Hex(Hash) & ", " & Bin_To_Hex(Keyed) & "), Got=(" & Bin_To_Hex(Result_Regular) & ", " & Bin_To_Hex(Result_Keyed) & ")");
			raise Constraint_Error with "TODO DEBUG TEST FAILED";
		end if;
	end Process_Test_Vector;

	function Hex_To_Bin(Hex: in String) return Octets
					with Pre => (Hex'Length mod 2 = 0) is
		Result: Octets(0 .. Hex'Length / 2 - 1);
	begin
		for I in Result'Range loop
			Result(I) := U8'Value("16#" &
					Hex(Hex'First + Integer(I * 2) ..
					Hex'First + Integer(I * 2 + 1)) & "#");
		end loop;
		return Result;
	end Hex_To_Bin;

	procedure Process_Test_Vector(Line: in String) is
		Length:       Integer := -1;
		Hash_Offset:  Integer := -1;
		Keyed_Offset: Integer := -1;
	begin
		-- Ad hoc CSV parser
		for I in Line'Range loop
			if Line(I) = ',' then
				if Length = -1 then
					Length := Integer'Value(
						Line(Line'First .. I - 1));
					Hash_Offset := I + 1;
				elsif Keyed_Offset = -1 then
					Keyed_Offset := I + 1;
				else
					raise Constraint_Error with
						"Found unknown trailing data: "
						& Line(I .. Line'Last);
				end if;
			end if;
		end loop;
		Assert(Keyed_Offset /= -1,
				"Keyed reference data not found. Length=" &
				Integer'Image(Length) & ", Hash_Offset=" &
				Integer'Image(Hash_Offset));
		declare
			Regular: constant String := Line(Hash_Offset .. Keyed_Offset - 2);
			Keyed:   constant String := Line(Keyed_Offset .. Line'Last);
		begin
			Ada.Text_IO.Put_Line("Data len = " & Integer'Image(Length) & ", regular len = " & Integer'Image(Regular'Length) & " Keyed Len = " & Integer'Image(Keyed'Length) & " regular val = <" & Regular & ">");
			Process_Test_Vector(Length, Hex_To_Bin(Regular),
							Hex_To_Bin(Keyed));
		end;
	end Process_Test_Vector;

	procedure Test_With_Test_Vectors is
	begin
		loop
			declare
				Line: constant String := Get_Line;
			begin
				Process_Test_Vector(Line);
			end;
		end loop;
	exception
		when End_Error => null;
	end Test_With_Test_Vectors;

begin

	Test_With_Test_Vectors;

end Blake3Test;
