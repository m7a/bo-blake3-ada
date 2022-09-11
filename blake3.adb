with Ada.Text_IO;
with Ada.Assertions;

package body Blake3 is

	-- Public API

	-- No New_Internal function since this causes problems regarding
	-- dispatch
	function Init return Hasher is
				(Chunk_State => New_Chunk_State(IV, 0, 0),
				Key_Words    => IV,
				CV_Stack     => (others => (others => 0)),
				CV_Stack_Len => 0,
				Flags        => 0);

	procedure Update(Self: in out Hasher; Input: in String) is
		Conv: Octets(0 .. Input'Length - 1);
		for Conv'Address use Input'Address;
	begin
		-- this slows down the processing a whole lot, skip as long as
		-- it works this way...
		-- for I in Conv'Range loop
		-- 	Conv(I) := U8(Character'Pos(Input(Input'First +
		-- 						Integer(I))));
		-- end loop;
		Self.Update(Conv);
	end Update;

	function Final(Self: in Hasher) return String is
		Conv:   Octets(0 .. 31);
		Result: String(1 .. 32);
		for Result'Address use Conv'Address;
	begin
		Self.Final(Conv);
		--for I in Result'Range loop
		--	Result(I) := Character'Val(Conv(U32(I - Result'First)));
		--end loop;
		return Result;
	end Final;

	procedure Final(Self: in Hasher; Out_Slice: out Octets) is
		Output:                 Output_T := Self.Chunk_State.Output;
		Parent_Nodes_Remaining: U8       := Self.CV_Stack_Len;
	begin
		while Parent_Nodes_Remaining > 0 loop
			Parent_Nodes_Remaining := Parent_Nodes_Remaining - 1;
			Output := Parent_Output(
				Self.CV_Stack(Parent_Nodes_Remaining),
				Chaining_Value(Output),
				Self.Key_Words,
				Self.Flags
			);
		end loop;
		Root_Output_Bytes(Output, Out_Slice);
	end Final;

	-- Generic Auxiliary API

	procedure Generic_Update(Self: in out Self_T; Input: in Octets) is
		Pos_In_Input:     U32 := Input'First;
		Available_Length: U32;
		Want:             U32;
		Take:             U32;
	begin
		loop
			Available_Length := Input'Length -
						(Pos_In_Input - Input'First);
			exit when Available_Length = 0;

			if State_Length(Self) = Comparison_Length then
				Process_Complete_Entity(Self);
			end if;

			Want := Comparison_Length - State_Length(Self);
			Take := U32'Min(Want, Available_Length);
			Ada.Assertions.Assert(Take > 0);

			Update_Inner(Self, Input(Pos_In_Input ..
						Pos_In_Input + Take - 1));
			Pos_In_Input := Pos_In_Input + Take;
		end loop;
	end Generic_Update;

	function Block_Length(Self: in Chunk_State_T) return U32 is
							(U32(Self.Block_Len));

	procedure Update_Chunk_State_Inner(Self: in out Chunk_State_T;
					Substr: in Octets) is
		Take: constant U32 := Substr'Length;
	begin
		Self.Block(U32(Self.Block_Len) .. U32(Self.Block_Len) +
							Take - 1) := Substr;
		Self.Block_Len := Self.Block_Len + U8(Take);
	end Update_Chunk_State_Inner;

	procedure Update_Chunk_State is new Generic_Update(
		Self_T                  => Chunk_State_T,
		Comparison_Length       => Block_Len,
		State_Length            => Block_Length,
		Process_Complete_Entity => Compress_Full_Block_Buffer,
		Update_Inner            => Update_Chunk_State_Inner
	);

	function Chunk_State_Length(Self: in Hasher) return U32 is
						(Self.Chunk_State.Length);

	procedure Update_Chunk_State(Self: in out Hasher; Input: in Octets) is
	begin
		Update_Chunk_State(Self.Chunk_State, Input);
	end;

	procedure Update_Hasher is new Generic_Update(
		Self_T                  => Hasher,
		Comparison_Length       => Chunk_Len,
		State_Length            => Chunk_State_Length,
		Process_Complete_Entity => Finalize_Complete_Chunk_Reset_State,
		Update_Inner            => Update_Chunk_State
	);

	procedure Update(Self: in out Hasher; Input: in Octets)
							renames Update_Hasher;

	-- Internal Functions

	procedure G(State: in out U32x16; A, B, C, D, MX, MY: in U32) is
	begin
		State(A) := State(A) + State(B) + MX;
		State(D) := Rotate_Right(State(D) xor State(A), 16);
		State(C) := State(C) + State(D);
		State(B) := Rotate_Right(State(B) xor State(C), 12);
		State(A) := State(A) + State(B) + MY;
		State(D) := Rotate_Right(State(D) xor State(A), 8);
		State(C) := State(C) + State(D);
		State(B) := Rotate_Right(State(B) xor State(C), 7);
	end G;

	procedure Round(State: in out U32x16; m: in U32x16) is
	begin
		G(State, 0, 4, 8,  12, M(0),  M(1));
		G(State, 1, 5, 9,  13, M(2),  M(3));
		G(State, 2, 6, 10, 14, M(4),  M(5));
		G(State, 3, 7, 11, 15, M(6),  M(7));

		G(State, 0, 5, 10, 15, M(8),  M(9));
		G(State, 1, 6, 11, 12, M(10), M(11));
		G(State, 2, 7, 8,  13, M(12), M(13));
		G(State, 3, 4, 9,  14, M(14), M(15));
	end Round;

	procedure Permute(M: in out U32x16) is
		Permuted: U32x16;
	begin
		for I in Permuted'Range  loop
			Permuted(I) := M(MSG_Permutation(I));
		end loop;
		M := Permuted;
	end Permute;

	function Counter_Low(XR: in U64) return U32 is
					(U32(XR and 16#ffffffff#));
	function Counter_High(XR: in U64) return U32 is
					(U32(Shift_Right(XR, 32)));

	-- PERF CRITICAL
	function Compress(Chaining_Value: in U32x8; Block_Words: in U32x16;
			Counter: in U64; Block_Len: in U32; Flags: in U32)
			return U32x16 is
		Tail: constant Words(0..3) := (Counter_Low(Counter),
				Counter_High(Counter), Block_Len, Flags);
		State: U32x16 := Words'(Chaining_Value & IV(0..3) & Tail);
		Block: U32x16 := Block_Words;
	begin
		for I in 1 .. 7 loop
			Round(State, Block); -- round I
			Permute(Block);
		end loop;
		for I in U32'(0) .. U32'(7) loop
			State(I    ) := State(I) xor State(I + 8);
			State(I + 8) := State(I) xor Chaining_Value(I);
		end loop;
		return State;
	end Compress;

	function First_8_Words(Compression_Output: in U32x16) return U32x8 is
					(U32x8(Compression_Output(0 .. 7)));

	function Load_32(Src: in Octets) return U32 is
		(U32(Src(Src'First)) or Shift_Left(U32(Src(Src'First + 1)), 8)
		or Shift_Left(U32(Src(Src'First + 2)), 16) or
		Shift_Left(U32(Src(Src'First + 3)), 24));

	procedure Words_From_Little_Endian_Bytes(B: in Octets; W: out Words) is
	begin
		for I in W'Range loop
			W(I) := Load_32(B(I * 4 .. (I * 4 + 3)));
		end loop;
	end Words_From_Little_Endian_Bytes;

	function Chaining_Value(Self: in Output_T) return U32x8 is
			(First_8_Words(Compress(Self.Input_Chaining_Value,
			Self.Block_Words, Self.Counter, Self.Block_Len,
			Self.Flags)));

	procedure Root_Output_Bytes(Self: in Output_T; Out_Slice: out Octets) is

		Output_Block_Counter: U32 := 0;

		procedure Output_Block(Real_Chunk_Last: in U32;
							Words: in U32x16) is
			Current_Octet: U32 := Out_Slice'First +
						Output_Block_Counter *
						Chunk_Size;
			Current_Word:  U32 := Words'First;
			Step_Size:     U32;
		begin
			loop
				Step_Size := U32'Min(4,
					Real_Chunk_Last - Current_Octet + 1);

				exit when Step_Size = 0;

				Out_Slice(Current_Octet ..
					Current_Octet + Step_Size - 1) :=
					To_LE_Bytes(Words(Current_Word),
					Step_Size);
				Current_Octet := Current_Octet + Step_Size;
				Current_Word  := Current_Word  + 1;
			end loop;
		end Output_Block;
	begin
		while Output_Block_Counter * Chunk_Size < Out_Slice'Length loop
			Output_Block(
				Real_Chunk_Last => U32'Min(
					Out_Slice'Last,
					Out_Slice'First + Chunk_Size *
						(Output_Block_Counter + 1) - 1
				),
				Words => Compress(
					Self.Input_Chaining_Value,
					Self.Block_Words,
					U64(Output_Block_Counter),
					Self.Block_Len,
					Self.Flags or Root
				)
			);
			Output_Block_Counter := Output_Block_Counter + 1;
		end loop;
	end Root_Output_Bytes;

	function To_LE_Bytes(Word: in U32; Output_Length: in U32)
								return Octets is
		Result: Octets(0 .. Output_Length - 1);
		Part:   U32 := Word;
		I:      U32 := 0;
	begin
		while I < Output_Length loop
			Result(I) := U8(Part and 16#ff#);
			Part      := Shift_Right(Part, 8);
			I         := I + 1;
		end loop;
		return Result;
	end To_LE_Bytes;

	function New_Chunk_State(Key_Words: in U32x8; Chunk_Counter_I: in U64;
					Flags_I: in U32) return Chunk_State_T is
					(Chaining_Value   => Key_Words,
					Chunk_Counter     => Chunk_Counter_I,
					Block             => (others => 0),
					Block_Len         => 0,
					Blocks_Compressed => 0,
					Flags             => Flags_I);

	function Length(Self: in Chunk_State_T) return U32 is
				(Block_Len * U32(Self.Blocks_Compressed) +
				U32(Self.Block_Len));

	function Start_Flag(Self: in Chunk_State_T) return U32 is
			(if Self.Blocks_Compressed = 0 then Chunk_Start else 0);

	-- PERF CRITICAL
	procedure Compress_Full_Block_Buffer(Self: in out Chunk_State_T) is
		Block_Words: U32x16;
		--for Block_Words'Address use Self.Block'Address;
	begin
		Words_From_Little_Endian_Bytes(Self.Block, Block_Words);
		Self.Chaining_Value := First_8_Words(Compress(
			Self.Chaining_Value, Block_Words, Self.Chunk_Counter,
			Block_Len, Self.Flags or Self.Start_Flag
		));
		Self.Blocks_Compressed := Self.Blocks_Compressed + 1;
		Self.Block             := (others => 0);
		Self.Block_Len         := 0;
	end Compress_Full_Block_Buffer;

	function Output(Self: in Chunk_State_T) return Output_T is
		Block_Words_I: U32x16;
		--for Block_Words_I'Address use Self.Block'Address;
	begin
		Words_From_Little_Endian_Bytes(Self.Block, Block_Words_I);
		return (
			Input_Chaining_Value => Self.Chaining_Value,
			Block_Words => Block_Words_I,
			Counter     => Self.Chunk_Counter,
			Block_Len   => U32(Self.Block_Len),
			Flags       => (Self.Flags or Self.Start_Flag or
								Chunk_End)
		);
	end Output;

	function Parent_Output(Left_Child_CV, Right_Child_CV, Key_Words:
				in U32x8; Flags_I: in U32) return Output_T is
		(
			Input_Chaining_Value => Key_Words,
			Block_Words          => Left_Child_CV & Right_Child_CV,
			Counter              => 0,
			Block_Len            => Block_Len,
			Flags                => Parent or Flags_I
		);

	function Parent_CV(Left_Child_CV, Right_Child_CV, Key_Words: in U32x8;
				Flags: in U32) return U32x8 is (Chaining_Value(
				Parent_Output(Left_Child_CV, Right_Child_CV,
				Key_Words, Flags)));

	-- NOT IMPLEMENTED: New_Keyed, New_Derive_Key

	procedure Push_Stack(Self: in out Hasher; CV: in U32x8) 
					with Pre => (Self.CV_Stack_Len < 54) is
	begin
		Self.CV_Stack(Self.CV_Stack_Len) := CV;
		Self.CV_Stack_Len := Self.CV_Stack_Len + 1;
	end Push_Stack;

	function Pop_Stack(Self: in out Hasher) return U32x8
				with Pre => (Self.CV_Stack_Len > 0) is
	begin
		Self.CV_Stack_Len := Self.CV_Stack_Len - 1;
		return Self.CV_Stack(Self.CV_Stack_Len);
	end Pop_Stack;

	procedure Add_Chunk_Chaining_Value(Self: in out Hasher;
				New_CV_I: in U32x8; Total_Chunks_I: U64) is
		New_CV:       U32x8 := New_CV_I;
		Total_Chunks: U64   := Total_Chunks_I;
	begin
		while (Total_Chunks and 1) = 0 loop
			New_CV := Parent_CV(Self.Pop_Stack, New_CV,
						Self.Key_Words, Self.Flags);
			Total_Chunks := Shift_Right(Total_Chunks, 1);
		end loop;
		Self.Push_Stack(New_CV);
	end Add_Chunk_Chaining_Value;

	procedure Finalize_Complete_Chunk_Reset_State(Self: in out Hasher) is
		Chunk_CV:     constant U32x8 :=
					Chaining_Value(Self.Chunk_State.Output);
		Total_Chunks: constant U64 :=
					Self.CHunk_State.Chunk_Counter + 1;
	begin
		Self.Add_Chunk_Chaining_Value(Chunk_CV, Total_Chunks);
		Self.Chunk_State := New_Chunk_State(Self.Key_Words,
						Total_Chunks, Self.Flags);
	end Finalize_Complete_Chunk_Reset_State;

end Blake3;
