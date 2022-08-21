-- CC0 code translated from reference_impl.rs
-- https://raw.githubusercontent.com/BLAKE3-team/BLAKE3/master/reference_impl/reference_impl.rs
-- see also
-- /data/main/114_projects_wrk_wrpru/114.115_test_extract_bupstash/testcpp/zblake3.cpp

package body Blake3 is

	function Init return Ctx is
	begin
		return (a => 0);
	end Init;

	procedure Update(C: in out Ctx; D: in String) is
	begin
		null;
	end Update;

	function Final(C: in out Ctx) return String is
	begin
		return "<hello>";
	end Final;

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
		-- Mix the columns
		G(State, 0, 4, 8, 12, M(0), M(1));
		G(State, 1, 5, 9, 13, M(2), M(3));
		G(State, 2, 6, 10, 14, M(4), M(5));
		G(State, 3, 7, 11, 15, M(6), M(7));
		-- Mix the diagonals
		G(State, 0, 5, 10, 15, M(8), M(9));
		G(State, 1, 6, 11, 12, M(10), M(11));
		G(State, 2, 7, 8, 13, M(12), M(13));
		G(State, 3, 4, 9, 14, M(14), M(15));
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

	function Compress(Chaining_Value: in U32x8; Block_Words: in U32x16;
			Counter: in U64; Block_Len: in U32; Flags: in U32)
			return U32x16 is
		Tail: constant Words(0..3) := (Counter_Low(Counter),
				Counter_High(Counter), Block_Len, Flags);
		State: U32x16 := U32x16(Words(Chaining_Value) & IV(0..3) &
				Tail);
		Block: U32x16 := Block_Words;
	begin
		for I in 1..7 loop
			Round(State, Block); -- round I
			Permute(Block);
		end loop;
		for I in U32'(0)..U32'(7) loop
			State(I    ) := State(I) xor State(I + 8);
			State(I + 8) := State(I) xor Chaining_Value(I);
		end loop;
		return State;
	end Compress;

	function First_8_Words(Compression_Output: in U32x16) return U32x8 is
					(U32x8(Compression_Output(0..7)));

	function Load_32(Src: in Octets) return U32 is
		(U32(Src(0)) or Shift_Left(U32(Src(1)), 8) or
		Shift_Left(U32(Src(2)), 16) or Shift_Left(U32(Src(3)), 24));

	procedure Words_From_Little_Endian_Bytes(B: in Octets; W: out Words)
				with Pre => ((4 * B'Length = W'Length) and
					(W'First = 0 and B'First = 0)) is
	begin
		for I in W'Range loop
			W(I) := Load_32(B(I * 4 .. (I * 4 + 3)));
		end loop;
	end Words_From_Little_Endian_Bytes;

	function Chaining_Value(Self: in Output) return U32x8 is
			(First_8_Words(Compress(Self.Input_Chaining_Value,
			Self.Block_Words, Self.Counter, Self.Block_Len,
			Self.Flags)));

	procedure Root_Output_Bytes(Self: in Output; Out_Slice: out Octets) is
		-- TODO THIS COUNTER IS u64 in original implementation, why?
		Output_Block_Counter: U32 := 0;
		Chunk_Size: constant U32 := (2 * Out_Len);
	begin
		while Output_Block_Counter * Chunk_Size < Out_Slice'Length loop
			declare
				Ideal_Chunk_End: constant U32 := Chunk_Size *
						(Output_Block_Counter + 1) - 1;
				Real_Chunk_End: constant U32 :=
					(if Out_Slice'Last < Ideal_Chunk_End
						then (Out_Slice'Last)
						else (Ideal_Chunk_End));
				-- TODO z might it be that we need to write
				--      to the chunk rather than define it
				--      this way here?
				--Out_Block: Octets := Out_Slice(
				--		Output_Block_Counter *
				--		Chunk_Size .. Real_Chunk_End);
			begin
				null;
			end;
		end loop;
		-- chunks_mut(N) := in chunks of size N (less if last + misfit)
		null;
	end Root_Output_Bytes;

--static uint32_t rotr32(uint32_t w, uint32_t c) {
--  return (w >> c) | (w << (32 - c));
--}
--
-- Translated Functions for C++ (at line 97)

--	function Highest_One(XR: in U64) return U32 is
--		X: U64 := XR;
--		C: U32 := 0;
--	begin
--		if (X and 16#ffffffff00000000#) /= 0 then
--			X := Shift_Right(X, 32);
--			C := C + 32;
--		end if;
--		if (X and 16#00000000ffff0000#) /= 0 then
--			X := Shift_Right(X, 16);
--			C := C + 16;
--		end if;
--		if (X and 16#000000000000ff00#) /= 0 then
--			X := Shift_Right(X,  8);
--			C := C +  8;
--		end if;
--		if (X and 16#00000000000000f0#) /= 0 then
--			X := Shift_Right(X,  4);
--			C := C +  4;
--		end if;
--		if (X and 16#000000000000000c#) /= 0 then
--			X := Shift_Right(X,  2);
--			C := C +  2;
--		end if;
--		if (X and 16#0000000000000002#) /= 0 then
--			C := C +  1;
--		end if;
--		return C;
--	end Highest_One;
--
--	function Popcnt(XR: in U64) return U32 is
--		X:     U64 := XR;
--		Count: U32 := 0;
--	begin
--		while X /= 0 loop
--			Count := Count + 1;
--			X := X and (X - 1);
--		end loop;
--		return Count;
--	end Popcnt;
--
--	function Round_Down_To_Power_Of_2(XR: in U64) return U64 is
--					(Shift_Left(1, Highest_One(XR or 1)));
--
--	procedure Load_Key_Words(Key: in Blake3_Key; Key_Words: out U32x8) is
--	begin
--		for I in 0..7 loop
--			Key_Words(I) := Load_32(Octets(Key(I .. (I+3))));
--		end loop;
--	end Load_Key_Words;
--
--	procedure Store_CV_Words(Bytes: out Octets, CV_Words: in U32x8) is
--	begin
--		null;
--	end Store_CV_Words;
--

end Blake3;
