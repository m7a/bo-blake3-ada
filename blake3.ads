-- CC0 code translated from reference_impl.rs
-- https://raw.githubusercontent.com/BLAKE3-team/BLAKE3/master/reference_impl/reference_impl.rs

with Interfaces;

package Blake3 is

	type U8     is mod 2**8;
	type U32    is new Interfaces.Unsigned_32;
	type Octets is array (U32 range <>) of U8;
	type Hasher is tagged limited private;

	function  Init return Hasher;
	procedure Update(Self: in out Hasher; Input: in String);
	procedure Update(Self: in out Hasher; Input: in Octets);
	function  Final(Self: in Hasher) return String;
	procedure Final(Self: in Hasher; Out_Slice: out Octets);

private

	pragma Assertion_Policy(Pre => Check, Post => Check);

	type    U64    is new Interfaces.Unsigned_64;
	type    Words  is array (U32 range <>) of U32;
	subtype U32x8  is Words(0..7);
	subtype U32x16 is Words(0..15);
	type    Stack  is array (U8 range <>) of U32x8;

	function Shift_Left(Value: in U32; Amount: in Natural) return U32 is
		(U32(Interfaces.Shift_Left(Interfaces.Unsigned_32(Value),
		Amount)));

	function Rotate_Right(Value: in U32; Amount: in Natural) return U32 is
		(U32(Interfaces.Rotate_Right(Interfaces.Unsigned_32(Value),
		Amount)));

	Out_Len:             constant U32 := 32;
	Chunk_Size:          constant U32 := (2 * Out_Len);
	Key_Len:             constant U32 := 32;
	Block_Len:           constant U32 := 64;
	Chunk_Len:           constant U32 := 1024;

	Chunk_Start:         constant U32 := Shift_Left(1, 0);
	Chunk_End:           constant U32 := Shift_Left(1, 1);
	Parent:              constant U32 := Shift_Left(1, 2);
	Root:                constant U32 := Shift_Left(1, 3);
	Keyed_Hash:          constant U32 := Shift_Left(1, 4);
	Derive_Key_Context:  constant U32 := Shift_Left(1, 5);
	Derive_Key_Material: constant U32 := Shift_Left(1, 6);

	IV: constant U32x8 := (
		16#6A09E667#, 16#BB67AE85#, 16#3C6EF372#, 16#A54FF53A#,
		16#510E527F#, 16#9B05688C#, 16#1F83D9AB#, 16#5BE0CD19#
	);

	MSG_Permutation: constant U32x16 := (
		2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8
	);

	-- We do not make this tagged to avoid issue rt. Output function that
	-- would have to potentially dispatch in two dimensions.
	type Output_T is record
		Input_Chaining_Value: U32x8;
		Block_Words:          U32x16;
		Counter:              U64;
		Block_Len:            U32;
		Flags:                U32;
	end record;

	type Chunk_State_T is tagged record
		Chaining_Value:    U32x8;
		Chunk_Counter:     U64;
		Block:             Octets(0..Block_Len-1);
		Block_Len:         U8;
		Blocks_Compressed: U8;
		Flags:             U32;
	end record;

	type Hasher is tagged limited record
		Chunk_State:  Chunk_State_T;
		Key_Words:    U32x8;
		CV_Stack:     Stack(0..53);
		CV_Stack_Len: U8;
		Flags:        U32;
	end record;
	
	function Chaining_Value(Self: in Output_T) return U32x8;
	procedure Root_Output_Bytes(Self: in Output_T; Out_Slice: out Octets);
	function To_LE_Bytes(Word: in U32; Output_Length: in U32) return Octets
			with Pre => (Output_Length <= 4 and Output_Length >= 1);
	function New_Chunk_State(Key_Words: in U32x8; Chunk_Counter_I: in U64;
					Flags_I: in U32) return Chunk_State_T;
	function Length(Self: in Chunk_State_T) return U32;
	procedure Compress_Full_Block_Buffer(Self: in out Chunk_State_T);
	function Output(Self: in Chunk_State_T) return Output_T;
	function Parent_Output(Left_Child_CV, Right_Child_CV, Key_Words:
				in U32x8; Flags_I: in U32) return Output_T;
	procedure Finalize_Complete_Chunk_Reset_State(Self: in out Hasher);

	generic
		type Self_T is tagged limited private;
		Comparison_Length: U32;
		with function State_Length(Self: in Self_T) return U32;
		with procedure Process_Complete_Entity(Self: in out Self_T);
		with procedure Update_Inner(Self: in out Self_T;
							Substr: in Octets);
	procedure Generic_Update(Self: in out Self_T; Input: in Octets);

	function Chunk_State_Length(Self: in Hasher) return U32;

end Blake3;
