with Interfaces;

package Blake3 is

	type Ctx is tagged limited private;

	function Init return Ctx;
	procedure Update(C: in out Ctx; D: in String);
	function Final(C: in out Ctx) return String;

private

	type U64 is new Interfaces.Unsigned_64;
	type U32 is new Interfaces.Unsigned_32;
	type U8  is mod 2**8;

	type Octets is array (U32 range <>) of U8;
	type Words  is array (U32 range <>) of U32;
	type U32x8  is new Words(0..7);
	type U32x16 is new Words(0..15);

	function Shift_Left(Value: in U32; Amount: in Natural) return U32 is
		(U32(Interfaces.Shift_Left(Interfaces.Unsigned_32(Value),
		Amount)));

	function Rotate_Right(Value: in U32; Amount: in Natural) return U32 is
		(U32(Interfaces.Rotate_Right(Interfaces.Unsigned_32(Value),
		Amount)));

	Out_Len:             constant U32 := 32;
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

	IV: constant Words(0..7) := (
		16#6A09E667#, 16#BB67AE85#, 16#3C6EF372#, 16#A54FF53A#,
		16#510E527F#, 16#9B05688C#, 16#1F83D9AB#, 16#5BE0CD19#
	);

	MSG_Permutation: U32x16 := (
		2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8
	);

	type Output is tagged record
		Input_Chaining_Value: U32x8;
		Block_Words:          U32x16;
		Counter:              U64;
		BLock_Len:            U32;
		Flags:                U32;
	end record;

	type Ctx is tagged limited record
		a: Integer;
	end record;

--	type Blake3_Key is new Octets(0..7);
--
--	Max_SIMD_Degree:      constant U32 := 16;
--	Max_SIMD_Degree_Or_2: constant U32 := 16;
--
--	function Shift_Left(Value: in U64; Amount: in U32) return U64 is
--		(U64(Interfaces.Shift_Left(Interfaces.Unsigned_64(Value),
--		Natural(Amount))));
--
--	function Shift_Right(Value: in U64; Amount: in Natural) return U64 is
--		(U64(Interfaces.Shift_Right(Interfaces.Unsigned_64(Value),
--		Amount)));
--
--	Chunk_Start:         constant Integer := 0;
--	Chunk_End:           constant Integer := 1;
--	Parent:              constant Integer := 2;
--	Root:                constant Integer := 3;
--	Keyed_Hash:          constant Integer := 4;
--	Derive_Key_Context:  constant Integer := 5;
--	Derive_Key_Material: constant Integer := 6;
--
--	Blake3_Flags: constant array(0..6) of U32 := (
--		Chunk_Start         => Shift_Left(1, 0),
--		Chunk_End           => Shift_Left(1, 1),
--		Parent              => Shift_Left(1, 2),
--		Root                => Shift_Left(1, 3),
--		Keyed_Hash          => Shift_Left(1, 4),
--		Derive_Key_Context  => Shift_Left(1, 5),
--		Derive_Key_material => Shift_Left(1, 6)
--	);
--
--	Msg_Schedule: constant array(0..6, 0..15) of U8 := (
--		(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
--		(2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8),
--		(3, 4, 10, 12, 13, 2, 7, 14, 6, 5, 9, 0, 11, 15, 8, 1),
--		(10, 7, 12, 9, 14, 3, 13, 15, 4, 0, 11, 2, 5, 8, 1, 6),
--		(12, 13, 9, 11, 15, 10, 14, 8, 7, 2, 5, 3, 0, 1, 6, 4),
--		(9, 14, 11, 5, 8, 12, 15, 1, 13, 3, 0, 10, 2, 6, 4, 7),
--		(11, 15, 5, 0, 1, 9, 8, 6, 14, 10, 2, 12, 3, 4, 7, 13)
--	);

-- Translated Functions

--	function Highest_One(XR: in U64) return U32;
--	function Popcnt(XR: in U64) return U32;
--	function Round_Down_To_Power_Of_2(XR: in U64) return U64;
--	function Counter_Low(XR: in U64) return U32;
--	function Counter_High(XR: in U64) return U32;
--	function Load_32(Src: in Octets)
--				return U32 with Pre => (Src'Length >= 4);
--	procedure Load_Key_Words(Key: in Blake3_Key; Key_Words: out Words);

end Blake3;
