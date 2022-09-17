-- BLAKE3 Implementation ported to Ada 1.0.0, 2022 Ma_Sys.ma <info@masysma.net>.
-- This implementation has been ported from the reference implementation.

-- Available under CC0, see below for full license text

-- Creative Commons Legal Code
-- 
-- CC0 1.0 Universal
-- 
--     CREATIVE COMMONS CORPORATION IS NOT A LAW FIRM AND DOES NOT PROVIDE
--     LEGAL SERVICES. DISTRIBUTION OF THIS DOCUMENT DOES NOT CREATE AN
--     ATTORNEY-CLIENT RELATIONSHIP. CREATIVE COMMONS PROVIDES THIS
--     INFORMATION ON AN "AS-IS" BASIS. CREATIVE COMMONS MAKES NO WARRANTIES
--     REGARDING THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS
--     PROVIDED HEREUNDER, AND DISCLAIMS LIABILITY FOR DAMAGES RESULTING FROM
--     THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS PROVIDED
--     HEREUNDER.
-- 
-- Statement of Purpose
-- 
-- The laws of most jurisdictions throughout the world automatically confer
-- exclusive Copyright and Related Rights (defined below) upon the creator
-- and subsequent owner(s) (each and all, an "owner") of an original work of
-- authorship and/or a database (each, a "Work").
-- 
-- Certain owners wish to permanently relinquish those rights to a Work for
-- the purpose of contributing to a commons of creative, cultural and
-- scientific works ("Commons") that the public can reliably and without fear
-- of later claims of infringement build upon, modify, incorporate in other
-- works, reuse and redistribute as freely as possible in any form whatsoever
-- and for any purposes, including without limitation commercial purposes.
-- These owners may contribute to the Commons to promote the ideal of a free
-- culture and the further production of creative, cultural and scientific
-- works, or to gain reputation or greater distribution for their Work in
-- part through the use and efforts of others.
-- 
-- For these and/or other purposes and motivations, and without any
-- expectation of additional consideration or compensation, the person
-- associating CC0 with a Work (the "Affirmer"), to the extent that he or she
-- is an owner of Copyright and Related Rights in the Work, voluntarily
-- elects to apply CC0 to the Work and publicly distribute the Work under its
-- terms, with knowledge of his or her Copyright and Related Rights in the
-- Work and the meaning and intended legal effect of CC0 on those rights.
-- 
-- 1. Copyright and Related Rights. A Work made available under CC0 may be
-- protected by copyright and related or neighboring rights ("Copyright and
-- Related Rights"). Copyright and Related Rights include, but are not
-- limited to, the following:
-- 
--   i. the right to reproduce, adapt, distribute, perform, display,
--      communicate, and translate a Work;
--  ii. moral rights retained by the original author(s) and/or performer(s);
-- iii. publicity and privacy rights pertaining to a person's image or
--      likeness depicted in a Work;
--  iv. rights protecting against unfair competition in regards to a Work,
--      subject to the limitations in paragraph 4(a), below;
--   v. rights protecting the extraction, dissemination, use and reuse of data
--      in a Work;
--  vi. database rights (such as those arising under Directive 96/9/EC of the
--      European Parliament and of the Council of 11 March 1996 on the legal
--      protection of databases, and under any national implementation
--      thereof, including any amended or successor version of such
--      directive); and
-- vii. other similar, equivalent or corresponding rights throughout the
--      world based on applicable law or treaty, and any national
--      implementations thereof.
-- 
-- 2. Waiver. To the greatest extent permitted by, but not in contravention
-- of, applicable law, Affirmer hereby overtly, fully, permanently,
-- irrevocably and unconditionally waives, abandons, and surrenders all of
-- Affirmer's Copyright and Related Rights and associated claims and causes
-- of action, whether now known or unknown (including existing as well as
-- future claims and causes of action), in the Work (i) in all territories
-- worldwide, (ii) for the maximum duration provided by applicable law or
-- treaty (including future time extensions), (iii) in any current or future
-- medium and for any number of copies, and (iv) for any purpose whatsoever,
-- including without limitation commercial, advertising or promotional
-- purposes (the "Waiver"). Affirmer makes the Waiver for the benefit of each
-- member of the public at large and to the detriment of Affirmer's heirs and
-- successors, fully intending that such Waiver shall not be subject to
-- revocation, rescission, cancellation, termination, or any other legal or
-- equitable action to disrupt the quiet enjoyment of the Work by the public
-- as contemplated by Affirmer's express Statement of Purpose.
-- 
-- 3. Public License Fallback. Should any part of the Waiver for any reason
-- be judged legally invalid or ineffective under applicable law, then the
-- Waiver shall be preserved to the maximum extent permitted taking into
-- account Affirmer's express Statement of Purpose. In addition, to the
-- extent the Waiver is so judged Affirmer hereby grants to each affected
-- person a royalty-free, non transferable, non sublicensable, non exclusive,
-- irrevocable and unconditional license to exercise Affirmer's Copyright and
-- Related Rights in the Work (i) in all territories worldwide, (ii) for the
-- maximum duration provided by applicable law or treaty (including future
-- time extensions), (iii) in any current or future medium and for any number
-- of copies, and (iv) for any purpose whatsoever, including without
-- limitation commercial, advertising or promotional purposes (the
-- "License"). The License shall be deemed effective as of the date CC0 was
-- applied by Affirmer to the Work. Should any part of the License for any
-- reason be judged legally invalid or ineffective under applicable law, such
-- partial invalidity or ineffectiveness shall not invalidate the remainder
-- of the License, and in such case Affirmer hereby affirms that he or she
-- will not (i) exercise any of his or her remaining Copyright and Related
-- Rights in the Work or (ii) assert any associated claims and causes of
-- action with respect to the Work, in either case contrary to Affirmer's
-- express Statement of Purpose.
-- 
-- 4. Limitations and Disclaimers.
-- 
--  a. No trademark or patent rights held by Affirmer are waived, abandoned,
--     surrendered, licensed or otherwise affected by this document.
--  b. Affirmer offers the Work as-is and makes no representations or
--     warranties of any kind concerning the Work, express, implied,
--     statutory or otherwise, including without limitation warranties of
--     title, merchantability, fitness for a particular purpose, non
--     infringement, or the absence of latent or other defects, accuracy, or
--     the present or absence of errors, whether or not discoverable, all to
--     the greatest extent permissible under applicable law.
--  c. Affirmer disclaims responsibility for clearing rights of other persons
--     that may apply to the Work or any use thereof, including without
--     limitation any person's Copyright and Related Rights in the Work.
--     Further, Affirmer disclaims responsibility for obtaining any necessary
--     consents, permissions or other rights required for any use of the
--     Work.
--  d. Affirmer understands and acknowledges that Creative Commons is not a
--     party to this document and has no duty or obligation with respect to
--     this CC0 or use of the Work.

with Interfaces;

-- Whenever this API allows you to pass or receive a String it expects that
-- string to be a binary representation of the data under consideration. I.e.
-- it does not contain hex values encoded in ASCII but rather the raw byte
-- values.
package Blake3 is

	type    U8     is mod 2**8;
	type    U32    is new Interfaces.Unsigned_32;
	type    Octets is array (U32 range <>) of U8;
	subtype U8x32  is Octets(0..31);
	type    Hasher is tagged limited private;

	Key_Len: constant U32 := 32;

	-- Initialize as hash function
	function  Init return Hasher;

	-- Initialize as keyed hash function
	function  Init(Key: in U8x32) return Hasher;
	function  Init(Key: in String) return Hasher
					with Pre => Key'Length = Key_Len;

	-- Add data to process. Do not call with "empty" Strings
	procedure Update(Self: in out Hasher; Input: in String)
					with Pre => Input'Length > 0;
	procedure Update(Self: in out Hasher; Input: in Octets);

	-- Compute hash, Octets-based API allows variable-length output
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
	
	procedure Words_From_Little_Endian_Bytes(B: in Octets; W: out Words);
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
