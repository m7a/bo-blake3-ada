with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
with Blake3;

procedure Blake3Ada is

	-- No need to do anything complicated here.
	-- for i in `seq 0 255`; do printf "Character'Val(%3d) => \"%02x\",\n" "$i" "$i"; done
	Hex_Table: constant array (Character) of String(1..2) := (
		Character'Val(  0) => "00", Character'Val(  1) => "01",
		Character'Val(  2) => "02", Character'Val(  3) => "03",
		Character'Val(  4) => "04", Character'Val(  5) => "05",
		Character'Val(  6) => "06", Character'Val(  7) => "07",
		Character'Val(  8) => "08", Character'Val(  9) => "09",
		Character'Val( 10) => "0a", Character'Val( 11) => "0b",
		Character'Val( 12) => "0c", Character'Val( 13) => "0d",
		Character'Val( 14) => "0e", Character'Val( 15) => "0f",
		Character'Val( 16) => "10", Character'Val( 17) => "11",
		Character'Val( 18) => "12", Character'Val( 19) => "13",
		Character'Val( 20) => "14", Character'Val( 21) => "15",
		Character'Val( 22) => "16", Character'Val( 23) => "17",
		Character'Val( 24) => "18", Character'Val( 25) => "19",
		Character'Val( 26) => "1a", Character'Val( 27) => "1b",
		Character'Val( 28) => "1c", Character'Val( 29) => "1d",
		Character'Val( 30) => "1e", Character'Val( 31) => "1f",
		Character'Val( 32) => "20", Character'Val( 33) => "21",
		Character'Val( 34) => "22", Character'Val( 35) => "23",
		Character'Val( 36) => "24", Character'Val( 37) => "25",
		Character'Val( 38) => "26", Character'Val( 39) => "27",
		Character'Val( 40) => "28", Character'Val( 41) => "29",
		Character'Val( 42) => "2a", Character'Val( 43) => "2b",
		Character'Val( 44) => "2c", Character'Val( 45) => "2d",
		Character'Val( 46) => "2e", Character'Val( 47) => "2f",
		Character'Val( 48) => "30", Character'Val( 49) => "31",
		Character'Val( 50) => "32", Character'Val( 51) => "33",
		Character'Val( 52) => "34", Character'Val( 53) => "35",
		Character'Val( 54) => "36", Character'Val( 55) => "37",
		Character'Val( 56) => "38", Character'Val( 57) => "39",
		Character'Val( 58) => "3a", Character'Val( 59) => "3b",
		Character'Val( 60) => "3c", Character'Val( 61) => "3d",
		Character'Val( 62) => "3e", Character'Val( 63) => "3f",
		Character'Val( 64) => "40", Character'Val( 65) => "41",
		Character'Val( 66) => "42", Character'Val( 67) => "43",
		Character'Val( 68) => "44", Character'Val( 69) => "45",
		Character'Val( 70) => "46", Character'Val( 71) => "47",
		Character'Val( 72) => "48", Character'Val( 73) => "49",
		Character'Val( 74) => "4a", Character'Val( 75) => "4b",
		Character'Val( 76) => "4c", Character'Val( 77) => "4d",
		Character'Val( 78) => "4e", Character'Val( 79) => "4f",
		Character'Val( 80) => "50", Character'Val( 81) => "51",
		Character'Val( 82) => "52", Character'Val( 83) => "53",
		Character'Val( 84) => "54", Character'Val( 85) => "55",
		Character'Val( 86) => "56", Character'Val( 87) => "57",
		Character'Val( 88) => "58", Character'Val( 89) => "59",
		Character'Val( 90) => "5a", Character'Val( 91) => "5b",
		Character'Val( 92) => "5c", Character'Val( 93) => "5d",
		Character'Val( 94) => "5e", Character'Val( 95) => "5f",
		Character'Val( 96) => "60", Character'Val( 97) => "61",
		Character'Val( 98) => "62", Character'Val( 99) => "63",
		Character'Val(100) => "64", Character'Val(101) => "65",
		Character'Val(102) => "66", Character'Val(103) => "67",
		Character'Val(104) => "68", Character'Val(105) => "69",
		Character'Val(106) => "6a", Character'Val(107) => "6b",
		Character'Val(108) => "6c", Character'Val(109) => "6d",
		Character'Val(110) => "6e", Character'Val(111) => "6f",
		Character'Val(112) => "70", Character'Val(113) => "71",
		Character'Val(114) => "72", Character'Val(115) => "73",
		Character'Val(116) => "74", Character'Val(117) => "75",
		Character'Val(118) => "76", Character'Val(119) => "77",
		Character'Val(120) => "78", Character'Val(121) => "79",
		Character'Val(122) => "7a", Character'Val(123) => "7b",
		Character'Val(124) => "7c", Character'Val(125) => "7d",
		Character'Val(126) => "7e", Character'Val(127) => "7f",
		Character'Val(128) => "80", Character'Val(129) => "81",
		Character'Val(130) => "82", Character'Val(131) => "83",
		Character'Val(132) => "84", Character'Val(133) => "85",
		Character'Val(134) => "86", Character'Val(135) => "87",
		Character'Val(136) => "88", Character'Val(137) => "89",
		Character'Val(138) => "8a", Character'Val(139) => "8b",
		Character'Val(140) => "8c", Character'Val(141) => "8d",
		Character'Val(142) => "8e", Character'Val(143) => "8f",
		Character'Val(144) => "90", Character'Val(145) => "91",
		Character'Val(146) => "92", Character'Val(147) => "93",
		Character'Val(148) => "94", Character'Val(149) => "95",
		Character'Val(150) => "96", Character'Val(151) => "97",
		Character'Val(152) => "98", Character'Val(153) => "99",
		Character'Val(154) => "9a", Character'Val(155) => "9b",
		Character'Val(156) => "9c", Character'Val(157) => "9d",
		Character'Val(158) => "9e", Character'Val(159) => "9f",
		Character'Val(160) => "a0", Character'Val(161) => "a1",
		Character'Val(162) => "a2", Character'Val(163) => "a3",
		Character'Val(164) => "a4", Character'Val(165) => "a5",
		Character'Val(166) => "a6", Character'Val(167) => "a7",
		Character'Val(168) => "a8", Character'Val(169) => "a9",
		Character'Val(170) => "aa", Character'Val(171) => "ab",
		Character'Val(172) => "ac", Character'Val(173) => "ad",
		Character'Val(174) => "ae", Character'Val(175) => "af",
		Character'Val(176) => "b0", Character'Val(177) => "b1",
		Character'Val(178) => "b2", Character'Val(179) => "b3",
		Character'Val(180) => "b4", Character'Val(181) => "b5",
		Character'Val(182) => "b6", Character'Val(183) => "b7",
		Character'Val(184) => "b8", Character'Val(185) => "b9",
		Character'Val(186) => "ba", Character'Val(187) => "bb",
		Character'Val(188) => "bc", Character'Val(189) => "bd",
		Character'Val(190) => "be", Character'Val(191) => "bf",
		Character'Val(192) => "c0", Character'Val(193) => "c1",
		Character'Val(194) => "c2", Character'Val(195) => "c3",
		Character'Val(196) => "c4", Character'Val(197) => "c5",
		Character'Val(198) => "c6", Character'Val(199) => "c7",
		Character'Val(200) => "c8", Character'Val(201) => "c9",
		Character'Val(202) => "ca", Character'Val(203) => "cb",
		Character'Val(204) => "cc", Character'Val(205) => "cd",
		Character'Val(206) => "ce", Character'Val(207) => "cf",
		Character'Val(208) => "d0", Character'Val(209) => "d1",
		Character'Val(210) => "d2", Character'Val(211) => "d3",
		Character'Val(212) => "d4", Character'Val(213) => "d5",
		Character'Val(214) => "d6", Character'Val(215) => "d7",
		Character'Val(216) => "d8", Character'Val(217) => "d9",
		Character'Val(218) => "da", Character'Val(219) => "db",
		Character'Val(220) => "dc", Character'Val(221) => "dd",
		Character'Val(222) => "de", Character'Val(223) => "df",
		Character'Val(224) => "e0", Character'Val(225) => "e1",
		Character'Val(226) => "e2", Character'Val(227) => "e3",
		Character'Val(228) => "e4", Character'Val(229) => "e5",
		Character'Val(230) => "e6", Character'Val(231) => "e7",
		Character'Val(232) => "e8", Character'Val(233) => "e9",
		Character'Val(234) => "ea", Character'Val(235) => "eb",
		Character'Val(236) => "ec", Character'Val(237) => "ed",
		Character'Val(238) => "ee", Character'Val(239) => "ef",
		Character'Val(240) => "f0", Character'Val(241) => "f1",
		Character'Val(242) => "f2", Character'Val(243) => "f3",
		Character'Val(244) => "f4", Character'Val(245) => "f5",
		Character'Val(246) => "f6", Character'Val(247) => "f7",
		Character'Val(248) => "f8", Character'Val(249) => "f9",
		Character'Val(250) => "fa", Character'Val(251) => "fb",
		Character'Val(252) => "fc", Character'Val(253) => "fd",
		Character'Val(254) => "fe", Character'Val(255) => "ff"
	);

	Ctx: Blake3.Hasher := Blake3.Init;

	Stream_Ptr: constant access Ada.Streams.Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Current_Input);
	Buffer: Ada.Streams.Stream_Element_Array(0 .. 4095);
	Last:   Ada.Streams.Stream_Element_Offset;
begin
	loop
		Ada.Streams.Read(Stream_Ptr.all, Buffer, Last);
		exit when Integer(Last) < 0;

		declare
			Data: String(1 .. Integer(Last) + 1);
			for Data'Address use Buffer'Address;
		begin
			Ctx.Update(Data);
		end;
	end loop;
	declare
		RV: constant String := Ctx.Final;
	begin
		for I in RV'Range loop
			Ada.Text_IO.Put(Hex_Table(RV(I)));
		end loop;
		Ada.Text_IO.New_Line;
	end;
end Blake3Ada;
