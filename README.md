---
section: 42
x-masysma-name: blake3_ada
title: BLAKE3 Hash Function for Ada
date: 2022/09/13 21:12:43
lang: en-US
author: ["Linux-Fan, Ma_Sys.ma (info@masysma.net)"]
keywords: ["blake3", "hash", "ada", "library"]
x-masysma-version: 1.0.0
x-masysma-website: https://masysma.net/32/blake3_ada.xhtml
x-masysma-repository: https://www.github.com/m7a/bo-blake3-ada
x-masysma-copyright: "2022 Ma_Sys.ma <info@masysma.net>"
---
Abstract
========

This repository provides an Ada implementation of the BLAKE3 Hash Function
reference implementation (cf.
<https://github.com/BLAKE3-team/BLAKE3/blob/master/reference_impl/reference_impl.rs>).

This implementation supports hashes and keyed hashes.
It does not currently implement key derivation.

License
=======

Like the reference implementation, this library is available under CC0 and can
hence be freely integrated. See `LICENSE.txt` for details.

Compiling
=========

The following dependencies are required for building:

 * Ada compiler (`gcc`, `gnatmake`)
 * And build tool (`ant`)

## Compile

	ant

## Run Tests

	cd test_suite
	ant run

## Install

	mkdir -p /usr/local/lib/x86_64-linux-gnu/ada/adalib/blake3
	install lib/libblake3.so /usr/local/lib/x86_64-linux-gnu
	install lib/blake3.ali /usr/local/lib/x86_64-linux-gnu/ada/adalib/blake3

Repository Structure
====================

This repository contains multiple subdirectories for the various components of
the library.

~~~
/bo-blake3-ada
   |
   +-- lib/
   |    |
   |    +-- blake3.adb                *** This is the implementation. ***
   |    |
   |    +-- blake3.ads                *** Implementation header file. ***
   |    |
   |    +-- build.xml                 Build instructions
   |
   +-- sample/
   |    |
   |    +-- blake3hello.adb           Minimal example program.
   |
   +-- test_suite/                    Test program to process test vectors.
   |
   +-- test_vectors/                  Test vector data and scripts.
   |
   +-- util/
   |    |
   |    +-- blake3ada.adb             Commandline sample application.
   |    |
   |    +-- build.xml                 Build instructions for Linux.
   |
   +-- README.md                      This file.
   |
   +-- LICENSE.txt                    CC0 license.
   |
   +-- build.xml                      Recursive antfile build instructions.
~~~

The important subdirectory regarding the library is `lib`. If you do not
need tests or example programs, it is sufficient to compile and use only the
files from that directory.

Sample Program
==============

This sample program computes the hash over `Hello world.` (hex
`48656c6c6f20776f726c642e0a`). You can find it in directory `sample` under
`blake3hello.adb`.

~~~{.ada}
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
~~~

The main complexity of this program arises from the necessity to write the
output. Neither a direct binary output to stdout, nor an easy means to convert
to hexadecimal are available in Ada. As a comparatively short alternative to
using a lookup table, this sample program uses the formatting provided by
Ada.Text_IO.Integer_IO and then postprocesses the output to cut-off any
`16#` leading markers and account for the output right-alignment.

Using the installed Library
===========================

Assuming the library is already installed on your system, you can compile
and run the sample program as follows:

	gnatmake -o blake3hello blake3hello.adb \
		-aO/usr/lib/x86_64-linux-gnu/ada/adalib/blake3 \
		-aI/usr/share/ada/adainclude/blake3 \
		-largs -lblake3
	./blake3hello

Output: `C7A4B65F79934D742EA07A9E85B3BBB1AB9AD9F42033D8A0698495D0F564C804`

Using the Library without Installation
======================================

If the library is not installed on your system, it can be integrated using
multiple different approaches.

## Easy Vendoring

The quickest way to get started is to just include the `blake3.ads` and
`blake3.adb` files into the source tree. Here is what the directory structure
may look like then:

~~~
  ...
   |
   +-- sample/
   |    +-- blake3.adb
   |    +-- blake3.ads
   |    +-- blake3hello.adb
   |    +-- build.xml
   |
  ...
~~~

Compilation and invocation then become trivial:

	cp ../lib/blake3.ad? .
	gnatmake -o blake3hello blake3.adb
	./blake3hello

Output: `C7A4B65F79934D742EA07A9E85B3BBB1AB9AD9F42033D8A0698495D0F564C804`

## Inclusion from different directory

It may not be suitable to just copy-over the files. In this case, it is also
possible to import the compiled library from a different directory. Assume
that the library is compiled but not installed, then the file structure may
look as follows:

~~~
  ...
   +-- lib/
   |    +-- blake3.adb
   |    +-- blake3.ads
   |    +-- blake3.ali
   |    +-- build.xml
   |    +-- libblake3.so
   |
   +-- sample/
   |    +-- blake3hello.adb
   |    +-- build.xml
   |
  ...
~~~

Compilation and invocation than have to account for the library not being
installed as follows:

	gnatmake -o blake3hello blake3hello.adb -aO../lib -aI../lib -largs -lblake3
	LD_LIBRARY_PATH=$PWD/../lib ./blake3hello

Output: `C7A4B65F79934D742EA07A9E85B3BBB1AB9AD9F42033D8A0698495D0F564C804`

API Description
===============

## Types

~~~{.ada}
type    U8     is mod 2**8;
type    U32    is new Interfaces.Unsigned_32;
type    Octets is array (U32 range <>) of U8;
subtype U8x32  is Octets(0..31);
type    Hasher is tagged limited private;
~~~

 * `U8`:     This type is used to represent a single byte.
 * `U32`:    This type is used to represent a 32 bit word.
 * `Octets`: This type is used to represent a byte string.
 * `U8x32`:  This type is used to represent a 32 bytes byte string.
 * `Hasher`: This type is used to represent an opaque context of operation.
 * `String`: Strings are used as an alternative representation of byte strings.
   This allows interfacing with other code without forcing it to use the
   types supplied by this library. Conversion between Octets and Strings can
   be performed by using the `for Octet_Var'Address use String_Var'Address;`
   syntax if necessary.

## Context Re-Use

The `Hasher` contexts provided by this API represent the state of hashing.
Since `Final` does not update the state in any way, it is possible to
call `Final` multiple times without expecting a change in output.

Additionally, it is possible to call `Final`, then perform multiple `Update`
invocations and then call `Final` again. The output of the last `Final`
corresponds to the concatenation of all data supplied through preceding uses
of `Update` on the same context.

It is currently not possible to reset the context to discard the old computation
state. If entirely new data is to be processed, it is suggested to create an
entirely new context for this purpose.

## Functions and Procedures

A detailed description of the API functions follows after the overview excerpt
from `blake3.ads`.

~~~{.ada}
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
~~~

### `function Init return Hasher`

This function creates a BLAKE3 context to be used as a _hash function_. It has
to be called before using any of the other API functions.

### `function Init(Key: in) return Hasher`

This variant of `Init` creates a BLAKE3 context to be used as a
_keyed hash function_. The user is free to either supply an `U32x8` input
or a 32-byte `String` which the library then interprets as `U32x8` internally
without incurring a notable performance overhead.

### `procedure Update(Self: in out Hasher; Input: in)`

Use this function to supply the data that is considered input into the
(optionally keyed) BLAKE3 hashing function. The user is free to supply the
internal type `Octets` which the library then interprets
as `Octets` internally.

This API expects the minimum number of bytes supplied to `Update` to be 1.
Do not call it with empty arrays or strings.

### `procedure Final(Self: in Hasher) return String`

This function outputs the 32-byte hash corresponding to the concatenation of all
data supplied with `Update` to the provided `Hasher` context and returns it as
a string with binary data. If this String is to be written to a text format,
a conversion to hexadecimal representation is recommended. See
`util/blake3ada.adb` for an easy means to achieve such a conversion.

### `procedure Final(Self: in Hasher; Out_Slice: out Octets)`

This alternative `Final` implementation uses the internal `Octets` datatype
for output. Additionally, by providing an `Out_Slice` of different length,
different lengths of hashes can be output. Note that BLAKE3 supports arbitrarily
long outputs as needed.
