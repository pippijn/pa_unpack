open Sexplib.Conv


type conversion =
  | Grouping of conversion list

  | List of conversion
  | FixedRepeat of int * conversion
  | Repeat of conversion * conversion

  | Char	(* a  A string with arbitrary binary data, will be null padded. *)

  | SInt8	(* c  A signed char (8-bit) value. *)
  | UInt8	(* C  An unsigned char (octet) value. *)

  | SInt16	(* s  A signed short (16-bit) value. *)
  | UInt16	(* S  An unsigned short value. *)

  | SInt32	(* l  A signed long (32-bit) value. *)
  | UInt32	(* L  An unsigned long value. *)

  | SInt64	(* q  A signed quad (64-bit) value. *)
  | UInt64	(* Q  An unsigned quad value. *)

  | SInt	(* i  A signed integer value. *)
  | UInt	(* I  A unsigned integer value. *)

  | Net16	(* n  An unsigned short (16-bit) in "network" (big-endian) order. *)
  | Net32	(* N  An unsigned long (32-bit) in "network" (big-endian) order. *)
  | VAX16	(* v  An unsigned short (16-bit) in "VAX" (little-endian) order. *)
  | VAX32	(* V  An unsigned long (32-bit) in "VAX" (little-endian) order. *)

  | BER		(* w  A BER compressed integer. *)
  with sexp


type conversions =
  conversion list
