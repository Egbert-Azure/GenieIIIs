(* ARCDEF.INC TR 311089 *)
(* DO NOT MODIFY DATA ORDER OF RECORDS!!! USED IN INLINES! *)

type  str4      = string[4];
      filenam   = string[16];
      longword  = record
                    case byte of
                      0 : (lo,hi : integer);
                      1 : (dum : byte; mid : integer);
                      2 : (b0,b1,b2,b3 : byte);
                    end;
      binfile   = record
                    buff         : array[0..255] of byte;
                    fsize,actpos : longword;
                    feof         : boolean;
                    id           : file;
                    end;
      headtype  = record
                    case byte of
                      0 : (vers          : byte;
                           name          : array[1..13] of char;
                           size          : longword;
                           date,time,crc : integer;
                           length        : longword);
                      1 : (arr           : array[1..28] of byte);
                    end;
      arglist   = array[1..10] of filenam;
      flaglist  = array[1..10] of boolean;

const long_null : longword = (lo:0; hi:0);
      long_eins : longword = (lo:1; hi:0);
