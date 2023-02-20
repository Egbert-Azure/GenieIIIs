UNIT UpperCase;

INTERFACE

FUNCTION StrCmpI(Str1,Str2 : STRING; p : INTEGER) : INTEGER;
FUNCTION StrCmp(Str1,Str2 : STRING; p : INTEGER) : INTEGER;
PROCEDURE ToUpper(VAR Str (* : STRING *) );

IMPLEMENTATION

(*$L STRCMP *)

FUNCTION StrCmpI; EXTERNAL;
FUNCTION StrCmp; EXTERNAL;
PROCEDURE ToUpper; EXTERNAL;

END. (* UpperCase *)