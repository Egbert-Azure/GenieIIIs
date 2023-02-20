PROGRAM TestChars;

CONST
  cFirstChar = $20;
  cLastChar  = $FF;
  cColumns   = 8;

TYPE
  tString20 = STRING(.20.);

VAR
  ThisChar : INTEGER;
  OneKey   : CHAR;
  Column   : INTEGER;
  ShowHighControls : BOOLEAN;

FUNCTION HexStr (Value : INTEGER; Digits : INTEGER) : tString20;
TYPE
  tHexDigits = ARRAY (.0..15.) OF CHAR;
CONST
  HexDigits : tHexDigits = '0123456789ABCDEF';
VAR
  Result    : tString20;
  ThisDigit : INTEGER;
BEGIN
  Result(.0.) := Chr (Digits);
  ThisDigit := Digits;
  WHILE Value > 0 DO BEGIN
    Result(.ThisDigit.) := HexDigits(.Value MOD 16.);
    Value := Value DIV 16;
    ThisDigit := Pred (ThisDigit);
  END;
  WHILE ThisDigit > 0 DO BEGIN
    Result(.ThisDigit.) := '0';
    ThisDigit := Pred (ThisDigit);
  END;
  HexStr := Result;
END;

BEGIN
  IF (ParamCount > 0) AND (ParamStr (1) = 'HIGHCTRLS')
  THEN ShowHighControls := TRUE
  ELSE ShowHighControls := FALSE;
  Column := 1;
  FOR ThisChar := cFirstChar TO cLastChar DO BEGIN
    IF (ThisChar < $80) OR (ThisChar >= $A0) OR ShowHighControls
    THEN BEGIN
      Write (ThisChar : 3, ' ', HexStr (ThisChar, 2), ' ', Chr (ThisChar));
      IF Column = cColumns
      THEN BEGIN
        WriteLn;
        Column := 1;
      END
      ELSE BEGIN
        Write ('  ');
        Column := Succ (Column);
      END;
    END;
  END;
END.
