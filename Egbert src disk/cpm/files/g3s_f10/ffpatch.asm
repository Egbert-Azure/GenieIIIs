;
; FF drive vector patch
;
; Fill in the values in the two data bytes below so that there is a 1
; beneath each drive that you want FF to scan as its default.  Then
; assemble this file to FFPATCH.HEX and overlay it onto FF.COM using
; MLOAD and the following command line
;
;	MLOAD FF=FF.COM,FFPATCH

	org	111h
;
; Make drives to be scanned a 1; leave other drives 0
;
;		ABCDEFGH
	db	11111111B	; drives A - H
;		IJKLMNOP
	db	11100000B	; drives I - P
;
	end
                               