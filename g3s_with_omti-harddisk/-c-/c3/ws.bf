..
..  ws.bf
..  Macros for Bradford to allow use of WordStar formatting
..
..  Copyright 1988 by:
..      Aaron Contorer
..      Box 5056
..      Champaign, IL 61820
..      USA
..  "Bradford" is a trademark of Aaron Contorer
..
..  To print a file called "myfile" with Bradford 2.0 using these macros:
..       A> bradford ws.bf myfile
..
.mnW31     Soft hyphen
.mdW "-"
.mnB2  Boldface
.mdB "\dk3 |\dk1 "
.mnD4  Double strike
.mdD "\dk2 |\dk1 "
.mnL12
.mdL "\nl \pa "
.mnS19 Underline
.mdS "\ul1 |\ul0 "
.mnT20 Superscript
.mdT "\sup |\su0 "
.mnV22 Subscript
.mdV "\sub |\su0 "
.mnY25 Italics 
.mdY "\fnipizza |\fnpizza "
.. Here's how we let Bradford recognize WordStar's paragraph markers.
.mnz141             If any line contains a character 141,
.mdz "\stj1 \nl "   we'll print that one line justified and go to a new line.
.sta                All other lines will be average-justified.
.. This works because WordStar ends every line with a character 141 if it's
.. in the middle of a paragraph, but ends a line with character 13 (carriage
.. return) if it's where someone pressed Return (the end of a paragraph).

completely functional copy, not just a crippled "demo version."

.fnpizza2
.stl
Bradford 2.0 new features include:
