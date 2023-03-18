# Program Code Documentation: program window #

This program is a test program that demonstrates the WINDOW function of the HOLTE CP/M. It creates several windows of different sizes and writes strings into them.

## Variables ##

`anystring`:

a string variable with a maximum length of 255 characters.

## Procedures ##

`fenster(windownummer, top_line, bottom_line, left_column, right_column)`:

this procedure opens a screen area with newly defined window sizes. All screen output after calling this function refers only to the new screen sizes.

## Parameters ##

- `windownummer`: a byte representing the window number.
- `top_line`: a byte representing the top line of the window.
- `bottom_line`: a byte representing the bottom line of the window.
- `left_column`: a byte representing the left column of the window.
- `right_column`: a byte representing the right column of the window.

## Main Program ##

The main program consists of the following steps:

1. Opens a window with number 1 and the following parameters: top line = 4, bottom line = 10, left column = 12, right column = 60.
2. Clears the screen.
3. Writes a string into the window.
4. Reads a line of input from the user.
5. Opens a window with number 2 and the following parameters: top line = 20, bottom line = 23, left column = 60, right column = 79.
6. Writes an inverted string into the window.
7. Waits for a keypress.
8. Closes all windows and writes a message to the screen.
9. Waits for a keypress.
``` console
Dies ist Fenster Nummer 1.
Die sind folgendermassen : Oben=4, Unten=10, Links=12 Rechts=60.
Das hier soll nur den gewaehlten Bereich voll machen, so das bei der jetzt folgenden Eingabe das scrollen beobachtet werden kann.
Test input
```
                      Dies ist jetzt ein  zweites zusaetzliches Fenster !
Und wieder alles ganz normal.
