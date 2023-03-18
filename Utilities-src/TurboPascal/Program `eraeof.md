# Program `eraeof.pas` ##

The program `eraeof` is designed to remove the end-of-file (EOF) marker from files that consist of a combination of source code and object code. The program was created in 1991 for the purpose of converting 8-inch floppy disks from the author's colleague H. Sick.

The program is written in Pascal and begins with an include statement for the `BYTEFILE.BIB file`, which provides a set of procedures for handling byte files. The constants `eof` and `ersatz` are defined as hexadecimal values for the EOF marker and a replacement character, respectively.

The program prompts the user to input the name of the source file with EOFs, which is stored in the variable `quelle`. The program then converts the file name to uppercase characters and opens the file using the `assignByteFile` and `resetByteFile` procedures. If the file cannot be opened, the program prompts the user again until a valid file name is entered.

If the program is executed with a command line parameter, the program attempts to open the file with that name. If the file cannot be opened, the program prompts the user to input a valid file name.

The program then extracts the file extension from the source file name and appends the `.NEW` extension to create the output file name, which is stored in the variable `ziel`. The program then opens the output file using the `assignByteFile` and `rewriteByteFile` procedures. If the output file cannot be created, the program prints an error message and terminates.

The program then reads each byte from the input file using the `readByteFile` procedure and checks if it is the EOF marker. If it is, the byte is replaced with the ersatz character. The program then writes the byte to the output file using the writeByteFile procedure. The program continues this process until the end of the input file is reached.

Finally, the program closes both files using the `closeByteFile` procedure, prints a message indicating that the file has been converted, and terminates.
