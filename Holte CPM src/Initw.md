## Initw.c ##

This is a program written in the C language for CP/M 3.0 to initialize the system and the hard disk. The program is designed to generate Genie III's CP/M 3.0 hard disk system. The program is written by Thomas Holte, and its version is 2.0. The program contains a set of language-dependent variables and some commands.

The program can be compiled for two types of controllers, Xebec SASI and Western Digital Winchester Disk Controller. The program defines various port addresses, commands, and variables for each type of controller.

The program starts by declaring variables for the current sector, surface, cylinder, and track numbers. It also initializes some buffers that contain formatting data, worst-case bit patterns, a spare directory, and a copyright message.

The program then declares a structure that defines a bad sector by its track and sector number. A pointer to the spare directory structure is also defined.

The program reads the complete CP/M 3 system from a file called `CPM3_SYS`. It reads a header record and extracts the top page number and length of the resident and banked portions of CP/M 3. The program then loads the resident portion into memory, sets up the system tables, and loads the banked portion.

Next, the program prompts the user for some information, such as the type of hard disk and the number of sectors per track. The program then calculates the number of tracks, surfaces, and cylinders based on this information and the formatting data in the buffer.

The program then formats the hard disk by looping over all the sectors on the disk and writing the formatting data to each one. For each track, the program checks for bad sectors by reading the spare directory, and if it finds any, it adds them to a list.

Finally, the program writes the spare directory to the last sector on the last track, updates the disk parameter block, and saves it to disk. The program then prints a message indicating that the hard disk is formatted and ready to use.
