BASIC Boot Maker
================

...a C64 tool to make auto-executable programs in BASIC text area

Version 1.04
Released on September 2, 2025

Copyright (c) 2025 Graham (Francesco Gramignani)
https://graham-it.itch.io/bbm
https://github.com/graham-it/bbm

Not only BASIC
Source file could also be an M/L code program launched by a "BASIC stub".

Overview
This tool uses an undocumented (and probably never released) exploit into
LOAD command routine of KERNAL ROM, to auto-execute a program file through
CHRGET routine, EAL pointer and screen RAM manipulation (for more info read
the "chrget boot.txt" file).

How to use
Once started, the program loads the current disk directory and the user can
choose the file to make "bootable" by pressing RETURN.
If the chosen file has a load address in the BASIC text area ($801), the user
will be prompted to edit a one-line message, which will be displayed during
the boot phase.
After pressing RETURN, the process will start and the output file will have
the same name as the input file, preceded by a "<-" symbol (back arrow),
and can be loaded with:

	LOAD"<-filename",n,1	(where n is the current drive number)

Then the program will start automatically, without having to type RUN, and
if a return to the BASIC prompt is expected, you will be able to list, edit,
and save it as usual.

Release notes
Data buffer is extended to 4 KB to reduce head seeking during disk access
and to speed-up the entire process.
