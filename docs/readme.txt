BASIC Boot Maker
================

...a C64 tool to make auto-executable programs in BASIC text area

Version 1.05
Released on October 28, 2025

Not only BASIC: source file could also be a "BASIC stub" (e.g. 10 SYS2061)
to launch an M/L code program.

Overview
========

This tool uses an undocumented (and probably never implemented before) exploit
into the LOAD command routine of KERNAL ROM, to auto-execute a program
immediately after loading it.
This is done by applying a header to the original file, that modifies TXTPTR
into CHRGET routine, EAL pointer and screen RAM (for more info read the
"chrget boot.txt" file).

How to use
==========

BASIC Boot Maker works indifferently with real disk drives, inside a disk image
(.d64 or other formats) or inside a "modern" DOS file system folder, through a
SD2IEC interface or VICE Virtual FS.

Once started, the program loads the current disk directory and the user can
scroll entries through the UP and DOWN cursor keys and select a file with the
RETURN key to make it "bootable".

The load address of the selected file is checked to ensure that it resides in
the BASIC text area ($0801). Otherwise, the operation will abort and you will
be asked whether to choose another file or quit to BASIC.

If the file is recognized correctly, you will be asked to edit a one-line
message, which will be displayed during the boot phase.
After pressing RETURN another prompt asks the user to enter the name of the
file to be created.

The prompt suggests the same name as the source file preceded by a prefix, an
upwards arrow "^" in the style of JiffyDOS "LOAD and RUN" command.
In any case, the name cannot exceed 16 characters and the remaining characters
will be ignored.

Finally the user is asked to start processing or to cancel current operation.
After pressing RETURN, the process will start and the size of the source file
is checked to determine the BASIC pointers (VARTAB, ARYTAB and STREND), that
will be stored in boot header, to allow the auto-executable program to work
properly.

The bootable file
=================

If you want to load the newly generated auto-executable file, you need to enter
the following command:

	LOAD"file name",n,1

where 'n' is the selected drive and '1' tells the system to load program at
its own load address, without relocating it in the BASIC text area ($0801).

After a while, the boot message appears at the bottom of the screen, and at
the end of the loading process, the program will start automatically, without
having to type RUN.

If the program, at the end of execution, allows you to return to the BASIC
prompt, it can be listed, modified and saved as usual. Note that the newly
saved file will no longer be bootable.

Copyright (c) 2025 Graham (Francesco Gramignani)
https://graham-it.itch.io
https://github.com/graham-it
https://csdb.dk/scener/?id=40810
