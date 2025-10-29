# BASIC Boot Maker
...a C64 tool to make auto-executable programs in BASIC text area

[![](https://img.youtube.com/vi/lcbM7BnLqNE/0.jpg)](https://www.youtube.com/watch?v=lcbM7BnLqNE)

Version 1.05\
Released on October 29, 2025

Copyright (c) 2025 Graham (Francesco Gramignani)

## Not only BASIC
Source file could also be an M/L code program launched by a BASIC stub!

## Overview
This tool uses an undocumented (and probably never implemented before) exploit into LOAD command routine of KERNAL ROM, to auto-execute a program without the need of typing RUN, after the following command:

LOAD"file name",n,1

where 'n' is the device address, and '1' indicates that the file will be loaded into its own address memory.

## Features
- Full navigable directory via up/down cursor keys;
- Editable boot message and target file name;
- Data buffer to reduce head seeking during disk access.

## Credits
- Marco Baye (ACME 0.97 cross assembler) \
  https://sourceforge.net/projects/acme-crossass
- Leonardo Boselli (Mille e Una Avventura) \
  www.youdev.it
- Isaac Garcia Peveri (IGP Tech Blog) \
  https://github.com/isacco1975
- Claudio Daffra (projectCD.Chronicles) \
  https://github.com/ClaudioDaffra

## Links
- https://graham-it.itch.io
- https://github.com/graham-it
- https://csdb.dk/scener/?id=40810
