# BASIC Boot Maker
...a C64 tool to make auto-executable programs in BASIC text area

Version 1.05\
Released on October 28, 2025

## Not only BASIC
Source file could also be a M/L code program launched by a BASIC stub!

## Overview
This tool uses an undocumented (and probably never implemented before) exploit into LOAD command routine of KERNAL ROM, to auto-execute a program without the need of typing RUN, after the following command:

LOAD"file name",n,1

where 'n' is the device address, and '1' indicates that the file will be loaded into its own address memory.

## Features
- Full navigable directory via up/down cursor keys;
- Editable boot message and target file name;
- Data buffer to reduce head seeking during disk access.

Copyright (c) 2025 Graham (Francesco Gramignani)

https://graham-it.itch.io/bbm \
https://github.com/graham-it/bbm \
https://csdb.dk/release/?id=255831
