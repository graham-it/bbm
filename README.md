# BASIC Boot Maker
...a C64 tool to make auto-executable programs in BASIC text area.

Version 1.04\
Released on September 2, 2025

Copyright (c) 2025 Graham (Francesco Gramignani)\
https://graham-it.itch.io

## Not only BASIC
Source file could also be an M/L code program launched by a "BASIC stub"!

## Overview
This tool uses an undocumented (and probably never released) exploit into LOAD command routine of KERNAL ROM, to auto-execute a program file through CHRGET routine, EAL pointer and screen RAM manipulation.

## Release notes
Data buffer is extended to 4 KB to reduce head seeking during disk access and to speed-up the entire process.
