@echo off
echo compiling: C64 Boot Maker
echo.

acme --cpu 6502 -f cbm -l bbm.sym -o "bbm" bbm.asm

if %errorlevel%==0 goto quit

echo.
pause

:quit
exit
