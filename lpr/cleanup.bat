@echo off
set fpc_dir=C:\dev\lazarus1.6.2-fpc3.0.0\fpc\3.0.0\bin\i386-win32
%fpc_dir%\strip ..\bin\rox.exe
erase /s /q ..\*.dbg
rmdir lib /s /q
