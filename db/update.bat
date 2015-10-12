svn update
@echo off
set PATH=C:\Program Files\WinRar;%PATH%
e:
cd ics\db
unrar e -o+ db
