@echo off
call mkhtml.cmd

cd html
REM Copy everything to a ccured directory 
mkdir ccured
copy *.html ccured
copy *.gif ccured
cp -r cil ccured
tar cvfz ccured.taz ccured/*

set upload=perl C:/Necula/bin/Perl/upload.pl
set dest=http://raw.cs.berkeley.edu/cgi/upload.cmd
%upload% %dest%   ccured.taz

rm -rf ccured
cd ..
