@echo off
call mkhtml.cmd

cd html

tar cvfz ccured.taz ccured/*
    
set upload=perl C:/Necula/bin/Perl/upload.pl
set dest=http://raw.cs.berkeley.edu/cgi/upload.cmd
%upload% %dest%   ccured.taz

cd ..
