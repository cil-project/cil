@echo off
set upload=perl C:/Necula/bin/Perl/upload.pl

call mkhtml.cmd

cd html
rm cil/*.haux
rm cil/cil.html

rm ccured/*.haux
rm ccured/ccured.html

tar cvfz ccured.taz ccured/*
set dest=http://raw.cs.berkeley.edu/cgi/upload.cmd
%upload% %dest%  ccured.taz

tar cvfz cil.taz cil/*
set dest=http://raw.cs.berkeley.edu/cgi/upload.cmd
%upload% %dest% cil.taz

cd ..
