@echo off
set HEVEADIR=%PROGRAMS%\hevea
set OPATH=%PATH%
set PATH=%OPATH%;%HEVEADIR%

@echo on
cd html
%HEVEADIR%\hevea -exec xxdate.exe ../manual
%HEVEADIR%\hacha -o toc.html manual.html

cd ..
