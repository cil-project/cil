#!/bin/sh

cd html
rm -f *


set HEVEADIR=C:\Programs\hevea

hevea -exec xxdate.exe ../setup
hevea -exec xxdate.exe ../setup

hevea -exec xxdate.exe ../cil
hevea -exec xxdate.exe ../cil
hacha -o ciltoc.html cil.html
cp ../cilindex.html .
cp ../cilheader.html . 


hevea -exec xxdate.exe ../ccured
hevea -exec xxdate.exe ../ccured
hacha -o ccuredtoc.html ccured.html
cp ../ccuredindex.html .
cp ../ccuredheader.html . 


rm -rf ccured
mkdir ccured
mkdir ccured\cil

cp ccured*.html ccured
mv ccured/ccuredindex.html ccured/index.html
cp *.gif ccured

cp cil*.html ccured/cil
cp *.gif ccured/cil 
mv ccured/cil/cilindex.html ccured/cil/index.html
cp setup*.html ccured/cil


cd ..
