
cd html
rm -rf *

mkdir ccured
cd ccured
hevea -exec xxdate.exe ../../setup
hevea -exec xxdate.exe ../../setup

hevea -exec xxdate.exe ../../ccured
hevea -exec xxdate.exe ../../ccured
hacha -o ccuredtoc.html ccured.html
cp ../../ccuredindex.html index.html
cp ../../ccuredheader.html . 
cd ..


mkdir cil
cd cil

hevea -exec xxdate.exe ../../cil
hevea -exec xxdate.exe ../../cil
hacha -o ciltoc.html cil.html
cp ../../cilindex.html index.html
cp ../../cilheader.html . 

cd ..
cd ..