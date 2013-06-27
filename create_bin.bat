cd ..
mkdir bin\deps\bson
mkdir bin\deps\mongodb
mkdir bin\deps\kdht
copy deps\bson\ebin\*.* bin\deps\bson\
copy deps\mongodb\ebin\*.* bin\deps\mongodb\
copy deps\kdht\ebin\*.* bin\deps\kdht\
mkdir bin\www
copy www\*.* bin\www\
copy tools\*.* bin\
mkdir bin\priv
copy priv\*.* bin\priv\
mkdir bin\ebin         
copy ebin\*.* bin\ebin\
pause
