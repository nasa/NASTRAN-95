RMDIR /S /Q build
MKDIR build
CD build
SET "PATH=C:\Windows\System32\;C:\Program Files\CMake\bin;"
CALL "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
@REM cmake -G "NMake Makefiles" .. -DCMAKE_Fortran_COMPILER=ifort
cmake -G "Ninja" .. -DCMAKE_Fortran_COMPILER=ifort
cmake --build .
@REM ctest --parallel
CD ..

