rm -rf build
mkdir build
cd build
cmake -G "Ninja" ..
# cmake ..
cmake --build .
# cmake --build . --verbose
cd ..