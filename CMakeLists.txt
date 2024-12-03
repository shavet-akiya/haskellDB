# Minimum CMake version required
cmake_minimum_required(VERSION 3.10)

# Set the project name and version
project(MongoFFI VERSION 1.0)

# Specify the required C++ standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Set the path for vcpkg toolchain file
set(CMAKE_TOOLCHAIN_FILE "C:/Users/bammi/Documents/vcpkg/scripts/buildsystems/vcpkg.cmake" CACHE STRING "vcpkg toolchain file")

# Add include directories for MongoC and BSON
include_directories(
    C:/Users/bammi/Documents/vcpkg/packages/mongo-c-driver_x64-windows/include
    C:/Users/bammi/Documents/vcpkg/packages/libbson_x64-windows/include
)

# Add link directories for MongoC and BSON
link_directories(
    C:/Users/bammi/Documents/vcpkg/packages/mongo-c-driver_x64-windows/lib
    C:/Users/bammi/Documents/vcpkg/packages/libbson_x64-windows/lib
)

# Create the library from your C source file
add_library(mongoffi SHARED 
    C:/Users/bammi/Documents/haskell/basicWebDev/src/mongo.c
)

# Link the MongoC and BSON libraries to your library
target_link_libraries(mongoffi PRIVATE 
    bson-1.0
    mongoc-1.0
)

# Specify the output directory for the compiled library
set_target_properties(mongoffi PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Debug
)

