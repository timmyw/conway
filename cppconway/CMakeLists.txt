cmake_minimum_required(VERSION 3.5)

project(conway)

set(CMAKE_CXX_EXTENSIONS off)
set(CMAKE_CXX_STANDARD 14)
set(CXX_STANDARD_REQUIRED on)

# Boost
find_package(Boost COMPONENTS program_options REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})
link_libraries(${Boost_LIBRARIES})

set(src
  src/conway.cc
  src/board.cc
  src/boarddisplay.cc)

add_executable(conway ${src})
