cmake_minimum_required(VERSION 3.20)
project(jaspresults_python CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# python/ lives in jasp-desktop/Engine/jaspBase/python
get_filename_component(JASPBASE_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/.." ABSOLUTE)
get_filename_component(JASP_DESKTOP_ROOT "${JASPBASE_ROOT}/../.." ABSOLUTE)

set(COMMON_DIR "${JASP_DESKTOP_ROOT}/Common" CACHE PATH "location of jasp-desktop/Common")
message(STATUS "jaspresults python: using Common at ${COMMON_DIR}")
if(NOT EXISTS "${COMMON_DIR}/json/json_value.cpp")
	message(FATAL_ERROR "Common/json not found at ${COMMON_DIR} — pass -DCOMMON_DIR=")
endif()

# The R-free core (shared with the R engine build of jaspBase).
file(GLOB CORE_SOURCES CONFIGURE_DEPENDS "${JASPBASE_ROOT}/src/core/*.cpp")

find_package(pybind11 CONFIG REQUIRED)
pybind11_add_module(_jaspresults
	${CORE_SOURCES}
	src/_jaspresults/pyConversions.cpp
	src/_jaspresults/pyModule.cpp)
target_include_directories(_jaspresults PRIVATE
	"${JASPBASE_ROOT}/src/core"
	"${COMMON_DIR}"
	"src/_jaspresults")

install(TARGETS _jaspresults DESTINATION jaspresults)
