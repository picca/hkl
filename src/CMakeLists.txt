include_directories(${HKL_SOURCE_DIR}/include)
file(GLOB sources *.c)
add_library(hkl SHARED ${sources})
install(TARGETS hkl
	LIBRARY DESTINATION lib)
