cmake_minimum_required(VERSION 4.0)
project(untitled C)

set(CMAKE_C_STANDARD 11)

add_executable(untitled main.c
        common.h
        chunk.h
        chunk.c
        memory.h
        memory.c
        debug.c
        debug.h
        value.h
        value.c
        vm.h
        vm.c
        compiler.h
        compiler.c
        scanner.c
        scanner.h
        object.h
        object.c
        table.h
        table.c
        object.c)
