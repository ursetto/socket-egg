@echo off
%CHICKEN_CSC% -C %CFLAGS% -L %LDFLAGS% socket-features.scm
echo "(import scheme)" > socket-config.scm
echo "(import (chicken platform))" >> socket-config.scm
./socket-features >> socket-config.scm
