@echo off
for /f "tokens=*" %%i in ('opam env') do @%%i
cmd /k