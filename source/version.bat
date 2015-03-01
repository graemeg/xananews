@echo off
"c:\Program Files (x86)\Git\bin\git.exe" log --pretty=format:"'%%h';" -n 1 > version.inc
