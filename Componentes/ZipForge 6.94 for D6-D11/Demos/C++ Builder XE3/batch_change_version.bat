@echo off
@SET from=ZipForged16
@SET to=ZipForged17

@SET mypath=%~dp0
@call :treeProcess
@goto :eof

:treeProcess
echo %cd%
@for %%f in (*proj) do (
	call "%mypath%BatchSubstitute.bat" %from% %to% "%cd%\%%f" >"%cd%\%%f2"
	@del /F /Q "%cd%\%%f"
	ren "%cd%\%%f2" "%%f"
)
@for /D %%d in (*) do @(
    @cd %%d
    @call :treeProcess
    @cd ..
)
@exit /b