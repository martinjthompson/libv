@ECHO OFF
setlocal

::Check GHDL version
set TESTEDVER=25
for /f "tokens=2" %%g in ('ghdl -v ^| findstr /i "edition"') do (
	set GHDLVER=%%g
)
set GHDLVER=%GHDLVER:"=%
for /f "delims=. tokens=1-2" %%v in ("%GHDLVER%") do (
	set MAJORVER=%%v
	set MINORVER=%%w
)
set MAJORVER=%MAJORVER:"=%
set MINORVER=%MINORVER:"=%

::Warn the user if they are using a GHDL version beyond 0.25, check if they wish to continue
if %MINORVER% GTR %TESTEDVER% (
	@echo WARNING: CURRENT VERSION OF GHDL IS %MAJORVER%.%MINORVER% - test_libv.bat has only been tested using GHDL 0.25 due to known issues with simulations in later versions of GHDL for Windows
	choice /M "Do you wish to continue" /c YN
	if errorlevel 255 (
	  echo Error
	) else if errorlevel 2 (
	  goto :no
	) else if errorlevel 1 (
	  goto :yes
	) else if errorlevel 0 (
	  echo Ctrl+C pressed.
	)
)

::Run the original test_libv script commands
:yes
ghdl -a libv.vhd
ghdl -e tb_libv
ghdl -r tb_libv  --assert-level=error
ECHO ------------------------------------------
ECHO If you see can read this, things worked OK!
:no
endlocal