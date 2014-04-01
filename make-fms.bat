@ECHO OFF
set target=fms
set target-temp=fms-temp
echo Lambdaroyal FSM Installer - this script downloads and builds the software into %target%. Using 

if exist %target% (
	echo Removing directory %target%
	rmdir /S /Q %target%
)
if exist %target-temp% (
	echo Removing directory %target-temp%
	rmdir /S /Q %target-temp%
)
echo Creating directory %target%
echo Creating directory %target-temp%

echo Downloading the sources
git clone https://github.com/lambdaroyal/fms %target-temp%
cd %target-temp%
echo Building fms
lein jar
lein uberjar

echo Find the software in %target%
echo You may start the software using java -jar wus-0.1-SNAPSHOT-standalone.jar --path ../samples
pause


