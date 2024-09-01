ECHO CALLING RSVARS.BAT
CALL "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
ECHO CALLING MSBuild
CALL MSBuild /v:diag /target:Build /p:config=Release /p:platform=Android64 /p:DCC_DcuOutput="C:\Program Files (x86)\Embarcadero\Componentes\TMS FNC WebSocket\Android64\Release" /p:DCC_HppOutput="C:\Program Files (x86)\Embarcadero\Componentes\TMS FNC WebSocket\Android64\Release" /p:DCC_ObjOutput="C:\Program Files (x86)\Embarcadero\Componentes\TMS FNC WebSocket\Android64\Release" /p:DCC_BpiOutput="C:\Users\Public\Documents\Embarcadero\Studio\22.0\Dcp\Android64" /p:DCC_DcpOutput="C:\Users\Public\Documents\Embarcadero\Studio\22.0\Dcp\Android64" /p:DCC_BplOutput="C:\Users\Public\Documents\Embarcadero\Studio\22.0\Bpl\Android64" FMXTMSFNCWebSocketPkgDXE14.dproj
ECHO COMPILATION DONE