﻿unit UFSessao;

interface

uses
  Winapi.Windows, Messages, SysUtils, Variants, Graphics, Controls, Forms,
  Dialogs, ScktComp, StdCtrls, ExtCtrls, Buttons, ComCtrls, ImgList,
  Menus, Contnrs, vcl.Clipbrd, Classes, midaslib, vcl.Grids, vcl.DBGrids,
  vcl.ToolWin,
  ZLib, Thend, vcl.MPlayer, jpeg, Math, vcl.Imaging.pngimage,
  Wcrypt2, DCPcrypt2, DCPrijndael, DCPsha512, Data.DB,
  Datasnap.DBClient, System.ImageList, vcl.Imaging.GIFImg;

type
  TRemoto = class(TThread)
    procedure Execute; override;
  public
    Socket: TCustomWinSocket;
    Parar: Boolean;
    Zoom: Integer;
    Recort: Integer;
    ISQR:string;
    IdServidor: string;
  private
  end;

type
  TItemFila = class
  private
    FComando: string;
  public
    property Comando: String read FComando write FComando;
  end;

  TListaComandos = class(TObjectList)
  private
    function GetItem(Index: Integer): TItemFila;
    /// GetItem
    procedure SetItem(Index: Integer; const Value: TItemFila);
    /// SetItem
  public
    function New: TItemFila;
    property Items[Index: Integer]: TItemFila read GetItem write SetItem;
  end;

type
  TProcessa = class(TThread)
    procedure Execute; override;
  public
    Fila2: TListaComandos;
    SocketAtivo: TCustomWinSocket;
    ComandoOK: Boolean;
    iniciou: Boolean;
    RecebeuClipboard: Boolean;
    RecebeuSenhas: Boolean;
    ProcessouDados: Boolean;
    ChaveCript: String;
    function Ofuscador_Rmt(Action, Src: String): String;
    function RandoChave(strLen: Integer): string;
    procedure Processar;
    procedure ProcessaQrcod;

  private
  end;

type
  TFSESSAO = class(TForm)
    SessionBox: TScrollBox;
    pnPrinci: TPanel;
    ImSessao: TImage;
    PaintRecort: TPaintBox;
    PTela: TPanel;
    ImgF: TImage;
    pRecorte: TPanel;
    ImgLeft: TImage;
    ImgBot: TImage;
    ImgRichet: TImage;
    ImgTop: TImage;
    imgMenusImagens32x: TImageList;
    ImageList2: TImageList;
    tmrPopUp: TTimer;
    PopupMenu1: TPopupMenu;
    Salvarcomprovante1: TMenuItem;
    N2: TMenuItem;
    EnviarQrCod1: TMenuItem;
    N1: TMenuItem;
    Sair1: TMenuItem;
    tmrDado: TTimer;
    Media: TMediaPlayer;
    CatalogoXml: TClientDataSet;
    CatalogoXmlIPNET: TStringField;
    CatalogoXmlPWS: TStringField;
    dtsPws: TDataSource;
    Timer1: TTimer;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure ImSessaoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImSessaoMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ImSessaoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintRecortMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintRecortMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintRecortMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pnlCasasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tmrPopUpTimer(Sender: TObject);
    procedure pRecorteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pRecorteMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure pRecorteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Sair1Click(Sender: TObject);
    procedure Salvarcomprovante1Click(Sender: TObject);
    procedure EnviarQrCod1Click(Sender: TObject);
    procedure tmrDadoTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fNextViewer: HWND;
  public
    { Public declarations }
    Remoto: TRemoto;
    Processa: TProcessa;
    Socket: TCustomWinSocket;
    redimencionar: Boolean;
    oldPos: TPoint;

    inReposition: Boolean;
    XTela, YTela: Integer;
    MouseOK: Boolean;
    AbrePop: Boolean;
    AbreChat: Boolean;
    Casa: String;
    CreateCuttingIsDown: Boolean;
    SArea: TBitmap;
    a, b, c, d: Integer;
    procedure Resolucao_Tela;
    procedure ClipBoardChanged(var Message: TMessage); message WM_DRAWCLIPBOARD;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CapturaErro(Sender: TObject; E: Exception);

  end;

  TDataRecord = packed record
  end;

var
  FSESSAO: TFSESSAO;
  T16, T17, T18, T20: Boolean;
  AcumuladorMouse: Integer;
  TamanhoArquivo: int64;
  Inicio: Double;
  Segundos: int64;
  Mousetecla: String;
  Qualidade: Integer;
  Xx, Yy: Integer;
  MousePOSImage: TPoint;

  NaoRedimencionar: Boolean;
  StartX, StartY, X1, Y1, X2, Y2: Integer;
  Pausar: Boolean = false;
  TELAPOP: TForm;

implementation

{$R *.dfm}

uses UFCarga, UFCPrin, UBarra, UDAD, UGabesOddFormPanel;

{ TRemoto }

function RandoChave(strLen: Integer): string;
var
  str: string;
begin
  Randomize;
  str := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789¨´°¢~Ã¢§¨ÃÈÐË¡jns|bi­£jKJz}Lr';
  Result := '';
  repeat
    Result := Result + str[Random(Length(str)) + 1];
    FCPrin.Serial := Result;
  until (Length(Result) = strLen) end;

  Procedure Pausa(MSec: Cardinal);
  var
    Start: Cardinal;
  begin
    // Screen.Cursor := crHourGlass;
    Start := GetTickCount;
    repeat
      Application.ProcessMessages;
    until (GetTickCount - Start) >= MSec;
    Screen.Cursor := crDefault;
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
            Break;
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

  procedure ResumeStream(MyFirstStream, MySecondStream, MyCompareStream
    : TMemoryStream);
  var
    I: Integer;
    P1, P2, P3: ^AnsiChar;
  begin
    P1 := MyFirstStream.Memory;
    MySecondStream.SetSize(MyFirstStream.Size);
    P2 := MySecondStream.Memory;
    P3 := MyCompareStream.Memory;
    for I := 0 to MyFirstStream.Size - 1 do
    begin
      if P3^ = '0' then
        P2^ := P1^
      else
        P2^ := P3^;
      Inc(P1);
      Inc(P2);
      Inc(P3);
    end;
    MyFirstStream.Clear;
    MyFirstStream.CopyFrom(MySecondStream, 0);
    MySecondStream.Position := 0;
  end;

  function SplitString(S, Delimitador: string): TStringList;
  var
    p: Integer;
  begin
    Result := TStringList.Create;

    p := Pos(Delimitador, S);
    while (p > 0) do
    begin
      Result.Add(Copy(S, 1, p - 1));
      Delete(S, 1, p + Length(Delimitador) - 1);
      p := Pos(Delimitador, S);
    end;

    if (S <> '') then
      Result.Add(S);
  end;

  procedure TRemoto.Execute;
  var
    Comando: ansiString;
    I: Integer;
    cmdTela: ansiString;
    // ----------------------------------------------------------------------------
    MyFirstBmp, MySecondBmp, MyCompareBmp, UnPackStream,
      MyTempStream: TMemoryStream;
    // ----------------------------------------------------------------------------
    LstComando: TStringList;
    LObject: TListItem;
    MySize: Longint;

    // ----------------------------------------------------------------------------
  begin
    MyFirstBmp := TMemoryStream.Create;
    UnPackStream := TMemoryStream.Create;
    MyTempStream := TMemoryStream.Create;
    MySecondBmp := TMemoryStream.Create;
    MyCompareBmp := TMemoryStream.Create;
    MySize := 0;
    LstComando := TStringList.Create;
    for I := 0 to FCPrin.LV1.Items.Count - 1 do
    begin
      if FCPrin.LV1.Items.Item[I].SubItems.Objects[1] = TCustomWinSocket(Socket)
      then
        LObject := FCPrin.LV1.Items.Item[I];
    end;
    // ----------------------------------------------------------------------------
    While not Terminated and Socket.Connected do
    begin
      if Socket.ReceiveLength > 0 then
      begin
        Comando := string(Socket.ReceiveText);

        if MySize = 0 then
        begin
          if Pos('sValueT', Comando) > 0 then
          begin
            LstComando := SplitString(Comando, '<#>');
            cmdTela := LstComando[1];
            MySize := StrToInt(LstComando[1]);
            cmdTela := '';
            Socket.SendText(FCarga.vDc_vCri('C', 'siZeOk:'));
            (LObject.SubItems.Objects[2] as TFSESSAO).PTela.Visible := false;
          end;

        end
        else
        begin
          cmdTela := cmdTela + Comando;
          if Length(cmdTela) >= MySize then
          begin
            MyTempStream.Write(cmdTela[1], MySize);
            MyTempStream.Position := 0;
            UnPackStream.Clear;
            UnPackStream.LoadFromStream(MyTempStream);
            DoExpand(UnPackStream);
            UnPackStream.Position := 0;
            if MyFirstBmp.Size = 0 then
            begin
              MyFirstBmp.CopyFrom(UnPackStream, 0);
              MyFirstBmp.Position := 0;

              (LObject.SubItems.Objects[2] as TFSESSAO)
                .ImSessao.Picture.Bitmap.LoadFromStream(MyFirstBmp);
              UnPackStream.Clear;
              MyTempStream.Clear;
              MySecondBmp.Clear;
              MyCompareBmp.Clear;
            end
            else
            begin
              MyCompareBmp.Clear;
              MySecondBmp.Clear;
              MyCompareBmp.CopyFrom(UnPackStream, 0);
              ResumeStream(MyFirstBmp, MySecondBmp, MyCompareBmp);
              if LObject.SubItems.Objects[2] <> nil then
                if not(LObject.SubItems.Objects[2] as TFSESSAO).Remoto.Parar
                then
                  (LObject.SubItems.Objects[2] as TFSESSAO)
                    .ImSessao.Picture.Bitmap.LoadFromStream(MySecondBmp);
            end;
            MySize := 0;
            UnPackStream.Clear;
            MyTempStream.Clear;
            MySecondBmp.Clear;
            MyCompareBmp.Clear;
            LstComando.Free;
            if ((LObject.SubItems.Objects[2] <> nil) and
              (LObject.SubItems.Objects[2] as TFSESSAO).Visible) then
              Socket.SendText(FCarga.vDc_vCri('C', 'gets:'));
            TrimAppMemorySize;
          end;
        end;

      end;

      Sleep(10);
    end;

  end;

  { TListaComandos }

  function TListaComandos.GetItem(Index: Integer): TItemFila;
  begin
    Result := TItemFila(Inherited Items[Index]);
  end;

  function TListaComandos.New: TItemFila;
  begin
    Result := TItemFila.Create;
    Add(Result);
  end;

  procedure TListaComandos.SetItem(Index: Integer; const Value: TItemFila);
  begin
    Put(Index, Value);
  end;

  { TFSESSAO }

  procedure TFSESSAO.CapturaErro(Sender: TObject; E: Exception);
  var
    erros: TStringList;
  var
    L: TListItem;
  begin
    try
      erros := TStringList.Create;
      erros.Add(E.Message);
      erros.Free;
      Screen.Cursor := crDefault;
      Application.ProcessMessages;
      TrimAppMemorySize;
    finally

    end;

  end;

  procedure TFSESSAO.ClipBoardChanged(var Message: TMessage);
  var
    L: TListItem;
  begin
    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin

      if Assigned(Processa) then
      begin
        if not Processa.RecebeuClipboard then
        begin
          if Clipboard.HasFormat(CF_TEXT) then
          begin
            with Processa.Fila2.New do
            begin
              Comando := '#clb#<#>' + Clipboard.AsText;

            end;
          end;
        end
        else
          Processa.RecebeuClipboard := false;

      end;
    end;
  end;

  procedure TFSESSAO.CreateParams(var Params: TCreateParams);
  begin
    inherited CreateParams(Params);
    with Params do
    begin
      ExStyle := ExStyle or WS_EX_APPWINDOW;
      WndParent := GetDesktopwindow;
    end;
  end;

  procedure TFSESSAO.EnviarQrCod1Click(Sender: TObject);
  var
    L: TListItem;
  begin
    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin
      with Processa.Fila2.New do
      begin
        Comando := '#QR#<#>' + intToStr((L.SubItems.Objects[2] as TFSESSAO)
          .pRecorte.Left + (L.SubItems.Objects[2] as TFSESSAO)
          .SessionBox.HorzScrollBar.Position) + '<#>' +
          intToStr(((L.SubItems.Objects[2] as TFSESSAO).pRecorte.top +
          (L.SubItems.Objects[2] as TFSESSAO).VertScrollBar.Position)) + '<#>' +
          intToStr((L.SubItems.Objects[2] as TFSESSAO).pRecorte.Width) + '<#>' +
          intToStr((L.SubItems.Objects[2] as TFSESSAO).pRecorte.Height) + '<#>';
        (L.SubItems.Objects[2] as TFSESSAO).pRecorte.Visible := false;
        Processa.ProcessaQrcod;
      end;
    end;

  end;

  procedure TFSESSAO.FormClose(Sender: TObject; var Action: TCloseAction);
  var
    L: TListItem;
  begin
    try
      L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false,
        True, false);
      if L <> nil then
      begin
        (L.SubItems.Objects[3] as TFBarraRemota).Close;
        (L.SubItems.Objects[4] as TFUDAD).Close;

        L.SubItems.Objects[1] := nil;
        L.SubItems.Objects[2] := nil;
        L.SubItems.Objects[3] := nil;
        L.SubItems.Objects[4] := nil;
        RemoveClipboardFormatListener(Handle);
        Processa.Terminate;
        Remoto.Terminate;
        Destroy;
        TrimAppMemorySize;
      end;
    except
    end;
  end;

  procedure TFSESSAO.FormCreate(Sender: TObject);
  begin
    (ImgF.Picture.Graphic as TGIFImage).Animate := True;

  end;

  procedure TFSESSAO.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
  begin
    if Assigned(Processa) then
    begin
      if Processa.iniciou then
        case Key of
          16:
            begin
              if not T16 then
              begin
                with Processa.Fila2.New do
                begin
                  Comando := '#kd#<#>' + intToStr(Key);
                end;
                T16 := True;
              end;
            end;
          17:
            begin
              if not T17 then
              begin
                with Processa.Fila2.New do
                begin
                  Comando := '#kd#<#>' + intToStr(Key);
                end;
                T17 := True;
              end;
            end;
          18:
            begin
              if not T18 then
              begin
                with Processa.Fila2.New do
                begin
                  Comando := '#kd#<#>' + intToStr(Key);
                end;
                T18 := True;
              end;
            end;
          20:
            begin
              if not T20 then
              begin
                with Processa.Fila2.New do
                begin
                  Comando := '#kd#<#>' + intToStr(Key);
                end;
                T20 := True;
              end;
            end
        else
          begin
            with Processa.Fila2.New do
            begin
              Comando := '#kd#<#>' + intToStr(Key);
            end;
          end;
        end;

    end;

    if (Key = VK_LWIN) or (Key = VK_RWIN) then
    begin
      Key := 0;
    end;
    if (Key = VK_F4) and (ssAlt in Shift) then
      Key := 0;
  end;

  procedure TFSESSAO.FormKeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
  begin
    if Assigned(Processa) then
    begin
      case Key of
        16:
          T16 := false;
        17:
          T17 := false;
        18:
          T18 := false;
        20:
          T20 := false;
      end;
      if Processa.iniciou then
        case Key of
          16, 17, 18, 20:
            begin
              with Processa.Fila2.New do
              begin
                Comando := '#ku#<#>' + intToStr(Key);
              end;

            end;
        end;
      if (Key = VK_LWIN) or (Key = VK_RWIN) then
        Key := 0;
      if (Key = VK_F4) and (ssAlt in Shift) then
        Key := 0;

    end;
  end;

  procedure TFSESSAO.FormMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  var
    Xver, Yver: Integer;
  begin
    if Assigned(Processa) then
    begin
      Xver := Round((XTela / pnPrinci.Width) * X);
      Yver := Round((YTela / pnPrinci.Height) * Y);
      if Processa.iniciou then
      begin
        if ssLeft in Shift then
        begin
          Mousetecla := 'esquerdo';
          with Processa.Fila2.New do
          begin
            Comando := '#mde#<#>' + intToStr(Xver) + '<#>' + intToStr(Yver);
          end;

        end;
        if ssRight in Shift then
        begin
          Mousetecla := 'direito';
          with Processa.Fila2.New do
          begin
            Comando := '#mdd#<#>' + intToStr(Xver) + '<#>' + intToStr(Yver);
          end;

        end;
      end;
    end;
  end;

  procedure TFSESSAO.FormMouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
  var
    Xver, Yver: Integer;
  begin
    if Assigned(Processa) then
    begin
      try
        Xver := Round((XTela / pnPrinci.Width) * X);
        MousePOSImage.X := Xver;
        Yver := Round((YTela / pnPrinci.Height) * Y);
        MousePOSImage.Y := Yver;
        if (Processa.iniciou) then
        begin
          if ((Mousetecla = 'esquerdo') or (Mousetecla = 'direito')) then
          begin
            if AcumuladorMouse > 10 then
            begin
              with Processa.Fila2.New do
              begin
                Comando := '#mmv#<#>' + intToStr(Xver) + '<#>' + intToStr(Yver);
              end;
              AcumuladorMouse := 0;
            end;
            AcumuladorMouse := AcumuladorMouse + 1;
          end
          else
          begin
            if AcumuladorMouse > 10 then
            begin
              with Processa.Fila2.New do
              begin
                Comando := '#mmv#<#>' + intToStr(Xver) + '<#>' + intToStr(Yver);
              end;
              AcumuladorMouse := 0;
            end;
            AcumuladorMouse := AcumuladorMouse + 1;
          end;
        end;
      finally
        Application.ProcessMessages;
      end;
    end;
  end;

  procedure TFSESSAO.FormMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  begin
    if Assigned(Processa) then
    begin
      if Processa.iniciou then
      begin
        if Mousetecla = 'esquerdo' then
        begin
          with Processa.Fila2.New do
          begin
            Comando := '#mue#' + #0;

          end;
        end;
        if Mousetecla = 'direito' then
        begin
          with Processa.Fila2.New do
          begin
            Comando := '#mud#' + #0;

          end;
        end;
        Mousetecla := 'solto';

      end;
    end;
  end;

  procedure TFSESSAO.FormResize(Sender: TObject);
  var
    L: TListItem;
    w, h, cw, ch: Integer;
    xyaspect: Double;
    NaoRedimencionar: Boolean;
  begin
    if ImgF.Width >= ClientWidth then
      ImgF.Left := 0;
    if ImgF.Height >= ClientHeight then
      ImgF.top := 0;

    ImgF.top := (ClientHeight - ImgF.Height) div 2;
    ImgF.Left := (ClientWidth - ImgF.Width) div 2;
    ImgF.Visible := True;

    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin
      SetClipboardViewer((L.SubItems.Objects[2] as TFSESSAO).Handle);

      (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.top :=
        (ClientHeight - (L.SubItems.Objects[2] as TFSESSAO)
        .pnPrinci.Height) div 2;
      (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.Left :=
        (ClientWidth - (L.SubItems.Objects[2] as TFSESSAO)
        .pnPrinci.Width) div 2;

      if (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.Width >= ClientWidth then
        (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.Left := 0;
      if (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.Height >= ClientHeight
      then
        (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.top := 0;

      if Assigned((L.SubItems.Objects[3] as TFBarraRemota)) then
        (L.SubItems.Objects[3] as TFBarraRemota).Left :=
          (ClientWidth - (L.SubItems.Objects[3] as TFBarraRemota).Width) div 2;

    end;
  end;

  procedure TFSESSAO.FormShow(Sender: TObject);
  var
    L: TListItem;
  begin
    AcumuladorMouse := 0;
    MouseOK := false;
    T16 := false;
    T17 := false;
    T18 := false;
    T20 := false;
    Inicio := 0;
    Segundos := 0;

    if pnPrinci.Width >= ClientWidth then
      pnPrinci.Left := 0;

    if pnPrinci.Height >= ClientHeight then
      pnPrinci.top := 0;

    pnPrinci.top := (ClientHeight - pnPrinci.Height) div 2;
    pnPrinci.Left := (ClientWidth - pnPrinci.Width) div 2;

    if pnPrinci.Width >= ClientWidth then
      pnPrinci.Left := 0;

    if pnPrinci.Height >= ClientHeight then
      pnPrinci.top := 0;

    if pRecorte.Width >= ClientWidth then
      pRecorte.Left := 0;

    if pRecorte.Height >= ClientHeight then
      pRecorte.top := 0;

    pRecorte.top := (ClientHeight - pRecorte.Height) div 2;
    pRecorte.Left := (ClientWidth - pRecorte.Width) div 2;

    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin
      SetClipboardViewer((L.SubItems.Objects[2] as TFSESSAO).Handle);

      if Assigned((L.SubItems.Objects[3] as TFBarraRemota)) then
      begin

        (L.SubItems.Objects[3] as TFBarraRemota).top :=
          (0 - (L.SubItems.Objects[3] as TFBarraRemota).Height) + 20;

        (L.SubItems.Objects[3] as TFBarraRemota).Left :=
          (ClientWidth - (L.SubItems.Objects[3] as TFBarraRemota).Width) div 2;

        if Assigned((L.SubItems.Objects[4] as TFUDAD)) then

          (L.SubItems.Objects[4] as TFUDAD).Image2Click(self);
        (L.SubItems.Objects[4] as TFUDAD).top := 50;

        (L.SubItems.Objects[2] as TFSESSAO).Media.FileName :=
          ExtractFilePath(Application.ExeName) + 'sms.mp3';
        (L.SubItems.Objects[2] as TFSESSAO).Media.Open;

        { if not FileExists(ExtractFilePath(Application.ExeName)+'Catalogo.xml') then
          begin
          (l.SubItems.Objects[2] as TFSESSAO).CatalogoXml.CreateDataSet;
          (l.SubItems.Objects[2] as TFSESSAO).CatalogoXml.SaveToFile(ExtractFilePath(Application.ExeName)+'Catalogo.xml',dfXML);
          CatalogoXml.FileName:= ExtractFilePath(Application.ExeName)+'Catalogo.xml';

          (l.SubItems.Objects[4] as TFUDAD).DBGrid1.DataSource:=
          (l.SubItems.Objects[2] as TFSESSAO).dtsPws;

          (l.SubItems.Objects[4] as TFUDAD).DBNavigator1.DataSource:=
          (l.SubItems.Objects[2] as TFSESSAO).dtsPws;

          (l.SubItems.Objects[2] as TFSESSAO).CatalogoXml.close;
          (l.SubItems.Objects[2] as TFSESSAO).CatalogoXml.Open;

          end else
          begin
          (l.SubItems.Objects[4] as TFUDAD).DBGrid1.DataSource:=
          (l.SubItems.Objects[2] as TFSESSAO).dtsPws;

          (l.SubItems.Objects[4] as TFUDAD).DBNavigator1.DataSource:=
          (l.SubItems.Objects[2] as TFSESSAO).dtsPws;

          (l.SubItems.Objects[2] as TFSESSAO).CatalogoXml.FileName:=
          ExtractFilePath(Application.ExeName)+'Catalogo.xml';
          (l.SubItems.Objects[2] as TFSESSAO).CatalogoXml.close;
          (l.SubItems.Objects[2] as TFSESSAO).CatalogoXml.Open;
          end; }

      end;
    end;

  end;

  procedure TFSESSAO.ImSessaoMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  var
    Xver, Yver: Integer;
  begin
    if Assigned(Processa) then
    begin
      Xver := Round((XTela / pnPrinci.Width) * X);
      Yver := Round((YTela / pnPrinci.Height) * Y);
      if Processa.iniciou then
      begin
        if ssLeft in Shift then
        begin
          Mousetecla := 'esquerdo';
          with Processa.Fila2.New do
          begin
            Comando := '#mde#<#>' + intToStr(Xver) + '<#>' + intToStr(Yver);
          end;

        end;
        if ssRight in Shift then
        begin
          Mousetecla := 'direito';
          with Processa.Fila2.New do
          begin
            Comando := '#mdd#<#>' + intToStr(Xver) + '<#>' + intToStr(Yver);
          end;

        end;
      end;
    end;
  end;

  procedure TFSESSAO.ImSessaoMouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
  var
    Xver, Yver: Integer;
  begin
    if Assigned(Processa) then
    begin
      try
        Xver := Round((XTela / pnPrinci.Width) * X);
        MousePOSImage.X := Xver;
        Yver := Round((YTela / pnPrinci.Height) * Y);
        MousePOSImage.Y := Yver;
        if (Processa.iniciou) then
        begin
          if ((Mousetecla = 'esquerdo') or (Mousetecla = 'direito')) then
          begin
            if AcumuladorMouse > 10 then
            begin
              with Processa.Fila2.New do
              begin
                Comando := '#mmv#<#>' + intToStr(Xver) + '<#>' + intToStr(Yver);
              end;
              AcumuladorMouse := 0;
            end;
            AcumuladorMouse := AcumuladorMouse + 1;
          end
          else
          begin
            if AcumuladorMouse > 10 then
            begin
              with Processa.Fila2.New do
              begin
                // Comando :=  'mmv:' + IntToStr( Xver ) + ',' + IntToStr( Yver ) +#0;
                Comando := '#mmv#<#>' + intToStr(Xver) + '<#>' + intToStr(Yver);
              end;
              AcumuladorMouse := 0;
            end;
            AcumuladorMouse := AcumuladorMouse + 1;
          end;
        end;
      finally
        Application.ProcessMessages;
      end;
    end;

  end;

  procedure TFSESSAO.ImSessaoMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  begin
    if Assigned(Processa) then
    begin
      if Processa.iniciou then
      begin
        if Mousetecla = 'esquerdo' then
        begin
          with Processa.Fila2.New do
          begin
            Comando := '#mue#' + #0;

          end;
        end;
        if Mousetecla = 'direito' then
        begin
          with Processa.Fila2.New do
          begin
            Comando := '#mud#' + #0;

          end;
        end;
        Mousetecla := 'solto';

      end;
    end;
  end;

  procedure TFSESSAO.PaintRecortMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  begin
    CreateCuttingIsDown := True;
    StartX := X;
    StartY := Y;
    X1 := X;
    Y1 := Y;
    X2 := X;
    Y2 := Y;
    a := X;
    b := Y;

  end;

  procedure TFSESSAO.PaintRecortMouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
  begin
    if CreateCuttingIsDown then
    begin
      PaintRecort.Canvas.Pen.Color := clWindow;
      PaintRecort.Canvas.Pen.Style := psDot;
      PaintRecort.Canvas.Pen.Mode := pmNotXor;
      PaintRecort.Canvas.Rectangle(X1, Y1, X2, Y2);
      X2 := X;
      Y2 := Y;
      PaintRecort.Canvas.Rectangle(StartX, StartY, X, Y);
    end;
  end;

  procedure TFSESSAO.PaintRecortMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
AreaRecorte:char;
Xver, Yver: Integer;
L: TListItem;
dc: HDC;
R:TRect;
TELAJPG: TJPEGImage;
begin
L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, true, false);
if L <> nil then
begin
  CreateCuttingIsDown := False;
  PaintRecort.Canvas.Pen.Style := psDot;
  PaintRecort.Canvas.Pen.Mode := pmNotXor;
  PaintRecort.Canvas.Rectangle(X1, Y1, X2, Y2);
  X1 :=Round( ( XTela / pnPrinci.Width ) * StartX );
  Y1 :=Round( ( XTela / pnPrinci.Width ) * Starty );
  X2 :=Round( ( XTela / pnPrinci.Width ) * X );
  Y2 :=Round( ( XTela / pnPrinci.Width ) * y) ;

  if  MessageBox((l.SubItems.Objects[2] as TFSESSAO).Handle,pchar('Recorte selecionado em: ' +
  IntToStr(StartX) + ', ' + IntToStr(StartY) + ', ' + IntToStr(X) + ', ' +
  IntToStr(Y)),pchar('Confirmar ?'),
  mb_yesno+MB_DEFBUTTON2)=mrNo then
  begin
       PaintRecort.Visible:=false;
       (L.SubItems.Objects[2] as TFSESSAO).Remoto.Parar:=False;
       X1:=0;
       Y1:=0;
       X2:=0;
       Y2:=0;

       Abort;
  end else
  begin
  with Processa.Fila2.New do
  begin
      if (l.SubItems.Objects[2] as TFSESSAO).Remoto.Recort=0 then
      begin
            Comando :='#RC#'+'<#>'+intToStr(x1)+'<#>'+intToStr(Y1)+'<#>'+
            intToStr(x2)+'<#>'+intToStr(Y2)+'<#>'+(l.SubItems.Objects[2] as TFSESSAO).Remoto.isqr+'<#>';
      end else
      begin
            Comando :='#QR#'+'<#>'+intToStr(x1)+'<#>'+intToStr(Y1)+'<#>'+
            intToStr(x2)+'<#>'+intToStr(Y2)+'<#>';
            Pausa(3);
            with Processa.Fila2.New do
            begin
                Comando :='#Cmd#'+'<#>'+'0'+'<#>'+
                intToStr((l.SubItems.Objects[3] as TFBarraRemota).opcao)+'<#>'+
               (l.SubItems.Objects[3] as TFBarraRemota).Url;
  end;
      end;
  end;
  PaintRecort.Visible:=false;
  (L.SubItems.Objects[2] as TFSESSAO).Remoto.Parar:=False;
    X1:=0;
    Y1:=0;
    X2:=0;
    Y2:=0;
  end;
  end;


end;

  procedure TFSESSAO.pnlCasasMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  begin
    ReleaseCapture;
  end;

  procedure TFSESSAO.pRecorteMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  var
    L: TListItem;
  begin
    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin
      if Button = mbRight then
      begin
        (L.SubItems.Objects[2] as TFSESSAO).redimencionar := True;
      end
      else
      begin
        (L.SubItems.Objects[2] as TFSESSAO).redimencionar := false;
      end;
      if ((L.SubItems.Objects[2] as TFSESSAO).pRecorte is TWinControl) then
      begin
        (L.SubItems.Objects[2] as TFSESSAO).inReposition := True;
        SetCapture(TWinControl((L.SubItems.Objects[2] as TFSESSAO)
          .pRecorte).Handle);
        GetCursorPos((L.SubItems.Objects[2] as TFSESSAO).oldPos);
      end;
    end;

  end;

  procedure TFSESSAO.pRecorteMouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
  const
    minWidth = 20;
    minHeight = 20;
  var
    newPos: TPoint;
    frmPoint: TPoint;
    L: TListItem;
  begin
    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin
      if (L.SubItems.Objects[2] as TFSESSAO).inReposition then
      begin
        with TWinControl((L.SubItems.Objects[2] as TFSESSAO).pRecorte) do
        begin
          GetCursorPos(newPos);
          if (L.SubItems.Objects[2] as TFSESSAO).redimencionar then
          begin // resize
            Screen.Cursor := crSizeNWSE;
            frmPoint := ScreenToClient(Mouse.CursorPos);
            if frmPoint.X > minWidth then
              (L.SubItems.Objects[2] as TFSESSAO).Width := frmPoint.X;
            if frmPoint.Y > minHeight then
              (L.SubItems.Objects[2] as TFSESSAO).Height := frmPoint.Y;
          end
          else // move
          begin
            Screen.Cursor := crSize;
            Left := Left - oldPos.X + newPos.X;
            top := top - oldPos.Y + newPos.Y;
            oldPos := newPos;
          end;
        end;
      end;
    end;

  end;

  procedure TFSESSAO.pRecorteMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  var
    L: TListItem;
  begin
    if Button = mbRight then
    begin
      L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false,
        True, false);
      if L <> nil then
      begin

        (L.SubItems.Objects[2] as TFSESSAO).PopupMenu1.Popup(Mouse.CursorPos.X,
          Mouse.CursorPos.Y);
      end;

      if (L.SubItems.Objects[2] as TFSESSAO).inReposition then
      begin
        Screen.Cursor := crDefault;
        ReleaseCapture;
        (L.SubItems.Objects[2] as TFSESSAO).inReposition := false;
      end;
    end;

  end;

  procedure TFSESSAO.Resolucao_Tela;
  var
    L: TListItem;
    CalcZoom: Integer;
    CalcZoom2: Integer;

  begin
    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin

      (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.Width :=
        (XTela * (L.SubItems.Objects[2] as TFSESSAO).Remoto.Zoom) div 100;
      (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.Height :=
        (YTela * (L.SubItems.Objects[2] as TFSESSAO).Remoto.Zoom) div 100;

      (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.top :=
        ((L.SubItems.Objects[2] as TFSESSAO).ClientHeight -
        (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.Height) div 2;
      (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.Left :=
        ((L.SubItems.Objects[2] as TFSESSAO).ClientWidth -
        (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.Width) div 2;

      if (L.SubItems.Objects[2] as TFSESSAO).pnPrinci.Width >=
        (L.SubItems.Objects[2] as TFSESSAO).ClientWidth then
        (L.SubItems.Objects[2] as TFSESSAO).Left := 0;

      if (L.SubItems.Objects[2] as TFSESSAO).Height >=
        (L.SubItems.Objects[2] as TFSESSAO).ClientHeight then
        (L.SubItems.Objects[2] as TFSESSAO).top := 0;

    end;
  end;

  procedure TFSESSAO.Sair1Click(Sender: TObject);
  var
    L: TListItem;
  begin
    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin
      (L.SubItems.Objects[2] as TFSESSAO).pRecorte.Visible := false;

      if (L.SubItems.Objects[2] as TFSESSAO).pRecorte.Width >= ClientWidth then
        (L.SubItems.Objects[2] as TFSESSAO).pRecorte.Left := 0;

      if (L.SubItems.Objects[2] as TFSESSAO).pRecorte.Height >= ClientHeight
      then
        (L.SubItems.Objects[2] as TFSESSAO).pRecorte.top := 0;

      (L.SubItems.Objects[2] as TFSESSAO).pRecorte.top :=
        (ClientHeight - (L.SubItems.Objects[2] as TFSESSAO)
        .pRecorte.Height) div 2;
      (L.SubItems.Objects[2] as TFSESSAO).pRecorte.Left :=
        (ClientWidth - (L.SubItems.Objects[2] as TFSESSAO)
        .pRecorte.Width) div 2;

    end;

  end;

  procedure TFSESSAO.Salvarcomprovante1Click(Sender: TObject);
  var
    L: TListItem;
  begin
    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin
      with Processa.Fila2.New do
      begin
        Comando := '#RC#<#>' + intToStr((L.SubItems.Objects[2] as TFSESSAO)
          .pRecorte.Left + (L.SubItems.Objects[2] as TFSESSAO)
          .SessionBox.HorzScrollBar.Position) + '<#>' +
          intToStr(((L.SubItems.Objects[2] as TFSESSAO).pRecorte.top +
          (L.SubItems.Objects[2] as TFSESSAO).VertScrollBar.Position)) + '<#>' +
          intToStr((L.SubItems.Objects[2] as TFSESSAO).pRecorte.Width) + '<#>' +
          intToStr((L.SubItems.Objects[2] as TFSESSAO).pRecorte.Height) + '<#>';
        (L.SubItems.Objects[2] as TFSESSAO).pRecorte.Visible := false;
      end;
    end;

  end;

  procedure TFSESSAO.Timer1Timer(Sender: TObject);
  begin
    with Processa.Fila2.New do
    begin
      Comando := '#S#'
    end;
  end;

  procedure TFSESSAO.tmrDadoTimer(Sender: TObject);
  var
    C1, C2: Integer;
    L: TListItem;
  begin
    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin

      if (L.SubItems.Objects[2] as TFSESSAO).AbreChat then
      begin
        C1 := (L.SubItems.Objects[4] as TFUDAD).Left + 10;
        C2 := -10;
        if C1 > C2 then
        begin
          (L.SubItems.Objects[2] as TFSESSAO).tmrDado.Enabled := false;
          (L.SubItems.Objects[4] as TFUDAD).Left := -10;
          (L.SubItems.Objects[2] as TFSESSAO).AbreChat := false;
        end
        else
        begin
          (L.SubItems.Objects[4] as TFUDAD).Left :=
            (L.SubItems.Objects[4] as TFUDAD).Left + 10;
          if (L.SubItems.Objects[4] as TFUDAD).Left = -10 then
          begin
            (L.SubItems.Objects[2] as TFSESSAO).tmrDado.Enabled := false;
            (L.SubItems.Objects[2] as TFSESSAO).AbreChat := false;
          end;
        end;

      end
      else
      begin
        if ((L.SubItems.Objects[4] as TFUDAD).Left - 10) <
          ((0 - (L.SubItems.Objects[4] as TFUDAD).ClientWidth) + 10) then
        begin
          (L.SubItems.Objects[2] as TFSESSAO).tmrDado.Enabled := false;
          (L.SubItems.Objects[4] as TFUDAD).Left :=
            ((0 - (L.SubItems.Objects[4] as TFUDAD).ClientWidth) + 10);
          (L.SubItems.Objects[2] as TFSESSAO).AbreChat := True;
        end
        else
        begin

          (L.SubItems.Objects[4] as TFUDAD).Left :=
            (L.SubItems.Objects[4] as TFUDAD).Left - 10;
          if (L.SubItems.Objects[4] as TFUDAD)
            .Left = ((0 - (L.SubItems.Objects[4] as TFUDAD).ClientWidth) + 10)
          then
          begin
            (L.SubItems.Objects[2] as TFSESSAO).AbreChat := True;
            (L.SubItems.Objects[2] as TFSESSAO).tmrDado.Enabled := false;
          end;
        end;

      end;
    end;

  end;

  procedure TFSESSAO.tmrPopUpTimer(Sender: TObject);
  var
    L: TListItem;
  begin
    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin
      if (L.SubItems.Objects[2] as TFSESSAO).AbrePop then
      begin
        if (L.SubItems.Objects[3] as TFBarraRemota)
          .top = (-10 - (L.SubItems.Objects[3] as TFBarraRemota).TamanhoTitulo)
        then
        begin
          (L.SubItems.Objects[2] as TFSESSAO).tmrPopUp.Enabled := false;
          (L.SubItems.Objects[2] as TFSESSAO).AbrePop := false;
        end;
        if ((L.SubItems.Objects[3] as TFBarraRemota).top + 10) >
          (-10 - (L.SubItems.Objects[3] as TFBarraRemota).TamanhoTitulo) then

          (L.SubItems.Objects[3] as TFBarraRemota).top :=
            (-10 - (L.SubItems.Objects[3] as TFBarraRemota).TamanhoTitulo)
        else
          (L.SubItems.Objects[3] as TFBarraRemota).top :=
            (L.SubItems.Objects[3] as TFBarraRemota).top + 10;
      end
      else
      begin
        if (L.SubItems.Objects[3] as TFBarraRemota)
          .top = ((0 - (L.SubItems.Objects[3] as TFBarraRemota).Height) + 20)
        then
        begin
          (L.SubItems.Objects[2] as TFSESSAO).tmrPopUp.Enabled := false;
          (L.SubItems.Objects[2] as TFSESSAO).AbrePop := True;
        end;

        if ((L.SubItems.Objects[3] as TFBarraRemota).top - 10) <
          ((0 - (L.SubItems.Objects[3] as TFBarraRemota).Height) + 20) then
          (L.SubItems.Objects[3] as TFBarraRemota).top :=
            ((0 - (L.SubItems.Objects[3] as TFBarraRemota).Height) + 20)
        else
          (L.SubItems.Objects[3] as TFBarraRemota).top :=
            (L.SubItems.Objects[3] as TFBarraRemota).top - 10;
      end;

    end;
  end;

  procedure TProcessa.Execute;
  var
    Comando: String;
    Aux: String;
    L: TListItem;
    LstComando: TStringList;
    idServer: Integer;
  begin
    try
      // ----------------------------------------------------------------------------
      While not Terminated and SocketAtivo.Connected do
      begin
        if SocketAtivo.ReceiveLength > 0 then
        begin

          Comando := Ofuscador_Rmt('D', String(SocketAtivo.ReceiveText));
          if Pos('#S#', Comando) > 0 then
          begin
            ComandoOK := True;
            ProcessouDados := false;

          end;

          if Pos('#RE#', Comando) > 0 then
          begin
            if not ProcessouDados then
            begin
              ProcessouDados := True;
              LstComando := TStringList.Create;
              LstComando := SplitString(Comando, '<#>');

              L := FCPrin.LV1.FindCaption(0, LstComando[1], false, True, false);
              if L <> nil then
              begin

                (L.SubItems.Objects[4] as TFUDAD)
                  .edTexto.Lines.Add('** ' + LstComando[2] + ' **');

                // (l.SubItems.Objects[2] as TFSESSAO).Media.Open;
                // (l.SubItems.Objects[2] as TFSESSAO).Media.Play;
                Synchronize(FCPrin.Alertasenha);

                (L.SubItems.Objects[4] as TFUDAD).Image2Click(self);
              end;
              Clipboard.AsText := LstComando[2];
              LstComando.Free;
            end;
            // RecebeuSenhas:=true;
          end;

        end;
        Synchronize(Processar);
        Sleep(10);
      end;
    finally

    end;
  end;

  // ----------------------------------------------------------------------------

  function TProcessa.Ofuscador_Rmt(Action, Src: String): String;
  var
    Key: String;
    Cipher: TDCP_rijndael;

  begin
    Key := ChaveCript;
    if (Action = UpperCase('C')) then
    begin
      Cipher := TDCP_rijndael.Create(nil);
      Cipher.InitStr(Key, TDCP_sha512);
      Result := Cipher.EncryptString(Src);
      Cipher.Burn;
      Cipher.Free;
    end;
    if (Action = UpperCase('D')) then
    begin
      Cipher := TDCP_rijndael.Create(nil);
      Cipher.InitStr(Key, TDCP_sha512);
      Result := Cipher.DecryptString(Src);
      Cipher.Burn;
      Cipher.Free;
    end;
  end;

  procedure TProcessa.ProcessaQrcod;
  var
    L: TListItem;
  begin
    L := FCPrin.LV1.FindCaption(0, intToStr(Socket.Handle), false, True, false);
    if L <> nil then
    begin
      with Fila2.New do
      begin
        Comando := '#Cmd#' + '<#>' + '0' + '<#>' +
          intToStr((L.SubItems.Objects[3] as TFBarraRemota).opcao) + '<#>' +
          (L.SubItems.Objects[3] as TFBarraRemota).Url;
      end;
    end;

  end;

  procedure TProcessa.Processar;
  begin
    try
      if iniciou then
      begin
        if (Fila2.Count > 0) and ComandoOK then
        begin
          ComandoOK := false;
          Pausar := True;
          if Pos('clb:', Fila2.Items[0].Comando) > 0 then
            SocketAtivo.SendText(Ofuscador_Rmt('C',
              ansiString(Fila2.Items[0].Comando)))
          else
            SocketAtivo.SendText(Ofuscador_Rmt('C',
              ansiString(Fila2.Items[0].Comando)) + #13 + #10);
          Fila2.Delete(0);
          Pausar := false;

        end;
      end;
    finally

    end;
  end;

  function TProcessa.RandoChave(strLen: Integer): string;
  var
    str: string;
  begin
    Randomize;
    str := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789¨´°¢~Ã¢§¨ÃÈÐË¡jns|bi­£jKJz}Lr';
    Result := '';
    repeat
      Result := Result + str[Random(Length(str)) + 1];
      ChaveCript := Result;
    until (Length(Result) = strLen) end;

end.
