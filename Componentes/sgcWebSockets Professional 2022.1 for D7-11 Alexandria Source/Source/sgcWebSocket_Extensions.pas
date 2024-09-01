{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}


unit sgcWebSocket_Extensions;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
  {$IFNDEF NEXTGEN}Contnrs, {$ENDIF}
  sgcWebSocket_Types,
  sgcWebSocket_Extension_DeflateFrame,
  sgcWebSocket_Extension_PerMessage_Deflate;

type


  TsgcWSExtensions = class(TPersistent)
  private
    FExtensionNegotiated: Boolean;
  private
    FDeflateFrame: TsgcWSExtension_DeflateFrame;
  private
    FList: TStringList;
    FMode: TwsApplicationMode;
    FPerMessage_Deflate: TsgcWSExtension_PerMessage_Deflate;
    function GetCompression: Boolean;
    function GetList: TStringList;
    procedure SetMode(const Value: TwsApplicationMode);
  public
    procedure Assign(aSource: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure DecodeHeader(aByte: Byte);
    procedure EncodeHeader(var aByte: Byte);
  public
    procedure DecodeExtensions(const aExtensions: String);
  public
    procedure WriteHeader(aHeader: TStringList);
  public
    procedure ReadStream(var aStream: TStream);
    procedure WriteStream(var aStream: TStream);
  public
    property List: TStringList read GetList;
  public
    property ExtensionNegotiated: Boolean read FExtensionNegotiated;
  public
    property Compression: Boolean read GetCompression;
  public
    property Mode: TwsApplicationMode read FMode write SetMode;
  published
    property DeflateFrame: TsgcWSExtension_DeflateFrame read FDeflateFrame write
        FDeflateFrame;
    property PerMessage_Deflate: TsgcWSExtension_PerMessage_Deflate read
        FPerMessage_Deflate write FPerMessage_Deflate;
  end;


implementation

uses sgcWebSocket_Classes, sgcWebSocket_Helpers, sgcWebSocket_Const;


constructor TsgcWSExtensions.Create;
begin
  FDeflateFrame := TsgcWSExtension_DeflateFrame.Create;
  FPerMessage_Deflate := TsgcWSExtension_PerMessage_Deflate.Create;
end;

destructor TsgcWSExtensions.Destroy;
begin
  sgcFree(FList);
  sgcFree(FPerMessage_Deflate);
  sgcFree(FDeflateFrame);
  inherited;
end;

procedure TsgcWSExtensions.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSExtensions then
  begin
    Mode := TsgcWSExtensions(aSource).Mode;
    FDeflateFrame.Assign(TsgcWSExtensions(aSource).DeflateFrame);
    FPerMessage_Deflate.Assign(TsgcWSExtensions(aSource).PerMessage_Deflate);
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSExtensions.DecodeExtensions(const aExtensions: String);
var
  i: Integer;
  oList: TsgcDelimitedStringList;
begin
  oList := TsgcDelimitedStringList.Create;
  Try
    oList.DelimitedText := aExtensions;
    for i := 0 to oList.Count - 1 do
    begin
      if PerMessage_Deflate.Enabled then
      begin
        if not PerMessage_Deflate.Active then
          PerMessage_Deflate.Active := PerMessage_Deflate.DecodeExtension(oList[i]);
      end;
      if DeflateFrame.Enabled then
      begin
        if not PerMessage_Deflate.Active then
          DeflateFrame.Active := DeflateFrame.DecodeExtension(oList[i]);
      end;
    end;
  Finally
    sgcFree(oList);
  End;

  FExtensionNegotiated := Compression;
end;

procedure TsgcWSExtensions.DecodeHeader(aByte: Byte);
begin
  if PerMessage_Deflate.Active then
    PerMessage_Deflate.DecodeHeader(aByte);
  if DeflateFrame.Active then
    DeflateFrame.DecodeHeader(aByte);
end;

procedure TsgcWSExtensions.EncodeHeader(var aByte: Byte);
begin
  if PerMessage_Deflate.Active then
    PerMessage_Deflate.EncodeHeader(aByte);
  if DeflateFrame.Active then
    DeflateFrame.EncodeHeader(aByte);
end;

function TsgcWSExtensions.GetCompression: Boolean;
begin
  Result := DeflateFrame.Active or PerMessage_Deflate.Active;
end;

function TsgcWSExtensions.GetList: TStringList;
begin
  if not Assigned(FList) then
    FList := TStringList.Create;
  FList.Clear;
  if PerMessage_Deflate.Active then
    FList.Add(CS_PERMESSAGE_DEFLATE);
  if DeflateFrame.Active then
    FList.Add(CS_DEFLATE_FRAME);
  Result := FList;
end;

procedure TsgcWSExtensions.ReadStream(var aStream: TStream);
begin
  if not Assigned(astream) or (aStream.Size = 0) then
    exit;

  // ... deflate message
  if PerMessage_Deflate.Active then
  begin
    if PerMessage_Deflate.MessageCompressed then
      PerMessage_Deflate.InflateFrame(aStream);
  end
  // ... deflate frame
  else if DeflateFrame.Active then
  begin
    if DeflateFrame.MessageCompressed then
      DeflateFrame.InflateFrame(aStream)
  end;
end;

procedure TsgcWSExtensions.SetMode(const Value: TwsApplicationMode);
begin
  FMode := Value;

  PerMessage_Deflate.Mode := FMode;
  DeflateFrame.Mode := FMode;
end;

procedure TsgcWSExtensions.WriteHeader(aHeader: TStringList);
begin
  case Mode of
    appClient:
      begin
        if PerMessage_Deflate.Enabled then
          aHeader.Add(PerMessage_Deflate.GetHeaderExtension);
        if DeflateFrame.Enabled then
          aHeader.Add(DeflateFrame.GetHeaderExtension);
      end;
    else
      begin
        if PerMessage_Deflate.Active then
          aHeader.Add(PerMessage_Deflate.GetHeaderExtension);
        if DeflateFrame.Active then
          aHeader.Add(DeflateFrame.GetHeaderExtension);
      end;
  end;
end;

procedure TsgcWSExtensions.WriteStream(var aStream: TStream);
begin
  if not Assigned(aStream) or (aStream.Size = 0) then
    exit;

  // ... inflate message
  if PerMessage_Deflate.Active then
    PerMessage_Deflate.DeflateFrame(aStream)
  // ... inflate frame
  else if DeflateFrame.Active then
    DeflateFrame.DeflateFrame(aStream);
end;


end.
