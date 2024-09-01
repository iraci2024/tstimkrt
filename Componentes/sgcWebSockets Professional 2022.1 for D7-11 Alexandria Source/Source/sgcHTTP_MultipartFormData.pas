{ ***************************************************************************
  sgcHTTP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_MultipartFormData;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_Classes, sgcBase_Helpers;

type
  TsgcHTTPMultipartFormDataBeforeSaveFileEvent = procedure(Sender: TObject; var
      aFileName, aFilePath: string) of object;
  TsgcHTTPMultipartFormDataAfterSaveFileEvent = procedure(Sender: TObject; const
      aFileName, aFilePath: string) of object;

  TsgcHTTPMultipartFormDataDecoder = class(TsgcHTTPComponent_Base)
  private
    FReadBlockSize: Integer;
    function SameBytes(const aBytes1, aBytes2: TBytes;
      var aPosition: Int64): Boolean;
  protected
    function GetFileName(const aText: String): string;
  public
    constructor Create(aOwner: TComponent); override;
  public
    procedure ExtractFiles(const aStream: TStream; const aBoundary: string; const
        aPath: String = '');
  public
    property ReadBlockSize: Integer read FReadBlockSize write FReadBlockSize;

    { events }
  private
    FOnBeforeSaveFile: TsgcHTTPMultipartFormDataBeforeSaveFileEvent;
    FOnAfterSaveFile: TsgcHTTPMultipartFormDataAfterSaveFileEvent;
  public
    property OnBeforeSaveFile: TsgcHTTPMultipartFormDataBeforeSaveFileEvent read
        FOnBeforeSaveFile write FOnBeforeSaveFile;
    property OnAfterSaveFile: TsgcHTTPMultipartFormDataAfterSaveFileEvent read
        FOnAfterSaveFile write FOnAfterSaveFile;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_HTTP}

uses
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  sgcWebSocket_Helpers;

constructor TsgcHTTPMultipartFormDataDecoder.Create(aOwner: TComponent);
begin
  inherited;
  ReadBlockSize := 100000;
end;

procedure TsgcHTTPMultipartFormDataDecoder.ExtractFiles(const aStream: TStream;
    const aBoundary: string; const aPath: String = '');
var
  oCRLF: TBytes;
  oBoundary: TBytes;
  oBytes: TBytes;
  oStream: TStringStream;
  oFileStream: TFileStream;
  vHeaderStart, vHeaderEnd: Int64;
  vCRLF: Boolean;
  vFileName, vFilePath: string;
  vPosition: Int64;
  vReadBlockSize: Int64;
begin
  vHeaderStart := 0;
  vHeaderEnd := 0;
  vCRLF := False;
  vFileName := '';
  SetLength(oCRLF, 2);
  oCRLF[0] := 13;
  oCRLF[1] := 10;

  oBoundary := sgcGetBytesFromUTF8String('--' + aBoundary);

  aStream.Position := 0;
  repeat
  begin
    vReadBlockSize := ReadBlockSize;
    if vReadBlockSize > aStream.Size - aStream.Position then
      vReadBlockSize := aStream.Size - aStream.Position;

    if vHeaderStart = 0 then
    begin
      if Length(oBytes) <> Length(oBoundary) then
        SetLength(oBytes, vReadBlockSize)
    end
    else
    begin
      if Length(oBytes) <> Length(oCRLF) then
        SetLength(oBytes, Length(oCRLF));
    end;
    aStream.Read(oBytes[0], Length(oBytes));
    // ... find boundaries
    if (vHeaderStart = 0) and SameBytes(oBytes, oBoundary, vPosition) then
    begin
      aStream.Position := aStream.Position - vReadBlockSize + vPosition +
        Length(oBoundary);
      vHeaderStart := aStream.Position;
      vCRLF := False;
      if vFileName <> '' then
      begin
        vFilePath := aPath;
        if Assigned(FOnBeforeSaveFile) then
          FOnBeforeSaveFile(self, vFileName, vFilePath);
        if vFilePath <> '' then
          vFilePath := IncludeTrailingPathDelimiter(vFilePath);

        oFileStream := TFileStream.Create(vFilePath + vFileName, fmCreate);
        Try
          aStream.Position := vHeaderEnd;
          oFileStream.CopyFrom(aStream, vHeaderStart - Length(oBoundary) -
            vHeaderEnd);
          vHeaderEnd := 0;
        Finally
          sgcFree(oFileStream);
        End;

        if Assigned(FOnAfterSaveFile) then
          FOnAfterSaveFile(self, vFileName, vFilePath);
        vFileName := '';

        if vReadBlockSize = ReadBlockSize then
          if SameBytes(oBytes, sgcGetBytesFromUTF8String('--' + aBoundary +
            '--'), vPosition) then
            exit;
      end;
    end
    else if (vFileName = '') and (vHeaderStart > 0) and (vHeaderEnd = 0) and
      (SameBytes(oBytes, oCRLF, vPosition) = True) and (vCRLF = False) then
      vCRLF := True
    else if (vFileName = '') and (vHeaderStart > 0) and (vHeaderEnd = 0) and
      (SameBytes(oBytes, oCRLF, vPosition) = True) and (vCRLF = True) then
    begin
      vHeaderEnd := aStream.Position;
      oStream := TStringStream.Create('');
      Try
        aStream.Position := vHeaderStart;
        oStream.CopyFrom(aStream, vHeaderEnd - vHeaderStart);
        vFileName := GetFileName(oStream.DataString);
      Finally
        sgcFree(oStream);
      End;
      aStream.Position := vHeaderEnd;
      vHeaderStart := 0;
      vCRLF := False;
    end
    else
    begin
      vCRLF := False;
      if vHeaderStart > 0 then
        aStream.Position := aStream.Position - Length(oBytes) + 1;
    end;
  end;
  until aStream.Position >= aStream.Size - 1;
end;

function TsgcHTTPMultipartFormDataDecoder.SameBytes(const aBytes1,
  aBytes2: TBytes; var aPosition: Int64): Boolean;
var
  vString1, vString2: {$IFDEF NEXTGEN}String{$ELSE}AnsiString{$ENDIF};
begin
  SetString(vString1,
    {$IFDEF NEXTGEN}PIdAnsiChar{$ELSE}PAnsiChar{$ENDIF}(@aBytes1[0]),
    Length(aBytes1));
  SetString(vString2,
    {$IFDEF NEXTGEN}PIdAnsiChar{$ELSE}PAnsiChar{$ENDIF}(@aBytes2[0]),
    Length(aBytes2));
  aPosition := Pos(vString2, vString1) - 1;
  result := aPosition > -1;
end;

function TsgcHTTPMultipartFormDataDecoder.GetFileName
  (const aText: String): string;
var
  oList, oList2, oList3: TStringList;
  i, j: Integer;
begin
  oList := TStringList.Create;
  Try
    oList.Text := aText;
    for i := 0 to oList.Count - 1 do
    begin
      if sgcContainsText(oList[i], 'filename=') then
      begin
        oList2 := TStringList.Create;
        Try
          oList2.Delimiter := ';';
          oList2.QuoteChar := '"';
          {$IFDEF D2006}
          oList2.StrictDelimiter := True;
          {$ENDIF}
          oList2.DelimitedText := oList[i];
          for j := 0 to oList2.Count - 1 do
          begin
            if sgcContainsText(oList2[j], 'filename') then
            begin
              oList3 := TStringList.Create;
              Try
                oList3.Text := Trim(oList2[j]);
                result := sgcStringReplace(oList3.Values['filename'], '"', '');
                exit;
              Finally
                sgcFree(oList3);
              End;
            end;
          end;
        Finally
          sgcFree(oList2);
        End;
      end;
    end;
  Finally
    sgcFree(oList);
  End;
end;

{$ENDIF}

end.
