{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_Files_Message;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  {$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Types, sgcWebSocket_Helpers;

type

  TsgcWSMessageFile = class;

  TsgcWSFileEvent = procedure(Connection: TsgcWSConnection; const
      aMessage: TsgcWSMessageFile) of object;
  TsgcWSFileFragmentEvent = procedure(Connection: TsgcWSConnection; const
      aMessage: TsgcWSMessageFile; var Cancel: Boolean) of object;
  TsgcWSFileErrorEvent = procedure(Connection: TsgcWSConnection; const
      aMessage: TsgcWSMessageFile; const Error: String) of object;
  TsgcWSFileAuthorizationEvent = procedure(Connection: TsgcWSConnection; const
      aMessage: TsgcWSMessageFile; var aFileName: string; var Accept: Boolean) of
      object;
  TsgcWSFileBeforeEvent = procedure(const aMessage: TsgcWSMessageFile) of object;
  TsgcWSFileFragmentRequestEvent = procedure(const aMessage: TsgcWSMessageFile;
      var Cancel: Boolean) of object;


  TsgcWSMessageFile = class(TComponent)
  private
    FBufferSize: Integer;
    FChannel: String;
    FConnectionId: string;
    FMethod: String;
    FData: string;
    FFileName: String;
    FFilePosition: Int64;
    FFileSize: Int64;
    FFileId: String;
    FId: string;
    FQoS: string;
    FStreaming: string;
    FText: String;
    FThreadId: cardinal;
  public
    procedure Read(const aText: String);
    function Write: string;
  public
    procedure Clear;
  public
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property Channel: String read FChannel write FChannel;
    property Method: String read FMethod write FMethod;
    property FileId: String read FFileId write FFileId;
    property Data: string read FData write FData;
    property FileName: String read FFileName write FFileName;
    property FilePosition: Int64 read FFilePosition write FFilePosition;
    property FileSize: Int64 read FFileSize write FFileSize;
    property Id: string read FId write FId;
    property QoS: string read FQoS write FQoS;
    property Streaming: string read FStreaming write FStreaming;
    property Text: String read FText write FText;
  public
    property ConnectionId: string read FConnectionId write FConnectionId;
    property ThreadId: cardinal read FThreadId write FThreadId;
  end;


  TsgcWSFileStream = class
  private
    FFileName: string;
    FGuid: string;
    FId: String;
    FLast: TDateTime;
    FStream: TsgcFileStream;
    FMessageFile: TsgcWSMessageFile;
    function GetMessageFile: TsgcWSMessageFile;
    procedure SetMessageFile(const Value: TsgcWSMessageFile);
    procedure SetStream(const Value: TsgcFileStream);
  public
    destructor Destroy; override;
  public
    property FileName: string read FFileName write FFileName;
    property Guid: string read FGuid write FGuid;
    property Id: String read FId write FId;
    property Last: TDateTime read FLast write FLast;
    property MessageFile: TsgcWSMessageFile read GetMessageFile write
        SetMessageFile;
    property Stream: TsgcFileStream read FStream write SetStream;
  end;


  TsgcWSFileStreams = class({$IFDEF NEXTGEN}TList<TsgcWSFileStream>{$ELSE}TObjectList{$ENDIF})
  public
    function Add(const aId: string): TsgcWSFileStream; overload;
    procedure Delete(const aId: string); overload;
    function Item(const aId: string): TsgcWSFileStream;
    function GetById(const aId: String): TsgcWSFileStream;
  end;

  TsgcWSFiles_Options = class(TPersistent)
  private
    FBufferSize: Integer;
    FClearReceivedStreamsOnDisconnect: Boolean;
    FClearSentStreamsOnDisconnect: Boolean;
    FSaveDirectory: String;
  private
    function GetSaveDirectory: String;
  public
    constructor Create; virtual;
  published
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property ClearReceivedStreamsOnDisconnect: Boolean read
        FClearReceivedStreamsOnDisconnect write FClearReceivedStreamsOnDisconnect;
    property ClearSentStreamsOnDisconnect: Boolean read
        FClearSentStreamsOnDisconnect write FClearSentStreamsOnDisconnect;
    property SaveDirectory: String read GetSaveDirectory write FSaveDirectory;
  end;


  TsgcWSFilesQoS_Options = class(TPersistent)
  private
    FInterval: Integer;
    FLevel: TwsQoS;
    FTimeout: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Interval: Integer read FInterval write FInterval;
    property Level: TwsQoS read FLevel write FLevel;
    property Timeout: Integer read FTimeout write FTimeout;
  end;


  TsgcWSFilesQoSItem = class
  private
    FDate: TDateTime;
    FGuid: string;
    FMessageFile: TsgcWSMessageFile;
    function GetMessageFile: TsgcWSMessageFile;
    procedure SetMessageFile(const Value: TsgcWSMessageFile);
  public
    destructor Destroy; override;
  public
    property Date: TDateTime read FDate write FDate;
    property Guid: string read FGuid write FGuid;
    property MessageFile: TsgcWSMessageFile read GetMessageFile write
        SetMessageFile;
  end;

  TsgcWSFilesQoSList = class({$IFDEF NEXTGEN}TList<TsgcWSFilesQoSItem>{$ELSE}TObjectList{$ENDIF})

  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
 sgcWebSocket_Const;

const
  CS_DELIMITER = '|';

procedure TsgcWSMessageFile.Clear;
begin
  Id := '';
  FileId := '';
  Data := '';
  FileName := '';
  FileSize := 0;
  FilePosition := 0;
  BufferSize := 0;
  Streaming := TwsStreaming_String[0];
  QoS := TwsQoS_String[0];
  Method := '';
  Text := '';
  Channel := '';
end;

procedure TsgcWSMessageFile.Read(const aText: String);
var
  oList: TsgcDelimitedStringList;
begin
  oList := TsgcDelimitedStringList.Create;
  Try
    oList.Delimiter := CS_DELIMITER;
    oList.DelimitedText := aText;
    if oList.Count = 12 then
    begin
      Id := oList[0];
      FileId := oList[1];
      FileName := oList[2];
      FileSize := StrToInt64(oList[3]);
      FilePosition := StrToInt64(oList[4]);
      BufferSize := StrToInt(oList[5]);
      Data := oList[6];
      Streaming := oList[7];
      QoS := oList[8];
      Method := oList[9];
      Text := oList[10];
      Channel := oList[11]
    end
    else
      raise Exception.Create(S_ERROR_DECODING_MESSAGE);
  Finally
    sgcFree(oList);
  End;
end;

function TsgcWSMessageFile.Write: string;
begin
  Result :=
    Id + CS_DELIMITER +
    FileId + CS_DELIMITER +
    FileName + CS_DELIMITER +
    IntToStr(FileSize) + CS_DELIMITER +
    IntToStr(FilePosition) + CS_DELIMITER +
    IntToStr(BufferSize) + CS_DELIMITER +
    Data + CS_DELIMITER +
    Streaming + CS_DELIMITER +
    QoS + CS_DELIMITER +
    Method + CS_DELIMITER +
    Text + CS_DELIMITER +
    Channel;
end;

destructor TsgcWSFileStream.Destroy;
begin
  sgcFree(FMessageFile);
  sgcFree(FStream);
  inherited;
end;

function TsgcWSFileStream.GetMessageFile: TsgcWSMessageFile;
begin
  if not Assigned(FMessageFile) then
    FMessageFile := TsgcWSMessageFile.Create(nil);
  Result := FMessageFile;
end;

procedure TsgcWSFileStream.SetMessageFile(const Value: TsgcWSMessageFile);
begin
  MessageFile.Method := Value.Method;
  MessageFile.Data := Value.Data;
  MessageFile.FileName := Value.FileName;
  MessageFile.FilePosition := Value.FilePosition;
  MessageFile.FileSize := Value.FileSize;
  MessageFile.Id := Value.Id;
  MessageFile.QoS := Value.QoS;
  MessageFile.Streaming := Value.Streaming;
  MessageFile.Text := Value.Text;
  MessageFile.BufferSize := Value.BufferSize;
  MessageFile.Channel := Value.Channel;
end;

procedure TsgcWSFileStream.SetStream(const Value: TsgcFileStream);
begin
  FStream := Value;
end;

function TsgcWSFileStreams.Add(const aId: string): TsgcWSFileStream;
begin
  result := TsgcWSFileStream.Create;
  result.ID := aId;
  result.Stream := nil;
  Add(result);
end;

procedure TsgcWSFileStreams.Delete(const aId: string);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if TsgcWSFileStream(Items[i]).Id = aId then
    begin
      Delete(i);
      break;
    end;
  end;
end;

function TsgcWSFileStreams.GetById(const aId: String): TsgcWSFileStream;
begin
  Result := Item(aId);
  if not Assigned(result) then
    Result := Add(aId);
  result.Last := Now;
end;

function TsgcWSFileStreams.Item(const aId: string): TsgcWSFileStream;
var
  i: Integer;
begin
  result := nil;

  for i := 0 to Count - 1 do
  begin
    if TsgcWSFileStream(Items[i]).Id = aId then
    begin
      result := TsgcWSFileStream(Items[i]);
      break;
    end;
  end;
end;

constructor TsgcWSFiles_Options.Create;
begin
  SaveDirectory := '';
  BufferSize := 0;
  ClearReceivedStreamsOnDisconnect := True;
  ClearSentStreamsOnDisconnect := True;
end;

function TsgcWSFiles_Options.GetSaveDirectory: String;
begin
  Result := '';
  if FSaveDirectory <> '' then
    Result := IncludeTrailingPathDelimiter(FSaveDirectory);
end;

constructor TsgcWSFilesQoS_Options.Create;
begin
  Interval := 60;
  Timeout := 300;
  Level := qosLevel0;
end;

procedure TsgcWSFilesQoS_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSFilesQoS_Options then
  begin
    Interval := TsgcWSFilesQoS_Options(aSource).Interval;
    Timeout := TsgcWSFilesQoS_Options(aSource).Timeout;
    Level := TsgcWSFilesQoS_Options(aSource).Level;
  end
  else
    inherited Assign(aSource);
end;

destructor TsgcWSFilesQoSItem.Destroy;
begin
  sgcFree(FMessageFile);
  inherited;
end;

function TsgcWSFilesQoSItem.GetMessageFile: TsgcWSMessageFile;
begin
  if not Assigned(FMessageFile) then
    FMessageFile := TsgcWSMessageFile.Create(nil);
  Result := FMessageFile;
end;

procedure TsgcWSFilesQoSItem.SetMessageFile(const Value: TsgcWSMessageFile);
begin
  MessageFile.Method := Value.Method;
  MessageFile.Data := Value.Data;
  MessageFile.FileId := Value.FileId;
  MessageFile.FileName := Value.FileName;
  MessageFile.FilePosition := Value.FilePosition;
  MessageFile.FileSize := Value.FileSize;
  MessageFile.Id := Value.Id;
  MessageFile.BufferSize := Value.BufferSize;
  MessageFile.QoS := Value.QoS;
  MessageFile.Streaming := Value.Streaming;
  MessageFile.Text := Value.Text;
  MessageFile.Channel := Value.Channel;  
end;

{$ENDIF}

end.
