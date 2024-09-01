{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_Files_Server;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  // indy
  {$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
  // sgcbase
  sgcBase_Classes,
  // sgcWebSocket
  sgcWebSocket_Types, sgcWebSocket_Classes, sgcWebSocket_Protocol_Files_Message,
  sgcWebSocket_Protocol_Base_Server, sgcWebSocket_HTTPResponse,
  sgcWebSocket_Protocol_sgc_Message, sgcWebSocket_Helpers,
  sgcWebSocket_Classes_SyncObjs;

type

  TsgcWSFilesServerQoS_Options = class(TsgcWSFilesQoS_Options)

  end;

  TsgcWSFilesServer_Options = class(TsgcWSFiles_Options)
  private
    FQoS: TsgcWSFilesServerQoS_Options;
    procedure SetQoS(const Value: TsgcWSFilesServerQoS_Options);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property QoS: TsgcWSFilesServerQoS_Options read FQoS write SetQoS;
  end;

  TsgcWSProtocol_Files_Server = class(TsgcWSProtocol_Subscription_Server_Base)
    { from TComponent }
  protected
    procedure Loaded; override;
    { from TComponent }

    { from TsgcWSComponent_Base }
  protected
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventBinary(const aConnection: TsgcWSConnection;
      Data: TMemoryStream); override;
    procedure DoEventFragmented(const aConnection: TsgcWSConnection;
      Data: TMemoryStream; OpCode: TOpCode; Continuation: Boolean); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSProtocol_Server_Base }
  protected
    procedure DoRawWriteData(aGuid, aMessage: string); overload; virtual;
    procedure DoRawWriteData(aGuid: String; aStream: TStream); overload; virtual;
  public
    function WriteData(aGuid, aMessage: string): Boolean; overload; override;
    { from TsgcWSProtocol_Server_Base }

    { from TsgcWSProtocol }
  protected
    procedure DoFinalize(aConnection: TsgcWSConnection); override;
    { from TsgcWSProtocol }

    { files }
  private
    FFiles: TsgcWSFilesServer_Options;
    procedure SetFiles(const Value: TsgcWSFilesServer_Options);
  public
    property Files: TsgcWSFilesServer_Options read FFiles write SetFiles;
    { files }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    { WSMessage }
  private
    FWSMessageId: string;
  protected
    function GetWSMessageByConnection(aConnection: TsgcWSConnection): TsgcWSMessage;
  protected
    function DoWriteMessageText(aGuid, aMessage: string): Boolean; overload;
    function DoWriteMessageText(aConnection: TsgcWSConnection; aMessage: string):
        Boolean; overload;
  protected
    { WSMessage }

    { WSMessageFile }
  private
    FWSMessageFileId: String;
  protected
    function GetWSMessageFileByConnection(aConnection: TsgcWSConnection):
        TsgcWSMessageFile;
    function GetWSMessageFileByConnectionId(aGuid: string): TsgcWSMessageFile;
  protected
    procedure DoWriteMessage(aGuid, aText: string); virtual;
    procedure DoProcessMessage(aConnection: TsgcWSConnection; Data: TMemoryStream);
        virtual;
  public
    { WSMessageFile }

    { QoS }
  private
    FQoSTimer: TsgcTimer;
    FQoSList: TsgcWSFilesQoSList;
    function GetQoSList: TsgcWSFilesQoSList;
  protected
    procedure DoStartQoS; virtual;
    procedure DoStopQoS; virtual;
  protected
    procedure DoQoSList; virtual;
  protected
    procedure DoProcessFileAcknowledgment(aMessageFile: TsgcWSMessageFile);
    procedure DoWriteAcknowledgment(aConnection: TsgcWSConnection); overload;
        virtual;
    procedure DoWriteAcknowledgment(aGuid: String); overload; virtual;
  protected
    procedure DoWriteFileRec(aConnection: TsgcWSConnection; aMessageFile:
        TsgcWSMessageFile); virtual;
  protected
    procedure DoWriteFileQoS(aGuid: String; aMessageFile: TsgcWSMessageFile;
        aDuplicate: Boolean = False); virtual;
  protected
    procedure DoQueueQoSLevel2(aGuid: String; aMessageFile: TsgcWSMessageFile);
        virtual;
  protected
    procedure DoDeleteQoSByFileId(aMessageFile: TsgcWSMessageFile);
  protected
    procedure OnQoSEvent(Sender: TObject); virtual;
    procedure OnQoSExceptionEvent(Sender: TObject; E: Exception); virtual;
  protected
    property QoSList: TsgcWSFilesQoSList read GetQoSList;
    { QoS }

    { FileStreams }
  private
    FFileReceivedStreams: TsgcWSFileStreams;
    FFileSentStreams: TsgcWSFileStreams;
    function GetFileReceivedStreams: TsgcWSFileStreams;
    function GetFileSentStreams: TsgcWSFileStreams;
  protected
    function DoGetFileReceivedStream(aId: string): TsgcWSFileStream; virtual;
    procedure DoDeleteFileReceivedStream(aId: String); virtual;
    procedure DoDeleteFileReceivedStreams(aGuid: String); virtual;
  protected
    function DoGetFileSentStream(aId: String): TsgcWSFileStream; virtual;
    procedure DoDeleteFileSentStream(aId: string); virtual;
    procedure DoDeleteFileSentStreams(aGuid: String); virtual;
  public
    property FileReceivedStreams: TsgcWSFileStreams read GetFileReceivedStreams
      write FFileReceivedStreams;
    property FileSentStreams: TsgcWSFileStreams read GetFileSentStreams
      write FFileSentStreams;
    { FileStreams }

    { file received }
  protected
    procedure DoWriteFileReceived(aConnection: TsgcWSConnection; aMessageFile:
        TsgcWSMessageFile); virtual;
    procedure DoWriteFileReceivedError(aConnection: TsgcWSConnection; aMessageFile:
        TsgcWSMessageFile; aError: string); virtual;
    procedure DoDeleteFileReceived(aFileName: String); virtual;
  protected
    procedure DoFileReceivedStream(aConnection: TsgcWSConnection; Data:
        TMemoryStream); virtual;
    { file received }

    { send file }
  protected
    procedure DoWriteFileStream(aGuid: String; aFileStream: TsgcFileStream; aSize:
        Integer); virtual;
    procedure DoWriteHeaderStream(aGuid, aId, aFileId: string; aFileStream:
        TsgcFileStream; aSize: Integer; aQoS: TwsQoS; aData: string; aChannel:
        string = ''); virtual;
    procedure DoWriteWSFileMessage(var Stream: TMemoryStream; aMessageFile:
        TsgcWSMessageFile; aFilePosition: Int64; aStreaming: TwsStreaming;
        aDuplicate: Boolean = False); virtual;
  protected
    procedure DoSendFile(aGuid, aFileName: string; aSize: Integer; aQoS: TwsQoS;
        aData, aFileId: string); virtual;
    procedure DoSendFileQoSLevel2(aGuid, aFileName: string; aSize: Integer; aData:
        string; aChannel: string = ''; aFileId: string = ''); virtual;
  protected
    procedure DoWriteFileSentError(aGuid: String; aMessageFile: TsgcWSMessageFile;
        aError: String); virtual;
  public
    procedure SendFile(aGuid, aFileName: string; aSize: Integer; aQoS: TwsQoS;
      aData: String; aFileId: String = ''); overload; virtual;
    procedure SendFile(aGuid, aFileName: string); overload; virtual;
    procedure SendFile(aGuid, aFileName, aData: string; aFileId: String = '');
      overload; virtual;
  protected
    procedure DoBroadCast(aMessage: TsgcWSMessage; aChannel: string = ''; Exclude:
        String = ''; Include: String = ''); overload; virtual;
    procedure DoBroadCast(aStream: TStream; aChannel: string = ''; Exclude: String
        = ''; Include: String = ''; aSize: Integer = 0; aStreaming: TwsStreaming =
        stmNone); overload; virtual;
  protected
    procedure DoBroadcastFile(aFileName: string; aSize: Integer; aData: string;
        aQoS: TwsQoS; aChannel: string = ''; Exclude: String = ''; Include: String
        = ''; aFileId: String = ''); overload; virtual;
    procedure DoBroadcastFileQoSLevel2(aFileName: string; aSize: Integer; aData:
        string = ''; aChannel: string = ''; Exclude: String = ''; Include: String =
        ''; aFileId: String = ''); overload; virtual;
  public
    procedure BroadcastFile(aFileName: string; aSize: Integer; aData: string;
      aChannel: string = ''; Exclude: String = ''; Include: String = '';
      aFileId: String = ''); overload;
    procedure BroadcastFile(aFileName: string; aSize: Integer; aData: string;
      aQoS: TwsQoS; aChannel: string = ''; Exclude: String = '';
      Include: String = ''; aFileId: String = ''); overload;
    procedure BroadcastFile(aFileName: string; aData: string = '';
      aChannel: string = ''; Exclude: String = ''; Include: String = '';
      aFileId: String = ''); overload;
    { send file }

    { events }
  private
    FOnFileReceivedFragment: TsgcWSFileFragmentEvent;
    FOnFileReceived: TsgcWSFileEvent;
    FOnFileReceivedError: TsgcWSFileErrorEvent;
    FOnFileReceivedAuthorization: TsgcWSFileAuthorizationEvent;
  private
    FOnFileSentAcknowledgment: TsgcWSFileEvent;
    FOnFileSentError: TsgcWSFileErrorEvent;
    FOnFileSent: TsgcWSFileEvent;
    FOnFileBeforeSent: TsgcWSFileBeforeEvent;
    FOnFileSentFragmentRequest: TsgcWSFileFragmentRequestEvent;
  protected
    procedure DoEventFileReceived(aConnection: TsgcWSConnection; aMessage:
        TsgcWSMessageFile); virtual;
    procedure DoEventFileReceivedFragment(aConnection: TsgcWSConnection; aMessage:
        TsgcWSMessageFile); virtual;
    procedure DoEventFileReceivedError(aConnection: TsgcWSConnection; aMessage:
        TsgcWSMessageFile; aError: String); virtual;
    procedure DoEventFileReceivedAuthorization(aConnection: TsgcWSConnection;
        aMessage: TsgcWSMessageFile; var aFileName: string); virtual;
  protected
    procedure DoEventFileSent(aConnection: TsgcWSConnection; aMessage:
        TsgcWSMessageFile); virtual;
    procedure DoEventFileSentAcknowledgment(aConnection: TsgcWSConnection;
        aMessage: TsgcWSMessageFile); virtual;
    procedure DoEventFileSentError(aConnection: TsgcWSConnection; aMessage:
        TsgcWSMessageFile; aError: String); virtual;
    procedure DoEventFileBeforeSent(aMessage: TsgcWSMessageFile); virtual;
    procedure DoEventFileSentFragmentRequest(aMessage: TsgcWSMessageFile); virtual;
  public
    property OnFileReceived: TsgcWSFileEvent read FOnFileReceived
      write FOnFileReceived;
    property OnFileReceivedError: TsgcWSFileErrorEvent read FOnFileReceivedError
      write FOnFileReceivedError;
    property OnFileReceivedFragment: TsgcWSFileFragmentEvent
      read FOnFileReceivedFragment write FOnFileReceivedFragment;
    property OnFileReceivedAuthorization: TsgcWSFileAuthorizationEvent
      read FOnFileReceivedAuthorization write FOnFileReceivedAuthorization;
  public
    property OnFileSentAcknowledgment: TsgcWSFileEvent
      read FOnFileSentAcknowledgment write FOnFileSentAcknowledgment;
    property OnFileSentError: TsgcWSFileErrorEvent read FOnFileSentError
      write FOnFileSentError;
    property OnFileSent: TsgcWSFileEvent read FOnFileSent write FOnFileSent;
    property OnFileBeforeSent: TsgcWSFileBeforeEvent read FOnFileBeforeSent
      write FOnFileBeforeSent;
    property OnFileSentFragmentRequest: TsgcWSFileFragmentRequestEvent
      read FOnFileSentFragmentRequest write FOnFileSentFragmentRequest;
    { events }
  end;

  TsgcWSProtocol_JS_Files = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

  TsgcWSProtocol_HTML_Files = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
{$IFDEF POSIX}Posix.Unistd, {$ENDIF}
  sgcWebSocket_Const, sgcWebSocket_Resources, sgcWebSocket_Server,
  sgcWebSocket_Server_Base, sgcWebSocket_Protocol_Base_Message, sgcBase_Helpers;

const
  CS_FILE_SENT_STREAMS = 0;
  CS_QOS_LIST = 1;
  CS_FILE_RECEIVED_STREAMS = 2;

type
  THackComponent_Server = class(TsgcWSComponent_Server);

constructor TsgcWSProtocol_Files_Server.Create(aOwner: TComponent);
begin
  inherited;
  FProtocol := CS_PROTOCOL_FILES;
  FWSMessageId := NewGuid;
  FWSMessageFileId := NewGuid;
  FFiles := TsgcWSFilesServer_Options.Create;
  MsgType := msgBinary;
end;

function TsgcWSProtocol_JS_Files.GetResponse: string;
begin
  Result := GetResourceString(CS_SGC_JS_FILES_ESEGECE_COM);
end;

class function TsgcWSProtocol_JS_Files.GetFileName: string;
begin
  Result := CS_JS_FILES_ESEGECE_COM;
end;

class function TsgcWSProtocol_HTML_Files.GetFileName: string;
begin
  Result := CS_HTML_FILES_ESEGECE_COM;
end;

function TsgcWSProtocol_HTML_Files.GetResponse: string;
begin
  Result := GetResourceString(CS_SGC_HTML_FILES_ESEGECE_COM);
end;

destructor TsgcWSProtocol_Files_Server.Destroy;
begin
  DoStopQoS;
  sgcFree(FQoSTimer);
  sgcFree(FQoSList);
  sgcFree(FFiles);
  sgcFree(FFileReceivedStreams);
  sgcFree(FFileSentStreams);
  inherited;
end;

procedure TsgcWSProtocol_Files_Server.BroadcastFile(aFileName: string;
  aSize: Integer; aData: string; aChannel: string = ''; Exclude: String = '';
  Include: String = ''; aFileId: String = '');
begin
  BroadcastFile(aFileName, aSize, aData, Files.QoS.Level, aChannel, Exclude,
    Include, aFileId);
end;

procedure TsgcWSProtocol_Files_Server.BroadcastFile(aFileName: string;
  aData: string = ''; aChannel: string = ''; Exclude: String = '';
  Include: String = ''; aFileId: String = '');
begin
  BroadcastFile(aFileName, Files.BufferSize, aData, aChannel, Exclude,
    Include, aFileId);
end;

procedure TsgcWSProtocol_Files_Server.BroadcastFile(aFileName: string;
  aSize: Integer; aData: string; aQoS: TwsQoS; aChannel: string = '';
  Exclude: String = ''; Include: String = ''; aFileId: String = '');
begin
  case aQoS of
    qosLevel0, qosLevel1:
      DoBroadcastFile(aFileName, aSize, aData, aQoS, aChannel, Exclude,
        Include, aFileId);
    qosLevel2:
      DoBroadcastFileQoSLevel2(aFileName, aSize, aData, aChannel, Exclude,
        Include, aFileId);
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoBroadCast(aStream: TStream; aChannel:
    string = ''; Exclude: String = ''; Include: String = ''; aSize: Integer =
    0; aStreaming: TwsStreaming = stmNone);
var
  vChannel: string;
begin
  if aChannel <> '' then
  begin
    vChannel := guid + '_' + aChannel;
    inherited Broadcast(aStream, vChannel, Exclude, Include, aSize, aStreaming)
  end
  else
    inherited Broadcast(aStream, '', Exclude, Include, aSize, aStreaming);
end;

procedure TsgcWSProtocol_Files_Server.DoBroadCast(aMessage: TsgcWSMessage;
    aChannel: string = ''; Exclude: String = ''; Include: String = '');
var
  vChannel: string;
begin
  if aChannel <> '' then
  begin
    vChannel := guid + '_' + aChannel;
    inherited Broadcast(aMessage.Write, vChannel, Exclude, Include)
  end
  else
    inherited Broadcast(aMessage.Write, Exclude, Include);
end;

procedure TsgcWSProtocol_Files_Server.DoBroadcastFile(aFileName: string; aSize:
    Integer; aData: string; aQoS: TwsQoS; aChannel: string = ''; Exclude:
    String = ''; Include: String = ''; aFileId: String = '');
var
  i: Int64;
  oFileStream: TsgcFileStream;
  oStream: TMemoryStream;
  oHeader: TMemoryStream;
  vChannel: String;
  oMessageFile: TsgcWSMessageFile;
begin
  oFileStream := TsgcFileStream.Create(aFileName, fmOpenRead or
    fmShareDenyWrite);
  Try
    // ... encode
    oMessageFile := TsgcWSMessageFile.Create(nil);
    Try
      oMessageFile.Method := CS_SGC_FILE;
      oMessageFile.Id := NewGuid;
      if aFileId <> '' then
        oMessageFile.FileId := aFileId
      else
        oMessageFile.FileId := NewGuid;
      oMessageFile.FileName := ExtractFileName(aFileName);
      oMessageFile.FileSize := oFileStream.Size;
      oMessageFile.Streaming := TwsStreaming_String[0];
      oMessageFile.QoS := TwsQoS_String[Ord(aQoS)];;
      oMessageFile.BufferSize := aSize;
      oMessageFile.Data := aData;
      oMessageFile.Channel := aChannel;

      // ... channel
      vChannel := aChannel;
      if vChannel <> '' then
        vChannel := guid + '_' + vChannel;

      // ... header
      oHeader := TMemoryStream.Create;
      Try
        sgcWSStreamWrite(oMessageFile.Write, oHeader);
        Broadcast(oHeader, vChannel, Exclude, Include, 0);
      Finally
        sgcFree(oHeader);
      End;
      // ... send file
      oStream := TMemoryStream.Create;
      Try
        i := 0;
        oFileStream.Position := 0;
        while i < oFileStream.Size do
        begin
          // ... broadcast
          if (oFileStream.Size - i < aSize) or (aSize = 0) then
          begin
            oStream.CopyFrom(oFileStream, oFileStream.Size - i);
            i := i + oStream.Size;
            DoWriteWSFileMessage(oStream, oMessageFile,
              oFileStream.Position, stmLast);
            Broadcast(oStream, vChannel, Exclude, Include, 0);
          end
          else
          begin
            oStream.CopyFrom(oFileStream, aSize);
            i := i + oStream.Size;
            DoWriteWSFileMessage(oStream, oMessageFile, oFileStream.Position,
              stmContinue);
            Broadcast(oStream, vChannel, Exclude, Include, 0);
          end;
          oStream.Clear;
        end;
      Finally
        sgcFree(oStream);
      End;
    Finally
      sgcFree(oFileStream);
    End;
  Finally
    sgcFree(oMessageFile);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoBroadcastFileQoSLevel2(aFileName:
    string; aSize: Integer; aData: string = ''; aChannel: string = ''; Exclude:
    String = ''; Include: String = ''; aFileId: String = '');
var
  i: Integer;
  oConnection: TsgcWSConnection;
  oConnections: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
begin
  oConnection := nil;

  oConnections := TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF}.Create;
  Try
    GetBroadcastConnections(aChannel, Exclude, Include, oConnections);
    for i := 0 to oConnections.Count - 1 do
    begin
      Try
        oConnection := TsgcWSConnection(oConnections[i]);
        if Assigned(oConnection) then
        begin
          if oConnection.Disconnected = False then
            DoSendFileQoSLevel2(oConnection.guid, aFileName, aSize, aData,
              aChannel, aFileId);
        end;
      Except
        On E: Exception do
          DoError(oConnection, E.Message);
      end;
    end;
  Finally
    sgcFree(oConnections);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoDeleteFileReceived(aFileName: String);
begin
  SysUtils.DeleteFile(aFileName);
end;

procedure TsgcWSProtocol_Files_Server.DoDeleteFileSentStream(aId: string);
begin
  DoEnterCS(CS_FILE_SENT_STREAMS);
  Try
    FileSentStreams.Delete(aId);
  Finally
    DoLeaveCS(CS_FILE_SENT_STREAMS);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoDeleteFileSentStreams(aGuid: String);
var
  i: Integer;
begin
  DoEnterCS(CS_FILE_SENT_STREAMS);
  Try
    For i := FileSentStreams.Count - 1 DownTo 0 do
    begin
      if TsgcWSFileStream(FileSentStreams[i]).guid = aGuid then
        FileSentStreams.Delete(i);
    end;
  Finally
    DoLeaveCS(CS_FILE_SENT_STREAMS);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoDeleteQoSByFileId(aMessageFile:
    TsgcWSMessageFile);
var
  i: Integer;
begin
  if (aMessageFile.QoS = CS_QOS_LEVEL1) or (aMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    DoEnterCS(CS_QOS_LIST);
    Try
      for i := QoSList.Count - 1 Downto 0 do
      begin
        if TsgcWSFilesQoSItem(QoSList.Items[i])
          .MessageFile.FileId = aMessageFile.FileId then
          QoSList.Delete(i);
      end;
    Finally
      DoLeaveCS(CS_QOS_LIST);
    End;
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoDeleteFileReceivedStream(aId: String);
begin
  DoEnterCS(CS_FILE_RECEIVED_STREAMS);
  Try
    FileReceivedStreams.Delete(aId);
  Finally
    DoLeaveCS(CS_FILE_RECEIVED_STREAMS);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoDeleteFileReceivedStreams(aGuid:
    String);
var
  i: Integer;
begin
  DoEnterCS(CS_FILE_RECEIVED_STREAMS);
  Try
    For i := FileReceivedStreams.Count - 1 DownTo 0 do
    begin
      if TsgcWSFileStream(FileReceivedStreams[i]).guid = aGuid then
        FileReceivedStreams.Delete(i);
    end;
  Finally
    DoLeaveCS(CS_FILE_RECEIVED_STREAMS);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoEventFileReceivedAuthorization(
    aConnection: TsgcWSConnection; aMessage: TsgcWSMessageFile; var aFileName:
    string);
var
  vAccept: Boolean;
begin
  vAccept := True;
  if aMessage.QoS = CS_QOS_LEVEL2 then
  begin
    if Assigned(FOnFileReceivedAuthorization) then
      FOnFileReceivedAuthorization(aConnection, aMessage,
        aFileName, vAccept);
    if not vAccept then
      raise Exception.Create(S_ERROR_FILE_REJECTED);
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoEventBinary(const aConnection
  : TsgcWSConnection; Data: TMemoryStream);
begin
  DoProcessMessage(aConnection, Data);
end;

procedure TsgcWSProtocol_Files_Server.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
var
  vGuid: string;
begin
  vGuid := aConnection.guid;
  // ... delete streams
  if Files.ClearReceivedStreamsOnDisconnect then
    DoDeleteFileReceivedStreams(vGuid);
  if Files.ClearSentStreamsOnDisconnect then
    DoDeleteFileSentStreams(vGuid);
  // ... notify disconnection only if not Assigned Broker
  if not Assigned(Broker) then
    inherited;
end;

procedure TsgcWSProtocol_Files_Server.DoEventFileBeforeSent(aMessage:
    TsgcWSMessageFile);
begin
  if Assigned(FOnFileBeforeSent) then
    FOnFileBeforeSent(aMessage);
end;

procedure TsgcWSProtocol_Files_Server.DoEventFileSentFragmentRequest(aMessage:
    TsgcWSMessageFile);
var
  vCancel: Boolean;
begin
  vCancel := False;
  if Assigned(FOnFileSentFragmentRequest) then
    FOnFileSentFragmentRequest(aMessage, vCancel);
  if vCancel then
    raise Exception.Create(S_ERROR_FILE_CANCELLED);
end;

procedure TsgcWSProtocol_Files_Server.DoEventFileReceivedFragment(aConnection:
    TsgcWSConnection; aMessage: TsgcWSMessageFile);
var
  vCancel: Boolean;
begin
  vCancel := False;
  if Assigned(FOnFileReceivedFragment) then
    FOnFileReceivedFragment(aConnection, aMessage, vCancel);
  if vCancel then
    raise Exception.Create(S_ERROR_FILE_CANCELLED);
end;

procedure TsgcWSProtocol_Files_Server.DoEventFileReceived(aConnection:
    TsgcWSConnection; aMessage: TsgcWSMessageFile);
begin
  if Assigned(FOnFileReceived) then
    FOnFileReceived(aConnection, aMessage);
end;

procedure TsgcWSProtocol_Files_Server.DoEventFileReceivedError(aConnection:
    TsgcWSConnection; aMessage: TsgcWSMessageFile; aError: String);
begin
  if Assigned(FOnFileReceivedError) then
    FOnFileReceivedError(aConnection, aMessage, aError);
end;

procedure TsgcWSProtocol_Files_Server.DoEventFileSent(aConnection:
    TsgcWSConnection; aMessage: TsgcWSMessageFile);
begin
  DoDeleteQoSByFileId(aMessage);
  DoDeleteFileSentStream(aMessage.FileId);

  if Assigned(FOnFileSent) then
    FOnFileSent(aConnection, aMessage);
end;

procedure TsgcWSProtocol_Files_Server.DoEventFileSentAcknowledgment(
    aConnection: TsgcWSConnection; aMessage: TsgcWSMessageFile);
begin
  if Assigned(FOnFileSentAcknowledgment) then
    FOnFileSentAcknowledgment(aConnection, aMessage);
end;

procedure TsgcWSProtocol_Files_Server.DoEventFileSentError(aConnection:
    TsgcWSConnection; aMessage: TsgcWSMessageFile; aError: String);
begin
  DoDeleteQoSByFileId(aMessage);
  DoDeleteFileSentStream(aMessage.FileId);

  if Assigned(FOnFileSentError) then
    FOnFileSentError(aConnection, aMessage, aError);
end;

procedure TsgcWSProtocol_Files_Server.DoEventFragmented(const aConnection
  : TsgcWSConnection; Data: TMemoryStream; OpCode: TOpCode;
  Continuation: Boolean);
begin
  if aConnection.FragmentedMessages = frgOnlyFragmented then
    DoProcessMessage(aConnection, Data);
end;

procedure TsgcWSProtocol_Files_Server.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
var
  vAccept: Boolean;
  vChannel: string;
  vText: string;
  oMessage: TsgcWSMessage;
begin
  if DoRawMessage(aConnection, Text) then
    exit;

  oMessage := GetWSMessageByConnection(aConnection);
  oMessage.Read(Text);

  vChannel := oMessage.Channel;
  vText := oMessage.Text;
  if oMessage.Method = CS_SGC_SUBSCRIBE then
  begin
    // ... OnBeforeSubscription
    vAccept := True;
    DoEventBeforeSubscription(aConnection, vChannel, vAccept);
    if vAccept then
    begin
      // ... subscribe
      aConnection.DoSubscribe(guid + '_' + vChannel);
      if aConnection.Subscribed(guid + '_' + vChannel) then
      begin
        aConnection.LastSubscription := vChannel;
        DoNotifySubscription(aConnection);
        oMessage.Result := CS_SGC_SUBSCRIBE;

        DoWriteMessageText(aConnection, oMessage.Write);
      end;
    end;
  end
  else if oMessage.Method = CS_SGC_UNSUBSCRIBE then
  begin
    aConnection.DoUnsubscribe(guid + '_' + vChannel);
    if not aConnection.Subscribed(guid + '_' + vChannel) then
    begin
      aConnection.LastUnSubscription := vChannel;
      DoNotifyUnSubscription(aConnection);
      oMessage.Result := CS_SGC_UNSUBSCRIBE;

      DoWriteMessageText(aConnection, oMessage.Write);
    end;
  end
  else if oMessage.Method = CS_SGC_MESSAGE then
    inherited DoEventMessage(aConnection, vText)
  else
    inherited;
end;

procedure TsgcWSProtocol_Files_Server.DoFileReceivedStream(aConnection:
    TsgcWSConnection; Data: TMemoryStream);
var
  oWSStream: TsgcWSFileStream;
  vFileName: string;
  vReceivedSize: Integer;
  oMessageFile: TsgcWSMessageFile;
  vFileId: String;
begin
  oMessageFile := GetWSMessageFileByConnection(aConnection);
  Try
    vFileId := oMessageFile.FileId;
    // ... Acknowledgment
    DoWriteAcknowledgment(aConnection);
    // ... search
    oWSStream := DoGetFileReceivedStream(vFileId);
    oWSStream.guid := aConnection.guid;
    oWSStream.MessageFile := oMessageFile;
    // ... filename
    if oWSStream.FileName <> '' then
      vFileName := oWSStream.FileName
    else
      vFileName := Files.SaveDirectory + oMessageFile.FileName;
    // ... save stream
    if not Assigned(oWSStream.Stream) then
    begin
      DoEventFileReceivedAuthorization(aConnection, oMessageFile, vFileName);
      oWSStream.FileName := vFileName;
      if FileExists(vFileName) then
      begin
        // ... file exists
        oWSStream.Stream := TsgcFileStream.Create(vFileName, fmOpenWrite or
          fmShareDenyWrite);
        oWSStream.Stream.Position := oWSStream.Stream.Size;
        oMessageFile.FilePosition := oWSStream.Stream.Position;
        // ... require more data if needed
        if oMessageFile.FilePosition < oMessageFile.FileSize then
          DoEventFileReceivedFragment(aConnection, oMessageFile)
        else
          oMessageFile.Streaming := CS_STM_LAST;
      end
      else
        oWSStream.Stream := TsgcFileStream.Create(vFileName, fmCreate);
    end
    else
    begin
      oWSStream.Stream.CopyFrom(Data, Data.Size);
      // ... event
      DoEventFileReceivedFragment(aConnection, oMessageFile);
    end;
    // ... save to file
    if oMessageFile.Streaming = CS_STM_LAST then
    begin
      vReceivedSize := oWSStream.Stream.Size;
      // ... delete
      DoDeleteFileReceivedStream(vFileId);
      // ... verification
      if oMessageFile.QoS <> CS_QOS_LEVEL0 then
      begin
        if oMessageFile.FileSize <> vReceivedSize then
          raise Exception.CreateFmt(S_ERROR_RECEIVING_FILE_INVALID_SIZE,
            [oMessageFile.FileSize, vReceivedSize]);
        // ... file sent
        DoWriteFileReceived(aConnection, oMessageFile);
      end;
      // ... event
      DoEventFileReceived(aConnection, oMessageFile);
    end
    else
      // ... FileRec
      DoWriteFileRec(aConnection, oMessageFile);
  Except
    On E: Exception do
    begin
      // ... delete
      DoDeleteFileReceivedStream(vFileId);
      // ... event
      DoEventFileReceivedError(aConnection, oMessageFile, E.Message);
      // ... file error
      DoWriteFileReceivedError(aConnection, oMessageFile, E.Message);
      // ... delete file
      DoDeleteFileReceived(vFileName);
    end;
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoFinalize(aConnection: TsgcWSConnection);
var
  vGuid: string;
begin
  inherited;
  vGuid := aConnection.guid;

  DoUnSubscriptions(aConnection);
  if Files.ClearReceivedStreamsOnDisconnect then
    DoDeleteFileReceivedStreams(vGuid);
  if Files.ClearSentStreamsOnDisconnect then
    DoDeleteFileSentStreams(vGuid);
end;

function TsgcWSProtocol_Files_Server.DoGetFileSentStream(aId: String):
    TsgcWSFileStream;
begin
  DoEnterCS(CS_FILE_SENT_STREAMS);
  Try
    Result := FileSentStreams.GetById(aId);
  Finally
    DoLeaveCS(CS_FILE_SENT_STREAMS);
  End;
end;

function TsgcWSProtocol_Files_Server.DoGetFileReceivedStream(aId: string):
    TsgcWSFileStream;
begin
  DoEnterCS(CS_FILE_RECEIVED_STREAMS);
  Try
    Result := FileReceivedStreams.GetById(aId);
  Finally
    DoLeaveCS(CS_FILE_RECEIVED_STREAMS);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoProcessFileAcknowledgment(aMessageFile:
    TsgcWSMessageFile);
var
  i: Integer;
begin
  if (aMessageFile.QoS = CS_QOS_LEVEL1) OR (aMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    DoEnterCS(CS_QOS_LIST);
    Try
      for i := QoSList.Count - 1 Downto 0 do
      begin
        if TsgcWSFilesQoSItem(QoSList.Items[i]).MessageFile.Id = aMessageFile.Id
        then
          QoSList.Delete(i);
      end;
    Finally
      DoLeaveCS(CS_QOS_LIST);
    End;
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoProcessMessage(aConnection:
    TsgcWSConnection; Data: TMemoryStream);
var
  vMessage: string;
  oMessageFile: TsgcWSMessageFile;
begin
  // ... decode
  sgcWSStreamRead(Data, vMessage);
  oMessageFile := GetWSMessageFileByConnection(aConnection);
  oMessageFile.Read(vMessage);

{$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoProcessMessage', '[Message]: ' + vMessage);
{$ENDIF}
  // ... process
  if oMessageFile.Method = CS_SGC_FILE then
    DoFileReceivedStream(aConnection, Data)
  else if oMessageFile.Method = CS_SGC_ACKNOWLEDGMENT then
  begin
    DoProcessFileAcknowledgment(oMessageFile);
    DoEventFileSentAcknowledgment(aConnection, oMessageFile);
  end
  else if oMessageFile.Method = CS_SGC_FILEREC then
    DoWriteFileQoS(aConnection.guid, oMessageFile)
  else if oMessageFile.Method = CS_SGC_FILE_RECEIVED then
    DoEventFileSent(aConnection, oMessageFile)
  else if oMessageFile.Method = CS_SGC_FILE_SENT_ERROR then
    DoEventFileSentError(aConnection, oMessageFile, oMessageFile.Text)
  else if oMessageFile.Method = CS_SGC_FILE_RECEIVED_ERROR then
  begin
    DoDeleteFileReceivedStream(oMessageFile.FileId);
    DoEventFileReceivedError(aConnection, oMessageFile, oMessageFile.Text);
    DoDeleteFileReceived(Files.SaveDirectory +
      ExtractFileName(oMessageFile.FileId));
  end
  else
    inherited;
end;

procedure TsgcWSProtocol_Files_Server.DoQoSList;
var
  i: Integer;
begin
  DoEnterCS(CS_QOS_LIST);
  Try
    for i := 0 to QoSList.Count - 1 do
    begin
      if Trunc((Now - TsgcWSFilesQoSItem(QoSList.Items[i]).Date) * 86400) >
        Files.QoS.Timeout then
      begin
        if TsgcWSFilesQoSItem(QoSList.Items[i]).MessageFile.Method = CS_SGC_FILEREC
        then
          DoWriteMessage(TsgcWSFilesQoSItem(QoSList.Items[i]).guid,
            TsgcWSFilesQoSItem(QoSList.Items[i]).MessageFile.Write)
        else
          DoWriteFileQoS(TsgcWSFilesQoSItem(QoSList.Items[i]).guid,
            TsgcWSFilesQoSItem(QoSList.Items[i]).MessageFile, True);
      end;
    end;
  Finally
    DoLeaveCS(CS_QOS_LIST);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoQueueQoSLevel2(aGuid: String;
    aMessageFile: TsgcWSMessageFile);
var
  oItem: TsgcWSFilesQoSItem;
begin
  if aMessageFile.QoS = CS_QOS_LEVEL2 then
  begin
    DoEnterCS(CS_QOS_LIST);
    Try
      oItem := TsgcWSFilesQoSItem.Create;
      oItem.guid := aGuid;
      oItem.MessageFile := aMessageFile;
      oItem.Date := Now;
      QoSList.Add(oItem)
    Finally
      DoLeaveCS(CS_QOS_LIST);
    End;
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoRawWriteData(aGuid, aMessage: string);
begin
  inherited WriteData(aGuid, aMessage);
end;

procedure TsgcWSProtocol_Files_Server.DoRawWriteData(aGuid: String; aStream:
    TStream);
begin
  inherited WriteData(aGuid, aStream);
end;

procedure TsgcWSProtocol_Files_Server.DoSendFile(aGuid, aFileName: string;
    aSize: Integer; aQoS: TwsQoS; aData, aFileId: string);
var
  oFileStream: TsgcFileStream;
  vFileId: String;
begin
  if aFileId <> '' then
    vFileId := aFileId
  else
    vFileId := NewGuid;

  oFileStream := TsgcFileStream.Create(aFileName, fmOpenRead or
    fmShareDenyWrite);
  Try
    // ... send header
    DoWriteHeaderStream(aGuid, NewGuid, vFileId, oFileStream, aSize,
      aQoS, aData);
    // ... send file
    DoWriteFileStream(aGuid, oFileStream, aSize);
  Finally
    sgcFree(oFileStream);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoSendFileQoSLevel2(aGuid, aFileName:
    string; aSize: Integer; aData: string; aChannel: string = ''; aFileId:
    string = '');
var
  oWSStream: TsgcWSFileStream;
  vId: String;
  vFileId: String;
begin
  vId := NewGuid;
  if aFileId <> '' then
    vFileId := aFileId
  else
    vFileId := NewGuid;

  // ... search
  oWSStream := DoGetFileSentStream(vFileId);
  oWSStream.guid := aGuid;
  // ... send
  if Assigned(oWSStream) then
  begin
    if not Assigned(oWSStream.Stream) then
    begin
      oWSStream.FileName := aFileName;
      oWSStream.Stream := TsgcFileStream.Create(aFileName,
        fmOpenRead or fmShareDenyWrite);
      DoWriteHeaderStream(aGuid, vId, vFileId, oWSStream.Stream, aSize,
        qosLevel2, aData, aChannel);
      oWSStream.MessageFile := GetWSMessageFileByConnectionId(aGuid);
    end
    else
      raise Exception.Create(S_ERROR_ALREADY_SENDING_FILE);
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoStartQoS;
begin
  if not IsDesigning and not IsLoading then
  begin
    if not Assigned(FQoSTimer) then
    begin
      FQoSTimer := TsgcTimer.Create;
      FQoSTimer.DebugName := 'QoS Timer';
      if Assigned(Server) then
        FQoSTimer.NotifyEvents := THackComponent_Server(Server).NotifyEvents;
      FQoSTimer.Interval := Files.QoS.Interval;
      FQoSTimer.OnTimer := OnQoSEvent;
      FQoSTimer.OnException := OnQoSExceptionEvent;
      FQoSTimer.Interval := Files.QoS.Interval * 1000;
    end;

    if not FQoSTimer.Enabled then
      FQoSTimer.Enabled := True;
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoStopQoS;
begin
  sgcThreadFree(FQoSTimer);
end;

procedure TsgcWSProtocol_Files_Server.DoWriteAcknowledgment(aConnection:
    TsgcWSConnection);
var
  oMessageFile: TsgcWSMessageFile;
begin
  oMessageFile := GetWSMessageFileByConnection(aConnection);
  if (oMessageFile.QoS = CS_QOS_LEVEL1) or (oMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    oMessageFile.Method := CS_SGC_ACKNOWLEDGMENT;
    DoWriteMessage(aConnection.Guid, oMessageFile.Write);
    oMessageFile.Method := '';
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoWriteAcknowledgment(aGuid: String);
begin

end;

procedure TsgcWSProtocol_Files_Server.DoWriteFileReceivedError(aConnection:
    TsgcWSConnection; aMessageFile: TsgcWSMessageFile; aError: string);
begin
  if (aMessageFile.QoS = CS_QOS_LEVEL1) OR (aMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    aMessageFile.Method := CS_SGC_FILE_SENT_ERROR;
    aMessageFile.Text := aError;
    DoWriteMessage(aConnection.Guid, aMessageFile.Write);
    aMessageFile.Method := '';
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoWriteFileQoS(aGuid: String;
    aMessageFile: TsgcWSMessageFile; aDuplicate: Boolean = False);
var
  oStream: TMemoryStream;
  oWSStream: TsgcWSFileStream;
  vFileId: String;
begin
  Try
    vFileId := aMessageFile.FileId;
    // ... Acknowledgment
    DoWriteAcknowledgment(aGuid);
    // ... search
    oWSStream := DoGetFileSentStream(vFileId);
    oWSStream.guid := aGuid;
    oWSStream.MessageFile := aMessageFile;
    // ... process stream
    if Assigned(oWSStream) and Assigned(oWSStream.Stream) then
    begin
      oStream := TMemoryStream.Create;
      Try
        oWSStream.Stream.Position := aMessageFile.FilePosition;
        if (oWSStream.Stream.Size - oWSStream.Stream.Position <=
          aMessageFile.BufferSize) or (aMessageFile.BufferSize = 0) then
        begin
          // ... copy
          oStream.CopyFrom(oWSStream.Stream, oWSStream.Stream.Size -
            oWSStream.Stream.Position);
          // ... message
          DoWriteWSFileMessage(oStream, aMessageFile, oWSStream.Stream.Position,
            stmLast, aDuplicate);
          // ... event
          DoEventFileSentFragmentRequest(aMessageFile);
          // ... write data
          DoRawWriteData(aGuid, oStream);
          // ... queue
          if not aDuplicate then
            DoQueueQoSLevel2(aGuid, aMessageFile);
          // ... delete
          DoDeleteFileSentStream(vFileId);
        end
        else
        begin
          // ... copy
          oStream.CopyFrom(oWSStream.Stream, aMessageFile.BufferSize);
          // ... message
          DoWriteWSFileMessage(oStream, aMessageFile, oWSStream.Stream.Position,
            stmContinue, aDuplicate);
          // ... event
          DoEventFileSentFragmentRequest(aMessageFile);
          // ... write data
          DoRawWriteData(aGuid, oStream);
          // ... queue
          if not aDuplicate then
            DoQueueQoSLevel2(aGuid, aMessageFile);
        end;
      Finally
        sgcFree(oStream);
      End;
    end;
  Except
    On E: Exception do
    begin
      // ... delete
      DoDeleteQoSByFileId(aMessageFile);
      DoDeleteFileSentStream(vFileId);
      // ... file error
      DoWriteFileSentError(aGuid, aMessageFile, E.Message);
    end;
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoWriteFileRec(aConnection:
    TsgcWSConnection; aMessageFile: TsgcWSMessageFile);
begin
  if aMessageFile.QoS = CS_QOS_LEVEL2 then
  begin
    aMessageFile.Method := CS_SGC_FILEREC;
    DoWriteMessage(aConnection.Guid, aMessageFile.Write);
    DoQueueQoSLevel2(aConnection.Guid, aMessageFile);
    aMessageFile.Method := '';
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoWriteFileReceived(aConnection:
    TsgcWSConnection; aMessageFile: TsgcWSMessageFile);
begin
  if (aMessageFile.QoS = CS_QOS_LEVEL1) OR (aMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    aMessageFile.Method := CS_SGC_FILE_RECEIVED;
    DoWriteMessage(aConnection.Guid, aMessageFile.Write);
    aMessageFile.Method := '';
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoWriteFileSentError(aGuid: String;
    aMessageFile: TsgcWSMessageFile; aError: String);
begin
  if (aMessageFile.QoS = CS_QOS_LEVEL1) OR (aMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    aMessageFile.Method := CS_SGC_FILE_RECEIVED_ERROR;
    aMessageFile.Text := aError;
    DoWriteMessage(Guid, aMessageFile.Write);
    aMessageFile.Method := '';
  end;
end;

procedure TsgcWSProtocol_Files_Server.DoWriteFileStream(aGuid: String;
    aFileStream: TsgcFileStream; aSize: Integer);
var
  i: Int64;
  oStream: TMemoryStream;
  oMessageFile: TsgcWSMessageFile;
begin
  oMessageFile := GetWSMessageFileByConnectionId(aGuid);

  oStream := TMemoryStream.Create;
  Try
    i := 0;
    aFileStream.Position := 0;
    while i < aFileStream.Size do
    begin
      if (aFileStream.Size - i <= aSize) or (aSize = 0) then
      begin
        oStream.CopyFrom(aFileStream, aFileStream.Size - i);
        i := i + oStream.Size;
        DoWriteWSFileMessage(oStream, oMessageFile,
          aFileStream.Position, stmLast);
        DoRawWriteData(aGuid, oStream);
      end
      else
      begin
        oStream.CopyFrom(aFileStream, aSize);
        i := i + oStream.Size;
        DoWriteWSFileMessage(oStream, oMessageFile, aFileStream.Position,
          stmContinue);
        DoRawWriteData(aGuid, oStream);
      end;
      oStream.Clear;
    end;
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoWriteHeaderStream(aGuid, aId, aFileId:
    string; aFileStream: TsgcFileStream; aSize: Integer; aQoS: TwsQoS; aData:
    string; aChannel: string = '');
var
  oHeader: TMemoryStream;
  oMessageFile: TsgcWSMessageFile;
begin
  oMessageFile := TsgcWSMessageFile.Create(nil);
  Try
    // ... encode
    oMessageFile.Clear;
    oMessageFile.Method := CS_SGC_FILE;
    oMessageFile.Id := aId;
    oMessageFile.FileId := aFileId;
    oMessageFile.Data := aData;
    oMessageFile.FileName := ExtractFileName(aFileStream.FileName);
    oMessageFile.FileSize := aFileStream.Size;
    oMessageFile.Streaming := TwsStreaming_String[Ord(stmFirst)];
    oMessageFile.QoS := TwsQoS_String[Ord(aQoS)];
    oMessageFile.BufferSize := aSize;
    oMessageFile.Channel := aChannel;
    // ... event
    DoEventFileBeforeSent(oMessageFile);
    // ... header
    oHeader := TMemoryStream.Create;
    Try
      sgcWSStreamWrite(oMessageFile.Write, oHeader);
      DoRawWriteData(aGuid, oHeader);
      DoQueueQoSLevel2(aGuid, oMessageFile);
    Finally
      sgcFree(oHeader);
    End;
  Finally
    sgcFree(oMessageFile);
  End;
end;

procedure TsgcWSProtocol_Files_Server.DoWriteMessage(aGuid, aText: string);
var
  oStream: TMemoryStream;
begin
  oStream := TMemoryStream.Create;
  Try
    sgcWSStreamWrite(aText, oStream);
    DoRawWriteData(aGuid, oStream);
  Finally
    sgcFree(oStream);
  End;
end;

function TsgcWSProtocol_Files_Server.DoWriteMessageText(aGuid, aMessage:
    string): Boolean;
begin
  Result := inherited WriteData(aGuid, aMessage);
end;

function TsgcWSProtocol_Files_Server.DoWriteMessageText(aConnection:
    TsgcWSConnection; aMessage: string): Boolean;
begin
  Result := inherited WriteData(aConnection, aMessage);
end;

procedure TsgcWSProtocol_Files_Server.DoWriteWSFileMessage(var Stream:
    TMemoryStream; aMessageFile: TsgcWSMessageFile; aFilePosition: Int64;
    aStreaming: TwsStreaming; aDuplicate: Boolean = False);
begin
  aMessageFile.Method := CS_SGC_FILE;
  aMessageFile.Streaming := TwsStreaming_String[Ord(aStreaming)];
  aMessageFile.FilePosition := aFilePosition;
  if not aDuplicate then
    aMessageFile.Id := NewGuid;
  sgcWSStreamWrite(aMessageFile.Write, Stream);
{$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'DoWriteWSFileMessage', '[Message]: ' + aMessageFile.Write);
{$ENDIF}
end;

function TsgcWSProtocol_Files_Server.GetFileReceivedStreams: TsgcWSFileStreams;
begin
  if not Assigned(FFileReceivedStreams) then
    FFileReceivedStreams := TsgcWSFileStreams.Create;
  Result := FFileReceivedStreams;
end;

function TsgcWSProtocol_Files_Server.GetFileSentStreams: TsgcWSFileStreams;
begin
  if not Assigned(FFileSentStreams) then
    FFileSentStreams := TsgcWSFileStreams.Create;
  Result := FFileSentStreams;
end;

function TsgcWSProtocol_Files_Server.GetQoSList: TsgcWSFilesQoSList;
begin
  if not Assigned(FQoSList) then
    FQoSList := TsgcWSFilesQoSList.Create;
  Result := FQoSList;
end;

function TsgcWSProtocol_Files_Server.GetWSMessageByConnection(aConnection:
    TsgcWSConnection): TsgcWSMessage;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSMessageId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSMessage.Create(nil);
      AddProtocolDataItem(aConnection, FWSMessageId, oItem);
    end;

    result := TsgcWSMessage(oItem.WSMessage);
  end;
end;

function TsgcWSProtocol_Files_Server.GetWSMessageFileByConnection(aConnection:
    TsgcWSConnection): TsgcWSMessageFile;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSMessageFileId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSMessageFile.Create(nil);
      AddProtocolDataItem(aConnection, FWSMessageFileId, oItem);
    end;

    result := TsgcWSMessageFile(oItem.WSMessage);
  end;
end;

function TsgcWSProtocol_Files_Server.GetWSMessageFileByConnectionId(aGuid:
    string): TsgcWSMessageFile;
var
  oConnection: TsgcWSConnection;
begin
  result := nil;
  if Server.InheritsFrom(TsgcWSServer_Base) then
    oConnection := TsgcWSServer_Base(Server).ConnectionsByGUID[aGuid]
  else
    oConnection := TsgcWSServer_Indy_Base(Server).ConnectionsByGUID[aGuid];

  if Assigned(oConnection) then
    result := GetWSMessageFileByConnection(oConnection);
end;

procedure TsgcWSProtocol_Files_Server.Loaded;
begin
  inherited;
  DoStartQoS;
end;

procedure TsgcWSProtocol_Files_Server.OnQoSEvent(Sender: TObject);
begin
  DoQoSList;
end;

procedure TsgcWSProtocol_Files_Server.OnQoSExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoNotifyException(E.Message, E);
end;

procedure TsgcWSProtocol_Files_Server.SendFile(aGuid, aFileName: string;
  aSize: Integer; aQoS: TwsQoS; aData: String; aFileId: String = '');
begin
  case aQoS of
    qosLevel0, qosLevel1:
      DoSendFile(aGuid, aFileName, aSize, aQoS, aData, aFileId);
    qosLevel2:
      DoSendFileQoSLevel2(aGuid, aFileName, aSize, aData, '', aFileId);
  end;
end;

procedure TsgcWSProtocol_Files_Server.SendFile(aGuid, aFileName: string);
begin
  SendFile(aGuid, aFileName, Files.BufferSize, Files.QoS.Level, '', '');
end;

procedure TsgcWSProtocol_Files_Server.SendFile(aGuid, aFileName, aData: string;
  aFileId: String = '');
begin
  SendFile(aGuid, aFileName, Files.BufferSize, Files.QoS.Level, aData, aFileId);
end;

procedure TsgcWSProtocol_Files_Server.SetFiles(const Value
  : TsgcWSFilesServer_Options);
begin
  FFiles.Assign(Value);
end;

function TsgcWSProtocol_Files_Server.WriteData(aGuid, aMessage: string):
    Boolean;
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  oMessage.DoEnterWrite;
  Try
    // ... pubrec
    oMessage.Method := CS_SGC_MESSAGE;
    oMessage.Text := aMessage;
    oMessage.guid := aGuid;
    oMessage.params := ''; // ... prevent params from other message
    Result := DoWriteMessageText(aGuid, oMessage.Write);
  Finally
    sgcFree(oMessage);
  End;
end;

constructor TsgcWSFilesServer_Options.Create;
begin
  inherited;
  FQoS := TsgcWSFilesServerQoS_Options.Create;
end;

destructor TsgcWSFilesServer_Options.Destroy;
begin
  sgcFree(FQoS);
  inherited;
end;

procedure TsgcWSFilesServer_Options.SetQoS(const Value
  : TsgcWSFilesServerQoS_Options);
begin
  FQoS.Assign(Value);
end;

initialization

Classes.RegisterClass(TsgcWSProtocol_Files_Server);
TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_JS_Files);
TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_HTML_Files);

finalization

  ;

{$ENDIF}

end.
