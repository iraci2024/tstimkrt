{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_Files_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  // sgcbase
  sgcBase_Classes, sgcBase_Helpers,
  // sgcWebSocket
  sgcWebSocket_Types, sgcWebSocket_Protocol_Files_Message,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Client,
  sgcWebSocket_HTTPResponse, sgcWebSocket_Protocol_sgc_Message,
  sgcWebSocket_Helpers, sgcWebSocket_Classes_SyncObjs,
  sgcTCP_Classes;

type

  TsgcWSFilesClientQoS_Options = class(TsgcWSFilesQoS_Options)

  end;

  TsgcWSFilesClient_Options = class(TsgcWSFiles_Options)
  private
    FQoS: TsgcWSFilesClientQoS_Options;
    procedure SetQoS(const Value: TsgcWSFilesClientQoS_Options);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property QoS: TsgcWSFilesClientQoS_Options read FQoS write SetQoS;
  end;

  TsgcWSProtocol_Files_Client = class(TsgcWSProtocol_Subscription_Client_Base)

    { from TsgcWSComponent_Base }
  private
    FWSConnection: TsgcWSConnection;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventBinary(const aConnection: TsgcWSConnection;
      Data: TMemoryStream); override;
    procedure DoEventFragmented(const aConnection: TsgcWSConnection;
      Data: TMemoryStream; OpCode: TOpCode; Continuation: Boolean); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
    procedure DoFinalize(aConnection: TsgcWSConnection); override;
    procedure DoClear(aConnection: TsgcWSConnection); override;
    { from TsgcWSProtocol }

    { from TsgcWSProtocol_Client_Base }
  protected
    procedure DoWriteRawData(const aText: String); overload; virtual;
    procedure DoWriteRawData(aStream: TStream); overload; virtual;
  public
    procedure WriteData(const aText: String); override;
    { from TsgcWSProtocol_Client_Base }

    { WSMessage }
  private
    FWSMessageId: String;
  protected
    FWSMessage: TsgcWSMessage;
    function GetWSMessage: TsgcWSMessage;
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection):
        TsgcWSMessage;
  protected
    procedure DoWriteMessageText; virtual;
  public
    procedure Subscribe(const aChannel: String;
      const aGuid: String = ''); override;
    procedure UnSubscribe(const aChannel: String;
      const aGuid: String = ''); override;
  protected
    property WSMessage: TsgcWSMessage read GetWSMessage write FWSMessage;
    { WSMessage }

    { WSMessageFile }
  private
    FWSMessageFileId: String;
    FWSMessageFile: TsgcWSMessageFile;
    function GetWSMessageFile: TsgcWSMessageFile;
  protected
    function GetWSMessageFileByConnection(const aConnection: TsgcWSConnection):
        TsgcWSMessageFile;
  protected
    procedure DoWriteMessage(const aText: String); virtual;
    procedure DoProcessMessage(const aConnection: TsgcWSConnection;
      Data: TMemoryStream); virtual;
  public
    property WSMessageFile: TsgcWSMessageFile read GetWSMessageFile
      write FWSMessageFile;
    { WSMessageFile }

    { FileStreams }
  private
    FFileReceivedStreams: TsgcWSFileStreams;
    FFileSentStreams: TsgcWSFileStreams;
    function GetFileReceivedStreams: TsgcWSFileStreams;
    function GetFileSentStreams: TsgcWSFileStreams;
  protected
    function DoGetFileReceivedStream(const aId: string)
      : TsgcWSFileStream; virtual;
    procedure DoDeleteFileReceivedStream(const aId: String); virtual;
  protected
    function DoGetFileSentStream(const aId: String): TsgcWSFileStream; virtual;
    procedure DoDeleteFileSentStream(const aId: string); virtual;
  public
    property FileReceivedStreams: TsgcWSFileStreams read GetFileReceivedStreams
      write FFileReceivedStreams;
    property FileSentStreams: TsgcWSFileStreams read GetFileSentStreams
      write FFileSentStreams;
    { FileStreams }

    { files }
  private
    FFiles: TsgcWSFilesClient_Options;
    procedure SetFiles(const Value: TsgcWSFilesClient_Options);
  public
    property Files: TsgcWSFilesClient_Options read FFiles write SetFiles;
    { files }

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
    procedure DoProcessFileAcknowledgment(const aMessageFile
      : TsgcWSMessageFile);
    procedure DoWriteAcknowledgment; virtual;
  protected
    procedure DoWriteFileRec; virtual;
  protected
    procedure DoWriteFileQoS(const aMessageFile: TsgcWSMessageFile;
      const aDuplicate: Boolean = False); virtual;
  protected
    procedure DoQueueQoSLevel2(const aMessageFile: TsgcWSMessageFile); virtual;
  protected
    procedure DoDeleteQoSByFileId(const aMessageFile: TsgcWSMessageFile);
  protected
    procedure OnQoSEvent(Sender: TObject); virtual;
    procedure OnQoSExceptionEvent(Sender: TObject; E: Exception); virtual;
  protected
    property QoSList: TsgcWSFilesQoSList read GetQoSList;
    { QoS }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    { file received }
  protected
    procedure DoWriteFileReceived; virtual;
    procedure DoWriteFileReceivedError(const aError: string); virtual;
    procedure DoDeleteFileReceived(const aFileName: String); virtual;
  protected
    procedure DoFileReceivedStream(const aConnection: TsgcWSConnection;
      Data: TMemoryStream); virtual;
    { file received }

    { send file }
  protected
    procedure DoWriteHeaderStream(const aId, aFileId: String;
      const aFileStream: TsgcFileStream; const aSize: Integer;
      const aQoS: TwsQoS; const aData: String); virtual;
    procedure DoWriteFileStream(const aFileStream: TsgcFileStream;
      const aSize: Integer); virtual;
    procedure DoWriteWSFileMessage(var Stream: TMemoryStream;
      const aMessageFile: TsgcWSMessageFile; const aFilePosition: Int64;
      const aStreaming: TwsStreaming;
      const aDuplicate: Boolean = False); virtual;
  protected
    procedure DoWriteFileSentError(const aError: String); virtual;
  protected
    procedure DoSendFile(const aFileName: String; const aSize: Integer;
      const aQoS: TwsQoS; const aData, aFileId: String); virtual;
    procedure DoSendFileQoSLevel2(const aFileName: String; const aSize: Integer;
      const aData, aFileId: String); virtual;
  public
    procedure SendFile(aFileName: String; aSize: Integer; aQoS: TwsQoS;
      aData: String; aFileId: String = ''); overload;
    procedure SendFile(aFileName: String); overload;
    procedure SendFile(aFileName, aData: String; aFileId: String = '');
      overload;
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
    procedure DoEventFileReceived(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessageFile); virtual;
    procedure DoEventFileReceivedError(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessageFile; const aError: String); virtual;
    procedure DoEventFileReceivedFragment(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessageFile); virtual;
    procedure DoEventFileReceivedAuthorization(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessageFile; var aFileName: string); virtual;
  protected
    procedure DoEventFileSentAcknowledgment(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessageFile); virtual;
    procedure DoEventFileSentError(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessageFile; const aError: String); virtual;
    procedure DoEventFileSent(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessageFile); virtual;
    procedure DoEventFileBeforeSent(const aMessage: TsgcWSMessageFile); virtual;
    procedure DoEventFileSentFragmentRequest(const aMessage
      : TsgcWSMessageFile); virtual;
  public
    property OnFileReceivedError: TsgcWSFileErrorEvent read FOnFileReceivedError
      write FOnFileReceivedError;
    property OnFileReceived: TsgcWSFileEvent read FOnFileReceived
      write FOnFileReceived;
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

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
{$IFDEF POSIX}Posix.Unistd, {$ENDIF}
  sgcWebSocket_Const;

const
  CS_FILE_RECEIVED_STREAMS = 2;
  CS_QOS_LIST = 1;
  CS_FILE_SENT_STREAMS = 0;

type
  THackComponent_Client = class(TsgcWSComponent_Client);

constructor TsgcWSProtocol_Files_Client.Create(aOwner: TComponent);
begin
  inherited;
  FWSMessageId := NewGuid;
  FWSMessageFileId := NewGuid;
  FProtocol := CS_PROTOCOL_FILES;
  FFiles := TsgcWSFilesClient_Options.Create;
  MsgType := msgBinary;
end;

destructor TsgcWSProtocol_Files_Client.Destroy;
begin
  sgcFree(FQoSTimer);
  sgcFree(FQoSList);
  sgcFree(FFileReceivedStreams);
  sgcFree(FFileSentStreams);
  sgcFree(FFiles);
  inherited;
end;

procedure TsgcWSProtocol_Files_Client.DoClear(aConnection: TsgcWSConnection);
begin
  inherited;
end;

procedure TsgcWSProtocol_Files_Client.DoDeleteFileReceived
  (const aFileName: String);
begin
  SysUtils.DeleteFile(aFileName);
end;

procedure TsgcWSProtocol_Files_Client.DoDeleteFileSentStream(const aId: string);
begin
  DoEnterCS(CS_FILE_SENT_STREAMS);
  Try
    FileSentStreams.Delete(aId);
  Finally
    DoLeaveCS(CS_FILE_SENT_STREAMS);
  End;
end;

procedure TsgcWSProtocol_Files_Client.DoDeleteQoSByFileId(const aMessageFile
  : TsgcWSMessageFile);
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
    end;
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoDeleteFileReceivedStream
  (const aId: String);
begin
  DoEnterCS(CS_FILE_RECEIVED_STREAMS);
  Try
    FileReceivedStreams.Delete(aId);
  Finally
    DoLeaveCS(CS_FILE_RECEIVED_STREAMS);
  End;
end;

procedure TsgcWSProtocol_Files_Client.DoEventFileReceivedAuthorization
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessageFile;
  var aFileName: string);
var
  vAccept: Boolean;
begin
  vAccept := True;
  if WSMessageFile.QoS = CS_QOS_LEVEL2 then
  begin
    if Assigned(FOnFileReceivedAuthorization) then
      FOnFileReceivedAuthorization(aConnection, WSMessageFile,
        aFileName, vAccept);
    if not vAccept then
      raise Exception.Create(S_ERROR_FILE_REJECTED);
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoEventBinary(const aConnection
  : TsgcWSConnection; Data: TMemoryStream);
begin
  DoProcessMessage(aConnection, Data);
end;

procedure TsgcWSProtocol_Files_Client.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;

  DoStartQoS;
end;

procedure TsgcWSProtocol_Files_Client.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
begin
  DoStopQoS;

  // ... clear queues
  if Files.ClearReceivedStreamsOnDisconnect then
    sgcFree(FFileReceivedStreams);
  if Files.ClearSentStreamsOnDisconnect then
    sgcFree(FFileSentStreams);

  // ... notify disconnection only if not Assigned Broker
  if not Assigned(Broker) then
    inherited;

  FWSConnection := nil;
end;

procedure TsgcWSProtocol_Files_Client.DoEventFileBeforeSent(const aMessage
  : TsgcWSMessageFile);
begin
  if Assigned(FOnFileBeforeSent) then
    FOnFileBeforeSent(aMessage);
end;

procedure TsgcWSProtocol_Files_Client.DoEventFileSentFragmentRequest
  (const aMessage: TsgcWSMessageFile);
var
  vCancel: Boolean;
begin
  vCancel := False;
  if Assigned(FOnFileSentFragmentRequest) then
    FOnFileSentFragmentRequest(aMessage, vCancel);
  if vCancel then
    raise Exception.Create(S_ERROR_FILE_CANCELLED);
end;

procedure TsgcWSProtocol_Files_Client.DoEventFileReceived
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessageFile);
begin
  if Assigned(FOnFileReceived) then
    FOnFileReceived(aConnection, aMessage);
end;

procedure TsgcWSProtocol_Files_Client.DoEventFileReceivedError
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessageFile;
  const aError: String);
begin
  if Assigned(FOnFileReceivedError) then
    FOnFileReceivedError(aConnection, aMessage, aError);
end;

procedure TsgcWSProtocol_Files_Client.DoEventFileReceivedFragment
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessageFile);
var
  vCancel: Boolean;
begin
  vCancel := False;
  if Assigned(FOnFileReceivedFragment) then
    FOnFileReceivedFragment(aConnection, WSMessageFile, vCancel);
  if vCancel then
    raise Exception.Create(S_ERROR_FILE_CANCELLED);
end;

procedure TsgcWSProtocol_Files_Client.DoEventFileSentAcknowledgment
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessageFile);
begin
  if Assigned(FOnFileSentAcknowledgment) then
    FOnFileSentAcknowledgment(aConnection, aMessage);
end;

procedure TsgcWSProtocol_Files_Client.DoEventFileSentError
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessageFile;
  const aError: String);
begin
  DoDeleteQoSByFileId(aMessage);
  DoDeleteFileSentStream(aMessage.FileId);

  if Assigned(FOnFileSentError) then
    FOnFileSentError(aConnection, aMessage, aError);
end;

procedure TsgcWSProtocol_Files_Client.DoEventFileSent
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessageFile);
begin
  DoDeleteQoSByFileId(aMessage);
  DoDeleteFileSentStream(aMessage.FileId);

  if Assigned(FOnFileSent) then
    FOnFileSent(aConnection, aMessage);
end;

procedure TsgcWSProtocol_Files_Client.DoEventFragmented(const aConnection
  : TsgcWSConnection; Data: TMemoryStream; OpCode: TOpCode;
  Continuation: Boolean);
begin
  if aConnection.FragmentedMessages = frgOnlyFragmented then
    DoProcessMessage(aConnection, Data);
end;

procedure TsgcWSProtocol_Files_Client.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
var
  oMessage: TsgcWSMessage;
begin
  if DoRawMessage(aConnection, Text) then
    exit;

  oMessage := WSMessage;
  oMessage.Read(Text);

  if oMessage.Method = CS_SGC_SUBSCRIBE then
  begin
    aConnection.DoSubscribe(aConnection.Guid + '_' + oMessage.Channel);
    aConnection.LastSubscription := oMessage.Channel;
    DoNotifySubscription(aConnection);
  end
  else if oMessage.Method = CS_SGC_UNSUBSCRIBE then
  begin
    aConnection.DoUnSubscribe(aConnection.Guid + '_' + oMessage.Channel);
    aConnection.LastUnSubscription := oMessage.Channel;
    DoNotifyUnSubscription(aConnection)
  end
  else if oMessage.Method = CS_SGC_MESSAGE then
    inherited DoEventMessage(aConnection, oMessage.Text)
  else
    inherited;
end;

function TsgcWSProtocol_Files_Client.DoGetFileSentStream(const aId: String)
  : TsgcWSFileStream;
begin
  DoEnterCS(CS_FILE_SENT_STREAMS);
  Try
    result := FileSentStreams.GetById(aId);
  Finally
    DoLeaveCS(CS_FILE_SENT_STREAMS);
  End;
end;

function TsgcWSProtocol_Files_Client.DoGetFileReceivedStream(const aId: string)
  : TsgcWSFileStream;
begin
  DoEnterCS(CS_FILE_RECEIVED_STREAMS);
  Try
    result := FileReceivedStreams.GetById(aId);
  Finally
    DoLeaveCS(CS_FILE_RECEIVED_STREAMS);
  End;
end;

procedure TsgcWSProtocol_Files_Client.DoFileReceivedStream(const aConnection
  : TsgcWSConnection; Data: TMemoryStream);
var
  oWSStream: TsgcWSFileStream;
  vFileName: string;
  vReceivedSize: Integer;
  oMessageFile: TsgcWSMessageFile;
begin
  oMessageFile := WSMessageFile;
  Try
    // ... Acknowledgment
    DoWriteAcknowledgment;
    // ... search
    oWSStream := DoGetFileReceivedStream(oMessageFile.FileId);
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
    // ... event
    if oMessageFile.Streaming = CS_STM_LAST then
    begin
      vReceivedSize := oWSStream.Stream.Size;
      // ... delete
      DoDeleteFileReceivedStream(oMessageFile.FileId);
      // ... verification
      if oMessageFile.QoS <> CS_QOS_LEVEL0 then
      begin
        if oMessageFile.FileSize <> vReceivedSize then
          raise Exception.CreateFmt(S_ERROR_RECEIVING_FILE_INVALID_SIZE,
            [oMessageFile.FileSize, vReceivedSize]);
        // ... file sent
        DoWriteFileReceived;
      end;
      // ... event
      DoEventFileReceived(aConnection, oMessageFile);
    end
    else
      // ... FileRec
      DoWriteFileRec;
  Except
    On E: Exception do
    begin
      // ... delete
      DoDeleteFileReceivedStream(oMessageFile.FileId);
      // ... event
      DoEventFileReceivedError(aConnection, oMessageFile, E.Message);
      // ... file error
      DoWriteFileReceivedError(E.Message);
      // ... delete file
      DoDeleteFileReceived(vFileName);
    end;
  End;
end;

procedure TsgcWSProtocol_Files_Client.DoInitialize
  (aConnection: TsgcWSConnection);
begin
  FWSConnection := aConnection;
  // ... start QoS
  DoStartQoS;
end;

procedure TsgcWSProtocol_Files_Client.DoFinalize(aConnection: TsgcWSConnection);
begin
  inherited;
  DoUnSubscriptions(aConnection);
  DoStopQoS;
  if Files.ClearReceivedStreamsOnDisconnect then
    sgcFree(FFileReceivedStreams);
  if Files.ClearSentStreamsOnDisconnect then
    sgcFree(FFileSentStreams);
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_Files_Client.DoProcessFileAcknowledgment
  (const aMessageFile: TsgcWSMessageFile);
var
  i: Integer;
begin
  if (aMessageFile.QoS = CS_QOS_LEVEL1) OR (aMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    for i := QoSList.Count - 1 Downto 0 do
    begin
      if TsgcWSFilesQoSItem(QoSList.Items[i]).MessageFile.Id = aMessageFile.Id
      then
      begin
        DoEnterCS(CS_QOS_LIST);
        Try
          QoSList.Delete(i);
        Finally
          DoLeaveCS(CS_QOS_LIST);
        End;
      end;
    end;
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoProcessMessage(const aConnection
  : TsgcWSConnection; Data: TMemoryStream);
var
  vMessage: string;
  oMessageFile: TsgcWSMessageFile;
begin
  // ... decode
  sgcWSStreamRead(Data, vMessage);

  oMessageFile := WSMessageFile;
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
    DoWriteFileQoS(oMessageFile)
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

procedure TsgcWSProtocol_Files_Client.DoQoSList;
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
          DoWriteMessage(TsgcWSFilesQoSItem(QoSList.Items[i]).MessageFile.Write)
        else
          DoWriteFileQoS(TsgcWSFilesQoSItem(QoSList.Items[i])
            .MessageFile, True);
      end;
    end;
  Finally
    DoLeaveCS(CS_QOS_LIST);
  End;
end;

procedure TsgcWSProtocol_Files_Client.DoQueueQoSLevel2(const aMessageFile
  : TsgcWSMessageFile);
var
  oItem: TsgcWSFilesQoSItem;
begin
  if aMessageFile.QoS = CS_QOS_LEVEL2 then
  begin
    DoEnterCS(CS_QOS_LIST);
    Try
      oItem := TsgcWSFilesQoSItem.Create;
      oItem.MessageFile := aMessageFile;
      oItem.Date := Now;
      QoSList.Add(oItem)
    Finally
      DoLeaveCS(CS_QOS_LIST);
    End;
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoWriteFileQoS(const aMessageFile
  : TsgcWSMessageFile; const aDuplicate: Boolean = False);
var
  oStream: TMemoryStream;
  oWSStream: TsgcWSFileStream;
begin
  Try
    // ... Acknowledgment
    DoWriteAcknowledgment;
    // ... search
    oWSStream := DoGetFileSentStream(aMessageFile.FileId);
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
          DoWriteRawData(oStream);
          // ... queue
          if not aDuplicate then
            DoQueueQoSLevel2(aMessageFile);
          // ... delete
          DoDeleteFileSentStream(aMessageFile.FileId);
        end
        else
        begin
          oStream.CopyFrom(oWSStream.Stream, aMessageFile.BufferSize);
          DoWriteWSFileMessage(oStream, aMessageFile, oWSStream.Stream.Position,
            stmContinue, aDuplicate);
          // ... event
          DoEventFileSentFragmentRequest(aMessageFile);
          // ... write data
          DoWriteRawData(oStream);
          // ... queue
          if not aDuplicate then
            DoQueueQoSLevel2(aMessageFile);
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
      DoDeleteFileSentStream(aMessageFile.FileId);
      // ... file error
      DoWriteFileSentError(E.Message);
    end;
  End;
end;

procedure TsgcWSProtocol_Files_Client.DoSendFile(const aFileName: String;
  const aSize: Integer; const aQoS: TwsQoS; const aData, aFileId: String);
var
  oFileStream: TsgcFileStream;
  vId, vFileId: String;
begin
  vId := NewGuid;
  if aFileId <> '' then
    vFileId := aFileId
  else
    vFileId := NewGuid;

  oFileStream := TsgcFileStream.Create(aFileName, fmOpenRead or
    fmShareDenyWrite);
  Try
    // ... send header
    DoWriteHeaderStream(vId, vFileId, oFileStream, aSize, aQoS, aData);
    // ... send file
    DoWriteFileStream(oFileStream, aSize);
  Finally
    sgcFree(oFileStream);
  End;
end;

procedure TsgcWSProtocol_Files_Client.DoSendFileQoSLevel2(const aFileName
  : String; const aSize: Integer; const aData, aFileId: String);
var
  oWSStream: TsgcWSFileStream;
  vId, vFileId: String;
begin
  vId := NewGuid;
  if aFileId <> '' then
    vFileId := aFileId
  else
    vFileId := NewGuid;

  // ... search
  oWSStream := DoGetFileSentStream(vFileId);
  // ... send
  if Assigned(oWSStream) then
  begin
    if not Assigned(oWSStream.Stream) then
    begin
      if Assigned(FWSConnection) then
        oWSStream.Guid := FWSConnection.Guid;
      oWSStream.Stream := TsgcFileStream.Create(aFileName,
        fmOpenRead or fmShareDenyWrite);
      DoWriteHeaderStream(vId, vFileId, oWSStream.Stream, aSize,
        qosLevel2, aData);
      oWSStream.MessageFile := WSMessageFile;
    end
    else
      raise Exception.Create(S_ERROR_ALREADY_SENDING_FILE);
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoStartQoS;
begin
  if not Assigned(FQoSTimer) then
  begin
    FQoSTimer := TsgcTimer.Create;
    FQosTimer.DebugName := 'Qos Timer';
    if Assigned(Client) then
      FQoSTimer.NotifyEvents := THackComponent_Client(Client).NotifyEvents;
    FQoSTimer.Interval := Files.QoS.Interval;
    FQoSTimer.OnTimer := OnQoSEvent;
    FQoSTimer.OnException := OnQoSExceptionEvent;
    FQoSTimer.Interval := Files.QoS.Interval * 1000;
  end;

  if FQoSTimer.Interval > 0 then
  begin
    if not FQoSTimer.Enabled then
      FQoSTimer.Enabled := True;
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoStopQoS;
begin
  sgcThreadFree(FQoSTimer);
end;

procedure TsgcWSProtocol_Files_Client.DoWriteAcknowledgment;
var
  oMessageFile: TsgcWSMessageFile;
begin
  oMessageFile := WSMessageFile;

  if (oMessageFile.QoS = CS_QOS_LEVEL1) or (oMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    oMessageFile.Method := CS_SGC_ACKNOWLEDGMENT;
    DoWriteMessage(oMessageFile.Write);
    oMessageFile.Method := '';
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoWriteFileReceivedError
  (const aError: string);
var
  oMessageFile: TsgcWSMessageFile;
begin
  oMessageFile := WSMessageFile;

  if (oMessageFile.QoS = CS_QOS_LEVEL1) OR (oMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    oMessageFile.Id := NewGuid;
    oMessageFile.Method := CS_SGC_FILE_SENT_ERROR;
    oMessageFile.Text := aError;
    DoWriteMessage(oMessageFile.Write);
    oMessageFile.Method := '';
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoWriteFileReceived;
var
  oMessageFile: TsgcWSMessageFile;
begin
  oMessageFile := WSMessageFile;

  if (oMessageFile.QoS = CS_QOS_LEVEL1) OR (oMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    oMessageFile.Id := NewGuid;
    oMessageFile.Method := CS_SGC_FILE_RECEIVED;
    DoWriteMessage(oMessageFile.Write);
    oMessageFile.Method := '';
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoWriteFileRec;
var
  oMessageFile: TsgcWSMessageFile;
begin
  oMessageFile := WSMessageFile;

  if oMessageFile.QoS = CS_QOS_LEVEL2 then
  begin
    oMessageFile.Id := NewGuid;
    oMessageFile.Method := CS_SGC_FILEREC;
    DoWriteMessage(oMessageFile.Write);
    DoQueueQoSLevel2(oMessageFile);
    oMessageFile.Method := '';
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoWriteFileSentError
  (const aError: String);
var
  oMessageFile: TsgcWSMessageFile;
begin
  oMessageFile := WSMessageFile;

  if (oMessageFile.QoS = CS_QOS_LEVEL1) OR (oMessageFile.QoS = CS_QOS_LEVEL2)
  then
  begin
    oMessageFile.Id := NewGuid;
    oMessageFile.Method := CS_SGC_FILE_RECEIVED_ERROR;
    oMessageFile.Text := aError;
    DoWriteMessage(oMessageFile.Write);
    oMessageFile.Method := '';
  end;
end;

procedure TsgcWSProtocol_Files_Client.DoWriteFileStream(const aFileStream
  : TsgcFileStream; const aSize: Integer);
var
  i: Int64;
  oStream: TMemoryStream;
begin
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
        DoWriteWSFileMessage(oStream, WSMessageFile,
          aFileStream.Position, stmLast);
        DoWriteRawData(oStream);
      end
      else
      begin
        oStream.CopyFrom(aFileStream, aSize);
        i := i + oStream.Size;
        DoWriteWSFileMessage(oStream, WSMessageFile, aFileStream.Position,
          stmContinue);
        DoWriteRawData(oStream);
      end;
      oStream.Clear;
    end;
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcWSProtocol_Files_Client.DoWriteHeaderStream(const aId,
  aFileId: String; const aFileStream: TsgcFileStream; const aSize: Integer;
  const aQoS: TwsQoS; const aData: String);
var
  oHeader: TMemoryStream;
  oMessageFile: TsgcWSMessageFile;
begin
  oMessageFile := WSMessageFile;

  // ... encode
  oMessageFile.Clear;
  oMessageFile.Method := CS_SGC_FILE;
  oMessageFile.Id := aId;
  oMessageFile.FileId := aFileId;
  oMessageFile.Data := aData;
  oMessageFile.FileName := ExtractFileName(aFileStream.FileName);
  oMessageFile.FileSize := aFileStream.Size;
  oMessageFile.Streaming := TwsStreaming_String[Ord(stmNone)];
  oMessageFile.QoS := TwsQoS_String[Ord(aQoS)];
  oMessageFile.BufferSize := aSize;
  // ... event
  DoEventFileBeforeSent(oMessageFile);
  // ... header
  oHeader := TMemoryStream.Create;
  Try
    sgcWSStreamWrite(oMessageFile.Write, oHeader);
    DoWriteRawData(oHeader);
    DoQueueQoSLevel2(oMessageFile);
  Finally
    sgcFree(oHeader);
  End;
end;

procedure TsgcWSProtocol_Files_Client.DoWriteMessage(const aText: String);
var
  oStream: TMemoryStream;
begin
  oStream := TMemoryStream.Create;
  Try
    sgcWSStreamWrite(aText, oStream);
    DoWriteRawData(oStream);
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcWSProtocol_Files_Client.DoWriteMessageText;
begin
  inherited WriteData(WSMessage.Write);
end;

procedure TsgcWSProtocol_Files_Client.DoWriteRawData(const aText: String);
begin
  inherited WriteData(aText);
end;

procedure TsgcWSProtocol_Files_Client.DoWriteRawData(aStream: TStream);
begin
  inherited WriteData(aStream);
end;

procedure TsgcWSProtocol_Files_Client.DoWriteWSFileMessage
  (var Stream: TMemoryStream; const aMessageFile: TsgcWSMessageFile;
  const aFilePosition: Int64; const aStreaming: TwsStreaming;
  const aDuplicate: Boolean = False);
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

function TsgcWSProtocol_Files_Client.GetFileReceivedStreams: TsgcWSFileStreams;
begin
  if not Assigned(FFileReceivedStreams) then
    FFileReceivedStreams := TsgcWSFileStreams.Create;
  result := FFileReceivedStreams;
end;

function TsgcWSProtocol_Files_Client.GetFileSentStreams: TsgcWSFileStreams;
begin
  if not Assigned(FFileSentStreams) then
    FFileSentStreams := TsgcWSFileStreams.Create;
  result := FFileSentStreams;
end;

function TsgcWSProtocol_Files_Client.GetQoSList: TsgcWSFilesQoSList;
begin
  if not Assigned(FQoSList) then
    FQoSList := TsgcWSFilesQoSList.Create;
  result := FQoSList;
end;

function TsgcWSProtocol_Files_Client.GetWSMessage: TsgcWSMessage;
begin
  result := GetWSMessageByConnection(FWSConnection);

  if not Assigned(result) then
  begin
    if not Assigned(FWSMessage) then
      FWSMessage := TsgcWSMessage.Create(self);
    result := FWSMessage;
  end;
end;

function TsgcWSProtocol_Files_Client.GetWSMessageByConnection(const
    aConnection: TsgcWSConnection): TsgcWSMessage;
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

function TsgcWSProtocol_Files_Client.GetWSMessageFileByConnection(const
    aConnection: TsgcWSConnection): TsgcWSMessageFile;
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

function TsgcWSProtocol_Files_Client.GetWSMessageFile: TsgcWSMessageFile;
begin
  result := GetWSMessageFileByConnection(FWSConnection);

  if not Assigned(result) then
  begin
    if not Assigned(FWSMessageFile) then
      FWSMessageFile := TsgcWSMessageFile.Create(self);
    result := FWSMessageFile;
  end;
end;

procedure TsgcWSProtocol_Files_Client.OnQoSEvent(Sender: TObject);
begin
  DoQoSList;
end;

procedure TsgcWSProtocol_Files_Client.OnQoSExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoEventException(FWSConnection, E.Message, E);
end;

procedure TsgcWSProtocol_Files_Client.SendFile(aFileName: String;
  aSize: Integer; aQoS: TwsQoS; aData: String; aFileId: String = '');
begin
  case aQoS of
    qosLevel0, qosLevel1:
      DoSendFile(aFileName, aSize, aQoS, aData, aFileId);
    qosLevel2:
      DoSendFileQoSLevel2(aFileName, aSize, aData, aFileId);
  end;
end;

procedure TsgcWSProtocol_Files_Client.SendFile(aFileName: String);
begin
  SendFile(aFileName, Files.BufferSize, Files.QoS.Level, '', '');
end;

procedure TsgcWSProtocol_Files_Client.SendFile(aFileName, aData: String;
  aFileId: String = '');
begin
  SendFile(aFileName, Files.BufferSize, Files.QoS.Level, aData, aFileId);
end;

procedure TsgcWSProtocol_Files_Client.SetFiles(const Value
  : TsgcWSFilesClient_Options);
begin
  FFiles.Assign(Value);
end;

procedure TsgcWSProtocol_Files_Client.Subscribe(const aChannel: String;
  const aGuid: String = '');
var
  oMessage: TsgcWSMessage;
begin
  oMessage := WSMessage;

  oMessage.DoEnterWrite(True);

  oMessage.Method := CS_SGC_SUBSCRIBE;
  oMessage.Channel := aChannel;
  oMessage.Guid := aGuid;

  DoWriteMessageText;
end;

procedure TsgcWSProtocol_Files_Client.UnSubscribe(const aChannel: String;
  const aGuid: String = '');
var
  oMessage: TsgcWSMessage;
begin
  oMessage := WSMessage;

  oMessage.DoEnterWrite(True);

  oMessage.Method := CS_SGC_UNSUBSCRIBE;
  oMessage.Channel := aChannel;
  oMessage.Guid := aGuid;

  DoWriteMessageText;
end;

procedure TsgcWSProtocol_Files_Client.WriteData(const aText: String);
var
  oMessage: TsgcWSMessage;
begin
  oMessage := WSMessage;

  oMessage.DoEnterWrite(True);

  oMessage.Method := CS_SGC_MESSAGE;
  oMessage.Text := aText;
  oMessage.Channel := '';
  oMessage.Guid := '';

  DoWriteMessageText;
end;

constructor TsgcWSFilesClient_Options.Create;
begin
  inherited;
  FQoS := TsgcWSFilesClientQoS_Options.Create;
end;

destructor TsgcWSFilesClient_Options.Destroy;
begin
  sgcFree(FQoS);
  inherited;
end;

procedure TsgcWSFilesClient_Options.SetQoS(const Value
  : TsgcWSFilesClientQoS_Options);
begin
  FQoS.Assign(Value);
end;

{$ENDIF}

end.
