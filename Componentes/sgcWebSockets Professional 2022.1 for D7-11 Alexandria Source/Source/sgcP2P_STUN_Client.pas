{ ***************************************************************************
  sgcP2P component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcP2P_STUN_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_STUN}

uses
  Classes, SysUtils,
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  // sgc
  sgcP2P_STUN_Classes, sgcUDP_Client, sgcUDP_Classes, sgcSocket_Classes,
  sgcTCP_Client, sgcTCP_Classes, sgcP2P_STUN_Types, sgcWebSocket_Classes_Queues,
  sgcWebSocket_Types, sgcBase_Helpers;

type
  TsgcSTUNRequestItem = class;

  TsgcSTUNResponseSuccessEvent = procedure(Sender: TObject;
    const aSocket: TsgcSocketConnection; const aMessage: TsgcSTUN_Message;
    const aBinding: TsgcSTUN_ResponseBinding) of object;
  TsgcSTUNResponseErrorEvent = procedure(Sender: TObject;
    const aSocket: TsgcSocketConnection; const aMessage: TsgcSTUN_Message;
    const aError: TsgcSTUN_ResponseError) of object;
  TsgcSTUNBeforeSendEvent = procedure(Sender: TObject;
    const aMessage: TsgcSTUN_Message) of object;

  TsgcSTUNPRetransmissionClient_Options = class
    (TsgcUDPRetransmissionClient_Options)
  public
    constructor Create; override;
  end;

  TsgcSTUNRequestItem = class(TsgcQueueItemBase)
  private
    FMessageIntegrityKey: TBytes;
    FMethod: TsgcStunMessageMethod;
    FParameters: TStringList;
    function GetParameters: TStringList;
    procedure SetMessageIntegrityKey(const Value: TBytes);
  public
    destructor Destroy; override;
  public
    property MessageIntegrityKey: TBytes read FMessageIntegrityKey
      write SetMessageIntegrityKey;
    property Method: TsgcStunMessageMethod read FMethod write FMethod;
    property Parameters: TStringList read GetParameters write FParameters;
  end;

  TsgcSTUNRequestsQueue = class(TsgcQueueBase)
  public
    procedure AddRequest(const aMessage: TsgcSTUN_Message);
    function DeleteRequest(const aMessage: TsgcSTUN_Message): Boolean;
    function ExistsRequest(const aMessage: TsgcSTUN_Message): Boolean;
    function GetRequest(const aMessage: TsgcSTUN_Message): TsgcSTUNRequestItem;
  public
    function GetMessageIntegrityKey(aTransactionId: string): TBytes;
  end;

  TsgcSTUNClient_Authentication = class(TPersistent)
  private
    FCredentials: TsgcStunCredentials;
    FPassword: string;
    FUsername: string;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Credentials: TsgcStunCredentials read FCredentials
      write FCredentials;
    property Password: string read FPassword write FPassword;
    property Username: string read FUsername write FUsername;
  end;

  TsgcSTUNClient_Options = class(TsgcSTUN_Options)
  private
    FAuthentication: TsgcSTUNClient_Authentication;
    procedure SetAuthentication(const Value: TsgcSTUNClient_Authentication);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Authentication: TsgcSTUNClient_Authentication read FAuthentication
      write SetAuthentication;
  end;

  TsgcSTUNClient_Base = class(TsgcSTUN_Component)
    { client }
  private
    FUDPClient: TsgcUDPCLient_Base;
    function GetUDPClient: TsgcUDPCLient_Base;
  private
    FTCPClient: TsgcTCPCLient_Base;
    function GetTCPClient: TsgcTCPCLient_Base;
  protected
    procedure DoRetransmissionAdd(const aTransactionId: string;
      const aBytes: TBytes; const aIPAddress: string; aPort: Word = 0);
    function DoRetransmissionDelete(const aMessage: TsgcSTUN_Message): Boolean;
  protected
    procedure OnUDPReadEvent(Sender: TObject; Socket: TsgcUDPSocket;
      Bytes: TsgcBytes); virtual;
    procedure OnUDPExceptionEvent(Sender: TObject; Socket: TsgcUDPSocket;
      E: Exception); virtual;
    procedure OnRetransmissionTimeoutEvent(Sender: TObject; const aBytes: TBytes;
        const aIPAddress: string; aPort: Word); virtual;
  protected
    procedure OnTCPReadEvent(Connection: TsgcTCPConnection;
      const aStream: TMemoryStream); virtual;
    procedure OnTCPExceptionEvent(Connection: TsgcTCPConnection;
      E: Exception); virtual;
  protected
    procedure DoTCPDisconnect; virtual;
  protected
    property UDPClient: TsgcUDPCLient_Base read GetUDPClient write FUDPClient;
    property TCPClient: TsgcTCPCLient_Base read GetTCPClient write FTCPClient;
    { client }

    { read }
  private
    function GetErrorFromMessage(const aMessage: TsgcSTUN_Message)
      : TsgcSTUN_ResponseError;
    function GetBindingFromMessage(const aSocket: TsgcSocketConnection;
      const aMessage: TsgcSTUN_Message): TsgcSTUN_ResponseBinding;
  protected
    procedure OnRequestMessageIntegrityKeyEvent(Sender: TObject;
      const aTransactionId: String; var MessageIntegrityKey: TBytes); virtual;
    procedure OnICEValidateMessageIntegrityEvent(Sender: TObject;
      const aUsername: string; var Password: string); virtual;
    procedure OnICERequestMessageIntegrityPasswordEvent(Sender: TObject;
      const aMessage: TsgcSTUN_Message; var Password: string); virtual;
  protected
    procedure DoSTUNResponseSuccess(const aSocket: TsgcSocketConnection;
      const aMessage: TsgcSTUN_Message); virtual;
    procedure DoSTUNIndication(const aSocket: TsgcSocketConnection;
      const aMessage: TsgcSTUN_Message); virtual; abstract;
    procedure DoSTUNRequest(const aSocket: TsgcSocketConnection;
      const aMessage: TsgcSTUN_Message); virtual;
  protected
    function GetRealmNonce(const aMessage: TsgcSTUN_Message;
      var Realm, Nonce: string): Boolean;
  protected
    function DoSTUNReceivedRealm(const aMessage: TsgcSTUN_Message)
      : Boolean; virtual;
    function DoSTUNTryAlternate(const aMessage: TsgcSTUN_Message)
      : Boolean; virtual;
  protected
    procedure DoSTUNRead(const aSocket: TsgcSocketConnection;
      const aBytes: TBytes); virtual;
    { read }

    { requests }
  protected
    FRequests: TsgcSTUNRequestsQueue;
    function GetRequests: TsgcSTUNRequestsQueue; virtual;
  protected
    property Requests: TsgcSTUNRequestsQueue read GetRequests write FRequests;
    { requests }

    { constructor/destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor/destructor }

    { methods }
  protected
    FRealm: string;
    FNonce: string;
  protected
    procedure DoWriteBytes(const aBytes: TBytes); virtual;
  protected
    procedure DoSendMessage(const aMessage: TsgcSTUN_Message;
      const aIPAddress: string = ''; aPort: Word = 0); virtual;
    procedure DoSendResponse(const aMessage: TsgcSTUN_Message;
      const aIPAddress: string = ''; aPort: Word = 0); virtual;
    procedure DoPrepareRequest(aMethod: TsgcStunMessageMethod;
      const aMessage: TsgcSTUN_Message;
      const aAttributes: TsgcSTUN_Attributes = nil); virtual;
    procedure DoSetLongTermCredentials(const aMessage: TsgcSTUN_Message;
      const aRealm, aNonce: string); virtual;
  protected
    procedure DoSendRequest_LongTermCredential(aMethod: TsgcStunMessageMethod;
      const aRealm, aNonce: string;
      const aAttributes: TsgcSTUN_Attributes = nil); virtual;
    procedure DoSendRequest(aMethod: TsgcStunMessageMethod;
      const aAttributes: TsgcSTUN_Attributes = nil); virtual;
  public
    procedure SendRequest;
  public
    procedure WriteData(const aBytes: TBytes);
    { methods }

    { retransmission }
  private
    FRetransmissionOptions: TsgcSTUNPRetransmissionClient_Options;
    procedure SetRetransmissionOptions(const Value
      : TsgcSTUNPRetransmissionClient_Options);
  public
    property RetransmissionOptions: TsgcSTUNPRetransmissionClient_Options
      read FRetransmissionOptions write SetRetransmissionOptions;
    { retransmission }

    { properties }
  private
    FLogFile: TsgcSTUNLogFile;
    FTransport: TsgcStunTransport;
    FSTUNOptions: TsgcSTUNClient_Options;
    function GetHost: string;
    function GetIPVersion: TIdIPVersion;
    function GetPort: Integer;
    function GetNotifyEvents: TwsNotifyEvent;
    procedure SetLogFile(const Value: TsgcSTUNLogFile); virtual;
    procedure SetHost(const Value: string);
    procedure SetIPVersion(const Value: TIdIPVersion);
    procedure SetNotifyEvents(const Value: TwsNotifyEvent);
    procedure SetPort(const Value: Integer);
    procedure SetSTUNOptions(const Value: TsgcSTUNClient_Options);
  public
    property Host: string read GetHost write SetHost;
    property IPVersion: TIdIPVersion read GetIPVersion write SetIPVersion;
    property LogFile: TsgcSTUNLogFile read FLogFile write SetLogFile;
    property NotifyEvents: TwsNotifyEvent read GetNotifyEvents
      write SetNotifyEvents;
    property Port: Integer read GetPort write SetPort;
    property STUNOptions: TsgcSTUNClient_Options read FSTUNOptions
      write SetSTUNOptions;
    property Transport: TsgcStunTransport read FTransport write FTransport;
    { properties }

    { events }
  private
    FOnSTUNBeforeSend: TsgcSTUNBeforeSendEvent;
    FOnSTUNException: TsgcSTUNExceptionEvent;
    FOnSTUNResponseError: TsgcSTUNResponseErrorEvent;
    FOnSTUNResponseSuccess: TsgcSTUNResponseSuccessEvent;
    FOnICEValidateMessageIntegrity: TsgcICEValidateMessageIntegrityEvent;
    FOnICERequestBinding: TsgcICERequestBindingEvent;
    FOnICERequestMessageIntegrityPassword
      : TsgcICERequestMessageIntegrityPasswordEvent;
  private
    FOnRetransmissionTimeout: TsgcUDPRetransmissionTimeoutEvent;
  protected
    procedure DoSTUNExceptionEvent(E: Exception); virtual;
  protected
    property OnICEValidateMessageIntegrity: TsgcICEValidateMessageIntegrityEvent
      read FOnICEValidateMessageIntegrity write FOnICEValidateMessageIntegrity;
    property OnICERequestMessageIntegrityPassword
      : TsgcICERequestMessageIntegrityPasswordEvent
      read FOnICERequestMessageIntegrityPassword
      write FOnICERequestMessageIntegrityPassword;
    property OnICERequestBinding: TsgcICERequestBindingEvent
      read FOnICERequestBinding write FOnICERequestBinding;
  protected
    property OnRetransmissionTimeout: TsgcUDPRetransmissionTimeoutEvent
      read FOnRetransmissionTimeout write FOnRetransmissionTimeout;
  public
    property OnSTUNException: TsgcSTUNExceptionEvent read FOnSTUNException
      write FOnSTUNException;
    property OnSTUNResponseError: TsgcSTUNResponseErrorEvent
      read FOnSTUNResponseError write FOnSTUNResponseError;
    property OnSTUNResponseSuccess: TsgcSTUNResponseSuccessEvent
      read FOnSTUNResponseSuccess write FOnSTUNResponseSuccess;
    property OnSTUNBeforeSend: TsgcSTUNBeforeSendEvent read FOnSTUNBeforeSend
      write FOnSTUNBeforeSend;
    { events }
  end;

const
  CS_STUN_PORT = 3478;

{$ENDIF}

implementation

{$IFDEF SGC_STUN}

uses
  // sgc
  sgcP2P_STUN_Helpers, sgcBase_Const;

type
  TsgcUDPClient_Base_Hack = class(TsgcUDPCLient_Base);
  TsgcTCPClient_Base_Hack = class(TsgcTCPCLient_Base);

constructor TsgcSTUNClient_Base.Create(aOwner: TComponent);
begin
  inherited;
  FRetransmissionOptions := TsgcSTUNPRetransmissionClient_Options.Create;
  FSTUNOptions := TsgcSTUNClient_Options.Create;
  FLogFile := TsgcSTUNLogFile.Create;
  Port := CS_STUN_PORT;
end;

destructor TsgcSTUNClient_Base.Destroy;
begin
  sgcFree(FLogFile);
  sgcFree(FSTUNOptions);
  sgcFree(FRequests);
  sgcFree(FRetransmissionOptions);
  sgcFree(FUDPClient);
  inherited;
end;

procedure TsgcSTUNClient_Base.DoPrepareRequest(aMethod: TsgcStunMessageMethod;
  const aMessage: TsgcSTUN_Message;
  const aAttributes: TsgcSTUN_Attributes = nil);
var
  i: Integer;
  oAttribute: TsgcSTUN_Attribute_SOFTWARE;
begin
  aMessage.Header.MessageType_Class := stmcRequest;
  aMessage.Header.MessageType_Method := aMethod;
  aMessage.Header.MagicCookie := CS_STUN_MAGIC_COOKIE;
  aMessage.Header.MessageLength := 0;
  aMessage.Header.TransactionId := sgcGetTransactionId;
  if STUNOptions.Fingerprint then
    aMessage.Fingerprint := stfsFingerprintValid;
  if STUNOptions.Authentication.Credentials = stauShortTermCredential then
    aMessage.Credentials := stauShortTermCredential
  else
    aMessage.Credentials := stauNone;
  // ... additional attributes
  if Assigned(aAttributes) then
  begin
    for i := Low(aAttributes) to High(aAttributes) do
      aMessage.Attributes.Add(aAttributes[i]);
  end;
  // ... software
  if STUNOptions.Software then
  begin
    oAttribute := TsgcSTUN_Attribute_SOFTWARE.Create();
    oAttribute.Software := CS_APPLICATION_NAME + ' ' + CS_VERSION;
    aMessage.Attributes.Add(oAttribute);
  end;
end;

procedure TsgcSTUNClient_Base.DoSTUNResponseSuccess(const aSocket
  : TsgcSocketConnection; const aMessage: TsgcSTUN_Message);
var
  oBinding: TsgcSTUN_ResponseBinding;
begin
  if Assigned(FOnSTUNResponseSuccess) then
  begin
    oBinding := GetBindingFromMessage(aSocket, aMessage);
    Try
      FOnSTUNResponseSuccess(self, aSocket, aMessage, oBinding);
    Finally
      sgcFree(oBinding);
    End;
  end;
end;

procedure TsgcSTUNClient_Base.DoRetransmissionAdd(const aTransactionId: string;
  const aBytes: TBytes; const aIPAddress: string; aPort: Word = 0);
var
  oItem: TsgcUDPRetransmissionItem;
begin
  oItem := TsgcUDPRetransmissionItem.Create;
  oItem.ID := aTransactionId;
  oItem.IPAddress := aIPAddress;
  oItem.Port := aPort;
  oItem.Bytes := aBytes;
  oItem.MaxRetries := RetransmissionOptions.MaxRetries;

  TsgcUDPClient_Base_Hack(FUDPClient).Retransmissions.AddItem(oItem);
end;

function TsgcSTUNClient_Base.DoRetransmissionDelete(const aMessage
  : TsgcSTUN_Message): Boolean;
begin
  result := TsgcUDPClient_Base_Hack(FUDPClient).Retransmissions.DeleteItem
    (aMessage.Header.TransactionIdString);
end;

procedure TsgcSTUNClient_Base.DoSendRequest_LongTermCredential
  (aMethod: TsgcStunMessageMethod; const aRealm, aNonce: string;
  const aAttributes: TsgcSTUN_Attributes = nil);
var
  oMessage: TsgcSTUN_Message;
begin
  oMessage := TsgcSTUN_Message.Create;
  Try
    oMessage.MessageType := stmtClientRequest;
    DoPrepareRequest(aMethod, oMessage, aAttributes);
    DoSetLongTermCredentials(oMessage, aRealm, aNonce);
    DoSendMessage(oMessage);

    FRealm := aRealm;
    FNonce := aNonce;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcSTUNClient_Base.DoSendMessage(const aMessage: TsgcSTUN_Message;
  const aIPAddress: string = ''; aPort: Word = 0);
var
  oBytes: TBytes;
begin
  if Assigned(FOnSTUNBeforeSend) then
    FOnSTUNBeforeSend(self, aMessage);

  oBytes := aMessage.Write;
  if aIPAddress <> '' then
    UDPClient.WriteData(aIPAddress, aPort, oBytes)
  else
    DoWriteBytes(oBytes);

  case Transport of
    stunUDP:
      DoRetransmissionAdd(aMessage.Header.TransactionIdString, oBytes,
        aIPAddress, aPort);
    stunTCP:
      ;
  end;

  Requests.AddRequest(aMessage);
end;

procedure TsgcSTUNClient_Base.DoSendRequest(aMethod: TsgcStunMessageMethod;
  const aAttributes: TsgcSTUN_Attributes = nil);
var
  oMessage: TsgcSTUN_Message;
begin
  oMessage := TsgcSTUN_Message.Create;
  Try
    oMessage.MessageType := stmtClientRequest;
    DoPrepareRequest(aMethod, oMessage, aAttributes);
    DoSendMessage(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcSTUNClient_Base.DoSendResponse(const aMessage: TsgcSTUN_Message;
  const aIPAddress: string = ''; aPort: Word = 0);
var
  oBytes: TBytes;
begin
  oBytes := aMessage.Write;
  if aIPAddress <> '' then
    UDPClient.WriteData(aIPAddress, aPort, oBytes)
  else
    DoWriteBytes(oBytes);
end;

procedure TsgcSTUNClient_Base.DoSetLongTermCredentials(const aMessage
  : TsgcSTUN_Message; const aRealm, aNonce: string);
begin
  aMessage.Username := STUNOptions.Authentication.Username;
  aMessage.Realm := aRealm;
  aMessage.Password := STUNOptions.Authentication.Password;
  aMessage.Nonce := aNonce;
  aMessage.Credentials := STUNOptions.Authentication.Credentials;
end;

procedure TsgcSTUNClient_Base.DoSTUNExceptionEvent(E: Exception);
begin
  if Assigned(FOnSTUNException) then
    FOnSTUNException(self, E);

  DoTCPDisconnect;
end;

procedure TsgcSTUNClient_Base.DoSTUNRead(const aSocket: TsgcSocketConnection;
  const aBytes: TBytes);
var
  oError: TsgcSTUN_ResponseError;
  oMessage: TsgcSTUN_Message;
begin
  if Length(aBytes) >= 20 then
  begin
    oMessage := TsgcSTUN_Message.Create;
    Try
      oMessage.MessageType := stmtClientRequest;
      if (aBytes[0] and $C0) = 0 then
      begin
        oMessage.OnRequestMessageIntegrityKey :=
          OnRequestMessageIntegrityKeyEvent;
        oMessage.OnICEValidateMessageIntegrity :=
          OnICEValidateMessageIntegrityEvent;
        oMessage.OnICERequestMessageIntegrityPassword :=
          OnICERequestMessageIntegrityPasswordEvent;
        oMessage.Read(aBytes);

        // ... validations
        if oMessage.Fingerprint = stfsFingerprintInvalid then
          raise Exception.Create(S_STUN_INVALID_FINGERPRINT);
        if oMessage.MessageIntegrity in [stisMessageIntegrityInvalid,
          stisMessageIntegrityInvalidRequest] then
          raise Exception.Create(S_STUN_INVALID_MESSAGE_INTEGRITY);

        // ... retransmission
        if RetransmissionOptions.Enabled then
          DoRetransmissionDelete(oMessage);

        case oMessage.Header.MessageType_Class of
          stmcRequest:
            DoSTUNRequest(aSocket, oMessage);
          stmcIndication:
            DoSTUNIndication(aSocket, oMessage);
          stmcResponseSuccess:
            begin
              if Requests.ExistsRequest(oMessage) then
              begin
                Try
                  DoSTUNResponseSuccess(aSocket, oMessage);
                Finally
                  Requests.DeleteRequest(oMessage);
                End;
              end;
            end;
          stmcResponseError:
            begin
              if not Requests.ExistsRequest(oMessage) then
                exit;

              Try
                if DoSTUNReceivedRealm(oMessage) then
                  exit;

                if Assigned(FOnSTUNResponseError) then
                begin
                  oError := GetErrorFromMessage(oMessage);
                  Try
                    if (oError.Code >= 300) and (oError.Code < 400) then
                    begin
                      if not DoSTUNTryAlternate(oMessage) then
                        FOnSTUNResponseError(self, aSocket, oMessage, oError);
                    end
                    else
                      FOnSTUNResponseError(self, aSocket, oMessage, oError);
                  Finally
                    sgcFree(oError);
                  End;
                end;
              Finally
                Requests.DeleteRequest(oMessage);
              End;
            end;
        else
          raise Exception.Create(S_STUN_INVALID_MESSAGE_CLASS);
        end;
      end
      else
        raise Exception.Create(S_STUN_INVALID_HEADER_FORMAT);
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

function TsgcSTUNClient_Base.DoSTUNReceivedRealm(const aMessage
  : TsgcSTUN_Message): Boolean;
var
  vRealm, vNonce: string;
begin
  result := false;
  if STUNOptions.Authentication.Credentials <> stauLongTermCredential then
    exit;

  result := GetRealmNonce(aMessage, vRealm, vNonce);
  if result then
    DoSendRequest_LongTermCredential(aMessage.Header.MessageType_Method,
      vRealm, vNonce);
end;

procedure TsgcSTUNClient_Base.DoSTUNRequest(const aSocket: TsgcSocketConnection;
  const aMessage: TsgcSTUN_Message);
begin
  if Assigned(FOnICERequestBinding) then
    FOnICERequestBinding(self, aSocket, aMessage)
  else
    raise Exception.Create(S_STUN_INVALID_MESSAGE_CLASS);
end;

function TsgcSTUNClient_Base.DoSTUNTryAlternate(const aMessage
  : TsgcSTUN_Message): Boolean;
var
  oAttribute: TsgcSTUN_Attribute_ALTERNATE_SERVER;
begin
  result := false;

  oAttribute := TsgcSTUN_Attribute_ALTERNATE_SERVER
    (aMessage.Attributes.GetAttribute(stmaAlternate_Server));
  if Assigned(oAttribute) then
  begin
    if oAttribute.Address <> '' then
    begin
      Host := oAttribute.Address;
      Port := oAttribute.Port;
      case oAttribute.Family of
        1:
          IPVersion := Id_IpV4;
        2:
          IPVersion := Id_IpV6;
      end;
      SendRequest;
      result := True;
    end;
  end;
end;

procedure TsgcSTUNClient_Base.DoTCPDisconnect;
begin
  if Transport = stunTCP then
    TCPClient.Active := false;
end;

procedure TsgcSTUNClient_Base.DoWriteBytes(const aBytes: TBytes);
begin
  case Transport of
    stunUDP:
      begin
        UDPClient.WriteData(aBytes);
      end;
    stunTCP:
      begin
        if not TCPClient.Active then
          TCPClient.Connect;
        if TCPClient.Active then
          TCPClient.WriteData(aBytes);
      end;
  end;
end;

function TsgcSTUNClient_Base.GetBindingFromMessage(const aSocket
  : TsgcSocketConnection; const aMessage: TsgcSTUN_Message)
  : TsgcSTUN_ResponseBinding;
var
  oAttribute: TsgcSTUN_Attribute;
begin
  result := TsgcSTUN_ResponseBinding.Create;
  oAttribute := aMessage.Attributes.GetAttribute(stmaXOR_Mapped_Address);
  if not Assigned(oAttribute) then
    oAttribute := aMessage.Attributes.GetAttribute(stmaMapped_Address);
  if Assigned(oAttribute) then
  begin
    if oAttribute.Family = 2 then
      result.IPVersion := ipV6
    else
      result.IPVersion := ipV4;
    result.RemotePort := oAttribute.Port;
    result.RemoteIP := oAttribute.Address;
    result.LocalIP := aSocket.LocalIP;
    result.LocalPort := aSocket.LocalPort;
  end;
end;

function TsgcSTUNClient_Base.GetErrorFromMessage(const aMessage
  : TsgcSTUN_Message): TsgcSTUN_ResponseError;
var
  oAttribute: TsgcSTUN_Attribute_ERROR_CODE;
begin
  result := TsgcSTUN_ResponseError.Create;
  oAttribute := TsgcSTUN_Attribute_ERROR_CODE
    (aMessage.Attributes.GetAttribute(stmaError_Code));
  if Assigned(oAttribute) then
  begin
    result.Code := oAttribute.Code;
    result.Reason := oAttribute.Reason;
  end;
end;

function TsgcSTUNClient_Base.GetHost: string;
begin
  result := UDPClient.Host;
end;

function TsgcSTUNClient_Base.GetIPVersion: TIdIPVersion;
begin
  result := UDPClient.IPVersion;
end;

function TsgcSTUNClient_Base.GetNotifyEvents: TwsNotifyEvent;
begin
  result := TsgcUDPClient_Base_Hack(UDPClient).NotifyEvents;
end;

function TsgcSTUNClient_Base.GetPort: Integer;
begin
  result := UDPClient.Port;
end;

function TsgcSTUNClient_Base.GetRealmNonce(const aMessage: TsgcSTUN_Message;
  var Realm, Nonce: string): Boolean;
var
  oREALM: TsgcSTUN_Attribute_REALM;
  oNONCE: TsgcSTUN_Attribute_NONCE;
begin
  Realm := '';
  Nonce := '';

  oREALM := TsgcSTUN_Attribute_REALM
    (aMessage.Attributes.GetAttribute(stmaRealm));
  result := Assigned(oREALM);
  if result then
  begin
    Realm := oREALM.Realm;

    oNONCE := TsgcSTUN_Attribute_NONCE
      (aMessage.Attributes.GetAttribute(stmaNONCE));
    if Assigned(oNONCE) then
    begin
      Nonce := oNONCE.Nonce;
      result := True;
    end;
  end;
end;

function TsgcSTUNClient_Base.GetRequests: TsgcSTUNRequestsQueue;
begin
  if not Assigned(FRequests) then
  begin
    FRequests := TsgcSTUNRequestsQueue.Create;
    FRequests.OwnObjects := True;
  end;
  result := FRequests;
end;

function TsgcSTUNClient_Base.GetTCPClient: TsgcTCPCLient_Base;
begin
  if not Assigned(FTCPClient) then
  begin
    FTCPClient := TsgcTCPCLient_Base.Create(nil);
    TsgcTCPClient_Base_Hack(FTCPClient).OnBinary := OnTCPReadEvent;
    TsgcTCPClient_Base_Hack(FTCPClient).OnException := OnTCPExceptionEvent;
  end;
  if not FTCPClient.LogFile.Enabled then
    FTCPClient.LogFile.Filename := LogFile.Filename;
  FTCPClient.LogFile.Enabled := LogFile.Enabled;

  result := FTCPClient;
end;

function TsgcSTUNClient_Base.GetUDPClient: TsgcUDPCLient_Base;
begin
  if not Assigned(FUDPClient) then
  begin
    FUDPClient := TsgcUDPCLient_Base.Create(nil);
    TsgcUDPClient_Base_Hack(FUDPClient).RetransmissionOptions :=
      RetransmissionOptions;
    TsgcUDPClient_Base_Hack(FUDPClient).OnUDPRead := OnUDPReadEvent;
    TsgcUDPClient_Base_Hack(FUDPClient).OnUDPException := OnUDPExceptionEvent;
    TsgcUDPClient_Base_Hack(FUDPClient).OnRetranmissionTimeout :=
      OnRetransmissionTimeoutEvent;
  end;
  FUDPClient.LogFile.Enabled := LogFile.Enabled;
  FUDPClient.LogFile.Filename := LogFile.Filename;

  result := FUDPClient;
end;

procedure TsgcSTUNClient_Base.OnICERequestMessageIntegrityPasswordEvent
  (Sender: TObject; const aMessage: TsgcSTUN_Message; var Password: string);
begin
  if Assigned(FOnICERequestMessageIntegrityPassword) then
    FOnICERequestMessageIntegrityPassword(self, aMessage, Password);
end;

procedure TsgcSTUNClient_Base.OnRequestMessageIntegrityKeyEvent(Sender: TObject;
  const aTransactionId: String; var MessageIntegrityKey: TBytes);
begin
  MessageIntegrityKey := Requests.GetMessageIntegrityKey(aTransactionId);
end;

procedure TsgcSTUNClient_Base.OnTCPExceptionEvent(Connection: TsgcTCPConnection;
  E: Exception);
begin
  if Assigned(FOnSTUNException) then
    FOnSTUNException(self, E);
end;

procedure TsgcSTUNClient_Base.OnTCPReadEvent(Connection: TsgcTCPConnection;
  const aStream: TMemoryStream);
var
  oBytes: TBytes;
begin
  Try
    if aStream.Size > 0 then
    begin
      SetLength(oBytes, aStream.Size);
      aStream.Read(oBytes[0], aStream.Size);

      DoSTUNRead(Connection, oBytes);
    end;
  Except
    On E: Exception do
      DoSTUNExceptionEvent(E);
  End;
end;

procedure TsgcSTUNClient_Base.OnUDPExceptionEvent(Sender: TObject;
  Socket: TsgcUDPSocket; E: Exception);
begin
  if Assigned(FOnSTUNException) then
    FOnSTUNException(self, E);
end;

procedure TsgcSTUNClient_Base.OnUDPReadEvent(Sender: TObject;
  Socket: TsgcUDPSocket; Bytes: TsgcBytes);
begin
  Try
    DoSTUNRead(Socket, TBytes(Bytes));
  Except
    On E: Exception do
      DoSTUNExceptionEvent(E);
  End;
end;

procedure TsgcSTUNClient_Base.OnICEValidateMessageIntegrityEvent
  (Sender: TObject; const aUsername: string; var Password: string);
begin
  if Assigned(FOnICEValidateMessageIntegrity) then
    FOnICEValidateMessageIntegrity(self, aUsername, Password);
end;

procedure TsgcSTUNClient_Base.OnRetransmissionTimeoutEvent(Sender: TObject;
    const aBytes: TBytes; const aIPAddress: string; aPort: Word);
begin
  if Assigned(FOnRetransmissionTimeout) then
    FOnRetransmissionTimeout(self, aBytes, aIPAddress, aPort);
end;

procedure TsgcSTUNClient_Base.SendRequest;
begin
  Try
    if (STUNOptions.Authentication.Credentials = stauLongTermCredential) and
      (FRealm <> '') and (FNonce <> '') then
      DoSendRequest_LongTermCredential(stmmBinding, FRealm, FNonce)
    else
      DoSendRequest(stmmBinding);
  Except
    On E: Exception do
      DoSTUNExceptionEvent(E);
  End;
end;

procedure TsgcSTUNClient_Base.SetHost(const Value: string);
begin
  UDPClient.Host := Value;
  TCPClient.Host := Value;
end;

procedure TsgcSTUNClient_Base.SetIPVersion(const Value: TIdIPVersion);
begin
  UDPClient.IPVersion := Value;
  TCPClient.IPVersion := Value;
end;

procedure TsgcSTUNClient_Base.SetLogFile(const Value: TsgcSTUNLogFile);
begin
  if Assigned(FLogFile) then
    FLogFile.Assign(Value);
end;

procedure TsgcSTUNClient_Base.SetNotifyEvents(const Value: TwsNotifyEvent);
begin
  TsgcUDPClient_Base_Hack(UDPClient).NotifyEvents := Value;
  TsgcTCPClient_Base_Hack(TCPClient).NotifyEvents := Value;
end;

procedure TsgcSTUNClient_Base.SetPort(const Value: Integer);
begin
  UDPClient.Port := Value;
  TCPClient.Port := Value;
end;

procedure TsgcSTUNClient_Base.SetRetransmissionOptions
  (const Value: TsgcSTUNPRetransmissionClient_Options);
begin
  if Assigned(FRetransmissionOptions) then
    FRetransmissionOptions.Assign(Value);
end;

procedure TsgcSTUNClient_Base.SetSTUNOptions(const Value
  : TsgcSTUNClient_Options);
begin
  if Assigned(FSTUNOptions) then
    FSTUNOptions.Assign(Value);
end;

procedure TsgcSTUNClient_Base.WriteData(const aBytes: TBytes);
begin
  DoWriteBytes(aBytes);
end;

constructor TsgcSTUNPRetransmissionClient_Options.Create;
begin
  inherited;
  Enabled := True;
end;

constructor TsgcSTUNClient_Authentication.Create;
begin
  inherited;
  FCredentials := stauNone;
end;

procedure TsgcSTUNClient_Authentication.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSTUNClient_Authentication then
  begin
    Username := TsgcSTUNClient_Authentication(aSource).Username;
    Password := TsgcSTUNClient_Authentication(aSource).Password;
    Credentials := TsgcSTUNClient_Authentication(aSource).Credentials;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSTUNClient_Options.Create;
begin
  inherited;
  FAuthentication := TsgcSTUNClient_Authentication.Create;
  Software := false;
end;

destructor TsgcSTUNClient_Options.Destroy;
begin
  sgcFree(FAuthentication);
  inherited;
end;

procedure TsgcSTUNClient_Options.Assign(aSource: TPersistent);
begin
  inherited;
  if aSource is TsgcSTUNClient_Options then
  begin
    Authentication := TsgcSTUNClient_Options(aSource).Authentication;
  end;
end;

procedure TsgcSTUNClient_Options.SetAuthentication
  (const Value: TsgcSTUNClient_Authentication);
begin
  FAuthentication := Value;
end;

procedure TsgcSTUNRequestsQueue.AddRequest(const aMessage: TsgcSTUN_Message);
var
  oItem: TsgcSTUNRequestItem;
  oAttribute: TsgcSTUN_Attribute;
  i: Integer;
begin
  oItem := TsgcSTUNRequestItem.Create;
  oItem.ID := aMessage.Header.TransactionIdString;
  oItem.Method := aMessage.Header.MessageType_Method;

  // message integrity
  oAttribute := aMessage.Attributes.GetAttribute(stmaMesssage_Integrity);
  if Assigned(oAttribute) then
    oItem.MessageIntegrityKey := TsgcSTUN_Attribute_MESSAGE_INTEGRITY
      (oAttribute).Key;

  // parameters
  case oItem.Method of
    stmmCreatePermission:
      begin
        for i := 0 to aMessage.Attributes.Count - 1 do
        begin
          oAttribute := aMessage.Attributes.Items[i];
          if oAttribute.AttributeType = stmaXOR_Peer_Address then
            oItem.Parameters.Add(TsgcSTUN_Attribute_XOR_PEER_ADDRESS
              (oAttribute).Address);
        end;
      end;
    stmmChannelBind:
      begin
        oAttribute := aMessage.Attributes.GetAttribute(stmaXOR_Peer_Address);
        if Assigned(oAttribute) then
        begin
          oItem.Parameters.Add(TsgcSTUN_Attribute_XOR_PEER_ADDRESS
            (oAttribute).Address);
          oItem.Parameters.Add
            (IntToStr(TsgcSTUN_Attribute_XOR_PEER_ADDRESS(oAttribute).Port));
        end;
        oAttribute := aMessage.Attributes.GetAttribute(stmaChannel_Number);
        if Assigned(oAttribute) then
          oItem.Parameters.Add
            (IntToStr(TsgcSTUN_Attribute_CHANNEL_NUMBER(oAttribute).Channel));
      end;
  end;

  // add item
  Add(oItem);
end;

function TsgcSTUNRequestsQueue.DeleteRequest(const aMessage
  : TsgcSTUN_Message): Boolean;
begin
  result := DeleteItem(aMessage.Header.TransactionIdString);
end;

function TsgcSTUNRequestsQueue.ExistsRequest(const aMessage
  : TsgcSTUN_Message): Boolean;
begin
  result := GetItem(aMessage.Header.TransactionIdString) <> nil;
end;

function TsgcSTUNRequestsQueue.GetMessageIntegrityKey(aTransactionId
  : string): TBytes;
var
  oItem: TsgcSTUNRequestItem;
begin
  result := nil;

  oItem := TsgcSTUNRequestItem(GetItem(aTransactionId));
  if Assigned(oItem) then
    result := oItem.MessageIntegrityKey;
end;

function TsgcSTUNRequestsQueue.GetRequest(const aMessage: TsgcSTUN_Message)
  : TsgcSTUNRequestItem;
begin
  result := TsgcSTUNRequestItem(GetItem(aMessage.Header.TransactionIdString));
end;

destructor TsgcSTUNRequestItem.Destroy;
begin
  sgcFree(FParameters);
  inherited;
end;

function TsgcSTUNRequestItem.GetParameters: TStringList;
begin
  if not Assigned(FParameters) then
    FParameters := TStringList.Create;
  result := FParameters;
end;

procedure TsgcSTUNRequestItem.SetMessageIntegrityKey(const Value: TBytes);
begin
  if Assigned(Value) then
  begin
    SetLength(FMessageIntegrityKey, Length(Value));
    sgcMove(Value[0], FMessageIntegrityKey[0], Length(FMessageIntegrityKey));
  end
  else
    SetLength(FMessageIntegrityKey, 0);
end;

{$ENDIF}

end.
