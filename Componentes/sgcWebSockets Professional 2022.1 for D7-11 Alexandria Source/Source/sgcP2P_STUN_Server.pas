{ ***************************************************************************
  sgcP2P component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcP2P_STUN_Server;

interface

{$I sgcVer.inc}
{$IFDEF SGC_STUN}

uses
  Classes, SysUtils,
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSocketHandle{$ELSE}IdSocketHandle{$ENDIF},
  // sgc
  sgcSocket_Classes, sgcUDP_Classes, sgcP2P_STUN_Classes, sgcUDP_Server,
  sgcP2P_STUN_Types, sgcWebSocket_Classes_Queues, sgcWebSocket_Types,
  sgcBase_Helpers;

type

  TsgcSTUNRequestSuccessEvent = procedure(Sender: TObject;
    const aSocket: TsgcSocketConnection;
    const aRequest, aResponse: TsgcSTUN_Message; var Accept: Boolean) of object;
  TsgcSTUNRequestErrorEvent = procedure(Sender: TObject;
    const aSocket: TsgcSocketConnection; const aRequest: TsgcSTUN_Message;
    const aResponse: TsgcSTUN_Message; var Accept: Boolean) of object;
  TsgcSTUNRequestAuthorizationEvent = procedure(Sender: TObject;
    const aRequest: TsgcSTUN_Message; const aUsername, aRealm: string;
    var Password: string) of object;

  TsgcSTUNServer_AlternateServer = class(TPersistent)
  private
    FDomain: string;
    FEnabled: Boolean;
    FFamily: TwsIPVersion;
    FIPAddress: String;
    FPort: Word;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Domain: string read FDomain write FDomain;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Family: TwsIPVersion read FFamily write FFamily;
    property IPAddress: String read FIPAddress write FIPAddress;
    property Port: Word read FPort write FPort;
  end;

  TsgcSTUNServer_Authentication_LongTermCredentials = class(TPersistent)
  private
    FEnabled: Boolean;
    FRealm: string;
    FStaleNonce: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Realm: string read FRealm write FRealm;
    property StaleNonce: Integer read FStaleNonce write FStaleNonce;
  end;

  TsgcSTUNServer_Authentication_ShortTermCredentials = class(TPersistent)
  private
    FEnabled: Boolean;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TsgcSTUNServer_BindingAttributes_Options = class(TPersistent)
  private
    FAlternateServer: TsgcSTUNServer_AlternateServer;
    FMappedAddressRFC3489: Boolean;
    FOtherAddress: Boolean;
    FResponseOrigin: Boolean;
    FSourceAddress: Boolean;
    procedure SetAlternateServer(const Value: TsgcSTUNServer_AlternateServer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property AlternateServer: TsgcSTUNServer_AlternateServer
      read FAlternateServer write SetAlternateServer;
    property MappedAddressRFC3489: Boolean read FMappedAddressRFC3489
      write FMappedAddressRFC3489;
    property OtherAddress: Boolean read FOtherAddress write FOtherAddress;
    property ResponseOrigin: Boolean read FResponseOrigin write FResponseOrigin;
    property SourceAddress: Boolean read FSourceAddress write FSourceAddress;
  end;

  TsgcSTUNNonceItem = class(TsgcQueueItemBase)
  private
    FExpires: TDateTime;
    FRealm: string;
  public
    property Expires: TDateTime read FExpires write FExpires;
    property Realm: string read FRealm write FRealm;
  end;

  TsgcSTUNNoncesQueue = class(TsgcQueueBase)
  private
    FStaleNonce: Integer;
  public
    function AddNonce(const aRealm: String): string;
    function GetNonceState(const aNonce: String): TsgcStunNonceState;
  public
    property StaleNonce: Integer read FStaleNonce write FStaleNonce;
  end;

  TsgcSTUNServer_Authentication = class(TPersistent)
  private
    FEnabled: Boolean;
    FLongTermCredentials: TsgcSTUNServer_Authentication_LongTermCredentials;
    FShortTermCredentials: TsgcSTUNServer_Authentication_ShortTermCredentials;
    procedure SetLongTermCredentials(const Value
      : TsgcSTUNServer_Authentication_LongTermCredentials);
    procedure SetShortTermCredentials(const Value
      : TsgcSTUNServer_Authentication_ShortTermCredentials);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property LongTermCredentials
      : TsgcSTUNServer_Authentication_LongTermCredentials
      read FLongTermCredentials write SetLongTermCredentials;
    property ShortTermCredentials
      : TsgcSTUNServer_Authentication_ShortTermCredentials
      read FShortTermCredentials write SetShortTermCredentials;
  end;

  TsgcSTUNServer_Options = class(TsgcSTUN_Options)
  private
    FAuthentication: TsgcSTUNServer_Authentication;
    FBindingAttributes: TsgcSTUNServer_BindingAttributes_Options;
    procedure SetAuthentication(const Value: TsgcSTUNServer_Authentication);
    procedure SetBindingAttributes(const Value
      : TsgcSTUNServer_BindingAttributes_Options);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Authentication: TsgcSTUNServer_Authentication read FAuthentication
      write SetAuthentication;
    property BindingAttributes: TsgcSTUNServer_BindingAttributes_Options
      read FBindingAttributes write SetBindingAttributes;
  end;

  TsgcSTUNServer_Base = class(TsgcSTUN_Component)
    { udpserver }
  private
    FUDPServer: TsgcUDPServer_Base;
    function GetUDPServer: TsgcUDPServer_Base;
  protected
    procedure OnUDPReadEvent(Sender: TObject; Socket: TsgcUDPSocket;
      Bytes: TsgcBytes); virtual;
    procedure OnUDPExceptionEvent(Sender: TObject; Socket: TsgcUDPSocket;
      E: Exception); virtual;
  protected
    property UDPServer: TsgcUDPServer_Base read GetUDPServer write FUDPServer;
  public
    function AddBinding(const aIPAddress: string; aPort: Integer)
      : TIdSocketHandle;
    function RemoveBinding(const aIPAddress: string; aPort: Integer): Boolean;
    { udpserver }

    { nonces }
  private
    FNonces: TsgcSTUNNoncesQueue;
    function GetNonces: TsgcSTUNNoncesQueue;
  protected
    property Nonces: TsgcSTUNNoncesQueue read GetNonces write FNonces;
    { nonces }

    { constructor/destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor/destructor }

    { properties }
  private
    FLogFile: TsgcSTUNLogFile;
    FSTUNOptions: TsgcSTUNServer_Options;
    function GetActive: Boolean;
    function GetBindings: TIdSocketHandles;
    function GetHost: string;
    function GetIPVersion: TIdIPVersion;
    function GetPort: Integer;
    function GetNotifyEvents: TwsNotifyEvent;
    procedure SetNotifyEvents(const Value: TwsNotifyEvent);
    procedure SetLogFile(const Value: TsgcSTUNLogFile); virtual;
    procedure SetBindings(const Value: TIdSocketHandles);
    procedure SetHost(const Value: string);
    procedure SetIPVersion(const Value: TIdIPVersion);
    procedure SetPort(const Value: Integer);
    procedure SetSTUNOptions(const Value: TsgcSTUNServer_Options);
  protected
    procedure SetActive(const Value: Boolean); virtual;
  public
    property Active: Boolean read GetActive write SetActive;
    property Bindings: TIdSocketHandles read GetBindings write SetBindings;
    property Host: string read GetHost write SetHost;
    property IPVersion: TIdIPVersion read GetIPVersion write SetIPVersion;
    property LogFile: TsgcSTUNLogFile read FLogFile write SetLogFile;
    property NotifyEvents: TwsNotifyEvent read GetNotifyEvents
      write SetNotifyEvents;
    property Port: Integer read GetPort write SetPort;
    property STUNOptions: TsgcSTUNServer_Options read FSTUNOptions
      write SetSTUNOptions;
    { properties }

    { methods }
  private
    function IsValidIPAddress(const aIPAddress: string): Boolean;
  protected
    procedure DoAddMappedAddress(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message); virtual;
    procedure DoAddResponseOrigin(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message); virtual;
    procedure DoAddSourceAddress(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message); virtual;
    procedure DoAddOtherAddress(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message); virtual;
    procedure DoAddSoftware(const aResponse: TsgcSTUN_Message); virtual;
  protected
    function DoValidateRequest_LongTermCredentials(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message;
      const aLongTermCredentials
      : TsgcSTUNServer_Authentication_LongTermCredentials): Boolean; virtual;
    function DoValidateRequest_RequiredAttributes(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message): Boolean;
  protected
    procedure OnRequestMessageIntegrityPasswordEvent(Sender: TObject;
      const aUsername, aRealm: String; var Password: string); virtual;
  protected
    function DoValidateRequest(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message;
      const aAuthentication: TsgcSTUNServer_Authentication): Boolean; virtual;
    procedure DoRequestSuccess(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message); virtual;
    procedure DoRequestError(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message;
      aError: TsgcStunErrorResponseCodes); virtual;
  protected
    procedure DoSendResponseSuccess(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message); virtual;
    procedure DoSendResponseError(const aSocket: TsgcUDPSocket;
      const aResponse: TsgcSTUN_Message); virtual;
  protected
    procedure DoSTUNRequestMethod(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message); virtual;
    procedure DoSTUNIndication(const aSocket: TsgcUDPSocket;
      const aRequest, aResponse: TsgcSTUN_Message); virtual;
    procedure DoReadData(const Socket: TsgcUDPSocket;
      const Bytes: TBytes); virtual;
    { methods }

    { events }
  protected
    FOnSTUNException: TsgcSTUNExceptionEvent;
    FOnSTUNRequestAuthorization: TsgcSTUNRequestAuthorizationEvent;
    FOnSTUNRequestError: TsgcSTUNRequestErrorEvent;
    FOnSTUNRequestSuccess: TsgcSTUNRequestSuccessEvent;
  protected
    procedure DoSTUNExceptionEvent(E: Exception); virtual;
  public
    property OnSTUNException: TsgcSTUNExceptionEvent read FOnSTUNException
      write FOnSTUNException;
    property OnSTUNRequestError: TsgcSTUNRequestErrorEvent
      read FOnSTUNRequestError write FOnSTUNRequestError;
    property OnSTUNRequestSuccess: TsgcSTUNRequestSuccessEvent
      read FOnSTUNRequestSuccess write FOnSTUNRequestSuccess;
    property OnSTUNRequestAuthorization: TsgcSTUNRequestAuthorizationEvent
      read FOnSTUNRequestAuthorization write FOnSTUNRequestAuthorization;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_STUN}

uses
  DateUtils, StrUtils,
  // sgc
  sgcP2P_STUN_Helpers, sgcBase_Const;

type
  TsgcUDPServer_Base_Hack = class(TsgcUDPServer_Base);

constructor TsgcSTUNServer_Base.Create(aOwner: TComponent);
begin
  inherited;
  FLogFile := TsgcSTUNLogFile.Create;
  NotifyEvents := neNoSync;
  FSTUNOptions := TsgcSTUNServer_Options.Create;
  Port := CS_STUN_DEFAULT_PORT;
end;

destructor TsgcSTUNServer_Base.Destroy;
begin
  sgcFree(FLogFile);
  sgcFree(FSTUNOptions);
  sgcFree(FUDPServer);
  inherited;
end;

function TsgcSTUNServer_Base.AddBinding(const aIPAddress: string;
  aPort: Integer): TIdSocketHandle;
begin
  Result := UDPServer.AddBinding(aIPAddress, aPort);
end;

procedure TsgcSTUNServer_Base.DoAddMappedAddress(const aSocket: TsgcUDPSocket;
  const aRequest, aResponse: TsgcSTUN_Message);
var
  oAttribute: TsgcSTUN_Attribute;
begin
  // ... add XOR or Mapped Address
  if aRequest.Header.MagicCookie = CS_STUN_MAGIC_COOKIE then
  begin
    oAttribute := TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS.Create();
    TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS(oAttribute).TransactionId :=
      aRequest.Header.TransactionId;
  end
  else
    oAttribute := TsgcSTUN_Attribute_MAPPED_ADDRESS.Create();
  case aSocket.IPVersion of
    ipV4:
      oAttribute.Family := 1;
    ipV6:
      oAttribute.Family := 2;
  end;
  oAttribute.Port := aSocket.Port;
  oAttribute.Address := aSocket.IP;
  aResponse.Attributes.Add(oAttribute);

  // ... if XOR Mapped Address, add Mapped Address for compatibilty with RFC3489
  if STUNOptions.BindingAttributes.MappedAddressRFC3489 then
  begin
    if aRequest.Header.MagicCookie = CS_STUN_MAGIC_COOKIE then
    begin
      oAttribute := TsgcSTUN_Attribute_MAPPED_ADDRESS.Create();
      case aSocket.IPVersion of
        ipV4:
          oAttribute.Family := 1;
        ipV6:
          oAttribute.Family := 2;
      end;
      oAttribute.Port := aSocket.Port;
      oAttribute.Address := aSocket.IP;
      aResponse.Attributes.Add(oAttribute);
    end;
  end;
end;

procedure TsgcSTUNServer_Base.DoAddOtherAddress(const aSocket: TsgcUDPSocket;
  const aRequest, aResponse: TsgcSTUN_Message);
var
  i: Integer;
  oAttribute: TsgcSTUN_Attribute;
begin
  for i := 1 to Bindings.Count - 1 do
  begin
{$IFDEF SGC_INDY_LIB}
    if LeftStr(Bindings[i].Name, Length(CS_STUN_RELAY)) <> CS_STUN_RELAY then
    begin
{$ENDIF}
      if IsValidIPAddress(Bindings[i].IP) then
      begin
        oAttribute := TsgcSTUN_Attribute_OTHER_ADDRESS.Create;
        case Bindings[i].IPVersion of
          Id_Ipv4:
            oAttribute.Family := 1;
          Id_Ipv6:
            oAttribute.Family := 2;
        end;
        oAttribute.Port := Bindings[i].Port;
        oAttribute.Address := Bindings[i].IP;
        aResponse.Attributes.Add(oAttribute);
      end;
{$IFDEF SGC_INDY_LIB}
    end;
{$ENDIF}
  end;
end;

procedure TsgcSTUNServer_Base.DoAddResponseOrigin(const aSocket: TsgcUDPSocket;
  const aRequest, aResponse: TsgcSTUN_Message);
var
  oAttribute: TsgcSTUN_Attribute;
begin
  if IsValidIPAddress(aSocket.LocalIP) then
  begin
    oAttribute := TsgcSTUN_Attribute_RESPONSE_ORIGIN.Create;
    case aSocket.IPVersion of
      ipV4:
        oAttribute.Family := 1;
      ipV6:
        oAttribute.Family := 2;
    end;
    oAttribute.Port := aSocket.LocalPort;
    oAttribute.Address := aSocket.LocalIP;
    aResponse.Attributes.Add(oAttribute);
  end;
end;

procedure TsgcSTUNServer_Base.DoAddSoftware(const aResponse: TsgcSTUN_Message);
var
  oAttribute: TsgcSTUN_Attribute;
begin
  oAttribute := TsgcSTUN_Attribute_SOFTWARE.Create();
  TsgcSTUN_Attribute_SOFTWARE(oAttribute).Software := CS_APPLICATION_NAME + ' '
    + CS_VERSION;
  aResponse.Attributes.Add(oAttribute);
end;

procedure TsgcSTUNServer_Base.DoAddSourceAddress(const aSocket: TsgcUDPSocket;
  const aRequest, aResponse: TsgcSTUN_Message);
var
  oAttribute: TsgcSTUN_Attribute;
begin
  if IsValidIPAddress(aSocket.LocalIP) then
  begin
    oAttribute := TsgcSTUN_Attribute_SOURCE_ADDRESS.Create;
    case aSocket.IPVersion of
      ipV4:
        oAttribute.Family := 1;
      ipV6:
        oAttribute.Family := 2;
    end;
    oAttribute.Port := aSocket.LocalPort;
    oAttribute.Address := aSocket.LocalIP;
    aResponse.Attributes.Add(oAttribute);
  end;
end;

procedure TsgcSTUNServer_Base.DoRequestError(const aSocket: TsgcUDPSocket;
  const aRequest, aResponse: TsgcSTUN_Message;
  aError: TsgcStunErrorResponseCodes);
var
  oAttribute: TsgcSTUN_Attribute;
  vAccept: Boolean;
  vCode: Word;
  vReason: string;
begin
  // ... error code
  oAttribute := TsgcSTUN_Attribute_ERROR_CODE.Create();
  GetErrorResponseCode(aError, vCode, vReason);
  TsgcSTUN_Attribute_ERROR_CODE(oAttribute).Code := vCode;
  TsgcSTUN_Attribute_ERROR_CODE(oAttribute).Reason := vReason;
  aResponse.Attributes.Add(oAttribute);
  // ... alternate server
  if (vCode = 300) and STUNOptions.BindingAttributes.AlternateServer.Enabled
  then
  begin
    oAttribute := TsgcSTUN_Attribute_ALTERNATE_SERVER.Create;
    case STUNOptions.BindingAttributes.AlternateServer.Family of
      ipV4:
        oAttribute.Family := 1;
      ipV6:
        oAttribute.Family := 2;
    end;
    oAttribute.Port := STUNOptions.BindingAttributes.AlternateServer.Port;
    oAttribute.Address := STUNOptions.BindingAttributes.
      AlternateServer.IPAddress;
    aResponse.Attributes.Add(oAttribute);

    if STUNOptions.BindingAttributes.AlternateServer.Domain <> '' then
    begin
      oAttribute := TsgcSTUN_Attribute_ALTERNATE_DOMAIN.Create;
      TsgcSTUN_Attribute_ALTERNATE_DOMAIN(oAttribute).Domain :=
        STUNOptions.BindingAttributes.AlternateServer.Domain;
      aResponse.Attributes.Add(oAttribute);
    end;
  end;
  // ... software
  if STUNOptions.Software then
  begin
    oAttribute := TsgcSTUN_Attribute_SOFTWARE.Create();
    TsgcSTUN_Attribute_SOFTWARE(oAttribute).Software := CS_APPLICATION_NAME +
      ' ' + CS_VERSION;
    aResponse.Attributes.Add(oAttribute);
  end;

  vAccept := True;
  if Assigned(FOnSTUNRequestError) then
    FOnSTUNRequestError(self, aSocket, aRequest, aResponse, vAccept);

  if not vAccept then
    exit;

  // ... send response
  aResponse.Header.MessageType_Method := aRequest.Header.MessageType_Method;
  aResponse.Header.MessageType_Class := stmcResponseError;
  aResponse.Header.MagicCookie := aRequest.Header.MagicCookie;
  DoSendResponseError(aSocket, aResponse);
end;

procedure TsgcSTUNServer_Base.DoSTUNExceptionEvent(E: Exception);
begin
  if Assigned(FOnSTUNException) then
    FOnSTUNException(self, E);
end;

procedure TsgcSTUNServer_Base.DoReadData(const Socket: TsgcUDPSocket;
  const Bytes: TBytes);
var
  oRequest, oResponse: TsgcSTUN_Message;
  vMessageClass: TsgcStunMessageClass;
begin
  if Length(Bytes) >= 20 then
  begin
    oRequest := TsgcSTUN_Message.Create;
    oResponse := TsgcSTUN_Message.Create;
    Try
      oRequest.MessageType := stmtServerRequest;
      oResponse.MessageType := stmtServerResponse;
      if (Bytes[0] and $C0) = 0 then
      begin
        oRequest.OnRequestMessageIntegrityPassword :=
          OnRequestMessageIntegrityPasswordEvent;
        oRequest.Read(Bytes);
        // ... fingerprint
        if oRequest.Fingerprint = stfsFingerprintInvalid then
          raise Exception.Create(S_STUN_INVALID_FINGERPRINT);
        if STUNOptions.Fingerprint then
          oResponse.Fingerprint := stfsFingerprintValid;
        // ... message integrity
        if oRequest.MessageIntegrity = stisMessageIntegrityInvalid then
          raise Exception.Create(S_STUN_INVALID_MESSAGE_INTEGRITY)
        else if oRequest.MessageIntegrity = stisMessageIntegrityInvalidRequest
        then
        begin
          DoRequestError(Socket, oRequest, oResponse, sercInvalidRequest);
          raise Exception.Create(S_STUN_INVALID_MESSAGE_INTEGRITY)
        end;

        oResponse.Header.MagicCookie := oRequest.Header.MagicCookie;
        sgcMove(oRequest.Header.TransactionId[0],
          oResponse.Header.TransactionId[0], 12);
        oResponse.Credentials := oRequest.Credentials;
        oResponse.Fingerprint := oRequest.Fingerprint;
        oResponse.MessageIntegrity := oRequest.MessageIntegrity;
        oResponse.MessageIntegritySHA256 := oRequest.MessageIntegritySHA256;

        vMessageClass := sgcGetSTUNMessageClass(oRequest.Header.MessageType);
        case vMessageClass of
          stmcRequest:
            begin
              if STUNOptions.BindingAttributes.AlternateServer.Enabled then
                DoRequestError(Socket, oRequest, oResponse, sercTryAlternate)
              else
                DoSTUNRequestMethod(Socket, oRequest, oResponse);
            end;
          stmcIndication:
            DoSTUNIndication(Socket, oRequest, oResponse);
          stmcResponseSuccess:
            ;
          stmcResponseError:
            ;
        else
          DoRequestError(Socket, oRequest, oResponse, sercInvalidRequest);
        end;
      end
      else
        DoRequestError(Socket, oRequest, oResponse, sercInvalidRequest);
    Finally
      sgcFree(oRequest);
      sgcFree(oResponse);
    End;
  end;
end;

procedure TsgcSTUNServer_Base.DoRequestSuccess(const aSocket: TsgcUDPSocket;
  const aRequest, aResponse: TsgcSTUN_Message);
var
  vAccept: Boolean;
begin
  // ... mapped address
  DoAddMappedAddress(aSocket, aRequest, aResponse);
  // ... response origin
  if STUNOptions.BindingAttributes.ResponseOrigin then
    DoAddResponseOrigin(aSocket, aRequest, aResponse);
  // ... source address
  if STUNOptions.BindingAttributes.SourceAddress then
    DoAddSourceAddress(aSocket, aRequest, aResponse);
  // ... other address
  if STUNOptions.BindingAttributes.OtherAddress then
    DoAddOtherAddress(aSocket, aRequest, aResponse);
  // ... software
  if STUNOptions.Software then
    DoAddSoftware(aResponse);

  vAccept := True;
  if Assigned(FOnSTUNRequestSuccess) then
    FOnSTUNRequestSuccess(self, aSocket, aRequest,
      aResponse, vAccept);

  if not vAccept then
    exit;

  // ... send response
  DoSendResponseSuccess(aSocket, aRequest, aResponse);
end;

procedure TsgcSTUNServer_Base.DoSendResponseError(const aSocket: TsgcUDPSocket;
  const aResponse: TsgcSTUN_Message);
begin
  aResponse.Header.MessageLength := Length(aResponse.Attributes.Write);

  aSocket.WriteData(aResponse.Write);
end;

procedure TsgcSTUNServer_Base.DoSendResponseSuccess(const aSocket
  : TsgcUDPSocket; const aRequest, aResponse: TsgcSTUN_Message);
begin
  aResponse.Header.MessageType_Method := sgcGetSTUNMessageMethod
    (aRequest.Header.MessageType);
  aResponse.Header.MessageType_Class := stmcResponseSuccess;
  aResponse.Header.MessageLength := Length(aResponse.Attributes.Write);

  aSocket.WriteData(aResponse.Write);
end;

procedure TsgcSTUNServer_Base.DoSTUNIndication(const aSocket: TsgcUDPSocket;
  const aRequest, aResponse: TsgcSTUN_Message);
begin
  DoRequestError(aSocket, aRequest, aResponse, sercInvalidRequest);
end;

procedure TsgcSTUNServer_Base.DoSTUNRequestMethod(const aSocket: TsgcUDPSocket;
  const aRequest, aResponse: TsgcSTUN_Message);
begin
  case sgcGetSTUNMessageMethod(aRequest.Header.MessageType) of
    stmmBinding:
      begin
        if DoValidateRequest(aSocket, aRequest, aResponse,
          STUNOptions.Authentication) then
          DoRequestSuccess(aSocket, aRequest, aResponse)
      end
  else
    begin
      DoRequestError(aSocket, aRequest, aResponse, sercInvalidRequest);
      raise Exception.Create(S_STUN_INVALID_REQUEST);
    end;
  end;
end;

function TsgcSTUNServer_Base.DoValidateRequest(const aSocket: TsgcUDPSocket;
  const aRequest, aResponse: TsgcSTUN_Message;
  const aAuthentication: TsgcSTUNServer_Authentication): Boolean;
begin
  Result := not aAuthentication.Enabled;

  if not Result then
  begin
    if DoValidateRequest_RequiredAttributes(aSocket, aRequest, aResponse) then
    begin
      if aAuthentication.LongTermCredentials.Enabled then
        Result := DoValidateRequest_LongTermCredentials(aSocket, aRequest,
          aResponse, aAuthentication.LongTermCredentials)
      else
        DoRequestError(aSocket, aRequest, aResponse, sercUnauthorized);
    end
    else
      DoRequestError(aSocket, aRequest, aResponse, sercUnknownAttribute);
  end;
end;

function TsgcSTUNServer_Base.DoValidateRequest_LongTermCredentials
  (const aSocket: TsgcUDPSocket; const aRequest, aResponse: TsgcSTUN_Message;
  const aLongTermCredentials
  : TsgcSTUNServer_Authentication_LongTermCredentials): Boolean;
var
  oAttribute: TsgcSTUN_Attribute;
  vNonceState: TsgcStunNonceState;
begin
  Result := False;
  vNonceState := stnsNonceUnknown;

  oAttribute := aRequest.Attributes.GetAttribute(stmaNonce);
  if Assigned(oAttribute) then
  begin
    vNonceState := Nonces.GetNonceState
      (TsgcSTUN_Attribute_NONCE(oAttribute).Nonce);
    Result := vNonceState = stnsNonceValid;

    // ... check that MessageIntegrity exists
    if Result then
    begin
      if (aRequest.Attributes.GetAttribute(stmaMesssage_Integrity) = nil) and
        (aRequest.Attributes.GetAttribute(stmaMesssage_Integrity_SHA256) = nil)
      then
        Result := False;
    end;
  end;

  if not Result then
  begin
    // nonce
    oAttribute := TsgcSTUN_Attribute_NONCE.Create;
    TsgcSTUN_Attribute_NONCE(oAttribute).Nonce :=
      Nonces.AddNonce(aLongTermCredentials.Realm);
    aResponse.Attributes.Add(oAttribute);
    // realm
    oAttribute := TsgcSTUN_Attribute_REALM.Create;
    TsgcSTUN_Attribute_REALM(oAttribute).Realm := aLongTermCredentials.Realm;
    aResponse.Attributes.Add(oAttribute);
    // send response
    case vNonceState of
      stnsNonceUnknown:
        DoRequestError(aSocket, aRequest, aResponse, sercUnauthorized);
      stnsNonceStaled:
        DoRequestError(aSocket, aRequest, aResponse, sercStaleNonce);
    end;
  end;
end;

function TsgcSTUNServer_Base.DoValidateRequest_RequiredAttributes
  (const aSocket: TsgcUDPSocket;
  const aRequest, aResponse: TsgcSTUN_Message): Boolean;
var
  i: Integer;
  oAttribute: TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES;
begin
  oAttribute := nil;

  for i := 0 to aRequest.Attributes.Count - 1 do
  begin
    if TsgcSTUN_Attribute(aRequest.Attributes[i]).AttributeType
      in [stmaPassword_Algorithm, stmaUserhash] then
    begin
      if not Assigned(oAttribute) then
        oAttribute := TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES.Create;
      oAttribute.UnknownAttributes := oAttribute.UnknownAttributes +
        [TsgcSTUN_Attribute(aRequest.Attributes[i]).AttributeType];
    end;
  end;

  Result := not Assigned(oAttribute);

  if not Result then
    aResponse.Attributes.Add(oAttribute);
end;

function TsgcSTUNServer_Base.GetActive: Boolean;
begin
  Result := UDPServer.Active;
end;

function TsgcSTUNServer_Base.GetBindings: TIdSocketHandles;
begin
  Result := UDPServer.Bindings;
end;

function TsgcSTUNServer_Base.GetHost: string;
begin
  Result := UDPServer.Host;
end;

function TsgcSTUNServer_Base.GetIPVersion: TIdIPVersion;
begin
  Result := UDPServer.IPVersion;
end;

function TsgcSTUNServer_Base.GetNonces: TsgcSTUNNoncesQueue;
begin
  if not Assigned(FNonces) then
  begin
    FNonces := TsgcSTUNNoncesQueue.Create;
    FNonces.OwnObjects := True;
  end;
  FNonces.StaleNonce := STUNOptions.Authentication.LongTermCredentials.
    StaleNonce;
  Result := FNonces;
end;

function TsgcSTUNServer_Base.GetNotifyEvents: TwsNotifyEvent;
begin
  Result := TsgcUDPServer_Base_Hack(UDPServer).NotifyEvents;
end;

function TsgcSTUNServer_Base.GetPort: Integer;
begin
  Result := UDPServer.Port;
end;

function TsgcSTUNServer_Base.GetUDPServer: TsgcUDPServer_Base;
begin
  if not Assigned(FUDPServer) then
  begin
    FUDPServer := TsgcUDPServer_Base.Create(nil);
    TsgcUDPServer_Base_Hack(FUDPServer).OnUDPRead := OnUDPReadEvent;
    TsgcUDPServer_Base_Hack(FUDPServer).OnUDPException := OnUDPExceptionEvent;
  end;
  if not FUDPServer.LogFile.Enabled then
    FUDPServer.LogFile.Filename := LogFile.Filename;
  FUDPServer.LogFile.Enabled := LogFile.Enabled;

  Result := FUDPServer;
end;

function TsgcSTUNServer_Base.IsValidIPAddress(const aIPAddress: string)
  : Boolean;
begin
  Result := (aIPAddress <> '') and (aIPAddress <> '0.0.0.0') and
    (aIPAddress <> '0:0:0:0:0:0:0:0');
end;

procedure TsgcSTUNServer_Base.OnRequestMessageIntegrityPasswordEvent
  (Sender: TObject; const aUsername, aRealm: String; var Password: string);
begin
  if Assigned(FOnSTUNRequestAuthorization) then
    FOnSTUNRequestAuthorization(self, TsgcSTUN_Message(Sender), aUsername,
      aRealm, Password);
end;

procedure TsgcSTUNServer_Base.OnUDPExceptionEvent(Sender: TObject;
  Socket: TsgcUDPSocket; E: Exception);
begin
  if Assigned(FOnSTUNException) then
    FOnSTUNException(self, E);
end;

procedure TsgcSTUNServer_Base.OnUDPReadEvent(Sender: TObject;
  Socket: TsgcUDPSocket; Bytes: TsgcBytes);
begin
  Try
    DoReadData(Socket, TBytes(Bytes));
  Except
    On E: TsgcSTUNExceptionSilent do
    begin
      // don't notify user
    end;
    On E: Exception do
      DoSTUNExceptionEvent(E);
  End;
end;

function TsgcSTUNServer_Base.RemoveBinding(const aIPAddress: string;
  aPort: Integer): Boolean;
begin
  Result := UDPServer.RemoveBinding(aIPAddress, aPort);
end;

procedure TsgcSTUNServer_Base.SetActive(const Value: Boolean);
begin
  UDPServer.Active := Value;
end;

procedure TsgcSTUNServer_Base.SetBindings(const Value: TIdSocketHandles);
begin
  UDPServer.Bindings := Value;
end;

procedure TsgcSTUNServer_Base.SetHost(const Value: string);
begin
  UDPServer.Host := Value;
end;

procedure TsgcSTUNServer_Base.SetIPVersion(const Value: TIdIPVersion);
begin
  UDPServer.IPVersion := Value;
end;

procedure TsgcSTUNServer_Base.SetLogFile(const Value: TsgcSTUNLogFile);
begin
  if Assigned(FLogFile) then
    FLogFile.Assign(Value);
end;

procedure TsgcSTUNServer_Base.SetNotifyEvents(const Value: TwsNotifyEvent);
begin
  TsgcUDPServer_Base_Hack(UDPServer).NotifyEvents := Value;
end;

procedure TsgcSTUNServer_Base.SetPort(const Value: Integer);
begin
  UDPServer.Port := Value;
end;

procedure TsgcSTUNServer_Base.SetSTUNOptions(const Value
  : TsgcSTUNServer_Options);
begin
  if Assigned(FSTUNOptions) then
    FSTUNOptions.Assign(Value);
end;

constructor TsgcSTUNServer_Authentication.Create;
begin
  inherited;
  FLongTermCredentials :=
    TsgcSTUNServer_Authentication_LongTermCredentials.Create;
  FShortTermCredentials :=
    TsgcSTUNServer_Authentication_ShortTermCredentials.Create;
  FEnabled := False;
end;

destructor TsgcSTUNServer_Authentication.Destroy;
begin
  sgcFree(FShortTermCredentials);
  sgcFree(FLongTermCredentials);
  inherited;
end;

procedure TsgcSTUNServer_Authentication.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSTUNServer_Authentication then
  begin
    LongTermCredentials := TsgcSTUNServer_Authentication(aSource)
      .LongTermCredentials;
    ShortTermCredentials := TsgcSTUNServer_Authentication(aSource)
      .ShortTermCredentials;
    Enabled := TsgcSTUNServer_Authentication(aSource).Enabled;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcSTUNServer_Authentication.SetLongTermCredentials
  (const Value: TsgcSTUNServer_Authentication_LongTermCredentials);
begin
  if Assigned(FLongTermCredentials) then
    FLongTermCredentials :=
      TsgcSTUNServer_Authentication_LongTermCredentials.Create;
  FLongTermCredentials := Value;
end;

procedure TsgcSTUNServer_Authentication.SetShortTermCredentials
  (const Value: TsgcSTUNServer_Authentication_ShortTermCredentials);
begin
  if Assigned(FShortTermCredentials) then
    FShortTermCredentials.Assign(Value);
end;

constructor TsgcSTUNServer_Authentication_LongTermCredentials.Create;
begin
  inherited;
  FEnabled := False;
  FRealm := CS_APPLICATION_NAME;
  FStaleNonce := 600;
end;

procedure TsgcSTUNServer_Authentication_LongTermCredentials.Assign
  (aSource: TPersistent);
begin
  if aSource is TsgcSTUNServer_Authentication_LongTermCredentials then
  begin
    StaleNonce := TsgcSTUNServer_Authentication_LongTermCredentials(aSource)
      .StaleNonce;
    Enabled := TsgcSTUNServer_Authentication_LongTermCredentials
      (aSource).Enabled;
    Realm := TsgcSTUNServer_Authentication_LongTermCredentials(aSource).Realm;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSTUNServer_Options.Create;
begin
  inherited;
  FAuthentication := TsgcSTUNServer_Authentication.Create;
  FBindingAttributes := TsgcSTUNServer_BindingAttributes_Options.Create;
end;

destructor TsgcSTUNServer_Options.Destroy;
begin
  sgcFree(FBindingAttributes);
  sgcFree(FAuthentication);
  inherited;
end;

procedure TsgcSTUNServer_Options.Assign(aSource: TPersistent);
begin
  inherited Assign(aSource);
  if aSource is TsgcSTUNServer_Options then
  begin
    Authentication := TsgcSTUNServer_Options(aSource).Authentication;
    BindingAttributes := TsgcSTUNServer_Options(aSource).BindingAttributes;
  end;
end;

procedure TsgcSTUNServer_Options.SetAuthentication
  (const Value: TsgcSTUNServer_Authentication);
begin
  if Assigned(FAuthentication) then
    FAuthentication.Assign(Value);
end;

procedure TsgcSTUNServer_Options.SetBindingAttributes
  (const Value: TsgcSTUNServer_BindingAttributes_Options);
begin
  if Assigned(FBindingAttributes) then
    FBindingAttributes.Assign(Value);
end;

function TsgcSTUNNoncesQueue.AddNonce(const aRealm: String): string;
var
  oItem: TsgcSTUNNonceItem;
begin
  Result := NewGuid;

  oItem := TsgcSTUNNonceItem.Create;
  oItem.ID := Result;
  oItem.Realm := aRealm;
  oItem.Expires := IncSecond(Now, StaleNonce);
  Add(oItem);
end;

function TsgcSTUNNoncesQueue.GetNonceState(const aNonce: String)
  : TsgcStunNonceState;
var
  oItem: TsgcQueueItemBase;
begin
  Result := stnsNonceUnknown;

  oItem := GetItem(aNonce);
  if Assigned(oItem) then
  begin
    Result := stnsNonceValid;
    if Now >= TsgcSTUNNonceItem(oItem).Expires then
    begin
      Result := stnsNonceStaled;
      DeleteItem(aNonce);
    end;
  end;
end;

constructor TsgcSTUNServer_Authentication_ShortTermCredentials.Create;
begin
  inherited;
  FEnabled := False;
end;

procedure TsgcSTUNServer_Authentication_ShortTermCredentials.Assign
  (aSource: TPersistent);
begin
  if aSource is TsgcSTUNServer_Authentication_ShortTermCredentials then
  begin
    Enabled := TsgcSTUNServer_Authentication_ShortTermCredentials
      (aSource).Enabled;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSTUNServer_BindingAttributes_Options.Create;
begin
  inherited;
  FAlternateServer := TsgcSTUNServer_AlternateServer.Create;
  FMappedAddressRFC3489 := True;
  FOtherAddress := True;
  FResponseOrigin := True;
  FSourceAddress := True;
end;

destructor TsgcSTUNServer_BindingAttributes_Options.Destroy;
begin
  sgcFree(FAlternateServer);
  inherited;
end;

procedure TsgcSTUNServer_BindingAttributes_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSTUNServer_BindingAttributes_Options then
  begin
    MappedAddressRFC3489 := TsgcSTUNServer_BindingAttributes_Options(aSource)
      .MappedAddressRFC3489;
    ResponseOrigin := TsgcSTUNServer_BindingAttributes_Options(aSource)
      .ResponseOrigin;
    SourceAddress := TsgcSTUNServer_BindingAttributes_Options(aSource)
      .SourceAddress;
    OtherAddress := TsgcSTUNServer_BindingAttributes_Options(aSource)
      .OtherAddress;
    AlternateServer := TsgcSTUNServer_BindingAttributes_Options(aSource)
      .AlternateServer;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcSTUNServer_BindingAttributes_Options.SetAlternateServer
  (const Value: TsgcSTUNServer_AlternateServer);
begin
  if Assigned(FAlternateServer) then
    FAlternateServer.Assign(Value);
end;

constructor TsgcSTUNServer_AlternateServer.Create;
begin
  inherited;
  FEnabled := False;
  FIPAddress := '';
  FPort := 3478;
  FFamily := ipV4;
  FDomain := '';
end;

procedure TsgcSTUNServer_AlternateServer.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSTUNServer_AlternateServer then
  begin
    Enabled := TsgcSTUNServer_AlternateServer(aSource).Enabled;
    IPAddress := TsgcSTUNServer_AlternateServer(aSource).IPAddress;
    Port := TsgcSTUNServer_AlternateServer(aSource).Port;
    Family := TsgcSTUNServer_AlternateServer(aSource).Family;
    Domain := TsgcSTUNServer_AlternateServer(aSource).Domain;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
