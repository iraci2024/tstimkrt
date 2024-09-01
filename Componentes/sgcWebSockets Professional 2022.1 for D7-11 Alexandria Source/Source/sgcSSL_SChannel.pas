{ ***************************************************************************
  sgcSSL component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcSSL_SChannel;

interface

{$I sgcVer.inc}
{$IFDEF SGC_SCHANNEL}
{$IFDEF D2010}
  {$POINTERMATH ON}
  {$DEFINE SGC_POINTERMATH}
{$ENDIF}
{$IFDEF Lazarus}
  {$POINTERMATH ON}
  {$DEFINE SGC_POINTERMATH}
{$ENDIF}

uses
  Classes, SysUtils, Windows, Winsock,
{$IFDEF DXE3}System.Types, {$ELSE}Types, {$ENDIF}
  // sgcSSL
  sgcSSL_WinSSPI,
  // websocket
  sgcWebSocket_Types;

type

  TSSLValidator = class
  public
    constructor Create;
    destructor Destroy; override;
    function ValidateChain(Chain: PCCERT_CHAIN_CONTEXT;
      var Status: CERT_CHAIN_POLICY_STATUS): Boolean; virtual;
    function ValidateElement(Element: PCERT_CHAIN_ELEMENT): Boolean; virtual;
  end;

  TsgcSChannelConnectionInfo = class
  private
    FCiperStrength: Cardinal;
    FCipher: String;
    FCipherId: Cardinal;
    FExchange: string;
    FExchangeId: Cardinal;
    FExchangeStrength: Cardinal;
    FHash: string;
    FHashId: Cardinal;
    FHashStrength: Cardinal;
    FProtocol: TwsTLSVersions;
  protected
    procedure SetProtocol(aValue: Cardinal); virtual;
    procedure SetCipher(aValue: Cardinal); virtual;
    procedure SetHash(aValue: Cardinal); virtual;
    procedure SetExchange(aValue: Cardinal); virtual;
  protected
    property HashId: Cardinal read FHashId write FHashId;
    property CipherId: Cardinal read FCipherId write FCipherId;
    property ExchangeId: Cardinal read FExchangeId write FExchangeId;
  public
    property CiperStrength: Cardinal read FCiperStrength write FCiperStrength;
    property Cipher: String read FCipher write FCipher;
    property Exchange: string read FExchange write FExchange;
    property ExchangeStrength: Cardinal read FExchangeStrength
      write FExchangeStrength;
    property Hash: string read FHash write FHash;
    property HashStrength: Cardinal read FHashStrength write FHashStrength;
    property Protocol: TwsTLSVersions read FProtocol write FProtocol;
  end;

  TsgcSChannelCertificate = class(TPersistent)
  private
    FCertFilePassword: string;
    FCertHash: String;
    FCertFile: String;
    FCertStoreName: TsgcSChannelCertStoreName;
    FCertStorePath: TsgcSChannelCertStorePath;
    FVerifyCertificate: Boolean;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property CertFilePassword: string read FCertFilePassword
      write FCertFilePassword;
    property CertHash: String read FCertHash write FCertHash;
    property CertFile: String read FCertFile write FCertFile;
    property CertStoreName: TsgcSChannelCertStoreName read FCertStoreName
      write FCertStoreName;
    property CertStorePath: TsgcSChannelCertStorePath read FCertStorePath
      write FCertStorePath;
    property VerifyCertificate: Boolean read FVerifyCertificate
      write FVerifyCertificate;
  end;

  TsgcSChannelStream = class
  private
    FData: PByte;
    FDataSize: Integer;
  protected
    procedure DoFinalize;
    procedure DoInitialize(aSize: Integer);
  public
    destructor Destroy; override;
  public
    procedure LoadFromStream(const aSource: TStream);
    procedure LoadFromFile(const aFileName: String);
  public
    property Data: PByte read FData;
    property DataSize: Integer read FDataSize;
  end;

  TCredentialsCallBack = procedure(SSL: THandle; UserData: Pointer);

var
  secur32: THandle;

function SSLAvailable: Boolean;
function SSLStart(aSocket: TSocket; aCertificate: TsgcSChannelCertificate;
  const Host: AnsiString = ''; aVersion: TwsTLSVersions = tls1_0;
  aALPNProtocols: TStrings = nil; aCipherList: String = ''): Integer;
procedure SSLCredentialsCallBack(SSL: THandle; CallBack: TCredentialsCallBack;
  UserData: Pointer);
function SSLPending(SSL: THandle): Boolean;
function SSLRead(SSL: THandle; var Data; Size: Integer): Integer;
function SSLWrite(SSL: THandle; var Data; Size: Integer): Integer;
function SSLClose(SSL: THandle): Integer;
function GetALPN(SSL: THandle): String;
function GetConnectionInfo(SSL: THandle): TsgcSChannelConnectionInfo;

var
  CertStatus: Cardinal;
  vMsgError: string;
  oError: HRESULT;

const
  CS_SChannel_Ciphers: Array [0 .. 49] of String = ('CALG_3DES',
    'CALG_3DES_112', 'CALG_AES', 'CALG_AES_128', 'CALG_AES_192', 'CALG_AES_256',
    'CALG_AGREEDKEY_ANY', 'CALG_CYLINK_MEK', 'CALG_DES', 'CALG_DESX',
    'CALG_DH_EPHEM', 'CALG_DH_SF', 'CALG_DSS_SIGN', 'CALG_ECDH',
    'CALG_ECDH_EPHEM', 'CALG_ECDSA', 'CALG_ECMQV', 'CALG_HASH_REPLACE_OWF',
    'CALG_HUGHES_MD5', 'CALG_HMAC', 'CALG_KEA_KEYX', 'CALG_MAC', 'CALG_MD2',
    'CALG_MD4', 'CALG_MD5', 'CALG_NO_SIGN', 'CALG_OID_INFO_CNG_ONLY',
    'CALG_OID_INFO_PARAMETERS', 'CALG_PCT1_MASTER', 'CALG_RC2', 'CALG_RC4',
    'CALG_RC5', 'CALG_RSA_KEYX', 'CALG_RSA_SIGN', 'CALG_SCHANNEL_ENC_KEY',
    'CALG_SCHANNEL_MAC_KEY', 'CALG_SCHANNEL_MASTER_HASH', 'CALG_SEAL',
    'CALG_SHA', 'CALG_SHA1', 'CALG_SHA_256', 'CALG_SHA_384', 'CALG_SHA_512',
    'CALG_SKIPJACK', 'CALG_SSL2_MASTER', 'CALG_SSL3_MASTER', 'CALG_SSL3_SHAMD5',
    'CALG_TEK', 'CALG_TLS1_MASTER', 'CALG_TLS1PRF');

  CS_SChannel_CiphersId: Array [0 .. 49] of Cardinal = ($00006603, $00006609,
    $00006611, $0000660E, $0000660F, $00006610, $0000AA03, $0000660C, $00006601,
    $00006604, $0000AA02, $0000AA01, $00002200, $0000AA05, $0000AE06, $00002203,
    $0000A001, $0000800B, $0000A003, $00008009, $0000AA04, $00008005, $00008001,
    $00008002, $00008003, $00002000, $FFFFFFFF, $FFFFFFFE, $00004C04, $00006602,
    $00006801, $0000660D, $0000A400, $00002400, $00004C07, $00004C03, $00004C02,
    $00006802, $00008004, $00008004, $0000800C, $0000800D, $0000800E, $0000660A,
    $00004C05, $00004C01, $00008008, $0000660B, $00004C06, $0000800A);

const
  CS_SChannel_ERROR_INTERRUPTED_SYSTEM_CALL = 10004;

{$ENDIF}

implementation

{$IFDEF SGC_SCHANNEL}

uses
  sgcBase_Helpers, sgcWebSocket_Helpers;

const
  CS_IO_BUFFER_SIZE = $10000;

{$IFNDEF SGC_POINTERMATH}
type
  TCertArray = array of CERT_SIMPLE_CHAIN;
  TCertChainArray = array of PCERT_CHAIN_ELEMENT;
{$ENDIF}

{$IFNDEF D10_4}

type
  CRYPT_HASH_BLOB = record
    cbData: DWORD;
    pbData: PByte;
  end;
{$ENDIF}

var
  oValidators: TList;
  vInitialized: Boolean;
  oSSPI: PSecurityFunctionTable;
  oMyStore: HCERTSTORE;

function GetCertStoreName(aValue: TsgcSChannelCertStoreName): string;
begin
  result := '';
  case aValue of
    scsnMY:
      result := 'MY';
    scsnRoot:
      result := 'Root';
    scsnTrust:
      result := 'Trust';
    scsnCA:
      result := 'CA';
  end;

end;

function GetCertStorePath(aValue: TsgcSChannelCertStorePath): DWORD;
begin
  result := 0;
  case aValue of
    scspStoreCurrentUser:
      result := CERT_SYSTEM_STORE_CURRENT_USER;
    scspStoreLocalMachine:
      result := CERT_SYSTEM_STORE_LOCAL_MACHINE;
  end;
end;

procedure FreeValidators;
var
  i: Integer;
begin
  if oValidators = nil then
    Exit;
  for i := oValidators.Count - 1 downto 0 do
    TSSLValidator(oValidators[i]).Free;
  oValidators.Free;
end;

function Validate(Chain: PCCERT_CHAIN_CONTEXT;
  var Status: CERT_CHAIN_POLICY_STATUS): Boolean;
var
  i: Integer;
begin
  result := True;
  if oValidators <> nil then
  begin
    for i := oValidators.Count - 1 downto 0 do
    begin
      if TSSLValidator(oValidators[i]).ValidateChain(Chain, Status) then
        Exit;
    end;
  end;
  result := False;
end;

{ TSSLValidator }

constructor TSSLValidator.Create;
begin
  inherited;
  if oValidators = nil then
    oValidators := TList.Create;
  oValidators.Add(Self);
end;

destructor TSSLValidator.Destroy;
begin
  oValidators.Remove(Self);
  inherited;
end;

function TSSLValidator.ValidateChain(Chain: PCCERT_CHAIN_CONTEXT;
  var Status: CERT_CHAIN_POLICY_STATUS): Boolean;
begin
  result := False;
  if Status.dwError <> CERT_E_UNTRUSTEDROOT then
    Exit;
  if (Status.lChainIndex < 0) or (DWORD(Status.lChainIndex) > Chain.cChain) then
    Exit;
  {$IFDEF SGC_POINTERMATH}
  if (Status.lElementIndex < 0) or
    (DWORD(Status.lElementIndex) > Chain.rgpChain[Status.lChainIndex].cElement)
  then
    Exit;
  result := ValidateElement(Chain.rgpChain[Status.lChainIndex].rgpElement
    [Status.lElementIndex]);
  {$ELSE}
  if (Status.lElementIndex < 0) or
    (DWORD(Status.lElementIndex) > TCertArray(Chain.rgpChain^)[Status.lChainIndex].cElement)
  then
    Exit;
  result := ValidateElement(TCertChainArray(TCertArray(Chain.rgpChain^)[Status.lChainIndex].rgpElement^)[Status.lElementIndex]);
  {$ENDIF}
end;

function TSSLValidator.ValidateElement(Element: PCERT_CHAIN_ELEMENT): Boolean;
begin
  result := False;
end;

function SendData(Socket: Integer; Data: PByte; Size: Integer): Integer;
var
  i: Integer;
begin
  result := 0;
  while Size > 0 do
  begin
    {$IFDEF SGC_POINTERMATH}
    i := send(Socket, Data[result], Size, 0);
    {$ELSE}
    i := send(Socket, PByteArray(Data)[result], Size, 0);
    {$ENDIF}
    if i <= 0 then
    begin
      oError := 0;
      vMsgError := 'send returns ' + IntToStr(WSAGetLastError);
      result := i;
      Exit;
    end;
    Inc(result, i);
    Dec(Size, result);
  end;
end;

function SendSecBuffer(Socket: Integer; var Buffer: TSecBuffer): Boolean;
begin
  result := True;
  if (Buffer.cbBuffer > 0) and (Buffer.pvBuffer <> nil) then
  begin
    if SendData(Socket, PByte(Buffer.pvBuffer), Buffer.cbBuffer) <= 0 then
    begin
      result := False;
      Exit;
    end;
    oSSPI.FreeContextBuffer(Buffer.pvBuffer);
    Buffer.pvBuffer := nil;
  end;
end;

type
  TSSLInit = set of (iCredentials, iContext);

  TSSLInfo = {$IFDEF D7}class{$ELSE}record{$ENDIF}
    Init: TSSLInit;
    Error: Cardinal;
    // Credentials
    CredentialsCallBack: TCredentialsCallBack;
    UserData: Pointer;
    // Connected socket
    Socket: Integer;
    // Remote server name
    Servername: string;
    // SChanel credentials
    SChannel: SCHANNEL_CRED;
    // Credentials
    Credentials: TCredHandle;
    // TLS Context
    Context: TCtxtHandle;
    // Data received from the socket (crypted)
    RecvBuffer: array of AnsiChar;
    RecvCount: Integer;
    // Data decrypted
    DataBuffer: array of AnsiChar;
    DataCount: Integer;
    DataStart: Integer;
    // Output buffer
    BuffSizes: TSecPkgContextStreamSizes;
    SendBuffer: array of Byte;
    // version
    Version: TwsTLSVersions;
    // alpn buffer
    ALPNProtocols: TStrings;
    alpn_result: TSecPkgContext_ApplicationProtocol;
    // certificate
    CertVerify: Boolean;
    CertHash: string;
    CertFile: string;
    CertFilePassword: string;
    CertStorePath: TsgcSChannelCertStorePath;
    CertStoreName: TsgcSChannelCertStoreName;
    // cipher list
    CipherList: string;
    // info
    ConnectionInfo: TsgcSChannelConnectionInfo;
    procedure Clean;
    function GetCipherId(const aCipher: string): Cardinal;
    function Read: Integer;
    function ReadLoop: Integer;
    function InitBuffer: Boolean;
    function DoGetCertificateFromChainStore: Boolean;
    function DoGetCertificate: Boolean;
    function DoImportCertPFX(const aFileName, aPassword: String): Boolean;
    function DoVerifyCertificate: Boolean;
    function Readable: Integer;
    function Decrypt(var Data; Size: Integer): Integer;
    function Encrypt(var Data; Size: Integer): Integer;
    function GetALPN: String;
    function GetConnectionInfo: TsgcSChannelConnectionInfo;
  private
    function Start: Boolean;
  end;

  PSSLInfo = {$IFDEF D7}TSSLInfo{$ELSE}^TSSLInfo{$ENDIF};

function TSSLInfo.Start: Boolean;
var
  WHost: string;
  inBuffers: array [0 .. 0] of TSecBuffer;
  inBuffer: TSecBufferDesc;
  OutBuffers: array [0 .. 0] of TSecBuffer;
  OutBuffer: TSecBufferDesc;
  Flags: DWORD;
  Error: Cardinal;
  i: Integer;
  ALPNBuffer: Array [0 .. 127] of Byte;
  vOffSet: Integer;
  vLength, vLengthTotal: Integer;
  oCiphers: TStringList;
  vCipherId: Cardinal;
  oAlgs: Array of ALG_ID;
begin
  result := False;

  if Assigned(CredentialsCallBack) then
  begin
    CertCloseStore(oMyStore, 0);
    CredentialsCallBack(Integer(@Self), UserData);
    oMyStore := CertOpenSystemStore(0, 'MY');
    if oMyStore = 0 then
    begin
      oError := 0;
      vMsgError := 'CertOpenSystemStore(0, ''MY'') returns 0';
      Exit;
    end;
  end;

  FillChar(SChannel, SizeOf(SChannel), 0);
  SChannel.dwVersion := SCHANNEL_CRED_VERSION;
  case Version of
    tls1_0:
      SChannel.grbitEnabledProtocols := SP_PROT_SSL3TLS1;
    tls1_1:
      SChannel.grbitEnabledProtocols := SP_PROT_TLS1_1;
    tls1_2:
      SChannel.grbitEnabledProtocols := SP_PROT_TLS1_2;
    tls1_3:
      SChannel.grbitEnabledProtocols := SP_PROT_TLS1_3;
    tlsUndefined:
      SChannel.grbitEnabledProtocols := SP_PROT_SSL3TLS1 or SP_PROT_TLS1_1 or
        SP_PROT_TLS1_2 or SP_PROT_TLS1_3;
  end;
  SChannel.dwFlags := SCH_CRED_NO_DEFAULT_CREDS or
    SCH_CRED_MANUAL_CRED_VALIDATION;

  // cipher list
  SChannel.palgSupportedAlgs := nil;
  SChannel.cSupportedAlgs := 0;
  if CipherList <> '' then
  begin
    oCiphers := TStringList.Create;
    Try
      oCiphers.Delimiter := ':';
      oCiphers.DelimitedText := CipherList;
      for i := 0 to oCiphers.Count - 1 do
      begin
        vCipherId := GetCipherId(oCiphers[i]);
        if vCipherId > 0 then
        begin
          SetLength(oAlgs, Length(oAlgs) + 1);
          oAlgs[Length(oAlgs) - 1] := vCipherId;
        end;
      end;

      if Length(oAlgs) > 0 then
      begin
        SChannel.palgSupportedAlgs := @oAlgs[0];
        SChannel.cSupportedAlgs := Length(oAlgs);
      end;
    Finally
      sgcFree(oCiphers);
    End;
  end;

  Error := oSSPI.AcquireCredentialsHandle(nil, UNISP_NAME, SECPKG_CRED_OUTBOUND,
    nil, @SChannel, nil, nil, @Credentials, nil);
  if Error <> SEC_E_OK then
  begin
    oError := Error;
    vMsgError := 'AcquireCredentialsHandle returns ' + IntToHex(Error, 8);
    Exit;
  end;
  Init := [iCredentials];

  // ... alpn
  if Assigned(ALPNProtocols) and (ALPNProtocols.Count > 0) then
  begin
    vOffSet := 10;
    vLengthTotal := 0;
    FillChar(ALPNBuffer, SizeOf(ALPNBuffer), 0);
    if ALPNProtocols.Count > 0 then
    begin
      for i := 0 to ALPNProtocols.Count - 1 do
      begin
        vLength := Length(ALPNProtocols[i]);
        if vLength > 0 then
        begin
          ALPNBuffer[vOffSet] := vLength;
          vOffSet := vOffSet + 1;
{$IFDEF MSWINDOWS}
          sgcMove(AnsiString(ALPNProtocols[i])[1], ALPNBuffer[vOffSet],
            vLength);
{$ELSE}
          sgcMove((GetALPNProtocols[i])[1], VBuffer[vOffSet], vLength);
{$ENDIF}
          vOffSet := vOffSet + vLength;
          vLengthTotal := vLengthTotal + vLength + 1;
        end;
      end;
      // ...  The first four bytes will be an unsigned int indicating number
      // ...  of bytes of data in the rest of the buffer.
      ALPNBuffer[0] := vLengthTotal + 4 + 2;
      // ... The next four bytes are an indicator that this buffer will contain
      // ... ALPN data, as opposed to NPN, for example.
      ALPNBuffer[4] := 2; // SecApplicationProtocolNegotiationExt_ALPN
      // ... The next two bytes will be an unsigned short indicating the number
      // ... of bytes used to list the preferred protocols.
      ALPNBuffer[8] := vLengthTotal;
    end;

    inBuffer.ulVersion := SECBUFFER_VERSION;
    inBuffer.cBuffers := 1;
    inBuffer.pBuffers := Addr(inBuffers[0]);

    inBuffers[0].cbBuffer := vOffSet;
    inBuffers[0].BufferType := SECBUFFER_APPLICATION_PROTOCOLS;
    inBuffers[0].pvBuffer := @ALPNBuffer;
  end
  else
    FillChar(inBuffer, SizeOf(inBuffer), 0);

  // ... outbuffer
  OutBuffer.ulVersion := SECBUFFER_VERSION;
  OutBuffer.cBuffers := 1;
  OutBuffer.pBuffers := Addr(OutBuffers[0]);

  OutBuffers[0].cbBuffer := 0;
  OutBuffers[0].BufferType := SECBUFFER_TOKEN;
  OutBuffers[0].pvBuffer := nil;

  WHost := Servername;

  Error := oSSPI.InitializeSecurityContext(@Credentials, nil, PWideChar(WideString(WHost)),
    ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or
    ISC_REQ_CONFIDENTIALITY or ISC_RET_EXTENDED_ERROR or
    ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM, 0, SECURITY_NATIVE_DREP,
    @inBuffer, 0, @Context, @OutBuffer, Flags, nil);

  if (Error <> SEC_I_CONTINUE_NEEDED) then
  begin
    oError := Error;
    vMsgError := 'First call to InitializeSecurityContext returns ' +
      IntToHex(Error, 8);
    Exit;
  end;
  Init := Init + [iContext];

  if not SendSecBuffer(Socket, OutBuffers[0]) then
  begin
    Exit;
  end;

  SetLength(RecvBuffer, CS_IO_BUFFER_SIZE);
  RecvCount := 0;

  if ReadLoop < 0 then
    Exit;

  if CertVerify then
  begin
    if DoVerifyCertificate = False then
      Exit;
  end;

  if InitBuffer = False then
    Exit;

  result := True;
end;

procedure TSSLInfo.Clean;
begin
  if iCredentials in Init then
  begin
    oSSPI.FreeCredentialsHandle(@Credentials);
  end;
  if iContext in Init then
  begin
    if oError = HRESULT(SEC_E_ILLEGAL_MESSAGE) then
      oSSPI.DeleteSecurityContext(@Context);
  end;
  Init := [];
  sgcFree(ConnectionInfo);
end;

function TSSLInfo.Read: Integer;
begin
  result := recv(Socket, RecvBuffer[RecvCount], Length(RecvBuffer) -
    RecvCount, 0);
  if result > 0 then
    Inc(RecvCount, result)
  else
  begin
    Error := WSAGetLastError;
    // don't raise interrupted system call
    if Error = CS_SChannel_ERROR_INTERRUPTED_SYSTEM_CALL then
      result := CS_SChannel_ERROR_INTERRUPTED_SYSTEM_CALL * (-1);
    oError := 0;
    vMsgError := 'recv returns ' + IntToHex(Error, 8);
  end;
end;

function TSSLInfo.Readable: Integer;
var
  vBuffers: array [0 .. 3] of TSecBuffer;
  VBuffer: TSecBufferDesc;
  i: Integer;
begin
  result := DataCount - DataStart;

  if result = 0 then
  begin
    DataCount := 0;
    DataStart := 0;

    repeat
      Error := 0;
      repeat
        if (RecvCount = 0) or (Error = SEC_E_INCOMPLETE_MESSAGE) then
        begin
          result := Read();
          if result <= 0 then
            Exit;
        end;
        FillChar(vBuffers, SizeOf(vBuffers), 0);
        vBuffers[0].cbBuffer := RecvCount;
        vBuffers[0].BufferType := SECBUFFER_DATA;
        vBuffers[0].pvBuffer := @RecvBuffer[0];

        VBuffer.ulVersion := SECBUFFER_VERSION;
        VBuffer.cBuffers := 4;
        VBuffer.pBuffers := Addr(vBuffers[0]);

        Error := oSSPI.DecryptMessage(@Context, @VBuffer, 0, nil);

      until Error <> SEC_E_INCOMPLETE_MESSAGE;

      if Error = SEC_I_CONTEXT_EXPIRED then
      begin
        oError := Error;
        vMsgError := 'DecryptMessage returns SEC_I_CONTEXT_EXPIRED';
        result := -1;
        Exit;
      end;

      if Error = SEC_E_DECRYPT_FAILURE then
      begin
        vMsgError := 'DecryptMessage returns SEC_E_DECRYPT_FAILURE';
        result := -1;
        Exit;
      end;

      RecvCount := 0;
      for i := 1 to 3 do
      begin
        case vBuffers[i].BufferType of
          SECBUFFER_DATA:
            begin
              if DataCount + Integer(vBuffers[i].cbBuffer) > Length(DataBuffer)
              then
                SetLength(DataBuffer,
                  DataCount + Integer(vBuffers[i].cbBuffer));
              sgcMove(vBuffers[i].pvBuffer^, DataBuffer[DataCount],
                vBuffers[i].cbBuffer);
              Inc(DataCount, vBuffers[i].cbBuffer);
            end;
          SECBUFFER_EXTRA:
            begin
              Assert(Integer(vBuffers[i].cbBuffer) <= Length(RecvBuffer));
              RecvCount := vBuffers[i].cbBuffer;
              sgcMove(vBuffers[i].pvBuffer^, RecvBuffer[0], RecvCount);
            end;
        end;
      end;

      if Error = SEC_I_RENEGOTIATE then
      begin
        if ReadLoop <= 0 then
        begin
          result := -1;
          Exit;
        end;
      end;

      if Error <> SEC_E_OK then
      begin
        result := -1;
        Exit;
      end;

    until DataCount > 0;

    result := DataCount;
  end;
end;

function TSSLInfo.ReadLoop: Integer;
var
  vInBuffers: array [0 .. 1] of TSecBuffer;
  vInBuffer: TSecBufferDesc;
  vOutBuffers: array [0 .. 0] of TSecBuffer;
  vOutBuffer: TSecBufferDesc;
  vRead: Boolean;
  vFlags: DWORD;
  vError: Cardinal;
  vSource: PByte;
begin
  vInBuffer.ulVersion := SECBUFFER_VERSION;
  vInBuffer.cBuffers := 2;
  vInBuffer.pBuffers := Addr(vInBuffers[0]);

  vOutBuffer.ulVersion := SECBUFFER_VERSION;
  vOutBuffer.cBuffers := 1;
  vOutBuffer.pBuffers := Addr(vOutBuffers[0]);

  vRead := True;
  repeat
    if vRead then
    begin
      vRead := False;
      result := Read;
      if result <= 0 then
        Exit;
    end;

    vInBuffers[0].cbBuffer := RecvCount;
    vInBuffers[0].BufferType := SECBUFFER_TOKEN;
    vInBuffers[0].pvBuffer := @RecvBuffer[0];

    vInBuffers[1].cbBuffer := 0;
    vInBuffers[1].BufferType := SECBUFFER_EMPTY;
    vInBuffers[1].pvBuffer := nil;

    vOutBuffers[0].cbBuffer := 0;
    vOutBuffers[0].BufferType := SECBUFFER_TOKEN;
    vOutBuffers[0].pvBuffer := nil;

    vError := oSSPI.InitializeSecurityContext(@Credentials, @Context, nil,
      ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or
      ISC_REQ_CONFIDENTIALITY or ISC_RET_EXTENDED_ERROR or
      ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM, 0, SECURITY_NATIVE_DREP,
      @vInBuffer, 0, nil, @vOutBuffer, vFlags, nil);

    if not SendSecBuffer(Socket, vOutBuffers[0]) then
    begin
      result := -1;
      Exit;
    end;

    if (vInBuffers[1].cbBuffer > 0) and
      (vInBuffers[1].BufferType = SECBUFFER_EXTRA) then
    begin
      vSource := @RecvBuffer[RecvCount - Integer(vInBuffers[1].cbBuffer)];
      RecvCount := vInBuffers[1].cbBuffer;
      sgcMove(vSource^, RecvBuffer[0], RecvCount);
    end
    else
    begin
      if vError <> SEC_E_INCOMPLETE_MESSAGE then
        RecvCount := 0;
    end;

    if (vError = SEC_E_INCOMPLETE_MESSAGE) or (vError = SEC_I_CONTINUE_NEEDED)
      or (vError = SEC_E_INCOMPLETE_MESSAGE) then
    begin
      vRead := True;
      Continue;
    end;

    if vError = SEC_I_INCOMPLETE_CREDENTIALS then
    begin
      if CertHash <> '' then
      begin
        if not DoGetCertificate then
        begin
          result := -1;
          Exit;
        end;
      end
      else if CertFile <> '' then
      begin
        if not DoImportCertPFX(CertFile, CertFilePassword) then
        begin
          result := -1;
          Exit;
        end;
      end
      else
        // ... try to get certificate from chain store
        // ... if not found try to continue
        DoGetCertificateFromChainStore;
      Assert(vRead = False);
      Continue;
    end;

    if FAILED(vError) then
    begin
      oError := Error;
      vMsgError := 'InitializeSecurityContext returns ' + IntToHex(vError, 8);
      result := -1;
      Exit;
    end;

  until vError = SEC_E_OK;

  result := 0;
end;

function TSSLInfo.DoGetCertificateFromChainStore: Boolean;
var
  vError: Integer;
  oIssuer: TSecPkgContextIssuerListInfoEx;
  vChainPara: CERT_CHAIN_FIND_BY_ISSUER_PARA;
  vChainCtxt: PCCERT_CHAIN_CONTEXT;
  vChainIndex: DWORD;
  vElementIndex: DWORD;
  pCertContext: PCCERT_CONTEXT;
  vCreds: TCredHandle;
begin
  if Assigned(CredentialsCallBack) then
    CredentialsCallBack(Integer(@Self), UserData);

  FillChar(oIssuer, SizeOf(oIssuer), 0);
  vError := oSSPI.QueryContextAttributes(@Context, SECPKG_ATTR_ISSUER_LIST_EX,
    @oIssuer);
  if vError <> SEC_E_OK then
  begin
    oError := Error;
    vMsgError := 'QueryContextAttributes(SECPKG_ATTR_ISSUER_LIST_EX) returns ' +
      IntToHex(vError, 8);
    result := False;
    Exit;
  end;
  FillChar(vChainPara, SizeOf(vChainPara), 0);
  vChainPara.cbSize := SizeOf(vChainPara);
  vChainPara.pszUsageIdentifier := szOID_PKIX_KP_CLIENT_AUTH;
  vChainPara.cIssuer := oIssuer.cIssuers;
  vChainPara.rgIssuer := oIssuer.aIssuers;
  vChainPara.pdwIssuerChainIndex := @vChainIndex;
  vChainPara.pdwIssuerElementIndex := @vElementIndex;
  vChainIndex := 0;
  vElementIndex := 0;

  vChainCtxt := nil;
  repeat
    vChainCtxt := CertFindChainInStore(oMyStore, X509_ASN_ENCODING, 0,
      CERT_CHAIN_FIND_BY_ISSUER, @vChainPara, vChainCtxt);
    if vChainCtxt = nil then
    begin
      oError := 0;
      vMsgError := 'CertFindChainInStore returns nil';
      result := False;
      Exit;
    end;

    {$IFDEF SGC_POINTERMATH}
    pCertContext := vChainCtxt.rgpChain[0].rgpElement[0].pCertContext;
    {$ELSE}
    pCertContext := TCertChainArray(TCertArray(vChainCtxt.rgpChain^)[0].rgpElement^)[0].pCertContext;
    {$ENDIF}
    SChannel.dwVersion := SCHANNEL_CRED_VERSION;
    SChannel.cCreds := 1;
    SChannel.paCred := @pCertContext;

    vError := oSSPI.AcquireCredentialsHandle(nil, UNISP_NAME,
      SECPKG_CRED_OUTBOUND, nil, @SChannel, nil, nil, @vCreds, nil);
  until vError = SEC_E_OK;
  oSSPI.FreeCredentialsHandle(@Credentials);
  Credentials := vCreds;
  result := True;
end;

function TSSLInfo.DoVerifyCertificate: Boolean;
const
  USAGES: array [0 .. 2] of PAnsiChar = (szOID_PKIX_KP_SERVER_AUTH,
    szOID_SERVER_GATED_CRYPTO, szOID_SGC_NETSCAPE);
var
  vError: Integer;
  pServer: PCCERT_CONTEXT;
  vChainPara: CERT_CHAIN_PARA;
  vChain: PCCERT_CHAIN_CONTEXT;
  vHTTPS: HTTPSPolicyCallbackData;
  vPolicy: CERT_CHAIN_POLICY_PARA;
  vStatus: CERT_CHAIN_POLICY_STATUS;
begin
  pServer := nil;
  vError := oSSPI.QueryContextAttributes(@Context,
    SECPKG_ATTR_REMOTE_CERT_CONTEXT, @pServer);
  if vError <> 0 then
  begin
    oError := Error;
    vMsgError := 'QueryCredentialsAttributes returns ' + IntToHex(vError, 8);
    result := False;
    Exit;
  end;

  FillChar(vChainPara, SizeOf(vChainPara), 0);
  vChainPara.cbSize := SizeOf(vChainPara);
  vChainPara.RequestedUsage.dwType := USAGE_MATCH_TYPE_OR;
  vChainPara.RequestedUsage.Usage.cUsageIdentifier := Length(USAGES);
  vChainPara.RequestedUsage.Usage.rgpszUsageIdentifier := PAnsiChar(@USAGES);

  result := CertGetCertificateChain(0, pServer, nil, pServer.HCERTSTORE,
    vChainPara, 0, nil, vChain);

  if result then
  begin

    FillChar(vHTTPS, SizeOf(vHTTPS), 0);
    vHTTPS.cbSize := SizeOf(vHTTPS);
    vHTTPS.dwAuthType := AUTHTYPE_SERVER;
    vHTTPS.fdwChecks := 0;
    vHTTPS.pwszServerName := PWideChar(WideString(Servername));

    FillChar(vPolicy, SizeOf(vPolicy), 0);
    vPolicy.cbSize := SizeOf(vPolicy);
    vPolicy.pvExtraPolicyPara := @vHTTPS;

    FillChar(vStatus, SizeOf(vStatus), 0);
    vStatus.cbSize := SizeOf(vStatus);

    result := CertVerifyCertificateChainPolicy(CERT_CHAIN_POLICY_SSL, vChain,
      vPolicy, vStatus);

    if result then
    begin
      CertStatus := vStatus.dwError;
      if vStatus.dwError = CERT_E_UNTRUSTEDROOT then
        result := Validate(vChain, vStatus)
      else
        result := vStatus.dwError = 0;
      if result = False then
      begin
        oError := vStatus.dwError;
        vMsgError := 'CertVerifyCertificateChainPolicy.vStatus = ' +
          IntToHex(vStatus.dwError, 8);
      end;
    end;

    CertFreeCertificateChain(vChain);
  end;

  CertFreeCertificateContext(pServer);
end;

function TSSLInfo.InitBuffer: Boolean;
var
  vError: Integer;
begin
  vError := oSSPI.QueryContextAttributes(@Context, SECPKG_ATTR_STREAM_SIZES,
    @BuffSizes);
  if vError <> SEC_E_OK then
  begin
    oError := Error;
    vMsgError := 'QueryContextAttributes(SECPKG_ATTR_STREAM_SIZES) returns ' +
      IntToHex(vError, 8);
    result := False;
    Exit;
  end;
  SetLength(RecvBuffer, BuffSizes.cbHeader + BuffSizes.cbMaximumMessage +
    BuffSizes.cbTrailer);
  SetLength(SendBuffer, BuffSizes.cbHeader + BuffSizes.cbMaximumMessage +
    BuffSizes.cbTrailer);
  // RecvCount := 0; // don't initalize to zero if there is still data to decrypt
  DataCount := 0;
  DataStart := 0;
  result := True;
end;

function TSSLInfo.Decrypt(var Data; Size: Integer): Integer;
begin
  result := Readable;
  if result > 0 then
  begin
    if result > Size then
      result := Size;
    sgcMove(DataBuffer[DataStart], Data, result);
    Inc(DataStart, result);
  end;
end;

function TSSLInfo.DoGetCertificate: Boolean;
var
  oCertContext: Array [0 .. 0] of PCCERT_CONTEXT;
  oBytes: Array [0 .. 19] of Byte;
  oHash: CRYPT_HASH_BLOB;
  vError: Integer;
  vCreds: TCredHandle;
begin
  result := False;
  if CertHash <> '' then
  begin
    oMyStore := CertOpenStore(CERT_STORE_PROV_SYSTEM, X509_ASN_ENCODING or
      PKCS_7_ASN_ENCODING, 0, CERT_STORE_OPEN_EXISTING_FLAG or
      GetCertStorePath(CertStorePath),
      PWideChar(WideString(GetCertStoreName(CertStoreName))));
    Try
      if oMyStore = 0 then
      begin
        vMsgError := 'Cannot Open Certificate Store.';
        Exit;
      end;

      if Length(CertHash) <> CERT_THUMBPRINT_DATA_LEN * 2 then
      begin
        vMsgError := Format('Invalid Certificate Hash Length, must be %d.',
          [CERT_THUMBPRINT_DATA_LEN * 2]);
        Exit;
      end;

      FillChar(oHash, SizeOf(oHash), 0);
      oHash.pbData := @oBytes[0];
      oHash.cbData := CERT_THUMBPRINT_DATA_LEN;
      if not CryptStringToBinary(PWideChar(WideString(CertHash)), Length(CertHash),
        CRYPT_STRING_HEX, @oBytes[0], oHash.cbData, nil, nil) then
      begin
        vMsgError := 'Error calling method CryptStringToBinary.';
        Exit;
      end;

      oCertContext[0] := CertFindCertificateInStore(oMyStore,
        X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, 0, CERT_FIND_HASH,
        @oHash, nil);

      if not Assigned(oCertContext[0]) then
      begin
        vMsgError := Format('Certificate not found in Store %s with Hash %s',
          [GetCertStoreName(CertStoreName), CertHash]);
        Exit;
      end;

      SChannel.dwVersion := SCHANNEL_CRED_VERSION;
      SChannel.paCred := @oCertContext;
      SChannel.cCreds := 1;

      vError := oSSPI.AcquireCredentialsHandle(nil, UNISP_NAME,
        SECPKG_CRED_OUTBOUND, nil, @SChannel, nil, nil, @vCreds, nil);

      if vError = SEC_E_OK then
      begin
        oSSPI.FreeCredentialsHandle(@Credentials);
        Credentials := vCreds;
      end;

      result := vError = SEC_E_OK;
    Finally
      CertCloseStore(oMyStore, 0);
    End;
  end;
end;

function TSSLInfo.Encrypt(var Data; Size: Integer): Integer;
var
  vSource: PByte;
  i: Integer;
  vBuffers: array [0 .. 3] of TSecBuffer;
  VBuffer: TSecBufferDesc;
begin
  result := 0;
  vSource := @Data;
  while Size > 0 do
  begin
    if Cardinal(Size) > BuffSizes.cbMaximumMessage then
      i := BuffSizes.cbMaximumMessage
    else
      i := Size;
    {$IFDEF SGC_POINTERMATH}
    sgcMove(vSource[result], SendBuffer[BuffSizes.cbHeader], i);
    {$ELSE}
    sgcMove(PByteArray(vSource)[result], SendBuffer[BuffSizes.cbHeader], i);
    {$ENDIF}
    Inc(result, i);
    Dec(Size, i);

    vBuffers[0].cbBuffer := BuffSizes.cbHeader;
    vBuffers[0].BufferType := SECBUFFER_STREAM_HEADER;
    vBuffers[0].pvBuffer := @SendBuffer[0];

    vBuffers[1].cbBuffer := i;
    vBuffers[1].BufferType := SECBUFFER_DATA;
    vBuffers[1].pvBuffer := @SendBuffer[BuffSizes.cbHeader];

    vBuffers[2].cbBuffer := BuffSizes.cbTrailer;
    vBuffers[2].BufferType := SECBUFFER_STREAM_TRAILER;
    vBuffers[2].pvBuffer := @SendBuffer[Integer(BuffSizes.cbHeader) + i];

    vBuffers[3].BufferType := SECBUFFER_EMPTY;

    VBuffer.ulVersion := SECBUFFER_VERSION;
    VBuffer.cBuffers := 4;
    VBuffer.pBuffers := Addr(vBuffers[0]);

    Error := oSSPI.EncryptMessage(@Context, 0, @VBuffer, 0);
    if FAILED(Error) then
    begin
      oError := Error;
      vMsgError := 'EncryptMessage returns ' + IntToHex(Error, 8);
      result := -1;
      Exit;
    end;

    i := SendData(Socket, PByte(SendBuffer), vBuffers[0].cbBuffer + vBuffers[1]
      .cbBuffer + vBuffers[2].cbBuffer);
    if i <= 0 then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TSSLInfo.GetALPN: String;
var
  i: Integer;
  vArray: {$IFDEF D7} Array of Byte{$ELSE}TBytes{$ENDIF};
begin
  result := '';
  FillChar(alpn_result, SizeOf(alpn_result), 0);
  Error := oSSPI.QueryContextAttributes(@Context,
    SECPKG_ATTR_APPLICATION_PROTOCOL, @alpn_result);
  if Error <> SEC_E_OK then
  begin
    vMsgError :=
      'QueryContextAttributes(SECPKG_ATTR_APPLICATION_PROTOCOL) returns ' +
      IntToHex(Error, 8);
  end
  else
  begin
    if alpn_result.ProtoNegoStatus = Ord
      (SecApplicationProtocolNegotiationStatus_Success) then
    begin
      if alpn_result.ProtoNegoExt = Ord
        (SecApplicationProtocolNegotiationExt_ALPN) then
      begin
        SetLength(vArray, alpn_result.ProtocolIdSize);
        for i := 0 to alpn_result.ProtocolIdSize - 1 do
          vArray[i] := alpn_result.ProtocolId[i];
{$IFDEF DXE}
        result := TEncoding.UTF8.GetString(vArray);
{$ELSE}
        SetString(result, PAnsiChar(@vArray[0]), Length(vArray));
{$ENDIF}
      end;
    end;
  end;
end;

function TSSLInfo.DoImportCertPFX(const aFileName, aPassword: String): Boolean;
var
  oPFX: CRYPT_DATA_BLOB;
  oStream: TsgcSChannelStream;
  vPassword: PWideChar;
  oCertContext: Array [0 .. 0] of PCCERT_CONTEXT;
  vCreds: TCredHandle;
  vError: Integer;
begin
  result := False;

  oStream := TsgcSChannelStream.Create;
  try
    oStream.LoadFromFile(aFileName);

    oPFX.cbData := oStream.DataSize;
    oPFX.pbData := oStream.Data;
    vPassword := PWideChar(WideString(aPassword));

    oMyStore := PFXImportCertStore(oPFX, vPassword, 0);
    Try
      if (oMyStore = 0) and (aPassword = '') then
        oMyStore := PFXImportCertStore(oPFX, nil, 0);
      if oMyStore = 0 then
      begin
        vMsgError := 'Cannot import the certificate.';
        Exit;
      end;

      oCertContext[0] := CertFindCertificateInStore(oMyStore,
        X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, 0, CERT_FIND_ANY, nil, nil);

      if not Assigned(oCertContext[0]) then
      begin
        vMsgError := 'Cannot import the certificate.';
        Exit;
      end;

      SChannel.dwVersion := SCHANNEL_CRED_VERSION;
      SChannel.paCred := @oCertContext;
      SChannel.cCreds := 1;

      vError := oSSPI.AcquireCredentialsHandle(nil, UNISP_NAME,
        SECPKG_CRED_OUTBOUND, nil, @SChannel, nil, nil, @vCreds, nil);

      if vError = SEC_E_OK then
      begin
        oSSPI.FreeCredentialsHandle(@Credentials);
        Credentials := vCreds;
      end;

      result := vError = SEC_E_OK;
    Finally
      CertCloseStore(oMyStore, 0);
    end;
  Finally
    sgcFree(oStream);
  end;
end;

function TSSLInfo.GetCipherId(const aCipher: string): Cardinal;
var
  i: Integer;
begin
  result := 0;
  for i := Low(CS_SChannel_Ciphers) to High(CS_SChannel_Ciphers) do
  begin
    if CS_SChannel_Ciphers[i] = aCipher then
    begin
      result := CS_SChannel_CiphersId[i];
      break;
    end;
  end;
end;

function TSSLInfo.GetConnectionInfo: TsgcSChannelConnectionInfo;
var
  oInfo: TSecPkgContextConnectionInfo;
begin
  result := ConnectionInfo;
  if not Assigned(result) then
  begin
    if oSSPI.QueryContextAttributes(@Context, SECPKG_ATTR_CONNECTION_INFO,
      @oInfo) = SEC_E_OK then
    begin
      ConnectionInfo := TsgcSChannelConnectionInfo.Create;
      ConnectionInfo.SetProtocol(oInfo.dwProtocol);
      ConnectionInfo.SetCipher(oInfo.aiCipher);
      ConnectionInfo.CiperStrength := oInfo.dwCipherStrength;
      ConnectionInfo.SetHash(oInfo.aiHash);
      ConnectionInfo.HashStrength := oInfo.dwHashStrength;
      ConnectionInfo.SetExchange(oInfo.aiExch);
      ConnectionInfo.ExchangeStrength := oInfo.dwExchStrength;

      result := ConnectionInfo;
    end;
  end;
end;

function SSLAvailable: Boolean;
var
  Init: function: PSecurityFunctionTable; stdcall;
begin
  result := vInitialized;
  oError := 0;
  if not vInitialized then
  begin
    vInitialized := True;

    secur32 := LoadLibrary('secur32.dll');
    @Init := GetProcAddress(secur32, 'InitSecurityInterfaceW');

    if @Init = nil then
    begin
      vMsgError := 'InitSecurityInterface() not found';
      Exit;
    end;

    oSSPI := Init();
    if oSSPI = nil then
    begin
      vMsgError := 'InitSecurityInterface() returns nil';
      Exit;
    end;

    oMyStore := CertOpenSystemStore(0, 'MY');
    if oMyStore = 0 then
    begin
      vMsgError := 'CertOpenSystemStore(0, ''MY'') returns 0';
      Exit;
    end;

    result := True;
  end;
end;

procedure SSLShutdown;
begin
  if vInitialized then
  begin
    FreeLibrary(secur32);
    secur32 := 0;
    vInitialized := False;
  end;
end;

function SSLStart(aSocket: TSocket; aCertificate: TsgcSChannelCertificate;
  const Host: AnsiString = ''; aVersion: TwsTLSVersions = tls1_0;
  aALPNProtocols: TStrings = nil; aCipherList: String = ''): Integer;
var
  pInfo: PSSLInfo;
begin
  result := 0;
  CertStatus := 0;
  oError := 0;
  vMsgError := '';

  if SSLAvailable = False then
    Exit;

  {$IFDEF D7}
  pInfo := TSSLInfo.Create;
  {$ELSE}
  New(pInfo);
  FillChar(pInfo^, SizeOf(TSSLInfo), 0);
  {$ENDIF}
  pInfo.Socket := aSocket;
  pInfo.Servername := string(Host);
  pInfo.Version := aVersion;
  pInfo.ALPNProtocols := aALPNProtocols;
  pInfo.CertVerify := aCertificate.VerifyCertificate;
  pInfo.CertHash := aCertificate.CertHash;
  pInfo.CertFile := aCertificate.CertFile;
  pInfo.CertFilePassword := aCertificate.CertFilePassword;
  pInfo.CertStorePath := aCertificate.CertStorePath;
  pInfo.CertStoreName := aCertificate.CertStoreName;
  pInfo.CipherList := aCipherList;
  if not pInfo.Start then
  begin
    pInfo.Clean;
    {$IFDEF D7}
    FreeAndNil(pInfo);
    {$ELSE}
    Dispose(pInfo);
    {$ENDIF}
    if (oError = HRESULT(SEC_E_ILLEGAL_MESSAGE)) or
      (oError = HRESULT(SEC_E_INTERNAL_ERROR)) then
      SSLShutdown;
    Exit;
  end;

  result := Integer(pInfo);
end;

procedure SSLCredentialsCallBack(SSL: THandle; CallBack: TCredentialsCallBack;
  UserData: Pointer);
var
  pInfo: PSSLInfo absolute SSL;
begin
  if SSL <> 0 then
  begin
    pInfo.CredentialsCallBack := CallBack;
    pInfo.UserData := UserData;
  end;
end;

function SSLConnect(SSL: Integer): Boolean;
var
  pInfo: PSSLInfo absolute SSL;
begin
  result := SSL <> 0;
  if result then
  begin
    pInfo.InitBuffer;
  end;
end;

function SSLPending(SSL: THandle): Boolean;
var
  pInfo: PSSLInfo absolute SSL;
begin
  result := (pInfo <> nil) and (pInfo.Readable > 0);
end;

function SSLRead(SSL: THandle; var Data; Size: Integer): Integer;
var
  pInfo: PSSLInfo absolute SSL;
begin
  result := pInfo.Decrypt(Data, Size);
end;

function SSLWrite(SSL: THandle; var Data; Size: Integer): Integer;
var
  pInfo: PSSLInfo absolute SSL;
begin
  result := pInfo.Encrypt(Data, Size);
end;

function SSLClose(SSL: THandle): Integer;
var
  pInfo: PSSLInfo absolute SSL;
begin
  result := 0;
  if SSL = 0 then
    Exit;
  pInfo.Clean;
  {$IFDEF D7}
  FreeAndNil(pInfo);
  {$ELSE}
  Dispose(pInfo);
  {$ENDIF}
end;

function GetALPN(SSL: THandle): String;
var
  pInfo: PSSLInfo absolute SSL;
begin
  result := '';
  if Assigned(pInfo) then
    result := pInfo.GetALPN;
end;

function GetConnectionInfo(SSL: THandle): TsgcSChannelConnectionInfo;
var
  pInfo: PSSLInfo absolute SSL;
begin
  result := nil;
  if Assigned(pInfo) then
    result := pInfo.GetConnectionInfo;
end;

procedure TsgcSChannelCertificate.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSChannelCertificate then
  begin
    VerifyCertificate := TsgcSChannelCertificate(aSource).VerifyCertificate;
    CertStoreName := TsgcSChannelCertificate(aSource).CertStoreName;
    CertStorePath := TsgcSChannelCertificate(aSource).CertStorePath;
    CertHash := TsgcSChannelCertificate(aSource).CertHash;
    CertFile := TsgcSChannelCertificate(aSource).CertFile;
    CertFilePassword := TsgcSChannelCertificate(aSource).CertFilePassword;
  end
  else
    inherited Assign(aSource);
end;

destructor TsgcSChannelStream.Destroy;
begin
  DoFinalize;
  inherited;
end;

{ TsgcSChannelStream }

procedure TsgcSChannelStream.DoInitialize(aSize: Integer);
begin
  DoFinalize();
  FDataSize := aSize;
  if (FDataSize > 0) then
    GetMem(FData, FDataSize);
end;

procedure TsgcSChannelStream.DoFinalize;
begin
  FreeMem(FData);
  FData := nil;
  FDataSize := 0;
end;

procedure TsgcSChannelStream.LoadFromFile(const aFileName: String);
var
  oFileStream: TFileStream;
begin
  oFileStream := TFileStream.Create(aFileName, fmOpenRead);
  Try
    LoadFromStream(oFileStream);
  Finally
    sgcFree(oFileStream);
  End;
end;

procedure TsgcSChannelStream.LoadFromStream(const aSource: TStream);
var
  i: Integer;
begin
  i := aSource.Size - aSource.Position;
  DoInitialize(i);
  aSource.Read(Data^, i);
end;

procedure TsgcSChannelConnectionInfo.SetCipher(aValue: Cardinal);
var
  i: Integer;
begin
  CipherId := aValue;

  FCipher := '';
  for i := Low(CS_SChannel_CiphersId) to High(CS_SChannel_CiphersId) do
  begin
    if CS_SChannel_CiphersId[i] = aValue then
    begin
      FCipher := CS_SChannel_Ciphers[i];
      break;
    end;
  end;
end;

procedure TsgcSChannelConnectionInfo.SetHash(aValue: Cardinal);
var
  i: Integer;
begin
  HashId := aValue;

  FHash := '';
  for i := Low(CS_SChannel_CiphersId) to High(CS_SChannel_CiphersId) do
  begin
    if CS_SChannel_CiphersId[i] = aValue then
    begin
      FHash := CS_SChannel_Ciphers[i];
      break;
    end;
  end;
end;

procedure TsgcSChannelConnectionInfo.SetExchange(aValue: Cardinal);
var
  i: Integer;
begin
  ExchangeId := aValue;

  FExchange := '';
  for i := Low(CS_SChannel_CiphersId) to High(CS_SChannel_CiphersId) do
  begin
    if CS_SChannel_CiphersId[i] = aValue then
    begin
      FExchange := CS_SChannel_Ciphers[i];
      break;
    end;
  end;
end;

procedure TsgcSChannelConnectionInfo.SetProtocol(aValue: Cardinal);
begin
  case aValue of
    SP_PROT_TLS1_CLIENT:
      FProtocol := tls1_0;
    SP_PROT_TLS1_1_CLIENT:
      FProtocol := tls1_1;
    SP_PROT_TLS1_2_CLIENT:
      FProtocol := tls1_2;
    SP_PROT_TLS1_3_CLIENT:
      FProtocol := tls1_3;
  else
    FProtocol := tlsUndefined;
  end;
end;

initialization

oValidators := nil;

finalization

FreeValidators;

{$ENDIF}

end.
