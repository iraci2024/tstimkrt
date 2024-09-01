
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScCryptoAPIStorage;
{$ENDIF}

interface

{$IFNDEF LINUX}
{$IFNDEF LINUX_BSD}
{$IFNDEF ANDROID}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  {$IFNDEF ANDROID}Macapi.CoreFoundation,{$ENDIF}
  Posix.Stdlib, Posix.String_, Posix.Dlfcn,
{$ENDIF}
{$IFDEF DARWIN}
  MacTypes, CFBase, CFData, CFString, CFDictionary, CFNumber, CFArray,
{$ENDIF}
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsBridge, TdsCryptoAPIIntf, TdsASN1, TdsOids, TdsCertificateExts;
{$ELSE}
  TdsUtilsUni, TdsBridgeUni, TdsCryptoAPIIntfUni, TdsASN1Uni, TdsOidsUni, TdsCertificateExtsUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes,
  ScUtils, ScBridge, ScCryptoAPIIntf, ScASN1, ScOids, ScCertificateExts;
{$ENDIF}

{$IFDEF VER16P}
const
  pidCryptoAPIPlatforms = pidWin32 or pidWin64 or pidOSX32{$IFDEF VER26P} or pidOSX64{$ENDIF}
    {$IFDEF VER18P} or pidiOSSimulator{$IFNDEF VER22P} or pidiOSDevice{$ELSE} or pidiOSDevice32 or pidiOSDevice64{$ENDIF}{$ENDIF};
{$ENDIF}

type
{$IFDEF MSWINDOWS}
  TScCertProviderType = (
    ptMemory,
    ptFile,
    ptRegistry,
    ptSystem,
    ptSystemRegistry,
    ptPhysical
  );

  /// Specifies the location of the X.509 certificate store.
  TScCertLocation = (
    clCurrentUser,
    clCurrentUserGroupPolicy,
    clLocalMachine,
    clLocalMachineEnterprise,
    clLocalMachineGroupPolicy,
    clCurrentService,
    clServices,
    clUsers
  );
{$ENDIF}

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidCryptoAPIPlatforms{$ENDIF})]
{$ENDIF}
  TScCryptoAPIStorage = class(TScStorage)
  private
  {$IFDEF MSWINDOWS}
    FCertProviderType: TScCertProviderType;
    FCertLocation: TScCertLocation;
    FCertStoreName: string;
    FProviderName: string;
    FProviderType: Cardinal;
    FFileHandle: Integer;

    procedure SetProviderName(const Value: string);
    procedure SetCertProviderType(const Value: TScCertProviderType);
    procedure SetCertLocation(const Value: TScCertLocation);
    procedure SetCertStoreName(const Value: string);

    /// Returns a certificate from the CertificateStore.
    function FindCertificate(Previous: TScCertificate): TScCertificate;
    /// Finds a certificate with a matching hash.
    function FindCertificateByHash(const Hash: TBytes; HashAlgorithm: TScHashAlgorithm): PCCERT_CONTEXT;

  {$ELSE}
    FKeychainPath: string;
    procedure SetKeychainPath(const Value: string);
    procedure GetKeychainObjectList(List: TStrings; ObjClass: SecItemClass; ObjTypeID: CFTypeID);
    function GetKeychainObject(ObjClass: SecItemClass; const ObjName: string): SecKeychainItemRef;
    class function GetKeyData(var KeyRef: IntPtr; out KeyExFormat: TScKeyExFormat;
      out Alg: TScAsymmetricAlgorithm; out IsPublicKey: boolean): TBytes;
  {$ENDIF}

    procedure Init(Flags: cardinal = 0); /// Initializes a new certificate store.
    function GetStoreHandle: {$IFDEF MSWINDOWS}HCERTSTORE{$ELSE}SecKeychainRef{$ENDIF};
    procedure GetKeyNames(List: TStrings);
    procedure GetCertificateNames(List: TStrings);

    procedure FreeCertHandle(CertHandle: IntPtr);
//    class procedure AssociateWithPrivateKey(CertHandle: IntPtr; const KeyBlob: TBytes);
//    class function GetCertKeyRef(CertHandle: IntPtr): IntPtr;
  {$IFDEF MSWINDOWS}
    class function GetCertHash(CertHandle: IntPtr; HashAlg: TScHashAlgorithm): TBytes;
    class function GetUserKey(hProv: HCRYPTPROV; CertHandle: IntPtr = nil): HCRYPTKEY;
  {$ELSE}
    class function ImportKeyFromBlob(const KeyBlob: TBytes; IsPrivate: boolean;
      Keychain: SecKeychainRef; KeyRef: IntPtr): IntPtr;
  {$ENDIF}

  protected
    FHandle: IntPtr;

    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
  {$IFDEF MSWINDOWS}
    procedure ReadCertStoreName(Reader: TReader);
    procedure WriteCertStoreName(Writer: TWriter);
  {$ELSE}
    procedure ReadKeychainPath(Reader: TReader);
    procedure WriteKeychainPath(Writer: TWriter);
  {$ENDIF}

    function GetCertKey(CertHandle: IntPtr;
      out KeyExFormat: TScKeyExFormat; out Alg: TScAsymmetricAlgorithm;
      out IsPublicKey: boolean): TBytes;

    procedure Invalidate; override;
    procedure Flush; override;
    procedure InternalLoad(Item: TScStorageItem); override;
    procedure InternalSave(Item: TScStorageItem); override;
    procedure InternalDelete(Item: TScStorageItem); override;
    procedure InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean); override;
    procedure InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings); override;
    function GetUsers: TScUserList; override;
    function GetCRLs: TScCRLList; override;

    function ImportCert(const Data: TBytes; const Password: string): IntPtr;
    function GetCertRawData(CertHandle: IntPtr): TBytes;
    function GetSubject(CertHandle: IntPtr): string;
    procedure InternalDeleteStorage; override;

  public
    constructor Create(AOwner: TComponent); override;

    function FindCertificateBySubject(DistinguishedName: TScDistinguishedName): TScCertificate;
    function CheckCertificateByIssuer(Certificate: TScCertificate): TScCertificateStatus;
  {$IFDEF IOS}
    class function CheckTrustCertificateByIssuer(Certificate: TScCertificate): TScCertificateStatus;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    procedure GetProviderNames(List: TStrings);
  {$ENDIF}

    property Handle: IntPtr read FHandle;

  published
  {$IFDEF MSWINDOWS}
    property ProviderName: string read FProviderName write SetProviderName;
    property CertProviderType: TScCertProviderType read FCertProviderType write SetCertProviderType default ptSystem;
    property CertLocation: TScCertLocation read FCertLocation write SetCertLocation default clCurrentUser;
    property CertStoreName: string read FCertStoreName write SetCertStoreName stored False;
  {$ELSE}
    property KeychainPath: string read FKeychainPath write SetKeychainPath;
  {$ENDIF}
  end;

{$IFDEF MSWINDOWS}
const
  ListSystemStoreNames: array[0..3] of string = ('MY', 'Root', 'Trust', 'CA');
{$ENDIF}

{$IFNDEF MSWINDOWS}
function CFStringToStr(Value: CFStringRef): WideString;
procedure CheckError(Status: OSStatus; const Message: string = '');
{$ENDIF}

{$ENDIF ANDROID}
{$ENDIF LINUX_BSD}
{$ENDIF LINUX}

implementation

{$IFNDEF LINUX}
{$IFNDEF LINUX_BSD}
{$IFNDEF ANDROID}

uses
{$IFDEF VER7P}
  StrUtils,
{$ENDIF}
{$IFNDEF MSWINDOWS}
  DateUtils,
{$ENDIF}
{$IFDEF VER17P}
{$IFNDEF NEXTGEN}
  Contnrs,
{$ENDIF}
{$ENDIF}
  SyncObjs,
{$IFNDEF SBRIDGE}
  CRDECUtil, CRFunctions, CRBigInteger,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts
{$ELSE}
  TdsSSLConstsUni
{$ENDIF}
{$ELSE}
  ScDECUtil, ScFunctions, ScBigInteger, ScConsts
{$ENDIF}
  {$IFDEF MSWINDOWS}, Registry{$ENDIF};

{$IFDEF MSWINDOWS}
const
  X509_PKCS7 = X509_ASN_ENCODING or PKCS_7_ASN_ENCODING;
  KEY_NAME_PREFIX = {$IFDEF UNICODE}WideString{$ENDIF}('SB_');
  KEY_CONTAINER = {$IFDEF UNICODE}WideString{$ENDIF}('{5DBA170A-13B6-4D3C-AE88-A29A1CDD350E}'); // 'SBridge-key-container';
  OID_COMMON_NAME = '2.5.4.3';

function IntPtrToNativeUInt(Value: IntPtr): NativeUInt;
begin
  Result := NativeUInt(Value {$IFDEF FPC}- nil{$ENDIF});
end;

function NativeUIntToIntPtr(Value: NativeUInt): IntPtr;
begin
  Result := IntPtr({$IFDEF FPC}IntPtr(0) + {$ENDIF}Value);
end;

function _StringToHGlobal(const s: {$IFDEF UNICODE}WideString{$ELSE}string{$ENDIF}): IntPtr;
begin
{$IFDEF UNICODE}
  Result := Marshal.StringToHGlobalUni(s);
{$ELSE}
  Result := Marshal.StringToHGlobalAnsi(s);
{$ENDIF}
end;

function _PtrToString(ptr: IntPtr): {$IFDEF UNICODE}WideString{$ELSE}string{$ENDIF};
begin
{$IFDEF UNICODE}
  Result := Marshal.PtrToStringUni(ptr);
{$ELSE}
  Result := Marshal.PtrToStringAnsi(ptr);
{$ENDIF}
end;

function GetSysErrorMessage: string;
begin
  Result := SysErrorMessage({$IFNDEF VER12P}Integer{$ENDIF}(GetLastError));
end;

type
  TCryptContext = class
  protected
    FHandle: HCRYPTPROV;
    FContainerName: string;
    FProviderName: string;
    FProviderType: cardinal;
    FDeleteOnDestroy: boolean;
  public
    constructor Create(const ContainerName, ProviderName: string;
      ProviderType: cardinal; CanCreate: boolean);
    destructor Destroy; override;

    property Handle: HCRYPTPROV read FHandle;
  end;

var
  CSP: TCryptContext;
  CSPError: string = '';
  CSPLock: TCriticalSection;

function CryptGetContext(const ContainerName, ProviderName: string;
  ProviderType: cardinal; CanCreate: boolean): HCRYPTPROV;
var
  pContainer: IntPtr;
  pProvider: IntPtr;
begin
  if ContainerName = '' then
    pContainer := nil
  else
    pContainer := _StringToHGlobal(ContainerName);

  if ProviderName = '' then
    pProvider := nil
  else
    pProvider := _StringToHGlobal(ProviderName);

  try
    Result := 0;
    if not CryptAcquireContext(Result, pContainer, pProvider, ProviderType, CRYPT_SILENT) then
      if (HRESULT(GetLastError) = NTE_BAD_KEYSET) and CanCreate then
        CryptAcquireContext(Result, pContainer, pProvider, ProviderType, CRYPT_NEWKEYSET or CRYPT_SILENT);
  finally
    if pProvider <> nil then
      Marshal.FreeCoTaskMem(pProvider);
    if pContainer <> nil then
      Marshal.FreeCoTaskMem(pContainer);
  end;
end;

{ TCryptContext }

constructor TCryptContext.Create(const ContainerName, ProviderName: string;
  ProviderType: cardinal; CanCreate: boolean);
begin
  inherited Create;

  FContainerName := ContainerName;
  FProviderName := ProviderName;
  FProviderType := ProviderType;

  FHandle := CryptGetContext(FContainerName, FProviderName, FProviderType, CanCreate);
  if (FHandle = 0) and (HRESULT(GetLastError) = NTE_EXISTS) and (FContainerName = '') then begin
    FContainerName := KEY_CONTAINER;
    FHandle := CryptGetContext(FContainerName, FProviderName, FProviderType, CanCreate);
  end;
  Win32Check(FHandle <> 0);
end;

destructor TCryptContext.Destroy;
var
  hProv: HCRYPTPROV;
  pContainer: IntPtr;
  pProvider: IntPtr;
begin
  if FHandle <> 0 then
    CryptReleaseContext(FHandle, 0);

  if FDeleteOnDestroy then begin
    if FProviderName = '' then
      pProvider := nil
    else
      pProvider := _StringToHGlobal(FProviderName);

    pContainer := _StringToHGlobal(FContainerName);
    try
      Win32Check(CryptAcquireContext(hProv, pContainer, pProvider, FProviderType,
        CRYPT_DELETEKEYSET or CRYPT_SILENT));
    finally
      if pProvider <> nil then
        Marshal.FreeCoTaskMem(pProvider);
      Marshal.FreeCoTaskMem(pContainer);
    end;
  end;

  inherited;
end;

function ContainerHandle: HCRYPTPROV;
begin
  lock(CSPLock);
  try
    if CSP = nil then begin
      if CSPError = '' then begin
        try
          CSP := TCryptContext.Create(KEY_CONTAINER, '', PROV_RSA_FULL, True);
        except
          on E: Exception do
            CSPError := E.Message;
        end;
      end;

      if CSPError <> '' then
        raise EScError.CreateFmt(CSPError, [], seCSPError);
    end;
  finally
    unlock(CSPLock);
  end;

  Result := CSP.Handle;
end;
{$ENDIF}

{ TScCryptoAPIStorage }

constructor TScCryptoAPIStorage.Create(AOwner: TComponent);
begin
  inherited;

{$IFDEF MSWINDOWS}
  FFileHandle := -1;
  FCertProviderType := ptSystem;
  FCertLocation := clCurrentUser;
  FCertStoreName := 'MY';
  FProviderName := '';
  FProviderType := PROV_RSA_FULL;
{$ENDIF}
  FHandle := nil;
end;

procedure TScCryptoAPIStorage.Invalidate;
begin
  inherited;

{$IFDEF MSWINDOWS}
  if FHandle <> nil then begin
    CertCloseStore(FHandle, 0);
    FHandle := nil;
  end;

  if FFileHandle > -1 then begin
    CloseHandle(THandle(FFileHandle));
    FFileHandle := -1;
  end;

{$ELSE}
  if FHandle <> nil then begin
    CFRelease(FHandle);
    FHandle := nil;
  end;
{$ENDIF}
end;

function TScCryptoAPIStorage.GetStoreHandle: {$IFDEF MSWINDOWS}HCERTSTORE{$ELSE}SecKeychainRef{$ENDIF};
begin
  if FHandle = nil then
    Init;

  Result := FHandle;
end;

procedure TScCryptoAPIStorage.Init(Flags: cardinal = 0);
{$IFDEF MSWINDOWS}
  function GetSystemLocationFlag: Cardinal;
  begin
    case CertLocation of
      clCurrentService:
        Result := CERT_SYSTEM_STORE_CURRENT_SERVICE;
      clCurrentUser:
        Result := CERT_SYSTEM_STORE_CURRENT_USER;
      clCurrentUserGroupPolicy:
        Result := CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY;
      clLocalMachine:
        Result := CERT_SYSTEM_STORE_LOCAL_MACHINE;
      clLocalMachineEnterprise:
        Result := CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE;
      clLocalMachineGroupPolicy:
        Result := CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY;
      clServices:
        Result := CERT_SYSTEM_STORE_SERVICES;
      clUsers:
        Result := CERT_SYSTEM_STORE_USERS;
    else
      Result := 0;
      Assert(False);
    end;
  end;

  function GetRegKey: HKEY;
  begin
    case CertLocation of
      clCurrentService, clCurrentUserGroupPolicy,
      clLocalMachineEnterprise, clLocalMachineGroupPolicy, clServices:
        Result := 0;
      clCurrentUser:
        Result := HKEY_CURRENT_USER;
      clLocalMachine:
        Result := HKEY_LOCAL_MACHINE;
      clUsers:
        Result := HKEY_USERS;
    else
      Result := 0;
      Assert(False);
    end;
  end;
{$ENDIF}

var
{$IFDEF MSWINDOWS}
  StoreProvider: LPCSTR;
  EncodingType: Cardinal;
  Store: IntPtr;
  RegKey: HKEY;
  CryptContext: TCryptContext;
  FileName: string;
  Disposition: Cardinal;
{$ELSE}
  Keychain: SecKeychainRef;
  Status: OSStatus;
  PathLength: UInt32;
  PathName: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if (CertStoreName = '') and (CertProviderType <> ptMemory) then
    raise EScError.Create(seEmptyStorageName);

  EncodingType := 0;
  RegKey := 0;
  Store := nil;
  if ReadOnly then
    Flags := Flags or CERT_STORE_READONLY_FLAG;

  case CertProviderType of
    ptMemory: begin
      StoreProvider := LPCSTR(CERT_STORE_PROV_MEMORY);
    end;

    ptFile: begin
      StoreProvider := LPCSTR(CERT_STORE_PROV_FILE);
      EncodingType := X509_PKCS7;

      if (CertStoreName <> '') and (CertStoreName[1] = '.') and  // Path = '.'; '..'; '.\'; '..\'
         (CurrentProjectOutputDir <> '') then
        FileName := IncludeTrailingBackslash(CurrentProjectOutputDir) + CertStoreName
      else
        FileName := CertStoreName;

      if FFileHandle > -1 then
        CloseHandle(THandle(FFileHandle));

      if ReadOnly then begin
        FFileHandle := Integer(FileOpen(FileName, fmShareDenyWrite));
      end
      else begin
        Flags := Flags or CERT_FILE_STORE_COMMIT_ENABLE_FLAG;
        FFileHandle := Integer(CreateFile(PChar(FileName),
          GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
      end;

      Win32Check(FFileHandle > -1);
      Store := NativeUIntToIntPtr(FFileHandle);
    end;

    ptRegistry: begin
      StoreProvider := LPCSTR(CERT_STORE_PROV_REG);

      if RegOpenKeyEx(GetRegKey, PChar(CertStoreName), 0, KEY_READ, RegKey) <> ERROR_SUCCESS then
        if ReadOnly or (RegCreateKeyEx(GetRegKey, PChar(CertStoreName),
          0, nil, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, nil, RegKey, @Disposition) <> ERROR_SUCCESS)
        then
          raise EScError.Create(seCannotOpenRegistry);

      Store := NativeUIntToIntPtr(RegKey);
    end;

    ptSystem: begin
      StoreProvider := LPCSTR(CERT_STORE_PROV_SYSTEM);
      Store := Marshal.StringToHGlobalUni(WideString(CertStoreName));
      Flags := Flags or GetSystemLocationFlag;
    end;
    ptSystemRegistry: begin
      StoreProvider := LPCSTR(CERT_STORE_PROV_SYSTEM_REGISTRY);
      Store := Marshal.StringToHGlobalUni(WideString(CertStoreName));
      Flags := Flags or GetSystemLocationFlag;
    end;
    ptPhysical: begin
      StoreProvider := LPCSTR(CERT_STORE_PROV_PHYSICAL);
      Store := Marshal.StringToHGlobalUni(WideString(CertStoreName));
      Flags := Flags or GetSystemLocationFlag;
    end;
  else
    StoreProvider := LPCSTR(0);
    Assert(False);
  end;

  try
    if FHandle <> nil then begin
      CertCloseStore(FHandle, 0);
      FHandle := nil;
    end;

    if ProviderName <> '' then begin
      CryptContext := TCryptContext.Create('', FProviderName, FProviderType, True);
      try
        FHandle := CertOpenStore(StoreProvider, EncodingType, CryptContext.Handle, Flags, Store);
      finally
        CryptContext.Free;
      end;
    end
    else
      FHandle := CertOpenStore(StoreProvider, EncodingType, 0, Flags, Store);

    Win32Check((FHandle <> nil) or
      (((Flags and CERT_STORE_DELETE_FLAG) > 0) and (GetLastError = 0)));
  finally
    if (Store <> nil) and (CertProviderType in [ptSystem, ptSystemRegistry, ptPhysical]) then
      Marshal.FreeCoTaskMem(Store);
    if RegKey <> 0 then
      RegCloseKey(RegKey);
  end;

{$ELSE MSWINDOWS}
  if FHandle <> nil then begin
    CFRelease(FHandle);
    FHandle := nil;
  end;

  if FKeychainPath = '' then begin
    CheckError(SecKeychainCopyDefault(Keychain));
    try
      PathLength := 128;
      SetLengthA(PathName, PathLength);
      Status := SecKeychainGetPath(Keychain, @PathLength, PAnsiChar(PathName));
      if Status = errSecBufferTooSmall then begin
        Inc(PathLength);
        SetLengthA(PathName, PathLength);
        CheckError(SecKeychainGetPath(Keychain, @PathLength, PAnsiChar(PathName)));
      end
      else
        CheckError(Status);

      FKeychainPath := {$IFNDEF UNIX}{$IFDEF SBRIDGE}ScFunctions{$ELSE}CRFunctions{$ENDIF}.Utf8Decode{$ENDIF}(PathName);
    finally
      if Keychain <> nil then
        CFRelease(Keychain);
    end;
  end;

  PathName := {$IFDEF SBRIDGE}ScFunctions{$ELSE}CRFunctions{$ENDIF}.Utf8Encode({$IFDEF FPC}WideString{$ENDIF}(FKeychainPath));
  CheckError(SecKeychainOpen(PAnsiChar(PathName), FHandle));
{$ENDIF}
end;

procedure TScCryptoAPIStorage.Flush;
begin
  inherited;

{$IFDEF MSWINDOWS}
  if (FHandle <> nil) and (CertProviderType = ptFile) then
    if FFileHandle > -1 then begin
      SetFilePointer(THandle(FFileHandle), 0, nil, FILE_BEGIN);
      CertSaveStore(FHandle, X509_PKCS7,
        CERT_STORE_SAVE_AS_STORE, CERT_STORE_SAVE_TO_FILE, NativeUIntToIntPtr(FFileHandle), 0);
      FlushFileBuffers(THandle(FFileHandle));
    end;
{$ENDIF}
end;

procedure TScCryptoAPIStorage.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScCryptoAPIStorage) then begin
    TScCryptoAPIStorage(Dest).FReadOnly := FReadOnly;

  {$IFDEF MSWINDOWS}
    TScCryptoAPIStorage(Dest).ProviderName := ProviderName;
    TScCryptoAPIStorage(Dest).CertProviderType := CertProviderType;
    TScCryptoAPIStorage(Dest).CertLocation := CertLocation;
    TScCryptoAPIStorage(Dest).CertStoreName := CertStoreName;
  {$ELSE}
    TScCryptoAPIStorage(Dest).KeychainPath := KeychainPath;
  {$ENDIF}
  end
  else
    inherited;
end;

procedure TScCryptoAPIStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

{$IFDEF MSWINDOWS}
  Filer.DefineProperty('CertStoreName', ReadCertStoreName, WriteCertStoreName, FCertStoreName <> 'MY');
{$ELSE}
  Filer.DefineProperty('KeychainPath', ReadKeychainPath, WriteKeychainPath, FKeychainPath <> '');
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TScCryptoAPIStorage.ReadCertStoreName(Reader: TReader);
begin
  FCertStoreName := Reader.ReadString;
end;

procedure TScCryptoAPIStorage.WriteCertStoreName(Writer: TWriter);
begin
  Writer.WriteString(FCertStoreName);
end;

{$ELSE}
procedure TScCryptoAPIStorage.ReadKeychainPath(Reader: TReader);
begin
  FKeychainPath := Reader.ReadString;
end;

procedure TScCryptoAPIStorage.WriteKeychainPath(Writer: TWriter);
begin
  Writer.WriteString(FKeychainPath);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure TScCryptoAPIStorage.GetProviderNames(List: TStrings);
var
  Index, NameLen: Cardinal;
  ProvType: Cardinal;
  ProvName: IntPtr;
begin
  List.Clear;

  NameLen := 0;
  Index := 0;
  while True do begin
    if not CryptEnumProviders(Index, nil, 0, ProvType, nil, NameLen) then
      if GetLastError = ERROR_NO_MORE_ITEMS then
        break
      else
        RaiseLastWin32Error;

    ProvName := Marshal.AllocHGlobal(NameLen);
    try
      Win32Check(CryptEnumProviders(Index, nil, 0, ProvType, ProvName, NameLen));
      List.Add(_PtrToString(ProvName));
    finally
      Marshal.FreeHGlobal(ProvName);
    end;

    Inc(Index);
  end;
end;

procedure TScCryptoAPIStorage.SetProviderName(const Value: string);
var
  Index, NameLen: Cardinal;
  ProvType: Cardinal;
  ProvName: IntPtr;
begin
  if Value <> FProviderName then begin
    Invalidate;

    if Value = '' then begin
      FProviderType := PROV_RSA_FULL;
      FProviderName := '';
      Exit;
    end;

    NameLen := 0;
    Index := 0;
    while True do begin
      if not CryptEnumProviders(Index, nil, 0, ProvType, nil, NameLen) then
        if GetLastError = ERROR_NO_MORE_ITEMS then
          raise EScError.Create(seCannotFindProviderName)
        else
          RaiseLastWin32Error;

      ProvName := Marshal.AllocHGlobal(NameLen);
      try
        Win32Check(CryptEnumProviders(Index, nil, 0, ProvType, ProvName, NameLen));
        if Value = _PtrToString(ProvName) then
          break;
      finally
        Marshal.FreeHGlobal(ProvName);
      end;

      Inc(Index);
    end;

    FProviderType := ProvType;
    FProviderName := Value;
  end;
end;

procedure TScCryptoAPIStorage.SetCertProviderType(const Value: TScCertProviderType);
begin
  if Value <> FCertProviderType then begin
    FCertProviderType := Value;
    Invalidate;
  end;
end;

procedure TScCryptoAPIStorage.SetCertLocation(const Value: TScCertLocation);
begin
  if Value <> FCertLocation then begin
    FCertLocation := Value;
    Invalidate;
  end;
end;

procedure TScCryptoAPIStorage.SetCertStoreName(const Value: string);
begin
  if Value <> FCertStoreName then begin
    FCertStoreName := Value;
    Invalidate;
  end;
end;

{$ELSE}
procedure TScCryptoAPIStorage.SetKeychainPath(const Value: string);
begin
  if Value <> FKeychainPath then begin
    FKeychainPath := Value;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TScCryptoAPIStorage.FreeCertHandle(CertHandle: IntPtr);
begin
{$IFDEF MSWINDOWS}
  if CertHandle <> nil then
    CertFreeCertificateContext(CertHandle);

{$ELSE}
  if CertHandle <> nil then
    CFRelease(CertHandle);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
function TScCryptoAPIStorage.FindCertificate(Previous: TScCertificate): TScCertificate;
var
  Prev: PCCERT_CONTEXT;
  ce: PCCERT_CONTEXT;
  FriendlyName: IntPtr;
  CertName: string;
  Size: Cardinal;
begin
  if (Previous = nil) or (Previous.Handle = nil) then
    Prev := nil
  else
    Prev := CertDuplicateCertificateContext(Previous.Handle);

  ce := CertFindCertificateInStore(GetStoreHandle, X509_PKCS7{X509_ASN_ENCODING}, 0, CERT_FIND_ANY, nil, Prev);

  if ce = nil then
    Result := nil
  else begin
    Size := 256;
    FriendlyName := Marshal.AllocHGlobal(Size);
    try
      if CertGetCertificateContextProperty(ce, CERT_FRIENDLY_NAME_PROP_ID, FriendlyName, Size) then begin
        if PWideChar(PtrOffset(FriendlyName, Size-2))^ = #0 then
          Dec(Size, 2);

        CertName := string(Marshal.PtrToStringUni(FriendlyName, Size shr 1));
      end
      else
        CertName := '';
    finally
      Marshal.FreeHGlobal(FriendlyName);
    end;

    Result := TScCertificate.Create(nil);
    try
      Result.SetRawData(ce, FreeCertHandle, GetCertRawData(ce));
      if CertName = '' then
        Result.CertName := Result.Subject
      else
        Result.CertName := CertName;
    except
      Result.Free;
      raise;
    end;
  end;
end;

function TScCryptoAPIStorage.FindCertificateByHash(const Hash: TBytes; HashAlgorithm: TScHashAlgorithm): PCCERT_CONTEXT;
var
  FindType: Integer;
  Data: CRYPTOAPI_BLOB;
  buf: IntPtr;
begin
  if Length(Hash) = 0 then
    raise EScError.Create(seInvalidInputArgs);

  if HashAlgorithm = haSHA1 then
    FindType := CERT_FIND_SHA1_HASH
  else if HashAlgorithm = haMD5 then
    FindType := CERT_FIND_MD5_HASH
  else
    FindType := CERT_FIND_HASH;

  Data.cbData := Length(Hash);
  Data.pbData := Marshal.AllocHGlobal(Length(Hash));
  try
    Marshal.Copy(Hash, 0, Data.pbData, Length(Hash));

    buf := @Data;
    Result := CertFindCertificateInStore(GetStoreHandle, X509_PKCS7, 0, FindType, buf, nil);
  finally
    Marshal.FreeHGlobal(Data.pbData);
  end;
end;

{$ELSE MSWINDOWS}
procedure TScCryptoAPIStorage.GetKeychainObjectList(List: TStrings;
  ObjClass: SecItemClass; ObjTypeID: CFTypeID);
var
  SearchRef: SecKeychainSearchRef;
  ItemRef: SecKeychainItemRef;
  AttrTag: UInt32;
  AttrFormat: UInt32;
  Info: SecKeychainAttributeInfo;
  PGetAttrList: PSecKeychainAttributeList;
  Status: OSStatus;
  ObjName: AnsiString;
  Cert: TScCertificate;
begin
  CheckError(SecKeychainSearchCreateFromAttributes(GetStoreHandle, ObjClass, nil, SearchRef));
  try
    AttrTag := kSecKeyPrintName;
    AttrFormat := CSSM_DB_ATTRIBUTE_FORMAT_BLOB;
    Info.Count := 1;
    Info.Tag := @AttrTag;
    Info.Format := @AttrFormat;

    Status := SecKeychainSearchCopyNext(SearchRef, ItemRef);
    while Status = 0 do begin
      try
        if CFGetTypeID(ItemRef) = ObjTypeID then begin
          if ObjClass = kSecCertificateItemClass then begin
            Cert := TScCertificate.Create(nil);
            try
              Cert.SetRawData(CFRetain(ItemRef), FreeCertHandle, GetCertRawData(ItemRef));
              Cert.CertName := Cert.Subject;
            except
              Cert.Free;
              raise;
            end;
            List.AddObject(Cert.CertName, Cert);
          end
          else begin
            CheckError(SecKeychainItemCopyAttributesAndData(ItemRef, @Info, nil, @PGetAttrList, nil, nil));
            Assert(PGetAttrList.Count = 1);
            Assert(PGetAttrList.Attr.Tag = kSecKeyPrintName);
            SetLengthA(ObjName, PGetAttrList.Attr.Length);
            Move(PGetAttrList.Attr.Data^, PAnsiChar(ObjName)^, LengthA(ObjName));
            List.AddObject({$IFDEF FPC}string{$ENDIF}({$IFDEF SBRIDGE}ScFunctions{$ELSE}CRFunctions{$ENDIF}.Utf8Decode(ObjName)), nil);
          end;
        end;
      finally
        if ItemRef <> nil then
          CFRelease(ItemRef);
      end;

      Status := SecKeychainSearchCopyNext(SearchRef, ItemRef);
    end;

    if Status <> errSecItemNotFound then
      CheckError(Status);
  finally
    if SearchRef <> nil then
      CFRelease(SearchRef);
  end;
end;

function TScCryptoAPIStorage.GetKeychainObject(
  ObjClass: SecItemClass; const ObjName: string): SecKeychainItemRef;
var
  SearchAttrList: SecKeychainAttributeList;
  SearchAttr: SecKeychainAttribute;
  SearchRef: SecKeychainSearchRef;
  sa: AnsiString;
  Status: OSStatus;
begin
  Result := nil;
  sa := {$IFDEF SBRIDGE}ScFunctions{$ELSE}CRFunctions{$ENDIF}.Utf8Encode({$IFDEF FPC}WideString{$ENDIF}(ObjName));
  SearchAttr.Tag := kSecKeyPrintName;
  SearchAttr.Length := LengthA(sa);
  SearchAttr.Data := PAnsiChar(sa);
  SearchAttrList.Count := 1;
  SearchAttrList.Attr := @SearchAttr;
  CheckError(SecKeychainSearchCreateFromAttributes(GetStoreHandle, ObjClass, @SearchAttrList, SearchRef));
  try
    Status := SecKeychainSearchCopyNext(SearchRef, Result);
    if Status <> errSecItemNotFound then
      CheckError(Status);
  finally
    if SearchRef <> nil then
      CFRelease(SearchRef);
  end;
end;

class function TScCryptoAPIStorage.GetKeyData(var KeyRef: IntPtr;
  out KeyExFormat: TScKeyExFormat; out Alg: TScAsymmetricAlgorithm;
  out IsPublicKey: boolean): TBytes;
var
  CssmKey: CSSM_KEY_PTR;
  ExportedData: CFDataRef;
  Range: CFRange;
  Keys: array [0..3] of pointer;
  Values: array [0..3] of pointer;
  NeyKeyRef: CFTypeRef;
  SearchAttrs: CFDictionaryRef;
  st: OSStatus;
begin
  if _Assigned(@SecKeyGetCSSMKey) then begin
    CheckError(SecKeyGetCSSMKey(KeyRef, CssmKey));

    case CssmKey.KeyHeader.Format of
      CSSM_KEYBLOB_RAW_FORMAT_NONE:
        KeyExFormat := kefOpenSSL;
      CSSM_KEYBLOB_RAW_FORMAT_PKCS1:
        KeyExFormat := kefDefault;
      CSSM_KEYBLOB_RAW_FORMAT_PKCS8:
        KeyExFormat := kefPKCS8;
    else
      raise EScError.Create(seWrongDataFormat);
    end;

    case CssmKey.KeyHeader.AlgorithmId of
      CSSM_ALGID_RSA:
        Alg := aaRSA;
      CSSM_ALGID_DSA:
        Alg := aaDSA;
    else
      raise EScError.Create(seWrongDataFormat);
    end;

    case CssmKey.KeyHeader.KeyClass of
      CSSM_KEYCLASS_PUBLIC_KEY:
        IsPublicKey := True;
      CSSM_KEYCLASS_PRIVATE_KEY:
        IsPublicKey := False;
    else
      raise EScError.Create(seWrongDataFormat);
    end;

    if CssmKey.KeyHeader.Format = CSSM_KEYBLOB_RAW_FORMAT_NONE then begin
      KeyExFormat := kefOpenSSL;
      CheckError(SecKeychainItemExport(KeyRef, kSecFormatOpenSSL, 0, nil, ExportedData));
      Range := CFRangeMake(0, CFDataGetLength(ExportedData));
      SetLength(Result, Range.length);
      if Range.length > 0 then
        CFDataGetBytes(ExportedData, Range, @Result[0]);
    end
    else begin
      SetLength(Result, CssmKey.KeyData.Length);
      Marshal.Copy(CssmKey.KeyData.Data, Result, 0, Length(Result));
    end;
  end
  else begin
    KeyExFormat := kefDefault;
    Alg := aaRSA;
    IsPublicKey := True;

    Keys[0] := kSecClass;
    Values[0] := kSecClassKey;
    Keys[1] := kSecValueRef;
    Values[1] := KeyRef;
    SearchAttrs := CFDictionaryCreate(nil, @Keys, @Values, 2, nil, nil);
    SecItemAdd(SearchAttrs, nil);
    CFRelease(SearchAttrs);

    Keys[2] := kSecAttrKeyClass;
    Values[2] := kSecAttrKeyClassPrivate;
    Keys[3] := kSecReturnRef;
    Values[3] := kCFBooleanTrue;
    SearchAttrs := CFDictionaryCreate(nil, @Keys, @Values, 4, nil, nil);
    NeyKeyRef := nil;
    st := SecItemCopyMatching(SearchAttrs, NeyKeyRef);
    if st = errSecSuccess then begin
      CFRelease(KeyRef);
      KeyRef := NeyKeyRef;
      IsPublicKey := False;
    end
    else
    if st = errSecItemNotFound then begin
      IsPublicKey := True;
      Values[2] := kSecAttrKeyClassPublic;
    end
    else
      CheckError(st);

    Keys[3] := kSecReturnData;
    Values[3] := kCFBooleanTrue;
    SearchAttrs := CFDictionaryCreate(nil, @Keys, @Values, 4, nil, nil);
    ExportedData := nil;
    try
      CheckError(SecItemCopyMatching(SearchAttrs, CFTypeRef(ExportedData)));

      Range := CFRangeMake(0, CFDataGetLength(ExportedData));
      SetLength(Result, Range.length);
      if Range.length > 0 then
        CFDataGetBytes(ExportedData, Range, @Result[0]);
    finally
      CFRelease(SearchAttrs);
      if ExportedData <> nil then
        CFRelease(ExportedData);
    end;
  end;
end;
{$ENDIF}

procedure TScCryptoAPIStorage.GetKeyNames(List: TStrings);
{$IFDEF MSWINDOWS}
var
  CryptContext: TCryptContext;
  Data: IntPtr;
  DataLen: Cardinal;
  Flags: Cardinal;
  KeyName: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Data := nil;
  CryptContext := TCryptContext.Create('', FProviderName, FProviderType, True);
  try
    DataLen := 0;
    Win32Check(CryptGetProvParam(CryptContext.Handle, PP_ENUMCONTAINERS, nil, DataLen, CRYPT_FIRST));

    Flags := CRYPT_FIRST;
    Data := Marshal.AllocHGlobal(DataLen);
    while True do begin
      if not CryptGetProvParam(CryptContext.Handle, PP_ENUMCONTAINERS, Data, DataLen, Flags) then
        if GetLastError = ERROR_NO_MORE_ITEMS then
          break
        else
          RaiseLastWin32Error;

      KeyName := string(Marshal.PtrToStringAnsi(Data));
      if LeftStr(KeyName, 3) = KEY_NAME_PREFIX then
        List.AddObject(Copy(KeyName, 4, MaxInt), nil);

      Flags := 0;
    end;
  finally
    Marshal.FreeHGlobal(Data);
    CryptContext.Free;
  end;

{$ELSE}
  GetKeychainObjectList(List, kSecPublicKeyItemClass, SecKeyGetTypeID);
  GetKeychainObjectList(List, kSecPrivateKeyItemClass, SecKeyGetTypeID);
{$ENDIF}
end;

procedure TScCryptoAPIStorage.GetCertificateNames(List: TStrings);
{$IFDEF MSWINDOWS}
var
  Cert: TScCertificate;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Cert := FindCertificate(nil);
  while Cert <> nil do begin
    List.AddObject(Cert.CertName, Cert);
    Cert := FindCertificate(Cert);
  end;

{$ELSE}
  GetKeychainObjectList(List, kSecCertificateItemClass, SecCertificateGetTypeID);
{$ENDIF}
end;

procedure TScCryptoAPIStorage.InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings);
begin
  if ItemClass = TScKey then
    GetKeyNames(List)
  else
  if ItemClass = TScCertificate then
    GetCertificateNames(List)
  else
    raise EScError.Create(seNotOverriddenMethod);
end;

function TScCryptoAPIStorage.GetUsers: TScUserList;
begin
  raise EScError.Create(seCryptoAPIStorageNotSupportUsers);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TScCryptoAPIStorage.GetCRLs: TScCRLList;
begin
  raise EScError.Create(seCryptoAPIStorageNotSupportCRLs);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

procedure TScCryptoAPIStorage.InternalLoad(Item: TScStorageItem);
var
{$IFDEF MSWINDOWS}
  CryptContext: TCryptContext;
  hKey: HCRYPTKEY;
  ce: PCCERT_CONTEXT;
  BlobType: Cardinal;
  Size: Cardinal;
  FriendlyName: IntPtr;
  CertName: string;
{$ELSE}
  ItemRef: SecKeychainItemRef;
  KeyExFormat: TScKeyExFormat;
  Alg: TScAsymmetricAlgorithm;
  IsPublicKey: boolean;
{$ENDIF}
  KeyBlob: TBytes;
begin
{$IFDEF MSWINDOWS}
  // TScKey
  if IsClass(Item, TScKey) then begin
    hKey := 0;
    CryptContext := TCryptContext.Create(KEY_NAME_PREFIX + TScKey(Item).KeyName,
      FProviderName, FProviderType, False);
    try
      hKey := GetUserKey(CryptContext.Handle);

      BlobType := PRIVATEKEYBLOB;
      if not CryptExportKey(hKey, 0, BlobType, 0, nil, Size) then begin
        if HRESULT(GetLastError) = NTE_BAD_KEY_STATE then begin
          BlobType := PUBLICKEYBLOB;
          Win32Check(CryptExportKey(hKey, 0, BlobType, 0, nil, Size));
        end
        else
          RaiseLastWin32Error;
      end;

      SetLength(KeyBlob, Size);
      Win32Check(CryptExportKey(hKey, 0, BlobType, 0, Windows.PBYTE(KeyBlob), Size));
    finally
      if hKey <> 0 then
        CryptDestroyKey(hKey);
      CryptContext.Free;
    end;

    TScKeyUtils.LoadKeyFromCryptoAPIFormat(TScKey(Item), KeyBlob, kefPVK, aaRSA, True);
  end
  else // TScCertificate
  if IsClass(Item, TScCertificate) then begin
    if Length(TScCertificate(Item).CertName) = 0 then
      raise EScError.Create(seNullName);

    ce := nil;
    if TScCertificate(Item).Handle <> nil then begin
      Size := 256;
      FriendlyName := Marshal.AllocHGlobal(Size);
      try
        repeat
          ce := CertFindCertificateInStore(GetStoreHandle, X509_ASN_ENCODING, 0, CERT_FIND_EXISTING,
            TScCertificate(Item).Handle, ce);

          if ce <> nil then begin
            if CertGetCertificateContextProperty(ce, CERT_FRIENDLY_NAME_PROP_ID, FriendlyName, Size) then begin
              if PWideChar(PtrOffset(FriendlyName, Size-2))^ = #0 then
                Dec(Size, 2);

              CertName := string(Marshal.PtrToStringUni(FriendlyName, Size shr 1));
            end
            else
              CertName := GetSubject(ce);
            if CertName = TScCertificate(Item).CertName then
              break;
          end;
        until ce = nil;
      finally
        Marshal.FreeHGlobal(FriendlyName);
      end;
    end;

    TScCertificate(Item).SetRawData(ce, FreeCertHandle, GetCertRawData(ce));
  end
  else
    raise EScError.Create(seNotOverriddenMethod);
{$ELSE}
  // TScKey
  if IsClass(Item, TScKey) then begin
    ItemRef := GetKeychainObject(kSecPrivateKeyItemClass, TScKey(Item).KeyName);
    try
      if ItemRef = nil then begin
        ItemRef := GetKeychainObject(kSecPublicKeyItemClass, TScKey(Item).KeyName);
        if ItemRef = nil then
          CheckError(errSecItemNotFound);
      end;

      KeyBlob := GetKeyData(ItemRef, KeyExFormat, Alg, IsPublicKey);
      TScKeyUtils.LoadKeyFromCryptoAPIFormat(TScKey(Item), KeyBlob, KeyExFormat, Alg, IsPublicKey);
    finally
      if ItemRef <> nil then
        CFRelease(ItemRef);
    end;
  end
  else // TScCertificate
  if IsClass(Item, TScCertificate) then begin
    if TScCertificate(Item).Handle = nil then
      CheckError(errSecItemNotFound); //d TODO
  end
  else
    raise EScError.Create(seNotOverriddenMethod);
{$ENDIF}
end;

procedure TScCryptoAPIStorage.InternalSave(Item: TScStorageItem);
var
  CertData: TBytes;
{$IFDEF MSWINDOWS}
  CryptContext: TCryptContext;
  hKey: HCRYPTKEY;
  KeyBlob: TBytes;
  NameBlob: CRYPTOAPI_BLOB;
  pBlob: IntPtr;
  CertContext, AddedCertContext: PCCERT_CONTEXT;
{$ELSE}
  CertHandle: IntPtr;
  Key: IntPtr;
{$ENDIF}
begin
  SetLength(CertData, 0);

{$IFDEF MSWINDOWS}
  // TScKey
  if IsClass(Item, TScKey) then begin
    hKey := 0;
    CryptContext := TCryptContext.Create(KEY_NAME_PREFIX + TScKey(Item).KeyName,
      FProviderName, FProviderType, True);
    try
      SetLength(KeyBlob, 0);
      KeyBlob := TScKeyUtils.SaveKeyToCryptoAPIFormat(TScKey(Item));
      Win32Check(CryptImportKey(CryptContext.Handle, KeyBlob, Length(KeyBlob), 0,
        CRYPT_EXPORTABLE, hKey));
    finally
      if hKey <> 0 then
        CryptDestroyKey(hKey);
      CryptContext.Free;
    end;
  end
  else // TScCertificate
  if IsClass(Item, TScCertificate) then begin
    CertData := TScCertificate(Item).GetRawData;
    CertContext := ImportCert(CertData, '');
    Assert(CertContext <> nil);

    Win32Check(CertAddCertificateContextToStore(GetStoreHandle,
      CertContext, CERT_STORE_ADD_NEW, AddedCertContext));
    TScCertificateUtils.SetHandle(TScCertificate(Item), AddedCertContext, FreeCertHandle);
    CertFreeCertificateContext(CertContext);

    NameBlob.cbData := Length(TScCertificate(Item).CertName) * 2;
    NameBlob.pbData := Marshal.StringToHGlobalUni(WideString(TScCertificate(Item).CertName));
    pBlob := @NameBlob;
    Win32Check(CertSetCertificateContextProperty(AddedCertContext, CERT_FRIENDLY_NAME_PROP_ID, 0, pBlob));
  end
  else
    raise EScError.Create(seNotOverriddenMethod);
{$ELSE}
  // TScKey
  if IsClass(Item, TScKey) then begin
    Key := ImportKeyFromBlob(TScKeyUtils.SaveKeyToCryptoAPIFormat(TScKey(Item)), TScKey(Item).IsPrivate, GetStoreHandle, nil);
    CFRelease(Key);
  end
  else // TScCertificate
  if IsClass(Item, TScCertificate) then begin
    CertData := TScCertificate(Item).GetRawData;
    CertHandle := ImportCert(CertData, '');

    TScCertificateUtils.SetHandle(TScCertificate(Item), CertHandle, FreeCertHandle);
    CheckError(SecCertificateAddToKeychain(CertHandle, GetStoreHandle));
  end
  else
    raise EScError.Create(seNotOverriddenMethod);
{$ENDIF}
end;

procedure TScCryptoAPIStorage.InternalDelete(Item: TScStorageItem);
{$IFDEF MSWINDOWS}
var
  hProv: HCRYPTPROV;
  FoundCert: PCCERT_CONTEXT;
  Hash: TBytes;
  pProvider, pContainer: IntPtr;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  // TScKey
  if IsClass(Item, TScKey) then begin
    if FProviderName = '' then
      pProvider := nil
    else
      pProvider := _StringToHGlobal(FProviderName);

    pContainer := _StringToHGlobal(KEY_NAME_PREFIX + TScKey(Item).KeyName);
    try
      Win32Check(CryptAcquireContext(hProv, pContainer,
        pProvider, FProviderType, CRYPT_DELETEKEYSET or CRYPT_SILENT));
    finally
      Marshal.FreeCoTaskMem(pContainer);
      if pProvider <> nil then
        Marshal.FreeCoTaskMem(pProvider);
    end;
  end
  else // TScCertificate
  if IsClass(Item, TScCertificate) then begin
    if TScCertificate(Item).Ready then begin
      TScCertificate(Item).GetFingerprint(haSHA1, Hash);
      FoundCert := FindCertificateByHash(Hash, haSHA1);
      if FoundCert = nil then
        raise EScError.Create(seCannotFindCertificate);

      Win32Check(CertDeleteCertificateFromStore(FoundCert));
    end;
  end
  else
    raise EScError.Create(seNotOverriddenMethod);
{$ELSE}
  raise EScError.Create(seNotOverriddenMethod); //d TODO
{$ENDIF}
end;

procedure TScCryptoAPIStorage.InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean);
{$IFDEF MSWINDOWS}
var
  CryptContext: TCryptContext;
  hProv: HCRYPTPROV;
  hKey: HCRYPTKEY;
  KeyBlob: TBytes;
  pProvider, pContainer: IntPtr;
  NameBlob: CRYPTOAPI_BLOB;
  pBlob: IntPtr;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  // TScKey
  if IsClass(Item, TScKey) then begin
    if (TScKey(Item).KeyName = '') or (NewName = '') then
      raise EScError.Create(seNullName);

    try
      TScKey(Item).Ready := True;
    except
      if CheckIfExists then
        raise
      else
        Exit;
    end;

    // new key saving
    hKey := 0;
    CryptContext := TCryptContext.Create(KEY_NAME_PREFIX + NewName, FProviderName,
      FProviderType, True);
    try
      SetLength(KeyBlob, 0);
      KeyBlob := TScKeyUtils.SaveKeyToCryptoAPIFormat(TScKey(Item));
      Win32Check(CryptImportKey(CryptContext.Handle, KeyBlob, Length(KeyBlob), 0,
        CRYPT_EXPORTABLE, hKey));
    finally
      if hKey <> 0 then
        CryptDestroyKey(hKey);
      CryptContext.Free;
    end;

    // old key deleting
    if FProviderName = '' then
      pProvider := nil
    else
      pProvider := _StringToHGlobal(FProviderName);

    pContainer := _StringToHGlobal(KEY_NAME_PREFIX + TScKey(Item).KeyName);
    try
      Win32Check(CryptAcquireContext(hProv, pContainer,
        pProvider, FProviderType, CRYPT_DELETEKEYSET or CRYPT_SILENT));
    finally
      Marshal.FreeCoTaskMem(pContainer);
      if pProvider <> nil then
        Marshal.FreeCoTaskMem(pProvider);
    end;
  end
  else // TScCertificate
  if IsClass(Item, TScCertificate) then begin
    if TScCertificate(Item).Handle <> nil then begin
      NameBlob.cbData := Length(NewName) * 2;
      NameBlob.pbData := Marshal.StringToHGlobalUni(WideString(NewName));
      pBlob := @NameBlob;
      Win32Check(CertSetCertificateContextProperty(TScCertificate(Item).Handle,
        CERT_FRIENDLY_NAME_PROP_ID, 0, pBlob));
    end;
  end
  else
    raise EScError.Create(seNotOverriddenMethod);
{$ELSE}
  raise EScError.Create(seNotOverriddenMethod); //d TODO
{$ENDIF}
end;

function TScCryptoAPIStorage.FindCertificateBySubject(DistinguishedName: TScDistinguishedName): TScCertificate;
{$IFDEF MSWINDOWS}
  function ASN1DataTypeToRDNValue(ASN1DataType: TScASN1DataType): DWORD;
  begin
    case ASN1DataType of
      dtOctetString:
        Result := CERT_RDN_OCTET_STRING;
      dtUTF8String:
        Result := CERT_RDN_UTF8_STRING;
      dtNumericString:
        Result := CERT_RDN_NUMERIC_STRING;
      dtPrintableString:
        Result := CERT_RDN_PRINTABLE_STRING;
      dtTeletexString:
        Result := CERT_RDN_TELETEX_STRING;
      dtVideotexString:
        Result := CERT_RDN_VIDEOTEX_STRING;
      dtIA5String:
        Result := CERT_RDN_IA5_STRING;
      dtGraphicString:
        Result := CERT_RDN_GRAPHIC_STRING;
      dtVisibleString:
        Result := CERT_RDN_VISIBLE_STRING;
      dtGeneralString:
        Result := CERT_RDN_GENERAL_STRING;
      dtUniversalString:
        Result := CERT_RDN_UNIVERSAL_STRING;
      dtBMPString:
        Result := CERT_RDN_BMP_STRING;
      dtSequence, dtSet:
        Result := CERT_RDN_ENCODED_BLOB;
      else
        Result := CERT_RDN_ANY_TYPE;
    end;
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
var
  FoundCert: PCCERT_CONTEXT;
  DNCount: integer;
  RDN: CERT_RDN;
  RDN_ATTR: array of CERT_RDN_ATTR;
  Oids: array of AnsiString;
  ASN1Attribute: TScASN1Attribute;
  i: integer;
  ws: WideString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  DNCount := DistinguishedName.Count;
  SetLength(RDN_ATTR, DNCount);
  SetLength(Oids, DNCount);

  RDN.cRDNAttr := DNCount;
  RDN.rgRDNAttr := @RDN_ATTR[0];
  ws := '';

  for i := 0 to DNCount - 1 do begin
    ASN1Attribute := DistinguishedName.Items[i].Items[0];
    if ASN1Attribute.OId.Value = OID_COMMON_NAME then
      ws := WideString(ASN1Attribute.AsString);

    Oids[i] := AnsiString(ASN1Attribute.OId.Value);
    RDN_ATTR[i].pszObjId := PAnsiChar(Oids[i]);
    RDN_ATTR[i].dwValueType := ASN1DataTypeToRDNValue(ASN1Attribute.ASN1DataType);

    RDN_ATTR[i].Value.cbData := Length(ASN1Attribute.RawData);
    if RDN_ATTR[i].Value.cbData > 0 then
      RDN_ATTR[i].Value.pbData := @ASN1Attribute.RawData[0];
  end;

  FoundCert := CertFindCertificateInStore(GetStoreHandle, X509_PKCS7, 0, CERT_FIND_SUBJECT_ATTR, @RDN, nil);

  Result := nil;
  if FoundCert <> nil then begin
    Result := TScCertificate.Create(nil);
    try
      Result.SetRawData(nil, nil, GetCertRawData(FoundCert));
      FreeCertHandle(FoundCert);
    except
      Result.Free;
      raise;
    end;
  end
  else
  if ws <> '' then begin
    while True do begin
      FoundCert := CertFindCertificateInStore(GetStoreHandle, X509_PKCS7, 0, CERT_FIND_SUBJECT_STR, PWideChar(ws), FoundCert);
      if FoundCert = nil then
        break;

      Result := TScCertificate.Create(nil);
      try
        Result.SetRawData(nil, nil, GetCertRawData(FoundCert));
        if WideString(Result.Subject) = ws then begin
          FreeCertHandle(FoundCert);
          Exit;
        end
        else
          FreeAndNil(Result);
      except
        Result.Free;
        raise;
      end;
    end;
  end;
{$ELSE}
  Result := nil; //d TODO
{$ENDIF}
end;

function TScCryptoAPIStorage.CheckCertificateByIssuer(Certificate: TScCertificate): TScCertificateStatus;
{$IFDEF MSWINDOWS}
var
  FoundCert: PCCERT_CONTEXT;
  Flags: cardinal;
  ws: WideString;
{$ELSE}
{$IFNDEF IOS}
var
  SearchAttrList: SecKeychainAttributeList;
  SearchAttr: SecKeychainAttribute;
  SearchRef: SecKeychainSearchRef;
  ItemRef: SecKeychainItemRef;
  sa: AnsiString;
  Status: OSStatus;
  FoundCert: TScCertificate;
  StatusSet: TScCertificateStatusSet;
  CertStatus: TScCertificateStatus;
{$ENDIF}
{$ENDIF}
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

{$IFDEF MSWINDOWS}
  if Certificate.Handle = nil then
    raise EScError.Create(seInvalidInputArgs);

  Flags := CERT_STORE_SIGNATURE_FLAG;
  FoundCert := CertGetIssuerCertificateFromStore(GetStoreHandle, Certificate.Handle, nil, Flags);
  CertFreeCertificateContext(FoundCert);

  if FoundCert = nil then begin
    ws := WideString(Certificate.Subject);
    FoundCert := CertFindCertificateInStore(GetStoreHandle, X509_ASN_ENCODING, 0, CERT_FIND_SUBJECT_STR, PWideChar(ws), nil);
  end;

  if FoundCert = nil then
    Result := csUntrustedRoot
  else
  if Flags > 0 then begin
    case Flags of
      CERT_STORE_SIGNATURE_FLAG:
        Result := csInvalidSignature;
      CERT_STORE_TIME_VALIDITY_FLAG:
        Result := csExpired;
      CERT_STORE_SIGNATURE_FLAG or CERT_STORE_TIME_VALIDITY_FLAG:
        Result := csUntrustedRoot;
    else
      Result := csOtherError;
    end;
  end
  else
    Result := csValid;
{$ELSE}
{$IFNDEF IOS}
  sa := {$IFDEF SBRIDGE}ScFunctions{$ELSE}CRFunctions{$ENDIF}.Utf8Encode({$IFDEF FPC}WideString{$ENDIF}(Certificate.Issuer));
  SearchAttr.Tag := kSecKeyPrintName;
  SearchAttr.Length := LengthA(sa);
  SearchAttr.Data := PAnsiChar(sa);
  SearchAttrList.Count := 1;
  SearchAttrList.Attr := @SearchAttr;
  CheckError(SecKeychainSearchCreateFromAttributes(GetStoreHandle, kSecCertificateItemClass, @SearchAttrList, SearchRef));
  try
    Status := SecKeychainSearchCopyNext(SearchRef, ItemRef);
    while Status = 0 do begin
      try
        FoundCert := TScCertificate.Create(nil);
        try
          FoundCert.SetRawData(CFRetain(ItemRef), FreeCertHandle, GetCertRawData(ItemRef));

          if Certificate.IssuerName.Equals(FoundCert.SubjectName) then begin
            StatusSet := [];
            Certificate.VerifyCertificateChain(nil, FoundCert, StatusSet);

            Result := csValid;
            if StatusSet <> [] then begin
              for CertStatus := High(TScCertificateStatus) downto Low(TScCertificateStatus) do
                if CertStatus in StatusSet then begin
                  Result := CertStatus;
                  break;
                end
            end;

            Exit;
          end;
        finally
          FoundCert.Free;
        end;
      finally
        if ItemRef <> nil then
          CFRelease(ItemRef);
      end;

      Status := SecKeychainSearchCopyNext(SearchRef, ItemRef);
    end;

    if Status = errSecItemNotFound then
      Result := csUntrustedRoot
    else begin
      CheckError(Status);
      Result := csOtherError;
    end;
  finally
    if SearchRef <> nil then
      CFRelease(SearchRef);
  end;
{$ELSE}
  Result := CheckTrustCertificateByIssuer(Certificate);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF IOS}
class function TScCryptoAPIStorage.CheckTrustCertificateByIssuer(Certificate: TScCertificate): TScCertificateStatus;
var
  Buf: TBytes;
  CertRef: IntPtr;
  Certs: array[0..0] of IntPtr;
  CFData: CFDataRef;
  ArrayRef: CFArrayRef;
  Policy: SecPolicyRef;
  Trust: SecTrustRef;
  TrustResult: SecTrustResultType;
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  Result := csOtherError;

  CFData := nil;
  CertRef := nil;
  ArrayRef := nil;
  Policy := nil;
  Trust := nil;
  try
    Buf := Certificate.GetRawData;
    CFData := CFDataCreateWithBytesNoCopy(nil, @buf[0], Length(buf), kCFAllocatorNull);
    CertRef := SecCertificateCreateWithData(kCFAllocatorMalloc, CFData);
    if CertRef = nil then
      raise EScError.Create(seNonCertificate);

    Certs[0] := CertRef;
    ArrayRef := CFArrayCreate(kCFAllocatorMalloc, @Certs[0], 1, nil);
    Policy := SecPolicyCreateBasicX509();
    CheckError(SecTrustCreateWithCertificates(ArrayRef, Policy, Trust));
    CheckError(SecTrustEvaluate(Trust, TrustResult));

    if TrustResult in [kSecTrustResultProceed, kSecTrustResultUnspecified] then
      Result := csValid
    else
    if TrustResult in [kSecTrustResultRecoverableTrustFailure, kSecTrustResultFatalTrustFailure] then
      Result := csUntrustedRoot;
  finally
    if CFData <> nil then
      CFRelease(CFData);
    if CertRef <> nil then
      CFRelease(CertRef);
    if ArrayRef <> nil then
      CFRelease(ArrayRef);
    if Policy <> nil then
      CFRelease(Policy);
    if Trust <> nil then
      CFRelease(Trust);
  end;
end;
{$ENDIF}

procedure TScCryptoAPIStorage.InternalDeleteStorage;
begin
  Keys.Clear;

{$IFDEF MSWINDOWS}
  Init(CERT_STORE_DELETE_FLAG); // deleting of certificates
{$ENDIF}
  Invalidate;
end;

{$IFNDEF MSWINDOWS}
function CFStringToStr(Value: CFStringRef): WideString;
var
  Range: CFRange;
begin
  Result := '';
  if Value = nil then
    Exit;

  Range := CFRangeMake(0, CFStringGetLength(Value));
  if Range.Length > 0 then begin
    SetLength(Result, Range.Length);
    CFStringGetCharacters(Value, Range, @Result[1]);
  end;
end;

procedure CheckError(Status: OSStatus; const Message: string = '');
var
  StrBuf: CFStringRef;
  Err: string;
begin
  if Status <> 0 then begin
    if _Assigned(@SecCopyErrorMessageString) then begin
      StrBuf := SecCopyErrorMessageString(Status, nil);
      try
        Err := {$IFDEF FPC}string{$ENDIF}(CFStringToStr(StrBuf));
      finally
        if StrBuf <> nil then
          CFRelease(StrBuf);
      end;
    end
    else
      Err := IntToStr(Status);

    Err := Message + Err;
    raise EScError.CreateFmt(Err, [], seCSPError);
  end;
end;

{$IFDEF UNIX}
procedure free(P : pointer); cdecl; external 'StdCLib' name 'free';
function memcmp(__s1:pointer; __s2:pointer; __n:size_t):integer;cdecl;external 'StdCLib' name 'memcmp';
{$ENDIF}

procedure FreeMem(Memblock: PVOID);
begin
  free(Memblock);
end;
{$ENDIF}

function TScCryptoAPIStorage.ImportCert(const Data: TBytes; const Password: string): IntPtr;
var
{$IFDEF MSWINDOWS}
  CData: IntPtr;
{$IFNDEF BCB}
  PFXBlob: CRYPTOAPI_BLOB;
  pPFX: IntPtr;
  hStore: HCERTSTORE;
  ws: WideString;
{$ENDIF}
{$ELSE}
  CFData: CFDataRef;
  StrBuf: CFStringRef;
{$ENDIF}
begin
  if Length(Data) = 0 then
    raise EScError.Create(seNonCertificate);

{$IFDEF MSWINDOWS}
  CData := @Data[0];
  Result := CertCreateCertificateContext(X509_PKCS7, CData, Length(Data));

{$IFNDEF BCB}
  if Result = nil then begin
    PFXBlob.pbData := CData;
    PFXBlob.cbData := Length(Data);
    ws := WideString(Password);
    pPFX := @PFXBlob;
    hStore := PFXImportCertStore(pPFX, PWideChar(ws), CRYPT_EXPORTABLE);
    if (hStore = nil) and (Password = '') then
      hStore := PFXImportCertStore(pPFX, nil, CRYPT_EXPORTABLE);
    Win32Check(hStore <> nil);

    try
      Result := CertFindCertificateInStore(hStore, X509_ASN_ENCODING, 0, CERT_FIND_ANY, nil, nil);
      Win32Check(Result <> nil);
    finally
      CertCloseStore(hStore, 0);
    end;
  end;
{$ENDIF}

  Win32Check(Result <> nil);

{$ELSE MSWINDOWS}
  Result := nil;
  try
    CFData := CFDataCreateWithBytesNoCopy(nil, @Data[0], Length(Data), kCFAllocatorNull);
    Result := SecCertificateCreateWithData(kCFAllocatorMalloc, CFData);
    CFRelease(CFData);
    if Result = nil then
      raise EScError.Create(seNonCertificate);

    if _Assigned(@SecCertificateCopyCommonName) then begin
      StrBuf := nil;
      CheckError(SecCertificateCopyCommonName(Result, StrBuf)); // checking if correct importing
      if StrBuf <> nil then
        CFRelease(StrBuf);
    end;
  except
    if Result <> nil then
      CFRelease(Result);
    raise;
  end;
{$ENDIF}
end;

function TScCryptoAPIStorage.GetCertRawData(CertHandle: IntPtr): TBytes;
var
{$IFDEF MSWINDOWS}
  CertContext: PCCERT_CONTEXT;
{$ELSE}
  DataRef: CFDataRef;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if CertHandle <> nil then begin
    CertContext := PCCERT_CONTEXT(CertHandle);
    SetLength(Result, CertContext.cbCertEncoded);
    Marshal.Copy(CertContext.pbCertEncoded, Result, 0, Length(Result));
  end
  else
    SetLength(Result, 0);
{$ELSE}
  DataRef := SecCertificateCopyData(CertHandle);
  if DataRef <> nil then begin
    SetLength(Result, CFDataGetLength(DataRef));
    Marshal.Copy(CFDataGetBytePtr(DataRef), Result, 0, Length(Result));
    CFRelease(DataRef);
  end
  else
    SetLength(Result, 0);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
class function TScCryptoAPIStorage.GetCertHash(CertHandle: IntPtr; HashAlg: TScHashAlgorithm): TBytes;
var
  HashType: Cardinal;
  Hash: IntPtr;
  Size: Cardinal;
begin
  HashType := CERT_HASH_PROP_ID;
  case HashAlg of
    haSHA1:
      HashType := CERT_SHA1_HASH_PROP_ID;
    haMD5:
      HashType := CERT_MD5_HASH_PROP_ID;
  else
    Assert(False);
  end;

  Size := 32;
  Hash := Marshal.AllocHGlobal(Size);
  try
    if (not CertGetCertificateContextProperty(CertHandle, HashType, Hash, Size))
      or (Size <= 0) or (Size > 32) then
      raise EScError.CreateFmt(SErrorRetrievingHash, [GetSysErrorMessage], seCSPError);

    SetLength(Result, Size);
    Marshal.Copy(Hash, Result, 0, Size);
  finally
    Marshal.FreeHGlobal(Hash);
  end;
end;
{$ENDIF}

function TScCryptoAPIStorage.GetSubject(CertHandle: IntPtr): string;
var
{$IFDEF MSWINDOWS}
  Length: Integer;
  Name: IntPtr;
  s: {$IFDEF UNICODE}WideString{$ELSE}string{$ENDIF};
{$ELSE}
  StrBuf: CFStringRef;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Length := CertGetNameString(CertHandle, CERT_NAME_SIMPLE_DISPLAY_TYPE,
    CERT_NAME_DISABLE_IE4_UTF8_FLAG, nil, nil, 0);
  if Length <= 0 then
    raise EScError.Create(seErrorRequestingSubject);

  SetLength(s, Length);
  Name := _StringToHGlobal(s);
  CertGetNameString(CertHandle, CERT_NAME_SIMPLE_DISPLAY_TYPE,
    CERT_NAME_DISABLE_IE4_UTF8_FLAG, nil, Name, Length);
  Result := _PtrToString(Name);
  Marshal.FreeCoTaskMem(Name);

{$ELSE}
  StrBuf := SecCertificateCopySubjectSummary(CertHandle);
  try
    Result := {$IFDEF FPC}string{$ENDIF}(CFStringToStr(StrBuf));
  finally
    if StrBuf <> nil then
      CFRelease(StrBuf);
  end;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
class function TScCryptoAPIStorage.GetUserKey(hProv: HCRYPTPROV; CertHandle: IntPtr = nil): HCRYPTKEY;
var
  pkInfo: PCERT_PUBLIC_KEY_INFO;
begin
  Result := 0;
  if not CryptGetUserKey(hProv, AT_KEYEXCHANGE, Result) then begin
    if HRESULT(GetLastError) = NTE_NO_KEY then begin
      if not CryptGetUserKey(hProv, AT_SIGNATURE, Result) then begin
        if (HRESULT(GetLastError) = NTE_NO_KEY) and (CertHandle <> nil) then begin
          pkInfo := @PCCERT_CONTEXT(CertHandle).pCertInfo.SubjectPublicKeyInfo;
          Win32Check(CryptImportPublicKeyInfoEx(hProv, X509_PKCS7, pkInfo, CALG_RSA_SIGN, 0, nil, Result));
        end
        else
          RaiseLastWin32Error;
      end;
    end
    else
      RaiseLastWin32Error;
  end;
end;

{$ELSE}

class function TScCryptoAPIStorage.ImportKeyFromBlob(const KeyBlob: TBytes;
  IsPrivate: boolean; Keychain: SecKeychainRef; KeyRef: IntPtr): IntPtr;
var
  CFData: CFDataRef;
  InputFormat: SecExternalFormat;
  ItemType: SecExternalItemType;
  OutItems: CFArrayRef;
  Key: CFTypeRef;
  Keys: array [0..3] of pointer;
  Values: array [0..3] of pointer;
  SearchAttrs, UpdAttrs: CFDictionaryRef;
  st: OSStatus;
begin
  if Length(KeyBlob) = 0 then
    raise EScError.Create(seBadKeyData);

  Result := nil;
  CFData := CFDataCreateWithBytesNoCopy(nil, @KeyBlob[0], Length(KeyBlob), kCFAllocatorNull);
  try
    if _Assigned(@SecKeychainItemImport) then begin
      OutItems := nil;
      try
        InputFormat := kSecFormatUnknown; //kSecFormatOpenSSL;
        if IsPrivate then
          ItemType := kSecItemTypePrivateKey
        else
          ItemType := kSecItemTypePublicKey;

        CheckError(SecKeychainItemImport(CFData, nil, @InputFormat, @ItemType, 0, nil, Keychain, OutItems));

        if CFArrayGetCount(OutItems) < 1 then
          raise EScError.Create(seBadKeyData);

        Key := CFArrayGetValueAtIndex(OutItems, 0);
        if (Key = nil) or (CFGetTypeID(Key) <> SecKeyGetTypeID) then
          raise EScError.Create(seBadKeyData);

        Result := CFRetain(Key);
      finally
        if OutItems <> nil then
          CFRelease(OutItems);
      end;
    end
    else begin
      Keys[0] := kSecValueData;
      Values[0] := CFData;
      Keys[1] := kSecAttrKeyClass;
      Values[1] := kSecAttrKeyClassPrivate;
      UpdAttrs := CFDictionaryCreate(nil, @Keys, @Values, 2, nil, nil);

      Keys[0] := kSecClass;
      Values[0] := kSecClassKey;
      Keys[1] := kSecValueRef;
      Values[1] := KeyRef;
      SearchAttrs := CFDictionaryCreate(nil, @Keys, @Values, 2, nil, nil);

      try
        st := SecItemUpdate(SearchAttrs, UpdAttrs);
        if st <> errSecDuplicateItem then
          CheckError(st);

        Keys[0] := kSecClass;
        Values[0] := kSecClassKey;
        Keys[1] := kSecValueRef;
        Values[1] := KeyRef;
        Keys[2] := kSecAttrKeyClass;
        Values[2] := kSecAttrKeyClassPrivate;
        Keys[3] := kSecReturnRef;
        Values[3] := kCFBooleanTrue;
        CFRelease(SearchAttrs);
        SearchAttrs := CFDictionaryCreate(nil, @Keys, @Values, 4, nil, nil);
        CheckError(SecItemCopyMatching(SearchAttrs, Result));
      finally
        CFRelease(UpdAttrs);
        CFRelease(SearchAttrs);
      end;
    end;
  finally
    CFRelease(CFData);
  end;
end;
{$ENDIF}

function TScCryptoAPIStorage.GetCertKey(CertHandle: IntPtr;
  out KeyExFormat: TScKeyExFormat; out Alg: TScAsymmetricAlgorithm;
  out IsPublicKey: boolean): TBytes;
var
{$IFDEF MSWINDOWS}
  hProv: HCRYPTPROV;
  KeySpec: Cardinal;
  CallerFreeProv: BOOL;
  hKey: HCRYPTKEY;
  pkInfo: PCERT_PUBLIC_KEY_INFO;
  Size: Cardinal;
  HasPrivateKey: Boolean;
  BlobType: Cardinal;
  CertHash: TBytes;
{$ELSE}
  st: OSStatus;
  Identity: SecIdentityRef;
  Key: SecKeyRef;
  Policy: SecPolicyRef;
  Trust: SecTrustRef;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  KeyExFormat := kefPVK;

  hKey := 0;
  HasPrivateKey := CryptAcquireCertificatePrivateKey(CertHandle,
    CRYPT_ACQUIRE_COMPARE_KEY_FLAG or CRYPT_ACQUIRE_SILENT_FLAG, nil,
    hProv, KeySpec, CallerFreeProv);

  try
    if HasPrivateKey then begin
      Win32Check(CryptGetUserKey(hProv, KeySpec, hKey));
      BlobType := PRIVATEKEYBLOB;

      if not CryptExportKey(hKey, 0, BlobType, 0, nil, Size) then begin
        if HRESULT(GetLastError) = NTE_BAD_KEY_STATE then begin
          BlobType := PUBLICKEYBLOB;
          Win32Check(CryptExportKey(hKey, 0, BlobType, 0, nil, Size));
        end
        else
          RaiseLastWin32Error;
      end;
    end
    else begin
      pkInfo := @PCCERT_CONTEXT(CertHandle).pCertInfo.SubjectPublicKeyInfo;

      SetLength(CertHash, 0);
      CertHash := GetCertHash(CertHandle, haSHA1);
      hProv := CryptGetContext(BytesToHexStr(CertHash, ''), '', PROV_RSA_FULL, True);
      Win32Check(hProv <> 0);

      Win32Check(CryptImportPublicKeyInfoEx(hProv, X509_PKCS7, pkInfo, CALG_RSA_SIGN, 0, nil, hKey));
      BlobType := PUBLICKEYBLOB;
      Win32Check(CryptExportKey(hKey, 0, BlobType, 0, nil, Size));
    end;

    SetLength(Result, Size);
    Win32Check(CryptExportKey(hKey, 0, BlobType, 0, Windows.PBYTE(Result), Size));
  finally
    if hKey <> 0 then
      CryptDestroyKey(hKey);
    if hProv <> 0 then
      CryptReleaseContext(hProv, 0);
  end;

{$ELSE MSWINDOWS}
  Key := nil;
  Identity := nil;
  Policy := nil;
  Trust := nil;

  try
    if _Assigned(@SecIdentityCreateWithCertificate) then begin
      st := SecIdentityCreateWithCertificate(nil, CertHandle, Identity);
      if st = 0 then
        CheckError(SecIdentityCopyPrivateKey(Identity, Key))
      else
        CheckError(SecCertificateCopyPublicKey(CertHandle, Key));
    end
    else begin
      Policy := SecPolicyCreateBasicX509;
      CheckError(SecTrustCreateWithCertificates(CertHandle, Policy, Trust));
      Key := SecTrustCopyPublicKey(Trust);
      if Key = nil then
        CheckError(errSecItemNotFound);
    end;

    Result := GetKeyData(Key, KeyExFormat, Alg, IsPublicKey);
  finally
    if Key <> nil then
      CFRelease(Key);
    if Identity <> nil then
      CFRelease(Identity);
    if Trust <> nil then
      CFRelease(Trust);
    if Policy <> nil then
      CFRelease(Policy);
  end;
{$ENDIF}
end;
(*
class function TScCryptoAPIStorage.GetCertKeyRef(CertHandle: IntPtr): IntPtr;
var
{$IFDEF MSWINDOWS}
  hProv: HCRYPTPROV;
  KeySpec: Cardinal;
  CallerFreeProv: BOOL;
  hKey: HCRYPTKEY;
  HasPrivateKey: Boolean;
  CertHash: TBytes;
{$ELSE}
  st: OSStatus;
  Identity: SecIdentityRef;
  Policy: SecPolicyRef;
  Trust: SecTrustRef;
  Key: SecKeyRef;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  hKey := 0;
  HasPrivateKey := CryptAcquireCertificatePrivateKey(CertHandle,
    CRYPT_ACQUIRE_COMPARE_KEY_FLAG or CRYPT_ACQUIRE_SILENT_FLAG, nil,
    hProv, KeySpec, CallerFreeProv);

  try
    if HasPrivateKey then
      Win32Check(CryptGetUserKey(hProv, KeySpec, hKey))
    else begin
      SetLength(CertHash, 0);
      CertHash := GetCertHash(CertHandle, haSHA1);
      hProv := CryptGetContext(BytesToHexStr(CertHash, ''), '', PROV_RSA_FULL, True);
      Win32Check(hProv <> 0);
    end;

    Result := NativeUIntToIntPtr(hProv);
  finally
    if hKey <> 0 then
      CryptDestroyKey(hKey);
  end;

{$ELSE MSWINDOWS}
  Key := nil;
  Identity := nil;
  Policy := nil;
  Trust := nil;

  try
    if _Assigned(@SecIdentityCreateWithCertificate) then begin
      st := SecIdentityCreateWithCertificate(nil, CertHandle, Identity);
      if st = 0 then
        CheckError(SecIdentityCopyPrivateKey(Identity, Key))
      else
        CheckError(SecCertificateCopyPublicKey(CertHandle, Key));
    end
    else begin
      Policy := SecPolicyCreateBasicX509;
      CheckError(SecTrustCreateWithCertificates(CertHandle, Policy, Trust));
      Key := SecTrustCopyPublicKey(Trust);
      if Key = nil then
        CheckError(errSecItemNotFound);
    end;

    Result := Key;
  finally
    if Identity <> nil then
      CFRelease(Identity);
    if Trust <> nil then
      CFRelease(Trust);
    if Policy <> nil then
      CFRelease(Policy);
  end;
{$ENDIF}
end;

class procedure TScCryptoAPIStorage.AssociateWithPrivateKey(CertHandle: IntPtr; const KeyBlob: TBytes);
var
  KeyRef: IntPtr;
{$IFDEF MSWINDOWS}
  hProv: HCRYPTPROV;
  hKey: HCRYPTKEY;
  KPI: CRYPT_KEY_PROV_INFO;
  pKPI: IntPtr;
  ContainerName: string;
  KeySpec: Cardinal;
  CallerFreeProv: BOOL;
  Data: IntPtr;
  DataLen: Cardinal;
{$ELSE}
  Key: IntPtr;
{$ENDIF}
begin
  KeyRef := GetCertKeyRef(CertHandle);
{$IFDEF MSWINDOWS}
  Win32Check(CryptGetProvParam(HCRYPTPROV(IntPtrToNativeUInt(KeyRef)), PP_CONTAINER, nil, DataLen, 0));
  Data := Marshal.AllocHGlobal(DataLen);
  try
    Win32Check(CryptGetProvParam(HCRYPTPROV(IntPtrToNativeUInt(KeyRef)), PP_CONTAINER, Data, DataLen, 0));
    ContainerName := string(Marshal.PtrToStringAnsi(Data));
  finally
    Marshal.FreeHGlobal(Data);
  end;

  if not CryptImportKey(HCRYPTPROV(IntPtrToNativeUInt(KeyRef)), KeyBlob, Length(KeyBlob), 0, CRYPT_EXPORTABLE, hKey) then
    raise EScError.CreateFmt(SBadKeyData, [GetSysErrorMessage], seBadKeyData);
  try
    KPI.pwszContainerName := Marshal.StringToHGlobalUni(WideString(ContainerName));
    KPI.pwszProvName := nil;
    KPI.dwProvType := PROV_RSA_FULL;
    KPI.dwFlags := 0;
    KPI.cProvParam := 0;
    KPI.rgProvParam := nil;
    KPI.dwKeySpec := AT_KEYEXCHANGE;

    pKPI := @KPI;
    if not CertSetCertificateContextProperty(CertHandle, CERT_KEY_PROV_INFO_PROP_ID, 0, pKPI) then
      raise EScError.CreateFmt(SCanNotAssociatePrivateKey, [GetSysErrorMessage], seCSPError);
  finally
    CryptDestroyKey(hKey);
  end;

  if not CryptAcquireCertificatePrivateKey(CertHandle,
           CRYPT_ACQUIRE_COMPARE_KEY_FLAG or CRYPT_ACQUIRE_SILENT_FLAG, nil,
           hProv, KeySpec, CallerFreeProv) then
    raise EScError.CreateFmt(SCanNotAssociatePrivateKey, [GetSysErrorMessage], seCSPError);

  if KeyRef <> nil then
    CryptReleaseContext(HCRYPTPROV(IntPtrToNativeUInt(KeyRef)), 0);
  if hProv <> 0 then
    CryptReleaseContext(hProv, 0);
{$ELSE}
  Key := ImportKeyFromBlob(KeyBlob, True, nil, KeyRef); //d TODO: try using kSecPublicKeyHashItemAttr
  if Key <> nil then
    CFRelease(Key);
  if KeyRef <> nil then
    CFRelease(KeyRef);
{$ENDIF}
end;
*)

initialization
{$IFDEF MSWINDOWS}
  CSPLock := TCriticalSection.Create;
  CSP := nil;
{$ENDIF}

finalization
{$IFDEF MSWINDOWS}
  CSP.Free;
  CSPLock.Free;
{$ENDIF}

{$ENDIF ANDROID}
{$ENDIF LINUX_BSD}
{$ENDIF LINUX}

end.
