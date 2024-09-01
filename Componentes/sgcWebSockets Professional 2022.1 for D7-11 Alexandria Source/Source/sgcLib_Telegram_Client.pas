{ ***************************************************************************
  sgcLib Telegram component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcLib_Telegram_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_TELEGRAM}

uses
  Classes, SysUtils,
{$IFDEF NEXTGEN} System.Generics.Collections, {$ELSE} Contnrs, {$ENDIF}
  // sgc
  sgcBase_Classes, sgcJSON, sgcWebSocket_Types;

{$IFDEF NEXTGEN}

Type
  PAnsiChar = MarshaledAString;
{$IFDEF DXE8}
{$IFNDEF D10_1}
{$IFDEF IOS64}
  AnsiString = String;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF D10_1}
  AnsiString = UTF8String;
{$ENDIF}
{$ENDIF}

type

  TsgcTelegramInlineKeyboardButtonType = (tikbCallback, tikbUrl);
  TsgcTelegramKeyboardButtonType = (tkbtRequestLocation, tkbtRequestPhoneNumber,
    tkbtText);
  TsgcTelegramSuperGroupMembersFilter = (tsgmFilterNone,
    tsgmFilterAdministrators, tsgmFilterBanned, tsgmFilterBots,
    tsgmFilterContacts, tsgmFilterMention, tsgmFilterRecent,
    tsgmFilterRestricted, tsgmFilterSearch);

  TsgcTelegramMessageBase = class
  private
    FCaptionText: String;
    FChatId: String;
    FMessageId: Int64;
    FSenderUserId: Int64;
  protected
    procedure ReadJSON(const aJSON: String); virtual;
    procedure DoReadContent(const aJSON: TsgcObjectJSON); virtual; abstract;
  public
    property CaptionText: String read FCaptionText write FCaptionText;
    property ChatId: String read FChatId write FChatId;
    property MessageId: Int64 read FMessageId write FMessageId;
    property SenderUserId: Int64 read FSenderUserId write FSenderUserId;
  end;

  TsgcTelegramMessageText = class(TsgcTelegramMessageBase)
  private
    FText: String;
  protected
    procedure DoReadContent(const aJSON: TsgcObjectJSON); override;
  public
    property Text: String read FText write FText;
  end;

  TsgcTelegramMessageDocument = class(TsgcTelegramMessageBase)
  private
    FFileName: String;
    FDocumentId: string;
    FLocalPath: String;
    FMimeType: String;
    FRemoteDocumentId: String;
    FSize: Int64;
  protected
    procedure DoReadContent(const aJSON: TsgcObjectJSON); override;
  public
    property FileName: String read FFileName write FFileName;
    property DocumentId: string read FDocumentId write FDocumentId;
    property LocalPath: String read FLocalPath write FLocalPath;
    property MimeType: String read FMimeType write FMimeType;
    property RemoteDocumentId: String read FRemoteDocumentId
      write FRemoteDocumentId;
    property Size: Int64 read FSize write FSize;
  end;

  TsgcTelegramMessagePhoto = class(TsgcTelegramMessageBase)
  private
    FLocalPath: String;
    FPhotoId: string;
    FRemotePhotoId: String;
    FSize: Int64;
  protected
    procedure DoReadContent(const aJSON: TsgcObjectJSON); override;
  public
    property LocalPath: String read FLocalPath write FLocalPath;
    property PhotoId: string read FPhotoId write FPhotoId;
    property RemotePhotoId: String read FRemotePhotoId write FRemotePhotoId;
    property Size: Int64 read FSize write FSize;
  end;

  TsgcTelegramMessageVideo = class(TsgcTelegramMessageBase)
  private
    FLocalPath: String;
    FRemoteVideoId: String;
    FDuration: Int64;
    FHeight: Integer;
    FSize: Int64;
    FVideoId: string;
    FWidth: Integer;
  protected
    procedure DoReadContent(const aJSON: TsgcObjectJSON); override;
  public
    property LocalPath: String read FLocalPath write FLocalPath;
    property RemoteVideoId: String read FRemoteVideoId write FRemoteVideoId;
    property Duration: Int64 read FDuration write FDuration;
    property Height: Integer read FHeight write FHeight;
    property Size: Int64 read FSize write FSize;
    property VideoId: string read FVideoId write FVideoId;
    property Width: Integer read FWidth write FWidth;
  end;

  TsgcTelegramChat = class
  private
    FChatId: String;
    FChatType: String;
    FIsChannel: Boolean;
    FSuperGroupId: Int64;
    FTitle: String;
  protected
    procedure ReadJSON(const aJSON: String); virtual;
  public
    property ChatId: String read FChatId write FChatId;
    property ChatType: String read FChatType write FChatType;
    property IsChannel: Boolean read FIsChannel write FIsChannel;
    property SuperGroupId: Int64 read FSuperGroupId write FSuperGroupId;
    property Title: String read FTitle write FTitle;
  end;

  TsgcTelegramInlineKeyboardButton = class
  private
    FButtonType: TsgcTelegramInlineKeyboardButtonType;
    FText: string;
  protected
    property ButtonType: TsgcTelegramInlineKeyboardButtonType read FButtonType;
  public
    property Text: string read FText write FText;
  end;

  TsgcTelegramInlineKeyboardButtonTypeCallback = class
    (TsgcTelegramInlineKeyboardButton)
  private
    FData: String;
  public
    constructor Create; virtual;
  public
    property Data: String read FData write FData;
  end;

  TsgcTelegramInlineKeyboardButtonTypeUrl = class
    (TsgcTelegramInlineKeyboardButton)
  private
    FUrl: string;
  public
    constructor Create; virtual;
  public
    property Url: string read FUrl write FUrl;
  end;

  TsgcTelegramInlineKeyboardButtons = class
    (TList{$IFDEF NEXTGEN}<TsgcTelegramInlineKeyboardButton>{$ENDIF})
  public
    destructor Destroy; override;
  public
    procedure Clear; {$IFNDEF NEXTGEN}override; {$ENDIF}
  public
    procedure AddButtonTypeCallback(aText, aData: String);
    procedure AddButtonTypeURL(aText, aURL: String);
  end;

  TsgcTelegramReplyMarkupInlineKeyboard = class
  private
    FButtons: TsgcTelegramInlineKeyboardButtons;
    function GetButtons: TsgcTelegramInlineKeyboardButtons;
  protected
    function GetJSONText: string;
  protected
    property Buttons: TsgcTelegramInlineKeyboardButtons read GetButtons
      write FButtons;
  public
    destructor Destroy; override;
  public
    procedure AddButtonTypeCallback(aText, aData: String);
    procedure AddButtonTypeURL(aText, aURL: String);
  end;

  TsgcTelegramKeyboardButton = class
  private
    FButtonType: TsgcTelegramKeyboardButtonType;
    FText: string;
  protected
    property ButtonType: TsgcTelegramKeyboardButtonType read FButtonType;
  public
    property Text: string read FText write FText;
  end;

  TsgcTelegramKeyboardButtonTypeRequestLocation = class
    (TsgcTelegramKeyboardButton)
  public
    constructor Create; virtual;
  end;

  TsgcTelegramKeyboardButtonTypeRequestPhoneNumber = class
    (TsgcTelegramKeyboardButton)
  public
    constructor Create; virtual;
  end;

  TsgcTelegramKeyboardButtonTypeText = class(TsgcTelegramKeyboardButton)
  public
    constructor Create; virtual;
  end;

  TsgcTelegramKeyboardButtons = class
    (TList{$IFDEF NEXTGEN}<TsgcTelegramKeyboardButton>{$ENDIF})
  public
    destructor Destroy; override;
  public
    procedure Clear; {$IFNDEF NEXTGEN}override; {$ENDIF}
  public
    procedure AddButtonTypeRequestLocation(aText: String);
    procedure AddButtonTypeRequestPhoneNumber(aText: String);
    procedure AddButtonTypeText(aText: String);
  end;

  TsgcTelegramReplyMarkupShowKeyboard = class
  private
    FButtons: TsgcTelegramKeyboardButtons;
    FOneTime: Boolean;
    FResizeKeyboard: Boolean;
    function GetButtons: TsgcTelegramKeyboardButtons;
  protected
    function GetJSONText: string;
  protected
    property Buttons: TsgcTelegramKeyboardButtons read GetButtons
      write FButtons;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure AddButtonTypeRequestLocation(aText: String);
    procedure AddButtonTypeRequestPhoneNumber(aText: String);
    procedure AddButtonTypeText(aText: String);
  public
    property OneTime: Boolean read FOneTime write FOneTime;
    property ResizeKeyboard: Boolean read FResizeKeyboard write FResizeKeyboard;
  end;

  TsgcTelegramCallbackQueryPayloadData = class
  private
    FData: string;
  public
    property Data: string read FData write FData;
  end;

  TsgcTelegramCallbackQuery = class
  private
    FChatId: String;
    FChatInstance: string;
    FId: string;
    FMessageId: Int64;
    FPayLoadData: TsgcTelegramCallbackQueryPayloadData;
    FSenderUserId: Int64;
    function GetPayLoadData: TsgcTelegramCallbackQueryPayloadData;
  protected
    procedure ReadJSON(const aJSON: String); virtual;
  public
    destructor Destroy; override;
  public
    property ChatId: String read FChatId write FChatId;
    property ChatInstance: string read FChatInstance write FChatInstance;
    property Id: string read FId write FId;
    property MessageId: Int64 read FMessageId write FMessageId;
    property PayLoadData: TsgcTelegramCallbackQueryPayloadData
      read GetPayLoadData write FPayLoadData;
    property SenderUserId: Int64 read FSenderUserId write FSenderUserId;
  end;

  TsgcTelegramConnectionStatusEvent = procedure(Sender: TObject;
    const Status: String) of object;
  TsgcTelegramAuthorizationStatusEvent = procedure(Sender: TObject;
    const Status: String) of object;
  TsgcTelegramBeforeReadEvent = procedure(Sender: TObject; const Text: String;
    var Handled: Boolean) of object;
  TsgcTelegramAuthenticationCodeEvent = procedure(Sender: TObject;
    var Code: String) of object;
  TsgcTelegramRegisterUserEvent = procedure(Sender: TObject;
    var FirstName, LastName: String) of object;
  TsgcTelegramAuthenticationPasswordEvent = procedure(Sender: TObject;
    var Password: String) of object;
  TsgcTelegramExceptionEvent = procedure(Sender: TObject; E: Exception)
    of object;
  TsgcTelegramEventEvent = procedure(Sender: TObject; const Event, Text: String)
    of object;
  TsgcTelegramMessageTextEvent = procedure(Sender: TObject;
    MessageText: TsgcTelegramMessageText) of object;
  TsgcTelegramMessageDocumentEvent = procedure(Sender: TObject;
    MessageDocument: TsgcTelegramMessageDocument) of object;
  TsgcTelegramMessagePhotoEvent = procedure(Sender: TObject;
    MessagePhoto: TsgcTelegramMessagePhoto) of object;
  TsgcTelegramMessageVideoEvent = procedure(Sender: TObject;
    MessageVideo: TsgcTelegramMessageVideo) of object;
  TsgcTelegramNewChatEvent = procedure(Sender: TObject; Chat: TsgcTelegramChat)
    of object;
  TsgcTelegramNewCallbackQueryEvent = procedure(Sender: TObject;
    CallbackQuery: TsgcTelegramCallbackQuery) of object;

  TsgcTelegramErrorLevels = (tvbFatalErrors, tvbErrors, tvbWarnings,
    tvbInformational, tvbDebug, tvbVerboseDebug);
  TsgcTelegramProxyTypeValues = (tptProxyHTTP, tptProxyMTProto, tptProxySocks5);

  TsgcTelegramAPI_Options = class(TPersistent)
  private
    FApiHash: String;
    FApiId: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiHash: String read FApiHash write FApiHash;
    property ApiId: String read FApiId write FApiId;
  end;

  TsgcTelegramParameters_Options = class(TPersistent)
  private
    FApplicationVersion: String;
    FDeviceModel: String;
    FLanguageCode: String;
    FSystemVersion: String;
  public
    constructor Create; virtual;
    procedure Assign(aSource: TPersistent); override;
  published
    property ApplicationVersion: String read FApplicationVersion
      write FApplicationVersion;
    property DeviceModel: String read FDeviceModel write FDeviceModel;
    property LanguageCode: String read FLanguageCode write FLanguageCode;
    property SystemVersion: String read FSystemVersion write FSystemVersion;
  end;

  TsgcTelegram_Options = class(TPersistent)
  private
    FAPI: TsgcTelegramAPI_Options;
    FBotToken: String;
    FDatabaseDirectory: String;
    FDBEncryptionKey: String;
    FErrorsLevel: TsgcTelegramErrorLevels;
    FParameters: TsgcTelegramParameters_Options;
    FPhoneNumber: String;
    procedure SetAPI(const Value: TsgcTelegramAPI_Options);
    procedure SetParameters(const Value: TsgcTelegramParameters_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property API: TsgcTelegramAPI_Options read FAPI write SetAPI;
    property BotToken: String read FBotToken write FBotToken;
    property DatabaseDirectory: String read FDatabaseDirectory
      write FDatabaseDirectory;
    property Parameters: TsgcTelegramParameters_Options read FParameters
      write SetParameters;
    property DBEncryptionKey: String read FDBEncryptionKey
      write FDBEncryptionKey;
    property ErrorsLevel: TsgcTelegramErrorLevels read FErrorsLevel
      write FErrorsLevel;
    property PhoneNumber: String read FPhoneNumber write FPhoneNumber;
  end;

  TsgcTDLib_Telegram_Client = class(TsgcComponent_Base)
    { thread }
  private
    FThread: TsgcTimer;
    function GetThread: TsgcTimer;
  protected
    procedure DoStartThread; virtual;
    procedure DoStopThread; virtual;
  protected
    procedure OnThreadEvent(Sender: TObject); virtual;
    procedure OnThreadExceptionEvent(Sender: TObject; E: Exception); virtual;
  protected
    property Thread: TsgcTimer read GetThread;
    { thread }

    { client }
  private
    FClient: Pointer;
  public
    property Client: Pointer read FClient;
    { client }

    { properties }
  private
    FTelegram: TsgcTelegram_Options;
    FNotifyEvents: TwsNotifyEvent;
    procedure SetTelegram(const Value: TsgcTelegram_Options);
  public
    property Telegram: TsgcTelegram_Options read FTelegram write SetTelegram;
    property NotifyEvents: TwsNotifyEvent read FNotifyEvents
      write FNotifyEvents;
    { properties }

    { constructor/destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor/destructor }

    { active }
  private
    FId: Integer;
    function GetId: Integer;
  private
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  private
  protected
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;
  public
    property Id: Integer read GetId;
    property Active: Boolean read GetActive write SetActive;
    { active }

    { error }
  protected
    procedure OnFatalErrorCallback(Error_Message: PAnsiChar); virtual;
    { error }

    { methods }
  private
    function GetDatabaseDirectory: String;
  private
    procedure DoTDLibSend(const aRequest: String);
    function DoTDLibExecute(const aRequest: String): string;
  protected
    procedure DoReadEvent(const aJSON: string); virtual;
  public
    procedure TDLibSend(const aRequest: String);
{$IFDEF ANDROID}public{$ELSE}protected{$ENDIF}
    procedure SetAuthenticationCode(const aCode: String);
    procedure SetAuthenticationPassword(const aPassword: String);
    procedure SetFirstLastName(const aFirstName, aLastName: String);
    // ... send messages
  protected
    function GetInlineKeyboard(const aInlineKeyboard
      : TsgcTelegramReplyMarkupInlineKeyboard = nil): string;
    function GetShowKeyboard(const aShowKeyboard
      : TsgcTelegramReplyMarkupShowKeyboard = nil): string;
    function GetTextEntitiesFromText(const aText: string;
      var Entities: string): string;
  public
    procedure SendTextMessage(const aChatId, aText: string;
      const aInlineKeyboard: TsgcTelegramReplyMarkupInlineKeyboard = nil;
      const aShowKeyboard: TsgcTelegramReplyMarkupShowKeyboard = nil);
    procedure SendRichTextMessage(const aChatId, aText: string;
      const aInlineKeyboard: TsgcTelegramReplyMarkupInlineKeyboard = nil;
      const aShowKeyboard: TsgcTelegramReplyMarkupShowKeyboard = nil);
    procedure SendDocumentMessage(const aChatId, aFilePath: string;
      const aInlineKeyboard: TsgcTelegramReplyMarkupInlineKeyboard = nil;
      const aShowKeyboard: TsgcTelegramReplyMarkupShowKeyboard = nil);
    procedure SendPhotoMessage(const aChatId, aFilePath: string;
      aWidth: Integer = 0; aHeight: Integer = 0;
      const aInlineKeyboard: TsgcTelegramReplyMarkupInlineKeyboard = nil;
      const aShowKeyboard: TsgcTelegramReplyMarkupShowKeyboard = nil);
    procedure SendVideoMessage(const aChatId, aFilePath: string;
      aWidth: Integer = 0; aHeight: Integer = 0; aDuration: Int64 = 0;
      const aInlineKeyboard: TsgcTelegramReplyMarkupInlineKeyboard = nil;
      const aShowKeyboard: TsgcTelegramReplyMarkupShowKeyboard = nil);
    // ... groups
  public
    procedure DeleteSupergroup(aGroupId: Int64);
    // ... members
  protected
    procedure DoAddChatMembers(const aChatId, aUsersIds: string);
  public
    procedure AddChatMember(const aChatId: string; const aUserId: Int64;
      aForwardLimit: Integer = 100);
    procedure AddChatMembers(const aChatId: string;
      const aUserIds: Array of Int64);
    procedure JoinChatByInviteLink(const aLink: string);
  private
    function GetSupergroupMembersFilter(const aSupergroupMembersFilter
      : TsgcTelegramSuperGroupMembersFilter = tsgmFilterNone;
      const aSuperGroupMembersQuery: string = ''): string;
  public
    procedure GetChatMember(const aChatId: string; aUserId: Int64);
    procedure GetBasicGroupFullInfo(aGroupId: Int64);
    procedure GetSupergroupMembers(aSuperGroupId: Int64;
      const aSupergroupMembersFilter: TsgcTelegramSuperGroupMembersFilter =
      tsgmFilterNone; const aSuperGroupMembersQuery: string = '';
      aOffset: Integer = 0; aLimit: Integer = 200);
    // ... chats
  protected
    procedure DoCreateNewBasicGroupChat(const aUserIds, aTitle: String);
  public
    procedure CreateNewSecretChat(const aUserId: Int64);
    procedure CreateNewBasicGroupChat(const aUserIds: Array of Int64;
      const aTitle: String);
    procedure CreateNewSupergroupChat(const aTitle: String;
      aIsChannel: Boolean = False; const aDescription: String = '');
    procedure CreatePrivateChat(const aUserId: Int64; aForce: Boolean = False);
  public
    procedure GetChat(aChatId: string);
    procedure GetChats(aOffsetOrder: Integer = MaxInt;
      aOffsetChatId: Integer = 0; aLimit: Integer = 50);
    procedure GetChatHistory(aChatId: string; aFromMessageId: String = '0';
      aOffset: Integer = 0; aLimit: Integer = 10);
    // ... user
  public
    procedure GetUser(aUserId: Integer);
    // ... logout
  public
    procedure LogOut;
    // ... proxy
  public
    procedure AddProxyHTTP(const aServer: string; aPort: Integer;
      const aUserName, aPassword: String; aHTTPOnly: Boolean);
    procedure AddProxyMTProto(const aServer: string; aPort: Integer;
      const aSecret: string);
    procedure AddProxySocks5(const aServer: string; aPort: Integer;
      const aUserName, aPassword: string);
  public
    procedure EnableProxy(aId: Integer);
    procedure DisableProxy;
    procedure RemoveProxy(aId: Integer);
    procedure GetProxies;
    { methods }

    { properties }
  private
    FMyId: Int64;
  public
    property MyId: Int64 read FMyId;
    { properties }

    { events }
  private
    FOnAuthenticationCode: TsgcTelegramAuthenticationCodeEvent;
    FOnAuthenticationPassword: TsgcTelegramAuthenticationPasswordEvent;
    FOnAuthorizationStatus: TsgcTelegramAuthorizationStatusEvent;
    FOnBeforeReadEvent: TsgcTelegramBeforeReadEvent;
    FOnException: TsgcTelegramExceptionEvent;
    FOnEvent: TsgcTelegramEventEvent;
    FOnRegisterUser: TsgcTelegramRegisterUserEvent;
    FOnConnectionStatus: TsgcTelegramConnectionStatusEvent;
    FOnMessageDocument: TsgcTelegramMessageDocumentEvent;
    FOnMessagePhoto: TsgcTelegramMessagePhotoEvent;
    FOnMessageText: TsgcTelegramMessageTextEvent;
    FOnMessageVideo: TsgcTelegramMessageVideoEvent;
    FOnNewCallbackQuery: TsgcTelegramNewCallbackQueryEvent;
    FOnNewChat: TsgcTelegramNewChatEvent;
  protected
    procedure DoAuthenticationCodeEvent(var Code: String); virtual;
    procedure DoRegisterUserEvent(var FirstName, LastName: String); virtual;
    procedure DoAuthenticationPasswordEvent(var Password: String); virtual;
    procedure DoExceptionEvent(E: Exception); virtual;
    procedure DoEventEvent(const aEvent, Text: String); virtual;
    function DoBeforeReadEvent(const Text: String): Boolean;
    procedure DoConnectionStatusEvent(const Status: String); virtual;
    procedure DoAuthorizationStatusEvent(const Status: String); virtual;
    procedure DoMessageTextEvent(const aJSON: String); virtual;
    procedure DoMessageDocumentEvent(const aJSON: String); virtual;
    procedure DoMessagePhotoEvent(const aJSON: String); virtual;
    procedure DoMessageVideoEvent(const aJSON: String); virtual;
    procedure DoNewChatEvent(const aJSON: String); virtual;
    procedure DoNewCallbackQueryEvent(const aJSON: string); virtual;
  public
    property OnAuthenticationCode: TsgcTelegramAuthenticationCodeEvent
      read FOnAuthenticationCode write FOnAuthenticationCode;
    property OnAuthenticationPassword: TsgcTelegramAuthenticationPasswordEvent
      read FOnAuthenticationPassword write FOnAuthenticationPassword;
    property OnAuthorizationStatus: TsgcTelegramAuthorizationStatusEvent
      read FOnAuthorizationStatus write FOnAuthorizationStatus;
    property OnBeforeReadEvent: TsgcTelegramBeforeReadEvent
      read FOnBeforeReadEvent write FOnBeforeReadEvent;
    property OnRegisterUser: TsgcTelegramRegisterUserEvent read FOnRegisterUser
      write FOnRegisterUser;
    property OnException: TsgcTelegramExceptionEvent read FOnException
      write FOnException;
    property OnEvent: TsgcTelegramEventEvent read FOnEvent write FOnEvent;
    property OnConnectionStatus: TsgcTelegramConnectionStatusEvent
      read FOnConnectionStatus write FOnConnectionStatus;
    property OnMessageDocument: TsgcTelegramMessageDocumentEvent
      read FOnMessageDocument write FOnMessageDocument;
    property OnMessagePhoto: TsgcTelegramMessagePhotoEvent read FOnMessagePhoto
      write FOnMessagePhoto;
    property OnMessageText: TsgcTelegramMessageTextEvent read FOnMessageText
      write FOnMessageText;
    property OnMessageVideo: TsgcTelegramMessageVideoEvent read FOnMessageVideo
      write FOnMessageVideo;
    property OnNewCallbackQuery: TsgcTelegramNewCallbackQueryEvent
      read FOnNewCallbackQuery write FOnNewCallbackQuery;
    property OnNewChat: TsgcTelegramNewChatEvent read FOnNewChat
      write FOnNewChat;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_TELEGRAM}

uses
  Math,
{$IFDEF SGC_MOBILE}System.IOUtils, {$ENDIF}
  // sgc
  sgcLib_Telegram, sgcBase_Helpers;

constructor TsgcTDLib_Telegram_Client.Create(aOwner: TComponent);
begin
  inherited;
  FClient := nil;
  FNotifyEvents := neAsynchronous;
  FTelegram := TsgcTelegram_Options.Create;
  FMyId := 0;
end;

destructor TsgcTDLib_Telegram_Client.Destroy;
begin
  DoDisconnect;
  sgcFree(FTelegram);
  sgcThreadFree(FThread);
  inherited;
end;

procedure TsgcTDLib_Telegram_Client.AddChatMember(const aChatId: string;
  const aUserId: Int64; aForwardLimit: Integer = 100);
begin
  DoTDLibSend
    (Format('{"@type": "addChatMember", "chat_id": "%s", "user_id": %d, "forward_limit": %d}',
    [aChatId, aUserId, aForwardLimit]));
end;

procedure TsgcTDLib_Telegram_Client.AddChatMembers(const aChatId: string;
  const aUserIds: Array of Int64);
var
  i: Integer;
  vUserIds: String;
begin
  vUserIds := '[';
  for i := Low(aUserIds) to High(aUserIds) do
  begin
    if i > 0 then
      vUserIds := vUserIds + ',';
    vUserIds := vUserIds + IntToStr(aUserIds[i]);
  end;
  vUserIds := vUserIds + ']';

  DoAddChatMembers(aChatId, vUserIds);
end;

procedure TsgcTDLib_Telegram_Client.AddProxyHTTP(const aServer: string;
  aPort: Integer; const aUserName, aPassword: String; aHTTPOnly: Boolean);
var
  oJSON: TsgcJSON;
  oJSONObject: IsgcObjectJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('@type', 'addProxy');
    oJSON.AddPair('is_enabled', true);
    oJSON.AddPair('server', aServer);
    oJSON.AddPair('port', aPort);
    oJSONObject := oJSON.AddObject('type');
    oJSONObject.JSONObject.AddPair('@type', 'proxyTypeHttp');
    oJSONObject.JSONObject.AddPair('username', aUserName);
    oJSONObject.JSONObject.AddPair('password', aPassword);
    oJSONObject.JSONObject.AddPair('http_only', aHTTPOnly);

    DoTDLibSend(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcTDLib_Telegram_Client.AddProxyMTProto(const aServer: string;
  aPort: Integer; const aSecret: string);
var
  oJSON: TsgcJSON;
  oJSONObject: IsgcObjectJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('@type', 'addProxy');
    oJSON.AddPair('is_enabled', true);
    oJSON.AddPair('server', aServer);
    oJSON.AddPair('port', aPort);
    oJSONObject := oJSON.AddObject('type');
    oJSONObject.JSONObject.AddPair('@type', 'proxyTypeMtproto');
    oJSONObject.JSONObject.AddPair('secret', aSecret);

    DoTDLibSend(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcTDLib_Telegram_Client.AddProxySocks5(const aServer: string;
  aPort: Integer; const aUserName, aPassword: string);
var
  oJSON: TsgcJSON;
  oJSONObject: IsgcObjectJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('@type', 'addProxy');
    oJSON.AddPair('is_enabled', true);
    oJSON.AddPair('server', aServer);
    oJSON.AddPair('port', aPort);
    oJSONObject := oJSON.AddObject('type');
    oJSONObject.JSONObject.AddPair('@type', 'proxyTypeSocks5');
    oJSONObject.JSONObject.AddPair('username', aUserName);
    oJSONObject.JSONObject.AddPair('password', aPassword);

    DoTDLibSend(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcTDLib_Telegram_Client.CreateNewBasicGroupChat(const aUserIds
  : Array of Int64; const aTitle: String);
var
  i: Integer;
  vUserIds: String;
begin
  vUserIds := '[';
  for i := Low(aUserIds) to High(aUserIds) do
  begin
    if i > 0 then
      vUserIds := vUserIds + ',';
    vUserIds := vUserIds + IntToStr(aUserIds[i]);
  end;
  vUserIds := vUserIds + ']';

  DoCreateNewBasicGroupChat(vUserIds, aTitle);
end;

procedure TsgcTDLib_Telegram_Client.CreateNewSupergroupChat
  (const aTitle: String; aIsChannel: Boolean = False;
  const aDescription: String = '');
var
  vIsChannel: String;
begin
  vIsChannel := 'false';
  if aIsChannel then
    vIsChannel := 'true';

  DoTDLibSend
    (Format('{"@type": "createNewSupergroupChat", "title": "%s", "is_channel": %s, "description": "%s"}',
    [aTitle, vIsChannel, aDescription]));
end;

procedure TsgcTDLib_Telegram_Client.CreateNewSecretChat(const aUserId: Int64);
begin
  DoTDLibSend(Format('{"@type": "createNewSecretChat", "user_id": %d}',
    [aUserId]));
end;

procedure TsgcTDLib_Telegram_Client.CreatePrivateChat(const aUserId: Int64;
  aForce: Boolean = False);
var
  vForce: string;
begin
  vForce := 'false';
  if aForce then
    vForce := 'true';

  DoTDLibSend
    (Format('{"@type": "createPrivateChat", "user_id": %d, "force": %s}',
    [aUserId, vForce]));
end;

procedure TsgcTDLib_Telegram_Client.DeleteSupergroup(aGroupId: Int64);
begin
  DoTDLibSend(Format('{"@type": "deleteSupergroup", "supergroup_id": %d}',
    [aGroupId]));
end;

procedure TsgcTDLib_Telegram_Client.DisableProxy;
begin
  DoTDLibSend('{"@type": "disableProxy"}');
end;

procedure TsgcTDLib_Telegram_Client.DoAddChatMembers(const aChatId,
  aUsersIds: string);
begin
  DoTDLibSend
    (Format('{"@type": "addChatMembers", "chat_id": "%s", "user_ids": %s}',
    [aChatId, aUsersIds]));
end;

procedure TsgcTDLib_Telegram_Client.DoAuthenticationCodeEvent(var Code: String);
begin
  if Assigned(FOnAuthenticationCode) then
    FOnAuthenticationCode(self, Code);
end;

procedure TsgcTDLib_Telegram_Client.DoAuthenticationPasswordEvent
  (var Password: String);
begin
  if Assigned(FOnAuthenticationPassword) then
    FOnAuthenticationPassword(self, Password);
end;

procedure TsgcTDLib_Telegram_Client.DoAuthorizationStatusEvent
  (const Status: String);
begin
  if Assigned(FOnAuthorizationStatus) then
    FOnAuthorizationStatus(self, Status);
end;

function TsgcTDLib_Telegram_Client.DoBeforeReadEvent
  (const Text: String): Boolean;
begin
  result := False;
  if Assigned(FOnBeforeReadEvent) then
    FOnBeforeReadEvent(self, Text, result);
end;

procedure TsgcTDLib_Telegram_Client.DoConnect;
begin
  Try
    FClient := td_json_client_create();
    if Assigned(FClient) then
    begin
      FId := 0;

      DoTDLibExecute('{"@type": "setLogVerbosityLevel", ' +
        '"new_verbosity_level": ' + IntToStr(Ord(Telegram.ErrorsLevel)) + ', ' +
        '"@extra": ' + IntToStr(Id) + '}');

      // error callback
{$IFNDEF SGC_MOBILE}
      td_set_log_fatal_error_callback(OnFatalErrorCallback);
{$ENDIF}
      // test TDLib execute method
      DoTDLibExecute('{"@type": "getTextEntities", ' +
        '"text": "@telegram /test_command https://telegram.org telegram.me", ' +
        '"@extra": ["5", 7.0]}');

      // test TDLib send method
      DoTDLibSend('{"@type": "getAuthorizationState", "@extra": ' +
        IntToStr(Id) + '}');

      DoStartThread;
    end;
  Except
    On E: Exception do
      DoExceptionEvent(E);
  End;
end;

procedure TsgcTDLib_Telegram_Client.DoDisconnect;
begin
  Try
    FMyId := 0;
    if Assigned(Client) then
    begin
      DoStopThread;
      if not IsDestroying then
        td_json_client_destroy(Client);
      FClient := nil;
    end;
  Except
    On E: Exception do
      DoExceptionEvent(E);
  End;
end;

procedure TsgcTDLib_Telegram_Client.DoExceptionEvent(E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(self, E);
end;

procedure TsgcTDLib_Telegram_Client.DoEventEvent(const aEvent, Text: String);
begin
  if Assigned(FOnEvent) then
    FOnEvent(self, aEvent, Text);
end;

procedure TsgcTDLib_Telegram_Client.DoReadEvent(const aJSON: string);
var
  oJSON: TsgcJSON;
  vEvent, vAuth: string;
  vCode, vFirstName, vLastName, vPassword: String;
  vMessage: string;
begin
  if aJSON = '' then
    exit;

  if DoBeforeReadEvent(aJSON) then
    exit;

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(String(aJSON));
    if oJSON.Node['@type'] <> nil then
    begin
      vEvent := oJSON.Node['@type'].Value;
      if vEvent = 'updateAuthorizationState' then
      begin
        if oJSON.Node['authorization_state'] <> nil then
        begin
          vAuth := oJSON.Node['authorization_state'].Node['@type'].Value;
          DoAuthorizationStatusEvent(vAuth);

          if vAuth = 'authorizationStateClosed' then
            exit;

          if vAuth = 'authorizationStateWaitTdlibParameters' then
          begin
            DoTDLibSend('{"@type": "setTdlibParameters", "parameters": {' +
              '"database_directory": "' + GetDatabaseDirectory + '",' +
              '"use_message_database": true,' + '"use_secret_chats": true,' +
              '"api_id": ' + Telegram.API.ApiId + ',' + '"api_hash": "' +
              Telegram.API.ApiHash + '", ' + '"system_language_code": "' +
              Telegram.Parameters.LanguageCode + '", ' + '"device_model": "' +
              Telegram.Parameters.DeviceModel + '", ' + '"system_version": "' +
              Telegram.Parameters.SystemVersion + '", ' +
              '"application_version": "' +
              Telegram.Parameters.ApplicationVersion + '", ' +
              '"enable_storage_optimizer": true}}');
          end
          else if vAuth = 'authorizationStateWaitEncryptionKey' then
            DoTDLibSend
              ('{"@type": "checkDatabaseEncryptionKey", "encryption_key": "' +
              Telegram.DBEncryptionKey + '"}')
          else if vAuth = 'authorizationStateWaitPhoneNumber' then
          begin
            if Telegram.BotToken <> '' then
              DoTDLibSend('{"@type": "checkAuthenticationBotToken", "token": "'
                + Telegram.BotToken + '"}')
            else
              DoTDLibSend
                ('{"@type": "setAuthenticationPhoneNumber", "phone_number": "' +
                Telegram.PhoneNumber + '"}')
          end
          else if vAuth = 'authorizationStateWaitCode' then
          begin
            vCode := '';
            DoAuthenticationCodeEvent(vCode);
{$IFNDEF ANDROID64}
            SetAuthenticationCode(vCode);
{$ENDIF}
          end
          else if vAuth = 'authorizationStateWaitRegistration' then
          begin
            vFirstName := '';
            vLastName := '';
            DoRegisterUserEvent(vFirstName, vLastName);
{$IFNDEF ANDROID64}
            SetFirstLastName(vFirstName, vLastName);
{$ENDIF}
          end
          else if vAuth = 'authorizationStateWaitPassword' then
          begin
            vPassword := '';
            DoAuthenticationPasswordEvent(vPassword);
{$IFNDEF ANDROID64}
            SetAuthenticationPassword(vPassword);
{$ENDIF}
          end
          else
            DoEventEvent(vEvent, aJSON);
        end;
      end
      else if vEvent = 'updateOption' then
      begin
        if oJSON.Node['name'] <> nil then
        begin
          if oJSON.Node['name'].Value = 'my_id' then
          begin
            if oJSON.Node['value'].JSONObject <> nil then
            begin
              if oJSON.Node['value'].JSONObject.Node['value'] <> nil then
                FMyId := oJSON.Node['value'].JSONObject.Node['value'].Value;
            end;
          end;
        end;
        DoEventEvent(vEvent, aJSON);
      end
      else if vEvent = 'updateConnectionState' then
      begin
        if oJSON.Node['state'] <> nil then
          DoConnectionStatusEvent(oJSON.Node['state'].Node['@type'].Value)
        else
          DoEventEvent(vEvent, aJSON);
      end
      else if vEvent = 'updateNewMessage' then
      begin
        if oJSON.Node['message'] <> nil then
        begin
          if oJSON.Node['message'].Node['content'] <> nil then
          begin
            vMessage := oJSON.Node['message'].Node['content'].Node
              ['@type'].Value;
            if vMessage = 'messageText' then
              DoMessageTextEvent(aJSON)
            else if vMessage = 'messageDocument' then
              DoMessageDocumentEvent(aJSON)
            else if vMessage = 'messagePhoto' then
              DoMessagePhotoEvent(aJSON)
            else if vMessage = 'messageVideo' then
              DoMessageVideoEvent(aJSON)
            else
              DoEventEvent(vEvent, aJSON);
          end;
        end;
      end
      else if vEvent = 'updateNewChat' then
        DoNewChatEvent(aJSON)
      else if vEvent = 'updateNewCallbackQuery' then
        DoNewCallbackQueryEvent(aJSON)
      else
        DoEventEvent(vEvent, aJSON);
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcTDLib_Telegram_Client.DoRegisterUserEvent(var FirstName,
  LastName: String);
begin
  if Assigned(FOnRegisterUser) then
    FOnRegisterUser(self, FirstName, LastName);
end;

procedure TsgcTDLib_Telegram_Client.DoStartThread;
begin
  if Assigned(FThread) then
    sgcFree(FThread);
  Thread.Enabled := true;
end;

procedure TsgcTDLib_Telegram_Client.DoStopThread;
begin
  Thread.Enabled := False;
end;

function TsgcTDLib_Telegram_Client.GetActive: Boolean;
begin
  result := Assigned(FClient);
end;

function TsgcTDLib_Telegram_Client.GetThread: TsgcTimer;
begin
  if not Assigned(FThread) then
  begin
    FThread := TsgcTimer.Create;
    FThread.DebugName := 'Telegram Thread';
    FThread.NotifyEvents := NotifyEvents;
    FThread.OnTimer := OnThreadEvent;
    FThread.OnException := OnThreadExceptionEvent;
    FThread.Interval := 1;
  end;
  result := FThread;
end;

procedure TsgcTDLib_Telegram_Client.OnFatalErrorCallback(Error_Message
  : PAnsiChar);
begin
  DoExceptionEvent(Exception.Create(String(Error_Message)));
end;

procedure TsgcTDLib_Telegram_Client.OnThreadEvent(Sender: TObject);
begin
  Try
    DoReadEvent(String(td_json_client_receive(Client, 0)));
  Except
    On E: Exception do
      DoExceptionEvent(E);
  End;
end;

procedure TsgcTDLib_Telegram_Client.OnThreadExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoExceptionEvent(E);
end;

procedure TsgcTDLib_Telegram_Client.SetActive(const Value: Boolean);
begin
  if Value then
    DoConnect
  else
    DoDisconnect;
end;

procedure TsgcTDLib_Telegram_Client.SetTelegram(const Value
  : TsgcTelegram_Options);
begin
  if Assigned(FTelegram) then
    FTelegram.Assign(Value);
end;

procedure TsgcTDLib_Telegram_Client.DoTDLibSend(const aRequest: String);
begin
  Try
    td_json_client_send(Client, PAnsiChar(AnsiString(aRequest)));
  Except
    On E: Exception do
      DoExceptionEvent(E);
  End;
end;

procedure TsgcTDLib_Telegram_Client.DoConnectionStatusEvent
  (const Status: String);
begin
  if Assigned(FOnConnectionStatus) then
    FOnConnectionStatus(self, Status);
end;

procedure TsgcTDLib_Telegram_Client.DoCreateNewBasicGroupChat(const aUserIds,
  aTitle: String);
begin
  DoTDLibSend
    (Format('{"@type": "createNewBasicGroupChat", "user_ids": %s, "title": "%s"}',
    [aUserIds, aTitle]));
end;

procedure TsgcTDLib_Telegram_Client.DoMessageDocumentEvent(const aJSON: String);
var
  oMessage: TsgcTelegramMessageDocument;
begin
  if Assigned(FOnMessageDocument) then
  begin
    oMessage := TsgcTelegramMessageDocument.Create;
    Try
      oMessage.ReadJSON(aJSON);
      FOnMessageDocument(self, oMessage);
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

procedure TsgcTDLib_Telegram_Client.DoMessagePhotoEvent(const aJSON: String);
var
  oMessage: TsgcTelegramMessagePhoto;
begin
  if Assigned(FOnMessagePhoto) then
  begin
    oMessage := TsgcTelegramMessagePhoto.Create;
    Try
      oMessage.ReadJSON(aJSON);
      FOnMessagePhoto(self, oMessage);
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

procedure TsgcTDLib_Telegram_Client.DoMessageTextEvent(const aJSON: String);
var
  oMessage: TsgcTelegramMessageText;
begin
  if Assigned(FOnMessageText) then
  begin
    oMessage := TsgcTelegramMessageText.Create;
    Try
      oMessage.ReadJSON(aJSON);
      FOnMessageText(self, oMessage);
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

procedure TsgcTDLib_Telegram_Client.DoMessageVideoEvent(const aJSON: String);
var
  oMessage: TsgcTelegramMessageVideo;
begin
  if Assigned(FOnMessageVideo) then
  begin
    oMessage := TsgcTelegramMessageVideo.Create;
    Try
      oMessage.ReadJSON(aJSON);
      FOnMessageVideo(self, oMessage);
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

procedure TsgcTDLib_Telegram_Client.DoNewCallbackQueryEvent
  (const aJSON: string);
var
  oCallback: TsgcTelegramCallbackQuery;
begin
  if Assigned(FOnNewCallbackQuery) then
  begin
    oCallback := TsgcTelegramCallbackQuery.Create;
    Try
      oCallback.ReadJSON(aJSON);
      FOnNewCallbackQuery(self, oCallback);
    Finally
      sgcFree(oCallback);
    End;
  end;

end;

procedure TsgcTDLib_Telegram_Client.DoNewChatEvent(const aJSON: String);
var
  oMessage: TsgcTelegramChat;
begin
  if Assigned(FOnNewChat) then
  begin
    oMessage := TsgcTelegramChat.Create;
    Try
      oMessage.ReadJSON(aJSON);
      FOnNewChat(self, oMessage);
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

function TsgcTDLib_Telegram_Client.DoTDLibExecute(const aRequest
  : String): string;
begin
  Try
    result := String(td_json_client_execute(Client,
      PAnsiChar(AnsiString(aRequest))));
  Except
    On E: Exception do
      DoExceptionEvent(E);
  End;
end;

procedure TsgcTDLib_Telegram_Client.EnableProxy(aId: Integer);
begin
  DoTDLibSend('{"@type": "enableProxy", "proxy_id": ' + IntToStr(aId) + '}');
end;

procedure TsgcTDLib_Telegram_Client.GetBasicGroupFullInfo(aGroupId: Int64);
begin
  DoTDLibSend(Format('{"@type": "getBasicGroupFullInfo",' +
    '"basic_group_id": %d}', [aGroupId]));
end;

procedure TsgcTDLib_Telegram_Client.GetChat(aChatId: string);
begin
  DoTDLibSend(Format('{"@type": "getChat",' + '"chat_id": "%s"}', [aChatId]));
end;

procedure TsgcTDLib_Telegram_Client.GetChats(aOffsetOrder: Integer = MaxInt;
  aOffsetChatId: Integer = 0; aLimit: Integer = 50);
begin
  if Telegram.BotToken = '' then
  begin
    DoTDLibSend('{"@type": "getChats", ' + '"offset_order": ' +
      IntToStr(aOffsetOrder) + ', ' + '"offset_chat_id": ' +
      IntToStr(aOffsetChatId) + ', ' + '"limit": ' + IntToStr(aLimit) + ', ' +
      '"@extra": ' + IntToStr(Id) + '}');
  end;
end;

procedure TsgcTDLib_Telegram_Client.GetChatHistory(aChatId: string;
  aFromMessageId: String = '0'; aOffset: Integer = 0; aLimit: Integer = 10);
begin
  DoTDLibSend(Format('{"@type": "getChatHistory",' + '"chat_id": "%s", ' +
    '"from_message_id": "%s", ' + '"offset": %d, "limit": %d}',
    [aChatId, aFromMessageId, aOffset, aLimit]));
end;

procedure TsgcTDLib_Telegram_Client.GetChatMember(const aChatId: string;
  aUserId: Int64);
begin
  DoTDLibSend
    (Format('{"@type": "getChatMember", "chat_id": "%s", "user_id": %d}',
    [aChatId, aUserId]));
end;

function TsgcTDLib_Telegram_Client.GetDatabaseDirectory: String;
begin
  result := Telegram.DatabaseDirectory;
  if result = '' then
  begin
{$IFDEF SGC_MOBILE}
    result := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + 'tdlib';
{$ELSE}
    result := 'tdlib';
{$ENDIF}
  end;
end;

function TsgcTDLib_Telegram_Client.GetId: Integer;
begin
  if FId = 0 then
  begin
    Randomize;
    FId := RandomRange(1, MaxInt);
  end;
  result := FId;
end;

function TsgcTDLib_Telegram_Client.GetInlineKeyboard(const aInlineKeyboard
  : TsgcTelegramReplyMarkupInlineKeyboard = nil): string;
begin
  result := '';
  if Assigned(aInlineKeyboard) then
    result := aInlineKeyboard.GetJSONText + ',';
end;

procedure TsgcTDLib_Telegram_Client.GetProxies;
begin
  DoTDLibSend('{"@type": "getProxies"}');
end;

function TsgcTDLib_Telegram_Client.GetShowKeyboard(const aShowKeyboard
  : TsgcTelegramReplyMarkupShowKeyboard = nil): string;
begin
  result := '';
  if Assigned(aShowKeyboard) then
    result := aShowKeyboard.GetJSONText + ',';
end;

procedure TsgcTDLib_Telegram_Client.GetSupergroupMembers(aSuperGroupId: Int64;
  const aSupergroupMembersFilter: TsgcTelegramSuperGroupMembersFilter =
  tsgmFilterNone; const aSuperGroupMembersQuery: string = '';
  aOffset: Integer = 0; aLimit: Integer = 200);
begin
  DoTDLibSend
    (Format('{"@type": "getSupergroupMembers", "supergroup_id": %d, "filter": %s, "offset": %d, "limit": %d}',
    [aSuperGroupId, GetSupergroupMembersFilter(aSupergroupMembersFilter,
    aSuperGroupMembersQuery), aOffset, aLimit]));
end;

function TsgcTDLib_Telegram_Client.GetSupergroupMembersFilter
  (const aSupergroupMembersFilter: TsgcTelegramSuperGroupMembersFilter =
  tsgmFilterNone; const aSuperGroupMembersQuery: string = ''): string;

  function GetFilterJSON(const aType: string;
    const aQuery: string = ''): string;
  begin
    if aQuery = '' then
      result := Format('{"@type": "%s"}', [aType])
    else
      result := Format('{"@type": "%s", "query": "%s"}', [aType, aQuery]);
  end;

begin
  case aSupergroupMembersFilter of
    tsgmFilterNone:
      result := 'null';
    tsgmFilterAdministrators:
      result := GetFilterJSON('supergroupMembersFilterAdministrators');
    tsgmFilterBanned:
      result := GetFilterJSON('supergroupMembersFilterBanned',
        aSuperGroupMembersQuery);
    tsgmFilterBots:
      result := GetFilterJSON('supergroupMembersFilterBots');
    tsgmFilterContacts:
      result := GetFilterJSON('supergroupMembersFilterContacts',
        aSuperGroupMembersQuery);
    tsgmFilterMention:
      result := GetFilterJSON('supergroupMembersFilterMention');
    tsgmFilterRecent:
      result := GetFilterJSON('supergroupMembersFilterRecent');
    tsgmFilterRestricted:
      result := GetFilterJSON('supergroupMembersFilterRestricted',
        aSuperGroupMembersQuery);
    tsgmFilterSearch:
      result := GetFilterJSON('supergroupMembersFilterSearch',
        aSuperGroupMembersQuery);
  end;
end;

function TsgcTDLib_Telegram_Client.GetTextEntitiesFromText(const aText: string;
  var Entities: string): string;
var
  i, j: Integer;
  b: char;
  c: char;

  procedure DoAddEntity(aOffset: Integer; aLength: Integer; aType: String);
  begin
    if Entities <> '' then
      Entities := Entities + ',';
    Entities := Entities +
      Format('{"@type": "textEntity", "offset": %d, "length": %d, "type": {"@type": "%s"}}',
      [aOffset, aLength, aType]);
  end;

  function GetTextEntityType(aChar: char): string;
  begin
    result := '';
    if aChar = '*' then
      result := 'textEntityTypeBold'
    else if aChar = '_' then
      result := 'textEntityTypeItalic'
    else if aChar = '-' then
      result := 'textEntityTypeStrikethrough'
    else if aChar = '~' then
      result := 'textEntityTypeUnderline'
    else if aChar = '#' then
      result := 'textEntityTypeCode';
  end;

  procedure CheckMarkDown(aChar: char);
  begin
    if c = aChar then
    begin
      if b = #0 then
      begin
        j := Length(result);
        b := aChar;
      end
      else if b = aChar then
      begin
        DoAddEntity(j, Length(result) - j, GetTextEntityType(aChar));
        b := #0;
      end
    end
    else if b = aChar then
    begin
      Try
        if aText[i + 1] <> aChar then
          result := result + aText[i];
      Except
        // nothing
      end;
    end;
  end;

begin
  result := '';
  c := #0;
  j := 0;
  b := #0;
  for i := {$IFDEF SGC_ZEROBASEDSTRINGS}0{$ELSE}1{$ENDIF} to Length
    (aText){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} do
  begin
    if GetTextEntityType(aText[i]) <> '' then
      CheckMarkDown(aText[i])
    else
      result := result + aText[i];
    c := aText[i];
  end;
end;

procedure TsgcTDLib_Telegram_Client.GetUser(aUserId: Integer);
begin
  DoTDLibSend(Format('{"@type": "getUser", "user_id": %d}', [aUserId]));
end;

procedure TsgcTDLib_Telegram_Client.JoinChatByInviteLink(const aLink: string);
begin
  DoTDLibSend(Format('{"@type": "joinChatByInviteLink", "invite_link": "%s"}',
    [aLink]));
end;

procedure TsgcTDLib_Telegram_Client.LogOut;
begin
  DoTDLibSend('{"@type": "logOut"}');
end;

procedure TsgcTDLib_Telegram_Client.RemoveProxy(aId: Integer);
begin
  DoTDLibSend('{"@type": "removeProxy", "proxy_id": ' + IntToStr(aId) + '}');
end;

procedure TsgcTDLib_Telegram_Client.SendDocumentMessage(const aChatId,
  aFilePath: string; const aInlineKeyboard
  : TsgcTelegramReplyMarkupInlineKeyboard = nil;
  const aShowKeyboard: TsgcTelegramReplyMarkupShowKeyboard = nil);
begin
  DoTDLibSend(Format('{"@type": "sendMessage",' + '"chat_id": "%s",' +
    GetInlineKeyboard(aInlineKeyboard) + GetShowKeyboard(aShowKeyboard) +
    '"input_message_content": {' + '"@type": "inputMessageDocument",' +
    '"document": {' + '"@type": "inputFileLocal",' + '"path": "%s"' + '}' + '}'
    + '}', [aChatId, sgcEncodeJSON(aFilePath)]));
end;

procedure TsgcTDLib_Telegram_Client.SendPhotoMessage(const aChatId,
  aFilePath: string; aWidth: Integer = 0; aHeight: Integer = 0;
  const aInlineKeyboard: TsgcTelegramReplyMarkupInlineKeyboard = nil;
  const aShowKeyboard: TsgcTelegramReplyMarkupShowKeyboard = nil);
begin
  DoTDLibSend(Format('{"@type": "sendMessage",' + '"chat_id": "%s",' +
    GetInlineKeyboard(aInlineKeyboard) + GetShowKeyboard(aShowKeyboard) +
    '"input_message_content": {' + '"@type": "inputMessagePhoto",' + '"width": '
    + IntToStr(aWidth) + ', ' + '"height": ' + IntToStr(aHeight) + ', ' +
    '"photo": {' + '"@type": "inputFileLocal",' + '"path": "%s"' + '}' + '}' +
    '}', [aChatId, sgcEncodeJSON(aFilePath)]));
end;

procedure TsgcTDLib_Telegram_Client.SendRichTextMessage(const aChatId,
  aText: string; const aInlineKeyboard: TsgcTelegramReplyMarkupInlineKeyboard =
  nil; const aShowKeyboard: TsgcTelegramReplyMarkupShowKeyboard = nil);
var
  vText: string;
  vEntities: string;
begin
  vText := GetTextEntitiesFromText(aText, vEntities);

  DoTDLibSend(Format('{"@type": "sendMessage",' + '"chat_id": "%s",' +
    GetInlineKeyboard(aInlineKeyboard) + GetShowKeyboard(aShowKeyboard) +
    '"input_message_content": {' + '"@type": "inputMessageText",' + '"text": {'
    + '"@type": "formattedText",' + '"text": "%s", "entities": [%s]' + '}' + '}'
    + '}', [aChatId, sgcEncodeJSON(vText), vEntities]));
end;

procedure TsgcTDLib_Telegram_Client.SendTextMessage(const aChatId,
  aText: string; const aInlineKeyboard: TsgcTelegramReplyMarkupInlineKeyboard =
  nil; const aShowKeyboard: TsgcTelegramReplyMarkupShowKeyboard = nil);
begin
  DoTDLibSend(Format('{"@type": "sendMessage",' + '"chat_id": "%s",' +
    GetInlineKeyboard(aInlineKeyboard) + GetShowKeyboard(aShowKeyboard) +
    '"input_message_content": {' + '"@type": "inputMessageText",' + '"text": {'
    + '"@type": "formattedText",' + '"text": "%s"' + '}' + '}' + '}',
    [aChatId, sgcEncodeJSON(aText)]));
end;

procedure TsgcTDLib_Telegram_Client.SendVideoMessage(const aChatId,
  aFilePath: string; aWidth: Integer = 0; aHeight: Integer = 0;
  aDuration: Int64 = 0;
  const aInlineKeyboard: TsgcTelegramReplyMarkupInlineKeyboard = nil;
  const aShowKeyboard: TsgcTelegramReplyMarkupShowKeyboard = nil);
begin
  DoTDLibSend(Format('{"@type": "sendMessage",' + '"chat_id": "%s",' +
    GetInlineKeyboard(aInlineKeyboard) + GetShowKeyboard(aShowKeyboard) +
    '"input_message_content": {' + '"@type": "inputMessageVideo",' + '"width": '
    + IntToStr(aWidth) + ', ' + '"height": ' + IntToStr(aHeight) + ', ' +
    '"duration": ' + IntToStr(aDuration) + ', ' + '"video": {' +
    '"@type": "inputFileLocal",' + '"path": "%s"' + '}' + '}' + '}',
    [aChatId, sgcEncodeJSON(aFilePath)]));
end;

procedure TsgcTDLib_Telegram_Client.SetAuthenticationCode(const aCode: String);
begin
  DoTDLibSend('{"@type": "checkAuthenticationCode", "code": "' + aCode + '"}');
end;

procedure TsgcTDLib_Telegram_Client.SetAuthenticationPassword
  (const aPassword: String);
begin
  DoTDLibSend('{"@type": "checkAuthenticationPassword", "password": "' +
    aPassword + '"}')

end;

procedure TsgcTDLib_Telegram_Client.SetFirstLastName(const aFirstName,
  aLastName: String);
begin
  DoTDLibSend('{"@type": "registerUser", "first_name": "' + aFirstName +
    '", "last_name": "' + aLastName + '"}')

end;

procedure TsgcTDLib_Telegram_Client.TDLibSend(const aRequest: String);
begin
  DoTDLibSend(aRequest);
end;

procedure TsgcTelegramAPI_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcTelegramAPI_Options then
  begin
    ApiId := TsgcTelegramAPI_Options(aSource).ApiId;
    ApiHash := TsgcTelegramAPI_Options(aSource).ApiHash;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcTelegram_Options.Create;
begin
  inherited;
  FAPI := TsgcTelegramAPI_Options.Create;
  FParameters := TsgcTelegramParameters_Options.Create;
  ErrorsLevel := tvbErrors;
  FDatabaseDirectory := '';
end;

destructor TsgcTelegram_Options.Destroy;
begin
  sgcFree(FParameters);
  sgcFree(FAPI);
  inherited;
end;

procedure TsgcTelegram_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcTelegram_Options then
  begin
    API := TsgcTelegram_Options(aSource).API;
    Parameters := TsgcTelegram_Options(aSource).Parameters;
    PhoneNumber := TsgcTelegram_Options(aSource).PhoneNumber;
    DBEncryptionKey := TsgcTelegram_Options(aSource).DBEncryptionKey;
    ErrorsLevel := TsgcTelegram_Options(aSource).ErrorsLevel;
    BotToken := TsgcTelegram_Options(aSource).BotToken;
    DatabaseDirectory := TsgcTelegram_Options(aSource).DatabaseDirectory;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcTelegram_Options.SetAPI(const Value: TsgcTelegramAPI_Options);
begin
  if Assigned(FAPI) then
    FAPI.Assign(Value);
end;

procedure TsgcTelegram_Options.SetParameters(const Value
  : TsgcTelegramParameters_Options);
begin
  if Assigned(FParameters) then
    FParameters.Assign(Value);
end;

constructor TsgcTelegramParameters_Options.Create;
begin
  inherited;
  LanguageCode := 'en';
  DeviceModel := 'Desktop';
  ApplicationVersion := '1.0';
  SystemVersion := 'Windows';
end;

procedure TsgcTelegramParameters_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcTelegramParameters_Options then
  begin
    LanguageCode := TsgcTelegramParameters_Options(aSource).LanguageCode;
    DeviceModel := TsgcTelegramParameters_Options(aSource).DeviceModel;
    ApplicationVersion := TsgcTelegramParameters_Options(aSource)
      .ApplicationVersion;
    SystemVersion := TsgcTelegramParameters_Options(aSource).SystemVersion;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcTelegramMessageBase.ReadJSON(const aJSON: String);
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aJSON);
    if oJSON.Node['message'] <> nil then
    begin
      if oJSON.Node['message'].Node['chat_id'] <> nil then
        ChatId := oJSON.Node['message'].Node['chat_id'].Value;
      if oJSON.Node['message'].Node['id'] <> nil then
        MessageId := oJSON.Node['message'].Node['id'].Value;
      if oJSON.Node['message'].Node['sender_user_id'] <> nil then
        SenderUserId := oJSON.Node['message'].Node['sender_user_id'].Value;
      if oJSON.Node['message'].Node['content'] <> nil then
      begin
        DoReadContent(TsgcObjectJSON(oJSON.Node['message'].Node['content']));
        if oJSON.Node['message'].Node['content'].Node['caption'] <> nil then
        begin
          if oJSON.Node['message'].Node['content'].Node['caption'].Node['text']
            <> nil then
            CaptionText := sgcDecodeJSON(oJSON.Node['message'].Node['content']
              .Node['caption'].Node['text'].Value);
        end;
      end;
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcTelegramMessageText.DoReadContent(const aJSON: TsgcObjectJSON);
begin
  inherited;
  if aJSON.Node['text'] <> nil then
  begin
    if aJSON.Node['@type'].Value = 'messageText' then
    begin
      if aJSON.Node['text'].Node['text'] <> nil then
        Text := sgcDecodeJSON(aJSON.Node['text'].Node['text'].Value);
    end;
  end;
end;

procedure TsgcTelegramMessageDocument.DoReadContent
  (const aJSON: TsgcObjectJSON);
begin
  inherited;
  if aJSON.Node['document'] <> nil then
  begin
    if aJSON.Node['document'].Node['file_name'] <> nil then
      FileName := sgcDecodeJSON(aJSON.Node['document'].Node['file_name'].Value);
    if aJSON.Node['document'].Node['mime_type'] <> nil then
      MimeType := aJSON.Node['document'].Node['mime_type'].Value;
    if aJSON.Node['document'].Node['document'] <> nil then
    begin
      if aJSON.Node['document'].Node['document'].Node['id'] <> nil then
        DocumentId := aJSON.Node['document'].Node['document'].Node['id'].Value;
      if aJSON.Node['document'].Node['document'].Node['size'] <> nil then
        Size := aJSON.Node['document'].Node['document'].Node['size'].Value;
      if aJSON.Node['document'].Node['document'].Node['local'] <> nil then
      begin
        if aJSON.Node['document'].Node['document'].Node['local'].Node['path'] <> nil
        then
          LocalPath := aJSON.Node['document'].Node['document'].Node['local']
            .Node['path'].Value;
      end;
      if aJSON.Node['document'].Node['document'].Node['remote'] <> nil then
      begin
        if aJSON.Node['document'].Node['document'].Node['remote'].Node['id'] <> nil
        then
          RemoteDocumentId := aJSON.Node['document'].Node['document'].Node
            ['remote'].Node['id'].Value;
      end;
    end;
  end;
end;

procedure TsgcTelegramMessagePhoto.DoReadContent(const aJSON: TsgcObjectJSON);
var
  oSizes, oPhoto: TsgcObjectJSON;
begin
  inherited;
  if aJSON.Node['photo'] <> nil then
  begin
    if aJSON.Node['photo'].Node['id'] <> nil then
      PhotoId := aJSON.Node['photo'].Node['id'].Value;
    oSizes := TsgcObjectJSON(aJSON.Node['photo'].Node['sizes']);
    if oSizes <> nil then
    begin
      if oSizes.JSONObject <> nil then
      begin
        if oSizes.JSONObject.Count > 0 then
        begin
          oPhoto := TsgcObjectJSON(oSizes.JSONObject.Item[0].JSONObject.Node
            ['photo']);
          if oPhoto <> nil then
          begin
            if oPhoto.Node['size'] <> nil then
            begin
              Size := oPhoto.Node['size'].Value;
              if oPhoto.Node['local'] <> nil then
              begin
                if oPhoto.Node['local'].Node['path'] <> nil then
                  LocalPath := oPhoto.Node['local'].Node['path'].Value;
              end;
              if oPhoto.Node['remote'] <> nil then
              begin
                if oPhoto.Node['remote'].Node['id'] <> nil then
                  RemotePhotoId := oPhoto.Node['remote'].Node['id'].Value;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TsgcTelegramChat.ReadJSON(const aJSON: String);
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aJSON);
    if oJSON.Node['chat'] <> nil then
    begin
      if oJSON.Node['chat'].Node['id'] <> nil then
        ChatId := oJSON.Node['chat'].Node['id'].Value;
      if oJSON.Node['chat'].Node['type'] <> nil then
      begin
        ChatType := oJSON.Node['chat'].Node['type'].Node['@type'].Value;
        if oJSON.Node['chat'].Node['type'].Node['supergroup_id'] <> nil then
          SuperGroupId := oJSON.Node['chat'].Node['type'].Node
            ['supergroup_id'].Value;
        if oJSON.Node['chat'].Node['type'].Node['supergroup_id'] <> nil then
          IsChannel := oJSON.Node['chat'].Node['type'].Node['is_channel'].Value;
      end;
      if oJSON.Node['chat'].Node['title'] <> nil then
        Title := sgcDecodeJSON(oJSON.Node['chat'].Node['title'].Value);
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcTelegramMessageVideo.DoReadContent(const aJSON: TsgcObjectJSON);
var
  oVideo: TsgcObjectJSON;
begin
  inherited;
  if aJSON.Node['video'] <> nil then
  begin
    if aJSON.Node['video'].Node['duration'] <> nil then
      Duration := aJSON.Node['video'].Node['duration'].Value;
    if aJSON.Node['video'].Node['width'] <> nil then
      Width := aJSON.Node['video'].Node['width'].Value;
    if aJSON.Node['video'].Node['height'] <> nil then
      Height := aJSON.Node['video'].Node['height'].Value;
    oVideo := TsgcObjectJSON(aJSON.Node['video'].Node['video']);
    if oVideo <> nil then
    begin
      if oVideo.Node['id'] <> nil then
        VideoId := oVideo.Node['id'].Value;
      if oVideo.Node['size'] <> nil then
        Size := oVideo.Node['size'].Value;
      if oVideo.Node['local'] <> nil then
      begin
        if oVideo.Node['local'].Node['path'] <> nil then
          LocalPath := oVideo.Node['local'].Node['path'].Value;
      end;
      if oVideo.Node['remote'] <> nil then
      begin
        if oVideo.Node['remote'].Node['id'] <> nil then
          RemoteVideoId := oVideo.Node['remote'].Node['id'].Value;
      end;
    end;
  end;
end;

destructor TsgcTelegramReplyMarkupInlineKeyboard.Destroy;
begin
  sgcFree(FButtons);
  inherited;
end;

procedure TsgcTelegramReplyMarkupInlineKeyboard.AddButtonTypeCallback(aText,
  aData: String);
begin
  Buttons.AddButtonTypeCallback(aText, aData);
end;

procedure TsgcTelegramReplyMarkupInlineKeyboard.AddButtonTypeURL(aText,
  aURL: String);
begin
  Buttons.AddButtonTypeURL(aText, aURL);
end;

function TsgcTelegramReplyMarkupInlineKeyboard.GetButtons
  : TsgcTelegramInlineKeyboardButtons;
begin
  if not Assigned(FButtons) then
    FButtons := TsgcTelegramInlineKeyboardButtons.Create;
  result := FButtons;
end;

function TsgcTelegramReplyMarkupInlineKeyboard.GetJSONText: string;
var
  i: Integer;
begin
  result := '';

  if Buttons.Count > 0 then
  begin
    result := '"reply_markup": {"@type": "replyMarkupInlineKeyboard","rows": [[';
    for i := 0 to Buttons.Count - 1 do
    begin
      if i > 0 then
        result := result + ',';
      case TsgcTelegramInlineKeyboardButton(Buttons[i]).ButtonType of
        tikbCallback:
          begin
            result := result +
              Format('{"@type": "inlineKeyboardButton","text": "%s",' +
              '"type": {"@type": "inlineKeyboardButtonTypeCallback","data": "%s"}}',
              [TsgcTelegramInlineKeyboardButtonTypeCallback(Buttons[i]).Text,
              TsgcTelegramInlineKeyboardButtonTypeCallback(Buttons[i]).Data]);
          end;
        tikbUrl:
          begin
            result := result +
              Format('{"@type": "inlineKeyboardButton","text": "%s",' +
              '"type": {"@type": "inlineKeyboardButtonTypeUrl","url": "%s"}}',
              [TsgcTelegramInlineKeyboardButtonTypeUrl(Buttons[i]).Text,
              TsgcTelegramInlineKeyboardButtonTypeUrl(Buttons[i]).Url]);
          end;
      end;
    end;
    result := result + ']]}';
  end;
end;

destructor TsgcTelegramInlineKeyboardButtons.Destroy;
begin
  Clear;
  inherited;
end;

procedure TsgcTelegramInlineKeyboardButtons.AddButtonTypeCallback(aText,
  aData: String);
var
  oButton: TsgcTelegramInlineKeyboardButtonTypeCallback;
begin
  oButton := TsgcTelegramInlineKeyboardButtonTypeCallback.Create;
  oButton.Text := sgcEncodeJSON(aText);
  oButton.Data := EncodeBase64(sgcEncodeJSON(aData));

  Add(oButton);
end;

procedure TsgcTelegramInlineKeyboardButtons.Clear;
begin
  while Count > 0 do
  begin
{$IFNDEF NEXTGEN}
    TObject(Items[0]).Free;
{$ENDIF}
    delete(0);
  end;
{$IFNDEF NEXTGEN} inherited; {$ENDIF}
end;

destructor TsgcTelegramCallbackQuery.Destroy;
begin
  sgcFree(FPayLoadData);
  inherited;
end;

function TsgcTelegramCallbackQuery.GetPayLoadData
  : TsgcTelegramCallbackQueryPayloadData;
begin
  if not Assigned(FPayLoadData) then
    FPayLoadData := TsgcTelegramCallbackQueryPayloadData.Create;
  result := FPayLoadData;
end;

procedure TsgcTelegramCallbackQuery.ReadJSON(const aJSON: String);
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aJSON);
    if oJSON.Node['id'] <> nil then
      Id := oJSON.Node['id'].Value;
    if oJSON.Node['sender_user_id'] <> nil then
      SenderUserId := oJSON.Node['sender_user_id'].Value;
    if oJSON.Node['message_id'] <> nil then
      MessageId := oJSON.Node['message_id'].Value;
    if oJSON.Node['chat_id'] <> nil then
      ChatId := oJSON.Node['chat_id'].Value;
    if oJSON.Node['message_id'] <> nil then
      MessageId := oJSON.Node['message_id'].Value;
    if oJSON.Node['chat_instance'] <> nil then
      ChatInstance := oJSON.Node['chat_instance'].Value;
    if oJSON.Node['payload'] <> nil then
    begin
      if oJSON.Node['payload'].Node['@type'] <> nil then
      begin
        if oJSON.Node['payload'].Node['@type'].Value = 'callbackQueryPayloadData'
        then
        begin
          if oJSON.Node['payload'].Node['data'] <> nil then
            PayLoadData.Data :=
              sgcDecodeJSON
              (DecodeBase64(oJSON.Node['payload'].Node['data'].Value));
        end;
      end;
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

constructor TsgcTelegramInlineKeyboardButtonTypeCallback.Create;
begin
  inherited;
  FButtonType := tikbCallback;
end;

constructor TsgcTelegramInlineKeyboardButtonTypeUrl.Create;
begin
  inherited;
  FButtonType := tikbUrl;
end;

procedure TsgcTelegramInlineKeyboardButtons.AddButtonTypeURL(aText,
  aURL: String);
var
  oButton: TsgcTelegramInlineKeyboardButtonTypeUrl;
begin
  oButton := TsgcTelegramInlineKeyboardButtonTypeUrl.Create;
  oButton.Text := sgcEncodeJSON(aText);
  oButton.Url := sgcEncodeJSON(aURL);

  Add(oButton);
end;

constructor TsgcTelegramReplyMarkupShowKeyboard.Create;
begin
  inherited;
  ResizeKeyboard := False;
  OneTime := False;
end;

destructor TsgcTelegramReplyMarkupShowKeyboard.Destroy;
begin
  sgcFree(FButtons);
  inherited;
end;

procedure TsgcTelegramReplyMarkupShowKeyboard.AddButtonTypeRequestLocation
  (aText: String);
begin
  Buttons.AddButtonTypeRequestLocation(aText);
end;

procedure TsgcTelegramReplyMarkupShowKeyboard.AddButtonTypeRequestPhoneNumber
  (aText: String);
begin
  Buttons.AddButtonTypeRequestPhoneNumber(aText);
end;

procedure TsgcTelegramReplyMarkupShowKeyboard.AddButtonTypeText(aText: String);
begin
  Buttons.AddButtonTypeText(aText);
end;

function TsgcTelegramReplyMarkupShowKeyboard.GetButtons
  : TsgcTelegramKeyboardButtons;
begin
  if not Assigned(FButtons) then
    FButtons := TsgcTelegramKeyboardButtons.Create;
  result := FButtons;
end;

function TsgcTelegramReplyMarkupShowKeyboard.GetJSONText: string;

  function GetBoolAsString(aValue: Boolean): string;
  begin
    if aValue then
      result := 'true'
    else
      result := 'false';
  end;

var
  i: Integer;
begin
  result := '';

  if Buttons.Count > 0 then
  begin
    result := Format('"reply_markup": {"@type": "replyMarkupShowKeyboard", ' +
      '"resize_keyboard": %s, "one_time": %s, "rows": [[',
      [GetBoolAsString(ResizeKeyboard), GetBoolAsString(OneTime)]);
    for i := 0 to Buttons.Count - 1 do
    begin
      if i > 0 then
        result := result + ',';
      case TsgcTelegramKeyboardButton(Buttons[i]).ButtonType of
        tkbtRequestLocation:
          begin
            result := result + Format('{"@type": "keyboardButton","text": "%s",'
              + '"type": {"@type": "keyboardButtonTypeRequestLocation"}}',
              [TsgcTelegramKeyboardButtonTypeRequestLocation(Buttons[i]).Text]);
          end;
        tkbtRequestPhoneNumber:
          begin
            result := result + Format('{"@type": "keyboardButton","text": "%s",'
              + '"type": {"@type": "keyboardButtonTypeRequestPhoneNumber"}}',
              [TsgcTelegramKeyboardButtonTypeRequestPhoneNumber(Buttons
              [i]).Text]);
          end;
        tkbtText:
          begin
            result := result + Format('{"@type": "keyboardButton","text": "%s",'
              + '"type": {"@type": "keyboardButtonTypeText"}}',
              [TsgcTelegramKeyboardButtonTypeText(Buttons[i]).Text]);
          end;
      end;
    end;
    result := result + ']]}';
  end;
end;

constructor TsgcTelegramKeyboardButtonTypeRequestLocation.Create;
begin
  inherited;
  FButtonType := tkbtRequestLocation;
end;

constructor TsgcTelegramKeyboardButtonTypeRequestPhoneNumber.Create;
begin
  inherited;
  FButtonType := tkbtRequestPhoneNumber;
end;

constructor TsgcTelegramKeyboardButtonTypeText.Create;
begin
  inherited;
  FButtonType := tkbtText;
end;

destructor TsgcTelegramKeyboardButtons.Destroy;
begin
  Clear;
  inherited;
end;

procedure TsgcTelegramKeyboardButtons.AddButtonTypeRequestLocation
  (aText: String);
var
  oButton: TsgcTelegramKeyboardButtonTypeRequestLocation;
begin
  oButton := TsgcTelegramKeyboardButtonTypeRequestLocation.Create;
  oButton.Text := sgcEncodeJSON(aText);

  Add(oButton);
end;

procedure TsgcTelegramKeyboardButtons.AddButtonTypeRequestPhoneNumber
  (aText: String);
var
  oButton: TsgcTelegramKeyboardButtonTypeRequestPhoneNumber;
begin
  oButton := TsgcTelegramKeyboardButtonTypeRequestPhoneNumber.Create;
  oButton.Text := sgcEncodeJSON(aText);

  Add(oButton);
end;

procedure TsgcTelegramKeyboardButtons.AddButtonTypeText(aText: String);
var
  oButton: TsgcTelegramKeyboardButtonTypeText;
begin
  oButton := TsgcTelegramKeyboardButtonTypeText.Create;
  oButton.Text := sgcEncodeJSON(aText);

  Add(oButton);
end;

procedure TsgcTelegramKeyboardButtons.Clear;
begin
  while Count > 0 do
  begin
{$IFNDEF NEXTGEN}
    TObject(Items[0]).Free;
{$ENDIF}
    delete(0);
  end;
{$IFNDEF NEXTGEN} inherited; {$ENDIF}
end;

initialization

// -->start sgc_trial
{$IFDEF SGC_TRIAL}
if ((Now > EncodeDate(2022, 6, 1)) and (FormatDateTime('s', Now) = '3')) then
begin
  raise Exception.Create
    (DecodeBase64
    ('VGhpcyBkZW1vIHZlcnNpb24gY2FuIG9ubHkgcnVuIGZvciBhIGxpbWl0ZWQgdGltZSBwZXJpb2QuIFBsZWFzZSB2aXNpdCB3d3cuZXNlZ2VjZS5jb20gdG8gcHVyY2hhc2UgeW91ciBjb3B5IG9mIHRoZSBsaWJyYXJ5Lg==')
    );
end;
{$ENDIF}
// <--end sgc_trial

finalization

{$ENDIF}

end.
