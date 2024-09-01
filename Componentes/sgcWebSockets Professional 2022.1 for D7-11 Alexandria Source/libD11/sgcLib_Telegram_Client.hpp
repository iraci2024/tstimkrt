// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcLib_Telegram_Client.pas' rev: 35.00 (Windows)

#ifndef Sgclib_telegram_clientHPP
#define Sgclib_telegram_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Contnrs.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgclib_telegram_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcTelegramMessageBase;
class DELPHICLASS TsgcTelegramMessageText;
class DELPHICLASS TsgcTelegramMessageDocument;
class DELPHICLASS TsgcTelegramMessagePhoto;
class DELPHICLASS TsgcTelegramMessageVideo;
class DELPHICLASS TsgcTelegramChat;
class DELPHICLASS TsgcTelegramInlineKeyboardButton;
class DELPHICLASS TsgcTelegramInlineKeyboardButtonTypeCallback;
class DELPHICLASS TsgcTelegramInlineKeyboardButtonTypeUrl;
class DELPHICLASS TsgcTelegramInlineKeyboardButtons;
class DELPHICLASS TsgcTelegramReplyMarkupInlineKeyboard;
class DELPHICLASS TsgcTelegramKeyboardButton;
class DELPHICLASS TsgcTelegramKeyboardButtonTypeRequestLocation;
class DELPHICLASS TsgcTelegramKeyboardButtonTypeRequestPhoneNumber;
class DELPHICLASS TsgcTelegramKeyboardButtonTypeText;
class DELPHICLASS TsgcTelegramKeyboardButtons;
class DELPHICLASS TsgcTelegramReplyMarkupShowKeyboard;
class DELPHICLASS TsgcTelegramCallbackQueryPayloadData;
class DELPHICLASS TsgcTelegramCallbackQuery;
class DELPHICLASS TsgcTelegramAPI_Options;
class DELPHICLASS TsgcTelegramParameters_Options;
class DELPHICLASS TsgcTelegram_Options;
class DELPHICLASS TsgcTDLib_Telegram_Client;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcTelegramInlineKeyboardButtonType : unsigned char { tikbCallback, tikbUrl };

enum DECLSPEC_DENUM TsgcTelegramKeyboardButtonType : unsigned char { tkbtRequestLocation, tkbtRequestPhoneNumber, tkbtText };

enum DECLSPEC_DENUM TsgcTelegramSuperGroupMembersFilter : unsigned char { tsgmFilterNone, tsgmFilterAdministrators, tsgmFilterBanned, tsgmFilterBots, tsgmFilterContacts, tsgmFilterMention, tsgmFilterRecent, tsgmFilterRestricted, tsgmFilterSearch };

class PASCALIMPLEMENTATION TsgcTelegramMessageBase : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FCaptionText;
	System::UnicodeString FChatId;
	__int64 FMessageId;
	__int64 FSenderUserId;
	
protected:
	virtual void __fastcall ReadJSON(const System::UnicodeString aJSON);
	virtual void __fastcall DoReadContent(Sgcjson::TsgcObjectJSON* const aJSON) = 0 ;
	
public:
	__property System::UnicodeString CaptionText = {read=FCaptionText, write=FCaptionText};
	__property System::UnicodeString ChatId = {read=FChatId, write=FChatId};
	__property __int64 MessageId = {read=FMessageId, write=FMessageId};
	__property __int64 SenderUserId = {read=FSenderUserId, write=FSenderUserId};
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramMessageBase() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramMessageBase() { }
	
};


class PASCALIMPLEMENTATION TsgcTelegramMessageText : public TsgcTelegramMessageBase
{
	typedef TsgcTelegramMessageBase inherited;
	
private:
	System::UnicodeString FText;
	
protected:
	virtual void __fastcall DoReadContent(Sgcjson::TsgcObjectJSON* const aJSON);
	
public:
	__property System::UnicodeString Text = {read=FText, write=FText};
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramMessageText() : TsgcTelegramMessageBase() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramMessageText() { }
	
};


class PASCALIMPLEMENTATION TsgcTelegramMessageDocument : public TsgcTelegramMessageBase
{
	typedef TsgcTelegramMessageBase inherited;
	
private:
	System::UnicodeString FFileName;
	System::UnicodeString FDocumentId;
	System::UnicodeString FLocalPath;
	System::UnicodeString FMimeType;
	System::UnicodeString FRemoteDocumentId;
	__int64 FSize;
	
protected:
	virtual void __fastcall DoReadContent(Sgcjson::TsgcObjectJSON* const aJSON);
	
public:
	__property System::UnicodeString FileName = {read=FFileName, write=FFileName};
	__property System::UnicodeString DocumentId = {read=FDocumentId, write=FDocumentId};
	__property System::UnicodeString LocalPath = {read=FLocalPath, write=FLocalPath};
	__property System::UnicodeString MimeType = {read=FMimeType, write=FMimeType};
	__property System::UnicodeString RemoteDocumentId = {read=FRemoteDocumentId, write=FRemoteDocumentId};
	__property __int64 Size = {read=FSize, write=FSize};
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramMessageDocument() : TsgcTelegramMessageBase() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramMessageDocument() { }
	
};


class PASCALIMPLEMENTATION TsgcTelegramMessagePhoto : public TsgcTelegramMessageBase
{
	typedef TsgcTelegramMessageBase inherited;
	
private:
	System::UnicodeString FLocalPath;
	System::UnicodeString FPhotoId;
	System::UnicodeString FRemotePhotoId;
	__int64 FSize;
	
protected:
	virtual void __fastcall DoReadContent(Sgcjson::TsgcObjectJSON* const aJSON);
	
public:
	__property System::UnicodeString LocalPath = {read=FLocalPath, write=FLocalPath};
	__property System::UnicodeString PhotoId = {read=FPhotoId, write=FPhotoId};
	__property System::UnicodeString RemotePhotoId = {read=FRemotePhotoId, write=FRemotePhotoId};
	__property __int64 Size = {read=FSize, write=FSize};
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramMessagePhoto() : TsgcTelegramMessageBase() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramMessagePhoto() { }
	
};


class PASCALIMPLEMENTATION TsgcTelegramMessageVideo : public TsgcTelegramMessageBase
{
	typedef TsgcTelegramMessageBase inherited;
	
private:
	System::UnicodeString FLocalPath;
	System::UnicodeString FRemoteVideoId;
	__int64 FDuration;
	int FHeight;
	__int64 FSize;
	System::UnicodeString FVideoId;
	int FWidth;
	
protected:
	virtual void __fastcall DoReadContent(Sgcjson::TsgcObjectJSON* const aJSON);
	
public:
	__property System::UnicodeString LocalPath = {read=FLocalPath, write=FLocalPath};
	__property System::UnicodeString RemoteVideoId = {read=FRemoteVideoId, write=FRemoteVideoId};
	__property __int64 Duration = {read=FDuration, write=FDuration};
	__property int Height = {read=FHeight, write=FHeight, nodefault};
	__property __int64 Size = {read=FSize, write=FSize};
	__property System::UnicodeString VideoId = {read=FVideoId, write=FVideoId};
	__property int Width = {read=FWidth, write=FWidth, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramMessageVideo() : TsgcTelegramMessageBase() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramMessageVideo() { }
	
};


class PASCALIMPLEMENTATION TsgcTelegramChat : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FChatId;
	System::UnicodeString FChatType;
	bool FIsChannel;
	__int64 FSuperGroupId;
	System::UnicodeString FTitle;
	
protected:
	virtual void __fastcall ReadJSON(const System::UnicodeString aJSON);
	
public:
	__property System::UnicodeString ChatId = {read=FChatId, write=FChatId};
	__property System::UnicodeString ChatType = {read=FChatType, write=FChatType};
	__property bool IsChannel = {read=FIsChannel, write=FIsChannel, nodefault};
	__property __int64 SuperGroupId = {read=FSuperGroupId, write=FSuperGroupId};
	__property System::UnicodeString Title = {read=FTitle, write=FTitle};
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramChat() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramChat() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramInlineKeyboardButton : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcTelegramInlineKeyboardButtonType FButtonType;
	System::UnicodeString FText;
	
protected:
	__property TsgcTelegramInlineKeyboardButtonType ButtonType = {read=FButtonType, nodefault};
	
public:
	__property System::UnicodeString Text = {read=FText, write=FText};
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramInlineKeyboardButton() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramInlineKeyboardButton() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramInlineKeyboardButtonTypeCallback : public TsgcTelegramInlineKeyboardButton
{
	typedef TsgcTelegramInlineKeyboardButton inherited;
	
private:
	System::UnicodeString FData;
	
public:
	__fastcall virtual TsgcTelegramInlineKeyboardButtonTypeCallback();
	__property System::UnicodeString Data = {read=FData, write=FData};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramInlineKeyboardButtonTypeCallback() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramInlineKeyboardButtonTypeUrl : public TsgcTelegramInlineKeyboardButton
{
	typedef TsgcTelegramInlineKeyboardButton inherited;
	
private:
	System::UnicodeString FUrl;
	
public:
	__fastcall virtual TsgcTelegramInlineKeyboardButtonTypeUrl();
	__property System::UnicodeString Url = {read=FUrl, write=FUrl};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramInlineKeyboardButtonTypeUrl() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramInlineKeyboardButtons : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	__fastcall virtual ~TsgcTelegramInlineKeyboardButtons();
	virtual void __fastcall Clear();
	void __fastcall AddButtonTypeCallback(System::UnicodeString aText, System::UnicodeString aData);
	void __fastcall AddButtonTypeURL(System::UnicodeString aText, System::UnicodeString aURL);
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramInlineKeyboardButtons() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramReplyMarkupInlineKeyboard : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcTelegramInlineKeyboardButtons* FButtons;
	TsgcTelegramInlineKeyboardButtons* __fastcall GetButtons();
	
protected:
	System::UnicodeString __fastcall GetJSONText();
	__property TsgcTelegramInlineKeyboardButtons* Buttons = {read=GetButtons, write=FButtons};
	
public:
	__fastcall virtual ~TsgcTelegramReplyMarkupInlineKeyboard();
	void __fastcall AddButtonTypeCallback(System::UnicodeString aText, System::UnicodeString aData);
	void __fastcall AddButtonTypeURL(System::UnicodeString aText, System::UnicodeString aURL);
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramReplyMarkupInlineKeyboard() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramKeyboardButton : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcTelegramKeyboardButtonType FButtonType;
	System::UnicodeString FText;
	
protected:
	__property TsgcTelegramKeyboardButtonType ButtonType = {read=FButtonType, nodefault};
	
public:
	__property System::UnicodeString Text = {read=FText, write=FText};
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramKeyboardButton() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramKeyboardButton() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramKeyboardButtonTypeRequestLocation : public TsgcTelegramKeyboardButton
{
	typedef TsgcTelegramKeyboardButton inherited;
	
public:
	__fastcall virtual TsgcTelegramKeyboardButtonTypeRequestLocation();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramKeyboardButtonTypeRequestLocation() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramKeyboardButtonTypeRequestPhoneNumber : public TsgcTelegramKeyboardButton
{
	typedef TsgcTelegramKeyboardButton inherited;
	
public:
	__fastcall virtual TsgcTelegramKeyboardButtonTypeRequestPhoneNumber();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramKeyboardButtonTypeRequestPhoneNumber() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramKeyboardButtonTypeText : public TsgcTelegramKeyboardButton
{
	typedef TsgcTelegramKeyboardButton inherited;
	
public:
	__fastcall virtual TsgcTelegramKeyboardButtonTypeText();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramKeyboardButtonTypeText() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramKeyboardButtons : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	__fastcall virtual ~TsgcTelegramKeyboardButtons();
	virtual void __fastcall Clear();
	void __fastcall AddButtonTypeRequestLocation(System::UnicodeString aText);
	void __fastcall AddButtonTypeRequestPhoneNumber(System::UnicodeString aText);
	void __fastcall AddButtonTypeText(System::UnicodeString aText);
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramKeyboardButtons() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramReplyMarkupShowKeyboard : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcTelegramKeyboardButtons* FButtons;
	bool FOneTime;
	bool FResizeKeyboard;
	TsgcTelegramKeyboardButtons* __fastcall GetButtons();
	
protected:
	System::UnicodeString __fastcall GetJSONText();
	__property TsgcTelegramKeyboardButtons* Buttons = {read=GetButtons, write=FButtons};
	
public:
	__fastcall virtual TsgcTelegramReplyMarkupShowKeyboard();
	__fastcall virtual ~TsgcTelegramReplyMarkupShowKeyboard();
	void __fastcall AddButtonTypeRequestLocation(System::UnicodeString aText);
	void __fastcall AddButtonTypeRequestPhoneNumber(System::UnicodeString aText);
	void __fastcall AddButtonTypeText(System::UnicodeString aText);
	__property bool OneTime = {read=FOneTime, write=FOneTime, nodefault};
	__property bool ResizeKeyboard = {read=FResizeKeyboard, write=FResizeKeyboard, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramCallbackQueryPayloadData : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FData;
	
public:
	__property System::UnicodeString Data = {read=FData, write=FData};
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramCallbackQueryPayloadData() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcTelegramCallbackQueryPayloadData() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcTelegramCallbackQuery : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FChatId;
	System::UnicodeString FChatInstance;
	System::UnicodeString FId;
	__int64 FMessageId;
	TsgcTelegramCallbackQueryPayloadData* FPayLoadData;
	__int64 FSenderUserId;
	TsgcTelegramCallbackQueryPayloadData* __fastcall GetPayLoadData();
	
protected:
	virtual void __fastcall ReadJSON(const System::UnicodeString aJSON);
	
public:
	__fastcall virtual ~TsgcTelegramCallbackQuery();
	__property System::UnicodeString ChatId = {read=FChatId, write=FChatId};
	__property System::UnicodeString ChatInstance = {read=FChatInstance, write=FChatInstance};
	__property System::UnicodeString Id = {read=FId, write=FId};
	__property __int64 MessageId = {read=FMessageId, write=FMessageId};
	__property TsgcTelegramCallbackQueryPayloadData* PayLoadData = {read=GetPayLoadData, write=FPayLoadData};
	__property __int64 SenderUserId = {read=FSenderUserId, write=FSenderUserId};
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramCallbackQuery() : System::TObject() { }
	
};


typedef void __fastcall (__closure *TsgcTelegramConnectionStatusEvent)(System::TObject* Sender, const System::UnicodeString Status);

typedef void __fastcall (__closure *TsgcTelegramAuthorizationStatusEvent)(System::TObject* Sender, const System::UnicodeString Status);

typedef void __fastcall (__closure *TsgcTelegramBeforeReadEvent)(System::TObject* Sender, const System::UnicodeString Text, bool &Handled);

typedef void __fastcall (__closure *TsgcTelegramAuthenticationCodeEvent)(System::TObject* Sender, System::UnicodeString &Code);

typedef void __fastcall (__closure *TsgcTelegramRegisterUserEvent)(System::TObject* Sender, System::UnicodeString &FirstName, System::UnicodeString &LastName);

typedef void __fastcall (__closure *TsgcTelegramAuthenticationPasswordEvent)(System::TObject* Sender, System::UnicodeString &Password);

typedef void __fastcall (__closure *TsgcTelegramExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

typedef void __fastcall (__closure *TsgcTelegramEventEvent)(System::TObject* Sender, const System::UnicodeString Event, const System::UnicodeString Text);

typedef void __fastcall (__closure *TsgcTelegramMessageTextEvent)(System::TObject* Sender, TsgcTelegramMessageText* MessageText);

typedef void __fastcall (__closure *TsgcTelegramMessageDocumentEvent)(System::TObject* Sender, TsgcTelegramMessageDocument* MessageDocument);

typedef void __fastcall (__closure *TsgcTelegramMessagePhotoEvent)(System::TObject* Sender, TsgcTelegramMessagePhoto* MessagePhoto);

typedef void __fastcall (__closure *TsgcTelegramMessageVideoEvent)(System::TObject* Sender, TsgcTelegramMessageVideo* MessageVideo);

typedef void __fastcall (__closure *TsgcTelegramNewChatEvent)(System::TObject* Sender, TsgcTelegramChat* Chat);

typedef void __fastcall (__closure *TsgcTelegramNewCallbackQueryEvent)(System::TObject* Sender, TsgcTelegramCallbackQuery* CallbackQuery);

enum DECLSPEC_DENUM TsgcTelegramErrorLevels : unsigned char { tvbFatalErrors, tvbErrors, tvbWarnings, tvbInformational, tvbDebug, tvbVerboseDebug };

enum DECLSPEC_DENUM TsgcTelegramProxyTypeValues : unsigned char { tptProxyHTTP, tptProxyMTProto, tptProxySocks5 };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramAPI_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiHash;
	System::UnicodeString FApiId;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiHash = {read=FApiHash, write=FApiHash};
	__property System::UnicodeString ApiId = {read=FApiId, write=FApiId};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcTelegramAPI_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcTelegramAPI_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegramParameters_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApplicationVersion;
	System::UnicodeString FDeviceModel;
	System::UnicodeString FLanguageCode;
	System::UnicodeString FSystemVersion;
	
public:
	__fastcall virtual TsgcTelegramParameters_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApplicationVersion = {read=FApplicationVersion, write=FApplicationVersion};
	__property System::UnicodeString DeviceModel = {read=FDeviceModel, write=FDeviceModel};
	__property System::UnicodeString LanguageCode = {read=FLanguageCode, write=FLanguageCode};
	__property System::UnicodeString SystemVersion = {read=FSystemVersion, write=FSystemVersion};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcTelegramParameters_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcTelegram_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcTelegramAPI_Options* FAPI;
	System::UnicodeString FBotToken;
	System::UnicodeString FDatabaseDirectory;
	System::UnicodeString FDBEncryptionKey;
	TsgcTelegramErrorLevels FErrorsLevel;
	TsgcTelegramParameters_Options* FParameters;
	System::UnicodeString FPhoneNumber;
	void __fastcall SetAPI(TsgcTelegramAPI_Options* const Value);
	void __fastcall SetParameters(TsgcTelegramParameters_Options* const Value);
	
public:
	__fastcall virtual TsgcTelegram_Options();
	__fastcall virtual ~TsgcTelegram_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcTelegramAPI_Options* API = {read=FAPI, write=SetAPI};
	__property System::UnicodeString BotToken = {read=FBotToken, write=FBotToken};
	__property System::UnicodeString DatabaseDirectory = {read=FDatabaseDirectory, write=FDatabaseDirectory};
	__property TsgcTelegramParameters_Options* Parameters = {read=FParameters, write=SetParameters};
	__property System::UnicodeString DBEncryptionKey = {read=FDBEncryptionKey, write=FDBEncryptionKey};
	__property TsgcTelegramErrorLevels ErrorsLevel = {read=FErrorsLevel, write=FErrorsLevel, nodefault};
	__property System::UnicodeString PhoneNumber = {read=FPhoneNumber, write=FPhoneNumber};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcTDLib_Telegram_Client : public Sgcbase_classes::TsgcComponent_Base
{
	typedef Sgcbase_classes::TsgcComponent_Base inherited;
	
private:
	Sgcbase_classes::TsgcTimer* FThread;
	Sgcbase_classes::TsgcTimer* __fastcall GetThread();
	
protected:
	virtual void __fastcall DoStartThread();
	virtual void __fastcall DoStopThread();
	virtual void __fastcall OnThreadEvent(System::TObject* Sender);
	virtual void __fastcall OnThreadExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	__property Sgcbase_classes::TsgcTimer* Thread = {read=GetThread};
	
private:
	void *FClient;
	
public:
	__property void * Client = {read=FClient};
	
private:
	TsgcTelegram_Options* FTelegram;
	Sgcwebsocket_types::TwsNotifyEvent FNotifyEvents;
	void __fastcall SetTelegram(TsgcTelegram_Options* const Value);
	
public:
	__property TsgcTelegram_Options* Telegram = {read=FTelegram, write=SetTelegram};
	__property Sgcwebsocket_types::TwsNotifyEvent NotifyEvents = {read=FNotifyEvents, write=FNotifyEvents, nodefault};
	__fastcall virtual TsgcTDLib_Telegram_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcTDLib_Telegram_Client();
	
private:
	int FId;
	int __fastcall GetId();
	bool __fastcall GetActive();
	void __fastcall SetActive(const bool Value);
	
protected:
	virtual void __fastcall DoConnect();
	virtual void __fastcall DoDisconnect();
	
public:
	__property int Id = {read=GetId, nodefault};
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	
protected:
	virtual void __fastcall OnFatalErrorCallback(char * Error_Message);
	
private:
	System::UnicodeString __fastcall GetDatabaseDirectory();
	void __fastcall DoTDLibSend(const System::UnicodeString aRequest);
	System::UnicodeString __fastcall DoTDLibExecute(const System::UnicodeString aRequest);
	
protected:
	virtual void __fastcall DoReadEvent(const System::UnicodeString aJSON);
	
public:
	void __fastcall TDLibSend(const System::UnicodeString aRequest);
	
protected:
	void __fastcall SetAuthenticationCode(const System::UnicodeString aCode);
	void __fastcall SetAuthenticationPassword(const System::UnicodeString aPassword);
	void __fastcall SetFirstLastName(const System::UnicodeString aFirstName, const System::UnicodeString aLastName);
	System::UnicodeString __fastcall GetInlineKeyboard(TsgcTelegramReplyMarkupInlineKeyboard* const aInlineKeyboard = (TsgcTelegramReplyMarkupInlineKeyboard*)(0x0));
	System::UnicodeString __fastcall GetShowKeyboard(TsgcTelegramReplyMarkupShowKeyboard* const aShowKeyboard = (TsgcTelegramReplyMarkupShowKeyboard*)(0x0));
	System::UnicodeString __fastcall GetTextEntitiesFromText(const System::UnicodeString aText, System::UnicodeString &Entities);
	
public:
	void __fastcall SendTextMessage(const System::UnicodeString aChatId, const System::UnicodeString aText, TsgcTelegramReplyMarkupInlineKeyboard* const aInlineKeyboard = (TsgcTelegramReplyMarkupInlineKeyboard*)(0x0), TsgcTelegramReplyMarkupShowKeyboard* const aShowKeyboard = (TsgcTelegramReplyMarkupShowKeyboard*)(0x0));
	void __fastcall SendRichTextMessage(const System::UnicodeString aChatId, const System::UnicodeString aText, TsgcTelegramReplyMarkupInlineKeyboard* const aInlineKeyboard = (TsgcTelegramReplyMarkupInlineKeyboard*)(0x0), TsgcTelegramReplyMarkupShowKeyboard* const aShowKeyboard = (TsgcTelegramReplyMarkupShowKeyboard*)(0x0));
	void __fastcall SendDocumentMessage(const System::UnicodeString aChatId, const System::UnicodeString aFilePath, TsgcTelegramReplyMarkupInlineKeyboard* const aInlineKeyboard = (TsgcTelegramReplyMarkupInlineKeyboard*)(0x0), TsgcTelegramReplyMarkupShowKeyboard* const aShowKeyboard = (TsgcTelegramReplyMarkupShowKeyboard*)(0x0));
	void __fastcall SendPhotoMessage(const System::UnicodeString aChatId, const System::UnicodeString aFilePath, int aWidth = 0x0, int aHeight = 0x0, TsgcTelegramReplyMarkupInlineKeyboard* const aInlineKeyboard = (TsgcTelegramReplyMarkupInlineKeyboard*)(0x0), TsgcTelegramReplyMarkupShowKeyboard* const aShowKeyboard = (TsgcTelegramReplyMarkupShowKeyboard*)(0x0));
	void __fastcall SendVideoMessage(const System::UnicodeString aChatId, const System::UnicodeString aFilePath, int aWidth = 0x0, int aHeight = 0x0, __int64 aDuration = 0LL, TsgcTelegramReplyMarkupInlineKeyboard* const aInlineKeyboard = (TsgcTelegramReplyMarkupInlineKeyboard*)(0x0), TsgcTelegramReplyMarkupShowKeyboard* const aShowKeyboard = (TsgcTelegramReplyMarkupShowKeyboard*)(0x0));
	void __fastcall DeleteSupergroup(__int64 aGroupId);
	
protected:
	void __fastcall DoAddChatMembers(const System::UnicodeString aChatId, const System::UnicodeString aUsersIds);
	
public:
	void __fastcall AddChatMember(const System::UnicodeString aChatId, const __int64 aUserId, int aForwardLimit = 0x64);
	void __fastcall AddChatMembers(const System::UnicodeString aChatId, const __int64 *aUserIds, const int aUserIds_High);
	void __fastcall JoinChatByInviteLink(const System::UnicodeString aLink);
	
private:
	System::UnicodeString __fastcall GetSupergroupMembersFilter(const TsgcTelegramSuperGroupMembersFilter aSupergroupMembersFilter = (TsgcTelegramSuperGroupMembersFilter)(0x0), const System::UnicodeString aSuperGroupMembersQuery = System::UnicodeString());
	
public:
	void __fastcall GetChatMember(const System::UnicodeString aChatId, __int64 aUserId);
	void __fastcall GetBasicGroupFullInfo(__int64 aGroupId);
	void __fastcall GetSupergroupMembers(__int64 aSuperGroupId, const TsgcTelegramSuperGroupMembersFilter aSupergroupMembersFilter = (TsgcTelegramSuperGroupMembersFilter)(0x0), const System::UnicodeString aSuperGroupMembersQuery = System::UnicodeString(), int aOffset = 0x0, int aLimit = 0xc8);
	
protected:
	void __fastcall DoCreateNewBasicGroupChat(const System::UnicodeString aUserIds, const System::UnicodeString aTitle);
	
public:
	void __fastcall CreateNewSecretChat(const __int64 aUserId);
	void __fastcall CreateNewBasicGroupChat(const __int64 *aUserIds, const int aUserIds_High, const System::UnicodeString aTitle);
	void __fastcall CreateNewSupergroupChat(const System::UnicodeString aTitle, bool aIsChannel = false, const System::UnicodeString aDescription = System::UnicodeString());
	void __fastcall CreatePrivateChat(const __int64 aUserId, bool aForce = false);
	void __fastcall GetChat(System::UnicodeString aChatId);
	void __fastcall GetChats(int aOffsetOrder = 0x7fffffff, int aOffsetChatId = 0x0, int aLimit = 0x32);
	void __fastcall GetChatHistory(System::UnicodeString aChatId, System::UnicodeString aFromMessageId = L"0", int aOffset = 0x0, int aLimit = 0xa);
	void __fastcall GetUser(int aUserId);
	void __fastcall LogOut();
	void __fastcall AddProxyHTTP(const System::UnicodeString aServer, int aPort, const System::UnicodeString aUserName, const System::UnicodeString aPassword, bool aHTTPOnly);
	void __fastcall AddProxyMTProto(const System::UnicodeString aServer, int aPort, const System::UnicodeString aSecret);
	void __fastcall AddProxySocks5(const System::UnicodeString aServer, int aPort, const System::UnicodeString aUserName, const System::UnicodeString aPassword);
	void __fastcall EnableProxy(int aId);
	void __fastcall DisableProxy();
	void __fastcall RemoveProxy(int aId);
	void __fastcall GetProxies();
	
private:
	__int64 FMyId;
	
public:
	__property __int64 MyId = {read=FMyId};
	
private:
	TsgcTelegramAuthenticationCodeEvent FOnAuthenticationCode;
	TsgcTelegramAuthenticationPasswordEvent FOnAuthenticationPassword;
	TsgcTelegramAuthorizationStatusEvent FOnAuthorizationStatus;
	TsgcTelegramBeforeReadEvent FOnBeforeReadEvent;
	TsgcTelegramExceptionEvent FOnException;
	TsgcTelegramEventEvent FOnEvent;
	TsgcTelegramRegisterUserEvent FOnRegisterUser;
	TsgcTelegramConnectionStatusEvent FOnConnectionStatus;
	TsgcTelegramMessageDocumentEvent FOnMessageDocument;
	TsgcTelegramMessagePhotoEvent FOnMessagePhoto;
	TsgcTelegramMessageTextEvent FOnMessageText;
	TsgcTelegramMessageVideoEvent FOnMessageVideo;
	TsgcTelegramNewCallbackQueryEvent FOnNewCallbackQuery;
	TsgcTelegramNewChatEvent FOnNewChat;
	
protected:
	virtual void __fastcall DoAuthenticationCodeEvent(System::UnicodeString &Code);
	virtual void __fastcall DoRegisterUserEvent(System::UnicodeString &FirstName, System::UnicodeString &LastName);
	virtual void __fastcall DoAuthenticationPasswordEvent(System::UnicodeString &Password);
	virtual void __fastcall DoExceptionEvent(System::Sysutils::Exception* E);
	virtual void __fastcall DoEventEvent(const System::UnicodeString aEvent, const System::UnicodeString Text);
	bool __fastcall DoBeforeReadEvent(const System::UnicodeString Text);
	virtual void __fastcall DoConnectionStatusEvent(const System::UnicodeString Status);
	virtual void __fastcall DoAuthorizationStatusEvent(const System::UnicodeString Status);
	virtual void __fastcall DoMessageTextEvent(const System::UnicodeString aJSON);
	virtual void __fastcall DoMessageDocumentEvent(const System::UnicodeString aJSON);
	virtual void __fastcall DoMessagePhotoEvent(const System::UnicodeString aJSON);
	virtual void __fastcall DoMessageVideoEvent(const System::UnicodeString aJSON);
	virtual void __fastcall DoNewChatEvent(const System::UnicodeString aJSON);
	virtual void __fastcall DoNewCallbackQueryEvent(const System::UnicodeString aJSON);
	
public:
	__property TsgcTelegramAuthenticationCodeEvent OnAuthenticationCode = {read=FOnAuthenticationCode, write=FOnAuthenticationCode};
	__property TsgcTelegramAuthenticationPasswordEvent OnAuthenticationPassword = {read=FOnAuthenticationPassword, write=FOnAuthenticationPassword};
	__property TsgcTelegramAuthorizationStatusEvent OnAuthorizationStatus = {read=FOnAuthorizationStatus, write=FOnAuthorizationStatus};
	__property TsgcTelegramBeforeReadEvent OnBeforeReadEvent = {read=FOnBeforeReadEvent, write=FOnBeforeReadEvent};
	__property TsgcTelegramRegisterUserEvent OnRegisterUser = {read=FOnRegisterUser, write=FOnRegisterUser};
	__property TsgcTelegramExceptionEvent OnException = {read=FOnException, write=FOnException};
	__property TsgcTelegramEventEvent OnEvent = {read=FOnEvent, write=FOnEvent};
	__property TsgcTelegramConnectionStatusEvent OnConnectionStatus = {read=FOnConnectionStatus, write=FOnConnectionStatus};
	__property TsgcTelegramMessageDocumentEvent OnMessageDocument = {read=FOnMessageDocument, write=FOnMessageDocument};
	__property TsgcTelegramMessagePhotoEvent OnMessagePhoto = {read=FOnMessagePhoto, write=FOnMessagePhoto};
	__property TsgcTelegramMessageTextEvent OnMessageText = {read=FOnMessageText, write=FOnMessageText};
	__property TsgcTelegramMessageVideoEvent OnMessageVideo = {read=FOnMessageVideo, write=FOnMessageVideo};
	__property TsgcTelegramNewCallbackQueryEvent OnNewCallbackQuery = {read=FOnNewCallbackQuery, write=FOnNewCallbackQuery};
	__property TsgcTelegramNewChatEvent OnNewChat = {read=FOnNewChat, write=FOnNewChat};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgclib_telegram_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCLIB_TELEGRAM_CLIENT)
using namespace Sgclib_telegram_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgclib_telegram_clientHPP
