// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCWebSocketCommon.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncwebsocketcommonHPP
#define Fmx_TmsfncwebsocketcommonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.IniFiles.hpp>
#include <IdContext.hpp>
#include <IdIOHandler.hpp>
#include <IdTCPConnection.hpp>
#include <IdCustomHTTPServer.hpp>
#include <System.Generics.Collections.hpp>
#include <IdHTTPServer.hpp>
#include <IdScheduler.hpp>
#include <IdSchedulerOfThreadDefault.hpp>
#include <IdHeaderList.hpp>
#include <IdSSLOpenSSL.hpp>
#include <IdGlobal.hpp>
#include <IdException.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncwebsocketcommon
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EWebSocket;
class DELPHICLASS TTMSFNCWebSocketBitConverter;
class DELPHICLASS TTMSFNCWebSocketFrame;
class DELPHICLASS TTMSFNCWebSocketException;
class DELPHICLASS TTMSFNCWebSocketHandshakeException;
class DELPHICLASS TTMSFNCWebSocketRequest;
class DELPHICLASS TTMSFNCWebSocketConnection;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION EWebSocket : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EWebSocket(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EWebSocket(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EWebSocket(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EWebSocket(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EWebSocket(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EWebSocket(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EWebSocket(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EWebSocket(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EWebSocket(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EWebSocket(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EWebSocket(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EWebSocket(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EWebSocket() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebSocketBitConverter : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	template<typename T> __classmethod T __fastcall InTo(System::TArray__1<System::Byte> Buffer, int Index);
	template<typename T> __classmethod void __fastcall From(T Value, System::TArray__1<System::Byte> &Buffer);
public:
	/* TObject.Create */ inline __fastcall TTMSFNCWebSocketBitConverter() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCWebSocketBitConverter() { }
	
};


enum DECLSPEC_DENUM TTMSFNCWebSocketTLSVersion : unsigned char { wstTLS1, wstTLS1_1, wstTLS1_2 };

typedef System::Set<TTMSFNCWebSocketTLSVersion, TTMSFNCWebSocketTLSVersion::wstTLS1, TTMSFNCWebSocketTLSVersion::wstTLS1_2> TTMSFNCWebSocketTLSVersions;

enum DECLSPEC_DENUM TTMSFNCWebSocketFrameType : unsigned char { ftFirst, ftContinuation, ftLast };

typedef System::Set<TTMSFNCWebSocketFrameType, TTMSFNCWebSocketFrameType::ftFirst, TTMSFNCWebSocketFrameType::ftLast> TTMSFNCWebSocketFrameTypes;

enum DECLSPEC_DENUM TTMSFNCWebSocketFrameOpcode : unsigned char { focContinuation, focText, focBinary, focClose = 8, focPing, focPong };

class PASCALIMPLEMENTATION TTMSFNCWebSocketFrame : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TTMSFNCWebSocketFrameOpcode FOpcode;
	bool FIsFin;
	unsigned __int64 FPayloadLength;
	System::TArray__1<System::Byte> FUnmaskedPayload;
	int FMaskingKey;
	bool FIsMasked;
	System::Byte FRSV;
	
protected:
	virtual void __fastcall ReadFromBuffer(System::TArray__1<System::Byte> &Buffer, Idiohandler::TIdIOHandler* aIOHandler);
	virtual void __fastcall ReadPayload(System::TArray__1<System::Byte> &Content, Idiohandler::TIdIOHandler* aIOHandler, int aOffset);
	
public:
	__fastcall virtual TTMSFNCWebSocketFrame(TTMSFNCWebSocketFrameOpcode AOpcode, System::TArray__1<System::Byte> APayload, bool AIsFin)/* overload */;
	__fastcall virtual TTMSFNCWebSocketFrame(TTMSFNCWebSocketFrameOpcode AOpcode)/* overload */;
	__fastcall virtual TTMSFNCWebSocketFrame(const System::UnicodeString aMessage)/* overload */;
	System::TArray__1<System::Byte> __fastcall ToBuffer();
	__classmethod TTMSFNCWebSocketFrame* __fastcall FromBuffer(System::TArray__1<System::Byte> &Buffer, Idiohandler::TIdIOHandler* aIOHandler);
	__classmethod System::TArray__1<System::Byte> __fastcall UnMask(System::TArray__1<System::Byte> &payload, int Key, int Offset = 0x0);
	__property System::Byte RSV = {read=FRSV, write=FRSV, nodefault};
	__property bool IsFin = {read=FIsFin, write=FIsFin, nodefault};
	__property bool IsMasked = {read=FIsMasked, write=FIsMasked, nodefault};
	__property unsigned __int64 PayloadLength = {read=FPayloadLength};
	__property int MaskingKey = {read=FMaskingKey, nodefault};
	__property System::TArray__1<System::Byte> UnmaskedPayload = {read=FUnmaskedPayload, write=FUnmaskedPayload};
	__property TTMSFNCWebSocketFrameOpcode Opcode = {read=FOpcode, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCWebSocketFrame() { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TTMSFNCWebSocketFrameClass);

typedef void __fastcall (__closure *TTMSFNCWebSocketBinDataEvent)(System::TObject* Sender, TTMSFNCWebSocketConnection* AConnection, const System::TArray__1<System::Byte> aData, TTMSFNCWebSocketFrameTypes AFrameType);

typedef void __fastcall (__closure *TTMSFNCWebSocketControlEvent)(System::TObject* Sender, TTMSFNCWebSocketConnection* AConnection, const System::TArray__1<System::Byte> aData);

typedef void __fastcall (__closure *TTMSFNCWebSocketMessageEvent)(System::TObject* Sender, TTMSFNCWebSocketConnection* AConnection, const System::UnicodeString aMessage);

typedef void __fastcall (__closure *TTMSFNCWebSocketConnectEvent)(System::TObject* Sender, TTMSFNCWebSocketConnection* AConnection);

typedef void __fastcall (__closure *TTMSFNCWebSocketNotifyEvent)(System::TObject* Sender, TTMSFNCWebSocketConnection* AConnection);

typedef TTMSFNCWebSocketNotifyEvent TTMSFNCWebSocketDisconnectEvent;

class PASCALIMPLEMENTATION TTMSFNCWebSocketException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall TTMSFNCWebSocketException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall TTMSFNCWebSocketException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall TTMSFNCWebSocketException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall TTMSFNCWebSocketException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall TTMSFNCWebSocketException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall TTMSFNCWebSocketException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall TTMSFNCWebSocketException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall TTMSFNCWebSocketException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TTMSFNCWebSocketException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TTMSFNCWebSocketException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TTMSFNCWebSocketException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TTMSFNCWebSocketException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~TTMSFNCWebSocketException() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebSocketHandshakeException : public TTMSFNCWebSocketException
{
	typedef TTMSFNCWebSocketException inherited;
	
public:
	/* Exception.Create */ inline __fastcall TTMSFNCWebSocketHandshakeException(const System::UnicodeString Msg) : TTMSFNCWebSocketException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall TTMSFNCWebSocketHandshakeException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : TTMSFNCWebSocketException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall TTMSFNCWebSocketHandshakeException(NativeUInt Ident)/* overload */ : TTMSFNCWebSocketException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall TTMSFNCWebSocketHandshakeException(System::PResStringRec ResStringRec)/* overload */ : TTMSFNCWebSocketException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall TTMSFNCWebSocketHandshakeException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : TTMSFNCWebSocketException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall TTMSFNCWebSocketHandshakeException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : TTMSFNCWebSocketException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall TTMSFNCWebSocketHandshakeException(const System::UnicodeString Msg, int AHelpContext) : TTMSFNCWebSocketException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall TTMSFNCWebSocketHandshakeException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : TTMSFNCWebSocketException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TTMSFNCWebSocketHandshakeException(NativeUInt Ident, int AHelpContext)/* overload */ : TTMSFNCWebSocketException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TTMSFNCWebSocketHandshakeException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : TTMSFNCWebSocketException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TTMSFNCWebSocketHandshakeException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : TTMSFNCWebSocketException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TTMSFNCWebSocketHandshakeException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : TTMSFNCWebSocketException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~TTMSFNCWebSocketHandshakeException() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWebSocketRequest : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FHost;
	System::UnicodeString FOrigin;
	System::UnicodeString FKey;
	System::UnicodeString FProtocol;
	System::UnicodeString FVersion;
	System::UnicodeString FResource;
	System::UnicodeString FExtensions;
	System::Classes::TStrings* FRawHeaders;
	System::UnicodeString FConnection;
	System::UnicodeString FUpgrade;
	
public:
	__fastcall TTMSFNCWebSocketRequest(System::UnicodeString aResource, System::Classes::TStrings* const AHeaderList);
	__fastcall virtual ~TTMSFNCWebSocketRequest();
	__property System::Classes::TStrings* RawHeaders = {read=FRawHeaders};
	__property System::UnicodeString Resource = {read=FResource, write=FResource};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property System::UnicodeString Origin = {read=FOrigin, write=FOrigin};
	__property System::UnicodeString Connection = {read=FConnection, write=FConnection};
	__property System::UnicodeString Upgrade = {read=FUpgrade, write=FUpgrade};
	__property System::UnicodeString Protocol = {read=FProtocol, write=FProtocol};
	__property System::UnicodeString Version = {read=FVersion, write=FVersion};
	__property System::UnicodeString Extensions = {read=FExtensions, write=FExtensions};
	__property System::UnicodeString Key = {read=FKey, write=FKey};
};


enum DECLSPEC_DENUM TTMSFNCWebsocketOption : unsigned char { twsoFrameByFrame, twsoSkipUpgradeCheck, twsoSkipVersionCheck, twsoManualPong, twsoManualClose };

typedef System::Set<TTMSFNCWebsocketOption, TTMSFNCWebsocketOption::twsoFrameByFrame, TTMSFNCWebsocketOption::twsoManualClose> TTMSFNCWebsocketOptions;

class PASCALIMPLEMENTATION TTMSFNCWebSocketConnection : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TTMSFNCWebSocketFrameOpcode FInitialOpcode;
	System::TArray__1<System::Byte> FMessageContent;
	TTMSFNCWebSocketRequest* FHandshakeRequest;
	TTMSFNCWebSocketMessageEvent FOnMessageReceived;
	TTMSFNCWebSocketBinDataEvent FOnBinaryDataReceived;
	TTMSFNCWebsocketOptions FOptions;
	TTMSFNCWebSocketControlEvent FOnPing;
	TTMSFNCWebSocketControlEvent FOnPong;
	TTMSFNCWebSocketControlEvent FOnClose;
	bool FCloseSent;
	int FWebSocketVersion;
	void __fastcall DoFrameEvents(TTMSFNCWebSocketFrame* aFrame);
	
protected:
	static const System::WideChar FRAME_START = (System::WideChar)(0x0);
	
	static const char FRAME_SIZE_START = '\x80';
	
	static const char FRAME_END = '\xff';
	
	void __fastcall SetHandShakeRequest(TTMSFNCWebSocketRequest* aRequest);
	virtual System::UnicodeString __fastcall GetPeerIP() = 0 ;
	virtual Idtcpconnection::TIdTCPConnection* __fastcall GetConnection() = 0 ;
	virtual bool __fastcall HandleFrame(TTMSFNCWebSocketFrame* aFrame);
	virtual bool __fastcall HandleAllPendingFrames(System::TArray__1<System::Byte> ABuffer, Idiohandler::TIdIOHandler* aIOHandler);
	virtual bool __fastcall GetHandshakeCompleted() = 0 ;
	__property Idtcpconnection::TIdTCPConnection* Connection = {read=GetConnection};
	
public:
	__fastcall virtual TTMSFNCWebSocketConnection(TTMSFNCWebsocketOptions aOptions);
	__fastcall virtual ~TTMSFNCWebSocketConnection();
	virtual TTMSFNCWebSocketFrameClass __fastcall GetFrameClass();
	virtual void __fastcall SendFrame(TTMSFNCWebSocketFrame* aFrame);
	void __fastcall Send(const System::UnicodeString aMessage, const bool AMasked = false);
	void __fastcall SendBytes(const System::TArray__1<System::Byte> ABytes, const bool AMasked = false);
	void __fastcall SendInMultipleFrames(const System::UnicodeString AMessage, unsigned __int64 AFrameLength, const bool AMasked = false);
	void __fastcall SendBytesInMultipleFrames(System::TArray__1<System::Byte> &ABytes, unsigned __int64 AFrameLength, const bool AMasked = false);
	void __fastcall SendSimpleFrame(TTMSFNCWebSocketFrameOpcode AOpcode, System::TArray__1<System::Byte> aData = System::TArray__1<System::Byte>());
	void __fastcall SendClose(System::TArray__1<System::Byte> aData = System::TArray__1<System::Byte>())/* overload */;
	void __fastcall SendClose(System::UnicodeString aMessage)/* overload */;
	__property int WebSocketVersion = {read=FWebSocketVersion, write=FWebSocketVersion, nodefault};
	__property System::UnicodeString PeerIP = {read=GetPeerIP};
	__property TTMSFNCWebsocketOptions Options = {read=FOptions, nodefault};
	__property TTMSFNCWebSocketRequest* HandshakeRequest = {read=FHandshakeRequest};
	__property bool HandshakeCompleted = {read=GetHandshakeCompleted, nodefault};
	__property bool CloseSent = {read=FCloseSent, nodefault};
	__property TTMSFNCWebSocketBinDataEvent OnBinaryDataReceived = {read=FOnBinaryDataReceived, write=FOnBinaryDataReceived};
	__property TTMSFNCWebSocketMessageEvent OnMessageReceived = {read=FOnMessageReceived, write=FOnMessageReceived};
	__property TTMSFNCWebSocketControlEvent OnPing = {read=FOnPing, write=FOnPing};
	__property TTMSFNCWebSocketControlEvent OnPong = {read=FOnPong, write=FOnPong};
	__property TTMSFNCWebSocketControlEvent OnClose = {read=FOnClose, write=FOnClose};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::Byte Fin;
extern DELPHI_PACKAGE System::Byte TwoBytesLengthCode;
extern DELPHI_PACKAGE System::Byte EightBytesLengthCode;
static const System::Word DefaultPort = System::Word(0x22b8);
static const System::Int8 CurrentWebSocketVersion = System::Int8(0xd);
extern DELPHI_PACKAGE System::ResourceString _SErrActive;
#define Fmx_Tmsfncwebsocketcommon_SErrActive System::LoadResourceString(&Fmx::Tmsfncwebsocketcommon::_SErrActive)
extern DELPHI_PACKAGE System::ResourceString _SErrInActive;
#define Fmx_Tmsfncwebsocketcommon_SErrInActive System::LoadResourceString(&Fmx::Tmsfncwebsocketcommon::_SErrInActive)
extern DELPHI_PACKAGE System::ResourceString _SErrHandshakeNotComplete;
#define Fmx_Tmsfncwebsocketcommon_SErrHandshakeNotComplete System::LoadResourceString(&Fmx::Tmsfncwebsocketcommon::_SErrHandshakeNotComplete)
}	/* namespace Tmsfncwebsocketcommon */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCWEBSOCKETCOMMON)
using namespace Fmx::Tmsfncwebsocketcommon;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncwebsocketcommonHPP
