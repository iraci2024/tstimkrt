// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcSocket_Classes.pas' rev: 35.00 (Windows)

#ifndef Sgcsocket_classesHPP
#define Sgcsocket_classesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdLogFile.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcWebSocket_Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcsocket_classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcSocketKeepAlive;
class DELPHICLASS TsgcSocketConnection_Base;
class DELPHICLASS TsgcSocketConnection;
class DELPHICLASS TsgcSocketComponent_Base;
class DELPHICLASS TsgcSocketLogFile;
class DELPHICLASS TsgcSocketThrottle;
class DELPHICLASS TsgcSocketOpenSSL_Options;
class DELPHICLASS TsgcSocketSChannel_Options;
class DELPHICLASS TsgcSocketTLS_Options_Base;
class DELPHICLASS TsgcOpenSSLClient_Options;
class DELPHICLASS TsgcSChannelClient_Options;
class DELPHICLASS TsgcSocketTLS_Options;
class DELPHICLASS TsgcSocketProxy_Options;
class DELPHICLASS TsgcSocketWatchDog_Options;
class DELPHICLASS TsgcSocketHeartBeat_Options;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::Byte> TsgcBytes;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSocketKeepAlive : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	int FTime;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property int Time = {read=FTime, write=FTime, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSocketKeepAlive() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcSocketKeepAlive() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSocketConnection_Base : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	Sgcwebsocket_classes_syncobjs::TsgcCriticalSection* FCS;
	System::StaticArray<Sgcwebsocket_classes_syncobjs::TsgcCriticalSection*, 6> FCSArray;
	virtual void __fastcall DoEnterCS()/* overload */;
	virtual void __fastcall DoEnterCS(const int aIndex)/* overload */;
	virtual void __fastcall DoLeaveCS()/* overload */;
	virtual void __fastcall DoLeaveCS(const int aIndex)/* overload */;
	virtual bool __fastcall GetActive() = 0 ;
	
public:
	__property bool Active = {read=GetActive, nodefault};
	
private:
	bool FIsInternal;
	
protected:
	System::UnicodeString FGuid;
	__int64 FRecBytes;
	__int64 FSendBytes;
	System::UnicodeString FIP;
	System::UnicodeString FLocalIP;
	int FLocalPort;
	int FPort;
	Sgcwebsocket_types::TwsIPVersion FIPVersion;
	Sgcwebsocket_types::TwsTransport FTransport;
	virtual System::UnicodeString __fastcall GetGuid();
	virtual System::UnicodeString __fastcall GetIP() = 0 ;
	virtual System::UnicodeString __fastcall GetLocalIP() = 0 ;
	virtual int __fastcall GetLocalPort() = 0 ;
	virtual int __fastcall GetPort() = 0 ;
	virtual Sgcwebsocket_types::TwsIPVersion __fastcall GetIPVersion() = 0 ;
	virtual void __fastcall SetTransport(const Sgcwebsocket_types::TwsTransport Value);
	__property bool IsInternal = {read=FIsInternal, write=FIsInternal, nodefault};
	
public:
	__property System::UnicodeString Guid = {read=GetGuid};
	__property System::UnicodeString IP = {read=GetIP};
	__property System::UnicodeString LocalIP = {read=GetLocalIP};
	__property int LocalPort = {read=GetLocalPort, nodefault};
	__property int Port = {read=GetPort, nodefault};
	__property Sgcwebsocket_types::TwsIPVersion IPVersion = {read=GetIPVersion, nodefault};
	__property __int64 RecBytes = {read=FRecBytes, write=FRecBytes};
	__property __int64 SendBytes = {read=FSendBytes, write=FSendBytes};
	__property Sgcwebsocket_types::TwsTransport Transport = {read=FTransport, write=SetTransport, nodefault};
	
private:
	bool FDisconnected;
	
public:
	__property bool Disconnected = {read=FDisconnected, write=FDisconnected, nodefault};
	
private:
	System::Sysutils::Exception* FRawException;
	
public:
	__property System::Sysutils::Exception* RawException = {read=FRawException, write=FRawException};
	
private:
	System::TObject* FData;
	
public:
	__property System::TObject* Data = {read=FData, write=FData};
	
protected:
	bool FFreed;
	
public:
	__fastcall virtual TsgcSocketConnection_Base();
	__fastcall virtual ~TsgcSocketConnection_Base();
	HIDESBASE void __fastcall Free();
};


class PASCALIMPLEMENTATION TsgcSocketConnection : public TsgcSocketConnection_Base
{
	typedef TsgcSocketConnection_Base inherited;
	
public:
	/* TsgcSocketConnection_Base.Create */ inline __fastcall virtual TsgcSocketConnection() : TsgcSocketConnection_Base() { }
	/* TsgcSocketConnection_Base.Destroy */ inline __fastcall virtual ~TsgcSocketConnection() { }
	
};


class PASCALIMPLEMENTATION TsgcSocketComponent_Base : public Sgcbase_classes::TsgcComponent_Base
{
	typedef Sgcbase_classes::TsgcComponent_Base inherited;
	
private:
	System::StaticArray<Sgcwebsocket_classes_syncobjs::TsgcCriticalSection*, 6> FCSArray;
	Sgcwebsocket_classes_syncobjs::TsgcCriticalSection* FCS;
	
protected:
	virtual void __fastcall DoEnterCS()/* overload */;
	virtual void __fastcall DoEnterCS(const int aIndex)/* overload */;
	virtual void __fastcall DoLeaveCS()/* overload */;
	virtual void __fastcall DoLeaveCS(const int aIndex)/* overload */;
	virtual bool __fastcall DoAssignedCS()/* overload */;
	virtual bool __fastcall DoAssignedCS(const int aIndex)/* overload */;
	Sgcwebsocket_types::TwsNotifyEvent FNotifyEvents;
	virtual void __fastcall SetNotifyEvents(const Sgcwebsocket_types::TwsNotifyEvent Value);
	__property Sgcwebsocket_types::TwsNotifyEvent NotifyEvents = {read=FNotifyEvents, write=SetNotifyEvents, nodefault};
	
public:
	__fastcall virtual TsgcSocketComponent_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcSocketComponent_Base();
	
protected:
	virtual void __fastcall DoDestroyConnection(TsgcSocketConnection* aConnection);
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSocketLogFile : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FFileName;
	
public:
	__fastcall virtual TsgcSocketLogFile();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString FileName = {read=FFileName, write=FFileName};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSocketLogFile() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSocketThrottle : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FBitsPerSec;
	bool FEnabled;
	
public:
	__fastcall virtual TsgcSocketThrottle();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property int BitsPerSec = {read=FBitsPerSec, write=FBitsPerSec, nodefault};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSocketThrottle() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSocketOpenSSL_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcwebsocket_types::TwsOpenSSLAPI FAPIVersion;
	System::UnicodeString FLibPathCustom;
	Sgcwebsocket_types::TwsOpenSSLLibPath FLibPath;
	Sgcwebsocket_types::TwsOpenSSLSymLinks FUnixSymLinks;
	
public:
	__fastcall virtual TsgcSocketOpenSSL_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property Sgcwebsocket_types::TwsOpenSSLAPI APIVersion = {read=FAPIVersion, write=FAPIVersion, nodefault};
	__property System::UnicodeString LibPathCustom = {read=FLibPathCustom, write=FLibPathCustom};
	__property Sgcwebsocket_types::TwsOpenSSLLibPath LibPath = {read=FLibPath, write=FLibPath, nodefault};
	__property Sgcwebsocket_types::TwsOpenSSLSymLinks UnixSymLinks = {read=FUnixSymLinks, write=FUnixSymLinks, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSocketOpenSSL_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSocketSChannel_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FCertHash;
	Sgcwebsocket_types::TsgcSChannelCertStoreName FCertStoreName;
	Sgcwebsocket_types::TsgcSChannelCertStorePath FCertStorePath;
	System::UnicodeString FCipherList;
	
public:
	__fastcall virtual TsgcSocketSChannel_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property Sgcwebsocket_types::TsgcSChannelCertStoreName CertStoreName = {read=FCertStoreName, write=FCertStoreName, nodefault};
	__property Sgcwebsocket_types::TsgcSChannelCertStorePath CertStorePath = {read=FCertStorePath, write=FCertStorePath, nodefault};
	__property System::UnicodeString CertHash = {read=FCertHash, write=FCertHash};
	__property System::UnicodeString CipherList = {read=FCipherList, write=FCipherList};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSocketSChannel_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSocketTLS_Options_Base : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FCertFile;
	System::UnicodeString FKeyFile;
	System::UnicodeString FPassword;
	System::UnicodeString FRootCertFile;
	bool FVerifyCertificate;
	int FVerifyDepth;
	Sgcwebsocket_types::TwsTLSVersions FVersion;
	
public:
	__fastcall virtual TsgcSocketTLS_Options_Base();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString CertFile = {read=FCertFile, write=FCertFile};
	__property System::UnicodeString KeyFile = {read=FKeyFile, write=FKeyFile};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString RootCertFile = {read=FRootCertFile, write=FRootCertFile};
	__property bool VerifyCertificate = {read=FVerifyCertificate, write=FVerifyCertificate, nodefault};
	__property int VerifyDepth = {read=FVerifyDepth, write=FVerifyDepth, nodefault};
	__property Sgcwebsocket_types::TwsTLSVersions Version = {read=FVersion, write=FVersion, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSocketTLS_Options_Base() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcOpenSSLClient_Options : public TsgcSocketOpenSSL_Options
{
	typedef TsgcSocketOpenSSL_Options inherited;
	
public:
	/* TsgcSocketOpenSSL_Options.Create */ inline __fastcall virtual TsgcOpenSSLClient_Options() : TsgcSocketOpenSSL_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcOpenSSLClient_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSChannelClient_Options : public TsgcSocketSChannel_Options
{
	typedef TsgcSocketSChannel_Options inherited;
	
public:
	/* TsgcSocketSChannel_Options.Create */ inline __fastcall virtual TsgcSChannelClient_Options() : TsgcSocketSChannel_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSChannelClient_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSocketTLS_Options : public TsgcSocketTLS_Options_Base
{
	typedef TsgcSocketTLS_Options_Base inherited;
	
private:
	System::Classes::TStringList* FALPNProtocols;
	Sgcwebsocket_types::TwsTLSIOHandler FIOHandler;
	TsgcOpenSSLClient_Options* FOpenSSL_Options;
	TsgcSChannelClient_Options* FSChannel_Options;
	void __fastcall SetALPNProtocols(System::Classes::TStringList* const Value);
	void __fastcall SetOpenSSL_Options(TsgcOpenSSLClient_Options* const Value);
	void __fastcall SetSChannel_Options(TsgcSChannelClient_Options* const Value);
	
public:
	__fastcall virtual TsgcSocketTLS_Options();
	__fastcall virtual ~TsgcSocketTLS_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::Classes::TStringList* ALPNProtocols = {read=FALPNProtocols, write=SetALPNProtocols};
	__property Sgcwebsocket_types::TwsTLSIOHandler IOHandler = {read=FIOHandler, write=FIOHandler, nodefault};
	__property TsgcOpenSSLClient_Options* OpenSSL_Options = {read=FOpenSSL_Options, write=SetOpenSSL_Options};
	__property TsgcSChannelClient_Options* SChannel_Options = {read=FSChannel_Options, write=SetSChannel_Options};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSocketProxy_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FHost;
	System::UnicodeString FPassword;
	int FPort;
	Sgcwebsocket_types::TwsProxy FProxyType;
	System::UnicodeString FUsername;
	
public:
	__fastcall virtual TsgcSocketProxy_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property Sgcwebsocket_types::TwsProxy ProxyType = {read=FProxyType, write=FProxyType, nodefault};
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSocketProxy_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSocketWatchDog_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	
protected:
	int FAttempts;
	
public:
	__fastcall virtual TsgcSocketWatchDog_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property int Attempts = {read=FAttempts, write=FAttempts, nodefault};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSocketWatchDog_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSocketHeartBeat_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	
public:
	__fastcall virtual TsgcSocketHeartBeat_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSocketHeartBeat_Options() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcsocket_classes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCSOCKET_CLASSES)
using namespace Sgcsocket_classes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcsocket_classesHPP
