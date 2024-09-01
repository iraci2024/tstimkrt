// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_LoadBalancer_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_loadbalancer_messageHPP
#define Sgcwebsocket_loadbalancer_messageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <sgcWebSocket_Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_loadbalancer_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSLoadBalancerMessage;
class DELPHICLASS TsgcWSLoadBalancerBinary;
class DELPHICLASS TsgcWSLoadBalancerClientConnection;
class DELPHICLASS TsgcWSLoadBalancerServerBinding;
class DELPHICLASS TsgcWSLoadBalancerServerData;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSLoadBalancerMessage : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FChannel;
	System::UnicodeString FExclude;
	System::UnicodeString FGuid;
	System::UnicodeString FInclude;
	System::UnicodeString FProtocol;
	System::UnicodeString FText;
	
public:
	void __fastcall Clear();
	void __fastcall Read(const System::UnicodeString aMessage);
	System::UnicodeString __fastcall Write();
	__property System::UnicodeString Channel = {read=FChannel, write=FChannel};
	__property System::UnicodeString Exclude = {read=FExclude, write=FExclude};
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
	__property System::UnicodeString Include = {read=FInclude, write=FInclude};
	__property System::UnicodeString Protocol = {read=FProtocol, write=FProtocol};
	__property System::UnicodeString Text = {read=FText, write=FText};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSLoadBalancerMessage(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSLoadBalancerMessage() { }
	
};


class PASCALIMPLEMENTATION TsgcWSLoadBalancerBinary : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FChannel;
	System::UnicodeString FExclude;
	System::UnicodeString FGuid;
	System::UnicodeString FInclude;
	System::UnicodeString FProtocol;
	int FSize;
	Sgcwebsocket_types::TwsStreaming FStreaming;
	
public:
	void __fastcall Clear();
	void __fastcall Read(System::Classes::TMemoryStream* &aStream);
	void __fastcall Write(System::Classes::TMemoryStream* &aData);
	__property System::UnicodeString Channel = {read=FChannel, write=FChannel};
	__property System::UnicodeString Exclude = {read=FExclude, write=FExclude};
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
	__property System::UnicodeString Include = {read=FInclude, write=FInclude};
	__property System::UnicodeString Protocol = {read=FProtocol, write=FProtocol};
	__property int Size = {read=FSize, write=FSize, nodefault};
	__property Sgcwebsocket_types::TwsStreaming Streaming = {read=FStreaming, write=FStreaming, nodefault};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSLoadBalancerBinary(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSLoadBalancerBinary() { }
	
};


class PASCALIMPLEMENTATION TsgcWSLoadBalancerClientConnection : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FActive;
	System::UnicodeString FChannel;
	System::UnicodeString FIP;
	System::UnicodeString FGuid;
	System::UnicodeString FProtocol;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	void __fastcall Clear();
	void __fastcall Read(const System::UnicodeString aMessage);
	System::UnicodeString __fastcall Write();
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property System::UnicodeString Channel = {read=FChannel, write=FChannel};
	__property System::UnicodeString IP = {read=FIP, write=FIP};
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
	__property System::UnicodeString Protocol = {read=FProtocol, write=FProtocol};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSLoadBalancerClientConnection(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSLoadBalancerClientConnection() { }
	
};


class PASCALIMPLEMENTATION TsgcWSLoadBalancerServerBinding : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FHost;
	int FPort;
	bool FSSL;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	void __fastcall Clear();
	void __fastcall Read(const System::UnicodeString aMessage);
	System::UnicodeString __fastcall Write();
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property bool SSL = {read=FSSL, write=FSSL, nodefault};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSLoadBalancerServerBinding(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSLoadBalancerServerBinding() { }
	
};


class PASCALIMPLEMENTATION TsgcWSLoadBalancerServerData : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FGuid;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	void __fastcall Clear();
	void __fastcall Read(const System::UnicodeString aMessage);
	System::UnicodeString __fastcall Write();
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSLoadBalancerServerData(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSLoadBalancerServerData() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_loadbalancer_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_LOADBALANCER_MESSAGE)
using namespace Sgcwebsocket_loadbalancer_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_loadbalancer_messageHPP
