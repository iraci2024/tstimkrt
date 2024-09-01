// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcIoT_Classes.pas' rev: 35.00 (Windows)

#ifndef Sgciot_classesHPP
#define Sgciot_classesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgciot_classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcIoTComponent_Base;
class DELPHICLASS TsgcIoTComponent_Base_Client;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcIoTComponent_Base : public Sgctcp_classes::TsgcTCPComponent_Base
{
	typedef Sgctcp_classes::TsgcTCPComponent_Base inherited;
	
public:
	/* TsgcSocketComponent_Base.Create */ inline __fastcall virtual TsgcIoTComponent_Base(System::Classes::TComponent* aOwner) : Sgctcp_classes::TsgcTCPComponent_Base(aOwner) { }
	/* TsgcSocketComponent_Base.Destroy */ inline __fastcall virtual ~TsgcIoTComponent_Base() { }
	
};


class PASCALIMPLEMENTATION TsgcIoTComponent_Base_Client : public TsgcIoTComponent_Base
{
	typedef TsgcIoTComponent_Base inherited;
	
public:
	/* TsgcSocketComponent_Base.Create */ inline __fastcall virtual TsgcIoTComponent_Base_Client(System::Classes::TComponent* aOwner) : TsgcIoTComponent_Base(aOwner) { }
	/* TsgcSocketComponent_Base.Destroy */ inline __fastcall virtual ~TsgcIoTComponent_Base_Client() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgciot_classes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCIOT_CLASSES)
using namespace Sgciot_classes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgciot_classesHPP
