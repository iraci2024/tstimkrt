// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCJSONWriter.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncjsonwriterHPP
#define Fmx_TmsfncjsonwriterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <FMX.TMSFNCTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncjsonwriter
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCJSONStreamWriter;
class DELPHICLASS TTMSFNCJSONWriter;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCJSONStreamWriter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStream* FStream;
	System::Classes::TStringStream* FWriteStream;
	
public:
	__fastcall TTMSFNCJSONStreamWriter(System::Classes::TStream* AStream);
	__fastcall virtual ~TTMSFNCJSONStreamWriter();
	void __fastcall Write(const System::UnicodeString Value);
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCJSONWriter : public System::TObject
{
	typedef System::TObject inherited;
	
	
public:
	class DELPHICLASS ECannotWriteName;
	class PASCALIMPLEMENTATION ECannotWriteName : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall ECannotWriteName();
	public:
		/* Exception.CreateFmt */ inline __fastcall ECannotWriteName(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall ECannotWriteName(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall ECannotWriteName(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall ECannotWriteName(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall ECannotWriteName(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall ECannotWriteName(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall ECannotWriteName(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall ECannotWriteName(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall ECannotWriteName(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall ECannotWriteName(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall ECannotWriteName(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~ECannotWriteName() { }
		
	};
	
	
	class DELPHICLASS EMultipleRootNotAllowed;
	class PASCALIMPLEMENTATION EMultipleRootNotAllowed : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EMultipleRootNotAllowed();
	public:
		/* Exception.CreateFmt */ inline __fastcall EMultipleRootNotAllowed(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EMultipleRootNotAllowed(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EMultipleRootNotAllowed(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EMultipleRootNotAllowed(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EMultipleRootNotAllowed(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EMultipleRootNotAllowed(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EMultipleRootNotAllowed(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EMultipleRootNotAllowed(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EMultipleRootNotAllowed(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EMultipleRootNotAllowed(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EMultipleRootNotAllowed(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EMultipleRootNotAllowed() { }
		
	};
	
	
	class DELPHICLASS EObjectOrArrayExpected;
	class PASCALIMPLEMENTATION EObjectOrArrayExpected : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EObjectOrArrayExpected();
	public:
		/* Exception.CreateFmt */ inline __fastcall EObjectOrArrayExpected(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EObjectOrArrayExpected(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EObjectOrArrayExpected(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EObjectOrArrayExpected(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EObjectOrArrayExpected(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EObjectOrArrayExpected(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EObjectOrArrayExpected(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EObjectOrArrayExpected(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EObjectOrArrayExpected(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EObjectOrArrayExpected(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EObjectOrArrayExpected(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EObjectOrArrayExpected() { }
		
	};
	
	
	class DELPHICLASS EInvalidNesting;
	class PASCALIMPLEMENTATION EInvalidNesting : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EInvalidNesting();
	public:
		/* Exception.CreateFmt */ inline __fastcall EInvalidNesting(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EInvalidNesting(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EInvalidNesting(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EInvalidNesting(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EInvalidNesting(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EInvalidNesting(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EInvalidNesting(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EInvalidNesting(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EInvalidNesting(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidNesting(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidNesting(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EInvalidNesting() { }
		
	};
	
	
	class DELPHICLASS EMissingValue;
	class PASCALIMPLEMENTATION EMissingValue : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EMissingValue();
	public:
		/* Exception.CreateFmt */ inline __fastcall EMissingValue(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EMissingValue(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EMissingValue(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EMissingValue(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EMissingValue(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EMissingValue(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EMissingValue(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EMissingValue(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EMissingValue(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EMissingValue(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EMissingValue(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EMissingValue() { }
		
	};
	
	
	class DELPHICLASS ETooManyDepthLevels;
	class PASCALIMPLEMENTATION ETooManyDepthLevels : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall ETooManyDepthLevels();
	public:
		/* Exception.CreateFmt */ inline __fastcall ETooManyDepthLevels(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall ETooManyDepthLevels(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall ETooManyDepthLevels(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall ETooManyDepthLevels(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall ETooManyDepthLevels(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall ETooManyDepthLevels(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall ETooManyDepthLevels(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall ETooManyDepthLevels(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall ETooManyDepthLevels(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall ETooManyDepthLevels(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall ETooManyDepthLevels(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~ETooManyDepthLevels() { }
		
	};
	
	
	class DELPHICLASS EEmptyJson;
	class PASCALIMPLEMENTATION EEmptyJson : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EEmptyJson();
	public:
		/* Exception.CreateFmt */ inline __fastcall EEmptyJson(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EEmptyJson(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EEmptyJson(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EEmptyJson(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EEmptyJson(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EEmptyJson(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EEmptyJson(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EEmptyJson(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EEmptyJson(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EEmptyJson(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EEmptyJson(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EEmptyJson() { }
		
	};
	
	
	class DELPHICLASS EEmptyName;
	class PASCALIMPLEMENTATION EEmptyName : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EEmptyName();
	public:
		/* Exception.CreateFmt */ inline __fastcall EEmptyName(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EEmptyName(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EEmptyName(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EEmptyName(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EEmptyName(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EEmptyName(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EEmptyName(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EEmptyName(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EEmptyName(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EEmptyName(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EEmptyName(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EEmptyName() { }
		
	};
	
	
	
private:
	enum DECLSPEC_DENUM TTMSFNCJSONScope : unsigned char { jscEmptyDocument, jscEmptyArray, jscEmptyObject, jscNonEmptyDocument, jscNonEmptyArray, jscNonEmptyObject, jscDanglingName };
	
	
private:
	static constexpr System::Byte MaxStackSize = System::Byte(0xff);
	
	TTMSFNCJSONStreamWriter* FWriter;
	System::StaticArray<TTMSFNCJSONScope, 256> FStack;
	int FStackSize;
	System::UnicodeString FIndent;
	System::UnicodeString FSeparator;
	System::UnicodeString FDeferredName;
	void __fastcall SetIndentLength(const int Value);
	int __fastcall GetIndentLength();
	TTMSFNCJSONWriter* __fastcall OpenItem(const TTMSFNCJSONScope Empty, const System::UnicodeString OpenBracket);
	TTMSFNCJSONWriter* __fastcall CloseItem(const TTMSFNCJSONScope Empty, const TTMSFNCJSONScope NonEmpty, const System::UnicodeString CloseBracket);
	void __fastcall PushScope(const TTMSFNCJSONScope Scope);
	TTMSFNCJSONScope __fastcall PeekScope();
	void __fastcall ReplaceTop(const TTMSFNCJSONScope Scope);
	void __fastcall WriteDeferredName();
	void __fastcall InternalWriteString(const System::UnicodeString Value);
	void __fastcall NewLine();
	void __fastcall BeforeName();
	void __fastcall BeforeValue(const bool Root);
	
public:
	__fastcall TTMSFNCJSONWriter(System::Classes::TStream* const AStream);
	__fastcall virtual ~TTMSFNCJSONWriter();
	TTMSFNCJSONWriter* __fastcall WriteBeginArray();
	TTMSFNCJSONWriter* __fastcall WriteEndArray();
	TTMSFNCJSONWriter* __fastcall WriteBeginObject();
	TTMSFNCJSONWriter* __fastcall WriteEndObject();
	TTMSFNCJSONWriter* __fastcall WriteName(const System::UnicodeString Name);
	TTMSFNCJSONWriter* __fastcall WriteString(const System::UnicodeString Value);
	TTMSFNCJSONWriter* __fastcall WriteRawString(const System::UnicodeString Value);
	TTMSFNCJSONWriter* __fastcall WriteBoolean(const bool Value);
	TTMSFNCJSONWriter* __fastcall WriteNull();
	TTMSFNCJSONWriter* __fastcall WriteDouble(const double Value);
	TTMSFNCJSONWriter* __fastcall WriteInteger(const __int64 Value);
	void __fastcall Close();
	__property int IndentLength = {read=GetIndentLength, write=SetIndentLength, nodefault};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncjsonwriter */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCJSONWRITER)
using namespace Fmx::Tmsfncjsonwriter;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncjsonwriterHPP
