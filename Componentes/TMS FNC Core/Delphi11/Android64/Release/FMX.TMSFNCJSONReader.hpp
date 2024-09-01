// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCJSONReader.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncjsonreaderHPP
#define Fmx_TmsfncjsonreaderHPP

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
namespace Tmsfncjsonreader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCJSONStreamReader;
class DELPHICLASS TTMSFNCJSONReader;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCJSONStreamReader : public System::TObject
{
	typedef System::TObject inherited;
	
	
public:
	class DELPHICLASS EInvalidJsonInput;
	class PASCALIMPLEMENTATION EInvalidJsonInput : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EInvalidJsonInput();
	public:
		/* Exception.CreateFmt */ inline __fastcall EInvalidJsonInput(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EInvalidJsonInput(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EInvalidJsonInput(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EInvalidJsonInput(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EInvalidJsonInput(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EInvalidJsonInput(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EInvalidJsonInput(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EInvalidJsonInput(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EInvalidJsonInput(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidJsonInput(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidJsonInput(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EInvalidJsonInput() { }
		
	};
	
	
	class DELPHICLASS EInternalError;
	class PASCALIMPLEMENTATION EInternalError : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EInternalError();
	public:
		/* Exception.CreateFmt */ inline __fastcall EInternalError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EInternalError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EInternalError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EInternalError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EInternalError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EInternalError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EInternalError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EInternalError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EInternalError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EInternalError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EInternalError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EInternalError() { }
		
	};
	
	
	class DELPHICLASS EEndOfInputReached;
	class PASCALIMPLEMENTATION EEndOfInputReached : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EEndOfInputReached();
	public:
		/* Exception.CreateFmt */ inline __fastcall EEndOfInputReached(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EEndOfInputReached(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EEndOfInputReached(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EEndOfInputReached(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EEndOfInputReached(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EEndOfInputReached(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EEndOfInputReached(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EEndOfInputReached(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EEndOfInputReached(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EEndOfInputReached(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EEndOfInputReached(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EEndOfInputReached() { }
		
	};
	
	
	
private:
	System::Classes::TStream* FStream;
	System::Classes::TStringStream* FReadStream;
	
public:
	__fastcall TTMSFNCJSONStreamReader(System::Classes::TStream* const aStream);
	__fastcall virtual ~TTMSFNCJSONStreamReader();
	System::WideChar __fastcall NextChar();
	System::WideChar __fastcall PeekChar();
	System::WideChar __fastcall ReadChar();
	void __fastcall Backup(const System::WideChar c);
	void __fastcall MoveNext(const int Count = 0x1);
	bool __fastcall Eof();
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


enum class DECLSPEC_DENUM TTMSFNCJSONToken : unsigned char { jstoBeginObject, jstoEndObject, jstoBeginArray, jstoEndArray, jstoName, jstoBoolean, jstoNull, jstoText, jstoNumber, jstoEOF };

class PASCALIMPLEMENTATION TTMSFNCJSONReader : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	enum class DECLSPEC_DENUM TTMSFNCJSONState : unsigned char { jstNone, jstBeginObject, jstEndObject, jstBeginArray, jstEndArray, jstTrue, jstFalse, jstNull, jstDoubleQuoted, jstBuffered, jstDoubleQuotedName, jstInt64, jstNumber, jstEOF };
	
	enum class DECLSPEC_DENUM TTMSFNCJSONNumberState : unsigned char { jnstNone, jnstSign, jnstDigit, jnstDecimal, jnstFraction, jnstExpE, jnstExpSign, jnstExpDigit };
	
	enum class DECLSPEC_DENUM TTMSFNCJSONScope : unsigned char { jscEmptyDocument, jscEmptyArray, jscEmptyObject, jscNonEmptyDocument, jscNonEmptyArray, jscNonEmptyObject, jscDanglingName };
	
	
public:
	class DELPHICLASS EInvalidStateException;
	class PASCALIMPLEMENTATION EInvalidStateException : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EInvalidStateException(const TTMSFNCJSONReader::TTMSFNCJSONState AState);
	public:
		/* Exception.CreateFmt */ inline __fastcall EInvalidStateException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EInvalidStateException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EInvalidStateException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EInvalidStateException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EInvalidStateException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EInvalidStateException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EInvalidStateException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EInvalidStateException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EInvalidStateException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidStateException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidStateException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EInvalidStateException() { }
		
	};
	
	
	class DELPHICLASS EUnterminatedArray;
	class PASCALIMPLEMENTATION EUnterminatedArray : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EUnterminatedArray();
	public:
		/* Exception.CreateFmt */ inline __fastcall EUnterminatedArray(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EUnterminatedArray(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EUnterminatedArray(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EUnterminatedArray(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EUnterminatedArray(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EUnterminatedArray(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EUnterminatedArray(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EUnterminatedArray(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EUnterminatedArray(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EUnterminatedArray(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EUnterminatedArray(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EUnterminatedArray() { }
		
	};
	
	
	class DELPHICLASS EUnterminatedObject;
	class PASCALIMPLEMENTATION EUnterminatedObject : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EUnterminatedObject();
	public:
		/* Exception.CreateFmt */ inline __fastcall EUnterminatedObject(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EUnterminatedObject(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EUnterminatedObject(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EUnterminatedObject(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EUnterminatedObject(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EUnterminatedObject(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EUnterminatedObject(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EUnterminatedObject(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EUnterminatedObject(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EUnterminatedObject(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EUnterminatedObject(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EUnterminatedObject() { }
		
	};
	
	
	class DELPHICLASS ENameExpected;
	class PASCALIMPLEMENTATION ENameExpected : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall ENameExpected();
	public:
		/* Exception.CreateFmt */ inline __fastcall ENameExpected(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall ENameExpected(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall ENameExpected(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall ENameExpected(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall ENameExpected(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall ENameExpected(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall ENameExpected(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall ENameExpected(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall ENameExpected(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall ENameExpected(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall ENameExpected(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~ENameExpected() { }
		
	};
	
	
	class DELPHICLASS EColonExpected;
	class PASCALIMPLEMENTATION EColonExpected : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EColonExpected();
	public:
		/* Exception.CreateFmt */ inline __fastcall EColonExpected(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EColonExpected(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EColonExpected(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EColonExpected(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EColonExpected(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EColonExpected(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EColonExpected(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EColonExpected(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EColonExpected(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EColonExpected(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EColonExpected(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EColonExpected() { }
		
	};
	
	
	class DELPHICLASS EReaderClosed;
	class PASCALIMPLEMENTATION EReaderClosed : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EReaderClosed();
	public:
		/* Exception.CreateFmt */ inline __fastcall EReaderClosed(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EReaderClosed(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EReaderClosed(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EReaderClosed(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EReaderClosed(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EReaderClosed(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EReaderClosed(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EReaderClosed(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EReaderClosed(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EReaderClosed(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EReaderClosed(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EReaderClosed() { }
		
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
	
	
	class DELPHICLASS EExpectedValue;
	class PASCALIMPLEMENTATION EExpectedValue : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EExpectedValue();
	public:
		/* Exception.CreateFmt */ inline __fastcall EExpectedValue(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EExpectedValue(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EExpectedValue(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EExpectedValue(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EExpectedValue(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EExpectedValue(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EExpectedValue(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EExpectedValue(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EExpectedValue(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EExpectedValue(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EExpectedValue(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EExpectedValue() { }
		
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
	
	
	class DELPHICLASS EInvalidEscaped;
	class PASCALIMPLEMENTATION EInvalidEscaped : public System::Sysutils::Exception
	{
		typedef System::Sysutils::Exception inherited;
		
	public:
		__fastcall EInvalidEscaped();
	public:
		/* Exception.CreateFmt */ inline __fastcall EInvalidEscaped(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
		/* Exception.CreateRes */ inline __fastcall EInvalidEscaped(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
		/* Exception.CreateRes */ inline __fastcall EInvalidEscaped(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
		/* Exception.CreateResFmt */ inline __fastcall EInvalidEscaped(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
		/* Exception.CreateResFmt */ inline __fastcall EInvalidEscaped(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
		/* Exception.CreateHelp */ inline __fastcall EInvalidEscaped(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
		/* Exception.CreateFmtHelp */ inline __fastcall EInvalidEscaped(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EInvalidEscaped(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
		/* Exception.CreateResHelp */ inline __fastcall EInvalidEscaped(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidEscaped(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
		/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidEscaped(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
		/* Exception.Destroy */ inline __fastcall virtual ~EInvalidEscaped() { }
		
	};
	
	
	
public:
	static constexpr System::Byte MaxNumberBuffer = System::Byte(0xff);
	
	static constexpr System::Byte MaxStackSize = System::Byte(0xff);
	
	
private:
	TTMSFNCJSONStreamReader* FReader;
	System::StaticArray<TTMSFNCJSONScope, 256> FStack;
	int FStackSize;
	TTMSFNCJSONState FPeeked;
	__int64 FPeekedInt64;
	System::StaticArray<System::WideChar, 256> FPeekedNumber;
	System::UnicodeString FPeekedString;
	TTMSFNCJSONState __fastcall NextPeek();
	void __fastcall CheckState(const TTMSFNCJSONState State);
	void __fastcall SkipChar();
	bool __fastcall IsLiteral(System::WideChar C);
	bool __fastcall IsDigit(System::WideChar C);
	TTMSFNCJSONState __fastcall DoPeek();
	void __fastcall PushScope(const TTMSFNCJSONScope Scope);
	System::WideChar __fastcall NextNonWhitespace();
	System::WideChar __fastcall ReadChar();
	TTMSFNCJSONState __fastcall PeekKeyword();
	TTMSFNCJSONState __fastcall PeekNumber();
	System::UnicodeString __fastcall InternalReadQuoted(const bool BuildString);
	System::UnicodeString __fastcall ReadQuoted();
	void __fastcall SkipQuoted();
	bool __fastcall SkipWhitespaceUntilEnd();
	
public:
	__fastcall TTMSFNCJSONReader(System::Classes::TStream* const AStream);
	__fastcall virtual ~TTMSFNCJSONReader();
	void __fastcall ReadBeginArray();
	void __fastcall ReadEndArray();
	void __fastcall ReadBeginObject();
	void __fastcall ReadEndObject();
	System::UnicodeString __fastcall PeekName();
	System::UnicodeString __fastcall ReadName();
	System::UnicodeString __fastcall ReadString();
	bool __fastcall ReadBoolean();
	double __fastcall ReadDouble();
	__int64 __fastcall ReadInt64();
	int __fastcall ReadInteger();
	void __fastcall SkipValue();
	void __fastcall ReadNull();
	bool __fastcall HasNext();
	TTMSFNCJSONToken __fastcall Peek();
	bool __fastcall IsNull();
	bool __fastcall Eof();
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


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncjsonreader */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCJSONREADER)
using namespace Fmx::Tmsfncjsonreader;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncjsonreaderHPP
