// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCPDFCoreLibBase.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncpdfcorelibbaseHPP
#define Fmx_TmsfncpdfcorelibbaseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.UITypes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncpdfcorelibbase
{
//-- forward type declarations -----------------------------------------------
struct TTMSFNCPDFGraphicsLibTextRange;
class DELPHICLASS TTMSFNCPDFGraphicsLibFont;
class DELPHICLASS TTMSFNCPDFGraphicsFill;
class DELPHICLASS TTMSFNCPDFGraphicsStroke;
class DELPHICLASS TTMSFNCPDFGraphicsLibBase;
class DELPHICLASS TTMSFNCPDFPlatformServicesService;
class DELPHICLASS TTMSFNCPDFPlatformServicesList;
class DELPHICLASS TTMSFNCPDFLibPlatformServices;
class DELPHICLASS TTMSFNCPDFGraphicsLibUsedFontCharArray;
struct TTMSFNCPDFGraphicsLibFontCharArray;
struct TTMSFNCPDFGraphicsLibFontCharWidth;
class DELPHICLASS TTMSFNCPDFGraphicsLibOutputWriterStream;
class DELPHICLASS TTMSFNCPDFGraphicsLibOutputWriterStreams;
class DELPHICLASS TTMSFNCPDFGraphicsLibOutputWriter;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCPDFGraphicsLibPathDrawingMode : unsigned char { dmPathFill, dmPathEOFill, dmPathStroke, dmPathEOStroke, dmPathFillStroke, dmPathEOFillStroke };

enum DECLSPEC_DENUM TTMSFNCPDFGraphicsLibImageType : unsigned char { itOriginal, itPNG, itJPG };

enum DECLSPEC_DENUM TTMSFNCPDFGraphicsLibLineBreakMode : unsigned char { bmLineBreakModeWordWrap, bmLineBreakModeCharacterWrap, bmLineBreakModeClip, bmLineBreakModeHeadTruncation, bmLineBreakModeMiddleTruncation, bmLineBreakModeTailTruncation };

typedef System::DynamicArray<System::Types::TPointF> TTMSFNCPDFGraphicsLibPointArray;

typedef System::DynamicArray<System::Types::TRectF> TTMSFNCPDFGraphicsLibRectArray;

struct DECLSPEC_DRECORD TTMSFNCPDFGraphicsLibTextRange
{
public:
	int location;
	int length;
};


class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibFont : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FUpdateCount;
	System::UnicodeString FName;
	float FSize;
	System::Uitypes::TAlphaColor FColor;
	System::Classes::TNotifyEvent FOnChanged;
	System::Uitypes::TFontStyles FStyle;
	System::UnicodeString FFileName;
	void __fastcall SetName(const System::UnicodeString Value);
	void __fastcall SetSize(const float Value);
	void __fastcall SetColor(const System::Uitypes::TAlphaColor Value);
	bool __fastcall IsSizeStored();
	void __fastcall SetStyle(const System::Uitypes::TFontStyles Value);
	void __fastcall SetSizeNoScale(const float Value);
	void __fastcall SetFileName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall DoChanged();
	
public:
	__fastcall TTMSFNCPDFGraphicsLibFont();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property System::UnicodeString FileName = {read=FFileName, write=SetFileName};
	__property float Size = {read=FSize, write=SetSize, stored=IsSizeStored};
	__property float SizeNoScale = {read=FSize, write=SetSizeNoScale, stored=IsSizeStored};
	__property System::Uitypes::TAlphaColor Color = {read=FColor, write=SetColor, default=-16777216};
	__property System::Uitypes::TFontStyles Style = {read=FStyle, write=SetStyle, default=0};
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibFont() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsFill : public Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsFill
{
	typedef Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsFill inherited;
	
__published:
	__property Kind;
	__property Orientation = {default=1};
	__property Color;
	__property ColorTo;
public:
	/* TTMSFNCCustomGraphicsFill.Create */ inline __fastcall virtual TTMSFNCPDFGraphicsFill(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFillKind AKind, const System::Uitypes::TAlphaColor AColor, const System::Uitypes::TAlphaColor AColorTo, const System::Uitypes::TAlphaColor AColorMirror, const System::Uitypes::TAlphaColor AColorMirrorTo) : Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsFill(AKind, AColor, AColorTo, AColorMirror, AColorMirrorTo) { }
	/* TTMSFNCCustomGraphicsFill.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsFill() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsStroke : public Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsStroke
{
	typedef Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsStroke inherited;
	
__published:
	__property Kind;
	__property Color;
	__property Width;
public:
	/* TTMSFNCCustomGraphicsStroke.Create */ inline __fastcall virtual TTMSFNCPDFGraphicsStroke(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStrokeKind AKind, const System::Uitypes::TAlphaColor AColor) : Fmx::Tmsfncgraphicstypes::TTMSFNCCustomGraphicsStroke(AKind, AColor) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsStroke() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibBase : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TTMSFNCPDFGraphicsLibFont* FFont;
	TTMSFNCPDFGraphicsLibLineBreakMode FLineBreakMode;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign FAlignment;
	TTMSFNCPDFGraphicsLibFont* FURLFont;
	TTMSFNCPDFGraphicsFill* FFill;
	TTMSFNCPDFGraphicsStroke* FStroke;
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	void __fastcall SetAlignment(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Value);
	void __fastcall SetFont(TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetLineBreakMode(const TTMSFNCPDFGraphicsLibLineBreakMode Value);
	void __fastcall SetURLFont(TTMSFNCPDFGraphicsLibFont* const Value);
	void __fastcall SetFill(TTMSFNCPDFGraphicsFill* const Value);
	void __fastcall SetStroke(TTMSFNCPDFGraphicsStroke* const Value);
	void __fastcall SetBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value);
	
protected:
	void __fastcall FontChanged(System::TObject* Sender);
	void __fastcall FillChanged(System::TObject* Sender);
	void __fastcall StrokeChanged(System::TObject* Sender);
	virtual void __fastcall InitializeAppearance();
	virtual void __fastcall UpdateFont();
	virtual void __fastcall UpdateAlignment();
	virtual void __fastcall UpdateFill();
	virtual void __fastcall UpdateLineBreakMode();
	virtual void __fastcall UpdateStroke();
	virtual void __fastcall CreateClasses();
	
public:
	__fastcall virtual TTMSFNCPDFGraphicsLibBase();
	__fastcall virtual ~TTMSFNCPDFGraphicsLibBase();
	__property TTMSFNCPDFGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property TTMSFNCPDFGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property TTMSFNCPDFGraphicsLibFont* Font = {read=FFont, write=SetFont};
	__property TTMSFNCPDFGraphicsLibFont* URLFont = {read=FURLFont, write=SetURLFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextAlign Alignment = {read=FAlignment, write=SetAlignment, default=1};
	__property TTMSFNCPDFGraphicsLibLineBreakMode LineBreakMode = {read=FLineBreakMode, write=SetLineBreakMode, default=0};
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=FBitmapContainer, write=SetBitmapContainer};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFPlatformServicesService : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::_di_IInterface FInterface;
	System::UnicodeString FGUID;
	
public:
	__fastcall TTMSFNCPDFPlatformServicesService(System::UnicodeString AGUID, System::_di_IInterface AInterface);
	__property System::UnicodeString GUID = {read=FGUID};
	__property System::_di_IInterface Interface = {read=FInterface};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Fmx::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
	HIDESBASE void __fastcall Log();
	HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
	HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
	HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCPDFPlatformServicesService() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TTMSFNCPDFPlatformServicesList : public System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>
{
	typedef System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*> inherited;
	
private:
	System::_di_IInterface __fastcall GetValue(System::UnicodeString AGUID);
	
public:
	bool __fastcall ContainsKey(System::UnicodeString AGUID);
	void __fastcall RemoveByGUID(System::UnicodeString AGUID);
	__property System::_di_IInterface Interfaces[System::UnicodeString AGUID] = {read=GetValue};
public:
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList()/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>() { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCPDFPlatformServicesService*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(System::Generics::Collections::TEnumerable__1<TTMSFNCPDFPlatformServicesService*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFPlatformServicesList() { }
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCPDFPlatformServicesService*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(System::Generics::Collections::TEnumerable__1<TTMSFNCPDFPlatformServicesService*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCPDFCoreLibBase_TTMSFNCPDFPlatformServicesService>.Create */ inline __fastcall TTMSFNCPDFPlatformServicesList(TTMSFNCPDFPlatformServicesService* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TTMSFNCPDFPlatformServicesService*>(Values, Values_High) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFLibPlatformServices : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TTMSFNCPDFPlatformServicesList* FServicesList;
	static TTMSFNCPDFLibPlatformServices* FCurrent;
	static bool FCurrentReleased;
	__classmethod void __fastcall ReleaseCurrent();
	
public:
	__fastcall TTMSFNCPDFLibPlatformServices();
	__fastcall virtual ~TTMSFNCPDFLibPlatformServices();
	void __fastcall AddPlatformService(const GUID &AServiceGUID, const System::_di_IInterface AService);
	void __fastcall RemovePlatformService(const GUID &AServiceGUID);
	System::_di_IInterface __fastcall GetPlatformService(const GUID &AServiceGUID);
	bool __fastcall SupportsPlatformService(const GUID &AServiceGUID)/* overload */;
	bool __fastcall SupportsPlatformService(const GUID &AServiceGUID, System::_di_IInterface &AService)/* overload */;
	__classmethod TTMSFNCPDFLibPlatformServices* __fastcall Current();
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

class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibUsedFontCharArray : public System::Generics::Collections::TList__1<int>
{
	typedef System::Generics::Collections::TList__1<int> inherited;
	
public:
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibUsedFontCharArray()/* overload */ : System::Generics::Collections::TList__1<int>() { }
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibUsedFontCharArray(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<int> > AComparer)/* overload */ : System::Generics::Collections::TList__1<int>(AComparer) { }
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibUsedFontCharArray(System::Generics::Collections::TEnumerable__1<int>* const Collection)/* overload */ : System::Generics::Collections::TList__1<int>(Collection) { }
	/* {System_Generics_Collections}TList<System_Integer>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibUsedFontCharArray(const int *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<int>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<System_Integer>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibUsedFontCharArray() { }
	
};


struct DECLSPEC_DRECORD TTMSFNCPDFGraphicsLibFontCharArray
{
public:
	System::DynamicArray<System::Word> v;
	int c;
	int __fastcall Add(System::Word AValue);
	int __fastcall IndexOf(System::Word AValue);
	int __fastcall FirstValue();
	int __fastcall LastValue();
};


struct DECLSPEC_DRECORD TTMSFNCPDFGraphicsLibFontCharWidth
{
public:
	System::Word w;
	System::Word g;
	int i;
};


typedef System::DynamicArray<TTMSFNCPDFGraphicsLibFontCharWidth> TTMSFNCPDFGraphicsLibFontCharWidths;

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyTextEvent)(System::TObject* Sender, System::UnicodeString AValue);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyURLEvent)(System::TObject* Sender, const System::Types::TRectF &ARect, System::UnicodeString AURL);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyGoToEvent)(System::TObject* Sender, const System::Types::TRectF &ARect, System::UnicodeString ADestination);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyUnicodeEvent)(System::TObject* Sender, System::UnicodeString AValue);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyBitmapEvent)(System::TObject* Sender, Fmx::Graphics::TBitmap* AValue, TTMSFNCPDFGraphicsLibImageType AImageType, float AQuality, System::Uitypes::TAlphaColor ABackgroundColor, System::UnicodeString &ABitmapReference);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingEvent)(System::TObject* Sender, System::Uitypes::TAlphaColor AFillColor, System::Uitypes::TAlphaColor AFillColorTo, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFillOrientation AFillOrientation, System::UnicodeString &AShadingReference);

typedef void __fastcall (__closure *TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingRectEvent)(System::TObject* Sender, const System::Types::TRectF &ARect);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibOutputWriterStream : public System::Classes::TStringStream
{
	typedef System::Classes::TStringStream inherited;
	
private:
	System::TObject* FReference;
	
public:
	__fastcall virtual ~TTMSFNCPDFGraphicsLibOutputWriterStream();
	__property System::TObject* Reference = {read=FReference, write=FReference};
public:
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream()/* overload */ : System::Classes::TStringStream() { }
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream(const System::UnicodeString AString)/* overload */ : System::Classes::TStringStream(AString) { }
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream(const System::RawByteString AString)/* overload */ : System::Classes::TStringStream(AString) { }
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream(const System::UnicodeString AString, System::Sysutils::TEncoding* AEncoding, bool AOwnsEncoding)/* overload */ : System::Classes::TStringStream(AString, AEncoding, AOwnsEncoding) { }
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream(const System::UnicodeString AString, int ACodePage)/* overload */ : System::Classes::TStringStream(AString, ACodePage) { }
	/* TStringStream.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStream(const System::DynamicArray<System::Byte> ABytes)/* overload */ : System::Classes::TStringStream(ABytes) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibOutputWriterStreams : public System::Generics::Collections::TObjectList__1<System::Classes::TStream*>
{
	typedef System::Generics::Collections::TObjectList__1<System::Classes::TStream*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams()/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>() { }
	/* {System_Generics_Collections}TObjectList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<System::Classes::TStream*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(System::Generics::Collections::TEnumerable__1<System::Classes::TStream*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<System_Classes_TStream>.Destroy */ inline __fastcall virtual ~TTMSFNCPDFGraphicsLibOutputWriterStreams() { }
	
public:
	/* {System_Generics_Collections}TList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<System::Classes::TStream*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(AComparer) { }
	/* {System_Generics_Collections}TList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(System::Generics::Collections::TEnumerable__1<System::Classes::TStream*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(Collection) { }
	/* {System_Generics_Collections}TList<System_Classes_TStream>.Create */ inline __fastcall TTMSFNCPDFGraphicsLibOutputWriterStreams(System::Classes::TStream* const *Values, const int Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<System::Classes::TStream*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCPDFGraphicsLibOutputWriter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::DelphiInterface<System::Generics::Defaults::IComparer__1<int> > FCompareValues;
	TTMSFNCPDFGraphicsLibOutputWriterStream* FStream;
	TTMSFNCPDFGraphicsLibOutputWriterStream* FContentStream;
	TTMSFNCPDFGraphicsLibOutputWriterStreams* FStreams;
	System::UnicodeString FFontRefName;
	float FFontSize;
	System::UnicodeString FFontName;
	System::Classes::TNotifyEvent FOnFontChanged;
	System::Uitypes::TAlphaColor FFontColor;
	System::Uitypes::TFontStyles FFontStyle;
	System::UnicodeString FFontBase;
	int FFontWordSpacing;
	float FFontLeading;
	TTMSFNCPDFGraphicsLibFontCharWidths FFontCharWidths;
	int FFontCharSpacing;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyUnicodeEvent FOnNotifyUnicode;
	TTMSFNCPDFGraphicsLibFontCharArray FFontCharArray;
	TTMSFNCPDFGraphicsLibUsedFontCharArray* FFontUsedCharArray;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyBitmapEvent FOnNotifyBitmap;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingEvent FOnNotifyShading;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingRectEvent FOnNotifyShadingRect;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyTextEvent FOnNotifyText;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyURLEvent FOnNotifyURL;
	bool FFontUnicode;
	int FFontCapHeight;
	int FFontUnitsPerEm;
	bool FFontEmbedding;
	System::UnicodeString FFontGlyphRefName;
	TTMSFNCPDFGraphicsLibOutputWriterNotifyGoToEvent FOnNotifyGoTo;
	__int64 __fastcall GetStreamPosition();
	void __fastcall SetStreamPosition(const __int64 Value);
	
public:
	__fastcall TTMSFNCPDFGraphicsLibOutputWriter();
	__fastcall virtual ~TTMSFNCPDFGraphicsLibOutputWriter();
	TTMSFNCPDFGraphicsLibOutputWriterStreams* __fastcall Streams();
	int __fastcall GetFontCharWidth(System::UnicodeString AText, int APos);
	System::UnicodeString __fastcall ConvertColorToString(System::Uitypes::TAlphaColor AValue);
	System::UnicodeString __fastcall ConvertFloatToString(System::Extended AValue);
	bool __fastcall IsUnicodeString(System::UnicodeString AValue);
	System::UnicodeString __fastcall ConvertStringToHex(System::UnicodeString AValue);
	System::UnicodeString __fastcall AddHex4(unsigned AWordValue);
	System::UnicodeString __fastcall EscapeString(System::UnicodeString AValue);
	bool __fastcall FontCharArrayContainsValue(int ACharValue);
	System::Classes::TStringStream* __fastcall CompressString(System::UnicodeString AValue);
	System::Classes::TStringStream* __fastcall CompressStream(System::Classes::TStringStream* AStream);
	System::Classes::TStringStream* __fastcall FinishContentStream(System::UnicodeString AReference, bool ACompress, System::UnicodeString AAdditionalFlags = System::UnicodeString());
	float __fastcall TextHeight();
	void __fastcall FontCharArrayAddValue(int ACharValue);
	void __fastcall BeginText();
	void __fastcall WriteMatrix(const System::Types::TPointF &APoint, const System::Types::TSizeF &ASize)/* overload */;
	void __fastcall WriteMatrix(double m11, double m12, double m21, double m22, double dx, double dy)/* overload */;
	void __fastcall WriteTextMatrix(double m11, double m12, double m21, double m22, double dx, double dy);
	void __fastcall WriteRectangle(const System::Types::TRectF &ARect);
	void __fastcall WriteFontColor();
	void __fastcall WriteFontLeading();
	void __fastcall WriteFont();
	void __fastcall WriteFillColor(System::Uitypes::TAlphaColor AColor);
	void __fastcall WriteStrokeColor(System::Uitypes::TAlphaColor AColor);
	void __fastcall WriteStrokeKind(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStrokeKind AKind);
	void __fastcall WriteStrokeWidth(float AWidth);
	void __fastcall EndText();
	void __fastcall MoveTo(const System::Types::TPointF &APoint);
	void __fastcall LineTo(const System::Types::TPointF &APoint);
	void __fastcall CurveTo(float AX1, float AX2, float AX3, float AX4, float AX5, float AX6);
	void __fastcall CurveTo2(float AX1, float AX2, float AX3, float AX4);
	void __fastcall MoveTextTo(const System::Types::TPointF &APoint);
	void __fastcall MoveTextToNextLine();
	void __fastcall AddText(System::UnicodeString AValue);
	void __fastcall StartContentStream(System::TObject* AReference = (System::TObject*)(0x0));
	void __fastcall StartNewStream(System::TObject* AReference = (System::TObject*)(0x0));
	void __fastcall WriteString(System::UnicodeString AValue);
	void __fastcall Write(void * Buffer, int Count);
	void __fastcall ReplaceString(System::UnicodeString ASearchValue, System::UnicodeString AValue);
	void __fastcall ClearProperties();
	void __fastcall NotifyURL(const System::Types::TRectF &ARect, System::UnicodeString AURL);
	void __fastcall NotifyGoTo(const System::Types::TRectF &ARect, System::UnicodeString ADestination);
	void __fastcall NotifyShading(System::Uitypes::TAlphaColor AFillColor, System::Uitypes::TAlphaColor AFillColorTo, Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFillOrientation AFillOrientation, System::UnicodeString &AShadingReference);
	void __fastcall NotifyShadingRect(const System::Types::TRectF &ARect);
	void __fastcall NotifyUnicode(System::UnicodeString AValue);
	void __fastcall NotifyText(System::UnicodeString AValue);
	void __fastcall NotifyBitmap(Fmx::Graphics::TBitmap* AValue, TTMSFNCPDFGraphicsLibImageType AImageType, float AQuality, System::Uitypes::TAlphaColor ABackgroundColor, System::UnicodeString &ABitmapReference);
	void __fastcall ReplaceStrings(System::UnicodeString *ASearchValues, const int ASearchValues_High, System::UnicodeString *AValues, const int AValues_High);
	__property TTMSFNCPDFGraphicsLibOutputWriterStream* Stream = {read=FStream, write=FStream};
	__property TTMSFNCPDFGraphicsLibOutputWriterStream* ContentStream = {read=FContentStream, write=FContentStream};
	__property System::UnicodeString FontBase = {read=FFontBase, write=FFontBase};
	__property System::UnicodeString FontName = {read=FFontName, write=FFontName};
	__property System::UnicodeString FontRefName = {read=FFontRefName, write=FFontRefName};
	__property System::UnicodeString FontGlyphRefName = {read=FFontGlyphRefName, write=FFontGlyphRefName};
	__property bool FontEmbedding = {read=FFontEmbedding, write=FFontEmbedding, nodefault};
	__property float FontSize = {read=FFontSize, write=FFontSize};
	__property int FontCapHeight = {read=FFontCapHeight, write=FFontCapHeight, nodefault};
	__property int FontUnitsPerEm = {read=FFontUnitsPerEm, write=FFontUnitsPerEm, nodefault};
	__property System::Uitypes::TAlphaColor FontColor = {read=FFontColor, write=FFontColor, nodefault};
	__property System::Uitypes::TFontStyles FontStyle = {read=FFontStyle, write=FFontStyle, nodefault};
	__property int FontWordSpacing = {read=FFontWordSpacing, write=FFontWordSpacing, nodefault};
	__property int FontCharSpacing = {read=FFontCharSpacing, write=FFontCharSpacing, nodefault};
	__property float FontLeading = {read=FFontLeading, write=FFontLeading};
	__property TTMSFNCPDFGraphicsLibFontCharWidths FontCharWidths = {read=FFontCharWidths, write=FFontCharWidths};
	__property TTMSFNCPDFGraphicsLibFontCharArray FontCharArray = {read=FFontCharArray, write=FFontCharArray};
	__property TTMSFNCPDFGraphicsLibUsedFontCharArray* FontUsedCharArray = {read=FFontUsedCharArray, write=FFontUsedCharArray};
	__property bool FontUnicode = {read=FFontUnicode, write=FFontUnicode, nodefault};
	__property System::Classes::TNotifyEvent OnFontChanged = {read=FOnFontChanged, write=FOnFontChanged};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyTextEvent OnNotifyText = {read=FOnNotifyText, write=FOnNotifyText};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyUnicodeEvent OnNotifyUnicode = {read=FOnNotifyUnicode, write=FOnNotifyUnicode};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyURLEvent OnNotifyURL = {read=FOnNotifyURL, write=FOnNotifyURL};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyGoToEvent OnNotifyGoTo = {read=FOnNotifyGoTo, write=FOnNotifyGoTo};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyBitmapEvent OnNotifyBitmap = {read=FOnNotifyBitmap, write=FOnNotifyBitmap};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingEvent OnNotifyShading = {read=FOnNotifyShading, write=FOnNotifyShading};
	__property TTMSFNCPDFGraphicsLibOutputWriterNotifyShadingRectEvent OnNotifyShadingRect = {read=FOnNotifyShadingRect, write=FOnNotifyShadingRect};
	__property __int64 StreamPosition = {read=GetStreamPosition, write=SetStreamPosition};
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
static const System::WideChar PDFCR = (System::WideChar)(0xd);
static const System::WideChar PDFLF = (System::WideChar)(0xa);
#define PDFLB L"\r\n"
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 8> PDFEC;
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 8> PDFRC;
#define PDFLHFACTOR  (1.400000E+00)
#define PDFULFACTOR  (8.000000E-01)
#define PDFSTFACTOR  (5.000000E-01)
#define PDFULLWFACTOR  (5.000000E-02)
#define DefaultFontName L"Arial"
#define PDFMetaData L"<?xpacket begin=\"\u00ef\u00bb\u00bf\" id=\"W5M0MpCehiHzre"\
	L"SzNTczkc9d\"?><x:xmpmeta xmlns:x=\"adobe:ns:meta/\" x:xmpt"\
	L"k=\"%s\"><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22"\
	L"-rdf-syntax-ns#\"><rdf:Description rdf:about=\"\" xmlns:xm"\
	L"p=\"http://ns.adobe.com/xap/1.0/\"><xmp:CreateDate>%s</xmp"\
	L":CreateDate><xmp:ModifyDate>%s</xmp:ModifyDate><xmp:Creato"\
	L"rTool>%s</xmp:CreatorTool></rdf:Description><rdf:Descripti"\
	L"on rdf:about=\"\" xmlns:dc=\"http://purl.org/dc/elements/1"\
	L".1/\"><dc:title><rdf:Alt><rdf:li xml:lang=\"x-default\">%s"\
	L"</rdf:li></rdf:Alt></dc:title><dc:creator><rdf:Seq><rdf:li"\
	L" xml:lang=\"x-default\">%s</rdf:li></rdf:Seq></dc:creator>"\
	L"<dc:description><rdf:Alt><rdf:li xml:lang=\"x-default\">%s"\
	L"</rdf:li></rdf:Alt></dc:description></rdf:Description><rdf"\
	L":Description rdf:about=\"\" xmlns:pdf=\"http://ns.adobe.co"\
	L"m/pdf/1.3/\"><pdf:Keywords>%s</pdf:Keywords><pdf:Producer>"\
	L"%s</pdf:Producer></rdf:Description><rdf:Description rdf:ab"\
	L"out=\"\" xmlns:pdfaid=\"http://www.aiim.org/pdfa/ns/id/\">"\
	L"<pdfaid:part>1</pdfaid:part><pdfaid:conformance>A</pdfaid:"\
	L"conformance></rdf:Description></rdf:RDF></x:xmpmeta><?xpac"\
	L"ket end=\"w\"?>"
extern DELPHI_PACKAGE bool __fastcall CanCompress(void);
extern DELPHI_PACKAGE TTMSFNCPDFGraphicsLibOutputWriterStream* __fastcall CreateStringStream(System::UnicodeString AValue = System::UnicodeString());
extern DELPHI_PACKAGE int __fastcall MaxRange(const TTMSFNCPDFGraphicsLibTextRange &ARange);
extern DELPHI_PACKAGE TTMSFNCPDFGraphicsLibTextRange __fastcall MakeTextRange(int ALocation, int ALength);
extern DELPHI_PACKAGE void __fastcall GetAspectSize(float &x, float &y, float &w, float &h, float ow, float oh, float nw, float nh, bool AspectRatio, bool Stretch, bool Cropping);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DateTimeToIso(System::TDateTime ADateTime);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ConvertToPDFDate(System::TDateTime ADateTime);
extern DELPHI_PACKAGE unsigned __fastcall GetTickCountX(void);
}	/* namespace Tmsfncpdfcorelibbase */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCPDFCORELIBBASE)
using namespace Fmx::Tmsfncpdfcorelibbase;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncpdfcorelibbaseHPP
