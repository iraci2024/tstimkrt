// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCTypes.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfnctypesHPP
#define Fmx_TmsfnctypesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <FMX.Graphics.hpp>
#include <System.SysUtils.hpp>
#include <FMX.Controls.hpp>
#include <System.Math.hpp>
#include <System.JSON.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnctypes
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE ITMSFNCProductInfo;
typedef System::DelphiInterface<ITMSFNCProductInfo> _di_ITMSFNCProductInfo;
class DELPHICLASS TTMSFNCSVGImport;
class DELPHICLASS TTMSFNCBitmap;
struct TRectFHelper /* Helper for record 'System::Types::TRectF' */;
struct TTMSFNCBitmapHelper /* Helper for class 'Fmx::Graphics::TBitmap*' */;
struct TTMSFNCObjectHelper /* Helper for class 'System::TObject*' */;
struct TTMSFNCJSONValueHelper /* Helper for class 'System::Json::TJSONValue*' */;
struct TTMSFNCJSONArrayHelper /* Helper for class 'System::Json::TJSONArray*' */;
class DELPHICLASS TTMSFNCMargins;
class DELPHICLASS TTMSFNCPoint;
class DELPHICLASS TTMSFNCScaledBitmap;
class DELPHICLASS TTMSFNCScaledBitmaps;
__interface DELPHIINTERFACE ITMSFNCCustomEditor;
typedef System::DelphiInterface<ITMSFNCCustomEditor> _di_ITMSFNCCustomEditor;
template<typename T> class DELPHICLASS TTMSFNCOwnedCollection__1;
class DELPHICLASS TTMSFNCPersistent;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{C53329EA-7B3A-4507-AD9E-88ACD6A85054}") ITMSFNCProductInfo  : public System::IInterface 
{
	virtual System::UnicodeString __fastcall GetVersion() = 0 ;
	virtual System::UnicodeString __fastcall GetDocURL() = 0 ;
	virtual System::UnicodeString __fastcall GetTipsURL() = 0 ;
};

typedef System::Uitypes::TMouseButton TTMSFNCMouseButton;

typedef System::Uitypes::TAlphaColor TTMSFNCSVGImportColor;

class PASCALIMPLEMENTATION TTMSFNCSVGImport : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	float FRotationAngle;
	System::Types::TRectF FViewRect;
	bool FGrayScale;
	System::Uitypes::TAlphaColor FCustomFillColor;
	System::Uitypes::TAlphaColor FCustomStrokeColor;
	System::Uitypes::TAlphaColor FTintColor;
	float FCustomOpacity;
	
public:
	virtual bool __fastcall HasElements() = 0 ;
	virtual int __fastcall ElementCount() = 0 ;
	virtual TTMSFNCBitmap* __fastcall GenerateBitmap(float AWidth = -1.000000E+00f, float AHeight = -1.000000E+00f) = 0 ;
	__fastcall virtual TTMSFNCSVGImport();
	virtual void __fastcall Clear() = 0 ;
	virtual void __fastcall Draw(Fmx::Graphics::TCanvas* const ACanvas, const System::Types::TRectF &ARect, bool ANative = false) = 0 ;
	virtual void __fastcall LoadFromText(const System::UnicodeString AText) = 0 ;
	virtual void __fastcall LoadFromFile(const System::UnicodeString AFile) = 0 ;
	virtual void __fastcall LoadFromStream(System::Classes::TStream* const AStream) = 0 ;
	virtual void __fastcall SaveToStream(System::Classes::TStream* const AStream) = 0 ;
	virtual void __fastcall SaveToFile(const System::UnicodeString AFile) = 0 ;
	__property System::Types::TRectF ViewRect = {read=FViewRect, write=FViewRect};
	
__published:
	__property bool GrayScale = {read=FGrayScale, write=FGrayScale, default=0};
	__property System::Uitypes::TAlphaColor CustomFillColor = {read=FCustomFillColor, write=FCustomFillColor, nodefault};
	__property System::Uitypes::TAlphaColor CustomStrokeColor = {read=FCustomStrokeColor, write=FCustomStrokeColor, nodefault};
	__property System::Uitypes::TAlphaColor TintColor = {read=FTintColor, write=FTintColor, nodefault};
	__property float CustomOpacity = {read=FCustomOpacity, write=FCustomOpacity};
	__property float RotationAngle = {read=FRotationAngle, write=FRotationAngle};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCSVGImport() { }
	
};


typedef Fmx::Graphics::TBitmap TTMSFNCDrawBitmap;

typedef Fmx::Graphics::TBitmap TTMSFNCBitmapHelperClass;

class PASCALIMPLEMENTATION TTMSFNCBitmap : public Fmx::Graphics::TBitmap
{
	typedef Fmx::Graphics::TBitmap inherited;
	
private:
	TTMSFNCSVGImport* FSVG;
	void __fastcall ReadSVG(System::Classes::TStream* Stream);
	void __fastcall WriteSVG(System::Classes::TStream* Stream);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	
public:
	virtual void __fastcall Draw(Fmx::Graphics::TCanvas* ACanvas, const System::Types::TRectF &Rect);
	__fastcall virtual TTMSFNCBitmap(System::Classes::TStream* const AStream);
	__fastcall virtual TTMSFNCBitmap(const System::UnicodeString AFileName);
	virtual bool __fastcall HasSVG();
	__fastcall virtual ~TTMSFNCBitmap();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	HIDESBASE virtual void __fastcall LoadFromFile(const System::UnicodeString AFileName);
	HIDESBASE virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	HIDESBASE virtual void __fastcall LoadFromBase64(const System::UnicodeString ABase64);
	HIDESBASE virtual void __fastcall LoadFromSVG(const System::UnicodeString ASVG);
	HIDESBASE virtual void __fastcall SaveToStream(System::Classes::TStream* Stream);
	HIDESBASE virtual void __fastcall SaveToFile(const System::UnicodeString AFilename);
	HIDESBASE virtual System::UnicodeString __fastcall SaveToBase64();
	__property TTMSFNCSVGImport* SVG = {read=FSVG};
public:
	/* TBitmap.Create */ inline __fastcall virtual TTMSFNCBitmap()/* overload */ : Fmx::Graphics::TBitmap() { }
	/* TBitmap.Create */ inline __fastcall virtual TTMSFNCBitmap(const int AWidth, const int AHeight)/* overload */ : Fmx::Graphics::TBitmap(AWidth, AHeight) { }
	/* TBitmap.CreateFromBitmapAndMask */ inline __fastcall TTMSFNCBitmap(Fmx::Graphics::TBitmap* const Bitmap, Fmx::Graphics::TBitmap* const Mask) : Fmx::Graphics::TBitmap(Bitmap, Mask) { }
	
};


typedef System::DynamicArray<System::UnicodeString> TTMSFNCObjectExcludePropertyListArray;

class PASCALIMPLEMENTATION TTMSFNCMargins : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	float FRight;
	float FBottom;
	float FTop;
	float FLeft;
	System::Classes::TNotifyEvent FOnChange;
	void __fastcall SetBottom(const float Value);
	void __fastcall SetLeft(const float Value);
	void __fastcall SetRight(const float Value);
	void __fastcall SetTop(const float Value);
	bool __fastcall IsLeftStored();
	bool __fastcall IsTopStored();
	bool __fastcall IsRightStored();
	bool __fastcall IsBottomStored();
	
protected:
	void __fastcall Changed();
	
public:
	__fastcall TTMSFNCMargins()/* overload */;
	__fastcall TTMSFNCMargins(const System::Types::TRectF &ARect)/* overload */;
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	bool __fastcall Empty();
	System::Types::TRectF __fastcall Rect();
	System::Types::TRectF __fastcall PaddingRect(const System::Types::TRectF &R);
	
__published:
	__property float Left = {read=FLeft, write=SetLeft, stored=IsLeftStored};
	__property float Top = {read=FTop, write=SetTop, stored=IsTopStored};
	__property float Right = {read=FRight, write=SetRight, stored=IsRightStored};
	__property float Bottom = {read=FBottom, write=SetBottom, stored=IsBottomStored};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCMargins() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCPoint : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	float FY;
	float FX;
	System::Classes::TNotifyEvent FOnChange;
	void __fastcall SetX(const float Value);
	void __fastcall SetY(const float Value);
	bool __fastcall IsXStored();
	bool __fastcall IsYStored();
	
protected:
	void __fastcall Changed();
	
public:
	__fastcall TTMSFNCPoint()/* overload */;
	__fastcall TTMSFNCPoint(const System::Types::TPointF &APoint)/* overload */;
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float X = {read=FX, write=SetX, stored=IsXStored};
	__property float Y = {read=FY, write=SetY, stored=IsYStored};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCPoint() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCScaledBitmap : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TTMSFNCBitmap* FBitmap;
	float FScale;
	System::UnicodeString FBitmapName;
	bool __fastcall IsScaleStored();
	void __fastcall SetBitmap(TTMSFNCBitmap* const Value);
	void __fastcall SetScale(const float Value);
	void __fastcall SetBitmapName(const System::UnicodeString Value);
	
protected:
	void __fastcall BitmapChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TTMSFNCScaledBitmap(System::Classes::TCollection* Collection);
	__fastcall virtual ~TTMSFNCScaledBitmap();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property TTMSFNCBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property System::UnicodeString BitmapName = {read=FBitmapName, write=SetBitmapName};
	__property float Scale = {read=FScale, write=SetScale, stored=IsScaleStored};
};


class PASCALIMPLEMENTATION TTMSFNCScaledBitmaps : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TTMSFNCScaledBitmap* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Classes::TNotifyEvent FOnChange;
	TTMSFNCScaledBitmap* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, TTMSFNCScaledBitmap* const Value);
	TTMSFNCBitmap* __fastcall GetBitmap(float Scale);
	void __fastcall SetBitmap(float Scale, TTMSFNCBitmap* const Value);
	
protected:
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TTMSFNCScaledBitmaps(System::Classes::TPersistent* AOwner);
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	HIDESBASE TTMSFNCScaledBitmap* __fastcall Add(float Scale = 1.000000E+00f)/* overload */;
	TTMSFNCScaledBitmap* __fastcall AddBitmapFromFile(System::UnicodeString FileName, float Scale = 1.000000E+00f);
	TTMSFNCScaledBitmap* __fastcall AddBitmapFromResource(System::UnicodeString ResourceName, float Scale = 1.000000E+00f)/* overload */;
	TTMSFNCScaledBitmap* __fastcall AddBitmapFromResource(System::UnicodeString ResourceName, NativeUInt AInstance, float Scale = 1.000000E+00f)/* overload */;
	TTMSFNCScaledBitmap* __fastcall AddBitmap(TTMSFNCBitmap* Bitmap, float Scale = 1.000000E+00f);
	TTMSFNCScaledBitmap* __fastcall AddBitmapName(System::UnicodeString BitmapName, float Scale = 1.000000E+00f);
	TTMSFNCScaledBitmap* __fastcall AddDrawBitmap(Fmx::Graphics::TBitmap* Bitmap, float Scale = 1.000000E+00f);
	HIDESBASE TTMSFNCScaledBitmap* __fastcall Insert(int Index)/* overload */;
	HIDESBASE TTMSFNCScaledBitmap* __fastcall Insert(int Index, float Scale)/* overload */;
	TTMSFNCBitmap* __fastcall GetBitmapByScale(float Scale);
	TTMSFNCScaledBitmap* __fastcall GetItemByScale(float Scale);
	__property TTMSFNCScaledBitmap* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
	__property TTMSFNCBitmap* Bitmaps[float Scale] = {read=GetBitmap, write=SetBitmap};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCScaledBitmaps() { }
	
};


enum DECLSPEC_DENUM TTMSFNCPictureFormat : unsigned char { pfBMP, pfGIF, pfJPG, pfPNG, pfICO, pfTiff, pfMetaFile, pfNone };

__interface  INTERFACE_UUID("{CC0C60B7-75F3-47CE-8A7F-8005A19F12E8}") ITMSFNCCustomEditor  : public System::IInterface 
{
	virtual void __fastcall SetText(System::UnicodeString AValue) = 0 ;
	virtual void __fastcall SetSelStart(int AValue) = 0 ;
	virtual void __fastcall SetSelLength(int AValue) = 0 ;
	virtual int __fastcall GetTextLength() = 0 ;
};

// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename T> class PASCALIMPLEMENTATION TTMSFNCOwnedCollection__1 : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
	
public:
	class DELPHICLASS TEnumerator;
	// Template declaration generated by Delphi parameterized types is
	// used only for accessing Delphi variables and fields.
	// Don't instantiate with new type parameters in user code.
	class PASCALIMPLEMENTATION TEnumerator : public System::TObject
	{
		typedef System::TObject inherited;
		
	private:
		int FIndex;
		TTMSFNCOwnedCollection__1<T>* FCol;
		
	public:
		__fastcall TEnumerator(TTMSFNCOwnedCollection__1<T>* ACol);
		T __fastcall GetCurrent();
		bool __fastcall MoveNext();
		__property T Current = {read=GetCurrent};
		HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
		HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
		__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
		HIDESBASE System::UnicodeString __fastcall ToJSON(TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
		HIDESBASE void __fastcall Log();
		HIDESBASE void __fastcall SaveToJSONFile(const System::UnicodeString AFileName);
		HIDESBASE void __fastcall LoadFromJSONFile(const System::UnicodeString AFileName);
		HIDESBASE void __fastcall SaveToJSONStream(System::Classes::TStream* const AStream);
		HIDESBASE void __fastcall LoadFromJSONStream(System::Classes::TStream* const AStream);
	public:
		/* TObject.Destroy */ inline __fastcall virtual ~TEnumerator() { }
		
	};
	
	
	
public:
	HIDESBASE TEnumerator* __fastcall GetEnumerator();
	HIDESBASE T __fastcall GetItem(int AIndex);
public:
	/* TOwnedCollection.Create */ inline __fastcall TTMSFNCOwnedCollection__1(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCOwnedCollection__1() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCPersistent : public System::Classes::TInterfacedPersistent
{
	typedef System::Classes::TInterfacedPersistent inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCPersistent() { }
	
public:
	/* TObject.Create */ inline __fastcall TTMSFNCPersistent() : System::Classes::TInterfacedPersistent() { }
	
};


typedef System::Types::TValueRelationship TTMSFNCValueRelationShip;

//-- var, const, procedure ---------------------------------------------------
#define TTMSFNCBaseDocURL L"https://download.tmssoftware.com/doc/"
#define TTMSFNCBaseTipsURL L"https://www.tmssoftware.com/site/tmsfnccore.asp?s=faq"
static const int pidWeb = int(0x10000);
static const System::Word TMSPlatformsDesktop = System::Word(0x1087);
static const int TMSPlatforms = int(0x194df);
static const int TMSPlatformsWeb = int(0x194df);
static const int TMSPlatformsWebDesktop = int(0x11087);
static const System::Int8 KEY_CANCEL = System::Int8(0x3);
static const System::Int8 KEY_CONTROL = System::Int8(0x11);
static const System::Int8 KEY_SHIFT = System::Int8(0x10);
static const System::Int8 KEY_ESCAPE = System::Int8(0x1b);
static const System::Int8 KEY_INSERT = System::Int8(0x2d);
static const System::Int8 KEY_DELETE = System::Int8(0x2e);
static const System::Int8 KEY_TAB = System::Int8(0x9);
static const System::Int8 KEY_SUBTRACT = System::Int8(0x6d);
static const System::Int8 KEY_ADD = System::Int8(0x6b);
static const System::Int8 KEY_MULTIPLY = System::Int8(0x6a);
static const System::Int8 KEY_DIVIDE = System::Int8(0x6f);
static const System::Int8 KEY_PRIOR = System::Int8(0x21);
static const System::Int8 KEY_NEXT = System::Int8(0x22);
static const System::Int8 KEY_UP = System::Int8(0x26);
static const System::Int8 KEY_DOWN = System::Int8(0x28);
static const System::Int8 KEY_RIGHT = System::Int8(0x27);
static const System::Int8 KEY_LEFT = System::Int8(0x25);
static const System::Int8 KEY_HOME = System::Int8(0x24);
static const System::Int8 KEY_END = System::Int8(0x23);
static const System::Int8 KEY_RETURN = System::Int8(0xd);
static const System::Int8 KEY_SPACE = System::Int8(0x20);
static const System::Int8 KEY_MENU = System::Int8(0x12);
static const System::Int8 KEY_BACK = System::Int8(0x8);
static const System::Int8 KEY_F1 = System::Int8(0x70);
static const System::Int8 KEY_F2 = System::Int8(0x71);
static const System::Int8 KEY_F3 = System::Int8(0x72);
static const System::Int8 KEY_F4 = System::Int8(0x73);
static const System::Int8 KEY_F5 = System::Int8(0x74);
static const System::Int8 KEY_F6 = System::Int8(0x75);
static const System::Int8 KEY_F7 = System::Int8(0x76);
static const System::Int8 KEY_F8 = System::Int8(0x77);
static const System::Int8 KEY_F9 = System::Int8(0x78);
static const System::Int8 KEY_F10 = System::Int8(0x79);
static const System::Int8 KEY_F11 = System::Int8(0x7a);
static const System::Int8 KEY_F12 = System::Int8(0x7b);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CreateNewGUID(void);
extern DELPHI_PACKAGE bool __fastcall CharIsHex(System::WideChar AChar);
extern DELPHI_PACKAGE bool __fastcall CharIsLetter(System::WideChar AChar);
extern DELPHI_PACKAGE bool __fastcall CharIsNumber(System::WideChar AChar);
extern DELPHI_PACKAGE bool __fastcall CharIsLetterOrNumber(System::WideChar AChar);
extern DELPHI_PACKAGE bool __fastcall CharInArray(System::WideChar AChar, System::WideChar *ACharArray, const int ACharArray_High);
extern DELPHI_PACKAGE bool __fastcall IsBitmapEmpty(Fmx::Graphics::TBitmap* ABitmap);
extern DELPHI_PACKAGE Fmx::Graphics::TBitmap* __fastcall BitmapToDrawBitmap(Fmx::Graphics::TBitmap* ABitmap)/* overload */;
extern DELPHI_PACKAGE bool __fastcall EqualRectEx(const System::Types::TRect &R1, const System::Types::TRect &R2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall EqualRectEx(const System::Types::TRectF &R1, const System::Types::TRectF &R2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IntersectRectEx(const System::Types::TRectF &Rect1, const System::Types::TRectF &Rect2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall OffsetRectEx(System::Types::TRect &R, int DX, int DY)/* overload */;
extern DELPHI_PACKAGE bool __fastcall OffsetRectEx(System::Types::TRectF &R, float DX, float DY)/* overload */;
extern DELPHI_PACKAGE void __fastcall InflateRectEx(System::Types::TRectF &R, float DX, float DY);
extern DELPHI_PACKAGE bool __fastcall PtInRectEx(const System::Types::TRectF &Rect, const System::Types::TPointF &P);
extern DELPHI_PACKAGE int __fastcall RectWidthEx(const System::Types::TRect &Rect)/* overload */;
extern DELPHI_PACKAGE int __fastcall RectHeightEx(const System::Types::TRect &Rect)/* overload */;
extern DELPHI_PACKAGE float __fastcall RectWidthEx(const System::Types::TRectF &Rect)/* overload */;
extern DELPHI_PACKAGE float __fastcall RectHeightEx(const System::Types::TRectF &Rect)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall RectCenterAtEx(const System::Types::TRectF &Rect, const System::Types::TRectF &ADesignatedArea);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall RectSnapToPixelEx(const System::Types::TRectF &Rect, const float AScale, const bool APlaceBetweenPixels);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall RectFitIntoEx(const System::Types::TRectF &Rect, const System::Types::TRectF &ADesignatedArea, /* out */ float &Ratio)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall RectFitIntoEx(const System::Types::TRectF &Rect, const System::Types::TRectF &ADesignatedArea)/* overload */;
extern DELPHI_PACKAGE System::Types::TRect __fastcall RectCenterEx(System::Types::TRect &R, const System::Types::TRect &B)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall RectCenterEx(System::Types::TRectF &R, const System::Types::TRectF &B)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IntersectRectEx(/* out */ System::Types::TRectF &Rect, const System::Types::TRectF &R1, const System::Types::TRectF &R2)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall EmptyRect(void);
extern DELPHI_PACKAGE bool __fastcall RectIsEmpty(const System::Types::TRectF &R);
extern DELPHI_PACKAGE System::Types::TValueRelationship __fastcall CompareValueEx(const System::Extended A, const System::Extended B, System::Extended Epsilon = 0.000000E+00);
extern DELPHI_PACKAGE bool __fastcall RectIntersectsWithEx(const System::Types::TRectF &ARect, const System::Types::TRectF &R);
extern DELPHI_PACKAGE System::Types::TPointF __fastcall CenterPointEx(const System::Types::TRectF &R);
extern DELPHI_PACKAGE System::Types::TRectF __fastcall MakeRectF(float Left, float Top, float Width, float Height);
extern DELPHI_PACKAGE float __fastcall GetPointLength(const System::Types::TPointF &Point);
extern DELPHI_PACKAGE System::Types::TRect __fastcall ConvertToRect(const System::Types::TRect &Rect)/* overload */;
extern DELPHI_PACKAGE System::Types::TRect __fastcall ConvertToRect(const System::Types::TRectF &Rect)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ConvertToRectF(const System::Types::TRectF &Rect)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ConvertToRectF(const System::Types::TRect &Rect)/* overload */;
extern DELPHI_PACKAGE System::Types::TPoint __fastcall ConvertToPoint(const System::Types::TPoint &Point)/* overload */;
extern DELPHI_PACKAGE System::Types::TPoint __fastcall ConvertToPoint(const System::Types::TPointF &Point)/* overload */;
extern DELPHI_PACKAGE System::Types::TPointF __fastcall ConvertToPointF(const System::Types::TPointF &Point)/* overload */;
extern DELPHI_PACKAGE System::Types::TPointF __fastcall ConvertToPointF(const System::Types::TPoint &Point)/* overload */;
extern DELPHI_PACKAGE System::Types::TSize __fastcall ConvertToSize(const System::Types::TSize &Size)/* overload */;
extern DELPHI_PACKAGE System::Types::TSize __fastcall ConvertToSize(const System::Types::TSizeF &Size)/* overload */;
extern DELPHI_PACKAGE System::Types::TSizeF __fastcall ConvertToSizeF(const System::Types::TSizeF &Size)/* overload */;
extern DELPHI_PACKAGE System::Types::TSizeF __fastcall ConvertToSizeF(const System::Types::TSize &Size)/* overload */;
}	/* namespace Tmsfnctypes */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCTYPES)
using namespace Fmx::Tmsfnctypes;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnctypesHPP
