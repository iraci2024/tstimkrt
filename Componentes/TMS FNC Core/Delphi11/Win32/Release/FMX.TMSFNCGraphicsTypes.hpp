// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCGraphicsTypes.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncgraphicstypesHPP
#define Fmx_TmsfncgraphicstypesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.Graphics.hpp>
#include <System.UITypes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncgraphicstypes
{
//-- forward type declarations -----------------------------------------------
struct TTMSFNCGraphicsColorEntry;
struct TTMSFNCGraphicsVector;
struct TTMSFNCGraphicsMatrix;
class DELPHICLASS TTMSFNCGraphicsSaveState;
class DELPHICLASS TTMSFNCGraphicsFillGradientItem;
class DELPHICLASS TTMSFNCGraphicsFillGradientItems;
class DELPHICLASS TTMSFNCCustomGraphicsFill;
class DELPHICLASS TTMSFNCGraphicsFill;
class DELPHICLASS TTMSFNCGraphicsFont;
__interface DELPHIINTERFACE ITMSFNCAppearanceGlobalFont;
typedef System::DelphiInterface<ITMSFNCAppearanceGlobalFont> _di_ITMSFNCAppearanceGlobalFont;
class DELPHICLASS TTMSFNCAppearanceGlobalFont;
class DELPHICLASS TTMSFNCCustomGraphicsStroke;
class DELPHICLASS TTMSFNCGraphicsStroke;
struct TTMSFNCGraphicsPathPoint;
class DELPHICLASS TTMSFNCGraphicsPathPoints;
class DELPHICLASS TTMSFNCGraphicsPath;
class DELPHICLASS TTMSFNCGraphicsColorObject;
//-- type declarations -------------------------------------------------------
typedef System::Uitypes::TAlphaColor TTMSFNCGraphicsColor;

struct DECLSPEC_DRECORD TTMSFNCGraphicsColorEntry
{
public:
	System::Uitypes::TAlphaColor Value;
	System::WideChar *Name;
};


enum DECLSPEC_DENUM TTMSFNCGraphicsStrokeKind : unsigned char { gskNone, gskSolid, gskDash, gskDot, gskDashDot, gskDashDotDot };

enum DECLSPEC_DENUM TTMSFNCGraphicsTextureMode : unsigned char { gtmOriginal, gtmFit, gtmStretch, gtmCenter, gtmTile };

enum DECLSPEC_DENUM TTMSFNCGraphicsFillKind : unsigned char { gfkNone, gfkSolid, gfkGradient, gfkTexture };

enum DECLSPEC_DENUM TTMSFNCGraphicsFillGradientMode : unsigned char { gfgmDefault, gfgmCollection };

enum DECLSPEC_DENUM TTMSFNCGraphicsFillGradientType : unsigned char { gfgtLinear, gfgtRadial };

enum DECLSPEC_DENUM TTMSFNCGraphicsFillOrientation : unsigned char { gfoHorizontal, gfoVertical, gfoCustom };

enum DECLSPEC_DENUM TTMSFNCGraphicsTextAlign : unsigned char { gtaCenter, gtaLeading, gtaTrailing };

enum DECLSPEC_DENUM TTMSFNCGraphicsTextTrimming : unsigned char { gttNone, gttCharacter, gttWord };

enum DECLSPEC_DENUM TTMSFNCGraphicsSide : unsigned char { gsLeft, gsTop, gsRight, gsBottom };

typedef System::Set<TTMSFNCGraphicsSide, TTMSFNCGraphicsSide::gsLeft, TTMSFNCGraphicsSide::gsBottom> TTMSFNCGraphicsSides;

enum DECLSPEC_DENUM TTMSFNCGraphicsTextQuality : unsigned char { gtqDefault, gtqAntiAliasing, gtqClearType };

typedef System::StaticArray<float, 3> TTMSFNCGraphicsVectorArray;

struct DECLSPEC_DRECORD TTMSFNCGraphicsVector
{
	
public:
	union
	{
		struct 
		{
			float X;
			float Y;
			float W;
		};
		struct 
		{
			TTMSFNCGraphicsVectorArray V;
		};
		
	};
};


typedef System::StaticArray<TTMSFNCGraphicsVector, 3> TTMSFNCGraphicsMatrixArray;

struct DECLSPEC_DRECORD TTMSFNCGraphicsMatrix
{
private:
	TTMSFNCGraphicsMatrix __fastcall Scale(const float AFactor);
	
public:
	static TTMSFNCGraphicsMatrix __fastcall Identity();
	static TTMSFNCGraphicsMatrix __fastcall CreateRotation(const float AAngle);
	static TTMSFNCGraphicsMatrix __fastcall CreateScaling(const float AScaleX, const float AScaleY);
	static TTMSFNCGraphicsMatrix __fastcall CreateTranslation(const float ADeltaX, const float ADeltaY);
	static TTMSFNCGraphicsMatrix __fastcall _op_Multiply(const TTMSFNCGraphicsMatrix &AMatrix1, const TTMSFNCGraphicsMatrix &AMatrix2);
	static System::Types::TPointF __fastcall _op_Multiply(const System::Types::TPointF &APoint, const TTMSFNCGraphicsMatrix &AMatrix);
	float __fastcall Determinant();
	TTMSFNCGraphicsMatrix __fastcall Adjoint();
	TTMSFNCGraphicsMatrix __fastcall Inverse();
	
	friend TTMSFNCGraphicsMatrix operator *(const TTMSFNCGraphicsMatrix &AMatrix1, const TTMSFNCGraphicsMatrix &AMatrix2) { return TTMSFNCGraphicsMatrix::_op_Multiply(AMatrix1, AMatrix2); }
	friend System::Types::TPointF operator *(const System::Types::TPointF &APoint, const TTMSFNCGraphicsMatrix &AMatrix) { return TTMSFNCGraphicsMatrix::_op_Multiply(APoint, AMatrix); }
	
public:
	union
	{
		struct 
		{
			float m11;
			float m12;
			float m13;
			float m21;
			float m22;
			float m23;
			float m31;
			float m32;
			float m33;
		};
		struct 
		{
			TTMSFNCGraphicsMatrixArray M;
		};
		
	};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsSaveState : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Fmx::Graphics::TCanvasSaveState* FSaveDC;
	TTMSFNCGraphicsFont* FFont;
	TTMSFNCGraphicsStroke* FStroke;
	TTMSFNCGraphicsFill* FFill;
	NativeUInt FCustomSaveDC;
	void __fastcall SetStroke(TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetFill(TTMSFNCGraphicsFill* const Value);
	void __fastcall SetFont(TTMSFNCGraphicsFont* const Value);
	
public:
	__fastcall TTMSFNCGraphicsSaveState();
	__fastcall virtual ~TTMSFNCGraphicsSaveState();
	__property TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property TTMSFNCGraphicsFont* Font = {read=FFont, write=SetFont};
	__property NativeUInt CustomSaveDC = {read=FCustomSaveDC, write=FCustomSaveDC, nodefault};
	__property Fmx::Graphics::TCanvasSaveState* SaveDC = {read=FSaveDC, write=FSaveDC};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsFillGradientItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::Uitypes::TAlphaColor FColor;
	float FPosition;
	float FOpacity;
	bool __fastcall IsPositionStored();
	void __fastcall SetColor(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetPosition(const float Value);
	bool __fastcall IsOpacityStored();
	void __fastcall SetOpacity(const float Value);
	
protected:
	void __fastcall UpdateGradient();
	
public:
	__fastcall virtual TTMSFNCGraphicsFillGradientItem(System::Classes::TCollection* Collection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float Opacity = {read=FOpacity, write=SetOpacity, stored=IsOpacityStored};
	__property System::Uitypes::TAlphaColor Color = {read=FColor, write=SetColor, default=-1};
	__property float Position = {read=FPosition, write=SetPosition, stored=IsPositionStored};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsFillGradientItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsFillGradientItems : public Fmx::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCGraphicsFillGradientItem*>
{
	typedef Fmx::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCGraphicsFillGradientItem*> inherited;
	
public:
	TTMSFNCGraphicsFillGradientItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	TTMSFNCCustomGraphicsFill* FOwner;
	TTMSFNCGraphicsFillGradientItem* __fastcall GetItemEx(int Index);
	void __fastcall SetItemEx(int Index, TTMSFNCGraphicsFillGradientItem* const Value);
	
protected:
	void __fastcall UpdateGradient();
	virtual System::Classes::TCollectionItemClass __fastcall GetGradientClass();
	
public:
	__fastcall TTMSFNCGraphicsFillGradientItems(TTMSFNCCustomGraphicsFill* AOwner);
	HIDESBASE TTMSFNCGraphicsFillGradientItem* __fastcall Add();
	HIDESBASE TTMSFNCGraphicsFillGradientItem* __fastcall Insert(int index);
	__property TTMSFNCGraphicsFillGradientItem* Items[int Index] = {read=GetItemEx, write=SetItemEx/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsFillGradientItems() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TTMSFNCCustomGraphicsFill : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FUpdateCount;
	System::Classes::TNotifyEvent FOnChanged;
	TTMSFNCGraphicsFillOrientation FOrientation;
	TTMSFNCGraphicsFillKind FKind;
	TTMSFNCGraphicsFillKind FDefaultKind;
	System::Uitypes::TAlphaColor FColor;
	System::Uitypes::TAlphaColor FDefaultColor;
	System::Uitypes::TAlphaColor FColorTo;
	System::Uitypes::TAlphaColor FColorMirror;
	System::Uitypes::TAlphaColor FColorMirrorTo;
	System::Uitypes::TAlphaColor FDefaultColorTo;
	System::Uitypes::TAlphaColor FDefaultColorMirror;
	System::Uitypes::TAlphaColor FDefaultColorMirrorTo;
	float FOpacity;
	Fmx::Tmsfnctypes::TTMSFNCBitmap* FTexture;
	TTMSFNCGraphicsTextureMode FTextureMode;
	TTMSFNCGraphicsFillGradientMode FGradientMode;
	TTMSFNCGraphicsFillGradientItems* FGradientItems;
	TTMSFNCGraphicsFillGradientType FGradientType;
	float FGradientAngle;
	System::Types::TPointF FGradientCenterPoint;
	System::Uitypes::TAlphaColor FGradientCenterColor;
	TTMSFNCGraphicsMatrix FGradientMatrix;
	void __fastcall SetKind(const TTMSFNCGraphicsFillKind Value);
	void __fastcall SetOrientation(const TTMSFNCGraphicsFillOrientation Value);
	void __fastcall SetColor(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetColorTo(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetColorMirror(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetColorMirrorTo(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetOpacity(const float Value);
	bool __fastcall IsOpacityStored();
	void __fastcall SetTexture(Fmx::Tmsfnctypes::TTMSFNCBitmap* const Value);
	void __fastcall SetTextureMode(const TTMSFNCGraphicsTextureMode Value);
	void __fastcall SetGradientItems(TTMSFNCGraphicsFillGradientItems* const Value);
	void __fastcall SetGradientMode(const TTMSFNCGraphicsFillGradientMode Value);
	void __fastcall SetGradientType(const TTMSFNCGraphicsFillGradientType Value);
	bool __fastcall IsGradientAngleStored();
	void __fastcall SetGradientAngle(const float Value);
	void __fastcall SetGradientCenterPoint(const System::Types::TPointF &Value);
	void __fastcall SetGradientCenterColor(const System::Uitypes::TAlphaColor Value);
	bool __fastcall IsColorStored();
	bool __fastcall IsKindStored();
	bool __fastcall IsColorMirrorStored();
	bool __fastcall IsColorMirrorToStored();
	bool __fastcall IsColorToStored();
	
protected:
	void __fastcall Changed();
	void __fastcall TextureChanged(System::TObject* Sender);
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
	__property float GradientAngle = {read=FGradientAngle, write=SetGradientAngle, stored=IsGradientAngleStored};
	__property TTMSFNCGraphicsFillGradientType GradientType = {read=FGradientType, write=SetGradientType, default=0};
	__property TTMSFNCGraphicsFillGradientMode GradientMode = {read=FGradientMode, write=SetGradientMode, default=0};
	__property TTMSFNCGraphicsFillGradientItems* GradientItems = {read=FGradientItems, write=SetGradientItems};
	__property System::Types::TPointF GradientCenterPoint = {read=FGradientCenterPoint, write=SetGradientCenterPoint};
	__property TTMSFNCGraphicsMatrix GradientMatrix = {read=FGradientMatrix, write=FGradientMatrix};
	__property System::Uitypes::TAlphaColor GradientCenterColor = {read=FGradientCenterColor, write=SetGradientCenterColor, default=0};
	__property TTMSFNCGraphicsFillOrientation GradientOrientation = {read=FOrientation, write=SetOrientation, default=1};
	__property TTMSFNCGraphicsFillKind Kind = {read=FKind, write=SetKind, stored=IsKindStored, nodefault};
	__property TTMSFNCGraphicsFillOrientation Orientation = {read=FOrientation, write=SetOrientation, default=1};
	__property System::Uitypes::TAlphaColor Color = {read=FColor, write=SetColor, stored=IsColorStored, nodefault};
	__property System::Uitypes::TAlphaColor ColorTo = {read=FColorTo, write=SetColorTo, stored=IsColorToStored, nodefault};
	__property System::Uitypes::TAlphaColor ColorMirror = {read=FColorMirror, write=SetColorMirror, stored=IsColorMirrorStored, nodefault};
	__property System::Uitypes::TAlphaColor ColorMirrorTo = {read=FColorMirrorTo, write=SetColorMirrorTo, stored=IsColorMirrorToStored, nodefault};
	__property float Opacity = {read=FOpacity, write=SetOpacity, stored=IsOpacityStored};
	__property Fmx::Tmsfnctypes::TTMSFNCBitmap* Texture = {read=FTexture, write=SetTexture};
	__property TTMSFNCGraphicsTextureMode TextureMode = {read=FTextureMode, write=SetTextureMode, default=2};
	
public:
	__fastcall virtual TTMSFNCCustomGraphicsFill(const TTMSFNCGraphicsFillKind AKind, const System::Uitypes::TAlphaColor AColor, const System::Uitypes::TAlphaColor AColorTo, const System::Uitypes::TAlphaColor AColorMirror, const System::Uitypes::TAlphaColor AColorMirrorTo);
	__fastcall virtual ~TTMSFNCCustomGraphicsFill();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	virtual void __fastcall ClearGradientItems();
	virtual TTMSFNCGraphicsFillGradientItem* __fastcall AddGradientItem(System::Uitypes::TAlphaColor AColor, float APosition, float AOpacity = 1.000000E+00f);
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsFill : public TTMSFNCCustomGraphicsFill
{
	typedef TTMSFNCCustomGraphicsFill inherited;
	
public:
	__property GradientMatrix;
	__property GradientAngle;
	__property GradientType = {default=0};
	__property GradientMode = {default=0};
	__property GradientItems;
	__property GradientCenterPoint;
	__property GradientCenterColor = {default=0};
	__property GradientOrientation = {default=1};
	
__published:
	__property OnChanged;
	__property Kind;
	__property Orientation = {default=1};
	__property Color;
	__property ColorTo;
	__property ColorMirror;
	__property ColorMirrorTo;
	__property TextureMode = {default=2};
	__property Texture;
	__property Opacity;
public:
	/* TTMSFNCCustomGraphicsFill.Create */ inline __fastcall virtual TTMSFNCGraphicsFill(const TTMSFNCGraphicsFillKind AKind, const System::Uitypes::TAlphaColor AColor, const System::Uitypes::TAlphaColor AColorTo, const System::Uitypes::TAlphaColor AColorMirror, const System::Uitypes::TAlphaColor AColorMirrorTo) : TTMSFNCCustomGraphicsFill(AKind, AColor, AColorTo, AColorMirror, AColorMirrorTo) { }
	/* TTMSFNCCustomGraphicsFill.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsFill() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsFont : public Fmx::Graphics::TFont
{
	typedef Fmx::Graphics::TFont inherited;
	
private:
	int FUpdateCount;
	System::Classes::TNotifyEvent FOnChanged;
	int FHeight;
	System::Uitypes::TAlphaColor FColor;
	void __fastcall SetColor(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetName();
	
protected:
	HIDESBASE void __fastcall DoChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TTMSFNCGraphicsFont();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall AssignSource(System::Classes::TPersistent* Source);
	__property int Height = {read=FHeight, write=FHeight, nodefault};
	
__published:
	__property System::Uitypes::TAlphaColor Color = {read=FColor, write=SetColor, default=-16777216};
	__property System::UnicodeString Name = {read=GetName, write=SetName};
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsFont() { }
	
};


enum DECLSPEC_DENUM TTMSFNCAppearanceGlobalFontType : unsigned char { aftNone, aftColor, aftSize, aftName, aftScale, aftStyle };

__interface  INTERFACE_UUID("{85CF4F6B-3FF9-4CB7-AD6A-7FC477ED5462}") ITMSFNCAppearanceGlobalFont  : public System::IInterface 
{
	virtual void __fastcall SetFonts(TTMSFNCAppearanceGlobalFontType ASetType) = 0 ;
};

class PASCALIMPLEMENTATION TTMSFNCAppearanceGlobalFont : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TComponent* FOwner;
	System::Uitypes::TAlphaColor FColor;
	float FSize;
	System::UnicodeString FName;
	double FScale;
	double FOldScale;
	System::Uitypes::TFontStyles FStyle;
	void __fastcall SetColor(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetSize(const float Value);
	void __fastcall SetName(const System::UnicodeString Value);
	void __fastcall SetScale(const double Value);
	void __fastcall SetStyle(const System::Uitypes::TFontStyles Value);
	
protected:
	void __fastcall SetFonts(TTMSFNCAppearanceGlobalFontType ASetType);
	
public:
	__fastcall TTMSFNCAppearanceGlobalFont(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCAppearanceGlobalFont();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall ApplyChange(TTMSFNCGraphicsFont* AFont, TTMSFNCAppearanceGlobalFontType ASetType);
	double __fastcall GetOldScale();
	
__published:
	__property System::Uitypes::TAlphaColor Color = {read=FColor, write=SetColor, default=0};
	__property float Size = {read=FSize, write=SetSize};
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property double Scale = {read=FScale, write=SetScale};
	__property System::Uitypes::TFontStyles Style = {read=FStyle, write=SetStyle, nodefault};
};


class PASCALIMPLEMENTATION TTMSFNCCustomGraphicsStroke : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FUpdateCount;
	System::Classes::TNotifyEvent FOnChanged;
	TTMSFNCGraphicsStrokeKind FKind;
	System::Uitypes::TAlphaColor FColor;
	float FWidth;
	float FOpacity;
	TTMSFNCGraphicsStrokeKind FDefaultKind;
	System::Uitypes::TAlphaColor FDefaultColor;
	void __fastcall SetKind(const TTMSFNCGraphicsStrokeKind Value);
	void __fastcall SetColor(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetWidth(const float Value);
	void __fastcall SetOpacity(const float Value);
	bool __fastcall IsOpacityStored();
	bool __fastcall IsWidthStored();
	bool __fastcall IsKindStored();
	bool __fastcall IsColorStored();
	
protected:
	void __fastcall Changed();
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
	__property TTMSFNCGraphicsStrokeKind Kind = {read=FKind, write=SetKind, stored=IsKindStored, nodefault};
	__property System::Uitypes::TAlphaColor Color = {read=FColor, write=SetColor, stored=IsColorStored, nodefault};
	__property float Width = {read=FWidth, write=SetWidth, stored=IsWidthStored};
	__property float Opacity = {read=FOpacity, write=SetOpacity, stored=IsOpacityStored};
	
public:
	__fastcall virtual TTMSFNCCustomGraphicsStroke(const TTMSFNCGraphicsStrokeKind AKind, const System::Uitypes::TAlphaColor AColor);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCCustomGraphicsStroke() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsStroke : public TTMSFNCCustomGraphicsStroke
{
	typedef TTMSFNCCustomGraphicsStroke inherited;
	
__published:
	__property OnChanged;
	__property Kind;
	__property Color;
	__property Width;
	__property Opacity;
public:
	/* TTMSFNCCustomGraphicsStroke.Create */ inline __fastcall virtual TTMSFNCGraphicsStroke(const TTMSFNCGraphicsStrokeKind AKind, const System::Uitypes::TAlphaColor AColor) : TTMSFNCCustomGraphicsStroke(AKind, AColor) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsStroke() { }
	
};


typedef System::DynamicArray<System::Types::TPointF> TTMSFNCGraphicsPathPolygon;

enum DECLSPEC_DENUM TTMSFNCGraphicsPathPointKind : unsigned char { gppMoveTo, gppLineTo, gppCurveTo, gppClose };

typedef System::StaticArray<System::Types::TPointF, 4> TTMSFNCGraphicsPathCubicBezier;

struct DECLSPEC_DRECORD TTMSFNCGraphicsPathPoint
{
public:
	TTMSFNCGraphicsPathPointKind Kind;
	bool Dummy;
	System::Types::TPointF Point;
};


enum DECLSPEC_DENUM TTMSFNCGraphicsPathDrawMode : unsigned char { pdmPolygon, pdmPolyline, pdmPath };

class PASCALIMPLEMENTATION TTMSFNCGraphicsPathPoints : public System::Generics::Collections::TList__1<TTMSFNCGraphicsPathPoint>
{
	typedef System::Generics::Collections::TList__1<TTMSFNCGraphicsPathPoint> inherited;
	
public:
	/* {System_Generics_Collections}TList<FMX_TMSFNCGraphicsTypes_TTMSFNCGraphicsPathPoint>.Create */ inline __fastcall TTMSFNCGraphicsPathPoints()/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsPathPoint>() { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCGraphicsTypes_TTMSFNCGraphicsPathPoint>.Create */ inline __fastcall TTMSFNCGraphicsPathPoints(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCGraphicsPathPoint> > AComparer)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsPathPoint>(AComparer) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCGraphicsTypes_TTMSFNCGraphicsPathPoint>.Create */ inline __fastcall TTMSFNCGraphicsPathPoints(System::Generics::Collections::TEnumerable__1<TTMSFNCGraphicsPathPoint>* const Collection)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsPathPoint>(Collection) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCGraphicsTypes_TTMSFNCGraphicsPathPoint>.Create */ inline __fastcall TTMSFNCGraphicsPathPoints(const TTMSFNCGraphicsPathPoint *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsPathPoint>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<FMX_TMSFNCGraphicsTypes_TTMSFNCGraphicsPathPoint>.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsPathPoints() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsPath : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	TTMSFNCGraphicsPathPoint operator[](int AIndex) { return this->Points[AIndex]; }
	
private:
	System::Types::TPointF FStartPoint;
	bool FClippable;
	TTMSFNCGraphicsPathPoints* FPoints;
	void __fastcall CalculateBezierCoefficients(const TTMSFNCGraphicsPathCubicBezier &Bezier, /* out */ double &AX, /* out */ double &BX, /* out */ double &CX, /* out */ double &AY, /* out */ double &BY, /* out */ double &CY);
	int __fastcall GetCount();
	TTMSFNCGraphicsPathPoint __fastcall GetPoint(int AIndex);
	
public:
	__fastcall virtual TTMSFNCGraphicsPath();
	__fastcall virtual ~TTMSFNCGraphicsPath();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	System::Types::TPointF __fastcall LastPoint();
	bool __fastcall IsPointVisible(const System::Types::TPointF &P);
	bool __fastcall IsRectVisible(const System::Types::TRectF &R);
	System::Types::TPointF __fastcall PointOnBezier(const System::Types::TPointF &StartPoint, const double AX, const double BX, const double CX, const double AY, const double BY, const double CY, const double T);
	TTMSFNCGraphicsPathPolygon __fastcall CreateBezier(const TTMSFNCGraphicsPathCubicBezier &Bezier, const int PointCount);
	System::Types::TRectF __fastcall GetBounds();
	bool __fastcall IsClippable();
	void __fastcall MoveTo(const System::Types::TPointF &P);
	void __fastcall LineTo(const System::Types::TPointF &P);
	void __fastcall CurveTo(const System::Types::TPointF &ControlPoint1, const System::Types::TPointF &ControlPoint2, const System::Types::TPointF &EndPoint);
	void __fastcall SmoothCurveTo(const System::Types::TPointF &ControlPoint2, const System::Types::TPointF &EndPoint);
	void __fastcall QuadCurveTo(const System::Types::TPointF &ControlPoint, const System::Types::TPointF &EndPoint);
	void __fastcall AddPolygon(const TTMSFNCGraphicsPathPolygon APolygon);
	void __fastcall ClosePath();
	void __fastcall AddLine(const System::Types::TPointF &StartPoint, const System::Types::TPointF &EndPoint);
	void __fastcall AddEllipse(const System::Types::TRectF &ARect);
	void __fastcall AddRectangle(const System::Types::TRectF &ARect, float ARoundingX = 0.000000E+00f, float ARoundingY = 0.000000E+00f);
	void __fastcall AddArc(const System::Types::TPointF &Center, const System::Types::TPointF &Radius, float StartAngle, float SweepAngle);
	void __fastcall AddPath(TTMSFNCGraphicsPath* APath);
	void __fastcall ApplyMatrix(const TTMSFNCGraphicsMatrix &AMatrix);
	void __fastcall Clear();
	void __fastcall Flatten(const float Flatness = 2.500000E-01f);
	void __fastcall FlattenToPolygon(TTMSFNCGraphicsPathPolygon &Polygon, const float Flatness = 2.500000E-01f);
	__property int Count = {read=GetCount, nodefault};
	__property TTMSFNCGraphicsPathPoint Points[int AIndex] = {read=GetPoint/*, default*/};
	__property TTMSFNCGraphicsPathPoints* PathData = {read=FPoints};
	__property bool Clippable = {read=FClippable, write=FClippable, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsColorObject : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Uitypes::TAlphaColor FColor;
	
public:
	__fastcall TTMSFNCGraphicsColorObject(System::Uitypes::TAlphaColor AColor);
	__property System::Uitypes::TAlphaColor Color = {read=FColor, write=FColor, nodefault};
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
	/* TObject.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsColorObject() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TTMSFNCGraphicsModifyRectMode : unsigned char { gcrmNone, gcrmExpandAll, gcrmShrinkAll, gcrmShiftRightAndExpandHeight, gcrmShiftDownAndExpandWidth, gcrmShiftRightAndShrinkHeight, gcrmShiftRightDown, gcrmShiftRightUp, gcrmShiftLeftUp, gcrmShiftLeftDown, gcrmShiftUpAndExpandWidth, gcrmShiftLeftAndExpandHeight };

enum DECLSPEC_DENUM TTMSFNCGraphicsModifyPointMode : unsigned char { gcpmNone, gcpmLeftUp, gcpmRightDown, gcpmLeftDown, gcpmRightUp };

enum DECLSPEC_DENUM TTMSFNCGraphicsExpanderState : unsigned char { gesCollapsed, gesExpanded };

enum DECLSPEC_DENUM TTMSFNCGraphicsCompactState : unsigned char { gcsCollapsed, gcsExpanded };

enum DECLSPEC_DENUM TTMSFNCGraphicsCorner : unsigned char { gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight };

typedef System::Set<TTMSFNCGraphicsCorner, TTMSFNCGraphicsCorner::gcTopLeft, TTMSFNCGraphicsCorner::gcBottomRight> TTMSFNCGraphicsCorners;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 FPC_FULLVERSION = System::Int8(0x0);
static const System::Extended PathScale = 1.000000E-04;
static const System::Extended PathFontSize = 1.000000E-02;
static const System::Extended PathPosition = 1.000000E-03;
static const System::Extended PathAngle = 1.000000E-04;
static const unsigned gcAlpha = unsigned(0xff000000);
static const unsigned gcAliceblue = unsigned(0xfff0f8ff);
static const unsigned gcAntiquewhite = unsigned(0xfffaebd7);
static const unsigned gcAqua = unsigned(0xff00ffff);
static const unsigned gcAquamarine = unsigned(0xff7fffd4);
static const unsigned gcAzure = unsigned(0xfff0ffff);
static const unsigned gcBeige = unsigned(0xfff5f5dc);
static const unsigned gcBisque = unsigned(0xffffe4c4);
static const unsigned gcBlack = unsigned(0xff000000);
static const unsigned gcBlanchedalmond = unsigned(0xffffebcd);
static const unsigned gcBlue = unsigned(0xff0000ff);
static const unsigned gcBlueviolet = unsigned(0xff8a2be2);
static const unsigned gcBrown = unsigned(0xffa52a2a);
static const unsigned gcBurlywood = unsigned(0xffdeb887);
static const unsigned gcCadetblue = unsigned(0xff5f9ea0);
static const unsigned gcChartreuse = unsigned(0xff7fff00);
static const unsigned gcChocolate = unsigned(0xffd2691e);
static const unsigned gcCoral = unsigned(0xffff7f50);
static const unsigned gcCornflowerblue = unsigned(0xff6495ed);
static const unsigned gcCornsilk = unsigned(0xfffff8dc);
static const unsigned gcCrimson = unsigned(0xffdc143c);
static const unsigned gcCyan = unsigned(0xff00ffff);
static const unsigned gcDarkblue = unsigned(0xff00008b);
static const unsigned gcDarkcyan = unsigned(0xff008b8b);
static const unsigned gcDarkgoldenrod = unsigned(0xffb8860b);
static const unsigned gcDarkgray = unsigned(0xffa9a9a9);
static const unsigned gcDarkgreen = unsigned(0xff006400);
static const unsigned gcDarkgrey = unsigned(0xffa9a9a9);
static const unsigned gcDarkkhaki = unsigned(0xffbdb76b);
static const unsigned gcDarkmagenta = unsigned(0xff8b008b);
static const unsigned gcDarkolivegreen = unsigned(0xff556b2f);
static const unsigned gcDarkorange = unsigned(0xffff8c00);
static const unsigned gcDarkorchid = unsigned(0xff9932cc);
static const unsigned gcDarkred = unsigned(0xff8b0000);
static const unsigned gcDarksalmon = unsigned(0xffe9967a);
static const unsigned gcDarkseagreen = unsigned(0xff8fbc8f);
static const unsigned gcDarkslateblue = unsigned(0xff483d8b);
static const unsigned gcDarkslategray = unsigned(0xff2f4f4f);
static const unsigned gcDarkslategrey = unsigned(0xff2f4f4f);
static const unsigned gcDarkturquoise = unsigned(0xff00ced1);
static const unsigned gcDarkviolet = unsigned(0xff9400d3);
static const unsigned gcDeeppink = unsigned(0xffff1493);
static const unsigned gcDeepskyblue = unsigned(0xff00bfff);
static const unsigned gcDimgray = unsigned(0xff696969);
static const unsigned gcDimgrey = unsigned(0xff696969);
static const unsigned gcDodgerblue = unsigned(0xff1e90ff);
static const unsigned gcFirebrick = unsigned(0xffb22222);
static const unsigned gcFloralwhite = unsigned(0xfffffaf0);
static const unsigned gcForestgreen = unsigned(0xff228b22);
static const unsigned gcFuchsia = unsigned(0xffff00ff);
static const unsigned gcGainsboro = unsigned(0xffdcdcdc);
static const unsigned gcGhostwhite = unsigned(0xfff8f8ff);
static const unsigned gcGold = unsigned(0xffffd700);
static const unsigned gcGoldenrod = unsigned(0xffdaa520);
static const unsigned gcGray = unsigned(0xff808080);
static const unsigned gcGreen = unsigned(0xff008000);
static const unsigned gcGreenyellow = unsigned(0xffadff2f);
static const unsigned gcGrey = unsigned(0xff808080);
static const unsigned gcHoneydew = unsigned(0xfff0fff0);
static const unsigned gcHotpink = unsigned(0xffff69b4);
static const unsigned gcIndianred = unsigned(0xffcd5c5c);
static const unsigned gcIndigo = unsigned(0xff4b0082);
static const unsigned gcIvory = unsigned(0xfffffff0);
static const unsigned gcKhaki = unsigned(0xfff0e68c);
static const unsigned gcLavender = unsigned(0xffe6e6fa);
static const unsigned gcLavenderblush = unsigned(0xfffff0f5);
static const unsigned gcLawngreen = unsigned(0xff7cfc00);
static const unsigned gcLemonchiffon = unsigned(0xfffffacd);
static const unsigned gcLightblue = unsigned(0xffadd8e6);
static const unsigned gcLightcoral = unsigned(0xfff08080);
static const unsigned gcLightcyan = unsigned(0xffe0ffff);
static const unsigned gcLightgoldenrodyellow = unsigned(0xfffafad2);
static const unsigned gcLightgray = unsigned(0xffd3d3d3);
static const unsigned gcLightgreen = unsigned(0xff90ee90);
static const unsigned gcLightgrey = unsigned(0xffd3d3d3);
static const unsigned gcLightpink = unsigned(0xffffb6c1);
static const unsigned gcLightsalmon = unsigned(0xffffa07a);
static const unsigned gcLightseagreen = unsigned(0xff20b2aa);
static const unsigned gcLightskyblue = unsigned(0xff87cefa);
static const unsigned gcLightslategray = unsigned(0xff778899);
static const unsigned gcLightslategrey = unsigned(0xff778899);
static const unsigned gcLightsteelblue = unsigned(0xffb0c4de);
static const unsigned gcLightyellow = unsigned(0xffffffe0);
static const unsigned gcLtGray = unsigned(0xffc0c0c0);
static const unsigned gcMedGray = unsigned(0xffa0a0a0);
static const unsigned gcDkGray = unsigned(0xff808080);
static const unsigned gcMoneyGreen = unsigned(0xffc0dcc0);
static const unsigned gcLegacySkyBlue = unsigned(0xfff0caa6);
static const unsigned gcCream = unsigned(0xfff0fbff);
static const unsigned gcLime = unsigned(0xff00ff00);
static const unsigned gcLimegreen = unsigned(0xff32cd32);
static const unsigned gcLinen = unsigned(0xfffaf0e6);
static const unsigned gcMagenta = unsigned(0xffff00ff);
static const unsigned gcMaroon = unsigned(0xff800000);
static const unsigned gcMediumaquamarine = unsigned(0xff66cdaa);
static const unsigned gcMediumblue = unsigned(0xff0000cd);
static const unsigned gcMediumorchid = unsigned(0xffba55d3);
static const unsigned gcMediumpurple = unsigned(0xff9370db);
static const unsigned gcMediumseagreen = unsigned(0xff3cb371);
static const unsigned gcMediumslateblue = unsigned(0xff7b68ee);
static const unsigned gcMediumspringgreen = unsigned(0xff00fa9a);
static const unsigned gcMediumturquoise = unsigned(0xff48d1cc);
static const unsigned gcMediumvioletred = unsigned(0xffc71585);
static const unsigned gcMidnightblue = unsigned(0xff191970);
static const unsigned gcMintcream = unsigned(0xfff5fffa);
static const unsigned gcMistyrose = unsigned(0xffffe4e1);
static const unsigned gcMoccasin = unsigned(0xffffe4b5);
static const unsigned gcNavajowhite = unsigned(0xffffdead);
static const unsigned gcNavy = unsigned(0xff000080);
static const unsigned gcOldlace = unsigned(0xfffdf5e6);
static const unsigned gcOlive = unsigned(0xff808000);
static const unsigned gcOlivedrab = unsigned(0xff6b8e23);
static const unsigned gcOrange = unsigned(0xffffa500);
static const unsigned gcOrangered = unsigned(0xffff4500);
static const unsigned gcOrchid = unsigned(0xffda70d6);
static const unsigned gcPalegoldenrod = unsigned(0xffeee8aa);
static const unsigned gcPalegreen = unsigned(0xff98fb98);
static const unsigned gcPaleturquoise = unsigned(0xffafeeee);
static const unsigned gcPalevioletred = unsigned(0xffdb7093);
static const unsigned gcPapayawhip = unsigned(0xffffefd5);
static const unsigned gcPeachpuff = unsigned(0xffffdab9);
static const unsigned gcPeru = unsigned(0xffcd853f);
static const unsigned gcPink = unsigned(0xffffc0cb);
static const unsigned gcPlum = unsigned(0xffdda0dd);
static const unsigned gcPowderblue = unsigned(0xffb0e0e6);
static const unsigned gcPurple = unsigned(0xff800080);
static const unsigned gcRed = unsigned(0xffff0000);
static const unsigned gcRosybrown = unsigned(0xffbc8f8f);
static const unsigned gcRoyalblue = unsigned(0xff4169e1);
static const unsigned gcSaddlebrown = unsigned(0xff8b4513);
static const unsigned gcSalmon = unsigned(0xfffa8072);
static const unsigned gcSandybrown = unsigned(0xfff4a460);
static const unsigned gcSeagreen = unsigned(0xff2e8b57);
static const unsigned gcSeashell = unsigned(0xfffff5ee);
static const unsigned gcSienna = unsigned(0xffa0522d);
static const unsigned gcSilver = unsigned(0xffc0c0c0);
static const unsigned gcSkyblue = unsigned(0xff87ceeb);
static const unsigned gcSlateblue = unsigned(0xff6a5acd);
static const unsigned gcSlategray = unsigned(0xff708090);
static const unsigned gcSlategrey = unsigned(0xff708090);
static const unsigned gcSnow = unsigned(0xfffffafa);
static const unsigned gcSpringgreen = unsigned(0xff00ff7f);
static const unsigned gcSteelblue = unsigned(0xff4682b4);
static const unsigned gcTan = unsigned(0xffd2b48c);
static const unsigned gcTeal = unsigned(0xff008080);
static const unsigned gcThistle = unsigned(0xffd8bfd8);
static const unsigned gcTomato = unsigned(0xffff6347);
static const unsigned gcTurquoise = unsigned(0xff40e0d0);
static const unsigned gcViolet = unsigned(0xffee82ee);
static const unsigned gcWheat = unsigned(0xfff5deb3);
static const unsigned gcWhite = unsigned(0xffffffff);
static const unsigned gcWhitesmoke = unsigned(0xfff5f5f5);
static const unsigned gcYellow = unsigned(0xffffff00);
static const unsigned gcYellowgreen = unsigned(0xff9acd32);
static const System::Int8 gcNull = System::Int8(0x0);
static const System::Byte TMSFNCGraphicsColorCount = System::Byte(0x9a);
extern DELPHI_PACKAGE System::StaticArray<TTMSFNCGraphicsColorEntry, 154> TMSFNCGraphicsColors;
#define AllSides (System::Set<TTMSFNCGraphicsSide, TTMSFNCGraphicsSide::gsLeft, TTMSFNCGraphicsSide::gsBottom>() << TTMSFNCGraphicsSide::gsLeft << TTMSFNCGraphicsSide::gsTop << TTMSFNCGraphicsSide::gsRight << TTMSFNCGraphicsSide::gsBottom )
extern DELPHI_PACKAGE float Epsilon;
extern DELPHI_PACKAGE bool __fastcall IsMatrixEmpty(const TTMSFNCGraphicsMatrix &AMatrix);
extern DELPHI_PACKAGE TTMSFNCGraphicsMatrix __fastcall MatrixIdentity(void);
extern DELPHI_PACKAGE TTMSFNCGraphicsMatrix __fastcall MatrixEmpty(void);
extern DELPHI_PACKAGE TTMSFNCGraphicsMatrix __fastcall MatrixCreateRotation(const float AAngle);
extern DELPHI_PACKAGE TTMSFNCGraphicsMatrix __fastcall MatrixCreateScaling(const float AScaleX, const float AScaleY);
extern DELPHI_PACKAGE TTMSFNCGraphicsMatrix __fastcall MatrixCreateTranslation(const float ADeltaX, const float ADeltaY);
extern DELPHI_PACKAGE System::Types::TPointF __fastcall MatrixMultiply(const System::Types::TPointF &APoint, const TTMSFNCGraphicsMatrix &AMatrix)/* overload */;
extern DELPHI_PACKAGE TTMSFNCGraphicsMatrix __fastcall MatrixMultiply(const TTMSFNCGraphicsMatrix &AMatrix1, const TTMSFNCGraphicsMatrix &AMatrix2)/* overload */;
extern DELPHI_PACKAGE System::Types::TRectF __fastcall ModifyRect(const System::Types::TRectF &ARect, const TTMSFNCGraphicsModifyRectMode ARectMode);
extern DELPHI_PACKAGE System::Types::TPointF __fastcall ModifyPoint(const System::Types::TPointF &APoint, const TTMSFNCGraphicsModifyPointMode APointMode);
extern DELPHI_PACKAGE System::Classes::TStringList* __fastcall ColorLookup(void);
extern DELPHI_PACKAGE bool __fastcall PolyInRect(TTMSFNCGraphicsPathPolygon APolygon, const System::Types::TRectF &ARect);
extern DELPHI_PACKAGE bool __fastcall RectInPoly(const System::Types::TRectF &ARect, TTMSFNCGraphicsPathPolygon APolygon);
extern DELPHI_PACKAGE bool __fastcall PointInPoly(const System::Types::TPointF &APoint, TTMSFNCGraphicsPathPolygon APolygon);
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall Blend(System::Uitypes::TAlphaColor AColor1, System::Uitypes::TAlphaColor AColor2, System::Byte ALevel);
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall Lighter(System::Uitypes::TAlphaColor AColor, System::Byte APercent);
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall Darker(System::Uitypes::TAlphaColor AColor, System::Byte APercent);
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall MakeGraphicsColor(System::Byte ARed, System::Byte AGreen, System::Byte ABlue, System::Byte AAlpha = (System::Byte)(0xff));
}	/* namespace Tmsfncgraphicstypes */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCGRAPHICSTYPES)
using namespace Fmx::Tmsfncgraphicstypes;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncgraphicstypesHPP
