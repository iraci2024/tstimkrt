// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCGraphicsTypes.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfncgraphicstypesHPP
#define Vcl_TmsfncgraphicstypesHPP

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
#include <VCL.TMSFNCTypes.hpp>
#include <Vcl.Graphics.hpp>
#include <System.UITypes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
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
typedef System::Uitypes::TColor TTMSFNCGraphicsColor;

struct DECLSPEC_DRECORD TTMSFNCGraphicsColorEntry
{
public:
	System::Uitypes::TColor Value;
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
	int FSaveDC;
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
	__property int SaveDC = {read=FSaveDC, write=FSaveDC, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsFillGradientItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::Uitypes::TColor FColor;
	float FPosition;
	float FOpacity;
	bool __fastcall IsPositionStored();
	void __fastcall SetColor(const System::Uitypes::TColor Value);
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
	__property System::Uitypes::TColor Color = {read=FColor, write=SetColor, default=16777215};
	__property float Position = {read=FPosition, write=SetPosition, stored=IsPositionStored};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsFillGradientItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTMSFNCGraphicsFillGradientItems : public Vcl::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCGraphicsFillGradientItem*>
{
	typedef Vcl::Tmsfnctypes::TTMSFNCOwnedCollection__1<TTMSFNCGraphicsFillGradientItem*> inherited;
	
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
	System::Uitypes::TColor FColor;
	System::Uitypes::TColor FDefaultColor;
	System::Uitypes::TColor FColorTo;
	System::Uitypes::TColor FColorMirror;
	System::Uitypes::TColor FColorMirrorTo;
	System::Uitypes::TColor FDefaultColorTo;
	System::Uitypes::TColor FDefaultColorMirror;
	System::Uitypes::TColor FDefaultColorMirrorTo;
	float FOpacity;
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FTexture;
	TTMSFNCGraphicsTextureMode FTextureMode;
	TTMSFNCGraphicsFillGradientMode FGradientMode;
	TTMSFNCGraphicsFillGradientItems* FGradientItems;
	TTMSFNCGraphicsFillGradientType FGradientType;
	float FGradientAngle;
	System::Types::TPointF FGradientCenterPoint;
	System::Uitypes::TColor FGradientCenterColor;
	TTMSFNCGraphicsMatrix FGradientMatrix;
	void __fastcall SetKind(const TTMSFNCGraphicsFillKind Value);
	void __fastcall SetOrientation(const TTMSFNCGraphicsFillOrientation Value);
	void __fastcall SetColor(const System::Uitypes::TColor Value);
	void __fastcall SetColorTo(const System::Uitypes::TColor Value);
	void __fastcall SetColorMirror(const System::Uitypes::TColor Value);
	void __fastcall SetColorMirrorTo(const System::Uitypes::TColor Value);
	void __fastcall SetOpacity(const float Value);
	bool __fastcall IsOpacityStored();
	void __fastcall SetTexture(Vcl::Tmsfnctypes::TTMSFNCBitmap* const Value);
	void __fastcall SetTextureMode(const TTMSFNCGraphicsTextureMode Value);
	void __fastcall SetGradientItems(TTMSFNCGraphicsFillGradientItems* const Value);
	void __fastcall SetGradientMode(const TTMSFNCGraphicsFillGradientMode Value);
	void __fastcall SetGradientType(const TTMSFNCGraphicsFillGradientType Value);
	bool __fastcall IsGradientAngleStored();
	void __fastcall SetGradientAngle(const float Value);
	void __fastcall SetGradientCenterPoint(const System::Types::TPointF &Value);
	void __fastcall SetGradientCenterColor(const System::Uitypes::TColor Value);
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
	__property System::Uitypes::TColor GradientCenterColor = {read=FGradientCenterColor, write=SetGradientCenterColor, default=-1};
	__property TTMSFNCGraphicsFillOrientation GradientOrientation = {read=FOrientation, write=SetOrientation, default=1};
	__property TTMSFNCGraphicsFillKind Kind = {read=FKind, write=SetKind, stored=IsKindStored, nodefault};
	__property TTMSFNCGraphicsFillOrientation Orientation = {read=FOrientation, write=SetOrientation, default=1};
	__property System::Uitypes::TColor Color = {read=FColor, write=SetColor, stored=IsColorStored, nodefault};
	__property System::Uitypes::TColor ColorTo = {read=FColorTo, write=SetColorTo, stored=IsColorToStored, nodefault};
	__property System::Uitypes::TColor ColorMirror = {read=FColorMirror, write=SetColorMirror, stored=IsColorMirrorStored, nodefault};
	__property System::Uitypes::TColor ColorMirrorTo = {read=FColorMirrorTo, write=SetColorMirrorTo, stored=IsColorMirrorToStored, nodefault};
	__property float Opacity = {read=FOpacity, write=SetOpacity, stored=IsOpacityStored};
	__property Vcl::Tmsfnctypes::TTMSFNCBitmap* Texture = {read=FTexture, write=SetTexture};
	__property TTMSFNCGraphicsTextureMode TextureMode = {read=FTextureMode, write=SetTextureMode, default=2};
	
public:
	__fastcall virtual TTMSFNCCustomGraphicsFill(const TTMSFNCGraphicsFillKind AKind, const System::Uitypes::TColor AColor, const System::Uitypes::TColor AColorTo, const System::Uitypes::TColor AColorMirror, const System::Uitypes::TColor AColorMirrorTo);
	__fastcall virtual ~TTMSFNCCustomGraphicsFill();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	virtual void __fastcall ClearGradientItems();
	virtual TTMSFNCGraphicsFillGradientItem* __fastcall AddGradientItem(System::Uitypes::TColor AColor, float APosition, float AOpacity = 1.000000E+00f);
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsFill : public TTMSFNCCustomGraphicsFill
{
	typedef TTMSFNCCustomGraphicsFill inherited;
	
public:
	__property Opacity;
	__property GradientMatrix;
	__property GradientAngle;
	__property GradientType = {default=0};
	__property GradientMode = {default=0};
	__property GradientItems;
	__property GradientCenterPoint;
	__property GradientCenterColor = {default=-1};
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
public:
	/* TTMSFNCCustomGraphicsFill.Create */ inline __fastcall virtual TTMSFNCGraphicsFill(const TTMSFNCGraphicsFillKind AKind, const System::Uitypes::TColor AColor, const System::Uitypes::TColor AColorTo, const System::Uitypes::TColor AColorMirror, const System::Uitypes::TColor AColorMirrorTo) : TTMSFNCCustomGraphicsFill(AKind, AColor, AColorTo, AColorMirror, AColorMirrorTo) { }
	/* TTMSFNCCustomGraphicsFill.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsFill() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsFont : public Vcl::Graphics::TFont
{
	typedef Vcl::Graphics::TFont inherited;
	
private:
	int FUpdateCount;
	System::Classes::TNotifyEvent FOnChanged;
	
protected:
	void __fastcall DoChanged(System::TObject* Sender);
	
public:
	__fastcall virtual TTMSFNCGraphicsFont();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall AssignSource(System::Classes::TPersistent* Source);
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
public:
	/* TFont.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsFont() { }
	
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
	System::Uitypes::TColor FColor;
	float FSize;
	System::UnicodeString FName;
	double FScale;
	double FOldScale;
	System::Uitypes::TFontStyles FStyle;
	void __fastcall SetColor(const System::Uitypes::TColor Value);
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
	__property System::Uitypes::TColor Color = {read=FColor, write=SetColor, default=-1};
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
	System::Uitypes::TColor FColor;
	float FWidth;
	float FOpacity;
	TTMSFNCGraphicsStrokeKind FDefaultKind;
	System::Uitypes::TColor FDefaultColor;
	void __fastcall SetKind(const TTMSFNCGraphicsStrokeKind Value);
	void __fastcall SetColor(const System::Uitypes::TColor Value);
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
	__property System::Uitypes::TColor Color = {read=FColor, write=SetColor, stored=IsColorStored, nodefault};
	__property float Width = {read=FWidth, write=SetWidth, stored=IsWidthStored};
	__property float Opacity = {read=FOpacity, write=SetOpacity, stored=IsOpacityStored};
	
public:
	__fastcall virtual TTMSFNCCustomGraphicsStroke(const TTMSFNCGraphicsStrokeKind AKind, const System::Uitypes::TColor AColor);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTMSFNCCustomGraphicsStroke() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCGraphicsStroke : public TTMSFNCCustomGraphicsStroke
{
	typedef TTMSFNCCustomGraphicsStroke inherited;
	
public:
	__property Opacity;
	
__published:
	__property OnChanged;
	__property Kind;
	__property Color;
	__property Width;
public:
	/* TTMSFNCCustomGraphicsStroke.Create */ inline __fastcall virtual TTMSFNCGraphicsStroke(const TTMSFNCGraphicsStrokeKind AKind, const System::Uitypes::TColor AColor) : TTMSFNCCustomGraphicsStroke(AKind, AColor) { }
	
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
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPathPoint>.Create */ inline __fastcall TTMSFNCGraphicsPathPoints()/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsPathPoint>() { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPathPoint>.Create */ inline __fastcall TTMSFNCGraphicsPathPoints(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TTMSFNCGraphicsPathPoint> > AComparer)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsPathPoint>(AComparer) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPathPoint>.Create */ inline __fastcall TTMSFNCGraphicsPathPoints(System::Generics::Collections::TEnumerable__1<TTMSFNCGraphicsPathPoint>* const Collection)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsPathPoint>(Collection) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPathPoint>.Create */ inline __fastcall TTMSFNCGraphicsPathPoints(const TTMSFNCGraphicsPathPoint *Values, const int Values_High)/* overload */ : System::Generics::Collections::TList__1<TTMSFNCGraphicsPathPoint>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<VCL_TMSFNCGraphicsTypes_TTMSFNCGraphicsPathPoint>.Destroy */ inline __fastcall virtual ~TTMSFNCGraphicsPathPoints() { }
	
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
	System::Uitypes::TColor FColor;
	
public:
	__fastcall TTMSFNCGraphicsColorObject(System::Uitypes::TColor AColor);
	__property System::Uitypes::TColor Color = {read=FColor, write=FColor, nodefault};
	HIDESBASE System::UnicodeString __fastcall ToJSON()/* overload */;
	HIDESBASE void __fastcall FromJSON(const System::UnicodeString Value);
	__property System::UnicodeString JSON = {read=ToJSON, write=FromJSON};
	HIDESBASE System::UnicodeString __fastcall ToJSON(Vcl::Tmsfnctypes::TTMSFNCObjectExcludePropertyListArray AExcludedProperties)/* overload */;
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
static const int gcAliceblue = int(0xfff8f0);
static const int gcAntiquewhite = int(0xd7ebfa);
static const int gcAqua = int(0xffff00);
static const int gcAquamarine = int(0xd4ff7f);
static const int gcAzure = int(0xfffff0);
static const int gcBeige = int(0xdcf5f5);
static const int gcBisque = int(0xc4e4ff);
static const System::Int8 gcBlack = System::Int8(0x0);
static const int gcBlanchedalmond = int(0xcdebff);
static const int gcBlue = int(0xff0000);
static const int gcBlueviolet = int(0xe22b8a);
static const int gcBrown = int(0x2a2aa5);
static const int gcBurlywood = int(0x87b8de);
static const int gcCadetblue = int(0xa09e5f);
static const System::Word gcChartreuse = System::Word(0xff7f);
static const int gcChocolate = int(0x1e69d2);
static const int gcCoral = int(0x507fff);
static const int gcCornflowerblue = int(0xed9564);
static const int gcCornsilk = int(0xdcf8ff);
static const int gcCrimson = int(0x3c14dc);
static const int gcCyan = int(0xffff00);
static const int gcDarkblue = int(0x8b0000);
static const int gcDarkcyan = int(0x8b8b00);
static const int gcDarkgoldenrod = int(0xb86b8);
static const int gcDarkgray = int(0xa9a9a9);
static const System::Word gcDarkgreen = System::Word(0x6400);
static const int gcDarkgrey = int(0xa9a9a9);
static const int gcDarkkhaki = int(0x6bb7bd);
static const int gcDarkmagenta = int(0x8b008b);
static const int gcDarkolivegreen = int(0x2f6b55);
static const System::Word gcDarkorange = System::Word(0x8cff);
static const int gcDarkorchid = int(0xcc3299);
static const System::Byte gcDarkred = System::Byte(0x8b);
static const int gcDarksalmon = int(0x7a96e9);
static const int gcDarkseagreen = int(0x8fbc8f);
static const int gcDarkslateblue = int(0x8b3d48);
static const int gcDarkslategray = int(0x4f4f2f);
static const int gcDarkslategrey = int(0x4f4f2f);
static const int gcDarkturquoise = int(0xd1ce00);
static const int gcDarkviolet = int(0xd30094);
static const int gcDeeppink = int(0x9314ff);
static const int gcDeepskyblue = int(0xffbf00);
static const int gcDimgray = int(0x696969);
static const int gcDimgrey = int(0x696969);
static const int gcDodgerblue = int(0xff901e);
static const int gcFirebrick = int(0x2222b2);
static const int gcFloralwhite = int(0xf0faff);
static const int gcForestgreen = int(0x228b22);
static const int gcFuchsia = int(0xff00ff);
static const int gcGainsboro = int(0xdcdcdc);
static const int gcGhostwhite = int(0xfff8f8);
static const System::Word gcGold = System::Word(0xd7ff);
static const int gcGoldenrod = int(0x20a5da);
static const int gcGray = int(0x808080);
static const System::Word gcGreen = System::Word(0x8000);
static const int gcGreenyellow = int(0x2fffad);
static const int gcGrey = int(0x808080);
static const int gcHoneydew = int(0xf0fff0);
static const int gcHotpink = int(0xb469ff);
static const int gcIndianred = int(0x5c5ccd);
static const int gcIndigo = int(0x82004b);
static const int gcIvory = int(0xf0ffff);
static const int gcKhaki = int(0x8ce6f0);
static const int gcLavender = int(0xfae6e6);
static const int gcLavenderblush = int(0xf5f0ff);
static const System::Word gcLawngreen = System::Word(0xfc7c);
static const int gcLemonchiffon = int(0xcdfaff);
static const int gcLightblue = int(0xe6d8ad);
static const int gcLightcoral = int(0x8080f0);
static const int gcLightcyan = int(0xffffe0);
static const int gcLightgoldenrodyellow = int(0xd2fafa);
static const int gcLightgray = int(0xd3d3d3);
static const int gcLightgreen = int(0x90ee90);
static const int gcLightgrey = int(0xd3d3d3);
static const int gcLightpink = int(0xc1b6ff);
static const int gcLightsalmon = int(0x7aa0ff);
static const int gcLightseagreen = int(0xaab220);
static const int gcLightskyblue = int(0xface87);
static const int gcLightslategray = int(0x998877);
static const int gcLightslategrey = int(0x998877);
static const int gcLightsteelblue = int(0xdec4b0);
static const int gcLightyellow = int(0xe0ffff);
static const int gcLtGray = int(0xc0c0c0);
static const int gcMedGray = int(0xa4a0a0);
static const int gcDkGray = int(0x808080);
static const int gcMoneyGreen = int(0xc0dcc0);
static const int gcLegacySkyBlue = int(0xf0caa6);
static const int gcCream = int(0xf0fbff);
static const System::Word gcLime = System::Word(0xff00);
static const int gcLimegreen = int(0x32cd32);
static const int gcLinen = int(0xe6f0fa);
static const int gcMagenta = int(0xff00ff);
static const System::Byte gcMaroon = System::Byte(0x80);
static const int gcMediumaquamarine = int(0xaacd66);
static const int gcMediumblue = int(0xcd0000);
static const int gcMediumorchid = int(0xd355ba);
static const int gcMediumpurple = int(0xdb7093);
static const int gcMediumseagreen = int(0x71b33c);
static const int gcMediumslateblue = int(0xee687b);
static const int gcMediumspringgreen = int(0x9afa00);
static const int gcMediumturquoise = int(0xccd148);
static const int gcMediumvioletred = int(0x8515c7);
static const int gcMidnightblue = int(0x701919);
static const int gcMintcream = int(0xfafff5);
static const int gcMistyrose = int(0xe1e4ff);
static const int gcMoccasin = int(0xb5e4ff);
static const int gcNavajowhite = int(0xaddeff);
static const int gcNavy = int(0x800000);
static const int gcOldlace = int(0xe6f5fd);
static const System::Word gcOlive = System::Word(0x8080);
static const int gcOlivedrab = int(0x238e6b);
static const System::Word gcOrange = System::Word(0xa5ff);
static const System::Word gcOrangered = System::Word(0x45ff);
static const int gcOrchid = int(0xd670da);
static const int gcPalegoldenrod = int(0xaae8ee);
static const int gcPalegreen = int(0x98fb98);
static const int gcPaleturquoise = int(0xeeeeaf);
static const int gcPalevioletred = int(0x9370db);
static const int gcPapayawhip = int(0xd5efff);
static const int gcPeachpuff = int(0xb9daff);
static const int gcPeru = int(0x3f85cd);
static const int gcPink = int(0xcbc0ff);
static const int gcPlum = int(0xdda0dd);
static const int gcPowderblue = int(0xe6e0b0);
static const int gcPurple = int(0x800080);
static const System::Byte gcRed = System::Byte(0xff);
static const int gcRosybrown = int(0x8f8fbc);
static const int gcRoyalblue = int(0xe16941);
static const int gcSaddlebrown = int(0x13458b);
static const int gcSalmon = int(0x7280fa);
static const int gcSandybrown = int(0x60a4f4);
static const int gcSeagreen = int(0x578b2e);
static const int gcSeashell = int(0xeef5ff);
static const int gcSienna = int(0x2d52a0);
static const int gcSilver = int(0xc0c0c0);
static const int gcSkyblue = int(0xebce87);
static const int gcSlateblue = int(0xcd5a6a);
static const int gcSlategray = int(0x908070);
static const int gcSlategrey = int(0x908070);
static const int gcSnow = int(0xfafaff);
static const int gcSpringgreen = int(0x7fff00);
static const int gcSteelblue = int(0xb48246);
static const int gcTan = int(0x8cb4d2);
static const int gcTeal = int(0x808000);
static const int gcThistle = int(0xd8bfd8);
static const int gcTomato = int(0x4763ff);
static const int gcTurquoise = int(0xd0e040);
static const int gcViolet = int(0xee82ee);
static const int gcWheat = int(0xb3def5);
static const int gcWhite = int(0xffffff);
static const int gcWhitesmoke = int(0xf5f5f5);
static const System::Word gcYellow = System::Word(0xffff);
static const int gcYellowgreen = int(0x32cd9a);
static const System::Int8 gcNull = System::Int8(-1);
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
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall Blend(System::Uitypes::TColor AColor1, System::Uitypes::TColor AColor2, System::Byte ALevel);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall Lighter(System::Uitypes::TColor AColor, System::Byte APercent);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall Darker(System::Uitypes::TColor AColor, System::Byte APercent);
extern DELPHI_PACKAGE void __fastcall DrawGradient(Vcl::Graphics::TCanvas* ACanvas, System::Uitypes::TColor AColor, System::Uitypes::TColor AColorTo, const System::Types::TRect &ARect, float ARounding, TTMSFNCGraphicsCorners ACorners, bool ADirection);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall MakeGraphicsColor(System::Byte ARed, System::Byte AGreen, System::Byte ABlue);
}	/* namespace Tmsfncgraphicstypes */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCGRAPHICSTYPES)
using namespace Vcl::Tmsfncgraphicstypes;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfncgraphicstypesHPP
