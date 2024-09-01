// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCCustomControl.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnccustomcontrolHPP
#define Fmx_TmsfnccustomcontrolHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCStyles.hpp>
#include <FMX.TMSFNCPersistence.hpp>
#include <FMX.TMSFNCUndo.hpp>
#include <FMX.TMSFNCHint.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <System.TypInfo.hpp>
#include <FMX.Forms.hpp>
#include <System.UITypes.hpp>
#include <System.Generics.Collections.hpp>
#include <FMX.Types.hpp>
#include <System.Messaging.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnccustomcontrol
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE ITMSFNCShortCutHint;
typedef System::DelphiInterface<ITMSFNCShortCutHint> _di_ITMSFNCShortCutHint;
class DELPHICLASS FNCJSLibReferenceAttribute;
class DELPHICLASS TTMSFNCCustomDesignerForm;
class DELPHICLASS TTMSFNCCustomControlBaseCommon;
class DELPHICLASS TTMSFNCCustomControlBase;
class DELPHICLASS TTMSFNCCustomControl;
class DELPHICLASS TTMSFNCControl;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{A3E21A73-163A-4617-841B-8E0A62BF41B0}") ITMSFNCShortCutHint  : public System::IInterface 
{
	virtual bool __fastcall HasShortCut(System::UnicodeString AShortCut) = 0 ;
	virtual bool __fastcall HandleShortCut(System::UnicodeString AShortCut) = 0 ;
	virtual System::UnicodeString __fastcall GetShortCutHint() = 0 ;
	virtual bool __fastcall IsShortCutHintActive() = 0 ;
	virtual void __fastcall ShowShortCutHint() = 0 ;
	virtual void __fastcall CancelShortCutHint(bool AClearShortCutHintString = true) = 0 ;
	virtual void __fastcall SetShortCutHint(const System::UnicodeString Value) = 0 ;
	__property System::UnicodeString ShortCutHint = {read=GetShortCutHint, write=SetShortCutHint};
};

class PASCALIMPLEMENTATION FNCJSLibReferenceAttribute : public System::TCustomAttribute
{
	typedef System::TCustomAttribute inherited;
	
private:
	System::UnicodeString FAttrs;
	System::UnicodeString FCSSs;
	System::UnicodeString FDesc;
	System::UnicodeString FSrcs;
	
public:
	__fastcall FNCJSLibReferenceAttribute(const System::UnicodeString AScrs)/* overload */;
	__fastcall FNCJSLibReferenceAttribute(const System::UnicodeString AScrs, const System::UnicodeString ACSSs)/* overload */;
	__fastcall FNCJSLibReferenceAttribute(const System::UnicodeString AScrs, const System::UnicodeString ACSSs, const System::UnicodeString AAttrs)/* overload */;
	__fastcall FNCJSLibReferenceAttribute(const System::UnicodeString AScrs, const System::UnicodeString ACSSs, const System::UnicodeString AAttrs, const System::UnicodeString ADesc)/* overload */;
	__property System::UnicodeString Attrs = {read=FAttrs};
	__property System::UnicodeString CSSs = {read=FCSSs};
	__property System::UnicodeString Desc = {read=FDesc};
	__property System::UnicodeString Srcs = {read=FSrcs};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~FNCJSLibReferenceAttribute() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCCustomDesignerForm : public Fmx::Forms::TForm
{
	typedef Fmx::Forms::TForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCCustomDesignerForm(System::Classes::TComponent* AOwner) : Fmx::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCCustomDesignerForm(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCCustomDesignerForm() { }
	
};


enum DECLSPEC_DENUM TTMSFNCControlAlignment : unsigned char { caNone, caTop, caBottom, caLeft, caRight, caClient };

typedef System::Generics::Collections::TObjectList__1<Fmx::Tmsfnchint::TTMSFNCHint*>* TTMSFNCCustomControlShortCutWindowList;

class PASCALIMPLEMENTATION TTMSFNCCustomControlBaseCommon : public Fmx::Controls::TControl
{
	typedef Fmx::Controls::TControl inherited;
	
private:
	float FResourceScaleFactor;
	float FPaintScaleFactor;
	int FDesigntimeFormPixelsPerInch;
	System::UnicodeString FShortCutHint;
	System::UnicodeString FShortCutLimitation;
	System::Generics::Collections::TObjectList__1<Fmx::Tmsfnchint::TTMSFNCHint*>* FShortCutWindowList;
	bool FAllowGetChildren;
	bool FAdaptToStyle;
	bool FAdaptedToStyle;
	bool FTransparent;
	bool FBufferedPainting;
	bool FNativeCanvas;
	bool FAntiAliasing;
	bool FOptimizedHTMLDrawing;
	bool FShowAcceleratorChar;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextQuality FTextQuality;
	System::Classes::TNotifyEvent FOnInternalDblClick;
	System::Classes::TNotifyEvent FOnInternalMouseDown;
	System::Classes::TNotifyEvent FOnInternalMouseMove;
	System::Classes::TNotifyEvent FOnInternalMouseUp;
	bool __fastcall GetAllowFocus();
	System::UnicodeString __fastcall GetShortCutHint();
	TTMSFNCControlAlignment __fastcall GetControlAlignment();
	void __fastcall SetAllowFocus(const bool Value);
	void __fastcall SetControlAlignment(const TTMSFNCControlAlignment Value);
	void __fastcall SetAntiAliasing(const bool Value);
	void __fastcall SetOptimizedHTMLDrawing(const bool Value);
	void __fastcall SetShowAcceleratorChar(const bool Value);
	void __fastcall SetNativeCanvas(const bool Value);
	void __fastcall SetTextQuality(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextQuality Value);
	void __fastcall SetShortCutHint(const System::UnicodeString Value);
	
protected:
	virtual System::UnicodeString __fastcall GetVersion();
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual System::UnicodeString __fastcall GetTipsURL();
	virtual bool __fastcall HandleDesignHitTest(const System::Types::TPoint &APoint);
	virtual System::Types::TPointF __fastcall GetClientMousePos();
	virtual System::Types::TPointF __fastcall ConvertScreenToClient(const System::Types::TPointF &APoint);
	virtual System::Types::TPointF __fastcall ConvertClientToScreen(const System::Types::TPointF &APoint);
	virtual Fmx::Tmsfnctypes::TTMSFNCBitmap* __fastcall GetDragObjectScreenShot();
	virtual bool __fastcall GetAdaptToStyle();
	virtual void __fastcall SetEnabled(const bool Value);
	virtual bool __fastcall HasShortCut(System::UnicodeString AShortCut);
	virtual bool __fastcall HandleShortCut(System::UnicodeString AShortCut);
	virtual bool __fastcall ExecuteShortCutMethod(System::UnicodeString AShortCut);
	virtual bool __fastcall IsShortCutHintActive();
	virtual void __fastcall SetParent(Fmx::Types::TFmxObject* const Value);
	virtual void __fastcall Loaded();
	void __fastcall CalcFactor();
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall DestroyGraphicElements() = 0 ;
	virtual void __fastcall SetDefaultGraphicColors();
	virtual void __fastcall SetAdaptToStyle(const bool Value);
	virtual void __fastcall CancelHint();
	virtual void __fastcall BeginDrag();
	void __fastcall InitializeStyle();
	virtual void __fastcall ApplyStyle();
	virtual void __fastcall ResetToDefaultStyle();
	virtual void __fastcall HandleMouseLeave();
	virtual void __fastcall HandleMouseEnter();
	virtual void __fastcall HandleMouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleDblClick(float X, float Y);
	virtual void __fastcall HandleMouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleKeyPress(System::WideChar &Key);
	virtual void __fastcall HandleKeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall HandleDialogKey(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall HandleKeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall HandleMouseWheel(System::Classes::TShiftState Shift, int WheelDelta, bool &Handled);
	virtual void __fastcall HandleDragOver(System::TObject* const Source, const System::Types::TPointF &Point, bool &Accept);
	virtual void __fastcall HandleDragDrop(System::TObject* const Source, const System::Types::TPointF &Point);
	virtual void __fastcall GetShortCutHints(System::Classes::TStringList* AShortCutHints);
	virtual void __fastcall ShowShortCutHint();
	virtual void __fastcall CustomizeShortCut(Fmx::Tmsfnchint::TTMSFNCHint* AShortCutWindow, System::UnicodeString AShortCut, const System::Types::TRectF &AShortCutRect, System::Types::TPointF &AShortCutPosition);
	virtual void __fastcall CancelShortCutHint(bool AClearShortCutHintString = true);
	__property bool AdaptToStyle = {read=GetAdaptToStyle, write=SetAdaptToStyle, default=0};
	__property bool Transparent = {read=FTransparent, write=FTransparent, default=0};
	__property bool BufferedPainting = {read=FBufferedPainting, write=FBufferedPainting, default=0};
	__property bool NativeCanvas = {read=FNativeCanvas, write=SetNativeCanvas, default=0};
	__property bool AntiAliasing = {read=FAntiAliasing, write=SetAntiAliasing, default=1};
	__property bool OptimizedHTMLDrawing = {read=FOptimizedHTMLDrawing, write=SetOptimizedHTMLDrawing, default=1};
	__property bool ShowAcceleratorChar = {read=FShowAcceleratorChar, write=SetShowAcceleratorChar, default=1};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsTextQuality TextQuality = {read=FTextQuality, write=SetTextQuality, default=1};
	__property System::Classes::TNotifyEvent OnInternalMouseDown = {read=FOnInternalMouseDown, write=FOnInternalMouseDown};
	__property System::Classes::TNotifyEvent OnInternalMouseUp = {read=FOnInternalMouseUp, write=FOnInternalMouseUp};
	__property System::Classes::TNotifyEvent OnInternalMouseMove = {read=FOnInternalMouseMove, write=FOnInternalMouseMove};
	__property System::Classes::TNotifyEvent OnInternalDblClick = {read=FOnInternalDblClick, write=FOnInternalDblClick};
	__property System::UnicodeString ShortCutHint = {read=GetShortCutHint, write=SetShortCutHint};
	__property bool AllowGetChildren = {read=FAllowGetChildren, write=FAllowGetChildren, default=0};
	
public:
	__fastcall virtual TTMSFNCCustomControlBaseCommon(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomControlBaseCommon();
	__property bool AllowFocus = {read=GetAllowFocus, write=SetAllowFocus, default=1};
	__property Canvas;
	__property TTMSFNCControlAlignment ControlAlignment = {read=GetControlAlignment, write=SetControlAlignment, default=0};
	void __fastcall SetControlMargins(float ALeft, float ATop, float ARight, float ABottom);
	void __fastcall GetControlMargins(float &ALeft, float &ATop, float &ARight, float &ABottom);
	int __fastcall ScaleResourceValue(const int Value)/* overload */;
	double __fastcall ScaleResourceValue(const double Value)/* overload */;
	int __fastcall ScalePaintValue(const int Value)/* overload */;
	double __fastcall ScalePaintValue(const double Value)/* overload */;
	__property float ResourceScaleFactor = {read=FResourceScaleFactor};
	__property float PaintScaleFactor = {read=FPaintScaleFactor};
	__property int DesigntimeFormPixelsPerInch = {read=FDesigntimeFormPixelsPerInch, nodefault};
	virtual void __fastcall SetBounds(float X, float Y, float AWidth, float AHeight);
private:
	void *__ITMSFNCShortCutHint;	// ITMSFNCShortCutHint 
	void *__ITMSFNCProductInfo;	// Fmx::Tmsfnctypes::ITMSFNCProductInfo 
	void *__ITMSFNCAdaptToStyle;	// Fmx::Tmsfncstyles::ITMSFNCAdaptToStyle 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {A3E21A73-163A-4617-841B-8E0A62BF41B0}
	operator _di_ITMSFNCShortCutHint()
	{
		_di_ITMSFNCShortCutHint intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ITMSFNCShortCutHint*(void) { return (ITMSFNCShortCutHint*)&__ITMSFNCShortCutHint; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {C53329EA-7B3A-4507-AD9E-88ACD6A85054}
	operator Fmx::Tmsfnctypes::_di_ITMSFNCProductInfo()
	{
		Fmx::Tmsfnctypes::_di_ITMSFNCProductInfo intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfnctypes::ITMSFNCProductInfo*(void) { return (Fmx::Tmsfnctypes::ITMSFNCProductInfo*)&__ITMSFNCProductInfo; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {3EFF288D-3927-4E86-8E9D-EF684B501C9E}
	operator Fmx::Tmsfncstyles::_di_ITMSFNCAdaptToStyle()
	{
		Fmx::Tmsfncstyles::_di_ITMSFNCAdaptToStyle intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfncstyles::ITMSFNCAdaptToStyle*(void) { return (Fmx::Tmsfncstyles::ITMSFNCAdaptToStyle*)&__ITMSFNCAdaptToStyle; }
	#endif
	
};


class PASCALIMPLEMENTATION TTMSFNCCustomControlBase : public TTMSFNCCustomControlBaseCommon
{
	typedef TTMSFNCCustomControlBaseCommon inherited;
	
private:
	int FStyleChangedId;
	float __fastcall GetL();
	float __fastcall GetT();
	HIDESBASE void __fastcall SetTop(const float Value);
	HIDESBASE void __fastcall SetLeft(const float Value);
	
protected:
	virtual void __fastcall StyleChangedHandler(System::TObject* const Sender, System::Messaging::TMessageBase* const Msg);
	virtual void __fastcall DoMouseEnter();
	virtual void __fastcall DoMouseLeave();
	virtual void __fastcall DragOver(const Fmx::Types::TDragObject &Data, const System::Types::TPointF &Point, Fmx::Types::TDragOperation &Operation);
	virtual void __fastcall DragDrop(const Fmx::Types::TDragObject &Data, const System::Types::TPointF &Point);
	virtual void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall KeyDown(System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	virtual void __fastcall KeyUp(System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	virtual void __fastcall DialogKey(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall MouseWheel(System::Classes::TShiftState Shift, int WheelDelta, bool &Handled);
	virtual void __fastcall DblClick();
	
public:
	__property float Left = {read=GetL, write=SetLeft};
	__property float Top = {read=GetT, write=SetTop};
	virtual void __fastcall Invalidate();
	virtual void __fastcall SetNewScene(Fmx::Controls::_di_IScene AScene);
	__fastcall virtual TTMSFNCCustomControlBase(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomControlBase();
};


typedef void __fastcall (__closure *TTMSFNCCustomControlBeforeDrawEvent)(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCCustomControlAfterDrawEvent)(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);

typedef void __fastcall (__closure *TTMSFNCCustomControlCanSavePropertyEvent)(System::TObject* Sender, System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanSave);

typedef void __fastcall (__closure *TTMSFNCCustomControlCanLoadPropertyEvent)(System::TObject* Sender, System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanLoad);

class PASCALIMPLEMENTATION TTMSFNCCustomControl : public TTMSFNCCustomControlBase
{
	typedef TTMSFNCCustomControlBase inherited;
	
private:
	static bool FBlockPersistenceInterface;
	bool FBlockRepaint;
	bool FAppearancePersisting;
	bool FExporting;
	System::Types::TRectF FExportRect;
	TTMSFNCCustomControlBeforeDrawEvent FOnBeforeDraw;
	TTMSFNCCustomControlAfterDrawEvent FOnAfterDraw;
	TTMSFNCCustomControlCanSavePropertyEvent FOnCanSaveProperty;
	TTMSFNCCustomControlCanLoadPropertyEvent FOnCanLoadProperty;
	System::Uitypes::TAlphaColor FColor;
	Fmx::Graphics::TBitmap* FCheckedChk;
	Fmx::Graphics::TBitmap* FUnCheckedChk;
	Fmx::Graphics::TBitmap* FCheckedFocusChk;
	Fmx::Graphics::TBitmap* FUnCheckedFocusChk;
	Fmx::Graphics::TBitmap* FCheckedDisabledChk;
	Fmx::Graphics::TBitmap* FUnCheckedDisabledChk;
	Fmx::Graphics::TBitmap* FCheckedRd;
	Fmx::Graphics::TBitmap* FUnCheckedRd;
	Fmx::Graphics::TBitmap* FCheckedFocusRd;
	Fmx::Graphics::TBitmap* FUnCheckedFocusRd;
	Fmx::Graphics::TBitmap* FCheckedDisabledRd;
	Fmx::Graphics::TBitmap* FUnCheckedDisabledRd;
	Fmx::Graphics::TBitmap* FDownBtn;
	Fmx::Graphics::TBitmap* FNormalBtn;
	Fmx::Graphics::TBitmap* FDownFocusBtn;
	Fmx::Graphics::TBitmap* FNormalFocusBtn;
	Fmx::Graphics::TBitmap* FNormalDisabledBtn;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	void *FDataPointer;
	bool FDataBoolean;
	System::UnicodeString FDataString;
	System::TObject* FDataObject;
	NativeInt FDataInteger;
	Fmx::Tmsfncundo::TTMSFNCUndoManager* FUndoManager;
	Fmx::Tmsfncundo::TTMSFNCUndoManager* __fastcall GetUndoManager();
	void __fastcall SetFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetColor(const System::Uitypes::TAlphaColor Value);
	
protected:
	virtual bool __fastcall IsAppearanceProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType);
	virtual bool __fastcall CanSaveProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType);
	virtual bool __fastcall CanLoadProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType);
	virtual System::UnicodeString __fastcall GetVersion();
	virtual System::Types::TPointF __fastcall LocalToScreenEx(const System::Types::TPointF &APoint);
	virtual System::Types::TPointF __fastcall ScreenToLocalEx(const System::Types::TPointF &APoint);
	virtual System::Types::TRectF __fastcall GetContentRect();
	virtual System::Types::TRectF __fastcall GetControlRect();
	virtual System::Types::TRectF __fastcall GetLocalRect();
	virtual bool __fastcall IsExporting();
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Export(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall BeforeExport();
	virtual void __fastcall AfterExport();
	virtual void __fastcall ApplyStyle();
	virtual void __fastcall ResetToDefaultStyle();
	virtual void __fastcall DestroyGraphicElements();
	virtual void __fastcall TurnOffAnimation(Fmx::Types::TFmxObject* chk);
	virtual void __fastcall UpdateControlAfterResize();
	virtual void __fastcall CreateCheckBoxBitmaps();
	virtual void __fastcall CreateRadioButtonBitmaps();
	virtual void __fastcall CreateButtonBitmaps(float AWidth, float AHeight);
	virtual void __fastcall DoBeforeDraw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect, bool &ADefaultDraw);
	virtual void __fastcall DoAfterDraw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DoCanSaveProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanSave);
	virtual void __fastcall DoCanLoadProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanLoad);
	virtual void __fastcall DrawBackground(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	void __fastcall StrokeChanged(System::TObject* Sender);
	void __fastcall FillChanged(System::TObject* Sender);
	virtual void __fastcall Draw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawControl();
	void __fastcall DoBitmapChanged(System::TObject* Sender);
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	virtual void __fastcall CaptureEx();
	virtual void __fastcall ReleaseCaptureEx();
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TTMSFNCCustomControl(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomControl();
	virtual Fmx::Graphics::TBitmap* __fastcall GetCheckBoxBitmap(bool AChecked = false, bool AFocused = false, bool AEnabled = true);
	virtual Fmx::Graphics::TBitmap* __fastcall GetRadioButtonBitmap(bool AChecked = false, bool AFocused = false, bool AEnabled = true);
	virtual Fmx::Graphics::TBitmap* __fastcall GetButtonBitmap(float AWidth, float AHeight, bool ADown = false, bool AFocused = false, bool AEnabled = true);
	virtual bool __fastcall IsDesignTime();
	virtual bool __fastcall IsLoading();
	virtual bool __fastcall IsReading();
	virtual bool __fastcall IsDesigning();
	virtual bool __fastcall IsDesignerForm();
	virtual bool __fastcall IsDestroying();
	virtual void __fastcall SaveSettingsToFile(System::UnicodeString AFileName, bool AAppearanceOnly = false);
	virtual void __fastcall LoadSettingsFromFile(System::UnicodeString AFileName);
	virtual void __fastcall SaveSettingsToStream(System::Classes::TStream* AStream, bool AAppearanceOnly = false);
	virtual void __fastcall LoadSettingsFromStream(System::Classes::TStream* AStream);
	void __fastcall DisableBackground();
	void __fastcall EnableBackground();
	void __fastcall DisableFill();
	void __fastcall EnableFill();
	void __fastcall DisableStroke();
	void __fastcall EnableStroke();
	virtual void __fastcall Resize();
	virtual void __fastcall Paint();
	__property void * DataPointer = {read=FDataPointer, write=FDataPointer};
	__property bool DataBoolean = {read=FDataBoolean, write=FDataBoolean, nodefault};
	__property System::TObject* DataObject = {read=FDataObject, write=FDataObject};
	__property System::UnicodeString DataString = {read=FDataString, write=FDataString};
	__property NativeInt DataInteger = {read=FDataInteger, write=FDataInteger};
	__property Fmx::Tmsfncundo::TTMSFNCUndoManager* UndoManager = {read=GetUndoManager};
	/* static */ __property bool BlockPersistenceInterface = {read=FBlockPersistenceInterface, write=FBlockPersistenceInterface, nodefault};
	__property bool BlockRepaint = {read=FBlockRepaint, write=FBlockRepaint, nodefault};
	
__published:
	__property AdaptToStyle = {default=0};
	__property AllowFocus = {default=1};
	__property Anchors;
	__property Hint = {default=0};
	__property ShowHint;
	__property System::Uitypes::TAlphaColor Color = {read=FColor, write=SetColor, default=-1};
	__property Align = {default=0};
	__property CanParentFocus = {default=0};
	__property ClipChildren = {default=1};
	__property ClipParent = {default=0};
	__property DisableFocusEffect = {default=1};
	__property DragMode = {default=0};
	__property EnableDragHighlight = {default=1};
	__property Enabled = {default=1};
	__property Height;
	__property HitTest = {default=1};
	__property Locked = {default=0};
	__property Padding;
	__property Opacity;
	__property Margins;
	__property PopupMenu;
	__property Position;
	__property RotationAngle = {default=0};
	__property RotationCenter;
	__property Scale;
	__property Size;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TouchTargetExpansion;
	__property Visible = {default=1};
	__property Width;
	__property OnApplyStyleLookup;
	__property OnDragEnter;
	__property OnDragLeave;
	__property OnDragOver;
	__property OnDragDrop;
	__property OnDragEnd;
	__property OnKeyDown;
	__property OnKeyUp;
	__property OnCanFocus;
	__property OnClick;
	__property OnDblClick;
	__property OnEnter;
	__property OnExit;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property ShowAcceleratorChar = {default=1};
	__property TTMSFNCCustomControlBeforeDrawEvent OnBeforeDraw = {read=FOnBeforeDraw, write=FOnBeforeDraw};
	__property TTMSFNCCustomControlAfterDrawEvent OnAfterDraw = {read=FOnAfterDraw, write=FOnAfterDraw};
	__property TTMSFNCCustomControlCanSavePropertyEvent OnCanSaveProperty = {read=FOnCanSaveProperty, write=FOnCanSaveProperty};
	__property TTMSFNCCustomControlCanLoadPropertyEvent OnCanLoadProperty = {read=FOnCanLoadProperty, write=FOnCanLoadProperty};
private:
	void *__ITMSFNCPersistence;	// Fmx::Tmsfncpersistence::ITMSFNCPersistence 
	void *__ITMSFNCGraphicsExport;	// Fmx::Tmsfncgraphics::ITMSFNCGraphicsExport 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {363F04AF-B8A7-4C47-A2D6-8ED9C44CEFF6}
	operator Fmx::Tmsfncpersistence::_di_ITMSFNCPersistence()
	{
		Fmx::Tmsfncpersistence::_di_ITMSFNCPersistence intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfncpersistence::ITMSFNCPersistence*(void) { return (Fmx::Tmsfncpersistence::ITMSFNCPersistence*)&__ITMSFNCPersistence; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {481CA803-8B50-4545-B212-57AC0D065D09}
	operator Fmx::Tmsfncgraphics::_di_ITMSFNCGraphicsExport()
	{
		Fmx::Tmsfncgraphics::_di_ITMSFNCGraphicsExport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfncgraphics::ITMSFNCGraphicsExport*(void) { return (Fmx::Tmsfncgraphics::ITMSFNCGraphicsExport*)&__ITMSFNCGraphicsExport; }
	#endif
	
};


_DECLARE_METACLASS(System::TMetaClass, TTMSFNCCustomControlClass);

class PASCALIMPLEMENTATION TTMSFNCControl : public TTMSFNCCustomControl
{
	typedef TTMSFNCCustomControl inherited;
	
protected:
	virtual void __fastcall RegisterRuntimeClasses();
public:
	/* TTMSFNCCustomControl.Create */ inline __fastcall virtual TTMSFNCControl(System::Classes::TComponent* AOwner) : TTMSFNCCustomControl(AOwner) { }
	/* TTMSFNCCustomControl.Destroy */ inline __fastcall virtual ~TTMSFNCControl() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 FPC_FULLVERSION = System::Int8(0x0);
}	/* namespace Tmsfnccustomcontrol */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCCUSTOMCONTROL)
using namespace Fmx::Tmsfnccustomcontrol;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnccustomcontrolHPP
