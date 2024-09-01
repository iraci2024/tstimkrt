// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCCustomControl.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfnccustomcontrolHPP
#define Vcl_TmsfnccustomcontrolHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <Vcl.Graphics.hpp>
#include <VCL.TMSFNCStyles.hpp>
#include <VCL.TMSFNCPersistence.hpp>
#include <VCL.TMSFNCUndo.hpp>
#include <VCL.TMSFNCHint.hpp>
#include <System.Types.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <System.TypInfo.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Messages.hpp>
#include <System.UITypes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
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


class PASCALIMPLEMENTATION TTMSFNCCustomDesignerForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCCustomDesignerForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCCustomDesignerForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCCustomDesignerForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomDesignerForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TTMSFNCControlAlignment : unsigned char { caNone, caTop, caBottom, caLeft, caRight, caClient };

typedef System::Generics::Collections::TObjectList__1<Vcl::Tmsfnchint::TTMSFNCHint*>* TTMSFNCCustomControlShortCutWindowList;

class PASCALIMPLEMENTATION TTMSFNCCustomControlBaseCommon : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	float FResourceScaleFactor;
	float FPaintScaleFactor;
	int FDesigntimeFormPixelsPerInch;
	bool FAllowFocus;
	System::UnicodeString FShortCutHint;
	System::UnicodeString FShortCutLimitation;
	System::Generics::Collections::TObjectList__1<Vcl::Tmsfnchint::TTMSFNCHint*>* FShortCutWindowList;
	bool FAllowGetChildren;
	bool FAdaptToStyle;
	bool FAdaptedToStyle;
	bool FTransparent;
	bool FBufferedPainting;
	bool FNativeCanvas;
	bool FAntiAliasing;
	bool FOptimizedHTMLDrawing;
	bool FShowAcceleratorChar;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextQuality FTextQuality;
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
	void __fastcall SetTextQuality(const Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextQuality Value);
	void __fastcall SetShortCutHint(const System::UnicodeString Value);
	
protected:
	virtual System::UnicodeString __fastcall GetVersion();
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual System::UnicodeString __fastcall GetTipsURL();
	virtual bool __fastcall HandleDesignHitTest(const System::Types::TPoint &APoint);
	virtual System::Types::TPointF __fastcall GetClientMousePos();
	virtual System::Types::TPointF __fastcall ConvertScreenToClient(const System::Types::TPointF &APoint);
	virtual System::Types::TPointF __fastcall ConvertClientToScreen(const System::Types::TPointF &APoint);
	virtual Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall GetDragObjectScreenShot();
	virtual bool __fastcall GetAdaptToStyle();
	virtual void __fastcall SetEnabled(bool Value);
	virtual bool __fastcall HasShortCut(System::UnicodeString AShortCut);
	virtual bool __fastcall HandleShortCut(System::UnicodeString AShortCut);
	virtual bool __fastcall ExecuteShortCutMethod(System::UnicodeString AShortCut);
	virtual bool __fastcall IsShortCutHintActive();
	void __fastcall CalcFactor();
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall DestroyGraphicElements() = 0 ;
	virtual void __fastcall SetDefaultGraphicColors();
	virtual void __fastcall SetAdaptToStyle(const bool Value);
	virtual void __fastcall CancelHint();
	HIDESBASE virtual void __fastcall BeginDrag();
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
	virtual void __fastcall CustomizeShortCut(Vcl::Tmsfnchint::TTMSFNCHint* AShortCutWindow, System::UnicodeString AShortCut, const System::Types::TRectF &AShortCutRect, System::Types::TPointF &AShortCutPosition);
	virtual void __fastcall CancelShortCutHint(bool AClearShortCutHintString = true);
	__property bool AdaptToStyle = {read=GetAdaptToStyle, write=SetAdaptToStyle, default=0};
	__property bool Transparent = {read=FTransparent, write=FTransparent, default=0};
	__property bool BufferedPainting = {read=FBufferedPainting, write=FBufferedPainting, default=0};
	__property bool NativeCanvas = {read=FNativeCanvas, write=SetNativeCanvas, default=0};
	__property bool AntiAliasing = {read=FAntiAliasing, write=SetAntiAliasing, default=1};
	__property bool OptimizedHTMLDrawing = {read=FOptimizedHTMLDrawing, write=SetOptimizedHTMLDrawing, default=1};
	__property bool ShowAcceleratorChar = {read=FShowAcceleratorChar, write=SetShowAcceleratorChar, default=1};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsTextQuality TextQuality = {read=FTextQuality, write=SetTextQuality, default=1};
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
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomControlBaseCommon(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
private:
	void *__ITMSFNCShortCutHint;	// ITMSFNCShortCutHint 
	void *__ITMSFNCProductInfo;	// Vcl::Tmsfnctypes::ITMSFNCProductInfo 
	void *__ITMSFNCAdaptToStyle;	// Vcl::Tmsfncstyles::ITMSFNCAdaptToStyle 
	
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
	operator Vcl::Tmsfnctypes::_di_ITMSFNCProductInfo()
	{
		Vcl::Tmsfnctypes::_di_ITMSFNCProductInfo intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Vcl::Tmsfnctypes::ITMSFNCProductInfo*(void) { return (Vcl::Tmsfnctypes::ITMSFNCProductInfo*)&__ITMSFNCProductInfo; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {3EFF288D-3927-4E86-8E9D-EF684B501C9E}
	operator Vcl::Tmsfncstyles::_di_ITMSFNCAdaptToStyle()
	{
		Vcl::Tmsfncstyles::_di_ITMSFNCAdaptToStyle intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Vcl::Tmsfncstyles::ITMSFNCAdaptToStyle*(void) { return (Vcl::Tmsfncstyles::ITMSFNCAdaptToStyle*)&__ITMSFNCAdaptToStyle; }
	#endif
	
};


class PASCALIMPLEMENTATION TTMSFNCCustomControlBase : public TTMSFNCCustomControlBaseCommon
{
	typedef TTMSFNCCustomControlBaseCommon inherited;
	
private:
	bool FStored;
	bool FHitTest;
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Winapi::Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFocusChanged(Vcl::Controls::TCMFocusChanged &Message);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TWMNCHitTest &Message);
	HIDESBASE MESSAGE void __fastcall CMDesignHitTest(Winapi::Messages::TWMMouse &Message);
	MESSAGE void __fastcall CMStyleChanged(Winapi::Messages::TMessage &Message);
	
protected:
	virtual System::Types::TRectF __fastcall GetLocalRect();
	virtual System::UnicodeString __fastcall GetHintString();
	virtual bool __fastcall HasHint();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall DragOver(System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	DYNAMIC void __fastcall KeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall DblClick();
	DYNAMIC void __fastcall ChangeScale(int M, int D, bool isDpiChange)/* overload */;
	
public:
	__fastcall virtual TTMSFNCCustomControlBase(System::Classes::TComponent* AOwner);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	DYNAMIC void __fastcall DragDrop(System::TObject* Source, int X, int Y);
	DYNAMIC bool __fastcall CanFocus();
	__property System::Types::TRectF LocalRect = {read=GetLocalRect};
	__property bool Stored = {read=FStored, write=FStored, default=1};
	__property bool HitTest = {read=FHitTest, write=FHitTest, default=1};
public:
	/* TTMSFNCCustomControlBaseCommon.Destroy */ inline __fastcall virtual ~TTMSFNCCustomControlBase() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomControlBase(HWND ParentWindow) : TTMSFNCCustomControlBaseCommon(ParentWindow) { }
	
	/* Hoisted overloads: */
	
protected:
	DYNAMIC inline void __fastcall  ChangeScale(int M, int D){ Vcl::Controls::TControl::ChangeScale(M, D); }
	
};


typedef void __fastcall (__closure *TTMSFNCCustomControlBeforeDrawEvent)(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCCustomControlAfterDrawEvent)(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);

typedef void __fastcall (__closure *TTMSFNCCustomControlCanSavePropertyEvent)(System::TObject* Sender, System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanSave);

typedef void __fastcall (__closure *TTMSFNCCustomControlCanLoadPropertyEvent)(System::TObject* Sender, System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanLoad);

class PASCALIMPLEMENTATION TTMSFNCCustomControl : public TTMSFNCCustomControlBase
{
	typedef TTMSFNCCustomControlBase inherited;
	
private:
	static bool FBlockPersistenceInterface;
	bool FBlockInvalidate;
	bool FAppearancePersisting;
	bool FExporting;
	System::Types::TRectF FExportRect;
	TTMSFNCCustomControlBeforeDrawEvent FOnBeforeDraw;
	TTMSFNCCustomControlAfterDrawEvent FOnAfterDraw;
	TTMSFNCCustomControlCanSavePropertyEvent FOnCanSaveProperty;
	TTMSFNCCustomControlCanLoadPropertyEvent FOnCanLoadProperty;
	Vcl::Graphics::TPicture* FCheckedChk;
	Vcl::Graphics::TPicture* FUnCheckedChk;
	Vcl::Graphics::TPicture* FCheckedFocusChk;
	Vcl::Graphics::TPicture* FUnCheckedFocusChk;
	Vcl::Graphics::TPicture* FCheckedDisabledChk;
	Vcl::Graphics::TPicture* FUnCheckedDisabledChk;
	Vcl::Graphics::TPicture* FCheckedRd;
	Vcl::Graphics::TPicture* FUnCheckedRd;
	Vcl::Graphics::TPicture* FCheckedFocusRd;
	Vcl::Graphics::TPicture* FUnCheckedFocusRd;
	Vcl::Graphics::TPicture* FCheckedDisabledRd;
	Vcl::Graphics::TPicture* FUnCheckedDisabledRd;
	Vcl::Graphics::TPicture* FDownBtn;
	Vcl::Graphics::TPicture* FNormalBtn;
	Vcl::Graphics::TPicture* FDownFocusBtn;
	Vcl::Graphics::TPicture* FNormalFocusBtn;
	Vcl::Graphics::TPicture* FNormalDisabledBtn;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	void *FDataPointer;
	bool FDataBoolean;
	System::UnicodeString FDataString;
	System::TObject* FDataObject;
	NativeInt FDataInteger;
	Vcl::Tmsfncundo::TTMSFNCUndoManager* FUndoManager;
	Vcl::Tmsfncundo::TTMSFNCUndoManager* __fastcall GetUndoManager();
	void __fastcall SetFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	
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
	virtual System::Uitypes::TColor __fastcall GetTransparentColor();
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Export(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall BeforeExport();
	virtual void __fastcall AfterExport();
	virtual void __fastcall ApplyStyle();
	virtual void __fastcall ResetToDefaultStyle();
	virtual void __fastcall DestroyGraphicElements();
	virtual void __fastcall CreateWnd();
	virtual void __fastcall UpdateControlAfterResize();
	virtual void __fastcall CreateCheckBoxBitmaps();
	virtual void __fastcall CreateRadioButtonBitmaps();
	virtual void __fastcall CreateButtonBitmaps(float AWidth, float AHeight);
	virtual void __fastcall DoBeforeDraw(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect, bool &ADefaultDraw);
	virtual void __fastcall DoAfterDraw(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DoCanSaveProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanSave);
	virtual void __fastcall DoCanLoadProperty(System::TObject* AObject, System::UnicodeString APropertyName, System::TTypeKind APropertyType, bool &ACanLoad);
	virtual void __fastcall DrawBackground(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	void __fastcall StrokeChanged(System::TObject* Sender);
	void __fastcall FillChanged(System::TObject* Sender);
	virtual void __fastcall Draw(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawControl();
	void __fastcall DoBitmapChanged(System::TObject* Sender);
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	virtual void __fastcall CaptureEx();
	virtual void __fastcall ReleaseCaptureEx();
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TTMSFNCCustomControl(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCCustomControl();
	virtual Vcl::Graphics::TPicture* __fastcall GetCheckBoxBitmap(bool AChecked = false, bool AFocused = false, bool AEnabled = true);
	virtual Vcl::Graphics::TPicture* __fastcall GetRadioButtonBitmap(bool AChecked = false, bool AFocused = false, bool AEnabled = true);
	virtual Vcl::Graphics::TPicture* __fastcall GetButtonBitmap(float AWidth, float AHeight, bool ADown = false, bool AFocused = false, bool AEnabled = true);
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
	DYNAMIC void __fastcall Resize();
	virtual void __fastcall Paint();
	__property void * DataPointer = {read=FDataPointer, write=FDataPointer};
	__property bool DataBoolean = {read=FDataBoolean, write=FDataBoolean, nodefault};
	__property System::TObject* DataObject = {read=FDataObject, write=FDataObject};
	__property System::UnicodeString DataString = {read=FDataString, write=FDataString};
	__property NativeInt DataInteger = {read=FDataInteger, write=FDataInteger};
	__property Vcl::Tmsfncundo::TTMSFNCUndoManager* UndoManager = {read=GetUndoManager};
	/* static */ __property bool BlockPersistenceInterface = {read=FBlockPersistenceInterface, write=FBlockPersistenceInterface, nodefault};
	__property bool BlockInvalidate = {read=FBlockInvalidate, write=FBlockInvalidate, nodefault};
	
__published:
	__property AdaptToStyle = {default=0};
	__property AllowFocus = {default=1};
	__property Align = {default=0};
	__property BevelEdges = {default=15};
	__property BevelInner = {index=0, default=2};
	__property BevelKind = {default=0};
	__property BevelOuter = {index=1, default=1};
	__property BevelWidth = {default=1};
	__property Ctl3D;
	__property ParentCtl3D = {default=1};
	__property ParentDoubleBuffered = {default=1};
	__property StyleElements = {default=7};
	__property Touch;
	__property OnGesture;
	__property OnMouseActivate;
	__property Anchors = {default=3};
	__property BiDiMode;
	__property Color = {default=16777215};
	__property Constraints;
	__property DoubleBuffered;
	__property DragCursor = {default=-12};
	__property DragKind = {default=0};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property TabStop = {default=1};
	__property ParentBiDiMode = {default=1};
	__property ParentColor = {default=0};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Hint = {default=0};
	__property TabOrder = {default=-1};
	__property Visible = {default=1};
	__property OnClick;
	__property OnContextPopup;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDock;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnStartDock;
	__property OnStartDrag;
	__property ShowAcceleratorChar = {default=1};
	__property TTMSFNCCustomControlBeforeDrawEvent OnBeforeDraw = {read=FOnBeforeDraw, write=FOnBeforeDraw};
	__property TTMSFNCCustomControlAfterDrawEvent OnAfterDraw = {read=FOnAfterDraw, write=FOnAfterDraw};
	__property TTMSFNCCustomControlCanSavePropertyEvent OnCanSaveProperty = {read=FOnCanSaveProperty, write=FOnCanSaveProperty};
	__property TTMSFNCCustomControlCanLoadPropertyEvent OnCanLoadProperty = {read=FOnCanLoadProperty, write=FOnCanLoadProperty};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomControl(HWND ParentWindow) : TTMSFNCCustomControlBase(ParentWindow) { }
	
private:
	void *__ITMSFNCPersistence;	// Vcl::Tmsfncpersistence::ITMSFNCPersistence 
	void *__ITMSFNCGraphicsExport;	// Vcl::Tmsfncgraphics::ITMSFNCGraphicsExport 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {363F04AF-B8A7-4C47-A2D6-8ED9C44CEFF6}
	operator Vcl::Tmsfncpersistence::_di_ITMSFNCPersistence()
	{
		Vcl::Tmsfncpersistence::_di_ITMSFNCPersistence intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Vcl::Tmsfncpersistence::ITMSFNCPersistence*(void) { return (Vcl::Tmsfncpersistence::ITMSFNCPersistence*)&__ITMSFNCPersistence; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {481CA803-8B50-4545-B212-57AC0D065D09}
	operator Vcl::Tmsfncgraphics::_di_ITMSFNCGraphicsExport()
	{
		Vcl::Tmsfncgraphics::_di_ITMSFNCGraphicsExport intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Vcl::Tmsfncgraphics::ITMSFNCGraphicsExport*(void) { return (Vcl::Tmsfncgraphics::ITMSFNCGraphicsExport*)&__ITMSFNCGraphicsExport; }
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
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCControl(HWND ParentWindow) : TTMSFNCCustomControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 FPC_FULLVERSION = System::Int8(0x0);
}	/* namespace Tmsfnccustomcontrol */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCCUSTOMCONTROL)
using namespace Vcl::Tmsfnccustomcontrol;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfnccustomcontrolHPP
