// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCHint.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfnchintHPP
#define Vcl_TmsfnchintHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <Vcl.Controls.hpp>
#include <VCL.TMSFNCCustomComponent.hpp>
#include <System.Types.hpp>
#include <Vcl.Forms.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCBitmapContainer.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfnchint
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCHintWindow;
class DELPHICLASS TTMSFNCCustomHint;
class DELPHICLASS TTMSFNCHint;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTMSFNCHintWindow : public Vcl::Controls::THintWindow
{
	typedef Vcl::Controls::THintWindow inherited;
	
private:
	TTMSFNCCustomHint* FHint;
	
protected:
	TTMSFNCCustomHint* __fastcall FindHintControl();
	virtual System::Types::TRect __fastcall Calculate(System::UnicodeString AHint);
	virtual Vcl::Tmsfnctypes::TTMSFNCBitmap* __fastcall Draw(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &ARect, System::UnicodeString AHint);
	
public:
	__fastcall virtual TTMSFNCHintWindow(System::Classes::TComponent* const AOwner, const bool AShadow);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall Paint();
	virtual System::Types::TRect __fastcall CalcHintRect(int MaxWidth, const System::UnicodeString AHint, void * AData);
	virtual void __fastcall ShowHintAt(TTMSFNCCustomHint* AHintControl, System::UnicodeString AHint, int X, int Y);
	virtual void __fastcall HideHint();
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TTMSFNCHintWindow() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCHintWindow(HWND ParentWindow) : Vcl::Controls::THintWindow(ParentWindow) { }
	
};


typedef void __fastcall (__closure *TTMSFNCHintBeforeDrawHintEvent)(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AHint, const System::Types::TRect &ARect, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCHintAfterDrawHintEvent)(System::TObject* Sender, Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AHint, const System::Types::TRect &ARect);

typedef void __fastcall (__closure *TTMSFNCHintCalculateHintRectEvent)(System::TObject* Sender, System::UnicodeString AHint, System::Types::TRect &ARect);

class PASCALIMPLEMENTATION TTMSFNCCustomHint : public Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Vcl::Controls::THintWindowClass FPrevHintWindowClass;
	Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FFont;
	TTMSFNCHintCalculateHintRectEvent FOnCalculateHintRect;
	TTMSFNCHintBeforeDrawHintEvent FOnBeforeDrawHint;
	TTMSFNCHintAfterDrawHintEvent FOnAfterDrawHint;
	bool FShadow;
	Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* __fastcall GetBitmapContainer();
	void __fastcall SetBitmapContainer(Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value);
	void __fastcall SetFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	HIDESBASE void __fastcall SetFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetShadow(const bool Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetDocURL();
	void __fastcall DoBeforeDrawHint(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AHint, const System::Types::TRect &ARect, bool &ADefaultDraw);
	void __fastcall DoAfterDrawHint(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AHint, const System::Types::TRect &ARect);
	void __fastcall DoCalculateHintRect(System::UnicodeString AHint, System::Types::TRect &ARect);
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__property Vcl::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=GetBitmapContainer, write=SetBitmapContainer};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* Font = {read=FFont, write=SetFont};
	__property bool Shadow = {read=FShadow, write=SetShadow, default=1};
	__property TTMSFNCHintBeforeDrawHintEvent OnBeforeDrawHint = {read=FOnBeforeDrawHint, write=FOnBeforeDrawHint};
	__property TTMSFNCHintAfterDrawHintEvent OnAfterDrawHint = {read=FOnAfterDrawHint, write=FOnAfterDrawHint};
	__property TTMSFNCHintCalculateHintRectEvent OnCalculateHintRect = {read=FOnCalculateHintRect, write=FOnCalculateHintRect};
	
public:
	__fastcall virtual TTMSFNCCustomHint(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual ~TTMSFNCCustomHint();
	virtual System::Types::TRect __fastcall Calculate(System::UnicodeString AHint);
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCCustomHint(HWND ParentWindow) : Vcl::Tmsfnccustomcomponent::TTMSFNCCustomComponent(ParentWindow) { }
	
private:
	void *__ITMSFNCBitmapContainer;	// Vcl::Tmsfncbitmapcontainer::ITMSFNCBitmapContainer 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {ED26710D-395F-4971-8AC9-A31083BF2A3C}
	operator Vcl::Tmsfncbitmapcontainer::_di_ITMSFNCBitmapContainer()
	{
		Vcl::Tmsfncbitmapcontainer::_di_ITMSFNCBitmapContainer intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Vcl::Tmsfncbitmapcontainer::ITMSFNCBitmapContainer*(void) { return (Vcl::Tmsfncbitmapcontainer::ITMSFNCBitmapContainer*)&__ITMSFNCBitmapContainer; }
	#endif
	
};


class PASCALIMPLEMENTATION TTMSFNCHint : public TTMSFNCCustomHint
{
	typedef TTMSFNCCustomHint inherited;
	
private:
	TTMSFNCHintWindow* FHintWindow;
	
protected:
	virtual void __fastcall RegisterRuntimeClasses();
	
public:
	bool __fastcall IsShowing();
	void __fastcall ShowHintAt(System::UnicodeString AHint, int X, int Y);
	void __fastcall HideHint();
	__fastcall virtual ~TTMSFNCHint();
	
__published:
	__property OnCalculateHintRect;
	__property OnBeforeDrawHint;
	__property OnAfterDrawHint;
	__property BitmapContainer;
	__property Fill;
	__property Stroke;
	__property Font;
	__property Shadow = {default=1};
public:
	/* TTMSFNCCustomHint.Create */ inline __fastcall virtual TTMSFNCHint(System::Classes::TComponent* AOwner) : TTMSFNCCustomHint(AOwner) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCHint(HWND ParentWindow) : TTMSFNCCustomHint(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfnchint */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCHINT)
using namespace Vcl::Tmsfnchint;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfnchintHPP
