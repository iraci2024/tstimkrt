// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCHint.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnchintHPP
#define Fmx_TmsfnchintHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.Controls.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <System.Types.hpp>
#include <FMX.Forms.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.Types.hpp>
#include <System.Messaging.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnchint
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCHintWindow;
class DELPHICLASS TTMSFNCCustomHint;
class DELPHICLASS TTMSFNCHint;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTimerMode : unsigned char { tmShow, tmHide };

class PASCALIMPLEMENTATION TTMSFNCHintWindow : public Fmx::Controls::THint
{
	typedef Fmx::Controls::THint inherited;
	
private:
	System::UnicodeString FHintString;
	TTimerMode FTimerMode;
	TTMSFNCCustomHint* FHint;
	
protected:
	TTMSFNCCustomHint* __fastcall FindHintControl();
	virtual System::Types::TRect __fastcall Calculate(System::UnicodeString AHint);
	virtual Fmx::Tmsfnctypes::TTMSFNCBitmap* __fastcall Draw(Fmx::Graphics::TCanvas* ACanvas, const System::Types::TRect &ARect, System::UnicodeString AHint);
	virtual void __fastcall SetEnabled(const bool Value);
	void __fastcall HintTimerProc(System::TObject* Sender);
	void __fastcall StartHintTimer(NativeUInt Value, TTimerMode TimerMode);
	void __fastcall StopHintTimer();
	
public:
	__fastcall virtual TTMSFNCHintWindow(Fmx::Types::TWindowHandle* const AHandle, const bool AShadow);
	__fastcall virtual ~TTMSFNCHintWindow();
	virtual void __fastcall SetHint(const System::UnicodeString AString);
	virtual void __fastcall SetPosition(const float X, const float Y);
	virtual void __fastcall ShowHintAt(TTMSFNCCustomHint* AHintControl, System::UnicodeString AHint, int X, int Y);
	virtual void __fastcall HideHint();
};


typedef void __fastcall (__closure *TTMSFNCHintBeforeDrawHintEvent)(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AHint, const System::Types::TRect &ARect, bool &ADefaultDraw);

typedef void __fastcall (__closure *TTMSFNCHintAfterDrawHintEvent)(System::TObject* Sender, Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AHint, const System::Types::TRect &ARect);

typedef void __fastcall (__closure *TTMSFNCHintCalculateHintRectEvent)(System::TObject* Sender, System::UnicodeString AHint, System::Types::TRect &ARect);

class PASCALIMPLEMENTATION TTMSFNCCustomHint : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FFont;
	TTMSFNCHintCalculateHintRectEvent FOnCalculateHintRect;
	TTMSFNCHintBeforeDrawHintEvent FOnBeforeDrawHint;
	TTMSFNCHintAfterDrawHintEvent FOnAfterDrawHint;
	bool FShadow;
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* __fastcall GetBitmapContainer();
	void __fastcall SetBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value);
	void __fastcall SetFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetShadow(const bool Value);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetDocURL();
	void __fastcall DoBeforeDrawHint(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AHint, const System::Types::TRect &ARect, bool &ADefaultDraw);
	void __fastcall DoAfterDrawHint(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString AHint, const System::Types::TRect &ARect);
	void __fastcall DoCalculateHintRect(System::UnicodeString AHint, System::Types::TRect &ARect);
	void __fastcall CreateFormsHandler(System::TObject* const Sender, System::Messaging::TMessageBase* const M);
	virtual void __fastcall ChangeDPIScale(int M, int D);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=GetBitmapContainer, write=SetBitmapContainer};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* Font = {read=FFont, write=SetFont};
	__property bool Shadow = {read=FShadow, write=SetShadow, default=1};
	__property TTMSFNCHintBeforeDrawHintEvent OnBeforeDrawHint = {read=FOnBeforeDrawHint, write=FOnBeforeDrawHint};
	__property TTMSFNCHintAfterDrawHintEvent OnAfterDrawHint = {read=FOnAfterDrawHint, write=FOnAfterDrawHint};
	__property TTMSFNCHintCalculateHintRectEvent OnCalculateHintRect = {read=FOnCalculateHintRect, write=FOnCalculateHintRect};
	
public:
	__fastcall virtual TTMSFNCCustomHint(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual ~TTMSFNCCustomHint();
	virtual System::Types::TRect __fastcall Calculate(System::UnicodeString AHint);
private:
	void *__ITMSFNCBitmapContainer;	// Fmx::Tmsfncbitmapcontainer::ITMSFNCBitmapContainer 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {ED26710D-395F-4971-8AC9-A31083BF2A3C}
	operator Fmx::Tmsfncbitmapcontainer::_di_ITMSFNCBitmapContainer()
	{
		Fmx::Tmsfncbitmapcontainer::_di_ITMSFNCBitmapContainer intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Fmx::Tmsfncbitmapcontainer::ITMSFNCBitmapContainer*(void) { return (Fmx::Tmsfncbitmapcontainer::ITMSFNCBitmapContainer*)&__ITMSFNCBitmapContainer; }
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
	
};


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Word HintDelay = System::Word(0x1f4);
static constexpr System::Word HintHideDelay = System::Word(0x9c4);
static constexpr System::Int8 HintOffsetX = System::Int8(0x0);
static constexpr System::Int8 HintOffsetY = System::Int8(0x14);
}	/* namespace Tmsfnchint */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCHINT)
using namespace Fmx::Tmsfnchint;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnchintHPP
