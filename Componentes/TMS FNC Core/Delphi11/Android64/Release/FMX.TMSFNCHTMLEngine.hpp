// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCHTMLEngine.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnchtmlengineHPP
#define Fmx_TmsfnchtmlengineHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <System.Types.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnchtmlengine
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::WideChar TMSFNCHTMLENGINE_ATTR_DELIM;
extern DELPHI_PACKAGE System::UnicodeString __fastcall HiLight(System::UnicodeString s, System::UnicodeString h, System::UnicodeString tag, bool DoCase);
extern DELPHI_PACKAGE System::UnicodeString __fastcall UnHiLight(System::UnicodeString s, System::UnicodeString tag);
extern DELPHI_PACKAGE void __fastcall ParseControl(System::UnicodeString Tag, System::UnicodeString &ControlType, System::UnicodeString &ControlID, System::UnicodeString &ControlValue, System::UnicodeString &ControlWidth, System::UnicodeString &ControlHeight, System::UnicodeString &ControlProp, System::UnicodeString &ControlLen);
extern DELPHI_PACKAGE bool __fastcall HasHTMLControl(System::UnicodeString HTML);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetNextControlID(System::UnicodeString HTML, System::UnicodeString ControlID);
extern DELPHI_PACKAGE bool __fastcall GetControlValue(System::UnicodeString HTML, System::UnicodeString ControlID, System::UnicodeString &ControlValue);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetControlProp(System::UnicodeString HTML, System::UnicodeString ControlID);
extern DELPHI_PACKAGE int __fastcall GetControlMaxLen(System::UnicodeString HTML, System::UnicodeString ControlID);
extern DELPHI_PACKAGE bool __fastcall SetControlValue(System::UnicodeString &HTML, System::UnicodeString ControlID, System::UnicodeString ControlValue);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ClearRadioControls(System::UnicodeString HTML);
extern DELPHI_PACKAGE bool __fastcall HTMLDrawEx(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString s, const System::Types::TRectF &fr, int XPos, int YPos, int FocusLink, int HoverLink, int ShadowOffset, bool CheckHotSpot, bool CheckHeight, bool Print, bool Selected, bool Blink, bool HoverStyle, bool WordWrap, bool Down, System::UnicodeString DownID, double ResFactor, System::Uitypes::TAlphaColor URLColor, System::Uitypes::TAlphaColor HoverColor, System::Uitypes::TAlphaColor HoverFontColor, System::Uitypes::TAlphaColor ShadowColor, System::UnicodeString &AnchorVal, System::UnicodeString &StripVal, System::UnicodeString &FocusAnchor, float &XSize, float &YSize, int &HyperLinks, int &MouseLink, System::Types::TRectF &HoverRect, System::Types::TRectF &ControlRect, System::UnicodeString &CID,
	System::UnicodeString &CV, System::UnicodeString &CT, int &LineCount, int LineSpacing, Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer, float Opacity, bool HyperLinkUnderline = true, System::Uitypes::TAlphaColor HighlightColor = (System::Uitypes::TAlphaColor)(0xff0000ff), System::Uitypes::TAlphaColor HighlightTextColor = (System::Uitypes::TAlphaColor)(0xffffffff), System::Uitypes::TFontStyles HighlightTextStyle = System::Uitypes::TFontStyles() )/* overload */;
extern DELPHI_PACKAGE bool __fastcall HTMLDrawEx(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, System::UnicodeString s, const System::Types::TRectF &fr, int XPos, int YPos, int FocusLink, int HoverLink, int ShadowOffset, bool CheckHotSpot, bool CheckHeight, bool Print, bool Selected, bool Blink, bool HoverStyle, bool WordWrap, double ResFactor, System::Uitypes::TAlphaColor URLColor, System::Uitypes::TAlphaColor HoverColor, System::Uitypes::TAlphaColor HoverFontColor, System::Uitypes::TAlphaColor ShadowColor, System::UnicodeString &AnchorVal, System::UnicodeString &StripVal, System::UnicodeString &FocusAnchor, float &XSize, float &YSize, int &HyperLinks, int &MouseLink, System::Types::TRectF &HoverRect, int &LineCount, int LineSpacing, Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer, float Opacity,
	bool HyperLinkUnderline = true, System::Uitypes::TAlphaColor HighlightColor = (System::Uitypes::TAlphaColor)(0xff0000ff), System::Uitypes::TAlphaColor HighlightTextColor = (System::Uitypes::TAlphaColor)(0xffffffff), System::Uitypes::TFontStyles HighlightTextStyle = System::Uitypes::TFontStyles() )/* overload */;
}	/* namespace Tmsfnchtmlengine */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCHTMLENGINE)
using namespace Fmx::Tmsfnchtmlengine;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnchtmlengineHPP
