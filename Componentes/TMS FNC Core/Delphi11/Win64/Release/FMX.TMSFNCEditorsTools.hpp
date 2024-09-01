// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCEditorsTools.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfnceditorstoolsHPP
#define Fmx_TmsfnceditorstoolsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCEditorButton.hpp>
#include <FMX.TMSFNCEditorListView.hpp>
#include <FMX.TMSFNCEditorPanel.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Memo.hpp>
#include <System.UITypes.hpp>
#include <System.Generics.Collections.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfnceditorstools
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCEditorDesignerTheme : unsigned char { edtDefault, edtLight, edtDark };

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x0);
static const System::Int8 BLD_VER = System::Int8(0x0);
static const System::Int8 EDITORBUTTONROUNDING = System::Int8(0x5);
static const unsigned EDITORPRIMCOLOR = unsigned(0xff0077be);
static const unsigned EDITORCOUNTERCOLOR = unsigned(0xffbe7700);
static const unsigned EDITOROKFONTCOLOR = unsigned(0xffffffff);
static const unsigned EDITORSTROKECOLORDARK = unsigned(0xff4f5e6b);
static const unsigned EDITORSTROKECOLORLIGHT = unsigned(0xffcedeec);
static const System::Int8 EDITORAUTOCOLOR = System::Int8(0x0);
static const unsigned EDITORFONTCOLORLIGHT = unsigned(0xff555555);
static const unsigned EDITORFONTCOLORDARK = unsigned(0xffaaaaaa);
static const unsigned EDITORFONTCOLORDISABLED = unsigned(0xff787878);
static const unsigned EDITORMAINBACKCOLORLIGHT = unsigned(0xffffffff);
static const unsigned EDITORMAINBACKCOLORDARK = unsigned(0xff2a2a2c);
static const unsigned EDITORSUBBACKCOLORLIGHT = unsigned(0xfff5f5f5);
static const unsigned EDITORSUBBACKCOLORDARK = unsigned(0xff3e3e43);
extern DELPHI_PACKAGE TTMSFNCEditorDesignerTheme TMSFNCDesignerTheme;
extern DELPHI_PACKAGE void __fastcall SetTMSFNCDesignerTheme(TTMSFNCEditorDesignerTheme ATheme);
extern DELPHI_PACKAGE TTMSFNCEditorDesignerTheme __fastcall GetTMSFNCDesignerTheme(void);
extern DELPHI_PACKAGE bool __fastcall IsLightTheme(void);
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall EditorDarkerShadeColor(System::Uitypes::TAlphaColor AColor, double AFactor = 1.000000E+01, int ATimes = 0x1);
extern DELPHI_PACKAGE System::Uitypes::TAlphaColor __fastcall EditorLighterShadeColor(System::Uitypes::TAlphaColor AColor, double AFactor = 1.000000E+01, int ATimes = 0x1);
extern DELPHI_PACKAGE void __fastcall SetEditorOKButtonAppearance(Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton, System::Uitypes::TAlphaColor APrimaryColor = (System::Uitypes::TAlphaColor)(0xff0077be), int ARounding = 0x5, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0xffffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorCancelButtonAppearance(Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton, System::Uitypes::TAlphaColor APrimaryColor = (System::Uitypes::TAlphaColor)(0xff0077be), int ARounding = 0x5, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor ABackGroundColor = (System::Uitypes::TAlphaColor)(0x0));
extern DELPHI_PACKAGE void __fastcall SetEditorBitmapListAppearance(Fmx::Tmsfnceditorlistview::TTMSFNCBitmapEditorListView* ABitmapListView, System::Uitypes::TAlphaColor APrimaryColor = (System::Uitypes::TAlphaColor)(0xff0077be), int ARounding = 0x5, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor ABackGroundColor = (System::Uitypes::TAlphaColor)(0x0));
extern DELPHI_PACKAGE void __fastcall SetEditorListAppearance(Fmx::Tmsfnceditorlistview::TTMSFNCEditorList* AEditorList, System::Uitypes::TAlphaColor APrimaryColor = (System::Uitypes::TAlphaColor)(0xff0077be), int ARounding = 0x5, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor ABackGroundColor = (System::Uitypes::TAlphaColor)(0x0));
extern DELPHI_PACKAGE void __fastcall SetEditorIconButtonAppearance(Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton, System::Uitypes::TAlphaColor APrimaryColor = (System::Uitypes::TAlphaColor)(0xff0077be), int ARounding = 0x5, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor ABackGroundColor = (System::Uitypes::TAlphaColor)(0x0));
extern DELPHI_PACKAGE void __fastcall SetEditorTabButtonAppearance(Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton, System::Uitypes::TAlphaColor APrimaryColor = (System::Uitypes::TAlphaColor)(0xff0077be), int ARounding = 0x5, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor ABackGroundColor = (System::Uitypes::TAlphaColor)(0x0));
extern DELPHI_PACKAGE void __fastcall SetEditorSubTabButtonAppearance(Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton, System::Uitypes::TAlphaColor APrimaryColor = (System::Uitypes::TAlphaColor)(0xff0077be), int ARounding = 0x5, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor ABackGroundColor = (System::Uitypes::TAlphaColor)(0x0));
extern DELPHI_PACKAGE void __fastcall SetEditorBackPanelAppearance(Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* APanel, System::Uitypes::TAlphaColor ABackGroundColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor AStrokeColor = (System::Uitypes::TAlphaColor)(0x0), int ARounding = 0x5);
extern DELPHI_PACKAGE void __fastcall SetEditorSubPanelAppearance(Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* APanel, System::Uitypes::TAlphaColor ABackGroundColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor AStrokeColor = (System::Uitypes::TAlphaColor)(0x0), int ARounding = 0x0);
extern DELPHI_PACKAGE void __fastcall SetEditorLabelAppearance(Fmx::Stdctrls::TLabel* ALabel, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0x0));
extern DELPHI_PACKAGE void __fastcall SetEditorCheckBoxAppearance(Fmx::Stdctrls::TCheckBox* ACheckBox, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0x0));
extern DELPHI_PACKAGE void __fastcall SetEditorEditAppearance(Fmx::Edit::TEdit* AEdit, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor ABackGroundColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor AStrokeColor = (System::Uitypes::TAlphaColor)(0x0));
extern DELPHI_PACKAGE void __fastcall SetEditorMemoAppearance(Fmx::Memo::TMemo* AMemo, System::Uitypes::TAlphaColor AFontColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor ABackGroundColor = (System::Uitypes::TAlphaColor)(0x0), System::Uitypes::TAlphaColor AStrokeColor = (System::Uitypes::TAlphaColor)(0x0));
}	/* namespace Tmsfnceditorstools */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCEDITORSTOOLS)
using namespace Fmx::Tmsfnceditorstools;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnceditorstoolsHPP
