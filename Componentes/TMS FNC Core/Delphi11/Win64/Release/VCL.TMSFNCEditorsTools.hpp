// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCEditorsTools.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfnceditorstoolsHPP
#define Vcl_TmsfnceditorstoolsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <VCL.TMSFNCTypes.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <Vcl.Graphics.hpp>
#include <VCL.TMSFNCEditorButton.hpp>
#include <VCL.TMSFNCEditorListView.hpp>
#include <VCL.TMSFNCEditorPanel.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <System.UITypes.hpp>
#include <System.Generics.Collections.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
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
static const int EDITORPRIMCOLOR = int(0xbe7700);
static const int EDITORSTROKECOLORLIGHT = int(0xecdece);
static const int EDITORSTROKECOLORDARK = int(0x6b5e4f);
static const System::Word EDITORCOUNTERCOLOR = System::Word(0x77be);
static const int EDITOROKFONTCOLOR = int(0xffffff);
static const System::Int8 EDITORAUTOCOLOR = System::Int8(-1);
static const int EDITORFONTCOLORLIGHT = int(0x555555);
static const int EDITORFONTCOLORDARK = int(0xaaaaaa);
static const int EDITORFONTCOLORDISABLED = int(0x787878);
static const int EDITORMAINBACKCOLORLIGHT = int(0xffffff);
static const int EDITORMAINBACKCOLORDARK = int(0x2c2a2a);
static const int EDITORSUBBACKCOLORLIGHT = int(0xf5f5f5);
static const int EDITORSUBBACKCOLORDARK = int(0x433e3e);
extern DELPHI_PACKAGE TTMSFNCEditorDesignerTheme TMSFNCDesignerTheme;
extern DELPHI_PACKAGE void __fastcall SetTMSFNCDesignerTheme(TTMSFNCEditorDesignerTheme ATheme);
extern DELPHI_PACKAGE TTMSFNCEditorDesignerTheme __fastcall GetTMSFNCDesignerTheme(void);
extern DELPHI_PACKAGE bool __fastcall IsLightTheme(void);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall EditorDarkerShadeColor(System::Uitypes::TColor AColor, double AFactor = 1.000000E+01, int ATimes = 0x1);
extern DELPHI_PACKAGE System::Uitypes::TColor __fastcall EditorLighterShadeColor(System::Uitypes::TColor AColor, double AFactor = 1.000000E+01, int ATimes = 0x1);
extern DELPHI_PACKAGE void __fastcall SetEditorOKButtonAppearance(Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton, System::Uitypes::TColor APrimaryColor = (System::Uitypes::TColor)(0xbe7700), int ARounding = 0x5, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorCancelButtonAppearance(Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton, System::Uitypes::TColor APrimaryColor = (System::Uitypes::TColor)(0xbe7700), int ARounding = 0x5, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor ABackGroundColor = (System::Uitypes::TColor)(0xffffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorBitmapListAppearance(Vcl::Tmsfnceditorlistview::TTMSFNCBitmapEditorListView* ABitmapListView, System::Uitypes::TColor APrimaryColor = (System::Uitypes::TColor)(0xbe7700), int ARounding = 0x5, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor ABackGroundColor = (System::Uitypes::TColor)(0xffffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorListAppearance(Vcl::Tmsfnceditorlistview::TTMSFNCEditorList* AEditorList, System::Uitypes::TColor APrimaryColor = (System::Uitypes::TColor)(0xbe7700), int ARounding = 0x5, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor ABackGroundColor = (System::Uitypes::TColor)(0xffffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorIconButtonAppearance(Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton, System::Uitypes::TColor APrimaryColor = (System::Uitypes::TColor)(0xbe7700), int ARounding = 0x5, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor ABackGroundColor = (System::Uitypes::TColor)(0xffffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorTabButtonAppearance(Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton, System::Uitypes::TColor APrimaryColor = (System::Uitypes::TColor)(0xbe7700), int ARounding = 0x5, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor ABackGroundColor = (System::Uitypes::TColor)(0xffffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorSubTabButtonAppearance(Vcl::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton, System::Uitypes::TColor APrimaryColor = (System::Uitypes::TColor)(0xbe7700), int ARounding = 0x5, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor ABackGroundColor = (System::Uitypes::TColor)(0xffffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorBackPanelAppearance(Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* APanel, System::Uitypes::TColor ABackGroundColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor AStrokeColor = (System::Uitypes::TColor)(0xffffffff), int ARounding = 0x5);
extern DELPHI_PACKAGE void __fastcall SetEditorSubPanelAppearance(Vcl::Tmsfnceditorpanel::TTMSFNCEditorPanel* APanel, System::Uitypes::TColor ABackGroundColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor AStrokeColor = (System::Uitypes::TColor)(0xffffffff), int ARounding = 0x0);
extern DELPHI_PACKAGE void __fastcall SetEditorLabelAppearance(Vcl::Stdctrls::TLabel* ALabel, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorCheckBoxAppearance(Vcl::Stdctrls::TCheckBox* ACheckBox, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorEditAppearance(Vcl::Stdctrls::TEdit* AEdit, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor ABackGroundColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor AStrokeColor = (System::Uitypes::TColor)(0xffffffff));
extern DELPHI_PACKAGE void __fastcall SetEditorMemoAppearance(Vcl::Stdctrls::TMemo* AMemo, System::Uitypes::TColor AFontColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor ABackGroundColor = (System::Uitypes::TColor)(0xffffffff), System::Uitypes::TColor AStrokeColor = (System::Uitypes::TColor)(0xffffffff));
}	/* namespace Tmsfnceditorstools */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCEDITORSTOOLS)
using namespace Vcl::Tmsfnceditorstools;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfnceditorstoolsHPP
