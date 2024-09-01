// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCBitmapContainerEditor.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfncbitmapcontainereditorHPP
#define Fmx_TmsfncbitmapcontainereditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.ExtCtrls.hpp>
#include <FMX.TMSFNCBitmapContainer.hpp>
#include <FMX.TMSFNCURLBitmapContainer.hpp>
#include <FMX.Forms.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCEditorListView.hpp>
#include <FMX.TMSFNCEditorButton.hpp>
#include <FMX.TMSFNCEditorPanel.hpp>
#include <FMX.TMSFNCEditorsTools.hpp>
#include <FMX.Types.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ComboEdit.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncbitmapcontainereditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCBitmapContainerEditorComboBox;
class DELPHICLASS TTMSFNCBitmapContainerEditor;
class DELPHICLASS TTMSFNCBitmapContainerEditorForm;
//-- type declarations -------------------------------------------------------
typedef Fmx::Types::TFmxObject TTMSFNCBitmapContainerEditorParent;

class PASCALIMPLEMENTATION TTMSFNCBitmapContainerEditorComboBox : public Fmx::Comboedit::TComboEdit
{
	typedef Fmx::Comboedit::TComboEdit inherited;
	
public:
	/* TCustomComboEdit.Create */ inline __fastcall virtual TTMSFNCBitmapContainerEditorComboBox(System::Classes::TComponent* AOwner)/* overload */ : Fmx::Comboedit::TComboEdit(AOwner) { }
	
public:
	/* TCustomEdit.Destroy */ inline __fastcall virtual ~TTMSFNCBitmapContainerEditorComboBox() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCBitmapContainerEditor : public Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent
{
	typedef Fmx::Tmsfnccustomcomponent::TTMSFNCCustomComponent inherited;
	
private:
	float FImgMargin;
	float FListItemHeight;
	Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm* frm;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FPanel;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FListPanel;
	Fmx::Tmsfnceditorpanel::TTMSFNCEditorPanel* FTopPanel;
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FCopyBitmapContainer;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonOk;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonCancel;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonFill;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonAppend;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonClear;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonDelete;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonAdd;
	Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* FButtonDuplicate;
	Fmx::Tmsfnceditorlistview::TTMSFNCBitmapEditorListView* FBitmapList;
	Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* FBitmapContainer;
	Fmx::Layouts::TVertScrollBox* FVScrlBox;
	void __fastcall SetBitmapContainer(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* const Value);
	void __fastcall AssignBitmapListItems();
	void __fastcall AssignControlItems();
	void __fastcall SetBitmapItemProperties(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapItem* AFrom, Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapItem* ATo)/* overload */;
	void __fastcall SetBitmapItemProperties(Fmx::Tmsfnceditorlistview::TTMSFNCEditorListItem* AFrom, Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapItem* ATo)/* overload */;
	void __fastcall SetEditorListItemProperties(Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapItem* AFrom, Fmx::Tmsfnceditorlistview::TTMSFNCEditorListItem* ATo);
	void __fastcall SetItemButtonAppearance(Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton);
	void __fastcall SetActionButtonAppearance(Fmx::Tmsfnceditorbutton::TTMSFNCEditorButton* AButton);
	void __fastcall EnableItemButtons(bool AEnabled);
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	void __fastcall UpdateBitmapList();
	void __fastcall UpdateBitmapContainer();
	void __fastcall DoBitmapListItemsChanged(System::TObject* Sender);
	void __fastcall DoBitmapListItemSelectChanged(System::TObject* Sender, int AIndex, Fmx::Tmsfnceditorlistview::TTMSFNCEditorListItem* AItem, bool ASelected);
	void __fastcall DoCopyBitmapChange(System::TObject* Sender);
	virtual void __fastcall RegisterRuntimeClasses();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation AOperation);
	virtual void __fastcall BuildEditor(Fmx::Types::TFmxObject* AParent);
	void __fastcall DoURLBitmapDownloadComplete(System::TObject* Sender, int ItemIndex);
	virtual void __fastcall DoAddItem(System::TObject* Sender);
	virtual void __fastcall DoFillItems(System::TObject* Sender);
	virtual void __fastcall DoDeleteItems(System::TObject* Sender);
	virtual void __fastcall DoDuplicateItems(System::TObject* Sender);
	virtual void __fastcall DoAppendItems(System::TObject* Sender);
	virtual void __fastcall DoClearItems(System::TObject* Sender);
	void __fastcall DoFormKeyDown(System::TObject* Sender, System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	virtual void __fastcall SetControlProperties(Fmx::Types::TFmxObject* AParent);
	
public:
	__fastcall virtual TTMSFNCBitmapContainerEditor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCBitmapContainerEditor();
	System::Uitypes::TModalResult __fastcall Execute();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Fmx::Tmsfncbitmapcontainer::TTMSFNCBitmapContainer* BitmapContainer = {read=FBitmapContainer, write=SetBitmapContainer};
};


class PASCALIMPLEMENTATION TTMSFNCBitmapContainerEditorForm : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm inherited;
	
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTMSFNCBitmapContainerEditorForm(System::Classes::TComponent* AOwner) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTMSFNCBitmapContainerEditorForm(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Tmsfnccustomcontrol::TTMSFNCCustomDesignerForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTMSFNCBitmapContainerEditorForm() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorOK;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorOK System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorOK)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorCancel;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorCancel System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorCancel)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorFill;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorFill System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorFill)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorAppend;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorAppend System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorAppend)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorClear;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorClear System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorClear)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorFillHint;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorFillHint System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorFillHint)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorAppendHint;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorAppendHint System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorAppendHint)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorClearHint;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorClearHint System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorClearHint)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorDeleteHint;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorDeleteHint System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorDeleteHint)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorAddHint;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorAddHint System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorAddHint)
extern DELPHI_PACKAGE System::ResourceString _sTMSFNCBitmapContainerEditorDuplicateHint;
#define Fmx_Tmsfncbitmapcontainereditor_sTMSFNCBitmapContainerEditorDuplicateHint System::LoadResourceString(&Fmx::Tmsfncbitmapcontainereditor::_sTMSFNCBitmapContainerEditorDuplicateHint)
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x0);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
#define TTMSFNCDELETEBITMAPITEM u"iVBORw0KGgoAAAANSUhEUgAAAQAAAAEACAYAAABccqhmAAAAAXNSR0IArs"\
	u"4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsAAAA7AAWrWiQkAAAgr"\
	u"SURBVHhe7d3Pa5x1HsDxZ2aSmEQo4tI9ePBgux5XYU+KhW7pxR715sXrbg"\
	u"8rotC/oWCR7qHsVVhkYbFHvZRSqKxX9ei2LnjwsEUpZU1jkpnZ55t8A6Ft"\
	u"mpnM93nmx+f1umS+T0unTPJ9z/c7eXieCgAAAAAAAAAAAAAAAAAAAAAAAA"\
	u"AAAAAAAAAAAAAAABhVJ3+dDZdunl7d6XywMui8lo/A3NnqDr/aXBpeqS6f"\
	u"u5MPzazpB+DApH/YG7yw3atO5j+BubXcr+6t9bs/znoMpheAPPH7neHbJj"\
	u"2LLMWgN+x8NoshaD8AJj5BzWIIWg3AqYu3z/+wvv2piU9kKQQvbiy/c/fa"\
	u"mRv50NS0E4D8rr/dHb7b71Zr+SiE1RtUD5cHnU+mvRpoPgD15F8eVFe3u9"\
	u"WFfATI6rnxeT033ptWBJoNgMkPR5pmBJoLgMkPI5tWBLr5a3F7e36TH0aR"\
	u"5kqaM3nYmkYCkD7tTx/45SEwgjRn0tzJw1aU3wKkpX+/+pdf9cH40q8I67"\
	u"nzeltbgeIrgN2lv8kPx5LmTptbgbIBqN/90xl+eQQcw+4cqudSHjaqaAC8"\
	u"+8Pk2lwFlAuAd38opq1VQLEAePeHctpaBRQLgIt4QFltzKkyAaiXKuliHn"\
	u"kEFLA7pxreBhQJgOU/lNfGNqBIACz/oRlNz61inwEA80cAIDABgMAEAAIT"\
	u"AAhMACAwAYDABAACEwAITAAgsCLXBDzx/q2vH6wMXsnDVqU7rDy70/0uD6"\
	u"G4X5YGL0/rjlYntrrfPPj47Kt5WNzcB6DpFwgW+efbFgACEwAITAAgMAGA"\
	u"wAQAAhMACEwAIDABgMBCngiUbsF875n+R3lIECd/7X1499qZG3k4skU+ES"\
	u"hkAKb5/2V6jjuZnAkILCQBgMAEAAITAAhMACAwAYDABAACEwAITAAgMGcC"\
	u"HsFFR2fbOBfsdCbg4wTgCE1/A5hMG9/LRQ6ALQAEJgAQmABAYAIAgQkABC"\
	u"YAEJgAQGACAIEJAAQmABCYAEBgAgCBCQAEJgAQmABAYAIAgQkABCYAEJgA"\
	u"QGACAIEJAAQmABCYAEBgAgCBCQAEJgAQmABAYAIAgQkABCYAEJgAQGACAI"\
	u"EJAAQmABCYAEBgAgCBCQAEJgAQmABAYAIAgQkABCYAEJgAQGACAIEJAAQm"\
	u"ABCYAEBgAgCBCQAEJgAQmABAYAIAgQkABCYAEJgAQGACAIEJAAQmABCYAE"\
	u"BgAgCBCQAEJgAQmABAYAIAgQkABCYAEJgAQGACAIEJAATWyV8ncuL9W18/"\
	u"WBm8koetOrHV/ebBx2dfzcORjPP/7Q2qh8/udL/LQ2bML0uDl/vdai0Pn+"\
	u"o4PyvJvP18j0MACEMAHmcLAIEJABxhqzv8Kj9cOAIAR9hcGv4nP1w4AgBH"\
	u"u77cr+7lxwtFAOAol8/d6Q07n+VRq5refggAjKDeBlxZHlT387AV6fnS8+"\
	u"ZhIwQARpFWAYPOP/KoFbvPVz9vHjZCAGBEeRXwUx42Kj1P0+/+iQDAqPZW"\
	u"Af/Mo0btPk/D7/5JyDMBT128ff7eM/2P8pAgTv7a+/DutTM38vB4Lt08Xb"\
	u"87X93uVhfykeLqf//z+t9/TwBG0PSpkvCYBiPQ5uRPbAFgXPXkTJN0dafz"\
	u"t6VB9SAfnVjbkz8RADiOepJuXvnjn3e61R/SxM1HjyWdZJRi0vbkTwQAJn"\
	u"FgNZC2o3UMRjpXIK0c0t/fnfi96vUUk7Ynf+IzACjp0s3T9aT+YGXQeS0f"\
	u"eUw6u2/3V3xTmPCPEgAITABo3fC3vz9df3krPf555bmXNpdW30iPqarVnc"\
	u"0vn9+6/30eXu/899tGVwlzHwCX7CqryeXp/sTf6K2dXe8/fHPvKIepX6cv"\
	u"6tfpVv2wsRDMfQAob2lQ/W9p0Pl7qRCY+JNpMgQCwKGWB9VP6ZTUSUKQJn"\
	u"/9A/xXE39yOQR/KRkBvwbkUNvd6jf15P9THYKr6dPtfHhkJn9Z6XVMr2de"\
	u"URUhABypDsGFcSNg8jejdAQEgJEcIwJvmfzNKBkBAWBkKQLpJJc8PFR+9z"\
	u"+bhzQgx3X3V6mTEADG0u8M337aKsDSvz0pspOuAgSAsWz3qpNHrAIs/VtS"\
	u"YhUgAIztaee5M18EgLFtLA9+96RtgL1/+ybdBggAY9vpVOuHbAMs/1s26T"\
	u"agSAAW+d5pPJltwGIoEoB0quii3joJFlmZLcDlc3fW+t0f8wiYE8U+A7AN"\
	u"gPlTLAC2ATB/igUgbQOmdQdV4HjKBaBmFQDzpWgArALi8JnPYigbgJpVwO"\
	u"JbGlYb6fuchwdd3+itfZEf04L8el/fG42veADSKuDFjeV30sU68xEWzPp2"\
	u"999PukRYulTV+t6162hJer0nuURY+QDU0h1YlwedT/KQBWP5vzgaCUCyux"\
	u"WY8J5pzJ60vTtk+b/PNqAlky7/k8YCkJaI6Z5pIrBYdj/kfcLyf59tQHsm"\
	u"Xf4nzQUgEYGFkr6PR7z777MKaFiJd/+kyH0BjnRp74aJ293hu/1utZaPMk"\
	u"fS5E8xf9q7/0EuDdacNPnr17XI/QHaCUB26uLt8z+sb3+aLiuVDzEHxp38"\
	u"+0SgvJKTP2k1ALvyaiBdXFIIZls98d0ZaIaUnvxJ+wHYJwQzy70BZ0ue+L"\
	u"N7b8CJ5BCkK8w87A1eEIPpSb/iS5/yl5r4jxKC8TQ58fdNPwAHHYhBPkJL"\
	u"mrwt+KP2Q5Ae/7zy3EubS6tvpMdU1erO5pfPb93/Pg8bm/gAAAAAAAAAAA"\
	u"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAX"\
	u"qur/BG7ewAty0h4AAAAASUVORK5CYII="
#define TTMSFNCADDBITMAPITEM u"iVBORw0KGgoAAAANSUhEUgAAAQAAAAEACAYAAABccqhmAAAAAXNSR0IArs"\
	u"4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADr8AAA6/ATgFUyQAAArh"\
	u"SURBVHhe7d3NixzHGcfxnu6ZfdFiSSTZHHSIEFZ0C/YhEASxMcIngS8rfA"\
	u"jBEMgl+ODFyEF/g4iFWR9MDjkEkpCA0F4EOpnF2AETkoOdoyIjyEGHKDaS"\
	u"HGlXOzM9qWfnGbO2NO55qaqeruf7AWv6mbWmxew+v64ataoyAAAAAAAAAA"\
	u"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAJNq6eNi"\
	u"uLRz2v26sdJrnVoqW2eHTwLNsZ8PPt5rD267w+3s8rlbw2cXV/0BcKjp+6"\
	u"3BhW6RrQ+/ADRXp5/dLQata4seBvUFgGt81/QXaXqk7lAYXFm0IIgfADQ+"\
	u"jFrEIIgbAK75O2W21c2z8/oMYI7rgRuuBzYXIQTiBABXfeBrFmU0ED4AuO"\
	u"oDY9U9GggbADQ/UKnOEAgXADQ/MLG6QiDXR+9kzk/zA5ORXpGe0TKaMAHg"\
	u"rv7ygZ9WACZw0DOud7SMwn8AjIb+fNoPTEV6RnonZgh4DwCG/sDsYk8F/A"\
	u"YAQ39gbjGnAl4D4ODqz9AfmIv0UKxRgL8A4OoPeBNrFOBzBLDB1R/wQ3tp"\
	u"Y1iF4y0A3JDllB4C8CBGT/kJAIb/gHcxpgG+RgAM/wHPYkwDvAQAw38gjN"\
	u"C95SUAWMATCCN0b3n7EBBA8xAAgGEEAGAYAQAYRgAAhhEAgGEEAGAYAQAY"\
	u"RgAAhnlZFvzomx988mCpfE7LqIoy213r5Te1BLx72C7P9PNsVcuoju7nnz"\
	u"5456XntfSu8QEQ+g0CUv75ZgoAGEYAAIYRAIBhBABgGAEAGEYAAIYRAIBh"\
	u"BABgmMkbgZ59/aOX7y7339YSRqw/Lt767L0X3tdyYinfCGQyAOr886I+sz"\
	u"YTdwICSBIBABhGAACGEQCAYQQAYBgBABhGAACGEQCAYQQAYBh3AlZg0dHF"\
	u"Ns2CndwJ+CQCoELobwDmE+N7mXIAMAUADCMAAMMIAMAwAgAwjAAADCMAAM"\
	u"MIAMAwAgAwjAAADCMAAMMIAMAwAgAwjAAADCMAAMMIAMAwAgAwjAAADCMA"\
	u"AMMIAMAwAgAwjAAADCMAAMMIAMAwAgAwjAAADCMAAMMIAMAwAgAwjAAADC"\
	u"MAAMMIAMAwAgAwjAAADCMAAMMIAMAwAgAwjAAADCMAAMMIAKDCfj74WA+T"\
	u"QwAAFfbag9t6GN1uUZ7ILu2c1tI7AgBYYN0iW3cPG8PKPwIAMIwAAAwjAA"\
	u"DDCADAMAIAMIwAAKptt8vsgR5HpefdHlb+EQBAlcvnbh3p5bXcC3BwXnd+"\
	u"Lb0jAIAJ1HU3YOjzEgDABPbagyudMrunZRRyPjmvlkEQAMAk3DC8KFt/0S"\
	u"qKg/MFHP4LAgCYkI4CPtcyKDlP6Ku/IACASQ1HAVe1CurgPIGv/oIAAKag"\
	u"o4AbWgYhrx/j6i8IAGAa7qrczbPNUCEgryuvH+PqLwgANNr64+Kto/v5p5"\
	u"P8J/+v/rb5aAis9Fq/9XmDUOzmFy19nMvRNz/45MFS+ZyWUck39sE7Lz2v"\
	u"5USm+fPO8vow5NLOade4W65xz+szU+v0s7vFoHXtYNgfsfkFAVCBAEjPa3"\
	u"/7wanlpWObnWL1p/rUE7r93b8+3r+/9Yef/Lv6DkAXAm40cHGpbJ3dbZcn"\
	u"XRgc16+MJSMHuctPbvSpo/FHCIAKBEAaDjf97uN7Z/rl3jP6pbGKfOXL1e"\
	u"XjN2cNA33mCXU3/WEEQAUCoNlGjV+WvVe7vYcn9Ompddprd/K8fXXiIGgI"\
	u"PgREsqT528Xqb1zTbs7T/EJ+v7yOvJ68rj7deAQAkjRq/l5/94I+5YW8Xk"\
	u"ohQAAgOaGafySlECAAkByZ84dq/hF5fTmPlo1FACApclUu+92faRmUnKfp"\
	u"owACAEmRq3K3/+j7WgYl52n6KIAAQDLkatzv7/9CyyjkfE0eBRAASIbO/Y"\
	u"9pGYWcr8mjAAIAyfi2W3tDquu8PnAnYIWizHbXevlNLbEg5F/2ffbeC+9r"\
	u"Ofqrv3+4K/J39Klo3Hm/cOf9cRPvECQA0Ejf/L67AHjDPWwNq1psugB4V4"\
	u"8bgykAYBgBABhGAACGEQCAYQQAYBgBABhGAACGEQCAYQQAYJjJOwGfff2j"\
	u"l+8u99/WEg30lFuBuRNwBiYDAOkhAGbDFAAwjAAADCMAAMMIAMAwAgAwjL"\
	u"8FSNWlndPu141hMZHtOjar1AU1XxlWs2sXqy/KWv1aRufOf82d/0Mt53E9"\
	u"5spCjQ8Alux6ut2iPNEtsnUtK8XcrlqaXhbSlLX09vbvn6pjGa9FJcuLrS"\
	u"wduz3VjsRzaHwAwD8XBv9rl60/+g6CUeP3+/s/d03/PX0aY7gwuF8US78P"\
	u"GQQEAMbqlNnnRdm6Om8QjBpfdtKJtWlHSjrFkf/kRefPIYKADwExVjfPvu"\
	u"ua/1cuCLb0M4WpSfO7K9lwi26afybyvoXampwAQCUXBOdnCYFR89f54VxK"\
	u"5H30HQIEACYySwjoTj00v0e+Q4AAwMQkBFZ6rYtafiv5Ae3391/TEh5JCP"\
	u"jajowAwFT6rcGFqlHAoaE/f70XSFn2XvUxCiAAMBW5t6BqFMDQP7xu7+EJ"\
	u"H6MAAgBTWypbZ/XwqeQGHz1EQD7eZwIAU3vUKX84bhogw9K9/fsntURAu4"\
	u"/vnZl3GuAlAOT2UT2EAb1WdmTcNECH/9zlF0G/3Htm3mmAlwDYaw8aty0y"\
	u"5jNuGsDwP655329fU4DtTj+7q8cAGsJPAFw+d6sYtK5pBaAhvH0IyDQAaB"\
	u"5vAeAwDQAaxl8AMA0AGsfnCECmAVcYBQDN4TUAGAXYMe7eD1nKSg8Rwbzv"\
	u"t98AcA5GAWV2Q0skqD3IHsn3WcuvkVVr2sXqF1oioCJf+VLeby1n4j0AZB"\
	u"TQzbNNpgLpOtLN/zVuiTBZskoWtdQSAa0uH7857xJh/gNAMBVIWtWt30wD"\
	u"4vDxPocJAIepQJpkZDdu+D8iw9JOe+2OlghA3t95h/8iWAB8NRUgBJJyML"\
	u"IbM/wfkWFpnrevaokA5P31sUJwuAAQhEBS5PtYdfUf0Q8DmQYGIO+rj6u/"\
	u"8LIvQKVLO6fln4/KclLT7FaDxSHNL2FedfU/7NDSYKwO5Ik0v3s/f+1rf4"\
	u"A4ATDigsD9IG3J4pL6DBpgluYfIQT88d38Im4ACEYDjeEan52BFkDInYHi"\
	u"B8AIQbCw2BtwMbgrfjP2BpzLcG25DRcGpwiD+rA78GJwTd+83YG9ORQGVS"\
	u"vPopo286Q/QNshm34cCQP38Mqwmp1rnBfr/JxB5+cfajmP66Gb/rDFCgBg"\
	u"Ri5I3nAPXv5qbEabrnHf1ePGCHsfAICFRgAAhhEAgGEEAGAYAQAYRgAAhh"\
	u"EAgGEEAGAYAQAYRgAAhhEAgGEEAGAYAQAYRgAAhhEAgGEEAGAYAYBUXJfl"\
	u"tPQ4Kj3v9WHVLAQAkiDLaNW1KamcN+YyXj4RAEhGXZuSNnkzVAIAydDtyP"\
	u"6rZRRyPl/bdNWBAEAyZBheFEt/0jIKOV9Th/+CAEBS5GosO+loGZScp8lX"\
	u"f0EAIClyNZZttLQMSs7T5Ku/IACQHP0sIOjW5PL6Tb/6CzYGQZJkxyHXpE"\
	u"F2JZbmd6/rdZfeuhT6CCTln7+7f+9Hv1z7+/LSMXeVy8+Ug96qfmkuKTW/"\
	u"YASA5PkYDXTaa3fyvH1Vhv2pNL8gAGDCN3YkPunCoHJ7chcaUXfqrQMBAH"\
	u"MOh4E+9YSUmx4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAbbLs"\
	u"/3rghYOA5SDiAAAAAElFTkSuQmCC"
#define TTMSFNCDUPLICATEBITMAPITEM u"iVBORw0KGgoAAAANSUhEUgAAAQAAAAEACAYAAABccqhmAAAAAXNSR0IArs"\
	u"4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsAAAA7AAWrWiQkAAAup"\
	u"SURBVHhe7d1tiBvHHcfxmdVK1h2ubSgGvz1i8qolDc0bJ5TExgm4Ly99kb"\
	u"Ym7YtQnBhiypXe277om0ttig0m7hO0gZrkRe+lDQbbbY0TSE1p37YGvyiY"\
	u"gJumcd2zKu3OdEb7l0/3LGlXul3N9wPyzuw5rU/a+e3s7GhWAQAAAAAAAA"\
	u"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACielu1kLN447P6cb6Z6"\
	u"rmH0C64cdfdn/47J/ltQFVZenmlH9narZu9JfVktHbsrZYxg/I2ur9EnkX"\
	u"090Wo2+wGQT93oa53IXndFgmBE4w0A1/jrRp3vROrrsgconDvGrrhj7Awh"\
	u"MLzxBIBr+M1EL7gz/jeTSO2XvcDYxEb9Jzb6t63YniMIBld8AHDWxy6iNz"\
	u"CcYgMga/wX3AdwQvYAEydjA6cJgZ0VFwDdxq8vujf+FdkD7BpCYDC923D5"\
	u"PDnz0/hRDv5Y9MekPzZlFzZRSAD4AT+6/Sgbf0z68ShCYGv5A8C9uUnNnp"\
	u"QaUCoyGD2f1bBevgDIuv7nE632yh6gdOpWv0wvYHN5ewDzkrBAaXW0Pe4v"\
	u"U6WKPqMHQHb2f1FqQKml2r5KL2CjPD0Azv6ojE5NHXQbxgLWGTkAXJdqTo"\
	u"pAJXDMbjTaRKCs+/8n1wM4IHuA0ouNephE6qtMDlo1ag/Ad/9p/KgU1/j3"\
	u"uQ2XAX3y3gUAUGEEABAwAgBBYSBwLQIAQWkYfUSKcAgAIGAEABAwAgAIGA"\
	u"EABGzUmYA/dH8uZZXxio36fDaNPpEqptBKzRya1OrR+9rRXx/+9KWvSDV4"\
	u"pQ6AeqoedGrqeaZuTjk/tTxVH8oXdsaKAFir1JcAM2l0n8YfAPcZdz9rTB"\
	u"xjAEDACAAgYAQAEDACAAgYAQAErNS3AQe9ZfPUW7eOP9iTnpUqSqId2Y8G"\
	u"fVrvvu///i8PG+YZqY4NtwHXmooAmNTBg5EsugB4R8pbIgB2B5cAQMAIAJ"\
	u"RCEqs7Uhwrf1kiRTgEAEphJTJ/k+JYtWJ7T4pwCAAEIzbqkdssZzV4BADK"\
	u"Ytmv2y/lsahb/T7fLVmLAEA5uIYZG31ZamPxuGb/LkUIAgCl4ecM1I36p1"\
	u"QL5f53r7oN3f91gpoH4A6CT2dYXGQ0ViVSGtgwE4F6mgs333X/zSmpFsIv"\
	u"KpNE6jm6/xuFNhFooEkp2EXZcyfPF/nk6WaiL7XOHX1TqujDJQDKxZ2lXe"\
	u"M/40LgiuwZWWzVo27j970QbIoAQPlICPjG6xrxiuwdSkPpPyRaPds989P1"\
	u"3xIBgHJyjdY3XteIn5lJ9c8GvUXo/14z1Zfayr5Bw98ZAYByc4348dmjp7"\
	u"rP9Vdq0fcK/NjQ+pff73/u/17rLGf9QTEICASMAMhr8cZh9+e863bONYx+"\
	u"wZV7vSr/3o72/mLaWXl5ph3Z263ak+8oLE+y90IAjKKv0SeRfd1dp85mPw"\
	u"DyqRt9rRPZ6644kSBgDGBYcp/alZZcap+i8aNIrvG/4jZL3WMsO9GMFQEw"\
	u"KPdh+FlqsVF3ipykAmzGH2PuWPuzP+bGGQQEwCDkrO+nqE7qGXaAO9a+4I"\
	u"+5cfYGCICdZI3/Amd97BZ/7NWNvjiOECAAttNt/Pqi+wBOyB5gV/ixgXGE"\
	u"AAGwlSdn/u6gDLDrshBQF4oMAQJgC81EL3DmR9n4Y7LIMQECYDPuzU1q9q"\
	u"TUgFKR8aj5rJYPAbBe1vU/n2i1V/YApVO3+uUiegEEwEbzkrBAaXW0Pe4v"\
	u"U6U6MgKgX3b2f1FqQKml2r6atxdAAKzF2R+V0ampg26TayyAAOjjulRzUg"\
	u"QqIe8xSwD0uK5UGtnXpAZUQhLZb+W5DCAAVvnu/wEpA5WQRGqf24x8GUAA"\
	u"AAEjAICAEQBAxeUZCCQAgIprGH1EikMjAICAEQBAwAgAIGAEABAwngvQM6"\
	u"HfyfPPq59No0+kiim0UjOHJrWA7KDtZDMEQM+Efqd6qh50aur5ST79BbvA"\
	u"f7M0VR/KF3bGKk8AcAkwYTNpdJ/GHwD3GXc/65IjAICAEQBAwAgAIGAEAB"\
	u"AwAgAIGLcBe0r2Oz311q3jD/akZ6WKkmhH9qNWbM8NcidniOMyF+YBTGEA"\
	u"TOrgwUi2Po76VCEAuAQAAkYAAAEjAICAEQBAwAgAIGAEABAwAgAIGPMAei"\
	u"r6O9WN+nSGxUVye1wzhzqR+qJUdzI18wAIgJ5p/J0wuOEW8GAiEDBVKrKA"\
	u"R9EIACBgBAAQMAIACFgwAeBX43Wb5awGwAsmAFiNF9iISwAgYAQAEDACAA"\
	u"gYAQAEjAAAAkYAAAEjAICAEQBAwAgAIGBTEQD+aS1S3NIgfwcIzVQEQPdR"\
	u"TX6Rhm1e8ncA9JmOSwA/x9+v0LL9i+8BAOswBgAEjAAAAkYAAAEr9arAfh"\
	u"GPTk0N/Iz8Wi2aMcYm1tqO7Foe+NqfVYGDV/R7zqrAOckSzb5RDvRKU/Mj"\
	u"1/h/3Ks3rP5Ft2Ev3jjs6gDWmepLgLa2L7nNUt2oC4QAsFEQYwCdSJ2Ijb"\
	u"rTXLj5LkEArApmEDCJ1P5WbE81E70gu4DgBXcXIInsSXoBQCbAAFB7ZXCQ"\
	u"EEDwggsAzw8ONlMuBYAgA8BLtX2NXgBCF2wAdCJ1wG3msxoQpmADwGsmek"\
	u"6KQJBGDYDlulGfSbmyGkYfkSIQpNECYOnY3ZrRH0gNQEWNfAngV9iJrVqR"\
	u"KoAKGn0MwPUC4lS/JzUAFZRrEJBeAFBtuQKAXgBQbfkCwOn2Aox6KNVKYa"\
	u"lwhC53AHR7AUZfllqluPC6J0UgSPkDwPG9gLpRV6VaCQ2rr7vNclYDwlRI"\
	u"APheQCdSb1cpBNraXvP/bqkCQSomALzVELgie0qrbvQ1t+Hsj+AVFwBeFg"\
	u"Jnmom+VLOqJXtLpaH07U5kT3P2B4oOAM81rNa5o2+mWn3ZXWfflL2lsEdF"\
	u"H7eV/S6NH8gUHwA9rpG56+zv+d5AbNUj2btrfBj9T5lv0/iBVeMLAE96A4"\
	u"lWz7ra4qzRv4qN+jz74eT4wUkfRjR+YK3xBkCPb3hLx95Z+cnRN5JIPef2"\
	u"LPqegX+iSVEv/xSh7P8s4ycnzaT6l6646AcnafzARqM9GqyMsuW9+lf4Gf"\
	u"yxYB6PBgsejwarMull9L044wM7mJ4AADA0AgAIGAEABIwAAAJGAAABm57b"\
	u"gHlV9Dagn/8wk0b3pYoc/hubp9NIzUh1O1NzG5AA6KnuPABMHvMAAFQfAQ"\
	u"AEjAAAAkYAAAEjAICAEQBAwAgAIGAEQEnx1KJyaij9R7eZmhWlmQjUU7KJ"\
	u"QJsscIJyGHihGWYCVknZAgCVx0xAAKVGAAABIwCAgDEG0DOhMQD/9d1OTZ"\
	u"2VKqaY+6x/4D7rg1IdGwYBizChAACKxiAggJEQAEDACAAgYAQAEDACAAgY"\
	u"AQBUXJ4vjhEAQMW1YntPikMjAICAEQBAwAiAVct1oz6TMlAJ7pj9t9uMvE"\
	u"AJAdCzdOxuzegPpAZUgjtm3x90gZLNEAB9WrE9F1u1IlWg9PIMAHoEQD+X"\
	u"pHGq35MaUGr+m6Vuk2t9QgJgHXoBqIqa1b/L0/33CID16AWgAupGX/MnK6"\
	u"mOjADYRLcXYNRDqQKl04ns9bxnf48A2IzvBRh9WWpAqdSNuuI2hTybgADY"\
	u"gu8FuDf6qlSBUvDHZCdSZ4o4+3ssCbadxRuH3Rt+wb3hJ2QPsGv8db/r+p"\
	u"8uqvF79AC2495o1/jfli4XsGv8MVh04/dqssVWbv/mX+Zr3/m4meiG1epL"\
	u"7hXLT4CJyBp/cd3+flwCDMNdErgU+Hlb26OyBxgbfyfKD0Z3b/eNofF7BM"\
	u"CwXAi43sBCUrMnE632yl6gMDWrWvVU/3qcDb+HABiVPL131uin28p+I4nU"\
	u"/uwHwPD82X42jf7RjuytVm38Db+HACiChIHrGcw1jD6S7QR25pfzcmd6/4"\
	u"WegR87DgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"\
	u"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAoSv0ffi5ERy54ASkAAAAASUVORK"\
	u"5CYII="
}	/* namespace Tmsfncbitmapcontainereditor */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCBITMAPCONTAINEREDITOR)
using namespace Fmx::Tmsfncbitmapcontainereditor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncbitmapcontainereditorHPP
