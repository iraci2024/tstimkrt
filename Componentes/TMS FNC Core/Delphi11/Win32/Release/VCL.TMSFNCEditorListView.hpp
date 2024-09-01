// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VCL.TMSFNCEditorListView.pas' rev: 35.00 (Windows)

#ifndef Vcl_TmsfnceditorlistviewHPP
#define Vcl_TmsfnceditorlistviewHPP

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
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <VCL.TMSFNCCustomControl.hpp>
#include <VCL.TMSFNCGraphics.hpp>
#include <VCL.TMSFNCGraphicsTypes.hpp>
#include <VCL.TMSFNCBitmapEditor.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.ActnList.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vcl
{
namespace Tmsfnceditorlistview
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCEditorListViewAppearance;
class DELPHICLASS TTMSFNCEditorListItem;
class DELPHICLASS TTMSFNCEditorListCollection;
class DELPHICLASS TTMSFNCEditorListView;
class DELPHICLASS TTMSFNCBitmapEditorListView;
class DELPHICLASS TTMSFNCEditorList;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TTMSFNCListEditorItemRectType : unsigned char { eirtNone, eirtItem, eirtImage, eirtName, eirtDataString };

class PASCALIMPLEMENTATION TTMSFNCEditorListViewAppearance : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TTMSFNCEditorListView* FOwner;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemDownStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FItemFont;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemSelectedFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FItemHoverFont;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FEditFont;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemSelectedStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemHoverFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FItemDownFont;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemHoverStroke;
	System::Classes::TNotifyEvent FOnChanged;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemDownFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FItemSelectedFont;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	float FItemVerticalSpacing;
	int FItemRounding;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemImageHoverStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemImageDownFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemImageDownStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemImageSelectedFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemImageFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemImageSelectedStroke;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemImageHoverFill;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemImageStroke;
	int FItemImageRounding;
	bool FUseImageAppearance;
	float FItemHorizontalSpacing;
	Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides FStrokeSides;
	void __fastcall SetEditFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetItemDownFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemDownFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetItemDownStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetItemHoverFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemHoverFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetItemHoverStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemSelectedFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemSelectedFont(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetItemSelectedStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemVerticalSpacing(const float Value);
	void __fastcall SetItemRounding(const int Value);
	void __fastcall SetItemImageDownFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemImageDownStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemImageFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemImageHoverFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemImageHoverStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemImageSelectedFill(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemImageSelectedStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemImageStroke(Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemImageRounding(const int Value);
	void __fastcall SetUseImageAppearance(const bool Value);
	void __fastcall SetItemHorizontalSpacing(const float Value);
	void __fastcall SetStrokeSides(const Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides Value);
	
protected:
	virtual void __fastcall DoChanged(System::TObject* Sender);
	virtual void __fastcall DoImageChanged(System::TObject* Sender);
	virtual void __fastcall DoFillChanged(System::TObject* Sender);
	virtual void __fastcall DoEditFontChanged(System::TObject* Sender);
	virtual void __fastcall DoStrokeChanged(System::TObject* Sender);
	virtual void __fastcall DoItemFillChanged(System::TObject* Sender);
	virtual void __fastcall DoItemFontChanged(System::TObject* Sender);
	virtual void __fastcall DoItemStrokeChanged(System::TObject* Sender);
	virtual void __fastcall DoItemImageFillChanged(System::TObject* Sender);
	virtual void __fastcall DoItemImageStrokeChanged(System::TObject* Sender);
	
public:
	__fastcall TTMSFNCEditorListViewAppearance(TTMSFNCEditorListView* AOwner);
	__fastcall virtual ~TTMSFNCEditorListViewAppearance();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetItemFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* EditFont = {read=FEditFont, write=SetEditFont};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemHoverFill = {read=FItemHoverFill, write=SetItemHoverFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* ItemHoverFont = {read=FItemHoverFont, write=SetItemHoverFont};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemHoverStroke = {read=FItemHoverStroke, write=SetItemHoverStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemDownFill = {read=FItemDownFill, write=SetItemDownFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* ItemDownFont = {read=FItemDownFont, write=SetItemDownFont};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemDownStroke = {read=FItemDownStroke, write=SetItemDownStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemSelectedFill = {read=FItemSelectedFill, write=SetItemSelectedFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* ItemSelectedFont = {read=FItemSelectedFont, write=SetItemSelectedFont};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemSelectedStroke = {read=FItemSelectedStroke, write=SetItemSelectedStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemFill = {read=FItemFill, write=SetItemFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* ItemFont = {read=FItemFont, write=SetItemFont};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemStroke = {read=FItemStroke, write=SetItemStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemImageHoverFill = {read=FItemImageHoverFill, write=SetItemImageHoverFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemImageHoverStroke = {read=FItemImageHoverStroke, write=SetItemImageHoverStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemImageDownFill = {read=FItemImageDownFill, write=SetItemImageDownFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemImageDownStroke = {read=FItemImageDownStroke, write=SetItemImageDownStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemImageSelectedFill = {read=FItemImageSelectedFill, write=SetItemImageSelectedFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemImageSelectedStroke = {read=FItemImageSelectedStroke, write=SetItemImageSelectedStroke};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemImageFill = {read=FItemImageFill, write=SetItemImageFill};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemImageStroke = {read=FItemImageStroke, write=SetItemImageStroke};
	__property float ItemVerticalSpacing = {read=FItemVerticalSpacing, write=SetItemVerticalSpacing};
	__property float ItemHorizontalSpacing = {read=FItemHorizontalSpacing, write=SetItemHorizontalSpacing};
	__property int ItemRounding = {read=FItemRounding, write=SetItemRounding, nodefault};
	__property int ItemImageRounding = {read=FItemImageRounding, write=SetItemImageRounding, nodefault};
	__property bool UseImageAppearance = {read=FUseImageAppearance, write=SetUseImageAppearance, nodefault};
	__property Vcl::Tmsfncgraphicstypes::TTMSFNCGraphicsSides StrokeSides = {read=FStrokeSides, write=SetStrokeSides, nodefault};
	
public:
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
};


class PASCALIMPLEMENTATION TTMSFNCEditorListItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::Types::TRectF FDrawRect;
	System::Types::TRectF FImgRect;
	System::Types::TRectF FNameRect;
	System::Types::TRectF FDataStringRect;
	Vcl::Tmsfnctypes::TTMSFNCBitmap* FBitmap;
	NativeInt FTag;
	System::UnicodeString FName;
	System::TObject* FDataObject;
	System::UnicodeString FDataString;
	System::Classes::TNotifyEvent FOnChanged;
	float FItemHeight;
	bool FSelected;
	System::Uitypes::TColor FFontColor;
	System::Uitypes::TColor FSelectedFontColor;
	void __fastcall SetBitmap(Vcl::Tmsfnctypes::TTMSFNCBitmap* const Value);
	void __fastcall SetDataObject(System::TObject* const Value);
	void __fastcall SetDataString(const System::UnicodeString Value);
	void __fastcall SetName(const System::UnicodeString Value);
	void __fastcall DoChanged();
	void __fastcall DoSelectItemChanged(int AIndex, TTMSFNCEditorListItem* AItem, bool ASelected);
	void __fastcall SetItemHeight(const float Value);
	void __fastcall SetSelected(const bool Value);
	void __fastcall SetFontColor(const System::Uitypes::TColor Value);
	void __fastcall SetSelectedFontColor(const System::Uitypes::TColor Value);
	
public:
	__fastcall virtual TTMSFNCEditorListItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TTMSFNCEditorListItem();
	virtual void __fastcall DoBitmapChanged(System::TObject* Sender);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::Types::TRectF DrawRect = {read=FDrawRect};
	
__published:
	__property Vcl::Tmsfnctypes::TTMSFNCBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property System::TObject* DataObject = {read=FDataObject, write=SetDataObject};
	__property System::UnicodeString DataString = {read=FDataString, write=SetDataString};
	__property NativeInt Tag = {read=FTag, write=FTag, nodefault};
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
	__property float ItemHeight = {read=FItemHeight, write=SetItemHeight};
	__property System::Uitypes::TColor FontColor = {read=FFontColor, write=SetFontColor, nodefault};
	__property System::Uitypes::TColor SelectedFontColor = {read=FSelectedFontColor, write=SetSelectedFontColor, nodefault};
	__property bool Selected = {read=FSelected, write=SetSelected, nodefault};
};


typedef System::DynamicArray<TTMSFNCEditorListItem*> TTMSFNCEditorListItemArray;

class PASCALIMPLEMENTATION TTMSFNCEditorListCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TTMSFNCEditorListItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	TTMSFNCEditorListView* FOwner;
	System::Classes::TNotifyEvent FOnChanged;
	TTMSFNCEditorListItem* __fastcall GetItemEx(int Index);
	void __fastcall SetItemEx(int Index, TTMSFNCEditorListItem* const Value);
	
protected:
	virtual System::Classes::TCollectionItemClass __fastcall GetEditorListItemClass();
	virtual void __fastcall DoChanged();
	
public:
	__fastcall TTMSFNCEditorListCollection(TTMSFNCEditorListView* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	HIDESBASE TTMSFNCEditorListItem* __fastcall Add();
	HIDESBASE System::TObject* __fastcall Insert(int index);
	__property TTMSFNCEditorListItem* Items[int Index] = {read=GetItemEx, write=SetItemEx/*, default*/};
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TTMSFNCEditorListCollection() { }
	
};


typedef void __fastcall (__closure *TTMSFNCOnDoubleClickEditorListItem)(System::TObject* Sender, int AIndex, TTMSFNCEditorListItem* AItem, float X, float Y);

typedef void __fastcall (__closure *TTMSFNCOnSelectItemChanged)(System::TObject* Sender, int AIndex, TTMSFNCEditorListItem* AItem, bool ASelected);

class PASCALIMPLEMENTATION TTMSFNCEditorListView : public Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	bool FItemsChanged;
	int FLastSelectedItemIndex;
	int FKeyIndex;
	Vcl::Tmsfncbitmapeditor::TTMSFNCBitmapEditor* FBitmapEditor;
	Vcl::Stdctrls::TEdit* FEdit;
	bool FEditMode;
	TTMSFNCListEditorItemRectType FEditType;
	int FEditIndex;
	System::UnicodeString FPrevText;
	bool FIntUpdateBlock;
	bool FUpdateBlock;
	TTMSFNCEditorListCollection* FItems;
	float FDefaultItemHeight;
	float FImgMargin;
	int FHoverIndex;
	System::Types::TPointF FDragStart;
	bool FMouseDown;
	int FDownIndex;
	int FAddItemIndex;
	System::Types::TRectF FAddItemRect;
	System::Classes::TNotifyEvent FOnItemsChanged;
	TTMSFNCOnDoubleClickEditorListItem FOnDoubleClickItem;
	System::Classes::TNotifyEvent FOnAddNewItem;
	TTMSFNCOnSelectItemChanged FOnItemSelectedChanged;
	TTMSFNCEditorListViewAppearance* FAppearance;
	bool FDrawDataString;
	bool FItemsReadOnly;
	bool FMultiSelect;
	bool FCanUnselectItems;
	TTMSFNCEditorListCollection* __fastcall GetItems();
	void __fastcall SetItems(TTMSFNCEditorListCollection* const Value);
	void __fastcall SetDefaultItemHeight(const float Value);
	int __fastcall GetItemIndex(float X, float Y);
	TTMSFNCListEditorItemRectType __fastcall GetItemRectType(float X, float Y, int AIndex = 0xffffffff);
	void __fastcall AssignBitmap(TTMSFNCEditorListItem* AItem);
	void __fastcall SetAppearance(TTMSFNCEditorListViewAppearance* const Value);
	void __fastcall SetDrawDataString(const bool Value);
	void __fastcall SetItemsReadOnly(const bool Value);
	void __fastcall SetMultiSelect(const bool Value);
	virtual bool __fastcall IsFileSupported(System::UnicodeString AFileName);
	
protected:
	virtual void __fastcall ChangeDPIScale(int M, int D);
	bool __fastcall MultiSelected();
	virtual void __fastcall MoveItems(float X, float Y);
	virtual void __fastcall DeleteSelectedItems();
	virtual void __fastcall DoEditKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall HandleKeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall HandleKeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall HandleKeyPress(System::WideChar &Key);
	virtual void __fastcall HandleMouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseEnter();
	virtual void __fastcall HandleMouseLeave();
	virtual void __fastcall HandleMouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleDblClick(float X, float Y);
	virtual TTMSFNCEditorListCollection* __fastcall CreateItems();
	virtual void __fastcall SetItemRect(int AIndex, float X, float Y);
	virtual void __fastcall SetItemImageRect(int AIndex);
	virtual void __fastcall SetItemNameRect(int AIndex);
	virtual void __fastcall SetItemDataStringRect(int AIndex);
	virtual void __fastcall InitializeItems();
	virtual void __fastcall UpdateControlAfterResize();
	virtual void __fastcall DoAppearanceChanged(System::TObject* Sender);
	virtual void __fastcall DoAddNewItem();
	virtual void __fastcall DoDblClickItem(int AIndex, TTMSFNCEditorListItem* AItem, float X, float Y);
	virtual void __fastcall DoItemsChanged(System::TObject* Sender);
	virtual void __fastcall DoSelectItemChanged(int AIndex, TTMSFNCEditorListItem* AItem, bool ASelected);
	virtual void __fastcall Draw(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawBackground(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawBitmap(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect, TTMSFNCEditorListItem* AItem);
	virtual void __fastcall DrawItems(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect, TTMSFNCEditorListCollection* AItemCollection);
	virtual void __fastcall DrawListItem(Vcl::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect, int AIndex, TTMSFNCEditorListItem* AItem);
	virtual void __fastcall SelectItem(int AIndex);
	virtual void __fastcall SelectItemInt(int AIndex);
	virtual TTMSFNCEditorListItemArray __fastcall GetSelectedItems();
	__property bool ItemsReadOnly = {read=FItemsReadOnly, write=SetItemsReadOnly, default=0};
	__property TTMSFNCEditorListViewAppearance* Appearance = {read=FAppearance, write=SetAppearance};
	__property TTMSFNCEditorListCollection* Items = {read=GetItems, write=SetItems};
	__property float DefaultItemHeight = {read=FDefaultItemHeight, write=SetDefaultItemHeight};
	__property int LastSelectedItemIndex = {read=FLastSelectedItemIndex, nodefault};
	__property bool DrawDataString = {read=FDrawDataString, write=SetDrawDataString, nodefault};
	__property bool MultiSelect = {read=FMultiSelect, write=SetMultiSelect, default=1};
	__property bool CanUnselectItems = {read=FCanUnselectItems, write=FCanUnselectItems, default=1};
	__property System::Classes::TNotifyEvent OnAddNewItem = {read=FOnAddNewItem, write=FOnAddNewItem};
	__property System::Classes::TNotifyEvent OnItemsChanged = {read=FOnItemsChanged, write=FOnItemsChanged};
	__property TTMSFNCOnDoubleClickEditorListItem OnDoubleClickItem = {read=FOnDoubleClickItem, write=FOnDoubleClickItem};
	__property TTMSFNCOnSelectItemChanged OnItemSelectedChanged = {read=FOnItemSelectedChanged, write=FOnItemSelectedChanged};
	
public:
	__fastcall virtual TTMSFNCEditorListView(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TTMSFNCEditorListView();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	virtual int __fastcall GetListHeight();
	virtual void __fastcall UpdateList();
	void __fastcall UnselectAllItems();
	void __fastcall SelectAllItems();
	virtual void __fastcall SetAcceptDrag(bool Value);
	void __fastcall StartEditMode();
	void __fastcall EndEditMode(const bool Execute);
	__property bool EditMode = {read=FEditMode, nodefault};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCEditorListView(HWND ParentWindow) : Vcl::Tmsfnccustomcontrol::TTMSFNCCustomControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCBitmapEditorListView : public TTMSFNCEditorListView
{
	typedef TTMSFNCEditorListView inherited;
	
private:
	MESSAGE void __fastcall WMDropFiles(Winapi::Messages::TMessage &Message);
	
protected:
	virtual bool __fastcall IsFileSupported(System::UnicodeString AFileName);
	
public:
	virtual void __fastcall DeleteSelectedItems();
	__property LastSelectedItemIndex;
	virtual void __fastcall SelectItem(int AIndex);
	virtual TTMSFNCEditorListItemArray __fastcall GetSelectedItems();
	
__published:
	__property DrawDataString;
	__property Appearance;
	__property Items;
	__property DefaultItemHeight = {default=0};
	__property ItemsReadOnly = {default=0};
	__property OnAddNewItem;
	__property OnItemsChanged;
	__property OnDoubleClickItem;
	__property OnItemSelectedChanged;
public:
	/* TTMSFNCEditorListView.Create */ inline __fastcall virtual TTMSFNCBitmapEditorListView(System::Classes::TComponent* AOwner) : TTMSFNCEditorListView(AOwner) { }
	/* TTMSFNCEditorListView.Destroy */ inline __fastcall virtual ~TTMSFNCBitmapEditorListView() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCBitmapEditorListView(HWND ParentWindow) : TTMSFNCEditorListView(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTMSFNCEditorList : public TTMSFNCEditorListView
{
	typedef TTMSFNCEditorListView inherited;
	
protected:
	virtual void __fastcall SetItemImageRect(int AIndex);
	
public:
	virtual TTMSFNCEditorListItemArray __fastcall GetSelectedItems();
	virtual void __fastcall SelectItem(int AIndex);
	
__published:
	__property DrawDataString;
	__property Appearance;
	__property Items;
	__property DefaultItemHeight = {default=0};
	__property ItemsReadOnly = {default=0};
	__property LastSelectedItemIndex;
	__property MultiSelect = {default=1};
	__property CanUnselectItems = {default=1};
	__property OnAddNewItem;
	__property OnItemsChanged;
	__property OnDoubleClickItem;
	__property OnItemSelectedChanged;
public:
	/* TTMSFNCEditorListView.Create */ inline __fastcall virtual TTMSFNCEditorList(System::Classes::TComponent* AOwner) : TTMSFNCEditorListView(AOwner) { }
	/* TTMSFNCEditorListView.Destroy */ inline __fastcall virtual ~TTMSFNCEditorList() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTMSFNCEditorList(HWND ParentWindow) : TTMSFNCEditorListView(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MAJ_VER = System::Int8(0x1);
static const System::Int8 MIN_VER = System::Int8(0x0);
static const System::Int8 REL_VER = System::Int8(0x1);
static const System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfnceditorlistview */
}	/* namespace Vcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_TMSFNCEDITORLISTVIEW)
using namespace Vcl::Tmsfnceditorlistview;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL)
using namespace Vcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vcl_TmsfnceditorlistviewHPP
