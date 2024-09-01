// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCEditorListView.pas' rev: 35.00 (Android)

#ifndef Fmx_TmsfnceditorlistviewHPP
#define Fmx_TmsfnceditorlistviewHPP

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
#include <FMX.Controls.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TMSFNCCustomControl.hpp>
#include <FMX.TMSFNCGraphics.hpp>
#include <FMX.TMSFNCGraphicsTypes.hpp>
#include <FMX.TMSFNCBitmapEditor.hpp>
#include <FMX.Types.hpp>
#include <FMX.Edit.hpp>
#include <System.UITypes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
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
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemDownStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FItemFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemSelectedFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FItemHoverFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FEditFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemSelectedStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemHoverFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FItemDownFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemHoverStroke;
	System::Classes::TNotifyEvent FOnChanged;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemDownFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* FItemSelectedFont;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FStroke;
	float FItemVerticalSpacing;
	int FItemRounding;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemImageHoverStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemImageDownFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemImageDownStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemImageSelectedFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemImageFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemImageSelectedStroke;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* FItemImageHoverFill;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* FItemImageStroke;
	int FItemImageRounding;
	bool FUseImageAppearance;
	float FItemHorizontalSpacing;
	Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides FStrokeSides;
	void __fastcall SetEditFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetItemDownFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemDownFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetItemDownStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetItemHoverFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemHoverFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetItemHoverStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemSelectedFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemSelectedFont(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* const Value);
	void __fastcall SetItemSelectedStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemVerticalSpacing(const float Value);
	void __fastcall SetItemRounding(const int Value);
	void __fastcall SetItemImageDownFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemImageDownStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemImageFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemImageHoverFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemImageHoverStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemImageSelectedFill(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* const Value);
	void __fastcall SetItemImageSelectedStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemImageStroke(Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* const Value);
	void __fastcall SetItemImageRounding(const int Value);
	void __fastcall SetUseImageAppearance(const bool Value);
	void __fastcall SetItemHorizontalSpacing(const float Value);
	void __fastcall SetStrokeSides(const Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides Value);
	
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
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* Fill = {read=FFill, write=SetItemFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* EditFont = {read=FEditFont, write=SetEditFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* Stroke = {read=FStroke, write=SetStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemHoverFill = {read=FItemHoverFill, write=SetItemHoverFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* ItemHoverFont = {read=FItemHoverFont, write=SetItemHoverFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemHoverStroke = {read=FItemHoverStroke, write=SetItemHoverStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemDownFill = {read=FItemDownFill, write=SetItemDownFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* ItemDownFont = {read=FItemDownFont, write=SetItemDownFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemDownStroke = {read=FItemDownStroke, write=SetItemDownStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemSelectedFill = {read=FItemSelectedFill, write=SetItemSelectedFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* ItemSelectedFont = {read=FItemSelectedFont, write=SetItemSelectedFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemSelectedStroke = {read=FItemSelectedStroke, write=SetItemSelectedStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemFill = {read=FItemFill, write=SetItemFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFont* ItemFont = {read=FItemFont, write=SetItemFont};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemStroke = {read=FItemStroke, write=SetItemStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemImageHoverFill = {read=FItemImageHoverFill, write=SetItemImageHoverFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemImageHoverStroke = {read=FItemImageHoverStroke, write=SetItemImageHoverStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemImageDownFill = {read=FItemImageDownFill, write=SetItemImageDownFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemImageDownStroke = {read=FItemImageDownStroke, write=SetItemImageDownStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemImageSelectedFill = {read=FItemImageSelectedFill, write=SetItemImageSelectedFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemImageSelectedStroke = {read=FItemImageSelectedStroke, write=SetItemImageSelectedStroke};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsFill* ItemImageFill = {read=FItemImageFill, write=SetItemImageFill};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsStroke* ItemImageStroke = {read=FItemImageStroke, write=SetItemImageStroke};
	__property float ItemVerticalSpacing = {read=FItemVerticalSpacing, write=SetItemVerticalSpacing};
	__property float ItemHorizontalSpacing = {read=FItemHorizontalSpacing, write=SetItemHorizontalSpacing};
	__property int ItemRounding = {read=FItemRounding, write=SetItemRounding, nodefault};
	__property int ItemImageRounding = {read=FItemImageRounding, write=SetItemImageRounding, nodefault};
	__property bool UseImageAppearance = {read=FUseImageAppearance, write=SetUseImageAppearance, nodefault};
	__property Fmx::Tmsfncgraphicstypes::TTMSFNCGraphicsSides StrokeSides = {read=FStrokeSides, write=SetStrokeSides, nodefault};
	
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
	Fmx::Tmsfnctypes::TTMSFNCBitmap* FBitmap;
	NativeInt FTag;
	System::UnicodeString FName;
	System::TObject* FDataObject;
	System::UnicodeString FDataString;
	System::Classes::TNotifyEvent FOnChanged;
	float FItemHeight;
	bool FSelected;
	System::Uitypes::TAlphaColor FFontColor;
	System::Uitypes::TAlphaColor FSelectedFontColor;
	void __fastcall SetBitmap(Fmx::Tmsfnctypes::TTMSFNCBitmap* const Value);
	void __fastcall SetDataObject(System::TObject* const Value);
	void __fastcall SetDataString(const System::UnicodeString Value);
	void __fastcall SetName(const System::UnicodeString Value);
	void __fastcall DoChanged();
	void __fastcall DoSelectItemChanged(int AIndex, TTMSFNCEditorListItem* AItem, bool ASelected);
	void __fastcall SetItemHeight(const float Value);
	void __fastcall SetSelected(const bool Value);
	void __fastcall SetFontColor(const System::Uitypes::TAlphaColor Value);
	void __fastcall SetSelectedFontColor(const System::Uitypes::TAlphaColor Value);
	
public:
	__fastcall virtual TTMSFNCEditorListItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TTMSFNCEditorListItem();
	virtual void __fastcall DoBitmapChanged(System::TObject* Sender);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::Types::TRectF DrawRect = {read=FDrawRect};
	
__published:
	__property Fmx::Tmsfnctypes::TTMSFNCBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property System::TObject* DataObject = {read=FDataObject, write=SetDataObject};
	__property System::UnicodeString DataString = {read=FDataString, write=SetDataString};
	__property NativeInt Tag = {read=FTag, write=FTag};
	__property System::Classes::TNotifyEvent OnChanged = {read=FOnChanged, write=FOnChanged};
	__property float ItemHeight = {read=FItemHeight, write=SetItemHeight};
	__property System::Uitypes::TAlphaColor FontColor = {read=FFontColor, write=SetFontColor, nodefault};
	__property System::Uitypes::TAlphaColor SelectedFontColor = {read=FSelectedFontColor, write=SetSelectedFontColor, nodefault};
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

class PASCALIMPLEMENTATION TTMSFNCEditorListView : public Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl
{
	typedef Fmx::Tmsfnccustomcontrol::TTMSFNCCustomControl inherited;
	
private:
	bool FItemsChanged;
	int FLastSelectedItemIndex;
	int FKeyIndex;
	Fmx::Tmsfncbitmapeditor::TTMSFNCBitmapEditor* FBitmapEditor;
	Fmx::Edit::TEdit* FEdit;
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
	virtual void __fastcall DoEditKeyDown(System::TObject* Sender, System::Word &Key, System::WideChar &KeyChar, System::Classes::TShiftState Shift);
	virtual void __fastcall HandleKeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall HandleKeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall HandleKeyPress(System::WideChar &Key);
	virtual void __fastcall HandleMouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseEnter();
	virtual void __fastcall HandleMouseLeave();
	virtual void __fastcall HandleMouseMove(System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall HandleMouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, float X, float Y);
	virtual void __fastcall DragOver(const Fmx::Types::TDragObject &Data, const System::Types::TPointF &Point, Fmx::Types::TDragOperation &Operation);
	virtual void __fastcall DragDrop(const Fmx::Types::TDragObject &Data, const System::Types::TPointF &Point);
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
	virtual void __fastcall Draw(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawBackground(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect);
	virtual void __fastcall DrawBitmap(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect, TTMSFNCEditorListItem* AItem);
	virtual void __fastcall DrawItems(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect, TTMSFNCEditorListCollection* AItemCollection);
	virtual void __fastcall DrawListItem(Fmx::Tmsfncgraphics::TTMSFNCGraphics* AGraphics, const System::Types::TRectF &ARect, int AIndex, TTMSFNCEditorListItem* AItem);
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
	void __fastcall StartEditMode();
	void __fastcall EndEditMode(const bool Execute);
	__property bool EditMode = {read=FEditMode, nodefault};
};


class PASCALIMPLEMENTATION TTMSFNCBitmapEditorListView : public TTMSFNCEditorListView
{
	typedef TTMSFNCEditorListView inherited;
	
protected:
	virtual bool __fastcall IsFileSupported(System::UnicodeString AFileName);
	virtual void __fastcall DragOver(const Fmx::Types::TDragObject &Data, const System::Types::TPointF &Point, Fmx::Types::TDragOperation &Operation);
	virtual void __fastcall DragDrop(const Fmx::Types::TDragObject &Data, const System::Types::TPointF &Point);
	
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
	
};


//-- var, const, procedure ---------------------------------------------------
static constexpr System::Int8 MAJ_VER = System::Int8(0x1);
static constexpr System::Int8 MIN_VER = System::Int8(0x0);
static constexpr System::Int8 REL_VER = System::Int8(0x1);
static constexpr System::Int8 BLD_VER = System::Int8(0x0);
}	/* namespace Tmsfnceditorlistview */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCEDITORLISTVIEW)
using namespace Fmx::Tmsfnceditorlistview;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfnceditorlistviewHPP
