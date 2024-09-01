// CodeGear C++Builder
// Copyright (c) 1995, 2010 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFFolderDialog.pas' rev: 22.00

#ifndef ZffolderdialogHPP
#define ZffolderdialogHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
#include <Registry.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <FileCtrl.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zffolderdialog
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TBrowseInfoFlags { OnlyComputers, OnlyPrinters, OnlyDomains, OnlyAncestors, OnlyFileSystem, ShowPath };
#pragma option pop

typedef System::Set<TBrowseInfoFlags, TBrowseInfoFlags::OnlyComputers, TBrowseInfoFlags::ShowPath>  TBrowseInfoFlagSet;

#pragma option push -b-
enum TSHFolders { foDesktop, foDesktopExpanded, foPrograms, foControlPanel, foPrinters, foPersonal, foFavorites, foStartup, foRecent, foSendto, foRecycleBin, foStartMenu, foDesktopFolder, foMyComputer, foNetwork, foNetworkNeighborhood, foFonts, foTemplates };
#pragma option pop

class DELPHICLASS TPBFolderDialog;
typedef void __fastcall (__closure *TBrowserInitializedEvent)(TPBFolderDialog* Sender, HWND DialogHandle);

typedef void __fastcall (__closure *TSelectionChangedEvent)(TPBFolderDialog* Sender, HWND DialogHandle, const Shlobj::PItemIDList ItemIDList, const System::UnicodeString Folder);

class PASCALIMPLEMENTATION TPBFolderDialog : public Classes::TComponent
{
	typedef Classes::TComponent inherited;
	
private:
	HWND FDialogHandle;
	HWND FNewFolderHandle;
	System::UnicodeString FLabelCaption;
	HWND FParentHandle;
	System::UnicodeString FDisplayName;
	int FImageIndex;
	System::UnicodeString FFolder;
	System::UnicodeString FSelectedFolder;
	TBrowseInfoFlagSet FFlags;
	TSHFolders FRootFolder;
	bool FNewFolderVisible;
	bool FNewFolderEnabled;
	System::UnicodeString FNewFolderCaption;
	int FNewFolderWidth;
	bool FRestart;
	bool FValidPath;
	System::UnicodeString FVersion;
	System::UnicodeString FLocale;
	Classes::TStringList* FNewFolderCaptions;
	Classes::TStringList* FLabelCaptions;
	TBrowserInitializedEvent FOnInitialized;
	TSelectionChangedEvent FOnSelectionChanged;
	System::UnicodeString __fastcall LocaleText(Classes::TStringList* List);
	System::UnicodeString __fastcall MakeDisplayPath(System::UnicodeString Path, int MaxL);
	void __fastcall Dummy(System::UnicodeString Value);
	void __fastcall SetNewFolderCaption(System::UnicodeString Value);
	void __fastcall SetNewFolderEnabled(bool Value);
	void __fastcall SetNewFolderVisible(bool Value);
	void __fastcall SetNewFolderWidth(int Value);
	void __fastcall SetNewFolderCaptions(Classes::TStringList* Value);
	void __fastcall SetSelectedFolder(System::UnicodeString Value);
	void __fastcall SetFlags(TBrowseInfoFlagSet Value);
	void __fastcall SetLabelCaptions(Classes::TStringList* Value);
	void __fastcall LabelCaptionsChange(System::TObject* Sender);
	void __fastcall NewFolderCaptionsChange(System::TObject* Sender);
	
public:
	__fastcall virtual TPBFolderDialog(Classes::TComponent* AOwner);
	virtual void __fastcall Loaded(void);
	__fastcall virtual ~TPBFolderDialog(void);
	bool __fastcall Execute(void);
	void __fastcall SetSelectionPIDL(const HWND Hwnd, const Shlobj::PItemIDList ItemIDList);
	void __fastcall SetSelectionPath(const HWND Hwnd, const System::UnicodeString Path);
	void __fastcall EnableOK(const HWND Hwnd, const bool Value);
	void __fastcall GetIDListFromPath(System::UnicodeString Path, Shlobj::PItemIDList &ItemIDList);
	__property System::UnicodeString DisplayName = {read=FDisplayName};
	__property int ImageIndex = {read=FImageIndex, nodefault};
	__property HWND ParentHandle = {read=FParentHandle, write=FParentHandle, nodefault};
	__property HWND DialogHandle = {read=FDialogHandle, write=FDialogHandle, nodefault};
	__property HWND NewFolderHandle = {read=FNewFolderHandle, write=FNewFolderHandle, nodefault};
	__property System::UnicodeString SelectedFolder = {read=FSelectedFolder, write=SetSelectedFolder};
	
__published:
	__property System::UnicodeString Folder = {read=FFolder, write=FFolder};
	__property TBrowseInfoFlagSet Flags = {read=FFlags, write=SetFlags, nodefault};
	__property TSHFolders RootFolder = {read=FRootFolder, write=FRootFolder, default=1};
	__property bool NewFolderVisible = {read=FNewFolderVisible, write=SetNewFolderVisible, nodefault};
	__property bool NewFolderEnabled = {read=FNewFolderEnabled, write=SetNewFolderEnabled, nodefault};
	__property int NewFolderWidth = {read=FNewFolderWidth, write=SetNewFolderWidth, default=75};
	__property TBrowserInitializedEvent OnInitialized = {read=FOnInitialized, write=FOnInitialized};
	__property TSelectionChangedEvent OnSelectionChanged = {read=FOnSelectionChanged, write=FOnSelectionChanged};
	__property Classes::TStringList* LabelCaptions = {read=FLabelCaptions, write=SetLabelCaptions};
	__property Classes::TStringList* NewFolderCaptions = {read=FNewFolderCaptions, write=SetNewFolderCaptions};
	__property System::UnicodeString Version = {read=FVersion, write=Dummy, stored=false};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Zffolderdialog */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Zffolderdialog;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZffolderdialogHPP
