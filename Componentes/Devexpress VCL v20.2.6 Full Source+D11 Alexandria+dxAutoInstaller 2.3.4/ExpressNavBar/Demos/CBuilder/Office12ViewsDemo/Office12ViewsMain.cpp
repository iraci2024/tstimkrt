//---------------------------------------------------------------------------
#include <vcl.h>
#include <shlobj.h>
#pragma hdrstop

#include "Office12ViewsMain.h"
#include "dxNavBarViewsFact.hpp"
#include "dxNavBarOffice12Views.hpp"
#include "dxNavBarSkinBasedViews.hpp"
#include "cxGeometry.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxNavBar"
#pragma link "dxNavBarBase"
#pragma link "dxNavBarCollns"

#include <dstring.h>
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "dxNavBarGroupItems"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxContainer"
#pragma link "cxEdit"
#pragma link "cxGroupBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxTextEdit"
#pragma link "dxNavBarOfficeNavigationBar"
#pragma resource "*.dfm"
TfmMain *fmMain;
//DECLARE_DINTERFACE_TYPE(IdxNavBarColorSchemes);
//---------------------------------------------------------------------------
__fastcall TfmMain::TfmMain(TComponent* Owner)
        : TForm(Owner)
{
}

//---------------------------------------------------------------------------
// PIDL Manipulation
//---------------------------------------------------------------------------

void __fastcall DisposePIDL(PItemIDList ID)
{
        LPMALLOC Malloc;

        if (ID != NULL)
        {
                OleCheck(SHGetMalloc(&Malloc));
                Malloc->Free(ID);
        }
}

PItemIDList __fastcall CopyITEMID(LPMALLOC Malloc, PItemIDList ID)
{
        void * Result = Malloc->Alloc(ID->mkid.cb + sizeof(ID->mkid.cb));
        CopyMemory(Result, ID, ID->mkid.cb + sizeof(ID->mkid.cb));
        return (PItemIDList)Result;
}
//---------------------------------------------------------------------------

PItemIDList __fastcall NextPIDL(PItemIDList IDList)
{
		PItemIDList Result = IDList;
        Result = PItemIDList(PCHAR(Result) + IDList->mkid.cb);
        return Result;
}
//---------------------------------------------------------------------------

int __fastcall GetPIDLSize(PItemIDList IDList)
{
        int Result = 0;
        if (IDList != NULL)
        {
                Result = sizeof(IDList->mkid.cb);
                while (IDList->mkid.cb != 0)
                {
                        Result = Result + IDList->mkid.cb;
                        IDList = NextPIDL(IDList);
                }
        }
        return Result;
}
//---------------------------------------------------------------------------

PItemIDList __fastcall StripLastID(PItemIDList IDList)
{
        PItemIDList MarkerID;

        MarkerID = IDList;
        if (IDList != NULL)
		{
                while (IDList->mkid.cb != 0)
                {
                        MarkerID = IDList;
						IDList = NextPIDL(IDList);
                }
                MarkerID->mkid.cb = 0;
        }
        return MarkerID;
}
//---------------------------------------------------------------------------

PItemIDList __fastcall CreatePIDL(int Size)
{
        LPMALLOC Malloc;
        HRESULT HR;
        void * Result;

        Result = NULL;


        if (!FAILED(SHGetMalloc(&Malloc)))
        {
                Result = Malloc->Alloc(Size);
                if ((PItemIDList)Result != NULL)
                {
                        memset(Result,0, sizeof(Result));
                }

        }
        return (PItemIDList) Result;
}
//---------------------------------------------------------------------------

PItemIDList __fastcall CopyPIDL(PItemIDList IDList)
{
		int Size;
        PItemIDList Result;

        Size = GetPIDLSize(IDList);
        Result = CreatePIDL(Size);
        if (Result != NULL)
        {
                CopyMemory(Result, IDList, Size);
        }
        return Result;
}
//---------------------------------------------------------------------------

PItemIDList __fastcall ConcatPIDLs(PItemIDList IDList1, PItemIDList IDList2)
{
        int cb1;
        int cb2;
        PItemIDList Result;

        if (IDList1 != NULL)
                cb1 = GetPIDLSize(IDList1) - sizeof(IDList1->mkid.cb);
        else
                cb1 = 0;

        cb2 = GetPIDLSize(IDList2);

        Result = CreatePIDL(cb1 + cb2);
        if (Result != NULL)
		{
                if (IDList1 != NULL)
                        CopyMemory(Result, IDList1, cb1);
                CopyMemory(PCHAR(Result) + cb1, IDList2, cb2);
        }
        return Result;
}
//---------------------------------------------------------------------------
// Shell Folder Item Info
//---------------------------------------------------------------------------

int __fastcall GetShellImage(PItemIDList PIDL, bool Large, bool Open)
{
        TSHFileInfoA FileInfo;
        int Flags;
        int Result;

        memset(&FileInfo, 0, sizeof(FileInfo));
        Flags = SHGFI_PIDL | SHGFI_SYSICONINDEX | SHGFI_ICON;

        if (Open)
                Flags = Flags | SHGFI_OPENICON;
        if (Large)
                Flags = Flags | SHGFI_LARGEICON;
        else
                Flags = Flags | SHGFI_SMALLICON;

        SHGetFileInfoA(PCHAR(PIDL), 0, &FileInfo, sizeof(FileInfo), Flags);
        Result = FileInfo.iIcon;
        return Result;
}
//---------------------------------------------------------------------------

bool IsFolder(PShellItem * ShellItem)
{
        if (ShellItem->TypeName == "File Folder")
                return true;
        else
                return false;
}
//---------------------------------------------------------------------------
bool IsFolder(LPSHELLFOLDER ShellFolder, PItemIDList ID)
{
        unsigned long Flags;
        bool Result;

        Flags = SFGAO_FOLDER;

        ShellFolder->GetAttributesOf(1, (const _ITEMIDLIST **) &ID, &Flags);
        Result = ((SFGAO_FOLDER & Flags) !=0);
        
        return Result;
}
//---------------------------------------------------------------------------

bool __fastcall ValidFileTime(TFileTime FileTime)
{
        bool Result = ((FileTime.dwLowDateTime != 0) || (FileTime.dwHighDateTime != 0));
        return Result;
}
//---------------------------------------------------------------------------

int __fastcall ListSortFunc(void * Item1, void * Item2)
{
		return Smallint(fmMain->ShellFolder->CompareIDs(0,
                 ((PShellItem *)Item1)->ID, ((PShellItem *)Item2)->ID));
}
//---------------------------------------------------------------------------

AnsiString __fastcall GetDisplayName(LPSHELLFOLDER ShellFolder, PItemIDList PIDL,
                       bool ForParsing)
{

        TStrRet StrRet;
		PCHAR P;
        int Flags;
        AnsiString * Result;

        if (ForParsing)
        {
                Flags = SHGDN_FORPARSING;
        }
        else
                Flags = SHGDN_NORMAL;

        ShellFolder->GetDisplayNameOf(PIDL, Flags, &StrRet);

        switch (StrRet.uType)
        {
				case STRRET_CSTR:       Result = new AnsiString(StrRet.cStr, strlen(StrRet.cStr));
                                        break;
				case STRRET_OFFSET:     P = (PCHAR)&PIDL->mkid.abID[StrRet.uOffset - sizeof(PIDL->mkid.cb)];
                                        Result = new AnsiString(P);
                                        break;
                case STRRET_WSTR:       Result = new AnsiString(StrRet.pOleStr);
                                        break;
        }
        return (*Result);
}
//--------------------------------------------------------------------------
//      ShellItem Constructor.
//--------------------------------------------------------------------------
PShellItem::PShellItem()
{
        FullID = NULL;
        ID = NULL;
        Empty = true;
        DisplayName = "";
        TypeName = "";
        ModDate = "";
        ImageIndex = 0;
        Size = 0;
        Attributes = 0;
}

void __fastcall TfmMain::FormCreate(TObject *Sender)
{
		cxGroupBox2->Parent = NULL;
		dxNavBar1->Parent = NULL;
		dxNavBar2->Parent = NULL;
		SHFILEINFOA FileInfo;
		THandle ImageListHandle;
		PItemIDList NewPIDL;
		OleCheck(SHGetDesktopFolder(&FDesktopFolder));
		FIDList = new TList();
		ilMainSmall->Handle = SHGetFileInfoA("C:\\", 0, &FileInfo, sizeof(FileInfo),
		  SHGFI_SYSICONINDEX | SHGFI_SMALLICON);
		ilMainLarge->Handle = SHGetFileInfoA("C:\\", 0, &FileInfo, sizeof(FileInfo),
		  SHGFI_SYSICONINDEX | SHGFI_LARGEICON);
		OleCheck(SHGetSpecialFolderLocation(Application->Handle, CSIDL_DRIVES, &NewPIDL));
		SetPath(NewPIDL);
		PopulateMyFavoritesList(GetIDBySpetialFolder(CSIDL_FAVORITES));
		PopulateMyComputerTree(NewPIDL);
}
//---------------------------------------------------------------------------

LPSHELLFOLDER __fastcall TfmMain::GetShellFolder()
{
        return GetShellFolderByID(FShellID);
}
//---------------------------------------------------------------------------
Integer __fastcall TfmMain::GetShellItemCount()
{
        return FIDList->Count;
}
//---------------------------------------------------------------------------
PShellItem* __fastcall TfmMain::GetShellItem(Integer Index)
{
       return (PShellItem *)FIDList->Items[Index];
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::ClearIDList()
{
		for (int I = 0; I <= FIDList->Count-1; I++)
		{
				DisposePIDL(ShellItems[I]->ID);
				FIDList->Delete(I);
		}
		FIDList->Clear();

}

bool __fastcall TfmMain::SwitchOption(TObject *Sender, bool AValue)
{
  const int UncheckImage = -1;
  const int CheckImage = 14;
  const int AImageIndex[] = {UncheckImage, CheckImage};
  bool AResult = !AValue;
  ((TAction*)Sender)->ImageIndex = AImageIndex[AResult];
  return AResult;
}
//---------------------------------------------------------------------------
PItemIDList __fastcall TfmMain::GetIDByPath(AnsiString APath)
{
        wchar_t * P;
        PItemIDList NewPIDL;
        unsigned long int Flags;
        unsigned long int NumChars;

        NumChars = APath.Length();
        Flags = 0;
        P = StringToOleStr(APath);

        OleCheck( FDesktopFolder->ParseDisplayName( Application->Handle, NULL,
                P, &NumChars, &NewPIDL, &Flags));
        return NewPIDL;
}
//---------------------------------------------------------------------------
PItemIDList __fastcall TfmMain::GetIDBySpetialFolder(Integer ASpetialFolder)
{
        PItemIDList NewPIDL;
        OleCheck( SHGetSpecialFolderLocation(Application->Handle, ASpetialFolder, &NewPIDL));
        return NewPIDL;
}
//---------------------------------------------------------------------------
LPSHELLFOLDER __fastcall TfmMain::GetShellFolderByID(PItemIDList AID)
{
        LPSHELLFOLDER NewShellFolder;
        if (AID != NULL)
                OleCheck(FDesktopFolder->BindToObject(AID, NULL, IID_IShellFolder,
                        (void **)&NewShellFolder));
        else NewShellFolder = NULL;
        return NewShellFolder;

}
//---------------------------------------------------------------------------
LPENUMIDLIST __fastcall TfmMain::GetEnumIDListByFolder(LPSHELLFOLDER AFolder)
{

        UINT Flags = SHCONTF_FOLDERS | SHCONTF_NONFOLDERS | SHCONTF_INCLUDEHIDDEN;
        LPENUMIDLIST EnumList;
        if (AFolder != NULL)
                OleCheck( AFolder->EnumObjects( Application->Handle, Flags, &EnumList));
        else EnumList = NULL;
        return EnumList;

}
//---------------------------------------------------------------------------
bool __fastcall TfmMain::CompareNames(AnsiString Path, AnsiString Pattern)
{
  int APos;
  AnsiString S, Name, Extention, PatName, PatExt;
  S = Path;
  do
  {
    APos = S.Pos("\\");
    if (APos > 0) S.Delete(1, APos);
  }
  while (APos != 0);
  APos = S.Pos(".");
  if (APos > 0)
  {
    Name = S;
    Name = UpperCase(Name.Delete(APos, Name.Length() - 1));
    Extention = UpperCase(S.Delete(1, APos));
  }
  else
  {
    Name = UpperCase(S);
    Extention = "";
  }
  Pattern = UpperCase(Pattern);
  APos = Pattern.Pos(".");
  if (APos > 0)
  {
    S = Pattern;
    PatName = S.Delete(APos, S.Length() - 1);
    PatExt = Pattern.Delete(1, APos);
  }
  else
  {
    PatName = Pattern;
    PatExt = "";
  }
  return (((Name == PatName) || (PatName == "*")) &&
    ((Extention == PatExt) || (PatExt == "*") || (PatExt == ""))) ||
    ((PatExt == "") && (PatName != "") && (Name.Pos(PatName) > 0));
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::SetSearch(PItemIDList AID, AnsiString Pattern)
{
  lvMain->Items->BeginUpdate();
  try
  {
    FSearching = true;
	actSearch->Caption = "Stop";
	try
	{
	  PopulateSearchIDList(AID, Pattern);
	  if (lvMain->Items->Count > 0)
	  {
		lvMain->Selected = lvMain->Items->Item[0];
		lvMain->Selected->Focused = true;
		lvMain->Selected->MakeVisible(false);
	  }
	}
	__finally
	{
	  StatusBar1->SimpleText = "";
      actSearch->Caption = "Search";
      FSearching = false;
    }
  }
  __finally
  {
    lvMain->Items->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::SetPath(const AnsiString Value)
{
  LPITEMIDLIST NewPIDL = GetIDByPath(Value);
  SetPath(NewPIDL);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::SetPath(LPITEMIDLIST ID)
{
        lvMain->Items->BeginUpdate();
        try
        {
                PopulateIDList(ID);
                if (lvMain->Items->Count > 0)
                {
                        lvMain->Selected = lvMain->Items->Item[0];
                        lvMain->Selected->Focused = true;
                        lvMain->Selected->MakeVisible(false);
                }
        }
        __finally
        {
                lvMain->Items->EndUpdate();
        }
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::PopulateIDList(PItemIDList AID)
{
  PShellItem * ShellItem;
  unsigned long NumIDs;
  PItemIDList ID;

  LPSHELLFOLDER AShellFolder = GetShellFolderByID(AID);
  TCursor SaveCursor = Screen->Cursor;
  try
  {
    Screen->Cursor = crHourGlass;
    ClearIDList();
    LPENUMIDLIST EnumList = GetEnumIDListByFolder(AShellFolder);

    FShellID = AID;
    FSearchShellID = NULL;
    while (EnumList->Next(1, &ID, &NumIDs) == S_OK)
    {
      ShellItem = new PShellItem;
      ShellItem->ID = ID;
      ShellItem->ParentID = AID;
      ShellItem->ShellFolder = ShellFolder;
      ShellItem->DisplayName = GetDisplayName(AShellFolder, ID, false);
      ShellItem->Empty = true;
      FIDList->Add(ShellItem);
    }
    FIDList->Sort(ListSortFunc);
  }
  __finally
  {
    lvMain->Items->Count = ShellItemCount;
    lvMain->Repaint();
    Screen->Cursor = SaveCursor;
  }

}
//---------------------------------------------------------------------------
void __fastcall TfmMain::CheckFolder(PItemIDList AID, AnsiString Pattern)
{
  PShellItem * ShellItem;
  unsigned long NumIDs;
  PItemIDList ID;

  LPSHELLFOLDER AFolder = GetShellFolderByID(AID);
  LPENUMIDLIST EnumList = GetEnumIDListByFolder(AFolder);
  while (EnumList->Next(1, &ID, &NumIDs) == S_OK)
  {
    if (CompareNames(GetDisplayName(AFolder, ID, true), Pattern))
    {
      ShellItem = new PShellItem;
      ShellItem->ID = ID;
      ShellItem->ParentID = AID;
      ShellItem->ShellFolder = AFolder;
      ShellItem->DisplayName = GetDisplayName(AFolder, ID, false);
      ShellItem->Empty = true;
      FIDList->Add(ShellItem);
    }
    Application->ProcessMessages();
    if (!FSearching) return;

    if (IsFolder(AFolder, ID))
    {
      StatusBar1->SimpleText = Format((String)"Search in %s ...",
        ARRAYOFCONST((GetDisplayName(AFolder, ID, false))));
      CheckFolder(ConcatPIDLs(AID, ID), Pattern);
    }
  }
  lvMain->Items->Count = ShellItemCount;
  lvMain->Repaint();
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::PopulateSearchIDList(PItemIDList ASearchID, AnsiString Pattern)
{
  FSearchShellID = ASearchID;
  ClearIDList();
  CheckFolder(ASearchID, Pattern);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::PopulateMyFavoritesList(PItemIDList AID)
{
  unsigned long NumIDs;
  PItemIDList ID;

  LPSHELLFOLDER AShellFolder = GetShellFolderByID(AID);
  lvMyFavorites->Items->BeginUpdate();
  TCursor SaveCursor = Screen->Cursor;
  try
  {
    Screen->Cursor = crHourGlass;
    LPENUMIDLIST EnumList = GetEnumIDListByFolder(AShellFolder);
    while (EnumList->Next(1, &ID, &NumIDs) == S_OK)
    {
      TListItem* AItem = lvMyFavorites->Items->Add();
      PItemIDList FullID = ConcatPIDLs(AID, ID);
      AItem->Caption = GetDisplayName(AShellFolder, ID, false);
      AItem->ImageIndex = GetShellImage(FullID, false, false);

      TSHFileInfoA FileInfo;
      SHGetFileInfoA(PCHAR(FullID), 0, &FileInfo, sizeof(FileInfo), SHGFI_TYPENAME | SHGFI_PIDL);
      AItem->SubItems->Add(FileInfo.szTypeName);
      if (IsFolder(AShellFolder, ID))
        AItem->Data = FullID;
      else AItem->Data = NULL;
    }
  }
  __finally
  {
    lvMyFavorites->Repaint();
    lvMyFavorites->Items->EndUpdate();
    Screen->Cursor = SaveCursor;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::PopulateMyComputerTree(PItemIDList AID)
{
  unsigned long NumIDs;
  PItemIDList ID;

  TTreeNode* ANode = tvMyComputer->Items->Add(NULL, "MyComputer");
  ANode->ImageIndex = GetShellImage(AID, false, false);
  ANode->SelectedIndex = GetShellImage(AID, false, false);
  ANode->Data = AID;
  LPSHELLFOLDER AShellFolder = GetShellFolderByID(AID);
  tvMyComputer->Items->BeginUpdate();
  TCursor SaveCursor = Screen->Cursor;
  try
  {
    Screen->Cursor = crHourGlass;
    LPENUMIDLIST EnumList = GetEnumIDListByFolder(AShellFolder);
    while (EnumList->Next(1, &ID, &NumIDs) == S_OK)
    {
      TTreeNode* AItemNode = tvMyComputer->Items->AddChild(ANode, GetDisplayName(AShellFolder, ID, false));
      PItemIDList FullID = ConcatPIDLs(AID, ID);
      AItemNode->ImageIndex = GetShellImage(FullID, false, false);
      AItemNode->SelectedIndex = GetShellImage(FullID, false, false);
      if (IsFolder(AShellFolder, ID))
        AItemNode->Data = FullID;
      else AItemNode->Data = NULL;
    }
  }
  __finally
  {
//    tvMyComputer->SortType = (TSortType)stText;
    tvMyComputer->Items->EndUpdate();
    Screen->Cursor = SaveCursor;
  }
  tvMyComputer->FullExpand();
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::CheckShellItems(int StartIndex, int EndIndex)
{
        TWin32FindData FileData;
        TSHFileInfoA FileInfo;
        TSystemTime SysTime;
        int I;
        TFileTime LocalFileTime;

        for(I = StartIndex; I <= EndIndex; I++)
        {
                if (GetShellItem(I)->Empty)
                {
                        GetShellItem(I)->FullID = ConcatPIDLs(GetShellItem(I)->ParentID, GetShellItem(I)->ID);
                        GetShellItem(I)->ImageIndex = GetShellImage(GetShellItem(I)->FullID, (lvMain->ViewStyle == vsIcon), false);

                        SHGetFileInfoA(PCHAR(GetShellItem(I)->FullID), 0, &FileInfo, sizeof(FileInfo), SHGFI_TYPENAME | SHGFI_PIDL);
                        GetShellItem(I)->TypeName = FileInfo.szTypeName;

                        memset(&FileData, 0, sizeof(FileData));
                        SHGetDataFromIDList(ShellFolder, GetShellItem(I)->ID, SHGDFIL_FINDDATA, &FileData, sizeof(FileData));

                        GetShellItem(I)->Size = FileData.nFileSizeLow/1000;
						if (GetShellItem(I)->Size == 0)
                                GetShellItem(I)->Size = 1;

                        memset(&LocalFileTime, 0, sizeof(LocalFileTime));
                        if (ValidFileTime(FileData.ftLastWriteTime)&&
                                FileTimeToLocalFileTime(&(FileData.ftLastWriteTime),
                                &LocalFileTime)&&
                                FileTimeToSystemTime(&LocalFileTime, &SysTime))
                        {
                                try
                                {
                                        GetShellItem(I)->ModDate = DateTimeToStr(SystemTimeToDateTime(SysTime));
                                }
                                catch(EConvertError &E)
                                {
                                        GetShellItem(I)->ModDate = "";
                                }
                        }
                        else
                                GetShellItem(I)->ModDate = "";

                        GetShellItem(I)->Attributes = FileData.dwFileAttributes;

                        GetShellItem(I)->Empty = False;
                }
        }
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::lvMainCustomDrawItem(
      TCustomListView *Sender, TListItem *Item, TCustomDrawState State,
      bool &DefaultDraw)
{
        int Attrs = 0;
        if ((Item != NULL)&(Item->Index < FIDList->Count))
        {
                Attrs = GetShellItem(Item->Index)->Attributes;
        }
        if (BOOL(Attrs & FILE_ATTRIBUTE_READONLY))
                lvMain->Canvas->Font->Color = clGrayText;
        if (BOOL(Attrs & FILE_ATTRIBUTE_HIDDEN))
                lvMain->Canvas->Font->Style = lvMain->Canvas->Font->Style << fsStrikeOut;
        if (BOOL(Attrs & FILE_ATTRIBUTE_SYSTEM))
                lvMain->Canvas->Font->Color = clHighlight;

}
//---------------------------------------------------------------------------

void __fastcall TfmMain::lvMainCustomDrawSubItem(
      TCustomListView *Sender, TListItem *Item, int SubItem,
      TCustomDrawState State, bool &DefaultDraw)
{
       if (SubItem != 0)
                lvMain->Canvas->Font->Color = (TColor)GetSysColor(COLOR_WINDOWTEXT);

}
//---------------------------------------------------------------------------

void __fastcall TfmMain::lvMainData(TObject *Sender,
      TListItem *Item)
{
        AnsiString Attrs;
        PShellItem * ShellItem;

		if ((Item->Index < FIDList->Count))
        {

                ShellItem = GetShellItem(Item->Index);
                Item->Caption = ShellItem->DisplayName;
                Item->ImageIndex = ShellItem->ImageIndex;

                if (lvMain->ViewStyle == vsReport)
                {
                        if (!(IsFolder(ShellFolder, ShellItem->ID)))
                                Item->SubItems->Add(Format((String)"%dKB", ARRAYOFCONST((ShellItem->Size))));
                        else
                                Item->SubItems->Add("");

                        Item->SubItems->Add(ShellItem->TypeName);

                        Item->SubItems->Add(ShellItem->ModDate);

                        if (BOOL(ShellItem->Attributes & FILE_ATTRIBUTE_READONLY))
                                Attrs = Attrs + 'R';
                        if (BOOL(ShellItem->Attributes & FILE_ATTRIBUTE_HIDDEN))
                                Attrs = Attrs + 'H';
                        if (BOOL(ShellItem->Attributes & FILE_ATTRIBUTE_SYSTEM))
                                Attrs = Attrs + 'S';
                        if (BOOL(ShellItem->Attributes & FILE_ATTRIBUTE_ARCHIVE))
                                Attrs = Attrs + 'A';

                        Item->SubItems->Add(Attrs);
                }
        }
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::lvMainDataFind(TObject *Sender,
      TItemFind Find, const String FindString,
      const TPoint &FindPosition, Pointer FindData, int StartIndex,
      TSearchDirection Direction, bool Wrap, int &Index)
{
        bool Found;
        PShellItem * ShellItem;

        int I = StartIndex;

        if ((Find == ifExactString) || (Find == ifPartialString))
        {
                do
                {
                        ShellItem = GetShellItem(I);
                        if (I == FIDList->Count-1)
                        {
                                if (Wrap)
                                        I = 0;
                                else
                                        break;
                        }
                        Found = (UpperCase(ShellItem->DisplayName)).Pos(UpperCase(FindString)) == 1;
                        I++;
                }while (!Found && (I != StartIndex));
                if (Found)
                        Index = I-1;

        }

}
//---------------------------------------------------------------------------

void __fastcall TfmMain::lvMainDataHint(TObject *Sender,
      int StartIndex, int EndIndex)
{
        if ((StartIndex < FIDList->Count) && (EndIndex < FIDList->Count))
                CheckShellItems(StartIndex, EndIndex);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::lvMainDblClick(TObject *Sender)
{
  if (lvMain->Selected != NULL)
  {
    PItemIDList AID = ShellItems[lvMain->Selected->Index]->ID;
    PItemIDList AParentID = ShellItems[lvMain->Selected->Index]->ParentID;
    if (FSearchShellID == NULL)
    {
      LPSHELLFOLDER AShellFolder = ShellItems[lvMain->Selected->Index]->ShellFolder;
      if (IsFolder(AShellFolder, AID)) SetPath(ConcatPIDLs(AParentID, AID));
    }
    else SetPath(AParentID);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::lvMainKeyDown(TObject *Sender,
      WORD &Key, TShiftState Shift)
{
        switch(Key)
        {
                case VK_RETURN:         lvMainDblClick(Sender);
										break;
                case VK_BACK:           btnBackClick(Sender);
                                        break;
        }
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::btnBackClick(TObject *Sender)
{
  if (FSearchShellID == NULL)
  {
    PItemIDList Temp = CopyPIDL(FShellID);
    if (Temp != NULL)
      StripLastID(Temp);
    if (Temp->mkid.cb != 0)
      SetPath(Temp);
    else Beep();
  }
  else SetPath(FSearchShellID);
  CloseNavBarPopup();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::btnLargeIconsClick(TObject *Sender)
{
  lvMain->ViewStyle = (TViewStyle)(((TComponent*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::FormDestroy(TObject *Sender)
{
  ClearIDList();
  FIDList->Free();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::Form1Close(TObject *Sender,
	  TCloseAction &Action)
{
  FSearching = false;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::btnSearchClick(TObject *Sender)
{
  if (!FSearching)
	SetSearch(FShellID, edSearch->Text);
  else FSearching = false;
  CloseNavBarPopup();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::tvMyComputerClick(TObject *Sender)
{
   if (tvMyComputer->Selected != NULL && tvMyComputer->Selected->Data != NULL)
	 SetPath(PItemIDList(tvMyComputer->Selected->Data));
   CloseNavBarPopup();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::lvMyFavoritesClick(TObject *Sender)
{
  if (lvMyFavorites->Selected != NULL && lvMyFavorites->Selected->Data != NULL)
	 SetPath(PItemIDList(lvMyFavorites->Selected->Data));
  CloseNavBarPopup();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::nbMainDesktopClick(TObject *Sender)
{
  SetPath(GetIDBySpetialFolder(CSIDL_DESKTOPDIRECTORY));
  CloseNavBarPopup();
}

void __fastcall TfmMain::nbMainMyDocumentsClick(TObject *Sender)
{
  SetPath(GetIDBySpetialFolder(CSIDL_MYDOCUMENTS));
  CloseNavBarPopup();
}

void __fastcall TfmMain::nbMainNetworkClick(TObject *Sender)
{
  SetPath(GetIDBySpetialFolder(CSIDL_NETWORK));
  CloseNavBarPopup();
}

void __fastcall TfmMain::actSchemeExecute(TObject *Sender)
{
  _di_IdxNavBarColorSchemes AColorSchemes;
  AnsiString AColorSchemeName;

  bgColorScheme->SelectedLinkIndex = ((TComponent*)Sender)->Tag;
  TStringList *AColorSchemeList = new TStringList();
  try
  {
	  AColorSchemes = GetNavBarColorSchemes(nbMain->ViewStyle);
	  AColorSchemes->PopulateNames(AColorSchemeList);
	  AColorSchemeName = AColorSchemeList->Strings[((TComponent*)Sender)->Tag];
	  AColorSchemes->SetName(AColorSchemeName);
	  AColorSchemes = GetNavBarColorSchemes(nbMyComputer->ViewStyle);
	  AColorSchemes->SetName(AColorSchemeName);
	  AColorSchemes = GetNavBarColorSchemes(nbOptions->ViewStyle);
	  AColorSchemes->SetName(AColorSchemeName);
  }
  __finally
  {
	  delete AColorSchemeList;
  }
}

void __fastcall TfmMain::actLargeIconsExecute(TObject *Sender)
{
  lvMain->ViewStyle = (TViewStyle)(((TComponent*)Sender)->Tag);
  CloseNavBarPopup();
}

void __fastcall TfmMain::nbOptionsAdjustWidthByPopupClick(TObject *Sender)
{
  nbMain->OptionsBehavior->NavigationPane->AdjustWidthByPopup =
	SwitchOption(Sender, nbMain->OptionsBehavior->NavigationPane->AdjustWidthByPopup);
}

void __fastcall TfmMain::nbOptionsAllowCustomizeClick(TObject *Sender)
{
  nbMain->OptionsBehavior->NavigationPane->AllowCustomizing =
	SwitchOption(Sender, nbMain->OptionsBehavior->NavigationPane->AllowCustomizing);
}

void __fastcall TfmMain::nbOptionsCollapsibleClick(TObject *Sender)
{
  nbMain->OptionsBehavior->NavigationPane->Collapsible =
	SwitchOption(Sender, nbMain->OptionsBehavior->NavigationPane->Collapsible);
}

void __fastcall TfmMain::nbOptionsTabStopClick(TObject *Sender)
{
  nbMain->TabStop = SwitchOption(Sender, nbMain->TabStop);
  nbMyComputer->TabStop = SwitchOption(Sender, nbMyComputer->TabStop);
  nbOptions->TabStop = SwitchOption(Sender, nbOptions->TabStop);
  if (!nbOptions->TabStop)lvMain->SetFocus();
}

void __fastcall TfmMain::CloseNavBarPopup()
{
	((TdxNavBarOffice12NavPanePainter *)(nbMain->Painter))->Controller->ClosePopupControl();
}
void __fastcall TfmMain::tvMyComputerAdvancedCustomDraw(TCustomTreeView *Sender,
      const TRect &ARect, TCustomDrawStage Stage, bool &DefaultDraw)
{
  bgMyComputerControl->DrawSizeGrip(Sender->Canvas, bgMyComputerControl->GetSizeGripRect(Sender));	
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::tvMyComputerMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  TControl *AControl = (TControl*)Sender;
  TRect ASizeGripRect = bgMyComputerControl->GetSizeGripRect(AControl);
  TPoint APoint = Point(X, Y);
  if (cxRectPtIn(ASizeGripRect, APoint))
	{ bgMyComputerControl->BeginResize(AControl, Button, Shift, APoint); }
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::tvMyComputerMouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
  TControl *AControl = (TControl*)Sender;
  TRect ASizeGripRect = bgMyComputerControl->GetSizeGripRect(AControl);
  TPoint APoint = Point(X, Y);
  if (cxRectPtIn(ASizeGripRect, APoint))
	AControl->Cursor = crSizeWE;
  else
	AControl->Cursor = crDefault;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::cxTextEdit1PropertiesChange(TObject *Sender)
{
  if (FLockSearchTextChange)
    return;
  FLockSearchTextChange = true;
  edSearch->Text = cxTextEdit1->Text;
  FLockSearchTextChange = false;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::edSearchPropertiesChange(TObject *Sender)
{
  if (FLockSearchTextChange)
    return;
  FLockSearchTextChange = true;
  cxTextEdit1->Text = edSearch->Text;
  FLockSearchTextChange = false;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::dxNavBarOfficeNavigationBar1QueryPeekFormContent(TObject *ASender,
          IdxNavigationItem *ANavigationItem, TWinControl *&AControl)
{
  if (FSearching)
	return;
  if (ANavigationItem->Text == "&Color Scheme")
	{AControl = dxNavBar2;}
  else
	if (ANavigationItem->Text == "&Options")
	  {AControl = dxNavBar1;}
	else
	  if (ANavigationItem->Text == "&Search")
		{AControl = cxGroupBox2;};
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::ShowOfficeNavigationBar1Click(TObject *Sender)
{
  if (ShowOfficeNavigationBar1->Checked)
  {
	dxNavBarOfficeNavigationBar1->ItemProvider = nbMain;
	dxNavBarOfficeNavigationBar1->Visible = true;
  }
  else
  {
	dxNavBarOfficeNavigationBar1->Visible = false;
	dxNavBarOfficeNavigationBar1->ItemProvider = NULL;
  };
}
//---------------------------------------------------------------------------

