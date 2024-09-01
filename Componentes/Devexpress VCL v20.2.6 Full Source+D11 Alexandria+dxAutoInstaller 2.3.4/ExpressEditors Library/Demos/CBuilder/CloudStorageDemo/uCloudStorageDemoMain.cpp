// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "uCloudStorageDemoMain.h"
#include "Shellapi.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxImageList"
#pragma link "cxListView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxTreeView"
#pragma link "dxAlertWindow"
#pragma link "dxAuthorizationAgents"
#pragma link "dxCloudStorage"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutControlAdapters"
#pragma link "dxActivityIndicator"
#pragma link "dxCloudStorageGoogleDriveProvider"
#pragma link "dxCloudStorageMicrosoftOneDriveProvider"
#pragma resource "*.dfm"
TfmCloudStorageDemoForm *fmCloudStorageDemoForm;

// ---------------------------------------------------------------------------
__fastcall TfmCloudStorageDemoForm::TfmCloudStorageDemoForm(TComponent* Owner)
	: TfmBaseForm(Owner) {
	csMain->OnError = csMainError;
}

// ---------------------------------------------------------------------------
void __fastcall TfmCloudStorageDemoForm::SpecifyAuthorizationSettings1Click
	(TObject *Sender) {
	ShowSetup();
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::miChooseProviderClick(TObject *Sender)
{
	miGoogleDrive->Checked = false;
	miMicrosoftOneDrive->Checked = false;
	DoChooseProvider(((TComponent*)Sender)->Tag);
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::lvMainEditing(TObject *Sender,
	TListItem *Item, bool &AllowEdit) {
	AllowEdit = false;
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::tvMainEditing(TObject *Sender,
	TTreeNode *Node, bool &AllowEdit) {
	AllowEdit = false;
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::csMainConnectedChanged(TObject *Sender)
{
	int AImageIndexMap[] = {4, 2, 2, 3, 0, 5};
	TTreeNode *ANode;
	miGoogleDrive->Checked =
		csMain->ProviderClassName ==
		TdxCloudStorageGoogleDriveProvider::ClassName();
	miMicrosoftOneDrive->Checked =
		csMain->ProviderClassName ==
		TdxCloudStorageMicrosoftOneDriveProvider::ClassName();
	tvMain->Items->BeginUpdate();
	try {
		tvMain->Items->Clear();
		lvMain->Items->Clear();
		if (!csMain->Connected)
			return;
		ANode = tvMain->Items->AddObject(NULL, "Root", csMain->Files->Root);
		ANode->ImageIndex = 1;
		ANode->SelectedIndex = 1;
		TListItem *AListItem = lvMain->Items->Add();
		AListItem->ImageIndex = 1;
		AListItem->Caption = "Root";
		AListItem->Data = csMain->Files->Root;
		PopulateNodes(ANode);
		csMain->Files->Root->FetchChildren();
		TdxCloudStorageSpecialFolder *ASpecialFolder;
		for (int i = 0; i < csMain->Files->SpecialFolders->Count; i++) {
			ASpecialFolder = csMain->Files->SpecialFolders->Items[i];
			ANode = tvMain->Items->AddObject(NULL, ASpecialFolder->Name,
				ASpecialFolder);
			ANode->ImageIndex = AImageIndexMap[(int)ASpecialFolder->Type];
			ANode->SelectedIndex = ANode->ImageIndex;
			AListItem = lvMain->Items->Add();
			AListItem->ImageIndex = ANode->ImageIndex;
			AListItem->Caption = ANode->Text;
			AListItem->Data = ANode->Data;
			PopulateNodes(ANode);
		}
		tvMain->Selected = NULL;
	}
	__finally {
		tvMain->Items->EndUpdate();
	}
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::csMainTreeDataLoaded(TObject *Sender,
	TdxCloudStorageCustomFolder *AFolder)

{
	if (AFolder == NULL)
		return;
	TTreeNode *ANode = tvMain->InnerTreeView->TopItem;
	while (ANode != NULL) {
		if (ANode->Parent == NULL)
			ForEachNode(ANode, AFolder);
		ANode = ANode->GetNext();
	}
	if ((SelectedNode != NULL) && (SelectedNode->Data == AFolder))
		PopulateListItems();
	else
		for (int i = 0; i < lvMain->Items->Count; i++)
			if (lvMain->Items->Item[i]->Data == AFolder) {
				lvMain->Items->Item[i]->Caption = AFolder->Name;
				break;
			}
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::tvMainExpanding(TObject *Sender,
	TTreeNode *Node, bool &AllowExpansion) {
	TdxCloudStorageCustomFolder *AFolder =
		(TdxCloudStorageCustomFolder*)Node->Data;
	if (!AFolder->IsLoaded) {
		AFolder->FetchChildren(true);
		WaitForFolderLoaded(AFolder);
		AllowExpansion = AFolder->HasChildren();
	}
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::tvMainClick(TObject *Sender) {
	SelectedNode = tvMain->Selected;
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::tvMainKeyPress(TObject *Sender,
	System::WideChar &Key)

{
	SelectedNode = tvMain->Selected;
}

// ---------------------------------------------------------------------------
void __fastcall TfmCloudStorageDemoForm::lvMainGetImageIndex(TObject *Sender,
	TListItem *Item) {
	TdxCloudStorageItem *AItem = (TdxCloudStorageItem*)Item->Data;
	if (AItem->IsFolder())
		return;
	TSHFileInfoA AInfo;
	memset(&AInfo, 0, sizeof(AInfo));
	int AFlags = SHGFI_SYSICONINDEX | SHGFI_LARGEICON | SHGFI_USEFILEATTRIBUTES;
	AnsiString AName = GetFileName(AItem);
	SHGetFileInfoA(AName.c_str(), 0, &AInfo, sizeof(AInfo), AFlags);
	int AIndex = AInfo.iIcon;
	DestroyIcon(AInfo.hIcon);
	while (FIconsMap->Count <= AIndex) {
		FIconsMap->Add(NULL);
	}
	int AResult;
	if (FIconsMap->Items[AIndex] == NULL) {
		AResult = il32x32->Count;
		FIconsMap->Items[AIndex] = (TObject*)AResult;
		Graphics::TBitmap *AImage = new Graphics::TBitmap();
		ilSystem->GetBitmap(AIndex, AImage);
		il32x32->Add(AImage, NULL);
		delete AImage;
	} else {
		AResult = (int)FIconsMap->Items[AIndex];
	}
	Item->ImageIndex = AResult;
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::lvMainDblClick(TObject *Sender) {
	if (lvMain->Selected == NULL)
		return;
	TdxCloudStorageItem *AItem = (TdxCloudStorageItem*)lvMain->Selected->Data;
	if (AItem->IsFolder()) {
		if (tvMain->Selected != NULL) {
			if (tvMain->Selected->Data == AItem)
				SelectedNode = tvMain->Selected;
			else {
				for (int i = 0; i < tvMain->Selected->Count; i++)
					if (tvMain->Selected->Item[i]->Data == AItem) {
						tvMain->Selected = tvMain->Selected->Item[i];
						SelectedNode = tvMain->Selected;
						break;
					}
			}
		}
		if ((tvMain->Selected == NULL) || (tvMain->Selected->Data != AItem))
			for (int i = 0; i < tvMain->Items->Count; i++)
				if (tvMain->Items->Item[i]->Data == AItem) {
					tvMain->Selected = tvMain->Items->Item[i];
					SelectedNode = tvMain->Selected;
					break;
				}
	}
	else
		((TdxCloudStorageFile*)AItem)->DownloadContent();
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::FormShow(TObject *Sender) {
	ShowSetup(true);
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::FormDestroy(TObject *Sender) {
	RemoveIE11KeyForWebBrowser();
	delete FIconsMap;
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::FormCreate(TObject *Sender) {
	FIconsMap = new TList();
	SetIE11KeyForWebBrowser();

	TSHFileInfoA AFileInfo;
	ilSystem->ShareImages = true;
	ilSystem->Handle = SHGetFileInfoA("", 0, &AFileInfo, sizeof(AFileInfo),
		SHGFI_SYSICONINDEX | SHGFI_LARGEICON);
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::lvMainKeyDown(TObject *Sender,
	WORD &Key, TShiftState Shift) {
	if (Key == VK_RETURN)
		lvMainDblClick(Sender);
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::csMainItemDownloaded(TObject *Sender,
	TdxCloudStorageItem * const AItem, TStream *AStream) {
	String ADir = TPath::GetTempPath().c_str();
	String ABaseName = GetFileName(AItem);
	String ABaseFileName = ExtractFileName(ABaseName);
	String ABaseExt = ExtractFileExt(ABaseName);
	String AFileName = ADir.c_str() + ABaseName;
	int I = 1;
	while (FileExists(AFileName)) {
		AFileName = String::Format("%s%s[%d].%s",
			ARRAYOFCONST((ADir, ABaseFileName, I, ABaseExt)));
		I++;
	}
	TFileStream *AFileStream = new TFileStream(AFileName, fmCreate);
	AFileStream->CopyFrom(AStream, 0);
	delete AFileStream;
	dxShellExecute(AFileName, SW_SHOWNORMAL);
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::csMainItemDownloading(TObject *Sender,
	TdxCloudStorageItem * const AItem, const int ASize) {
	if (ASize == -1)
		awmMain->Show("Downloading...", String::Format("File: %s",
		ARRAYOFCONST((AItem->Name))));
	else
		awmMain->Show("Downloading...", String::Format("File: %s\r\nSize: %s",
		ARRAYOFCONST((AItem->Name, SizeToString(ASize)))));
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::alMainUpdate(TBasicAction *Action,
	bool &Handled)

{
	TdxCloudStorageCustomFolder *ATopFolder = GetTopFolder();
	acCreateFolder->Enabled = (ATopFolder != NULL) && ATopFolder->IsRoot();
	acUploadFile->Enabled = (ATopFolder != NULL) && ATopFolder->IsRoot();
	acDelete->Enabled = (ATopFolder != NULL) && ATopFolder->IsRoot() &&
		(lvMain->Selected != NULL);
	acRefresh->Enabled = ATopFolder != NULL;
	acCreateSharedLink->Enabled =
		(lvMain->Selected != NULL) && (ATopFolder != NULL)
		&& ATopFolder->IsRoot();
	acDeletePermission->Enabled =
		acCreateSharedLink->Enabled && (lvPermissions->Selected != NULL);
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::acCreateFolderExecute(TObject *Sender)
{
	String AName = "New Folder";
	if (InputQuery("Create Folder", "Name", AName))
		((TdxCloudStorageFolder*)tvMain->Selected->Data)->CreateFolder(AName);
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::acUploadFileExecute(TObject *Sender) {
	if (FileOpenDialog1->Execute())
		((TdxCloudStorageFolder*)SelectedNode->Data)->UploadFile
			(FileOpenDialog1->FileName);
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::acDeleteExecute(TObject *Sender) {
	if (MessageDlg("Are you sure want to delete the item(s)?", mtConfirmation,
		mbYesNoCancel, 0) == mrYes)
		((TdxCloudStorageItem*)lvMain->Selected->Data)->MoveToTrash();
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::acRefreshExecute(TObject *Sender) {
	((TdxCloudStorageCustomFolder*)SelectedNode->Data)->FetchChildren(true);
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::csMainFolderCreated(TObject *Sender,
	TdxCloudStorageCustomFolder *AFolder) {
	((TdxCloudStorageCustomFolder*)SelectedNode->Data)->FetchChildren(true);
	for (int I = 0; I < AFolder->Parents->Count; I++)
		AFolder->Parents->Items[I]->FetchChildren();
	PopulateListItems();
	for (int I = 0; I < lvMain->Items->Count; I++)
		if (lvMain->Items->Item[I]->Data == AFolder) {
			lvMain->Selected = lvMain->Items->Item[I];
			break;
		}
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::csMainItemMovedToTrash(TObject *Sender,
	TdxCloudStorageItem * const AItem)

{
	if (csMain->Files->Trash != NULL)
		csMain->Files->Trash->FetchChildren();
	if (SelectedNode != NULL)
		((TdxCloudStorageCustomFolder*)SelectedNode->Data)->FetchChildren(true);
	PopulateListItems();
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::csMainItemUploading(TObject *Sender,
	const UnicodeString AFileName, const int ASize) {
	if (ASize == -1)
		awmMain->Show("Uploading...", String::Format("File: %s",
		ARRAYOFCONST((AFileName))));
	else
		awmMain->Show("Uploading...", String::Format("File: %s\r\nSize: %s",
		ARRAYOFCONST((AFileName, SizeToString(ASize)))));
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::csMainItemPermissionsLoaded
	(TObject *Sender, TdxCloudStorageItem * const AItem) {
	if ((lvMain->Selected == NULL) || (AItem == lvMain->Selected->Data))
		UpdatePermissionList();
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::csMainItemPermissionCreated
	(TObject *Sender, TdxCloudStorageItemPermission * const APermission) {
	if ((lvMain->Selected == NULL) ||
		(APermission->Owner == lvMain->Selected->Data))
		UpdatePermissionList();
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::csMainItemPermissionDeleted
	(TObject *Sender, TdxCloudStorageItemPermission * const APermission) {
	if ((lvMain->Selected == NULL) ||
		(APermission->Owner == lvMain->Selected->Data))
		UpdatePermissionList();
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::acCreateSharedLinkExecute
	(TObject *Sender)

{
	if ((lvMain->Selected == NULL) || !(((TObject*)lvMain->Selected->Data)
		->InheritsFrom(__classid(TdxCloudStorageFile)) ||
		((TObject*)lvMain->Selected->Data)->InheritsFrom
		(__classid(TdxCloudStorageFolder))))
		return;
	((TdxCloudStorageItem*)lvMain->Selected->Data)
		->Permissions->CreateSharedLink();
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::acDeletePermissionExecute
	(TObject *Sender)

{
	if (lvPermissions->Selected == NULL)
		return;
	((TdxCloudStorageItemPermission*)lvPermissions->Selected->Data)->Delete();
}
// ---------------------------------------------------------------------------

void __fastcall TfmCloudStorageDemoForm::lvMainSelectItem(TObject *Sender,
	TListItem *Item, bool Selected) {
	if (Selected && (Item->Data != NULL))
		((TdxCloudStorageItem*)Item->Data)->Permissions->FetchAll();
	UpdatePermissionList();
}

// ---------------------------------------------------------------------------
void __fastcall TfmCloudStorageDemoForm::csMainError(TObject *Sender,
	const void *AErrorObject) {
	TdxJSONObject *AError = (TdxJSONObject*)AErrorObject;
	TdxJSONValue *APair = AError->Get("error")->JsonValue;
	if (APair != NULL) {
		TdxJSONValue *AMessage = ((TdxJSONObject*)APair)->Get("message")
			->JsonValue;
		if (AMessage != NULL)
			MessageDlg(AMessage->Value(), mtError, TMsgDlgButtons() << mbOK, 0);
	}
}

// ---------------------------------------------------------------------------
void TfmCloudStorageDemoForm::ShowSetup(bool AIsFirstTime) {
	TfmCloudSetupWizard *AForm = new TfmCloudSetupWizard(NULL);
	AForm->teGoogleApiClientID->Text = aaGDrive->ClientID;
	AForm->teGoogleApiClientSecret->Text = aaGDrive->ClientSecret;
	AForm->teMSGraphClientID->Text = aaOneDrive->ClientID;
	AForm->teMSGraphClientSecret->Text = aaOneDrive->ClientSecret;
	if (!AIsFirstTime)
		AForm->btnStart->Caption = "OK";
	if (AForm->ShowModal() == mrOk) {
		aaGDrive->ClientID = AForm->teGoogleApiClientID->Text;
		aaGDrive->ClientSecret = AForm->teGoogleApiClientSecret->Text;
		aaOneDrive->ClientID = AForm->teMSGraphClientID->Text;
		aaOneDrive->ClientSecret = AForm->teMSGraphClientSecret->Text;
		miGoogleDrive->Enabled =
			(aaGDrive->ClientID != "") && (aaGDrive->ClientSecret != "");
		miMicrosoftOneDrive->Enabled =
			(aaOneDrive->ClientID != "") && (aaOneDrive->ClientSecret != "");
		DoChooseProvider(AForm->dxLayoutGroup4->ItemIndex);
	}
	delete AForm;
}

// ---------------------------------------------------------------------------
void TfmCloudStorageDemoForm::DoChooseProvider(int ATag) {
	if (ATag == 0) {
		if (aaOneDrive->ClientID != "")
			csMain->ProviderClassName =
				TdxCloudStorageMicrosoftOneDriveProvider::ClassName();
		else if (aaGDrive->ClientID != "")
			csMain->ProviderClassName =
				TdxCloudStorageGoogleDriveProvider::ClassName();
		else
			csMain->ProviderClassName = "";
	}
	else {
		if (aaGDrive->ClientID != "")
			csMain->ProviderClassName =
				TdxCloudStorageGoogleDriveProvider::ClassName();
		else if (aaOneDrive->ClientID != "")
			csMain->ProviderClassName =
				TdxCloudStorageMicrosoftOneDriveProvider::ClassName();
		else
			csMain->ProviderClassName = "";
	}
	if (csMain->Provider->ClassName() == TdxCloudStorageGoogleDriveProvider::ClassName())
		csMain->Provider->AuthorizationAgent = aaGDrive;
	else if (csMain->Provider->ClassName() == TdxCloudStorageMicrosoftOneDriveProvider::ClassName())
		csMain->Provider->AuthorizationAgent = aaOneDrive;
	SelectedNode = NULL;
	csMain->Connected = true;
}

// ---------------------------------------------------------------------------
int CALLBACK NodesCompare(LPARAM lParam1, LPARAM lParam2, LPARAM Reverse) {
	TdxCloudStorageItem *AItem1 =
		(TdxCloudStorageItem*)reinterpret_cast<TTreeNode*>(lParam1)->Data;
	TdxCloudStorageItem *AItem2 =
		(TdxCloudStorageItem*)reinterpret_cast<TTreeNode*>(lParam2)->Data;
	if (AItem1->IsFolder() != AItem2->IsFolder()) {
		if (AItem1->IsFolder())
			return -1;
		else
			return 1;
	}
	else
		return CompareText(AItem1->Name, AItem2->Name);
}

// ---------------------------------------------------------------------------
void TfmCloudStorageDemoForm::PopulateNodes(TTreeNode *AParentNode) {
	TdxCloudStorageCustomFolder *AFolder =
		(TdxCloudStorageCustomFolder*)AParentNode->Data;
	AParentNode->Text = AFolder->Name;
	if (!AFolder->IsLoaded) {
		if (AParentNode->Count)
			tvMain->Items->AddChild(AParentNode, "(loading...)");
	}
	else {
		tvMain->Items->BeginUpdate();
		try {
			AParentNode->DeleteChildren();
			for (int I = 0; I < AFolder->Children->Count; I++)
				if (AFolder->Children->Items[I]->InheritsFrom
					(__classid(TdxCloudStorageCustomFolder)))
					PopulateNodes(tvMain->Items->AddChildObject(AParentNode,
					AFolder->Children->Items[I]->Name,
					AFolder->Children->Items[I]));
			AParentNode->CustomSort(NodesCompare, 0, False);
		}
		__finally {
			tvMain->Items->EndUpdate();
		}
	}
}

// ---------------------------------------------------------------------------
void TfmCloudStorageDemoForm::ForEachNode(TTreeNode *ANode,
	TdxCloudStorageCustomFolder *AFolder) {
	if (ANode->Data == AFolder)
		PopulateNodes(ANode);
	for (int I = 0; I < ANode->Count; I++)
		ForEachNode(ANode->Item[I], AFolder);
}

// ---------------------------------------------------------------------------
TTreeNode* TfmCloudStorageDemoForm::GetSelectedNode() {
	return FSelectedNode;
}

// ---------------------------------------------------------------------------
void TfmCloudStorageDemoForm::SetSelectedNode(TTreeNode *AValue) {
	if (FSelectedNode == AValue)
		return;
	FSelectedNode = AValue;
	PopulateListItems();
}
// ---------------------------------------------------------------------------
#if defined(_WIN32) && !defined(_WIN64)
int __stdcall ListItemsCompare(long Item1, long Item2, long ParamSort)
#else

int CALLBACK ListItemsCompare(LPARAM Item1, LPARAM Item2, LPARAM ParamSort)
#endif
{
	TdxCloudStorageItem *AItem1 =
		(TdxCloudStorageItem*)reinterpret_cast<TListItem*>(Item1)->Data;
	TdxCloudStorageItem *AItem2 =
		(TdxCloudStorageItem*)reinterpret_cast<TListItem*>(Item2)->Data;
	if (AItem1->IsFolder() != AItem2->IsFolder()) {
		if (AItem1->IsFolder())
			return -1;
		else
			return 1;
	}
	else
		return CompareText(AItem1->Name, AItem2->Name);
}

// ---------------------------------------------------------------------------
void TfmCloudStorageDemoForm::PopulateListItems() {
	if (SelectedNode == NULL)
		return;
	TdxCloudStorageCustomFolder *AFolder =
		(TdxCloudStorageCustomFolder*)SelectedNode->Data;
	if (!AFolder->IsLoaded) {
		AFolder->FetchChildren();
		WaitForFolderLoaded(AFolder);
	}
	lvMain->Items->BeginUpdate();
	try {
		lvMain->Clear();
		for (int I = 0; I < AFolder->Children->Count; I++) {
			TListItem *AListItem = lvMain->Items->Add();
			AListItem->Caption = AFolder->Children->Items[I]->Name;
			AListItem->Data = AFolder->Children->Items[I];
		}
		lvMain->InnerListView->CustomSort(ListItemsCompare, 0);
	}
	__finally {
		lvMain->Items->EndUpdate();
	}
}

// ---------------------------------------------------------------------------
void TfmCloudStorageDemoForm::WaitForFolderLoaded
	(TdxCloudStorageCustomFolder *AFolder) {
	if (AFolder->IsLoaded)
		return;
	if (!aiMain->Active) {
		aiMain->Active = true;
		aiMain->Left = lvMain->Left + (lvMain->Width - aiMain->Width) / 2;
		aiMain->Top = lvMain->Top + (lvMain->Height - aiMain->Height) / 2;
		aiMain->Visible = true;
	}
	while (!AFolder->IsLoaded)
		Application->ProcessMessages();
	aiMain->Active = false;
	aiMain->Visible = false;
}

// ---------------------------------------------------------------------------
String TfmCloudStorageDemoForm::GetFileName(TdxCloudStorageItem *AFile) {
	String AExt = csMain->Provider->GetExtension(AFile);
	if ((AExt != "") && (Pos(LowerCase(AExt), LowerCase(AFile->Name)) == 0))
		return AFile->Name + AExt;
	else
		return AFile->Name;
}

// ---------------------------------------------------------------------------
String TfmCloudStorageDemoForm::SizeToString(int ASize) {
	if (ASize > 1024 * 1024)
		return String::Format("%3.2f MB", ARRAYOFCONST((ASize / 1024 / 1024)));
	else if (ASize > 1024)
		return String::Format("%3.2f KB", ARRAYOFCONST((ASize / 1024)));
	else
		return String::Format("%d B", ARRAYOFCONST((ASize)));
}

// ---------------------------------------------------------------------------
TdxCloudStorageCustomFolder* TfmCloudStorageDemoForm::GetTopFolder() {
	if (SelectedNode == NULL)
		return NULL;
	TTreeNode *ANode = tvMain->Selected;
	while (ANode->Parent != NULL) {
		ANode = ANode->Parent;
	}
	return (TdxCloudStorageCustomFolder*)ANode->Data;
}

// ---------------------------------------------------------------------------
void TfmCloudStorageDemoForm::UpdatePermissionList() {
	lvPermissions->Items->BeginUpdate();
	try {
		lvPermissions->Items->Clear();
		if (lvMain->Selected == NULL)
			return;
		if (lvMain->Selected->Data != NULL) {
			TdxCloudStorageItemPermissions *APermissions =
				((TdxCloudStorageItem*)lvMain->Selected->Data)->Permissions;
			if (APermissions->IsLoaded) {
				for (int I = 0; I < APermissions->Count; I++) {
					TListItem *AListItem = lvPermissions->Items->Add();
					AListItem->Data = APermissions->Items[I];
					AListItem->Caption = APermissions->Items[I]->ID;
					AListItem->SubItems->Add
						(BoolToStr(APermissions->Items[I]->ReadOnly, true));
					AListItem->SubItems->Add(APermissions->Items[I]->Link);
				}
			}
		}
	}
	__finally {
		lvPermissions->Items->EndUpdate();
	}
}

// ---------------------------------------------------------------------------
void TfmCloudStorageDemoForm::SetIE11KeyForWebBrowser() {
	TRegistry *ARegistry = new TRegistry;
	try {
		ARegistry->RootKey = HKEY_CURRENT_USER;
		String AKey =
			"Software\\Microsoft\\Internet Explorer\\Main\\FeatureControl\\FEATURE_BROWSER_EMULATION";
		if (ARegistry->OpenKey(AKey, true))
			ARegistry->WriteInteger
				(ExtractFileName(Application->ExeName), 11001);
	}
	__finally {
		delete ARegistry;
	}
}

// ---------------------------------------------------------------------------
void TfmCloudStorageDemoForm::RemoveIE11KeyForWebBrowser() {
	TRegistry *ARegistry = new TRegistry;
	try {
		ARegistry->RootKey = HKEY_CURRENT_USER;
		String AKey =
			"Software\\Microsoft\\Internet Explorer\\Main\\FeatureControl\\FEATURE_BROWSER_EMULATION";
		if (ARegistry->OpenKey(AKey, true))
			ARegistry->DeleteValue(ExtractFileName(Application->ExeName));
	}
	__finally {
		delete ARegistry;
	}
}
// ---------------------------------------------------------------------------
