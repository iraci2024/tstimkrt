//---------------------------------------------------------------------------

#ifndef uCloudStorageDemoMainH
#define uCloudStorageDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxImageList.hpp"
#include "cxListView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxTreeView.hpp"
#include "dxAlertWindow.hpp"
#include "dxAuthorizationAgents.hpp"
#include "dxCloudStorage.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutControlAdapters.hpp"
#include "dxCloudStorageGoogleDriveProvider.hpp"
#include "dxCloudStorageMicrosoftOneDriveProvider.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <IOUtils.hpp>
#include <ShellAPI.hpp>
#include "dxActivityIndicator.hpp"
#include "dxWinInet.hpp"
#include "uCloudSetupForm.h"
//---------------------------------------------------------------------------
class TfmCloudStorageDemoForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TdxLayoutControl *dxLayoutControl1;
	TcxTreeView *tvMain;
	TcxListView *lvMain;
	TcxButton *cxButton1;
	TcxButton *cxButton2;
	TcxButton *cxButton3;
	TcxButton *cxButton4;
	TcxButton *cxButton5;
	TcxListView *lvPermissions;
	TcxButton *cxButton6;
	TdxLayoutGroup *dxLayoutControl1Group_Root;
	TdxLayoutItem *dxLayoutItem1;
	TdxLayoutItem *dxLayoutItem2;
	TdxLayoutSplitterItem *dxLayoutSplitterItem1;
	TdxLayoutGroup *lgTools;
	TdxLayoutGroup *dxLayoutGroup1;
	TdxLayoutItem *dxLayoutItem5;
	TdxLayoutItem *dxLayoutItem6;
	TdxLayoutItem *dxLayoutItem7;
	TdxLayoutItem *dxLayoutItem3;
	TdxLayoutSeparatorItem *dxLayoutSeparatorItem1;
	TdxLayoutSeparatorItem *dxLayoutSeparatorItem2;
	TdxLayoutItem *dxLayoutItem4;
	TdxLayoutItem *liPermissions;
	TdxLayoutSplitterItem *dxLayoutSplitterItem2;
	TdxLayoutGroup *dxLayoutGroup2;
	TdxLayoutLabeledItem *dxLayoutLabeledItem1;
	TdxLayoutAutoCreatedGroup *dxLayoutAutoCreatedGroup1;
	TdxLayoutGroup *dxLayoutGroup3;
	TdxLayoutItem *dxLayoutItem8;
	TcxImageList *ilActions;
	TcxImageList *il16x16;
	TcxImageList *il32x32;
	TImageList *ilSystem;
	TdxCloudStorage *csMain;
	TdxAlertWindowManager *awmMain;
	TFileOpenDialog *FileOpenDialog1;
	TdxMicrosoftGraphAPIOAuth2AuthorizationAgent *aaOneDrive;
	TdxGoogleAPIOAuth2AuthorizationAgent *aaGDrive;
	TActionList *alMain;
	TAction *acCreateFolder;
	TAction *acUploadFile;
	TAction *acDelete;
	TAction *acRefresh;
	TAction *acCreateSharedLink;
	TAction *acDeletePermission;
	TMenuItem *Options1;
	TMenuItem *miProviders;
	TMenuItem *miGoogleDrive;
	TMenuItem *miMicrosoftOneDrive;
	TMenuItem *SpecifyAuthorizationSettings1;
	TdxActivityIndicator *aiMain;
	void __fastcall csMainError(TObject *Sender, const void *AErrorObject);
	void __fastcall SpecifyAuthorizationSettings1Click(TObject *Sender);
	void __fastcall miChooseProviderClick(TObject *Sender);
	void __fastcall lvMainEditing(TObject *Sender, TListItem *Item, bool &AllowEdit);
	void __fastcall tvMainEditing(TObject *Sender, TTreeNode *Node, bool &AllowEdit);
	void __fastcall csMainConnectedChanged(TObject *Sender);
	void __fastcall csMainTreeDataLoaded(TObject *Sender, TdxCloudStorageCustomFolder *AFolder);
	void __fastcall tvMainExpanding(TObject *Sender, TTreeNode *Node, bool &AllowExpansion);
	void __fastcall tvMainClick(TObject *Sender);
	void __fastcall tvMainKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall lvMainGetImageIndex(TObject *Sender, TListItem *Item);
	void __fastcall lvMainDblClick(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall lvMainKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall csMainItemDownloaded(TObject *Sender, TdxCloudStorageItem * const AItem,
          TStream *AStream);
	void __fastcall csMainItemDownloading(TObject *Sender, TdxCloudStorageItem * const AItem,
          const int ASize);
	void __fastcall alMainUpdate(TBasicAction *Action, bool &Handled);
	void __fastcall acCreateFolderExecute(TObject *Sender);
	void __fastcall acUploadFileExecute(TObject *Sender);
	void __fastcall acDeleteExecute(TObject *Sender);
	void __fastcall acRefreshExecute(TObject *Sender);
	void __fastcall csMainFolderCreated(TObject *Sender, TdxCloudStorageCustomFolder *AFolder);
	void __fastcall csMainItemMovedToTrash(TObject *Sender, TdxCloudStorageItem * const AItem);
	void __fastcall csMainItemUploading(TObject *Sender, const UnicodeString AFileName,
          const int ASize);
	void __fastcall csMainItemPermissionsLoaded(TObject *Sender, TdxCloudStorageItem * const AItem);
	void __fastcall csMainItemPermissionCreated(TObject *Sender, TdxCloudStorageItemPermission * const APermission);
	void __fastcall csMainItemPermissionDeleted(TObject *Sender, TdxCloudStorageItemPermission * const APermission);
	void __fastcall acCreateSharedLinkExecute(TObject *Sender);
	void __fastcall acDeletePermissionExecute(TObject *Sender);
	void __fastcall lvMainSelectItem(TObject *Sender, TListItem *Item, bool Selected);

private:	// User declarations
	TTreeNode *FSelectedNode;
	TList *FIconsMap;
	void ShowSetup(bool AIsFirstTime = false);
	void DoChooseProvider(int ATag);
	void PopulateNodes(TTreeNode *AParentNode);
	void ForEachNode(TTreeNode *ANode, TdxCloudStorageCustomFolder *AFolder);
	TTreeNode* GetSelectedNode();
	void SetSelectedNode(TTreeNode *AValue);
	void PopulateListItems();
	void WaitForFolderLoaded(TdxCloudStorageCustomFolder *AFolder);
	String GetFileName(TdxCloudStorageItem *AFile);
	String SizeToString(int ASize);
	TdxCloudStorageCustomFolder* GetTopFolder();
	void UpdatePermissionList();

    void SetIE11KeyForWebBrowser();
    void RemoveIE11KeyForWebBrowser();
public:		// User declarations
	__fastcall TfmCloudStorageDemoForm(TComponent* Owner);
	__property TTreeNode *SelectedNode = {read=GetSelectedNode, write=SetSelectedNode};
};
extern PACKAGE TfmCloudStorageDemoForm *fmCloudStorageDemoForm;
//---------------------------------------------------------------------------
#endif
