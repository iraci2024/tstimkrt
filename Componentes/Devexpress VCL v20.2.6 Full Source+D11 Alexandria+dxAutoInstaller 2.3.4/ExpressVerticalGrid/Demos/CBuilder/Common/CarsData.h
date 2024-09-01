//---------------------------------------------------------------------------

#ifndef CarsDataH
#define CarsDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <DB.hpp>
#include "dxmdaset.hpp"
#include "cxClasses.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxDBEditRepository.hpp"
#include "cxStyles.hpp"
#include "cxCustomData.hpp" 
#include "cxGraphics.hpp"
#include "cxFilter.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxNavigator.hpp"
#include "cxControls.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxDBData.hpp"
#include "cxMemo.hpp"
#include "cxImageComboBox.hpp"
#include "cxImageList.hpp"
#include "cxExtEditRepositoryItems.hpp"

//---------------------------------------------------------------------------
class TdmCars : public TDataModule
{
__published:  // IDE-managed Components
    TDataSource* dsBodyStyle;
    TDataSource* dsCategory;
    TDataSource* dsModels;
    TDataSource* dsTrademark;
    TDataSource* dsTransmissionType;
    TcxEditRepository* EditRepository;
    TcxEditRepositoryLookupComboBoxItem* EditRepositoryBodyStyleLookup;
    TcxEditRepositoryLookupComboBoxItem* EditRepositoryCategoryLookup;
    TcxEditRepositoryImageItem* EditRepositoryImage;
    TcxEditRepositoryBlobItem* EditRepositoryImageBlob;
    TcxEditRepositoryMemoItem* EditRepositoryMemo;
    TcxEditRepositoryBlobItem* EditRepositoryMemoBlob;
    TcxEditRepositoryCheckBoxItem* EditRepositoryTransmissionTypeCheckBox;
    TcxEditRepositoryLookupComboBoxItem* EditRepositoryTransmissionTypeLookup;
    TdxMemData* mdBodyStyle;
    TIntegerField* mdBodyStyleID;
    TWideStringField* mdBodyStyleName;
    TdxMemData* mdCategory;
    TIntegerField* mdCategoryID;
    TWideStringField* mdCategoryName;
    TBlobField* mdCategoryPicture;
    TdxMemData* mdModels;
    TIntegerField* mdModelsBodyStyleID;
    TIntegerField* mdModelsCategoryID;
    TIntegerField* mdModelsCilinders;
    TDateTimeField* mdModelsDelivery_Date;
    TWideMemoField* mdModelsDescription;
    TIntegerField* mdModelsDoors;
    TWideStringField* mdModelsFullName;
    TWideStringField* mdModelsHorsepower;
    TStringField* mdModelsHyperlink;
    TIntegerField* mdModelsID;
    TBlobField* mdModelsImage;
    TBooleanField* mdModelsInStock;
    TWideStringField* mdModelsModification;
    TIntegerField* mdModelsMPG_City;
    TIntegerField* mdModelsMPG_Highway;
    TWideStringField* mdModelsName;
    TBlobField* mdModelsPhoto;
    TBCDField* mdModelsPrice;
    TWideStringField* mdModelsTorque;
    TWideStringField* mdModelsTrademark;
    TIntegerField* mdModelsTrademarkID;
    TWideStringField* mdModelsTransmission_Speeds;
    TIntegerField* mdModelsTransmission_Type;
    TStringField* mdModelsTransmissionTypeName;
    TdxMemData* mdTrademark;
    TWideMemoField* mdTrademarkDescription;
    TIntegerField* mdTrademarkID;
    TBlobField* mdTrademarkLogo;
    TWideStringField* mdTrademarkName;
    TWideStringField* mdTrademarkSite;
    TdxMemData* mdTransmissionType;
    TIntegerField* mdTransmissionTypeID;
    TWideStringField* mdTransmissionTypeName;
    TStringField* mdModelsCategory;
    TStringField* mdModelsBodyStyle;
    TDataSource* dsCarOrders;
    TdxMemData* mdCarOrders;
    TIntegerField* mdCarOrdersID;
    TIntegerField* mdCarOrdersParentID;
    TWideStringField* mdCarOrdersName;
    TWideStringField* mdCarOrdersModification;
    TBCDField* mdCarOrdersPrice;
    TIntegerField* mdCarOrdersMPG_City;
    TIntegerField* mdCarOrdersMPG_Highway;
    TIntegerField* mdCarOrdersBodyStyleID;
    TIntegerField* mdCarOrdersCilinders;
    TDateField* mdCarOrdersSalesDate;
    TStringField* mdCarOrdersBodyStyle;
    TDataSource* dsTowns;
    TdxMemData* mdTowns;
    TAutoIncField* mdTownsID;
    TStringField* mdTownsName;
    TDataSource* dsCarOrdersAndDelivery;
    TdxMemData* mdCarOrdersAndDelivery;
    TWideStringField* mdCarOrdersAndDeliveryName;
    TIntegerField* mdCarOrdersAndDeliveryBodyStyleID;
    TStringField* mdCarOrdersAndDeliveryBodyStyle;
    TCurrencyField* mdCarOrdersAndDeliveryPrice;
    TDateField* mdCarOrdersAndDeliverySalesDate;
    TCurrencyField* mdCarOrdersAndDeliverySalesPrice;
    TDateField* mdCarOrdersAndDeliveryDeliveryDate;
    TBooleanField* mdCarOrdersAndDeliveryDeliveryComplete;
    TStringField* mdCarOrdersAndDeliveryDeliveryFrom;
    TStringField* mdCarOrdersAndDeliveryDeliveryTo;
    TIntegerField* mdCarOrdersAndDeliveryParentID;
    TIntegerField* mdCarOrdersAndDeliveryID;
    TcxEditRepositoryImageComboBoxItem* EditRepositoryTrademarkLogo;
    TcxEditRepositoryRatingControl* EditRepositoryModelRating;
    TDataSource* dsCarOrdersAndTransfer;
    TdxMemData* mdCarOrdersAndTransfer;
    TStringField* mdCarOrdersAndTransferTrademark;
    TWideStringField* mdCarOrdersAndTransferName;
    TIntegerField* mdCarOrdersAndTransferBodyStyleID;
    TStringField* mdCarOrdersAndTransferBodyStyle;
    TCurrencyField* mdCarOrdersAndTransferPrice;
    TDateField* mdCarOrdersAndTransferSalesDate;
    TCurrencyField* mdCarOrdersAndTransferSalesPrice;
    TDateField* mdCarOrdersAndTransferDeliveryDate;
    TBooleanField* mdCarOrdersAndTransferDeliveryComplete;
    TStringField* mdCarOrdersAndTransferDeliveryFrom;
    TStringField* mdCarOrdersAndTransferDeliveryTo;
    TIntegerField* mdCarOrdersAndTransferModelID;
    TStringField* mdCarOrdersAndDeliveryTrademark;
    TIntegerField* mdCarOrdersAndDeliveryModelID;
    TFloatField* mdModelsRating;
    TcxImageList* ilLogo;

    void __fastcall DataModuleCreate(TObject* Sender);
    void __fastcall mdModelsCalcFields(TDataSet* DataSet);	
public:   // User declarations
	__fastcall TdmCars(TComponent* Owner);

    void LoadCarOrders(String APath);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmCars *dmCars;
//---------------------------------------------------------------------------
#endif
