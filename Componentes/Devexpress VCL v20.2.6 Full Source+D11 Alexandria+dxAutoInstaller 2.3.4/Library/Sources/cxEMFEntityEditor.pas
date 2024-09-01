unit cxEMFEntityEditor; // for internal use

interface

uses
  Types, SysUtils, Generics.Collections, TypInfo, Rtti, Classes,
  dxCoreClasses, cxEdit, dxEMF.Metadata,
  cxEditRegisteredRepositoryItems,
  cxEditDataRegisteredRepositoryItems, cxEMFLookupEdit;

type
  { TdxEMFEntityProperties }

  TdxEMFEntityProperties = class(TcxCustomEMFLookupComboBoxProperties)
  public
    constructor Create(AOwner: TPersistent); override;
  end;

  { TcxEditRepositoryEMFEntityRepositoryItem }

  TcxEditRepositoryEMFEntityRepositoryItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TdxEMFEntityProperties;
    procedure SetProperties(Value: TdxEMFEntityProperties);
  public
    class function GetEditPropertiesClass
      : TcxCustomEditPropertiesClass; override;
  published
    property Properties: TdxEMFEntityProperties read GetProperties
      write SetProperties;
  end;

implementation

uses
  cxEMFData;

{ TdxEMFEntityProperties }

constructor TdxEMFEntityProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  ListOptions.ShowHeader := False;
end;

{ TcxEditRepositoryEMFEntityRepositoryItem }

class function TcxEditRepositoryEMFEntityRepositoryItem.GetEditPropertiesClass
  : TcxCustomEditPropertiesClass;
begin
  Result := TdxEMFEntityProperties;
end;

function TcxEditRepositoryEMFEntityRepositoryItem.GetProperties
  : TdxEMFEntityProperties;
begin
  Result := TdxEMFEntityProperties(inherited Properties);
end;

procedure TcxEditRepositoryEMFEntityRepositoryItem.SetProperties
  (Value: TdxEMFEntityProperties);
begin
  inherited Properties := Value;
end;

initialization

RegisterClasses([TcxEditRepositoryEMFEntityRepositoryItem]);
GetDefaultEditDataRepositoryItems.RegisterItem(TcxEMFEntityValueType,
  cxEditRegisteredItemsStandardVersion,
  GetDefaultEditRepository.CreateItem
  (TcxEditRepositoryEMFEntityRepositoryItem));

finalization

UnregisterClasses([TcxEditRepositoryEMFEntityRepositoryItem]);
GetDefaultEditDataRepositoryItems.UnregisterItem(TcxEMFEntityValueType,
  cxEditRegisteredItemsStandardVersion);
GetDefaultEditRepository.RemoveItems(TcxEditRepositoryEMFEntityRepositoryItem);

end.
