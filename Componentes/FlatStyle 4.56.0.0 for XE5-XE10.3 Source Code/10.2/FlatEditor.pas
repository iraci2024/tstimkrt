unit FlatEditor;

interface

{$I FlatStyle.inc}

uses

  DesignIntf, DesignEditors, DesignMenus,



  Classes, MaskUtils, SysUtils, Controls, Forms, TypInfo;

procedure Register;

implementation

uses
  FlatCtrls, FlatSysex, FlatUtils, FlatAttrib, FlatVersion, FlatWatfm;

{ TWaterEditor }
type TWaterEditor = class(TStringProperty)
     public
       procedure Edit; override;
       function GetValue: string; override;
       function GetWater: TDefineWater;
       function GetAttributes: TPropertyAttributes; override;
       property Water: TDefineWater read GetWater;
     end;

procedure TWaterEditor.Edit;
begin
  with TWaterForm.Create(WaterForm) do
 begin
  try
   WaterList.Items.Assign(Water.Items);
   FlatWater.Items.Assign(Water.Items);
   ShowModal;
   if ModalResult = mrOk then
   begin
      Water.Items.Assign(WaterList.Items);
      TWaterEditor(self).Designer.Modified;
   end;
  finally
   Free;
  end;
 end;
end;

function TWaterEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TWaterEditor.GetValue: string;
begin
  Result := '(Editor)';
end;

function TWaterEditor.GetWater: TDefineWater;
begin
  result := (GetComponent(0) as TDefineWater);
end;

{ TMaskEditor }
type TMaskEditor = class(TStringProperty)
     public
      procedure Edit; override;
      function GetValue: string; override;
      function GetMask: TDefineMask;
      function GetAttributes: TPropertyAttributes; override;
      property Mask: TDefineMask read GetMask;
     end;
function TMaskEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TMaskEditor.GetValue: string;
begin
  Result := Mask.Text;
end;

function TMaskEditor.GetMask: TDefineMask;
begin
  result := (GetComponent(0) as TDefineMask);
end;

procedure TMaskEditor.Edit;
begin
 with TMaskForm.Create(MaskForm) do
 begin
  try
   Font.Assign(Screen.MenuFont);
   EditText.EditMask := Mask.EditMask;
   EditText.Text     := Mask.Text;
   if Mask.EditMask <> '' then
      EditMask.Text := Mask.EditMask;
   ShowModal;
   if ModalResult = mrOk then
   begin
      Mask.Text := EditText.Text;
      TMaskEditor(Self).Designer.Modified;
   end;
  finally
   Free;
  end;
 end;
end;

{ TVersionEditor }

type TVersionEditor = class(TPropertyEditor)
     private
       function GetGropName: TComponent;
     public
       procedure Edit; override;
       function GetValue: string; override;
       function GetAttributes: TPropertyAttributes; override;
       property GropName:TComponent read GetGropName;
     end;

procedure TVersionEditor.Edit;
const
  AboutStr = '1.Information' +#13+
             '>>Control: %s'  +#13+
             '>>Version: %s'  +#13+
             '>>In Unit: %s.pas'+#13+
             '2.Packages'    +#13+
             '>>Version: %s'  +#13+
             '>>Copyright: %s'+#13+
             '3.Compile Platform'+#13+
             '>>Version: %s'  +#13+
             '4.Completion Date'+#13+
             '>>Date: %s'+#13+
             '5.This version by the comerose update!';
var
  vName,vUnit:String;
  vInfo: PTypeData;
begin 
 with TVersionForm.Create(VersionForm) do begin
  try
   Font.Assign(Screen.MenuFont);
   Caption := 'About FlatStyle components';
   if GropName <> nil then begin
      vInfo := GetTypeData(PTypeInfo(GropName.ClassInfo));
      vName := GropName.ClassName;
      vUnit := vInfo.UnitName;
   end else
      vName := 'Unkown class';
   About.Caption := format(AboutStr,[vName,FileVersion,vUnit,FileVersion,
                                     FileCopyright,CompilePlat,FileFinish]);
   Height        := About.Top*2  + About.Height + OkBtn.Height*3;
   Width         := About.Left*2 + About.Width;
   OkBtn.Top     := About.Height + OkBtn.Height;
   OkBtn.Left    := (Width-OkBtn.Width) div 2;
   ShowModal;
  finally
   Free;
  end;
 end;
end;

function TVersionEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TVersionEditor.GetGropName: TComponent;
var
  Vers:TPersistent;
begin
  Vers := GetComponent(0);
  if Vers is TVersionEdit then
     result := TVersionEdit(Vers)
  else if Vers is TVersionComboBox then
     result := TVersionComboBox(Vers)
  else if Vers is TVersionControl then
     result := TVersionControl(Vers)
  else if Vers is TVersionGraphic then
     result := TVersionGraphic(Vers)
  else if Vers is TVersionTreeView then
     result := TVersionTreeView(Vers)
  else if Vers is TVersionListView then
     result := TVersionListView(Vers)
  else if Vers is TVersionComponent then
     result := TVersionComponent(Vers)
  else if Vers is TVersionMemo then
     result := TVersionMemo(Vers)
  else if Vers is TVersionPages then
     result := TVersionPages(Vers)
  else if Vers is TVersionSheet then
     result := TVersionSheet(Vers)
  else if Vers is TVersionCtrlExt then
     result := TVersionCtrlExt(Vers)
  else if Vers is TVersionDBGrid then
     result := TVersionDBGrid(Vers)
  else if Vers is TVersionDrawGrid then
     result := TVersionDrawGrid(Vers)
  else
     result := nil;
end;

function TVersionEditor.GetValue: string;
begin
  result := FileVersion;
end;

type
 TExcelEditor = class(TDefaultEditor)
  private
    function GetDefineExcel: TDefineExcel;
  public
    procedure ExecuteVerb(index: Integer);  override;
    function GetVerb(index: Integer): String;  override;
    function GetVerbCount: Integer;  override;
    property Excel:TDefineExcel read GetDefineExcel;
  end;

procedure TExcelEditor.ExecuteVerb(index: Integer);
begin
  case index of
   0: Excel.InitFields;
   1: Excel.RestoreFields;
   2: Excel.ClearFields;
  end;
  TExcelEditor(self).Designer.Modified;
end;

function TExcelEditor.GetDefineExcel: TDefineExcel;
begin
  result := (Component as TDefineExcel);
end;

function TExcelEditor.GetVerb(index: Integer): String;
begin
  case index of
    0: result := '添加全部字段';
    1: result := '还原--->字段';
    2: result := '清除全部字段';
  end;
end;

function TExcelEditor.GetVerbCount: Integer;
begin
   result := 3;
end;

type
  TFlatActivePage = class(TComponentProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TFlatActivePage.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TFlatActivePage.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
  for I := 0 to Designer.GetRoot.ComponentCount - 1 do
  begin
    Component := Designer.GetRoot.Components[I];
    if (Component.Name <> '') and (Component is TFlatSheet) and
       (TFlatSheet(Component).PageControl = GetComponent(0)) then
      Proc(Component.Name);
  end;
end;

type
  TPagesEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TPagesEditor.ExecuteVerb(Index: Integer);
var
  Pages: TFlatPages;
  Sheet: TFlatSheet;
  {$IFDEF DELPHI3}
  ADesigner: TFormDesigner;
  {$ELSE}

  ADesigner: IDesigner;

  {$ENDIF}
begin
  if Component is TFlatSheet then
     Pages := TFlatSheet(Component).PageControl as TFlatPages
  else
     Pages := TFlatPages(Component);
  if Pages <> nil then
  begin
    ADesigner := Self.Designer;
    case Index of
     0: begin

      Sheet := TFlatSheet.Create(ADesigner.Root);

      try
        Sheet.Name        := ADesigner.UniqueName(TFlatSheet.ClassName);
        Sheet.Parent      := Pages;
        Sheet.PageControl := Pages;
        Sheet.ImageIndex  := Sheet.TabIndex;
      except
        Sheet.Free;
        raise
      end;
      Pages.ActivePage := Sheet;
      Pages.UpdateGlyphs;
      ADesigner.SelectComponent(Sheet);
      end;
      1,2:begin
        Sheet := Pages.FindNextPage(Pages.ActivePage,Index=1,False) as TFlatSheet;
        if (Sheet <> nil) then begin
         if (Sheet <> Pages.ActivePage) then begin
            Pages.ActivePage := Sheet;
            ADesigner.SelectComponent(Sheet);
         end;
        end;
      end;
      3:if (Pages.ActivePage <> nil) then
       begin
         Designer.SelectComponent(Pages.ActivePage as TFlatSheet);
         Pages.ActivePage.Free;
       end;
    end;
    ADesigner.Modified
  end
end;

function TPagesEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:result:='New Page';
    1:result:='Next Page';
    2:result:='Previous Page';
    3:result:='Delete Page';
  end
end;

function TPagesEditor.GetVerbCount: Integer;
begin
  Result := 4
end;

procedure Register;
begin
  RegisterComponentEditor(TFlatPages, TPagesEditor);
  RegisterComponentEditor(TFlatSheet, TPagesEditor);
  RegisterPropertyEditor(TypeInfo(TFlatSheet), TFlatPages, 'ActivePage', TFlatActivePage);
  RegisterPropertyEditor(TypeInfo(TMaskedText), TDefineMask,  'Text',    TMaskEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionEdit,      'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionMemo,      'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionComboBox,  'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionControl,   'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionGraphic,   'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionTreeView,  'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionListView,  'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionComponent, 'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionPages,     'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionSheet,     'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionCtrlExt,   'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionDBGrid,    'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(String), TVersionDrawGrid,  'Version', TVersionEditor);
  RegisterPropertyEditor(TypeInfo(TStringList), TDefineWater, 'Items',   TWaterEditor);
  RegisterComponentEditor(TDefineExcel, TExcelEditor);
  RegisterClasses([TFlatSheet]);
end;



end.
