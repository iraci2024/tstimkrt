{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c)  2020 - 2021                                    }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit FMX.TMSFNCDataBindingDE;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, FMX.TMSFNCDataBinding, FMX.TMSFNCGeneralDE, FMX.TMSFNCDataBindingEditor, TypInfo
  {$IFNDEF LCLWEBLIB}
  ,DesignEditors, DesignIntf, DesignMenus, VCL.Menus, Data.DB
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf, Web, WEBLib.Forms, DB, WEBLib.DB
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,ComponentEditors, PropEdits, Menus, DB
  {$ENDIF}
  ;

type
  TTMSFNCDataBinderEditor = class(TTMSFNCDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    {$IFDEF LCLLIB}
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
    {$ENDIF}
    procedure MenuItemExecute(Sender: TObject);
    procedure ExecuteVerb(Index: Integer{$IFDEF WEBLIB}; AProc: TModalResultProc{$ENDIF}); override;
    {$IFNDEF WEBLIB}
    {$IFNDEF LCLLIB}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    {$ENDIF}
    {$IFDEF LCLLIB}
    procedure EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean); override;
    {$ENDIF}
    {$ENDIF}
  end;

  TTMSFNCDataBinderObjectProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TTMSFNCDataBinderColumnsProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TTMSFNCDataBinderColumnsSubProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TTMSFNCDataBinderFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  SysUtils, FMX.Dialogs, FMX.TMSFNCUtils, FMX.TMSFNCPersistence
  {$IFDEF FMXLIB}
  ,FMX.Forms
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,VCL.Forms
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,Forms
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.WEBTools
  {$ENDIF}
  {$IFNDEF LCLLIB}
  , UITypes
  {$ENDIF}
  ;

{ TTMSFNCDataBinderEditor }

procedure TTMSFNCDataBinderEditor.MenuItemExecute(Sender: TObject);
var
  g: TTMSFNCDataBinder;
  idx: Integer;
  f: TCustomForm;
  c: TComponent;
  MenuItem: {$IFDEF WEBLIB}TObject{$ELSE}TMenuItem{$ENDIF};
  dsf: TStringList;
  ds: TDataSource;
  e: TTMSFNCDataBindingEditor;
begin
  {$IFDEF WEBLIB}
  idx := -1;
  MenuItem := Sender;
  asm
    idx = MenuItem.tag;
  end;
  {$ELSE}
  MenuItem := Sender as TMenuItem;
  idx := MenuItem.Tag;
  {$ENDIF}

  if idx >= 0 then
  begin
    g := nil;
    if Component is TTMSFNCDataBinder then
      g := Component as TTMSFNCDataBinder;

    if Assigned(g) then
    begin
      f := TTMSFNCUtils.GetParentForm(g);
      if Assigned(f) and (idx >= 0) and (idx <= f.ComponentCount - 1) then
      begin
        ds := nil;
        dsf := TStringList.Create;
        try
          g.GetDataSources(dsf);
          if dsf.Count > 0 then
            ds := dsf.Objects[0] as TDataSource;
        finally
          dsf.Free;
        end;

        c := f.Components[idx];
        g.Connect(c, ds, '');

        e := TTMSFNCDataBindingEditor.Create(g);
        try
          e.DataBinder := g;
          e.&Object := c;
          e.Execute
          {$IFDEF WEBLIB}
          (procedure(AResult: TModalResult)
          begin
            e.Free;
          end)
          {$ENDIF}
          ;
        finally
          {$IFNDEF WEBLIB}
          e.Free;
          {$ENDIF}
        end;
      end;
    end;
  end;
end;

{$IFDEF LCLLIB}
procedure TTMSFNCDataBinderEditor.PrepareItem(Index: Integer;
  const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TTMSFNCDataBinderEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
{$ENDIF}
var
  {$IFDEF LCLLIB}
  MenuItem: TMenuItem;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  MenuItem: IMenuItem;
  {$ENDIF}
  I: Integer;
  g: TTMSFNCDataBinder;
  f: TCustomForm;
begin
  inherited PrepareItem(Index, AItem);

  g := nil;
  if Component is TTMSFNCDataBinder then
    g := Component as TTMSFNCDataBinder;

  if Assigned(g) then
  begin
    case Index - inherited GetVerbCount of
      1:
      begin
        f := TTMSFNCUtils.GetParentForm(g);
        if Assigned(f) then
        begin
          for I := 0 to f.ComponentCount - 1 do
          begin
            if (f.Components[I] <> g) and not g.IsConnected(f.Components[I]) then
            begin
              {$IFNDEF LCLLIB}
              MenuItem := AItem.AddItem(f.Components[I].Name, 0, False, True, {$IFDEF WEBLIB}@{$ENDIF}MenuItemExecute);
              MenuItem.Tag := I;
              {$ENDIF}
              {$IFDEF LCLLIB}
              MenuItem := TMenuItem.Create(AItem);
              MenuItem.Tag := I;
              MenuItem.Caption := f.Components[I].Name;
              MenuItem.OnClick := MenuItemExecute;
              AItem.Add(MenuItem);
              {$ENDIF}
            end
          end;
        end;
      end;
    end;
  end;
end;

{$IFNDEF WEBLIB}
{$IFNDEF LCLLIB}
procedure TTMSFNCDataBinderEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ENDIF}
{$IFDEF LCLLIB}
procedure TTMSFNCDataBinderEditor.EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;
{$ENDIF}

procedure TTMSFNCDataBinderEditor.ExecuteVerb(Index: Integer{$IFDEF WEBLIB}; AProc: TModalResultProc{$ENDIF});
var
  e: TTMSFNCDataBindingEditor;
  db: TTMSFNCDataBinder;
begin
  inherited ExecuteVerb(Index{$IFDEF WEBLIB}, AProc{$ENDIF});

  if (Component is TTMSFNCDataBinder) then
  begin
    db := TTMSFNCDataBinder(Component);
    case Index - inherited GetVerbCount of
      0:
      begin
        e := TTMSFNCDataBindingEditor.Create(db);
        try
          e.DataBinder := db;
          e.Execute
          {$IFDEF WEBLIB}
          (procedure(AResult: TModalResult)
          begin
            e.Free;
            if Assigned(AProc) then
              AProc(AResult);
          end)
          {$ENDIF}
          ;
        finally
          {$IFNDEF WEBLIB}
          e.Free;
          {$ENDIF}
        end;
      end;
    end;
  end;
end;

function TTMSFNCDataBinderEditor.GetVerb(Index: Integer): string;
begin
  Result := inherited GetVerb(Index);

  case Index - inherited GetVerbCount of
    0: Result := 'Edit...';
    1: Result := 'Bind...';
  end;
end;

function TTMSFNCDataBinderEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{ TTMSFNCDataBinderObjectProperty }

function TTMSFNCDataBinderObjectProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TTMSFNCDataBinderObjectProperty.GetValues(Proc: TGetStrProc);
var
  p: TPersistent;
  i, k: Integer;
  col: TTMSFNCDataBinderItem;
  a, aa: TTMSFNCDataBinderPropertyList;
  o: TObject;
  oi: TObject;
begin
  p := GetComponent(0);
  if not Assigned(p) then
    Exit;

  col := nil;
  if (p is TTMSFNCDataBinderItem) then
    col := (p as TTMSFNCDataBinderItem)
  else if p is TTMSFNCDataBinderPropertyName then
    col := (p as TTMSFNCDataBinderPropertyName).DataBinderItem;

  if Assigned(col) and Assigned(col.&Object) then
  begin
    a := TTMSFNCDataBinder.GetPropertyList(col.&Object);
    for I := 0 to Length(a) - 1 do
    begin
      if col.PropertyName <> '' then
      begin
        if TTMSFNCPersistence.GetPropInfoName(a[I]) = col.PropertyName then
        begin
          if TTMSFNCPersistence.GetPropInfoType(a[I]) = tkClass then
          begin
            o := GetObjectProp(col.&Object, col.PropertyName);
            if Assigned(o) then
            begin
              if o is TCollection then
              begin
                oi := (o as TCollection).ItemClass.Create(nil);
                try
                  aa := TTMSFNCDataBinder.GetPropertyList(oi);
                finally
                  oi.Free;
                end;
              end
              else
                aa := TTMSFNCDataBinder.GetPropertyList(o);

              for K := 0 to Length(aa) - 1 do
                Proc(TTMSFNCPersistence.GetPropInfoName(aa[K]));
              Break;
            end;
          end;
        end;
      end
      else
        Proc(TTMSFNCPersistence.GetPropInfoName(a[I]));
    end;
  end;
end;

{ TTMSFNCDataBinderColumnsProperty }

function TTMSFNCDataBinderColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TTMSFNCDataBinderColumnsProperty.GetValues(Proc: TGetStrProc);
var
  p: TPersistent;
  i: Integer;
  col: TTMSFNCDataBinderItem;
  a: TTMSFNCDataBinderPropertyList;
begin
  p := GetComponent(0);
  if not Assigned(p) then
    Exit;

  col := nil;
  if (p is TTMSFNCDataBinderItem) then
    col := (p as TTMSFNCDataBinderItem)
  else if p is TTMSFNCDataBinderPropertyName then
    col := (p as TTMSFNCDataBinderPropertyName).DataBinderItem;

  if Assigned(col) and Assigned(col.&Object) then
  begin
    a := TTMSFNCDataBinder.GetPropertyList(col.&Object);
    for I := 0 to Length(a) - 1 do
      Proc(TTMSFNCPersistence.GetPropInfoName(a[I]));
  end;
end;

{ TTMSFNCDataBinderColumnsSubProperty }

function TTMSFNCDataBinderColumnsSubProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TTMSFNCDataBinderColumnsSubProperty.GetValues(Proc: TGetStrProc);
var
  p: TPersistent;
  i, k: Integer;
  col: TTMSFNCDataBinderItem;
  a, aa: TTMSFNCDataBinderPropertyList;
  o: TObject;
  oi: TObject;
begin
  p := GetComponent(0);
  if not Assigned(p) then
    Exit;

  col := nil;
  if (p is TTMSFNCDataBinderItem) then
    col := (p as TTMSFNCDataBinderItem)
  else if p is TTMSFNCDataBinderPropertyName then
    col := (p as TTMSFNCDataBinderPropertyName).DataBinderItem;

  if Assigned(col) and Assigned(col.&Object) then
  begin
    a := TTMSFNCDataBinder.GetPropertyList(col.&Object);
    for I := 0 to Length(a) - 1 do
    begin
      if col.ColumnsPropertyName <> '' then
      begin
        if TTMSFNCPersistence.GetPropInfoName(a[I]) = col.ColumnsPropertyName then
        begin
          if TTMSFNCPersistence.GetPropInfoType(a[I]) = tkClass then
          begin
            o := GetObjectProp(col.&Object, col.ColumnsPropertyName);
            if Assigned(o) then
            begin
              if o is TCollection then
              begin
                oi := (o as TCollection).ItemClass.Create(nil);
                try
                  aa := TTMSFNCDataBinder.GetPropertyList(oi);
                finally
                  oi.Free;
                end;
              end
              else
                aa := TTMSFNCDataBinder.GetPropertyList(o);

              for K := 0 to Length(aa) - 1 do
                Proc(TTMSFNCPersistence.GetPropInfoName(aa[K]));
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;


{ TTMSFNCDataBinderFieldNameProperty }

function TTMSFNCDataBinderFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

//------------------------------------------------------------------------------

procedure TTMSFNCDataBinderFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  b: TTMSFNCDataBinderItem;
  d: TDataSource;
  ds: TDataSet;
  st: TStringList;
  i: Integer;
begin
  if (GetComponent(0) is TTMSFNCDataBinderItem) then
    b := (GetComponent(0) as TTMSFNCDataBinderItem)
  else if GetComponent(0) is TTMSFNCDataBinderFieldName then
    b := (GetComponent(0) as TTMSFNCDataBinderFieldName).DataBinderItem
  else
    Exit;

  if not Assigned(b) then
    Exit;

  d := b.DataSource;

  if not Assigned(d) then
    Exit;

  ds := d.DataSet;

  if not Assigned(ds) then
    Exit;

  st := TStringList.Create;
  ds.GetFieldNames(st);
  for i := 1 to st.Count do
    Proc(st.Strings[i - 1]);
  st.Free;
end;


end.
