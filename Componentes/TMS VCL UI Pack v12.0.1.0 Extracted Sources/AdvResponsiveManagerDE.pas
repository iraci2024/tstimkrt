{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c)  2022                                           }
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

unit AdvResponsiveManagerDE;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, AdvResponsiveManager, AdvGeneralDE, AdvResponsiveManagerDimensionsEditor, TypInfo
  {$IFNDEF LCLWEBLIB}
  ,DesignEditors, DesignIntf, DesignMenus, VCL.Menus
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf, Web, WEBLib.Forms
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,ComponentEditors, PropEdits, Menus, fgl
  {$ELSE}
  ,Generics.Collections
  {$ENDIF}
  ;

type
  TAdvResponsiveManagerEditor = class(TAdvDefaultEditor)
  private
    FDimensions: TAdvResponsiveManagerDimensionsEditorDimensions;
    function GetDimensions: TAdvResponsiveManagerDimensionsEditorDimensions;
  protected
    procedure LoadDimensions; virtual;
    property Dimensions: TAdvResponsiveManagerDimensionsEditorDimensions read GetDimensions;
  public
    destructor Destroy; override;

    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    {$IFDEF LCLLIB}
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;
    procedure AddSaveSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: TMenuItem);
    procedure AddLoadSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: TMenuItem);
    procedure AddDimensionSubMenuItem(ATag: Integer; AText: string; const AItem: TMenuItem);
    procedure AddSelectSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: TMenuItem);
    {$ENDIF}
    {$IFNDEF LCLLIB}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
    procedure AddSaveSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: IMenuItem);
    procedure AddLoadSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: IMenuItem);
    procedure AddDimensionSubMenuItem(ATag: Integer; AText: string; const AItem: IMenuItem);
    procedure AddSelectSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: IMenuItem);
    {$ENDIF}
    procedure MenuItemSaveExecute(Sender: TObject);
    procedure MenuItemLoadExecute(Sender: TObject);
    procedure MenuItemDimensionExecute(Sender: TObject);
    procedure MenuItemSelectExecute(Sender: TObject);
    procedure ExecuteVerb(Index: Integer{$IFDEF WEBLIB}; AProc: TModalResultProc{$ENDIF}); override;
    {$IFNDEF WEBLIB}
    {$IFNDEF LCLLIB}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    {$ENDIF}
    {$IFDEF LCLLIB}
    procedure EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean); override;
    {$ENDIF}
    {$ENDIF}
    procedure OpenSettings(AManager: TAdvResponsiveManager); {$IFDEF WEBLIB}async;{$ENDIF}
  end;

  TAdvResponsiveManagerStateProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Registry,
  {$ENDIF}
  Controls, SysUtils, VCL.Dialogs, AdvUtils, AdvTypes,
  AdvPersistence, AdvStateManager
  {$IFDEF FMXLIB}
  ,Forms, JSON
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,VCL.Forms, JSON
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,Forms, fpjson
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.WEBTools, WEBLib.JSON
  {$ENDIF}
  {$IFNDEF LCLLIB}
  , UITypes
  {$ENDIF}
  ;

type
  TAdvResponsiveManagerOpen = class(TAdvResponsiveManager);
  {$IFNDEF WEBLIB}
  TAdvResponsiveDEOpenDialog = TOpenDialog;
  {$ENDIF}
  {$IFDEF WEBLIB}
  TAdvResponsiveDEOpenDialog = TWebOpenDialog;
  {$ENDIF}


{ TAdvResponsiveManagerEditor }

procedure TAdvResponsiveManagerEditor.MenuItemSaveExecute(Sender: TObject);
var
  r: TAdvResponsiveManager;
  idx: Integer;
  MenuItem: {$IFDEF WEBLIB}TObject{$ELSE}TMenuItem{$ENDIF};
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
    r := nil;
    if Component is TAdvResponsiveManager then
      r := Component as TAdvResponsiveManager;

    if Assigned(r) then
    begin
      //save
      case idx of
        0: r.SaveToNewState;
        else
        begin
          if (idx - 1 >= 0) and (idx - 1 <= r.States.Count - 1) then
            r.SaveToState(r.States[idx - 1]);
        end;
      end;
    end;
  end;
end;

procedure TAdvResponsiveManagerEditor.MenuItemSelectExecute(Sender: TObject);
var
  r: TAdvResponsiveManager;
  idx: Integer;
  c: TAdvStateManagerControl;
  f: TCustomForm;
  MenuItem: {$IFDEF WEBLIB}TObject{$ELSE}TMenuItem{$ENDIF};
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
    r := nil;
    if Component is TAdvResponsiveManager then
      r := Component as TAdvResponsiveManager;

    if Assigned(r) then
    begin
      f := TAdvUtils.GetParentForm(r);
      if Assigned(f) then
      begin
        c := nil;
        if idx = 0 then
          c := f
        else
        begin
          if f.Components[idx - 1] is TAdvStateManagerControl then
            c := f.Components[idx - 1] as TAdvStateManagerControl
        end;

        if Assigned(c) then
        begin
          if (Assigned(r.Control) and (TAdvUtils.&Message('Are you sure you want to change the responsive control from ' +
            r.Control.Name + ' to ' + c.Name + '? This will clear all current states!', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes)) or not Assigned(r.Control) then
          begin
            r.States.Clear;
            r.Control := c;
            {$IFNDEF WEBLIB}
            Designer.Modified;
            {$ENDIF}
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvResponsiveManagerEditor.MenuItemLoadExecute(Sender: TObject);
var
  r: TAdvResponsiveManager;
  s: TAdvResponsiveManagerItem;
  idx: Integer;
  MenuItem: {$IFDEF WEBLIB}TObject{$ELSE}TMenuItem{$ENDIF};
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
    r := nil;
    if Component is TAdvResponsiveManager then
      r := Component as TAdvResponsiveManager;

    if Assigned(r) then
    begin
      //load
      case idx of
        0: r.LoadState;
        else
        begin
          if (idx - 1 >= 0) and (idx - 1 <= r.States.Count - 1) then
          begin
            s := r.States[idx - 1];
            s.Load;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvResponsiveManagerEditor.MenuItemDimensionExecute(Sender: TObject);
var
  r: TAdvResponsiveManager;
  idx: Integer;
  f: TCustomForm;
  w, h: Integer;
  e: TAdvResponsiveManagerDimensionsEditor;
  it: TAdvResponsiveManagerDimensionsEditorDimension;
  MenuItem: {$IFDEF WEBLIB}TObject{$ELSE}TMenuItem{$ENDIF};
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
    r := nil;
    if Component is TAdvResponsiveManager then
      r := Component as TAdvResponsiveManager;

    if Assigned(r) then
    begin
      if idx = 0 then
      begin
        e := TAdvResponsiveManagerDimensionsEditor.Create(Application);
        try
          e.Execute;
        finally
          e.Free;
        end;
      end
      else if idx > 0 then
      begin     
        f := TAdvUtils.GetParentForm(r);
        if Assigned(f) then
        begin
          if idx = 1 then
          begin
            w := f.ClientWidth;
            h := f.ClientHeight;      

            f.ClientWidth := h;
            f.ClientHeight := w;                      
          end
          else
          begin
            if (idx - 2 >= 0) and (idx - 2 <= Dimensions.Count - 1) then
            begin
              it := Dimensions[idx - 2];
              f.ClientWidth := it.Width;
              f.ClientHeight := it.Height;
              {$IFNDEF WEBLIB}
              Designer.Modified;
              {$ENDIF}
            end;
          end;        
        end;
      end;
    end;
  end;
end;

{$IFDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.PrepareItem(Index: Integer;
  const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
{$ENDIF}
var
  r: TAdvResponsiveManager;
  I: Integer;
  f: TCustomForm;
begin
  inherited PrepareItem(Index, AItem);

  r := nil;
  if Component is TAdvResponsiveManager then
    r := Component as TAdvResponsiveManager;

  if Assigned(r) then
  begin
    case Index - inherited GetVerbCount of
      0:
      begin
        f := TAdvUtils.GetParentForm(r);
        if Assigned(f) then
        begin
          AddSelectSubMenuItem(0, f.Name, r.Control = f, AItem);
          for I := 0 to f.ComponentCount - 1 do
          begin
            if (f.Components[I] <> r) and (f.Components[I] is TAdvStateManagerControl) then
            begin
              AddSelectSubMenuItem(I + 1, f.Components[I].Name, r.Control = f.Components[I], AItem);
            end
          end;
        end;
      end;
      1:
      begin
        AddSaveSubMenuItem(0, 'Save To New State', false, AItem);
        AddSaveSubMenuItem(-1, '-', false, AItem);
        for I := 0 to r.States.Count - 1 do
          AddSaveSubMenuItem(I + 1, 'Save To ' + r.States[I].Name, r.ActiveState = I, AItem);
      end;
      2:
      begin
        AddLoadSubMenuItem(0, 'Load Active State', false, AItem);
        AddLoadSubMenuItem(-1, '-', false, AItem);        
        for I := 0 to r.States.Count - 1 do
          AddLoadSubMenuItem(I + 1, 'Load From ' + r.States[I].Name, r.ActiveState = I, AItem);
      end;
      6:
      begin
        AddDimensionSubMenuItem(0, 'Edit...', AItem);
        AddDimensionSubMenuItem(1, 'Rotate',  AItem);                      
        AddDimensionSubMenuItem(-1, '-', AItem);

        {$IFNDEF WEBLIB}
        AddDimensionSubMenuItem(-4, 'Desktop', AItem);
        AddDimensionSubMenuItem(-3, 'Phone', AItem);
        AddDimensionSubMenuItem(-2, 'Tablet', AItem);
        {$ENDIF}
      end;
    end;
  end;
end;

{$IFDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.AddLoadSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.AddLoadSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: IMenuItem);
{$ENDIF}
var
  {$IFDEF LCLLIB}
  MenuItem: TMenuItem;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  MenuItem: IMenuItem;
  {$ENDIF}
begin
  {$IFNDEF LCLLIB}
  MenuItem := AItem.AddItem(AText, 0, AChecked, True, {$IFDEF WEBLIB}@{$ENDIF}MenuItemLoadExecute);
  MenuItem.Tag := ATag;
  {$ENDIF}
  {$IFDEF LCLLIB}
  MenuItem := TMenuItem.Create(AItem);
  MenuItem.Tag := ATag;
  MenuItem.Caption := AText;
  MenuItem.Checked := AChecked;
  MenuItem.OnClick := MenuItemLoadExecute;
  AItem.Add(MenuItem);
  {$ENDIF}
end;

{$IFDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.AddDimensionSubMenuItem(ATag: Integer; AText: string; const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.AddDimensionSubMenuItem(ATag: Integer; AText: string; const AItem: IMenuItem);
{$ENDIF}
var
  {$IFDEF LCLLIB}
  MenuItem, m: TMenuItem;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  MenuItem, m: IMenuItem;
  {$ENDIF}
  I: Integer;
  d: TAdvResponsiveManagerDimensionsEditorDimension;
begin
  {$IFNDEF LCLLIB}
  MenuItem := AItem.AddItem(AText, 0, False, True, {$IFDEF WEBLIB}@{$ENDIF}MenuItemDimensionExecute);
  MenuItem.Tag := ATag;

  if ATag < -1 then
  begin
    for I := 0 to Dimensions.Count - 1 do
    begin
      d := Dimensions[I];
      if d.Default and ((Integer(d.&Type) - 4) = ATag) then
      begin
        m := MenuItem.AddItem(d.Title, 0, False, True, {$IFDEF WEBLIB}@{$ENDIF}MenuItemDimensionExecute);
        m.Tag := I + 2;
      end;
    end;
  end;

  {$ENDIF}
  {$IFDEF LCLLIB}
  MenuItem := TMenuItem.Create(AItem);
  MenuItem.Tag := ATag;
  MenuItem.Caption := AText;
  MenuItem.OnClick := MenuItemDimensionExecute;
  AItem.Add(MenuItem);
  {$ENDIF}
end;


{$IFDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.AddSelectSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.AddSelectSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: IMenuItem);
{$ENDIF}
var
  {$IFDEF LCLLIB}
  MenuItem: TMenuItem;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  MenuItem: IMenuItem;
  {$ENDIF}
begin
  {$IFNDEF LCLLIB}
  MenuItem := AItem.AddItem(AText, 0, AChecked, True, {$IFDEF WEBLIB}@{$ENDIF}MenuItemSelectExecute);
  MenuItem.Tag := ATag;
  {$ENDIF}
  {$IFDEF LCLLIB}
  MenuItem := TMenuItem.Create(AItem);
  MenuItem.Tag := ATag;
  MenuItem.Caption := AText;
  MenuItem.OnClick := MenuItemSelectExecute;
  MenuItem.Checked := AChecked;
  AItem.Add(MenuItem);
  {$ENDIF}
end;

destructor TAdvResponsiveManagerEditor.Destroy;
begin
  FreeAndNil(FDimensions);
  inherited;
end;

{$IFDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.AddSaveSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.AddSaveSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: IMenuItem);
{$ENDIF}
var
  {$IFDEF LCLLIB}
  MenuItem: TMenuItem;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  MenuItem: IMenuItem;
  {$ENDIF}
begin
  {$IFNDEF LCLLIB}
  MenuItem := AItem.AddItem(AText, 0, AChecked, True, {$IFDEF WEBLIB}@{$ENDIF}MenuItemSaveExecute);
  MenuItem.Tag := ATag;
  {$ENDIF}
  {$IFDEF LCLLIB}
  MenuItem := TMenuItem.Create(AItem);
  MenuItem.Tag := ATag;
  MenuItem.Caption := AText;
  MenuItem.Checked := AChecked;
  MenuItem.OnClick := MenuItemSaveExecute;
  AItem.Add(MenuItem);
  {$ENDIF}
end;

{$IFNDEF WEBLIB}
{$IFNDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ENDIF}
{$IFDEF LCLLIB}
procedure TAdvResponsiveManagerEditor.EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'States') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;
{$ENDIF}

procedure TAdvResponsiveManagerEditor.OpenSettings(AManager: TAdvResponsiveManager);
var
  {$IFDEF WEBLIB}
  s: TStream;
  res: Boolean;
  {$ENDIF}
  ofd: TAdvResponsiveDEOpenDialog;
  ref: TObject;
begin
  ofd := TAdvResponsiveDEOpenDialog.Create(AManager);
  ofd.Options := [TOpenOption.ofFileMustExist];
  ofd.Filter := 'All (*.*)|*.*|Responsive Settings file (*.rsettings)|*.rsettings';
  ofd.FilterIndex := 0;
  try
    {$IFNDEF WEBLIB}
    if ofd.Execute then
    {$ENDIF}
    {$IFDEF WEBLIB}
    res := await(Boolean, ofd.Perform);
    if res then
    {$ENDIF}
    begin
      if ofd.Files.Count > 0 then
      begin
        ref := TAdvPersistence.IOReference;
        TAdvPersistence.IOReference := AManager.States;
        {$IFNDEF WEBLIB}
        AManager.States.LoadFromJSONFile(ofd.Files[0]);
        {$ENDIF}
        {$IFDEF WEBLIB}
        s := await(TStream, ofd.Files[0].FileAsStream);
        AManager.States.LoadFromJSONStream(s);
        {$ENDIF}
        TAdvPersistence.IOReference := ref;
      end;
    end;
  finally
    ofd.Free;
  end;
end;

procedure TAdvResponsiveManagerEditor.ExecuteVerb(Index: Integer{$IFDEF WEBLIB}; AProc: TModalResultProc{$ENDIF});
var
  r: TAdvResponsiveManager;
  {$IFNDEF WEBLIB}
  sd: TSaveDialog;
  fn: string;
  fe: string;
  {$ENDIF}
  ref: TObject;
begin
  inherited ExecuteVerb(Index{$IFDEF WEBLIB}, AProc{$ENDIF});

  if (Component is TAdvResponsiveManager) then
  begin
    r := TAdvResponsiveManager(Component);
    case Index - inherited GetVerbCount of
      4: r.Preview;
      5:
      begin
        if TAdvUtils.&Message('Are you sure you want to clear existing states?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
          r.States.Clear;
      end;
      7:
      begin
        if TAdvUtils.&Message('Are you sure you want to optimize the current states? This action will remove all unnecessary information from each state. ' +
          'While this will lower the size of the form file, modifying controls afterwards will require to re-save all existing states. Note that optimization automatically happens at runtime.', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
          r.Optimize;
      end;
      9:
      begin
        {$IFNDEF WEBLIB}
        sd := TSaveDialog.Create(Component);
        sd.Filter := 'All (*.*)|*.*|Responsive Settings file (*.rsettings)|*.rsettings';
        sd.DefaultExt := 'rsettings';
        sd.Options := sd.Options + [TOpenOption.ofOverwritePrompt];
        sd.FilterIndex := 2;

        try
          if sd.Execute then
          begin
            fn := sd.FileName;

            fe := Uppercase(ExtractFileExt(sd.FileName));

            if (fe = '') then
            begin
              if sd.FilterIndex = 2 then
                fn := fn + '.rsettings';
            end
            else
            begin
              if fe = '.rsettings' then
                sd.FilterIndex := 2;
            end;

            ref := TAdvPersistence.IOReference;
            TAdvPersistence.IOReference := r.States;
            r.States.SaveToJSONFile(fn);
            TAdvPersistence.IOReference := ref;
          end;
        finally
          sd.Free;
        end;
        {$ENDIF}
      end;
      10:
      begin
        OpenSettings(r);
      end;
    end;
  end;
end;

function TAdvResponsiveManagerEditor.GetDimensions: TAdvResponsiveManagerDimensionsEditorDimensions;
begin
  if not Assigned(FDimensions) then
  begin
    FDimensions := TAdvResponsiveManagerDimensionsEditorDimensions.Create;
    LoadDimensions;
  end;

  Result := FDimensions;
end;

function TAdvResponsiveManagerEditor.GetVerb(Index: Integer): string;
begin
  Result := inherited GetVerb(Index);

  case Index - inherited GetVerbCount of
    0: Result := 'Select';
    1: Result := 'Save';
    2: Result := 'Load';
    3: Result := '-';
    4: Result := 'Preview';
    5: Result := 'Clear States';
    6: Result := 'Form Dimensions';
    7: Result := 'Optimize';
    8: Result := '-';
    9: Result := 'Save Settings';
    10: Result := 'Load Settings';
  end;
end;

function TAdvResponsiveManagerEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 11;
end;

procedure TAdvResponsiveManagerEditor.LoadDimensions;
 {$IFNDEF WEBLIB}
var
  r: TResourceStream;
  sl: TStringList;
  j, a, va, sr: TJSONValue;
  I: Integer;
  it: TAdvResponsiveManagerDimensionsEditorDimension;
  {$IFDEF MSWINDOWS}
  f, fn: string;
  RegIniFile: TRegIniFile;
  {$ENDIF}
{$ENDIF}
begin
  {$IFNDEF WEBLIB}
  r := TAdvUtils.GetResourceStream('ADVRESPONSIVEMANAGERDEVICELIST', HInstance);
  if Assigned(r) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromStream(r);
      j := TAdvUtils.ParseJSON(sl.Text);
      if Assigned(j) then
      begin
        try
          a := j['devices'];
          if a is TJSONArray then
          begin
            for I := 0 to a.AsArray.Length - 1 do
            begin
              va := a.AsArray[I];
              if Assigned(va['default']) then
              begin
                it := TAdvResponsiveManagerDimensionsEditorDimension.Create;

                if Assigned(va['type']) then
                begin
                  if va['type'].AsString = 'desktop' then
                    it.&Type := edtDesktop
                  else if va['type'].AsString = 'phone' then
                    it.&Type := edtPhone
                  else if va['type'].AsString = 'tablet' then
                    it.&Type := edtTablet
                end;

                if Assigned(va['title']) then
                  it.Title := va['title'].AsString;

                if Assigned(va['default']) then
                  it.&Default := va['default'].AsBoolean;

                if Assigned(va['screen']) then
                begin
                  sr := va['screen']['vertical'];
                  if not Assigned(sr) then
                    sr := va['screen']['horizontal'];

                  if Assigned(sr) then
                  begin
                    if Assigned(sr['width']) and Assigned(sr['height']) then
                    begin
                      it.Width := sr['width'].AsInteger;
                      it.Height := sr['height'].AsInteger;

                      it.Title := it.Title + '  [W=' + IntToStr(it.Width) + ', H=' + IntToStr(it.Height) + ']';
                    end;
                  end;
                end;

                Dimensions.Add(it);
              end;
            end;
          end;
        finally
          j.Free;
        end;
      end;
    finally
      sl.Free;
      r.Free;
    end;
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  sl := TStringList.Create;
  RegIniFile := TRegIniFile.Create('AdvResponsiveManagerDimensions');
  try
    RegIniFile.ReadSectionValues('', sl);
    for I := 0 to sl.Count - 1 do
    begin
      f := sl.Names[I];
      fn := sl.Values[f];
      it := TAdvResponsiveManagerDimensionsEditorDimension.Create;
      it.Resource := False;
      it.FromJSON(fn);
      it.Title := it.Title + '  [W=' + IntToStr(it.Width) + ', H=' + IntToStr(it.Height) + ']';

      Dimensions.Add(it);
    end;
  finally
    RegIniFile.Free;
  end;
  {$ENDIF}
end;

{ TAdvResponsiveManagerStateProperty }

function TAdvResponsiveManagerStateProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList{$IFNDEF LCLLIB}, paValueEditable{$ENDIF}];
end;

function TAdvResponsiveManagerStateProperty.GetValue: string;
var
  L: Integer;
  p: TPersistent;
begin
  Result := '';
  {$IFNDEF WEBLIB}
  with GetTypeData(GetPropType)^ do if OrdType = otULong then
    L := Cardinal(GetOrdValue)
  else
  {$ENDIF}
    L := GetOrdValue;

  p := GetComponent(0);
  if Assigned(p) and (p is TAdvResponsiveManager) then
  begin
    if (L >= 0) and (L <= (p as TAdvResponsiveManager).States.Count - 1) then
      Result := (p as TAdvResponsiveManager).States[L].Name;
  end;
end;

procedure TAdvResponsiveManagerStateProperty.GetValues(Proc: TGetStrProc);
var
  p: TPersistent;
  b: TAdvResponsiveManager;
  i: Integer;
begin
  p := GetComponent(0);
  if not Assigned(p) then
    Exit;

  if Assigned(p) and (p is TAdvResponsiveManager) then
  begin
    b := p as TAdvResponsiveManager;
    for I := 0 to b.States.Count - 1 do
      Proc(b.States[I].Name);
  end;
end;

procedure TAdvResponsiveManagerStateProperty.SetValue(const Value: string);
var
  s: TAdvStateManagerItem;
  p: TPersistent;
  L: Integer;
begin
  p := GetComponent(0);
  L := -1;
  if Assigned(p) and (p is TAdvResponsiveManager) then
  begin
    s := (p as TAdvResponsiveManager).FindStateByName(Value);
    if Assigned(s) then
      L := s.Index;
  end;
  SetOrdValue(L);
end;

end.
