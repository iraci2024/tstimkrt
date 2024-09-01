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

unit WEBLib.TMSFNCResponsiveManagerDE;

{$I WEBLib.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, WEBLib.TMSFNCResponsiveManager, WEBLib.TMSFNCGeneralDE, WEBLib.TMSFNCResponsiveManagerDimensionsEditor, TypInfo
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
  TTMSFNCResponsiveManagerEditor = class(TTMSFNCDefaultEditor)
  private
    FDimensions: TTMSFNCResponsiveManagerDimensionsEditorDimensions;
    function GetDimensions: TTMSFNCResponsiveManagerDimensionsEditorDimensions;
  protected
    procedure LoadDimensions; virtual;
    property Dimensions: TTMSFNCResponsiveManagerDimensionsEditorDimensions read GetDimensions;
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
    procedure OpenSettings(AManager: TTMSFNCResponsiveManager); {$IFDEF WEBLIB}async;{$ENDIF}
  end;

  TTMSFNCResponsiveManagerStateProperty = class(TIntegerProperty)
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
  WEBLib.Controls, SysUtils, WEBLib.Dialogs, WEBLib.TMSFNCUtils, WEBLib.TMSFNCTypes,
  WEBLib.TMSFNCPersistence, WEBLib.TMSFNCStateManager
  {$IFDEF FMXLIB}
  ,WEBLib.Forms, JSON
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
  TTMSFNCResponsiveManagerOpen = class(TTMSFNCResponsiveManager);
  {$IFNDEF WEBLIB}
  TTMSFNCResponsiveDEOpenDialog = TOpenDialog;
  {$ENDIF}
  {$IFDEF WEBLIB}
  TTMSFNCResponsiveDEOpenDialog = TWebOpenDialog;
  {$ENDIF}


{ TTMSFNCResponsiveManagerEditor }

procedure TTMSFNCResponsiveManagerEditor.MenuItemSaveExecute(Sender: TObject);
var
  r: TTMSFNCResponsiveManager;
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
    if Component is TTMSFNCResponsiveManager then
      r := Component as TTMSFNCResponsiveManager;

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

procedure TTMSFNCResponsiveManagerEditor.MenuItemSelectExecute(Sender: TObject);
var
  r: TTMSFNCResponsiveManager;
  idx: Integer;
  c: TTMSFNCStateManagerControl;
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
    if Component is TTMSFNCResponsiveManager then
      r := Component as TTMSFNCResponsiveManager;

    if Assigned(r) then
    begin
      f := TTMSFNCUtils.GetParentForm(r);
      if Assigned(f) then
      begin
        c := nil;
        if idx = 0 then
          c := f
        else
        begin
          if f.Components[idx - 1] is TTMSFNCStateManagerControl then
            c := f.Components[idx - 1] as TTMSFNCStateManagerControl
        end;

        if Assigned(c) then
        begin
          if (Assigned(r.Control) and (TTMSFNCUtils.&Message('Are you sure you want to change the responsive control from ' +
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

procedure TTMSFNCResponsiveManagerEditor.MenuItemLoadExecute(Sender: TObject);
var
  r: TTMSFNCResponsiveManager;
  s: TTMSFNCResponsiveManagerItem;
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
    if Component is TTMSFNCResponsiveManager then
      r := Component as TTMSFNCResponsiveManager;

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

procedure TTMSFNCResponsiveManagerEditor.MenuItemDimensionExecute(Sender: TObject);
var
  r: TTMSFNCResponsiveManager;
  idx: Integer;
  f: TCustomForm;
  w, h: Integer;
  e: TTMSFNCResponsiveManagerDimensionsEditor;
  it: TTMSFNCResponsiveManagerDimensionsEditorDimension;
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
    if Component is TTMSFNCResponsiveManager then
      r := Component as TTMSFNCResponsiveManager;

    if Assigned(r) then
    begin
      if idx = 0 then
      begin
        e := TTMSFNCResponsiveManagerDimensionsEditor.Create(Application);
        try
          e.Execute;
        finally
          e.Free;
        end;
      end
      else if idx > 0 then
      begin     
        f := TTMSFNCUtils.GetParentForm(r);
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
procedure TTMSFNCResponsiveManagerEditor.PrepareItem(Index: Integer;
  const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TTMSFNCResponsiveManagerEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
{$ENDIF}
var
  r: TTMSFNCResponsiveManager;
  I: Integer;
  f: TCustomForm;
begin
  inherited PrepareItem(Index, AItem);

  r := nil;
  if Component is TTMSFNCResponsiveManager then
    r := Component as TTMSFNCResponsiveManager;

  if Assigned(r) then
  begin
    case Index - inherited GetVerbCount of
      0:
      begin
        f := TTMSFNCUtils.GetParentForm(r);
        if Assigned(f) then
        begin
          AddSelectSubMenuItem(0, f.Name, r.Control = f, AItem);
          for I := 0 to f.ComponentCount - 1 do
          begin
            if (f.Components[I] <> r) and (f.Components[I] is TTMSFNCStateManagerControl) then
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
procedure TTMSFNCResponsiveManagerEditor.AddLoadSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TTMSFNCResponsiveManagerEditor.AddLoadSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: IMenuItem);
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
procedure TTMSFNCResponsiveManagerEditor.AddDimensionSubMenuItem(ATag: Integer; AText: string; const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TTMSFNCResponsiveManagerEditor.AddDimensionSubMenuItem(ATag: Integer; AText: string; const AItem: IMenuItem);
{$ENDIF}
var
  {$IFDEF LCLLIB}
  MenuItem, m: TMenuItem;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  MenuItem, m: IMenuItem;
  {$ENDIF}
  I: Integer;
  d: TTMSFNCResponsiveManagerDimensionsEditorDimension;
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
procedure TTMSFNCResponsiveManagerEditor.AddSelectSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TTMSFNCResponsiveManagerEditor.AddSelectSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: IMenuItem);
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

destructor TTMSFNCResponsiveManagerEditor.Destroy;
begin
  FreeAndNil(FDimensions);
  inherited;
end;

{$IFDEF LCLLIB}
procedure TTMSFNCResponsiveManagerEditor.AddSaveSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: TMenuItem);
{$ENDIF}
{$IFNDEF LCLLIB}
procedure TTMSFNCResponsiveManagerEditor.AddSaveSubMenuItem(ATag: Integer; AText: string; AChecked: Boolean; const AItem: IMenuItem);
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
procedure TTMSFNCResponsiveManagerEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ENDIF}
{$IFDEF LCLLIB}
procedure TTMSFNCResponsiveManagerEditor.EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean);
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

procedure TTMSFNCResponsiveManagerEditor.OpenSettings(AManager: TTMSFNCResponsiveManager);
var
  {$IFDEF WEBLIB}
  s: TStream;
  res: Boolean;
  {$ENDIF}
  ofd: TTMSFNCResponsiveDEOpenDialog;
  ref: TObject;
begin
  ofd := TTMSFNCResponsiveDEOpenDialog.Create(AManager);
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
        ref := TTMSFNCPersistence.IOReference;
        TTMSFNCPersistence.IOReference := AManager.States;
        {$IFNDEF WEBLIB}
        AManager.States.LoadFromJSONFile(ofd.Files[0]);
        {$ENDIF}
        {$IFDEF WEBLIB}
        s := await(TStream, ofd.Files[0].FileAsStream);
        AManager.States.LoadFromJSONStream(s);
        {$ENDIF}
        TTMSFNCPersistence.IOReference := ref;
      end;
    end;
  finally
    ofd.Free;
  end;
end;

procedure TTMSFNCResponsiveManagerEditor.ExecuteVerb(Index: Integer{$IFDEF WEBLIB}; AProc: TModalResultProc{$ENDIF});
var
  r: TTMSFNCResponsiveManager;
  {$IFNDEF WEBLIB}
  sd: TSaveDialog;
  fn: string;
  fe: string;
  {$ENDIF}
  ref: TObject;
begin
  inherited ExecuteVerb(Index{$IFDEF WEBLIB}, AProc{$ENDIF});

  if (Component is TTMSFNCResponsiveManager) then
  begin
    r := TTMSFNCResponsiveManager(Component);
    case Index - inherited GetVerbCount of
      4: r.Preview;
      5:
      begin
        if TTMSFNCUtils.&Message('Are you sure you want to clear existing states?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
          r.States.Clear;
      end;
      7:
      begin
        if TTMSFNCUtils.&Message('Are you sure you want to optimize the current states? This action will remove all unnecessary information from each state. ' +
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

            ref := TTMSFNCPersistence.IOReference;
            TTMSFNCPersistence.IOReference := r.States;
            r.States.SaveToJSONFile(fn);
            TTMSFNCPersistence.IOReference := ref;
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

function TTMSFNCResponsiveManagerEditor.GetDimensions: TTMSFNCResponsiveManagerDimensionsEditorDimensions;
begin
  if not Assigned(FDimensions) then
  begin
    FDimensions := TTMSFNCResponsiveManagerDimensionsEditorDimensions.Create;
    LoadDimensions;
  end;

  Result := FDimensions;
end;

function TTMSFNCResponsiveManagerEditor.GetVerb(Index: Integer): string;
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

function TTMSFNCResponsiveManagerEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 11;
end;

procedure TTMSFNCResponsiveManagerEditor.LoadDimensions;
 {$IFNDEF WEBLIB}
var
  r: TResourceStream;
  sl: TStringList;
  j, a, va, sr: TJSONValue;
  I: Integer;
  it: TTMSFNCResponsiveManagerDimensionsEditorDimension;
  {$IFDEF MSWINDOWS}
  f, fn: string;
  RegIniFile: TRegIniFile;
  {$ENDIF}
{$ENDIF}
begin
  {$IFNDEF WEBLIB}
  r := TTMSFNCUtils.GetResourceStream('TMSFNCRESPONSIVEMANAGERDEVICELIST', HInstance);
  if Assigned(r) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromStream(r);
      j := TTMSFNCUtils.ParseJSON(sl.Text);
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
                it := TTMSFNCResponsiveManagerDimensionsEditorDimension.Create;

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
  RegIniFile := TRegIniFile.Create('TMSFNCResponsiveManagerDimensions');
  try
    RegIniFile.ReadSectionValues('', sl);
    for I := 0 to sl.Count - 1 do
    begin
      f := sl.Names[I];
      fn := sl.Values[f];
      it := TTMSFNCResponsiveManagerDimensionsEditorDimension.Create;
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

{ TTMSFNCResponsiveManagerStateProperty }

function TTMSFNCResponsiveManagerStateProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList{$IFNDEF LCLLIB}, paValueEditable{$ENDIF}];
end;

function TTMSFNCResponsiveManagerStateProperty.GetValue: string;
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
  if Assigned(p) and (p is TTMSFNCResponsiveManager) then
  begin
    if (L >= 0) and (L <= (p as TTMSFNCResponsiveManager).States.Count - 1) then
      Result := (p as TTMSFNCResponsiveManager).States[L].Name;
  end;
end;

procedure TTMSFNCResponsiveManagerStateProperty.GetValues(Proc: TGetStrProc);
var
  p: TPersistent;
  b: TTMSFNCResponsiveManager;
  i: Integer;
begin
  p := GetComponent(0);
  if not Assigned(p) then
    Exit;

  if Assigned(p) and (p is TTMSFNCResponsiveManager) then
  begin
    b := p as TTMSFNCResponsiveManager;
    for I := 0 to b.States.Count - 1 do
      Proc(b.States[I].Name);
  end;
end;

procedure TTMSFNCResponsiveManagerStateProperty.SetValue(const Value: string);
var
  s: TTMSFNCStateManagerItem;
  p: TPersistent;
  L: Integer;
begin
  p := GetComponent(0);
  L := -1;
  if Assigned(p) and (p is TTMSFNCResponsiveManager) then
  begin
    s := (p as TTMSFNCResponsiveManager).FindStateByName(Value);
    if Assigned(s) then
      L := s.Index;
  end;
  SetOrdValue(L);
end;

end.
