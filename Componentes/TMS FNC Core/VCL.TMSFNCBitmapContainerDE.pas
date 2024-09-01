{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c)  2016 - 2021                                    }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit VCL.TMSFNCBitmapContainerDE;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, VCL.TMSFNCGeneralDE, VCL.TMSFNCBitmapContainer
  {$IFNDEF LCLWEBLIB}
  ,DesignEditors, DesignIntf
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf, Web, WEBLib.Forms
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,ComponentEditors, PropEdits
  {$ENDIF}
  ;

type
  TTMSFNCBitmapContainerDEEditor = class(TTMSFNCDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer{$IFDEF WEBLIB}; AProc: TModalResultProc{$ENDIF}); override; 
    {$IFNDEF WEBLIB}
    {$IFNDEF LCLLIB}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    {$ENDIF}
    {$IFDEF LCLLIB}
    procedure EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean); override;
    {$ENDIF}
    {$ENDIF}
    procedure FillItems(ABitmapContainer: TTMSFNCBitmapContainer); {$IFDEF WEBLIB}async;{$ENDIF}
  end;

  TTMSFNCBitmapNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  SysUtils, VCL.Dialogs, VCL.TMSFNCBitmapContainerEditor
  {$IFDEF WEBLIB}
  ,WEBLib.WEBTools
  {$ENDIF}
  {$IFNDEF LCLLIB}
  ,UITypes
  {$ENDIF}
  ;

type
  {$IFNDEF WEBLIB}
  TTMSFNCBitmapContainerDEOpenDialog = TOpenDialog;
  {$ENDIF}
  {$IFDEF WEBLIB}
  TTMSFNCBitmapContainerDEOpenDialog = TWebOpenDialog;
  {$ENDIF}

{ TTMSFNCBitmapContainerEditor }

{$IFNDEF WEBLIB}
{$IFNDEF LCLLIB}
procedure TTMSFNCBitmapContainerDEEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ENDIF}
{$IFDEF LCLLIB}
procedure TTMSFNCBitmapContainerDEEditor.EditProperty(const PropertyEditor: TPropertyEditor; var Continue: Boolean);
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

procedure TTMSFNCBitmapContainerDEEditor.FillItems(ABitmapContainer: TTMSFNCBitmapContainer);
var
  i: integer;
  str: String;
  ofd: TTMSFNCBitmapContainerDEOpenDialog;
  bi: TTMSFNCBitmapItem;
  {$IFDEF WEBLIB}
  res: boolean;
  {$ENDIF}
begin
  ofd := TTMSFNCBitmapContainerDEOpenDialog.Create(ABitmapContainer);
  ofd.Options := [TOpenOption.ofAllowMultiSelect, TOpenOption.ofFileMustExist];
  ofd.Filter := 'All (*.gif;*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf;*.svg)|*.gif;'+
  '*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf;*.svg|GIF Image (*.gif)|*.gif|Portable Network Graphics (*.png)|*.png|'+
  'JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|TIFF Images (*.tif)|*.tif|TIFF Images'+
  '(*.tiff)|*.tiff|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|Scalable Vector Graphics (*.svg)|*.svg';
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
      for I := 0 to ofd.Files.Count - 1 do
      begin
        bi := ABitmapContainer.Items.Add;
        str := ofd.Files[I]{$IFDEF WEBLIB}.Name{$ENDIF};
        bi.Name := ExtractFileName(str);

        {$IFNDEF WEBLIB}
        str := ofd.Files[I];
        bi.Bitmap.LoadFromFile(str);
        {$ENDIF}
        {$IFDEF WEBLIB}
        str := await(string, ofd.Files[I].FileAsBase64);
        str := 'data:' + ofd.Files[I].MimeType + ';base64,' + str;
        bi.Bitmap.LoadFromResource(str);
        {$ENDIF}
      end;
    end;
  finally
    ofd.Free;
  end;
end;


procedure TTMSFNCBitmapContainerDEEditor.ExecuteVerb(Index: Integer{$IFDEF WEBLIB}; AProc: TModalResultProc{$ENDIF});
var
  bmp: TTMSFNCBitmapContainer;
  bce: TTMSFNCBitmapContainerEditor;
begin
  inherited ExecuteVerb(Index{$IFDEF WEBLIB}, AProc{$ENDIF});

  bmp := nil;
  if Component is TTMSFNCBitmapContainer then
    bmp := Component as TTMSFNCBitmapContainer;

  if Assigned(bmp) then
  begin
    case Index - inherited GetVerbCount of
      0:
      begin
        bce := TTMSFNCBitmapContainerEditor.Create(bmp);
        try
          bce.BitmapContainer := bmp;
          bce.Execute
          {$IFDEF WEBLIB}
          (procedure(AResult: TModalResult)
          begin
            bce.Free;
            if Assigned(AProc) then
              AProc(AResult);
          end)
          {$ENDIF}
          ;
        finally
          {$IFNDEF WEBLIB}
          bce.Free;
          {$ENDIF}
        end;
      end;
      1, 2:
      begin
        if Index = 1 then
          bmp.Items.Clear;
        FillItems(bmp);
      end;
      3: bmp.Items.Clear;
    end;
  end;
end;

function TTMSFNCBitmapContainerDEEditor.GetVerb(Index: Integer): string;
begin
  Result := inherited GetVerb(Index);

  case Index - inherited GetVerbCount of
    0: Result := 'Open Editor';
    1: Result := 'Load files from folder...';
    2: Result := 'Add files from folder...';
    3: Result := 'Clear files';
  end;
end;

function TTMSFNCBitmapContainerDEEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 4;
end;

{ TTMSFNCBitmapNameProperty }

function TTMSFNCBitmapNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList{$IFNDEF LCLLIB}, paValueEditable{$ENDIF}];
end;

procedure TTMSFNCBitmapNameProperty.GetValues(Proc: TGetStrProc);
var
  p: TPersistent;
  b: TTMSFNCBitmapContainer;
  i: Integer;
  col: TCollection;
  bmp: ITMSFNCBitmapContainer;
  bmpg: ITMSFNCBitmapContainerGetItem;
  bmpit: TTMSFNCBitmapItem;
begin
  p := GetComponent(0);
  if not Assigned(p) then
    Exit;

  if p is TCollectionItem then
  begin
    col := (p as TCollectionItem).Collection;
    if Assigned(col) then
      p := col.Owner;
  end;

  if Assigned(p) and Supports(p, ITMSFNCBitmapContainer, bmp) then
  begin
    b := bmp.GetBitmapContainer;
    if Assigned(b) and Supports(b, ITMSFNCBitmapContainerGetItem, bmpg) then
    begin
      for I := 0 to bmpg.ItemCount - 1 do
      begin
        bmpit := bmpg.GetItem(I);
        if Assigned(bmpit) then
          Proc(bmpit.Name);
      end;
    end;
  end;
end;

end.
