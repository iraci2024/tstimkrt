
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//  Tab Editor
//////////////////////////////////////////////////

{$I SB.inc}

unit ScTabEditor;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons,
{$IFDEF DBTOOLS}
  DBToolsClient,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  SysUtils, Classes,
  ScEditor, ScFrame, ScDesignUtils, ScUtils;

type
  TScTabEditorForm = class(TScEditorForm)
    PageControl: TPageControl;
    procedure FormDestroy(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlChanging(Sender: TObject;
      var AllowChange: Boolean);

  protected
    FFramesList: TList;

    procedure Resize; override;
    procedure DoInit; override;
    procedure DoActivate; override;
    procedure SaveControlData; override;

    function AddTab(FrameClass: TScFrameClass; Page: TTabSheet): TScFrame;

    function GetFrameByInitProp: TScFrame; virtual;

    function GetModified: boolean; override;
    procedure SetModified(Value: boolean); override;

    function GetActiveFrame: TScFrame;

    // Avoid Kylix bug 
    procedure DoPageControlChange(Sender: TObject); virtual;
    procedure DoPageControlChanging(Sender: TObject; var AllowChange: Boolean); virtual;
  public
    constructor Create(Owner: TComponent; ScDesignUtilsClass: TScDesignUtilsClass); override;
    procedure ActivateFrame(Frame: TScFrame);

    property ActiveFrame: TScFrame read GetActiveFrame;    
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TScTabEditorForm }

procedure TScTabEditorForm.Resize;
begin
  inherited;

  if PageControl <> nil then begin
    PageControl.Height := Self.ClientHeight - PageControl.Top - BtnPanel.Height;
    PageControl.Width := Self.ClientWidth - PageControl.Left * 2;
  end;
end;

procedure TScTabEditorForm.DoInit;
begin
  inherited;

{$IFDEF FPC}
  PageControl.Height := Height - 49;
  PageControl.Width := Width - 16;
{$ENDIF}
end;

procedure TScTabEditorForm.DoActivate;
var
  Frame: TScFrame;
begin
  inherited;

  Frame := GetFrameByInitProp;

  if Frame <> nil then begin
    PageControl.ActivePage := Frame.Page;
    Frame.Activate;
  end
{$IFDEF FPC}
  else if (PageControl.ActivePage = nil) and (PageControl.PageCount > 0) then begin
    PageControl.ActivePage := PageControl.Pages[0];
    Frame := GetActiveFrame;
    if Frame <> nil then
      Frame.Activate;
  end;
{$ENDIF}
end;

procedure TScTabEditorForm.SaveControlData;
var
  ActiveFrame: TScFrame;
begin
  ActiveFrame := GetActiveFrame;
  if ActiveFrame <> nil then
    ActiveFrame.Finish;

  inherited;
end;

function TScTabEditorForm.AddTab(FrameClass: TScFrameClass; Page: TTabSheet): TScFrame;
begin
  Result := FrameClass.Create(Self);
  Result.Parent := Page;
  Result.Align := alClient;
  Result.Name := Page.Name + FrameClass.ClassName;
  Result.Editor := Self;

  FFramesList.Add(Result);
end;

function TScTabEditorForm.GetModified: boolean;
var
  i :integer;
begin
  Result := inherited GetModified;
  for i := 0 to FFramesList.Count - 1 do
    Result := Result or TScFrame(FFramesList[i]).Modified;
end;

procedure TScTabEditorForm.SetModified(Value: boolean);
var
  i :integer;
begin
  inherited;
  for i := 0 to FFramesList.Count - 1 do
    TScFrame(FFramesList[i]).Modified := Value;
end;

constructor TScTabEditorForm.Create(Owner: TComponent; ScDesignUtilsClass: TScDesignUtilsClass);
begin
  FFramesList := TList.Create;
  inherited;
end;

procedure TScTabEditorForm.FormDestroy(Sender: TObject);
begin
  inherited;
  FFramesList.Free;
end;

function TScTabEditorForm.GetActiveFrame: TScFrame;
var
  i: integer;
begin
  for i := 0 to FFramesList.Count - 1 do
    if TScFrame(FFramesList[i]).Page = PageControl.ActivePage then begin
      Result := TScFrame(FFramesList[i]);
      Exit;
    end;
  Result := nil;
end;

procedure TScTabEditorForm.ActivateFrame(Frame: TScFrame);
var
  ActiveFrame: TScFrame;
begin
  ActiveFrame := GetActiveFrame;
  if ActiveFrame <> nil then
    ActiveFrame.Finish;

  if Frame.Page <> PageControl.ActivePage then
    PageControl.ActivePage := Frame.Page;
  Frame.Activate;
end;

procedure TScTabEditorForm.DoPageControlChange(Sender: TObject);
var
  ActiveFrame: TScFrame;
begin
  ActiveFrame := GetActiveFrame;
  if ActiveFrame <> nil then
    ActiveFrame.Activate;
end;

procedure TScTabEditorForm.PageControlChange(Sender: TObject);
begin
  DoPageControlChange(Sender); 
end;

procedure TScTabEditorForm.DoPageControlChanging(Sender: TObject; var AllowChange: Boolean); 
var
  ActiveFrame: TScFrame;
begin
  try
    ActiveFrame := GetActiveFrame;
    if ActiveFrame <> nil then
      ActiveFrame.Finish;
  except
    on E: Exception do begin
      AllowChange := False;
      ApplicationHandleException(E);
    end;
  end;
end;

procedure TScTabEditorForm.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  DoPageControlChanging(Sender, AllowChange); 
end;

function TScTabEditorForm.GetFrameByInitProp: TScFrame;
var
  i :integer;
begin
  Result := nil;
  if InitialProperty <> '' then begin
    for i := 0 to FFramesList.Count - 1 do
      if TScFrame(FFramesList[i]).Page.Caption = InitialProperty then begin
        Result := TScFrame(FFramesList[i]);
        Break;
      end;

    Assert(Result <> nil, 'Unknown frame ' + InitialProperty);
  end;
end;

end.
