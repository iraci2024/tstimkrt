{===============================================================================
  RzPathBarEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2021 by Embarcadero Technologies, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzPathBarEditor
    Allows user to add items to a TRzPathBar at design-time.


  Modification History
  ------------------------------------------------------------------------------
  7.0.0  (15 March 2021)
    * Removed old conditional defines. All code designed only for RAD Studio 10.4+.
    * Added High-DPI support for all elements in all controls
    * Added improvments and optimizations in code
  ------------------------------------------------------------------------------
  6.5.0  (28 Nov 2020)
    * Removed old conditional defines. All code designed for RAD Studio 10+.
    * Updated all uses clauses to reference complete unit scope names.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzPathBarEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  System.Classes,
  Vcl.Menus,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.ExtCtrls, 
  DesignIntf,
  DesignEditors,
  DesignMenus,
  RzPathBar,
  RzDesignEditors,
  RzLabel, 
  RzPanel;

type
  TRzPathBarEditor = class( TRzComponentEditor )
  protected
    function PathBar: TRzPathBar;
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ItemsMenuHandler( Sender: TObject );
    procedure ImagesMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
    procedure Edit; override;
  end;



implementation

uses
  Winapi.Windows,
  Vcl.ImgList,
  Vcl.ActnList,
  RzCommon;


{==============================}
{== TRzPathBarEditor Methods ==}
{==============================}

function TRzPathBarEditor.PathBar: TRzPathBar;
begin
  Result := Component as TRzPathBar;
end;


function TRzPathBarEditor.GetVerbCount: Integer;
begin
  Result := 6;
end;


function TRzPathBarEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Add Item';
    1: Result := '-';
    2: Result := 'Edit Items Collection';
    3: Result := 'Edit Item';
    4: Result := '-';
    5: Result := 'Set ImageList';
  end;
end;


function TRzPathBarEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                          var CompRefPropName: string;
                                          var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  case Index of
    5:
    begin
      CompRefClass := TCustomImageList;
      CompRefPropName := 'Images';
      CompRefMenuHandler := ImagesMenuHandler;
      Result := True;
    end;
  end;
end;


procedure TRzPathBarEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
var
  I: Integer;

  procedure CreateItemMenu( Index: Integer; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    if Caption = '-' then
      NewItem.Caption := 'Separator'
    else
      NewItem.Caption := Caption;
    NewItem.Tag := Index;
    NewItem.OnClick := ItemsMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  case Index of
    3: // Edit Item
    begin
      Item.Enabled := PathBar.Items.Count > 0;
      for I := 0 to PathBar.Items.Count - 1 do
        CreateItemMenu( I, PathBar.Items[ I ].Caption );
    end;
  end;
end;


procedure TRzPathBarEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    0: // Add Item
    begin
      PathBar.Items.Add;
      DesignerModified;
      // Select the item we just added in the Object Inspector
      Designer.SelectComponent( PathBar.Items[ PathBar.Items.Count - 1 ] );
    end;

    2: // Edit Items Collection
    begin
      EditPropertyByName( 'Items' );
    end;
  end;
end;


procedure TRzPathBarEditor.ItemsMenuHandler( Sender: TObject );
begin
  Designer.SelectComponent( PathBar.Items[ TMenuItem( Sender ).Tag ] );
end;


procedure TRzPathBarEditor.ImagesMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    PathBar.Images := Designer.GetRoot.FindComponent( S ) as TCustomImageList;
    DesignerModified;
  end;
end;


procedure TRzPathBarEditor.Edit;
begin
  // Display the Collection Editor for the Items property
  EditPropertyByName( 'Items' );
end;


end.
