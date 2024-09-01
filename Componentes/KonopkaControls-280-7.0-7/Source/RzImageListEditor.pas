{===============================================================================
  RzImageListEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2021 by Embarcadero Technologies, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzImageListEditor
    Improved design-time editor for managing images in an ImageList.


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
  5.0    (30 Sep 2008)
    * Redesigned the TRzImageEditor component editor such the built-in editor
      dialog for editing image lists is used instead of the previous custom
      editing dialog. This change will provide seamless support for future
      image formats supported by the VCL. The Select Image dialog with the stock
      images is still present.
===============================================================================}

{$I RzComps.inc}

unit RzImageListEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Winapi.Windows,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.Graphics,
  DesignIntf,
  DesignMenus,
  DesignEditors,
  VCLEditors,
  RzDesignEditors;


type
  {==========================================}
  {== TRzImageListEditor Class Declaration ==}
  {==========================================}

  TRzImageListEditor = class( TRzComponentEditor )
  protected
    function ImageList: TCustomImageList;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;



implementation

uses
  Winapi.CommCtrl,
  System.SysUtils,
  DsnConst,
  ImgEdit,
  RzSelectImageEditor;


{================================}
{== TRzImageListEditor Methods ==}
{================================}

function TRzImageListEditor.ImageList: TCustomImageList;
begin
  Result := Component as TCustomImageList;
end;


function TRzImageListEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;


function TRzImageListEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Edit ImageList...';
    1: Result := 'Select Images...';
    2: Result := '-';
    3: Result := '16 x 16 Images';
    4: Result := '32 x 32 Images';
  end;
end;


procedure TRzImageListEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    3: Item.Checked := ( ImageList.Width = 16 ) and ( ImageList.Height = 16 );
    4: Item.Checked := ( ImageList.Width = 32 ) and ( ImageList.Height = 32 );
  end;
end;


procedure TRzImageListEditor.ExecuteVerb( Index: Integer );
var
  SelDlg: TRzSelectImageEditDlg;
  OwnerName: string;
begin
  case Index of
    0:
    begin
      EditImageList( Component as TImageList );
    end;

    1:
    begin
      // Display Select Images Editor...
      SelDlg := TRzSelectImageEditDlg.Create( Application );
      try
        if Component.Owner <> nil then
          OwnerName := Component.Owner.Name + '.'
        else
          OwnerName := '';
        SelDlg.Caption := OwnerName + Component.Name + SelDlg.Caption;
        SelDlg.CompOwner := Designer.GetRoot;
        SelDlg.SetObject( TControl( Component ), False );
        SelDlg.UpdateControls;
        SelDlg.Reposition;

        SelDlg.ShowModal;
        DesignerModified;
      finally
        SelDlg.Free;
      end;
    end;

    3:
    begin
      ImageList.Width := 16;
      ImageList.Height := 16;
      DesignerModified;
    end;

    4:
    begin
      ImageList.Width := 32;
      ImageList.Height := 32;
      DesignerModified;
    end;
  end;
end;


end.

