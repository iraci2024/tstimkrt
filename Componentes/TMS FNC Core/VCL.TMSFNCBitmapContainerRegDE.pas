{*************************************************************************}
{                                                                         }
{ written by TMS Software                                                 }
{           copyright (c)  2016 - 2021                                    }
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

unit VCL.TMSFNCBitmapContainerRegDE;

{$I VCL.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, VCL.TMSFNCBitmapContainerDE, VCL.TMSFNCBitmapContainer, VCL.TMSFNCTypes
  {$IFNDEF LCLWEBLIB}
  ,DesignIntF
  {$ENDIF}
  {$IFDEF WEBLIB}
  ,WEBLib.DesignIntf
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,ComponentEditors, PropEdits
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TTMSFNCBitmapContainer, TTMSFNCBitmapContainerDEEditor);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'BitmapName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'BitmapNameLarge', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'EditBitmapName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'AcceptBitmapName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'CancelBitmapName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'HoverImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'ClickImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'BlinkImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'SelectedImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'DefaultLeftImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'DefaultRightImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'LeftImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'RightImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'ExpandedIconName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'ExpandedIconLargeName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'CollapsedIconName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'CollapsedIconLargeName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'ImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'BitmapName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'BitmapNameLarge', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'EditBitmapName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'AcceptBitmapName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'CancelBitmapName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'HoverImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'ClickImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'BlinkImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'SelectedImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'DefaultLeftImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'DefaultRightImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'LeftImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'RightImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'ExpandedIconName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'ExpandedIconLargeName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'CollapsedIconName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'CollapsedIconLargeName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TCollectionItem,'ImageName', TTMSFNCBitmapNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTMSFNCScaledBitmap,'BitmapName', TTMSFNCBitmapNameProperty);
end;

{$IFDEF WEBLIB}
initialization
  Register;
{$ENDIF}

end.

