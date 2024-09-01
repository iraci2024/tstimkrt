{***********************************************************************}
{ TToolPanels Design Editor component                                   }
{ for Delphi & C++Builder                                               }
{ version 1.3                                                           }
{                                                                       }
{ written by TMS Software                                               }
{            copyright � 2003 - 2021                                    }
{            Email: info@tmssoftware.com                                }
{            Web: https://www.tmssoftware.com                           }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

unit ToolPanelsDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, ToolPanels, Windows, Forms, TypInfo, Dialogs, ExtCtrls, Controls, SysUtils, AdvToolButtonStyles, AdvStyleIF
  , DesignIntf, DesignEditors, ContNrs;

type
  TAdvToolPanelTabEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvToolPanelEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

{ TToolPanelTabEditor }

procedure TAdvToolPanelTabEditor.ExecuteVerb(Index: integer);
var
  style: TTMSStyle;
  psf: TAdvToolButtonStyleForm;
begin
  inherited;
  case Index of
  0:
    begin
      TCustomPanel(Component).ControlStyle := TCustomPanel(Component).ControlStyle + [csAcceptsControls];
      Designer.CreateComponent(TAdvToolPanel,Component,23,0,100,100);

      with TAdvToolPanelTab(Component) do
        UpdatePanels(ControlCount - 1);

      (Component as TCustomPanel).Invalidate;
      TCustomPanel(Component).ControlStyle := TCustomPanel(Component).ControlStyle - [csAcceptsControls];
    end;
  1: TAdvToolPanelTab(Component).PrevPanel;
  2: TAdvToolPanelTab(Component).NextPanel;
  3:
  begin
    style := (Component as TAdvToolPanelTab).GetComponentStyle;

    psf := TAdvToolButtonStyleForm.Create(Application);
    case style of
      tsOffice2003Blue: psf.RadioGroup1.ItemIndex := 0;
      tsOffice2003Olive: psf.RadioGroup1.ItemIndex := 1;
      tsOffice2003Silver: psf.RadioGroup1.ItemIndex := 2;
      tsOffice2003Classic: psf.RadioGroup1.ItemIndex := 3;
      tsOffice2007Luna: psf.RadioGroup1.ItemIndex := 4;
      tsOffice2007Obsidian: psf.RadioGroup1.ItemIndex := 5;
      tsOffice2007Silver: psf.RadioGroup1.ItemIndex := 6;
      tsOffice2010Blue: psf.RadioGroup1.ItemIndex := 7;
      tsOffice2010Silver: psf.RadioGroup1.ItemIndex := 8;
      tsOffice2010Black: psf.RadioGroup1.ItemIndex := 9;
      tsWindowsXP: psf.RadioGroup1.ItemIndex := 10;
      tsWindowsVista: psf.RadioGroup1.ItemIndex := 11;
      tsWindows7: psf.RadioGroup1.ItemIndex := 12;
      tsTerminal: psf.RadioGroup1.ItemIndex := 13;
      tsWindows8: psf.RadioGroup1.ItemIndex := 14;
      tsOffice2013White: psf.RadioGroup1.ItemIndex := 15;
      tsOffice2013LightGray: psf.RadioGroup1.ItemIndex := 16;
      tsOffice2013Gray: psf.RadioGroup1.ItemIndex := 17;
      tsWindows10: psf.RadioGroup1.ItemIndex := 18;
      tsOffice2016White: psf.RadioGroup1.ItemIndex := 19;
      tsOffice2016Gray: psf.RadioGroup1.ItemIndex := 20;
      tsOffice2016Black: psf.RadioGroup1.ItemIndex := 21;
      tsOffice2019White: psf.RadioGroup1.ItemIndex := 22;
      tsOffice2019Gray: psf.RadioGroup1.ItemIndex := 23;
      tsOffice2019Black: psf.RadioGroup1.ItemIndex := 24;
    end;

    if psf.ShowModal = mrOK then
    begin
      case psf.RadioGroup1.ItemIndex of
        0: style := tsOffice2003Blue;
        1: style := tsOffice2003Olive;
        2: style := tsOffice2003Silver;
        3: style := tsOffice2003Classic;
        4: style := tsOffice2007Luna;
        5: style := tsOffice2007Obsidian;
        6: style := tsOffice2007Silver;
        7: style := tsOffice2010Blue;
        8: style := tsOffice2010Silver;
        9: style := tsOffice2010Black;
        10: style := tsWindowsXP;
        11: style := tsWindowsVista;
        12: style := tsWindows7;
        13: style := tsTerminal;
        14: style := tsWindows8;
        15: style := tsOffice2013White;
        16: style := tsOffice2013LightGray;
        17: style := tsOffice2013Gray;
        18: style := tsWindows10;
        19: style := tsOffice2016White;
        20: style := tsOffice2016Gray;
        21: style := tsOffice2016Black;
        22: style := tsOffice2019White;
        23: style := tsOffice2019Gray;
        24: style := tsOffice2019Black;
      end;
      if (Component is TAdvToolPanelTab) then
         (Component as TAdvToolPanelTab).SetComponentStyle(style);
         Designer.Modified;
    end;
    psf.Free;
    end;
  end;
end;

function TAdvToolPanelTabEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New Panel';
  1: Result := 'Previous Panel';
  2: Result := 'Next Panel';
  3: Result := 'Styles';
  end;
end;

function TAdvToolPanelTabEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

{ TToolPanelEditor }

procedure TAdvToolPanelEditor.ExecuteVerb(Index: integer);
var
  psf: TAdvToolButtonStyleForm;
  style: TTMSStyle;
  tab: TAdvToolPanelTab;
  idx,i: integer;

begin
  inherited;

  //if not ((Component as TAdvToolPanel).Parent is TAdvToolPanelTab) then
  //  Index := Index + 3;

  tab := TAdvToolPanelTab(TCustomPanel(Component).Parent);

  idx := -1;

  if Assigned(tab) then

    for i := 0 to tab.ControlCount - 1 do
    begin
      if tab.Controls[i] = Component then
        idx := i;
    end;

  case Index of
  0:
    begin
      TCustomPanel(Component).Parent.ControlStyle := TCustomPanel(Component).Parent.ControlStyle + [csAcceptsControls];
      Designer.CreateComponent(TAdvToolPanel,TCustomPanel(Component).Parent,23,0,100,100);

      with TAdvToolPanelTab(TCustomPanel(Component).Parent) do
        UpdatePanels(ControlCount - 1);

      (TCustomPanel(Component).Parent as TCustomPanel).Invalidate;
      TCustomPanel(Component).Parent.ControlStyle := TCustomPanel(Component).Parent.ControlStyle - [csAcceptsControls];
    end;
  1: TAdvToolPanelTab(TCustomPanel(Component).Parent).PrevPanel;
  2: TAdvToolPanelTab(TCustomPanel(Component).Parent).NextPanel;
  3:
    begin
      if (idx >= 0) and (idx < tab.ControlCount - 1) then
        tab.MovePanel(TAdvToolPanel(Component), idx + 1);
      tab.Invalidate;
    end;
  4:begin
      if (idx > 0) then
        tab.MovePanel(TAdvToolPanel(Component), idx - 1);
      tab.Invalidate;
    end;
  5:
  begin
    style := (Component as TAdvToolPanel).GetComponentStyle;

    psf := TAdvToolButtonStyleForm.Create(Application);
    case style of
      tsOffice2003Blue: psf.RadioGroup1.ItemIndex := 0;
      tsOffice2003Olive: psf.RadioGroup1.ItemIndex := 1;
      tsOffice2003Silver: psf.RadioGroup1.ItemIndex := 2;
      tsOffice2003Classic: psf.RadioGroup1.ItemIndex := 3;
      tsOffice2007Luna: psf.RadioGroup1.ItemIndex := 4;
      tsOffice2007Obsidian: psf.RadioGroup1.ItemIndex := 5;
      tsOffice2007Silver: psf.RadioGroup1.ItemIndex := 6;
      tsOffice2010Blue: psf.RadioGroup1.ItemIndex := 7;
      tsOffice2010Silver: psf.RadioGroup1.ItemIndex := 8;
      tsOffice2010Black: psf.RadioGroup1.ItemIndex := 9;
      tsWindowsXP: psf.RadioGroup1.ItemIndex := 10;
      tsWindowsVista: psf.RadioGroup1.ItemIndex := 11;
      tsWindows7: psf.RadioGroup1.ItemIndex := 12;
      tsTerminal: psf.RadioGroup1.ItemIndex := 13;
      tsWindows8: psf.RadioGroup1.ItemIndex := 14;
      tsOffice2013White: psf.RadioGroup1.ItemIndex := 15;
      tsOffice2013LightGray: psf.RadioGroup1.ItemIndex := 16;
      tsOffice2013Gray: psf.RadioGroup1.ItemIndex := 17;
      tsWindows10: psf.RadioGroup1.ItemIndex := 18;
      tsOffice2016White: psf.RadioGroup1.ItemIndex := 19;
      tsOffice2016Gray: psf.RadioGroup1.ItemIndex := 20;
      tsOffice2016Black: psf.RadioGroup1.ItemIndex := 21;
      tsOffice2019White: psf.RadioGroup1.ItemIndex := 22;
      tsOffice2019Gray: psf.RadioGroup1.ItemIndex := 23;
      tsOffice2019Black: psf.RadioGroup1.ItemIndex := 24;
    end;

    if psf.ShowModal = mrOK then
    begin
      case psf.RadioGroup1.ItemIndex of
        0: style := tsOffice2003Blue;
        1: style := tsOffice2003Olive;
        2: style := tsOffice2003Silver;
        3: style := tsOffice2003Classic;
        4: style := tsOffice2007Luna;
        5: style := tsOffice2007Obsidian;
        6: style := tsOffice2007Silver;
        7: style := tsOffice2010Blue;
        8: style := tsOffice2010Silver;
        9: style := tsOffice2010Black;
        10: style := tsWindowsXP;
        11: style := tsWindowsVista;
        12: style := tsWindows7;
        13: style := tsTerminal;
        14: style := tsWindows8;
        15: style := tsOffice2013White;
        16: style := tsOffice2013LightGray;
        17: style := tsOffice2013Gray;
        18: style := tsWindows10;
        19: style := tsOffice2016White;
        20: style := tsOffice2016Gray;
        21: style := tsOffice2016Black;
        22: style := tsOffice2019White;
        23: style := tsOffice2019Gray;
        24: style := tsOffice2019Black;
      end;
      if (Component is TAdvToolPanel) then
         (Component as TAdvToolPanel).SetComponentStyle(style);
         Designer.Modified;
    end;
    psf.Free;
    end;
  end;
end;

procedure TAdvToolPanelEditor.EditProperty(const PropertyEditor:IProperty;
                                      var Continue:Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'SECTIONS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

function TAdvToolPanelEditor.GetVerb(Index: Integer): string;
begin
  if (Component as TAdvToolPanel).Parent is TAdvToolPanelTab then
  begin
    case Index of
    0: Result := 'New Panel';
    1: Result := 'Previous Panel';
    2: Result := 'Next Panel';
    3: Result := 'Move forward';
    4: Result := 'Move backward';
    5: Result := 'Styles';
    end;
  end;
end;

function TAdvToolPanelEditor.GetVerbCount: Integer;
begin
  if (Component as TAdvToolPanel).Parent is TAdvToolPanelTab then
    Result := 6
  else
    Result := 0;
end;

end.
