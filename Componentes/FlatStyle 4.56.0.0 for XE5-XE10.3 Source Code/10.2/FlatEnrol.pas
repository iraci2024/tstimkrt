unit FlatEnrol;
//download by http://www.codefans.net
interface

{$I FlatStyle.inc}

uses Classes;

procedure Register;

implementation

uses
  FlatCtrls, FlatCtrdb, FlatSysex, FlatVideo;

procedure Register;
begin
  RegisterComponents ('FlatStyle Vcl', [TFlatButton, TFlatSpeedButton, TFlatRadioButton, TFlatCheckBox]);
  RegisterComponents ('FlatStyle Vcl', [TFlatEdit, TFlatMaskEdit, TFlatInteger, TFlatFloat, TFlatIPEdit]);
  RegisterComponents ('FlatStyle Vcl', [TFlatMemo, TFlatTreeView, TFlatListView, TFlatListBox, TFlatListBoxExt]);
  RegisterComponents ('FlatStyle Vcl', [TFlatGUIListBox, TFlatCheckListBox, TFlatCheckListExt, TFlatComboBox]);
  RegisterComponents ('FlatStyle Vcl', [TFlatColorBox, TFlatBarcode, TFlatGroupBox, TFlatRadioGroup, TFlatPanel]);
  RegisterComponents ('FlatStyle Vcl', [TFlatGauge, TFlatScrollBar, TFlatProgressBar,TFlatGUIScrollBar,TFlatPages]);
  RegisterComponents ('FlatStyle Vcl', [TFlatPucker, TFlatDrawGrid, TFlatStringGrid, TFlatSplitter, TFlatTitleBar]);
  RegisterComponents ('FlatStyle Vcl', [TFlatLabel, TFlatImage]);
  RegisterComponents ('FlatStyle Sys', [TFlatAnimWnd, TFlatSound, TFlatAnimation,TFlatTimer, TFlatWater]);
  RegisterComponents ('FlatStyle Sys', [TFlatHint, TFlatExcel, TFlatSingle, TFlatTaskbarIcon, TFlatAviter]);
  RegisterComponents ('FlatStyle DB',  [TFlatDBEdit, TFlatDBMemo, TFlatDBListBox, TFlatDBMaskEdit]);
  RegisterComponents ('FlatStyle DB',  [TFlatDBFloat,TFlatDBInteger, TFlatDBComboBox, TFlatDBGrid]);
  RegisterComponents ('FlatStyle DB',  [TFlatDBButton, TFlatDBNavigator, TFlatDBCheckBox, TFlatDBRadioGroup]);
end; 

end.
