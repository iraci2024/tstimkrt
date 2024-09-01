unit Utooltipdemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvToolTip, Vcl.StdCtrls, AdvEdit,
  AdvGlowButton, AdvCombo, System.ImageList, Vcl.ImgList, Vcl.Mask, AdvSpin;

type
  TForm2 = class(TForm)
    GroupBox1: TGroupBox;
    edtFirstName: TAdvEdit;
    edtDescr: TMemo;
    edtLastName: TAdvEdit;
    ErrorToolTip: TAdvToolTip;
    WarningToolTip: TAdvToolTip;
    Label1: TLabel;
    edtAge: TAdvSpinEdit;
    edtCity: TAdvEdit;
    ImageList1: TImageList;
    edtCountry: TAdvComboBox;
    InfoToolTip: TAdvToolTip;
    ValidToolTip: TAdvToolTip;
    AdvGlowButton1: TAdvGlowButton;
    procedure AdvGlowButton1Click(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.AdvGlowButton1Click(Sender: TObject);
begin
  if edtFirstName.Text = '' then
     ShowValidator(edtFirstName, 'First name cannot be empty', ErrorToolTip)
  else
  if Length(edtFirstName.Text) < 2 then
     ShowValidator(edtFirstName, 'First name should have more than one character', WarningToolTip)
  else
     ShowValidator(edtFirstName, 'First name is OK', ValidToolTip);


  if edtLastName.Text = '' then
     ShowValidator(edtLastName, 'Last name cannot be empty', ErrorToolTip)
  else
  if Length(edtLastName.Text) < 2 then
     ShowValidator(edtLastName, 'Last name should have more than one character', WarningToolTip)
  else
     ShowValidator(edtLastName, 'Last name is OK', ValidToolTip);

  if edtAge.Value = 0 then
     ShowValidator(edtAge, 'Age must be higher than 0', ErrorToolTip)
  else
  if edtAge.Value > 100 then
     ShowValidator(edtAge, 'Age hiher than 100?', InfoToolTip)
  else
  if edtAge.Value < 18 then
     ShowValidator(edtAge, 'Person not adult?', WarningToolTip)
  else
     ShowValidator(edtAge, 'Age is OK', ValidToolTip);

  if edtCity.Text = '' then
     ShowValidator(edtCity, 'City cannot be empty', ErrorToolTip)
  else
     ShowValidator(edtCity, 'City is OK', ValidToolTip);

  if edtCountry.Text = '' then
     ShowValidator(edtCountry, 'Country cannot be empty', ErrorToolTip)
  else
    if edtCountry.Items.IndexOf(edtCountry.Text) = -1 then
       ShowValidator(edtCountry, 'Country not in list', WarningToolTip)
  else
    ShowValidator(edtCountry, 'Country is OK', ValidToolTip);

  if edtDescr.Lines.Count = 0 then
     ShowValidator(edtDescr, 'Please enter a description', infoToolTip)
  else
     ShowValidator(edtDescr, 'Description is OK', ValidToolTip)
end;

procedure TForm2.GroupBox1Click(Sender: TObject);
begin
  HideValidator(edtFirstName);
  HideValidator(edtLastName);
  HideValidator(edtAge);
  HideValidator(edtCity);
  HideValidator(edtCountry);
  HideValidator(edtDescr);
end;

end.
