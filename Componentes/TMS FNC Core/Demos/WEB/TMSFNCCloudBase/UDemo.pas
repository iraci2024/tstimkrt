unit UDemo;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.ExtCtrls, WEBLib.WebCtrls,
  WEBLib.TMSFNCCloudBase, WEBLib.TMSFNCUtils, Vcl.StdCtrls, WEBLib.JSON, Contnrs,
  WEBLib.StdCtrls, WEBLib.TMSFNCTypes, VCL.TMSFNCTypes, VCL.TMSFNCUtils,
  VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes, VCL.TMSFNCCustomControl,
  VCL.TMSFNCImage;

type
  TMovie = class(TPersistent)
  private
    FName: string;
    FImageUrl: string;
  published
    property Name: string read FName write FName;
    property ImageUrl: string read FImageUrl write FImageUrl;
  end;

  TMovies = class(TObjectList)
  private
    function GetItem(Index: Integer): TMovie;
    procedure SetItem(Index: Integer; const Value: TMovie);
  public
    property Items[Index: Integer]: TMovie read GetItem write SetItem; default;
  end;

  TForm1 = class(TWebForm)
    WebEdit1: TWebEdit;
    WebButton1: TWebButton;
    WebListBox1: TWebListBox;
    WebImageControl1: TWebImageControl;
    WebLabel1: TWebLabel;
    procedure WebButton1Click(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
    procedure WebListBox1Click(Sender: TObject);
  private
    { Private declarations }
    cb: TTMSFNCCloudBase;
    movies: TMovies;
    procedure UpdateList;
    procedure DoGetMovies(const ARequestResult: TTMSFNCCloudBaseRequestResult);
    procedure DoGetMovieImage(const ARequestResult: TTMSFNCCloudBaseRequestResult);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TMovies.GetItem(Index: Integer): TMovie;
begin
  Result := TMovie(inherited Items[Index]);
end;

procedure TMovies.SetItem(Index: Integer; const Value: TMovie);
begin
  inherited Items[Index] := Value;
end;

{ TForm1 }

procedure TForm1.DoGetMovieImage(
  const ARequestResult: TTMSFNCCloudBaseRequestResult);
var
  img: TTMSFNCBitmap;
begin
  if ARequestResult.Success then
  begin
    img := TTMSFNCBitmap.Create;
    try
      ARequestResult.ResultStream.Position := 0;
      img.LoadFromResource('data:image/png;base64,'+TTMSFNCUtils.Encode64(ARequestResult.ResultStream.Bytes));
      WebImageControl1.Picture.Data := img.Data;
    finally
      img.Free;
    end;
  end;
end;

procedure TForm1.DoGetMovies(
  const ARequestResult: TTMSFNCCloudBaseRequestResult);
var
  o, fo: TJSONValue;
  jv, jvv, jvvv: TJSONValue;
  ja: TJSONArray;
  I: Integer;
  m: TMovie;
begin
  if ARequestResult.ResultString <> '' then
  begin
    o := TTMSFNCUtils.ParseJSON(ARequestResult.ResultString);

    if Assigned(o) then
    begin
      try
        ja := o as TJSONArray;
        for i := 0 to TTMSFNCUtils.GetJSONArraySize(ja) - 1 do
        begin
          fo := TTMSFNCUtils.GetJSONArrayItem(ja, i);

          m := TMovie.Create;

          jv := TTMSFNCUtils.GetJSONValue(fo, 'show');
          if Assigned(jv) then
          begin
            jvv := TTMSFNCUtils.GetJSONValue(jv, 'name');
            if Assigned(jvv) then
              m.Name := TTMSFNCUtils.GetJSONProp(jv, 'name');

            jvv := TTMSFNCUtils.GetJSONValue(jv, 'image');
            if Assigned(jvv) then
            begin
              jvvv := TTMSFNCUtils.GetJSONValue(jvv, 'medium');
              if Assigned(jvvv) then
                m.ImageUrl := TTMSFNCUtils.GetJSONProp(jvv, 'medium');
            end;
          end;

          movies.Add(m);
        end;
      finally
        o.Free;
      end;
    end;
  end;

  UpdateList;
end;

procedure TForm1.UpdateList;
var
  I: Integer;
begin
  WebListBox1.Items.Clear;
  WebListBox1.BeginUpdate;
  for I := 0 to movies.Count - 1 do
  begin
    WebListBox1.Items.AddObject(movies[I].Name, movies[I]);
  end;
  WebListBox1.EndUpdate;
end;

procedure TForm1.WebButton1Click(Sender: TObject);
begin
  //http://www.tvmaze.com/api

  if not Assigned(movies) then
    movies := TMovies.Create
  else
    movies.Clear;

  if not Assigned(cb) then
    cb := TTMSFNCCloudBase.Create
  else
    cb.Request.Clear;
  cb.Request.Host := 'http://api.tvmaze.com';
  cb.Request.Path := '/search/shows';
  cb.Request.Query := 'q=' + WebEdit1.Text;

  cb.ExecuteRequest(@DoGetMovies);
end;

procedure TForm1.WebFormCreate(Sender: TObject);
var
  lbl: TWebLabel;
begin
  FormContainer := 'appcontent';
  lbl := TWebLabel.Create(Self);
  lbl.ElementID := 'title';
  lbl.Caption := 'TMSFNCCloudBase Demo';

  lbl := TWebLabel.Create(Self);
  lbl.ElementID := 'description';
  lbl.Caption := 'Seamless access to cloud services from Windows, cross-platform and the web';
end;

procedure TForm1.WebListBox1Click(Sender: TObject);
begin
  if WebListBox1.ItemIndex > -1 then
  begin
//   WebImageControl1.URL := (WebListBox1.Items.Objects[WebListBox1.ItemIndex] as TMovie).ImageUrl;
    if not Assigned(cb) then
      cb := TTMSFNCCloudBase.Create
    else
      cb.Request.Clear;
    cb.Request.Method := rmGET;
    cb.Request.ResultType := rrtStream;
    cb.Request.Host := (WebListBox1.Items.Objects[WebListBox1.ItemIndex] as TMovie).ImageUrl;

    cb.ExecuteRequest(@DoGetMovieImage);
  end;
end;

end.