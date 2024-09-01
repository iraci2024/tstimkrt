unit UDemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLTMSFNCCloudBase, LCLTMSFNCUtils, FGL, LCLTMSFNCTypes, fpjson;

type

  { TMovie }

  TMovie = class(TPersistent)
  private
    FImageUrl: string;
    FName: string;
  published
    property Name: string read FName write FName;
    property ImageUrl: string read FImageUrl write FImageUrl;
  end;

  TMovies =  class(specialize TFPGObjectList<TMovie>);


  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Image1: TImage;
    Label1: TLabel;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    cb: TTMSFNCCloudBase;
    movies: TMovies;
    procedure UpdateList;
    procedure DoGetMovies(const ARequestResult: TTMSFNCCloudBaseRequestResult);
    procedure DoGetMovieImage(const ARequestResult: TTMSFNCCloudBaseRequestResult);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
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
  cb.Request.Query := 'q='+Edit1.Text;

  cb.ExecuteRequest(@DoGetMovies);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  movies.Free;
  cb.Free;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex > -1 then
  begin
    if not Assigned(cb) then
      cb := TTMSFNCCloudBase.Create
    else
      cb.Request.Clear;
    cb.Request.Method := rmGET;
    cb.Request.ResultType := rrtStream;
    cb.Request.Host := (ListBox1.Items.Objects[ListBox1.ItemIndex] as TMovie).ImageUrl;

    cb.ExecuteRequest(@DoGetMovieImage);
  end;
end;

procedure TForm1.UpdateList;
var
  I: Integer;
begin
  ListBox1.Items.Clear;
  for I := 0 to movies.Count - 1 do
  begin
    ListBox1.Items.AddObject(movies[I].Name, movies[I]);
  end;
end;

procedure TForm1.DoGetMovies(const ARequestResult: TTMSFNCCloudBaseRequestResult);
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

procedure TForm1.DoGetMovieImage(
  const ARequestResult: TTMSFNCCloudBaseRequestResult);
var
  ms: TMemoryStream;
begin
  if ARequestResult.Success then
  begin
    ms := TMemoryStream.Create;
    try
      ms.CopyFrom(ARequestResult.ResultStream, ARequestResult.ResultStream.Size);
      ms.Position := 0;
      Image1.Picture.LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end;
end;

end.

