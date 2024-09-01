{*********************************************************************}
{ TADVSTRINGGRID component                                            }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by TMS Software                                             }
{            copyright © 1996-2020                                    }
{            Email : info@tmssoftware.com                             }
{            Web : http://www.tmssoftware.com                         }
{*********************************************************************}

unit AdvGridCSVPager;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  AdvGrid, AdvUtil, Dialogs;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.
  DATE_VER = 'May, 2020'; // Month version


type
  TLocalAdvGrid = class(TAdvStringGrid);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridCSVPager = class(TComponent)
  private
    FPageSize: Integer;
    FFileName: string;
    FGrid: TAdvStringGrid;
    FOpened: Boolean;
    FPage: Integer;
    FPageCount: Integer;
    FFile: TextFile;
    FOldDelimiter: Char;
    FTotalRows: Integer;
    FAutoNumber: boolean;
    FColumnHeader: boolean;
    function GetPage: Integer;
    procedure SetFileName(const Value: string);
    procedure SetGrid(const Value: TAdvStringGrid);
    procedure SetPage(const Value: Integer);
    procedure SetPageSize(const Value: Integer);
    function JumpPages(I: Integer): Integer;
    function GetFirstRow: integer;
    function GetLastRow: integer;
    procedure SetAutoNumber(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure LoadPage;
    procedure GoToPage(PageNo: Integer);
    procedure MoveBy(I: Integer);  // +1 forward, -1 backward
    function GetVersionNr: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prev;
    procedure Reload;
    procedure Open;
    procedure Close;
    procedure SavePage;
    function Bof: Boolean;
    function Eof: Boolean;
    function Pages: Integer;
    property Page: Integer read GetPage write SetPage;
    property IsOpen: Boolean read FOpened write FOpened;
    property FirstRow: integer read GetFirstRow;
    property LastRow: integer read GetLastRow;
    property TotalRows: integer read FTotalRows;
  published
    property AutoNumber: boolean read FAutoNumber write SetAutoNumber;
    property ColumnHeader: boolean read FColumnHeader write FColumnHeader default false;
    property FileName: string read FFileName write SetFileName;
    property PageSize: Integer read FPageSize write SetPageSize default 15;
    property Grid: TAdvStringGrid read FGrid write SetGrid;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

const
  CSVSeparators: array[1..10] of char = (',',';','#',#9,'|','@','*','-','+','&');

//------------------------------------------------------------------------------

{ TAdvGridCSVPager }

constructor TAdvGridCSVPager.Create(AOwner: TComponent);
begin
  inherited;
  FPageSize := 15;
  FFileName := '';
  FGrid := nil;
  FOpened := False;
  FPage := 0;
  FPageCount := 0;
end;

//------------------------------------------------------------------------------

destructor TAdvGridCSVPager.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvGridCSVPager.Bof: Boolean;
begin
  Result := Page = 1;
end;

//------------------------------------------------------------------------------

function TAdvGridCSVPager.Eof: Boolean;
begin
  Result := Page = Pages;
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.First;
begin
  GoToPage(1);
  LoadPage;
end;

//------------------------------------------------------------------------------

function TAdvGridCSVPager.GetPage: Integer;
begin
  Result := FPage;
end;

//------------------------------------------------------------------------------

function TAdvGridCSVPager.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvGridCSVPager.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.Last;
begin
  MoveBy(Pages - Page);
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.Next;
begin
  MoveBy(1);
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.Open;
var
  buffer,celltext: string;
  s,z, r: Integer;
  strtCol,strtRow: Integer;
  c1,c2,cm: Integer;
  delimiterpos,quotepos: Integer;
  lr: TStringList;

begin
  if not Assigned(FGrid) or FOpened then
    Exit;

  StrtCol := FGrid.FixedCols;
  StrtRow := FGrid.FixedRows;

  if FGrid.SaveFixedCells then
  begin
    StrtCol := 0;
    StrtRow := 0;
  end;

  AssignFile(FFile, FFileName);
  {$i-}
  Reset(FFile);
  {$i+}

  if (IOResult<>0) then
    raise Exception.Create('Cannot open file ' + FFileName);

  FOpened := True;

  FTotalRows := 0;
  while not System.Eof(FFile) do
  begin
    ReadLn(FFile,buffer);
    inc(FTotalRows);
  end;

  Reset(FFile);

  if ColumnHeader and (FGrid.FixedRows > 0) then
  begin
    z := FGrid.FixedRows - 1
  end
  else
    z := StrtRow;

  lr := TStringList.Create;

  FOldDelimiter := FGrid.Delimiter;

  // do intelligent estimate of the separator
  if FGrid.Delimiter = #0 then
  begin
    CellText := '';
    ReadLn(FFile,buffer);

    if not System.Eof(FFile) then
      ReadLn(FFile,CellText);
      
    Reset(FFile);
    cm := 0;
    for s := 1 to 10 do
    begin
      c1 := NumSingleChar(CSVSeparators[s],Buffer);
      c2 := NumSingleChar(CSVSeparators[s],Celltext);
      if (c1 = c2) and (c1 > cm) then
      begin
        FGrid.Delimiter := CSVSeparators[s];
        cm := c1;
      end;
    end;

    if cm = 0 then
      for s := 1 to 10 do
      begin
        c1 := NumChar(CSVSeparators[s],Buffer);
        c2 := NumChar(CSVSeparators[s],Celltext);
        if (c1 = c2) and (c1 > cm) then
        begin
          FGrid.Delimiter := CSVSeparators[s];
          cm := c1;
        end;
      end;

    // if no matching delimiter count found on line1 & line2, take maximum
    if cm = 0 then
      for s := 1 to 10 do
      begin
        c1 := NumChar(CSVSeparators[s],Buffer);
        c2 := NumChar(CSVSeparators[s],Celltext);
        if (c1 > cm) or (c2 > cm) then
        begin
          FGrid.Delimiter := CSVSeparators[s];
          cm := MaxI(c1,c2);
        end;
      end;
  end;

  Reset(FFile);

  ReadLn(FFile,buffer);
  if FGrid.OemConvert then
    OemToString(Buffer);

  s := StrtCol;

  if z >= FGrid.RowCount - FGrid.FixedFooters then
  begin
    FGrid.RowCount := z + 1000;
  end;

  while VarCharPos(FGrid.Delimiter,Buffer,DelimiterPos) > 0 do
  begin
    if Buffer[1] = '"' then
    begin
      Delete(buffer,1,1);   //delete first quote from buffer

      if SinglePos('"',Buffer,QuotePos) > 0 then  //search for next single quote
      begin
        CellText := Copy(buffer,1,QuotePos - 1);
        CellText := DoubleToSingleChar('"',CellText);
        Delete(buffer,1,QuotePos);
      end
      else
        CellText := '';
      VarCharPos(FGrid.Delimiter,buffer,DelimiterPos);
    end
    else
    begin
      CellText := Copy(buffer,1,DelimiterPos - 1);
      CellText := DoubleToSingleChar('"',CellText);
    end;


    if FGrid.JavaCSV then
      JavaToLineFeeds(CellText)
    else
      CSVToLineFeeds(CellText);

    TLocalAdvGrid(FGrid).LoadCell(s,z,CellText);

    Delete(buffer,1,DelimiterPos);

    Inc(s);
    if s >= FGrid.ColCount then
      FGrid.ColCount := s;
  end;

  if Length(Buffer) > 0 then
  begin
    if Buffer[1] = '"' then
      Delete(buffer,1,1);
    if Length(Buffer) > 0 then
    begin
      if Buffer[Length(Buffer)] = '"' then
        Delete(Buffer,Length(Buffer),1);
    end;

    CellText := DoubleToSingleChar('"',Buffer);

    if FGrid.JavaCSV then
      JavaToLineFeeds(CellText)
    else
      CSVToLineFeeds(CellText);

    TLocalAdvGrid(FGrid).LoadCell(s,z,CellText);

    Inc(s);
    if s > FGrid.ColCount then
      FGrid.ColCount := s;
  end;

  // Calculating Pages Count
  r := 0;

  Reset(FFile);
  
  while not System.Eof(FFile) do
  begin
    ReadLn(FFile, buffer);
    Inc(r);
  end;
  FPageCount := r div FPageSize;
  if (r mod FPageSize) > 0 then
    Inc(FPageCount);

  First;

  if FGrid.FloatingFooter.Visible then
    FGrid.Rows[FGrid.RowCount - 1].Assign(lr);

  lr.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.Close;
begin
  if FOpened then
  begin
    CloseFile(FFile);
    FOpened := False;
    if Assigned(FGrid) then
      FGrid.Delimiter := FOldDelimiter;
  end;
  FPage := 0;
  FPageCount := 0;
end;

//------------------------------------------------------------------------------

function TAdvGridCSVPager.Pages: Integer;
begin
  Result := FPageCount;
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.Prev;
begin
  MoveBy(-1);
end;

procedure TAdvGridCSVPager.Reload;
begin
  MoveBy(0);
end;


//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.SavePage;
var
  MemStr: TMemoryStream;
  buffer, CellText: String;
  r, p, z, s, n, rs: Integer;
  Delim: Char;
  dblquotes: Boolean;
begin
  if not Assigned(FGrid) or not FOpened then
    Exit;

  MemStr := TMemoryStream.Create;

  Reset(FFile);
//  ReadLn(FFile, buffer);
//  buffer := buffer + #13#10;
//  MemStr.Write(buffer[1], Length(buffer));

  r := 0;
  p := 1;

  while not System.Eof(FFile) do
  begin
    // read & skip when not in active page
    if (r >= FPageSize) then
    begin
      Inc(p);
      r := 0;
    end;

    // the page was found to update
    if (p = Page) then
    begin
      {
      if FGrid.SaveHiddenCells then
        n := FNumHidden
      else  }
        n := 0;

     { if FDelimiter = #0 then
        Delim := ','
      else }
        Delim := FGrid.Delimiter;

      FGrid.ExportNotification(esExportStart, -1);

      for z := FGrid.SaveStartRow to FGrid.RowCount - 1{SaveEndRow} do
      begin
        FGrid.ExportNotification(esExportNewRow, z);

        for s := FGrid.SaveStartCol to FGrid.ColCount-1{SaveEndCol} + n do
        begin
          if s > FGrid.SaveStartCol then
            MemStr.Write(Delim, Length(Delim));
          {
          if FSaveHiddenCells then
            rs := s
          else   }
            rs := s;//TLocalAdvGrid(FGrid).RemapCol(s);

          CellText := TLocalAdvGrid(FGrid).SaveCell(rs,z);

          CellText := CSVQuotes(CellText);

          if FGrid.OemConvert then
            StringToOem(CellText);

          dblquotes := false;

          if ({(FAlwaysQuotes) or }((Pos(Delim,CellText) = 0) and (Pos('"',CellText) > 0))) then
          begin
            CellText := '"' + CellText + '"';
            dblquotes := true;
          end;

          if CellText = '' then
          begin
            if FGrid.JavaCSV then
              CellText := '^'
            else
              if FGrid.QuoteEmptyCells then
                CellText := '""';
          end;

          if (Pos(Delim,CellText) > 0) or (LinesInText(CellText,FGrid.MultiLineCells) > 1) then
          begin
            if FGrid.JavaCSV then
              LinefeedstoJava(CellText)
            else
            begin
              if not dblquotes then
                LinefeedsToCSV(CellText)
              else
                LinefeedsToCSVNQ(CellText);
            end;
          end;

          MemStr.Write(CellText[1], Length(CellText));
        end;

        CellText := #13#10;
        MemStr.Write(CellText[1], Length(CellText));
        //Writeln(f);

      end;

      FGrid.ExportNotification(esExportDone, -1);

      // Skip Current Page
      r := 0;
      while not System.Eof(FFile) do
      begin
        ReadLn(FFile, buffer);
        Inc(r);
        if (r >= FPageSize) then
          Break;
      end;
      Inc(p);
      r := 0;
    end;

    ReadLn(FFile, buffer);

    if not System.Eof(FFile) then
      buffer := buffer + #13#10;
      
    MemStr.Write(buffer[1], Length(buffer));
    Inc(r);
  end;
  // Close opened file
  //Self.Close;
  if FOpened then
    CloseFile(FFile);

  MemStr.SaveToFile(FFileName);
  MemStr.Free;
  // Reopen File and Set position
  //Open;
  AssignFile(FFile, FFileName);
  {$i-}
  Reset(FFile);
  {$i+}
  if (IOResult<>0) then
    raise Exception.Create('Cannot open file ' + FFileName);

  n := FPage;
  GoToPage(FPage);
  // Setting File position
  r := 0;
  while not System.Eof(FFile) do
  begin
    ReadLn(FFile, buffer);
    Inc(r);
    if (r >= FPageSize) then
      Break;
  end;

  FPage := n;

end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.SetFileName(const Value: string);
begin
  if not FOpened then
    FFileName := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.SetGrid(const Value: TAdvStringGrid);
begin
  if Value <> FGrid then
  begin
    if Value = nil then
    begin
      FGrid := Value;
      self.Close;
    end
    else if not FOpened then
      FGrid := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.SetPage(const Value: Integer);
begin
  MoveBy(Value - Page);
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.SetPageSize(const Value: Integer);
begin
  if not FOpened then
  begin
    FPageSize := Value;
  end
  else
  begin
    Close;
    FPageSize := Value;
    Open;
  end;
end;

procedure TAdvGridCSVPager.SetVersion(const Value: string);
begin
  //
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FGrid) then
    Grid := nil;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.MoveBy(I: Integer);
begin
  if (I = 0) or not FOpened then
    Exit;

  if (I < 0) then
  begin
    if not self.BOF then
    begin
      GoToPage(Page + I);
      LoadPage;
    end;
  end
  else
  begin
    JumpPages(I - 1);
    LoadPage;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.GoToPage(PageNo: Integer);
//var
//  buffer: String;
begin
  if (PageNo <= 0) or not FOpened then
    Exit;
  Reset(FFile);
//  ReadLn(FFile, buffer);
  FPage := 0;
  JumpPages(PageNo - 1);
end;

//------------------------------------------------------------------------------

procedure TAdvGridCSVPager.LoadPage;
var
  buffer,celltext: string;
  strtCol,strtRow: Integer;
  s,z: Integer;
  delimiterpos,quotepos: Integer;
begin
  if not FOpened or (self.Eof) or
  (System.Eof(FFile))
  then
    Exit;

  StrtCol := FGrid.FixedCols;
  StrtRow := FGrid.FixedRows;

  if FGrid.SaveFixedCells then
  begin
    StrtCol := 0;
    StrtRow := 0;
  end;

  if ColumnHeader and (FGrid.FixedRows > 0) then
  begin
    if FPage = 0 then
      z := FGrid.FixedRows - 1
    else
      z := FGrid.FixedRows;
  end
  else
    z := StrtRow;

  while not System.Eof(FFile) do
  begin
    ReadLn(FFile,buffer);
    if FGrid.OemConvert then
      OemToString(Buffer);

    //outputdebugstring(pchar(buffer));

    s := StrtCol;

    if z >= FGrid.RowCount - FGrid.FixedFooters then
    begin
      FGrid.RowCount := z + 1000;
    end;

    if AutoNumber and (z - 1 >= FGrid.FixedRows) then
     TLocalAdvGrid(FGrid).Cells[0,z - 1] := IntToStr(FirstRow + z + PageSize - 2);

    while VarCharPos(FGrid.Delimiter,Buffer,DelimiterPos) > 0 do
    begin
      if Buffer[1] = '"' then
      begin
        Delete(buffer,1,1);   //delete first quote from buffer

        if SinglePos('"',Buffer,QuotePos) > 0 then  //search for next single quote
        begin
          CellText := Copy(buffer,1,QuotePos - 1);
          CellText := DoubleToSingleChar('"',CellText);
          Delete(buffer,1,QuotePos);
        end
        else
          CellText := '';
        VarCharPos(FGrid.Delimiter,buffer,DelimiterPos);
      end
      else
      begin
        CellText := Copy(buffer,1,DelimiterPos - 1);
        CellText := DoubleToSingleChar('"',CellText);
      end;

      if FGrid.JavaCSV then
        JavaToLineFeeds(CellText)
      else
        CSVToLineFeeds(CellText);

      TLocalAdvGrid(FGrid).LoadCell(s,z,CellText);

      Delete(buffer,1,DelimiterPos);

      Inc(s);
      if s >= FGrid.ColCount then
        FGrid.ColCount := s;
    end;

    if Length(Buffer) > 0 then
    begin
      if Buffer[1] = '"' then
        Delete(buffer,1,1);
      if Length(Buffer) > 0 then
      begin
        if Buffer[Length(Buffer)] = '"' then
          Delete(Buffer,Length(Buffer),1);
      end;

      CellText := DoubleToSingleChar('"',Buffer);

      if FGrid.JavaCSV then
        JavaToLineFeeds(CellText)
      else
        CSVToLineFeeds(CellText);

      TLocalAdvGrid(FGrid).LoadCell(s,z,CellText);

      Inc(s);
      if s > FGrid.ColCount then
        FGrid.ColCount := s;
    end;

    Inc(z);

    if (z - FGrid.FixedRows >= FPageSize) then
      Break;
  end;

    if  AutoNumber then
     TLocalAdvGrid(FGrid).Cells[0,z - 1] := IntToStr(FirstRow + z + PageSize - 2);

  Inc(FPage);

  FGrid.RowCount := z + FGrid.FixedFooters;
  
  TLocalAdvGrid(FGrid).CellsChanged(Rect(0,0, FGrid.ColCount, FGrid.RowCount));
  TLocalAdvGrid(FGrid).CellsLoaded;
end;

//------------------------------------------------------------------------------

function TAdvGridCSVPager.JumpPages(I: Integer): Integer;
var
  buffer: String;
  r: Integer;
begin
  Result := 0;
  if (I <= 0) or not FOpened or (self.Eof) or
  (System.Eof(FFile))
  then
    Exit;

  r := 0;
  Result := 1;

  while not System.Eof(FFile) do
  begin
    if (r >= FPageSize) then
    begin
      Inc(Result);
      r := 0;
      if (Result > I) then
      begin
        Dec(Result);
        Break;
      end;
    end;
    ReadLn(FFile, buffer);
    Inc(r);
  end;
   FPage := FPage + Result;
end;

function TAdvGridCSVPager.GetFirstRow: integer;
begin
  Result := PageSize * (Page - 1) + 1;
end;

function TAdvGridCSVPager.GetLastRow: integer;
begin
  Result := PageSize * Page;
  if Result > FTotalRows then
    Result := FTotalRows;
end;


procedure TAdvGridCSVPager.SetAutoNumber(const Value: boolean);
begin
  FAutoNumber := Value;
end;

end.
