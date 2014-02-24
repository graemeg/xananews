(*======================================================================*
 | unitNNTPFilters unit for NewsReader3                                 |
 |                                                                      |
 | Message filters for XanaNews                                         |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      19/11/2001  CPWW  Original                                  |
 *======================================================================*)

unit unitNNTPFilters;

interface

uses
  Windows, Classes, SysUtils, StrUtils, ConTnrs, unitExSettings, unitSearchString;

type
  // Note: Be careful when changing the following definition(s), some units depend on the specific order.
  TNNTPFilterColumn = (ftSubject, ftAuthor, ftDate, ftLines, ftMessageBody, ftMessageID, ftHeaderLines, ftCrossposted, ftNumber);
  TNNTPFilterOperator = (opContains, opDoesntContain, opLess, opGreater, opEqual, opNotEqual);
  TNNTPValidOperators = set of TNNTPFilterOperator;

  TNNTPFilter = class
  private
    fName: string;
    fColumn: TNNTPFilterColumn;
    fOperator: TNNTPFilterOperator;
    fIntVal: Int64;
    fDateVal: TDateTime;
    fStrVal: string;
    fUnread: Boolean;
    fPattern: string;
    fSearcher: TStringSearcher;
    fTag: Integer;
    fDormant: Boolean;
    fCaseSensitive: Boolean;
    fInteresting: Boolean;
    fTemporary: Boolean;
    constructor Create(const ANAme: string; AColumn: TNNTPFilterColumn; AOperator: TNNTPFilterOperator; AUnread, AInteresting: Boolean; ACaseSensitive: Boolean); overload;
    constructor CreateAndLoad(reg: TExSettings; const AName: string);
    function GetText: string;
  public
    constructor Create(const ANAme: string; AColumn: TNNTPFilterColumn; AOperator: TNNTPFilterOperator; AValue: Int64; AUnread, AInteresting: Boolean; ACaseSensitive: Boolean); overload;
    constructor Create(const ANAme: string; AColumn: TNNTPFilterColumn; AOperator: TNNTPFilterOperator; const AValue: string; AUnread, AInteresting: Boolean; ACaseSensitive: Boolean); overload;
    constructor Create(const ANAme: string; AColumn: TNNTPFilterColumn; AOperator: TNNTPFilterOperator; const AValue: TDateTime; AUnread, AInteresting: Boolean; ACaseSensitive: Boolean); overload;
    constructor Clone(AFilter: TNNTPFilter);
    destructor Destroy; override;
    procedure Assign(f: TNNTPFilter);
    function Matches(obj: TObject): Boolean;
    procedure Save;
    property Name: string read fName write fName;
    property Text: string read GetText;
    property Column: TNNTPFilterColumn read fColumn write fColumn;
    property Operator: TNNTPFilterOperator read fOperator write fOperator;
    property StrVal: string read fStrVal write fStrVal;
    property Tag: Integer read fTag write fTag;
    property Dormant: Boolean read fDormant write fDormant;
    property CaseSensitive: Boolean read fCaseSensitive write fCaseSensitive;
    property Temporary: Boolean read fTemporary write fTemporary;
  end;

  TNNTPFilters = class(TStringList)
  private
    fOwnsObjects: Boolean;
    function GetFilter(idx: Integer): TNNTPFilter;
  protected
  public
    constructor Create(AOwnsObjects: Boolean);
    destructor Destroy; override;
    procedure Delete(idx: Integer); override;
    function MatchesAny(obj: TObject): Boolean;
    function MatchesAll(obj: TObject): Boolean;
    function IndexOfFilter(filter: TNNTPFilter): Integer;
    procedure DeleteFilter(filter: TNNTPFilter);
    procedure DeleteTemporaryFilters;
    property Filter[idx: Integer]: TNNTPFilter read GetFilter; default;
    property OwnsObjects: Boolean read fOwnsObjects write fOwnsObjects;
  end;

  TAllFilters = class(TObjectList)
  public
    procedure Load;
    function FindFilter(const Name: string): TNNTPFilter;
    function AddFilter(filter: TNNTPFilter): Integer;
    procedure Save;
    procedure DeleteFilter(filter: TNNTPFilter);
  end;

resourcestring
  sNumber = 'Number';
  sSubject = 'Subject';
  sAuthor = 'Author';
  sDate = 'Date';
  sLines = 'Lines';
  sMessageBody = 'Message Body';
  sContains = 'matches';
  sDoesntContain = 'doesn''t match';
  sLess = 'is less than';
  sGreater = 'is greater or equal';
  sEqual = 'is equal to';
  sNotEqual = 'is not equal to';
  sMessageID = 'Message ID';
  sHeaderLines = 'Header Lines';
  sCrossposted = 'Crossposted';

const
  ColumnNames: array[TNNTPFilterColumn] of string = (sSubject, sAuthor, sDate, sLines, sMessageBody, sMessageID, sHeaderLines, sCrossposted, sNumber);
  OperatorNames: array[TNNTPFilterOperator] of string = (sContains, sDoesntContain, sLess, sGreater, sEqual, sNotEqual);
  ValidOperators: array[TNNTPFilterColumn] of TNNTPValidOperators =
    ([opContains, opDoesntContain, opEqual, opNotEqual],
     [opContains, opDoesntContain, opEqual, opNotEqual],
     [opEqual, opNotEqual, opLess, opGreater],
     [opEqual, opNotEqual, opLess, opGreater],
     [opContains, opDoesntContain],
     [opEqual, opNotEqual],
     [opContains, opDoesntContain],
     [opEqual, opNotEqual, opLess, opGreater],
     [opEqual, opNotEqual, opLess, opGreater]);

var
  AllFilters: TAllFilters;

function GetOperator(const opName: string; var op: TNNTPFilterOperator): Boolean;
function GetColumn(const colName: string; var col: TNNTPFilterColumn): Boolean;
function TextToNNTPFilter(var text: string; var col: TNNTPFilterColumn; var op: TNNTPFilterOperator): Boolean;

implementation

uses
  {$if CompilerVersion >= 24.0} // 24.0 = Delphi XE3
    System.Types,
  {$ifend}
  NewsGlobals, unitNNTPServices, unitMessages;

resourcestring
  sFilterFormat = '%s %s %s';

function GetOperator(const opName: string; var op: TNNTPFilterOperator): Boolean;
var
  i: TNNTPFilterOperator;
begin
  Result := False;
  for i := Low(TNNTPFilterOperator) to High(TNNTPFilterOperator) do
    if SameText(OperatorNames[i], opName) then
    begin
      op := i;
      Result := True;
      Break;
    end;
end;

function GetColumn(const colName: string; var col: TNNTPFilterColumn): Boolean;
var
  c: TNNTPFilterColumn;
begin
  Result := False;
  for c := Low(TNNTPFilterColumn) to High(TNNTPFilterColumn) do
    if SameText(ColumnNames[c], colName) then
    begin
      col := c;
      Result := True;
      Break;
    end;
end;

function TextToNNTPFilter(var text: string; var col: TNNTPFilterColumn; var op: TNNTPFilterOperator): Boolean;
var
  s1: string;
begin
  Result := False;
  s1 := '';
  repeat
    if s1 = '' then
      s1 := SplitString(' ', text)
    else
      s1 := s1 + ' ' + SplitString(' ', text);

    if GetColumn(s1, col) then
    begin
      s1 := '';
      repeat
        if s1 = '' then
          s1 := SplitString(' ', text)
        else
          s1 := s1 + ' ' + SplitString(' ', text);

        if GetOperator(s1, op) then
        begin
          Result := True;
          Break;
        end;
      until text = '';
      Break;
    end;
  until text = '';
end;

{ TNNTPFilter }

constructor TNNTPFilter.Create(const AName: string; AColumn: TNNTPFilterColumn;
  AOperator: TNNTPFilterOperator; const AValue: string; AUnread, AInteresting: Boolean; ACaseSensitive: Boolean);
begin
  Create(AName, AColumn, AOperator, AUnread, AInteresting, ACaseSensitive);
  fStrVal := AValue;
end;

constructor TNNTPFilter.Create(const AName: string; AColumn: TNNTPFilterColumn;
  AOperator: TNNTPFilterOperator; AValue: Int64; AUnread, AInteresting: Boolean; ACaseSensitive: Boolean);
begin
  Create(AName, AColumn, AOperator, AUnread, AInteresting, ACaseSensitive);
  fIntVal := AValue;
  fStrVal := IntToStr(AValue);
end;

constructor TNNTPFilter.Clone(AFilter: TNNTPFilter);
begin
  Self.Assign(AFilter);
end;

procedure TNNTPFilter.Assign(f: TNNTPFilter);
begin
  fName := f.fName;
  fStrVal := f.fStrVal;
  fColumn := f.fColumn;
  fOperator := f.fOperator;
  fIntVal := f.fIntVal;
  fDateVal := f.fDateVal;
  fUnread := f.fUnread;
  fInteresting := f.fInteresting;
  fPattern := f.fPattern;
  fCaseSensitive := f.fCaseSensitive;
  fTemporary := f.fTemporary;
end;

constructor TNNTPFilter.Create(const AName: string; AColumn: TNNTPFilterColumn;
  AOperator: TNNTPFilterOperator; const AValue: TDateTime; AUnread, AInteresting: Boolean; ACaseSensitive: Boolean);
begin
  Create(AName, AColumn, AOperator, AUnread, AInteresting, ACaseSensitive);
  fDateVal := AValue;
  fStrVal := DateTimeToStr(AValue);
end;

constructor TNNTPFilter.Create(const AName: string; AColumn: TNNTPFilterColumn;
  AOperator: TNNTPFilterOperator; AUnread, AInteresting: Boolean; ACaseSensitive: Boolean);
begin
  fName := AName;
  fColumn := AColumn;
  fOperator := AOperator;
  fUnread := AUnread;
  fInteresting := AInteresting;
  fCaseSensitive := ACaseSensitive;
end;

constructor TNNTPFilter.CreateAndLoad(reg: TExSettings; const AName: string);
var
  st: string;
  p: Integer;
  column: TNNTPFilterColumn;
  operator: TNNTPFilterOperator;
  err: Boolean;
begin
  st := reg.StringValue[AName];
  err := True;

  column := ftSubject;
  operator := opEqual;

  if st <> '' then
  begin
    p := Pos(',', st);
    if p > 0 then
    begin
      column := TNNTPFilterColumn(StrToInt(Copy(st, 1, p - 1)));
      st := Copy(st, p + 1, MaxInt);

      p := Pos(',', st);

      if p > 0 then
      begin
        operator := TNNTPFilterOperator(StrToInt(Copy(st, 1, p - 1)));
        st := Copy(st, p + 1, MaxInt);

        case column of
          ftDate: fDateVal := StrToDateTime(st);
          ftLines: fIntVal := StrToInt(st);
          ftCrossposted: fIntVal := StrToInt(st);
        end;

        err := False;
      end;
    end;
  end;

  if err then
    raise Exception.CreateFmt('Corrupt filter ''%s'' in registry', [AName])
  else
    Create(AName, column, operator, st, False, False, False);
end;

function TNNTPFilter.GetText: string;
begin
  Result := Format(sFilterFormat, [ColumnNames[fColumn], OperatorNames[fOperator], fStrVal]);
end;

function TNNTPFilter.Matches(obj: TObject): Boolean;
var
  st, searchSt, fn: string;
  dt: TDateTime;
  i: Integer;
  it: Int64;
  article: TArticleBase;
  m: TmvMessage;
begin
  Result := False;
  if Dormant then Exit;

  article := obj as TArticleBase;
  if fUnread and article.IsRead then Exit;
  if fInteresting and not article.IsInteresting then Exit;

  dt := 0;
  it := 0;

  if not Assigned(fSearcher) and (fOperator in [opContains, opDoesntContain]) then
    fSearcher := TGoogleLikeStringSearcher.Create('', fCaseSensitive)
  else
    if Assigned(fSearcher) then
      fSearcher.CaseSensitive := fCaseSensitive;

  st := '';
  case fColumn of
    ftNumber : it := article.ArticleNo;
    ftSubject: st := article.Subject;
    ftAuthor: st := article.FromName;
    ftMessageID: st := article.MessageId;
    ftDate: dt := article.Date;
    ftLines: it := article.Lines;
    ftMessageBody: if Assigned(article.Msg) then
                    begin
                      m := article.Msg;
                      if Assigned(m.TextPart) then
                        st := string(m.TextPart.Text);

                      if Assigned(m.MessageParts) then
                        for i := 0 to m.MessageParts.Count - 1 do
                        begin
                          fn := m.MessageParts[i].FileName;
                          if Pos('.', fn) > 0 then
                            st := st + ' ' + fn;
                        end;
                    end;

    ftHeaderLines: if Assigned(article.Msg) and Assigned(article.Msg.Header) then
                      st := string(article.Msg.Header.Text)
                    else
                      if (article is TArticle) then
                        st := TArticle(article).TempExtraHeaders;
    ftCrossposted: it := article.CrosspostedTo;
  end;

  if not caseSensitive then
  begin
    st := UpperCase(st);
    searchSt := UpperCase(fStrVal);
  end
  else
    searchSt := fStrVal;

  case fOperator of
    opContains,
    opDoesntContain:
      case fColumn of
        ftSubject, ftAuthor, ftMessageBody, ftMessageID, ftHeaderLines:
          begin
            fSearcher.Parse(searchSt);
            Result := fSearcher.Matches(st);
          end;
        ftDate:
          Result := AnsiContainsText(DateTimeToStr(dt), searchSt);
        ftLines,
        ftNumber,
        ftCrossposted:
          Result := AnsiContainsText(IntToStr(it), searchSt);
      end;

    opLess,
    opGreater:
      case fColumn of
        ftSubject, ftAuthor, ftMessageBody, ftMessageID, ftHeaderLines:
          Result := st < searchSt;
        ftDate:
          Result := dt < fDateVal;
        ftLines,
        ftNumber,
        ftCrossposted:
          Result := it < fIntVal;
      end;

    opEqual,
    opNotEqual:
      case fColumn of
        ftSubject, ftAuthor, ftMessageBody, ftMessageID, ftHeaderLines:
          Result := st = searchSt;
        ftDate:
          Result := dt = fDateVal;
        ftLines,
        ftNumber,
        ftCrossposted:
          Result := it = fIntVal;
      end;
  end;

  if Odd(Integer(fOperator)) then
    Result := not Result;
end;

procedure TNNTPFilter.Save;
var
  reg: TExSettings;
begin
  if Temporary then Exit;

  reg := CreateExSettings;
  try
    reg.Section := 'Filters';
    reg.StringValue[fName] := Format('%d,%d,%s', [Integer(fColumn), Integer(fOperator), fStrVal]);
  finally
    reg.Free;
  end;
end;

destructor TNNTPFilter.Destroy;
begin
  fSearcher.Free;
  inherited Destroy;
end;

{ TNNTPFilters }

constructor TNNTPFilters.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  fOwnsObjects := AOwnsObjects;
end;

procedure TNNTPFilters.DeleteFilter(filter: TNNTPFilter);
var
  idx: Integer;
begin
  idx := IndexOfObject(filter);
  if idx <> 0 then
    Delete(idx);
end;

procedure TNNTPFilters.DeleteTemporaryFilters;
var
  idx: Integer;
begin
  idx := 0;
  while idx < Count do
    if TNNTPFilter(Objects[idx]).Temporary then
      Delete(idx)
    else
      Inc(idx);
end;

procedure TNNTPFilters.Delete(idx: Integer);
var
  obj: TObject;
begin
  obj := Objects[idx];
  if OwnsObjects or (TNNTPFilter(obj).Temporary) then
    obj.Free;

  inherited;
end;

destructor TNNTPFilters.Destroy;
var
  i: Integer;
  obj: TObject;
begin
  for i := 0 to Count - 1 do
  begin
    obj := Objects[i];
    if OwnsObjects or (TNNTPFilter(obj).Temporary) then
      obj.Free;
  end;
  inherited Destroy;
end;

function TNNTPFilters.GetFilter(idx: Integer): TNNTPFilter;
begin
  Result := TNNTPFilter(Objects[idx]);
end;

function TNNTPFilters.IndexOfFilter(filter: TNNTPFilter): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Objects[i] = filter then
    begin
      Result := i;
      Break;
    end;
end;

function TNNTPFilters.MatchesAll(obj: TObject): Boolean;
var
  i: Integer;
begin
  Result := Count > 0;
  for i := 0 to Count - 1 do
    if not ((Self.Strings[i] <> 'off') and TNNTPFilter(Objects[i]).Matches(obj)) then
    begin
      Result := False;
      Break;
    end;
end;

function TNNTPFilters.MatchesAny(obj: TObject): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if (Self.Strings[i] <> 'off') and TNNTPFilter(Objects[i]).Matches(obj) then
    begin
      Result := True;
      Break;
    end;
end;

{ TAllFilters }

function TAllFilters.AddFilter(filter: TNNTPFilter): Integer;
begin
  Result := Add(filter);
end;

procedure TAllFilters.DeleteFilter(filter: TNNTPFilter);
begin
  Extract(filter);
  filter.Dormant := True;
end;

function TAllFilters.FindFilter(const Name: string): TNNTPFilter;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to count - 1 do
    if TNNTPFilter(Items[i]).fName = Name then
    begin
      Result := TNNTPFilter(Items[i]);
      Break;
    end;
end;

procedure TAllFilters.Load;
var
  reg: TExSettings;
  valueNames: TStrings;
  i: Integer;
  filter: TNNTPFilter;
begin
  valueNames := nil;
  reg := CreateExSettings;
  try
    reg.Section := 'Filters';
    valueNames := TStringList.Create;
    if reg.Open(True) then
    begin
      reg.GetValueNames(valueNames);

      for i := 0 to valueNames.Count - 1 do
      begin
        try
          filter := TNNTPFilter.CreateAndLoad(reg, valueNames[i]);
          Add(filter);
        except
        end;
      end;
    end;
  finally
    valueNames.Free;
    reg.Free;
  end;
end;

procedure TAllFilters.Save;
var
  reg: TExSettings;
  i: Integer;
  filter: TNNTPFilter;
begin
  reg := CreateExSettings;
  try
    reg.DeleteSection('Filters');
    reg.Section := 'Filters';
    for i := 0 to Count - 1 do
    begin
      filter := TNNTPFilter(Items[i]);
      with filter do
        reg.StringValue[fName] := Format('%d,%d,%s', [Integer(fColumn), Integer(fOperator), fStrVal]);
    end;
  finally
    reg.Free;
  end;
end;

initialization
  AllFilters := TAllFilters.Create;
finalization
  AllFilters.Free;
end.
