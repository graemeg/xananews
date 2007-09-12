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

uses Windows, Classes, SysUtils, StrUtils, ConTnrs, unitExSettings, unitSearchString;

type

TNNTPFilterColumn = (ftSubject, ftAuthor, ftDate, ftLines, ftMessageBody, ftMessageID, ftHeaderLines, ftCrossposted, ftNumber);
TNNTPFilterOperator = (opContains, opDoesntContain, opLess, opGreater, opEqual, opNotEqual);
TNNTPValidOperators = set of TNNTPFilterOperator;

TNNTPFilter = class
private
  fName : string;
  fColumn : TNNTPFilterColumn;
  fOperator : TNNTPFilterOperator;
  fIntVal : Integer;
  fDateVal : TDateTime;
  fStrVal : string;
  fUnread : boolean;
  fPattern : string;
  fSearcher : TStringSearcher;
  fTag : Integer;
  fDormant: boolean;
  fCaseSensitive : boolean;
  fInteresting : boolean;
  fTemporary : boolean;
  constructor Create (const ANAme : string; AColumn : TNNTPFilterColumn; AOperator : TNNTPFilterOperator; AUnread, AInteresting : boolean; ACaseSensitive : boolean); overload;
  constructor CreateAndLoad (reg : TExSettings; const AName : string);
  function GetText: string;
public
  constructor Create (const ANAme : string; AColumn : TNNTPFilterColumn; AOperator : TNNTPFilterOperator; AValue : Integer; AUnread, AInteresting: boolean; ACaseSensitive : boolean); overload;
  constructor Create (const ANAme : string; AColumn : TNNTPFilterColumn; AOperator : TNNTPFilterOperator; const AValue : string; AUnread, AInteresting: boolean; ACaseSensitive : boolean); overload;
  constructor Create (const ANAme : string; AColumn : TNNTPFilterColumn; AOperator : TNNTPFilterOperator; const AValue : TDateTime;  AUnread, AInteresting: boolean; ACaseSensitive : boolean); overload;
  constructor Clone (AFilter : TNNTPFilter);
  destructor Destroy; override;
  procedure Assign (f : TNNTPFilter);
  function Matches (obj : TObject) : boolean;
  procedure Save;
  property Name : string read fName write fName;
  property Text : string read GetText;
  property Column : TNNTPFilterColumn read fColumn write fColumn;
  property Operator : TNNTPFilterOperator read fOperator write fOperator;
  property StrVal : string read fStrVal write fStrVal;
  property Tag : Integer read fTag write fTag;
  property Dormant : boolean read fDormant write fDormant;
  property CaseSensitive : boolean read fCaseSensitive write fCaseSensitive;
  property Temporary : boolean read fTemporary write fTemporary;
end;

TNNTPFilters = class (TStringList)
private
  fOwnsObjects : boolean;
  function GetFilter(idx: Integer): TNNTPFilter;
protected
public
  constructor Create (AOwnsObjects : boolean);
  destructor Destroy; override;
  procedure Delete (idx : Integer); override;
  function MatchesAny (obj : TObject) : boolean;
  function MatchesAll (obj : TObject) : boolean;
  function IndexOfFilter (filter : TNNTPFilter) : Integer;
  procedure DeleteFilter (filter : TNNTPFilter);
  procedure DeleteTemporaryFilters;
  property Filter [idx : Integer] : TNNTPFilter read GetFilter; default;
  property OwnsObjects : boolean read fOwnsObjects write fOwnsObjects;
end;

TAllFilters = class (TObjectList)
public
  procedure Load;
  function FindFilter (const Name : string) : TNNTPFilter;
  function AddFilter (filter : TNNTPFilter) : Integer;
  procedure Save;
  procedure DeleteFilter (filter : TNNTPFilter);
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
  sGreater = 'is greater or equal ';
  sEqual = 'is equal to';
  sNotEqual = 'is not equal to';
  sMessageID = 'Message ID';
  sHeaderLines = 'Header Lines';
  sCrossposted = 'Crossposted';

const
  ColumnNames : array [TNNTPFilterColumn] of string = (sSubject, sAuthor, sDate, sLines, sMessageBody, sMessageID, sHeaderLines, sCrossposted, sNumber);
  OperatorNames : array [TNNTPFilterOperator] of string = (sContains, sDoesntContain, sLEss, sGreater, sEqual, sNotEqual);
  ValidOperators : array [TNNTPFilterColumn] of TNNTPValidOperators =
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
  AllFilters : TAllFilters;

function GetOperator (const opName : string; var op : TNNTPFilterOperator) : boolean;
function GetColumn (const colName : string; var col : TNNTPFilterColumn) : boolean;
function TextToNNTPFilter (var text : string; var col : TNNTPFilterColumn; var op : TNNTPFilterOperator) : boolean;

implementation

uses NewsGlobals, unitNNTPServices, unitMessages;

resourcestring
  sFilterFormat = '%s %s %s';

function GetOperator (const opName : string; var op : TNNTPFilterOperator) : boolean;
var
  i : TNNTPFilterOperator;
begin
  result := false;
  for i := Low (TNNTPFilterOperator) to High (TNNTPFilterOperator) do
    if SameText (OperatorNames [i], opName) then
    begin
      op := i;
      result := True;
      break
    end
end;

function GetColumn (const colName : string; var col : TNNTPFilterColumn) : boolean;
var
  c : TNNTPFilterColumn;
begin
  result := false;
  for c := Low (TNNTPFilterColumn) to High (TNNTPFilterColumn) do
    if SameText (ColumnNames [c], colName) then
    begin
      col := c;
      result := True;
      break
    end
end;

function TextToNNTPFilter (var text : string; var col : TNNTPFilterColumn; var op : TNNTPFilterOperator) : boolean;
var
  s1 : string;
begin
  result := False;
  s1 := '';
  repeat
    if s1 = '' then
      s1 := SplitString (' ', text)
    else
      s1 := s1 + ' ' + SplitString (' ', text);

    if GetColumn (s1, col) then
    begin
      s1 := '';
      repeat
        if s1 = '' then
          s1 := SplitString (' ', text)
        else
          s1 := s1 + ' ' + SplitString (' ', text);

        if GetOperator (s1, op) then
        begin
          result := True;
          break
        end
      until text = '';
      break
    end
  until text = ''
end;

{ TNNTPFilter }

constructor TNNTPFilter.Create(const AName : string; AColumn: TNNTPFilterColumn;
  AOperator: TNNTPFilterOperator; const AValue: string; AUnread, AInteresting : boolean; ACaseSensitive : boolean);
begin
  Create (AName, AColumn, AOperator, AUnread, AInteresting, ACaseSensitive);
  fStrVal := AValue;
end;

constructor TNNTPFilter.Create(const AName : string; AColumn: TNNTPFilterColumn;
  AOperator: TNNTPFilterOperator; AValue: Integer; AUnread, AInteresting: boolean; ACaseSensitive : boolean);
begin
  Create (AName, AColumn, AOperator, AUnread, AInteresting, ACaseSensitive);
  fIntVal := AValue;
  fStrVal := IntToStr (AValue);
end;

constructor TNNTPFilter.Clone(AFilter: TNNTPFilter);
begin
  self.Assign (AFilter);
end;

procedure TNNTPFilter.Assign (f : TNNTPFilter);
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

constructor TNNTPFilter.Create(const AName : string; AColumn: TNNTPFilterColumn;
  AOperator: TNNTPFilterOperator; const AValue: TDateTime; AUnread, AInteresting: boolean; ACaseSensitive : boolean);
begin
  Create (AName, AColumn, AOperator, AUnread, AInteresting, ACaseSensitive);
  fDateVal := AValue;
  fStrVal := DateTimeToStr (AValue)
end;

constructor TNNTPFilter.Create(const AName : string; AColumn: TNNTPFilterColumn;
  AOperator: TNNTPFilterOperator; AUnread, AInteresting: boolean; ACaseSensitive : boolean);
begin
  fName := AName;
  fColumn := AColumn;
  fOperator := AOperator;
  fUnread := AUnread;
  fInteresting := AInteresting;
  fCaseSensitive := ACaseSensitive
end;

constructor TNNTPFilter.CreateAndLoad(reg: TExSettings; const AName: string);
var
  st : string;
  p : Integer;
  column : TNNTPFilterColumn;
  operator : TNNTPFilterOperator;
  err : boolean;
begin
  st := reg.StringValue [AName];
  err := True;

  column := ftSubject;
  operator := opEqual;

  if st <> '' then
  begin
    p := Pos (',', st);
    if p > 0 then
    begin
      column := TNNTPFilterColumn (StrToInt (Copy (st, 1, p - 1)));
      st := Copy (st, p + 1, MaxInt);

      p := Pos (',', st);

      if p > 0 then
      begin
        operator := TNNTPFilterOperator (StrToInt (Copy (st, 1, p - 1)));
        st := Copy (st, p + 1, MaxInt);

        case column of
          ftDate : fDateVal := StrToDateTime (st);
          ftLines : fIntVal := StrToInt (st);
          ftCrossposted : fIntVal := StrToInt (st);
        end;

        err := False
      end
    end
  end;

  if err then
    Raise Exception.CreateFmt ('Corrupt filter ''%s'' in registry', [AName])
  else
    Create (AName, column, operator, st, False, False, False);
end;

function TNNTPFilter.GetText: string;
begin
  result := Format (sFilterFormat, [ColumnNames [fColumn], OperatorNames [fOperator], fStrVal]);
end;

function TNNTPFilter.Matches(obj : TObject): boolean;
var
  st, searchSt, fn : string;
  dt : TDateTime;
  i, it: Integer;
  article : TArticleBase;
  m : TmvMessage;
begin
  result := False;
  if Dormant then Exit;

  article := obj as TArticleBase;
  if fUnread and article.IsRead then Exit;
  if fInteresting and not article.IsInteresting then Exit;

  dt := 0;
  it := 0;

  if not Assigned (fSearcher) and (fOperator in [opContains, opDoesntContain]) then
    fSearcher := TGoogleLikeStringSearcher.Create('', fCaseSensitive)
  else
    if Assigned (fSearcher) then
      fSearcher.CaseSensitive := fCaseSensitive;


  st := '';
  case fColumn of
    ftNumber  : it := article.ArticleNo;
    ftSubject : st := article.Subject;
    ftAuthor : st := article.From;
    ftMessageID : st := article.MessageId;
    ftDate : dt := article.Date;
    ftLines : it := article.Lines;
    ftMessageBody : if Assigned (article.Msg) then
                    begin
                      m := article.Msg;
                      if Assigned (m.TextPart) then
                        st := m.TextPart.Text;

                      if Assigned (m.MessageParts) then
                        for i := 0 to m.MessageParts.Count - 1 do
                        begin
                          fn := m.MessageParts [i].FileName;
                          if Pos ('.', fn) > 0 then
                            st := st + ' ' + fn
                        end
                    end;

    ftHeaderLines : if Assigned (article.Msg) and Assigned (article.Msg.Header) then
                      st := article.Msg.Header.Text
                    else
                      if (article is TArticle) then
                        st := TArticle (article).TempExtraHeaders;
    ftCrossposted : it := article.CrosspostedTo;
  end;

  if not caseSensitive then
  begin
    st := UpperCase (st);
    searchSt := UpperCase (fStrVal);
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
            result := fSearcher.Matches(st)
          end;
        ftDate :
          result := AnsiContainsText (DateTimeToStr (dt), searchSt);
        ftLines,
        ftNumber,
        ftCrossposted :
          result := AnsiContainsText (IntToStr (it), searchSt)
      end;

    opLess,
    opGreater :
      case fColumn of
        ftSubject, ftAuthor, ftMessageBody, ftMessageID, ftHeaderLines :
          result := st < searchSt;
        ftDate:
          result := dt < fDateVal;
        ftLines,
        ftNumber,
        ftCrossposted :
          result := it < fIntVal;
      end;

    opEqual,
    opNotEqual :
      case fColumn of
        ftSubject, ftAuthor, ftMessageBody, ftMessageID, ftHeaderLines:
          result := st = searchSt;
        ftDate:
          result := dt = fDateVal;
        ftLines,
        ftNumber,
        ftCrossposted :
          result := it = fIntVal
      end
  end;

  if odd (Integer (fOperator)) then
    result := not result;
end;

procedure TNNTPFilter.Save;
var
  reg : TExSettings;
begin
  if Temporary then Exit;

  reg := CreateExSettings;
  try
    reg.Section := 'Filters';
    reg.StringValue [fName] := Format ('%d,%d,%s', [Integer (fColumn), Integer (fOperator), fStrVal]);
  finally
    reg.Free
  end
end;

destructor TNNTPFilter.Destroy;
begin
  fSearcher.Free;

  inherited;
end;

{ TNNTPFilters }

constructor TNNTPFilters.Create (AOwnsObjects : boolean);
begin
  inherited Create;
  fOwnsObjects := AOwnsObjects
end;

procedure TNNTPFilters.DeleteFilter (filter : TNNTPFilter);
var
  idx : Integer;
begin
  idx := IndexOfObject(filter);
  if idx <> 0 then
    Delete(idx);
end;

procedure TNNTPFilters.DeleteTemporaryFilters;
var
  idx : Integer;
begin
  idx := 0;
  while idx < Count do
    if TNNTPFilter (Objects [idx]).Temporary then
      Delete(idx)
    else
      Inc (idx)
end;

procedure TNNTPFilters.Delete (idx : Integer);
var
  obj : TObject;
begin
  obj := Objects [idx];
  if OwnsObjects  or (TNNTPFilter (obj).Temporary) then
    obj.Free;

  inherited;
end;

destructor TNNTPFilters.Destroy;
var
  i : Integer;
  obj : TObject;
begin
  for i := 0 to Count - 1 do
  begin
    obj := Objects [i];
    if OwnsObjects  or (TNNTPFilter (obj).Temporary) then
      obj.Free
  end;
  inherited;
end;

function TNNTPFilters.GetFilter(idx: Integer): TNNTPFilter;
begin
  result := TNNTPFilter (Objects [idx])
end;

function TNNTPFilters.IndexOfFilter(filter: TNNTPFilter): Integer;
var
  i : Integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
    if Objects [i] = filter then
    begin
      result := i;
      break
    end
end;

function TNNTPFilters.MatchesAll(obj: TObject): boolean;
var
  i : Integer;
begin
  result := Count > 0;
  for i := 0 to Count - 1 do
    if not ((self.Strings [i] <> 'off') and TNNTPFilter (Objects [i]).Matches(obj)) then
    begin
      result := False;
      break
    end
end;

function TNNTPFilters.MatchesAny(obj : TObject): boolean;
var
  i : Integer;
begin
  result := False;
  for i := 0 to Count - 1 do
    if (self.Strings [i] <> 'off') and TNNTPFilter (Objects [i]).Matches(obj) then
    begin
      result := True;
      break
    end
end;

{ TAllFilters }

function TAllFilters.AddFilter(filter: TNNTPFilter) : Integer;
begin
  result := Add (filter);
end;

procedure TAllFilters.DeleteFilter(filter: TNNTPFilter);
begin
  Extract (filter);
  filter.Dormant := True;
end;

function TAllFilters.FindFilter(const Name: string): TNNTPFilter;
var
  i : Integer;
begin
  result := Nil;
  for i := 0 to count - 1 do
    if TNNTPFilter (Items [i]).fName = Name then
    begin
      result := TNNTPFilter (Items [i]);
      break
    end
end;

procedure TAllFilters.Load;
var
  reg : TExSettings;
  valueNames : TStrings;
  i : Integer;
  filter : TNNTPFilter;
begin
  valueNames := Nil;
  reg := CreateExSettings;
  try
    reg.Section := 'Filters';
    valueNames := TStringList.Create;
    if reg.Open (true) then
    begin
      reg.GetValueNames(valueNames);

      for i := 0 to valueNames.Count - 1 do
      begin
        try
          filter := TNNTPFilter.CreateAndLoad (reg, valueNames [i]);
          Add (filter)
        except
        end
      end
    end
  finally
    valueNames.Free;
    reg.Free;
  end
end;

procedure TAllFilters.Save;
var
  reg : TExSettings;
  i : Integer;
  filter : TNNTPFilter;
begin
  reg := CreateExSettings;
  try
    reg.DeleteSection ('Filters');
    reg.Section := 'Filters';
    for i := 0 to Count - 1 do
    begin
      filter := TNNTPFilter (Items [i]);
      with filter do
        reg.StringValue [fName] := Format ('%d,%d,%s', [Integer (fColumn), Integer (fOperator), fStrVal])
    end
  finally
    reg.Free
  end
end;

initialization
  AllFilters := TAllFilters.Create;
finalization
  AllFilters.Free
end.
