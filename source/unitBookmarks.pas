unit unitBookmarks;

interface

uses
  Windows, Classes, SysUtils, Contnrs, XnClasses, XnRawByteStrings;

type
  TBookmark = class;

  TMarkedArticle = class
  private
    fOwner: TBookmark;
    fLines: Integer;
    fSubject: string;
    fFrom: string;
    fBookmarkedDate: TDateTime;
    fDate: TDateTime;
    fCodePage: Integer;
    fGroup: string;
    fAccount: string;
    fMessageID: string;
    function GetRawMessageID: RawByteString;
  public
    constructor Create(AOwner: TBookmark; const AAccount, AGroup, AMessageID, ASubject, AFrom: string; ADate: TDateTime; ALines, ACodePage: Integer; ABookmarkedDate: TDateTime = 0);
    constructor CreateFromBookmarkFileLine(AOwner: TBookmark; st: string);
    constructor CreateFromObject(AOwner: TBookmark; obj: TObject);
    property Owner: TBookmark read fOwner;
    property Account: string read fAccount;
    property Group: string read fGroup;
    property MessageID: string read fMessageID;
    property Subject: string read fSubject;
    property From: string read fFrom;
    property Date: TDateTime read fDate;
    property BookmarkedDate: TDateTime read fBookmarkedDate;
    property Lines: Integer read fLines;
    property CodePage: Integer read fCodePage;
    property RawMessageID: RawByteString read GetRawMessageID;
  end;

  TBookmark = class
  private
    fMarkedArticles: TObjectList;
    fName: string;
    fLoaded: Boolean;
    fDirty: Boolean;
    function GetMarkedArticle(idx: Integer): TMarkedArticle;
    function GetMarkedArticleCount: Integer;

    procedure Load;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure AddArticle(art: TObject);
    procedure DeleteArticle(idx: Integer);
    function Find(const accName, msgid: string; var idx: Integer): Boolean;
    procedure Save;
    procedure Clear;

    property Name: string read fName;
    property Dirty: Boolean read fDirty write fDirty;

    property MarkedArticleCount: Integer read GetMarkedArticleCount;
    property MarkedArticle[idx: Integer]: TMarkedArticle read GetMarkedArticle;
  end;

  TBookmarkSet = class
  private
    fEnumerated: Boolean;
    fNames: TStringList;

    procedure Enumerate;
    function GetBookmarkCount: Integer;
    function GetBookmarkName(idx: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateBookmark(const name: string): TBookmark;
    function GetUniqueName: string;
    procedure DeleteBookmarkSet(const name: string);
    procedure DeleteAllBookmarkSets;

    property BookmarkCount: Integer read GetBookmarkCount;
    property BookmarkName[idx: Integer]: string read GetBookmarkName;
  end;

implementation

uses
  unitCharsetMap, unitStreamTextReader, unitNNTPServices, unitSearchString,
  NewsGlobals, IdGlobal;

{ TMarkedArticle }

constructor TMarkedArticle.Create(AOwner: TBookmark;
  const AAccount, AGroup, AMessageID, ASubject, AFrom: string;
  ADate: TDateTime; ALines, ACodePage: Integer;
  ABookmarkedDate: TDateTime);
begin
  fOwner := AOwner;
  fAccount := AAccount;
  fGroup := AGroup;
  fMessageID := AMessageID;
  fSubject := ASubject;
  fFrom := AFrom;
  fDate := ADate;
  fLines := ALines;
  fCodePage := ACodePAge;
  if ABookmarkedDate = 0 then
    fBookmarkedDate := Now
  else
    fBookmarkedDate := ABookmarkedDate;
end;

constructor TMarkedArticle.CreateFromBookmarkFileLine(AOwner: TBookmark; st: string);
var
  lAccount, lGroup, lMessageID, lSubject, lFrom: string;
  lDate, lBookmarkedDate: TDateTime;
  lLines, lCodePage, i, derr: Integer;
begin
  derr := DateTimeToFileDate(EncodeDate(1980, 1, 1));
  lAccount := Fetch(st, #9);
  lGroup := Fetch(st, #9);
  lMessageID := Fetch(st, #9);
  lSubject := Fetch(st, #9);
  lFrom := Fetch(st, #9);
  i := StrToIntDef(Fetch(st, #9), 0);
  if i = 0 then i := derr;
  lDate := FileDateToDateTime(i);
  lLines := StrToIntDef(Fetch(st, #9), 0);
  lCodePage := StrToIntDef(Fetch(st, #9), 0);
  i := StrToIntDef(Fetch(st, #9), 0);
  if i = 0 then i := derr;
  lBookmarkedDate := FileDateToDateTime(i);

  Create(AOwner, lAccount, lGroup, lMessageID, lSubject, lFrom, lDate, lLines, lCodePage, lBookmarkedDate);
end;

constructor TMarkedArticle.CreateFromObject(AOwner: TBookmark; obj: TObject);
var
  article: TArticle;
begin
  article := TArticle(obj);
  Create(AOwner, TSubscribedGroup(article.Owner).Owner.AccountName,
                 TSubscribedGroup(article.Owner).Name,
                 article.MessageId,
                 article.Subject,
                 '"' + Article.FromName + '" <' + Article.FromEmail + '>',
                 article.Date,
                 article.Lines,
                 article.CodePage);

end;

function TMarkedArticle.GetRawMessageID: RawByteString;
begin
  Result := RawByteString(fMessageID);
end;

{ TBookmark }

procedure TBookmark.AddArticle(art: TObject);
var
  article: TArticle;
  idx: Integer;
begin
  if art is TArticle then
    article := TArticle(art)
  else
    Exit;

  if not Find(TSubscribedGroup(article.Owner).Owner.AccountName, article.MessageId, idx) then
  begin
    fMarkedArticles.Add(TMarkedArticle.CreateFromObject(self, article));
    fDirty := True;
  end;
end;

procedure TBookmark.Clear;
begin
  fMarkedArticles.Clear;
  fLoaded := True;
  fDirty := True;
end;

constructor TBookmark.Create(const AName: string);
begin
  fName := AName;
  fMarkedArticles := TObjectList.Create;
end;

procedure TBookmark.DeleteArticle(idx: Integer);
begin
  fMarkedArticles.Delete(idx);
  fDirty := True;
end;

destructor TBookmark.Destroy;
begin
  Save;
  fMarkedArticles.Free;
  inherited Destroy;
end;

function TBookmark.Find(const accName, msgid: string;
  var idx: Integer): Boolean;
var
  i: Integer;
  ma: TMarkedArticle;
begin
  Result := False;
  for i := 0 to MarkedArticleCount - 1 do
  begin
    ma := MarkedArticle[i];
    if (ma.Account = accName) and (ma.MessageID = msgid) then
    begin
      idx := i;
      Result := True;
      Break;
    end;
  end;
end;

function TBookmark.GetMarkedArticle(idx: Integer): TMarkedArticle;
begin
  Load;
  Result := TMarkedArticle(fMarkedArticles[idx]);
end;

function TBookmark.GetMarkedArticleCount: Integer;
begin
  Load;
  Result := fMarkedArticles.Count;
end;

procedure TBookmark.Load;
var
  f: TFileStream;
  raw: RawByteString;
  str: TStreamTextReader;
  st: string;
begin
  if fLoaded then Exit;

// Line 1 = 'Bookmark <BookmarkName>
// Subsequent lines =
// <Account>#9<Group>#9<Message ID>#9<Subject>#9<From>#9<Date (Dos date)>#9<Lines>#9<CodePage>#9<BookmarkedDate DOS Date>

  str := nil;
  f := TFileStream.Create(gMessageBaseRoot + '\' + Name + '.bmk', fmOpenRead or fmShareDenyNone);
  try
    str := TStreamTextReader.Create(f);

    if str.ReadLn(raw) then
    begin
      st := UTF8ToString(raw);
      if not ((SplitString (' ', st) = 'Bookmark') and (st = name)) then
        raise Exception.CreateFmt(rstBadBookmarkFile, [Name]);

      while str.ReadLn(raw) do
      begin
        st := UTF8ToString(raw);
        if st <> '' then
          fMarkedArticles.Add(TMarkedArticle.CreateFromBookmarkFileLine(Self, st));
      end;
    end;
  finally
    str.Free;
    f.Free;
  end;
  fLoaded := True;
  fDirty := False;
end;

procedure TBookmark.Save;
var
  f: TTextFileWriter;
  i: Integer;
  s: string;
  ma: TMarkedArticle;
begin
  if not fDirty then Exit;
  f := TTextFileWriter.Create(gMessageBaseRoot + '\' + Name + '.bmk');
  try
    f.WriteLn(UTF8Encode('Bookmark ' + Name));
    if fLoaded then
      for i := 0 to MarkedArticleCount - 1 do
      begin
        ma := MarkedArticle[i];
        s := ma.Account + #9 +
             ma.Group + #9 +
             ma.MessageId + #9 +
             ma.Subject + #9 +
             ma.From + #9 +
             IntToStr(DateTimeToFileDate(ma.Date)) + #9 +
             IntToStr(ma.Lines) + #9 +
             IntToStr(ma.CodePage) + #9 +
             IntToStr(DateTimeToFileDate(ma.BookmarkedDate));
         f.WriteLn(UTF8Encode(s));
      end;
  finally
    f.Free;
  end;
  fDirty := False;
  fLoaded := True;
end;

{ TBookmarkSet }

constructor TBookmarkSet.Create;
begin
  fNames := TStringList.Create;
  fNames.Sorted := True;
end;

function TBookmarkSet.CreateBookmark(const name: string): TBookmark;
var
  i: Integer;
begin
  for i := 0 to fNames.Count - 1 do
    if CompareText(fNames[i], name) = 0 then
    begin
      Result := TBookmark.Create(fNames[i]);
      Exit;
    end;
  Result := TBookmark.Create(Name);
  Result.Dirty := True;
  Result.Save;
  fNames.Add(name);
end;

procedure TBookmarkSet.DeleteAllBookmarkSets;
begin
  while fNAmes.Count > 0 do
    DeleteBookmarkSet(fNames[0]);
end;

procedure TBookmarkSet.DeleteBookmarkSet(const name: string);
var
  idx: Integer;
begin
  idx := fNames.IndexOf(name);
  if idx >= 0 then
  begin
    DeleteFile(gMessageBaseRoot + '\' + name + '.bmk');
    fNames.Delete(idx);
  end;
end;

destructor TBookmarkSet.Destroy;
begin
  fNames.Free;
  inherited Destroy;
end;

procedure TBookmarkSet.Enumerate;
var
  f: TSearchRec;
  st: string;
begin
  if fEnumerated then Exit;

  if FindFirst(gMessageBaseRoot + '\*.bmk', faAnyFile, f) = 0 then
  try
    repeat
      st := f.Name;
      fNames.Add(SplitString('.', st));
    until FindNext(f) <> 0;
  finally
    FindClose(f);
  end;
  fEnumerated := True;
end;

function TBookmarkSet.GetBookmarkCount: Integer;
begin
  Enumerate;
  Result := fNames.Count;
end;

function TBookmarkSet.GetBookmarkName(idx: Integer): string;
begin
  Enumerate;
  Result := fNames[idx];
end;

function TBookmarkSet.GetUniqueName: string;
var
  i, bmn, n: Integer;
  st: string;
begin
  bmn := -1;

  for i := 0 to BookmarkCount - 1 do
  begin
    st := BookmarkName[i];
    if Pos('Bookmark ', st) = 1 then
    begin
      st := Copy(st, 9, MaxInt);
      n := StrToIntDef(st, -1);
      if n > bmn then
        bmn := n;
    end;
  end;
  Result := 'Bookmark ' + IntToStr(bmn + 1);
end;

end.
