unit unitBookmarks;

interface

uses Windows, Classes, SysUtils, ConTnrs;

type

TBookmark = class;

TMarkedArticle = class
private
  fOwner : TBookmark;
  fLines: Integer;
  fSubject: string;
  fFrom: string;
  fBookmarkedDate: TDateTime;
  fDate: TDateTime;
  fCodePage : Integer;
  fGroup: string;
  fAccount: string;
  fMessageID: string;
public
  constructor Create (AOwner : TBookmark; const AAccount, AGroup, AMessageID, ASubject, AFrom : string; ADate : TDateTime; ALines, ACodePage : Integer; ABookmarkedDate : TDateTime = 0);
  constructor CreateFromBookmarkFileLine (AOwner : TBookmark; st : string);
  constructor CreateFromObject (AOwner : TBookmark; obj : TObject);
  property Owner : TBookmark read fOwner;
  property Account : string read fAccount;
  property Group : string read fGroup;
  property MessageID : string read fMessageID;
  property Subject : string read fSubject;
  property From : string read fFrom;
  property Date : TDateTime read fDate;
  property BookmarkedDate : TDateTime read fBookmarkedDate;
  property Lines : Integer read fLines;
  property CodePage : Integer read fCodePage;
end;

TBookmark = class
private
  fMarkedArticles : TObjectList;
  fName: string;
  fLoaded : boolean;
  fDirty : boolean;
  function GetMarkedArticle(idx: Integer): TMarkedArticle;
  function GetMarkedArticleCount: Integer;

  procedure Load;
public
  constructor Create (const AName : string);
  destructor Destroy; override;

  procedure AddArticle (art : TObject);
  procedure DeleteArticle (idx : Integer);
  function Find (const accName, msgid : string; var idx : Integer) : boolean;
  procedure Save;
  procedure Clear;

  property Name : string read fName;
  property Dirty : boolean read fDirty write fDirty;

  property MarkedArticleCount : Integer read GetMarkedArticleCount;
  property MarkedArticle [idx : Integer] : TMarkedArticle read GetMarkedArticle;
end;

TBookmarkSet = class
private
  fEnumerated : boolean;
  fNames : TStringList;

  procedure Enumerate;
  function GetBookmarkCount: Integer;
  function GetBookmarkName(idx: Integer): string;
public
  constructor Create;
  destructor Destroy; override;

  function CreateBookmark (const name : string) : TBookmark;
  function GetUniqueName : string;
  procedure DeleteBookmarkSet (const name : string);
  procedure DeleteAllBookmarkSets;

  property BookmarkCount : Integer read GetBookmarkCount;
  property BookmarkName [idx : Integer] : string read GetBookmarkName;
end;

implementation

uses unitStreamTextReader, unitNNTPServices, unitSearchString, NewsGlobals, IdGlobal;

{ TMarkedArticle }

constructor TMarkedArticle.Create(AOwner: TBookmark; const  AAccount, AGroup, AMessageID, ASubject,
  AFrom: string; ADate: TDateTime; ALines, ACodePage : Integer;
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
    fBookmarkedDate := ABookmarkedDate
end;

constructor TMarkedArticle.CreateFromBookmarkFileLine(AOwner: TBookmark;
  st: string);
var
  lAccount, lGroup, lMessageID, lSubject, lFrom : string;
  lDate, lBookmarkedDate : TDateTime;
  lLines, lCodePage, i, derr : Integer;
begin
  derr := DateTimeToFileDate (EncodeDate (1980, 1, 1));
  lAccount := Fetch (st, #9);
  lGroup := Fetch (st, #9);
  lMessageID := Fetch (st, #9);
  lSubject := Fetch (st, #9);
  lFrom := Fetch (st, #9);
  i := StrToIntDef (Fetch (st, #9), 0);
  if i = 0 then i := derr;
  lDate := FileDateToDateTime (i);
  lLines := StrToIntDef (Fetch (st, #9), 0);
  lCodePage := StrToIntDef (Fetch (st, #9), 0);
  i := StrToIntDef (Fetch (st, #9), 0);
  if i = 0 then i := derr;
  lBookmarkedDate := FileDateToDateTime (i);

  Create (AOwner, lAccount, lGroup, lMessageID, lSubject, lFrom, lDate, lLines, lCodePage, lBookmarkedDate)
end;

constructor TMarkedArticle.CreateFromObject(AOwner : TBookmark; obj: TObject);
var
  article : TArticle;
begin
  article := TArticle (obj);
  Create (AOwner, TSubscribedGroup (article.Owner).Owner.AccountName,
                  TSubscribedGroup (article.Owner).Name,
                  article.MessageId,
                  article.Subject, article.From,
                  article.Date,
                  article.Lines, article.CodePage)

end;

{ TBookmark }

procedure TBookmark.AddArticle(art: TObject);
var
  article : TArticle;
  idx : Integer;
begin
  if art is TArticle then
    article := TArticle (art)
  else
    Exit;

  if not Find (TSubscribedGroup (article.Owner).Owner.AccountName, article.MessageId, idx) then
  begin
    fMarkedArticles.Add(TMarkedArticle.CreateFromObject (self, article));
    fDirty := True
  end
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

  inherited;
end;

function TBookmark.Find(const accName, msgid: string;
  var idx: Integer): boolean;
var
  i : Integer;
  ma : TMarkedArticle;
begin
  result := False;
  for i := 0 to MarkedArticleCount - 1 do
  begin
    ma := MarkedArticle [i];
    if (ma.Account = accName) and (ma.MessageID = msgid) then
    begin
      idx := i;
      result := True;
      break
    end
  end
end;

function TBookmark.GetMarkedArticle(idx: Integer): TMarkedArticle;
begin
  Load;
  result := TMarkedArticle (fMarkedArticles [idx])
end;

function TBookmark.GetMarkedArticleCount: Integer;
begin
  Load;
  result := fMarkedArticles.Count
end;

procedure TBookmark.Load;
var
  f : TFileStream;
  str : TStreamTextReader;
  st : string;
begin
  if fLoaded then exit;

// Line 1 = 'Bookmark <BookmarkName>
// Subsequent lines =
// <Account>#9<Group>#9<Message ID>#9<Subject>#9<From>#9<Date (Dos date)>#9<Lines>#9<CodePage>#9<BookmarkedDate DOS Date>

  str := Nil;
  f := TFileStream.Create(gMessageBaseRoot + '\' + Name + '.bmk', fmOpenRead or fmShareDenyNone);
  try
    str := TStreamTextReader.Create(f);

    if str.ReadLn (st) then
    begin
      if not ((SplitString (' ', st) = 'Bookmark') and (st = name)) then
        raise Exception.CreateFmt (rstBadBookmarkFile, [Name]);

      while str.ReadLn(st) do
        if st <> '' then
          fMarkedArticles.Add(TMarkedArticle.CreateFromBookmarkFileLine(self, st))
    end;

  finally
    str.Free;
    f.Free;
  end;
  fLoaded := True;
  fDirty := False
end;

procedure TBookmark.Save;
var
  f : TTextFileWriter;
  i : Integer;
  ma : TMarkedArticle;
begin
  if not fDirty then Exit;
  f := TTextFileWriter.Create(gMessageBaseRoot + '\' + Name + '.bmk');
  try
    f.WriteLn('Bookmark ' + Name);
    if fLoaded then
      for i := 0 to MarkedArticleCount - 1 do
      begin
        ma := MarkedArticle [i];
        f.Write(ma.Account + #9);
        f.Write(ma.Group + #9);
        f.Write(ma.MessageId + #9);
        f.Write(ma.Subject + #9);
        f.Write(ma.From + #9);
        f.Write(IntToStr (DateTimeToFileDate (ma.Date))+ #9);
        f.Write(IntToStr (ma.Lines)+#9);
        f.Write(IntToStr (ma.CodePage)+#9);
        f.WriteLn(IntToStr (DateTimeToFileDate (ma.BookmarkedDate)))
      end
  finally
    f.Free
  end;
  fDirty := False;
  fLoaded := True
end;

{ TBookmarkSet }

constructor TBookmarkSet.Create;
begin
  fNames := TStringList.Create;
end;

function TBookmarkSet.CreateBookmark(const name: string): TBookmark;
var
  i : Integer;
begin
  for i := 0 to fNames.Count - 1 do
    if CompareText (fNames [i], name) = 0 then
    begin
      result := TBookmark.Create(fNames [i]);
      Exit
    end;
  result := TBookmark.Create(Name);
  result.Dirty := True;
  result.Save;
  fNames.Add(name)
end;

procedure TBookmarkSet.DeleteAllBookmarkSets;
begin
  while fNAmes.Count > 0 do
    DeleteBookmarkSet (fNames [0])
end;

procedure TBookmarkSet.DeleteBookmarkSet(const name: string);
var
  idx : Integer;
begin
  idx := fNames.IndexOf(name);
  if idx >= 0 then
  begin
    DeleteFile (gMessageBaseRoot + '\' + name + '.bmk');
    fNames.Delete(idx)
  end
end;

destructor TBookmarkSet.Destroy;
begin
  fNames.Free;

  inherited;
end;

procedure TBookmarkSet.Enumerate;
var
  f : TSearchRec;
  st : string;
begin
  if fEnumerated then Exit;

  if FindFirst (gMessageBaseRoot + '\*.bmk', faAnyFile, f) = 0 then
  try
    repeat
      st := f.Name;
      fNames.Add(SplitString ('.', st))
    until FindNext (f) <> 0
  finally
    FindClose (f)
  end;
  fEnumerated := True
end;

function TBookmarkSet.GetBookmarkCount: Integer;
begin
  Enumerate;
  result := fNames.Count
end;

function TBookmarkSet.GetBookmarkName(idx: Integer): string;
begin
  Enumerate;
  result := fNames [idx]
end;

function TBookmarkSet.GetUniqueName: string;
var
  i, bmn, n : Integer;
  st : string;
begin
  bmn := -1;

  for i := 0 to BookmarkCount - 1 do
  begin
    st := BookmarkName [i];
    if Pos ('Bookmark ', st) = 1 then
    begin
      st := Copy (st, 9, MaxInt);
      n := StrToIntDef (st, -1);
      if n > bmn then
        bmn := n
    end
  end;
  result := 'Bookmark ' + IntToStr (bmn + 1)
end;

end.
