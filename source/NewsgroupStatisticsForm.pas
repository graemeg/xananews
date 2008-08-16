(*======================================================================*
 | NewsgroupStatisticsForm                                              |
 |                                                                      |
 | Calculate newsgroup statistics                                       |
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
 | Copyright © Colin Wilson 2003  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      01/04/2004  CPWW  Original                                  |
 *======================================================================*)


unit NewsgroupStatisticsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, NewsGlobals, ConTnrs, unitNNTPServices, unitSettings, unitMessages,
  cmpPersistentPosition, unitExSettings;

type
  TStatistic = class
  private
    fNumber: Integer;
    fRanking: Integer;
    fDataString: string;
    fDataInteger : Integer;
  public
    property Number : Integer read fNumber;
    property Ranking : Integer read fRanking;
    property DataString : string read fDataString;
    property DataInteger : Integer read fDataInteger;
  end;

  TStatistics = class
  private
    fGroup : TArticleContainer;
    fPosters : TStringList;
    fReaders : TStringList;
    fThreads : TStringList;
    fNoUnanswered : Integer;
    fNoXanaNews : Integer;
    fNonDummyArticleCount: Integer;
  public
    constructor Create (AGroup : TArticleContainer);
    destructor Destroy; override;

    property NonDummyArticleCount : Integer read fNonDummyArticleCount;
  end;

  TStatisticContainer = class (TArticleObjectContainer)
  private
    fMessagebaseSize : Int64;
  protected
    function GetServerSettings : TServerSettings; override;
    function GetNext : TArticleContainer; override;
    function GetUnreadArticleCount: Integer; override;
    function GetMessagebaseSize: Int64; override;
  public
    constructor Create (AGroup : TArticleContainer; startDate, endDate : TDateTime);
    procedure LoadArticles; override;
    procedure SaveArticles (recreateMessageFile : boolean); override;
  end;

  TStatisticArticle = class (TArticleBase)
  private
    fAgent : string;
  protected
    function GetHeader(const name: string): string; override;
    function GetMsg: TmvMessage; override;
  public
    constructor Create (AOwner : TArticleContainer); override;
    procedure Assign (article : TArticleBase); override;
  end;

  TfmNewsgroupStatistics = class(TForm)
    btnClose: TButton;
    pcSelect: TPageControl;
    tsThreads: TTabSheet;
    tsPosters: TTabSheet;
    tsNewsreaders: TTabSheet;
    lvThreads: TListView;
    lvPosters: TListView;
    lvNewsreaders: TListView;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    stNoArticles: TLabel;
    stNoThreads: TLabel;
    stNoUnanswered: TLabel;
    stMessagebaseSize: TLabel;
    Label5: TLabel;
    btnCopyToClipboard: TButton;
    Bevel1: TBevel;
    btnStart: TButton;
    dtpFrom: TDateTimePicker;
    Label6: TLabel;
    Label7: TLabel;
    dtpTo: TDateTimePicker;
    PersistentPosition1: TPersistentPosition;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvNewsreadersData(Sender: TObject; Item: TListItem);
    procedure lvPostersData(Sender: TObject; Item: TListItem);
    procedure lvPostersColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvNewsreadersColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure lvThreadsData(Sender: TObject; Item: TListItem);
    procedure lvThreadsColumnClick(Sender: TObject; Column: TListColumn);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure PersistentPosition1GetSettingsClass(Owner: TObject;
      var SettingsClass: TExSettingsClass);
    procedure PersistentPosition1GetSettingsFile(Owner: TObject;
      var fileName: string);
  private
    fStatistics : TStatistics;
    fStatisticContainer : TStatisticContainer;
    fGroup: TSubscribedGroup;

    fPostersReverseSort : boolean;
    fPostersSortColumn : Integer;

    fReadersReverseSort : boolean;
    fReadersSortColumn : Integer;

    fThreadsReverseSort : boolean;
    fCalculating : boolean;
    fThreadsSortColumn : Integer;

    procedure WmSetup (var msg : TMessage); message WM_SETUP;
  protected
    procedure UpdateActions; override;
  public
    property Group : TSubscribedGroup read fGroup write fGroup;
  end;

var
  fmNewsgroupStatistics: TfmNewsgroupStatistics;

implementation

uses ClipBrd;

var
  gReverseSort : boolean = False;

{$R *.dfm}


function ComparePosters (list : TStringList; idx1, idx2 : Integer) : Integer;
var
  i1, i2 : Integer;
begin
  i1 := Integer (list.Objects [idx1]);
  i2 := Integer (list.Objects [idx2]);
  result := i2 - i1;
  if gReverseSort then
    result := -result;
end;


function CompareStatisticDataIntegers (list : TStringList; idx1, idx2 : Integer) : Integer;
var
  stat1, stat2 : TStatistic;
begin
  stat1 := TStatistic (list.Objects [idx1]);
  stat2 := TStatistic (list.Objects [idx2]);
  result := stat1.DataInteger - stat2.DataInteger;
  if gReverseSort then
    result := -result;
end;

function CompareStatisticNumbers (list : TStringList; idx1, idx2 : Integer) : Integer;
var
  stat1, stat2 : TStatistic;
begin
  stat1 := TStatistic (list.Objects [idx1]);
  stat2 := TStatistic (list.Objects [idx2]);
  result := stat1.Number - stat2.Number;
  if gReverseSort then
    result := -result;
end;

function CompareStatisticRankings (list : TStringList; idx1, idx2 : Integer) : Integer;
var
  stat1, stat2 : TStatistic;
begin
  stat1 := TStatistic (list.Objects [idx1]);
  stat2 := TStatistic (list.Objects [idx2]);
  result := stat1.Ranking - stat2.Ranking;
  if gReverseSort then
    result := -result;
end;

function CompareStatisticStrings (list : TStringList; idx1, idx2 : Integer) : Integer;
begin
  result := CompareText (list [idx1], list [idx2]);
  if gReverseSort then
    result := -result;
end;

function CompareStatisticDataStrings(list : TStringList; idx1, idx2 : Integer) : Integer;
var
  stat1, stat2 : TStatistic;
begin
  stat1 := TStatistic (list.Objects [idx1]);
  stat2 := TStatistic (list.Objects [idx2]);
  result := CompareText (stat1.DataString, stat2.DataString);
  if gReverseSort then
    result := -result;
end;

{ TfmNewsgroupStatistics }

procedure TfmNewsgroupStatistics.btnCopyToClipboardClick(Sender: TObject);
var
  report : TStringList;
  i : Integer;
  stat : TStatistic;
begin
  report := TStringList.Create;
  try
    report.Add('XanaNews Statistic for ' + fStatisticContainer.Name + '.  ' + DateTimeToStr (Now));
    report.Add ('');

    if fStatisticContainer.ArticleCount > 0 then
    begin
      i := fStatisticContainer.ArticleCount - 1;
      while (i > 0) and (fStatisticContainer.ArticleBase [i].ArticleNo = 0) do
        Dec (i);
      report.Add (Format ('From article %d (%s) to article %d (%s)', [
        fStatisticContainer.ArticleBase [0].ArticleNo,
        DateTimeToStr (fStatisticContainer.ArticleBase [0].Date),
        fStatisticContainer.ArticleBase [i].ArticleNo,
        DateTimeToStr (fStatisticContainer.ArticleBase [i].Date)]));
      report.Add ('');
    end;

    report.Add ('Number of threads  ................... ' + IntToStr (fStatisticContainer.ThreadCount));
    report.Add ('Number of articles  .................. ' + IntToStr (fStatisticContainer.ArticleCount));
    report.Add ('Average articles per thread  ......... ' + Format ('%0.2f', [fStatisticContainer.ArticleCount / fStatisticContainer.ThreadCount]));
    report.Add ('Number of unanswered posts  .......... ' + IntToStr (fStatistics.fNoUnanswered));
    report.Add ('Number of posts from XanaNews users .. ' + IntToStr (fStatistics.fNoXanaNews));
    report.Add ('');
    report.Add ('');
    report.Add ('Top Threads');
    report.Add ('');
    report.Add ('Ranking  Articles  Subject');
    report.Add ('-------  --------  ----------------------------------');

    for i := 0 to fStatistics.fThreads.Count - 1 do
    begin
      stat := TStatistic (fStatistics.fThreads.Objects [i]);

      if stat.Number > 1 then
        report.Add (Format ('%7d  %8d  %s', [stat.Ranking, stat.Number, fStatistics.fThreads [i]]));
    end;
    report.Add ('');
    report.Add ('');

    report.Add ('Top Posters');
    report.Add ('');
    report.Add ('Ranking  Articles  Name');
    report.Add ('-------  --------  ----------------------------------');

    for i := 0 to fStatistics.fPosters.Count - 1 do
    begin
      stat := TStatistic (fStatistics.fPosters.Objects [i]);

      report.Add (Format ('%7d  %8d  %s', [stat.Ranking, stat.Number, fStatistics.fPosters [i]]));
    end;
    report.Add ('');
    report.Add ('');

    report.Add ('Top Newsreaders');
    report.Add ('');
    report.Add ('Ranking  Articles  Newsreader                                          Users');
    report.Add ('-------  --------  --------------------------------------------------  -----');

    for i := 0 to fStatistics.fReaders.Count - 1 do
    begin
      stat := TStatistic (fStatistics.fReaders.Objects [i]);

      report.Add(Format('%7d  %8d  %-50.50s  %5s', [stat.Ranking, stat.Number, fStatistics.fReaders[i], IntToStr(stat.DataInteger)]));
    end;

    Clipboard.AsText := report.Text
  finally
    report.Free;
  end
end;

procedure TfmNewsgroupStatistics.btnStartClick(Sender: TObject);
begin
  FreeAndNil (fStatistics);
  FreeAndNil (fStatisticContainer);
  PostMessage (handle, WM_SETUP, 0, 0);
end;

procedure TfmNewsgroupStatistics.FormDestroy(Sender: TObject);
begin
  fStatistics.Free;
  fStatisticContainer.Free;
end;

procedure TfmNewsgroupStatistics.FormShow(Sender: TObject);
var
  i : Integer;
  art : TArticle;
  sd, ed : TDateTime;
begin
  AdjustFormConstraints (self);
  Caption := 'Newsgroup Statistics for ' + fGroup.Name;

  sd := 0;
  ed := 0;

  for i := 0 to fGroup.ArticleCount - 1 do
  begin
    art := fGroup.Articles [i];

    if sd = 0 then
      sd := art.Date
    else
      if art.Date < sd then
        sd := art.Date;

    if ed = 0 then
      ed := art.Date
    else
      if art.Date > ed then
        ed := art.Date
  end;

  dtpFrom.DateTime := Int(sd);
  dtpTo.DateTime := ed;
end;


procedure TfmNewsgroupStatistics.lvNewsreadersColumnClick(Sender: TObject;
  Column: TListColumn);
var
  col : Integer;
begin
  col := Column.Index;

  if col = fReadersSortColumn then
    fReadersReverseSort := not fReadersReverseSort
  else
    fReadersReverseSort := False;

  gReverseSort := fReadersReverseSort;

  case col of
    0 : fStatistics.fReaders.CustomSort(CompareStatisticRankings);
    1 : fStatistics.fReaders.CustomSort(CompareStatisticNumbers);
    2 : fStatistics.fReaders.CustomSort(CompareStatisticStrings);
    3 : fStatistics.fReaders.CustomSort(CompareStatisticDataIntegers);
  end;

  fReadersSortColumn := Col;
  lvNewsreaders.Invalidate
end;

procedure TfmNewsgroupStatistics.lvNewsreadersData(Sender: TObject;
  Item: TListItem);
var
  idx : Integer;
  statistic : TStatistic;
begin
  idx := Item.Index;
  if idx < fStatistics.fReaders.Count then
  begin
    statistic := TStatistic (fStatistics.fReaders.Objects [idx]);

    Item.Caption := IntToStr (statistic.Ranking);
    Item.SubItems.Add (IntToStr (statistic.Number));
    Item.SubItems.Add (fStatistics.fReaders [idx]);
    Item.SubItems.Add (IntToStr (statistic.DataInteger));
  end
end;

procedure TfmNewsgroupStatistics.lvPostersColumnClick(Sender: TObject;
  Column: TListColumn);
var
  col : Integer;
begin
  col := Column.Index;

  if col = fPostersSortColumn then
    fPostersReverseSort := not fPostersReverseSort
  else
    fPostersReverseSort := False;

  gReverseSort := fPostersReverseSort;

  case col of
    0 : fStatistics.fPosters.CustomSort(CompareStatisticRankings);
    1 : fStatistics.fPosters.CustomSort(CompareStatisticNumbers);
    2 : fStatistics.fPosters.CustomSort(CompareStatisticStrings);
    3 : fStatistics.fPosters.CustomSort(CompareStatisticDataStrings);
  end;

  fPostersSortColumn := Col;
  lvPosters.Invalidate
end;

procedure TfmNewsgroupStatistics.lvPostersData(Sender: TObject;
  Item: TListItem);
var
  idx : Integer;
  statistic : TStatistic;
begin
  idx := Item.Index;
  if idx < fStatistics.fPosters.Count then
  begin
    statistic := TStatistic (fStatistics.fPosters.Objects [idx]);

    Item.Caption := IntToStr (statistic.Ranking);
    Item.SubItems.Add (IntToStr (statistic.Number));
    Item.SubItems.Add (fStatistics.fPosters [idx]);
    Item.SubItems.Add (statistic.DataString)
  end
end;

procedure TfmNewsgroupStatistics.lvThreadsColumnClick(Sender: TObject;
  Column: TListColumn);
var
  col : Integer;
begin
  col := Column.Index;

  if col = fThreadsSortColumn then
    fThreadsReverseSort := not fThreadsReverseSort
  else
    fThreadsReverseSort := False;

  gReverseSort := fThreadsReverseSort;

  case col of
    0 : fStatistics.fThreads.CustomSort(CompareStatisticRankings);
    1 : fStatistics.fThreads.CustomSort(CompareStatisticNumbers);
    2 : fStatistics.fThreads.CustomSort(CompareStatisticStrings);
    3 : fStatistics.fThreads.CustomSort(CompareStatisticDataIntegers);
  end;

  fThreadsSortColumn := Col;
  lvThreads.Invalidate
end;


procedure TfmNewsgroupStatistics.lvThreadsData(Sender: TObject;
  Item: TListItem);
var
  idx : Integer;
  statistic : TStatistic;
begin
  idx := Item.Index;
  if idx < fStatistics.fThreads.Count then
  begin
    statistic := TStatistic (fStatistics.fThreads.Objects [idx]);

    Item.Caption := IntToStr (statistic.Ranking);
    Item.SubItems.Add (IntToStr (statistic.Number));
    Item.SubItems.Add (fStatistics.fThreads[idx]);
  end
end;

procedure TfmNewsgroupStatistics.PersistentPosition1GetSettingsClass(
  Owner: TObject; var SettingsClass: TExSettingsClass);
begin
  SettingsClass := gExSettingsClass;
end;

procedure TfmNewsgroupStatistics.PersistentPosition1GetSettingsFile(
  Owner: TObject; var fileName: string);
begin
 fileName := gExSettingsFile
end;

procedure TfmNewsgroupStatistics.UpdateActions;
begin
  btnCopyToClipboard.Enabled := Assigned (fStatistics) and not fCalculating;
end;

procedure TfmNewsgroupStatistics.WmSetup(var msg: TMessage);
var
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  try
    fCalculating := True;
    lvNewsReaders.Items.Count := 0;
    lvPosters.Items.Count := 0;
    lvThreads.Items.Count := 0;
    lvNewsReaders.Invalidate;
    lvPosters.Invalidate;
    lvThreads.Invalidate;
    Screen.Cursor := crHourGlass;
    UpdateActions;
    Application.ProcessMessages;
    fStatisticContainer := TStatisticContainer.Create(fGroup, dtpFrom.DateTime, dtpTo.DateTime);
    fStatistics := TStatistics.Create(fStatisticContainer);

    stNoThreads.Caption := IntToStr (fStatisticContainer.ThreadCount);
    stNoArticles.Caption := IntToStr (fStatistics.NonDummyArticleCount);
    stNoUnanswered.Caption := IntToStr (fStatistics.fNoUnanswered);
    stMessagebaseSize.Caption := IntToStr (fStatisticContainer.MessagebaseSize div 1024);

    fPostersReverseSort := True;
    fPostersSortColumn := 0;

    fReadersReverseSort := True;
    fReadersSortColumn := 0;

    fThreadsReverseSort := True;
    fThreadsSortColumn := 0;

    lvNewsReaders.Items.Count := fStatistics.fReaders.Count;
    lvPosters.Items.Count := fStatistics.fPosters.Count;
    lvThreads.Items.Count := fStatistics.fThreads.Count;
  finally
    Screen.Cursor := oldCursor;
    fCalculating := False;
  end
end;

{ TStatisticArticle }

procedure TStatisticArticle.Assign(article: TArticleBase);
begin
  inherited;

  self.fFlags := article.Flags;
  self.fArticleNo := article.ArticleNo;
  self.fAgent := article.Header ['X-NewsReader'];
  if self.fAgent = '' then
    self.fAgent := article.Header ['User-Agent'];
end;

constructor TStatisticArticle.Create (AOwner : TArticleContainer);
begin
  inherited;
  fCodePage := CP_ACP;
end;

function TStatisticArticle.GetHeader(const name: string): string;
begin
  if SameText (name, 'User-Agent') or SameText (name, 'X-NewsReader') then
    result := fAgent
  else
    result := ''
end;

function TStatisticArticle.GetMsg: TmvMessage;
begin
  result := Nil
end;

{ TStatisticContainer }

constructor TStatisticContainer.Create(AGroup: TArticleContainer;
  startDate, endDate: TDateTime);
var
  i : Integer;
  art, nart : TArticleBase;
  d : TDateTime;
begin
  inherited Create ('', Nil, Nil);
  Name := AGroup.Name;
  ThreadSortOrder := soDate;
  ThreadOrder := toChronological;

  endDate := Int(endDate) + 1;

  fMessagebaseSize := AGroup.MessagebaseSize;

  for i := 0 to AGroup.ArticleCount - 1 do
  begin
    art := AGroup.ArticleBase [i];
    d := art.Date;
    if (d >= startDate) and (d < endDate) then
    begin
      nart := TStatisticArticle.Create(self);
      nart.Assign(art);
      RawAddArticle (nart)
    end
  end;

  fUnreadArticleCount := -1;
  fUnloadedArticleCount := -1;
  fArticlesLoaded := True;
  GetUnreadArticleCount;
  ThreadOrder := toThreaded;
end;

function TStatisticContainer.GetMessagebaseSize: Int64;
begin
  result := fMessagebaseSize;
end;

function TStatisticContainer.GetNext: TArticleContainer;
begin
  result := Nil
end;

function TStatisticContainer.GetServerSettings: TServerSettings;
begin
  result := Nil
end;

function TStatisticContainer.GetUnreadArticleCount: Integer;
var
  i : Integer;
begin
  result := 0;

  if fUnreadArticleCount = -1 then
  begin
    fUnreadArticleToMeCount := 0;
    fUnreadXananewsArticleCount := 0;
    fUnloadedArticleCount := 0;
    fUnreadReplyCount := 0;
    fUnloadedArticleCount := ArticleCount;
    for i := 0 to ArticleCount - 1 do
      if not ArticleBase [i].IsRead then
      begin
        Inc (result);
        if ArticleBase [i].IsMine then
          Inc (fUnreadArticleToMeCount);
        if ArticleBase [i].IsReply then
          Inc (fUnreadReplyCount);
        if ArticleBase [i].IsXanaNews then
          Inc (fUnreadXananewsArticleCount)
      end;
    fUnreadArticleCount := result
  end
  else
    result := fUnreadArticleCount
end;

procedure TStatisticContainer.LoadArticles;
begin
end;

procedure TStatisticContainer.SaveArticles(recreateMessageFile: boolean);
begin
end;

{ TStatistics }

constructor TStatistics.Create(AGroup: TArticleContainer);
var
  i, idx : Integer;
  article : TArticleBase;
  bart : TArticleBase;
  agent, poster : string;
  stat : TStatistic;

  function GetAgent (article : TArticleBase) : string;
  var
    p, idx : Integer;
  begin
    result := article.Header ['X-Newsreader'];
    if result = '' then
      result := article.Header ['User-Agent'];

    if result <> '' then
    begin
      p := Pos ('/', result);
      if p > 0 then
        result := Copy (result, 1, p - 1);
      p := 0;
      for idx := 3 to Length (result) do       // Start at 3 so that 40tude doesn't feel left out.
        if result [idx] in ['0'..'9'] then
        begin
          p := idx;
          break
        end;
      if p > 0 then
        result := Trim (Copy (result, 1, p - 1));

      if (Length (result) > 2) and SameText (Copy (result, Length (result) - 1, MaxInt), ' v') then
        result := Copy (result, 1, Length (result) - 2);
    end
  end;

  function GetThreadHeight (article : TArticleBase) : Integer;
  var
     p : TArticleBase;
  begin
    result := 1;
    p := Article.Child;
    while Assigned (p) do
    begin
      Inc (result, GetThreadHeight (p));
      p := p.sibling
    end
  end;

begin
  fGroup := AGroup;

  fPosters := TStringList.Create;  fPosters.CaseSensitive := False; fPosters.Duplicates := dupError; fPosters.Sorted := True;
  fReaders := TStringList.Create;  fReaders.CaseSensitive := False; fReaders.Duplicates := dupError; fReaders.Sorted := True;
  fThreads := TStringList.Create;  fThreads.CaseSensitive := False; fThreads.Duplicates := dupAccept; fThreads.Sorted := True;

  for i := 0 to fGroup.ArticleCount - 1 do
  begin
    article := fGroup.ArticleBase [i];
    if article.ArticleNo <> 0 then
      Inc (fNonDummyArticleCount);

    agent := GetAgent (article);
    if SameText (agent, 'XanaNews') then
      Inc (fNoXanaNews);

    if agent <> '' then
    begin
      idx := fReaders.IndexOf(agent);
      if idx = -1 then
      begin
        stat := TStatistic.Create;
        fReaders.AddObject(agent, stat)
      end
      else
        stat := TStatistic (fReaders.Objects [idx]);
      Inc (stat.fNumber)
    end;

    poster := article.FromName;

    if poster <> '' then
    begin
      idx := fPosters.IndexOf(poster);
      if idx = -1 then
      begin
        stat := TStatistic.Create;
        fPosters.AddObject(poster, stat)
      end
      else
        stat := TStatistic (fPosters.Objects [idx]);
      Inc (stat.fNumber);
      if agent <> '' then
        stat.fDataString := agent
    end
  end;

  fPosters.Sorted := False;
  gReverseSort := True;
  fPosters.CustomSort(CompareStatisticNumbers);
  for i := 0 to fPosters.Count - 1 do
  begin
    stat := TStatistic (fPosters.Objects [i]);
    stat.fRanking := i + 1;
    if stat.fDataString <> '' then
    begin
      idx := fReaders.IndexOf(stat.fDataString);
      if idx >= 0 then
      begin
        stat := TStatistic (fReaders.Objects [idx]);
        Inc (stat.fDataInteger)
      end
    end
  end;

  fReaders.Sorted := False;
  fReaders.CustomSort(CompareStatisticNumbers);
  for idx := 0 to fReaders.Count - 1 do
    TStatistic (fReaders.Objects [idx]).fRanking := idx + 1;

  for i := 0 to fGroup.ThreadCount - 1 do
  begin
    bart := fGroup.Threads [i];

    if (bart.Child = Nil) and (bart.Header ['References'] = '') then
      Inc (fNoUnanswered);

    stat := TStatistic.Create;
    stat.fNumber := GetThreadHeight (bart);
    fThreads.AddObject(DecodeSubject(bart.Subject, bart.CodePage), stat);
  end;

  fThreads.Sorted := False;
  fThreads.CustomSort(CompareStatisticNumbers);
  for idx := 0 to fThreads.Count - 1 do
    TStatistic (fThreads.Objects [idx]).fRanking := idx + 1;
end;

destructor TStatistics.Destroy;
var
  i : Integer;
begin
  for i := 0 to fReaders.Count - 1 do
    fReaders.Objects [i].Free;
  for i := 0 to fPosters.Count - 1 do
    fPosters.Objects [i].Free;
  for i := 0 to fThreads.Count - 1 do
    fThreads.Objects [i].Free;
  inherited;
end;

end.
