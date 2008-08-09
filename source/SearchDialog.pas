(*======================================================================*
 | SearchDialog unit for NewsReader3                                    |
 |                                                                      |
 | Display 'Search Messages' dialog                                     |
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
 | 1.0      29/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit SearchDialog;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, Controls, StdCtrls,
  Buttons, ExtCtrls, unitNNTPFilters, unitNNTPServices, unitBookmarks,
  cmpUCtrls, cmpPersistentPosition, ConTnrs, ComCtrls, unitExSettings;

type
  TSearchableColumns = ftSubject..ftCrossposted;
  TOnArticleFound = procedure (article : TArticleBase; bookmark : boolean; var continue : boolean) of object;
  TdlgSearch = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    cbUnreadMessagesOnly: TCheckBox;
    cbSearchToBookmark: TCheckBox;
    PersistentPosition1: TPersistentPosition;
    cbCaseSensitive: TCheckBox;
    Label1: TLabel;
    edMaxResults: TEdit;
    cbSubject: TCheckBox;
    cbCrossposted: TCheckBox;
    cbHeaderLines: TCheckBox;
    cbMessageID: TCheckBox;
    cbMessageBody: TCheckBox;
    cbLines: TCheckBox;
    cbDate: TCheckBox;
    cbAuthor: TCheckBox;
    cbxSubject: TComboBox;
    cbxAuthor: TComboBox;
    cbxDate: TComboBox;
    cbxLines: TComboBox;
    cbxMessageBody: TComboBox;
    cbxMessageID: TComboBox;
    cbxHeaderLines: TComboBox;
    cbxCrossposted: TComboBox;
    Bevel1: TBevel;
    DateTimePicker1: TDateTimePicker;
    edSubject: TuEdit;
    edAuthor: TuEdit;
    edLines: TuEdit;
    edMessageBody: TuEdit;
    edMessageID: TuEdit;
    edHeaderLines: TuEdit;
    edCrossposted: TuEdit;
    Timer1: TTimer;
    cbInterestingMessagesOnly: TCheckBox;
    procedure PersistentPosition1GetSettingsFile(Owner: TObject;
      var fileName: string);
    procedure PersistentPosition1GetSettingsClass(Owner: TObject;
      var SettingsClass: TExSettingsClass);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure cbSubjectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxSubjectChange(Sender: TObject);
    procedure edAuthorChange(Sender: TObject);
    procedure DateTimePicker1Change(Sender: TObject);
  private
    fFilters : TNNTPFilters;
    fArticle: TArticleBase;
    fOnArticleFound: TOnArticleFound;
    fCodePage: Integer;
    fGroups: TObjectList;
    fLoading :boolean;
    fCont : boolean;
    fSearchToBookmark : boolean;
    function GetFilters: TNNTPFilters;
    function GetOperators(column : TNNTPFilterColumn): TComboBox;
    function GetEditors(column: TNNTPFilterColumn): TuEdit;
    function GetEditorCtrls(column: TNNTPFilterColumn): TWinControl;
    function GetEnableds(column: TNNTPFilterColumn): TCheckBox;
  protected
    procedure UpdateActions; override;
    procedure CreateParams (var params : TCreateParams); override;
    procedure LoadPreviousSettings;
    procedure SavePreviousSettings;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    property Filters : TNNTPFilters read GetFilters;
    property Groups : TObjectList read fGroups write fGroups;
    property Article : TArticleBase read fArticle write fArticle;

    property OnArticleFound : TOnArticleFound read fOnArticleFound write fOnArticleFound;

    property Operators [column : TNNTPFilterColumn] : TComboBox read GetOperators;
    property Editors [column : TNNTPFilterColumn] : TuEdit read GetEditors;
    property EditorCtrls [column : TNNTPFilterColumn] : TWinControl read GetEditorCtrls;
    property Enableds [column : TNNTPFilterColumn] : TCheckBox read GetEnableds;
  end;

implementation

uses unitNewsReaderOptions, NewsGlobals, unitCharsetMap, unitSearchString, unitMessageBaseSearch;

{$R *.dfm}

{ TdlgSearch }

destructor TdlgSearch.Destroy;
var
  i : Integer;
begin
  FreeAndNil (fFilters);
  for i := 0 to Groups.Count - 1 do
    TArticleContainer (Groups [i]).fSearching := False;

  fGroups.Free;

  inherited;
end;

function TdlgSearch.GetFilters: TNNTPFilters;
var
  operator : TNNTPFilterOperator;
  unread, caseSensitive, interesting: boolean;
  col : TNNTPFilterColumn;
  en : TCheckBox;
  op : TComboBox;
  ed : TuEdit;
begin
  if Assigned (fFilters) then
  begin
    Result := fFilters;
    exit
  end;

  fFilters := TNNTPFilters.Create (true);

  unread := cbUnreadMessagesOnly.Checked;
  caseSensitive := cbCaseSensitive.Checked;
  interesting := cbInterestingMessagesOnly.Checked;

  for col := Low (TNNTPFilterColumn) to High (TNNTPFilterColumn) do
  begin
    en := Enableds [col];
    if not Assigned (en) or not en.Checked then
      Continue;

    op := Operators [col];
    ed := Editors [col];

    getOperator (op.Text, operator);

    case col of
      ftDate :
        fFilters.AddObject('', TNNTPFilter.Create('', col, operator, DateTimePicker1.DateTime, unread, interesting, caseSensitive));
      ftLines,
      ftCrossposted,
      ftNumber :
        fFilters.AddObject('', TNNTPFilter.Create('', col, operator, StrToIntDef (ed.Text, 0), unread, interesting, caseSensitive));

      else
        fFilters.AddObject ('', TNNTPFilter.Create('', col, operator, ed.WideText, unread, interesting, caseSensitive))
    end
  end;
  result := fFilters
end;

procedure TdlgSearch.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TdlgSearch.CancelBtnClick(Sender: TObject);
begin
  if fCont and fSearchToBookmark then
  begin
    fCont := False;
    Timer1.Enabled := True
  end
  else
    Close
end;

procedure TdlgSearch.OKBtnClick(Sender: TObject);
var
  max : Integer;
  cont : boolean;
  found : boolean;
begin
  if not Assigned (Filters) then Exit;

  max := StrToIntDef (edMaxResults.Text, 200);
  if max <= 0 then max := MaxInt;

  SavePreviousSettings;

  cont := True;
  found := False;
  fSearchToBookmark := cbSearchToBookmark.Checked;

  while cont do
  begin
    article := unitMessageBaseSearch.Search(article, fGroups, Filters);

    if Assigned (article) then
    begin
      found := True;

      if Assigned (OnArticleFound) then
        OnArticleFound (article, fSearchToBookmark, cont);

      if fSearchToBookmark then
      begin
        Dec (max);
        if max < 0 then
          cont := False  // 'Max' articles added to bookmark.  End
      end
      else
        cont := False    // Article found and not searching to bookmark. End.
    end
    else
    begin
      if not found then
        ShowMessage ('Could not find a matching article');
      cont := False      // Article not found.  End
    end
  end
end;

procedure TdlgSearch.PersistentPosition1GetSettingsClass(Owner: TObject;
  var SettingsClass: TExSettingsClass);
begin
  SettingsClass := gExSettingsClass;
end;

procedure TdlgSearch.PersistentPosition1GetSettingsFile(Owner: TObject;
  var fileName: string);
begin
  fileName := gExSettingsFile;
end;

(*
procedure TdlgSearch.OKBtnClick(Sender: TObject);
var
  found : boolean;
  oldCursor : TCursor;
  bmk : TBookmark;
  ct : Integer;
  frDate : string;
  max : Integer;
begin
  if not Assigned (Filters) then Exit;
  max := StrToIntDef (edMaxResults.Text, 200);
  if max <= 0 then
    max := MaxInt;

  if fArticleContainer = nil then
    if fGroups.Count > 0 then
      fArticleContainer := TSubscribedGroup (fGroups [0]);

  if article = nil then
  begin
    if fArticleContainer.ArticleCount > 0 then
    begin
      if fArticleContainer.ThreadOrder = toThreaded then
        article := fArticleContainer.Threads [0]
      else
        article := fArticleContainer.ArticleBase [0];
    end
  end
  else
  begin
    fArticleContainer := TSubscribedGroup (article.Owner);
    article := article.Next;
    if article = Nil then
    begin
      ct := fGroups.IndexOf(fArticleContainer);
      if ct < fGroups.Count - 1 then
        while ct + 1 < fGroups.Count do
        begin
          ct := ct + 1;
          fArticleContainer := TSubscribedGroup (fGroups [ct]);
          if fArticleContainer.ArticleCount > 0 then
            if fArticleContainer.ThreadOrder = toThreaded then
              article := fArticleContainer.Threads [0]
            else
              article := fArticleContainer.ArticleBase [0];
          if article <> Nil then
            break
        end
      else
       fArticleContainer := Nil;
    end
  end;

  SavePreviousSettings;

  ct := 0;
  found := False;
  oldCursor := Screen.Cursor;
  screen.Cursor := crAppStart;
  Application.ProcessMessages;
  try
    fCont := True;
    fSearchToBookmark := cbSearchToBookmark.Checked;
    while Assigned (article) do
    begin
      if Filters.MatchesAll(article) then
      begin
        if fSearchToBookmark then
        begin
          frDate := StringReplace (DateToStr (Now),'/', '-', [rfReplaceAll]);
          frDate := StringReplace (frDate, '.', '-', [rfReplaceAll]);

          if not Assigned (fBookmark) then
          begin
            fBookmark := fmMain.fBookmarkSet.CreateBookmark('Search Results ' + frDate);
            fBookmark.Clear;
            fmMain.PopulateBookmarkCombo;
            fmMain.SetCurrentBookmark(fBookmark)
          end;
          bmk := fmMain.fCurrentBookmark;
          if bmk = nil then
          begin
            bmk := fmMain.fBookmarkSet.CreateBookmark('Search Results ' + frDate);
            fBookmark.Clear;
            fmMain.PopulateBookmarkCombo;
            fmMain.SetCurrentBookmark(fBookmark)
          end
        end
        else
          bmk := Nil;

        if Assigned (OnArticleFound) then
          OnArticleFound (article, bmk, fCont);

        Inc (ct);

        found := True;
      end;
      article := article.Next;
      if article = Nil then
      begin
        ct := fGroups.IndexOf(fArticleContainer);
        if ct < fGroups.Count -1 then
          while ct + 1 < fGroups.Count do
          begin
            ct := ct + 1;
            fArticleContainer := TSubscribedGroup (fGroups [ct]);
            if fArticleContainer.ArticleCount > 0 then
              if fArticleContainer.ThreadOrder = toThreaded then
                article := fArticleContainer.Threads [0]
              else
                article := fArticleContainer.ArticleBase [0];
            if article <> Nil then
              break
          end
        else
          fArticleContainer := Nil;
      end;
      Application.ProcessMessages;
      if (not fCont) or (ct > max) then
        break;
    end;
  finally
    fSearchToBookmark := False;
    screen.Cursor := oldCursor
  end;

  if fCont and not Found then
    ShowMessage ('Could not find a matching article')
end;
*)

procedure TdlgSearch.cbSubjectClick(Sender: TObject);
var
  col : TNNTPFilterColumn;
  ctrl : TCheckBox;
  op : TComboBox;
  ed : TWinControl;
begin
  if not (sender is TCheckBox) then Exit;
  if not fLoading then
    FreeAndNil (fFilters);
  ctrl := TCheckBox (Sender);
  col := TNNTPFilterColumn (ctrl.Tag);

  op := Operators [col];
  ed := EditorCtrls [col];

  op.Enabled := ctrl.Checked;
  if ctrl.Checked then
    op.Font.Color := clBlack
  else
    op.Font.Color := clGrayText;
  ed.Enabled := ctrl.Checked
end;

procedure TdlgSearch.UpdateActions;
var
  somethingSelected : Boolean;
begin
  somethingSelected := cbSubject.Checked or cbAuthor.Checked or cbLines.Checked or cbDate.Checked or cbMessageBody.Checked or cbMessageID.Checked or cbHeaderLines.Checked or cbCrossposted.Checked;

//  somethingSelected := somethingSelected and (cbSearchString.Text <> '');

  OKBtn.Enabled := somethingSelected
end;

procedure TdlgSearch.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams(params);
  params.ExStyle   := params.ExStyle or WS_EX_APPWINDOW;
  params.WndParent := Application.Handle;
end;

procedure TdlgSearch.FormShow(Sender: TObject);
var
  i : Integer;
  col : TNNTPFilterColumn;
  op : TNNTPFilterOperator;
  ed : TuEdit;
  cb : TComboBox;
  en : TCheckBox;
begin
  AdjustFormConstraints (self);
  for i := 0 to Groups.Count - 1 do
    TSubscribedGroup (Groups [i]).fSearching := True;

  if Groups.Count > 0 then
    if (groups.Count = 1) and (Article <> Nil) then
      fCodePage := Article.CodePage
    else
    begin
      fCodePage := TSubscribedGroup (fGroups [0]).DisplaySettings.DefaultCodePage;
      Article := Nil
    end
  else
    fCodePage := NNTPAccounts.DisplaySettings.DefaultCodepage;

  for col := Low (TNNTPFilterColumn) to High (TNNTPFilterColumn) do
  begin
    en := Enableds [col];
    ed := Editors [col];
    if Assigned (ed) then
    begin
      ed.CodePage := fCodePage;
      ed.Enabled := en.Checked;
    end;

    cb := Operators [col];
    if Assigned (cb) then
    begin
      cb.Items.BeginUpdate;
      try
        for op := Low (TNNTPFilterOperator) to High (TNNTPFilterOperator) do
          if op in ValidOperators [col] then
            cb.Items.Add(OperatorNames [op])
      finally
        cb.Items.EndUpdate
      end;
      if cb.Items.Count > 0 then
        cb.ItemIndex := 0;

      cb.Enabled := en.Checked;
      if en.Checked then
        cb.Font.Color := clBlack
      else
        cb.Font.Color := clGrayText;
    end
  end;

  LoadPreviousSettings
end;

procedure TdlgSearch.LoadPreviousSettings;
var
  i : Integer;
  s : string;
  sl : TStringList;
  col : TNNTPFilterColumn;
  op : TNNTPFilterOperator;
  en : TCheckBox;
  opc : TComboBox;
  ed : TuEdit;

begin
  FreeAndNil (fFilters);
  fLoading := True;
  try
    sl := Nil;
    with CreateExSettings do
    try
      Section := 'Search';
      if Open (true) then
      begin
        sl := TStringList.Create;
        GetStrings ('S0', sl);

        if sl.Count > 0 then
          fFilters := TNNTPFilters.Create (True);

        for i := 0 to sl.Count - 1 do
        begin
          s := sl [i];
          if TextToNNTPFilter (s, col, op) then
          case col of
            ftDate :
              fFilters.AddObject('', TNNTPFilter.Create('', col, op, StrToDateTime (s), false, false, false));
            ftLines,
            ftCrossposted,
            ftNumber :
              fFilters.AddObject('', TNNTPFilter.Create('', col, op, StrToIntDef (s, 0), false, false, false));
            else
              fFilters.AddObject('', TNNTPFilter.Create('', col, op, s, false, false, false));
          end
        end;
      end
    finally
      sl.Free;
      Free
    end;

    if Assigned (fFilters) then
    begin
      for i := 0 to fFilters.Count - 1 do
      begin
        col := fFilters [i].Column;
        en := Enableds [col];
        if Assigned (en) then
        begin
          en.Checked := True;
          opc := Operators [col];
          opc.Text := OperatorNames [fFilters [i].Operator];

          if col = ftDate then
            DateTimePicker1.Date := StrToDateDef (fFilters [i].StrVal, Now)
          else
          begin
            ed := Editors [col];
            ed.WideText := fFilters [i].StrVal
          end
        end
      end
    end
    else
    begin
      Enableds [ftMessageBody].Checked := True;
    end
  finally
    fLoading := False;
    FreeAndNil (fFilters);
  end
end;

procedure TdlgSearch.SavePreviousSettings;
var
  i, idx : Integer;
  reg : TExSettings;
  s, sl1 : TStringList;
begin
  if not Assigned (fFilters) then Exit;
  s := Nil;
  sl1 := Nil;
  reg := CreateExSettings;
  try
    reg.Section := 'Search';

    s := TStringList.Create;
    sl1 := TStringList.Create;

    reg.GetValueNames(s);

    s.Sorted := True;

    for i := 9 downto 0 do
    begin
      idx := s.IndexOf('S' + IntToStr (i));
      if idx >= 0 then
      begin
        reg.GetStrings ('S' + IntToStr (i), sl1);
        reg.DeleteValue('S' + IntToStr (i));
        reg.SetStrings('S' + IntToStr (i + 1), sl1);

        s.Delete (idx);
      end
    end;

    for i := 0 to s.Count - 1 do
      reg.DeleteValue(s [i]);

    s.Clear;
    for i := 0 to Filters.Count - 1 do
      s.Add(Filters [i].Text);

    reg.SetStrings('S0', s);

  finally
    reg.Free;
    s.Free;
    sl1.Free;
  end
end;

constructor TdlgSearch.Create(AOwner: TComponent);
begin
  inherited;
  fGroups := TObjectList.Create;
  fGroups.OwnsObjects := False
end;

function TdlgSearch.GetOperators(column : TNNTPFilterColumn): TComboBox;
begin
  case column of
    ftSubject     : result := cbxSubject;
    ftAuthor      : result := cbxAuthor;
    ftDate        : result := cbxDate;
    ftLines       : result := cbxLines;
    ftMessageBody : result := cbxMessageBody;
    ftMessageID   : result := cbxMessageId;
    ftHeaderLines : result := cbxHeaderLines;
    ftCrossposted : result := cbxCrossposted;
    else
      result := Nil
  end
end;

function TdlgSearch.GetEditors(column: TNNTPFilterColumn): TuEdit;
begin
  case column of
    ftSubject     : result := edSubject;
    ftAuthor      : result := edAuthor;
    ftDate        : result := Nil;
    ftLines       : result := edLines;
    ftMessageBody : result := edMessageBody;
    ftMessageID   : result := edMessageId;
    ftHeaderLines : result := edHeaderLines;
    ftCrossposted : result := edCrossposted;
    else
      result := Nil
  end
end;

function TdlgSearch.GetEditorCtrls(column: TNNTPFilterColumn): TWinControl;
begin
  case column of
    ftSubject     : result := edSubject;
    ftAuthor      : result := edAuthor;
    ftDate        : result := DateTimePicker1;
    ftLines       : result := edLines;
    ftMessageBody : result := edMessageBody;
    ftMessageID   : result := edMessageId;
    ftHeaderLines : result := edHeaderLines;
    ftCrossposted : result := edCrossposted;
    else
      result := Nil
  end
end;

function TdlgSearch.GetEnableds(column: TNNTPFilterColumn): TCheckBox;
begin
  case column of
    ftSubject     : result := cbSubject;
    ftAuthor      : result := cbAuthor;
    ftDate        : result := cbDate;
    ftLines       : result := cbLines;
    ftMessageBody : result := cbMessageBody;
    ftMessageID   : result := cbMessageId;
    ftHeaderLines : result := cbHeaderLines;
    ftCrossposted : result := cbCrossposted;
    else
      result := Nil
  end
end;

procedure TdlgSearch.cbxSubjectChange(Sender: TObject);
begin
  if not fLoading then
    FreeAndNil (fFilters);
end;

procedure TdlgSearch.edAuthorChange(Sender: TObject);
begin
  if not fLoading then
    FreeAndNil (fFilters)
end;

procedure TdlgSearch.DateTimePicker1Change(Sender: TObject);
begin
  if not fLoading then
    FreeAndNil (fFilters);
end;

procedure TdlgSearch.Timer1Timer(Sender: TObject);
begin
  Close
end;

end.
