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

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, Controls, StdCtrls,
  Buttons, ExtCtrls, unitNNTPFilters, unitNNTPServices, unitBookmarks,
  cmpPersistentPosition, ConTnrs, ComCtrls, unitExSettings;

type
  TSearchableColumns = ftSubject..ftCrossposted;
  TOnArticleFound = procedure(article: TArticleBase; bookmark: Boolean; var continue: Boolean) of object;
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
    edSubject: TEdit;
    edAuthor: TEdit;
    edLines: TEdit;
    edMessageBody: TEdit;
    edMessageID: TEdit;
    edHeaderLines: TEdit;
    edCrossposted: TEdit;
    Timer1: TTimer;
    cbInterestingMessagesOnly: TCheckBox;
    procedure PersistentPosition1GetSettingsFile(Owner: TObject; var fileName: string);
    procedure PersistentPosition1GetSettingsClass(Owner: TObject; var SettingsClass: TExSettingsClass);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure cbClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxChange(Sender: TObject);
    procedure edChange(Sender: TObject);
    procedure DateTimePicker1Change(Sender: TObject);
  private
    fFilters: TNNTPFilters;
    fArticle: TArticleBase;
    fOnArticleFound: TOnArticleFound;
    fCodePage: Integer;
    fGroups: TObjectList;
    fLoading: Boolean;
    fCont: Boolean;
    fSearchToBookmark: Boolean;
    function GetFilters: TNNTPFilters;
    function GetOperators(column: TNNTPFilterColumn): TComboBox;
    function GetEditors(column: TNNTPFilterColumn): TEdit;
    function GetEditorCtrls(column: TNNTPFilterColumn): TWinControl;
    function GetEnableds(column: TNNTPFilterColumn): TCheckBox;
  protected
    procedure UpdateActions; override;
    procedure CreateParams(var params: TCreateParams); override;
    procedure LoadPreviousSettings;
    procedure SavePreviousSettings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Filters: TNNTPFilters read GetFilters;
    property Groups: TObjectList read fGroups write fGroups;
    property Article: TArticleBase read fArticle write fArticle;

    property OnArticleFound: TOnArticleFound read fOnArticleFound write fOnArticleFound;

    property Operators[column: TNNTPFilterColumn]: TComboBox read GetOperators;
    property Editors[column: TNNTPFilterColumn]: TEdit read GetEditors;
    property EditorCtrls[column: TNNTPFilterColumn]: TWinControl read GetEditorCtrls;
    property Enableds[column: TNNTPFilterColumn]: TCheckBox read GetEnableds;
  end;

implementation

uses
  unitNewsReaderOptions, NewsGlobals, unitCharsetMap, unitSearchString, unitMessageBaseSearch;

{$R *.dfm}

{ TdlgSearch }

destructor TdlgSearch.Destroy;
var
  i: Integer;
begin
  FreeAndNil(fFilters);
  for i := 0 to Groups.Count - 1 do
    TArticleContainer(Groups[i]).fSearching := False;

  fGroups.Free;

  inherited Destroy;
end;

function TdlgSearch.GetFilters: TNNTPFilters;
var
  operator: TNNTPFilterOperator;
  unread, caseSensitive, interesting: Boolean;
  col: TNNTPFilterColumn;
  en: TCheckBox;
  op: TComboBox;
  ed: TEdit;
begin
  if Assigned(fFilters) then
  begin
    Result := fFilters;
    Exit;
  end;

  fFilters := TNNTPFilters.Create(True);

  unread := cbUnreadMessagesOnly.Checked;
  caseSensitive := cbCaseSensitive.Checked;
  interesting := cbInterestingMessagesOnly.Checked;

  for col := Low(TNNTPFilterColumn) to High(TNNTPFilterColumn) do
  begin
    en := Enableds[col];
    if not Assigned(en) or not en.Checked then
      Continue;

    op := Operators[col];
    ed := Editors[col];

    GetOperator(op.Text, operator);

    case col of
      ftDate:
        fFilters.AddObject('', TNNTPFilter.Create('', col, operator, DateTimePicker1.Date, unread, interesting, caseSensitive));
      ftLines,
      ftCrossposted,
      ftNumber:
        fFilters.AddObject('', TNNTPFilter.Create('', col, operator, StrToInt64Def(ed.Text, 0), unread, interesting, caseSensitive));
    else
      fFilters.AddObject('', TNNTPFilter.Create('', col, operator, ed.Text, unread, interesting, caseSensitive))
    end;
  end;
  Result := fFilters;
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
    Timer1.Enabled := True;
  end
  else
    Close;
end;

procedure TdlgSearch.OKBtnClick(Sender: TObject);
var
  max: Integer;
  cont: Boolean;
  found: Boolean;
begin
  if not Assigned(Filters) then Exit;

  max := StrToIntDef(edMaxResults.Text, 200);
  if max <= 0 then max := MaxInt;

  SavePreviousSettings;

  cont := True;
  found := False;
  fSearchToBookmark := cbSearchToBookmark.Checked;

  while cont do
  begin
    article := unitMessageBaseSearch.Search(article, fGroups, Filters);

    if Assigned(article) then
    begin
      found := True;

      if Assigned(OnArticleFound) then
        OnArticleFound(article, fSearchToBookmark, cont);

      if fSearchToBookmark then
      begin
        Dec(max);
        if max < 0 then
          cont := False; // 'Max' articles added to bookmark.  End
      end
      else
        cont := False;   // Article found and not searching to bookmark. End.
    end
    else
    begin
      if not found then
        ShowMessage('Could not find a matching article');
      cont := False;     // Article not found.  End
    end;
  end;
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

procedure TdlgSearch.cbClick(Sender: TObject);
var
  col: TNNTPFilterColumn;
  ctrl: TCheckBox;
  op: TComboBox;
  ed: TWinControl;
begin
  if not (sender is TCheckBox) then Exit;
  if not fLoading then
    FreeAndNil(fFilters);
  ctrl := TCheckBox(Sender);
  col := TNNTPFilterColumn(ctrl.Tag);

  op := Operators[col];
  ed := EditorCtrls[col];

  op.Enabled := ctrl.Checked;
  if ctrl.Checked then
    op.Font.Color := clBlack
  else
    op.Font.Color := clGrayText;
  ed.Enabled := ctrl.Checked;
end;

procedure TdlgSearch.UpdateActions;
begin
  OKBtn.Enabled := cbSubject.Checked or cbAuthor.Checked or
    cbLines.Checked or cbDate.Checked or cbMessageBody.Checked or
    cbMessageID.Checked or cbHeaderLines.Checked or cbCrossposted.Checked;
end;

constructor TdlgSearch.Create(AOwner: TComponent);
begin
  inherited;
  fGroups := TObjectList.Create;
  fGroups.OwnsObjects := False;
end;

procedure TdlgSearch.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams(params);
  params.ExStyle   := params.ExStyle or WS_EX_APPWINDOW;
  params.WndParent := Application.Handle;
end;

procedure TdlgSearch.FormShow(Sender: TObject);
var
  i: Integer;
  col: TNNTPFilterColumn;
  op: TNNTPFilterOperator;
  ed: TEdit;
  cb: TComboBox;
  en: TCheckBox;
begin
  AdjustFormConstraints(Self);
  for i := 0 to Groups.Count - 1 do
    TSubscribedGroup(Groups[i]).fSearching := True;

  if Groups.Count > 0 then
    if (groups.Count = 1) and (Article <> nil) then
      fCodePage := Article.CodePage
    else
    begin
      fCodePage := TSubscribedGroup(fGroups[0]).DisplaySettings.DefaultCodePage;
      Article := nil;
    end
  else
    fCodePage := NNTPAccounts.DisplaySettings.DefaultCodepage;

  for col := Low(TNNTPFilterColumn) to High(TNNTPFilterColumn) do
  begin
    en := Enableds[col];
    ed := Editors[col];
    if Assigned(ed) then
      ed.Enabled := en.Checked;

    cb := Operators[col];
    if Assigned(cb) then
    begin
      cb.Items.BeginUpdate;
      try
        for op := Low(TNNTPFilterOperator) to High(TNNTPFilterOperator) do
          if op in ValidOperators[col] then
            cb.Items.Add(OperatorNames[op]);
      finally
        cb.Items.EndUpdate;
      end;
      if cb.Items.Count > 0 then
        cb.ItemIndex := 0;

      cb.Enabled := en.Checked;
      if en.Checked then
        cb.Font.Color := clBlack
      else
        cb.Font.Color := clGrayText;
    end;
  end;

  DateTimePicker1.Date := Date;
  DateTimePicker1.Time := 0;

  LoadPreviousSettings;
end;

procedure TdlgSearch.LoadPreviousSettings;
var
  i: Integer;
  idx: Integer;
  s: string;
  sl: TStringList;
  col: TNNTPFilterColumn;
  op: TNNTPFilterOperator;
  en: TCheckBox;
  opc: TComboBox;
  ed: TEdit;
begin
  FreeAndNil(fFilters);
  fLoading := True;
  try
    sl := nil;
    with CreateExSettings do
    try
      Section := 'Search';
      if Open(True) then
      begin
        sl := TStringList.Create;
        GetValueNames(sl);
        if sl.IndexOf('S0') >= 0 then
        begin
          GetStrings('S0', sl);

          if sl.Count > 0 then
            fFilters := TNNTPFilters.Create(True);

          for i := 0 to sl.Count - 1 do
          begin
            s := sl[i];
            if TextToNNTPFilter(s, col, op) then
            case col of
              ftDate:
                fFilters.AddObject('', TNNTPFilter.Create('', col, op, StrToDate(s), False, False, False));
              ftLines,
              ftCrossposted,
              ftNumber:
                fFilters.AddObject('', TNNTPFilter.Create('', col, op, StrToInt64Def(s, 0), False, False, False));
            else
              fFilters.AddObject('', TNNTPFilter.Create('', col, op, s, False, False, False));
            end;
          end;
        end;
      end
    finally
      sl.Free;
      Free
    end;

    if Assigned(fFilters) then
    begin
      for i := 0 to fFilters.Count - 1 do
      begin
        col := fFilters[i].Column;
        en := Enableds[col];
        if Assigned(en) then
        begin
          en.Checked := True;
          opc := Operators[col];
          idx := opc.Items.IndexOf(OperatorNames[fFilters[i].Operator]);
          if idx <> -1 then
            opc.ItemIndex := idx;

          if col = ftDate then
            DateTimePicker1.Date := StrToDateDef(fFilters[i].StrVal, Now)
          else
          begin
            ed := Editors[col];
            ed.Text := fFilters[i].StrVal;
          end;
        end;
      end;
    end
    else
      Enableds[ftMessageBody].Checked := True;
  finally
    fLoading := False;
    FreeAndNil(fFilters);
  end;
end;

procedure TdlgSearch.SavePreviousSettings;
var
  i: Integer;
  reg: TExSettings;
  s, sl1: TStringList;
begin
  if not Assigned(fFilters) then Exit;
  s := nil;
  sl1 := nil;
  reg := CreateExSettings;
  try
    reg.Section := 'Search';

    s := TStringList.Create;
    sl1 := TStringList.Create;

    reg.GetValueNames(s);

    // Clean-up unused entries from previous versions.
    s.Sorted := True;
    for i := 1 to s.Count - 1 do
      reg.DeleteValue(s[i]);

    s.Clear;
    for i := 0 to Filters.Count - 1 do
      s.Add(Filters[i].Text);

    reg.SetStrings('S0', s);
  finally
    reg.Free;
    s.Free;
    sl1.Free;
  end;
end;

function TdlgSearch.GetOperators(column: TNNTPFilterColumn): TComboBox;
begin
  case column of
    ftSubject    : Result := cbxSubject;
    ftAuthor     : Result := cbxAuthor;
    ftDate       : Result := cbxDate;
    ftLines      : Result := cbxLines;
    ftMessageBody: Result := cbxMessageBody;
    ftMessageID  : Result := cbxMessageId;
    ftHeaderLines: Result := cbxHeaderLines;
    ftCrossposted: Result := cbxCrossposted;
  else
    Result := nil;
  end;
end;

function TdlgSearch.GetEditors(column: TNNTPFilterColumn): TEdit;
begin
  case column of
    ftSubject    : Result := edSubject;
    ftAuthor     : Result := edAuthor;
    ftDate       : Result := nil;
    ftLines      : Result := edLines;
    ftMessageBody: Result := edMessageBody;
    ftMessageID  : Result := edMessageId;
    ftHeaderLines: Result := edHeaderLines;
    ftCrossposted: Result := edCrossposted;
  else
    Result := nil;
  end;
end;

function TdlgSearch.GetEditorCtrls(column: TNNTPFilterColumn): TWinControl;
begin
  case column of
    ftSubject    : Result := edSubject;
    ftAuthor     : Result := edAuthor;
    ftDate       : Result := DateTimePicker1;
    ftLines      : Result := edLines;
    ftMessageBody: Result := edMessageBody;
    ftMessageID  : Result := edMessageId;
    ftHeaderLines: Result := edHeaderLines;
    ftCrossposted: Result := edCrossposted;
  else
    Result := nil;
  end;
end;

function TdlgSearch.GetEnableds(column: TNNTPFilterColumn): TCheckBox;
begin
  case column of
    ftSubject    : Result := cbSubject;
    ftAuthor     : Result := cbAuthor;
    ftDate       : Result := cbDate;
    ftLines      : Result := cbLines;
    ftMessageBody: Result := cbMessageBody;
    ftMessageID  : Result := cbMessageId;
    ftHeaderLines: Result := cbHeaderLines;
    ftCrossposted: Result := cbCrossposted;
  else
    Result := nil;
  end;
end;

procedure TdlgSearch.cbxChange(Sender: TObject);
begin
  if not fLoading then
    FreeAndNil(fFilters);
end;

procedure TdlgSearch.edChange(Sender: TObject);
begin
  if not fLoading then
    FreeAndNil(fFilters);
end;

procedure TdlgSearch.DateTimePicker1Change(Sender: TObject);
begin
  if not fLoading then
    FreeAndNil(fFilters);
end;

procedure TdlgSearch.Timer1Timer(Sender: TObject);
begin
  Close;
end;

end.
