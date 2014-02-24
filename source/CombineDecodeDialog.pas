unit CombineDecodeDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, CheckLst, Contnrs, unitNNTPServices, unitMessages,
  Menus, XnRawByteStrings;

type
  TdlgCombineDecode = class(TForm)
    clbArticles: TCheckListBox;
    sbUp: TSpeedButton;
    sbDown: TSpeedButton;
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    edFileName: TEdit;
    SpeedButton1: TSpeedButton;
    SaveDialog: TSaveDialog;
    PopupMenu1: TPopupMenu;
    SelectAll1: TMenuItem;
    SelectNone1: TMenuItem;
    procedure SelectNone1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbUpClick(Sender: TObject);
    procedure sbDownClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    procedure GetFileNameFromFirstArticle;
  protected
    procedure UpdateActions; override;
  public
    Articles: TObjectList;
  end;

var
  dlgCombineDecode: TdlgCombineDecode;

implementation

uses
  MainForm;

{$R *.dfm}

procedure TdlgCombineDecode.FormCreate(Sender: TObject);
begin
  Articles := TObjectList.Create;
  Articles.OwnsObjects := False;
end;

procedure TdlgCombineDecode.FormDestroy(Sender: TObject);
begin
  Articles.Free;
end;

function CompareArticlesSubjects(Item1, Item2: Pointer): Integer;
var
  n, x: Integer;
  S1, S2: RawByteString;
begin
  S1 := TArticleBase(Item1).RawSubject;
  S2 := TArticleBase(Item2).RawSubject;
  DecodeMultipartSubject(S1, n, x, True, False);
  DecodeMultipartSubject(S2, n, x, True, False);
  Result := RawCompareText(S1, S2);
end;

procedure TdlgCombineDecode.FormShow(Sender: TObject);
var
  i: Integer;
  article: TArticleBase;
begin
  Articles.Sort(CompareArticlesSubjects);

  for i := 0 to Articles.Count - 1 do
  begin
    article := TArticleBase(Articles[i]);
    clbArticles.Items.AddObject(article.Subject, article);
    clbArticles.State[i] := cbChecked;
  end;

  if Articles.Count > 0 then
  begin
    clbArticles.Selected[0] := True;
    GetFileNameFromFirstArticle;
  end;

  Caption := 'Combine & Decode ' + IntToStr(Articles.Count) + ' Articles';
end;

procedure TdlgCombineDecode.FormResize(Sender: TObject);
var
  r, sbRect: TRect;
  m, sbHeight: Integer;
begin
  sbRect.Top := sbUp.Top;
  sbRect.Left := sbUp.Left;
  sbRect.Right := sbUp.Left + sbUp.Width;
  sbRect.Bottom := sbDown.Top + sbDown.Height;
  sbHeight := sbRect.Bottom - sbRect.Top;

  r := clbArticles.BoundsRect;
  m := r.Top + (r.Bottom - r.Top) div 2;

  sbRect.Top := m - sbHeight div 2;
  sbRect.Bottom := sbRect.Top + sbHeight;

  sbUp.Top := sbRect.Top;
  sbDown.Top := sbRect.Bottom - sbDown.Height;
end;

procedure TdlgCombineDecode.UpdateActions;
var
  idx: Integer;
begin
  idx := clbArticles.ItemIndex;
  sbUp.Enabled := idx > 0;
  sbDown.Enabled := idx < clbArticles.Count - 1;
end;

procedure TdlgCombineDecode.sbUpClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := clbArticles.ItemIndex;
  if idx < 1 then Exit;

  clbArticles.Items.Exchange(idx, idx - 1);
  clbArticles.ItemIndex := idx - 1;
end;

procedure TdlgCombineDecode.sbDownClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := clbArticles.ItemIndex;
  if idx >= clbArticles.Count - 1 then Exit;

  clbArticles.Items.Exchange(idx, idx + 1);
  clbArticles.ItemIndex := idx + 1;
end;

procedure TdlgCombineDecode.btnOKClick(Sender: TObject);
var
  i: Integer;
  p: string;
  ok: boolean;
begin
  ok := True;
  p := ExtractFilePath(edFileName.Text);
  if (p <> '') and not DirectoryExists(p) then
  begin
    MessageBox(0, PChar(Format('Path %s does not exist.  Please verify that the correct path was given', [p])),
      'XanaNews', MB_OK or MB_ICONEXCLAMATION);
    ok := False
  end;

  if ok then
  begin
    Articles.Clear;
    for i := 0 to clbArticles.Count - 1 do
      if clbArticles.State[i] = cbChecked then
        Articles.Add(clbArticles.Items.Objects[i]);
    modalResult := mrOk;
  end;
end;

procedure TdlgCombineDecode.SpeedButton1Click(Sender: TObject);
begin
  SaveDialog.FileName := edFileName.Text;
  if SaveDialog.Execute then
    edFileName.Text := SaveDialog.FileName;
end;

procedure TdlgCombineDecode.GetFileNameFromFirstArticle;
var
  fileName: string;
  article: TArticleBase;
  mp: TmvMessagePart;
  i: Integer;
begin
  if clbArticles.Count = 0 then Exit;
  article := TArticleBase(clbArticles.Items.Objects[0]);

  fileName := '';
  if article.HasMsg and Assigned(article.Msg) then
    if article.Msg.MessageParts.Count > 0 then
      for i := 0 to article.Msg.MessageParts.Count - 1 do
      begin
        mp := article.Msg.MessageParts[i];
        if (mp.FileName <> '') and (CompareText(mp.FileName, 'text/plain') <> 0) then
        begin
          fileName := mp.FileName;
          Break;
        end;
      end;

  fileName := StringReplace(fileName, '\', '-', [rfReplaceAll]);
  fileName := StringReplace(fileName, '/', '-', [rfReplaceAll]);

  fileName := fmMain.GetAttachmentsDirectory + fileName;
  edFileName.Text := fileName;
end;

procedure TdlgCombineDecode.SelectAll1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to clbArticles.Count - 1 do
    clbArticles.State[i] := cbChecked;
end;

procedure TdlgCombineDecode.SelectNone1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to clbArticles.Count - 1 do
    clbArticles.State[i] := cbUnchecked;
end;

end.
