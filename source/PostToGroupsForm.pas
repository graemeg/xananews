unit PostToGroupsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, unitNNTPServices;

type
  TfmPostToGroups = class(TForm)
    lvActions: TListView;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure lvActionsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    fAccount: TNNTPAccount;
    fGroups: string;
    fIsFollowUp: boolean;
    fPosterIdx : Integer;
    fInShow : boolean;
    { Private declarations }
  public
    property Account : TNNTPAccount read fAccount write fAccount;
    property Groups : string read fGroups write fGroups;
    property IsFollowUp : boolean read fIsFollowUp write fIsFollowUp;
    { Public declarations }
  end;

var
  fmPostToGroups: TfmPostToGroups;

implementation

{$R *.dfm}

procedure TfmPostToGroups.FormShow(Sender: TObject);
var
  i, idx, n : Integer;
  grps : TStringList;
  posterChecked : boolean;
begin
  fInShow := True;
  try
    grps := Nil;
    fPosterIdx := -1;
    posterChecked := False;

    lvActions.Items.BeginUpdate;
    try
      grps := TStringList.Create;
      grps.CaseSensitive := False;
      grps.CommaText := Groups;
      grps.Sorted := True;
      n := -1;

      if fIsFollowUp then with lvActions.Items.Add do
      begin
        fPosterIdx := Index;
        data := Nil;
        caption := 'poster';
        checked := grps.Find('poster', idx);
        if checked then
        begin
          n := 0;
          posterChecked := True
        end
      end;

      for i := 0 to Account.SubscribedGroupCount - 1 do
        with lvActions.Items.Add do
        begin
          data := account.SubscribedGroups [i];
          caption := account.SubscribedGroups [i].Name;
          checked := grps.Find(account.SubscribedGroups [i].Name, idx) and not posterChecked;
          if checked and (n = -1) then
            n := i;
        end;
    finally
      lvActions.Items.EndUpdate;
      grps.Free;
    end;

    if n >= 0 then
    begin
      lvActions.Items [n].Selected := True;

      if n < lvActions.Items.Count - 1 then
        Inc (n);

      if n < lvActions.Items.Count - 1 then
        Inc (n);

      lvActions.Items [n].MakeVisible(False);
    end
  finally
    fInShow := False
  end
end;

procedure TfmPostToGroups.btnOKClick(Sender: TObject);
var
  i : Integer;
begin
  fGroups := '';

  for i := 0 to lvActions.Items.Count - 1 do
    if lvActions.Items [i].Checked then
    begin
      if fGroups <> '' then
        fGroups := fGroups + ',';
      fGroups := fGroups + lvActions.Items [i].Caption
    end
end;

procedure TfmPostToGroups.lvActionsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);

  procedure UncheckNonPosterItems;
  var
    i : Integer;
  begin
    fInShow := True;
    try
      for i := 0 to lvActions.Items.Count - 1 do
        if i <> fPosterIdx then
          lvActions.Items [i].Checked := False
    finally
      fInShow := False
    end
  end;

begin
  if fInShow then Exit;
  if Change <> ctState then Exit;

  if Item.Index = fPosterIdx then
  begin
    if Item.Checked then
      UncheckNonPosterItems
  end
  else
    if Item.Checked and (fPosterIdx <> -1) then
      lvActions.Items [fPosterIdx].Checked := False
end;

end.
