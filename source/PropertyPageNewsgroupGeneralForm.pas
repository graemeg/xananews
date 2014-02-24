unit PropertyPageNewsgroupGeneralForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageNewsgroupGeneralData = class (TPropertyPageData)
  private
    fGroupName : string;
    fNickname : string;
    fSecret : boolean;

  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageNewsgroupGeneral = class(TfmPropertyPage)
    edNewsgroup: TEdit;
    Label2: TLabel;
    cbNickname: TComboBox;
    cbSecret: TCheckBox;
    procedure cbNicknameChange(Sender: TObject);
    procedure cbSecretClick(Sender: TObject);
  private
    fData : TPropertyPageNewsgroupGeneralData;
    procedure UpdateData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageNewsgroupGeneral: TfmPropertyPageNewsgroupGeneral;

implementation

uses unitNNTPServices;

{$R *.dfm}

{ TPropertyPageNewsgroupGeneralData }

function TPropertyPageNewsgroupGeneralData.Apply : boolean;
var
  grp : TSubscribedGroup;
begin
  result := true;
  grp := TSubscribedGroup (Param);

  grp.Nickname := fNickname;
  grp.Secret := fSecret;
end;

procedure TPropertyPageNewsgroupGeneralData.Initialize;
var
  grp : TSubscribedGroup;
begin
  grp := TSubscribedGroup (Param);

  fGroupName := grp.Name;
  fSecret := grp.Secret;
  fNickname := grp.Nickname;
end;

{ TfmPropertyPageNewsgroupGeneral }

class function TfmPropertyPageNewsgroupGeneral.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageNewsgroupGeneralData;
end;

procedure TfmPropertyPageNewsgroupGeneral.PopulateControls(
  AData: TPropertyPageData);
  procedure AddSuggestion (st : string);
  begin
    if st = '' then Exit;
    if not SameText (st, fData.fNickname) and (cbNickname.Items.IndexOf (st) = -1) then
    cbNickname.Items.Add(st)
  end;

  function TrimGroupName (n : Integer) : string;
  var
    st : string;
    p : Integer;
  begin
    st := fData.fGroupName;
    p := Length (st);
    while (n > 0) and (p > 0) do
    begin
      while p > 0 do
        if st [p] = '.' then
        begin
          Dec (n);
          break
        end
        else
          Dec (p);
      if n > 0 then
        Dec (p)
    end;

    if p < 0 then
      p := 0;

    if p < Length (fData.fGroupName) then
    begin
      result := Copy (fData.fGroupName, p + 1, MaxInt);
      result := StringReplace (result, '.', ' ', [rfReplaceAll]);
      p := 1;
      while p < Length (result) do
      begin
        if result [p] in ['a'..'z'] then
          result [p] := UpCase (result [p]);

        while p < Length (result) do
          if result [p] = ' ' then break else Inc (p);

        Inc (p)
      end
    end
  end;

begin
  inherited;
  fData := AData as TPropertyPageNewsgroupGeneralData;

  edNewsgroup.Text := fData.fGroupName;
  AddSuggestion (TrimGroupName (2));
  AddSuggestion (TrimGroupName (1));
  if fData.fNickname <> '' then
  begin
    cbNickname.Items.Insert(0, fData.fNickname);
    cbNickname.ItemIndex := 0
  end;
  cbSecret.Checked := fData.fSecret
end;

procedure TfmPropertyPageNewsgroupGeneral.UpdateData;
begin
  if Populating then Exit;
  fData.fNickname := cbNickname.Text;
  fData.fSecret := cbSecret.Checked;
end;

procedure TfmPropertyPageNewsgroupGeneral.cbNicknameChange(
  Sender: TObject);
begin
  UpdateData;
end;

procedure TfmPropertyPageNewsgroupGeneral.cbSecretClick(Sender: TObject);
begin
  UpdateData
end;

end.
