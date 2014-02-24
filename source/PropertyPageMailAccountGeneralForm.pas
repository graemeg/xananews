unit PropertyPageMailAccountGeneralForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageMailAccountGeneralData = class (TPropertyPageData)
  private
    fAccountName : string;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageMailAccountGeneral = class(TfmPropertyPage)
    Label7: TLabel;
    edAccountName: TEdit;
    procedure edAccountNameChange(Sender: TObject);
    procedure cbIdentitiesChange(Sender: TObject);
  private
    fData : TPropertyPageMailAccountGeneralData;

    procedure UpdateData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageMailAccountGeneral: TfmPropertyPageMailAccountGeneral;

implementation

uses unitMailServices, unitNNTPServices;

{$R *.dfm}

{ TPropertyPageMailAccountGeneralData }

function TPropertyPageMailAccountGeneralData.Apply : boolean;
var
  acct : TMailAccount;
begin
  result := True;
  acct := TMailAccount (Param);
  if fAccountName <> '' then
    acct.Name := fAccountName;
end;

procedure TPropertyPageMailAccountGeneralData.Initialize;
var
  acct : TMailAccount;
begin
  acct := TMailAccount (Param);
  fAccountName := acct.Name;
end;

{ TfmPropertyPageMailAccountGeneral }

class function TfmPropertyPageMailAccountGeneral.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageMailAccountGeneralData
end;

procedure TfmPropertyPageMailAccountGeneral.PopulateControls(
  AData: TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageMailAccountGeneralData;

  edAccountName.Text := fData.fAccountName;
end;

procedure TfmPropertyPageMailAccountGeneral.edAccountNameChange(
  Sender: TObject);
begin
  UpdateData
end;

procedure TfmPropertyPageMailAccountGeneral.UpdateData;
begin
  if Populating then Exit;
  fData.fAccountName := edAccountName.Text;
end;

procedure TfmPropertyPageMailAccountGeneral.cbIdentitiesChange(
  Sender: TObject);
begin
  UpdateData
end;

end.
