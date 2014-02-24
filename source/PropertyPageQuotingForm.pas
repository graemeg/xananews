unit PropertyPageQuotingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageDefaultsForm, StdCtrls, ExtCtrls, PropertyPageForm,
  PropertyPagePosting, unitSettings;

type
  TPropertyPageQuotingData = class (TPropertyPagePostingData)
  public
    function Apply: Boolean; override;
  end;

  TfmPropertyPageQuoting = class(TfmPropertyPageDefaults)
    Label16: TLabel;
    edQuoteHeader: TEdit;
    edQuoteLineMarker: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    edQuoteFooter: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label1: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    edSalutation: TEdit;
    Label2: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    procedure edQuoteHeaderChange(Sender: TObject);
  private
    fData: TPropertyPageQuotingData;
    procedure InitializeControls(settings: TPostingSettings);
    procedure UpdateData;
  protected
    function CanRestoreParentSettings: Boolean; override;
    procedure RestoreParentSettings; override;
  public
    class function GetDataClass: TPropertyPageDataClass; override;
    procedure PopulateControls(AData: TPropertyPageData); override;
  end;

var
  fmPropertyPageQuoting: TfmPropertyPageQuoting;

implementation

{$R *.dfm}

{ TPropertyPageQuotingData }

function TPropertyPageQuotingData.Apply: Boolean;
var
  settings: TPostingSettings;
begin
  Result := True;
  settings := TPostingSettings(Param);

  { nb. Can't just do settings.Assign (fSettings) because only some
        posting settings are handled by this form }

  settings.QuoteHeader := fSettings.QuoteHeader;
  settings.QuoteLineMarker := TrimLeft(fSettings.QuoteLineMarker);
  settings.QuoteFooter := fSettings.QuoteFooter;
  settings.QuoteSalutation := fSettings.QuoteSalutation;
end;

{ TfmPropertyPageQuoting }

function TfmPropertyPageQuoting.CanRestoreParentSettings: Boolean;
begin
  Result := Assigned(fData.fSettings.Parent);
end;

class function TfmPropertyPageQuoting.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageQuotingData;
end;

procedure TfmPropertyPageQuoting.InitializeControls(
  settings: TPostingSettings);
begin
  edQuoteHeader.Text := settings.QuoteHeader;
  edQuoteFooter.Text := settings.QuoteFooter;
  edQuoteLineMarker.Text := TrimLeft(settings.QuoteLineMarker);
  edSalutation.Text := settings.QuoteSalutation;
end;

procedure TfmPropertyPageQuoting.PopulateControls(
  AData: TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageQuotingData;
  InitializeControls(fData.fSettings);
end;

procedure TfmPropertyPageQuoting.RestoreParentSettings;
begin
  fData.fSettings.Assign(fData.fSettings.Parent);
  Populating := True;
  InitializeControls(fData.fSettings);
  Populating := False;
end;

procedure TfmPropertyPageQuoting.UpdateData;
var
  settings: TPostingSettings;
begin
  if Populating then Exit;
  settings := fData.fSettings;

  settings.QuoteLineMarker := edQuoteLineMarker.Text;
  settings.QuoteHeader := edQuoteHeader.Text;
  settings.QuoteFooter := edQuoteFooter.Text;
  settings.QuoteSalutation := edSalutation.Text;
end;

procedure TfmPropertyPageQuoting.edQuoteHeaderChange(Sender: TObject);
begin
  inherited;
  UpdateData;
end;

end.
