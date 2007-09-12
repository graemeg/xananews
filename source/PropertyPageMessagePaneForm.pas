unit PropertyPageMessagePaneForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageMessagePaneData = class (TPropertyPageData)
  private
    fShowDetailsBar : boolean;
    fCheckSpelling : boolean;
    fNoXFaces : boolean;
    fNoHTML : boolean;
    fWrapLines : Integer;
    fStrictSigSep : boolean;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageMessagePane = class(TfmPropertyPage)
    cbShowDetailsBar: TCheckBox;
    cbCheckSpelling: TCheckBox;
    cbNOXFaces: TCheckBox;
    cbNoHTML: TCheckBox;
    Label22: TLabel;
    edWrapLines: TEdit;
    Label21: TLabel;
    cbStrictSigSep: TCheckBox;
    procedure ControlClick(Sender: TObject);
    procedure edWrapLinesChange(Sender: TObject);
  private
    fData : TPropertyPageMessagePaneData;
    procedure UpdateData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (Data : TPropertyPageData); override;
  end;

var
  fmPropertyPageMessagePane: TfmPropertyPageMessagePane;

implementation

uses unitNewsReaderOptions;

{$R *.dfm}

procedure TfmPropertyPageMessagePane.ControlClick(Sender: TObject);
begin
  UpdateData;
end;

procedure TfmPropertyPageMessagePane.edWrapLinesChange(Sender: TObject);
begin
  UpdateData;
end;

class function TfmPropertyPageMessagePane.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageMessagePaneData
end;

procedure TfmPropertyPageMessagePane.PopulateControls(
  Data: TPropertyPageData);
begin
  inherited;
  fData := TPropertyPageMessagePaneData (Data);

  cbShowDetailsBar.Checked := Options.ShowDetailsBar;
  cbCheckSpelling.Checked := Options.CheckSpelling;
  cbNOXFaces.Checked := Options.NoXFaces;
  cbNoHTML.Checked := Options.NoHTML;
  edWrapLines.Text := IntToStr (Options.WrapLines);
  cbStrictSigSep.Checked := Options.StrictSigSep;
end;

procedure TfmPropertyPageMessagePane.UpdateData;
begin
  if Populating then Exit;
  fData.fShowDetailsBar := cbShowDetailsBar.Checked;
  fData.fCheckSpelling := cbCheckSpelling.Checked;
  fData.fNoXFaces := cbNoXFaces.Checked;
  fData.fNoHTML := cbNoHTML.Checked;
  fData.fStrictSigSep := cbStrictSigSep.Checked;
  fData.fWrapLines := StrToIntDef (edWrapLines.Text, 0);
end;

{ TPropertyPageMessagePaneData }

function TPropertyPageMessagePaneData.Apply : boolean;
begin
  result := True;
  Options.ShowDetailsBar := fShowDetailsBar;
  Options.CheckSpelling := fCheckSpelling;
  Options.NoXFaces := fNoXFaces;
  Options.StrictSigSep := fStrictSigSep;
  Options.NoHTML := fNoHTML;
  Options.WrapLines := fWrapLines;
end;

procedure TPropertyPageMessagePaneData.Initialize;
begin
  fShowDetailsBar := Options.ShowDetailsBar;
  fCheckSpelling := Options.CheckSpelling;
  fNoXFaces := Options.NoXFaces;
  fStrictSigSep := Options.StrictSigSep;
  fNoHTML := Options.NoHTML;
  fWrapLines := Options.WrapLines
end;

end.
