unit PropertyPageWin98Form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageWin98Data = class (TPropertyPageData)
  private
    fTextWindowSizeK : Integer;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageWin98 = class(TfmPropertyPage)
    Label5: TLabel;
    edTextWindowSize: TEdit;
    procedure edTextWindowSizeChange(Sender: TObject);
  private
    fData : TPropertyPageWin98Data;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageWin98: TfmPropertyPageWin98;

implementation

uses unitNewsReaderOptions;

{$R *.dfm}

{ TfmPropertyPageWin98 }

class function TfmPropertyPageWin98.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageWin98Data
end;

procedure TfmPropertyPageWin98.PopulateControls(AData: TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageWin98Data;

  edTextWindowSize.Text := IntToStr (fData.fTextWindowSizeK);
end;

{ TPropertyPageWin98Data }

function TPropertyPageWin98Data.Apply : boolean;
begin
  result := True;
  XNOptions.TextWindowSizeK := fTextWindowSizeK
end;

procedure TPropertyPageWin98Data.Initialize;
begin
  fTextWindowSizeK := XNOptions.TextWindowSizeK;
end;

procedure TfmPropertyPageWin98.edTextWindowSizeChange(Sender: TObject);
begin
  if Populating then Exit;
  fData.fTextWindowSizeK := StrToIntDef (edTextWindowSize.Text, 3);
end;

end.
