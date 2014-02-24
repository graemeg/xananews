unit PropertyPageKeywordsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageKeywordData = class (TPropertyPageData)
  private
    fKeywordColors : array [0..7] of TColorRef;
    fKeyPhrase : array [0..7] of string;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageKeywords = class(TfmPropertyPage)
    Label18: TLabel;
    Label17: TLabel;
    Label16: TLabel;
    Label15: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label11: TLabel;
    Label10: TLabel;
    edKeyPhrase0: TEdit;
    edKeyPhrase1: TEdit;
    edKeyPhrase2: TEdit;
    edKeyPhrase3: TEdit;
    edKeyPhrase4: TEdit;
    edKeyPhrase5: TEdit;
    edKeyPhrase6: TEdit;
    edKeyPhrase7: TEdit;
    pnlKeyPhrase7: TPanel;
    pnlKeyPhrase6: TPanel;
    pnlKeyPhrase4: TPanel;
    pnlKeyPhrase3: TPanel;
    pnlKeyPhrase2: TPanel;
    pnlKeyPhrase1: TPanel;
    pnlKeyPhrase0: TPanel;
    Label28: TLabel;
    pnlKeyPhrase5: TPanel;
    ColorDialog1: TColorDialog;
    procedure EditChange(Sender: TObject);
    procedure KeywordColorButtonClick(Sender: TObject);
  private
    fData : TPropertyPageKeywordData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageKeywords: TfmPropertyPageKeywords;

implementation

uses unitNewsReaderOptions;

{$R *.dfm}

{ TfmPropertyPageKeywords }

class function TfmPropertyPageKeywords.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageKeywordData;
end;

procedure TfmPropertyPageKeywords.PopulateControls(
  AData: TPropertyPageData);
var
  i : Integer;
  ctrl : TControl;
  pnl : TPanel;
  ed : TEdit;
begin
  inherited;
  fData := AData as TPropertyPageKeywordData;

  for i := 0 to ControlCount - 1 do
  begin
    ctrl := Controls [i];
    if (ctrl.Tag >= 1) and (ctrl.Tag <= 8) then
      if ctrl is TEdit then
      begin
        ed := TEdit (ctrl);
        ed.Text := fData.fKeyPhrase [ed.Tag - 1];
      end
      else
        if ctrl is TPanel then
        begin
          pnl := TPanel (ctrl);
          pnl.Color := fData.fKeywordColors [pnl.Tag - 1];
          pnl.Font.Color := fData.fKeywordColors [pnl.Tag - 1]
        end
  end
end;

procedure TfmPropertyPageKeywords.KeywordColorButtonClick(Sender: TObject);
var
  pnlBtn : TPanel;
  clr : TColor;
begin
  if Populating then Exit;
  if not (Sender is TPanel) then Exit;

  pnlBtn := TPanel (Sender);

  ColorDialog1.Color := fData.fKeywordColors [TPanel (Sender).Tag - 1];

  if ColorDialog1.Execute then
  begin
    clr := ColorDialog1.Color;
    case pnlBtn.Tag - 1 of
      0 : begin pnlKeyPhrase0.Color := clr; pnlKeyPhrase0.Font.Color := clr; end;
      1 : begin pnlKeyPhrase1.Color := clr; pnlKeyPhrase1.Font.Color := clr; end;
      2 : begin pnlKeyPhrase2.Color := clr; pnlKeyPhrase2.Font.Color := clr; end;
      3 : begin pnlKeyPhrase3.Color := clr; pnlKeyPhrase3.Font.Color := clr; end;
      4 : begin pnlKeyPhrase4.Color := clr; pnlKeyPhrase4.Font.Color := clr; end;
      5 : begin pnlKeyPhrase5.Color := clr; pnlKeyPhrase5.Font.Color := clr; end;
      6 : begin pnlKeyPhrase6.Color := clr; pnlKeyPhrase6.Font.Color := clr; end;
      7 : begin pnlKeyPhrase7.Color := clr; pnlKeyPhrase7.Font.Color := clr; end;
    end;
    fData.fKeywordColors [pnlBtn.Tag - 1] := ColorDialog1.Color
  end
end;

{ TPropertyPageKeywordData }

function TPropertyPageKeywordData.Apply : boolean;
var
  i : Integer;
begin
  result := True;
  for i := 0 to 7 do
  begin
    XNOptions.KeywordColors [i] := fKeywordColors [i];
    XNOptions.KeyPhrase [i] := fKeyPhrase [i]
  end
end;

procedure TPropertyPageKeywordData.Initialize;
var
  i : Integer;
begin
  for i := 0 to 7 do
  begin
    fKeywordColors [i] := XNOptions.KeywordColors [i];
    fKeyPhrase [i] := XNOptions.KeyPhrase [i]
  end
end;

procedure TfmPropertyPageKeywords.EditChange(Sender: TObject);
var
  ed : TEdit;
  idx : Integer;
begin
  if Populating then Exit;
  if not (Sender is TEdit) then Exit;

  ed := TEdit (Sender);
  idx := ed.Tag - 1;

  fData.fKeyPhrase [idx] := ed.Text
end;

end.
