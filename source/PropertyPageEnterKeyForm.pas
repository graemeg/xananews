unit PropertyPageEnterKeyForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, ExtCtrls, StdCtrls;

type
  TPropertyPageEnterKeyData = class (TPropertyPageData)
  private
    fEnterGetMessages : boolean;
    fEnterLoadsMessage : boolean;
    fEnterGoToNextGroup : boolean;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageEnterKey = class(TfmPropertyPage)
    Label8: TLabel;
    rbEnterGetMessages: TRadioButton;
    rbEnterGoToMessageTree: TRadioButton;
    Label9: TLabel;
    cbEnterGoToNextGroup: TCheckBox;
    cbEnterLoadsMessage: TCheckBox;
    procedure ControlClick(Sender: TObject);
  private
    fData : TPropertyPageEnterKeyData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageEnterKey: TfmPropertyPageEnterKey;

implementation

uses unitNNTPServices, unitNewsReaderOptions;

{$R *.dfm}

{ TfmPropertyPageEnterKey }

class function TfmPropertyPageEnterKey.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageEnterKeyData;
end;

procedure TfmPropertyPageEnterKey.PopulateControls (AData : TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageEnterKeyData;

  if fData.fEnterGetMessages then
    rbEnterGetMessages.Checked := True
  else
    rbEnterGoToMessageTree.Checked := True;

  cbEnterGoToNextGroup.Checked := fData.fEnterGoToNextGroup;
  cbEnterLoadsMessage.Checked := fData.fEnterLoadsMessage
end;

{ TPropertyPageEnterKeyData }

function TPropertyPageEnterKeyData.Apply : boolean;
begin
  result := True;
  XNOptions.EnterGetMessages := fEnterGetMessages;
  XNOptions.EnterLoadsMessage := fEnterLoadsMessage;
  XNOptions.EnterGoToNextGroup := fEnterGotoNextGroup;
end;

procedure TPropertyPageEnterKeyData.Initialize;
begin
  fEnterGetMessages := XNOptions.EnterGetMessages;
  fEnterLoadsMessage := XNOptions.EnterLoadsMessage;
  fEnterGoToNextGroup := XNOptions.EnterGoToNextGroup;
end;

procedure TfmPropertyPageEnterKey.ControlClick(Sender: TObject);
begin
  if Populating then Exit;
  fData.fEnterGetMessages := rbEnterGetMessages.Checked;
  fData.fEnterLoadsMessage := cbEnterLoadsMessage.Checked;
  fData.fEnterGoToNextGroup := cbEnterGoToNextGroup.Checked;
end;

end.
