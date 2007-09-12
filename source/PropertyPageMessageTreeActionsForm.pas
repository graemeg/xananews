unit PropertyPageMessageTreeActionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageMessageTreeActionsData = class (TPropertyPageData)
  private
    fAutoExpandThread : boolean;
    fAutoCentralizeMessage : boolean;
    fAutoExpandAll : boolean;
    fAutoDownloadOnClick : boolean;
    fAutoMarkAsRead : boolean;
    fMarkAsReadTime : Integer;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageMessageTreeActions = class(TfmPropertyPage)
    cbAutoExpandThread: TCheckBox;
    cbAutoCentralizeMessage: TCheckBox;
    cbAutoExpandAll: TCheckBox;
    cbAutoDownloadOnClick: TCheckBox;
    cbAutoMarkAsRead: TCheckBox;
    Label14: TLabel;
    edAutoMarkSeconds: TEdit;
    Label30: TLabel;
    procedure ControlClick(Sender: TObject);
  private
    fData : TPropertyPageMessageTreeActionsData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageMessageTreeActions: TfmPropertyPageMessageTreeActions;

implementation

uses unitNNTPServices, unitNewsReaderOptions;

{$R *.dfm}

{ TfmPropertyPageMessageTreeActions }

class function TfmPropertyPageMessageTreeActions.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageMessageTreeActionsData;
end;

procedure TfmPropertyPageMessageTreeActions.PopulateControls(
  AData: TPropertyPageData);
begin
  inherited;

  fData := TPropertyPageMessageTreeActionsData (AData);

  cbAutoExpandThread.Checked := fData.fAutoExpandThread;
  cbAutoCentralizeMessage.Checked := fData.fAutoCentralizeMessage;
  cbAutoExpandAll.Checked := fData.fAutoExpandAll;
  cbAutoDownloadOnClick.Checked := fData.fAutoDownloadOnClick;
  cbAutoMarkAsRead.Checked := fData.fAutoMarkAsRead;
  edAutoMarkSeconds.Text := IntToStr (fData.fMarkAsReadTime)
end;

{ TPropertyPageMessageTreeActionsData }

function TPropertyPageMessageTreeActionsData.Apply : boolean;
begin
  result := True;
  Options.AutoExpandThread := fAutoExpandThread;
  Options.AutoCentralizeMessage := fAutoCentralizeMessage;
  Options.AutoExpandAll := fAutoExpandAll;
  Options.AutoDownloadOnClick := fAutoDownloadOnClick;
  Options.AutoMarkAsRead := fAutoMarkAsRead;
  Options.AutoMarkSeconds := fMarkAsReadTime;
end;

procedure TPropertyPageMessageTreeActionsData.Initialize;
begin
  fAutoExpandThread := Options.AutoExpandThread;
  fAutoCentralizeMessage := Options.AutoCentralizeMessage;
  fAutoExpandAll := Options.AutoExpandAll;
  fAutoDownloadOnClick := Options.AutoDownloadOnClick;
  fAutoMarkAsRead := Options.AutoMarkAsRead;
  fMarkAsReadTime := Options.AutoMarkSeconds;
end;

procedure TfmPropertyPageMessageTreeActions.ControlClick(Sender: TObject);
begin
  if Populating then Exit;
  fData.fAutoExpandThread := cbAutoExpandThread.Checked;
  fData.fAutoCentralizeMessage := cbAutoCentralizeMessage.Checked;
  fData.fAutoExpandAll := cbAutoExpandAll.Checked;
  fData.fAutoDownloadOnClick := cbAutoDownloadOnClick.Checked;
  fData.fAutoMarkAsRead := cbAutoMarkAsRead.Checked;
  fData.fMarkAsReadTime := StrToIntDef (edAutoMarkSeconds.Text, 0);
end;

end.
