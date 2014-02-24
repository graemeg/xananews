unit PropertyPageConnectionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, StdCtrls, ExtCtrls;

type
  TPropertyPageConnectionData = class (TPropertyPageData)
  private
    fAutoDisconnectOnIdle : boolean;
    fAutoDisconnectOnExit : boolean;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
  end;

  TfmPropertyPageConnection = class(TfmPropertyPage)
    cbAutoDisconnectOnIdle: TCheckBox;
    cbAutoDisconnectOnExit: TCheckBox;
    procedure ControlsClick(Sender: TObject);
  private
    fData : TPropertyPageConnectionData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageConnection: TfmPropertyPageConnection;

implementation

uses unitNewsReaderOptions;

{$R *.dfm}

{ TfmPropertyPageConnection }


class function TfmPropertyPageConnection.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageConnectionData;
end;

procedure TfmPropertyPageConnection.PopulateControls (AData : TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageConnectionData;

  cbAutoDisconnectOnIdle.Checked := fData.fAutoDisconnectOnIdle;
  cbAutoDisconnectOnExit.Checked := fData.fAutoDisconnectOnExit;
end;

{ TPropertyPageConnectionData }

function TPropertyPageConnectionData.Apply : boolean;
begin
  result := True;
  fAutoDisconnectOnIdle := XNOptions.AutoDisconnectOnIdle;
  fAutoDisconnectOnExit := XNOptions.AutoDisconnectOnExit;
end;

procedure TPropertyPageConnectionData.Initialize;
begin
  XNOptions.AutoDisconnectOnIdle := fAutoDisconnectOnIdle;
  XNOptions.AutoDisconnectOnExit := fAutoDisconnectOnExit;
end;

procedure TfmPropertyPageConnection.ControlsClick(Sender: TObject);
begin
  if Populating then Exit;
  fData.fAutoDisconnectOnIdle := cbAutoDisconnectOnIdle.Checked;
  fData.fAutoDisconnectOnExit := cbAutoDisconnectOnExit.Checked
end;

end.
