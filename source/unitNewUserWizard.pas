unit unitNewUserWizard;

interface

uses Windows, Classes, SysUtils, Controls, unitExSettings;

function DoNewUserConfig (const keyName : string) : boolean;

implementation

uses NewUserWizardForm, NewsGlobals, SplashForm;

function  DoNewUserConfig (const keyName : string) : boolean;
var
  dlg : TfrmNewUserWizard;
  reg : TExSettings;
begin
  dlg := TfrmNewUserWizard.Create(nil);
  try
    if Assigned(fmSplash) then
      fmSplash.Visible := False;
    if dlg.ShowModal = mrOK then
    begin
      reg := CreateEXSettings;
      try
        reg.Section := 'Identities\Default Identity';
        try
          reg.SetBooleanValue ('Default', True);
          reg.SetStringValue ('User Name', dlg.edYourName.Text);
          reg.SetStringValue ('EMail Address', dlg.edYourEMail.Text);
        finally
          reg.Close
        end;

        reg.Section := 'Accounts\' + dlg.edAccountName.Text;
        try
          reg.SetStringValue('Server Name', dlg.edServerName.Text);

          if dlg.cbServerLogon.Checked then
          begin
            reg.SetStringValue('Server Account Name', dlg.edServerUserName.Text);
            reg.SetStringValue('Server Password', dlg.edServerPassword.Text)
          end;

          if dlg.cbAlwaysConnectUsing.Checked then
            reg.SetStringValue ('RAS Connection', dlg.cbRasEntries.Text);
        finally
          reg.Close;
        end;

        reg.Section := '';
        reg.SetBooleanValue ('Messagebase Directory Confirmed', True);
      finally
        reg.Free
      end;
      result := True;
      if Assigned(fmSplash) then
        fmSplash.Visible := True;
    end
    else
      result := False
  finally
    dlg.Free
  end
end;

end.
