(*======================================================================*
 | IdentitiesDialog unit for NewsReader3                                |
 |                                                                      |
 | Allow the user to set up identities                                  |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      14/11/2002  CPWW  Original                                  |
 *======================================================================*)

unit IdentitiesDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, unitNNTPServices, unitIdentities;

type
  TdlgIdentities = class(TForm)
    lvIdentities: TListView;
    btnAdd: TButton;
    btnRemove: TButton;
    btnProperties: TButton;
    btnClose: TButton;
    btnSetDefault: TButton;
    btnClone: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure btnSetDefaultClick(Sender: TObject);
    procedure btnCloneClick(Sender: TObject);
  private
    fIdentity : TIdentity;
    function EditIdentity (identity : TIdentity) : Boolean;
    procedure PopulateForm;
  protected
    procedure UpdateActions; override;
    { Private declarations }
  public
    property Identity : TIdentity read fIdentity;
    { Public declarations }
  end;

var
  dlgIdentities: TdlgIdentities;

implementation

uses IdentityDialog, NewsGlobals;

{$R *.dfm}

function IsIdentityUnique (const id : string; excep : TIdentity = Nil) : boolean;
var
  i : Integer;
begin
  result := True;
  for i := 0 to NNTPAccounts.Identities.Count - 1 do
    if SameText (NNTPAccounts.Identities.Identity [i].Name, id) then
      if NNTPAccounts.Identities.Identity [i] <> excep then
      begin
        result := False;
        break
      end
end;

function GenerateUniqueIdentity (orig : string) : string;
var
  n : Integer;
  ok : boolean;
begin
  n := Pos ('(', orig);
  if n > 0 then
    orig := Copy (orig, 1, n - 1);

  ok := False;
  n := 1;

  while not ok do
  begin
    result := Format ('%s (%d)', [orig, n]);
    ok := IsIdentityUnique (result);
    Inc (n)
  end
end;

{ TdlgIdentities }

procedure TdlgIdentities.UpdateActions;
var
  selected : Boolean;
  Identity : TIdentity;
begin
  selected := Assigned (lvIdentities.Selected);
  if selected then
    identity := TIdentity(lvIdentities.Selected.Data)
  else
    identity := Nil;
  btnRemove.Enabled := selected and not Identity.IsDefault;
  btnProperties.Enabled := selected;
  btnClone.Enabled := selected;
  btnSetDefault.Enabled := selected;
end;

procedure TdlgIdentities.btnAddClick(Sender: TObject);
var
  identity : TIdentity;
begin
  identity := TIdentity.Create;
  try
    if EditIdentity (identity) and (identity.Name <> '') and IsIdentityUnique (identity.Name) then
    begin
      NNTPAccounts.Identities.Add (identity);
      NNTPAccounts.SaveToRegistry;
      with lvIdentities.Items.Add do
      begin
        Caption := identity.Name;
        SubItems.Add(identity.UserName);
        SubItems.Add(identity.EMailAddress);
        data := identity
      end
    end
    else
      identity.Free
  except
    identity.Free;
    raise
  end
end;

function TdlgIdentities.EditIdentity (identity : TIdentity) : Boolean;
var
  dlg : TdlgIdentity;
begin
  Result := False;
  Application.CreateForm(TdlgIdentity, dlg);
  try
    dlg.Identity := identity;
    if dlg.ShowModal = mrOK then
      Result := True
   finally
    dlg.Free
  end
end;

procedure TdlgIdentities.FormShow(Sender: TObject);
begin
  AdjustFormConstraints (self);
  PopulateForm
end;

procedure TdlgIdentities.btnRemoveClick(Sender: TObject);
var
  idx : Integer;
begin
  if Assigned (lvIdentities.Selected) then
  begin
    idx := lvIdentities.Selected.Index;

    if idx >= 0 then
    begin
      if NNTPAccounts.Identities.Delete(idx) then
      begin
        NNTPAccounts.Identities.Save;
        lvIdentities.DeleteSelected
      end
      else
        ShowMessage ('Unable to delete default identity');
    end
  end
end;

procedure TdlgIdentities.btnPropertiesClick(Sender: TObject);
var
  identity : Tidentity;
begin
  if Assigned (lvIdentities.Selected) then
  begin
    identity := TIdentity(lvIdentities.Selected.Data);
    if EditIdentity (identity) and (identity.Name <> '') and IsIdentityUnique (identity.Name, identity) then
    begin
      NNTPAccounts.Identities.Save;

      if identity.IsDefault then
        lvIdentities.Selected.Caption := '* ' + identity.Name
      else
        lvIdentities.Selected.Caption := identity.Name;
      lvIdentities.Selected.SubItems [0] := identity.UserName;
      lvIdentities.Selected.SubItems [1] := identity.EMailAddress
    end
  end
end;

procedure TdlgIdentities.btnSetDefaultClick(Sender: TObject);
var
  i : Integer;
  identity : TIdentity;
begin
  if Assigned (lvIdentities.Selected) then
  begin
    identity := TIdentity(lvIdentities.Selected.Data);
    for i := 0 to NNTPAccounts.Identities.Count - 1 do
      NNTPAccounts.Identities [i].IsDefault := False;
    identity.IsDefault := True;
    NNTPAccounts.Identities.Save;
    PopulateForm;
  end
end;

procedure TdlgIdentities.PopulateForm;
var
  i : Integer;
  idx : Integer;
begin
  if Assigned (lvIdentities.Selected) then
    idx := lvIdentities.Selected.Index
  else
    idx := 0;

  lvIdentities.Items.BeginUpdate;
  try
    lvIdentities.Clear;
    for i := 0 to NNTPAccounts.Identities.Count - 1 do
      with lvIdentities.Items.Add do
      begin
        if NNTPAccounts.Identities [i].IsDefault then
          Caption := '* ' + NNTPAccounts.Identities [i].Name
        else
          Caption := NNTPAccounts.Identities [i].Name;
        SubItems.Add(NNTPAccounts.Identities [i].UserName);
        SubItems.Add(NNTPAccounts.Identities [i].EMailAddress);
        Data := NNTPAccounts.Identities [i]
      end;
  finally
    lvIdentities.Items.EndUpdate
  end;

  if idx < lvIdentities.Items.Count then
    lvIdentities.Items [idx].Selected := True
end;

procedure TdlgIdentities.btnCloneClick(Sender: TObject);
var
  oldid, identity : TIdentity;
begin
  if Assigned (lvIdentities.Selected) then
  begin
    oldid := TIdentity(lvIdentities.Selected.Data);

    identity := TIdentity.Create;
    try
      oldid.AssignTo (identity);
      identity.ChangeName (GenerateUniqueIdentity (identity.Name));

      if EditIdentity (identity) and (identity.Name <> '') and IsIdentityUnique (identity.Name) then
      begin
        NNTPAccounts.Identities.Add (identity);
        NNTPAccounts.SaveToRegistry;
        with lvIdentities.Items.Add do
        begin
          Caption := identity.Name;
          SubItems.Add(identity.UserName);
          SubItems.Add(identity.EMailAddress);
          data := identity
        end
      end
      else
        identity.Free
    except
      identity.Free;
      raise
    end
  end
end;

end.
