unit unitMailServices;

interface

uses Windows, Classes, SysUtils, ConTnrs, unitNNTPServices, unitCharsetMap, NewsGlobals, unitIdentities, MultiMon, unitSettings, unitMessages;

type
TMailAccounts = class;
TMailAccount = class;

TMailAccount = class (TServerAccount)
private
  fOwner: TMailAccounts;

  fIdentity : TIdentity;
  fPostingSettings: TPostingSettings;
  fServerSettings : TServerSettings;
protected
  function GetServerSettings : TServerSettings; override;
  function GetPostingSettings : TPostingSettings; override;
  function GetIdentity: TIdentity; override;
  procedure SetName(const Value: string); override;
public
  constructor Create (AOwner : TMailAccounts);
  destructor Destroy; override;
  procedure SetIdentityName(const Value: string);
  procedure CopySettingsFrom (account : TMailAccount);

  property Owner : TMailAccounts read fOwner;

  property Identity : TIdentity read GetIdentity;
end;

TMailAccounts = class
private
  fAccounts : TStringList;
  function GetCount: Integer;
  function GetItems(idx: Integer): TMailAccount;
public
  constructor Create;
  destructor Destroy; override;

  procedure LoadFromRegistry;
  procedure SaveToRegistry;

  procedure Add (account : TMailAccount);
  procedure Delete (account : TMailAccount);
  procedure Rename (account : TMailAccount; const oldName : string);
  function FindMailAccount (const accountName : string) : TMailAccount;
  function FindMailAccountServer (const serverName : string) : TMailAccount;

  property Count : Integer read GetCount;
  property Items [idx : Integer] : TMailAccount read GetItems; default;
end;

var
  MailAccounts : TMailAccounts;

implementation

uses unitExSettings, IdGlobal, unitSearchString;


{ TMailAccounts }

procedure TMailAccounts.Add(account: TMailAccount);
begin
  account.fOwner := self;
  fAccounts.AddObject(account.Name, account)
end;

constructor TMailAccounts.Create;
begin
  fAccounts := TStringList.Create;
end;

procedure TMailAccounts.Delete(account: TMailAccount);
var
 idx : Integer;
begin
  idx := fAccounts.IndexOfObject(account);
  if idx >= 0 then
  begin
    fAccounts.Delete(idx);
    FreeAndNil (account);
    SaveToRegistry
  end
end;

destructor TMailAccounts.Destroy;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    Items [i].Free;

  fAccounts.Free;
  inherited
end;

function TMailAccounts.FindMailAccount(const accountName: string): TMailAccount;
var
  i : Integer;
begin
  result := Nil;
  for i := 0 to Count - 1 do
    if Items [i].Name = accountName then
    begin
      result := Items [i];
      break
    end
end;

function TMailAccounts.FindMailAccountServer(
  const serverName: string): TMailAccount;
var
  i : Integer;
begin
  result := Nil;
  for i := 0 to Count - 1 do
    if Items [i].ServerSettings.ServerName = serverName then
    begin
      result := Items [i];
      break
    end
end;

function TMailAccounts.GetCount: Integer;
begin
  result := fAccounts.Count
end;

function TMailAccounts.GetItems(idx: Integer): TMailAccount;
begin
  if idx < fAccounts.Count then
    result := fAccounts.Objects [idx] as TMailAccount
  else
    result := Nil
end;

procedure TMailAccounts.LoadFromRegistry;
var
  reg, reg1 : TExSettings;
  keyNames, keyNames1 : TStringList;
  i : Integer;

  account : TMailAccount;

begin
  keyNames := Nil;
  keyNames1 := nil;
  reg1 := Nil;
  reg := CreateExSettings;
  try
    reg.Section := 'Mail Accounts';
    if reg.Open (true) then
    begin
      keyNames := TStringList.Create;
      reg1 := CreateChildSettings (reg);

      reg.GetSectionNames(keyNames);

      for i := 0 to keyNames.Count - 1 do

      begin                     // For each account...
        reg1.Section := keyNames [i];

        account := TMailAccount.Create (self);
        try
          account.Name := keyNames [i];

          account.fPostingSettings.ReadSettings (reg1);
          account.fServerSettings.ReadSettings (reg1);
          Add (account);
        except
          account.Free
        end
      end
    end

  finally
    reg1.Free;
    keyNames.Free;
    keyNames1.Free;
    reg.Free;
  end
end;

procedure TMailAccounts.Rename(account: TMailAccount;
  const oldName: string);
var
  reg : TExSettings;
begin
  reg := CreateExSettings;
  try
    reg.Section := 'Mail Accounts';

    reg.RenameSection (oldName, account.Name);

    try
      if DirectoryExists (oldName) then
        if not RenameFile (oldNAme, account.Name) then
          Raise Exception.Create ('Cannot rename directory')
    except
      reg.RenameSection (account.Name, oldName);
      raise
    end
  finally
    reg.Free
  end
end;

procedure TMailAccounts.SaveToRegistry;
var
  reg, reg1 : TExSettings;
  i, p : Integer;

  account : TMailAccount;
  actionName : string;
  keyNames: TStringList;
begin
  reg1 := Nil;
  reg := Nil;
  keyNames := nil;

  fAccounts.Sort;

  try
    keyNames := TStringList.Create;
    reg := CreateExSettings;
    reg.Section := 'Mail Accounts';
    reg.GetSectionNames(keyNames);
    reg1 := CreateChildSettings (reg);

    for i := 0 to Count - 1 do
    begin
      account := Items [i];

      p := keyNames.IndexOf(account.Name);
      if p >= 0 then
        keyNames.Delete (p);

      reg1.Section := account.Name;

      reg1.SetStringValue ('Identity', account.Identity.Name, NNTPAccounts.Identities.DefaultIdentity.Name);

      account.fPostingSettings.WriteSettings (reg1);
      account.fServerSettings.ServerLogonRequired := True;
      account.fServerSettings.WriteSettings (reg1);
    end;

    if Assigned (keyNames) then
      for i:= 0 to keyNames.Count - 1 do
      begin
        actionName := keyNames [i];
        reg.DeleteSection(keyNames [i]);

        if DirectoryExists (gMessageBaseRoot + '\Mail\' + actionName) then
          PurgeDirectory (gMessageBaseRoot + '\Mail\' + actionName)
      end;

  finally
    reg1.Free;
    reg.Free;
    keyNames.Free;
  end
end;

{ TMailAccount }

constructor TMailAccount.Create(AOwner: TMailAccounts);
begin
  fOwner := AOwner;
  inherited Create ('');
  fPostingSettings := TPostingSettings.Create(NNTPAccounts.PostingSettings);
  fServerSettings := TSMTPServerSettings.Create(nil);
end;

destructor TMailAccount.Destroy;
begin
  fPostingSettings.Free;
  fServerSettings.Free;
  inherited;
end;


function TMailAccount.GetIdentity: TIdentity;
begin
  if Assigned (fIdentity) then
    result := fIdentity
  else
    result := NNTPAccounts.Identities.DefaultIdentity
end;

procedure TMailAccount.SetIdentityName(const Value: string);
begin
  fIdentity := NNTPAccounts.Identities.Find(Value);
  if fIdentity = NNTPAccounts.Identities.DefaultIdentity then
    fIdentity := Nil
end;

function TMailAccount.GetServerSettings: TServerSettings;
begin
  result := fServerSettings
end;

function TMailAccount.GetPostingSettings: TPostingSettings;
begin
  result := fPostingSettings;
end;

procedure TMailAccount.SetName(const Value: string);
var
  oldName : string;
begin
  if value <> Name then
  begin
    if Name = '' then
      inherited
    else
    begin
      if RenameFile (gMessageBaseRoot + '\Mail\' + Name, gMessageBaseRoot + '\Mail\' + Value) then
      begin
        oldName := Name;
        inherited;
        MailAccounts.SaveToRegistry;
      end
      else
        RaiseLastOSError
    end
  end
end;

procedure TMailAccount.CopySettingsFrom(account: TMailAccount);
begin
  self.SetIdentityName(account.Identity.Name);
  self.fPostingSettings.Assign(account.fPostingSettings);
  self.fServerSettings.Assign(account.fServerSettings);
end;

end.
