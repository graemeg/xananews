unit unitMailServices;

interface

uses
  Windows, Classes, SysUtils, ConTnrs, unitNNTPServices, unitCharsetMap,
  NewsGlobals, unitIdentities, MultiMon, unitSettings, unitMessages;

type
  TMailAccounts = class;
  TMailAccount = class;

  TMailAccount = class(TServerAccount)
  private
    fOwner: TMailAccounts;

    fIdentity: TIdentity;
    fPostingSettings: TPostingSettings;
    fServerSettings: TServerSettings;
  protected
    function GetServerSettings: TServerSettings; override;
    function GetPostingSettings: TPostingSettings; override;
    function GetIdentity: TIdentity; override;
    procedure SetName(const Value: string); override;
  public
    constructor Create(AOwner: TMailAccounts);
    destructor Destroy; override;
    procedure SetIdentityName(const Value: string);
    procedure CopySettingsFrom(const account: TMailAccount);

    property Owner: TMailAccounts read fOwner;

    property Identity: TIdentity read GetIdentity;
  end;

  TMailAccounts = class
  private
    fAccounts: TStringList;
    function GetCount: Integer;
    function GetItems(const idx: Integer): TMailAccount;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromRegistry;
    procedure SaveToRegistry;

    procedure Add(const account: TMailAccount);
    procedure Delete(const account: TMailAccount);
    procedure Rename(const account: TMailAccount; const oldName: string);
    function FindMailAccount(const accountName: string): TMailAccount;
    function FindMailAccountServer(const serverName: string): TMailAccount;

    property Count: Integer read GetCount;
    property Items[const idx: Integer]: TMailAccount read GetItems; default;
  end;

var
  MailAccounts: TMailAccounts;

implementation

uses
  unitExSettings, IdGlobal, unitSearchString;

{ TMailAccounts }

procedure TMailAccounts.Add(const account: TMailAccount);
begin
  account.fOwner := Self;
  fAccounts.AddObject(account.Name, account);
end;

constructor TMailAccounts.Create;
begin
  inherited Create;
  fAccounts := TStringList.Create;
end;

procedure TMailAccounts.Delete(const account: TMailAccount);
var
  idx: Integer;
begin
  idx := fAccounts.IndexOfObject(account);
  if idx >= 0 then
  begin
    fAccounts.Delete(idx);
    account.Free;
    SaveToRegistry;
  end;
end;

destructor TMailAccounts.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;

  fAccounts.Free;
  inherited Destroy;
end;

function TMailAccounts.FindMailAccount(const accountName: string): TMailAccount;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Name = accountName then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TMailAccounts.FindMailAccountServer(const serverName: string): TMailAccount;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items [i].ServerSettings.ServerName = serverName then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TMailAccounts.GetCount: Integer;
begin
  Result := fAccounts.Count;
end;

function TMailAccounts.GetItems(const idx: Integer): TMailAccount;
begin
  if idx < fAccounts.Count then
    Result := fAccounts.Objects[idx] as TMailAccount
  else
    Result := nil;
end;

procedure TMailAccounts.LoadFromRegistry;
var
  reg, reg1: TExSettings;
  keyNames, keyNames1: TStringList;
  i: Integer;
  account: TMailAccount;
begin
  keyNames := nil;
  keyNames1 := nil;
  reg1 := nil;
  reg := CreateExSettings;
  try
    reg.Section := 'Mail Accounts';
    if reg.Open(True) then
    begin
      keyNames := TStringList.Create;
      reg1 := CreateChildSettings(reg);

      reg.GetSectionNames(keyNames);

      for i := 0 to keyNames.Count - 1 do
      begin // For each account...
        reg1.Section := keyNames[i];

        account := TMailAccount.Create(Self);
        try
          account.Name := keyNames[i];
          account.SetIdentityName(reg1.GetStringValue('Identity', NNTPAccounts.Identities.DefaultIdentity.Name));
          account.fPostingSettings.ReadSettings(reg1);
          account.fServerSettings.ReadSettings(reg1);
          Add(account);
        except
          account.Free;
        end;
      end;
    end;
  finally
    reg1.Free;
    keyNames.Free;
    keyNames1.Free;
    reg.Free;
  end;
end;

procedure TMailAccounts.Rename(const account: TMailAccount; const oldName: string);
var
  reg: TExSettings;
begin
  reg := CreateExSettings;
  try
    reg.Section := 'Mail Accounts';

    reg.RenameSection(oldName, account.Name);

    try
      if DirectoryExists(oldName) then
        if not RenameFile(oldName, account.Name) then
          raise Exception.Create('Cannot rename directory');
    except
      reg.RenameSection(account.Name, oldName);
      raise;
    end;
  finally
    reg.Free;
  end;
end;

procedure TMailAccounts.SaveToRegistry;
var
  reg, reg1: TExSettings;
  i, p: Integer;
  account: TMailAccount;
  actionName: string;
  keyNames: TStringList;
begin
  reg1 := nil;
  reg := nil;
  keyNames := nil;

  fAccounts.Sort;

  try
    keyNames := TStringList.Create;
    reg := CreateExSettings;
    reg.Section := 'Mail Accounts';
    reg.GetSectionNames(keyNames);
    reg1 := CreateChildSettings(reg);

    for i := 0 to Count - 1 do
    begin
      account := Items[i];

      p := keyNames.IndexOf(account.Name);
      if p >= 0 then
        keyNames.Delete(p);

      reg1.Section := account.Name;

      reg1.SetStringValue('Identity', account.Identity.Name, NNTPAccounts.Identities.DefaultIdentity.Name);

      account.fPostingSettings.WriteSettings(reg1);
      account.fServerSettings.ServerLogonRequired := True;
      account.fServerSettings.WriteSettings(reg1);
    end;

    if Assigned(keyNames) then
      for i := 0 to keyNames.Count - 1 do
      begin
        actionName := keyNames[i];
        reg.DeleteSection(keyNames[i]);

        if DirectoryExists(gMessageBaseRoot + '\Mail\' + actionName) then
          PurgeDirectory(gMessageBaseRoot + '\Mail\' + actionName);
      end;

  finally
    reg1.Free;
    reg.Free;
    keyNames.Free;
  end;
end;

{ TMailAccount }

constructor TMailAccount.Create(AOwner: TMailAccounts);
begin
  fOwner := AOwner;
  inherited Create('');
  fPostingSettings := TPostingSettings.Create(NNTPAccounts.PostingSettings);
  fServerSettings := TSMTPServerSettings.Create(nil);
end;

destructor TMailAccount.Destroy;
begin
  fPostingSettings.Free;
  fServerSettings.Free;
  inherited Destroy;
end;

function TMailAccount.GetIdentity: TIdentity;
begin
  if Assigned(fIdentity) then
    Result := fIdentity
  else
    Result := NNTPAccounts.Identities.DefaultIdentity;
end;

procedure TMailAccount.SetIdentityName(const Value: string);
begin
  fIdentity := NNTPAccounts.Identities.Find(Value);
  if fIdentity = NNTPAccounts.Identities.DefaultIdentity then
    fIdentity := nil;
end;

function TMailAccount.GetServerSettings: TServerSettings;
begin
  Result := fServerSettings;
end;

function TMailAccount.GetPostingSettings: TPostingSettings;
begin
  Result := fPostingSettings;
end;

procedure TMailAccount.SetName(const Value: string);
var
  oldName: string;
begin
  if Value <> Name then
  begin
    if Name = '' then
      inherited
    else
    begin
      if RenameFile(gMessageBaseRoot + '\Mail\' + Name, gMessageBaseRoot + '\Mail\' + Value) then
      begin
        oldName := Name;
        inherited;
        MailAccounts.SaveToRegistry;
      end
      else
        RaiseLastOSError;
    end;
  end;
end;

procedure TMailAccount.CopySettingsFrom(const account: TMailAccount);
begin
  Self.SetIdentityName(account.Identity.Name);
  Self.fPostingSettings.Assign(account.fPostingSettings);
  Self.fServerSettings.Assign(account.fServerSettings);
end;

end.
