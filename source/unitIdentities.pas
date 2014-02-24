(*======================================================================*
 | unitIdentities                                                       |
 |                                                                      |
 | XanaNews unit to manage identities.                                  |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      15/11/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitIdentities;

interface

uses
  Windows, Classes, SysUtils, StrUtils, ConTnrs, unitExSettings, XnClasses;

type
  TIdentity = class
  private
    fOrganization: string;
    fXface: string;
    fEMailAddress: string;
    fName: string;
    fReplyAddress: string;
    fSignature: string;
    fUserName: string;
    fIsDefault: Boolean;
    fSigFile: string;

    procedure Load(rootReg: TExSettings; const AName: string);
    procedure Save(rootReg: TExSettings);
  public
    procedure ChangeName(const newName: string);
    property Name: string read fName;
    property EMailAddress: string read fEMailAddress write fEMailAddress;
    property ReplyAddress: string read fReplyAddress write fReplyAddress;
    property Organization: string read fOrganization write fOrganization;
    property Signature: string read fSignature write fSignature;
    property UserName: string read fUserName write fUserName;
    property XFace: string read fXface write fXFace;
    property IsDefault: Boolean read fIsDefault write fIsDefault;
    property SigFile: string read fSigFile write fSigFile;
    function ChooseSignature(sigOverride: string): string;
    function GetFullyQualifiedSigFilename(const filename: string): string;
    procedure AssignTo(id: TIdentity);
  end;

  TIdentities = class
  private
    fIdentities: TObjectList;
    function GetCount: Integer;
    function GetIdentity(idx: Integer): TIdentity;
    function CreateDefaultIdentity: Boolean;
    function GetDefaultIdentity: TIdentity;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Identity[idx: Integer]: TIdentity read GetIdentity; default;
    property DefaultIdentity: TIdentity read GetDefaultIdentity;
    procedure Load(rootReg: TExSettings);
    procedure Save;
    function Find(const name: string): TIdentity;
    procedure Add(Identity: TIdentity);
    function Delete(idx: Integer): Boolean;
  end;

implementation

uses
  unitNNTPServices, NewsGlobals, IdentityDialog, unitStreamTextReader;

{ TIdentities }

procedure TIdentities.Add(Identity: TIdentity);
begin
  fIdentities.Add(Identity);
end;

constructor TIdentities.Create;
begin
  fIdentities := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | procedure TIdentities.CreateDefaultIdentity                          |
 |                                                                      |
 | This is called when migrating from previous versions of XanaNews     |
 | that didn't have multiple identities.  Get the user to fill in       |
 | details for the initial Default Identity                             |
 *----------------------------------------------------------------------*)
function TIdentities.CreateDefaultIdentity: Boolean;
var
  dlg: TdlgIdentity;
begin
  Result := False;
  dlg := TdlgIdentity.Create(nil);
  try
    dlg.Identity := TIdentity.Create;
    dlg.DefaultIdentity := True;
    try
      if (dlg.ShowModal = idOK) and (dlg.Identity.Name <> '') then
      begin
        fIdentities.Add(dlg.Identity);
        dlg.Identity.IsDefault := True;
        Save;
        Result := True;
      end
      else
        dlg.Identity.Free;
    except
      dlg.Identity.Free;
      raise;
    end;
  finally
    dlg.Free;
  end;
end;

function TIdentities.Delete(idx: Integer): Boolean;
var
  id: TIdentity;
  defaultName: string;
  acct: TNNTPAccount;
  grp: TSubscribedGroup;
  i, j: Integer;
begin
  defaultName := DefaultIdentity.Name;
  id := Identity[idx];
  Result := False;
  if not id.IsDefault then
  begin
                      // Check that nothing's using this identity
                      // before we delete it.  If it is, then make it
                      // use the default identity instead.
    for i := 0 to NNTPAccounts.Count - 1 do
    begin
      acct := NNTPAccounts.Items[i];
      if acct.NNTPSettings.Identity = id then
        acct.NNTPSettings.SetIdentityName(defaultName);

      for j := 0 to acct.SubscribedGroupCount - 1 do
      begin
        grp := acct.SubscribedGroups[j];
        if grp.NNTPSettings.Identity = id then
          grp.NNTPSettings.SetIdentityName(defaultName);
      end;
    end;
    fIdentities.Delete(idx);
    Result := True;
  end;
end;

destructor TIdentities.Destroy;
begin
  fIdentities.Free;
  inherited Destroy;
end;

function TIdentities.Find(const name: string): TIdentity;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(name, Identity[i].Name) = 0 then
    begin
      Result := Identity[i];
      Break;
    end;
end;

function TIdentities.GetCount: Integer;
begin
  Result := fIdentities.Count;
end;

function TIdentities.GetDefaultIdentity: TIdentity;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Identity[i].IsDefault then
    begin
      Result := Identity[i];
      Break;
    end;
end;

function TIdentities.GetIdentity(idx: Integer): TIdentity;
begin
  Result := TIdentity(fIdentities[idx]);
end;

procedure TIdentities.Load(rootReg: TExSettings);
var
  keyNames: TStrings;
  i: Integer;
  identity: TIdentity;
  gotDefault: Boolean;
begin
  keyNames := nil;
  gotDefault := False;
  try
    if rootReg.HasSection('Identities') then
    begin
      rootReg.Section := 'Identities';
      keyNames := TStringList.Create;
      rootReg.GetSectionNames(keyNames);

      for i := 0 to keyNames.Count - 1 do
      begin
        identity := TIdentity.Create;
        identity.Load(rootReg, keyNames[i]);
        fIdentities.Add(identity);
        if identity.IsDefault then
          gotDefault := True;
      end;

      if not gotDefault and (Count > 0) then
        GetIdentity(0).IsDefault := True;
    end
  finally
    rootReg.Section := '';
    keyNames.Free;
  end;

  if Count = 0 then
    if not CreateDefaultIdentity then
      Halt;
end;

procedure TIdentities.Save;
var
  oldIdentities: TStringList;
  reg: TExSettings;
  i, idx: Integer;
  id: TIdentity;
begin
  oldIdentities := nil;
  reg := CreateExSettings;
  try
    reg.Section := 'Identities';

    // Create list of existing registry identities so we can tidy up by deleting
    // ones that no longer exist.
    oldIdentities := TStringList.Create;
    oldIdentities.CaseSensitive := False;
    reg.GetSectionNames(oldIdentities);

    // Save the identities
    for i := 0 to Count - 1 do
    begin
      id := Identity[i];
      id.Save(reg);
      idx := oldIdentities.IndexOf(id.Name);
      if idx >= 0 then
        oldIdentities.Delete(idx);
    end;

    // Delete identities that no loner exist.
    for i := 0 to oldIdentities.Count - 1 do
      reg.DeleteSection(oldIdentities[i]);
  finally
    reg.Free;
    oldIdentities.Free;
  end;
end;

{ TIdentity }

procedure TIdentity.AssignTo(id: TIdentity);
begin
  id.fOrganization := Self.fOrganization;
  id.fXface := Self.fXface;
  id.fEMailAddress := Self.EMailAddress;
  id.fName := Self.fName;
  id.fReplyAddress := Self.fReplyAddress;
  id.fSignature := Self.fSignature;
  id.fUserName := Self.fUserName;
  id.fIsDefault := False;
  id.fSigFile := Self.fSigFile
end;

(*----------------------------------------------------------------------*
 | procedure TIdentity.ChangeName                                       |
 |                                                                      |
 | Change the name of an identity.  I didn't want the 'Name' property   |
 | to be read/write because it may confuse things.                      |
 *----------------------------------------------------------------------*)
procedure TIdentity.ChangeName(const newName: string);
begin
  fName := newName;
end;

function TrimRightWhitespace(const S, WhiteSpace: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (Pos(S[I], WhiteSpace) > 0) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function TIdentity.ChooseSignature(sigOverride: string): string;
var
  filename: string;
  f: TFileStream;
  rdr: TStreamTextReader;
  idx: TList;
  st: string;
  raw: RawByteString;
  sig: string;
  useSigFile: Boolean;

  function GetSig(pos: Integer): string;
  var
    st: string;
  begin
    rdr.Position := pos;
    Result := '';
    st := '';
    while rdr.ReadLn(raw) do
    begin
      st := string(raw);
      if st <> '.' then
        Result := Result + st + #13#10
      else
        Exit;
    end;
    if raw <> '' then
      Result := Result + string(raw) + #13#10;
  end;

begin
  sigOverride := TrimRightWhitespace(sigOverride, #13#10#9' ');
  if sigOverride <> '' then   // Use the override value, but still insert
  begin                       // the sigfile info if there's a %sigfile%
    sig := sigOverride;
    useSigFile := ContainsText(sig, '%sigfile%');
  end
  else
  begin                       // For compatibility with previous versions,
    sig := Signature;         // use the sigfile value in preference to
    useSigFile := True;       // this one.  But if this value contains
  end;                        // '%sigfile', then use both.

  filename := GetFullyQualifiedSigFilename(fSigFile);
  if useSigFile and (fSigFile <> '') and FileExists(filename) then
  begin
    rdr := nil;
    idx := nil;
    f := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
    try
      rdr := TStreamTextReader.Create(f);
      idx := TList.Create;

      idx.Add(Pointer(0));
      while rdr.ReadLn(raw) do
        if raw = '.' then
          idx.Add(Pointer(rdr.Position));

      if idx.Count = 1 then
        st := GetSig(0)
      else
        st := GetSig(Integer(idx[Random(idx.Count)]));

      st := TrimRightWhitespace(st, #13#10#9' ');

      if ContainsText(sig, '%sigfile%') then
        Result := StringReplace(sig, '%sigfile%', st, [])
      else
        Result := st;
    finally
      rdr.Free;
      f.Free;
      idx.Free;
    end;
  end
  else
    Result := StringReplace(sig, '%sigfile%', '', []);

  Result := TrimRightWhitespace(Result, #13#10#9' ') + #13#10;
end;

function TIdentity.GetFullyQualifiedSigFilename(const filename: string): string;
begin
  if (filename <> '') and (Pos(':', filename) = 0) and (Copy(filename, 1, 2) <> '\\') then
  begin
    if filename[1] = '\' then
      Result := ExtractFileDrive(gXanaNewsDir) + filename
    else
      Result := ExpandFileName(gXanaNewsDir + '\' + filename);
  end
  else
    Result := filename;
end;

procedure TIdentity.Load(rootReg: TExSettings; const AName: string);
var
  reg: TExSettings;
begin
  fName := AName;
  reg := CreateChildSettings(rootReg, AName);
  try
    fUserName := reg.StringValue['User Name'];
    fEMailAddress := reg.StringValue['EMail Address'];
    fReplyAddress := reg.StringValue['Reply Address'];
    fOrganization := reg.StringValue['Organization'];
    fSignature := reg.StringValue['Signature'];
    fSigFile := reg.StringValue['SigFile'];
    fXFace := StringReplace(reg.StringValue['XFace'], #13#10' ', '', [rfReplaceAll]);
    fIsDefault := reg.BooleanValue['Default'];
  finally
    reg.Free;
  end;
end;

procedure TIdentity.Save(rootReg: TExSettings);
var
  reg: TExSettings;
begin
  reg := CreateChildSettings(rootReg, Name);
  try
    reg.StringValue['User Name'] := UserName;
    reg.StringValue['EMail Address'] := EMailAddress;
    reg.StringValue['Reply Address'] := ReplyAddress;
    reg.StringValue['Organization'] := Organization;
    reg.StringValue['Signature'] := Signature;
    reg.StringValue['SigFile'] := fSigFile;
    reg.StringValue['XFace'] := XFace;
    reg.BooleanValue['Default'] := fIsDefault;
  finally
    reg.Free;
  end;
end;

end.
