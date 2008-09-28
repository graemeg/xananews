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
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      15/11/2002  CPWW  Original                                  |
 *======================================================================*)


unit unitIdentities;

interface

uses Windows, Classes, SysUtils, StrUtils, ConTnrs, unitExSettings;

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
    fIsDefault : boolean;
    fSigFile : string;

    procedure Load (rootReg : TExSettings; const AName : string);
    procedure Save (rootReg : TExSettings);
  public
    procedure ChangeName (const newName : string);
    property Name : string read fName;
    property EMailAddress : string read fEMailAddress write fEMailAddress;
    property ReplyAddress : string read fReplyAddress write fReplyAddress;
    property Organization : string read fOrganization write fOrganization;
    property Signature : string read fSignature write fSignature;
    property UserName : string read fUserName write fUserName;
    property XFace : string read fXface write fXFace;
    property IsDefault : boolean read fIsDefault write fIsDefault;
    property SigFile : string read fSigFile write fSigFile;
    function ChooseSignature (sigOverride : string): string;
    procedure AssignTo (id : TIdentity);
  end;

  TIdentities = class
  private
    fIdentities : TObjectList;
    function GetCount: Integer;
    function GetIdentity(idx: Integer): TIdentity;
    function CreateDefaultIdentity : boolean;
    function GetDefaultIdentity: TIdentity;
  public
    constructor Create;
    destructor Destroy; override;
    property Count : Integer read GetCount;
    property Identity [idx : Integer] : TIdentity read GetIdentity; default;
    property DefaultIdentity : TIdentity read GetDefaultIdentity;
    procedure Load (rootReg : TExSettings);
    procedure Save;
    function Find (const name : string) : TIdentity;
    procedure Add (Identity : TIdentity);
    function Delete (idx : Integer) : boolean;
  end;

implementation

uses unitNNTPServices, NewsGlobals, IdentityDialog, unitStreamTextReader;

{ TIdentities }

(*----------------------------------------------------------------------*
 | procedure TIdentities.Add                                            |
 |                                                                      |
 | Add an identity to the fIdentities list.                             |
 |                                                                      |
 | Parameters:                                                          |
 |   Identity: TIdentity        // The identity to add                  |
 *----------------------------------------------------------------------*)
procedure TIdentities.Add(Identity: TIdentity);
begin
  fIdentities.Add (Identity);
end;

(*----------------------------------------------------------------------*
 | constructor TIdentities.Create                                       |
 |                                                                      |
 | Constructor for TIdentities.  Create the fIdentities list.           |
 *----------------------------------------------------------------------*)

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
function TIdentities.CreateDefaultIdentity : boolean;
var
  dlg : TdlgIdentity;
begin
  result := False;
  dlg := TdlgIdentity.Create (Nil);
  try
    dlg.Identity := TIdentity.Create;
    dlg.DefaultIdentity := True;
    try
      if (dlg.ShowModal = idOK) and (dlg.Identity.Name <> '') then
      begin
        fIdentities.Add(dlg.Identity);
        dlg.Identity.IsDefault := True;
        Save;
        result := True;
      end
      else
        dlg.Identity.Free
    except
      dlg.Identity.Free;
      raise
    end
  finally
    dlg.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TIdentities.Delete                                         |
 |                                                                      |
 | Delete an identity                                                   |
 |                                                                      |
 | Parameters:                                                          |
 |   idx: Integer                       The identity to delete          |
 *----------------------------------------------------------------------*)
function TIdentities.Delete(idx: Integer) : boolean;
var
  id : TIdentity;
  defaultName : string;
  acct : TNNTPAccount;
  grp : TSubscribedGroup;

  i, j : Integer;
begin
  defaultName := DefaultIdentity.Name;
  id := Identity [idx];
  result := False;
  if not id.IsDefault then
  begin

                      // Check that nothing's using this identity
                      // before we delete it.  If it is, then make it
                      // use the default identity instead.
    for i := 0 to NNTPAccounts.Count - 1 do
    begin
      acct := NNTPAccounts.Items [i];
      if acct.NNTPSettings.Identity = id then
        acct.NNTPSettings.SetIdentityName(defaultName);

      for j := 0 to acct.SubscribedGroupCount - 1 do
      begin
        grp := acct.SubscribedGroups [j];
        if grp.NNTPSettings.Identity = id then
          grp.NNTPSettings.SetIdentityName (defaultName)
      end
    end;
    fIdentities.Delete(idx);
    result := True
  end
end;

(*----------------------------------------------------------------------*
 | destructor TIdentities.Destroy                                       |
 |                                                                      |
 | Destructor for TIdentities                                           |
 *----------------------------------------------------------------------*)
destructor TIdentities.Destroy;
begin
  fIdentities.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | function TIdentities.Find                                            |
 |                                                                      |
 | Find an identity by name.                                            |
 |                                                                      |
 | Parameters:                                                          |
 |   const name: string         // The name of the identity to find     |
 |                                                                      |
 | The function returns the identity - or Nil                           |
 *----------------------------------------------------------------------*)
function TIdentities.Find(const name: string): TIdentity;
var
  i : Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if CompareText (name, Identity [i].Name) = 0 then
    begin
      result := Identity [i];
      break
    end
end;

(*----------------------------------------------------------------------*
 | function TIdentities.GetCount                                        |
 |                                                                      |
 | Get method for 'Count' property.  Return the number of identities    |
 *----------------------------------------------------------------------*)
function TIdentities.GetCount: Integer;
begin
  result := fIdentities.Count;
end;

(*----------------------------------------------------------------------*
 | function TIdentities.GetIdentity                                     |
 |                                                                      |
 | Get method for 'Identity' property.  Return the identifier           |
 *----------------------------------------------------------------------*)
function TIdentities.GetDefaultIdentity: TIdentity;
var
  i : Integer;
begin
  result := Nil;
  for i := 0 to Count - 1 do
    if Identity [i].IsDefault then
    begin
      result := Identity [i];
      break
    end
end;

function TIdentities.GetIdentity(idx: Integer): TIdentity;
begin
  result := TIdentity (fIdentities [idx])
end;

(*----------------------------------------------------------------------*
 | procedure TIdentities.Load                                           |
 |                                                                      |
 | Load the identities from the registry                                |
 *----------------------------------------------------------------------*)
procedure TIdentities.Load (rootReg : TExSettings);
var
  keyNames : TStrings;
  i : Integer;
  identity : TIdentity;
  gotDefault : boolean;
begin
  keyNames := Nil;
  gotDefault := False;
  try
    if rootReg.HasSection ('Identities') then
    begin
      rootReg.Section := 'Identities';
      keyNames := TStringList.Create;
      rootReg.GetSectionNames(keyNames);

      for i := 0 to keyNames.Count - 1 do
      begin
        identity := TIdentity.Create;
        identity.Load(rootReg, keyNames [i]);
        fIdentities.Add(identity);
        if identity.IsDefault then
          gotDefault := True
      end;

      if not gotDefault and (Count > 0) then
        GetIdentity (0).IsDefault := True;
    end
  finally
    rootReg.Section := '';
    keyNames.Free
  end;

  if Count = 0 then
    if not CreateDefaultIdentity then
      Halt
end;

(*----------------------------------------------------------------------*
 | procedure TIdentities.Save                                           |
 |                                                                      |
 | Save the identities to the registry.                                 |
 *----------------------------------------------------------------------*)
procedure TIdentities.Save;
var
  oldIdentities : TStringList;
  reg : TExSettings;
  i, idx : Integer;
  id : TIdentity;
begin
  oldIdentities := Nil;
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
      id := Identity [i];
      id.Save(reg);
      idx := oldIdentities.IndexOf(id.Name);
      if idx >= 0 then
        oldIdentities.Delete(idx)
    end;

    // Delete identities that no loner exist.
    for i := 0 to oldIdentities.Count - 1 do
      reg.DeleteSection(oldIdentities [i])
  finally
    reg.Free;
    oldIdentities.Free
  end
end;

{ TIdentity }

(*----------------------------------------------------------------------*
 | procedure TIdentity.ChangeName                                       |
 |                                                                      |
 | Change the name of an identity.  I didn't want the 'Name' property   |
 | to be read/write because it may confuse things.                      |
 *----------------------------------------------------------------------*)
procedure TIdentity.AssignTo(id: TIdentity);
begin
  id.fOrganization := self.fOrganization;
  id.fXface := self.fXface;
  id.fEMailAddress := self.EMailAddress;
  id.fName := self.fName;
  id.fReplyAddress := self.fReplyAddress;
  id.fSignature := self.fSignature;
  id.fUserName := self.fUserName;
  id.fIsDefault := False;
  id.fSigFile := self.fSigFile
end;

procedure TIdentity.ChangeName(const newName: string);
begin
  fName := newName
end;

function TrimRightWhitespace(const S, WhiteSpace: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (Pos (S[I], WhiteSpace) > 0) do Dec(I);
  Result := Copy(S, 1, I);
end;

(*----------------------------------------------------------------------*
 | procedure TIdentity.Load                                             |
 |                                                                      |
 | Load an identity from the registry                                   |
 |                                                                      |
 | Parameters:                                                          |
 |   rootKey: HKEY;             // The root of the identities registry  |
 |                              // branch                               |
 |   const AName: string        // Name of the identity to load.        |
 *----------------------------------------------------------------------*)
function TIdentity.ChooseSignature (sigOverride: string): string;
var
  f: TFileStream;
  rdr: TStreamTextReader;
  idx: TList;
  st: string;
  raw: UTF8String;
  sig: string;
  useSigFile: boolean;

  function GetSig(pos: Integer) : string;
  var
    st: string;
  begin
    rdr.Position := pos;
    result := '';
    st := '';
    while rdr.ReadLn(raw) do
    begin
      st := UTF8ToString(raw);
      if st <> '.' then
        Result := Result + st + #13#10
      else
        Exit;
      if st <> '' then
        Result := Result + st + #13#10;
    end;
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
                              // '%sigfile', then use both.
  end;

  if useSigFile and (SigFile <> '') and FileExists(sigFile) then
  begin
    rdr := nil;
    idx := nil;
    f := TFileStream.Create(sigFile, fmOpenRead or fmShareDenyNone);
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

procedure TIdentity.Load(rootReg : TExSettings; const AName: string);
var
  reg : TExSettings;
begin
  fName := AName;
  reg := CreateChildSettings (rootReg, AName);
  try

    fUserName := reg.StringValue ['User Name'];
    fEMailAddress := reg.StringValue ['EMail Address'];
    fReplyAddress := reg.StringValue['Reply Address'];
    fOrganization := reg.StringValue ['Organization'];
    fSignature := reg.StringValue ['Signature'];
    fSigFile := reg.StringValue ['SigFile'];

    if (fSigFile <> '') and (Pos (':', fSigFile) = 0) and (Copy (fSigFile, 1, 2) <> '\\') then
    begin
      fSigFile := gMessageBaseRoot + '\' + fSigFile;
      reg.StringValue['SigFile'] := fSigFile
    end;
    fXFace := reg.StringValue ['XFace'];
    fXFace := StringReplace (fXFace, #13#10' ', '', [rfReplaceAll]);
    fIsDefault := reg.BooleanValue ['Default'];
  finally
    reg.Free
  end
end;

procedure TIdentity.Save(rootReg : TExSettings);
var
  reg : TExSettings;
begin
  reg := CreateChildSettings (rootReg, Name);
  try

    reg.StringValue ['User Name'] := UserName;
    reg.StringValue ['EMail Address'] := EMailAddress;
    reg.StringValue ['Reply Address'] := ReplyAddress;
    reg.StringValue ['Organization'] := Organization;
    reg.StringValue ['Signature'] := Signature;
    reg.StringValue ['XFace'] := XFace;
    reg.StringValue ['SigFile'] := SigFile;
    reg.BooleanValue ['Default'] := fIsDefault;
  finally
    reg.Free
  end
end;

end.
