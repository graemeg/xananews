(*======================================================================*
 | unitExRegSettings                                                    |
 |                                                                      |
 | Registry application settings classes.                               |
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
 | Copyright © Colin Wilson 2006  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      02/03/2006  CPWW  Original                                  |
 *======================================================================*)
unit unitExRegSettings;

interface

uses
  Windows, Classes, SysUtils, unitExSettings, Registry;

type
  //-----------------------------------------------------------------------
  // TExRegSettings.
  //
  // Class to store application and other settings to the registry
  TExRegSettings = class(TExSettings)
  private
    fCurrentReg: TRegistry;
    fParentKey: HKEY;
    fAutoReadOnly: Boolean;
    fChildSection: string;
    function GetCurrentKey: HKEY;

    function ApplicationKeyName: string;
  protected
    function IsOpen: Boolean; override;
    function CheckIsOpen(readOnly, autoReadOnly: Boolean): TIsOpen; override;
    procedure SetSection(const SectionPath: string); override;
    procedure InternalSetIntegerValue(const valueName: string; value: Integer); override;
    procedure InternalSetStringValue(const valueName, value: string); override;
  public
    constructor CreateChild(AParent: TExSettings; const ASection: string); override;
    procedure Close; override;
    function Open(readOnly: Boolean = False): Boolean; override;
    procedure Flush; override;

    property CurrentKey: HKEY read GetCurrentKey;

    procedure DeleteValue(const valueName: string); override;
    procedure DeleteSection(const sectionName: string); override;
    function HasSection(const ASection: string): Boolean; override;
    function HasValue(const AValue: string): Boolean; override;
    procedure GetValueNames(names: TStrings); override;
    procedure GetSectionNames(names: TStrings); override;
    function GetExportValue(const valueName: string): string; override;

    function GetStringValue(const valueName: string; const deflt: string = ''): string; override;
    function GetIntegerValue(const valueName: string; deflt: Integer = 0): Integer; override;
    function GetStrings(const valueName: string; sl: TStrings): Integer; override;

    procedure SetStrings(const valueName: string; sl: TStrings); override;

    procedure RenameSection(const oldValue, newValue: string); override;
    procedure RenameValue(const oldValue, newValue: string); override;
  end;

implementation

resourcestring
  rstReadError = 'Error %d reading value %s';
  rstOpenError = 'Error %d opening %s';
  rstDeleteError = 'Error %d deleting %s';
  rstWriteError = 'Error %d writing value %s';


{ TExRegSettings }

(*----------------------------------------------------------------------*
 | function TExRegSettings.ApplicationKeyName                           |
 |                                                                      |
 | Return the key name for the currently selected section               |
 *----------------------------------------------------------------------*)
function TExRegSettings.ApplicationKeyName: string;
begin
  if Application = '' then
    Result := Section
  else
  begin
    if Manufacturer = '' then
      Result := 'Software\' + Application
    else
      Result := 'Software\' + Manufacturer + '\' + Application;

    if Version <> '' then
      Result := Result + '\' + Version;

    if Section <> '' then
      Result := Result + '\' + Section
  end;

  if Result = '' then
    raise EExSettings.Create('Must specify an Application or a Section');
end;

function TExRegSettings.CheckIsOpen(readOnly, autoReadOnly: Boolean): TIsOpen;
var
  ok: Boolean;
begin
  Result := inherited CheckIsOpen(readOnly, autoReadOnly);

  if (Result = woClosed) or (Result = woReopen) then
  begin
    ok := Open(readOnly);
    if Result = woClosed then
      fReadOnly := False;

    if ok then
      Result := woOpen
    else
      Result := woClosed;
  end;
end;

procedure TExRegSettings.Close;
begin
  FreeAndNil(fCurrentReg);
end;

constructor TExRegSettings.CreateChild(AParent: TExSettings;
  const ASection: string);
begin
  inherited;
  fChildSection := ASection;
  fParentKey := TExRegSettings(AParent).CurrentKey;
end;

procedure TExRegSettings.DeleteSection(const sectionName: string);
begin
  if CheckIsOpen(False, fAutoReadOnly) = woOpen then
    fCurrentReg.DeleteKey(sectionName);
end;

procedure TExRegSettings.DeleteValue(const valueName: string);
begin
  if CheckIsOpen(False, fAutoReadOnly) = woOpen then
    fCurrentReg.DeleteValue(valueName);
end;

procedure TExRegSettings.Flush;
begin
  inherited;
  // Could call RegFlushKey - but isn't usually required
end;

function TExRegSettings.GetCurrentKey: HKEY;
begin
  if CheckIsOpen(True, fAutoReadOnly) = woOpen then
    Result := fCurrentReg.CurrentKey
  else
    Result := 0;
end;

function MakeCStringConst(const s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if s[i] in ['\', '"'] then
      Result := Result + '\';
    Result := Result + s[i];
  end;
end;

function TExRegSettings.GetExportValue(const valueName: string): string;
var
  tp: DWORD;
  st, st1: string;
  j, dataLen: Integer;
  data: PByte;
begin
  Result := '';
  if CheckIsOpen(True, fAutoReadOnly) = woOpen then
  begin
    if RegQueryValueEx(fCurrentReg.CurrentKey, PChar(valueName), nil, @tp, nil, PDWORD(@dataLen)) = ERROR_SUCCESS then
    begin
      if valueName = '' then
        st := '@='
      else
        st := Format('"%s"=', [MakeCStringConst(valueName)]);

      case tp of
        REG_DWORD:
          begin
            st1 := LowerCase(Format('%8.8x', [IntegerValue[valueName]]));
            st := st + Format('dword:%s', [st1]);
          end;

        REG_SZ:
          st := st + Format('"%s"', [MakeCStringConst(StringValue[valueName])]);

        else
          if tp = REG_BINARY then
            st := st + 'hex:'
          else
            st := st + Format('hex(%d):', [tp]);

          GetMem(data, dataLen);
          try
            RegQueryValueEx(fCurrentReg.CurrentKey, PChar(valueName), nil, @tp, data, @dataLen);
            for j := 0 to dataLen - 1 do
            begin
              st1 := LowerCase(Format('%2.2x', [data[j]]));
              if j < dataLen - 1 then
                st1 := st1 + ',';

              if Length(st) + Length(st1) >= 77 then
              begin
                Result := Result + st + st1 + '\' + #13#10;
                st := '  ';
              end
              else
                st := st + st1;
            end;
          finally
            FreeMem(data);
          end;
      end;

      Result := Result + st;
    end;
  end;
end;

function TExRegSettings.GetIntegerValue(const valueName: string; deflt: Integer): Integer;
begin
  if (CheckIsOpen(True, fAutoReadOnly) = woOpen) and fCurrentReg.ValueExists(valueName) then
    Result := fCurrentReg.ReadInteger(valueName)
  else
    Result := deflt;
end;

procedure TExRegSettings.GetSectionNames(names: TStrings);
begin
  if CheckIsOpen(True, fAutoReadOnly) = woOpen then
    fCurrentReg.GetKeyNames(names)
  else
    names.Clear;
end;

function TExRegSettings.GetStrings(const valueName: string; sl: TStrings): Integer;
var
  valueType, rv: DWORD;
  valueLen: DWORD;
  p, buffer: PChar;
begin
  sl.Clear;
  if CheckIsOpen(True, fAutoReadOnly) = woOpen then
  begin
    rv := RegQueryValueEx(CurrentKey, PChar(valueName), nil, @valueType, nil, @valueLen);
    if rv = ERROR_SUCCESS then
      if valueType = REG_MULTI_SZ then
      begin
        GetMem(buffer, valueLen);
        try
          RegQueryValueEx(CurrentKey, PChar(valueName), nil, nil, PBYTE(buffer), @valueLen);
          p := buffer;
          while p^ <> #0 do
          begin
            sl.Add(p);
            Inc(p, lstrlen(p) + 1);
          end;
        finally
          FreeMem(buffer);
        end;
      end
      else
        raise EExSettings.Create('String list expected')
    else
      raise EExSettings.Create('Unable read MULTI_SZ value');
  end;
  Result := sl.Count;
end;

function TExRegSettings.GetStringValue(const valueName, deflt: string): string;
begin
  if (CheckIsOpen(True, fAutoReadOnly) = woOpen) and fCurrentReg.ValueExists(valueName) then
    Result := fCurrentReg.ReadString(valueName)
  else
    Result := deflt;
end;

procedure TExRegSettings.GetValueNames(names: TStrings);
begin
  if CheckIsOpen(True, fAutoReadOnly) = woOpen then
    fCurrentReg.GetValueNames(names)
  else
    names.Clear;
end;

function TExRegSettings.HasSection(const ASection: string): Boolean;
begin
  if CheckIsOpen(True, fAutoReadOnly) = woOpen then
    Result := fCurrentReg.KeyExists(ASection)
  else
    Result := False;
end;

function TExRegSettings.HasValue(const AValue: string): Boolean;
begin
  if CheckIsOpen(True, fAutoReadOnly) = woOpen then
    Result := fCurrentReg.ValueExists(AValue)
  else
    Result := False;
end;

procedure TExRegSettings.InternalSetIntegerValue(const valueName: string;
  value: Integer);
begin
  if CheckIsOpen(False, fAutoReadOnly) = woOpen then
    fCurrentReg.WriteInteger(valueName, value)
  else
    raise EExSettings.Create('Unable to write value ' + valueName);
end;

procedure TExRegSettings.InternalSetStringValue(const valueName, value: string);
begin
  if CheckIsOpen(False, fAutoReadOnly) = woOpen then
    fCurrentReg.WriteString(valueName, value)
  else
    raise EExSettings.Create('Unable to write value ' + valueName);
end;

function TExRegSettings.IsOpen: Boolean;
begin
  Result := Assigned(fCurrentReg)
end;

function TExRegSettings.Open(readOnly: Boolean): Boolean;
var
  rootKey: HKEY;
  section: string;
  rights: DWORD;
begin
  inherited Open(readOnly);
  Close;
  fAutoReadOnly := readOnly;

  if fParentKey = 0 then
  begin
    section := ApplicationKeyName;
    if SettingsType = stMachine then
      rootKey := HKEY_LOCAL_MACHINE
    else
      rootKey := HKEY_CURRENT_USER;
  end
  else
  begin
    rootKey := fParentKey;
    section := fChildSection;
  end;

  if readOnly then
    rights := KEY_READ
  else
    rights := KEY_READ or KEY_WRITE;

  fCurrentReg := TRegistry.Create(rights);
  fCurrentReg.RootKey := rootKey;

  Result := fCurrentReg.OpenKey(section, not readOnly);
end;

procedure TExRegSettings.RenameSection(const oldValue, newValue: string);
begin
  if CheckIsOpen(False, fAutoReadOnly) = woOpen then
    fCurrentReg.MoveKey(oldValue, newValue, True);
end;

procedure TExRegSettings.RenameValue(const oldValue, newValue: string);
begin
  if CheckIsOpen(False, fAutoReadOnly) = woOpen then
    fCurrentReg.RenameValue(oldValue, newValue);
end;

procedure TExRegSettings.SetSection(const SectionPath: string);
begin
  Close;      // Close the registry

  if fParentKey <> 0 then
    fChildSection := SectionPath
  else
    inherited;
end;

procedure TExRegSettings.SetStrings(const valueName: string; sl: TStrings);
var
  i: Integer;
  size: DWORD;
  st: string;
begin
  if CheckIsOpen(False, fAutoReadOnly) <> woOpen then
    raise EExSettings.Create('Unable to write MULTI_SZ value');

  st := '';
  for i := 0 to sl.Count - 1 do
    st := st + sl[I] + #0;
  st := st + #0;

  size := Length(st);

  SetLastError(RegSetValueEx(CurrentKey, PChar(valueName), 0, REG_MULTI_SZ, @st[1], size));
  if GetLastError <> ERROR_SUCCESS then
    raise EExSettings.Create('Unable to write MULTI_SZ value');
end;

end.