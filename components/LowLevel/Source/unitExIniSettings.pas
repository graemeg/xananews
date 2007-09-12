(*======================================================================*
 | unitExIniSettings                                                    |
 |                                                                      |
 | Ini file application settings classes.                               |
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
unit unitExIniSettings;

interface

uses Classes, Sysutils, unitExSettings, unitExFileSettings, IniFiles;

type

//-----------------------------------------------------------------------
// TIniExSettings.
//
// Class to store application and other settings to INI files
TExIniSettings = class (TExFileSettings)
private
  fIniFile : TCustomIniFile;
  fAutoReadOnly : boolean;

  function FixSection : string;

protected
  function IsOpen : boolean; override;
  function CheckIsOpen (readOnly, autoReadOnly : boolean) : TIsOpen; override;
  procedure InternalSetStringValue (const valueName, value : string); override;
public
  procedure Close; override;
  function Open (readOnly : boolean = false) : boolean; override;

  procedure DeleteValue (const valueName : string); override;
  procedure DeleteSection (const sectionName : string); override;

  function HasSection (const ASection : string) : boolean; override;
  function HasValue (const AValue : string) : boolean; override;
  procedure GetValueNames (names : TStrings); override;
  procedure GetSectionNames (names : TStrings); override;

  function GetStringValue  (const valueName : string; const deflt : string = '') : string; override;
  function GetIntegerValue (const valueName : string; deflt : Integer = 0) : Integer; override;
end;

implementation

{ TExIniSettings }

(*----------------------------------------------------------------------*
 | function TExIniSettings.CheckIsOpen                                  |
 |                                                                      |
 | Ensure that the file is open.                                        |
 *----------------------------------------------------------------------*)
function TExIniSettings.CheckIsOpen (readOnly, autoReadOnly : boolean) : TIsOpen;
var
  fn : string;
begin
  result := inherited CheckIsOpen (readOnly, autoReadOnly);

  case result of
    woClosed :
      begin
        if Open (readOnly) then
        begin
          result := woOpen;
          fReadOnly := False
        end
        else
          result := woClosed;
      end;

    woReopen:
      begin
        fAutoReadOnly := readOnly;

        fn := GetFileName ('.ini');
        if not readOnly then
          ForceDirectories (ExtractFilePath (fn));
        result := woOpen;
      end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TExINISettings.Close                                       |
 |                                                                      |
 | Close the INI file                                                   |
 *----------------------------------------------------------------------*)
procedure TExIniSettings.Close;
begin
  FreeAndNil (fIniFile);
end;

(*----------------------------------------------------------------------*
 | procedure TExINISettings.DeleteValue                                 |
 |                                                                      |
 | Delete a value                                                       |
 *----------------------------------------------------------------------*)
procedure TExIniSettings.DeleteSection(const sectionName: string);
begin
  CheckIsOpen (false, fAutoReadOnly);

  fIniFile.EraseSection(FixSection);
end;

procedure TExIniSettings.DeleteValue(const valueName: string);
begin
  CheckIsOpen (false, fAutoReadOnly);

  fIniFile.DeleteKey(FixSection, valueName);
end;

(*----------------------------------------------------------------------*
 | procedure TExINISettings.FixSection                                  |
 |                                                                      |
 | Return a valid INI file section name for the current section         |
 *----------------------------------------------------------------------*)
function TExIniSettings.FixSection: string;
begin
  result := Section;
  if result = '' then
    result := 'Default';
end;

(*----------------------------------------------------------------------*
 | function TExINISettings.GetIntegerValue                              |
 |                                                                      |
 | Return an integer value, or default if the value doesn't exist       |
 *----------------------------------------------------------------------*)
function TExIniSettings.GetIntegerValue(const valueName: string;
  deflt: Integer): Integer;
var
  st : string;
begin
  CheckIsOpen (true, fAutoReadOnly);

  st := fIniFile.ReadString(FixSection, valueName, #1);
  if st = #1 then
    result := deflt
  else
    if not TryStrToInt (st, result) then
      raise EExSettings.Create('Integer value expected');
end;

(*----------------------------------------------------------------------*
 | function TExINISettings.GetStringValue                               |
 |                                                                      |
 | Return a string value, or default if the value doesn't exist         |
 *----------------------------------------------------------------------*)
procedure TExIniSettings.GetSectionNames(names: TStrings);
begin
  names.Clear;
  if CheckIsOpen (true, fAutoReadOnly) = woOpen then
    fIniFile.ReadSections(names);
end;

function TExIniSettings.GetStringValue(const valueName, deflt: string): string;
begin
  if CheckIsOpen (true, fAutoReadOnly) = woOpen then
    result := fIniFile.ReadString(FixSection, valueName, deflt)
  else
    result := deflt;
end;

procedure TExIniSettings.GetValueNames(names: TStrings);
begin
  names.Clear;
  if CheckIsOpen (true, fAutoReadOnly) = woOpen then
    fIniFile.ReadSection(FixSection, names);
end;

function TExIniSettings.HasSection(const ASection: string): boolean;
var
  sec : string;
begin
  if CheckIsOpen (true, fAutoReadOnly) = woOpen then
  begin
    if Section = '' then
      sec := ASection
    else
      sec := Section + '\' + ASection;

    if sec = '' then
      result := True
    else
      result := fIniFile.SectionExists(sec)
  end
  else
    result := False
end;

function TExIniSettings.HasValue(const AValue: string): boolean;
begin
  if CheckIsOpen (true, fAutoReadOnly) = woOpen then
    result := fIniFile.ValueExists(FixSection, AValue)
  else
    result := False
end;

(*----------------------------------------------------------------------*
 | procedure TExINISettings.InternalSetStringValue                      |
 |                                                                      |
 | Set a string value                                                   |
 *----------------------------------------------------------------------*)
procedure TExIniSettings.InternalSetStringValue(const valueName, value: string);
begin
  CheckIsOpen (false, fAutoReadOnly);

  fIniFile.WriteString(FixSection, valueName, value);
end;

(*----------------------------------------------------------------------*
 | function TExINISettings.IsOpen                                       |
 |                                                                      |
 | Return true if the object is Open                                    |
 *----------------------------------------------------------------------*)
function TExIniSettings.IsOpen: boolean;
begin
  result := fIniFile <> Nil;
end;

(*----------------------------------------------------------------------*
 | procedure TExIniSettings.Open                                        |
 |                                                                      |
 | Open the Ini file.  Create it if it doesn't exist                    |
 *----------------------------------------------------------------------*)
function TExIniSettings.Open(readOnly: boolean) : boolean;
var
  fn : string;
begin
  inherited Open (readOnly);
  result := True;
  Close;
  fAutoReadOnly := readOnly;

  fn := GetFileName ('.ini');
  if not readOnly then
    ForceDirectories (ExtractFilePath (fn))
  else
    if not FileExists (fn) then
    begin
      result := False;
      Exit;
    end;
    

  fIniFile := TIniFile.Create(fn);
end;

end.
