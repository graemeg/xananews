(*======================================================================*
 | unitExSettings                                                       |
 |                                                                      |
 | Base application settings class using file persistors                |
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
 | Copyright © Colin Wilson 2005  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      02/03/2006  CPWW  Original                                  |
 *======================================================================*)
unit unitExFileSettings;

interface

uses
  Windows, Sysutils, unitExSettings;

type
  //-----------------------------------------------------------------------
  // TExFileSettings.
  //
  // Base class for derived classes that store application and other settings
  // in files - ini files, XML files, etc.
  TExFileSettings = class(TExSettings)
  private
    fCustomPath: string;
    procedure SetCustomPath(const Value: string);
  protected
    function GetFileName(const ext: string): string;
  public
    constructor CreateChild(AParent: TExSettings; const ASection: string); override;
    property CustomPath: string read fCustomPath write SetCustomPath;
  end;

implementation

uses
  ShFolder;

constructor TExFileSettings.CreateChild(AParent: TExSettings; const ASection: string);
begin
  inherited;
  CustomPath := TExFileSettings(AParent).CustomPath;
end;

(*----------------------------------------------------------------------*
 | procedure TExFileSettings.GetFileName                                |
 |                                                                      |
 | Return the file name, including the path.                            |
 |                                                                      |
 | For 'Machine' settings use the same path as the .exe file.           |
 |                                                                      |
 | For 'User' settings use the user's local appdata path, plus the      |
 | Application, Manufacturer and Version.                               |
 *----------------------------------------------------------------------*)
function TExFileSettings.GetFileName(const ext: string): string;
var
  path: string;
begin
  if Application = '' then
    raise EExSettings.Create('Must specify an application');

  if fCustomPath <> '' then
    Result := fCustomPath
  else
  begin
    SetLength(path, MAX_PATH);
    if (SettingsType = stMachine) or not SUCCEEDED(SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, PChar(path)))  then
      Result := ExtractFilePath(ParamStr(0))
    else
    begin
      Result := PChar(path);
      Result := Result + '\';

      if Manufacturer <> '' then
        Result := Result + Manufacturer + '\';

      if Application <> '' then
        Result := Result + Application + '\';

      if Version <> '' then
        Result := Result + Version + '\';

      Result := Result + Application + ext;
    end;
  end;
end;

procedure TExFileSettings.SetCustomPath(const Value: string);
begin
  if Value <> fCustomPath then
  begin
    Close;
    fCustomPath := Value;
  end;
end;

end.