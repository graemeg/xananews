(*======================================================================*
 | unitExSettings                                                       |
 |                                                                      |
 | Base application settings classes.                                   |
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
unit unitExSettings;

interface

uses
  Windows, Classes, SysUtils;

type
  TExSettingsType = (stUser, stMachine);
  TIsOpen = (woClosed, woReopen, woOpen);

  //-----------------------------------------------------------------------
  // TExSettings.
  //
  // Base class for derived classes that store application and other settings
  // to the registry, ini files, XML files, etc.
  TExSettings = class
  private
    fSettingsType: TExSettingsType;
    fApplication: string;
    fManufacturer: string;
    fVersion: string;
    fSection: string;
    fParentSection: string;
    fUpdateCount: Integer;
    fParent: TExSettings;

    function GetBoolValue(const name: string): Boolean;
    function GetIntValue(const name: string): Integer;
    function GetInt64Value(const name: string): Int64;
    function GetStrValue(const name: string): string;

    procedure SetStrValue(const name, value: string);
    procedure SetIntValue(const name: string; value: Integer);
    procedure SetInt64Value(const name: string; const value: Int64);
    procedure SetBoolValue(const name: string; value: Boolean);
    function GetRegStub(const section: string): string;

  protected
    fReadOnly: Boolean;

    function IsOpen: Boolean; virtual; abstract;
    function CheckIsOpen(readOnly, autoReadOnly: Boolean): TIsOpen; virtual;
    procedure SetSection(const SectionPath: string); virtual;

    procedure InternalSetIntegerValue(const valueName: string; value: Integer); virtual;
    procedure InternalSetStringValue(const valueName, value: string); virtual; abstract;

  public
    constructor Create(const AManufacturer, AApplication: string; const AVersion: string = ''; ASettingsType: TExSettingsType = stUser);
    constructor CreateChild(AParent: TExSettings; const ASection: string); virtual;

    destructor Destroy; override;
    function Open(readOnly: Boolean = False): Boolean; virtual;
    procedure Close; virtual;
    procedure Flush; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure DeleteValue(const valueName: string); virtual; abstract;
    procedure DeleteSection(const sectionName: string); virtual; abstract;
    procedure GetValueNames(names: TStrings); virtual; abstract;
    procedure GetSectionNames(names: TStrings); virtual; abstract;

    function GetBooleanValue(const valueName: string; deflt: Boolean = False): Boolean; virtual;
    function GetStringValue(const valueName: string; const deflt: string = ''): string; virtual; abstract;
    function GetIntegerValue(const valueName: string; deflt: Integer = 0): Integer; virtual;
    function GetInteger64Value(const valueName: string; deflt: Int64 = 0): Integer; virtual;
    function HasSection(const ASection: string): Boolean; virtual; abstract;
    function HasValue(const AValueName: string): Boolean; virtual; abstract;
    function GetStrings(const valueName: string; sl: TStrings): Integer; virtual;

    procedure SetBooleanValue(const valueName: string; value: Boolean; deflt: Boolean = False); virtual;
    procedure SetStringValue(const valueName: string; const value: string; const deflt: string = ''); virtual;
    procedure SetIntegerValue(const valueName: string; value: Integer; deflt: Integer = 0); virtual;
    procedure SetInteger64Value(const valueName: string; value: Int64; deflt: Int64 = 0); virtual;
    procedure SetStrings(const valueName: string; sl: TStrings); virtual;

    procedure RenameSection(const oldValue, newValue: string); virtual; abstract;
    procedure RenameValue(const oldValue, newValue: string); virtual; abstract;

    function GetExportValue(const valueName: string): string; virtual;

    procedure ExportToRegStream(const section: string; stream: TStream; excludeSections: TStrings = nil);
    procedure ImportFromRegStream(stream: TStream);

    property Application: string read fApplication;
    property Manufacturer: string read fManufacturer;
    property ReadOnly: Boolean read fReadOnly;
    property SettingsType: TExSettingsType read fSettingsType;
    property Version: string read fVersion;

    property Section: string read fSection write SetSection;
    property Parent: TExSettings read fParent;

    property IntegerValue[const name: string]: integer read GetIntValue write SetIntValue;
    property Int64Value[const name: string]: Int64 read GetInt64Value write SetInt64Value;
    property StringValue[const name: string]: string read GetStrValue write SetStrValue;
    property BooleanValue[const name: string]: Boolean read GetBoolValue write SetBoolValue;
  end;

  TExSettingsClass = class of TExSettings;

  EExSettings = class(Exception);

function MakeCStringConst(const s: string): string;

implementation

uses
  unitStreamTextReader, unitSearchString;


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

{ TExSettings }

procedure TExSettings.BeginUpdate;
begin
  if fUpdateCount <= 0 then
    fUpdateCount := 1
  else
    Inc(fUpdateCount);
end;

(*----------------------------------------------------------------------*
 | function TExSettings.CheckIsOpen                                     |
 |                                                                      |
 | Returns woOpen if the persistor is open in the correct state for the |
 | read or write operation.                                             |
 |                                                                      |
 | Returns woReopen if the persistor is open, but for read-only when    |
 | write access is required.                                            |
 |                                                                      |
 | Returns woClosed if the persistor is closed.                         |
 |                                                                      |
 | autoReadOnly should contain the actual read-only state of the        |
 | persistor, whereas fReadOnly should contain the maximum ro value,    |
 | and readOnly contains the requested state.                           |
 |                                                                      |
 | Derived classes override this to provide further processing to       |
 | ensure that their return value is woOpen or an exception is raised.  |
 *----------------------------------------------------------------------*)
function TExSettings.CheckIsOpen(readOnly, autoReadOnly: Boolean): TIsOpen;
var
  wasOpen: Boolean;
begin
  wasOpen := IsOpen;
  if wasOpen then
    if readOnly or not autoReadOnly then
    begin
      Result := woOpen;
      Exit;
    end
    else
      if fReadOnly then
        raise EExSettings.Create('Can''t write to read-only settings');

  if wasOpen then
    Result := woReopen
  else
    Result := woClosed;
end;

procedure TExSettings.Close;
begin
  Flush;
end;

constructor TExSettings.Create(const AManufacturer, AApplication,
  AVersion: string; ASettingsType: TExSettingsType);
begin
  fApplication := AApplication;
  fVersion := AVersion;
  fManufacturer := AManufacturer;
  fSettingsType := ASettingsType;
end;

constructor TExSettings.CreateChild(AParent: TExSettings; const ASection: string);
begin
  if ClassType <> AParent.ClassType then
    raise EExSettings.Create('Child class must be the same as the parent class');

  fParent := AParent;
  fSettingsType := AParent.SettingsType;
  fApplication := AParent.Application;
  fManufacturer := AParent.Manufacturer;
  fVersion := AParent.Version;
  fParentSection := AParent.Section;

  if ASection = '' then
    fSection := fParentSection
  else
    if fParentSection = '' then
      fSection := ASection
    else
      fSection := AParent.Section + '\' + ASection;
end;

destructor TExSettings.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TExSettings.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount <= 0 then
  begin
    Flush;
    fUpdateCount := 0;
  end;
end;

procedure TExSettings.ImportFromRegStream(stream: TStream);
var
  strings: TStrings;
  st: string;
  i: Integer;

  procedure SyntaxError;
  begin
    raise Exception.CreateFmt('Syntax error in reg stream at line %d', [i])
  end;

  procedure CreateNewKey;
  var
    s: string;
  begin
    Delete(st, 1, 1);
    if st[Length(st)] <> ']' then
      SyntaxError;

    Delete(st, Length(st), 1);

    s := SplitString('\', st);

    if st = '' then
      SyntaxError;

    if s = 'HKEY_LOCAL_MACHINE' then
    begin
      if SettingsType <> stMachine then
        raise EExSettings.Create('Type mismatch');
    end
    else
      if s = 'HKEY_CURRENT_USER' then
      begin
        if SettingsType <> stUser then
          raise EExSettings.Create('Type mismatch');
      end
      else
        raise EExSettings.Create('Type mismatch');

    s := GetRegStub('');
    if Copy(st, 1, Length(s)) = s then
    begin
      Delete(st, 1, Length(s));
      if (Length(st) > 0) and (Copy(st, 1, 1) = '\') then
        Delete(st, 1, 1);
      Section := st;
    end
    else
      raise EExSettings.Create('Can''t import');
  end;

  function GetCString(st: string; var n: Integer): string;
  var
    i: Integer;
    eos: Boolean;
  begin
    Result := '';
    i := 2;
    repeat
      eos := False;
      while i <= Length(st) do
      begin
        if st[i] = '"' then
        begin
          eos := True;
          Break;
        end;

        if st[i] = '\' then
          Inc(i);

        if i <= Length(st) then
          Result := Result + st[i];

        Inc(i);
      end;

      if not eos then
      begin
        Result := Result + #13#10;
        Inc(n);
        st := strings[n];
        i := 1;
      end;
    until eos;
  end;

  function GetBinaryBuffer(const st: string): string;
  var
    i: Integer;
    val: string;
  begin
    i := 1;
    Result := '';
    while i <= Length(st) do
    begin
      if st[i] in ['0'..'9', 'a'..'f', 'A'..'F'] then
        val := val + st[i]
      else
      begin
        if val <> '' then
        begin
          Result := Result + Chr(StrToInt('$' + val));
          val := '';
        end;
      end;

      Inc(i);
    end;
  end;

  procedure CreateNewValue(var i: Integer);
  var
    s, s1: string;
    fn: string;
    p: Integer;
    tp: Integer;
    buf: string;
    sl: TStrings;
  begin
    if st[1] = '"' then
    begin
      Delete(st, 1, 1);
      p := Pos('"', st);
      if p = 0 then
        SyntaxError;

      s := Copy(st, 1, p - 1);
      st := Copy(st, p + 1, MaxInt);
    end
    else
    begin
      Delete(st, 1, 1);
      s := '';
    end;

    st := TrimLeft(st);

    if st = '' then
      SyntaxError;

    if st[1] <> '=' then
      SyntaxError;

    Delete(st, 1, 1);

    st := TrimLeft(st);

    if st[1] = '"' then
      SetStringValue(s, GetCString(st, i))
    else
    begin
      p := 1;
      while (p <= Length(st)) and not (st[p] in [':', '(', ' ']) do
        Inc(p);

      fn := Copy(st, 1, p - 1);

      st := TrimLeft(Copy(st, p, MaxInt));

      if CompareText(fn, 'hex') = 0 then
      begin
        tp := 3;
        if st[1] = '(' then
        begin
          Delete(st, 1, 1);
          fn := '';
          p := 1;
          while (p <= Length(st)) and (st[p] <> ')') do
          begin
            fn := fn + st[p];
            Inc(p);
          end;

          tp := StrToInt(fn);
          st := Trim(Copy(st, p + 1, MaxInt));
        end;

        if st[1] <> ':' then
          SyntaxError;

        Delete(st, 1, 1);

        buf := GetBinaryBuffer(st);

        if tp = REG_MULTI_SZ then
        begin
          sl := TStringList.Create;
          try
            while buf <> '' do
            begin
              s1 := SplitString(#0, buf);

              if (buf <> '') or (s1 <> '') then
                sl.Add(s1);
            end;

            SetStrings(s, sl);

          finally
            sl.Free;
          end;
        end;
      end
      else
        if CompareText(fn, 'dword') = 0 then
        begin
          if st[1] <> ':' then
            SyntaxError;

          Delete(st, 1, 1);
          SetIntegerValue(s, StrToInt('$' + TrimLeft(st)));
        end
        else
          SyntaxError;
    end;
  end;

begin
  strings := TStringList.Create;
  try
    strings.LoadFromStream(stream);

    while (strings.Count > 0) do
    begin
      st := Trim(strings[0]);
      if (st = '') or (st[1] = ';') then
        strings.Delete(0)
      else
        Break;
    end;

    if strings[0] <> 'REGEDIT4' then
      raise Exception.Create('Bad file format.  Missing REGEDIT4 in first line.');

    i := 1;
    while i < strings.Count do
    begin
      st := Trim(strings[i]);

      if st <> '' then
        while st[Length(st)] = '\' do
        begin
          Inc(i);
          Delete(st, Length(st), 1);
          if i < strings.Count then
            st := st + strings[i]
          else
            Break;
        end;

      if (Length(st) > 0) and (st[1] <> ';') then
      begin
        case st[1] of
          '[': CreateNewKey;
          '"': CreateNewValue(i);
          '@': CreateNewValue(i);
        else
          SyntaxError;
        end;
      end;

      Inc(i);
    end;
  finally
    strings.Free;
  end;
end;

function TExSettings.GetRegStub(const section: string): string;
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
      Result := Result + '\' + Section;
  end;
end;

procedure TExSettings.ExportToRegStream(const section: string; stream: TStream;
  excludeSections: TStrings);
var
  s: TTextStreamWriter;
  rootKeyName: string;
  rs: string;

  procedure Exp(sctn: string);
  var
    sl: TStrings;
    i: Integer;
    sn: string;
  begin
    SetSection(sctn);

    if sctn = '' then
      sn := rs
    else
      sn := rs + '\' + sctn;

    s.WriteLn('');
    s.WriteLn(Format('[%s\%s]', [rootKeyName, sn]));

    sl := TStringList.Create;
    try
      GetValueNames(sl);
      for i := 0 to sl.Count - 1 do
        s.WriteLn(GetExportValue(sl[i]));

      GetSectionNames(sl);
      for i := 0 to sl.Count - 1 do
      begin
        sn := sl[i];
        if (ExcludeSections = nil) or (ExcludeSections.IndexOf(sn) = -1) then
        begin
          if sctn <> '' then
            sn := sctn + '\' + sn;
          Exp(sn);
        end;
      end;
    finally
      sl.Free;
    end;
  end;

begin
  if CheckIsOpen(True, False) = woOpen then
  begin
    if SettingsType = stUser then
      rootKeyName := 'HKEY_CURRENT_USER'
    else
      rootKeyName := 'HKEY_LOCAL_MACHINE';

    rs := GetRegStub('');

    s := TTextStreamWriter.Create(stream);

    try
      s.WriteLn('REGEDIT4');
      Exp(section);
    finally
      s.Free;
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TExSettings.Flush                                          |
 |                                                                      |
 | Can be overridden by the persistor to ensure that any cached         |
 | written data is persisted.  Typically it won't be overriden, because |
 | calling 'Close' will do.                                             |
 *----------------------------------------------------------------------*)
procedure TExSettings.Flush;
begin
  // Stub
end;

function TExSettings.GetStrings(const valueName: string; sl: TStrings): Integer;
begin
  sl.Text := StringValue[valueName];
  Result := sl.Count;
end;

function TExSettings.GetStrValue(const name: string): string;
begin
  Result := GetStringValue(name);
end;

function TExSettings.GetBooleanValue(const valueName: string; deflt: Boolean): Boolean;
begin
  Result := GetIntegerValue(valueName, Ord(deflt)) <> 0;
end;

function TExSettings.GetBoolValue(const name: string): Boolean;
begin
  Result := GetBooleanValue(name);
end;

function TExSettings.GetExportValue(const valueName: string): string;
begin
  Result := '"' + valueName + '"="' + StringValue[valueName] + '"';
end;

function TExSettings.GetInteger64Value(const valueName: string; deflt: Int64): Integer;
begin
  deflt := GetIntegerValue(valueName, deflt);
  Result := StrToIntDef(GetStringValue(valueName + '64', IntToStr(deflt)), deflt);
end;

function TExSettings.GetIntegerValue(const valueName: string; deflt: Integer): Integer;
begin
  Result := StrToIntDef(GetStringValue(valueName, IntToStr(deflt)), deflt);
end;

function TExSettings.GetInt64Value(const name: string): Int64;
begin
  Result := GetInteger64Value(name);
end;

function TExSettings.GetIntValue(const name: string): Integer;
begin
  Result := GetIntegerValue(name);
end;

procedure TExSettings.InternalSetIntegerValue(const valueName: string; value: Integer);
begin
  InternalSetStringValue(valueName, IntToStr(value));
end;

function TExSettings.Open(readOnly: Boolean): Boolean;
begin
  fReadOnly := readOnly;
  Result := False;
end;

procedure TExSettings.SetBooleanValue(const valueName: string; value: Boolean; deflt: Boolean);
begin
  SetIntegerValue(valueName, Ord(value), Ord(deflt));
end;

procedure TExSettings.SetBoolValue(const name: string; value: Boolean);
begin
  SetBooleanValue(name, value);
end;

procedure TExSettings.SetInteger64Value(const valueName: string; value, deflt: Int64);
begin
  if value = deflt then
  begin
    DeleteValue(valueName);
    DeleteValue(valueName + '64');
  end
  else
    InternalSetStringValue(valueName + '64', IntToStr(value));
end;

procedure TExSettings.SetIntegerValue(const valueName: string; value, deflt: Integer);
begin
  if value = deflt then
    DeleteValue(valueName)
  else
    InternalSetIntegerValue(valueName, value);
end;

procedure TExSettings.SetInt64Value(const name: string; const value: Int64);
begin
  SetInteger64Value(name, value);
end;

procedure TExSettings.SetIntValue(const name: string; value: Integer);
begin
  SetIntegerValue(name, value);
end;

procedure TExSettings.SetSection(const SectionPath: string);
begin
  if SectionPath = '' then
    fSection := fParentSection
  else
    if fParentSection = '' then
      fSection := SectionPath
    else
      fSection := fParentSection + '\' + SectionPath;
end;

procedure TExSettings.SetStrings(const valueName: string; sl: TStrings);
begin
  StringValue[valueName] := sl.Text;
end;

procedure TExSettings.SetStringValue(const valueName, value, deflt: string);
begin
  if value = deflt then
    DeleteValue(valueName)
  else
    InternalSetStringValue(valueName, value);
end;

procedure TExSettings.SetStrValue(const name, value: string);
begin
  SetStringValue(name, value);
end;

end.
