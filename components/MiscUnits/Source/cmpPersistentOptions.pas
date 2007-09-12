(*======================================================================*
 | cmpPersistentOptions                                                 |
 |                                                                      |
 | TRegistryPersistentOptions & TIniFilePersistentOptions components    |
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
 | Copyright © Colin Wilson 2003  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      10/09/2003  CPWW  Original                                  |
 | 2.0      27/04/2004  CPWW  Added impersonation so that interactive   |
 |                            services can access HKEY_CURRENT_USER     |
 |                            for the logged-on user.                   |
 *======================================================================*)

unit cmpPersistentOptions;

interface

uses
  Windows, SysUtils, Classes, unitExSettings;

const
  systemUser = 'SYSTEM';

type
  TOptions = class;
  TSections = class;
  TPersistentOptions = class;

  TOptionType = (otInteger, otBoolean, otString, otEnum);

//---------------------------------------------------------------------
// TOption class.
  TOption = class (TCollectionItem)
  private
    fDefaultValue: string;
    fName: string;
    fEnumValues: TStringList;
    fOptionType: TOptionType;
    fIntVal : Integer;
    fStrVal : string;
    fBoolVal : boolean;
    fDirty : boolean;

    function GetBase: TPersistentOptions;
    function GetAsBoolean: Boolean;
    function GetAsInteger: Integer;
    function GetAsString: string;
    function GetAsEnum: string;

    procedure SetEnumValues(const Value: TStrings);
    function GetEnumValues: TStrings;
    procedure SetOptionType(const Value: TOptionType);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsEnum(const Value: string);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: string);
    procedure Flush;
    function GetHasDefaultValue: boolean;
  protected
    function GetDisplayName : string; override;
  public
    constructor Create (Collection : TCollection); override;
    destructor Destroy; override;
    property Base : TPersistentOptions read GetBase;

    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property AsString : string read GetAsString write SetAsString;
    property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
    property AsEnum : string read GetAsEnum write SetAsEnum;

    property HasDefaultValue : boolean read GetHasDefaultValue;

  published
    property Name : string read fName write fName;
    property DefaultValue : string read fDefaultValue write fDefaultValue;
    property EnumValues : TStrings read GetEnumValues write SetEnumValues;
    property OptionType : TOptionType read fOptionType write SetOptionType;
  end;

//---------------------------------------------------------------------
// TOptions class - a collection of options
  TOptions = class (TOwnedCollection)
  private
    fDeleteObsoleteOptions: boolean;
    function GetOption(idx: Integer): TOption;
    function GetOptionByName(const name: string): TOption;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    property Option [idx : Integer] : TOption read GetOption; default;
    property OptionByName [const name : string] : TOption read GetOptionByName;
  published
    property DeleteObsoleteOptions : boolean read fDeleteObsoleteOptions write fDeleteObsoleteOptions default True;
  end;

//---------------------------------------------------------------------
// TSection class
  TSection = class (TCollectionItem)
  private
    fName: string;
    fOptions: TOptions;
    fSections: TSections;
    function GetOption(const name: string): TOption;
    function GetSection(const name: string): TSection;
  protected
    function GetDisplayName : string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Option [const name : string] : TOption read GetOption;
    property Section [const name : string] : TSection read GetSection;
  published
    property Name : string read fName write fName;
    property Options : TOptions read fOptions write fOptions;
    property Sections : TSections read fSections write fSections;
  end;

//---------------------------------------------------------------------
// TSections class - a collection of sections.
  TSections = class (TOwnedCollection)
  private
    fDeleteObsoleteSections: boolean;
    function GetSection(idx: Integer): TSection;
    function GetSectionByName(const name: string): TSection;
  public
    property Section [idx : Integer] : TSection read GetSection; default;
    property SectionByName [const name : string] : TSection read GetSectionByName;
  published
    property DeleteObsoleteSections : boolean read fDeleteObsoleteSections write fDeleteObsoleteSections default False;
  end;

//---------------------------------------------------------------------
// TPersistentOptions - base class for TRegistryPersistentOptions and
// TIniFilePersistentOptions
  TPersistentOptions = class(TComponent)
  private
    function GetPersist: boolean;
    function GetLoading: boolean;
  private
    fManufacturer: string;
    fApplication: string;
    fVersion: string;
    fOptions: TOptions;
    fSections: TSections;
    fUpdating: boolean;
    function GetDesigning: boolean;
    function GetOption(path: string): TOption;
    function GetSection(name: string): TSection;
    procedure SetApplication(const Value: string);
    procedure SetManufacturer(const Value: string);
    procedure SetVersion(const Value: string);
    procedure RemoveLeadingSlash (var path : string);

    property Designing : boolean read GetDesigning;
    property Loading : boolean read GetLoading;
    property Persist : boolean read GetPersist;
    function GetDirty: boolean;
    { Private declarations }
  protected
    procedure Loaded; override;
    procedure DeleteOldOptions (const application, manufacturer, version : string); virtual; abstract;
    property InUpdate : boolean read fUpdating;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Save; virtual; abstract;
    procedure Load; virtual; abstract;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Option [path : string] : TOption read GetOption; default;
    property Section [name : string] : TSection read GetSection;
    property Dirty : boolean read GetDirty;
    { Public declarations }
  published
    property Application : string read fApplication write SetApplication;
    property Manufacturer : string read fManufacturer write SetManufacturer;
    property Version : string read fVersion write SetVersion;
    property Options : TOptions read fOptions write fOptions;
    property Sections : TSections read fSections write fSections;
  end;

  TCustomPersistentOptions = class (TPersistentOptions)
  private
    fOptionsType: TExSettingsType;
    procedure LoadOptions (settings : TExSettings; Options : TOptions; forceDefaults : boolean);
    procedure LoadSections (settings : TExSettings; Sections : TSections; forceDefaults : boolean);
    procedure SaveOptions (settings : TExSettings; Options : TOptions);
    procedure SaveSections (settings : TExSettings; Sections : TSections);
  protected
    procedure DeleteOldOptions (const application, manufacturer, version : string); override;
    property OptionsType : TExSettingsType read fOptionsType write fOptionsType;
    function GetSettingsClass : TExSettingsClass; virtual; abstract;
    function GetSettingsFile : string; virtual;
    procedure SettingsCreated (settings : TExSettings); virtual;
  public
    procedure Load; override;
    procedure Save; override;
  published
  end;

  TOnGetSettingsClass = procedure (sender : TObject; var settingsType : TExSettingsClass) of object;
  TOnGetSettingsFile = procedure (sender : TObject; var fileName : string) of object;

  TUniPersistentOptions = class (TCustomPersistentOptions)
  private
    fOnGetSettingsClass: TOnGetSettingsClass;
    fOnGetSettingsFile: TOnGetSettingsFile;
  protected
    function GetSettingsClass : TExSettingsClass; override;
    function GetSettingsFile : string; override;
  published
    property OptionsType;
    property OnGetSettingsClass : TOnGetSettingsClass read fOnGetSettingsClass write fOnGetSettingsClass;
    property OnGetSettingsFile : TOnGetSettingsFile read fOnGetSettingsFile write fOnGetSettingsFile;
  end;

  EOptionError = class (Exception)
  end;

  EOptionTypeMismatch = class (EOptionError)
  public
    constructor Create (Option : TOption);
  end;

resourcestring
  rstNoAppName = 'Application property can not be blank';

implementation

uses unitExFileSettings;

resourcestring
  rstNotEnum = 'Not an enum type';
  rstTypeMismatch = '%s is not of %s type';
  rstSectionNotFound = 'Section %s not found';
  rstOptionNotFound = 'Option %s not found in section %s';

const
  OptionTypeNames : array [TOptionType] of string = ('integer', 'boolean', 'string', 'emumerated');

{ TPersistentOptions }

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.BeginUpdate                             |
 |                                                                      |
 | Start updating.  Must be matched with EndUpdate                      |A
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.BeginUpdate;
begin
  fUpdating := True
end;

(*----------------------------------------------------------------------*
 | constructor TPersistentOptions.Create                                |
 |                                                                      |
 | Constructor for TPersistentOptions                                   |
 *----------------------------------------------------------------------*)
constructor TPersistentOptions.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  fOptions := TOptions.Create(self, TOption);
  fSections := TSections.Create(self, TSection);
end;

(*----------------------------------------------------------------------*
 | destructor TPersistentOptions.Destroy                                |
 |                                                                      |
 | Destructor for TPersistentOptions                                    |
 *----------------------------------------------------------------------*)
destructor TPersistentOptions.Destroy;
begin
  if Dirty then
    Save;
  fOptions.Free;
  fSections.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.EndUpdate                               |
 |                                                                      |
 | End a batch update started with BeginUpdate                          |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.EndUpdate;
begin
  if fUpdating and Dirty then
    Save;

  fUpdating := False
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetDesigning                             |
 |                                                                      |
 | Return True if the component is in 'design' mode.                    |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetDesigning: boolean;
begin
  result := csDesigning in ComponentState
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetDirty                                 |
 |                                                                      |
 | Return True if any option has had it's value changed, but has not    |
 | yet been persisted to the registry or INI file.                      |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetDirty: boolean;

  function OptionsDirty (options : TOptions) : boolean;
  var
    i : Integer;
  begin
    result := False;
    for i := 0 to options.Count - 1 do
      if options [i].fDirty then
      begin
        result := True;
        break
      end
  end;

  function SectionsDirty (sections : TSections) : boolean;
  var
    i : Integer;
    section : TSection;
  begin
    result := False;
    for i := 0 to sections.Count - 1 do
    begin
      section := sections [i];
      result := OptionsDirty (section.Options);
      if not result then
        result := SectionsDirty (section.Sections);
      if result then
        break
    end
  end;

begin
  result := OptionsDirty (Options) or SectionsDirty (Sections)
end;

(*----------------------------------------------------------------------*
 | TPersistentOptions.GetOption                                         |
 |                                                                      |
 | Return an option by name or path.  Note that as a shortcut the       |
 | path passed can contain sections - so you can say                    |
 |                                                                      |
 |   MyPersistentOptions.Section ['Position'].Option ['Width']          |
 |                                                                      |
 | or                                                                   |
 |                                                                      |
 |   MyPersistentOptions.Option ['Position\Width']                      |
 |                                                                      |
 | or even (because it's the default property):                         |
 |                                                                      |
 |   MyPersistentOptions ['Position\Width']                             |
 |                                                                      |
 | Parameters:                                                          |
 |   path: string               The option name or path                 |
 |                                                                      |
 | The function always returns a valid TOption, or raised an exception  |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetLoading: boolean;
begin
  result := (csLoading in ComponentState);

end;

function TPersistentOptions.GetOption(path: string): TOption;
var
  p : PChar;
  n : Integer;
  s : TSection;
  secName  : string;
begin
  RemoveLeadingSlash (path);
  p := StrRScan (PChar (path), '\');

  s := Nil;
  if Assigned (p) then
  begin
    n := Integer (p) - Integer (PChar (path)) + 1;
    s := Sections.SectionByName [Trim (Copy (path, 1, n - 1))];
    path := Trim (Copy (path, n + 1, MaxInt));
  end;

  if Assigned (s) then
    result := s.Options.OptionByName [path]
  else
    result := Options.OptionByName [path];

  if result = Nil then
  begin
    if Assigned (s) then
      secName := s.Name
    else
      secName := '[Default]';
    raise EOptionError.CreateFmt (rstOptionNotFound, [path, secName])
  end
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetPersist                               |
 |                                                                      |
 | Return true if changes to the option values should be persisted to   |
 | the registry or INI file - ie. it's not in design mode, and it's not |
 | Loading.                                                             |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetPersist: boolean;
begin
  result := not Designing and not (csLoading in ComponentState);
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetSection                               |
 |                                                                      |
 | Return a section by name or path.  Note that as a shortcut the       |
 | path passed can contain sub-sections - so you can say                |
 |                                                                      |
 |   MyPersistentOptions.Section ['Position'].Section ['Attributes']    |
 |                                                                      |
 | or                                                                   |
 |                                                                      |
 |   MyPersistentOptions.Section ['Position\Attributes']                |
 |                                                                      |
 | Parameters:                                                          |
 |   name: string               The section name or path                |
 |                                                                      |
 | The function returns a valid TSection, or raises an exception        |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetSection(name: string): TSection;
begin
  RemoveLeadingSlash (name);
  result := Sections.SectionByName [name]
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.Loaded                                  |
 |                                                                      |
 | Overridden 'Loaded' method.  Load the registry or ini file           |
 | information.                                                         |
 *----------------------------------------------------------------------*)

procedure TPersistentOptions.Loaded;
begin
  inherited;
  Load
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.RemoveLeadingSlash                      |
 |                                                                      |
 | Remove the leading slash from a path.                                |
 *----------------------------------------------------------------------*)

procedure TPersistentOptions.RemoveLeadingSlash (var path : string);
begin
  if Copy (path, 1, 1) = '\' then
    path := Copy (path, 2, MaxInt);
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.SetApplication                          |
 |                                                                      |
 | Set method for 'Application' property.  If this is changed at        |
 | runtime clear the old .ini file, or delete the old registry entries  |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: string                New 'Application' value         |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.SetApplication(const Value: string);
var
  oldApplication : string;
begin
  if fApplication <> Value then
  begin
    oldApplication := fApplication;
    try
      fApplication := Value;

      if not (csLoading in ComponentState) then
        Save;
    except
      fApplication := oldApplication;
      raise
    end;

    DeleteOldOptions (oldApplication, Manufacturer, Version);
  end
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.SetManufacturer                         |
 |                                                                      |
 | Set method for 'Manufacturer' property.  If this is changed at       |
 | runtime clear the old .ini file, or delete the old registry entries  |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: string                New 'Manufacturer' value        |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.SetManufacturer(const Value: string);
var
  oldManufacturer : string;
begin
  if fManufacturer <> Value then
  begin
    oldManufacturer := fManufacturer;
    try
      fManufacturer := Value;
      if not (csLoading in ComponentState) then
        Save;
    except
      fManufacturer := oldManufacturer;
      raise
    end;

    DeleteOldOptions (Application, oldManufacturer, Version);
  end
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.SetVersion                              |
 |                                                                      |
 | Set method for 'Version' property.  If this is changed at runtime    |
 | clear the old .ini file, or delete the old registry entries          |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: string                New 'Version' value             |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.SetVersion(const Value: string);
var
  oldVersion : string;
begin
  if fVersion <> Value then
  begin
    oldVersion := fVersion;
    try
      fVersion := Value;
      if not (csLoading in ComponentState) then
        Save
    except
      fVersion := oldVersion;
      raise
    end;

    DeleteOldOptions (Application, Manufacturer, oldVersion)
  end
end;

{ TOptions }

(*----------------------------------------------------------------------*
 | constructor TOptions.Create                                          |
 |                                                                      |
 | Constructor for TOptions collection                                  |
 *----------------------------------------------------------------------*)
constructor TOptions.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  inherited Create (AOwner, ItemClass);
  fDeleteObsoleteOptions := True
end;

(*----------------------------------------------------------------------*
 | procedure TOptions.GetOption                                         |
 |                                                                      |
 | Get method for 'Option' array property                               |
 *----------------------------------------------------------------------*)
function TOptions.GetOption(idx: Integer): TOption;
begin
  result := TOption (Items [idx])
end;

(*----------------------------------------------------------------------*
 | procedure TOptions.GetOptionByName                                   |
 |                                                                      |
 | 'Get' method for OptionByName array property                         |
 *----------------------------------------------------------------------*)
function TOptions.GetOptionByName(const name: string): TOption;
var
  o : TOption;
  i : Integer;
begin
  result := Nil;
  for i := 0 to Count - 1 do
  begin
    o := Option [i];
    if AnsiSameText (Name, o.Name) then
    begin
      result := o;
      break
    end
  end
end;

{ TOption }

(*----------------------------------------------------------------------*
 | constructor TOption.Create                                           |
 |                                                                      |
 | Constructor for TOption class                                        |
 *----------------------------------------------------------------------*)
constructor TOption.Create(Collection: TCollection);
begin
  inherited;
  if Base.Designing or Base.Loading then
  begin
    fEnumValues := TStringList.Create;
    fEnumValues.CaseSensitive := False
  end
end;

(*----------------------------------------------------------------------*
 | destructor TOption.Destroy                                           |
 |                                                                      |
 | Destructor for TOption class                                         |
 *----------------------------------------------------------------------*)
destructor TOption.Destroy;
begin
  fEnumValues.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TOption.Flush                                              |
 |                                                                      |
 | Save the option.                                                     |
 *----------------------------------------------------------------------*)
procedure TOption.Flush;
begin
  if fDirty and not Base.InUpdate then
    Base.Save
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsBoolean                                        |
 |                                                                      |
 | Return the option value if it's a boolean option - otherwise raise   |
 | an exception.                                                        |
 *----------------------------------------------------------------------*)
function TOption.GetAsBoolean: Boolean;
begin
  if OptionType = otBoolean then
    result := fBoolVal
  else
    raise EOptionTypeMismatch (self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsEnum                                           |
 |                                                                      |
 | Return the option value if it's an enum option - otherwise raise an  |
 | exception.                                                           |
 *----------------------------------------------------------------------*)
function TOption.GetAsEnum: string;
begin
  if OptionType = otEnum then
    result := EnumValues [fIntVal]
  else
    raise EOptionTypeMismatch (self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsInteger                                        |
 |                                                                      |
 | Return the option value if it's an integer option - otherwise raise  |
 | an exception.                                                        |
 *----------------------------------------------------------------------*)
function TOption.GetAsInteger: Integer;
begin
  if OptionType = otInteger then
    result := fIntVal
  else
    raise EOptionTypeMismatch (self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsString                                         |
 |                                                                      |
 | Return the option value if it's a string option - otherwise raise    |
 | an exception.                                                        |
 *----------------------------------------------------------------------*)
function TOption.GetAsString: String;
begin
  if OptionType = otString then
    result := fStrVal
  else
    raise EOptionTypeMismatch (self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetBase                                             |
 |                                                                      |
 | Return the owning 'TPersistentOptions' derived object.               |
 *----------------------------------------------------------------------*)
function TOption.GetBase: TPersistentOptions;
var
  own : TPersistent;
begin
  own := TOwnedCollection (Collection).Owner;

  while own is TSection do
    own := TOwnedCollection (TSection (own).Collection).Owner;

  result := own as TPersistentOptions
end;

(*----------------------------------------------------------------------*
 | function TOption.GetDisplayName                                      |
 |                                                                      |
 | Overridden from TCollectionItem base.  Helps the designer.           |
 *----------------------------------------------------------------------*)
function TOption.GetDisplayName: string;
begin
  result := Name
end;

(*----------------------------------------------------------------------*
 | function TOption.GetEnumValues : TStringList                         |
 |                                                                      |
 | 'Get' method for EnumValues property                                 |
 *----------------------------------------------------------------------*)
function TOption.GetEnumValues: TStrings;
begin
  result := fEnumValues
end;

(*----------------------------------------------------------------------*
 | function TOption.GetHasDefaultValue : boolean                        |
 |                                                                      |
 | Return True if the option's current value is it's default.           |
 *----------------------------------------------------------------------*)
function TOption.GetHasDefaultValue: boolean;
begin
  result := False;
  case OptionType of
    otString  : result := AnsiCompareStr (DefaultValue, fStrVal) = 0;
    otInteger : result := StrToIntDef (DefaultValue, 0) = fIntVal;
    otBoolean : result := StrToBoolDef (DefaultValue, False) = fBoolVal;
    otEnum    : result := fIntVal = EnumValues.IndexOf(DefaultValue)
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsBoolean                                       |
 |                                                                      |
 | Set the option's value if it's a boolean option - otherwise raise    |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsBoolean(const Value: Boolean);
begin
  if OptionType <> otBoolean then
    raise EOptionTypeMismatch (self);

  if Value <> fBoolVal then
  begin
    fDirty := True;
    fBoolVal := Value;
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsEnum                                          |
 |                                                                      |
 | Set the option's value if it's an enum option - otherwise raise      |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsEnum(const Value: string);
begin
  if Value <> AsEnum then
  begin
    fDirty := True;
    fIntVal := EnumValues.IndexOf(Value);
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsInteger                                       |
 |                                                                      |
 | Set the option's value if it's an integer option - otherwise raise   |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsInteger(const Value: Integer);
begin
  if OptionType <> otInteger then
    raise EOptionTypeMismatch (self);

  if Value <> fIntVal then
  begin
    fDirty := True;
    fIntVal := Value;
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsString                                        |
 |                                                                      |
 | Set the option's value if it's a string option - otherwise raise     |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsString(const Value: string);
begin
  if OptionType <> otString then
    raise EOptionTypeMismatch (self);

  if Value <> fStrVal then
  begin
    fDirty := True;
    fStrVal := Value;
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetEnumValues                                      |
 |                                                                      |
 | 'Set' method for EnumValues property                                 |
 *----------------------------------------------------------------------*)
procedure TOption.SetEnumValues(const Value: TStrings);
begin
  if (OptionType = otEnum) then
    fEnumValues.Assign(Value)
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetOptionType                                      |
 |                                                                      |
 | 'Set' method for the OptionType property.                            |
 *----------------------------------------------------------------------*)
procedure TOption.SetOptionType(const Value: TOptionType);
begin
  if fOptionType <> Value then
  begin
    if Base.Designing then
    begin
      if not Base.Loading then fEnumValues.Clear
    end
    else
      if not Base.Loading or (Value <> otEnum) then
        FreeAndNil (fEnumValues);

    fOptionType := Value;

    if fOptionType = otEnum then
      if not Base.Designing and not Base.Loading then
        fEnumValues := TStringList.Create
  end
end;

{ TSection }

(*----------------------------------------------------------------------*
 | constructor TSection.Create                                          |
 |                                                                      |
 | Constructor for TSection                                             |
 *----------------------------------------------------------------------*)
constructor TSection.Create(Collection: TCollection);
begin
  inherited;

  fOptions := TOptions.Create(self, TOption);
  fSections := TSections.Create(self, TSection);
end;

(*----------------------------------------------------------------------*
 | destructor TSection.Destroy                                          |
 |                                                                      |
 | Destructor for TSection                                              |
 *----------------------------------------------------------------------*)
destructor TSection.Destroy;
begin
  fOptions.Free;
  fSections.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | function TSection.GetDisplayName                                     |
 |                                                                      |
 | Override TCollectionItem method to help the designer                 |
 *----------------------------------------------------------------------*)
function TSection.GetDisplayName: string;
begin
  result := Name
end;

(*----------------------------------------------------------------------*
 | function TSection.GetOption                                          |
 |                                                                      |
 | 'Get' method for Option property                                     |
 *----------------------------------------------------------------------*)
function TSection.GetOption(const name: string): TOption;
begin
  result := Options.OptionByName [name];
  if result = Nil then
    raise Exception.Create ('Option ' + name + ' not found');
end;

(*----------------------------------------------------------------------*
 | function TSection.GetSection                                         |
 |                                                                      |
 | 'Get' method for Section propery                                     |
 *----------------------------------------------------------------------*)
function TSection.GetSection(const name: string): TSection;
begin
  result := Sections.SectionByName [name];
  if result = Nil then
    raise Exception.Create ('Section ' + name + ' not found');
end;

{ EOptionTypeMismatch }

constructor EOptionTypeMismatch.Create(Option: TOption);
begin
  inherited CreateFmt (rstTypeMismatch, [Option.Name, OptionTypeNames [Option.OptionType]])
end;

{ TSections }

(*----------------------------------------------------------------------*
 | function TSections.GetSection                                        |
 |                                                                      |
 | Get method for Section property.                                     |
 *----------------------------------------------------------------------*)
function TSections.GetSection(idx: Integer): TSection;
begin
  result := TSection (Items [idx])
end;

(*----------------------------------------------------------------------*
 | function TSections.GetSectionByName                                  |
 |                                                                      |
 | 'Get' method for SectionByName property                              |
 *----------------------------------------------------------------------*)
function TSections.GetSectionByName(const name: string): TSection;
var
  i, p : Integer;
  s : TSection;
begin
  result := Nil;

  p := Pos ('\', name);
  if p > 0 then
  begin
    s := SectionByName [Trim (Copy (name, 1, p - 1))];
    if Assigned (s) then
      result := s.Sections.SectionByName [Trim (Copy (name, p + 1, MaxInt))]
    else
      raise EOptionError.CreateFmt(rstSectionNotFound, [s.Name]);
  end
  else
  for i := 0 to Count - 1 do
  begin
    s := Section [i];
    if AnsiSameText (name, s.Name) then
    begin
      result := s;
      break
    end
  end;

  if not Assigned (result) then
    raise EOptionError.CreateFmt (rstSectionNotFound, [name])
end;

{ TCustomPersistentOptions }


procedure TCustomPersistentOptions.DeleteOldOptions(const application, manufacturer,
  version: string);
begin
//
end;

function TCustomPersistentOptions.GetSettingsFile: string;
begin
// stub
end;

procedure TCustomPersistentOptions.Load;
var
  settings : TExSettings;
  openedOK : boolean;
begin
  if not Persist then Exit;

  settings := GetSettingsClass.Create (Manufacturer, Application, Version, fOptionsType);
  try
    SettingsCreated (settings);
    openedOk := settings.Open(true);
    LoadOptions (settings, Options, not openedOK);
    LoadSections (settings, Sections, not openedOK)
  finally
    settings.Free
  end
end;

procedure TCustomPersistentOptions.LoadOptions(settings: TExSettings;
  Options: TOptions; forceDefaults: boolean);
var
  i : Integer;
  option : TOption;
begin
  for i := 0 to Options.Count - 1 do
  begin
    option := Options [i];

    if forceDefaults or not settings.HasValue(option.Name) then
    case option.OptionType of
      otString  : option.fStrVal  := option.fDefaultValue;
      otInteger : option.fIntVal  := StrToIntDef (option.fDefaultValue, 0);
      otBoolean : option.fBoolVal := StrToBoolDef (option.fDefaultValue, False);
      otEnum    : option.fIntVal  := option.fEnumValues.IndexOf(option.fDefaultValue)
    end
    else
    case option.OptionType of
      otString  : option.fStrVal  := settings.StringValue [option.Name];
      otInteger : option.fIntVal  := settings.IntegerValue [option.Name];
      otBoolean : option.fBoolVal := settings.BooleanValue [option.Name];
      otEnum    : option.fIntVal  := settings.IntegerValue [option.Name]
    end;
    option.fDirty := False
  end
end;

procedure TCustomPersistentOptions.LoadSections(settings: TExSettings;
  Sections: TSections; forceDefaults: boolean);
var
  i : Integer;
  section : TSection;
  settings1 : TExSettings;
begin
  for i := 0 to Sections.Count - 1 do
  begin
    section := Sections [i];

    settings1 := Nil;
    try
      if not forceDefaults then
      begin
        settings1 := GetSettingsClass.CreateChild(settings, section.Name);
        forceDefaults := not settings1.Open (true);
      end;
      LoadOptions (settings1, section.Options, forceDefaults);
      LoadSections (settings1, section.Sections, forceDefaults)
    finally
      settings1.Free
    end;
  end
end;

procedure TCustomPersistentOptions.Save;
var
  settings : TExSettings;
begin
  if not Persist then Exit;

  settings := GetSettingsClass.Create(Manufacturer, Application, Version, fOptionsType);
  try
    SettingsCreated (settings);
    SaveOptions (settings, Options);
    SaveSections (settings, Sections)
  finally
    settings.Free
  end
end;

procedure TCustomPersistentOptions.SaveOptions(settings : TExSettings; Options: TOptions);
var
  i, idx : Integer;
  deleteValues : TStringList;
  option : TOption;

begin
  deleteValues := TStringList.Create;
  try
  // Get a list of values to delete.  Start by filling it with all the values
  // then remove values as we save them, leaving only the obsolete ones.

    deleteValues.CaseSensitive := False;
    settings.GetValueNames(deleteValues);
    deleteValues.Sort;

    for i := 0 to Options.Count - 1 do
    begin
      Option := Options [i];

      idx := deleteValues.IndexOf(Option.Name);
      if idx >= 0 then
        deleteValues.Delete(idx);

      case Option.OptionType of
        otString  : settings.SetStringValue (Option.Name, Option.fStrVal, Option.DefaultValue);
        otInteger : settings.IntegerValue [Option.Name] := Option.fIntVal;
        otBoolean : settings.BooleanValue [Option.Name] := Option.fBoolVal;
        otEnum    : settings.IntegerValue [Option.Name] := Option.fIntVal;
      end;
      Option.fDirty := False
    end;

    if Options.DeleteObsoleteOptions then          // Delete obsolete values.
      for i := 0 to deleteValues.count - 1 do
        settings.DeleteValue(deleteValues [i])
  finally
    deleteValues.Free
  end
end;

procedure TCustomPersistentOptions.SaveSections(settings : TExSettings; Sections: TSections);
var
  i, idx : Integer;
  section : TSection;
  settings1 : TExSettings;
  deleteSections : TStringList;
begin
  deleteSections := TStringList.Create;
  try
  // Build list of obsolete sections to delete.

    deleteSections.CaseSensitive := False;
    settings.GetSectionNames(deleteSections);
    deleteSections.Sort;

  // Save the sections & options
    for i := 0 to Sections.Count - 1 do
    begin
      section := Sections [i];

      settings1 := GetSettingsClass.CreateChild(settings, section.Name);
      try
        idx := deleteSections.IndexOf(Section.Name);
        if idx >= 0 then
          deleteSections.Delete(idx);

        SaveOptions (settings1, section.Options);
        SaveSections (settings1, section.Sections)
      finally
        settings1.Free
      end;
    end;

    // Delete the obsolete sections
    if Sections.DeleteObsoleteSections then
      for i := 0 to deleteSections.Count - 1 do
//        settings.DeleteSection (deleteSections [i])
  finally
    deleteSections.Free;
  end
end;

procedure TCustomPersistentOptions.SettingsCreated(settings: TExSettings);
begin
  if settings is TExFileSettings then
    TExFileSettings (settings).CustomPath := GetSettingsFile;
end;

{ TUniPersistentOptions }

function TUniPersistentOptions.GetSettingsClass: TExSettingsClass;
begin
  if Assigned (fOnGetSettingsClass) then
    OnGetSettingsClass (self, result)
  else
    raise EOptionError.Create ('Unable to get settings class');
end;

function TUniPersistentOptions.GetSettingsFile: string;
begin
  if Assigned (fOnGetSettingsFile) then
    OnGetSettingsFile (self, result)
end;

end.
