unit cmpRegistryPersistentOptions;

interface

uses Windows, Classes, SysUtils, cmpPersistentOptions, unitExSettings;

type
//---------------------------------------------------------------------
// TRegistryPersistentOptions class  - persistent options held in the
// registry
  TRegistryPersistentOptions = class (TCustomPersistentOptions)
  private
    function GetRootKeyName: string;
  protected
    function GetSettingsClass : TExSettingsClass; override;
  public
    property RootKeyName : string read GetRootKeyName;
  published
    property OptionsType;
  end;

implementation

uses unitExRegSettings;

{ TRegistryPersistentOptions }

(*----------------------------------------------------------------------*
 | function TRegistryPersistentOptions.GetRootKeyName                   |
 |                                                                      |
 | Get the root key name for the options.  This is...                   |
 |                                                                      |
 | \Software\[<Manufacturer>\]<Application\[<Version>]                  |
 *----------------------------------------------------------------------*)
function TRegistryPersistentOptions.GetRootKeyName: string;
begin
  if Application = '' then
    raise EOptionError.Create (rstNoAppName);

  if Manufacturer <> '' then
    result := '\Software\' + Manufacturer + '\' + Application
  else
    result := '\Software\' + Application;

  if Version <> '' then
    result := result + '\' + Version
end;

function TRegistryPersistentOptions.GetSettingsClass: TExSettingsClass;
begin
  result := TExRegSettings;
end;

end.
