unit cmpIniFilePersistentOptions;

interface

uses Windows, Classes, SysUtils, cmpPersistentOptions, unitExSettings;

type
//---------------------------------------------------------------------
// TIniFilePersistentOptions class  - persistent options held in an INI
// file
  TIniFilePersistentOptions = class (TCustomPersistentOptions)
  private
    fCustomFileName : string;
  protected
    function GetSettingsClass : TExSettingsClass; override;
    procedure SettingsCreated (settings : TExSettings); override;
  published
    property FileName : string read fCustomFileName write fCustomFileName;
  end;

implementation

uses unitExIniSettings;

{ TIniFilePersistentOptions }

function TIniFilePersistentOptions.GetSettingsClass: TExSettingsClass;
begin
  result := TExIniSettings;
end;

procedure TIniFilePersistentOptions.SettingsCreated(settings: TExSettings);
begin
  inherited;

  TExIniSettings (settings).CustomPath := fCustomFileName
end;

end.
