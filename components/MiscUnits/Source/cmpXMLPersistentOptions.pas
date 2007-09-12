unit cmpXMLPersistentOptions;

interface

uses Windows, Classes, SysUtils, cmpPersistentOptions, unitExSettings;

type

//---------------------------------------------------------------------
// TXMLPersistentOptions class  - persistent options held in an INI
// file
  TXMLPersistentOptions = class (TCustomPersistentOptions)
  private
    fCustomFileName : string;
  protected
    function GetSettingsClass : TExSettingsClass; override;
    procedure SettingsCreated (settings : TExSettings); override;
  published
    property FileName : string read fCustomFileName write fCustomFileName;
  end;

implementation

uses unitExXMLSettings;

{ TXMLPersistentOptions }

function TXMLPersistentOptions.GetSettingsClass: TExSettingsClass;
begin
  result := TExXMLSettings;
end;

procedure TXMLPersistentOptions.SettingsCreated(settings: TExSettings);
begin
  inherited;

  TExXMLSettings (settings).CustomPath := FileName;
end;

end.
