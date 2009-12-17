unit IdHeaderCoderBig5;

interface

{$i IdCompilerDefines.inc}

uses
  IdGlobal, IdHeaderCoderBase;

type
  TIdHeaderCoderBig5 = class(TIdHeaderCoder)
  public
    class function Decode(const ACharSet: string; const AData: TIdBytes): String; override;
    class function Encode(const ACharSet, AData: String): TIdBytes; override;
    class function CanHandle(const ACharSet: String): Boolean; override;
  end;

implementation
uses SysUtils, IdException;

class function TIdHeaderCoderBig5.Decode(const ACharSet: string; const AData: TIdBytes): String;
begin
  Result := '';
  ToDo('Decode() method of TIdHeaderCoderBig5 class is not implemented yet'); {do not localize}
end;

class function TIdHeaderCoderBig5.Encode(const ACharSet, AData: String): TIdBytes;
begin
  Result := nil;
  ToDo('Encode() method of TIdHeaderCoderBig5 class is not implemented yet'); {do not localize}
end;

class function TIdHeaderCoderBig5.CanHandle(const ACharSet: String): Boolean;
begin
  Result := TextIsSame(ACharSet, 'Big5');
end;

initialization
  RegisterHeaderCoder(TIdHeaderCoderBig5);
finalization
  UnregisterHeaderCoder(TIdHeaderCoderBig5);

end.
