unit IdHeaderCoderUTF;

interface

{$i IdCompilerDefines.inc}

uses
  IdGlobal, IdHeaderCoderBase;

type
  TIdHeaderCoderUTF = class(TIdHeaderCoder)
  public
    class function Decode(const ACharSet, AData: String): String; override;
    class function Encode(const ACharSet, AData: String): String; override;
    class function CanHandle(const ACharSet: String): Boolean; override;
  end;

implementation

class function TIdHeaderCoderUTF.Decode(const ACharSet, AData: String): String;
var
  LEncoding: TIdTextEncoding;
  LBytes: TIdBytes;
begin
  Result := '';
  LBytes := nil;
  if TextIsSame(ACharSet, 'UTF-7') then begin {do not localize}
    LEncoding := TIdTextEncoding.UTF7;
  end
  else if TextIsSame(ACharSet, 'UTF-8') then begin {do not localize}
    LEncoding := TIdTextEncoding.UTF8;
  end else
  begin
    Exit;
  end;
  {$IFDEF DOTNET_OR_UNICODESTRING}
  // RLebeau 1/27/09: do not use the same Encoding class to decode the input
  // string to bytes and then decode the bytes to a string.  Doing so will
  // undo what TIdTextEncoding.Convert() does, effectively making this class
  // behave the same as TIdHeaderCoderPlain.  The output of this class needs
  // to be a string that contains codeunits in the UTF-16 range, not
  // codeunits that have been converted back to the input encoding...
  LBytes := Indy8BitEncoding.GetBytes(AData);
  {$ELSE}
  // RLebeau 2/12/09: Not using TIdTextEncoding.GetBytes() here. Although
  // the input string (should) contain the correct values, the conversion
  // performed by the RTL when assigning an AnsiString to the WideString
  // parameter can change characters!!  Just assign the input characters
  // directly to the buffer to avoid that...
  if AData <> '' then begin
    LBytes := RawToBytes(PChar(AData)^, Length(AData));
  end;
  {$ENDIF}
  LBytes := TIdTextEncoding.Convert(
    LEncoding,
    TIdTextEncoding.Unicode,
    LBytes);
  Result := TIdTextEncoding.Unicode.GetString(LBytes, 0, Length(LBytes));
end;

class function TIdHeaderCoderUTF.Encode(const ACharSet, AData: String): String;
var
  LBytes: TIdBytes;
  LEncoding: TIdTextEncoding;
begin
  Result := '';
  LBytes := nil;
  if TextIsSame(ACharSet, 'UTF-7') then begin {do not localize}
    LEncoding := TIdTextEncoding.UTF7;
  end
  else if TextIsSame(ACharSet, 'UTF-8') then begin {do not localize}
    LEncoding := TIdTextEncoding.UTF8;
  end else
  begin
    Exit;
  end;
  LBytes := TIdTextEncoding.Convert(
    TIdTextEncoding.Unicode,
    LEncoding,
    TIdTextEncoding.Unicode.GetBytes(AData));
  {$IFDEF DOTNET_OR_UNICODESTRING}
  // RLebeau 1/27/09: do not use the same Encoding class to encode the input
  // string to bytes and then encode the bytes to a string.  Doing so will
  // undo what TIdTextEncoding.Convert() does, effectively making this class
  // behave the same as TIdHeaderCoderPlain.  The output of this class needs
  // to be a string that contains codeunits in the UTF-7/8 Ansi range, not
  // codeunits that have been converted back to UTF-16...
  Result := Indy8BitEncoding.GetString(LBytes, 0, Length(LBytes));
  {$ELSE}
  // RLebeau 2/12/09: Not using TIdTextEncoding.GetString() here. Although
  // the encoded bytes contain the correct values, the conversion performed
  // by the RTL when assigning a WideString to the AnsiString Result can
  // lose characters!!  Just assign the encoded bytes directly to the Result
  // to avoid that...
  SetString(Result, PAnsiChar(LBytes), Length(LBytes));
  {$ENDIF}
end;

class function TIdHeaderCoderUTF.CanHandle(const ACharSet: String): Boolean;
begin
  Result := PosInStrArray(ACharSet, ['UTF-7', 'UTF-8'], False) > -1; {do not localize}
end;

initialization
  RegisterHeaderCoder(TIdHeaderCoderUTF);
finalization
  UnregisterHeaderCoder(TIdHeaderCoderUTF);

end.
