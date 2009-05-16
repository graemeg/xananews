{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}
{
  Rev 1.6    2004.10.27 9:17:46 AM  czhower
  For TIdStrings

  Rev 1.5    10/26/2004 11:08:08 PM  JPMugaas
  Updated refs.

  Rev 1.4    13.04.2004 12:56:44  ARybin
  M$ IE behavior

  Rev 1.3    2004.02.03 5:45:00 PM  czhower
  Name changes

  Rev 1.2    2004.01.22 6:09:02 PM  czhower
  IdCriticalSection

  Rev 1.1    1/22/2004 7:09:58 AM  JPMugaas
  Tried to fix AnsiSameText depreciation.

  Rev 1.0    11/14/2002 02:16:20 PM  JPMugaas

  Mar-31-2001 Doychin Bondzhev
  - Changes in the class heirarchy to implement Netscape specification[Netscape],
      RFC 2109[RFC2109] & 2965[RFC2965]

  Feb-2001 Doychin Bondzhev
  - Initial release
}

unit IdCookie;

{
  Implementation of the HTTP State Management Mechanism as specified in RFC 2109, 2965.
  Author: Doychin Bondzhev (doychin@dsoft-bg.com)
  Copyright: (c) Chad Z. Hower and The Indy Team.

  TIdNetscapeCookie - The base code used in all cookies. It implments cookies
  as proposed by Netscape

    TIdCookieRFC2109 - The RFC 2109 implmentation. Not used too much.

  TIdCookieRFC2965 - The RFC 2965 implmentation. Not used yet or at least I don't
    know any HTTP server that supports this specification.

  TIdServerCooke - Used in the HTTP server compoenent.

REFERENCES
-------------------
 [Netscape] "Persistent Client State -- HTTP Cookies",
            formerly available at <http://www.netscape.com/newsref/std/cookie_spec.html>,
            now at <http://curl.haxx.se/rfc/cookie_spec.html>,
	    undated.

 [RFC2109]  Kristol, D. and L. Montulli, "HTTP State Management Mechanism",
            RFC 2109, February 1997.

 [RFC2965]  Kristol, D. and L. Montulli, "HTTP State Management Mechanism",
            RFC 2965, October 2000.

Implementation status
--------------------------

 [Netscape] - 100%
 [RFC2109]  - 100% (there is still some code to write and debugging)
 [RFC2965]  -  70% (client and server cookie generation is not ready)
}

// TODO: Make this unit to implement completely [Netscape], [RFC2109] & [RFC2965]

interface

{$i IdCompilerDefines.inc}

uses
  Classes,
  IdGlobal, IdException, IdGlobalProtocols, SysUtils;
  //must keep for now

const
  GFMaxAge = -1;

type
  TIdNetscapeCookie = class;

  TIdCookieList = class(TStringList)
  protected
    function GetCookie(Index: Integer): TIdNetscapeCookie;
  public
    property Cookies[Index: Integer]: TIdNetscapeCookie read GetCookie;
  end;

  TIdCookieDomainList = class(TStringList)
  protected
    function GetCookieList(Index: Integer): TIdCookieList;
  public
    property CookieList[Index: Integer]: TIdCookieList read GetCookieList;
  end;

  {
    Base Cookie class as described in
    "Persistent Client State -- HTTP Cookies"
  }
  TIdNetscapeCookie = class(TCollectionItem)
  protected
    FCookieText: String;
    FDomain: String;
    FExpires: String;
    FHttpOnly: Boolean;
    FName: String;
    FPath: String;
    FSecure: Boolean;
    FValue: String;

    function GetCookie: String; virtual;
    procedure SetExpires(const AValue: String); virtual;
    procedure SetCookie(const AValue: String);

    function GetServerCookie: String; virtual;
    function GetClientCookie: String; virtual;

    procedure LoadProperties(APropertyList: TStrings); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property ClientCookie: String read GetClientCookie;
    property CookieName: String read FName write FName;
    property CookieText: String read GetCookie write SetCookie;
    property Domain: String read FDomain write FDomain;
    property Expires: String read FExpires write SetExpires;
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
    property Path: String read FPath write FPath;
    property Secure: Boolean read FSecure write FSecure;
    property ServerCookie: String read GetServerCookie;
    property Value: String read FValue write FValue;
  end;

  { Cookie as described in [RFC2109] }
  // Adds Version, Secure and MaxAge
  TIdCookieRFC2109 = class(TIdNetscapeCookie)
  protected
    FMax_Age: Int64;
    FVersion: Integer;
    FComment: String;

    function GetClientCookie: String; override;
    function GetCookie: String; override;
    procedure SetExpires(const AValue: String); override;
    procedure LoadProperties(APropertyList: TStrings); override;
  public
    constructor Create(ACollection: TCollection); override;

    property Comment: String read FComment write FComment;
    property MaxAge: Int64 read FMax_Age write FMax_Age;
    property Version: Integer read FVersion write FVersion;
  end;

  { Cookie as described in [RFC2965] }
  // Adds CommentURL, Discard, Port and Version is now required
  TIdCookieRFC2965 = class(TIdCookieRFC2109)
  protected
    FCommentURL: String;
    FDiscard: Boolean;
    FPortList: array of TIdPort;
    FRecvPort: TIdPort;
    FUsePort: Boolean;

    function GetCookie: String; override;
    function GetPort(AIndex: Integer): TIdPort;
    function GetPortCount: Integer;
    procedure LoadProperties(APropertyList: TStrings); override;
    procedure SetPort(AIndex: Integer; AValue: TIdPort);
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;

    property CommentURL: String read FCommentURL write FCommentURL;
    property Discard: Boolean read FDiscard write FDiscard;
    property PortCount: Integer read GetPortCount;
    property PortList[AIndex: Integer]: TIdPort read GetPort write SetPort;
    property UsePort: Boolean read FUsePort;
    property RecvPort: TIdPort read FRecvPort;
  end;

  TIdNetscapeCookieClass = class of TIdNetscapeCookie;
  TIdCookieRFC2109Class = class of TIdCookieRFC2109;
  TIdCookieRFC2965Class = class of TIdCookieRFC2965;

  { Used in the HTTP server }
  // This class descends from TIdCookieRFC2109 but uses Expires and not Max-Age which is not
  // supported from new browsers
  TIdServerCookie = class(TIdCookieRFC2109)
  protected
    function GetCookie: String; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure AddAttribute(const Attribute, Value: String);
  end;

  { The Cookie collection }

  TIdCookieAccess = (caRead, caReadWrite);

  TIdCookies = class(TOwnedCollection)
  protected
    FCookieListByDomain: TIdCookieDomainList;
    FRWLock: TMultiReadExclusiveWriteSynchronizer;

    function GetCookie(const AName, ADomain: string): TIdCookieRFC2109;
    function GetItem(Index: Integer): TIdCookieRFC2109;
    procedure SetItem(Index: Integer; const Value: TIdCookieRFC2109);
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;

    function Add: TIdCookieRFC2109;
    function Add2: TIdCookieRFC2965;

    procedure AddCookie(ACookie: TIdCookieRFC2109);
    procedure AddSrcCookie(const ACookie: string);
    procedure AddCookies(ASource: TIdCookies);

    procedure Assign(ASource: TPersistent); override;
    procedure Clear; reintroduce;
    procedure Delete(Index: Integer);

    function GetCookieIndex(FirstIndex: Integer; const AName: string): Integer; overload;
    function GetCookieIndex(FirstIndex: Integer; const AName, ADomain: string): Integer; overload;

    function LockCookieListByDomain(AAccessType: TIdCookieAccess): TIdCookieDomainList;
    procedure UnlockCookieListByDomain(AAccessType: TIdCookieAccess);

    property Cookie[const AName, ADomain: string]: TIdCookieRFC2109 read GetCookie;
    property Items[Index: Integer]: TIdCookieRFC2109 read GetItem write SetItem; Default;
  end;

  TIdServerCookies = class(TIdCookies)
  protected
    function GetCookie(const AName: string): TIdCookieRFC2109;
  public
    function Add: TIdServerCookie;

    property Cookie[const AName: string]: TIdCookieRFC2109 read GetCookie;
  end;

implementation

uses
  IdAssignedNumbers;
  
{ base functions used for construction of Cookie text }

function AddCookieProperty(const AProperty, AValue, ACookie: String): String;
begin
  Result := ACookie;
  if Length(AValue) > 0 then
  begin
    if Length(Result) > 0 then
    begin
      Result := Result + '; ';    {Do not Localize}
    end;
    Result := Result + AProperty + '=' + AValue;    {Do not Localize}
  end;
end;

function AddCookieFlag(const AFlag, ACookie: String): String;
begin
  Result := ACookie;
  if Length(Result) > 0 then
  begin
    Result := Result + '; ';    {Do not Localize}
  end;
  Result := Result + AFlag;
end;

{ TIdCookieList }

function TIdCookieList.GetCookie(Index: Integer): TIdNetscapeCookie;
begin
  Result := TIdNetscapeCookie(Objects[Index]);
end;

{ TIdDomainList }

function TIdCookieDomainList.GetCookieList(Index: Integer): TIdCookieList;
begin
  Result := TIdCookieList(Objects[Index]);
end;

{ TIdNetscapeCookie }

constructor TIdNetscapeCookie.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TIdNetscapeCookie.Destroy;
Var
  LListByDomain: TIdCookieDomainList;
  LCookieList: TIdCookieList;
  i: Integer;
begin
  if Assigned(Collection) then try
    LListByDomain := TIdCookies(Collection).LockCookieListByDomain(caReadWrite);
    if Assigned(LListByDomain) then try
      i := LListByDomain.IndexOf(Domain);
      if i > -1 then
      begin
        LCookieList := LListByDomain.CookieList[i];
        i := LCookieList.IndexOf(CookieName);
        if i > -1 then
        begin
          LCookieList.Delete(i);
        end;
      end;
    finally
      TIdCookies(Collection).UnlockCookieListByDomain(caReadWrite);
    end;
  finally
    inherited Destroy;
  end;
end;

procedure TIdNetscapeCookie.Assign(Source: TPersistent);
begin
  if Source is TIdCookieRFC2109 then
  begin
    CookieText := TIdCookieRFC2109(Source).CookieText;
  end else begin
    inherited Assign(Source);
  end;
end;

procedure TIdNetscapeCookie.SetExpires(const AValue: String);
begin
  FExpires := AValue;
end;

{
Set-Cookie: NAME=VALUE; expires=DATE;
path=PATH; domain=DOMAIN_NAME; secure; HttpOnly
}
function TIdNetscapeCookie.GetServerCookie: String;
begin
  Result := GetCookie;
end;

{
Cookie: NAME1=OPAQUE_STRING1; NAME2=OPAQUE_STRING2 ...
}
function TIdNetscapeCookie.GetClientCookie: String;
begin
  Result := FName + '=' + FValue;    {Do not Localize}
end;

function TIdNetscapeCookie.GetCookie: String;
begin
  Result := AddCookieProperty(FName, FValue, '');    {Do not Localize}
  Result := AddCookieProperty('path', FPath, Result);    {Do not Localize}
  Result := AddCookieProperty('expires', FExpires, Result);    {Do not Localize}
  Result := AddCookieProperty('domain', FDomain, Result);    {Do not Localize}
  if FSecure then
  begin
    Result := AddCookieFlag('secure', Result);    {Do not Localize}
  end;
  if FHttpOnly then
  begin
    Result := AddCookieFlag('HttpOnly', Result);    {Do not Localize}
  end;
end;

procedure TIdNetscapeCookie.LoadProperties(APropertyList: TStrings);
var
  s: string;
begin
  FPath := APropertyList.Values['PATH'];    {Do not Localize}
  // Tomcat can return SetCookie2 with path wrapped in "
  if Length(FPath) > 0 then
  begin
    if FPath[1] = '"' then begin   {Do not Localize}
      Delete(FPath, 1, 1);
    end;
    if (FPath <> '') and (FPath[Length(FPath)] = '"') then begin   {Do not Localize}
      SetLength(FPath, Length(FPath) - 1);
    end;
  end
  else begin
    FPath := '/'; {Do not Localize}
  end;
  Expires := APropertyList.Values['EXPIRES'];    {Do not Localize}

  // RLebeau: have encountered one cookie in the 'Set-Cookie' header that
  // includes a port number in the domain, though the RFCs do not indicate
  // this is allowed. RFC 2965 defines an explicit "port" attribute in the
  // 'Set-Cookie2' header for that purpose instead. We'll just strip it off
  // here if present...
  s := APropertyList.Values['DOMAIN'];    {Do not Localize}
  FDomain := Fetch(s, ':');    {Do not Localize}

  FSecure := APropertyList.IndexOfName('SECURE') <> -1;    {Do not Localize}
  FHttpOnly := APropertyList.IndexOfName('HTTPONLY') <> -1;    {Do not Localize}
end;

procedure TIdNetscapeCookie.SetCookie(const AValue: String);
Var
  i, j: Integer;
  CookieProp: TStringList;
  LTemp: String;
begin
  if AValue <> FCookieText then
  begin
    FCookieText := AValue;

    CookieProp := TStringList.Create;
    try
      LTemp := Trim(AValue);
      while LTemp <> '' do    {Do not Localize}
      begin
        CookieProp.Add(Trim(Fetch(LTemp, ';')));    {Do not Localize}
        LTemp := Trim(LTemp);
      end;

      FName := CookieProp.Names[0];
      FValue := CookieProp.Values[FName];
      CookieProp.Delete(0);

      for i := 0 to CookieProp.Count - 1 do
      begin
        j := Pos('=', CookieProp[i]);    {Do not Localize}
        if j = 0 then begin    {Do not Localize}
          CookieProp[i] := UpperCase(CookieProp[i]) + '=';  // This is for cookie flags (secure)
        end else begin
          CookieProp[i] := UpperCase(CookieProp.Names[i]) + '=' + Copy(CookieProp[i], j+1, MaxInt);    {Do not Localize}
        end;
      end;

      LoadProperties(CookieProp);
    finally
      FreeAndNil(CookieProp);
    end;
  end;
end;

{ TIdCookieRFC2109 }

constructor TIdCookieRFC2109.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMax_Age := GFMaxAge;
end;

procedure TIdCookieRFC2109.SetExpires(const AValue: String);
begin
  if Length(AValue) > 0 then
  begin
    try
      // If you see an exception here then that means the HTTP server has returned an invalid expires
      // date/time value. The correct format is Wdy, DD-Mon-YY HH:MM:SS GMT

      // AValue := StringReplace(AValue, '-', ' ', [rfReplaceAll]);    {Do not Localize}
      FMax_Age := Trunc((GMTToLocalDateTime(AValue) - Now) * MSecsPerDay / 1000);
    except end;
  end;
  inherited SetExpires(AValue);
end;

{
   cookie          =       "Cookie:" cookie-version
                           1*((";" | ",") cookie-value)
   cookie-value    =       NAME "=" VALUE [";" path] [";" domain]
   cookie-version  =       "$Version" "=" value
   NAME            =       attr
   VALUE           =       value
   path            =       "$Path" "=" value
   domain          =       "$Domain" "=" value
}

function TIdCookieRFC2109.GetClientCookie: String;
begin
  Result := inherited GetClientCookie; 

  {if (Length(Version) > 0) and (Length(result) > 0) then
  begin
    result := AddCookieProperty('$Version', '"' + Version + '"', '') + ';' + result;
  end;

  result := AddCookieProperty('$Path', Path, result);
  if IsDomain(Domain) then
  begin
    result := AddCookieProperty('$Domain', Domain, result);    
  end;}
end;

{
   set-cookie      =       "Set-Cookie:" cookies
   cookies         =       1#cookie
   cookie          =       NAME "=" VALUE *(";" cookie-av)
   NAME            =       attr
   VALUE           =       value
   cookie-av       =       "Comment" "=" value
                   |       "Domain" "=" value
                   |       "Max-Age" "=" value
                   |       "Path" "=" value
                   |       "Secure"
                   |       "Version" "=" 1*DIGIT
}
function TIdCookieRFC2109.GetCookie: String;
var
  LExpires: String;
begin
  if FVersion > 0 then
  begin
    LExpires := FExpires;
    try
      FExpires := '';
      Result := inherited GetCookie;
    finally
      FExpires := LExpires;
    end;
    if FMax_Age > -1 then
    begin
      Result := AddCookieProperty('max-age', IntToStr(FMax_Age), Result);    {Do not Localize}
    end;
  end else
  begin
    Result := inherited GetCookie;
  end;

  Result := AddCookieProperty('comment', FComment, Result);    {Do not Localize}

  if FVersion > 0 then begin
    Result := AddCookieProperty('version', IntToStr(FVersion), Result);    {Do not Localize}
  end;
end;

procedure TIdCookieRFC2109.LoadProperties(APropertyList: TStrings);
begin
  inherited LoadProperties(APropertyList);

  FMax_Age := IndyStrToInt(APropertyList.Values['MAX-AGE'], -1);    {Do not Localize}
  FVersion := IndyStrToInt(APropertyList.Values['VERSION'], 0);    {Do not Localize}
  FComment := APropertyList.Values['COMMENT'];    {Do not Localize}

  if (Length(Expires) = 0) and (FMax_Age > -1) then begin
    Expires := LocalDateTimeToCookieStr(Now + FMax_Age * 1000 / MSecsPerDay);
  end;
end;

{ TIdCookieRFC2965 }

constructor TIdCookieRFC2965.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  SetLength(FPortList, 0);
end;

procedure TIdCookieRFC2965.Assign(Source: TPersistent);
begin
  if Source is TIdCookieRFC2965 then begin
    FRecvPort := TIdCookieRFC2965(Source).RecvPort;
  end;
  inherited Assign(Source);
end;

function TIdCookieRFC2965.GetCookie: String;
var
  s: String;
  i: Integer;
begin
  Result := inherited GetCookie;

  if FUsePort then
  begin
    if PortCount > 0 then
    begin
      s := IntToStr(PortList[0]);
      for i := 1 to PortCount-1 do begin
        s := s + ',' + IntToStr(PortList[i]); {Do not Localize}
      end;
      Result := AddCookieProperty('port', '"' + s + '"', Result); {Do not Localize}
    end else begin
      Result := AddCookieFlag('port', Result); {Do not Localize}
    end;
  end;
end;

procedure TIdCookieRFC2965.LoadProperties(APropertyList: TStrings);
Var
  LPortList: TStringList;
  i: Integer;
  S: String;
begin
  SetLength(FPortList, 0);
  inherited LoadProperties(APropertyList);

  FCommentURL := APropertyList.Values['COMMENTURL'];    {Do not Localize}
  FDiscard := APropertyList.IndexOfName('DISCARD') <> -1;    {Do not Localize}
  FUsePort := APropertyList.IndexOfName('PORT') <> -1;  {Do not Localize}

  if FUsePort then
  begin
    LPortList := TStringList.Create;
    try
      S := APropertyList.Values['PORT'];    {Do not Localize}
      if Length(S) > 0 then
      begin
        if (S[1] = '"') and (S[Length(S)] = '"') then    {Do not Localize}
        begin
          LPortList.CommaText := Copy(S, 2, Length(S) - 2);
          SetLength(FPortList, LPortList.Count);
          for i := 0 to LPortList.Count - 1 do
          begin
            PortList[i] := IndyStrToInt(LPortList[i]);
          end;
        end;
      end;
    finally
      LPortList.Free;
    end;
  end;
end;

procedure TIdCookieRFC2965.SetPort(AIndex: Integer; AValue: TIdPort);
begin
  if ((AIndex - High(FPortList)) > 1) or (AIndex < Low(FPortList)) then
  begin
    raise EIdException.Create('Index out of range.');    {Do not Localize}
  end;
  if (AIndex - High(FPortList)) = 1 then
  begin
    SetLength(FPortList, AIndex + 1);
  end;
  FPortList[AIndex] := AValue;
end;

function TIdCookieRFC2965.GetPortCount: Integer;
begin
  Result := Length(FPortList);
end;

function TIdCookieRFC2965.GetPort(AIndex: Integer): TIdPort;
begin
  if (AIndex > High(FPortList)) or (AIndex < Low(FPortList)) then
  begin
    raise EIdException.Create('Index out of range.');    {Do not Localize}
  end;
  Result := FPortList[AIndex];
end;

{ TIdServerCookie }

constructor TIdServerCookie.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  // Version := '1';    {Do not Localize}
end;

function TIdServerCookie.GetCookie: String;
begin
  if FMax_Age >= 0 then
  begin
    FExpires := LocalDateTimeToCookieStr(Now + FMax_Age * 1000 / MSecsPerDay);
  end;
  Result := inherited GetCookie;
end;

procedure TIdServerCookie.AddAttribute(const Attribute, Value: String);
begin
  case PosInStrArray(Attribute, ['$PATH', '$DOMAIN', '$VERSION'], False) of    {Do not Localize}
    0: Path := Value;
    1: Domain := Value;
    2: FVersion := IndyStrToInt(Value, 0);
  end;
end;

{ TIdCookies }

constructor TIdCookies.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TIdCookieRFC2109);
  FRWLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FCookieListByDomain := TIdCookieDomainList.Create;
end;

destructor TIdCookies.Destroy;
begin
  // This will force the Cookie removing process before we free the FCookieListByDomain and FRWLock
  Self.Clear;
  FreeAndNil(FCookieListByDomain);
  FreeAndNil(FRWLock);
  inherited Destroy;
end;

procedure TIdCookies.AddCookie(ACookie: TIdCookieRFC2109);
var
  LList: TIdCookieList;
  LIndex: Integer;
begin
  with LockCookieListByDomain(caReadWrite) do
  try
    LIndex := IndexOf(ACookie.Domain);
    if LIndex = -1 then
    begin
      LList := TIdCookieList.Create;
      try
        AddObject(ACookie.Domain, LList);
      except
        FreeAndNil(LList);
        raise;
      end;
    end
    else begin
      LList := TIdCookieList(Objects[LIndex]);
    end;

    LIndex := LList.IndexOf(ACookie.CookieName);
    if LIndex = -1 then
    begin
      LList.AddObject(ACookie.CookieName, TIdNetscapeCookie(ACookie));
    end
    else begin
      TIdCookieRFC2109(LList.Objects[LIndex]).Assign(ACookie);
      ACookie.Collection := nil;
      ACookie.Free;
    end;
  finally
    UnlockCookieListByDomain(caReadWrite);
  end;
end;

procedure TIdCookies.Assign(ASource: TPersistent);
begin
  if (ASource = nil) or (ASource is TIdCookies) then
  begin
    LockCookieListByDomain(caReadWrite);
    try
      Self.Clear;
      AddCookies(TIdCookies(ASource));
    finally
      UnlockCookieListByDomain(caReadWrite);
    end;
  end else begin
    inherited Assign(ASource);
  end;
end;

function TIdCookies.GetItem(Index: Integer): TIdCookieRFC2109;
begin
  Result := (inherited Items[Index]) as TIdCookieRFC2109;
end;

procedure TIdCookies.SetItem(Index: Integer; const Value: TIdCookieRFC2109);
begin
  inherited Items[Index] := Value;
end;

function TIdCookies.Add: TIdCookieRFC2109;
begin
  Result := TIdCookieRFC2109.Create(Self);
end;

function TIdCookies.Add2: TIdCookieRFC2965;
begin
  Result := TIdCookieRFC2965.Create(Self);
end;

procedure TIdCookies.AddSrcCookie(const ACookie: string);
begin
  Add.CookieText := ACookie;
end;

procedure TIdCookies.AddCookies(ASource: TIdCookies);
var
  LSrcDomains: TIdCookieDomainList;
  LSrcCookies: TIdCookieList;
  LSrcCookie: TIdNetscapeCookie;
  LDestCookie: TIdCookieRFC2109;
  I, J: Integer;
begin
  if (ASource <> nil) and (ASource <> Self) then
  begin
    LSrcDomains := ASource.LockCookieListByDomain(caRead);
    try
      LockCookieListByDomain(caReadWrite);
      try
        for I := 0 to LSrcDomains.Count-1 do
	begin
          LSrcCookies := LSrcDomains.CookieList[I];
          for J := 0 to LSrcCookies.Count-1 do
          begin
            LSrcCookie := LSrcCookies.Cookies[J];
            LDestCookie := TIdCookieRFC2109Class(LSrcCookie.ClassType).Create(Self);
            try
              LDestCookie.Assign(LSrcCookie);
              AddCookie(LDestCookie);
            except
              LDestCookie.Collection := nil;
              LDestCookie.Free;
              raise;
            end;
          end;
	end;
      finally
        UnlockCookieListByDomain(caReadWrite);
      end;
    finally
      ASource.UnlockCookieListByDomain(caRead);
    end;
  end;
end;

function TIdCookies.GetCookie(const AName, ADomain: string): TIdCookieRFC2109;
var
  i: Integer;
begin
  i := GetCookieIndex(0, AName, ADomain);
  if i = -1 then
  begin
    Result := nil;
  end
  else begin
    Result := Items[i];
  end;
end;

function TIdCookies.GetCookieIndex(FirstIndex: Integer; const AName, ADomain: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := FirstIndex to Count - 1 do
  begin
    if TextIsSame(Items[i].CookieName, AName) and TextIsSame(Items[i].Domain, ADomain) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TIdCookies.GetCookieIndex(FirstIndex: Integer; const AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := FirstIndex to Count - 1 do
  begin
    if TextIsSame(Items[i].CookieName, AName) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TIdCookies.Clear;
var
  I: Integer;
begin
  LockCookieListByDomain(caReadWrite);
  try
    inherited Clear;
    for I := 0 to FCookieListByDomain.Count-1 do begin
      FCookieListByDomain.Objects[I].Free;
    end;
    FCookieListByDomain.Clear;
  finally
    UnlockCookieListByDomain(caReadWrite);
  end;
end;

procedure TIdCookies.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TIdCookies.LockCookieListByDomain(AAccessType: TIdCookieAccess): TIdCookieDomainList;
begin
  case AAccessType of
    caRead:
      begin
        FRWLock.BeginRead;
      end;
    caReadWrite:
      begin
        FRWLock.BeginWrite;
      end;
  end;
  Result := FCookieListByDomain;
end;

procedure TIdCookies.UnlockCookieListByDomain(AAccessType: TIdCookieAccess);
begin
  case AAccessType of
    caRead:
      begin
        FRWLock.EndRead;
      end;
    caReadWrite:
      begin
        FRWLock.EndWrite;
      end;
  end;
end;

{ TIdServerCookies }

function TIdServerCookies.Add: TIdServerCookie;
begin
  Result := TIdServerCookie.Create(Self);
end;

function TIdServerCookies.GetCookie(const AName: string): TIdCookieRFC2109;
var
  i: Integer;
begin
  i := GetCookieIndex(0, AName);
  if i = -1 then begin
    Result := nil;
  end else begin
    Result := Items[i];
  end;
end;

end.
