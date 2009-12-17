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
  Rev 1.5    2004.10.27 9:17:46 AM  czhower
  For TIdStrings

  Rev 1.4    7/28/04 11:43:32 PM  RLebeau
  Bug fix for CleanupCookieList()

  Rev 1.3    2004.02.03 5:45:02 PM  czhower
  Name changes

  Rev 1.2    1/22/2004 7:10:02 AM  JPMugaas
  Tried to fix AnsiSameText depreciation.

  Rev 1.1    2004.01.21 1:04:54 PM  czhower
  InitComponenet

  Rev 1.0    11/14/2002 02:16:26 PM  JPMugaas

  2001-Mar-31 Doychin Bondzhev
  - Added new method AddCookie2 that is called when we have Set-Cookie2 as response
  - The common code in AddCookie and AddCookie2 is now in DoAdd

  2001-Mar-24 Doychin Bondzhev
  - Added OnNewCookie event
    This event is called for every new cookie. Can be used to ask the user program
    do we have to store this cookie in the cookie collection
  - Added new method AddCookie
    This calls the OnNewCookie event and if the result is true it adds the new cookie
    in the collection
}

unit IdCookieManager;

{
  Implementation of the HTTP State Management Mechanism as specified in RFC 2109, 2965.

  Author: Doychin Bondzhev (doychin@dsoft-bg.com)
  Copyright: (c) Chad Z. Hower and The Indy Team.
}

interface

{$i IdCompilerDefines.inc}

uses
  IdBaseComponent,
  IdCookie,
  IdURI;

Type
  TOnNewCookieEvent = procedure(ASender: TObject; ACookie: TIdCookieRFC2109; var VAccept: Boolean) of object;

  TOnCookieManagerEvent = procedure(ASender: TObject; ACookieCollection: TIdCookies) of object;
  TOnCookieCreateEvent = TOnCookieManagerEvent;
  TOnCookieDestroyEvent = TOnCookieManagerEvent;

  TIdCookieManager = class(TIdBaseComponent)
  protected
    FOnCreate: TOnCookieCreateEvent;
    FOnDestroy:  TOnCookieDestroyEvent;
    FOnNewCookie: TOnNewCookieEvent;
    FCookieCollection: TIdCookies;

    procedure CleanupCookieList;
    procedure DoAdd(ACookie: TIdCookieRFC2109; ACookieText: String; AURL: TIdURI);
    procedure DoOnCreate; virtual;
    procedure DoOnDestroy; virtual;
    function DoOnNewCookie(ACookie: TIdCookieRFC2109): Boolean; virtual;
    procedure InitComponent; override;
  public
    destructor Destroy; override;
    //
    procedure AddCookie(ACookie: String; AURL: TIdURI);
    procedure AddCookie2(ACookie: String; AURL: TIdURI);
    procedure AddCookies(ASource: TIdCookieManager);
    procedure CopyCookie(ACookie: TIdCookieRFC2109);
    //
    function GenerateCookieList(URL: TIdURI; SecureConnection: Boolean = false): String;
    //
    property CookieCollection: TIdCookies read FCookieCollection;
  published
    property OnCreate: TOnCookieCreateEvent read FOnCreate write FOnCreate;
    property OnDestroy: TOnCookieDestroyEvent read FOnDestroy write FOnDestroy;
    property OnNewCookie: TOnNewCookieEvent read FOnNewCookie write FOnNewCookie;
  end;

implementation

uses
  IdAssignedNumbers, IdException, IdGlobal, IdGlobalProtocols, SysUtils;

function IsPortMatch(ACookie: TIdCookieRFC2965; const APort: String): Boolean;
var
  LPort: TIdPort;
  I: Integer;
begin
  {
  Per RFC 2965:
  
  ...

  Port Selection
      There are three possible behaviors, depending on the Port
      attribute in the Set-Cookie2 response header:

      1. By default (no Port attribute), the cookie MAY be sent to any
         port.

      2. If the attribute is present but has no value (e.g., Port), the
         cookie MUST only be sent to the request-port it was received
         from.

      3. If the attribute has a port-list, the cookie MUST only be
         returned if the new request-port is one of those listed in
         port-list.
  }

  if not ACookie.UsePort then
  begin
    Result := True;
    Exit;
  end;

  LPort := IndyStrToInt(APort, IdPORT_HTTP);

  if ACookie.PortCount = 0 then
  begin
    Result := (ACookie.RecvPort = LPort);
    Exit;
  end;

  for I := 0 to ACookie.PortCount-1 do
  begin
    if ACookie.PortList[I] = LPort then
    begin
      Result := True;
      Exit;
    end;
  end;
  
  Result := False;
end;
  
function IsRejectedCookie(ACookie: TIdCookieRFC2109; AURL: TIdURI): Boolean;
var
  S: string;
begin
  Result := True;

  {
  Per RFC 2109:

  To prevent possible security or privacy violations, a user agent
  rejects a cookie (shall not store its information) if any of the
  following is true:

   * The value for the Path attribute is not a prefix of the request-
     URI.

   * The value for the Domain attribute contains no embedded dots or
     does not start with a dot.

   * The value for the request-host does not domain-match the Domain
     attribute.

   * The request-host is a FQDN (not IP address) and has the form HD,
     where D is the value of the Domain attribute, and H is a string
     that contains one or more dots.
  }

  {
  Per RFC 2965:

  A user agent rejects (SHALL NOT store its information) if the Version
  attribute is missing.  Moreover, a user agent rejects (SHALL NOT
  store its information) if any of the following is true of the
  attributes explicitly present in the Set-Cookie2 response header:

    *  The value for the Path attribute is not a prefix of the
       request-URI.

    *  The value for the Domain attribute contains no embedded dots,
       and the value is not .local.

    *  The effective host name that derives from the request-host does
       not domain-match the Domain attribute.

    *  The request-host is a HDN (not IP address) and has the form HD,
       where D is the value of the Domain attribute, and H is a string
       that contains one or more dots.

    *  The Port attribute has a "port-list", and the request-port was
       not in the list.
  }
  
  if not TextStartsWith(AURL.Path, ACookie.Path) then begin
    Exit;
  end;

  S := ACookie.Domain;

  if ACookie is TIdCookieRFC2965 then
  begin
    if ACookie.Version < 1 then begin
      Exit;
    end;
    if CharEquals(s, 1, '.') then begin
      S := Copy(S, 2, MaxInt);
    end;
    if (Pos('.', S) = 0) and (not TextIsSame(ACookie.Domain, '.local')) then begin
      Exit;
    end;
    if not IsDomainMatch(EffectiveHostName(AURL.Host), ACookie.Domain) then begin
      Exit;
    end;
  end else
  begin
    if not CharEquals(s, 1, '.') then begin
      Exit;
    end;
    S := Copy(S, 2, MaxInt);
    if Pos('.', S) = 0 then begin
      Exit;
    end;
    if not IsDomainMatch(AURL.Host, ACookie.Domain) then begin
      Exit;
    end;
  end;

  if IsHostName(AURL.Host) and TextEndsWith(AURL.Host, ACookie.Domain) then
  begin
    S := Copy(AURL.Host, 1, Length(AURL.Host)-Length(ACookie.Domain));
    if Pos('.', S) <> 0 then begin
      Exit;
    end;
  end;
  
  if ACookie is TIdCookieRFC2965 then
  begin
    if not IsPortMatch(TIdCookieRFC2965(ACookie), AURL.Port) then begin
      Exit;
    end;
  end;
  
  Result := False;
end;

{ TIdCookieManager }

destructor TIdCookieManager.Destroy;
begin
  CleanupCookieList;
  DoOnDestroy;
  FreeAndNil(FCookieCollection);
  inherited Destroy;
end;

function TIdCookieManager.GenerateCookieList(URL: TIdURI; SecureConnection: Boolean = false): String;
Var
  S: String;
  i, j: Integer;
  LCookieList: TIdCookieList;
  LCookie: TIdNetscapeCookie;
  LResultList: TIdCookieList;
  LCookiesByDomain: TIdCookieDomainList;
begin
  CleanupCookieList;
  S := '';    {Do not Localize}
  LCookiesByDomain := FCookieCollection.LockCookieListByDomain(caRead);
  try
    if LCookiesByDomain.Count > 0 then
    begin
      LResultList := TIdCookieList.Create;
      try
        // Search for cookies for this domain
        for i := 0 to LCookiesByDomain.Count - 1 do
        begin
          if IsDomainMatch(EffectiveHostName(URL.Host), LCookiesByDomain.Strings[i]) then
          begin
            LCookieList := LCookiesByDomain.CookieList[i];
            for j := LCookieList.Count - 1 downto 0 do
            begin
              LCookie := LCookieList.Cookies[j];
              if (LCookie is TIdCookieRFC2965) and (not IsPortMatch(TIdCookieRFC2965(LCookie), URL.Port)) then begin
                Continue;
              end;
              if TextStartsWith(URL.Path, LCookie.Path) then
              begin
                if ((LCookie.Secure and SecureConnection) or (not LCookie.Secure)) and (LCookie.Value <> '') then    {Do not Localize}
                begin
                  LResultList.AddObject(LCookie.Path, LCookie);
                end;
              end;
            end;
          end;
        end;

        for i := LResultList.Count - 1 downto 0 do
        begin
          if Length(S) > 0 then begin
            S := S + '; ';    {Do not Localize}
          end;
          LCookie := LResultList.Cookies[i];
          S := S + LCookie.CookieName + '=' + LCookie.Value;    {Do not Localize}
        end;
      finally
        LResultList.Free;
      end;
    end;
  finally
    FCookieCollection.UnlockCookieListByDomain(caRead);
  end;
  Result := S;
end;

procedure TIdCookieManager.DoAdd(ACookie: TIdCookieRFC2109; ACookieText: String; AURL: TIdURI);
begin
  ACookie.CookieText := ACookieText;

  if Length(ACookie.Domain) = 0 then
  begin
    if ACookie is TIdCookieRFC2965 then begin
      ACookie.Domain := '.' + EffectiveHostName(AURL.Host); {Do not Localize}
    end else begin
      ACookie.Domain := '.' + AURL.Host; {Do not Localize}
    end;
  end
  else if (not TextStartsWith(ACookie.Domain, '.')) and {do not localize}
          (ACookie is TIdCookieRFC2965) then
  begin
    ACookie.Domain := '.' + ACookie.Domain; {do not localize}
  end;

  if Length(ACookie.Path) = 0 then begin
    ACookie.Path := AURL.Path;
    if CharEquals(ACookie.Path, Length(ACookie.Path), '/') then begin
      ACookie.Path := Copy(ACookie.Path, 1, Length(ACookie.Path)-1);
    end;
  end;

  if not IsRejectedCookie(ACookie, AURL) then
  begin
    if DoOnNewCookie(ACookie) then
    begin
      FCookieCollection.AddCookie(ACookie);
      Exit;
    end;
    ACookie.Collection := nil;
  end;

  ACookie.Free;
end;

procedure TIdCookieManager.AddCookie(ACookie: String; AURL: TIdURI);
var
  LCookie: TIdCookieRFC2109;
begin
  LCookie := FCookieCollection.Add;
  DoAdd(LCookie, ACookie, AURL);
end;

type
  TIdCookieRFC2965Access = class(TIdCookieRFC2965)
  end;

procedure TIdCookieManager.AddCookie2(ACookie: String; AURL: TIdURI);
var
  LCookie: TIdCookieRFC2965;
begin
  LCookie := FCookieCollection.Add2;
  TIdCookieRFC2965Access(LCookie).FRecvPort := IndyStrToInt(AURL.Port, IdPORT_HTTP);
  DoAdd(LCookie, ACookie, AURL);
end;

procedure TIdCookieManager.AddCookies(ASource: TIdCookieManager);
begin
  if (ASource <> nil) and (ASource <> Self) then begin
    FCookieCollection.AddCookies(ASource.CookieCollection);
  end;
end;

procedure TIdCookieManager.CopyCookie(ACookie: TIdCookieRFC2109);
var
  LCookie: TIdCookieRFC2109;
begin
  LCookie := TIdCookieRFC2109Class(ACookie.ClassType).Create(FCookieCollection);
  try
    LCookie.Assign(ACookie);

    // RLebeau: copied from DoAdd()...

    if (Length(LCookie.Domain) > 0) and
       (not TextStartsWith(ACookie.Domain, '.')) and {do not localize}
       (ACookie is TIdCookieRFC2965) then
    begin
      LCookie.Domain := '.' + LCookie.Domain; {do not localize}
    end;

    if Length(LCookie.Domain) > 0 then
    begin
      if DoOnNewCookie(LCookie) then
      begin
        FCookieCollection.AddCookie(LCookie);
        LCookie := nil;
      end;
    end;
  finally
    if LCookie <> nil then
    begin
      LCookie.Collection := nil;
      LCookie.Free;
    end;
  end;
end;

function TIdCookieManager.DoOnNewCookie(ACookie: TIdCookieRFC2109): Boolean;
begin
  Result := True;
  if Assigned(FOnNewCookie) then
  begin
    OnNewCookie(Self, ACookie, Result);
  end;
end;

procedure TIdCookieManager.DoOnCreate;
begin
  if Assigned(FOnCreate) then
  begin
    OnCreate(Self, FCookieCollection);
  end;
end;

procedure TIdCookieManager.DoOnDestroy;
begin
  if Assigned(FOnDestroy) then
  begin
    OnDestroy(Self, FCookieCollection);
  end;
end;

procedure TIdCookieManager.CleanupCookieList;
var
  S: String;
  i, j, LLastCount: Integer;
  LCookieList: TIdCookieList;
  LCookiesByDomain: TIdCookieDomainList;
begin
  LCookiesByDomain := FCookieCollection.LockCookieListByDomain(caReadWrite);
  try
    if LCookiesByDomain.Count > 0 then
    begin
      for i := 0 to LCookiesByDomain.Count - 1 do
      begin
        LCookieList := LCookiesByDomain.CookieList[i];

        for j := LCookieList.Count - 1 downto 0 do
        begin
          S := LCookieList.Cookies[j].Expires;
          if (Length(S) > 0) and (GMTToLocalDateTime(S) < Now) then
          begin
            // The Cookie has expired. It has to be removed from the collection
            LLastCount := LCookieList.Count; // RLebeau
            LCookieList.Cookies[j].Free;
            // RLebeau - the cookie may already be removed from the list via
            // its destructor.  If that happens then doing so again below can
            // cause an "index out of bounds" error, so don't do it if not needed.
            if LLastCount = LCookieList.Count then begin
              LCookieList.Delete(j);
            end;
          end;
        end;
      end;
    end;
  finally
    FCookieCollection.UnlockCookieListByDomain(caReadWrite);
  end;
end;

procedure TIdCookieManager.InitComponent;
begin
  inherited InitComponent;
  FCookieCollection := TIdCookies.Create(self);
  DoOnCreate;
end;

end.
