{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SsCertMgrUtils Unit                               }
{
  LICENSE

  Copyright (c) 2004, Henrick Wibell Hellström, StreamSec
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of StreamSec nor the names of its contributors may be
      used to endorse or promote products derived from this software without
      specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
{*******************************************************}
{$I ver.inc}
unit SsCertMgrUtils;

interface

uses
  Classes;

type
  TSsCertMgrPaths = class
  public
    class function GetCertMgrExeName: string;          
    class procedure RegisterCertMgr(AExeName: string);
    class function GetCertMgrSrvExeName: string;
    class procedure RegisterCertMgrSrv(AExeName: string);
    class function GetDefaultPathName: string;
    class procedure GetNamesAndPaths(AStrings: TStrings);
    class procedure GetPaths(AStrings: TStrings);
    class function GetPathName(APath: string): string;
    class function GetPath(AName: string): string;
    class procedure RegisterPath(APath, AName: string);
    class procedure LockPath(APath: string);
    class procedure UnLockPath(APath: string);
    class function PathIsLocked(APath: string): Boolean;
  end;
     
  TSignCertReq = procedure (AHandle: LongInt;
                            ACACertStoreName: PWideChar;
                            CertReq: Pointer; CertReqLen: LongInt;
                            Cert: Pointer; var CertLen: LongInt); stdcall;
  TCheckSignCertReq = function (AHandle: LongInt;
                                out CertReq: Pointer; out CertReqLen: LongInt): Boolean; stdcall;
  TSignedCertReq = procedure (AHandle: LongInt;
                              Cert: Pointer; CertLen: LongInt); stdcall;
var
  GCliHandle: LongInt;
  GSignCertReq: TSignCertReq = nil;
  GCheckSignCertReq: TCheckSignCertReq = nil;
  GSignedCertReq: TSignedCertReq = nil;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF MSWINDOWS}
  SysUtils;
            
{$IFDEF MSWINDOWS}
function OpenKey(Reg: TRegistry): Boolean;
begin
  Result := False;
  Reg.RootKey := HKEY_CURRENT_USER;
  if Reg.OpenKey('SOFTWARE',True) then
    if Reg.OpenKey('StreamSec',True) then
      if Reg.OpenKey('StrSecII',True) then
        if Reg.OpenKey('CertMgr',True) then
          Result := True;
end;              
{$ENDIF MSWINDOWS}

{ TSsCertMgrPaths }

class function TSsCertMgrPaths.GetCertMgrExeName: string;
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF MSWINDOWS}
begin
{$IFDEF MSWINDOWS}
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE',True) then
      if Reg.OpenKey('StreamSec',True) then
        if Reg.OpenKey('StrSecII',True) then
          if Reg.OpenKey('CertMgrApp',True) then
            if Reg.ValueExists('CertMgr') then
              Result := Reg.ReadString('CertMgr');
  finally
    Reg.Free;
  end;
{$ENDIF MSWINDOWS}
end;

class function TSsCertMgrPaths.GetCertMgrSrvExeName: string;
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF MSWINDOWS}
begin
{$IFDEF MSWINDOWS}
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE',True) then
      if Reg.OpenKey('StreamSec',True) then
        if Reg.OpenKey('StrSecII',True) then
          if Reg.OpenKey('CertMgrApp',True) then
            if Reg.ValueExists('CertMgrSrv') then
              Result := Reg.ReadString('CertMgrSrv');
  finally
    Reg.Free;
  end;
{$ENDIF MSWINDOWS}
end;

class function TSsCertMgrPaths.GetDefaultPathName: string;
begin
  Result := '(unnamed)';
end;

class procedure TSsCertMgrPaths.GetNamesAndPaths(AStrings: TStrings);
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
  SL: TStringList;
  I: Integer;
{$ENDIF MSWINDOWS}
begin
  AStrings.Clear;
{$IFDEF MSWINDOWS}
  Reg := TRegistry.Create;
  try
    if OpenKey(Reg) then begin
      SL := TStringList.Create;
      try
        Reg.GetValueNames(SL);
        for I := 0 to SL.Count - 1 do
          AStrings.Values[Reg.ReadString(SL[I])] := SL[I];
      finally
        SL.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
{$ENDIF MSWINDOWS}
end;

class function TSsCertMgrPaths.GetPath(AName: string): string;
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
  SL: TStringList;
  I: Integer;
{$ENDIF MSWINDOWS}
begin
  Result := '';
{$IFDEF MSWINDOWS}
  Reg := TRegistry.Create;
  try
    if OpenKey(Reg) then begin
      SL := TStringList.Create;
      try
        Reg.GetValueNames(SL);
        for I := 0 to SL.Count - 1 do begin
          Result := SL[I];
          if Reg.ReadString(Result) = AName then
            Break;
          Result := '';
        end;
      finally
        SL.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
{$ENDIF MSWINDOWS}
end;

class function TSsCertMgrPaths.GetPathName(APath: string): string;
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF MSWINDOWS}
begin
  Result := '';
{$IFDEF MSWINDOWS}
  Reg := TRegistry.Create;
  try
    if OpenKey(Reg) then
      Result := Reg.ReadString(APath);
  finally
    Reg.Free;
  end;
{$ENDIF MSWINDOWS}
end;

class procedure TSsCertMgrPaths.GetPaths(AStrings: TStrings);
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF MSWINDOWS}
begin
{$IFDEF MSWINDOWS}
  Reg := TRegistry.Create;
  try
    if OpenKey(Reg) then
      Reg.GetValueNames(AStrings);
  finally
    Reg.Free;
  end;
{$ENDIF MSWINDOWS}
end;

class procedure TSsCertMgrPaths.LockPath(APath: string);
begin
  with TFileStream.Create(Format('%sCertMgrLock',[APath]),fmCreate) do
    Free;
end;

class function TSsCertMgrPaths.PathIsLocked(APath: string): Boolean;
begin
  Result := FileExists(Format('%sCertMgrLock',[APath]));
end;

class procedure TSsCertMgrPaths.RegisterCertMgr(AExeName: string);
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF MSWINDOWS}
begin
{$IFDEF MSWINDOWS}
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE',True) then
      if Reg.OpenKey('StreamSec',True) then
        if Reg.OpenKey('StrSecII',True) then
          if Reg.OpenKey('CertMgrApp',True) then
            if not Reg.ValueExists('CertMgr') then
              Reg.WriteString('CertMgr',AExeName);
  finally
    Reg.Free;
  end;
{$ENDIF MSWINDOWS}
end;

class procedure TSsCertMgrPaths.RegisterCertMgrSrv(AExeName: string);
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF MSWINDOWS}
begin
{$IFDEF MSWINDOWS}
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE',True) then
      if Reg.OpenKey('StreamSec',True) then
        if Reg.OpenKey('StrSecII',True) then
          if Reg.OpenKey('CertMgrApp',True) then
            if not Reg.ValueExists('CertMgrSrv') then
              Reg.WriteString('CertMgrSrv',AExeName);
  finally
    Reg.Free;
  end;
{$ENDIF MSWINDOWS}
end;

class procedure TSsCertMgrPaths.RegisterPath(APath, AName: string);
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF MSWINDOWS}
begin
{$IFDEF MSWINDOWS}
  Reg := TRegistry.Create;
  try
    if OpenKey(Reg) then
      Reg.WriteString(APath,AName);
  finally
    Reg.Free;
  end;
{$ENDIF MSWINDOWS}
end;

class procedure TSsCertMgrPaths.UnLockPath(APath: string);
begin
  DeleteFile(Format('%sCertMgrLock',[APath]));
end;

end.
