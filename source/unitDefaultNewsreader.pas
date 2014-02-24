(* ======================================================================*
  | unitDefaultNewsreader unit for NewsReader3                           |
  |                                                                      |
  | Utility functions to make XanaNews the default newsreader on Vista   |
  | and later Windows systems.                                                                     |
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
  | Copyright © Pieter Zijlstra 2010  All Rights Reserved                |
  |                                                                      |
  | Version    Date        By    Description                             |
  | -------    ----------  ----  ----------------------------------------|
  | 1.19.1.136 04/11/2010  PZ    Original                                |
  *====================================================================== *)
unit unitDefaultNewsreader;

interface

uses
  Windows, SysUtils, Forms;

function CheckSetAsDefaultNewsreader: Boolean;
procedure SetupXanaNewsAssociations;
procedure SetupShellURLAssociations;

implementation

uses
  Registry, ShellApi;

const
  REG_NEWS = 'XanaNews.Url.news';
  REG_NNTP = 'XanaNews.Url.nntp';
  REG_SNEWS = 'XanaNews.Url.snews';

procedure SetupXanaNewsAssociations;
var
  filename: string;
  reg: TRegistry;
  reg1: TRegistry;

  procedure SetPaths(rootKey: HKEY);
  var
    reg: TRegistry;
  begin
    reg := TRegistry.Create;
    try
      reg.rootKey := rootKey;
      if reg.OpenKey('\DefaultIcon', True) then
        reg.WriteString('', filename + ',-1');

      if reg.OpenKey('\shell\open\command', True) then
        reg.WriteString('', '"' + filename + '" "%1"');
    finally
      reg.Free;
    end;
  end;

  procedure SetGeneralURLInfo(const proto: string; friendly: Boolean = False);
  var
    editflags: Integer;
  begin
    editflags := 2;
    reg1.WriteString('', 'URL:' + proto + ' Protocol');
    reg1.WriteInteger('EditFlags', editflags);
    if friendly then
      reg1.WriteExpandString('FriendlyTypeName', proto + ' Protocol');
    reg1.WriteString('URL Protocol', '');
  end;

begin
  filename := Application.ExeName;

  reg := TRegistry.Create;
  try
    reg.rootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('\SOFTWARE\Classes', True) then
    begin
      reg1 := TRegistry.Create;
      try
        reg1.rootKey := reg.CurrentKey;
        if reg1.OpenKey('\' + REG_NEWS, True) then
        begin
          SetGeneralURLInfo('News', True);
          SetPaths(reg1.CurrentKey);
        end;

        if reg1.OpenKey('\' + REG_NNTP, True) then
        begin
          SetGeneralURLInfo('NNTP', True);
          SetPaths(reg1.CurrentKey);
        end;

        if reg1.OpenKey('\' + REG_SNEWS, True) then
        begin
          SetGeneralURLInfo('Snews', True);
          SetPaths(reg1.CurrentKey);
        end;

      finally
        reg1.Free;
      end;
    end;

    reg.rootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('\SOFTWARE\Clients\News\XanaNews', True) then
    begin
      reg.WriteString('', 'XanaNews');

      reg1 := TRegistry.Create;
      try
        reg1.rootKey := reg.CurrentKey;

        if reg1.OpenKey('\Capabilities', True) then
        begin
          reg1.WriteExpandString('ApplicationDescription', 'Newsreader');
          reg1.WriteExpandString('ApplicationName', 'XanaNews');
        end;

        if reg1.OpenKey('\Capabilities\FileAssociations', True) then; // empty

        if reg1.OpenKey('\Capabilities\URLAssociations', True) then
        begin
          reg1.WriteString('news', 'XanaNews.Url.news');
          reg1.WriteString('nntp', 'XanaNews.Url.nntp');
          reg1.WriteString('snews', 'XanaNews.Url.snews');
        end;

        if reg1.OpenKey('\Protocols\news', True) then
        begin
          SetGeneralURLInfo('News');
          SetPaths(reg1.CurrentKey);
        end;

        if reg1.OpenKey('\Protocols\nntp', True) then
        begin
          SetGeneralURLInfo('NNTP');
          SetPaths(reg1.CurrentKey);
        end;

        if reg1.OpenKey('\Protocols\snews', True) then
        begin
          SetGeneralURLInfo('Snews');
          SetPaths(reg1.CurrentKey);
        end;

        if reg1.OpenKey('\shell\open\command', True) then
          reg1.WriteExpandString('', '"' + filename + '"');

      finally
        reg1.Free;
      end;
    end;

    reg.rootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('\SOFTWARE\RegisteredApplications', True) then
      reg.WriteString('XanaNews', 'Software\Clients\News\XanaNews\Capabilities');

  finally
    reg.Free;
  end;
end;

procedure SetupShellURLAssociations;
const
  HKCU_URL = '\Software\Microsoft\Windows\Shell\Associations\UrlAssociations';
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.rootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(HKCU_URL + '\news\UserChoice', True) then
      reg.WriteString('Progid', REG_NEWS);
    if reg.OpenKey(HKCU_URL + '\nntp\UserChoice', True) then
      reg.WriteString('Progid', REG_NNTP);
    if reg.OpenKey(HKCU_URL + '\snews\UserChoice', True) then
      reg.WriteString('Progid', REG_SNEWS);
  finally
    reg.Free;
  end;
end;

function CheckSetAsDefaultNewsreader: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if ParamStr(I) = '-sadnr' then
    begin
      SetupXanaNewsAssociations;
      SetupShellURLAssociations;
      Result := True;
      Exit;
    end;
end;

end.
