{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     PasswordMain Unit                                 }
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
{$R-}
{$I ver.inc}
unit PasswordMain;

interface     

uses
  NewPWUnit, PasswordUnit, UserPWUnit,
  {$IFDEF MSWINDOWS}
  Controls,
  {$ENDIF}
  {$IFDEF LINUX}
  QControls,
  {$ENDIF}
  SecUtils;

  function NewPasswordDlg(TargetName: string;
                          var Value: string): Boolean; overload;
  function PasswordDlg(TargetName: string;
                       var Value: string): Boolean; overload;
  function PasswordDlg(TargetName: string;
                       var Value: string;
                       var Save: Boolean): Boolean; overload;
  function UserPasswordDlg(TargetName: string;
                           var Name, Password: string): Boolean; overload;

  function NewPasswordDlg(TargetName: string;
                          var Password: ISecretKey;
                          AConvertToBMPString: Boolean = False): Boolean; overload;
  function PasswordDlg(TargetName: string;
                       var Password: ISecretKey;
                       AConvertToBMPString: Boolean = False): Boolean; overload;
  function PasswordDlg(TargetName: string;
                       var Password: ISecretKey;
                       var Save: Boolean;
                       AConvertToBMPString: Boolean = False): Boolean; overload;
  function UserPasswordDlg(TargetName: string;
                           var Name: string;
                           var Password: ISecretKey;
                           AConvertToBMPString: Boolean = False): Boolean; overload;

procedure ConvertToBMPString(Password: ISecretKey; PW: PAnsiChar; PWLen: Integer); overload;
procedure ConvertToBMPString(Password: ISecretKey; PW: PWideChar; PWLen: Integer); overload;

implementation

function NewPasswordDlg(TargetName: string; var Value: string): Boolean;
var
  Dlg: TNewPasswordDlg;
begin
  Dlg := ANewPasswordDlg;
  try
    Dlg.TargetName := TargetName;
    Result := Dlg.ShowModal = mrOK;
    if Result then begin
      ProtectClear(Value[1],Length(Value));
      Value := Copy(Dlg.Password,1,Dlg.PWLength);
    end;
  finally
    Dlg.Free;
  end;
end;

function PasswordDlg(TargetName: string; var Value: string): Boolean;
var
  Dlg: TPasswordDlg;
begin
  Dlg := CreatePasswordDlg;
  try
    Dlg.TargetName := TargetName;
    Result := Dlg.ShowModal = mrOK;
    if Result then begin
      ProtectClear(Pointer(Value)^,Length(Value));
      Value := Copy(Dlg.Password,1,Dlg.PWLength);
    end;
  finally
    Dlg.Free;
  end;
end;

function PasswordDlg( TargetName: string; var Value: string;
                        var Save: Boolean): Boolean;
var
  Dlg: TPasswordDlg;
begin
  Dlg := TPasswordDlg.Create(nil);
  try
    Dlg.TargetName := TargetName;
    Dlg.CheckBox1.Visible := True;
    Dlg.CheckBox1.Checked := Save;
    Dlg.OldPassword := Value;
    Result := Dlg.ShowModal = mrOK;
    if Result then begin
      ProtectClear(Value[1],Length(Value));
      Value := Copy(Dlg.Password,1,Dlg.PWLength);
    end;
    Save := Dlg.CheckBox1.Checked;
  finally
    Dlg.Free;
  end;
end;

function UserPasswordDlg(TargetName: string; var Name, Password: string): Boolean;
var
  Dlg: TUserPWDlg;
begin
  Dlg := TUserPWDlg.Create(nil);
  try
    Dlg.UsernameEdt.Text := Name;
    Dlg.TargetName := TargetName;
    if Name = '' then
      Dlg.ActiveControl := Dlg.UsernameEdt
    else
      Dlg.ActiveControl := Dlg.PasswordEdt;
    Result := Dlg.ShowModal = mrOK;
    if Result then begin
      Name := Dlg.UsernameEdt.Text;
      ProtectClear(Password[1],Length(Password));
      Password := Copy(Dlg.Password,1,Dlg.PWLength);
    end;
  finally
    Dlg.Free;
  end;
end;

procedure ConvertToBMPString(Password: ISecretKey; PW: PAnsiChar; PWLen: Integer);
var
  I: Integer;
  P: PAnsiChar;
begin
  Password.SetLength((PWLen+1)*2);
  P := Password.Key;
  FillChar(P^,(PWLen+1)*2,0);
  for I := 0 to PWLen-1 do
    P[I*2+1] := PW[I];
end;

procedure ConvertToBMPString(Password: ISecretKey; PW: PWideChar; PWLen: Integer);
var
  I: Integer;
  D, S: PAnsiChar;
begin
  Password.SetLength((PWLen+1)*2);
  D := Password.Key;
  FillChar(D^,(PWLen+1)*2,0);
  S := Pointer(PW);
  for I := 0 to PWLen-1 do begin
    D[I*2] := S[I*2+1];
    D[I*2+1] := S[I*2];
  end;
end;

function NewPasswordDlg(TargetName: string;
                        var Password: ISecretKey;
                        AConvertToBMPString: Boolean): Boolean;
var
  Dlg: TNewPasswordDlg;
begin
  Dlg := ANewPasswordDlg;
  try
    Dlg.TargetName := TargetName;
    Result := Dlg.ShowModal = mrOK;
    if Result then begin
      if Password = nil then Password := TSecretKey.Create('');
      if AConvertToBMPString then
        ConvertToBMPString(Password,PChar(Dlg.Password),Dlg.PWLength)
      else
        Password.SetKey(Pointer(Dlg.Password),Dlg.PWLength,0);
    end;
  finally
    Dlg.Free;
  end;
end;

function PasswordDlg(TargetName: string;
                     var Password: ISecretKey;
                     AConvertToBMPString: Boolean): Boolean;
var
  Dlg: TPasswordDlg;
begin
  Dlg := CreatePasswordDlg;
  try
    Dlg.TargetName := TargetName;
    Result := Dlg.ShowModal = mrOK;
    if Result then begin
      if Password = nil then Password := TSecretKey.Create('');
      if AConvertToBMPString then
        ConvertToBMPString(Password,PChar(Dlg.Password),Dlg.PWLength)
      else
        Password.SetKey(Pointer(Dlg.Password),Dlg.PWLength,0);
    end;
  finally
    Dlg.Free;
  end;
end;

function PasswordDlg(TargetName: string;
                     var Password: ISecretKey;
                     var Save: Boolean;
                     AConvertToBMPString: Boolean): Boolean;
var
  Dlg: TPasswordDlg;
begin
  Dlg := TPasswordDlg.Create(nil);
  try
    Dlg.TargetName := TargetName;
    Dlg.CheckBox1.Visible := True;
    Dlg.CheckBox1.Checked := Save;
    if Assigned(Password) then
      Dlg.PutOldPassword(Password.Key,Password.KeyLen)
    else
      Dlg.OldPassword := '';
    Result := Dlg.ShowModal = mrOK;
    if Result then begin
      if Password = nil then Password := TSecretKey.Create('');;
      if AConvertToBMPString then
        ConvertToBMPString(Password,PChar(Dlg.Password),Dlg.PWLength)
      else
        Password.SetKey(Pointer(Dlg.Password),Dlg.PWLength,0);
    end;
    Save := Dlg.CheckBox1.Checked;
  finally
    Dlg.Free;
  end;
end;

function UserPasswordDlg(TargetName: string;
                         var Name: string;
                         var Password: ISecretKey;
                         AConvertToBMPString: Boolean): Boolean;
var
  Dlg: TUserPWDlg;
begin
  Dlg := TUserPWDlg.Create(nil);
  try
    Dlg.UsernameEdt.Text := Name;
    Dlg.TargetName := TargetName;
    if Name = '' then
      Dlg.ActiveControl := Dlg.UsernameEdt
    else
      Dlg.ActiveControl := Dlg.PasswordEdt;
    Result := Dlg.ShowModal = mrOK;
    if Result then begin
      Name := Dlg.UsernameEdt.Text;
      if Password = nil then Password := TSecretKey.Create('');
      if AConvertToBMPString then
        ConvertToBMPString(Password,PChar(Dlg.Password),Dlg.PWLength)
      else
        Password.SetKey(Pointer(Dlg.Password),Dlg.PWLength,0);
    end;
  finally
    Dlg.Free;
  end;
end;

end.
