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
}
{$I ver.inc}
unit NewPWUnit;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc, Qt, QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  {$ENDIF}
  SysUtils, Classes;

type
  TNewPasswordDlg = class(TForm)
    Label1: TLabel;
    PasswordEdt: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    Label2: TLabel;
    ConfirmPWEdt: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PasswordEdtKeyPress(Sender: TObject; var Key: Char);
    procedure PasswordEdtKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ConfirmPWEdtKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ConfirmPWEdtKeyPress(Sender: TObject; var Key: Char);
    procedure ConfirmPWEdtClick(Sender: TObject);
    procedure PasswordEdtClick(Sender: TObject);
  private
    FTargetName: string;
    procedure SetTargetName(const Value: string);
    { Private declarations }
  public
    Password: string;
    Confirm: string;
    PWLength: Integer;
    CPWLength: Integer;
    property TargetName: string read FTargetName write SetTargetName;
  end;

function ANewPasswordDlg: TNewPasswordDlg;

var
  NewPasswordDlg: TNewPasswordDlg;

implementation

uses
  PasswordUnit;

{$IFDEF MSWINDOWS}
{$R *.DFM}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

function ANewPasswordDlg: TNewPasswordDlg;
begin
  Result := TNewPasswordDlg.Create(nil);
end;

procedure TNewPasswordDlg.FormCreate(Sender: TObject);
begin
  Label1.Caption := NewPasswordPrompt;
  Label2.Caption := ConfirmPrompt;
  CancelBtn.Caption := CancelCaption;
  Password := StringOfChar(#0,1024);
  Confirm := StringOfChar(#0,1024);
end;

procedure TNewPasswordDlg.FormShow(Sender: TObject);
begin
  FillChar(Password[1],1024,$FF);
  FillChar(Password[1],1024,$AA);
  FillChar(Password[1],1024,$55);
  FillChar(Password[1],1024,$00);
  FillChar(Confirm[1],1024,$FF);
  FillChar(Confirm[1],1024,$AA);
  FillChar(Confirm[1],1024,$55);
  FillChar(Confirm[1],1024,$00);
  PasswordEdt.Text := '';
  ConfirmPWEdt.Text := '';
  PWLength := 0;
  CPWLength := 0;
  OKBtn.Enabled := False;
end;

procedure TNewPasswordDlg.PasswordEdtKeyPress(Sender: TObject; var Key: Char);
begin
  if Key in [#0..#30] then Exit;
  Password[PWLength + 1] := Key;
  Inc(PWLength);
  Key := '*';
  if (StrLComp(@Password[1],@Confirm[1],PWLength) = 0) and
     (PWLength = CPWLength) and
     (PWLength >= 12) then OKBtn.Enabled := True
end;

procedure TNewPasswordDlg.PasswordEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  {$IFDEF MSWINDOWS}
    VK_DELETE,VK_BACK,VK_INSERT:
  {$ENDIF}
  {$IFDEF LINUX}
    Key_Delete,Key_Backspace,Key_Insert:
  {$ENDIF}
    begin
      PWLength := 0;
      PasswordEdt.Text := '';
    end;
  end;
end;

procedure TNewPasswordDlg.ConfirmPWEdtKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
  {$IFDEF MSWINDOWS}
    VK_DELETE,VK_BACK,VK_INSERT:
  {$ENDIF}
  {$IFDEF LINUX}
    Key_Delete,Key_Backspace,Key_Insert:
  {$ENDIF}
    begin
      CPWLength := 0;
      ConfirmPWEdt.Text := '';
      OKBtn.Enabled := False;
    end;
  end;
end;

procedure TNewPasswordDlg.ConfirmPWEdtKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#0..#30]) then begin;
    Confirm[CPWLength + 1] := Key;
    Inc(CPWLength);
    Key := '*';
    if (StrLComp(@Password[1],@Confirm[1],PWLength) = 0) and
       (PWLength = CPWLength) and
       (PWLength >= 12) then
      OKBtn.Enabled := True
  end
end;

procedure TNewPasswordDlg.ConfirmPWEdtClick(Sender: TObject);
begin
  ConfirmPWEdt.Text := '';
  CPWLength := 0;
end;

procedure TNewPasswordDlg.PasswordEdtClick(Sender: TObject);
begin
  PasswordEdt.Text := '';
  PWLength := 0;
end;

procedure TNewPasswordDlg.SetTargetName(const Value: string);
begin
  FTargetName := Value;
  Caption := Format(NewPasswordCaption,[Value]);
end;

end.
