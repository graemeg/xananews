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
{$Q-,R-}
{$I ver.inc}
unit UserPWUnit;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Graphics, Forms, Controls, StdCtrls, Buttons,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc, Qt, QGraphics, QForms, QControls, QStdCtrls, QButtons,
  {$ENDIF}
  SysUtils, Classes;

resourcestring
  PasswordCaption = 'Enter password for %s';
  PasswordPrompt = 'Enter password:';
  CancelCaption = 'Cancel';

type
  TUserPWDlg = class(TForm)
    Label1: TLabel;
    PasswordEdt: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    Label2: TLabel;
    UsernameEdt: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PasswordEdtKeyPress(Sender: TObject; var Key: Char);
    procedure PasswordEdtKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PasswordEdtClick(Sender: TObject);
    procedure UsernameEdtKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FTargetName: string;
    procedure SetTargetName(const Value: string);
    { Private declarations }
  public
    Password: string;
    PWLength: Integer;
    property TargetName: string read FTargetName write SetTargetName;
  end;

implementation

{$IFDEF MSWINDOWS}
{$R *.DFM}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TUserPWDlg.FormCreate(Sender: TObject);
begin
  Label1.Caption := PasswordPrompt;
  CancelBtn.Caption := CancelCaption;
  Password := StringOfChar(#0,1024);
end;

procedure TUserPWDlg.FormShow(Sender: TObject);
begin
  FillChar(Pointer(Password)^,1024,$FF);
  FillChar(Pointer(Password)^,1024,$AA);
  FillChar(Pointer(Password)^,1024,$55);
  FillChar(Pointer(Password)^,1024,$00);
  PasswordEdt.Text := '';
  PWLength := 0;
end;

procedure TUserPWDlg.PasswordEdtKeyPress(Sender: TObject; var Key: Char);
begin
  if Key in [#0..#30] then Exit;
  Password[PWLength + 1] := Key;
  Inc(PWLength);
  Key := '*'
end;

procedure TUserPWDlg.PasswordEdtKeyDown(Sender: TObject; var Key: Word;
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

procedure TUserPWDlg.SetTargetName(const Value: string);
begin
  FTargetName := Value;
  Caption := Format(PasswordCaption,[Value]);
end;

procedure TUserPWDlg.PasswordEdtClick(Sender: TObject);
begin
  PWLength := 0;
  PasswordEdt.Text := '';
end;

procedure TUserPWDlg.UsernameEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = {$IFDEF MSWINDOWS}VK_TAB{$ENDIF}
           {$IFDEF LINUX}Key_Tab{$ENDIF} then begin
    ActiveControl := PasswordEdt;
    Key := {$IFDEF MSWINDOWS}VK_INSERT{$ENDIF}
           {$IFDEF LINUX}Key_Insert{$ENDIF};
  end;
end;

end.

