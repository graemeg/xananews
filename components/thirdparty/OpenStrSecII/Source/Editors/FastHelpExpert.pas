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
{$B-}
unit FastHelpExpert;

interface

uses
  SysUtils, Classes, ToolsAPI;

type
  EStrSecIIDocServices = class(Exception);

  TStrSecIIDocServices = class({$IFNDEF D5UP}TInterfacedObject,{$ELSE}
                                TNotifierObject,{$ENDIF}
                                {$IFDEF D5UP}
                                IOTANotifier, IOTAKeyboardBinding,
                                {$ENDIF D5UP}
                                IOTAWizard, IOTAMenuWizard)
  private
    FIndex: Integer;                                     
    {$IFDEF D5UP}
    procedure HelpKeyword(const Context: IOTAKeyContext; KeyCode: TShortcut;
      var BindingResult: TKeyBindingResult);
    function InvokeStrSecIIHelp(const Context: IOTAKeyContext;
                                Search: Boolean): Boolean;
    {$ENDIF D5UP}
    procedure SetIndex(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    {$IFNDEF D5UP}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    {$ENDIF}
    {$IFDEF D5UP}
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
    {$ENDIF D5UP}
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    function GetMenuText: string;
    property Index: Integer read FIndex write SetIndex;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows, Controls, Forms, Menus, Dialogs,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc, QControls, QForms, QMenus, QDialogs,
  {$ENDIF}
  PasToHTMLADS, FastHelpMain;

var
  StrSecIIDocServices: TStrSecIIDocServices;

{ TStrSecIIDocServices }

{$IFNDEF D5UP}
procedure TStrSecIIDocServices.AfterSave;
begin

end;

procedure TStrSecIIDocServices.BeforeSave;
begin

end;
{$ENDIF}
                     
{$IFDEF D5UP}
procedure TStrSecIIDocServices.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  try
    BindingServices.AddKeyBinding([ShortCut(VK_F1, [])], HelpKeyword, nil);
  except end;
  try
    BindingServices.AddKeyBinding([ShortCut(VK_F1, [ssCtrl])], HelpKeyword, nil);
  except end;
  try
    BindingServices.AddKeyBinding([ShortCut(VK_F1, [ssShift])], HelpKeyword, nil);
  except end;
end;
{$ENDIF D5UP}

constructor TStrSecIIDocServices.Create;
begin
  inherited Create;
end;

destructor TStrSecIIDocServices.Destroy;
var
  Frm: TfrmStrSecIIDoc;
begin
  Frm := frmStrSecIIDoc;
  frmStrSecIIDoc := nil;
  Frm.Free;
  inherited;
end;

{$IFNDEF D5UP}
procedure TStrSecIIDocServices.Destroyed;
begin

end;
{$ENDIF}

procedure TStrSecIIDocServices.Execute;
var
  Curs: TCursor;
begin
  Curs := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if frmStrSecIIDoc = nil then
      frmStrSecIIDoc := TfrmStrSecIIDoc.Create(nil);

    frmStrSecIIDoc.Show;
  finally
    Screen.Cursor := Curs;
  end;
end;
                          
{$IFDEF D5UP}
function TStrSecIIDocServices.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TStrSecIIDocServices.GetDisplayName: string;
begin
  Result := 'StrSecII Help F1 Capture';
end;
{$ENDIF D5UP}

function TStrSecIIDocServices.GetIDString: string;
begin
  Result := 'StreamSec.StrSecIIDocServices.Docs.1'; { do not localize }
end;

function TStrSecIIDocServices.GetMenuText: string;
resourcestring
  sMenuText = 'StrSecII Help';

begin
  Result := sMenuText;
end;

function TStrSecIIDocServices.GetName: string;
begin
  Result := 'StreamSec.StrSecIIDocServices.Docs'; { do not localize }
end;

function TStrSecIIDocServices.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;
                 
{$IFDEF D5UP}
procedure TStrSecIIDocServices.HelpKeyword(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  if InvokeStrSecIIHelp(Context,KeyCode = ShortCut(VK_F1, [ssShift])) then
    BindingResult := krHandled
  else
    BindingResult := krUnhandled
end;

function TStrSecIIDocServices.InvokeStrSecIIHelp(const Context: IOTAKeyContext;
  Search: Boolean): Boolean;
var
  ADS: TMember;
  EP: IOTAEditPosition;
  S: string;
begin
  EP := Context.EditBuffer.EditPosition;
  S := EP.RipText(['A'..'Z','a'..'z','_','.','0'..'9'],rfBackward) +
       EP.RipText(['A'..'Z','a'..'z','_','.','0'..'9'],0);
  if frmStrSecIIDoc = nil then
    frmStrSecIIDoc := TfrmStrSecIIDoc.Create(nil);
  ADS := frmStrSecIIDoc.GetModule(Context.EditBuffer.Module.FileName);
  if ADS = nil then
    ADS := frmStrSecIIDoc.ADS.LocateAnyByName(S)
  else begin
    ADS := ADS.LocateAnyByName(S);
    if ADS = nil then
      ADS := frmStrSecIIDoc.ADS.LocateAnyByName(S)
  end;
  if Assigned(ADS) then begin
    if (ADS is TClassesRecord) or
       (ADS is TRoutinesRecord) or
       (ADS is TAsnclientData1Record) then begin
      frmStrSecIIDoc.SelectMember(ADS);
      Result := True;
    end else if Search then begin
      frmStrSecIIDoc.PageControl1.ActivePage := frmStrSecIIDoc.TabSheet2;
      frmStrSecIIDoc.edtIndexSearch.Text := '';
      frmStrSecIIDoc.edtIndexSearch.Text := S;
      frmStrSecIIDoc.Show;
      frmStrSecIIDoc.RichEdit1.Clear;
      Result := True;
    end else
      Result := False;
  end else if Search then begin
    frmStrSecIIDoc.PageControl1.ActivePage := frmStrSecIIDoc.TabSheet3;
    frmStrSecIIDoc.edtSearch.Text := S;
    frmStrSecIIDoc.Show;
    Result := True;
  end else
    Result := False;
end;
{$ENDIF D5UP}

{$IFNDEF D5UP}
procedure TStrSecIIDocServices.Modified;
begin

end;
{$ENDIF}

procedure TStrSecIIDocServices.SetIndex(const Value: Integer);
begin
  FIndex := Value;
end;

resourcestring
  sStrSecIIDocError = 'Error creating StrSecIIDocServices wizard';

procedure InitStrSecIIDocServices;
begin
  if (BorlandIDEServices <> nil) then begin
    StrSecIIDocServices := TStrSecIIDocServices.Create;
    StrSecIIDocServices.Index := (BorlandIDEServices as IOTAWizardServices).AddWizard(StrSecIIDocServices as IOTAWizard);
    {$IFDEF D5UP}
    try
      (BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(StrSecIIDocServices);
    except
    end;
    {$ENDIF D5UP}
    if StrSecIIDocServices.Index < 0 then
      raise EStrSecIIDocServices.Create(sStrSecIIDocError);
  end;
end;

procedure DoneStrSecIIDocServices;
var
  Frm: TfrmStrSecIIDoc;
begin
  if (BorlandIDEServices <> nil) then
  begin
    if frmStrSecIIDoc <> nil then begin
      Frm := frmStrSecIIDoc;
      frmStrSecIIDoc := nil;
      Frm.Free;
    end;

    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(StrSecIIDocServices.Index);
  end;
end;
           
initialization
  InitStrSecIIDocServices;

finalization
  DoneStrSecIIDocServices;
  
end.
