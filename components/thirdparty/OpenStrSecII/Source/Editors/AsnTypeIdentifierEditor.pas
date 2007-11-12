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
unit AsnTypeIdentifierEditor;

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc, QForms, QStdCtrls, QControls,
  {$ENDIF}
  Asn1;

type
  TfrmTypeIdentifierEditor = class(TForm)
    Label2: TLabel;
    cbObjectIdentifier: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    cbTypeName: TComboBox;
  private
    FChoice: PASN1Struct;
    procedure SetChoice(const Value: PASN1Struct);
    { Private declarations }
  public
    property Choice: PASN1Struct read FChoice write SetChoice;
  end;

var
  frmTypeIdentifierEditor: TfrmTypeIdentifierEditor;

implementation

{$R *.dfm}

{ TfrmTypeIdentifierEditor }

procedure TfrmTypeIdentifierEditor.SetChoice(const Value: PASN1Struct);
begin
  FChoice := Value;
  cbTypeName.Text := Value^.TypeName;
  cbObjectIdentifier.Text := Value^.IdentifiedBy;
end;

end.
