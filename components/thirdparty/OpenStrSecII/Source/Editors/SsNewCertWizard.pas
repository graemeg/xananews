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
unit SsNewCertWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CheckLst, ComCtrls;

type
  TfrmNewCertWizard = class(TForm)
    rgPurpose: TRadioGroup;
    lblPurposeHint: TLabel;
    clbSubjectNameFields: TListView;
    Label1: TLabel;
    Label2: TLabel;
    clbSubjectAltNameFields: TListView;
    Label3: TLabel;
    Label4: TLabel;
    cbPublicKeyAlgorithm: TComboBox;
    Label5: TLabel;
    edtPublicKeySize: TEdit;
    Label6: TLabel;
    edtValidityPeriod: TEdit;
    edtExportFileName: TEdit;
    Label7: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnOpen: TButton;
    edtTemplateFile: TEdit;
    Label8: TLabel;
    OpenDialog1: TOpenDialog;
    Label9: TLabel;
    edtClassName: TEdit;
    procedure rgPurposeClick(Sender: TObject);
    procedure cbPublicKeyAlgorithmChange(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmNewCertWizard: TfrmNewCertWizard;

implementation

{$R *.dfm}

procedure TfrmNewCertWizard.rgPurposeClick(Sender: TObject);
begin
  case rgPurpose.ItemIndex of
    0: begin
         lblPurposeHint.Caption := 'The expert will create a class that ' +
                                   'generates a Root CA Certificate.';
         edtExportFileName.Text := 'A:\Root.cer';
       end;
    1: begin
         lblPurposeHint.Caption := 'The expert will create a class that ' +
                                   'generates a Certificate signed by a local ' +
                                   'CA.';
         edtExportFileName.Text := 'A:\NewCert.pfx';
       end;
    2: begin
         lblPurposeHint.Caption := 'The expert will create a class that ' +
                                   'generates a PKCS#10 Certification Request.';
         edtExportFileName.Text := 'A:\NewCert.p10';
       end;
    3: begin
         lblPurposeHint.Caption := 'The expert will create a class that ' +
                                   'generates and signs a Certificate from a ' +
                                   'Certification Request.';
         edtExportFileName.Text := 'A:\NewCert.cer';
       end;
  end;
end;

procedure TfrmNewCertWizard.cbPublicKeyAlgorithmChange(Sender: TObject);
begin
  case cbPublicKeyAlgorithm.ItemHeight of
    0: edtPublicKeySize.Text := '1024';
    1: edtPublicKeySize.Text := '1024';
    2: edtPublicKeySize.Text := '256';
  end;
end;

procedure TfrmNewCertWizard.btnOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edtTemplateFile.Text := OpenDialog1.FileName;
end;

procedure TfrmNewCertWizard.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to clbSubjectNameFields.Items.Count - 1 do
    clbSubjectNameFields.Items[I].Checked := True;
  for I := 0 to clbSubjectAltNameFields.Items.Count - 1 do
    clbSubjectAltNameFields.Items[I].Checked := True;
  rgPurposeClick(Sender);
  cbPublicKeyAlgorithm.ItemIndex := 0;
end;

end.
