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
unit SignCodeWizardDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;
                       
const
  MapFileOptionName      = 'MapFile';
  OutputDirOptionName    = 'OutputDir';
  RuntimeOnlyOptionName  = 'RuntimeOnly';
  PkgDllDirOptionName    = 'PkgDllDir';
  BPLOutputDirOptionName = 'PackageDPLOutput';
  LIBPREFIXOptionName    = 'SOPrefix';
  LIBSUFFIXOptionName    = 'SOSuffix';

  MapFileOptionDetailed  = 3;

  BPLExtension           = '.bpl';
  DPKExtension           = '.dpk';
  PrelExtension          = '.exe';
  DRCExtension           = '.drc';
  
type
  TfrmSignCodeWizard = class(TForm)
    Label2: TLabel;
    edtDescription: TEdit;
    Label3: TLabel;
    edtURL: TEdit;
    Label5: TLabel;
    edtPrivateKeyRing: TEdit;
    Label4: TLabel;
    edtMyCertificates: TEdit;
    Label6: TLabel;
    edtRootCertificates: TEdit;
    btnBrowseRoot: TButton;
    btnBrowseUser: TButton;
    btnBrowsePKR: TButton;
    OpenDialog1: TOpenDialog;
    btnOK: TButton;
    Button2: TButton;
    procedure btnBrowsePKRClick(Sender: TObject);
    procedure btnBrowseUserClick(Sender: TObject);
    procedure btnBrowseRootClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetDescription: string;
    function GetMyCerts: string;
    function GetPrivateKeyRing: string;
    function GetRootCerts: string;
    function GetURL: string;
    procedure SetDescription(const Value: string);
    procedure SetMyCerts(const Value: string);
    procedure SetPrivateKeyRing(const Value: string);
    procedure SetRootCerts(const Value: string);
    procedure SetURL(const Value: string);
    function GetModified: Boolean;
  public
    property Description: string read GetDescription write SetDescription;
    property URL: string read GetURL write SetURL;
    property PrivateKeyRing: string read GetPrivateKeyRing write SetPrivateKeyRing;
    property MyCerts: string read GetMyCerts write SetMyCerts;
    property RootCerts: string read GetRootCerts write SetRootCerts;
    property Modified: Boolean read GetModified;
  end;

var
  frmSignCodeWizard: TfrmSignCodeWizard;

implementation

{$R *.dfm}

{ TfrmSignCodeWizard }

function TfrmSignCodeWizard.GetDescription: string;
begin
  Result := edtDescription.Text;
end;

function TfrmSignCodeWizard.GetMyCerts: string;
begin
  Result := edtMyCertificates.Text;
end;

function TfrmSignCodeWizard.GetPrivateKeyRing: string;
begin
  Result := edtPrivateKeyRing.Text;
end;

function TfrmSignCodeWizard.GetRootCerts: string;
begin
  Result := edtRootCertificates.Text;
end;

function TfrmSignCodeWizard.GetURL: string;
begin
  Result := edtURL.Text;
end;

procedure TfrmSignCodeWizard.SetDescription(const Value: string);
begin
  edtDescription.Text := Value;
end;

procedure TfrmSignCodeWizard.SetMyCerts(const Value: string);
begin
  edtMyCertificates.Text := Value;
end;

procedure TfrmSignCodeWizard.SetPrivateKeyRing(const Value: string);
begin
  edtPrivateKeyRing.Text := Value;
end;

procedure TfrmSignCodeWizard.SetRootCerts(const Value: string);
begin
  edtRootCertificates.Text := Value;
end;

procedure TfrmSignCodeWizard.SetURL(const Value: string);
begin
  edtURL.Text := Value;
end;

procedure TfrmSignCodeWizard.btnBrowsePKRClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Private key ring (*.pkr)|*.pkr';
  if OpenDialog1.Execute then
    PrivateKeyRing := OpenDialog1.FileName;
end;

procedure TfrmSignCodeWizard.btnBrowseUserClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Certificate file (*.scl,*.p7b,*.cer)|*.scl;*.p7b;*.cer';
  if OpenDialog1.Execute then
    MyCerts := OpenDialog1.FileName;
end;

procedure TfrmSignCodeWizard.btnBrowseRootClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Certificate file (*.scl,*.p7b,*.cer)|*.scl;*.p7b;*.cer';
  if OpenDialog1.Execute then
    RootCerts := OpenDialog1.FileName;
end;

procedure TfrmSignCodeWizard.FormShow(Sender: TObject);
begin
  edtDescription.Modified := False;
  edtURL.Modified := False;
  edtPrivateKeyRing.Modified := False;
  edtMyCertificates.Modified := False;
  edtRootCertificates.Modified := False;
end;

function TfrmSignCodeWizard.GetModified: Boolean;
begin
  Result := edtDescription.Modified or
            edtURL.Modified or
            edtPrivateKeyRing.Modified or
            edtMyCertificates.Modified or
            edtRootCertificates.Modified;
end;

end.
