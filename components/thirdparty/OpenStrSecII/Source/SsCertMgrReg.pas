{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SsCertMgrReg Unit                                 }
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
unit SsCertMgrReg;

interface

uses
  Classes,
  {$IFDEF LINUX}
  QDialogs, DesignIntf, DesignWindows, DesignEditors, DesignMenus, QForms,
  QControls,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  {$IFDEF D6UP}
  DesignIntf, DesignEditors, DesignMenus,
  {$ELSE}
  DsgnIntf, DsgnWnds, Menus,
  {$ENDIF}
  Dialogs, Controls,
  {$ENDIF}
  Asn1, TlsInternalServer, SsCertMgr;

type
  TX509CertificateAuthorityEditor = class(TComponentEditor)
  private
    {$IFDEF D5UP}
    FCACert: TASN1Struct;
    {$ENDIF}
    FSignCert: TASN1Struct;
  public
    destructor Destroy; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    {$IFDEF D6UP}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
    {$ELSE}
    {$IFDEF D5UP}
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;
    {$ENDIF}
    {$ENDIF}
  end;

procedure Register;

implementation

uses
  SysUtils,
{$IFDEF LINUX}
  QNewCertDlg,
{$ENDIF}
{$IFDEF MSWINDOWS}
  NewCertDlg,
{$ENDIF}
  SecUtils, SsRijndael, Pkix, MpPK, MpX509, StreamSecII;

type
  TX509CertificateAuthorityHack = class(TX509CertificateAuthority);

procedure Register;
begin
  RegisterComponents('StreamSec',[TX509CertificateAuthority]);
  RegisterComponentEditor(TX509CertificateAuthority,TX509CertificateAuthorityEditor);
end;

{ TX509CertificateAuthorityEditor }

destructor TX509CertificateAuthorityEditor.Destroy;
begin

  inherited;
end;

procedure TX509CertificateAuthorityEditor.ExecuteVerb(Index: Integer); 
var
  OpenDialog: TOpenDialog;
  SaveDialog: TSaveDialog;
  Comp: TX509CertificateAuthorityHack;
  Password: ISecretKey;
  FileName, Filter: string;
  SignKey: IMPPrivateKey;
begin
  Comp := TX509CertificateAuthorityHack(Component);
  case Index of
    0:
      begin
        OpenDialog := TOpenDialog.Create(nil);
        try
          OpenDialog.Filter := 'Private Key Ring (*.pkr)|*.pkr|' +
                               'Root Certificate File (*.cer,*.p7b,*.p7c,*.scl)|*.cer;*.p7b;*.p7c;*.scl|' +
                               'Personal Certificate File (*.cer,*.p7b,*.p7c,*.scl)|*.cer;*.p7b;*.p7c;*.scl|' +
                               'Issued Certificate File (*.cer,*.p7b,*.p7c,*.scl)|*.cer;*.p7b;*.p7c;*.scl';
          if OpenDialog.Execute then begin
            case OpenDialog.FilterIndex of
              2:
                begin
                  Comp.RootCertsFileName := OpenDialog.FileName;
                  if Comp.RootCertsFileName <> '' then
                    Comp.InternalRootCertsFile.FileName := OpenDialog.FileName;
                end;
              3:
                begin
                  Comp.MyCertsFileName := OpenDialog.FileName;
                  if Comp.MyCertsFileName <> '' then
                    Comp.InternalMyCertsFile.FileName := OpenDialog.FileName;
                end;
              4:
                begin
                  Comp.LoadIssuedCertsFromFile(OpenDialog.FileName);
                  Comp.InternalMyCertsFile.FileName := OpenDialog.FileName;
                end;
            else
              Comp.PrivateKeyRingFileName := OpenDialog.FileName;
              if Comp.PrivateKeyRingFileName <> '' then
                Comp.InternalPrivateKeyRingFile.FileName := OpenDialog.FileName;
            end;
          end;
        finally
          OpenDialog.Free;
        end;
      end;
    1:
      begin
        SaveDialog := TSaveDialog.Create(nil);
        try
          Filter := 'Private Key Ring (*.pkr)|*.pkr|' +
                    'Root Certificate File (*.p7b)|*.p7b|' +
                    'Personal Certificate File (*.p7b)|*.p7b|' +
                    'Issued Certificate File (*.p7b)|*.p7b';
          if (not Comp.RootCerts.Modified) or Assigned(FSignCert) or
             Comp.HasPrivateCACert(FSignCert) or
             Comp.HasPrivateSignCert(FSignCert) then begin
            Filter := Filter + '|' +
                      'Root Signed Certificate File (*.scl)|*.scl|' +
                      'Personal Signed Certificate File (*.scl)|*.scl|' +
                      'Issued Signed Certificate File (*.scl)|*.scl';
          end;
          SaveDialog.Filter := Filter;
          if SaveDialog.Execute then begin
            case SaveDialog.FilterIndex of
              2:
                begin
                  FileName := SaveDialog.FileName;
                  if ExtractFileExt(FileName) = '' then
                    FileName := FileName + '.p7b';
                  Comp.RootCerts.ExportAllToCMSFile(FileName);
                end;
              3:
                begin
                  FileName := SaveDialog.FileName;
                  if ExtractFileExt(FileName) = '' then
                    FileName := FileName + '.p7b';
                  Comp.MyCerts.ExportAllToCMSFile(FileName);
                end;
              4:
                begin
                  FileName := SaveDialog.FileName;
                  if ExtractFileExt(FileName) = '' then
                    FileName := FileName + '.p7b';
                  Comp.IssuedCerts.ExportAllToCMSFile(FileName);
                end;
              5:
                begin
                  FileName := SaveDialog.FileName;
                  if ExtractFileExt(FileName) = '' then
                    FileName := FileName + '.scl';
                  if not Comp.RootCerts.Modified then
                    Comp.RootCerts.SaveToFile(FileName)
                  else begin
                    SignKey := Comp.StreamSecIIComp.FindCreatePrivateKey(FSignCert);
                    Comp.RootCerts.SignSaveToFile(FileName,FSignCert,SignKey);
                    Comp.InternalRootCertsFile.FileName := FileName;
                    Comp.RootCertsSCLFile := Comp.InternalRootCertsFile;
                  end;
                end;
              6:
                begin
                  FileName := SaveDialog.FileName;
                  if ExtractFileExt(FileName) = '' then
                    FileName := FileName + '.scl';
                  if not Comp.MyCerts.Modified then
                    Comp.MyCerts.SaveToFile(FileName)
                  else begin
                    SignKey := Comp.StreamSecIIComp.FindCreatePrivateKey(FSignCert);
                    Comp.MyCerts.SignSaveToFile(FileName,FSignCert,SignKey);
                    Comp.InternalMyCertsFile.FileName := FileName;
                    Comp.MyCertsSCLFile := Comp.InternalMyCertsFile;
                  end;
                end;
              7:
                begin
                  FileName := SaveDialog.FileName;
                  if ExtractFileExt(FileName) = '' then
                    FileName := FileName + '.scl';
                  if not Comp.IssuedCerts.Modified then
                    Comp.IssuedCerts.SaveToFile(FileName)
                  else begin
                    SignKey := Comp.StreamSecIIComp.FindCreatePrivateKey(FSignCert);
                    Comp.IssuedCerts.SignSaveToFile(FileName,FSignCert,SignKey);
                    Comp.InternalIssuedCertsFile.FileName := FileName;
                    Comp.SetIssuedCertsSCLFile(Comp.InternalIssuedCertsFile);
                  end;
                end;
            else
              FileName := SaveDialog.FileName;
              if ExtractFileExt(FileName) = '' then
                FileName := FileName + '.pkr';
              Password := TSecretKey.Create('');
              Comp.StreamSecIIComp.DoPassword(Password);
              Comp.StreamSecIIComp.SavePrivateKeyRingToFile(FileName,
                                                            Password,
                                                            kdfWPv2SHA1,
                                                            1 shl Comp.KeyDerivationBits,
                                                            True,
                                                            id_aes256_wrap,
                                                            TRijndael_ABC);
              Comp.InternalPrivateKeyRingFile.FileName := FileName;
              Comp.PrivateKeyRingFile := Comp.InternalPrivateKeyRingFile;
            end;
          end;
        finally
          SaveDialog.Free;
        end;
      end;
    2: Comp.NewCACertDlg(nil);
    3: Comp.NewServerCertDlg(nil);
    4: Comp.NewClientCertDlg(nil);
    5: Comp.NewCodeSigningCertDlg(nil);
    6: Comp.NewCACertReqDlg(nil);
    7: Comp.NewServerCertReqDlg(nil);
    8: Comp.NewClientCertReqDlg(nil);
    9: Comp.NewCodeSigningCertReqDlg(nil);
    10: Comp.NewCertAdvancedDlg(nil);
    11: if MessageDlg('Are you sure you want delete all certificates in ' +
                      Comp.Name + '?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
          Comp.ClearCertificates;
  end;
end;

function TX509CertificateAuthorityEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Open File';
    1: Result := 'Save File';
    2: Result := 'Create Root Certificate';
    3: Result := 'Create Server Certificate';
    4: Result := 'Create Client Certificate';
    5: Result := 'Create Code Signing Certificate';
    6: Result := 'Create Root Certificate Request';
    7: Result := 'Create Server Certificate Request';
    8: Result := 'Create Client Certificate Request';
    9: Result := 'Create Code Signing Certificate Request';
    10: Result := 'Create Certificate (All Options)';
    11: Result := 'Clear Certificates';
  else
    Result := '';
  end;
end;

function TX509CertificateAuthorityEditor.GetVerbCount: Integer;
begin
  Result := 12;
end;

{$IFDEF D6UP}
procedure TX509CertificateAuthorityEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
var
  Comp: TX509CertificateAuthorityHack;
begin
  Comp := TX509CertificateAuthorityHack(Component);
  case Index of
    3,4,5:
      AItem.Enabled := Assigned(FCACert) or
                       Comp.HasPrivateCACert(FCACert);
    6,7,8,9:
      AItem.Enabled := Assigned(Comp.RootCerts) and (Comp.RootCerts.Count > 0);
  end;
end;
{$ELSE}
{$IFDEF D5UP}
procedure TX509CertificateAuthorityEditor.PrepareItem(Index: Integer;
  const AItem: TMenuItem);
var
  Comp: TX509CertificateAuthorityHack;
begin
  Comp := TX509CertificateAuthorityHack(Component);
  case Index of
    3,4,5: AItem.Enabled := Assigned(FCACert) or
                          Comp.HasPrivateCACert(FCACert);
    6,7,8,9:
      AItem.Enabled := Assigned(Comp.RootCerts) and (Comp.RootCerts.Count > 0);
  end;
end;
{$ENDIF}
{$ENDIF}

end.
