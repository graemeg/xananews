{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     TLSInternalServerReg Unit                         }
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
unit TlsInternalServerReg;

interface

uses
  Classes,
  {$IFDEF LINUX}
  QDialogs, DesignIntf, DesignWindows, DesignEditors, DesignMenus, QForms,
  QControls,
  {$ENDIF LINUX}
  {$IFDEF MSWINDOWS}
  {$IFDEF D6UP}
  DesignIntf, DesignEditors, DesignMenus,
  {$ELSE}
  DsgnIntf, DsgnWnds, Menus,
  {$ENDIF D6UP}
  Dialogs, Controls,
  {$ENDIF MSWINDOWS}
  Asn1, TlsInternalServer, SsCertMgrUtils;

type
  TSimpleTLSInternalServerEditor = class(TComponentEditor)
  private
    {$IFDEF D5UP}
    FCACert: TASN1Struct;
    {$ENDIF D5UP}
    FSignCert: TASN1Struct;
  protected
    procedure OpenFileVerb;
    procedure SaveFileVerb;
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
    {$ENDIF D5UP}
    {$ENDIF D6UP}
  end;

  TPathNameProperty = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

uses
  SysUtils, TypInfo,
{$IFDEF LINUX}
  QNewCertDlg,
{$ENDIF}
{$IFDEF MSWINDOWS}
  NewCertDlg,
{$ENDIF}
  SecUtils, SsRijndael, Pkix, MpPK, MpX509, StreamSecII;

type
  TSimpleTLSInternalServerHack = class(TSimpleTLSInternalServer);

procedure Register;
begin
  RegisterComponents('StreamSec',[TTLSInternalServer,TSimpleTLSInternalServer]);
  RegisterComponentEditor(TSimpleTLSInternalServer,TSimpleTLSInternalServerEditor);
  RegisterPropertyEditor(TypeInfo(string),TCustomTLSInternalServer,'RegisteredPathName',TPathNameProperty);
end;

{ TSimpleTLSInternalServerEditor }

destructor TSimpleTLSInternalServerEditor.Destroy;
begin
  FSignCert.Free;
  inherited;
end;

procedure TSimpleTLSInternalServerEditor.ExecuteVerb(Index: Integer);
var
  Comp: TSimpleTLSInternalServerHack;
  SaveDialog: TSaveDialog;
  Password: ISecretKey;
  FileName, Filter, PathName: string;
  {$IFDEF D5UP}
  SignKey: IMPPrivateKey;
  {$ENDIF D5UP}
begin
  if not (Component is TSimpleTLSInternalServer) then Exit;
  Comp := TSimpleTLSInternalServerHack(Component);
  case Index of
    0: OpenFileVerb;
    1: SaveFileVerb;
    2: Comp.NewCACertDlg(nil);
    3: Comp.NewServerCertDlg(nil);
    4: Comp.NewClientCertDlg(nil);
    5: Comp.NewCACertReqDlg(nil);
    6: Comp.NewServerCertReqDlg(nil);
    7: Comp.NewClientCertReqDlg(nil);
    8: if MessageDlg('Are you sure you want delete all certificates in ' +
                     Comp.Name + '?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
         Comp.ClearCertificates;
  else
  end;
  if Index in [2..7] then begin
    Comp := TSimpleTLSInternalServerHack(Component);

    PathName := Comp.RegisteredPathName;
    if PathName = '' then begin
      PathName := TSsCertMgrPaths.GetDefaultPathName;
      SaveDialog := TSaveDialog.Create(nil);
      try
        Filter := 'Private Key Ring (*.pkr)|*.pkr';
        SaveDialog.FileName := 'User.pkr';
        SaveDialog.Filter := Filter;
        SaveDialog.DefaultExt := 'pkr';
        if SaveDialog.Execute then begin
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
      finally
        SaveDialog.Free;
      end;
      if not InputQuery('Register ' + ExtractFilePath(FileName),'Enter name',PathName) then
        PathName := '';
    end else begin
      FileName := TSsCertMgrPaths.GetPath(PathName) + 'User.pkr';
      if MessageDlg('Save keys to ' +
                    ExtractFilePath(FileName) + '?',
                    mtConfirmation,[mbYes,mbNo],0) = mrNo then
        PathName := '';
    end;
    if PathName <> '' then begin
      if Comp.HasPrivateCACert(FSignCert) or
         Comp.HasPrivateSignCert(FSignCert) then begin
        if Comp.RootCerts.Count > 0 then begin
          FileName := ExtractFilePath(FileName) + 'Root.scl';
          if not Comp.RootCerts.Modified then
            Comp.RootCerts.SaveToFile(FileName)
          else begin
            {$IFDEF D5UP}
            SignKey := Comp.StreamSecIIComp.FindCreatePrivateKey(FSignCert);
            Comp.RootCerts.SignSaveToFile(FileName,FSignCert,SignKey);
            Comp.InternalRootCertsFile.FileName := FileName;
            Comp.RootCertsSCLFile := Comp.InternalRootCertsFile;
            {$ELSE}
            Comp.SaveRootCertsToSCLFile(FileName);
            {$ENDIF D5UP}
          end;
        end;
        FileName := ExtractFilePath(FileName) + 'User.scl';
        if not Comp.MyCerts.Modified then
          Comp.MyCerts.SaveToFile(FileName)
        else begin
          {$IFDEF D5UP}
          if SignKey = nil then
            SignKey := Comp.StreamSecIIComp.FindCreatePrivateKey(FSignCert);
          Comp.MyCerts.SignSaveToFile(FileName,FSignCert,SignKey);
          Comp.InternalMyCertsFile.FileName := FileName;
          Comp.MyCertsSCLFile := Comp.InternalMyCertsFile;
          {$ELSE}
          Comp.SaveMyCertsToSCLFile(FileName);
          {$ENDIF D5UP}
        end;
        if Comp.TrustedCerts.Count > 0 then begin
          FileName := ExtractFilePath(FileName) + 'Collected.scl';
          if not Comp.TrustedCerts.Modified then
            Comp.TrustedCerts.SaveToFile(FileName)
          else begin
            {$IFDEF D5UP}
            if SignKey = nil then
              SignKey := Comp.StreamSecIIComp.FindCreatePrivateKey(FSignCert);
            Comp.TrustedCerts.SignSaveToFile(FileName,FSignCert,SignKey);
            Comp.InternalTrustedCertsFile.FileName := FileName;
            Comp.TrustedCertsSCLFile := Comp.InternalTrustedCertsFile;
            {$ELSE}
            Comp.SaveTrustedCertsToSCLFile(FileName);
            {$ENDIF D5UP}
          end;
        end;
      end;
      TSsCertMgrPaths.RegisterPath(ExtractFilePath(FileName),PathName);
      Comp.RegisteredPathName := PathName;
    end;
  end;
end;

function TSimpleTLSInternalServerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Open File';
    1: Result := 'Save File';
    2: Result := 'Create Root Certificate';
    3: Result := 'Create Server Certificate';
    4: Result := 'Create Client Certificate';
    5: Result := 'Create Root Certificate Request';
    6: Result := 'Create Server Certificate Request';
    7: Result := 'Create Client Certificate Request';
    8: Result := 'Clear Certificates';
  else
    Result := '';
  end;
end;

function TSimpleTLSInternalServerEditor.GetVerbCount: Integer;
begin
  Result := 9;
end;

procedure TSimpleTLSInternalServerEditor.OpenFileVerb;
var
  OpenDialog: TOpenDialog;
  Comp: TSimpleTLSInternalServerHack;
begin
  Comp := TSimpleTLSInternalServerHack(Component);

        OpenDialog := TOpenDialog.Create(nil);
        try
          OpenDialog.Filter := 'Private Key Ring (*.pkr)|*.pkr|' +
                               'Root Certificate File (*.cer,*.p7b,*.p7c,*.scl)|*.cer;*.p7b;*.p7c;*.scl|' +
                               'Personal Certificate File (*.cer,*.p7b,*.p7c,*.scl)|*.cer;*.p7b;*.p7c;*.scl|' +
                               'Trusted Certificate File (*.cer,*.p7b,*.p7c,*.scl)|*.cer;*.p7b;*.p7c;*.scl|' +
                               'PKCS#12 Personal Information Exchange (*.pfx,*.p12)|*.pfx;*.p12';
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
                  Comp.TrustedCertsFileName := OpenDialog.FileName;
                  if Comp.TrustedCertsFileName <> '' then
                    Comp.InternalTrustedCertsFile.FileName := OpenDialog.FileName;
                end;
              5:
                begin
                  Comp.ImportFromPFX(OpenDialog.FileName,nil);
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

{$IFDEF D6UP}
procedure TSimpleTLSInternalServerEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
var
  Comp: TSimpleTLSInternalServerHack;
begin
  Comp := TSimpleTLSInternalServerHack(Component);
  case Index of
    3,4:
      AItem.Enabled := Assigned(FCACert) or
                       Comp.HasPrivateCACert(FCACert);
    5,6,7:
      AItem.Enabled := Assigned(Comp.RootCerts) and (Comp.RootCerts.Count > 0);
  end;
end;
{$ELSE}
{$IFDEF D5UP}
procedure TSimpleTLSInternalServerEditor.PrepareItem(Index: Integer;
  const AItem: TMenuItem);
var
  Comp: TSimpleTLSInternalServerHack;
begin
  Comp := TSimpleTLSInternalServerHack(Component);
  case Index of
    3,4: AItem.Enabled := Assigned(FCACert) or
                          Comp.HasPrivateCACert(FCACert);
    5,6,7:
      AItem.Enabled := Assigned(Comp.RootCerts) and (Comp.RootCerts.Count > 0);
  end;
end;
{$ENDIF}
{$ENDIF}

const
  NoKeyFilter = 'Private Key Ring (*.pkr)|*.pkr|' +
                'Root Certificate File (*.p7b)|*.p7b|' +
                'Personal Certificate File (*.p7b)|*.p7b|' +
                'Trusted Certificate File (*.p7b)|*.p7b';
{$IFDEF D5UP}
  KeyFilter   = 'Root Signed Certificate File (*.scl)|*.scl|' +
                'Personal Signed Certificate File (*.scl)|*.scl|' +
                'Trusted Signed Certificate File (*.scl)|*.scl|' +
                'PKCS#12 Personal Information Exchange (*.pfx)|*.pfx';
{$ELSE}                                                               
  KeyFilter   = 'Root Signed Certificate File (*.scl)|*.scl|' +
                'Personal Signed Certificate File (*.scl)|*.scl|' +
                'Trusted Signed Certificate File (*.scl)|*.scl';
{$ENDIF D5UP}

procedure TSimpleTLSInternalServerEditor.SaveFileVerb;
var
  SaveDialog: TSaveDialog;
  Comp: TSimpleTLSInternalServerHack;
  Password: ISecretKey;
  FileName, Filter: string;
  {$IFDEF D5UP}
  SignKey: IMPPrivateKey;
  {$ENDIF D5UP}
begin
  Comp := TSimpleTLSInternalServerHack(Component);

        SaveDialog := TSaveDialog.Create(nil);
        try
          Filter := NoKeyFilter;
          if (not Comp.RootCerts.Modified) or Assigned(FSignCert) or
             Comp.HasPrivateCACert(FSignCert) or
             Comp.HasPrivateSignCert(FSignCert) then begin
            Filter := Filter + '|' + KeyFilter;
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
                  Comp.TrustedCerts.ExportAllToCMSFile(FileName);
                end;
              5:
                begin
                  FileName := SaveDialog.FileName;
                  if ExtractFileExt(FileName) = '' then
                    FileName := FileName + '.scl';
                  if not Comp.RootCerts.Modified then
                    Comp.RootCerts.SaveToFile(FileName)
                  else begin
                    {$IFDEF D5UP}
                    SignKey := Comp.StreamSecIIComp.FindCreatePrivateKey(FSignCert);
                    Comp.RootCerts.SignSaveToFile(FileName,FSignCert,SignKey);
                    Comp.InternalRootCertsFile.FileName := FileName;
                    Comp.RootCertsSCLFile := Comp.InternalRootCertsFile;
                    {$ELSE}
                    Comp.SaveRootCertsToSCLFile(FileName);
                    {$ENDIF D5UP}
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
                    {$IFDEF D5UP}
                    SignKey := Comp.StreamSecIIComp.FindCreatePrivateKey(FSignCert);
                    Comp.MyCerts.SignSaveToFile(FileName,FSignCert,SignKey);
                    Comp.InternalMyCertsFile.FileName := FileName;
                    Comp.MyCertsSCLFile := Comp.InternalMyCertsFile;
                    {$ELSE}
                    Comp.SaveMyCertsToSCLFile(FileName);
                    {$ENDIF D5UP}
                  end;
                end;
              7:
                begin
                  FileName := SaveDialog.FileName;
                  if ExtractFileExt(FileName) = '' then
                    FileName := FileName + '.scl';
                  if not Comp.TrustedCerts.Modified then
                    Comp.TrustedCerts.SaveToFile(FileName)
                  else begin
                    {$IFDEF D5UP}
                    SignKey := Comp.StreamSecIIComp.FindCreatePrivateKey(FSignCert);
                    Comp.TrustedCerts.SignSaveToFile(FileName,FSignCert,SignKey);
                    Comp.InternalTrustedCertsFile.FileName := FileName;
                    Comp.TrustedCertsSCLFile := Comp.InternalTrustedCertsFile;
                    {$ELSE}
                    Comp.SaveTrustedCertsToSCLFile(FileName);
                    {$ENDIF D5UP}
                  end;
                end;
{$IFDEF D5UP}
              8:
                begin
                  FileName := SaveDialog.FileName;
                  if ExtractFileExt(FileName) = '' then
                    FileName := FileName + '.pfx';
                  Comp.ExportToPFX(FileName,nil);
                end;
{$ENDIF D5UP}
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

{ TPathNameProperty }

function TPathNameProperty.AllEqual: Boolean;
var
  I: Integer;
  V: string;
begin
  Result := False;
  if PropCount > 1 then begin
    V := GetStrValue;
    for I := 1 to PropCount - 1 do
      if GetStrValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TPathNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect,paValueList, paSortList, paRevertable];
end;

function TPathNameProperty.GetEditLimit: Integer;
begin
  Result := GetTypeData(GetPropType)^.MaxLength;
end;

function TPathNameProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TPathNameProperty.GetValues(Proc: TGetStrProc);
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    TSsCertMgrPaths.GetNamesAndPaths(SL);
    for I := 0 to SL.Count - 1 do
      Proc(SL.Names[I]);
  finally
    SL.Free;
  end;
end;

procedure TPathNameProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

end.
