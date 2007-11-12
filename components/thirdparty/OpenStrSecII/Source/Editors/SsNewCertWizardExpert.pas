{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SsNewCertWizardExpert Unit                        }
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
unit SsNewCertWizardExpert;

interface

uses
  Classes, Windows, Toolsapi, SsNewCertWizard;


Type
  TSsNewCertWizard = class({$IFNDEF D5UP}TInterfacedObject,{$ELSE}
                           TNotifierObject,{$ENDIF}
                           IOTAWizard,
                           IOTARepositoryWizard,
                           IOTAFormWizard
                           {$IFDEF D6UP},IOTARepositoryWizard60{$ENDIF})
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;

{$IFNDEF D5UP}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
{$ENDIF}
{$IFDEF D6UP}
    function GetDesigner: string;
    function GetGlyph: Cardinal;
{$ELSE}
    function GetGlyph: HICON;
{$ENDIF}
  end;

procedure Register;

implementation

uses
{$IFNDEF D5UP}
  MpX509,
{$ENDIF}
{$IFNDEF D6UP}
  ComObj,
{$ENDIF}
  SysUtils, Registry, Dialogs, Controls,
  Pkix_Cert;

{$R *.res}

procedure Register;
begin
  RegisterPackageWizard(TssNewCertWizard.Create);
end;

const
  LF: string = #13#10;

type
  TSsModuleCreator = class(TInterfacedObject,IOTACreator,IOTAModuleCreator)
  private
    FForm: TfrmNewCertWizard;
    procedure GenerateProperty(I: Integer;
                               PrivDecl,GetDecl,SetDecl,PropDecl,Implements,
                               Idents: TStrings;
                               AClassName: string);
    procedure SubjectAltNameFromProperty(Compose: TStrings;
                                         Cert: TCertificate);
    function GenerateSource(const ModuleIdent: string): string;
    function GenerateSourceRoot(const ModuleIdent: string): string;
    function GenerateSourceCert(const ModuleIdent: string): string;
    procedure GenerateSourceCertOrRoot(PrivDecl,GetDecl,SetDecl,PropDecl,
                                       Implements,Idents,Compose: TStrings;
                                       AClassName: string;
                                       Cert: TCertificate);
    function GenerateSourceCertReq(const ModuleIdent: string): string;
    function GenerateSourceCertFromReq(const ModuleIdent: string): string;
  public
    constructor Create(AForm: TfrmNewCertWizard);

    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TSsSourceFile = class(TInterfacedObject,IOTAFile)
  private
    FSource: string;
  public
    function GetSource: string;
    function GetAge: TDateTime;
    constructor Create(const Source: string);
  end;

{ TSsNewCertWizard }

{$IFNDEF D5UP}
procedure TSsNewCertWizard.AfterSave;
begin

end;

procedure TSsNewCertWizard.BeforeSave;
begin

end;

procedure TSsNewCertWizard.Destroyed;
begin

end;
{$ENDIF}

procedure TSsNewCertWizard.Execute;
begin
  frmNewCertWizard := TfrmNewCertWizard.Create(nil);
  try
    if frmNewCertWizard.ShowModal = mrOK then
      with BorlandIDEServices as IOTAModuleServices do
        CreateModule(TSsModuleCreator.Create(frmNewCertWizard));
  finally
    frmNewCertWizard.Free;
  end;
end;

function TSsNewCertWizard.GetAuthor: string;
begin
  Result := 'Henrick W. Hellström/StreamSec';
end;

function TSsNewCertWizard.GetComment: string;
begin
  Result := 'NewCertDlg Class';
end;

{$IFNDEF LINUX}
 {$IFDEF D6UP}
function TSsNewCertWizard.GetGlyph: Cardinal;
 {$ELSE}
function TSsNewCertWizard.GetGlyph: HICON;
 {$ENDIF}
{$ELSE}
function TSsNewCertWizard.GetGlyph: Cardinal;
{$ENDIF}
begin
{$IFDEF LINUX}
  Result := 0;
{$ELSE}
  Result := LoadIcon(hInstance, 'NEWCERTDLGCLASS');
{$ENDIF}
end;

{$IFDEF D6UP}
function TSsNewCertWizard.GetDesigner: string;
begin
  Result := dAny;
end;
{$ENDIF}

function TSsNewCertWizard.GetIDString: string;
begin
  Result := 'STREAMSEC.NewCertDlgClass';
end;

function TSsNewCertWizard.GetName: string;
begin
  Result := 'StreamSec NewCertDlg Class';
end;

function TSsNewCertWizard.GetPage: string;
begin
  Result := 'StreamSec';
end;

function TSsNewCertWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

{$IFNDEF D5UP}
procedure TSsNewCertWizard.Modified;
begin

end;
{$ENDIF}

{ TSsModuleCreator }

constructor TSsModuleCreator.Create(AForm: TfrmNewCertWizard);
begin
  inherited Create;
  FForm := AForm;
end;

procedure TSsModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Do nothing
end;

function TSsModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TSsModuleCreator.GetCreatorType: string;
begin
  Result := sUnit;
end;

function TSsModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TSsModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TSsModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TSsModuleCreator.GetImplFileName: string;
begin
{$IFNDEF D5UP}
  if Assigned(FForm) then
    Result := Format('%s.pas',[Copy(FForm.edtClassName.Text,2,MaxInt)])
  else
    Result := 'Unit1.pas';
{$ELSE}
  Result := '';
{$ENDIF}
end;

function TSsModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TSsModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TSsModuleCreator.GetOwner: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  NewModule: IOTAModule;
begin
  Result := nil;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Module := ModuleServices.CurrentModule;

  if Assigned(Module) then begin
    if Supports(Module,IOTAProject,NewModule) then begin
      Result := NewModule
{$IFDEF D6UP}
    end else if Module.OwnerModuleCount > 0 then begin
      NewModule := Module.OwnerModules[0];
{$ELSE}
    end else if Module.GetOwnerCount > 0 then begin
      NewModule := Module.GetOwner(0);
{$ENDIF}
      if Assigned(NewModule) then
        if not Supports(NewModule,IOTAProject,Result) then
          Result := nil;
    end;
  end;
end;

function TSsModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TSsModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TSsModuleCreator.GetUnnamed:boolean;
begin
  Result := True;
end;

function TSsModuleCreator.NewFormFile(const FormIdent: string;
                                      const AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

const
  SubjectNameFields: array [0..10] of string = (
    'CommonName',
    'Surname',
    'GivenName',
    'Initials',
    'GenerationQualifier',
    'Country',
    'LocalityName',
    'StateOrProvinceName',
    'OrganizationName',
    'OrganizationalUnitName',
    'DnQualifier'
  );
  SubjectNameFieldEnums: array [0..10] of TAttrValueEnum = (
    aveIdAtCommonName,
    aveIdAtSurname,
    aveIdAtGivenName,
    aveIdAtInitials,
    aveIdAtGenerationQualifier,
    aveIdAtCountry,
    aveIdAtLocalityName,
    aveIdAtStateOrProvinceName,
    aveIdAtOrganizationName,
    aveIdAtOrganizationalUnitName,
    aveIdAtDnQualifier
  );
  SubjectAltNameFields: array [11..14] of string = (
    'Rfc822Name',
    'UniformResourceIdentifier',
    'DNSName',
    'IPAddress'
  );
  (*
  SubjectAltNameFieldEnums: array [11..14] of TGeneralNameEnum = (
    gneRfc822Name,
    gneUniformResourceIdentifier,
    gneDNSName,
    gneIPAddress
  );
  *)

procedure EncodeConstStr(var Value: string);
var
  I: Integer;
  Res: string;
  LastInString: Boolean;
begin
  if Value <> '' then begin
    Res := '';
    LastInString := False;
    for I := 1 to Length(Value) do begin
      if Value[I] in [#0..#31,#128..#255] then begin
        if LastInString then
          Res := Format('%s''#%d',[Res,Ord(Value[I])])
        else
          Res := Format('%s#%d',[Res,Ord(Value[I])]);
        LastInString := False;
      end else begin
        if not LastInString then
          Res := Format('%s''',[Res]);
        if Value[I] = '''' then
          Res := Format('%s''''',[Res])
        else
          Res := Format('%s%s',[Res,Value[I]]);
        LastInString := True;
      end;
    end;
    if LastInString then
      Value := Format('%s''',[Res])
    else
      Value := Res;
  end;
end;

procedure TSsModuleCreator.GenerateProperty(I: Integer; PrivDecl, GetDecl,
  SetDecl, PropDecl, Implements, Idents: TStrings; AClassName: string);
var
  Idx: Integer;
  Ident, Implement, Name: string;
begin
  if I > High(SubjectNameFields) then
    Name := SubjectAltNameFields[I]
  else                               
    Name := SubjectNameFields[I];
  PrivDecl.Add(Format('    F%s: WideString;',[Name]));
  Ident := Format('Get%s',[Name]);
  GetDecl.Add(Format('    function %s: WideString;',[Ident]));
  Implement := Format('function %s.%s: WideString;' + LF +
                      'begin' + LF +
                      '  Result := F%s;' + LF +
                      'end;',
                      [AClassName,
                       Ident,
                       Name]);
  Idx := Implements.Add(Implement);
  Idents.AddObject(Ident,TObject(Idx));
  Ident := Format('Set%s',[Name]);
  SetDecl.Add(Format('    procedure %s(const Value: WideString);',
                     [Ident]));
  Implement := Format('procedure %s.%s(const Value: WideString);' + LF +
                      'begin' + LF +
                      '  F%s := Value;' + LF +
                      'end;',
                      [AClassName,
                       Ident,
                       Name]);
   Idx := Implements.Add(Implement);
   Idents.AddObject(Ident,TObject(Idx));
   Ident := Format('    property %s: WideString read Get%s write Set%s;',
                   [Name,
                    Name,
                    Name]);
   PropDecl.Add(Ident);
end;

function TSsModuleCreator.GenerateSource(const ModuleIdent: string): string;
begin
  case FForm.rgPurpose.ItemIndex of
    0: Result := GenerateSourceRoot(ModuleIdent);
    1: Result := GenerateSourceCert(ModuleIdent);
    2: Result := GenerateSourceCertReq(ModuleIdent);
    3: Result := GenerateSourceCertFromReq(ModuleIdent);
  else
    Result := '';
  end;
end;

function TSsModuleCreator.GenerateSourceCert(
  const ModuleIdent: string): string;
var
  Idents, Implements, PrivDecl, GetDecl, SetDecl,
  PropDecl, Compose, Impl: TStringList;
  ClassName, FileName, FileExt: string;
  Cert: TCertificate;
  {$IFDEF D6UP}
  GUID: TGUID;
  {$ENDIF}
  I, Idx: Integer;
begin
  Cert := TCertificate.Create(nil,nil);
  try
    if FileExists(FForm.edtTemplateFile.Text) then
      Cert.LoadFromFile(FForm.edtTemplateFile.Text);
    ClassName := FForm.edtClassName.Text;
    Idents := TStringList.Create;
    try
      Idents.Sorted := True;
      {$IFDEF D6UP}
      Idents.CaseSensitive := False;
      {$ENDIF}
      Implements := TStringList.Create;
      Compose := TStringList.Create;
      GetDecl := TStringList.Create;
      SetDecl := TStringList.Create;
      PrivDecl := TStringList.Create;
      PropDecl := TStringList.Create;
      Impl := TStringList.Create;
      try
        Compose.Add('  if CreateOpt <> nccCert then' + LF +
                    '    raise Exception.Create(''Create Option not supported'');');
        Compose.Add('');
        GenerateSourceCertOrRoot(PrivDecl,GetDecl,SetDecl,PropDecl,Implements,
                                 Idents,Compose,ClassName,Cert);

        Compose.Add('');
        Compose.Add('  //Validity');
        Compose.Add('  Cert.Validity.NotBefore.Choice := teUTCTime;');
        Compose.Add('  Cert.Validity.NotBefore.AsUtcTime.AsDateTime := Trunc(Now - OffsetFromUTC/24);');
        Compose.Add('  Cert.Validity.NotAfter.Choice := teUTCTime;');
        Compose.Add(Format(
          '  Cert.Validity.NotAfter.AsUtcTime.AsDateTime := Trunc(Now - OffsetFromUTC/24 + %d);',
          [StrToInt(FForm.edtValidityPeriod.Text)]));

        Compose.Add('');
        Compose.Add('  //Key usage');
        case FForm.cbPublicKeyAlgorithm.ItemIndex of
          0: Compose.Add(
               '  case Wizard of' + LF +
               '    ncwCA:          Cert.TbsCertificate.Extensions.KeyUsage := [keyCertSign,cRLSign];' + LF +
               '    ncwServer:      Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,keyEncipherment];' + LF +
               '    ncwClient:      Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,keyEncipherment];' + LF +
               '    ncwCodeSigning: Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,nonRepudiation];' + LF +
               '    ncwEmail:       Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,keyEncipherment,dataEncipherment];' + LF +
               '    ncwPeerToPeer:  Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,keyEncipherment];' + LF +
               '  end;');
          1: Compose.Add(
               '  case Wizard of' + LF +
               '    ncwCA:          Cert.TbsCertificate.Extensions.KeyUsage := [keyCertSign,cRLSign];' + LF +
               '    ncwServer:      Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature];' + LF +
               '    ncwClient:      Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature];' + LF +
               '    ncwCodeSigning: Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,nonRepudiation];' + LF +
               '    ncwEmail:       Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature];' + LF +
               '    ncwPeerToPeer:  Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature];' + LF +
               '  end;');
          2: Compose.Add(
               '  case Wizard of' + LF +
               '    ncwCA:          Cert.TbsCertificate.Extensions.KeyUsage := [keyCertSign,cRLSign];' + LF +
               '    ncwServer:      Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,keyAgreement];' + LF +
               '    ncwClient:      Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,keyAgreement];' + LF +
               '    ncwCodeSigning: Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,nonRepudiation];' + LF +
               '    ncwEmail:       Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,keyAgreement];' + LF +
               '    ncwPeerToPeer:  Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,keyAgreement];' + LF +
               '  end;');
        end;

        Compose.Add('');
        Compose.Add('  //Extended Key Usage');
        Compose.Add(
          '  if not (Wizard in [ncwAdvanced,ncwCA]) then begin' + LF +
          '    Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeExtKeyUsage);' + LF +
          '    case Wizard of' + LF +
          '      ncwServer:      Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_serverAuth);' + LF +
          '      ncwClient:      Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_clientAuth);' + LF +
          '      ncwCodeSigning: Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_codeSigning);' + LF +
          '      ncwEMail:       Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_emailProtection);' + LF +
          '      ncwPeerToPeer:' + LF +
          '        begin' + LF +
          '          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_serverAuth);' + LF +
          '          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_clientAuth);' + LF +
          '        end;' + LF +
          '    end;' + LF +
          '  end;');

        Compose.Add('');
        Compose.Add('  //Basic constraints');
        Compose.Add(
          '  if Wizard = ncwCA then begin' + LF +
          '    Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeBasicConstraints);' + LF +
          '    Ext.ExtnValue.AsCe_BasicConstraints.CA := True;' + LF +
          '    // Change PathLen to allow intermediary CAs in the chain:' + LF +
          '    Ext.ExtnValue.AsCe_BasicConstraints.PathLenConstraint.AsInteger := 0;' + LF +
          '  end else if Wizard <> ncwAdvanced then begin' + LF +
          '    Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeBasicConstraints);' + LF +
          '    Ext.ExtnValue.AsCe_BasicConstraints.CA := False;' + LF +
          '  end;');

        Compose.Add('');
        Compose.Add('  //Additional extensions can be set here, such as CRL Distribution Point,');
        Compose.Add('  //Certificate Policies, Name Constraints, etc');
        
        Compose.Add('');
        Compose.Add('  //Private key');
        case FForm.cbPublicKeyAlgorithm.ItemIndex of
          0:
            begin
              Compose.Add('  PrivateKeyAlg := rsaEncryption;');
              Compose.Add('  if not (Wizard in [ncwAdvanced,ncwCA]) then');
              Compose.Add('    ExportableRSA := True;');
            end;
          1: Compose.Add('  PrivateKeyAlg := id_dsa;');
          2: Compose.Add('  PrivateKeyAlg := id_ecPublicKey;');
        end;
        Compose.Add(Format('  KeySize := %d;',[StrToInt(FForm.edtPublicKeySize.Text)]));
        Compose.Add('  SignatureDigestAlg := haSHA1;');

        Compose.Add('');
        Compose.Add('  //Select issuer');
        Compose.Add('  Idx := MyCerts.Count - 1;');
        Compose.Add('  while Idx >= 0 do begin');
        Compose.Add('    CACert := MyCerts.Certs[Idx];');
        Compose.Add('    if CheckValidity(CACert,OffsetFromUTC) and (keyCertSign in ExtractKeyUsage(CACert)) then ');
        Compose.Add('      Break;');
        Compose.Add('    Dec(Idx);');       
        Compose.Add('  end;');

        Idx := Implements.Add(Format(
          'procedure %s.ComposeCertificate;' + LF +
          'var' + LF +
          '  Ext: TExtension;' + LF +
          '  Idx: Integer;' + LF +
          'begin' + LF +
          '%s' + LF +
          '  inherited;' + LF +
          'end;',
          [ClassName,Compose.Text]));
        Idents.AddObject('ComposeCertificate',TObject(Idx));

        if FForm.edtExportFileName.Text <> '' then begin
          FileName := FForm.edtExportFileName.Text;
          EncodeConstStr(FileName);
          FileExt := LowerCase(ExtractFileExt(FForm.edtExportFileName.Text));
          if (FileExt = '.pfx') or (FileExt = '.p12') then
            Idx := Implements.Add(Format(
              'procedure %s.AfterExecute(ATLSServer: ITLSServer);' + LF +
              'var' + LF +
              '  WS: WideString;' + LF +
              '  PW: ISecretKey;' + LF +
              'begin' + LF +
              '  WS := ''somepassword'';' + LF +
              '  PW := TSecretKey.CreateBMPStr(PWideChar(WS));' + LF +
              '  ATLSServer.ExportToPFX(%s,PW);' + LF +
              '  // inherited will save Cert to ExportData' + LF +
              '  inherited;' + LF +
              'end;',
              [ClassName,FileName]))
          else
            Idx := Implements.Add(Format(
              'procedure %s.AfterExecute(ATLSServer: ITLSServer);' + LF +
              'begin' + LF +
              '  Cert.SaveToFile(%s);' + LF +
              '  ATLSServer.SavePrivateKeyRing(''User.pkr'');' + LF +
              '  ATLSServer.SaveMyCertsToSCLFile(''User.scl'');' + LF +
              '  // inherited will save Cert to ExportData' + LF +
              '  inherited;' + LF +
              'end;',
              [ClassName,FileName]))
        end else
          Idx := Implements.Add(Format(
            'procedure %s.AfterExecute(ATLSServer: ITLSServer);' + LF +
            'begin' + LF +
            '  // This is where you should save the new certificate' + LF +
            '  ATLSServer.SavePrivateKeyRing(''User.pkr'');' + LF +
            '  ATLSServer.SaveMyCertsToSCLFile(''User.scl'');' + LF +
            '  // inherited will save Cert to ExportData' + LF +
            '  inherited;' + LF +
            'end;',
            [ClassName]));
        Idents.AddObject('AfterExecute',TObject(Idx));

        for I := 0 to Idents.Count - 1 do begin
          Idx := Integer(Idents.Objects[I]);
          Impl.Add(Implements[Idx]);
          Impl.Add('');
        end;
        {$IFDEF D6UP}
        CreateGUID(GUID);
        {$ENDIF}

        Result := Format(
          'unit %s;' + LF +
          LF +
          'interface' + LF +
          LF +
          'uses' + LF +
          '  SysUtils,' + LF +
          '  SecUtils, Pkix, Asn1, Pkix_Cert, Pkcs_10, X509Base, MpX509,' + LF +
          '  StreamSecII;' + LF +
          LF +
          'type' + LF +
          '  I%s = interface(INewCertDlg)' + LF +
          '  [''%s'']' + LF +
          '%s' + LF +
          '%s' + LF +
          '%s' + LF +
          '  end;' + LF +
          LF +
          '  %s = class(TNewCertDlgObject,I%s)' + LF +
          '  private' + LF +
          '%s' +
          '  protected' + LF +
          '%s' + LF +
          '%s' + LF +
          '    procedure ComposeCertificate; override;' + LF +
          '    procedure AfterExecute(ATLSServer: ITLSServer); override;' + LF +
          '  end;' + LF +
          LF +
          'implementation' + LF +
          LF +
          '{ %s }' + LF +
          LF +
          '%s' + LF +
          LF +
          'end.',
          [ModuleIdent,
           Copy(ClassName,2,MaxInt),
           {$IFDEF D6UP}
           GUIDToString(GUID),
           {$ELSE}
           CreateClassID,
           {$ENDIF}
           GetDecl.Text,SetDecl.Text,PropDecl.Text,
           ClassName,Copy(ClassName,2,MaxInt),
           PrivDecl.Text,GetDecl.Text,SetDecl.Text,
           ClassName,
           Impl.Text]);
      finally
        Implements.Free;
        Compose.Free;
        GetDecl.Free;
        SetDecl.Free;
        PrivDecl.Free;
        PropDecl.Free;
        Impl.Free;
      end;
    finally
      Idents.Free;
    end;
  finally
    Cert.Free;
  end;
end;

function TSsModuleCreator.GenerateSourceCertFromReq(
  const ModuleIdent: string): string;
var
  Idents, Implements, PrivDecl, GetDecl, SetDecl,
  PropDecl, Compose, Impl: TStringList;
  ClassName, FileName: string;
  Cert: TCertificate;       
  {$IFDEF D6UP}
  GUID: TGUID;
  {$ENDIF}
  I, Idx: Integer;
begin
  Cert := TCertificate.Create(nil,nil);
  try
    if FileExists(FForm.edtTemplateFile.Text) then
      Cert.LoadFromFile(FForm.edtTemplateFile.Text);
    ClassName := FForm.edtClassName.Text;
    Idents := TStringList.Create;
    try
      Idents.Sorted := True;
      {$IFDEF D6UP}
      Idents.CaseSensitive := False;
      {$ENDIF}
      Implements := TStringList.Create;
      Compose := TStringList.Create;
      GetDecl := TStringList.Create;
      SetDecl := TStringList.Create;
      PrivDecl := TStringList.Create;
      PropDecl := TStringList.Create;
      Impl := TStringList.Create;
      try
        Compose.Add('  if CreateOpt <> nccCertFromReq then' + LF +
                    '    raise Exception.Create(''Create Option not supported'');');

        Compose.Add('');
        Compose.Add('  //Select issuer');
        Compose.Add('  Idx := MyCerts.Count - 1;');
        Compose.Add('  while Idx >= 0 do begin');
        Compose.Add('    CACert := MyCerts.Certs[Idx];');
        Compose.Add('    if CheckValidity(CACert,OffsetFromUTC) and (keyCertSign in ExtractKeyUsage(CACert)) then ');
        Compose.Add('      Break;');
        Compose.Add('    Dec(Idx);');
        Compose.Add('  end;');

        Compose.Add('');
        Compose.Add('  //Verify RegToken (Example)');
        Compose.Add('  if not Verified then');
        Compose.Add('    raise Exception.Create(''Unable to verify the request'');');

        Compose.Add('  //Subject Name from request');
        Compose.Add('  Cert.Subject.Assign(NewCert.Subject);');
        Compose.Add('');
        Compose.Add('  //Subject Alt Name from request');
        Compose.Add('  Ext := NewCert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];');
        Compose.Add('  if Assigned(Ext) then');
        Compose.Add('    Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectAltName).Assign(Ext);');
        Compose.Add('');

        Compose.Add('  // The following assignments are CA overrides of the values entered by the client');
        GenerateSourceCertOrRoot(PrivDecl,GetDecl,SetDecl,PropDecl,Implements,
                                 Idents,Compose,ClassName,Cert);

        Compose.Add('');
        Compose.Add('  //Validity');
        Compose.Add('  Cert.Validity.NotBefore.Choice := teUTCTime;');
        Compose.Add('  Cert.Validity.NotBefore.AsUtcTime.AsDateTime := Trunc(Now - OffsetFromUTC/24);');
        Compose.Add('  Cert.Validity.NotAfter.Choice := teUTCTime;');
        Compose.Add(Format(
          '  Cert.Validity.NotAfter.AsUtcTime.AsDateTime := Trunc(Now - OffsetFromUTC/24 + %d);',
          [StrToInt(FForm.edtValidityPeriod.Text)]));

        Compose.Add('');
        Compose.Add('  //Key usage (for client certificate; change if other)');
        Compose.Add('  if SubjectPublicKeyInfo.Algorithm.Algorithm = rsaEncryption then');
        Compose.Add('    Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature,keyEncipherment]');
        Compose.Add('  else if SubjectPublicKeyInfo.Algorithm.Algorithm = id_dsa then');
        Compose.Add('    Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature]');
        Compose.Add('  else if SubjectPublicKeyInfo.Algorithm.Algorithm = id_ecPublicKey then');
        Compose.Add('    Cert.TbsCertificate.Extensions.KeyUsage := [digitalSignature];');

        Compose.Add('');
        Compose.Add('  //Extended Key Usage (for client certificate; change if other)');
        Compose.Add(
          '  Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeExtKeyUsage);' + LF +
          '  Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_clientAuth);');

        Compose.Add('');
        Compose.Add('  //Basic constraints (for client certificate; change if other)');
        Compose.Add(
          '  Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeBasicConstraints);' + LF +
          '  Ext.ExtnValue.AsCe_BasicConstraints.CA := False;');

        Compose.Add('');
        Compose.Add('  //Additional extensions can be set here, such as CRL Distribution Point,');
        Compose.Add('  //Certificate Policies, Name Constraints, etc');


        Compose.Add('');
        Compose.Add('  //The Private key is unknown at this point. The public key');
        Compose.Add('  //will be copied from the NewCert object to the Cert object by');
        Compose.Add('  //the TLSServer component');

        Idx := Implements.Add(Format(
          'procedure %s.ComposeCertificate;' + LF +
          'var' + LF +
          '  Idx: Integer;' + LF +
          '  Ext: TExtension;' + LF +
          'begin' + LF +
          '%s' + LF +
          '  inherited;' + LF +
          'end;',
          [ClassName,Compose.Text]));
        Idents.AddObject('ComposeCertificate',TObject(Idx));

        if FForm.edtExportFileName.Text <> '' then begin
          FileName := FForm.edtExportFileName.Text;
          EncodeConstStr(FileName);
          Idx := Implements.Add(Format(
            'procedure %s.AfterExecute(ATLSServer: ITLSServer);' + LF +
            'begin' + LF +
            '  Cert.SaveToFile(%s);' + LF +
            '  // inherited will save Cert to ExportData' + LF +
            '  inherited;' + LF +
            'end;',
            [ClassName,FileName]));
        end else
          Idx := Implements.Add(Format(
            'procedure %s.AfterExecute(ATLSServer: ITLSServer);' + LF +
            'begin' + LF +
            '  // This is where you should save the new certificate' + LF +
            '  // inherited will save Cert to ExportData' + LF +
            '  inherited;' + LF +
            'end;',
            [ClassName]));
        Idents.AddObject('AfterExecute',TObject(Idx));

        for I := 0 to Idents.Count - 1 do begin
          Idx := Integer(Idents.Objects[I]);
          Impl.Add(Implements[Idx]);
          Impl.Add('');
        end;
        {$IFDEF D6UP}
        CreateGUID(GUID);
        {$ENDIF}

        Result := Format(
          'unit %s;' + LF +
          LF +
          'interface' + LF +
          LF +
          'uses' + LF +
          '  SysUtils,' + LF +
          '  SecUtils, Pkix, Asn1, Pkix_Cert, Pkcs_10, X509Base, MpX509,' + LF +
          '  StreamSecII;' + LF +
          LF +
          'type' + LF +
          '  I%s = interface(INewCertDlg)' + LF +
          '  [''%s'']' + LF +
          '%s' + LF +
          '%s' + LF +
          '%s' + LF +
          '  end;' + LF +
          LF +
          '  %s = class(TNewCertDlgObject,I%s)' + LF +
          '  private' + LF +
          '%s' +
          '  protected' + LF +
          '%s' + LF +
          '%s' + LF +
          '    procedure ComposeCertificate; override;' + LF +
          '    procedure AfterExecute(ATLSServer: ITLSServer); override;' + LF +
          '  end;' + LF +
          LF +
          'implementation' + LF +
          LF +
          '{ %s }' + LF +
          LF +
          '%s' + LF +
          LF +
          'end.',
          [ModuleIdent,
           Copy(ClassName,2,MaxInt),
          {$IFDEF D6UP}
           GUIDToString(GUID),
           {$ELSE}
           CreateClassID,
           {$ENDIF}
           GetDecl.Text,SetDecl.Text,PropDecl.Text,
           ClassName,Copy(ClassName,2,MaxInt),
           PrivDecl.Text,GetDecl.Text,SetDecl.Text,
           ClassName,
           Impl.Text]);
      finally
        Implements.Free;
        Compose.Free;
        GetDecl.Free;
        SetDecl.Free;
        PrivDecl.Free;
        PropDecl.Free;
        Impl.Free;
      end;
    finally
      Idents.Free;
    end;
  finally
    Cert.Free;
  end;
end;

procedure TSsModuleCreator.GenerateSourceCertOrRoot(PrivDecl, GetDecl,
  SetDecl, PropDecl, Implements, Idents, Compose: TStrings;
  AClassName: string; Cert: TCertificate);
var
  I: Integer;
  Value: string;
begin
  Cert.Subject.LineSeparator := '/';
  Compose.Add('  // Subject');
  Compose.Add('  Cert.Subject.LineSeparator := ''/'';');
  for I := Low(SubjectNameFields) to High(SubjectNameFields) do begin
    Value := Cert.Subject.AsRdnSequence.Values[SubjectNameFieldEnums[I]];
    if FForm.clbSubjectNameFields.Items[I-Low(SubjectNameFields)].Checked then begin
      GenerateProperty(I,PrivDecl,GetDecl,SetDecl,PropDecl,Implements,
                       Idents,AClassName);

      if Value <> '' then begin
        EncodeConstStr(Value);
        Compose.Add(Format('  if F%s <> '''' then' + LF +
                           '    Cert.Subject.%s := F%s' + LF +
                           '  else' + LF +
                           '    Cert.Subject.%s := %s;',
                           [SubjectNameFields[I],
                            SubjectNameFields[I],
                            SubjectNameFields[I],
                            SubjectNameFields[I],
                            Value]))
      end else
        Compose.Add(Format('  Cert.Subject.%s := F%s;',
                           [SubjectNameFields[I],
                            SubjectNameFields[I]]));
    end else if Value <> '' then begin
      EncodeConstStr(Value);
      Compose.Add(Format('  Cert.Subject.%s := %s;',
                         [SubjectNameFields[I],
                          Value]));
    end;
  end;
  for I := Low(SubjectAltNameFields) to High(SubjectAltNameFields) do
    if FForm.clbSubjectAltNameFields.Items[I-Low(SubjectAltNameFields)].Checked then
      GenerateProperty(I,PrivDecl,GetDecl,SetDecl,PropDecl,Implements,
                       Idents,AClassName);
  SubjectAltNameFromProperty(Compose,Cert);
end;

function TSsModuleCreator.GenerateSourceCertReq(
  const ModuleIdent: string): string;
var
  Idents, Implements, PrivDecl, GetDecl, SetDecl,
  PropDecl, Compose, Impl: TStringList;
  ClassName, FileName, Value: string;
  Cert: TCertificate;
  {$IFDEF D6UP}
  GUID: TGUID;
  {$ENDIF}
  I, Idx: Integer;
begin
  Cert := TCertificate.Create(nil,nil);
  try
    if FileExists(FForm.edtTemplateFile.Text) then
      Cert.LoadFromFile(FForm.edtTemplateFile.Text);
    ClassName := FForm.edtClassName.Text;
    Idents := TStringList.Create;
    try
      Idents.Sorted := True;
      {$IFDEF D6UP}
      Idents.CaseSensitive := False;
      {$ENDIF}
      Implements := TStringList.Create;
      Compose := TStringList.Create;
      GetDecl := TStringList.Create;
      SetDecl := TStringList.Create;
      PrivDecl := TStringList.Create;
      PropDecl := TStringList.Create;
      Impl := TStringList.Create;
      try
        Compose.Add('  if CreateOpt <> nccCertReq then' + LF +
                    '    raise Exception.Create(''Create Option not supported'');');
        Cert.Subject.LineSeparator := '/';
        Compose.Add('  // Subject');
        Compose.Add('  P10.Subject.LineSeparator := ''/'';');
        for I := Low(SubjectNameFields) to High(SubjectNameFields) do begin
          Value := Cert.Subject.AsRdnSequence.Values[SubjectNameFieldEnums[I]];
          if FForm.clbSubjectNameFields.Items[I-Low(SubjectNameFields)].Checked then begin
            GenerateProperty(I,PrivDecl,GetDecl,SetDecl,PropDecl,Implements,
                             Idents,ClassName);

            if Value <> '' then begin
              EncodeConstStr(Value);
              Compose.Add(Format('  if F%s <> '''' then' + LF +
                                 '    P10.Subject.%s := F%s' + LF +
                                 '  else' + LF +
                                 '    P10.Subject.%s := %s;',
                                 [SubjectNameFields[I],
                                  SubjectNameFields[I],
                                  SubjectNameFields[I],
                                  SubjectNameFields[I],
                                  Value]))
            end else
              Compose.Add(Format('  P10.Subject.%s := F%s;',
                                 [SubjectNameFields[I],
                                  SubjectNameFields[I]]));
          end else if Value <> '' then begin
            EncodeConstStr(Value);
            Compose.Add(Format('  P10.Subject.%s := %s;',
                               [SubjectNameFields[I],
                                Value]));
          end;
        end;
        for I := Low(SubjectAltNameFields) to High(SubjectAltNameFields) do
          if FForm.clbSubjectAltNameFields.Items[I-Low(SubjectAltNameFields)].Checked then
            GenerateProperty(I,PrivDecl,GetDecl,SetDecl,PropDecl,Implements,
                             Idents,ClassName);
        SubjectAltNameFromProperty(Compose,Cert);

        Compose.Add('');
        Compose.Add('  //Private key');
        case FForm.cbPublicKeyAlgorithm.ItemIndex of
          0:
            begin
              Compose.Add('  PrivateKeyAlg := rsaEncryption;');
              Compose.Add('  if not (Wizard in [ncwAdvanced,ncwCA]) then');
              Compose.Add('    ExportableRSA := True;');
            end;
          1: Compose.Add('  PrivateKeyAlg := id_dsa;');
          2: Compose.Add('  PrivateKeyAlg := id_ecPublicKey;');
        end;
        Compose.Add(Format('  KeySize := %d;',[StrToInt(FForm.edtPublicKeySize.Text)]));
        Compose.Add('  SignatureDigestAlg := haSHA1;');

        Compose.Add('');
        Compose.Add('  //RegToken and ChallengePassword');
        Compose.Add('  P10.CertificationRequestInfo.Attributes.RegToken := RegToken;');
        Compose.Add('  P10.CertificationRequestInfo.Attributes.ChallengePassword := ChallengePassword;');

        Idx := Implements.Add(Format(
          'procedure %s.ComposeCertificate;' + LF +
          'var' + LF +
          '  Ext: TExtension;' + LF +
          'begin' + LF +
          '%s' + LF +
          '  inherited;' + LF +
          'end;',
          [ClassName,Compose.Text]));
        Idents.AddObject('ComposeCertificate',TObject(Idx));

        if FForm.edtExportFileName.Text <> '' then begin
          FileName := FForm.edtExportFileName.Text;
          EncodeConstStr(FileName);
          Idx := Implements.Add(Format(
            'procedure %s.AfterExecute(ATLSServer: ITLSServer);' + LF +
            'begin' + LF +
            '  P10.SaveToFile(%s);' + LF +
            '  ATLSServer.SavePrivateKeyRing(''User.pkr'');' + LF +
            '  // inherited will save P10 to ExportData' + LF +
            '  inherited;' + LF +
            'end;',
            [ClassName,FileName]));
        end else
          Idx := Implements.Add(Format(
            'procedure %s.AfterExecute(ATLSServer: ITLSServer);' + LF +
            'begin' + LF +
            '  // This is where you should save the key or request to a file' + LF +
            '  ATLSServer.SavePrivateKeyRing(''User.pkr'');' + LF +
            '  // inherited will save P10 to ExportData' + LF +
            '  inherited;' + LF +
            'end;',
            [ClassName]));
        Idents.AddObject('AfterExecute',TObject(Idx));

        for I := 0 to Idents.Count - 1 do begin
          Idx := Integer(Idents.Objects[I]);
          Impl.Add(Implements[Idx]);
          Impl.Add('');
        end;
        {$IFDEF D6UP}
        CreateGUID(GUID);
        {$ENDIF}

        Result := Format(
          'unit %s;' + LF +
          LF +
          'interface' + LF +
          LF +
          'uses' + LF +
          '  SysUtils,' + LF +
          '  SecUtils, Pkix, Asn1, Pkix_Cert, Pkcs_10, X509Base, MpX509,' + LF +
          '  StreamSecII;' + LF +
          LF +
          'type' + LF +
          '  I%s = interface(INewCertReqDlg)' + LF +
          '  [''%s'']' + LF +
          '%s' + LF +
          '%s' + LF +
          '%s' + LF +
          '  end;' + LF +
          LF +
          '  %s = class(TNewCertDlgObject,I%s)' + LF +
          '  private' + LF +
          '%s' +
          '  protected' + LF +
          '%s' + LF +
          '%s' + LF +
          '    procedure ComposeCertificate; override;' + LF +
          '    procedure AfterExecute(ATLSServer: ITLSServer); override;' + LF +
          '  end;' + LF +
          LF +
          'implementation' + LF +
          LF +
          '{ %s }' + LF +
          LF +
          '%s' + LF +
          LF +
          'end.',
          [ModuleIdent,
           Copy(ClassName,2,MaxInt),
           {$IFDEF D6UP}
           GUIDToString(GUID),
           {$ELSE}
           CreateClassID,
           {$ENDIF}
           GetDecl.Text,SetDecl.Text,PropDecl.Text,
           ClassName,Copy(ClassName,2,MaxInt),
           PrivDecl.Text,GetDecl.Text,SetDecl.Text,
           ClassName,
           Impl.Text]);
      finally
        Implements.Free;
        Compose.Free;
        GetDecl.Free;
        SetDecl.Free;
        PrivDecl.Free;
        PropDecl.Free;
        Impl.Free;
      end;
    finally
      Idents.Free;
    end;
  finally
    Cert.Free;
  end;
end;

function TSsModuleCreator.GenerateSourceRoot(
  const ModuleIdent: string): string;
var
  Idents, Implements, PrivDecl, GetDecl, SetDecl,
  PropDecl, Compose, Impl: TStringList;
  ClassName, FileName: string;
  Cert: TCertificate;
  {$IFDEF D6UP}
  GUID: TGUID;
  {$ENDIF}
  I, Idx: Integer;
begin
  Cert := TCertificate.Create(nil,nil);
  try
    if FileExists(FForm.edtTemplateFile.Text) then
      Cert.LoadFromFile(FForm.edtTemplateFile.Text);
    ClassName := FForm.edtClassName.Text;
    Idents := TStringList.Create;
    try
      Idents.Sorted := True;
      {$IFDEF D6UP}
      Idents.CaseSensitive := False;
      {$ENDIF}
      Implements := TStringList.Create;
      Compose := TStringList.Create;
      GetDecl := TStringList.Create;
      SetDecl := TStringList.Create;
      PrivDecl := TStringList.Create;
      PropDecl := TStringList.Create;
      Impl := TStringList.Create;
      try
        Compose.Add('  if not (CreateOpt in [nccRoot,nccCert]) then' + LF +
                    '    raise Exception.Create(''Create Option not supported'');');
        Compose.Add('  if not (Wizard in [ncwAdvanced,ncwCA]) then' + LF +
                    '    raise Exception.Create(''Create Option not supported'');');
        Compose.Add('');
        GenerateSourceCertOrRoot(PrivDecl,GetDecl,SetDecl,PropDecl,Implements,
                                 Idents,Compose,ClassName,Cert);

        Compose.Add('');
        Compose.Add('  //Validity');
        Compose.Add('  Cert.Validity.NotBefore.Choice := teUTCTime;');
        Compose.Add('  Cert.Validity.NotBefore.AsUtcTime.AsDateTime := Trunc(Now - OffsetFromUTC/24);');
        Compose.Add('  Cert.Validity.NotAfter.Choice := teUTCTime;');
        Compose.Add(Format(
          '  Cert.Validity.NotAfter.AsUtcTime.AsDateTime := Trunc(Now - OffsetFromUTC/24 + %d);',
          [StrToInt(FForm.edtValidityPeriod.Text)]));

        Compose.Add('');
        Compose.Add('  //Key usage');
        Compose.Add('  Cert.TbsCertificate.Extensions.KeyUsage := [keyCertSign,cRLSign];');

        Compose.Add('');
        Compose.Add('  //Basic constraints');
        Compose.Add(
          '  Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeBasicConstraints);' + LF +
          '  Ext.ExtnValue.AsCe_BasicConstraints.CA := True;' + LF +
          '  // Change PathLen to allow intermediary CAs in the chain:' + LF +
          '  Ext.ExtnValue.AsCe_BasicConstraints.PathLenConstraint.AsInteger := 0;');

        Compose.Add('');
        Compose.Add('  //Additional extensions can be set here, such as CRL Distribution Point,');
        Compose.Add('  //Certificate Policies, Name Constraints, etc');
        
        Compose.Add('');
        Compose.Add('  //Private key');
        case FForm.cbPublicKeyAlgorithm.ItemIndex of
          0: Compose.Add('  PrivateKeyAlg := rsaEncryption;');
          1: Compose.Add('  PrivateKeyAlg := id_dsa;');
          2: Compose.Add('  PrivateKeyAlg := id_ecPublicKey;');
        end;
        Compose.Add(Format('  KeySize := %d;',[StrToInt(FForm.edtPublicKeySize.Text)]));
        Compose.Add('  SignatureDigestAlg := haSHA1;');

        Compose.Add('');
        Compose.Add('  //Self signed certificate');
        Compose.Add('  CACert := Cert.Data;');

        Idx := Implements.Add(Format(
          'procedure %s.ComposeCertificate;' + LF +
          'var' + LF +
          '  Ext: TExtension;' + LF +
          'begin' + LF +
          '%s' + LF +
          '  inherited;' + LF +
          'end;',
          [ClassName,Compose.Text]));
        Idents.AddObject('ComposeCertificate',TObject(Idx));

        if FForm.edtExportFileName.Text <> '' then begin
          FileName := FForm.edtExportFileName.Text;
          EncodeConstStr(FileName);
          Idx := Implements.Add(Format(
            'procedure %s.AfterExecute(ATLSServer: ITLSServer);' + LF +
            'begin' + LF +
            '  Cert.SaveToFile(%s);' + LF +
            '  ATLSServer.SavePrivateKeyRing(''User.pkr'');' + LF +
            '  ATLSServer.SaveMyCertsToSCLFile(''User.scl'');' + LF +
            '  // inherited will save Cert to ExportData' + LF +
            '  inherited;' + LF +
            'end;',
            [ClassName,FileName]))
        end else
          Idx := Implements.Add(Format(
            'procedure %s.AfterExecute(ATLSServer: ITLSServer);' + LF +
            'begin' + LF +
            '  // This is where you should save the new certificate' + LF +
            '  ATLSServer.SavePrivateKeyRing(''User.pkr'');' + LF +
            '  ATLSServer.SaveMyCertsToSCLFile(''User.scl'');' + LF +
            '  // inherited will save Cert to ExportData' + LF +
            '  inherited;' + LF +
            'end;',
            [ClassName]));
        Idents.AddObject('AfterExecute',TObject(Idx));

        for I := 0 to Idents.Count - 1 do begin
          Idx := Integer(Idents.Objects[I]);
          Impl.Add(Implements[Idx]);
          Impl.Add('');
        end;
        {$IFDEF D6UP}
        CreateGUID(GUID);
        {$ENDIF}

        Result := Format(
          'unit %s;' + LF +
          LF +
          'interface' + LF +
          LF +
          'uses' + LF +
          '  SysUtils,' + LF +
          '  SecUtils, Pkix, Asn1, Pkix_Cert, Pkcs_10, X509Base, MpX509,' + LF +
          '  StreamSecII;' + LF +
          LF +
          'type' + LF +
          '  I%s = interface(INewCertDlg)' + LF +
          '  [''%s'']' + LF +
          '%s' + LF +
          '%s' + LF +
          '%s' + LF +
          '  end;' + LF +
          LF +
          '  %s = class(TNewCertDlgObject,I%s)' + LF +
          '  private' + LF +
          '%s' +
          '  protected' + LF +
          '%s' + LF +
          '%s' + LF +
          '    procedure ComposeCertificate; override;' + LF +
          '    procedure AfterExecute(ATLSServer: ITLSServer); override;' + LF +
          '  end;' + LF +
          LF +
          'implementation' + LF +
          LF +
          '{ %s }' + LF +
          LF +
          '%s' + LF +
          LF +
          'end.',
          [ModuleIdent,
           Copy(ClassName,2,MaxInt),
           {$IFDEF D6UP}
           GUIDToString(GUID),
           {$ELSE}
           CreateClassID,
           {$ENDIF}
           GetDecl.Text,SetDecl.Text,PropDecl.Text,
           ClassName,Copy(ClassName,2,MaxInt),
           PrivDecl.Text,GetDecl.Text,SetDecl.Text,
           ClassName,
           Impl.Text]);
      finally
        Implements.Free;
        Compose.Free;
        GetDecl.Free;
        SetDecl.Free;
        PrivDecl.Free;
        PropDecl.Free;
        Impl.Free;
      end;
    finally
      Idents.Free;
    end;
  finally
    Cert.Free;
  end;
end;

function TSsModuleCreator.NewImplSource(const ModuleIdent,FormIdent,AncestorIdent:string):IOTAFile;
begin
  if FForm.edtClassName.Text = '' then
    raise Exception.Create('Classname must be given');
  Result := TSsSourceFile.Create(Self.GenerateSource(ModuleIdent));
end;

function TSsModuleCreator.NewIntfSource(const ModuleIdent,FormIdent,AncestorIdent:string):IOTAFile;
begin
  Result := nil;
end;

procedure TSsModuleCreator.SubjectAltNameFromProperty(Compose: TStrings;
  Cert: TCertificate);
var
  Values: array [11..14] of string;
  Target: string;
  I: Integer;
  Ext: TExtension;
  Critical, Used: Boolean;

begin
  if FForm.rgPurpose.ItemIndex = 2 then
    Target := 'P10.CertificationRequestInfo'
  else
    Target := 'Cert.TbsCertificate';
  Ext := Cert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];
  if Assigned(Ext) then begin
    for I := 0 to Ext.ExtnValue.AsCe_SubjectAltName.Count - 1 do
      case Ext.ExtnValue.AsCe_SubjectAltName.Items[I].Choice of
        gneRfc822Name:
          Values[11] := Ext.ExtnValue.AsCe_SubjectAltName.Items[I].AsRfc822Name;
        gneUniformResourceIdentifier:
          Values[12] := Ext.ExtnValue.AsCe_SubjectAltName.Items[I].AsUniformResourceIdentifier;
        gneDNSName:
          Values[13] := Ext.ExtnValue.AsCe_SubjectAltName.Items[I].AsDNSName;
        gneIPAddress:
          Values[14] := FormatIPAddress(Ext.ExtnValue.AsCe_SubjectAltName.Items[I].AsIPAddress);
      else
        Continue;
      end;
    Critical := Ext.Critical;
  end else
    Critical := False;

  Used := False;
  for I := Low(SubjectAltNameFields) to High(SubjectAltNameFields) do begin
    EncodeConstStr(Values[I]);
    if FForm.clbSubjectAltNameFields.Items[I-Low(SubjectAltNameFields)].Checked then begin
      if not Used then begin
        Compose.Add('');
        Compose.Add('  // Subject Alt Name');
      end;
      Used := True;
      if Values[I] <> '' then
        Compose.Add(Format('  if F%s = '''' then' + LF +
                           '    F%s := %s;',
                           [SubjectAltNameFields[I],
                            SubjectAltNameFields[I],
                            Values[I]]));
      Compose.Add(Format('  if F%s <> '''' then begin',[SubjectAltNameFields[I]]));
      Compose.Add(Format('    Ext := %s.Extensions.AddUniqueItem(eveIdCeSubjectAltName);',[Target]));
      Compose.Add('    with Ext.ExtnValue.AsCe_SubjectAltName.Add do begin');
      Compose.Add(Format('      Choice := gne%s;',[SubjectAltNameFields[I]]));
      if I < High(SubjectAltNameFields) then
        Compose.Add(Format('      As%s := F%s;',
                           [SubjectAltNameFields[I],SubjectAltNameFields[I]]))
      else
        Compose.Add('      ParseIPAddress(FIPAddress,AsIPAddress);');
      Compose.Add('    end;');
      Compose.Add('  end;');
    end else if Values[I] <> '' then begin
      if not Used then begin
        Compose.Add('');
        Compose.Add('  // Subject Alt Name');
      end;
      Used := True;
      Compose.Add(Format('  Ext := %s.Extensions.AddUniqueItem(eveIdCeSubjectAltName);',[Target]));
      Compose.Add('  with Ext.ExtnValue.AsCe_SubjectAltName.Add do begin');
      Compose.Add(Format('    Choice := gne%s;',[SubjectAltNameFields[I]]));
      if I < High(SubjectAltNameFields) then
        Compose.Add(Format('    As%s := %s;',
                           [SubjectAltNameFields[I],Values[I]]))
      else
        Compose.Add(Format('    ParseIPAddress(%s,AsIPAddress);',[Values[I]]));
      Compose.Add('  end;');
    end;
  end;
  if Used and Critical then begin
    Compose.Add(Format('  Ext := %s.Extensions.UniqueItem[eveIdCeSubjectAltName];',[Target]));
    Compose.Add('  if Assigned(Ext) then');
    Compose.Add('    Ext.Critical := True;');
  end;
end;

{ TSsSourceFile }

constructor TSsSourceFile.Create(const Source: string);
begin
  FSource := Source;
end;

function TSsSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TSsSourceFile.GetSource: string;
begin
  Result := FSource;
end;

end.


