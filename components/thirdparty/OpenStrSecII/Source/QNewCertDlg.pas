{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     NewCertDlg Unit                                   }
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
unit QNewCertDlg;

interface

uses         
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QComCtrls, QCheckLst, QMask, QExtCtrls,
  Asn1, Pkix_Cert, Pkix_CRMF, Pkcs_10, StreamSecII, MpX509;

type
  TCRMKind = (crmCRMF,crmPKCS10,crmCert);

  TfrmNewCertDlg = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    btnNext: TButton;
    PageControl1: TPageControl;
    tabCreate: TTabSheet;
    tabIssuer: TTabSheet;
    tabSubject: TTabSheet;
    tabSubjectAltName: TTabSheet;
    tabValidity: TTabSheet;
    tabKeyUsage: TTabSheet;
    tabBasicConstraints: TTabSheet;
    tabCRLDistributionPoints: TTabSheet;
    tabNameConstraints: TTabSheet;
    tabPolicyConstraints: TTabSheet;
    tabPolicyMappings: TTabSheet;
    tabCertificatePolicies: TTabSheet;
    tabSubjectPublicKey: TTabSheet;
    btnBack: TButton;
    tabFinished: TTabSheet;
    rgCreate: TRadioGroup;
    lvIssuer: TListView;
    Label1: TLabel;
    edtCommonName: TEdit;
    edtSurname: TEdit;
    Label2: TLabel;
    edtCountryName: TEdit;
    Label3: TLabel;
    edtOrganization: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    edtOrganizationalUnit: TEdit;
    Label6: TLabel;
    edtGivenName: TEdit;
    Label7: TLabel;
    edtTitle: TEdit;
    Label8: TLabel;
    edtInitials: TEdit;
    Label9: TLabel;
    edtGenerationQualifier: TEdit;
    Label10: TLabel;
    edtStateOrProvince: TEdit;
    Label11: TLabel;
    edtLocality: TEdit;
    Label12: TLabel;
    edtDNQualifier: TEdit;
    Label13: TLabel;
    edtRFC822Name: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    edtDNSName: TEdit;
    Label16: TLabel;
    Label17: TLabel;
    edtURI: TEdit;
    Label18: TLabel;
    Label19: TLabel;
    edtIPAddress: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    cbSPKIAlgorithm: TComboBox;
    Label23: TLabel;
    Label24: TLabel;
    cbKeySize: TComboBox;
    clbKeyUsage: TCheckListBox;
    Label26: TLabel;
    Label25: TLabel;
    edtPathLenConstraint: TEdit;
    Label27: TLabel;
    edtCRLDistPointURI: TEdit;
    Label28: TLabel;
    edtCRLDistPointIP: TEdit;
    clbCRLReason: TCheckListBox;
    Label29: TLabel;
    lbCRLDistPoints: TListBox;
    btnDeleteCRLDistPoint: TButton;
    btnAddCRLDistPoint: TButton;
    Label30: TLabel;
    btnUpdateCRLDistPoint: TButton;
    Label31: TLabel;
    edtSubTreeId: TEdit;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    lbPermittedSubTrees: TListBox;
    Label39: TLabel;
    Label40: TLabel;
    lbExcludedSubTrees: TListBox;
    Label41: TLabel;
    rgSubTreeType: TRadioGroup;
    btnSubTreeUpdate: TButton;
    btnExclSubTreeAdd: TButton;
    btnSubTreeDelete: TButton;
    btnPermSubTreeAdd: TButton;
    Label42: TLabel;
    edtIPM: TEdit;
    Label43: TLabel;
    edtREP: TEdit;
    Label44: TLabel;
    memPolicyMappings: TMemo;
    Label45: TLabel;
    Label46: TLabel;
    Label49: TLabel;
    tabExtKeyUsage: TTabSheet;
    Label48: TLabel;
    clbExtKeyUsage: TCheckListBox;
    chbExtKeyUsageCritical: TCheckBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    Label50: TLabel;
    cbPolicy: TComboBox;
    Label51: TLabel;
    edtCPS: TEdit;
    btnDeletePolicy: TButton;
    btnAddPolicy: TButton;
    Button3: TButton;
    lbCertificatePolicies: TListBox;
    lblFinishedMsg: TLabel;
    cbSignatureAlgorithm: TComboBox;
    Label47: TLabel;
    rgWizard: TRadioGroup;
    meNotBefore: TMaskEdit;
    meNotAfter: TMaskEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvIssuerData(Sender: TObject; Item: TListItem);
    procedure lvIssuerSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure cbSPKIAlgorithmSelect(Sender: TObject);
    procedure tabCRLDistributionPointsShow(Sender: TObject);
    procedure lbCRLDistPointsClick(Sender: TObject);
    procedure btnUpdateCRLDistPointClick(Sender: TObject);
    procedure btnAddCRLDistPointClick(Sender: TObject);
    procedure btnDeleteCRLDistPointClick(Sender: TObject);
    procedure tabNameConstraintsShow(Sender: TObject);
    procedure lbPermittedSubTreesClick(Sender: TObject);
    procedure lbExcludedSubTreesClick(Sender: TObject);
    procedure btnSubTreeUpdateClick(Sender: TObject);
    procedure btnSubTreeDeleteClick(Sender: TObject);
    procedure btnExclSubTreeAddClick(Sender: TObject);
    procedure btnPermSubTreeAddClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnAddPolicyClick(Sender: TObject);
    procedure tabCertificatePoliciesShow(Sender: TObject);
    procedure lbCertificatePoliciesClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnDeletePolicyClick(Sender: TObject);
    procedure tabFinishedShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure rgCreateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FPages: TList;
    FPageIndex: Integer;
    FCACert: TASN1Struct;
    FNewCert: TCertificate;
    FDistPoints: TCrldistributionPointsSyntax;
    FNameConstraints: TNameConstraintsSyntax;
    FCABC: TBasicConstraintsSyntax;
    FCANC: TNameConstraintsSyntax;
    FPolicyMappings: TStrings;
    FAllowMapping: Boolean;
    FRequirePolicy: Boolean;
    FCertificatePolicies: TCertificatePolicies;
    FPrivateKeyRing: TSsPrivateKeyRingComponent;
    FIssuedCerts: TX509TrustedCertificates;
    FMyCerts: TX509TrustedCertificates;
    FRootCerts: TX509TrustedCertificates;
    FWizardMode: Boolean;
    FExportData: TStream;
    procedure AddPage(Page: TTabSheet);
    procedure RemovePage(Page: TTabSheet);
    function CheckAltName: Boolean;
    function CheckName: Boolean;
    procedure CheckValues;
    procedure PrepareIssuer;
    procedure PrepareValidity;
    procedure PrepareKeyUsage;
    procedure PrepareExtKeyUsage;
    procedure PrepareBasicConstraints;
    function LoadCRM(const FileName: string): Boolean;
    function LoadCRMStream(Stream: TStream; AKind: TCRMKind): Boolean;
    function NextPage: TTabSheet;
    function PrevPage: TTabSheet;
    procedure SetIssuedCerts(const Value: TX509TrustedCertificates);
    procedure SetMyCerts(const Value: TX509TrustedCertificates);
    procedure SetPrivateKeyRing(const Value: TSsPrivateKeyRingComponent);
    procedure SetRootCerts(const Value: TX509TrustedCertificates);
    procedure SetExportData(const Value: TStream);
  public                 
    Issuers: TX509TrustedCertificates;
    procedure SelectWizard(Index: Integer; GenerateRequest: Boolean = False);
    property MyCerts: TX509TrustedCertificates read FMyCerts write SetMyCerts;
    property RootCerts: TX509TrustedCertificates read FRootCerts write SetRootCerts;
    property IssuedCerts: TX509TrustedCertificates read FIssuedCerts write SetIssuedCerts;
    property PrivateKeyRing: TSsPrivateKeyRingComponent read FPrivateKeyRing write SetPrivateKeyRing;
    property ExportData: TStream read FExportData write SetExportData;
  end;

implementation

uses
  SsBase64, SecUtils, Pkix, X509Base, ImportDlg, ExportDlg;

{$R *.xfm}

procedure TfrmNewCertDlg.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Issuers := TX509TrustedCertificates.Create(Self);
  for I := 0 to PageControl1.PageCount - 1 do begin
    PageControl1.Pages[I].TabVisible := False;
    PageControl1.Pages[I].Tag := I;
  end;
  FPages := TList.Create;
  FDistPoints := TCrldistributionPointsSyntax.Create(nil,nil);
  FDistPoints.Data.ReadOnly := False;
  FNameConstraints := TNameConstraintsSyntax.Create(nil,nil);
  FCABC := TBasicConstraintsSyntax.Create(nil,nil);
  FCANC := TNameConstraintsSyntax.Create(nil,nil);
  FPolicyMappings := TStringList.Create;
  FCertificatePolicies := TCertificatePolicies.Create(nil,nil);
end;

procedure TfrmNewCertDlg.FormShow(Sender: TObject);
var
  I: Integer;
  rbTmp: TRadioButton;
begin
  if FWizardMode then
    Exit; // The form has already been initialized by the wizard

  for I := 0 to PageControl1.PageCount - 1 do
    if FPages.IndexOf(PageControl1.Pages[I]) < 0 then
      FPages.Add(PageControl1.Pages[I]);
  FPageIndex := 0;
  PageControl1.ActivePage := tabCreate;
  btnNext.Enabled := True;
  btnBack.Enabled := False;
  btnOK.Enabled := False;
  FCACert := nil;
//  FDistPoints.Clear;
//  FNameConstraints.PermittedSubtrees.Clear;
//  FNameConstraints.ExcludedSubtrees.Clear;
  FCABC.CA := True;
  FCABC.PathLenConstraint.Data.ReadOnly := False;
  FCABC.PathLenConstraint.Data.Length := 0;
  FCANC.PermittedSubtrees.Clear;
  FCANC.ExcludedSubtrees.Clear;
  cbPolicy.Clear;
  cbPolicy.Items.Add('ANY');
  FPolicyMappings.Clear;
  FAllowMapping := True;
  FRequirePolicy := False;
  FCertificatePolicies.Clear;

  rgCreate.ItemIndex := 1;
  for I := 0 to rgCreate.ComponentCount - 1 do
    if rgCreate.Components[I] is TRadioButton then begin
      rbTmp := TRadioButton(rgCreate.Components[I]);
      if rbTmp.Caption = rgCreate.Items[1] then
        rbTmp.Enabled := MyCerts.Count > 0
      else if rbTmp.Caption = rgCreate.Items[2] then
        rbTmp.Enabled := RootCerts.Count > 0
      else if rbTmp.Caption = rgCreate.Items[3] then
        rbTmp.Enabled := (MyCerts.Count > 0) and Assigned(IssuedCerts);
      if rbTmp.Checked and not rbTmp.Enabled then begin
        rbTmp.Checked := False;
        rgCreate.ItemIndex := 0;
      end;
    end;
  PageControl1.TabHeight := 1;
end;

procedure TfrmNewCertDlg.btnBackClick(Sender: TObject);
begin
  PageControl1.ActivePage := PrevPage;
  btnNext.Enabled := True;
  btnBack.Enabled := FPageIndex > 0;
  btnOK.Enabled := False;
end;

function ComparePages(Item1, Item2: Pointer): Integer;
begin
  Result := TTabSheet(Item1).Tag - TTabSheet(Item2).Tag;
end;


procedure TfrmNewCertDlg.CheckValues;
var
  ErrorMsg, WarningMsg: string;
  SS: TStringStream;
  CertReq: string;
  CRMKind: TCRMKind;
  OID: string;
  I, KS: Integer;
  ValCA: TX509Validity;
  ImportDlg: TfrmImportDlg;
begin
  ErrorMsg := '';
  if PageControl1.ActivePage = nil then
    PageControl1.ActivePage := nil;
  case PageControl1.ActivePage.Tag of
    0: if rgCreate.ItemIndex < 0 then
         ErrorMsg := 'Please select a creation option'
       else begin
         if rgCreate.ItemIndex = 0 then begin
           RemovePage(tabIssuer);
           meNotAfter.Text := DateTimeToStr(Trunc(Now - RootCerts.HoursOffsetFromGMT/24) + 2*365);
         end else begin
           if rgWizard.ItemIndex = 0 then
             AddPage(tabIssuer)
           else
             RemovePage(tabIssuer);
           lvIssuer.Selected := nil;
           PrepareIssuer;
         end;
         if rgCreate.ItemIndex = 2 then
           RemovePage(tabValidity)
         else begin
           if rgWizard.ItemIndex = 0 then
             AddPage(tabValidity)
           else
             RemovePage(tabValidity);
           PrepareValidity;
         end;
         if rgCreate.ItemIndex = 3 then begin
           ImportDlg := TfrmImportDlg.Create(nil);
           try
             case ImportDlg.ShowModal of
               mrOK:
                 begin
                   CRMKind := crmPKCS10;
                   CertReq := MIME64ToStr('-----BEGIN CERTIFICATE REQUEST-----',
                                          '-----END CERTIFICATE REQUEST-----',
                                          ImportDlg.memImport.Text);
                   if CertReq = '' then
                     CertReq := MIME64ToStr('-----BEGIN NEW CERTIFICATE REQUEST-----',
                                            '-----END NEW CERTIFICATE REQUEST-----',
                                            ImportDlg.memImport.Text);
                   if CertReq = '' then begin
                     CertReq := MIME64ToStr('-----BEGIN CERTIFICATE-----',
                                            '-----END CERTIFICATE-----',
                                            ImportDlg.memImport.Text);
                     if CertReq <> '' then
                       CRMKind := crmCert;
                   end;
                   SS := TStringStream.Create(CertReq);
                   try
                     if SS.Size = 0 then
                       ErrorMsg := 'Wrong format'
                     else if not LoadCRMStream(SS,CRMKind) then
                       ErrorMsg := 'Unknown format';
                   finally
                     SS.Free;
                   end;
                 end;
               mrCancel:
                 ErrorMsg := 'No Certificate Request was selected';
               mrAbort:
                 if OpenDialog1.Execute then begin
                   if not LoadCRM(OpenDialog1.FileName) then
                     ErrorMsg := 'Unknown format';
                 end else
                   ErrorMsg := 'No Certificate Request was selected';
             end;
           finally
             ImportDlg.Free;
           end;
           if ErrorMsg = '' then begin
             cbSPKIAlgorithm.Text := GetObjectName(FNewCert.SubjectPublicKeyInfo.Algorithm.Algorithm);
             if FNewCert.SubjectPublicKeyInfo.Algorithm.Parameters.Choice = aideIdEcPublicKey then
               cbKeySize.Text := GetObjectName(FNewCert.SubjectPublicKeyInfo.Algorithm.Parameters.AsEcPublicKey)
             else begin
               ExtractSubjectPublicKeySize(FNewCert.Data,KS);
               cbKeySize.Text := IntToStr(KS);
             end;
           end;
         end;
       end;
    1: if (FCACert = nil) and (rgCreate.ItemIndex <> 2) then
         ErrorMsg := 'Please select an issuer'
       else begin
         FCABC.CA := True;
         FCABC.PathLenConstraint.Data.Length := 0;
         FCANC.PermittedSubtrees.Clear;
         FCANC.ExcludedSubtrees.Clear;
         cbPolicy.Clear;
         cbPolicy.Items.Add('ANY');
         FPolicyMappings.Clear;
         FAllowMapping := True;
         FRequirePolicy := False;
         if Assigned(FCACert) and
            not MyCerts.ExtractConstraintsFromChain(FCACert,
                                                    FCABC,
                                                    FCANC,
                                                    FPolicyMappings,
                                                    cbPolicy.Items,
                                                    FRequirePolicy,
                                                    FAllowMapping) then
           ErrorMsg := 'Selected certificate cannot be used as CA';

         PrepareValidity;
       end;
    2: if not CheckName then
         ErrorMsg := 'The Subject Name violates a Name Constraint';
    3: if not CheckAltName then
         ErrorMsg := 'The Subject Alternative Name violates a Name Constraint';
    4: if StrToDateTime(meNotBefore.Text) >= StrToDateTime(meNotAfter.Text) then
         ErrorMsg := 'Not Before must be before Not After'
       else if StrToDateTime(meNotAfter.Text) < Now then
         ErrorMsg := 'You are about to compose an expired certificate'
       else if rgCreate.ItemIndex <> 0 then begin
         ExtractValidity(FCACert,ValCA);
         if StrToDateTime(meNotBefore.Text) < ValCA.notBefore then
           ErrorMsg := 'Not Before must be after Not Before of Issuer Certificate'
         else if StrToDateTime(meNotAfter.Text) > ValCA.notAfter then
           ErrorMsg := 'Not After must be before Not After of Issuer Certificate';
       end;
    5: begin
         PrepareKeyUsage;
         if rgWizard.ItemIndex = 0 then
           AddPage(tabKeyUsage)
         else begin
           RemovePage(tabKeyUsage);
           RemovePage(tabExtKeyUsage);
           PrepareExtKeyUsage;
           PrepareBasicConstraints;
         end;
         if not (cbSPKIAlgorithm.ItemIndex in [0..3]) then
           ErrorMsg := 'Please select an algorithm for your Public Key';
       end;
    6: begin
         PrepareExtKeyUsage;
       end;
    7: {Anything goes (almost) };
    8: if FCABC.PathLenConstraint.Length > 0 then begin
         if edtPathLenConstraint.Text = '' then
           ErrorMsg := 'Please set then Path Length Constraint to a value equal to or less than ' +
                       IntToStr(FCABC.PathLenConstraint.AsInteger)
         else if FCABC.PathLenConstraint.AsInteger <
                 StrToInt(edtPathLenConstraint.Text) then
           ErrorMsg := 'Path Length Constraint exceeds the number of CA certificates allowed by issuer';
       end else if edtPathLenConstraint.Text = '' then
         WarningMsg := 'You have set the Path Length Constraint to the value Infinte.'#13#10 +
                       'This means that you may have any number of CA certificates in the chain below this certificate'#13#10 +
                       'This is NOT recommended.';
    13:if FRequirePolicy and
          ((lbCertificatePolicies.Items.Count = 0) or
           (lbCertificatePolicies.Items[0] = '')) then
         ErrorMsg := 'You MUST specify an acceptable policy'
       else if FRequirePolicy then begin
         if (cbPolicy.Items.Count <> 1) or
            (cbPolicy.Items[0] <> 'ANY') then begin
           ErrorMsg := 'You MUST specify an acceptable policy';
           for I := 0 to lbCertificatePolicies.Items.Count - 1 do begin
             OID := lbCertificatePolicies.Items[I];
             if OID = '' then Continue;
             repeat
               if cbPolicy.Items.IndexOf(OID) >= 0 then begin
                 ErrorMsg := '';
                 Break;
               end;
               OID := FPolicyMappings.Values[OID];
             until OID = '';
             if ErrorMsg = '' then Break;
           end;
         end;
       end;
  end;
  if ErrorMsg <> '' then
    raise Exception.Create(ErrorMsg)
  else if WarningMsg <> '' then
    MessageDlg(WarningMsg,mtWarning,[mbOK],0);
  FPages.Sort(ComparePages);
end;

procedure TfrmNewCertDlg.btnNextClick(Sender: TObject);
begin
  CheckValues;
  PageControl1.ActivePage := NextPage;
  btnNext.Enabled := FPageIndex < FPages.Count - 1;
  btnBack.Enabled := True;
  btnOK.Enabled := FPageIndex = FPages.Count - 1;
end;

function TfrmNewCertDlg.NextPage: TTabSheet;
begin
  Inc(FPageIndex);
  Result := FPages[FPageIndex];
end;

function TfrmNewCertDlg.PrevPage: TTabSheet;
begin
  Dec(FPageIndex);
  Result := FPages[FPageIndex];
end;

procedure TfrmNewCertDlg.FormDestroy(Sender: TObject);
begin
//  frmNewCertDlg := nil;
  FPages.Free;
  FDistPoints.Free;
  FNameConstraints.Free;
  FCABC.Free;
  FCANC.Free;
  FNewCert.Free;
  FPolicyMappings.Free;
  FCertificatePolicies.Clear;
end;

procedure TfrmNewCertDlg.lvIssuerData(Sender: TObject; Item: TListItem);
var
  C: TASN1Struct;
{$IFDEF NEVER}
  RDName: TName;
  F: PASN1Struct;
{$ELSE}
  RDName: TX501Name;
{$ENDIF}
begin
  C := Issuers.Certs[Item.Index];
  {
  case rgCreate.ItemIndex of
    1,3: C := MyCerts.Certs[Item.Index];
    2:   C := RootCerts.Certs[Item.Index];
  else
    C := nil;
  end;
  }
{$IFDEF NEVER}
  RDName := TName.Create(nil,nil);
  try
    try
      F := nil;
      ExtractSubjectStruct(C,F);
      RDName.AssignStruct(F^);
    except
    end;
    Item.Caption := RDName.CommonName;
    Item.SubItems.Add(RDName.OrganizationName);
    Item.SubItems.Add(RDName.OrganizationalUnitName);
  finally
    RDName.Free;
  end;
{$ELSE}
  ExtractSubject(C,RDName);
  Item.Caption := RDName.commonName.Str;
  Item.SubItems.Add(RDName.OrganizationName.Str);
  Item.SubItems.Add(RDName.OrganizationalUnitName.Str);
{$ENDIF}
end;

procedure TfrmNewCertDlg.lvIssuerSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    FCACert := Issuers.Certs[Item.Index];
end;

procedure TfrmNewCertDlg.cbSPKIAlgorithmSelect(Sender: TObject);
begin
  cbKeySize.Clear;
  case cbSPKIAlgorithm.ItemIndex of
    0:
      begin
        if StrToDateTime(meNotAfter.Text) < Now + 365*5 then
          cbKeySize.Items.Add('1024');
        cbKeySize.Items.Add('2048');
        cbKeySize.Items.Add('3072');
        cbKeySize.Items.Add('4096');
        cbKeySize.Style := csDropDown;
        cbKeySize.Enabled := True;
        cbKeySize.ItemIndex := 1;
        cbSignatureAlgorithm.Enabled := True;
      end;
    1:
      begin
        cbKeySize.Items.Add('1024');
        cbKeySize.Enabled := False;
        cbKeySize.ItemIndex := 0;
        cbSignatureAlgorithm.Enabled := False;
        cbSignatureAlgorithm.ItemIndex := 0;
      end;
    2:
      begin
        if rgCreate.ItemIndex = 0 then
          raise Exception.Create('Diffie-Hellman key cannot be used in root certificates');
        if StrToDateTime(meNotAfter.Text) < Now + 365*5 then
          cbKeySize.Items.Add('1024');
        cbKeySize.Items.Add('2048');
        cbKeySize.Items.Add('3072');
        cbKeySize.Items.Add('4096');
        cbKeySize.Style := csDropDown;
        cbKeySize.Enabled := True;
        cbKeySize.ItemIndex := 1;
        cbSignatureAlgorithm.Enabled := False;
        cbSignatureAlgorithm.ItemIndex := 0;
      end;
    3:
      begin
        if StrToDateTime(meNotAfter.Text) < Now + 365*5 then begin
          cbKeySize.Items.Add('192v1');
          cbKeySize.Items.Add('192v2');
          cbKeySize.Items.Add('192v3');
        end else if StrToDateTime(meNotAfter.Text) < Now + 365*10 then begin
          cbKeySize.Items.Add('239v1');
          cbKeySize.Items.Add('239v2');
          cbKeySize.Items.Add('239v1');
        end;
        cbKeySize.Items.Add('256');
        cbKeySize.Style := csDropDownList;
        cbKeySize.Enabled := True;
        cbKeySize.ItemIndex := cbKeySize.Items.Count - 1;
        cbSignatureAlgorithm.Enabled := False;
        cbSignatureAlgorithm.ItemIndex := 0;
      end;
  end;
end;

procedure TfrmNewCertDlg.tabCRLDistributionPointsShow(Sender: TObject);
var
  I: Integer;
  GenName: TGeneralName;
begin
  lbCRLDistPoints.Clear;
  for I := 0 to FDistPoints.Count - 1 do begin
    if FDistPoints[I].DistributionPoint.AsFullName.Count = 0 then
      lbCRLDistPoints.Items.AddObject(Format('Point #%d',[I]),nil)
    else begin
      GenName := FDistPoints[I].DistributionPoint.AsFullName[0];
      if GenName.Choice = gneUniformResourceIdentifier then
        lbCRLDistPoints.Items.AddObject(GenName.AsUniformResourceIdentifier,GenName)
      else if GenName.Choice = gneIPAddress then
        lbCRLDistPoints.Items.AddObject(FormatIPAddress(GenName.AsIPAddress),GenName)
      else
        lbCRLDistPoints.Items.AddObject(Format('Point #%d',[I]),GenName);
    end;
  end;
  lbCRLDistPoints.ItemIndex := -1;
  edtCRLDistPointURI.Enabled := False;
  edtCRLDistPointIP.Enabled := False;
  clbCRLReason.Enabled := False;
end;

procedure TfrmNewCertDlg.lbCRLDistPointsClick(Sender: TObject);
var
  I: Integer;
  GenName: TGeneralName;
begin
  I := lbCRLDistPoints.ItemIndex;
  if I >= 0 then begin
    FDistPoints.Data.ReadOnly := False;
    if FDistPoints[I].DistributionPoint.AsFullName.Count = 0 then
      edtCRLDistPointURI.Text := ''
    else begin
      GenName := FDistPoints[I].DistributionPoint.AsFullName[0];
      if GenName.Choice = gneUniformResourceIdentifier then
        edtCRLDistPointURI.Text := GenName.AsUniformResourceIdentifier
      else if GenName.Choice = gneIPAddress then
        edtCRLDistPointIP.Text := FormatIPAddress(GenName.AsIPAddress);
    end;

    if FDistPoints[I].Reasons.BitCount < 9 then
      FDistPoints[I].Reasons.BitCount := 9;
    clbCRLReason.Checked[0] := FDistPoints[I].Reasons.Bits[1];
    clbCRLReason.Checked[1] := FDistPoints[I].Reasons.Bits[2];
    clbCRLReason.Checked[2] := FDistPoints[I].Reasons.Bits[3];
    clbCRLReason.Checked[3] := FDistPoints[I].Reasons.Bits[4];
    clbCRLReason.Checked[4] := FDistPoints[I].Reasons.Bits[5];
    clbCRLReason.Checked[5] := FDistPoints[I].Reasons.Bits[6];
    clbCRLReason.Checked[6] := FDistPoints[I].Reasons.Bits[8];

    edtCRLDistPointURI.Enabled := True;
    edtCRLDistPointIP.Enabled := True;
    clbCRLReason.Enabled := True;
  end else begin
    edtCRLDistPointURI.Enabled := False;
    edtCRLDistPointIP.Enabled := False;
    clbCRLReason.Enabled := False;
  end;
end;

procedure TfrmNewCertDlg.btnUpdateCRLDistPointClick(Sender: TObject);
var
  I, J: Integer;
  GenName: TGeneralName;
  URISet, IPSet: Boolean;
begin
  I := lbCRLDistPoints.ItemIndex;
  if I >= 0 then begin
    FDistPoints.BeginUpdate;
    try
    URISet := edtCRLDistPointURI.Text = '';
    IPSet := edtCRLDistPointIP.Text = '';
    for J := FDistPoints[I].DistributionPoint.AsFullName.Count - 1 downto 0 do begin
      GenName := FDistPoints[I].DistributionPoint.AsFullName[J];
      if GenName.Choice = gneUniformResourceIdentifier then begin
        if edtCRLDistPointURI.Text = '' then
          FDistPoints[I].DistributionPoint.AsFullName.Delete(J)
        else begin
          GenName.AsUniformResourceIdentifier := edtCRLDistPointURI.Text;
          URISet := True;
        end;
      end else if GenName.Choice = gneIPAddress then begin
        if edtCRLDistPointIP.Text = '' then
          FDistPoints[I].DistributionPoint.AsFullName.Delete(J)
        else begin
          ParseIPAddress(edtCRLDistPointIP.Text,GenName.AsIPAddress);
          IPSet := True;
        end;
      end;
    end;
    if not URISet then begin
      GenName := FDistPoints[I].DistributionPoint.AsFullName.Add;
      GenName.Choice := gneUniformResourceIdentifier;
      GenName.AsUniformResourceIdentifier := edtCRLDistPointURI.Text;
    end;
    if not IPSet then begin
      GenName := FDistPoints[I].DistributionPoint.AsFullName.Add;
      GenName.Choice := gneIPAddress;
      ParseIPAddress(edtCRLDistPointIP.Text,GenName.AsIPAddress);
    end;

    if FDistPoints[I].Reasons.BitCount < 9 then
      FDistPoints[I].Reasons.BitCount := 9;
    FDistPoints[I].Reasons.Bits[1] := clbCRLReason.Checked[0];
    FDistPoints[I].Reasons.Bits[2] := clbCRLReason.Checked[1];
    FDistPoints[I].Reasons.Bits[3] := clbCRLReason.Checked[2];
    FDistPoints[I].Reasons.Bits[4] := clbCRLReason.Checked[3];
    FDistPoints[I].Reasons.Bits[5] := clbCRLReason.Checked[4];
    FDistPoints[I].Reasons.Bits[6] := clbCRLReason.Checked[5];
    FDistPoints[I].Reasons.Bits[8] := clbCRLReason.Checked[6];
    finally
      FDistPoints.EndUpdate;
    end;
  end;
end;

procedure TfrmNewCertDlg.btnAddCRLDistPointClick(Sender: TObject);
begin
  FDistPoints.Add.DistributionPoint.Choice := dpneFullName;
  tabCRLDistributionPointsShow(Sender);
  lbCRLDistPoints.ItemIndex := lbCRLDistPoints.Items.Count - 1;
  lbCRLDistPointsClick(Sender);
end;

procedure TfrmNewCertDlg.btnDeleteCRLDistPointClick(Sender: TObject);
begin
  FDistPoints.Delete(lbCRLDistPoints.ItemIndex);
  tabCRLDistributionPointsShow(Sender);
  lbCRLDistPoints.ItemIndex := lbCRLDistPoints.Items.Count - 1;
  lbCRLDistPointsClick(Sender);
end;

procedure TfrmNewCertDlg.tabNameConstraintsShow(Sender: TObject);
var
  I: Integer;
  GenName: TGeneralName;
begin
  if (FCANC.PermittedSubtrees.Count > 0) or
     (FCANC.ExcludedSubtrees.Count > 0) then
    FCANC.Compress(FNameConstraints);
  lbPermittedSubtrees.Clear;
  for I := 0 to FNameConstraints.PermittedSubtrees.Count - 1 do begin
    GenName := FNameConstraints.PermittedSubtrees[I].Base;
    if GenName.Choice = gneUniformResourceIdentifier then
      lbPermittedSubtrees.Items.AddObject(GenName.AsUniformResourceIdentifier,GenName)
    else if GenName.Choice = gneRfc822Name then
      lbPermittedSubtrees.Items.AddObject(GenName.AsRfc822Name,GenName)
    else if GenName.Choice = gneDNSName then
      lbPermittedSubtrees.Items.AddObject(GenName.AsDNSName,GenName)
    else if GenName.Choice = gneIPAddress then
      lbPermittedSubtrees.Items.AddObject(FormatIPAddress(GenName.AsIPAddress),GenName)
    else
      lbPermittedSubtrees.Items.AddObject(Format('Subtree #%d',[I]),GenName);
  end;
  lbExcludedSubtrees.Clear;
  for I := 0 to FNameConstraints.ExcludedSubtrees.Count - 1 do begin
    GenName := FNameConstraints.ExcludedSubtrees[I].Base;
    if GenName.Choice = gneUniformResourceIdentifier then
      lbExcludedSubtrees.Items.AddObject(GenName.AsUniformResourceIdentifier,GenName)
    else if GenName.Choice = gneRfc822Name then
      lbExcludedSubtrees.Items.AddObject(GenName.AsRfc822Name,GenName)
    else if GenName.Choice = gneDNSName then
      lbExcludedSubtrees.Items.AddObject(GenName.AsDNSName,GenName)
    else if GenName.Choice = gneIPAddress then
      lbExcludedSubtrees.Items.AddObject(FormatIPAddress(GenName.AsIPAddress),GenName)
    else
      lbExcludedSubtrees.Items.AddObject(Format('Subtree #%d',[I]),GenName);
  end;
  edtSubTreeId.Enabled := False;
  rgSubTreeType.Enabled := False;
end;

procedure TfrmNewCertDlg.lbPermittedSubTreesClick(Sender: TObject);
var
  I: Integer;
  GenName: TGeneralName;
begin
  I := lbPermittedSubtrees.ItemIndex;
  if I >= 0 then begin
    lbExcludedSubtrees.ItemIndex := -1;
    GenName := FNameConstraints.PermittedSubtrees[I].Base;
    if GenName.Choice = gneUniformResourceIdentifier then begin
      edtSubTreeId.Text := GenName.AsUniformResourceIdentifier;
      rgSubTreeType.ItemIndex := 0;
    end else if GenName.Choice = gneRfc822Name then begin
      edtSubTreeId.Text := GenName.AsRfc822Name;
      rgSubTreeType.ItemIndex := 1;
    end else if GenName.Choice = gneDNSName then begin
      edtSubTreeId.Text := GenName.AsDNSName;
      rgSubTreeType.ItemIndex := 2;
    end else if GenName.Choice = gneIPAddress then begin
      edtSubTreeId.Text := FormatIPAddress(GenName.AsIPAddress);
      rgSubTreeType.ItemIndex := 3;
    end else begin
      edtSubTreeId.Text := '(Unsupported type)';
      rgSubTreeType.ItemIndex := -1;
    end;
    edtSubTreeId.Enabled := True;
    rgSubTreeType.Enabled := True;
  end else begin
    edtSubTreeId.Enabled := False;
    rgSubTreeType.Enabled := False;
  end;
end;

procedure TfrmNewCertDlg.lbExcludedSubTreesClick(Sender: TObject);
var
  I: Integer;
  GenName: TGeneralName;
begin
  I := lbExcludedSubtrees.ItemIndex;
  if I >= 0 then begin
    lbPermittedSubtrees.ItemIndex := -1;
    GenName := FNameConstraints.ExcludedSubtrees[I].Base;
    if GenName.Choice = gneUniformResourceIdentifier then begin
      edtSubTreeId.Text := GenName.AsUniformResourceIdentifier;
      rgSubTreeType.ItemIndex := 0;
    end else if GenName.Choice = gneRfc822Name then begin
      edtSubTreeId.Text := GenName.AsRfc822Name;
      rgSubTreeType.ItemIndex := 1;
    end else if GenName.Choice = gneDNSName then begin
      edtSubTreeId.Text := GenName.AsDNSName;
      rgSubTreeType.ItemIndex := 2;
    end else if GenName.Choice = gneIPAddress then begin
      edtSubTreeId.Text := FormatIPAddress(GenName.AsIPAddress);
      rgSubTreeType.ItemIndex := 3;
    end else begin
      edtSubTreeId.Text := '(Unsupported type)';
      rgSubTreeType.ItemIndex := -1;
    end;
    edtSubTreeId.Enabled := True;
    rgSubTreeType.Enabled := True;
  end else begin
    edtSubTreeId.Enabled := False;
    rgSubTreeType.Enabled := False;
  end;
end;

procedure TfrmNewCertDlg.btnSubTreeUpdateClick(Sender: TObject);
var
  I: Integer;
  Subtrees: TGeneralsubtrees;
  GenName: TGeneralName;
  GNE: TGeneralNameEnum;
begin
  case rgSubTreeType.ItemIndex of
    0: GNE := gneUniformResourceIdentifier;
    1: GNE := gneRfc822Name;
    2: GNE := gneDNSName;
    3: GNE := gneIPAddress;
  else
    raise Exception.Create('Please set type');
  end;
  I := lbPermittedSubtrees.ItemIndex;
  if I >= 0 then
    Subtrees := FNameConstraints.PermittedSubtrees
  else begin
    I := lbExcludedSubtrees.ItemIndex;
    if I >= 0 then
      Subtrees := FNameConstraints.ExcludedSubtrees
    else
      Subtrees := nil;
  end;
  if Assigned(SubTrees) then begin
    GenName := SubTrees[I].Base;
    GenName.Choice := GNE;
    case GNE of
      gneUniformResourceIdentifier:
                       GenName.AsUniformResourceIdentifier := edtSubTreeId.Text;
      gneRfc822Name:   GenName.AsRfc822Name := edtSubTreeId.Text;
      gneDNSName:      GenName.AsDNSName := edtSubTreeId.Text;
      gneIPAddress:    ParseIPAddress(edtSubTreeId.Text,GenName.AsIPAddress);
    end;
  end;
  tabNameConstraintsShow(Sender);
end;

procedure TfrmNewCertDlg.btnSubTreeDeleteClick(Sender: TObject);
var
  I: Integer;
  Subtrees: TGeneralsubtrees;
begin
  I := lbPermittedSubtrees.ItemIndex;
  if I >= 0 then
    Subtrees := FNameConstraints.PermittedSubtrees
  else begin
    I := lbExcludedSubtrees.ItemIndex;
    if I >= 0 then
      Subtrees := FNameConstraints.ExcludedSubtrees
    else
      SubTrees := nil;
  end;
  if Assigned(SubTrees) then begin
    SubTrees.Delete(I);
    tabNameConstraintsShow(Sender);
    lbPermittedSubtrees.ItemIndex := -1;
    lbExcludedSubtrees.ItemIndex := -1;
    lbPermittedSubTreesClick(Sender);
  end;
end;

procedure TfrmNewCertDlg.btnExclSubTreeAddClick(Sender: TObject);
begin
  FNameConstraints.ExcludedSubtrees.Add;
  tabNameConstraintsShow(Sender);
  lbExcludedSubtrees.ItemIndex := lbExcludedSubTrees.Items.Count - 1;
  lbExcludedSubTreesClick(Sender);
end;

procedure TfrmNewCertDlg.btnPermSubTreeAddClick(Sender: TObject);
begin
  FNameConstraints.PermittedSubtrees.Add;
  tabNameConstraintsShow(Sender);
  lbPermittedSubtrees.ItemIndex := lbPermittedSubTrees.Items.Count - 1;
  lbPermittedSubTreesClick(Sender);
end;

procedure TfrmNewCertDlg.btnOKClick(Sender: TObject);
var
  Cert: TCertificate;
  P10: TCertificationRequest;
  Status: TCertStatusCode;
  Ext: TExtension;
  Serial: string;
  I: Integer;
  ExportDlg: TfrmExportDlg;
begin
  if rgCreate.ItemIndex <> 2 then begin
    Cert := TCertificate.Create(nil,nil);
    try
      Cert.Data.ReadOnly := False;

      // Subject
      Cert.Subject.CommonName := edtCommonName.Text;
      Cert.Subject.Surname := edtSurname.Text;
      Cert.Subject.GivenName := edtGivenName.Text;
      Cert.Subject.Initials := edtInitials.Text;
      Cert.Subject.GenerationQualifier := edtGenerationQualifier.Text;
      Cert.Subject.Country := edtCountryName.Text;
      Cert.Subject.LocalityName := edtLocality.Text;
      Cert.Subject.StateOrProvinceName := edtStateOrProvince.Text;
      Cert.Subject.OrganizationName := edtOrganization.Text;
      Cert.Subject.OrganizationalUnitName := edtOrganizationalUnit.Text;
      Cert.Subject.DnQualifier := edtDNQualifier.Text;

      // Validity
      Cert.Validity.NotBefore.Choice := teUTCTime;
      Cert.Validity.NotBefore.AsUtcTime.AsDateTime := Trunc(StrToDateTime(meNotBefore.Text));
      Cert.Validity.NotAfter.Choice := teUTCTime;
      Cert.Validity.NotAfter.AsUtcTime.AsDateTime := Trunc(StrToDateTime(meNotAfter.Text));

      // Subject Alt Name
      if edtRFC822Name.Text <> '' then begin
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectAltName);
        with Ext.ExtnValue.AsCe_SubjectAltName.Add do begin
          Choice := gneRfc822Name;
          AsRfc822Name := edtRFC822Name.Text;
        end;
      end;
      if edtDNSName.Text <> '' then begin
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectAltName);
        with Ext.ExtnValue.AsCe_SubjectAltName.Add do begin
          Choice := gneDNSName;
          AsDNSName := edtDNSName.Text;
        end;
      end;
      if edtURI.Text <> '' then begin
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectAltName);
        with Ext.ExtnValue.AsCe_SubjectAltName.Add do begin
          Choice := gneUniformResourceIdentifier;
          AsUniformResourceIdentifier := edtURI.Text;
        end;
      end;
      if edtIPAddress.Text <> '' then begin
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectAltName);
        with Ext.ExtnValue.AsCe_SubjectAltName.Add do begin
          Choice := gneIPAddress;
          ParseIPAddress(edtIPAddress.Text,AsIPAddress);
        end;
      end;

      // Key Usage
      Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeKeyUsage);
      Ext.ExtnValue.AsCe_KeyUsage.BitCount := 9;
      for I := 0 to 8 do
        Ext.ExtnValue.AsCe_KeyUsage[I] := clbKeyUsage.Checked[I];

      // Extended Key Usage
      if clbExtKeyUsage.Checked[0] or
         clbExtKeyUsage.Checked[1] or
         clbExtKeyUsage.Checked[2] or
         clbExtKeyUsage.Checked[3] or
         clbExtKeyUsage.Checked[4] then begin
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeExtKeyUsage);
        if clbExtKeyUsage.Checked[0] then
          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_serverAuth);
        if clbExtKeyUsage.Checked[1] then
          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_clientAuth);
        if clbExtKeyUsage.Checked[2] then
          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_codeSigning);
        if clbExtKeyUsage.Checked[3] then
          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_emailProtection);
        if clbExtKeyUsage.Checked[4] then
          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_timeStamping);
      end;

      // Basic Constraints
      Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeBasicConstraints);
      if FPages.IndexOf(tabBasicConstraints) >= 0 then begin
        Ext.ExtnValue.AsCe_BasicConstraints.CA := True;
        if edtPathLenConstraint.Text <> '' then
          Ext.ExtnValue.AsCe_BasicConstraints.PathLenConstraint.Value :=
            edtPathLenConstraint.Text;
      end else
        Ext.ExtnValue.AsCe_BasicConstraints.CA := False;

      // CRL Distribution Points
      if (FPages.IndexOf(tabCRLDistributionPoints) >= 0) and
         (FDistPoints.Count > 0) then begin
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeCRLDistributionPoints);
        Ext.ExtnValue.AsCe_CRLDistributionPoints.Assign(FDistPoints);
      end;

      // Name Constraints
      if (FPages.IndexOf(tabNameConstraints) >= 0) and
         ((FNameConstraints.PermittedSubtrees.Count > 0) or
          (FNameConstraints.ExcludedSubtrees.Count > 0)) then begin
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeNameConstraints);
        Ext.ExtnValue.AsCe_NameConstraints.Assign(FNameConstraints);
      end;

      // Policy Constraints
      if (FPages.IndexOf(tabPolicyConstraints) >= 0) and
         ((edtIPM.Text <> '') or
          (edtREP.Text <> '')) then begin
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCePolicyConstraints);
        Ext.ExtnValue.AsCe_PolicyConstraints.RequireExplicitPolicy.Value := edtREP.Text;
        Ext.ExtnValue.AsCe_PolicyConstraints.InhibitPolicyMapping.Value := edtIPM.Text;
      end;

      // Policy Mappings
      if (FPages.IndexOf(tabPolicyMappings) >= 0) and
         (memPolicyMappings.Lines.Names[0] <> '') then begin
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCePolicyMappings);
        for I := 0 to memPolicyMappings.Lines.Count - 1 do
          if memPolicyMappings.Lines.Names[I] <> '' then
            with Ext.ExtnValue.AsCe_PolicyMappings.Add do begin
              SubjectDomainPolicy := memPolicyMappings.Lines.Names[I];
              IssuerDomainPolicy := memPolicyMappings.Lines.Values[SubjectDomainPolicy];
            end;
      end;

      // Certificate Policies
      if (FPages.IndexOf(tabCertificatePolicies) >= 0) and
         (lbCertificatePolicies.Items.Count > 0) and
         (lbCertificatePolicies.Items[0] <> '') then begin
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeCertificatePolicies);
        Ext.ExtnValue.AsCe_CertificatePolicies.Assign(FCertificatePolicies);
      end;

      Cert.Version := 3;

      // Subject Public Key (incl. Subject Key Identifier)
      if rgCreate.ItemIndex <> 3 then begin
        case cbSignatureAlgorithm.ItemIndex of
          0: PrivateKeyRing.DefaultHashAlgorithm := haSHA1;
          1: PrivateKeyRing.DefaultHashAlgorithm := haMD5;
          2: PrivateKeyRing.DefaultHashAlgorithm := haSHA256;
          3: PrivateKeyRing.DefaultHashAlgorithm := haSHA384;
          4: PrivateKeyRing.DefaultHashAlgorithm := haSHA512;
        end;
        if cbSPKIAlgorithm.ItemIndex < 3 then begin
          try
            Assert(PrivateKeyRing.CreateKeyPair(Cert,
                                                GetRegistredOID(cbSPKIAlgorithm.Text),
                                                StrToInt(cbKeySize.Text)),'Failure creating key')
          except
            on E: Exception do begin
              ShowMessage(E.Message);
              Abort;
            end;
          end;
        end else if cbKeySize.Text = '192' then
          Assert(PrivateKeyRing.CreateKeyPair(Cert,
                                              GetRegistredOID(cbSPKIAlgorithm.Text),
                                              -1,
                                              prime192v1))
        else if cbKeySize.Text = '224' then
          Assert(PrivateKeyRing.CreateKeyPair(Cert,
                                              GetRegistredOID(cbSPKIAlgorithm.Text),
                                              -1,
                                              prime224))
        else if cbKeySize.Text = '256' then
          Assert(PrivateKeyRing.CreateKeyPair(Cert,
                                              GetRegistredOID(cbSPKIAlgorithm.Text),
                                              -1,
                                              prime256v1))
        else if cbKeySize.Text = '384' then
          Assert(PrivateKeyRing.CreateKeyPair(Cert,
                                              GetRegistredOID(cbSPKIAlgorithm.Text),
                                              -1,
                                              prime384))
        else if cbKeySize.Text = '521' then
          Assert(PrivateKeyRing.CreateKeyPair(Cert,
                                              GetRegistredOID(cbSPKIAlgorithm.Text),
                                              -1,
                                              prime521));
      end else begin
        Cert.SubjectPublicKeyInfo.AssignStruct(FNewCert.SubjectPublicKeyInfo.Data);
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
        Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString := Cert.SubjectPublicKeyInfo.PublicKeyIdentifier;
      end;

      if rgCreate.ItemIndex = 0 then
        FCACert := Cert.Data;

      // Serial
      if Assigned(IssuedCerts) then
        Serial := IssuedCerts.NextSerialNumber(FCACert)
      else
        Serial := MyCerts.NextSerialNumber(FCACert);
      Cert.SerialNumber.SetBinary(Pointer(Serial)^,Length(Serial));

      // Signature (incl. Issuer, Issuer Alt Name, Authority Key Identifier)
      Assert(PrivateKeyRing.SignCertificate(Cert,FCACert));

      if rgCreate.ItemIndex = 3 then begin
        Assert(Assigned(IssuedCerts),'No IssuedCerts component assigned');
        IssuedCerts.AddCertificate(Cert.Data,False,Status);
        if Assigned(ExportData) then
          Cert.SaveToStream(ExportData)
        else begin
          ExportDlg := TfrmExportDlg.Create(nil);
          try
            ExportDlg.memImport.Text := StrToMIME64('-----BEGIN CERTIFICATE-----',
                                                    '-----END CERTIFICATE-----',
                                                    Cert.Data.ContentAsOctetString);
            case ExportDlg.ShowModal of
              mrAbort:
                if SaveDialog2.Execute then
                  Cert.SaveToFile(SaveDialog2.FileName);
            end;
          finally
            ExportDlg.Free;
          end;
        end;
      end else
        MyCerts.AddCertificate(Cert.Data,True,Status);

      Assert(Status = crcOK);
    finally
      Cert.Free;
    end;
  end else begin
    P10 := TCertificationRequest.Create(nil,nil);
    try
      P10.Data.ReadOnly := False;

      // Subject
      P10.Subject.CommonName := edtCommonName.Text;
      P10.Subject.Surname := edtSurname.Text;
      P10.Subject.GivenName := edtGivenName.Text;
      P10.Subject.Initials := edtInitials.Text;
      P10.Subject.GenerationQualifier := edtGenerationQualifier.Text;
      P10.Subject.Country := edtCountryName.Text;
      P10.Subject.LocalityName := edtLocality.Text;
      P10.Subject.StateOrProvinceName := edtStateOrProvince.Text;
      P10.Subject.OrganizationName := edtOrganization.Text;
      P10.Subject.OrganizationalUnitName := edtOrganizationalUnit.Text;
      P10.Subject.DnQualifier := edtDNQualifier.Text;


      // Subject Alt Name
      if edtRFC822Name.Text <> '' then begin
        Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeSubjectAltName);
        with Ext.ExtnValue.AsCe_SubjectAltName.Add do begin
          Choice := gneRfc822Name;
          AsRfc822Name := edtRFC822Name.Text;
        end;
      end;
      if edtDNSName.Text <> '' then begin
        Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeSubjectAltName);
        with Ext.ExtnValue.AsCe_SubjectAltName.Add do begin
          Choice := gneDNSName;
          AsDNSName := edtDNSName.Text;
        end;
      end;
      if edtURI.Text <> '' then begin
        Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeSubjectAltName);
        with Ext.ExtnValue.AsCe_SubjectAltName.Add do begin
          Choice := gneUniformResourceIdentifier;
          AsUniformResourceIdentifier := edtURI.Text;
        end;
      end;
      if edtIPAddress.Text <> '' then begin
        Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeSubjectAltName);
        with Ext.ExtnValue.AsCe_SubjectAltName.Add do begin
          Choice := gneIPAddress;
          ParseIPAddress(edtIPAddress.Text,AsIPAddress);
        end;
      end;

      // Key Usage
      Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeKeyUsage);
      Ext.ExtnValue.AsCe_KeyUsage.BitCount := 9;
      for I := 0 to 8 do
        Ext.ExtnValue.AsCe_KeyUsage[I] := clbKeyUsage.Checked[I];

      // Extended Key Usage
      if clbExtKeyUsage.Checked[0] or
         clbExtKeyUsage.Checked[1] or
         clbExtKeyUsage.Checked[2] or
         clbExtKeyUsage.Checked[3] or
         clbExtKeyUsage.Checked[4] then begin
        Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeExtKeyUsage);
        if clbExtKeyUsage.Checked[0] then
          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_serverAuth);
        if clbExtKeyUsage.Checked[1] then
          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_clientAuth);
        if clbExtKeyUsage.Checked[2] then
          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_codeSigning);
        if clbExtKeyUsage.Checked[3] then
          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_emailProtection);
        if clbExtKeyUsage.Checked[4] then
          Ext.ExtnValue.AsCe_ExtKeyUsage.AddValue(id_kp_timeStamping);
      end;

      // Basic Constraints
      Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeBasicConstraints);
      if FPages.IndexOf(tabBasicConstraints) >= 0 then begin
        Ext.ExtnValue.AsCe_BasicConstraints.CA := True;
        Ext.ExtnValue.AsCe_BasicConstraints.PathLenConstraint.Value :=
          edtPathLenConstraint.Text;
      end else
        Ext.ExtnValue.AsCe_BasicConstraints.CA := False;

      // CRL Distribution Points
      if (FPages.IndexOf(tabCRLDistributionPoints) >= 0) and
         (FDistPoints.Count > 0) then begin
        Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeCRLDistributionPoints);
        Ext.ExtnValue.AsCe_CRLDistributionPoints.Assign(FDistPoints);
      end;

      // Name Constraints
      if (FPages.IndexOf(tabNameConstraints) >= 0) and
         ((FNameConstraints.PermittedSubtrees.Count > 0) or
          (FNameConstraints.ExcludedSubtrees.Count > 0)) then begin
        Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeNameConstraints);
        Ext.ExtnValue.AsCe_NameConstraints.Assign(FNameConstraints);
      end;

      // Policy Constraints
      if (FPages.IndexOf(tabPolicyConstraints) >= 0) and
         ((edtIPM.Text <> '') or
          (edtREP.Text <> '')) then begin
        Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCePolicyConstraints);
        Ext.ExtnValue.AsCe_PolicyConstraints.RequireExplicitPolicy.Value := edtREP.Text;
        Ext.ExtnValue.AsCe_PolicyConstraints.InhibitPolicyMapping.Value := edtIPM.Text;
      end;

      // Policy Mappings
      if (FPages.IndexOf(tabPolicyMappings) >= 0) and
         (memPolicyMappings.Lines.Names[0] <> '') then begin
        Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCePolicyMappings);
        for I := 0 to memPolicyMappings.Lines.Count - 1 do
          if memPolicyMappings.Lines.Names[I] <> '' then
            with Ext.ExtnValue.AsCe_PolicyMappings.Add do begin
              SubjectDomainPolicy := memPolicyMappings.Lines.Names[I];
              IssuerDomainPolicy := memPolicyMappings.Lines.Values[SubjectDomainPolicy];
            end;
      end;

      // Certificate Policies                
      if (FPages.IndexOf(tabCertificatePolicies) >= 0) and
         (lbCertificatePolicies.Items.Count > 0) and
         (lbCertificatePolicies.Items[0] <> '') then begin
        Ext := P10.CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeCertificatePolicies);
        Ext.ExtnValue.AsCe_CertificatePolicies.Assign(FCertificatePolicies);
      end;

      P10.CertificationRequestInfo.Version.AsInteger := 3 - 1;

      // Subject Public Key (incl. Subject Key Identifier)
      if cbSPKIAlgorithm.ItemIndex < 3 then
        Assert(PrivateKeyRing.CreateKeyPair(P10,
                                            GetRegistredOID(cbSPKIAlgorithm.Text),
                                            StrToInt(cbKeySize.Text)))
      else
        Assert(PrivateKeyRing.CreateKeyPair(P10,
                                            GetRegistredOID(cbSPKIAlgorithm.Text),
                                            -1,
                                            GetRegistredOID(cbKeySize.Text)));

      // Sign Certificate Request
      case cbSignatureAlgorithm.ItemIndex of
        0: Assert(PrivateKeyRing.SignSigned(P10,P10,haSHA1));
        1: Assert(PrivateKeyRing.SignSigned(P10,P10,haMD5));
        2: Assert(PrivateKeyRing.SignSigned(P10,P10,haSHA256));
        3: Assert(PrivateKeyRing.SignSigned(P10,P10,haSHA384));
        4: Assert(PrivateKeyRing.SignSigned(P10,P10,haSHA512));
      end;


      if Assigned(ExportData) then
        P10.SaveToStream(ExportData)
      else begin
        ExportDlg := TfrmExportDlg.Create(nil);
        try
          ExportDlg.memImport.Text := StrToMIME64('-----BEGIN CERTIFICATE REQUEST-----',
                                                  '-----END CERTIFICATE REQUEST-----',
                                                  P10.Data.ContentAsOctetString);
          case ExportDlg.ShowModal of
            mrAbort:
              if SaveDialog1.Execute then
                P10.SaveToFile(SaveDialog1.FileName);
          end;
        finally
          ExportDlg.Free;
        end;
      end;
    finally
      P10.Free;
    end;
  end;
  if not (fsModal in FormState) then
    Close;
end;

procedure TfrmNewCertDlg.btnAddPolicyClick(Sender: TObject);
begin
  FCertificatePolicies.Add;
  tabCertificatePoliciesShow(Sender);
  lbCertificatePolicies.ItemIndex := lbCertificatePolicies.Items.Count - 1;
  lbCertificatePoliciesClick(Sender);
end;

function TfrmNewCertDlg.CheckName: Boolean;
var
  RDName: TName;
begin
  if (FCANC.ExcludedSubtrees.Count = 0) and
     (FCANC.PermittedSubtrees.Count = 0) then begin
    Result := True;
    Exit;
  end;
  RDName := TName.Create(nil,nil);
  try
    RDName.CommonName := edtCommonName.Text;
    RDName.Surname := edtSurname.Text;
    RDName.GivenName := edtGivenName.Text;
    RDName.Initials := edtInitials.Text;
    RDName.GenerationQualifier := edtGenerationQualifier.Text;
    RDName.Country := edtCountryName.Text;
    RDName.LocalityName := edtLocality.Text;
    RDName.StateOrProvinceName := edtStateOrProvince.Text;
    RDName.OrganizationName := edtOrganization.Text;
    RDName.OrganizationalUnitName := edtOrganizationalUnit.Text;
    RDName.DnQualifier := edtDNQualifier.Text;

    Result := FCANC.Verify(RDName,nil);
  finally
    RDName.Free;
  end;
end;

function TfrmNewCertDlg.CheckAltName: Boolean;
var
  RDName: TName;
  AltName: TGeneralNames;
begin
  if (FCANC.ExcludedSubtrees.Count = 0) and
     (FCANC.PermittedSubtrees.Count = 0) then begin
    Result := True;
    Exit;
  end;
  RDName := TName.Create(nil,nil);
  try
    RDName.CommonName := edtCommonName.Text;
    RDName.Surname := edtSurname.Text;
    RDName.GivenName := edtGivenName.Text;
    RDName.Initials := edtInitials.Text;
    RDName.GenerationQualifier := edtGenerationQualifier.Text;
    RDName.Country := edtCountryName.Text;
    RDName.LocalityName := edtLocality.Text;
    RDName.StateOrProvinceName := edtStateOrProvince.Text;
    RDName.OrganizationName := edtOrganization.Text;
    RDName.OrganizationalUnitName := edtOrganizationalUnit.Text;
    RDName.DnQualifier := edtDNQualifier.Text;

    AltName := TGeneralNames.Create(nil,nil);
    try
      if edtRFC822Name.Text <> '' then begin
        with AltName.Add do begin
          Choice := gneRfc822Name;
          AsRfc822Name := edtRFC822Name.Text;
        end;
      end;
      if edtDNSName.Text <> '' then begin
        with AltName.Add do begin
          Choice := gneDNSName;
          AsDNSName := edtDNSName.Text;
        end;
      end;
      if edtURI.Text <> '' then begin
        with AltName.Add do begin
          Choice := gneUniformResourceIdentifier;
          AsUniformResourceIdentifier := edtURI.Text;
        end;
      end;
      if edtIPAddress.Text <> '' then begin
        with AltName.Add do begin
          Choice := gneIPAddress;
          ParseIPAddress(edtIPAddress.Text,AsIPAddress);
        end;
      end;
      Result := FCANC.Verify(RDName,AltName);
    finally
      AltName.Free;
    end;
  finally
    RDName.Free;
  end;
end;

function TfrmNewCertDlg.LoadCRM(const FileName: string): Boolean;
var
  FileExt: string;
  FS: TFileStream;
begin
  FileExt := LowerCase(ExtractFileExt(FileName));
  if FileExt = '.p10' then begin
    FS := TFileStream.Create(FileName,fmOpenRead);
    try
      Result := LoadCRMStream(FS,crmPKCS10);
    finally
      FS.Free;
    end;
  end else if FileExt = '.crm' then begin
    FS := TFileStream.Create(FileName,fmOpenRead);
    try
      Result := LoadCRMStream(FS,crmCRMF);
    finally
      FS.Free;
    end;
  end else if (FileExt = '.cer') or (FileExt = '.crt') then begin
    FS := TFileStream.Create(FileName,fmOpenRead);
    try
      Result := LoadCRMStream(FS,crmCert);
    finally
      FS.Free;
    end;
  end else
    Result := False;
end;

procedure TfrmNewCertDlg.tabCertificatePoliciesShow(Sender: TObject);
var
  I: Integer;
begin
  lbCertificatePolicies.Clear;
  for I := 0 to FCertificatePolicies.Count - 1 do
    lbCertificatePolicies.Items.Add(FCertificatePolicies[I].PolicyIdentifier);
  cbPolicy.Enabled := False;
  cbPolicy.Text := '';
  edtCPS.Enabled := False;
  edtCPS.Text := '';
end;

procedure TfrmNewCertDlg.lbCertificatePoliciesClick(Sender: TObject);
var
  I: Integer;
begin
  I := lbCertificatePolicies.ItemIndex;
  if I < 0 then begin
    cbPolicy.Enabled := False;
    cbPolicy.Text := '';
    edtCPS.Enabled := False;
    edtCPS.Text := '';
  end else begin
    cbPolicy.Enabled := True;
    cbPolicy.Text := '';
    edtCPS.Enabled := True;
    if FCertificatePolicies[I].PolicyQualifiers.Count = 0 then
      edtCPS.Text := ''
    else
      edtCPS.Text := FCertificatePolicies[I].PolicyQualifiers.Items[0].Qualifier.AsQt_Cps;
  end;
end;

procedure TfrmNewCertDlg.Button3Click(Sender: TObject);
var
  I: Integer;
  PQI: TPolicyQualifierInfo;
begin
  I := lbCertificatePolicies.ItemIndex;
  if I >= 0 then begin
    FCertificatePolicies.BeginUpdate;
    try
      FCertificatePolicies[I].PolicyIdentifier := cbPolicy.Text;
      if edtCPS.Text = '' then
        FCertificatePolicies[I].PolicyQualifiers.Clear
      else begin
        PQI := FCertificatePolicies[I].PolicyQualifiers.AddUniqueItem(qeIdQtCps);
        PQI.PolicyQualifierId := id_qt_cps;
        PQI.Qualifier.Choice := qeIdQtCps;
        PQI.Qualifier.AsQt_Cps := edtCPS.Text;
      end;
    finally
      FDistPoints.EndUpdate;
    end;
  end;
end;

procedure TfrmNewCertDlg.btnDeletePolicyClick(Sender: TObject);
begin
  FCertificatePolicies.Delete(lbCertificatePolicies.ItemIndex);
  tabCertificatePoliciesShow(Sender);
  lbCertificatePolicies.ItemIndex := lbCertificatePolicies.Items.Count - 1;
  lbCertificatePoliciesClick(Sender);
end;

procedure TfrmNewCertDlg.tabFinishedShow(Sender: TObject);
const
  Note = #13#10#13#10 +
         'NOTE: Adding keys to the private key ring requires'#13#10 +
         'administrator access. If no administrator key is'#13#10 +
         'found in the key ring a 2048 bit RSA key will be'#13#10 +
         'generated, in addition to the certificate key.'#13#10 +
         'This may take a couple of seconds.';
begin
  if rgCreate.ItemIndex in [0,1] then
    lblFinishedMsg.Caption := 'Press OK to generate the certificate.'#13#10 +
                              'It will be added to your Personal certificate store.' +
                              Note
  else if rgCreate.ItemIndex = 2 then
    lblFinishedMsg.Caption := 'Press OK to generate the certificate request.'#13#10 +
                              'You will be prompted to save it to a file once it has been generated.' +
                              Note
  else if rgCreate.ItemIndex = 3 then
    lblFinishedMsg.Caption := 'Press OK to generate the certificate.'#13#10 +
                              'It will be added to your Issued certificate store.'#13#10 +
                              'You may also save it to a file.';
end;

function TfrmNewCertDlg.LoadCRMStream(Stream: TStream;
  AKind: TCRMKind): Boolean;
var
  Cert: TCertificate;
  P10: TCertificationRequest;
  CRM: TCertReqMsg;

  procedure ExtractExtensions(Extensions: TExtensions);
  var
    Ext: TExtension;
    I: Integer;
  begin
    if Extensions <> nil then begin
        Ext := Extensions.UniqueItem[eveIdCeSubjectAltName];
        if Assigned(Ext) then
          for I := 0 to Ext.ExtnValue.AsCe_SubjectAltName.Count - 1 do
            case Ext.ExtnValue.AsCe_SubjectAltName[I].Choice of
              gneRfc822Name:
                edtRfc822Name.Text := Ext.ExtnValue.AsCe_SubjectAltName[I].AsRfc822Name;
              gneUniformResourceIdentifier:
                edtURI.Text := Ext.ExtnValue.AsCe_SubjectAltName[I].AsUniformResourceIdentifier;
              gneDNSName:
                edtDNSName.Text := Ext.ExtnValue.AsCe_SubjectAltName[I].AsDNSName;
              gneIPAddress:
                edtIPAddress.Text := FormatIPAddress(Ext.ExtnValue.AsCe_SubjectAltName[I].AsIPAddress);
            end;

        Ext := Extensions.UniqueItem[eveIdCeKeyUsage];
        if Assigned(Ext) then
          for I := 0 to 8 do
            clbKeyUsage.Checked[I] := Ext.ExtnValue.AsCe_KeyUsage.Bits[I];

        Ext := Extensions.UniqueItem[eveIdCeExtKeyUsage];
        if Assigned(Ext) then
          for I := 0 to Ext.ExtnValue.AsCe_ExtKeyUsage.Count - 1 do
            if Ext.ExtnValue.AsCe_ExtKeyUsage.Values[I] = id_kp_serverAuth then
              clbExtKeyUsage.Checked[0] := True
            else if Ext.ExtnValue.AsCe_ExtKeyUsage.Values[I] = id_kp_clientAuth then
              clbExtKeyUsage.Checked[1] := True
            else if Ext.ExtnValue.AsCe_ExtKeyUsage.Values[I] = id_kp_codeSigning then
              clbExtKeyUsage.Checked[2] := True
            else if Ext.ExtnValue.AsCe_ExtKeyUsage.Values[I] = id_kp_emailProtection then
              clbExtKeyUsage.Checked[3] := True
            else if Ext.ExtnValue.AsCe_ExtKeyUsage.Values[I] = id_kp_timeStamping then
              clbExtKeyUsage.Checked[4] := True;

        Ext := Extensions.UniqueItem[eveIdCeBasicConstraints];
        if Assigned(Ext) then
          if Ext.ExtnValue.AsCe_BasicConstraints.CA then
            edtPathLenConstraint.Text := Ext.ExtnValue.AsCe_BasicConstraints.PathLenConstraint.Value;

        Ext := Extensions.UniqueItem[eveIdCeNameConstraints];
        if Assigned(Ext) then
          FNameConstraints.AssignStruct(Ext.ExtnValue.Data);

        Ext := Extensions.UniqueItem[eveIdCeCRLDistributionPoints];
        if Assigned(Ext) then
          FDistPoints.AssignStruct(Ext.ExtnValue.Data);

        Ext := Extensions.UniqueItem[eveIdCeCertificatePolicies];
        if Assigned(Ext) then
          FCertificatePolicies.AssignStruct(Ext.ExtnValue.Data);

        Ext := Extensions.UniqueItem[eveIdCePolicyMappings];
        if Assigned(Ext) then
          for I := 0 to Ext.ExtnValue.AsCe_PolicyMappings.Count - 1 do
            memPolicyMappings.Lines.Values[Ext.ExtnValue.AsCe_PolicyMappings[I].SubjectDomainPolicy] :=
              Ext.ExtnValue.AsCe_PolicyMappings[I].IssuerDomainPolicy;
    end;
  end;

  procedure ExtractSubject(Subject: TRdnSequence);
  begin
        edtCommonName.Text := Subject.CommonName;
        edtSurName.Text := Subject.Surname;
        edtGivenName.Text := Subject.GivenName;
        edtInitials.Text := Subject.Initials;
        edtGenerationQualifier.Text := Subject.GenerationQualifier;
        edtTitle.Text := Subject.Title;
        edtLocality.Text := Subject.LocalityName;
        edtStateOrProvince.Text := Subject.StateOrProvinceName;
        edtCountryName.Text := Subject.Country;
        edtOrganization.Text := Subject.OrganizationName;
        edtOrganizationalUnit.Text := Subject.OrganizationalUnitName;
        edtDNQualifier.Text := Subject.DnQualifier;
  end;

begin
  Result := True;
  if AKind = crmCert then begin
    Cert := TCertificate.Create(nil,nil);
    try
      Cert.LoadFromStream(Stream);
      if Cert.CheckSignature(nil) then begin
        ExtractSubject(Cert.Subject.AsRdnSequence);

        ExtractExtensions(Cert.TbsCertificate.Extensions);

        FNewCert.Free;
        FNewCert := TCertificate.Create(nil,nil);
        FNewCert.SubjectPublicKeyInfo.AssignStruct(Cert.SubjectPublicKeyInfo.Data);
      end else
        Result := False;
    finally
      Cert.Free;
    end;
  end else if AKind = crmPKCS10 then begin
    P10 := TCertificationRequest.Create(nil,nil);
    try
      P10.LoadFromStream(Stream);
      if P10.CheckSignature(nil) then begin
        ExtractSubject(P10.CertificationRequestInfo.Subject);

        ExtractExtensions(P10.CertificationRequestInfo.Extensions);

        FNewCert.Free;
        FNewCert := TCertificate.Create(nil,nil);
        FNewCert.SubjectPublicKeyInfo.AssignStruct(P10.CertificationRequestInfo.SubjectPKInfo.Data);
      end else
        Result := False;
    finally
      P10.Free;
    end;
  end else begin
    CRM := TCertReqMsg.Create(nil,nil);
    try
      CRM.LoadFromStream(Stream);
      if CRM.CheckSignature(nil) then begin
        ExtractSubject(CRM.CertReq.CertTemplate.Subject.AsRdnSequence);

        ExtractExtensions(CRM.CertReq.CertTemplate.Extensions);

        FNewCert.Free;
        FNewCert := TCertificate.Create(nil,nil);
        FNewCert.SubjectPublicKeyInfo.AssignStruct(CRM.CertReq.CertTemplate.PublicKey.Data);
      end else
        Result := False;
    finally
      CRM.Free;
    end;
  end;
end;

procedure TfrmNewCertDlg.SetIssuedCerts(
  const Value: TX509TrustedCertificates);
begin
  FIssuedCerts := Value;
end;

procedure TfrmNewCertDlg.SetMyCerts(const Value: TX509TrustedCertificates);
begin
  FMyCerts := Value;
end;

procedure TfrmNewCertDlg.SetPrivateKeyRing(
  const Value: TSsPrivateKeyRingComponent);
begin
  FPrivateKeyRing := Value;
end;

procedure TfrmNewCertDlg.SetRootCerts(
  const Value: TX509TrustedCertificates);
begin
  FRootCerts := Value;
  meNotBefore.Text := DateTimeToStr(Trunc(Now - Value.HoursOffsetFromGMT/24));
end;

procedure TfrmNewCertDlg.btnCancelClick(Sender: TObject);
begin
  if not (fsModal in FormState) then
    Close;
end;

procedure TfrmNewCertDlg.rgCreateClick(Sender: TObject);
var
  I: Integer;
  rbTmp: TRadioButton;
begin
  for I := 0 to rgWizard.ComponentCount - 1 do
    if rgWizard.Components[I] is TRadioButton then begin
      rbTmp := TRadioButton(rgWizard.Components[I]);
      if rbTmp.Caption = rgWizard.Items[1] then
        rbTmp.Enabled := rgCreate.ItemIndex < 3
      else if rbTmp.Caption = rgWizard.Items[2] then
        rbTmp.Enabled := rgCreate.ItemIndex in [1,2]
      else if rbTmp.Caption = rgWizard.Items[3] then
        rbTmp.Enabled := rgCreate.ItemIndex in [1,2]
      else if rbTmp.Caption = rgWizard.Items[4] then
        rbTmp.Enabled := rgCreate.ItemIndex in [1,2];
      if rbTmp.Checked and not rbTmp.Enabled then
        rgWizard.ItemIndex := 0;
    end;
end;

procedure TfrmNewCertDlg.PrepareBasicConstraints;
begin

end;

procedure TfrmNewCertDlg.PrepareExtKeyUsage;
var
  I: Integer;
begin
  {$IFDEF D5UP}
  if clbKeyUsage.Checked[0] or
     clbKeyUsage.Checked[2] or
     clbKeyUsage.Checked[4] then
    clbExtKeyUsage.ItemEnabled[0] := True
  else
    clbExtKeyUsage.ItemEnabled[0] := False;
  if clbKeyUsage.Checked[0] or
     clbKeyUsage.Checked[4] then
    clbExtKeyUsage.ItemEnabled[1] := True
  else
    clbExtKeyUsage.ItemEnabled[1] := False;
  if clbKeyUsage.Checked[0] then
    clbExtKeyUsage.ItemEnabled[2] := True
  else
    clbExtKeyUsage.ItemEnabled[2] := False;
  if clbKeyUsage.Checked[0] or
     clbKeyUsage.Checked[1] or
     clbKeyUsage.Checked[2] or
     clbKeyUsage.Checked[4] then
    clbExtKeyUsage.ItemEnabled[3] := True
  else
    clbExtKeyUsage.ItemEnabled[3] := False;
  if clbKeyUsage.Checked[0] or
     clbKeyUsage.Checked[1] then
    clbExtKeyUsage.ItemEnabled[4] := True
  else
    clbExtKeyUsage.ItemEnabled[4] := False;
  {$ENDIF}

  if rgWizard.ItemIndex > 0 then begin
    for I := 0 to 4 do
      clbExtKeyUsage.Checked[I] := False;
    case rgWizard.ItemIndex of
      2: clbExtKeyUsage.Checked[0] := True;
      3: clbExtKeyUsage.Checked[1] := True;
      4: clbExtKeyUsage.Checked[2] := True;
    end;
  end;
  {$IFDEF D5UP}
  for I := 0 to 4 do
    if not clbExtKeyUsage.ItemEnabled[I] then
      clbExtKeyUsage.Checked[I] := False;
  {$ENDIF}
  if (clbKeyUsage.Checked[5]) and
     (rgWizard.ItemIndex = 0) then begin
    AddPage(tabBasicConstraints);
    AddPage(tabNameConstraints);
    AddPage(tabPolicyConstraints);
    if FAllowMapping then
      AddPage(tabPolicyMappings)
    else
      RemovePage(tabPolicyMappings);
    AddPage(tabCRLDistributionPoints);
  end else begin
    if clbKeyUsage.Checked[5] then
      AddPage(tabBasicConstraints)
    else
      RemovePage(tabBasicConstraints);
    RemovePage(tabNameConstraints);
    RemovePage(tabPolicyConstraints);
    RemovePage(tabPolicyMappings);
    RemovePage(tabCRLDistributionPoints);
    if rgWizard.ItemIndex = 0 then
      AddPage(tabCertificatePolicies)
    else
      RemovePage(tabCertificatePolicies);
  end;
end;

procedure TfrmNewCertDlg.PrepareIssuer;
var
  I: Integer;
  C: TASN1Struct;
  Status: TCertStatusCode;
begin
  Issuers.Clear;
  if rgCreate.ItemIndex = 2 then
    for I := 0 to RootCerts.Count - 1 do begin
      C := RootCerts.Certs[I];
      if CheckValidity(C,RootCerts.HoursOffsetFromGMT) and
         (keyCertSign in ExtractKeyUsage(C)) then
        Issuers.AddCertificate(C,True,Status);
    end
  else
    for I := 0 to MyCerts.Count - 1 do begin
      C := MyCerts.Certs[I];
      if CheckValidity(C,MyCerts.HoursOffsetFromGMT) and
         (keyCertSign in ExtractKeyUsage(C)) then
        Issuers.AddCertificate(C,True,Status);
    end;
  if Issuers.Count > 0 then begin
    for I := 0 to Issuers.Count - 1 do
      Self.lvIssuerData(Self,lvIssuer.Items.Add);
    lvIssuer.Items[Issuers.Count - 1].Selected := True;
    FCACert := Issuers.Certs[Issuers.Count - 1];
  end else
    lvIssuer.Items.Clear;
end;

procedure TfrmNewCertDlg.PrepareKeyUsage;
begin
  if rgWizard.ItemIndex > 0 then begin
    clbKeyUsage.Checked[0] := rgWizard.ItemIndex in [2,3,4];
    clbKeyUsage.Checked[1] := rgWizard.ItemIndex in [2,3,4];
    clbKeyUsage.Checked[2] := rgWizard.ItemIndex in [2,3];
    clbKeyUsage.Checked[3] := False;
    clbKeyUsage.Checked[4] := rgWizard.ItemIndex in [2,3];
    clbKeyUsage.Checked[5] := rgWizard.ItemIndex = 1;
    clbKeyUsage.Checked[6] := rgWizard.ItemIndex = 1;
    clbKeyUsage.Checked[7] := False;
    clbKeyUsage.Checked[8] := False;
  end;
  case cbSPKIAlgorithm.ItemIndex of
    0:
      begin
        {$IFDEF D5UP}
        clbKeyUsage.ItemEnabled[0] := True;
        clbKeyUsage.ItemEnabled[1] := True;
        clbKeyUsage.ItemEnabled[2] := True;
        clbKeyUsage.ItemEnabled[3] := True;
        clbKeyUsage.ItemEnabled[4] := False;
        clbKeyUsage.ItemEnabled[5] := True;
        clbKeyUsage.ItemEnabled[6] := True;
        clbKeyUsage.ItemEnabled[7] := False;
        clbKeyUsage.ItemEnabled[8] := False;
        {$ENDIF}
        clbKeyUsage.Checked[4] := False;
        clbKeyUsage.Checked[7] := False;
        clbKeyUsage.Checked[8] := False;
      end;
    1:
      begin
        {$IFDEF D5UP}
        clbKeyUsage.ItemEnabled[0] := True;
        clbKeyUsage.ItemEnabled[1] := True;
        clbKeyUsage.ItemEnabled[2] := False;
        clbKeyUsage.ItemEnabled[3] := False;
        clbKeyUsage.ItemEnabled[4] := False;
        clbKeyUsage.ItemEnabled[5] := True;
        clbKeyUsage.ItemEnabled[6] := True;
        clbKeyUsage.ItemEnabled[7] := False;
        clbKeyUsage.ItemEnabled[8] := False;
        {$ENDIF}

        clbKeyUsage.Checked[2] := False;
        clbKeyUsage.Checked[3] := False;
        clbKeyUsage.Checked[4] := False;
        clbKeyUsage.Checked[7] := False;
        clbKeyUsage.Checked[8] := False;
      end;
    2:
      begin
        if (rgCreate.ItemIndex in [0,2]) or
           (rgWizard.ItemIndex in [1,4]) then
          raise Exception.Create('Diffie-Hellman keys cannot be present in CA Certificates, Signing Certificates or PKCS#10 Certificate Requests');
        {$IFDEF D5UP}
        clbKeyUsage.ItemEnabled[0] := False;
        clbKeyUsage.ItemEnabled[1] := False;
        clbKeyUsage.ItemEnabled[2] := False;
        clbKeyUsage.ItemEnabled[3] := False;
        clbKeyUsage.ItemEnabled[4] := False;
        clbKeyUsage.ItemEnabled[5] := False;
        clbKeyUsage.ItemEnabled[6] := False;
        clbKeyUsage.ItemEnabled[7] := True;
        clbKeyUsage.ItemEnabled[8] := True;
        {$ENDIF}

        clbKeyUsage.Checked[0] := False;
        clbKeyUsage.Checked[1] := False;
        clbKeyUsage.Checked[2] := False;
        clbKeyUsage.Checked[3] := False;
        clbKeyUsage.Checked[4] := True;
        clbKeyUsage.Checked[5] := False;
        clbKeyUsage.Checked[6] := False;
      end;
    3:
      begin
        {$IFDEF D5UP}
        clbKeyUsage.ItemEnabled[0] := True;
        clbKeyUsage.ItemEnabled[1] := True;
        clbKeyUsage.ItemEnabled[2] := False;
        clbKeyUsage.ItemEnabled[3] := False;
        clbKeyUsage.ItemEnabled[4] := True;
        clbKeyUsage.ItemEnabled[5] := True;
        clbKeyUsage.ItemEnabled[6] := True;
        clbKeyUsage.ItemEnabled[7] := True;
        clbKeyUsage.ItemEnabled[8] := True;
        {$ENDIF}

        clbKeyUsage.Checked[2] := False;
        clbKeyUsage.Checked[3] := False;
      end;
  end;
  if rgCreate.ItemIndex = 0 then begin
    {$IFDEF D5UP}
    clbKeyUsage.ItemEnabled[5] := False;
    {$ENDIF}
    clbKeyUsage.Checked[5] := True;
  end;
end;

procedure TfrmNewCertDlg.PrepareValidity;
var
  ValCA: TX509Validity;
begin
  meNotBefore.Text := DateTimeToStr(Trunc(Now - MyCerts.HoursOffsetFromGMT/24));
  if Assigned(FCACert) then begin
    ExtractValidity(FCACert,ValCA);
    if Now + 365 < ValCA.notAfter then
      meNotAfter.Text := DateTimeToStr(Trunc(Now + 365))
    else
      meNotAfter.Text := DateTimeToStr(Trunc(ValCA.notAfter));
  end;
end;

procedure TfrmNewCertDlg.AddPage(Page: TTabSheet);
begin
  if FPages.IndexOf(Page) < 0 then
    FPages.Add(Page);
end;

procedure TfrmNewCertDlg.RemovePage(Page: TTabSheet);
begin
  FPages.Remove(Page);
end;

procedure TfrmNewCertDlg.SelectWizard(Index: Integer; GenerateRequest: Boolean);
begin
  if Index > 0 then begin
    FormShow(Self);
    if GenerateRequest then
      rgCreate.ItemIndex := 2;
    rgWizard.ItemIndex := Index;
    case Index of
      1: Caption := 'New CA Certificate Dialog';
      2: Caption := 'New Server Certificate Dialog';
      3: Caption := 'New Client Certificate Dialog';
      4: Caption := 'New Code Signing Certificate Dialog';
    end;
    FWizardMode := True;
    btnNext.Click;
  end;
end;

procedure TfrmNewCertDlg.SetExportData(const Value: TStream);
begin
  FExportData := Value;
end;

procedure TfrmNewCertDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
end;

end.
