{$I ver.inc}
{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     PKIX Unit                                         }
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
{PKIX ASN.1 Module}
unit Pkix;

interface

uses
  Classes, SecUtils;

const
  id_streamsec = '1.3.6.1.4.1.13085';

  id_kivcipher = id_streamsec + '.1';

  id_cipher = id_streamsec + '.2';
  id_twofish = id_cipher + '.2';
  id_blowfish = id_cipher + '.3';

  id_ssec = id_streamsec + '.4';
  id_ssa  = id_ssec + '.1';
  id_rw = id_ssa + '.1';
  id_rwssa_pss = id_rw + '.1';
  id_nr = id_ssa + '.2';
  id_nr_with_sha1 = id_nr + '.1';
  id_ecnr = id_ssa + '.3';
  id_ecnr_with_sha1 = id_ecnr + '.1';
  id_kea  = id_ssec + '.2';
  id_ecdhStatic = id_kea + '.1';
  id_ecdhEphem = id_kea + '.2';
  id_ecdhOnePass = id_kea + '.3';
  id_ecdhHybrid1 = id_kea + '.4';
  id_ecdhHybrid2 = id_kea + '.5';
  id_ecdhHybridOnePass = id_kea + '.6';
  id_ecdhMQV2 = id_kea + '.7';
  id_ecdhMQV1 = id_kea + '.8';

  id_tls = id_streamsec + '.5';

  id_mastersecret = id_streamsec + '.6';

  id_roca = id_streamsec + '.7';
  id_roca_policy = id_roca + '.1';
  id_roca_ca_policy = id_roca_policy + '.1';
  id_roca_client_policy = id_roca_policy + '.2';
  id_roca_server_policy = id_roca_policy + '.3';
  id_roca_bin_access = id_roca + '.2';

  blowfishECB = '1.3.6.1.4.1.3029.1.1';
  blowfishCBC = '1.3.6.1.4.1.3029.1.2';
  blowfishCFB = '1.3.6.1.4.1.3029.1.3';
  blowfishOFB = '1.3.6.1.4.1.3029.1.4';
  
  id_md2                  = '1.2.840.113549.2.2';
  id_md5                  = '1.2.840.113549.2.5';
  id_sha1                 = '1.3.14.3.2.26';
  id_ripemd160            = '1.3.36.3.2.1';
  id_sha256               = '2.16.840.1.101.3.4.2.1';
  id_sha384               = '2.16.840.1.101.3.4.2.2';
  id_sha512               = '2.16.840.1.101.3.4.2.3';

  id_pkix = '1.3.6.1.5.5.7';
// arc for private certificate extensions
  id_pe         = id_pkix + '.1';
// arc for policy qualifier types
  id_qt         = id_pkix + '.2';
// arc for extended key purpose OIDS
  id_kp         = id_pkix + '.3';
// arc for access descriptors
  id_ad         = id_pkix + '.48';

// arc for Internet X.509 PKI protocols and their components
  id_pkip       = id_pkix + '.5';
// Registration Controls in CRMF
  id_regCtrl                     = id_pkip + '.1';
  id_regCtrl_regToken            = id_regCtrl + '.1';
  id_regCtrl_authenticator       = id_regCtrl + '.2';
  id_regCtrl_pkiPublicationInfo  = id_regCtrl + '.3';
  id_regCtrl_pkiArchiveOptions   = id_regCtrl + '.4';
  id_regCtrl_oldCertID           = id_regCtrl + '.5';
  id_regCtrl_protocolEncrKey     = id_regCtrl + '.6';

// policyQualifierIds for Internet policy qualifiers
//         OID for CPS qualifier
  id_qt_cps     = id_qt + '.1';
//         OID for user notice qualifier
  id_qt_unotice = id_qt + '.2';

type
  TKeyUsageMask = Word;
const
  digitalSignatureMask: TKeyUsageMask = $0080;
  nonRepudiationMask: TKeyUsageMask   = $0040;
  keyEnciphermentMask: TKeyUsageMask  = $0020;
  dataEnciphermentMask: TKeyUsageMask = $0010;
  keyAgreementMask: TKeyUsageMask     = $0008;
  keyCertSignMask: TKeyUsageMask      = $0004;
  cRLSignMask: TKeyUsageMask          = $0002;
  encipherOnlyMask: TKeyUsageMask     = $0001;
  decipherOnlyMask: TKeyUsageMask     = $8000;
type
  TKeyUsageItem = (encipherOnly,cRLSign,keyCertSign,keyAgreement,
                   dataEncipherment,keyEncipherment,nonRepudiation,
                   digitalSignature,decipherOnly);
  TKeyUsage = set of TKeyUsageItem;
const
//  digitalSignature: TKeyUsageItem = 7;
//  nonRepudiation: TKeyUsageItem   = 6;
//  keyEncipherment: TKeyUsageItem  = 5;
//  dataEncipherment: TKeyUsageItem = 4;
//  keyAgreement: TKeyUsageItem     = 3;
//  keyCertSign: TKeyUsageItem      = 2;
//  cRLSign: TKeyUsageItem          = 1;
//  encipherOnly: TKeyUsageItem     = 0;
//  decipherOnly: TKeyUsageItem     = 8;

// PKIX-defined extended key purpose OIDs
  id_kp_serverAuth      = id_kp + '.1';
  id_kp_clientAuth      = id_kp + '.2';
  id_kp_codeSigning     = id_kp + '.3';
  id_kp_emailProtection = id_kp + '.4';
  id_kp_ipsecEndSystem  = id_kp + '.5';
  id_kp_ipsecTunnel     = id_kp + '.6';
  id_kp_ipsecUser       = id_kp + '.7';
  id_kp_timeStamping    = id_kp + '.8';

  netscape                = '2.16.840.1.113730';
  netscape_cert_extension = netscape + '.1';
  netscape_cert_type      = netscape_cert_extension + '.1';
type
  TNetscapeKeyUsageItem = (nkuObjectSigningCA,nkuSMimeCA,nkuSslCA,nkuReserved,
                           nkuObjectSigning,nkuSMime,nkuSslServer,nkuSslClient);
  TNetscapeKeyUsage = set of TNetscapeKeyUsageItem;

  CRLReason = (unspecified0,keyCompromise,cACompromise,affiliationChanged,
               superseded,cessationOfOperation,certificateHold,unspecified7,
               removeFromCRL);
const
  {
  unspecified: CRLReason         = 0;
  keyCompromise: CRLReason       = 1;
  cACompromise: CRLReason        = 2;
  affiliationChanged: CRLReason  = 3;
  superseded: CRLReason          = 4;
  cessationOfOperation: CRLReason= 5;
  certificateHold: CRLReason     = 6;
  removeFromCRL: CRLReason       = 7;
  }
// ANSI x9 arc holdinstruction arc
  holdInstruction = '2.2.840.10040.2';
// ANSI X9 holdinstructions referenced by this standard
  id_holdinstruction_none       = holdInstruction + '.1';
  id_holdinstruction_callissuer = holdInstruction + '.2';
  id_holdinstruction_reject     = holdInstruction + '.3';

// Object identifier assignments for ISO certificate extensions
  id_ce                            = '2.5.29';
  id_ce_authorityKeyIdentifier_old = id_ce + '.1';
  id_ce_primary_key_usage_restriction = id_ce + '.4';
  id_ce_subjectDirectoryAttributes = id_ce + '.9';
  id_ce_subjectKeyIdentifier       = id_ce + '.14';
  id_ce_keyUsage                   = id_ce + '.15';
  id_ce_privateKeyUsagePeriod      = id_ce + '.16';
  id_ce_subjectAltName             = id_ce + '.17';
  id_ce_issuerAltName              = id_ce + '.18';
  id_ce_basicConstraints           = id_ce + '.19';
  id_ce_cRLNumber                  = id_ce + '.20';
  id_ce_reasonCode                 = id_ce + '.21';
  id_ce_cRLReason                  = id_ce_reasonCode;
  id_ce_instructionCode            = id_ce + '.23';
  id_ce_holdInstructionCode        = id_ce_instructionCode;
  id_ce_invalidityDate             = id_ce + '.24';
  id_ce_deltaCRLIndicator          = id_ce + '.27';
  id_ce_issuingDistributionPoint   = id_ce + '.28';
  id_ce_certificateIssuer          = id_ce + '.29';
  id_ce_nameConstraints            = id_ce + '.30';
  id_ce_cRLDistributionPoints      = id_ce + '.31';
  id_ce_certificatePolicies        = id_ce + '.32';
  anyPolicy                        = id_ce_certificatePolicies + '.0';
  id_ce_policyMappings             = id_ce + '.33';
  id_ce_policyConstraints          = id_ce + '.36';
  id_ce_authorityKeyIdentifier     = id_ce + '.35';
  id_ce_extKeyUsage                = id_ce + '.37';

  id_pe_authorityInfoAccess        = id_pe + '.1';

  id_ad_ocsp                       = id_ad + '.1';
  id_ad_caIssuers                  = id_ad + '.2';

  pkcs   = '1.2.840.113549.1';

  pkcs_9 = pkcs + '.9';
  {$IFDEF UNIQUENAMES}                             
  id_emailAddress                  = pkcs_9 + '.1';
  {$ELSE}
  {$NODEFINE emailAddress}
  {$HPPEMIT '#define id_emailAddress "1.2.840.113549.1.9.1"'}
  emailAddress                     = pkcs_9 + '.1';
  {$ENDIF}
  pkcs_9_at_contentType            = pkcs_9 + '.3';
  pkcs_9_at_messageDigest          = pkcs_9 + '.4';
  pkcs_9_at_signingTime            = pkcs_9 + '.5';
  pkcs_9_at_counterSignature       = pkcs_9 + '.6';
  pkcs_9_at_signingDescription     = pkcs_9 + '.13';
  pkcs_9_at_friendlyName           = pkcs_9 + '.20';
  pkcs_9_at_localKeyId             = pkcs_9 + '.21';
  pkcs_9_certTypes                 = pkcs_9 + '.22';

  id_ct                            = pkcs_9 + '.16.1';
  id_ct_scvp_psRequest             = id_ct + '.10';
  id_ct_scvp_psResponse            = id_ct + '.11';

  pkcs_7 = pkcs + '.7';                            
  id_data                          = pkcs_7 + '.1';
  id_signedData                    = pkcs_7 + '.2';
  id_envelopedData                 = pkcs_7 + '.3';
  id_signedAndEnvelopedData        = pkcs_7 + '.4';
  id_digestedData                  = pkcs_7 + '.5';
  id_encryptedData                 = pkcs_7 + '.6';
  id_ct_authData                   = id_ct + '.2';

  pkcs_12 = pkcs + '.12';
  pkcs_12PbeIds = pkcs_12 + '.1';

  id_ri = id_pkix + '.16';
  id_ri_crl                        = id_ri + '.1';
  id_ri_ocsp_response              = id_ri + '.2';

  id_stc = id_pkix + '.17';
  id_stc_build_path                = id_stc + '.1';
  id_stc_build_valid_path          = id_stc + '.2';
  id_stc_build_valid_status_checked_path
                                   = id_stc + '.3';
  id_swb = id_pkix + '.18';
  id_swb_cert_path                 = id_swb + '.1';
  id_swb_revocation_info           = id_swb + '.2';


  id_at  = '2.5.4';
// Attributes
  id_at_commonName                 = id_at + '.3';
  id_at_surname                    = id_at + '.4';
  id_at_countryName                = id_at + '.6';
  id_at_localityName               = id_at + '.7';
  id_at_stateOrProvinceName        = id_at + '.8';
  id_at_organizationName           = id_at + '.10';
  id_at_organizationalUnitName     = id_at + '.11';
  id_at_title                      = id_at + '.12';
  id_at_name                       = id_at + '.41';
  id_at_givenName                  = id_at + '.42';
  id_at_initials                   = id_at + '.43';
  id_at_generationQualifier        = id_at + '.44';
  id_at_dnQualifier                = id_at + '.46';


  pkcs_1 = pkcs + '.1';
  rsaEncryption           = pkcs_1 + '.1';
  md2WithRSAEncryption    = pkcs_1 + '.2';
  md5WithRSAEncryption    = pkcs_1 + '.4';
  sha1WithRSAEncryption   = pkcs_1 + '.5';
  sha256WithRSAEncryption = pkcs_1 + '.11';
  sha384WithRSAEncryption = pkcs_1 + '.12';
  sha512WithRSAEncryption = pkcs_1 + '.13';

  rsaOAEPEncryptionSET    = pkcs_1 + '.6';

  id_RSAES_OAEP           = pkcs_1 + '.7';
  id_mgf1                 = pkcs_1 + '.8';

  id_pSpecified           = pkcs_1 + '.9';
  id_RSASSA_PSS           = pkcs_1 + '.10';

  hMAC_SHA1               = '1.3.6.1.5.5.8.1.2.';
  
  pkcs_3 = pkcs + '.3';                   
  rc2_cbc                 = pkcs_3 + '.2';
  arcfour                 = pkcs_3 + '.4';
  DES_EDE3_CBC            = pkcs_3 + '.7';

  ansi_X9_42              = '1.2.840.10046';
  fieldtype               = ansi_X9_42 + '.0';
  gfPrime                 = fieldtype + '.0';
  algorithm               = ansi_X9_42 + '.1';
  tripleDES               = algorithm + '.2';
  numberType              = ansi_X9_42 + '.2';
  dhPublicNumber          = numberType + '.1';
  scheme                  = ansi_X9_42 + '.3';
  dhStatic                = scheme + '.1'; // Diffie-Hellman, Static Only
  dhEphem                 = scheme + '.2'; // Diffie-Hellman, Ephemeral Only
  dhOneFlow               = scheme + '.3'; // Diffie-Hellman, One Flow
  dhHybrid1               = scheme + '.4'; // Diffie-Hellman, Hybrid, One Group
  dhHybrid2               = scheme + '.5'; // Diffie-Hellman, Hybrid, Two Groups
  dhHybridOneFlow         = scheme + '.6'; // Diffie-Hellman, Hybrid, OneFlow
  mqv2                    = scheme + '.7'; // Menezes-Qu-Vanstone Method, Two pairs/Two pairs
  mqv1                    = scheme + '.8'; // Menezes-Qu-Vanstone Method, Two pairs/One pair
  namedScheme             = ansi_X9_42 + '.4';
  dhStatic_sha1           = namedScheme + '.1';
  dhEphem_sha1            = namedScheme + '.2';
  dhOneFlow_sha1          = namedScheme + '.3';
  dhHybrid1_sha1          = namedScheme + '.4';
  dhHybrid2_sha1          = namedScheme + '.5';
  dhHybridOneFlow_sha1    = namedScheme + '.6';
  mqv2_sha1               = namedScheme + '.7';
  mqv1_sha1               = namedScheme + '.8';
  keyDerivationMethod     = ansi_X9_42 + '.5';
  kdasn1der               = keyDerivationMethod + '.0';
  kdConcatenation         = keyDerivationMethod + '.1';

  ansi_X9_57              = '1.2.840.10040';
  id_dsa                  = ansi_X9_57 + '.4.1';
  id_dsa_with_sha1        = ansi_X9_57 + '.4.3';
                                               
  id_aes                  = '2.16.840.1.101.3.4.1';
  id_aes128_ECB           = id_aes + '.1';
  id_aes128_CBC           = id_aes + '.2';
  id_aes128_OFB           = id_aes + '.3';
  id_aes128_CFB           = id_aes + '.4';
  id_aes128_wrap          = id_aes + '.5';
  id_aes192_ECB           = id_aes + '.21';
  id_aes192_CBC           = id_aes + '.22';
  id_aes192_OFB           = id_aes + '.23';
  id_aes192_CFB           = id_aes + '.24';
  id_aes192_wrap          = id_aes + '.25';
  id_aes256_ECB           = id_aes + '.41';
  id_aes256_CBC           = id_aes + '.42';
  id_aes256_OFB           = id_aes + '.43';
  id_aes256_CFB           = id_aes + '.44';
  id_aes256_wrap          = id_aes + '.45';

  ansi_X9_62              = '1.2.840.10045';
  id_ecSigType            = ansi_X9_62 + '.4';
  ecdsa_with_SHA1         = id_ecSigType + '.1';

  id_fieldType            = ansi_X9_62 + '.1';
  prime_field             = id_fieldType + '.1';
   
  id_publicKeyType        = ansi_X9_62 + '.2';
  id_ecPublicKey          = id_publicKeyType + '.1';

  ansi_ellipticCurve      = ansi_X9_62 + '.3';
{$IFDEF UNIQUENAMES}                          
  id_primeCurve           = ansi_ellipticCurve + '.1';
  prime192v1              = id_primeCurve + '.1';
  prime192v2              = id_primeCurve + '.2';
  prime192v3              = id_primeCurve + '.3';
  prime239v1              = id_primeCurve + '.4';
  prime239v2              = id_primeCurve + '.5';
  prime239v3              = id_primeCurve + '.6';
  prime256v1              = id_primeCurve + '.7';
{$ELSE}                             
  {$NODEFINE primeCurve}
  {$HPPEMIT '#define id_primeCurve "1.2.840.10045.3.1"'}
  primeCurve              = ansi_ellipticCurve + '.1';
  prime192v1              = primeCurve + '.1';
  prime192v2              = primeCurve + '.2';
  prime192v3              = primeCurve + '.3';
  prime239v1              = primeCurve + '.4';
  prime239v2              = primeCurve + '.5';
  prime239v3              = primeCurve + '.6';
  prime256v1              = primeCurve + '.7';
{$ENDIF}
  certicom_arc            = '1.3.132';
  ellipticCurve           = certicom_arc + '.0';
  prime224                = ellipticCurve + '.33';
  prime384                = ellipticCurve + '.34';
  prime521                = ellipticCurve + '.35';


function GetObjectName(OID: string): string;
function HashAlgorithmToOID(HashAlgorithm: THashAlgorithm): string;
function OIDToHashAlgorithm(const OID: string; var HA: THashAlgorithm): Boolean;

procedure RegisterOID(var OID, OIDName: string; OIDDecl: string = '');
function GetRegistredOID(OIDName: string): string;
function GetRegistredOIDName(OID: string): string;
function GetOIDDeclaration(OID: string): string;
procedure GetOIDDeclarations(List: TStrings);

implementation

uses
  SysUtils, Asn1;

var
  OIDNames: TStringList;
  OIDs: TStringList;

procedure SetUp;
begin
  OIDNames := TStringList.Create;
  OIDs := TStringList.Create;
end;

procedure CleanUpOIDs(SL: TStringList);
var
  I: Integer;
begin
  for I := 0 to SL.Count - 1 do
    if Assigned(SL.Objects[I]) then
      CleanUpOIDs(TStringList(SL.Objects[I]));
  SL.Free;
end;

procedure CleanUp;
begin
  OIDNames.Free;
  CleanUpOIDs(OIDs);
end;

procedure RegisterOID(var OID, OIDName: string; OIDDecl: string = '');
const
  NumChars: set of Char = ['0','1','2','3','4','5','6','7','8','9'];
type
  IntegerAndName = record
    SubIden: Int64;
    Name: string;
  end;
var
  I, J, K: Integer;
  Id: Int64;
  SId, SName: string;
  Ids: array of IntegerAndName;
  SL, SL0: TStringList;
begin
  SL := nil;
  Trim(OIDDecl);
  if OID = '' then begin
    K := Pos('{',OIDDecl) + 1;
    repeat
      while (K < Length(OIDDecl)) and
            (OIDDecl[K] = ' ') do
        Inc(K);
      if (K <= Length(OIDDecl)) and
         not (OIDDecl[K] in NumChars) then begin
        SName := '';
        while (K < Length(OIDDecl)) and
              (OIDDecl[K] <> '(') and
              (OIDDecl[K] <> ' ') do begin
          SName := SName + OIDDecl[K];
          Inc(K);
        end;
        SId := GetRegistredOID(SName);
        if (SId = '') and (OIDDecl[K] = ' ')  then
          raise Exception.Create(Format('RegisterOID: Unknown identifier %s',[SName]));
        if SId <> ''  then begin
          SetLength(Ids,0);
          I := 1;
          repeat
            Id := ExtractSubIden(SId,I);
            SetLength(Ids,Length(Ids) + 1);
            Ids[Length(Ids) - 1].SubIden := Id;
            if I > Length(SId) then
              Ids[Length(Ids) - 1].Name := GetRegistredOIDName(SId)
            else
              Ids[Length(Ids) - 1].Name := GetRegistredOIDName(Copy(SId,1,I-2));
          until I > Length(SId);
          OID := SId;
          if (K < Length(OIDDecl)) and (OIDDecl[K] = '(') then
            while (K <= Length(OIDDecl)) and (OIDDecl[K] <> ')') do
              Inc(K);
          Inc(K);
        end;
      end else begin
        SId := '';
        SName := '';
      end;
      if (SId = '') and
         (K < Length(OIDDecl)) and
         ((OIDDecl[K] = '(') or
          (OIDDecl[K] in NumChars)) then begin
        if OIDDecl[K] = '(' then
          Inc(K);
        SId := '';
        while (K < Length(OIDDecl)) and
              (OIDDecl[K] <> ')') and
              (OIDDecl[K] <> ' ') and
              (OIDDecl[K] <> '}') do begin
          SId := SId + OIDDecl[K];
          Inc(K);
        end;
        if (K <= Length(OIDDecl)) and
           ((OIDDecl[K] = ')') or
            (OIDDecl[K] = ' ')) then begin
          SId := Trim(SId);
          Id := StrToInt64(SId);
          SetLength(Ids,Length(Ids) + 1);
          Ids[Length(Ids) - 1].SubIden := Id;
          Ids[Length(Ids) - 1].Name := SName;

          if OID = '' then
            OID := SId
          else
            OID := OID + '.' + SId;

          if OIDDecl[K] = ')' then
            Inc(K);
        end;
      end;                 
      while (K < Length(OIDDecl)) and
            (OIDDecl[K] = ' ') do
        Inc(K);
    until (K >= Length(OIDDecl)) or (OIDDecl[K] = '}');
    if OID = '' then
      raise Exception.Create('RegisterOID: Illegal declaration');
  end else begin
    I := 1;
    repeat
      Id := ExtractSubIden(OID,I);
      SetLength(Ids,Length(Ids) + 1);
      Ids[Length(Ids) - 1].SubIden := Id;
      Ids[Length(Ids) - 1].Name := '';
    until I > Length(OID);
  end;
  if OIDName = '' then begin
    I := Pos(' ::= ',OIDDecl);
    if I = 0 then
      raise Exception.Create('RegisterOID: Illegal declaration');
    J := Pos(' OBJECT ',OIDDecl);
    if J = 0 then
      raise Exception.Create('RegisterOID: Illegal declaration');
    if J < I then
      I := J;
    OIDName := Trim(Copy(OIDDecl,1,I-1));
  end;
  OIDNames.Values[OIDName] := OID;
  SL0 := OIDs;
  for I := 0 to Length(IDs) - 1 do begin            
    SL := SL0;
    Id := IDs[I].SubIden;
    SName := IDs[I].Name;
    SId := IntToStr(Id);
    J := SL.IndexOfName(SId);
    if J < 0 then begin
      SL0 := TStringList.Create;
      SL.AddObject(Format('%s=%s',[SId,SName]),SL0);
    end else
      SL0 := TStringList(SL.Objects[J]);
  end;
  SL.Values[SId] := OIDName;
end;

function GetRegistredOID(OIDName: string): string;
begin
  Result := OIDNames.Values[OIDName];
end;

procedure GetOIDDeclarations(List: TStrings);
var
  I: Integer;
  S: string;
begin
  List.Clear;
  for I := 0 to OIDNames.Count - 1 do begin
    S := OIDNames.Values[OIDNames.Names[I]];
    List.Add(GetOIDDeclaration(S));
  end;
end;

function GetRegistredOIDName(OID: string): string;
var
  Id: Int64;
  I, J: Integer;
  SId: string;
  SL: TStringList;
begin
  Result := '';
  if OID = '' then Exit;
  I := 1;
  J := -1;
  Id := 0;
  SL := OIDs;
  while I <= Length(OID) do begin
    Id := ExtractSubIden(OID,I);
    SId := IntToStr(Id);
    J := SL.IndexOfName(SId);
    if J < 0 then Break;
    Result := SL.Values[SId];
    SL := TStringList(SL.Objects[J]);
  end;
  if (I < Length(OID)) or (J < 0) then begin
    if J < 0 then begin                     
      Result := Format('%s %d',[Result,Id]);
    end;
    while I <= Length(OID) do begin
      Id := ExtractSubIden(OID,I);
      Result := Format('%s %d',[Result,Id]);
    end;
    Result := Format('{ %s }',[Result]);
  end;
end;

function GetOIDDeclaration(OID: string): string;
var
  SId: string;
  I, J: Integer;
  Id: Int64;
  SL: TStringList;
begin
  Result := '';
  I := 1;
  SL := OIDs;
  while I <= Length(OID) do begin
    Id := ExtractSubIden(OID,I);
    SId := IntToStr(Id);
    if Assigned(SL) then begin
      J := SL.IndexOfName(SId);
      if (J < 0) or (I > Length(OID)) then begin
        if J < 0 then
          SL := nil;
        Result := Format('%s %d',[Result,Id]);
      end else begin
        Result := Format('%s %s(%d)',[Result,SL.Values[SId],Id]);
        SL := TStringList(SL.Objects[J]);
      end;
    end else
      Result := Format('%s %d',[Result,Id]);
  end;
  if (Result <> '') and (SId <> '') and
     Assigned(SL) and (SL.Values[SId] <> '') then
    Result := Format('%s OBJECT IDENTIFIER ::= { %s }',[SL.Values[SId],Result])
  else
    Result := '';
end;

function HashAlgorithmToOID(HashAlgorithm: THashAlgorithm): string;
begin
  case HashAlgorithm of
{$IFDEF MD2}      haMD2:       Result := id_md2;      {$ENDIF}
{$IFDEF MD5}      haMD5:       Result := id_md5;      {$ENDIF}
{$IFDEF SHA1}     haSHA1:      Result := id_sha1;     {$ENDIF}
{$IFDEF SHA256}   haSHA256:    Result := id_sha256;   {$ENDIF}
{$IFDEF SHA512}   haSHA384:    Result := id_sha384;   {$ENDIF}
{$IFDEF SHA512}   haSHA512:    Result := id_sha512;   {$ENDIF}
{$IFDEF RIPEMD160}haRipeMD160: Result := id_RipeMD160;{$ENDIF}
  else
    Result := '';
  end;
end;       

function OIDToHashAlgorithm(const OID: string; var HA: THashAlgorithm): Boolean;
begin
  Result := True;
{$IFDEF MD2}
  if OID = id_md2 then
    HA := haMD2
  else           
{$ENDIF MD2}
{$IFDEF MD5}
  if OID = id_md5 then
    HA := haMD5
  else
{$ENDIF MD5}
{$IFDEF SHA1}
  if OID = id_sha1 then
    HA := haSHA1
  else
{$ENDIF SHA1}
{$IFDEF RIPEMD160}
  if OID = id_ripeMD160 then
    HA := haRipeMD160
  else
{$ENDIF RIPEMD160}
{$IFDEF SHA256}
  if OID = id_sha256 then
    HA := haSHA256
  else
{$ENDIF SHA256}
{$IFDEF SHA512}
  if OID = id_sha384 then
    HA := haSHA384
  else if OID = id_sha512 then
    HA := haSHA512
  else
{$ENDIF SHA512}
    Result := False;
end;

function GetObjectName(OID: string): string;
var
  P, I: Integer;
  BestMatch, BestName: string;

  function Derivate(Match, Name, OID: string): string;
  var
    I: Integer;
  begin
    if Match = '' then begin
      I := 1;
      Result := '{ ';
    end else begin
      I := Length(Match) + 2;
      Result := '{ ' + Name + ' ';
    end;
    while I <= Length(OID) do
      Result := Result + IntToStr(ExtractSubIden(OID,I)) + ' ';
    Result := Result + '}';
  end;

  function IsSubOIDOf(OID, Match: string): Boolean;
  begin
    Result := Length(Match) < Length(OID);
    if Result then
      Result := (OID[Length(Match)+1] = '.') and
                (Copy(OID,1,Length(Match)) = Match)
    else if Length(Match) = Length(OID) then
      Result := Match = OID;
  end;

begin
  Result := GetRegistredOIDName(OID);
  if (Result <> '') and (Result[1] <> '{') then
    Exit;

  BestMatch := '';
  BestName := '';
  if IsSubOIDOf(OID,id_pkix) then begin
    BestMatch := id_pkix;
    BestName := 'id-pkix';
    if Length(BestMatch) < Length(OID) then begin
      P := Length(BestMatch) + 2;
      I := ExtractSubIden(OID,P);
      BestMatch := BestMatch + '.' + IntToStr(I);
      case I of
        1: if IsSubOIDOf(OID,id_pe_authorityInfoAccess) then begin
             BestMatch := id_pe_authorityInfoAccess;
             BestName := 'id-pe-authorityInfoAccess';
           end else
             BestName := 'id-pe';
        2: if IsSubOIDOf(OID,id_qt_cps) then begin
             BestMatch := id_qt_cps;
             BestName := 'id-qt-cps';
           end else if IsSubOIDOf(OID,id_qt_unotice) then begin
             BestMatch := id_qt_cps;
             BestName := 'id-qt-cps';
           end else
             BestName := 'id-qt';
        3: begin
             BestName := 'id-kp';
             if Length(BestMatch) < Length(OID) then begin
               P := Length(BestMatch) + 2;
               I := ExtractSubIden(OID,P);
               BestMatch := BestMatch + '.' + IntToStr(I);
               case I of
                 1: BestName := 'id-kp-serverAuth';
                 2: BestName := 'id-kp-clientAuth';
                 3: BestName := 'id-kp-codeSigning';
                 4: BestName := 'id-kp-emailProtection';
                 5: BestName := 'id-kp-ipsecEndSystem';
                 6: BestName := 'id-kp-ipsecTunnel';
                 7: BestName := 'id-kp-ipsecUser';
                 8: BestName := 'id-kp-timeStamping';
               else
                 BestMatch := id_kp;
               end;
             end;
           end;
        16:begin
             BestName := 'id-ri';
             if Length(BestMatch) < Length(OID) then begin
               P := Length(BestMatch) + 2;
               I := ExtractSubIden(OID,P);
               BestMatch := BestMatch + '.' + IntToStr(I);
               case I of
                 1: BestName := 'id-ri-crl';
                 2: BestName := 'id-ri-ocsp-response';
               else
                 BestMatch := id_kp;
               end;
             end;
           end;
        17:begin
             BestName := 'id-stc';
             if Length(BestMatch) < Length(OID) then begin
               P := Length(BestMatch) + 2;
               I := ExtractSubIden(OID,P);
               BestMatch := BestMatch + '.' + IntToStr(I);
               case I of
                 1: BestName := 'id-stc-build_path';
                 2: BestName := 'id-stc-build-valid-path';
                 3: BestName := 'id-stc-build-valid-status-checked-path';
               else
                 BestMatch := id_kp;
               end;
             end;
           end;
        18:begin
             BestName := 'id-swb';
             if Length(BestMatch) < Length(OID) then begin
               P := Length(BestMatch) + 2;
               I := ExtractSubIden(OID,P);
               BestMatch := BestMatch + '.' + IntToStr(I);
               case I of
                 1: BestName := 'id-swb-cert-path';
                 2: BestName := 'id-swb-revocation-info';
               else
                 BestMatch := id_kp;
               end;
             end;
           end;
        48:if IsSubOIDOf(OID,id_ad_caIssuers) then begin
             BestMatch := id_ad_caIssuers;
             BestName := 'id-ad-caIssuers';
           end else if IsSubOIDOf(OID,id_ad_ocsp) then begin
             BestMatch := id_ad_ocsp;
             BestName := 'id-ad-ocsp';
           end else
             BestName := 'id-ad';
      else
        BestMatch := id_pkix;
      end;
    end;
  end else if IsSubOIDOf(OID,id_at) then begin
    BestMatch := id_at;
    BestName := 'id-at';
    if Length(BestMatch) < Length(OID) then begin
      P := Length(BestMatch) + 2;
      I := ExtractSubIden(OID,P);
      BestMatch := BestMatch + '.' + IntToStr(I);
      case I of
        3:  BestName := 'id-at-commonName';
        4:  BestName := 'id-at-surname';
        6:  BestName := 'id-at-countryName';
        7:  BestName := 'id-at-localityName';
        8:  BestName := 'id-at-stateOrProvinceName';
        10: BestName := 'id-at-organizationName';
        11: BestName := 'id-at-organizationalUnitName';
        12: BestName := 'id-at-title';
        41: BestName := 'id-at-name';
        42: BestName := 'id-at-givenName';
        43: BestName := 'id-at-initials';
        44: BestName := 'id-at-generationQualifier';
        46: BestName := 'id-at-dnQualifier';
      else
        BestMatch := id_at;
      end;
    end;
  end else if IsSubOIDOf(OID,holdInstruction) then begin
    BestMatch := holdInstruction;
    BestName := 'holdInstruction';
    if Length(BestMatch) < Length(OID) then begin
      P := Length(BestMatch) + 2;
      I := ExtractSubIden(OID,P);
      BestMatch := BestMatch + '.' + IntToStr(I);
      case I of
        1:  BestName := 'id-holdInstruction-none';
        2:  BestName := 'id-holdInstruction-callIssuer';
        3:  BestName := 'id-holdInstruction-reject';
      else
        BestMatch := holdInstruction;
      end;
    end;
  end else if IsSubOIDOf(OID,id_ce) then begin
    BestMatch := id_ce;
    BestName := 'id-ce';
    if IsSubOIDOf(OID,id_ce_authorityKeyIdentifier_old) then begin
      BestMatch := id_ce_authorityKeyIdentifier_old;
      BestName := 'id_ce_authorityKeyIdentifier (deprecated)';
    end else if IsSubOIDOf(OID,id_ce_primary_key_usage_restriction) then begin
      BestMatch := id_ce_primary_key_usage_restriction;
      BestName := 'id_ce_primary_key_usage_restriction (deprecated)';
    end else if IsSubOIDOf(OID,id_ce_subjectDirectoryAttributes) then begin
      BestMatch := id_ce_subjectDirectoryAttributes;
      BestName := 'id-ce-subjectDirectoryAttributes';
    end else if IsSubOIDOf(OID,id_ce_subjectKeyIdentifier) then begin
      BestMatch := id_ce_subjectKeyIdentifier;
      BestName := 'id-ce-subjectKeyIdentifier';
    end else if IsSubOIDOf(OID,id_ce_keyUsage) then begin
      BestMatch := id_ce_keyUsage;
      BestName := 'id-ce-keyUsage';
    end else if IsSubOIDOf(OID,id_ce_privateKeyUsagePeriod) then begin
      BestMatch := id_ce_privateKeyUsagePeriod;
      BestName := 'id-ce-privateKeyUsagePeriod';
    end else if IsSubOIDOf(OID,id_ce_subjectAltName) then begin
      BestMatch := id_ce_subjectAltName;
      BestName := 'id-ce-subjectAltName';
    end else if IsSubOIDOf(OID,id_ce_issuerAltName) then begin
      BestMatch := id_ce_issuerAltName;
      BestName := 'id-ce-issuerAltName';
    end else if Copy(OID,1,Length(id_ce_basicConstraints)) = id_ce_basicConstraints then begin
      BestMatch := id_ce_basicConstraints;
      BestName := 'id-ce-basicConstraints';
    end else if Copy(OID,1,Length(id_ce_cRLNumber)) = id_ce_cRLNumber then begin
      BestMatch := id_ce_cRLNumber;
      BestName := 'id-ce-cRLNumber';
    end else if Copy(OID,1,Length(id_ce_reasonCode)) = id_ce_reasonCode then begin
      BestMatch := id_ce_reasonCode;
      BestName := 'id-ce-reasonCode';
    end else if Copy(OID,1,Length(id_ce_instructionCode)) = id_ce_instructionCode then begin
      BestMatch := id_ce_instructionCode;
      BestName := 'id-ce-instructionCode';
    end else if Copy(OID,1,Length(id_ce_invalidityDate)) = id_ce_invalidityDate then begin
      BestMatch := id_ce_invalidityDate;
      BestName := 'id-ce-invalidityDate';
    end else if Copy(OID,1,Length(id_ce_deltaCRLIndicator)) = id_ce_deltaCRLIndicator then begin
      BestMatch := id_ce_deltaCRLIndicator;
      BestName := 'id-ce-deltaCRLIndicator';
    end else if Copy(OID,1,Length(id_ce_issuingDistributionPoint)) = id_ce_issuingDistributionPoint then begin
      BestMatch := id_ce_issuingDistributionPoint;
      BestName := 'id-ce-issuingDistributionPoint';
    end else if Copy(OID,1,Length(id_ce_certificateIssuer)) = id_ce_certificateIssuer then begin
      BestMatch := id_ce_certificateIssuer;
      BestName := 'id-ce-certificateIssuer';
    end else if Copy(OID,1,Length(id_ce_nameConstraints)) = id_ce_nameConstraints then begin
      BestMatch := id_ce_nameConstraints;
      BestName := 'id-ce-nameConstraints';
    end else if Copy(OID,1,Length(id_ce_cRLDistributionPoints)) = id_ce_cRLDistributionPoints then begin
      BestMatch := id_ce_cRLDistributionPoints;
      BestName := 'id-ce-cRLDistributionPoints';
    end else if Copy(OID,1,Length(id_ce_certificatePolicies)) = id_ce_certificatePolicies then begin
      BestMatch := id_ce_certificatePolicies;
      BestName := 'id-ce-certificatePolicies';
      if Copy(OID,1,Length(anyPolicy)) = anyPolicy then begin
        BestMatch := anyPolicy;
        BestName := 'anyPolicy';
      end;
    end else if Copy(OID,1,Length(id_ce_policyMappings)) = id_ce_policyMappings then begin
      BestMatch := id_ce_policyMappings;
      BestName := 'id-ce-policyMappings';
    end else if Copy(OID,1,Length(id_ce_policyConstraints)) = id_ce_policyConstraints then begin
      BestMatch := id_ce_policyConstraints;
      BestName := 'id-ce-policyConstraints';
    end else if Copy(OID,1,Length(id_ce_authorityKeyIdentifier)) = id_ce_authorityKeyIdentifier then begin
      BestMatch := id_ce_authorityKeyIdentifier;
      BestName := 'id-ce-authorityKeyIdentifier';
    end else if Copy(OID,1,Length(id_ce_extKeyUsage)) = id_ce_extKeyUsage then begin
      BestMatch := id_ce_extKeyUsage;
      BestName := 'id-ce-extKeyUsage';
    end;
  end else if IsSubOIDOf(OID,pkcs) then begin
    BestMatch := pkcs;
    BestName := 'pkcs';
    if IsSubOIDOf(OID,pkcs_1) then begin
      BestMatch := pkcs_1;
      BestName := 'pkcs-1';
      if Length(BestMatch) < Length(OID) then begin
        P := Length(BestMatch) + 2;
        I := ExtractSubIden(OID,P);
        BestMatch := BestMatch + '.' + IntToStr(I);
        case I of
          1:  BestName := 'rsaEncryption';
          2:  BestName := 'md2WithRSAEncryption';
          4:  BestName := 'md5WithRSAEncryption';
          5:  BestName := 'sha1WithRSAEncryption';
          6:  BestName := 'rsaOAEPEncryptionSET';
          7:  BestName := 'id-RSAES-OAEP';
          8:  BestName := 'id-mgf1';
          9:  BestName := 'id-pSpecified';
          10: BestName := 'id-RSASSA-PSS';
          11: BestName := 'sha256WithRSAEncryption';
          12: BestName := 'sha384WithRSAEncryption';
          13: BestName := 'sha512WithRSAEncryption';
        else
          BestMatch := pkcs_1;
        end;
      end;
    end else if IsSubOIDOf(OID,pkcs_7) then begin
      BestMatch := pkcs_7;
      BestName := 'pkcs-7';
      if Length(BestMatch) < Length(OID) then begin
        P := Length(BestMatch) + 2;
        I := ExtractSubIden(OID,P);
        BestMatch := BestMatch + '.' + IntToStr(I);
        case I of
          1:  BestName := 'data';
          2:  BestName := 'signedData';
          3:  BestName := 'envelopedData';
          4:  BestName := 'signedAndEnvelopedData';
          5:  BestName := 'digestedData';
          6:  BestName := 'encryptedData';
        else
          BestMatch := pkcs_1;
        end;
      end;
    end else if IsSubOIDOf(OID,pkcs_9) then begin
      BestMatch := pkcs_9;
      BestName := 'pkcs-9';
      {$IFDEF UNIQUENAMES}
      if IsSubOIDOf(OID,id_emailAddress) then begin
        BestMatch := id_emailAddress;
      {$ELSE}
      if IsSubOIDOf(OID,emailAddress) then begin
        BestMatch := emailAddress;
      {$ENDIF}
        BestName := 'emailAddress';
      end else if IsSubOIDOf(OID,pkcs_9_at_contentType) then begin
        BestMatch := pkcs_9_at_contentType;
        BestName := 'pkcs-9-at-contentType';
      end else if IsSubOIDOf(OID,pkcs_9_at_messageDigest) then begin
        BestMatch := pkcs_9_at_messageDigest;
        BestName := 'pkcs-9-at-messageDigest';
      end else if IsSubOIDOf(OID,pkcs_9_at_signingTime) then begin
        BestMatch := pkcs_9_at_signingTime;
        BestName := 'pkcs-9-at-signingTime';
      end else if IsSubOIDOf(OID,pkcs_9_at_counterSignature) then begin
        BestMatch := pkcs_9_at_counterSignature;
        BestName := 'pkcs-9-at-counterSignature';
      end else if IsSubOIDOf(OID,pkcs_9_at_signingDescription) then begin
        BestMatch := pkcs_9_at_signingDescription;
        BestName := 'pkcs-9-at-signingDescription';
      end else if IsSubOIDOf(OID,pkcs_9_at_friendlyName) then begin
        BestMatch := pkcs_9_at_friendlyName;
        BestName := 'pkcs-9-at-friendlyName';
      end else if IsSubOIDOf(OID,pkcs_9_at_localKeyId) then begin
        BestMatch := pkcs_9_at_localKeyId;
        BestName := 'pkcs-9-at-localKeyId';
      end else if IsSubOIDOf(OID,pkcs_9_certTypes) then begin
        BestMatch := pkcs_9_certTypes;
        BestName := 'pkcs-9-at-certTypes';
      end else if IsSubOIDOf(OID,id_ct) then begin
        BestMatch := id_ct;
        BestName := 'id-ct';
        if Length(BestMatch) < Length(OID) then begin
          P := Length(BestMatch) + 2;
          I := ExtractSubIden(OID,P);
          BestMatch := BestMatch + '.' + IntToStr(I);
          case I of
            10: BestName := 'id-ct-scvp-psRequest';
            11: BestName := 'id-ct-scvp-psResponse';
          else
            BestMatch := id_ct;
          end;
        end;
      end;
    end;
  end else if IsSubOIDOf(OID,ansi_X9_42) then begin
    BestMatch := ansi_X9_42;
    BestName := 'ansi-X9-42';
    if Copy(OID,1,Length(fieldType)) = fieldType then begin
      BestMatch := fieldType;
      BestName := 'fieldType';
      if Copy(OID,1,Length(gfPrime)) = gfPrime then begin
        BestMatch := gfPrime;
        BestName := 'gfPrime';
      end;
    end else if Copy(OID,1,Length(algorithm)) = algorithm then begin
      BestMatch := algorithm;
      BestName := 'algorithm';
      if Copy(OID,1,Length(tripleDES)) = tripleDES then begin
        BestMatch := tripleDES;
        BestName := 'tripleDES';
      end;
    end else if Copy(OID,1,Length(numberType)) = numberType then begin
      BestMatch := numberType;
      BestName := 'numberType';
      if Copy(OID,1,Length(dhPublicNumber)) = dhPublicNumber then begin
        BestMatch := dhPublicNumber;
        BestName := 'dhPublicNumber';
      end;
    end else if Copy(OID,1,Length(scheme)) = scheme then begin
      BestMatch := scheme;
      BestName := 'scheme';
      if Copy(OID,1,Length(dhStatic)) = dhStatic then begin
        BestMatch := dhStatic;
        BestName := 'dhStatic';
      end else if Copy(OID,1,Length(dhEphem)) = dhEphem then begin
        BestMatch := dhEphem;
        BestName := 'dhEphem';
      end else if Copy(OID,1,Length(dhOneFlow)) = dhOneFlow then begin
        BestMatch := dhOneFlow;
        BestName := 'dhOneFlow';
      end else if Copy(OID,1,Length(dhHybrid1)) = dhHybrid1 then begin
        BestMatch := dhHybrid1;
        BestName := 'dhHybrid1';
      end else if Copy(OID,1,Length(dhHybrid2)) = dhHybrid2 then begin
        BestMatch := dhHybrid2;
        BestName := 'dhHybrid2';
      end else if Copy(OID,1,Length(dhHybridOneFlow)) = dhHybridOneFlow then begin
        BestMatch := dhHybridOneFlow;
        BestName := 'dhHybridOneFlow';
      end else if Copy(OID,1,Length(mqv2)) = mqv2 then begin
        BestMatch := mqv2;
        BestName := 'mqv2';
      end else if Copy(OID,1,Length(mqv1)) = mqv1 then begin
        BestMatch := mqv1;
        BestName := 'mqv1';
      end;
    end else if Copy(OID,1,Length(namedScheme)) = namedScheme then begin
      BestMatch := namedScheme;
      BestName := 'namedScheme';
      if Copy(OID,1,Length(dhStatic_sha1)) = dhStatic_sha1 then begin
        BestMatch := dhStatic_sha1;
        BestName := 'dhStatic-sha1';
      end else if Copy(OID,1,Length(dhEphem_sha1)) = dhEphem_sha1 then begin
        BestMatch := dhEphem_sha1;
        BestName := 'dhEphem-sha1';
      end else if Copy(OID,1,Length(dhOneFlow_sha1)) = dhOneFlow_sha1 then begin
        BestMatch := dhOneFlow_sha1;
        BestName := 'dhOneFlow-sha1';
      end else if Copy(OID,1,Length(dhHybrid1_sha1)) = dhHybrid1_sha1 then begin
        BestMatch := dhHybrid1_sha1;
        BestName := 'dhHybrid1-sha1';
      end else if Copy(OID,1,Length(dhHybrid2_sha1)) = dhHybrid2_sha1 then begin
        BestMatch := dhHybrid2_sha1;
        BestName := 'dhHybrid2-sha1';
      end else if Copy(OID,1,Length(dhHybridOneFlow_sha1)) = dhHybridOneFlow_sha1 then begin
        BestMatch := dhHybridOneFlow_sha1;
        BestName := 'dhHybridOneFlow-sha1';
      end else if Copy(OID,1,Length(mqv2_sha1)) = mqv2_sha1 then begin
        BestMatch := mqv2_sha1;
        BestName := 'mqv2-sha1';
      end else if Copy(OID,1,Length(mqv1_sha1)) = mqv1_sha1 then begin
        BestMatch := mqv1_sha1;
        BestName := 'mqv1-sha1';
      end;
    end else if Copy(OID,1,Length(keyDerivationMethod)) = keyDerivationMethod then begin
      BestMatch := keyDerivationMethod;
      BestName := 'keyDerivationMethod';
      if Copy(OID,1,Length(kdasn1der)) = kdasn1der then begin
        BestMatch := kdasn1der;
        BestName := 'kdasn1der';
      end else if Copy(OID,1,Length(kdConcatenation)) = kdConcatenation then begin
        BestMatch := kdConcatenation;
        BestName := 'kdConcatenation';
      end;
    end;
  end else if Copy(OID,1,Length(id_md2)) = id_md2 then begin
    BestMatch := id_md2;
    BestName := 'id-md2';
  end else if Copy(OID,1,Length(id_md5)) = id_md5 then begin
    BestMatch := id_md5;
    BestName := 'id-md5';
  end else if Copy(OID,1,Length(id_sha1)) = id_sha1 then begin
    BestMatch := id_sha1;
    BestName := 'id-sha1';
  end else if Copy(OID,1,Length(id_ripemd160)) = id_ripemd160 then begin
    BestMatch := id_ripemd160;
    BestName := 'id-ripemd160';
  end else if Copy(OID,1,Length(id_sha256)) = id_sha256 then begin
    BestMatch := id_sha256;
    BestName := 'id-sha256';
  end else if Copy(OID,1,Length(id_sha384)) = id_sha384 then begin
    BestMatch := id_sha384;
    BestName := 'id-sha384';
  end else if Copy(OID,1,Length(id_sha512)) = id_sha512 then begin
    BestMatch := id_sha512;
    BestName := 'id-sha512';
  end else if Copy(OID,1,Length(id_dsa)) = id_dsa then begin
    BestMatch := id_dsa;
    BestName := 'id-dsa';
  end else if Copy(OID,1,Length(id_dsa_with_sha1)) = id_dsa_with_sha1 then begin
    BestMatch := id_dsa_with_sha1;
    BestName := 'id-dsa-with-sha1';
  end else if Copy(OID,1,Length(id_aes)) = id_aes then begin
    BestMatch := id_aes;
    BestName := 'id-aes';
    if Length(BestMatch) < Length(OID) then begin
      P := Length(BestMatch) + 2;
      I := ExtractSubIden(OID,P);
      BestMatch := BestMatch + '.' + IntToStr(I);
      case I of
        1:  BestName := 'id-aes128-ECB';
        2:  BestName := 'id-aes128-CBC';
        3:  BestName := 'id-aes128-OFB';
        4:  BestName := 'id-aes128-CFB';
        21: BestName := 'id-aes192-ECB';
        22: BestName := 'id-aes192-CBC';
        23: BestName := 'id-aes192-OFB';
        24: BestName := 'id-aes192-CFB';
        41: BestName := 'id-aes256-ECB';
        42: BestName := 'id-aes256-CBC';
        43: BestName := 'id-aes256-OFB';
        44: BestName := 'id-aes256-CFB';
      else
        BestMatch := id_aes;
      end;
    end;
  end else if Copy(OID,1,Length(ansi_X9_62)) = ansi_X9_62 then begin
    BestMatch := ansi_X9_62;
    BestName := 'ansi-X9-62';
    if Length(BestMatch) < Length(OID) then begin
      P := Length(BestMatch) + 2;
      I := ExtractSubIden(OID,P);
      BestMatch := BestMatch + '.' + IntToStr(I);
      if I = 1 then begin
        BestName := 'id-fieldType';
        if Length(BestMatch) < Length(OID) then begin
          P := Length(BestMatch) + 2;
          I := ExtractSubIden(OID,P);
          if I = 1 then begin
            BestMatch := prime_field;
            BestName := 'prime_field';
          end;
        end;
      end else if I = 2 then begin
        BestName := 'id-publicKeyType';
        if Length(BestMatch) < Length(OID) then begin
          P := Length(BestMatch) + 2;
          I := ExtractSubIden(OID,P);
          if I = 1 then begin
            BestMatch := id_ecPublicKey;
            BestName := 'id-ecPublicKey';
          end;
        end;
      end else if I = 3 then begin
        BestName := 'ansi-ellipticCurve';
        if Length(BestMatch) < Length(OID) then begin
          P := Length(BestMatch) + 2;
          I := ExtractSubIden(OID,P);
          if I = 1 then begin
          {$IFDEF UNIQUENAMES}      
            BestMatch := id_primeCurve;
          {$ELSE}
            BestMatch := primeCurve;
          {$ENDIF}
            BestName := 'primeCurve';
            if Length(BestMatch) < Length(OID) then begin
              P := Length(BestMatch) + 2;
              I := ExtractSubIden(OID,P);   
              BestMatch := BestMatch + '.' + IntToStr(I);
              case I of
                1: BestName := 'prime192v1';
                2: BestName := 'prime192v2';
                3: BestName := 'prime192v3';
                4: BestName := 'prime239v1';
                5: BestName := 'prime239v2';
                6: BestName := 'prime239v3';
                7: BestName := 'prime256v1';
              else
              {$IFDEF UNIQUENAMES}      
                BestMatch := id_primeCurve;
              {$ELSE}
                BestMatch := primeCurve;
              {$ENDIF}
              end;
            end;
          end;
        end;
      end else if I = 4 then begin
        BestName := 'id_ecSigType';
        if Copy(OID,1,Length(ecdsa_with_SHA1)) = ecdsa_with_SHA1 then begin
          BestName := 'ecdsa-with-SHA1';
          BestMatch := ecdsa_with_SHA1;
        end;
      end else
        BestMatch := ansi_X9_62;
    end;
  end else if Copy(OID,1,Length(netscape)) = netscape then begin
    BestName := 'netscape';
    BestMatch := netscape;
    if Copy(OID,1,Length(netscape_cert_extension)) = netscape_cert_extension then begin
      BestName := 'netscape-cert-extension';
      BestMatch := netscape_cert_extension;
      if Copy(OID,1,Length(netscape_cert_type)) = netscape_cert_type then begin
        BestName := 'netscape-cert-type';
        BestMatch := netscape_cert_type;
      end;
    end;
  end;

  if Length(BestMatch) = Length(OID) then
    Result := BestName
  else
    Result := Derivate(BestMatch,BestName,OID);
end;

initialization
  SetUp;
finalization
  CleanUp;
end.
