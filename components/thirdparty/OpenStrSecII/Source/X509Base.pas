{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     X509 Unit                                         }
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
{$B-}
unit X509Base;

interface

uses
  Classes, Asn1, Pkix, MpArithTypes, MpIF, MpDL, MpEC, SecUtils;

const
  E_OK = 0;
  E_SYNTAX = -1;
  E_EMPTY = 1;
  E_STRING_FORMAT = 2;
  E_AMBIGUOUS_SET = 4;
  E_NOT_SUPPORTED = 8;
  E_MISMATCH = 16;

  E_VALID = 0;
  E_INVALID = -1;

  E_CRL_OTHER_CA = 32;

  E_NO_PARAMETERS = 64;

type
  TX509AlgorithmIdentifier = class
  public
    Algorithm: string;
    AlgorithmName: string;
    Parameters: TASN1Struct;
    destructor Destroy; override;
  end;

  TKeyAlgorithmType = (katUnknown,katIF,katDL,katEC);

  TX509PublicKey = class
  private
    FAlgorithm: TX509AlgorithmIdentifier;
    procedure SetAlgorithm(const Value: TX509AlgorithmIdentifier);
    function GetKeyAlgorithmType: TKeyAlgorithmType;
  public
    PublicKey: TASN1Struct;
    constructor Create;
    constructor CreateFromDLKey(const AKey: TDLPublicKey; AsDSA: Boolean);
    constructor CreateFromECKey(const AKey: TECPublicKey);
    constructor CreateFromIFKey(const AKey: TIFPublicKey;
                                AsOAEP: Boolean;
                                HA: THashAlgorithm;
                                MGFHA: THashAlgorithm;
                                P: string);
    destructor Destroy; override;
    procedure AsDLKey(var AKey: TDLPublicKey);
    procedure AsECKey(var AKey: TECPublicKey);
    procedure AsIFKey(var AKey: TIFPublicKey);
    property Algorithm: TX509AlgorithmIdentifier read FAlgorithm write SetAlgorithm;
    property KeyAlgorithmType: TKeyAlgorithmType read GetKeyAlgorithmType;
  end;

  TX500String = packed record
    Str: WideString;
    Tag: Cardinal;
  end;

  TX501RelativeDistinguishedName = packed record
    attributeType: string;
    attributeValue: TX500String;
  end;

  TX501Name = packed record
    commonName: TX500String;
    surname:    TX500String;
    countryName: TX500String;
    localityName: TX500String;
    stateOrProvinceName: TX500String;
    organizationName: TX500String;
    organizationalUnitName: TX500String;
    title: TX500String;
    name: TX500String;
    givenName: TX500String;
    initials: TX500String;
    generationQualifier: TX500String;
    dnQualifier: TX500String;
    emailAddress: TX500String;
  end;

  TX509Validity = record
    notBefore: TDateTime;
    notAfter: TDateTime;
  end;

  TX509Extension = class
  public
    extnID: string;
    extnIDName: string;
    Critical: Boolean;
    extnValue: TASN1Struct;
    destructor Destroy; override;
  end;

  {  This record *MUST* be treated as if it had a case declaration with
     the variable Tag as the case tag. TranslateGeneralNames will only
     translate the variable corresponding to Tag. }
  TX509GeneralName = packed record
    Tag: Byte;
    // OtherName: TX509OtherName; --Not supported
    {Tag = 1} rfc822Name: string;
    {Tag = 2} dNSName: string;
    // x400Address: TX400ORAddress; --Not supported
    {Tag = 4} directoryName: TX501Name;
    {Tag = 5} ediPartyName: packed record nameAssigner: TX501Name; partyName: TX501Name; end;
    {Tag = 6} uniformResourceIdentifier: string;
    {Tag = 7} ipAddress: packed array [0..3] of Byte;
              ipMask: 0..4;        // ipMask = 0: No mask
                                   //          1: 255.0.0.0
                                   //          2: 255.255.0.0
                                   //          3: 255.255.255.0
                                   //          4: 255.255.255.255
    {Tag = 8} registredID: string;
  end;

  {  The subjectAltName and issuerAltName extensions have extnValue of
     type generalName. Use:

       ErrorCode := InterpretGeneralNames(Ext.extnValue,GN);

     to convert these structs to an array GN of type TX509GeneralNames. }

  TX509GeneralNames = array of TX509GeneralName;

  TX509ReasonFlag = (rfUnused7,
                     rfcertificateHold,
                     rfcessationOfOperation,
                     rfSuperseeded,
                     rfAffilationChanged,
                     rfCACompromise,
                     rfKeyCompromise,
                     rfUnused,
                     rfUnused15,
                     rfUnused14,
                     rfUnused13,
                     rfUnused12,
                     rfUnused11,
                     rfUnused10,
                     rfUnused9,
                     rfRemoveFromCRL);

  TX509RevokedCertificate = class
  private
    FList: TList;
    function GetcrlEntryExtensions(index: Integer): TX509Extension;
    procedure SetcrlEntryExtensions(index: Integer;
      const Value: TX509Extension);
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    function GetCI: TX509GeneralNames;
    function GetHI: string;
    function GetInvDate: TDateTime;
    function GetReasonCode: CRLReason;
    procedure SetCI(const Value: TX509GeneralNames);
    procedure SetHI(const Value: string);
    procedure SetInvDate(const Value: TDateTime);
    procedure SetReasonCode(const Value: CRLReason);
  public
    userCertificate: string;
    revocationDate: TDateTime;
    constructor Create;
    destructor Destroy; override;
    function Add: TX509Extension;
    function Append(Item: TX509Extension): Integer;
    procedure Clear;
    procedure Delete(index: Integer);
    property ReasonCode: CRLReason read GetReasonCode write SetReasonCode;
    property HoldInstruction: string read GetHI write SetHI;
    property InvalidityDate: TDateTime read GetInvDate write SetInvDate;
    property CertificateIssuer: TX509GeneralNames read GetCI write SetCI;
    property Count: Integer read GetCount write SetCount;
    property crlEntryExtensions[index: Integer]: TX509Extension read GetcrlEntryExtensions write SetcrlEntryExtensions; default;
  end;

  TX509RevokedCertificates = class
  private
    FList: TList;
    function GetCount: Integer;
    function GetItems(index: Integer): TX509RevokedCertificate;
    procedure SetItems(index: Integer;
      const Value: TX509RevokedCertificate);
    procedure SetCount(const Value: Integer);
  public                                  
    constructor Create;
    destructor Destroy; override;
    function Add: TX509RevokedCertificate;
    function Append(Item: TX509RevokedCertificate): Integer;
    procedure Clear;
    procedure Delete(index: Integer);
    property Count: Integer read GetCount write SetCount;
    property Items[index: Integer]: TX509RevokedCertificate
      read GetItems write SetItems; default;
  end;

const
  LowReasonFlag = TX509ReasonFlag(1);
  UnusedReasonFlagBits: Byte = 7;
type
  TX509ReasonFlags = set of TX509ReasonFlag;
  TX509DistributionPointName = packed record
    Tag: Byte;
    {Tag = 0} fullName: TX509GeneralNames;
    {Tag = 1} nameRelativeToCRLIssuer: TX501Name;
  end;

  TX509DistributionPoint = packed record
    distributionPoint: TX509DistributionPointName;
    reasons:           TX509ReasonFlags;
    CRLIssuer:         TX509GeneralNames;
  end;

  TX509CRLDistPoints = array of TX509DistributionPoint;

  TX509issuingDistributionPoint = packed record
    distributionPoint:       TX509DistributionPointName;
    onlyContainsUserCerts:   Boolean;
    onlyContainsCACerts:     Boolean;
    onlySomeReasons:         TX509ReasonFlags;
    indirectCRL:             Boolean;
  end;

  TX509issuingDistributionPoints = array of TX509issuingDistributionPoint;

// From ASN.1
function CheckCertSyntax(var Cert: TASN1Struct): Boolean;

function CompareCertificate(const Cert0, Cert1: TASN1Struct): Boolean;

function ExtractSerial(const Cert: TASN1Struct; var Serial: string): Integer;

function InterpretName(const Struct: TASN1Struct;
                       var Name: TX501Name;
                       AdjustCase: Boolean = True;
                       Delim: WideChar = #0013): Integer;
function ExtractIssuerStruct(const Cert: TASN1Struct;
                             var NameStruct: PASN1Struct): Integer;
function ExtractIssuer(const Cert: TASN1Struct;
                       var Name: TX501Name;
                       AdjustCase: Boolean = True;
                       Delim: WideChar = #0013): Integer;
function ExtractSubjectStruct(const Cert: TASN1Struct;
                              var NameStruct: PASN1Struct): Integer;
function ExtractSubject(const Cert: TASN1Struct;
                        var Name: TX501Name;
                        AdjustCase: Boolean = True;
                        Delim: WideChar = #0013): Integer;
function CompareName(const Name0, Name1: TX501Name): Boolean;

function Extractv2IssuerID(const Cert: TASN1Struct; var UniqueID: string): Integer;
function Extractv2SubjectID(const Cert: TASN1Struct; var UniqueID: string): Integer;
function Comparev2UniqueID(const UID0, UID1: string): Boolean;

function ExtractValidity(const Cert: TASN1Struct; var Validity: TX509Validity): Integer;
function CheckValidity(const Cert: TASN1Struct; HoursOffsetFromGMT: Double): Boolean;

function InterpretAttribute(const Struct: TASN1Struct; var Ext: TX509Extension): Integer;

function InterpretExtension(const Struct: TASN1Struct; var Ext: TX509Extension): Integer;
function ExtractExtension(const Cert: TASN1Struct; Index: Integer; var Ext: TX509Extension): Integer;
function ExtractNamedExtension(const Cert: TASN1Struct; OID: string; var Ext: TX509Extension): Integer;
function ExtractCRLExtension(const CRL: TASN1Struct; Index: Integer; var Ext: TX509Extension): Integer;
function ExtractNamedCRLExtension(const CRL: TASN1Struct; OID: string; var Ext: TX509Extension): Integer;
                                        
function ExtractKeyUsage(const Cert: TASN1Struct): TKeyUsage;    
function ExtractSubjectKeyIdentifier(const Cert: TASN1Struct): string;

function ExtractIssuerKeyIdentifier(const Cert: TASN1Struct): string;

function ExtractPathLenConstraint(const Cert: TASN1Struct): Integer;

function ExtractCRLNumber(const CRL: TASN1Struct): Integer;
function ExtractBaseCRLNumber(const CRL: TASN1Struct): Integer;

function CheckCriticalExtensions(const Cert: TASN1Struct; var Ext: TX509Extension): Integer;
                                                                          
function ExtractDNSName(Cert: TASN1Struct): string;
function CheckDNSName(Cert: TASN1Struct; const ADNSName: string): Boolean;
function ExtractEmailAddress(Cert: TASN1Struct): string;
function CheckEmailAddress(Cert: TASN1Struct; const AEmailAddress: string): Boolean;
function ExtractURI(Cert: TASN1Struct): string;
function CheckURI(Cert: TASN1Struct; const AURI: string): Boolean;
function ExtractIP(Cert: TASN1Struct): string;
function CheckIP(Cert: TASN1Struct; const AIP: string): Boolean;
                                                          
function CheckCertificatePolicy(const Cert: TASN1Struct;
                                const Policy: ObjectIdentifier): Boolean;

function ExtractCRLDistributionPoints(const Cert: TASN1Struct; var DPs: TX509CRLDistPoints): Integer;
function ExtractIssuingDistributionPoint(const CRL: TASN1Struct; var IDP: TX509issuingDistributionPoint): Integer;

function CompareIssuingDistributionPoints(const IDP0, IDP1: TX509issuingDistributionPoint): Integer;
function SuggestIssuingDistributionPoints(const CACert: TASN1Struct;
                                          const Reasons: TX509ReasonFlags;
                                          var IDPs: TX509issuingDistributionPoints): Integer;

function CertIsRevoked(const Cert, CRL: TASN1Struct): Integer;  
function ExtractRevokedCertificate(const CRL: TASN1Struct; Index: Integer;
  var RC: TX509RevokedCertificate): Integer;
function ExtractRevokedCertificates(const CRL: TASN1Struct;
  var RCList: TX509RevokedCertificates): Integer;

function InterpretGeneralNames(const Struct: TASN1Struct; var Name: TX509GeneralNames): Integer;

function InterpretAlgorithmIdentifier(const Struct: TASN1Struct; var Alg: TX509AlgorithmIdentifier): Integer;
function ExtractSignatureAlgorithm(const Signed: TASN1Struct; var Alg: TX509AlgorithmIdentifier): Integer;
function ExtractSignatureAlgorithmCertOrCRL(const Cert: TASN1Struct; var Alg: TX509AlgorithmIdentifier): Integer;

function ExtractSubjectPublicKey(const Cert: TASN1Struct; var PK: TX509PublicKey): Integer;
function ExtractSubjectPublicKeySize(const Cert: TASN1Struct; var KeySize: Integer): Integer;
function ExtractSubjectPublicKeyAlg(const Cert: TASN1Struct; var OID: string): Integer;
function ExtractSubjectPublicKeyStruct(const Cert: TASN1Struct; var PK: PASN1Struct): Integer;

function InterpretSubjectRSAPublicKey(PK: TX509PublicKey;
                                      var RSAKey: TIFPublicKey;
                                      AcceptOAEP: Boolean = False): Integer;
function InterpretSubjectRSA_OAEPPublicKey(PK: TX509PublicKey;
                                           var RSAKey: TIFPublicKey;
                                           var HashFunc: THashAlgorithm;
                                           var MGFHashFunc: THashAlgorithm;
                                           var P: string): Integer;
function InterpretRSASignatureAlgorithm(const Alg: TX509AlgorithmIdentifier;
                                        var HA: THashAlgorithm;
                                        var MHA: THashAlgorithm;
                                        var EM: TSignEncoding): Boolean;
function InterpretSubjectDHPublicKey(PK: TX509PublicKey;
                                     var DHKey: TDLPublicKey): Integer;
function InterpretSubjectDSAPublicKey(PK: TX509PublicKey;
                                      var DSAKey: TDLPublicKey): Integer;
function InterpretSubjectECPublicKey(PK: TX509PublicKey;
                                     var ECKey: TECPublicKey): Integer;

function ExtractSubjectRSAPublicKey(const Cert: TASN1Struct;
                                    var RSAKey: TIFPublicKey;
                                    AcceptOAEP: Boolean = False): Integer;  
function ExtractSubjectRSA_OAEPPublicKey(const Cert: TASN1Struct;
                                         var RSAKey: TIFPublicKey;
                                         var HashFunc: THashAlgorithm;
                                         var MGFHashFunc: THashAlgorithm;
                                         var P: string): Integer;
function ExtractSubjectDHPublicKey(const Cert: TASN1Struct; var DHKey: TDLPublicKey): Integer;
function ExtractSubjectDSAPublicKey(const Cert: TASN1Struct; var DSAKey: TDLPublicKey): Integer;
function ExtractSubjectECPublicKey(const Cert: TASN1Struct; var ECKey: TECPublicKey): Integer;

function IsAllegedIssuer(const Cert, CACert: TASN1Struct): Boolean;
function IsAllegedCRLIssuer(const CRL, CACert: TASN1Struct): Boolean;

function CheckSignature(const Signed, CACert: TASN1Struct;
                        RaiseBitAlignError: Boolean = True): Boolean;
function CheckSignatureCert(const Cert, CACert: TASN1Struct): Boolean;
function CheckSignatureCRL(const CRL, CACert: TASN1Struct): Boolean;

{$IFDEF SHA1}
function RSAPrivateKeyFromStream(AStream: TStream;
                                 const Password; PWLen: Integer;
                                 var PrivKey: TIFPrivateKey): Boolean;
{$ENDIF SHA1}
function VerifyRSAPrivateKey(const Cert: TASN1Struct; const PrivKey: TIFPrivateKey): Boolean;
             
{$IFDEF SHA1}
function DLPrivateKeyFromStream(AStream: TStream;
                               const Password; PWLen: Integer;
                               var PrivKey: TDLPrivateKey): Boolean;
{$ENDIF SHA1}
function VerifyDLPrivateKey(const Cert: TASN1Struct; const PrivKey: TDLPrivateKey): Boolean;

function VerifyECPrivateKey(const Cert: TASN1Struct; const PrivKey: TECPrivateKey): Boolean;

// To ASN.1
procedure TranslatePublicKey(const PK: TX509PublicKey; var Struct: TASN1Struct);
procedure TranslateRSAPublicKey(const RSAKey: TIFPublicKey; var PK: TX509PublicKey);  
procedure TranslateRSA_OAEPPublicKey(const RSAKey: TIFPublicKey;
                                     HashAlgorithm,
                                     MGFHashAlgorithm: THashAlgorithm;
                                     const P: string;
                                     var PK: TX509PublicKey);
procedure TranslateDHPublicKey(const DLKey: TDLPublicKey; var PK: TX509PublicKey);
procedure TranslateDSAPublicKey(const DLKey: TDLPublicKey; var PK: TX509PublicKey);
procedure TranslateECPublicKey(const ECKey: TECPublicKey; var PK: TX509PublicKey);

procedure TranslateName(const Name: TX501Name; var Struct: TASN1Struct);

procedure TranslateGeneralNames(const Name: TX509GeneralNames; var Struct: TASN1Struct);

procedure TranslateExtension(const Ext: TX509Extension; var Struct: TASN1Struct);
procedure ImposeExtension(const Ext: TX509Extension; var Cert: TASN1Struct);
procedure ImposeCRLExtension(const Ext: TX509Extension; var CRL: TASN1Struct);

procedure TranslateAttribute(const Ext: TX509Extension; var Struct: TASN1Struct);

procedure ComposeCertificate(var Cert: TASN1Struct;
                             const SubjectName: TX501Name;
                             const SubjectPublicKey: TX509PublicKey;
                             const CACert: TASN1Struct;
                             const Validity: TX509Validity;
                             const KeyUsage: TKeyUsage;
                             const ExtKeyUsage: TStrings;
                             const SubjectAltName: TX509GeneralNames;
                             const Serial: string = '');
procedure ComposeCACertificate(var Cert: TASN1Struct;
                               const SubjectName: TX501Name;
                               const SubjectPublicKey: TX509PublicKey;
                               const Validity: TX509Validity;
                               const KeyUsage: TKeyUsage;
                               const ExtKeyUsage: TStrings;
                               const SubjectAltName: TX509GeneralNames;
                               const CRLDistributionPoints: TX509CRLDistPoints;
                               PathLenConstraint: Cardinal;
                               const Serial: string = '');
procedure RSASignCertificate(var Cert: TASN1Struct;
                             const CACert: TASN1Struct;
                             const CAPrivKey: TIFPrivateKey;
                             HashAlgorithm: THashAlgorithm;
                             MGFHashAlgorithm: THashAlgorithm;
                             Encoding: TSignEncoding);
procedure DSASignCertificate(var Cert: TASN1Struct;
                             const CACert: TASN1Struct;
                             const CAPrivKey: TDLPrivateKey;
                             // Allow for future expansion:
                             HashAlgorithm: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF});
procedure ECDSASignCertificate(var Cert: TASN1Struct;
                               const CACert: TASN1Struct;
                               const CAPrivKey: TECPrivateKey;
                               // Allow for future expansion:
                               HashAlgorithm: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF});
                         

procedure ComposeCRL(var CRL: TASN1Struct;
                     const CACert: TASN1Struct;
                     ThisUpdate, NextUpdate: TDateTime;
                     const RCList: TX509RevokedCertificates;
                     CRLNumber: Cardinal;
                     BaseCRLNumber: Cardinal;
                     issuingDistributionPoint: TX509issuingDistributionPoint);
procedure RSASignCRL(var CRL: TASN1Struct;
                     const CACert: TASN1Struct;
                     const CAPrivKey: TIFPrivateKey;
                     HashAlgorithm: THashAlgorithm;
                     MGFHashAlgorithm: THashAlgorithm;
                     Encoding: TSignEncoding);
procedure DSASignCRL(var CRL: TASN1Struct;
                     const CACert: TASN1Struct;
                     const CAPrivKey: TDLPrivateKey;
                     // Allow for future expansion:
                     HashAlgorithm: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF});
procedure ECDSASignCRL(var CRL: TASN1Struct;
                       const CACert: TASN1Struct;
                       const CAPrivKey: TECPrivateKey;
                       HashAlgorithm: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF});

procedure RSASignStruct(var Signed: TASN1Struct;
//                       const CACert: TASN1Struct;
                        const CAPrivKey: TIFPrivateKey;
                        HashAlgorithm: THashAlgorithm;
                        MGFHashAlgorithm: THashAlgorithm;
                        Encoding: TSignEncoding);
procedure DSASignStruct(var Signed: TASN1Struct;
//                        const CACert: TASN1Struct;
                        const CAPrivKey: TDLPrivateKey;
                        // Allow for future expansion:
                        HashAlgorithm: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF});
procedure ECDSASignStruct(var Signed: TASN1Struct;
//                          const CACert: TASN1Struct;
                          const CAPrivKey: TECPrivateKey;
                          // Allow for future expansion:
                          HashAlgorithm: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF});
                                                    
{$IFDEF SHA1}
procedure RSAPrivateKeyToStream(AStream: TStream;
                                const Password; PWLen: Integer;
                                const PrivKey: TIFPrivateKey);

procedure DLPrivateKeyToStream(AStream: TStream;
                               const Password; PWLen: Integer;
                               const PrivKey: TDLPrivateKey);
{$ENDIF SHA1}

procedure ComposeCertificate2(var Cert: TASN1Struct;
                              const SubjectName: TX501Name;
                              const SubjectPublicKey: TX509PublicKey;
                              const CACert: TASN1Struct;
                              const Validity: TX509Validity;
                              const KeyUsage: TKeyUsage;
                              const ExtKeyUsage: TStrings;
                              const SubjectAltName: TX509GeneralNames;
                              const Serial: string);
procedure ComposeCACertificate2(var Cert: TASN1Struct;
                                const SubjectName: TX501Name;
                                const SubjectPublicKey: TX509PublicKey;
                                const Validity: TX509Validity;
                                const KeyUsage: TKeyUsage;
                                const ExtKeyUsage: TStrings;
                                const SubjectAltName: TX509GeneralNames;
                                const CRLDistributionPoints: TX509CRLDistPoints;
                                PathLenConstraint: Cardinal;
                                const Serial: string = '';
                                const CACert: TASN1Struct = nil);

implementation

uses
  Windows, SysUtils, ReadStrm, MpArith, SsRijndael, MpYarrow, MpEC_X9_62Curves,
  MpEC_NISTCurves;

function CompareCertificate(const Cert0, Cert1: TASN1Struct): Boolean;
var
  MS0, MS1: TSecureMemoryStream;
begin
  MS0 := TSecureMemoryStream.Create;
  try
    ASN1ToStream(Cert0,MS0);
    MS1 := TSecureMemoryStream.Create;
    try
      ASN1ToStream(Cert1,MS1);
      Result := MS0.Size = MS1.Size;
      if Result then
        Result := CompareMem(MS0.Memory,MS1.Memory,MS0.Size);
    finally
      MS1.Free;
    end;
  finally
    MS0.Free;
  end;
end;

function InterpretRSASignatureAlgorithm(const Alg: TX509AlgorithmIdentifier;
                                        var HA: THashAlgorithm;
                                        var MHA: THashAlgorithm;
                                        var EM: TSignEncoding): Boolean;
var
  S: PASN1Struct;
  A, A2: TX509AlgorithmIdentifier;
begin
    Result := True;
    if ExtractHashAlgEMSA3(Alg.Algorithm,HA) then
      EM := seEMSA3
    else if Alg.Algorithm = id_RSASSA_PSS then begin
      EM := seEMSA4;
      if ASN1StructAssigned(Alg.Parameters) and
         (Alg.Parameters.ItemCount > 0) then begin
        A := nil;
        S := @Alg.Parameters.Contents^.Items[0];
        S := @S^.Contents^.Items[0];
        if InterpretAlgorithmIdentifier(S^,A) = E_OK then begin
          Result := OIDToHashAlgorithm(A.Algorithm,HA);
          if Result and (Alg.Parameters.ItemCount > 1) then begin
            S := @Alg.Parameters.Contents^.Items[1];
            S := @S^.Contents^.Items[0];
            if InterpretAlgorithmIdentifier(S^,A) = E_OK then begin
              Result := A.Algorithm = id_mgf1;
              if Result and ASN1StructAssigned(A.Parameters) and
                 (A.Parameters.Tag <> V_ASN1_NULL) then begin
                A2 := nil;
                if InterpretAlgorithmIdentifier(A.Parameters,A2) = E_OK then
                  Result := OIDToHashAlgorithm(A2.Algorithm,MHA)
                else
                {$IFDEF SHA1}
                  MHA := haSHA1;
                {$ELSE}
                  Result := False;
                {$ENDIF}
                A2.Free;
              end else
              {$IFDEF SHA1}
                MHA := haSHA1;
              {$ELSE}
                Result := False;
              {$ENDIF}
            end else
              Result := False;
          end else
            Result := False;
          A.Free;
        end else
          Result := False;
      end else begin
      {$IFDEF SHA1}
        HA := haSHA1;
        MHA := haSHA1;
      {$ELSE}
        Result := False;
      {$ENDIF}
      end;
    end else
      Result := False;
end;

procedure ComposeRSASignAlg(var Struct: TASN1Struct;
                            HashAlgorithm: THashAlgorithm;
                            MGFHashAlgorithm: THashAlgorithm;
                            Encoding: TSignEncoding);
var
  Alg: string;
  S, R: PASN1Struct;
begin
  Struct.Free;
  Struct := nil;
  if Encoding = seEMSA3 then begin
    Alg := SignatureAlgEMSA3(HashAlgorithm);
    NewComposeASN1Struct(Struct,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
    Struct.Persistent := True;
    Struct.TypeName := 'AlgorithmIdentifier-PKCS1-v1.5';
    Struct.AddField('algorithm','OBJECT',Alg,True);
    Struct.AddField('parameters','NULL',nil);
  end else if Encoding = seEMSA4 then begin
    Alg := id_RSASSA_PSS;
    NewComposeASN1Struct(Struct,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
    Struct.Persistent := True;
    Struct.TypeName := 'AlgorithmIdentifier-PSS';
    Struct.AddField('algorithm','OBJECT',Alg,True);
    S := Struct.AddField('parameters','RSASSA-PSS-params',nil);
    S.Tag := V_ASN1_SEQUENCE;
{$IFDEF SHA1}
    if HashAlgorithm <> haSHA1 then begin
{$ENDIF SHA1}
      Alg := HashAlgorithmToOID(HashAlgorithm);

      R := S^.AddField('hashFunc','[0] AlgorithmIdentifier-hashFunc',nil);
      R^.Tag := V_ASN1_SEQUENCE;
      R^.AddField('algorithm','OBJECT',Alg,True);
      R^.AddField('parameters','NULL',nil);

      R := S^.AddField('maskGenFunc','[1] AlgorithmIdentifier-maskGenFunc',nil);
      R^.Tag := V_ASN1_SEQUENCE;
      R^.AddField('algorithm','OBJECT',id_mgf1,True);
      R := R^.AddField('parameter','AlgorithmIdentifier-hashFunc',nil);
      R^.Tag := V_ASN1_SEQUENCE;
      Alg := HashAlgorithmToOID(MGFHashAlgorithm);
      R^.AddField('algorithm','OBJECT',Alg,True);
      R^.AddField('parameters','NULL',nil);
{$IFDEF SHA1}
    end;
{$ENDIF SHA1}
  end else
    raise Exception.Create('Encoding not supported');
end;

function CheckCertSyntax(var Cert: TASN1Struct): Boolean;
var
  T, S, V: PASN1STruct;
  Name: TX501Name;
  Version: Byte;
  Idx, I: Integer;
  Alg: TX509AlgorithmIdentifier;
  HA, MHA: THashAlgorithm;
  EM: TSignEncoding;
  Validity: TX509Validity;
  PK: TX509PublicKey;
  RSAKey: TIFPublicKey;
  DHKey: TDLPublicKey;
  OK: Boolean;
begin
  Result := False;

  if (Cert.Cls <> V_ASN1_UNIVERSAL) or
     (Cert.Tag <> V_ASN1_SEQUENCE) or
     not Cert.Constructed then
    Exit;
  if Cert.ItemCount <> 3 then
    Exit;
  if Cert.Items[2].Tag <> V_ASN1_BIT_STRING then
    Exit;
  Cert.Persistent := True;
  Cert.TypeName := 'Certificate';
  Cert.Items[2].Persistent := True;
  Cert.Items[2].VarName := 'signature';

  T := Cert.Items[0];
  if (T.Cls <> V_ASN1_UNIVERSAL) or
     (T.Tag <> V_ASN1_SEQUENCE) or
     not T.Constructed then
    Exit;
  if T.ItemCount < 6 then
    Exit;
  T^.Persistent := True;
  T.TypeName := 'TBSCertificate';
  T.VarName := 'tbsCertificate';

  Idx := 0;

  S := T^.Items[Idx];
  if (S.Cls = V_ASN1_CONTEXT_SPECIFIC) and
     (S.Tag = 0) and
     S.Constructed then begin
    if T.ItemCount < 7 then
      Exit;
    S^.Persistent := True;
    S^.VarName := 'version';
    S^.SetTypeNameNoCheck('[0] INTEGER DEFAULT A003020100');
    S := S^.Items[0];
    if (S.Cls <> V_ASN1_UNIVERSAL) or
       (S.Tag <> V_ASN1_INTEGER) or
       S.Constructed then
      Exit;
    if S.Length <> 1 then
      Exit;
    S^.Persistent := True;
    Move(S^.Content^,Version,1);
    Inc(Idx);
  end else
    Version := 0;

  S := T^.Items[Idx];
  if (S.Cls <> V_ASN1_UNIVERSAL) or
     (S.Tag <> V_ASN1_INTEGER) or
     S.Constructed then
    Exit;
  if S.Length < 1 then
    Exit;
  S.Persistent := True;
  S.VarName := 'serialNumber';
  Inc(Idx);

  Alg := nil;
  try
    OK := False;
    if ExtractSignatureAlgorithmCertOrCRL(Cert,Alg) = E_OK then begin
      S := T^.Items[Idx];
      if Alg.Algorithm = id_dsa_with_sha1 then begin
        OK := True;
        S^.Persistent := True;
        S^.TypeName := 'AlgorithmIdentifier-DSA';
        S^.Items[0].Persistent := True;
        S^.Items[0].VarName := 'algorithm';
        S^.Items[1].Persistent := True;
        S^.Items[1].VarName := 'parameters';
      end else begin
        OK := InterpretRSASignatureAlgorithm(Alg,HA,MHA,EM);
        if OK then begin
          New(V);
          try
            V^ := nil;
            ComposeRSASignAlg(V^,HA,MHA,EM);
            S.Assign(V^);
            Cert.Items[1].Assign(V^);
            Cert.Items[1].Persistent := True;
            Cert.Items[1].VarName := 'signatureAlgorithm';
            V^.Free;
          finally
            Dispose(V);
          end;
        end;
      end;
      if OK then begin
        S^.Persistent := True;
        S^.VarName := 'signature';
      end;
    end;
  finally
    Alg.Free;
  end;
  if not OK then Exit;
  Inc(Idx);

  S := T^.Items[Idx];
  FillChar(Name,SizeOf(Name),0);
  if ExtractIssuer(Cert,Name) <> E_OK then
    Exit;
  TranslateName(Name,S^); 
  S^.Persistent := True;
  S^.TypeName := 'Name';
  S^.VarName := 'issuer';
  Inc(Idx);

  S := T^.Items[Idx];
  FillChar(Validity,SizeOf(Validity),0);
  if ExtractValidity(Cert,Validity) <> E_OK then
    Exit;
  S.Persistent := True;
  S.TypeName := 'Validity';
  S.VarName := 'validity';
  S^.Items[0].Persistent := True;
  S^.Items[0].VarName := 'notBefore';
  S^.Items[1].Persistent := True;
  S^.Items[1].VarName := 'notAfter';
  Inc(Idx);

  S := T^.Items[Idx];
  FillChar(Name,SizeOf(Name),0);
  if ExtractSubject(Cert,Name) <> E_OK then
    Exit;
  TranslateName(Name,S^);
  S^.Persistent := True;
  S^.TypeName := 'Name';
  S^.VarName := 'subject';
  Inc(Idx);

  OK := False;
  S := T^.Items[Idx];
  FillChar(RSAKey,SizeOf(RSAKey),0);
  FillChar(DHKey,SizeOf(DHKey),0);
  if ExtractSubjectRSAPublicKey(Cert,RSAKey) = E_OK then begin
    OK := True;
    PK := nil;
    try
      TranslateRSAPublicKey(RSAKey,PK);
      TranslatePublicKey(PK,S^);              
      S^.Persistent := True;
      S^.TypeName := 'SubjectPublicKeyInfo-RSA';
    finally
      PK.Free;
    end;
  end else if ExtractSubjectDSAPublicKey(Cert,DHKey) = E_OK then begin
    OK := True;
    PK := nil;
    try
      TranslateDSAPublicKey(DHKey,PK);
      TranslatePublicKey(PK,S^);
      S^.Persistent := True;
      S^.TypeName := 'SubjectPublicKeyInfo-DSA';
    finally
      PK.Free;
    end;
  end else if ExtractSubjectDHPublicKey(Cert,DHKey) = E_OK then begin
    OK := True;
    PK := nil;
    try
      TranslateDHPublicKey(DHKey,PK);
      TranslatePublicKey(PK,S^);
      S^.Persistent := True;
      S^.TypeName := 'SubjectPublicKeyInfo-DH';
    finally
      PK.Free;
    end;
  end;
  DisposeIFPublicKey(RSAKey);
  DisposeDLPublicKey(DHKey);
  if not OK then Exit;
  S^.VarName := 'subjectPublicKeyInfo';
  Inc(Idx);

  S := T^.Items[Idx];
  if (Version >= 1) and
     (S.Cls = V_ASN1_CONTEXT_SPECIFIC) and
     (S.Tag = 1) and
     not S.Constructed then begin
    S^.Persistent := True;
    S^.VarName := 'issuerUniqueID';
    S^.SetTypeNameNoCheck('[1] IMPLICIT BIT STRING OPTIONAL');
    Inc(Idx);
  end;

  S := T^.Items[Idx];
  if (Version >= 1) and
     (S.Cls = V_ASN1_CONTEXT_SPECIFIC) and
     (S.Tag = 2) and
     not S.Constructed then begin
    S^.Persistent := True;
    S^.VarName := 'subjectUniqueID';
    S^.SetTypeNameNoCheck('[2] IMPLICIT BIT STRING OPTIONAL');
    Inc(Idx);
  end;

  S := T^.Items[Idx];
  if (Version = 2) and
     (S.Cls = V_ASN1_CONTEXT_SPECIFIC) and
     (S.Tag = 3) and
     S.Constructed then begin
    if S^.ItemCount = 1 then begin
      S^.Persistent := True;
      S^.VarName := 'extensions';
      S^.SetTypeNameNoCheck('[3] Extensions OPTIONAL');
      S := S.Items[0];
      if (S.Cls = V_ASN1_UNIVERSAL) and
         (S.Tag = V_ASN1_SEQUENCE) and
         S.Constructed then begin
        S.Persistent := True;
        S.TypeName := 'Extensions';
        if S.Template = nil then begin
          S.CreateOFTemplate;
          S.Template.Persistent := True;
          S.Template.Tag := V_ASN1_SEQUENCE;
          S.Template.Constructed := True;
          S.Template.TypeName := 'Extension';
          S.Template.AddField('extnID','OBJECT','',True);
          S.Template.AddField('critical','BOOLEAN','False',True);
          S.Template.FindField('critical').SetAsDefault;
          S.Template.AddField('extnValue','OCTET STRING','',True);
        end;
        OK := True;
        for I := 0 to S.ItemCount - 1 do begin
          V := S.Items[I];
          OK := (V.Tag = V_ASN1_SEQUENCE) and
                (V.Cls = V_ASN1_UNIVERSAL) and
                V.Constructed;
          if OK then begin
            if V.ItemCount = 2 then begin
              OK := (V.Items[0].Cls = V_ASN1_UNIVERSAL) and
                    (V.Items[0].Tag = V_ASN1_OBJECT) and
                    (not V.Items[0].Constructed) and
                    (V.Items[1].Cls = V_ASN1_UNIVERSAL) and
                    (V.Items[1].Tag = V_ASN1_OCTET_STRING) and
                    not V.Items[1].Constructed;
              if OK then begin
                V.Items[0].Persistent := True;
                V.Items[0].VarName := 'extnID';
                V.Items[1].Persistent := True;
                V.Items[1].VarName := 'extnValue';
              end;
            end else if V.ItemCount = 3 then begin
              OK := (V.Items[0].Cls = V_ASN1_UNIVERSAL) and
                    (V.Items[0].Tag = V_ASN1_OBJECT) and
                    (not V.Items[0].Constructed) and
                    (V.Items[1].Cls = V_ASN1_UNIVERSAL) and
                    (V.Items[1].Tag = V_ASN1_BOOLEAN) and
                    (V.Items[1].Length = 1) and
                    (not V.Items[1].Constructed) and
                    (V.Items[2].Cls = V_ASN1_UNIVERSAL) and
                    (V.Items[2].Tag = V_ASN1_OCTET_STRING) and
                    not V.Items[2].Constructed;
              if OK then begin
                V.Items[0].Persistent := True;
                V.Items[0].VarName := 'extnID';
                V.Items[1].Persistent := True;
                V.Items[1].VarName := 'critical';
                V.Items[2].Persistent := True;
                V.Items[2].VarName := 'extnValue';
              end;
            end else
              OK := False;
          end;
          if not OK then Break;
          V.Persistent := True;
          V.TypeName := 'Extension';
        end;
        if OK then
          Inc(Idx);
      end;
    end;
  end;

  Result := T.ItemCount = Idx;
end;

procedure ComposeCertificate(var Cert: TASN1Struct;
                             const SubjectName: TX501Name;
                             const SubjectPublicKey: TX509PublicKey;
                             const CACert: TASN1Struct;
                             const Validity: TX509Validity;
                             const KeyUsage: TKeyUsage;
                             const ExtKeyUsage: TStrings;
                             const SubjectAltName: TX509GeneralNames;
                             const Serial: string);
var
  T, V, S, E: PASN1Struct;
  B: Byte;
  Ext: TX509Extension;
{$IFDEF SHA1}
  H: TSHA1;  
{$ENDIF SHA1}
  D: string;
  I, J: Integer;
  IssuerAltName: TX509GeneralNames;
begin
  if [] = KeyUsage then
    raise Exception.Create('KeyUsage must not be empty');

  if ASN1StructAssigned(Cert) then
    DisposeASN1Struct(Cert);
  NewComposeASN1Struct(Cert,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  AddComposeASN1Field(Cert,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  T := @Cert.Contents^.Items[0];

  AddComposeASN1Field(T^,V_ASN1_CONTEXT_SPECIFIC,True,0);
  V := @T^.Contents^.Items[0];
  AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  B := 2;
  V^.Contents^.Items[0].SetContent(B,1);
  V^.CalculateLength;

  AddComposeASN1Field(T^,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  if Serial <> '' then
    T^.Items[1].SetContent(Serial[1],Length(Serial));

  AddComposeASN1Field(T^,V_ASN1_UNIVERSAL,False,V_ASN1_SEQUENCE);

  V := @CACert.Contents^.Items[0];
  V := @V^.Contents^.Items[5];
  AddASN1Field(T^,V^);

  AddComposeASN1Field(T^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  V := @T^.Contents^.Items[4];
  New(S);
  try
    FillChar(S^,SizeOf(TASN1Struct),0);
    DateTimeToASN1(Validity.notBefore,S^);
    AddASN1Field(V^,S^);
    DateTimeToASN1(Validity.notAfter,S^);
    AddASN1Field(V^,S^);
    DisposeASN1Struct(S^);
  finally
    Dispose(S);
  end;
  V^.CalculateLength;

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateName(SubjectName,V^);
    AddASN1Field(T^,V^);

    TranslatePublicKey(SubjectPublicKey,V^);
    AddASN1Field(T^,V^);

{$IFDEF SHA1}
    SetLength(D,20);
    H := TSHA1.Create(V^.Contents^.Items[1].Content[1],V^.Contents^.Items[1].Length-1);
    try
      H.Done(Pointer(D));
    finally
      H.Free;
    end;
{$ELSE  SHA1}
    D := '';
{$ENDIF SHA1}

    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;

  AddComposeASN1Field(T^,V_ASN1_CONTEXT_SPECIFIC,True,3);
  E := @T^.Contents^.Items[7];

  FillChar(Ext,SizeOf(Ext),0);
  if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,Ext) = E_OK then begin
    Ext.Critical := False;
    Ext.extnID := id_ce_authorityKeyIdentifier;
    New(V);
    try
      V^ := TASN1Struct.Create;
      V^.Cls := V_ASN1_UNIVERSAL;
      V^.Constructed := True;
      V^.Tag := V_ASN1_SEQUENCE;
      AddComposeASN1Field(V^,V_ASN1_CONTEXT_SPECIFIC,True,0);
      S := @V^.Contents^.Items[0];
      AddASN1Field(S^,Ext.extnValue);
      CopyASN1Struct(Ext.extnValue,V^);
      TranslateExtension(Ext,V^);
      AddASN1Field(E^,V^);
      V^.Free;
    finally
      Dispose(V);
    end;
    DisposeASN1Struct(Ext.extnValue);
  end;
            
{$IFDEF SHA1}
  Ext := TX509Extension.Create;
  try
    NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_OCTET_STRING);
    Ext.extnValue.SetContent(D,20);
    Ext.extnID := id_ce_subjectKeyIdentifier;
    Ext.Critical := False;
    New(V);
    try
      FillChar(V^,SizeOf(TASN1Struct),0);
      TranslateExtension(Ext,V^);
      AddASN1Field(E^,V^);
      DisposeASN1Struct(V^);
    finally
      Dispose(V);
    end;
  finally
    Ext.Free;
  end;       
{$ENDIF SHA1}

  Ext := TX509Extension.Create;
  try
    NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_BIT_STRING);
    SetLength(D,2);
    Move(KeyUsage,D[1],2);
    D[2] := Char(Byte(D[2]) and 1);
    D := #7 + D;
    Ext.extnValue.SetContent(D[1],3);
    Ext.extnID := id_ce_keyUsage;
    Ext.Critical := True;
    New(V);
    try
      FillChar(V^,SizeOf(TASN1Struct),0);
      TranslateExtension(Ext,V^);
      AddASN1Field(E^,V^);
      DisposeASN1Struct(V^);
    finally
      Dispose(V);
    end;
  finally
    Ext.Free;
  end;

  if Assigned(ExtKeyUsage) then begin
    Ext := TX509Extension.Create;
    try
      Ext.Critical := False;
      Ext.extnID := id_ce_extKeyUsage;
      NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      for I := 0 to ExtKeyUsage.Count - 1 do begin
        D := Trim(ExtKeyUsage[I]);
        if D <> '' then begin
          J := Pos('--',D);
          if J > 0 then
            D := Trim(Copy(D,1,J-1));
          if D <> '' then begin
            D := InterpretOIDtoBER(D);
            if Ext.extnValue.Contents <> nil then
              J := Ext.extnValue.Contents^.ItemCount
            else
              J := 0;
            AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
            Ext.extnValue.Contents^.Items[J].SetContent(D[1],Length(D));
          end;
        end;
      end;
      Ext.extnValue.CalculateLength;
      New(V);
      try
        FillChar(V^,SizeOf(TASN1Struct),0);
        TranslateExtension(Ext,V^);
        AddASN1Field(E^,V^);
        DisposeASN1Struct(V^);
      finally
        Dispose(V);
      end;
    finally
      Ext.Free;
    end;
  end;

  if Length(SubjectAltName) > 0 then begin
    Ext := TX509Extension.Create;
    try
      TranslateGeneralNames(SubjectAltName,Ext.extnValue);
      if Ext.extnValue.Contents <> nil then begin
        Ext.extnID := id_ce_subjectAltName;
        Ext.Critical := False;
        New(V);
        try
          FillChar(V^,SizeOf(TASN1Struct),0);
          TranslateExtension(Ext,V^);
          AddASN1Field(E^,V^);
          DisposeASN1Struct(V^);
        finally
          Dispose(V);
        end;
      end;
    finally
      Ext.Free;
    end;
  end;

  FillChar(Ext,SizeOf(Ext),0);
  if ExtractNamedExtension(CACert,id_ce_subjectAltName,Ext) = E_OK then begin
    if InterpretGeneralNames(Ext.extnValue,issuerAltName) = E_SYNTAX then
      SetLength(issuerAltName,0);
    if Length(IssuerAltName) > 0 then begin
      TranslateGeneralNames(IssuerAltName,Ext.extnValue);
      if Ext.extnValue.Contents <> nil then begin
        Ext.extnID := id_ce_issuerAltName;
        Ext.Critical := False;
        New(V);
        try
          FillChar(V^,SizeOf(TASN1Struct),0);
          TranslateExtension(Ext,V^);
          AddASN1Field(E^,V^);
          DisposeASN1Struct(V^);
        finally
          Dispose(V);
        end;
      end;
    end;
    Ext.Free;
  end;

  E^.CalculateLength;
  T^.CalculateLength;
  Cert.CalculateLength;
end;

procedure ComposeCertificate2(var Cert: TASN1Struct;
                              const SubjectName: TX501Name;
                              const SubjectPublicKey: TX509PublicKey;
                              const CACert: TASN1Struct;
                              const Validity: TX509Validity;
                              const KeyUsage: TKeyUsage;
                              const ExtKeyUsage: TStrings;
                              const SubjectAltName: TX509GeneralNames;
                              const Serial: string);
var
  T, V, S, E: PASN1Struct;
  Issuer: TX501Name;
  Ext: TX509Extension;
{$IFDEF SHA1}
  H: TSHA1;           
{$ENDIF SHA1}
  D: string;
  I, J: Integer;
  IssuerAltName: TX509GeneralNames;
begin
  if [] = KeyUsage then
    raise Exception.Create('KeyUsage must not be empty');

  Cert.Free;
  NewComposeASN1Struct(Cert,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  Cert.Persistent := True;
  Cert.TypeName := 'Certificate';
  T := Cert.AddField('tbsCertificate','TBSCertificate',nil);
  T.Tag := V_ASN1_SEQUENCE;
  T.AddField('version','[0] INTEGER',nil);
  T.FindField('/version/').EditContent('00');
  T.FindField('version').SetAsDefault;
  T.FindField('/version/').EditContent('02');

  T.AddField('serialNumber','INTEGER',nil).SetContent(Pointer(Serial)^,Length(Serial));

  V := T.AddField('signature','AlgorithmIdentifier',nil);
  V.Tag := V_ASN1_SEQUENCE;
  V.AddField('algorithm','OBJECT','',True);
  V.AddField('parameters','ANY',nil);
           
  Assert(Assigned(CACert));
  FillChar(Issuer,SizeOf(Issuer),0);
  ExtractSubject(CACert,Issuer);
  New(V);
  try
    V^:=  nil;
    TranslateName(Issuer,V^);
    T.AddField('issuer','Name',V^);
    T.CalculateLength;
    V^.Free;
  finally
    Dispose(V);
  end;

  V := T.AddField('validity','Validity',nil);
  V.Tag := V_ASN1_SEQUENCE;
  New(S);
  try
    FillChar(S^,SizeOf(TASN1Struct),0);
    DateTimeToASN1(Validity.notBefore,S^);
    V^.AddField('notBefore',S.TypeName,S^);
    DateTimeToASN1(Validity.notAfter,S^);
    V^.AddField('notAfter',S.TypeName,S^);
    DisposeASN1Struct(S^);
  finally
    Dispose(S);
  end;
  V^.CalculateLength;

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateName(SubjectName,V^);
    T.AddField('subject','Name',V^);

    TranslatePublicKey(SubjectPublicKey,V^);
    T.AddField('subjectPublicKeyInfo','SubjectPublicKeyInfo',V^);
                     
{$IFDEF SHA1}
    SetLength(D,20);
    H := TSHA1.Create(V^.Contents^.Items[1].Content[1],V^.Contents^.Items[1].Length-1);
    try
      H.Done(Pointer(D));
    finally
      H.Free;
    end;     
{$ELSE  SHA1}
    D := '';
{$ENDIF SHA1}

    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;

  E := T^.AddField('extensions','[3] EXTENSIONS OPTIONAL',nil);
  E^.Tag := V_ASN1_SEQUENCE;
  E^.CreateOFTemplate;
  E^.Template.TypeName := 'Extension';
  E^.Template.Persistent := True;
  E^.Template.Tag := V_ASN1_SEQUENCE;
  E^.Template.AddField('extnID','OBJECT',nil);
  E^.Template.AddField('critical','BOOLEAN','False',True);
  E^.Template.FindField('critical').SetAsDefault;
  E^.Template.AddField('extnValue','OCTET STRING',nil);

  FillChar(Ext,SizeOf(Ext),0);
  if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,Ext) = E_OK then begin
    Ext.Critical := False;
    Ext.extnID := id_ce_authorityKeyIdentifier;
    New(V);
    try
      V^ := TASN1Struct.Create;
      V^.Cls := V_ASN1_UNIVERSAL;
      V^.Constructed := True;
      V^.Tag := V_ASN1_SEQUENCE;
      AddComposeASN1Field(V^,V_ASN1_CONTEXT_SPECIFIC,False,0);
      S := @V^.Contents^.Items[0];
      S^.SetContent(Ext.extnValue.Content^,Ext.extnValue.Length);
      V^.CalculateLength;
      DisposeASN1Struct(Ext.extnValue);
      CopyASN1Struct(Ext.extnValue,V^);
      TranslateExtension(Ext,V^);
      TranslateExtension(Ext,V^);
      E.AddField('','Extension',V^);
      V^.Free;
    finally
      Dispose(V);
    end;
    DisposeASN1Struct(Ext.extnValue);
  end;
       
{$IFDEF SHA1}
  Ext := TX509Extension.Create;
  try
    NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_OCTET_STRING);
    Ext.extnValue.SetContent(Pointer(D)^,20);
    Ext.extnID := id_ce_subjectKeyIdentifier;
    Ext.Critical := False;
    New(V);
    try
      FillChar(V^,SizeOf(TASN1Struct),0);
      TranslateExtension(Ext,V^);
      E.AddField('','Extension',V^);
      V^.Free;
    finally
      Dispose(V);
    end;
  finally
    Ext.Free;
  end;       
{$ENDIF SHA1}

  if KeyUsage <> [Low(TKeyUsageItem)..High(TKeyUsageItem)] then begin
    Ext := TX509Extension.Create;
    try
      NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_BIT_STRING);
      SetLength(D,2);
      Move(KeyUsage,D[1],2);
      D[2] := Char(Byte(D[2]) shl 7);
      D := #7 + D;
      Ext.extnValue.SetContent(D[1],3);
      Ext.extnID := id_ce_keyUsage;
      Ext.Critical := True;
      New(V);
      try
        FillChar(V^,SizeOf(TASN1Struct),0);
        TranslateExtension(Ext,V^);
        E.AddField('','Extension',V^);
        V^.Free;
      finally
        Dispose(V);
      end;
    finally
      Ext.Free;
    end;
  end;

  if Assigned(ExtKeyUsage) then begin
    Ext := TX509Extension.Create;
    try
      Ext.Critical := False;
      Ext.extnID := id_ce_extKeyUsage;
      NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      for I := 0 to ExtKeyUsage.Count - 1 do begin
        D := Trim(ExtKeyUsage[I]);
        if D <> '' then begin
          J := Pos('--',D);
          if J > 0 then
            D := Trim(Copy(D,1,J-1));
          if D <> '' then begin
            D := InterpretOIDtoBER(D);
            if Ext.extnValue.Contents <> nil then
              J := Ext.extnValue.Contents^.ItemCount
            else
              J := 0;
            AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
            Ext.extnValue.Contents^.Items[J].SetContent(D[1],Length(D));
          end;
        end;
      end;
      Ext.extnValue.CalculateLength;
      New(V);
      try
        FillChar(V^,SizeOf(TASN1Struct),0);
        TranslateExtension(Ext,V^);
        E^.AddField('','Extension',V^);
        V^.Free;
      finally
        Dispose(V);
      end;
    finally
      Ext.Free;
    end;
  end;

  if Length(SubjectAltName) > 0 then begin
    Ext := TX509Extension.Create;
    try
      TranslateGeneralNames(SubjectAltName,Ext.extnValue);
      if Ext.extnValue.Contents <> nil then begin
        Ext.extnID := id_ce_subjectAltName;
        Ext.Critical := False;
        New(V);
        try
          FillChar(V^,SizeOf(TASN1Struct),0);
          TranslateExtension(Ext,V^);
          E^.AddField('','Extension',V^);
          V^.Free;
        finally
          Dispose(V);
        end;
      end;
    finally
      Ext.Free;
    end;
  end;

  FillChar(Ext,SizeOf(Ext),0);
  if ExtractNamedExtension(CACert,id_ce_subjectAltName,Ext) = E_OK then begin
    if InterpretGeneralNames(Ext.extnValue,issuerAltName) = E_SYNTAX then
      SetLength(issuerAltName,0);
    if Length(IssuerAltName) > 0 then begin
      TranslateGeneralNames(IssuerAltName,Ext.extnValue);
      if Ext.extnValue.Contents <> nil then begin
        Ext.extnID := id_ce_issuerAltName;
        Ext.Critical := False;
        New(V);
        try
          FillChar(V^,SizeOf(TASN1Struct),0);
          TranslateExtension(Ext,V^);
          E^.AddField('','Extension',V^);
          V^.Free;
        finally
          Dispose(V);
        end;
      end;
    end;
    Ext.Free;
  end;

  Cert.CalculateLength;
end;

procedure ComposeCACertificate(var Cert: TASN1Struct;
                               const SubjectName: TX501Name;
                               const SubjectPublicKey: TX509PublicKey;
                               const Validity: TX509Validity;
                               const KeyUsage: TKeyUsage;
                               const ExtKeyUsage: TStrings;
                               const SubjectAltName: TX509GeneralNames;
                               const CRLDistributionPoints: TX509CRLDistPoints;
                               PathLenConstraint: Cardinal;
                               const Serial: string = '');
var
  T, V, S, E: PASN1Struct;
  B: Byte;
  Ext: TX509Extension;  
{$IFDEF SHA1}
  H: TSHA1;  
{$ENDIF SHA1}
  D: string;
  I, J, Idx: Integer;
begin
  if not ([keyCertSign,cRLSign] <= KeyUsage) then
    raise Exception.Create('KeyUsage must include cRLSign and keyCertSign');

  if ASN1StructAssigned(Cert) then
    DisposeASN1Struct(Cert);
  NewComposeASN1Struct(Cert,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  AddComposeASN1Field(Cert,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  T := @Cert.Contents^.Items[0];

  AddComposeASN1Field(T^,V_ASN1_CONTEXT_SPECIFIC,True,0);
  V := @T^.Contents^.Items[0];
  AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  B := 2;
  V^.Contents^.Items[0].SetContent(B,1);
  V^.CalculateLength;

  AddComposeASN1Field(T^,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  if Serial <> '' then
    T^.Items[1].SetContent(Serial[1],Length(Serial));

  AddComposeASN1Field(T^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateName(SubjectName,V^);
    AddASN1Field(T^,V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;

  AddComposeASN1Field(T^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  V := @T^.Contents^.Items[4];
  New(S);
  try
    FillChar(S^,SizeOf(TASN1Struct),0);
    DateTimeToASN1(Validity.notBefore,S^);
    AddASN1Field(V^,S^);
    DateTimeToASN1(Validity.notAfter,S^);
    AddASN1Field(V^,S^);
    DisposeASN1Struct(S^);
  finally
    Dispose(S);
  end;
  V^.CalculateLength;

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateName(SubjectName,V^);
    AddASN1Field(T^,V^);

    TranslatePublicKey(SubjectPublicKey,V^);
    AddASN1Field(T^,V^);
     
{$IFDEF SHA1}
    SetLength(D,20);
    H := TSHA1.Create(V^.Contents^.Items[1].Content[1],V^.Contents^.Items[1].Length-1);
    try
      H.Done(Pointer(D));
    finally
      H.Free;
    end;     
{$ELSE  SHA1}
    D := '';
{$ENDIF SHA1}

    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;

  AddComposeASN1Field(T^,V_ASN1_CONTEXT_SPECIFIC,True,3);
  E := @T^.Contents^.Items[7];
  AddComposeASN1Field(E^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  E := @E^.Contents^.Items[0];
            
{$IFDEF SHA1}
  Ext := TX509Extension.Create;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_OCTET_STRING);
  Ext.extnValue.SetContent(D,20);
  Ext.extnID := id_ce_subjectKeyIdentifier;
  Ext.Critical := False;
  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateExtension(Ext,V^);
    AddASN1Field(E^,V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Ext.Free;  
{$ENDIF SHA1}

  Ext := TX509Extension.Create;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_BIT_STRING);
  SetLength(D,2);
  Move(KeyUsage,D[1],2);
  D[2] := Char(Byte(D[2]) and 1);
  D := #7 + D;
  Ext.extnValue.SetContent(D[1],3);
  Ext.extnID := id_ce_keyUsage;
  Ext.Critical := True;
  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateExtension(Ext,V^);
    AddASN1Field(E^,V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Ext.Free;

  if Assigned(ExtKeyUsage) and (ExtKeyUsage.Count > 0) then begin
    Ext := TX509Extension.Create;
    Ext.Critical := False;
    Ext.extnID := id_ce_extKeyUsage;
    NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
    for I := 0 to ExtKeyUsage.Count - 1 do begin
      D := Trim(ExtKeyUsage[I]);
      if D <> '' then begin
        J := Pos('--',D);
        if J > 0 then
          D := Trim(Copy(D,1,J-1));
        if D <> '' then begin
          D := InterpretOIDtoBER(D);
          if Ext.extnValue.Contents <> nil then
            J := Ext.extnValue.Contents^.ItemCount
          else
            J := 0;
          AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
          Ext.extnValue.Contents^.Items[J].SetContent(D[1],Length(D));
        end;
      end;
    end;
    Ext.extnValue.CalculateLength;
    New(V);
    try
      FillChar(V^,SizeOf(TASN1Struct),0);
      TranslateExtension(Ext,V^);
      AddASN1Field(E^,V^);
      DisposeASN1Struct(V^);
    finally
      Dispose(V);
    end;
    Ext.Free;
  end;

  if Length(SubjectAltName) > 0 then begin
    Ext := TX509Extension.Create;
    TranslateGeneralNames(SubjectAltName,Ext.extnValue);
    if Ext.extnValue.Contents <> nil then begin
      Ext.extnID := id_ce_subjectAltName;
      Ext.Critical := False;
      New(V);
      try
        FillChar(V^,SizeOf(TASN1Struct),0);
        TranslateExtension(Ext,V^);
        AddASN1Field(E^,V^);
        DisposeASN1Struct(V^);
      finally
        Dispose(V);
      end;
    end;
    Ext.Free;
  end;

  Ext := TX509Extension.Create;
  Ext.Critical := False;
  Ext.extnID := id_ce_basicConstraints;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_BOOLEAN);
  B := $FF;
  Ext.extnValue.Contents^.Items[0].SetContent(B,1);
  if PathLenConstraint < $FFFFFFFF then begin
    AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
    D := '';
    while PathLenConstraint > 0 do begin
      B := pathLenConstraint and $FF;
      pathLenConstraint := pathLenConstraint shr 8;
      D := Char(B) + D;
    end;
    if D = '' then D := #0;
    Ext.extnValue.Contents^.Items[1].SetContent(D[1],Length(D));
  end;
  Ext.extnValue.CalculateLength;
  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateExtension(Ext,V^);
    AddASN1Field(E^,V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Ext.Free;

  if Length(CRLDistributionPoints) > 0 then begin
    Ext := TX509Extension.Create;
    Ext.Critical := False;
    Ext.extnID := id_ce_cRLDistributionPoints;
    NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
    for I := 0 to Length(CRLDistributionPoints)-1 do begin
      AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      S := @Ext.extnValue.Contents^.Items[I];
      Idx := 0;
      if CRLDistributionPoints[I].distributionPoint.Tag = 0 then begin
        if Length(CRLDistributionPoints[I].distributionPoint.fullName) > 0 then begin
          AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,0);
          S := @S^.Contents^.Items[Idx];
          Inc(Idx);
          AddComposeASN1Field(S^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
          TranslateGeneralNames(CRLDistributionPoints[I].distributionPoint.fullName,S^.Contents^.Items[0]);
          S^.Items[0].Persistent := True;
          S^.Items[0].Cls := V_ASN1_CONTEXT_SPECIFIC;
          S^.Items[0].Tag := 0;
          S^.Items[0].Implicit := True;
          S^.Items[0].ImplicitTag := V_ASN1_SEQUENCE;
        end;
      end else if CRLDistributionPoints[I].distributionPoint.Tag = 1 then begin
        // Note: Use PKIX-Cert instead.
        raise Exception.Create('Not supported');
        AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,0);
        S := @S^.Contents^.Items[Idx];
        Inc(Idx);
        AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,1);
        V := @S^.Contents^.Items[0];
        AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        TranslateName(CRLDistributionPoints[I].distributionPoint.nameRelativeToCRLIssuer,V^.Contents^.Items[0]);
      end;
      S := @Ext.extnValue.Contents^.Items[I];
      if CRLDistributionPoints[I].reasons <> [LowReasonFlag..High(TX509ReasonFlag)] then begin
        SetLength(D,SizeOf(TX509ReasonFlags));
        Move(CRLDistributionPoints[I].reasons,D[1],Length(D));
        D := Char(UnusedReasonFlagBits) + D;
        AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,1);
        S := @S^.Contents^.Items[Idx];
        Inc(Idx);
        AddComposeASN1Field(S^,V_ASN1_UNIVERSAL,False,V_ASN1_BIT_STRING);
        S^.Contents^.Items[0].SetContent(D[1],Length(D));
      end;
      S := @Ext.extnValue.Contents^.Items[I];
      if Length(CRLDistributionPoints[I].CRLIssuer) > 0 then begin
        AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,2);
        S := @S^.Contents^.Items[Idx];
        AddComposeASN1Field(S^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        TranslateGeneralNames(CRLDistributionPoints[I].CRLIssuer,S^.Contents^.Items[0]);
      end;

      Ext.extnValue.Contents^.Items[I].CalculateLength;
    end;
    New(V);
    try
      FillChar(V^,SizeOf(TASN1Struct),0);
      TranslateExtension(Ext,V^);
      AddASN1Field(E^,V^);
      DisposeASN1Struct(V^);
    finally
      Dispose(V);
    end;
    Ext.Free;
  end;

  Cert.CalculateLength;
end;  

procedure ComposeCACertificate2(var Cert: TASN1Struct;
                                const SubjectName: TX501Name;
                                const SubjectPublicKey: TX509PublicKey;
                                const Validity: TX509Validity;
                                const KeyUsage: TKeyUsage;
                                const ExtKeyUsage: TStrings;
                                const SubjectAltName: TX509GeneralNames;
                                const CRLDistributionPoints: TX509CRLDistPoints;
                                PathLenConstraint: Cardinal;
                                const Serial: string = '';
                                const CACert: TASN1Struct = nil);
var
  T, V, S, E: PASN1Struct;
  B: Byte;
  Ext: TX509Extension; 
{$IFDEF SHA1}
  H: TSHA1;  
{$ENDIF SHA1}
  D: string;
  I, J, Idx: Integer;
  Issuer: TX501Name;
  IssuerAltName: TX509GeneralNames;
begin
  if not ([keyCertSign,cRLSign] <= KeyUsage) then
    raise Exception.Create('KeyUsage must include cRLSign and keyCertSign');

  Cert.Free;
  NewComposeASN1Struct(Cert,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  Cert.Persistent := True;
  Cert.TypeName := 'Certificate';
  T := Cert.AddField('tbsCertificate','TBSCertificate',nil);
  T.Tag := V_ASN1_SEQUENCE;
  T.AddField('version','[0] INTEGER',nil);
  T.FindField('/version/').EditContent('00');
  T.FindField('version').SetAsDefault;
  T.FindField('/version/').EditContent('02');

  T.AddField('serialNumber','INTEGER',Serial,True);

  V := T.AddField('signature','AlgorithmIdentifier',nil);
  V.Tag := V_ASN1_SEQUENCE;
  V.AddField('algorithm','OBJECT','',True);
  V.AddField('parameters','ANY',nil);

  if Assigned(CACert) then begin
    FillChar(Issuer,SizeOf(Issuer),0);
    ExtractSubject(CACert,Issuer);
    New(V);
    try
      V^:=  nil;
      TranslateName(Issuer,V^);
      T.AddField('issuer','Name',V^);
      T.CalculateLength;
      V^.Free;
    finally
      Dispose(V);
    end;
  end else begin
    New(V);
    try
      FillChar(V^,SizeOf(TASN1Struct),0);
      TranslateName(SubjectName,V^);
      T.AddField('issuer','Name',V^);
      V^.Free;
    finally
      Dispose(V);
    end;
  end;

  V := T.AddField('validity','Validity',nil);
  V.Tag := V_ASN1_SEQUENCE;
  New(S);
  try
    FillChar(S^,SizeOf(TASN1Struct),0);
    DateTimeToASN1(Validity.notBefore,S^);
    V^.AddField('notBefore',S.TypeName,S^);
    DateTimeToASN1(Validity.notAfter,S^);
    V^.AddField('notAfter',S.TypeName,S^);
    DisposeASN1Struct(S^);
  finally
    Dispose(S);
  end;
  V^.CalculateLength;

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateName(SubjectName,V^);
    T.AddField('subject','Name',V^);

    TranslatePublicKey(SubjectPublicKey,V^);
    T.AddField('subjectPublicKeyInfo','SubjectPublicKeyInfo',V^);
     
{$IFDEF SHA1}
    SetLength(D,20);
    H := TSHA1.Create(V^.Contents^.Items[1].Content[1],V^.Contents^.Items[1].Length-1);
    try
      H.Done(Pointer(D));
    finally
      H.Free;
    end;     
{$ELSE SHA1}
    D := '';
{$ENDIF SHA1}

    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;

  E := T^.AddField('extensions','[3] EXTENSIONS OPTIONAL',nil);
  E^.Tag := V_ASN1_SEQUENCE;
  E^.CreateOFTemplate;
  E^.Template.TypeName := 'Extension';
  E^.Template.Persistent := True;
  E^.Template.Tag := V_ASN1_SEQUENCE;
  E^.Template.AddField('extnID','OBJECT',nil);
  E^.Template.AddField('critical','BOOLEAN','False',True);
  E^.Template.FindField('critical').SetAsDefault;
  E^.Template.AddField('extnValue','OCTET STRING',nil);
       
{$IFDEF SHA1}
  Ext := TX509Extension.Create;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_OCTET_STRING);
  Ext.extnValue.SetContent(D[1],20);
  Ext.extnID := id_ce_subjectKeyIdentifier;
  Ext.Critical := False;
  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateExtension(Ext,V^);
    E^.AddField('','Extension',V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Ext.Free;  
{$ENDIF SHA1}

  Ext := TX509Extension.Create;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_BIT_STRING);
  SetLength(D,2);
  Move(KeyUsage,D[1],2);
  D[2] := Char(Byte(D[2]) and 1);
  D := #7 + D;
  Ext.extnValue.SetContent(D[1],3);
  Ext.extnID := id_ce_keyUsage;
  Ext.Critical := True;
  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateExtension(Ext,V^);
    E^.AddField('','Extension',V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Ext.Free;

  if Assigned(ExtKeyUsage) and (ExtKeyUsage.Count > 0) then begin
    Ext := TX509Extension.Create;
    Ext.Critical := False;
    Ext.extnID := id_ce_extKeyUsage;
    NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
    for I := 0 to ExtKeyUsage.Count - 1 do begin
      D := Trim(ExtKeyUsage[I]);
      if D <> '' then begin
        J := Pos('--',D);
        if J > 0 then
          D := Trim(Copy(D,1,J-1));
        if D <> '' then begin
          D := InterpretOIDtoBER(D);
          if Ext.extnValue.Contents <> nil then
            J := Ext.extnValue.Contents^.ItemCount
          else
            J := 0;
          AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
          Ext.extnValue.Contents^.Items[J].SetContent(D[1],Length(D));
        end;
      end;
    end;
    Ext.extnValue.CalculateLength;
    New(V);
    try
      FillChar(V^,SizeOf(TASN1Struct),0);
      TranslateExtension(Ext,V^);
      E^.AddField('','Extension',V^);
      DisposeASN1Struct(V^);
    finally
      Dispose(V);
    end;
    Ext.Free;
  end;

  if Length(SubjectAltName) > 0 then begin
    Ext := TX509Extension.Create;
    TranslateGeneralNames(SubjectAltName,Ext.extnValue);
    if Ext.extnValue.Contents <> nil then begin
      Ext.extnID := id_ce_subjectAltName;
      Ext.Critical := False;
      New(V);
      try
        FillChar(V^,SizeOf(TASN1Struct),0);
        TranslateExtension(Ext,V^);
        E^.AddField('','Extension',V^);
        DisposeASN1Struct(V^);
      finally
        Dispose(V);
      end;
    end;
    Ext.Free;
  end;

  Ext := TX509Extension.Create;
  Ext.Critical := False;
  Ext.extnID := id_ce_basicConstraints;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_BOOLEAN);
  B := $FF;
  Ext.extnValue.Contents^.Items[0].SetContent(B,1);
  if PathLenConstraint < $FFFFFFFF then begin
    AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
    D := '';
    while PathLenConstraint > 0 do begin
      B := pathLenConstraint and $FF;
      pathLenConstraint := pathLenConstraint shr 8;
      D := Char(B) + D;
    end;
    if D = '' then D := #0;
    Ext.extnValue.Contents^.Items[1].SetContent(D[1],Length(D));
  end;
  Ext.extnValue.CalculateLength;
  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateExtension(Ext,V^);
    E^.AddField('','Extension',V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Ext.Free;

  if Length(CRLDistributionPoints) > 0 then begin
    Ext := TX509Extension.Create;
    Ext.Critical := False;
    Ext.extnID := id_ce_cRLDistributionPoints;
    NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
    for I := 0 to Length(CRLDistributionPoints)-1 do begin
      AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      S := @Ext.extnValue.Contents^.Items[I];
      Idx := 0;
      if CRLDistributionPoints[I].distributionPoint.Tag = 0 then begin
        if Length(CRLDistributionPoints[I].distributionPoint.fullName) > 0 then begin
          AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,0);
          S := @S^.Contents^.Items[Idx];
          Inc(Idx);
          AddComposeASN1Field(S^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
          TranslateGeneralNames(CRLDistributionPoints[I].distributionPoint.fullName,S^.Contents^.Items[0]);
          S^.Items[0].Persistent := True;
          S^.Items[0].Cls := V_ASN1_CONTEXT_SPECIFIC;
          S^.Items[0].Tag := 0;
          S^.Items[0].Implicit := True;
          S^.Items[0].ImplicitTag := V_ASN1_SEQUENCE;
        end;
      end else if CRLDistributionPoints[I].distributionPoint.Tag = 1 then begin
        // Note: Use PKIX-Cert instead.
        raise Exception.Create('Not supported');
        AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,0);
        S := @S^.Contents^.Items[Idx];
        Inc(Idx);
        AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,1);
        TranslateName(CRLDistributionPoints[I].distributionPoint.nameRelativeToCRLIssuer,S^.Contents^.Items[0]);
        S^.Items[0].Persistent := True;
        S^.Items[0].Cls := V_ASN1_CONTEXT_SPECIFIC;
        S^.Items[0].Tag := 0;
        S^.Items[0].Implicit := True;
        S^.Items[0].ImplicitTag := V_ASN1_SEQUENCE;
      end;
      S := @Ext.extnValue.Contents^.Items[I];
      if CRLDistributionPoints[I].reasons <> [LowReasonFlag..High(TX509ReasonFlag)] then begin
        SetLength(D,SizeOf(TX509ReasonFlags));
        Move(CRLDistributionPoints[I].reasons,D[1],Length(D));
        D := Char(UnusedReasonFlagBits) + D;
        AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,1);
        S := @S^.Contents^.Items[Idx];
        Inc(Idx);
        AddComposeASN1Field(S^,V_ASN1_UNIVERSAL,False,V_ASN1_BIT_STRING);
        S^.Contents^.Items[0].SetContent(D[1],Length(D));
      end;
      S := @Ext.extnValue.Contents^.Items[I];
      if Length(CRLDistributionPoints[I].CRLIssuer) > 0 then begin
        AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,2);
        S := @S^.Contents^.Items[Idx];
        AddComposeASN1Field(S^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        TranslateGeneralNames(CRLDistributionPoints[I].CRLIssuer,S^.Contents^.Items[0]);
      end;

      Ext.extnValue.Contents^.Items[I].CalculateLength;
    end;
    New(V);
    try
      FillChar(V^,SizeOf(TASN1Struct),0);
      TranslateExtension(Ext,V^);
      E^.AddField('','Extension',V^);
      DisposeASN1Struct(V^);
    finally
      Dispose(V);
    end;
    Ext.Free;
  end;

  FillChar(Ext,SizeOf(Ext),0);
  if Assigned(CACert) and
     (ExtractNamedExtension(CACert,id_ce_subjectAltName,Ext) = E_OK) then begin
    if InterpretGeneralNames(Ext.extnValue,issuerAltName) = E_SYNTAX then
      SetLength(issuerAltName,0);
    if Length(IssuerAltName) > 0 then begin
      TranslateGeneralNames(IssuerAltName,Ext.extnValue);
      if Ext.extnValue.Contents <> nil then begin
        Ext.extnID := id_ce_issuerAltName;
        Ext.Critical := False;
        New(V);
        try
          FillChar(V^,SizeOf(TASN1Struct),0);
          TranslateExtension(Ext,V^);
          E^.AddField('','Extension',V^);
          V^.Free;
        finally
          Dispose(V);
        end;
      end;
    end;
    Ext.Free;
  end;

  Cert.CalculateLength;
end;

procedure ComposeCRL(var CRL: TASN1Struct;
                     const CACert: TASN1Struct;
                     ThisUpdate, NextUpdate: TDateTime;
                     const RCList: TX509RevokedCertificates;
                     CRLNumber: Cardinal;
                     BaseCRLNumber: Cardinal;
                     issuingDistributionPoint: TX509issuingDistributionPoint);
var
  I, J, Idx: Integer;
  S, T, V, R, E: PASN1Struct;
  B: Byte;
  Str: string;
  Ext: TX509Extension;
  IssuerAltName: TX509GeneralNames;
  Res: Integer;
begin
  Assert(CRLNumber >= BaseCRLNumber);

  if ASN1StructAssigned(CRL) then
    DisposeASN1Struct(CRL);
  NewComposeASN1Struct(CRL,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  CRL.Persistent := True;
  CRL.TypeName := 'Certificate';
  S := CRL.AddField('tbsCertList','TBSCertList',nil);
  T := CRL.AddField('signatureAlgorithm','AlgorithmIdentifier',nil);
  T.Persistent := True;
  T.Tag := V_ASN1_SEQUENCE;
  CRL.AddField('signature','BIT STRING',nil);
  S.Tag := V_ASN1_SEQUENCE;
  T := S.AddField('version','INTEGER','00',True);
  T.SetAsDefault;
  T.EditContent('01');

  V := S.AddField('signature','AlgorithmIdentifier',nil);
  V.Tag := V_ASN1_SEQUENCE;
  V.AddField('algorithm','OBJECT','',True);
  V.AddField('parameters','ANY OPTIONAL',nil);

  Assert(Assigned(CACert));
  V := nil;
  Res := ExtractSubjectStruct(CACert,V);
  Assert(Res = E_OK);
  S.AddField('issuer','Name',V^);

  New(T);
  try
    FillChar(T^,SizeOf(T^),0);
    try
      DateTimeToASN1(ThisUpdate,T^);
      S.AddField('thisUpdate','',T^);
      if NextUpdate > ThisUpdate then begin
        DateTimeToASN1(NextUpdate,T^);
        S.AddField('nextUpdate','',T^);
      end;
    finally
      DisposeASN1Struct(T^);
    end;
  finally
    Dispose(T);
  end;

  if Assigned(RCList) and (RCList.Count > 0) then begin
    V := S.AddField('revokedCertificates','RevokedCertificates OPTIONAL',nil);
    V.Tag := V_ASN1_SEQUENCE;
    V.CreateOFTemplate;
    V.Template.Tag := V_ASN1_SEQUENCE;
    V.Template.Constructed := True;
    V.Template.Persistent := True;
    V.Template.TypeName := 'RevokedCertificate';
    V.Template.AddField('userCertificate','INTEGER','',True);
    V.Template.AddField('revocationDate','Date','',True);
    T := V.Template.AddField('crlEntryExtensions','EntryExtensions OPTIONAL',nil);
    T.Persistent := True;
    T.Tag := V_ASN1_SEQUENCE;
    T.Constructed := True;
    T.CreateOFTemplate;
    T.Template.Tag := V_ASN1_SEQUENCE;
    T.Template.Constructed := True;
    T.Template.Persistent := True;
    T.Template.TypeName := 'EntryExtension';
    T.Template.AddField('extnID','OBJECT','',True);
    T.Template.AddField('critical','BOOLEAN','False',True);
    T.Template.FindField('critical').SetAsDefault;
    T.Template.AddField('extnValue','OCTET STRING','',True);
    for I := 0 to RCList.Count - 1 do begin
      R := V.AddField;
      Str := RCList[I].userCertificate;
      R.FindField('userCertificate').SetContent(Str[1],Length(Str));
      New(T);
      try
        FillChar(T^,SizeOf(T^),0);
        try
          DateTimeToASN1(RCList[I].revocationDate,T^);
          E := R.FindField('revocationDate');
          E.Assign(T^);
          E.VarName := 'revocationDate';
          if RCList[I].Count > 0 then begin
            E := R.FindField('crlEntryExtensions');
            for J := 0 to RCList[I].Count - 1 do begin
              TranslateExtension(RCList[I].crlEntryExtensions[J],T^);
              E^.AddField.Assign(T^);
            end;
          end;
        finally
          DisposeASN1Struct(T^);
        end;
      finally
        Dispose(T);
      end;
    end;
  end;

  E := S^.AddField('extensions','[0] EXTENSIONS OPTIONAL',nil);
  E^.Tag := V_ASN1_SEQUENCE;
  E^.CreateOFTemplate;
  E^.Template.TypeName := 'Extension';
  E^.Template.Persistent := True;
  E^.Template.Tag := V_ASN1_SEQUENCE;
  E^.Template.Constructed := True;
  E^.Template.AddField('extnID','OBJECT',nil);
  E^.Template.AddField('critical','BOOLEAN','False',True);
  E^.Template.FindField('critical').SetAsDefault;
  E^.Template.AddField('extnValue','OCTET STRING',nil);

  Ext := TX509Extension.Create;
  if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,Ext) = E_OK then begin
    Ext.Critical := False;
    Ext.extnID := id_ce_authorityKeyIdentifier;
    New(V);
    try
      V^ := nil;
      NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      AddComposeASN1Field(V^,V_ASN1_CONTEXT_SPECIFIC,False,0);
      S := @V^.Contents^.Items[0];
      S^.SetContent(Ext.extnValue.Content^,Ext.extnValue.Length);
      V^.CalculateLength;
      CopyASN1Struct(Ext.extnValue,V^);
      TranslateExtension(Ext,V^);
      E.AddField.Assign(V^);
      DisposeASN1Struct(V^);
    finally
      Dispose(V);
    end;
  end;
  Ext.Free;

  Ext := TX509Extension.Create;
  if ExtractNamedExtension(CACert,id_ce_subjectAltName,Ext) = E_OK then begin
    if InterpretGeneralNames(Ext.extnValue,issuerAltName) = E_SYNTAX then
      SetLength(issuerAltName,0);
    if Length(IssuerAltName) > 0 then begin
      TranslateGeneralNames(IssuerAltName,Ext.extnValue);
      if Ext.extnValue.Contents <> nil then begin
        Ext.extnID := id_ce_issuerAltName;
        Ext.Critical := False;
        New(V);
        try
          FillChar(V^,SizeOf(TASN1Struct),0);
          TranslateExtension(Ext,V^);
          E.AddField.Assign(V^);
          DisposeASN1Struct(V^);
        finally
          Dispose(V);
        end;
      end;
    end;
  end;
  Ext.Free;

  Ext := TX509Extension.Create;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  I := CRLNumber;
  Str := '';
  while I > 0 do begin
    B := I and $FF;
    I := I shr 8;
    Str := Char(B) + Str;
  end;
  if Str = '' then
    Str := #0
  else if Byte(Str[1]) and $80 > 0 then
    Str := #0 + Str;
  Ext.extnValue.SetContent(Str[1],Length(Str));
  Ext.extnID := id_ce_cRLNumber;
  Ext.Critical := False;
  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateExtension(Ext,V^);
    E.AddField.Assign(V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Ext.Free;

  if BaseCRLNumber < CRLNumber then begin
    Ext := TX509Extension.Create;
    NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
    I := BaseCRLNumber;
    Str := '';
    while I > 0 do begin
      B := I and $FF;
      I := I shr 8;
      Str := Char(B) + Str;
    end;
    if Str = '' then
      Str := #0
    else if Byte(Str[1]) and $80 > 0 then
      Str := #0 + Str;
    Ext.extnValue.SetContent(Str[1],Length(Str));
    Ext.extnID := id_ce_deltaCRLIndicator;
    Ext.Critical := False;
    New(V);
    try
      FillChar(V^,SizeOf(TASN1Struct),0);
      TranslateExtension(Ext,V^);
      E.AddField.Assign(V^);
      DisposeASN1Struct(V^);
    finally
      Dispose(V);
    end;
    Ext.Free;
  end;

  Ext := TX509Extension.Create;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  Ext.extnID := id_ce_issuingDistributionPoint;
  Ext.Critical := True;
  Idx := 0;
  if issuingDistributionPoint.distributionPoint.Tag = 0 then begin
    AddComposeASN1Field(Ext.extnValue,V_ASN1_CONTEXT_SPECIFIC,True,0);
    R := @Ext.extnValue.Contents^.Items[Idx];
    Inc(Idx);
    New(V);
    try
      FillChar(V^,SizeOf(TASN1Struct),0);
      TranslateGeneralNames(issuingDistributionPoint.distributionPoint.fullName,V^);
      V^.Persistent := True;
      V^.Cls := V_ASN1_CONTEXT_SPECIFIC;
      V^.Tag := 0;
      V^.Implicit := True;
      V^.ImplicitTag := V_ASN1_SEQUENCE;
      AddASN1Field(R^,V^);
      DisposeASN1Struct(V^);
    finally
      Dispose(V);
    end;
  end else if issuingDistributionPoint.distributionPoint.Tag = 1 then begin
    raise Exception.Create('Not supported');  // Use PKIX-CRL instead
  end;
  if issuingDistributionPoint.onlyContainsUserCerts then begin
    AddComposeASN1Field(Ext.extnValue,V_ASN1_CONTEXT_SPECIFIC,False,1);
    R := @Ext.extnValue.Contents^.Items[Idx];
    Inc(Idx);
    B := $FF;
    R^.SetContent(B,1);
  end else if issuingDistributionPoint.onlyContainsCACerts then begin
    AddComposeASN1Field(Ext.extnValue,V_ASN1_CONTEXT_SPECIFIC,False,2);
    R := @Ext.extnValue.Contents^.Items[Idx];
    Inc(Idx);
    B := $FF;
    R^.SetContent(B,1);
  end;
  if issuingDistributionPoint.onlySomeReasons <> [lowReasonFlag..High(TX509ReasonFlag)] then begin
    AddComposeASN1Field(Ext.extnValue,V_ASN1_CONTEXT_SPECIFIC,False,3);
    R := @Ext.extnValue.Contents^.Items[Idx];
    Inc(Idx);
    Str := StringOfChar(#0,SizeOf(TX509ReasonFlags));
    Move(issuingDistributionPoint.onlySomeReasons,Str[1],Length(Str));
    while (Str <> '') and (Str[Length(Str)] = #0) do
      Delete(Str,Length(Str),1);
    if Str = '' then
      Str := #0#0
    else begin
      B := 0;
      while Byte(Str[Length(Str)]) and (1 shl B) = 0 do
        Inc(B);
      Str := Char(B) + Str;
    end;
    R^.SetContent(Str[1],Length(Str));
  end;
  if issuingDistributionPoint.indirectCRL then begin
    AddComposeASN1Field(Ext.extnValue,V_ASN1_CONTEXT_SPECIFIC,False,4);
    R := @Ext.extnValue.Contents^.Items[Idx];
    B := $FF;
    R^.SetContent(B,1);
  end;
  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateExtension(Ext,V^);
    E.AddField.Assign(V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Ext.Free;
  CRL.CalculateLength;
end;

{$IFDEF SHA1}
procedure RSAPrivateKeyToStream(AStream: TStream;
                                const Password; PWLen: Integer;
                                const PrivKey: TIFPrivateKey);
var
  C: TRijndael_CTR;
  CK, PW: PChar;
  MS: TSecureMemoryStream;
  B: array [0..15] of Byte;
  I, Len: Integer;
  S: TASN1Struct;
begin
  S := nil;
  NewComposeASN1Struct(S,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  try
    case PrivKey.ReprType of
      0: S.AllocContents(3);
      1: S.AllocContents(6);
      2: S.AllocContents(4);
    end;
    for I := 0 to S.Contents^.ItemCount - 1 do begin
      S.Contents^.Items[I].Tag := V_ASN1_INTEGER;
      S.Contents^.Items[I].Cls := V_ASN1_UNIVERSAL;
      S.Contents^.Items[I].Constructed := False;
    end;
    B[0] := PrivKey.ReprType;
    case PrivKey.Scheme of
      ifRSA2: B[0] := B[0] or $4;
      ifRW:   B[0] := B[0] or $C;
    end;
    S.Contents^.Items[0].SetContent(B[0],1);
    if PrivKey.ReprType = 0 then begin
      Len := (MPMSB(PrivKey.zN) + 8) shr 3;
      GetMem(CK,Len);
      try
        MPIntToBase256(PrivKey.zN,CK^,Len);
        S.Contents^.Items[1].SetContent(CK^,Len);
        ProtectClear(CK^,Len);
      finally
        FreeMem(CK);
      end;
      Len := (MPMSB(PrivKey.zD) + 8) shr 3;
      GetMem(CK,Len);
      try
        MPIntToBase256(PrivKey.zD,CK^,Len);
        S.Contents^.Items[2].SetContent(CK^,Len);
        ProtectClear(CK^,Len);
      finally
        FreeMem(CK);
      end;
    end else if PrivKey.ReprType = 1 then begin
      Len := (MPMSB(PrivKey.oP) + 8) shr 3;
      GetMem(CK,Len);
      try
        MPIntToBase256(PrivKey.oP,CK^,Len);
        S.Contents^.Items[1].SetContent(CK^,Len);
        ProtectClear(CK^,Len);
      finally
        FreeMem(CK);
      end;
      Len := (MPMSB(PrivKey.oQ) + 8) shr 3;
      GetMem(CK,Len);
      try
        MPIntToBase256(PrivKey.oQ,CK^,Len);
        S.Contents^.Items[2].SetContent(CK^,Len);
        ProtectClear(CK^,Len);
      finally
        FreeMem(CK);
      end;
      Len := (MPMSB(PrivKey.oD1) + 8) shr 3;
      GetMem(CK,Len);
      try
        MPIntToBase256(PrivKey.oD1,CK^,Len);
        S.Contents^.Items[3].SetContent(CK^,Len);
        ProtectClear(CK^,Len);
      finally
        FreeMem(CK);
      end;
      Len := (MPMSB(PrivKey.oD2) + 8) shr 3;
      GetMem(CK,Len);
      try
        MPIntToBase256(PrivKey.oD2,CK^,Len);
        S.Contents^.Items[4].SetContent(CK^,Len);
        ProtectClear(CK^,Len);
      finally
        FreeMem(CK);
      end;
      Len := (MPMSB(PrivKey.oC) + 8) shr 3;
      GetMem(CK,Len);
      try
        MPIntToBase256(PrivKey.oC,CK^,Len);
        S.Contents^.Items[5].SetContent(CK^,Len);
        ProtectClear(CK^,Len);
      finally
        FreeMem(CK);
      end;
    end else if PrivKey.ReprType = 2 then begin
      Len := (MPMSB(PrivKey.tP) + 8) shr 3;
      GetMem(CK,Len);
      try
        MPIntToBase256(PrivKey.tP,CK^,Len);
        S.Contents^.Items[1].SetContent(CK^,Len);
        ProtectClear(CK^,Len);
      finally
        FreeMem(CK);
      end;
      Len := (MPMSB(PrivKey.tQ) + 8) shr 3;
      GetMem(CK,Len);
      try
        MPIntToBase256(PrivKey.tQ,CK^,Len);
        S.Contents^.Items[2].SetContent(CK^,Len);
        ProtectClear(CK^,Len);
      finally
        FreeMem(CK);
      end;
      Len := (MPMSB(PrivKey.tD) + 8) shr 3;
      GetMem(CK,Len);
      try
        MPIntToBase256(PrivKey.tD,CK^,Len);
        S.Contents^.Items[3].SetContent(CK^,Len);
        ProtectClear(CK^,Len);
      finally
        FreeMem(CK);
      end;
    end;
    S.CalculateLength;
    MS := TSecureMemoryStream.Create;
    try
      ASN1ToStream(S,MS);
      GetMem(PW,PWLen + 16);
      try
        RawRandom(PW^,16*8);
        try
          Move(Password,PW[16],PWLen);
          GetMem(CK,64);
          try
            WeakPasswordToPrivKey(PW^,PWLen+16,$1000,CK^,64);
            try
              HMAC(CK^,64,MS.Memory^,MS.Size,haSHA1,B,16);
              MS.Write(B,16);
              WeakPasswordToPrivKey(PW^,PWLen+16,$8000,CK^,48);
              C := TRijndael_CTR.Create(CK^,48,1);
              try
                FillChar(B,16,0);
                Len := MS.Size;
                for I := 15 downto 12 do begin
                  B[I] := Len and $FF;
                  Len := Len shr 8;
                end;
                C.Encrypt(B,16);
                C.Encrypt(MS.Memory^,MS.Size);
              finally
                C.Free;
              end;
            finally
              ProtectClear(CK^,64);
            end;
          finally
            FreeMem(CK);
          end;
          AStream.Write(PW^,16);
          AStream.Write(B,16);
          AStream.CopyFrom(MS,0);
        finally
          ProtectClear(PW^,PWLen + 16);
        end;
      finally
        FreeMem(PW);
      end;
    finally
      MS.Free;
    end;
  finally
    DisposeASN1Struct(S);
  end;
end;

procedure DLPrivateKeyToStream(AStream: TStream;
                               const Password; PWLen: Integer;
                               const PrivKey: TDLPrivateKey);
var
  C: TRijndael_CTR;
  CK, PW: PChar;
  MS: TSecureMemoryStream;
  B: array [0..15] of Byte;
  I, Len: Integer;
  S: TASN1Struct;
begin
  NewComposeASN1Struct(S,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  try
    S.AllocContents(6);
    for I := 0 to S.Contents^.ItemCount - 1 do begin
      S.Contents^.Items[I].Tag := V_ASN1_INTEGER;
      S.Contents^.Items[I].Cls := V_ASN1_UNIVERSAL;
      S.Contents^.Items[I].Constructed := False;
    end;
    B[0] := Ord(PrivKey.SignScheme) or (Ord(PrivKey.KAScheme) shl 3) or $80;
    S.Contents^.Items[0].SetContent(B[0],1);
    Len := (MPMSB(PrivKey.Params.P) + 8) shr 3;
    GetMem(CK,Len);
    try
      MPIntToBase256(PrivKey.Params.P,CK^,Len);
      S.Contents^.Items[1].SetContent(CK^,Len);
      ProtectClear(CK^,Len);
    finally
      FreeMem(CK);
    end;
    Len := (MPMSB(PrivKey.Params.Q) + 8) shr 3;
    GetMem(CK,Len);
    try
      MPIntToBase256(PrivKey.Params.Q,CK^,Len);
      S.Contents^.Items[2].SetContent(CK^,Len);
      ProtectClear(CK^,Len);
    finally
      FreeMem(CK);
    end;
    Len := (MPMSB(PrivKey.Params.G) + 8) shr 3;
    GetMem(CK,Len);
    try
      MPIntToBase256(PrivKey.Params.G,CK^,Len);
      S.Contents^.Items[3].SetContent(CK^,Len);
      ProtectClear(CK^,Len);
    finally
      FreeMem(CK);
    end;
    Len := (MPMSB(PrivKey.Params.J) + 8) shr 3;
    GetMem(CK,Len);
    try
      MPIntToBase256(PrivKey.Params.J,CK^,Len);
      S.Contents^.Items[4].SetContent(CK^,Len);
      ProtectClear(CK^,Len);
    finally
      FreeMem(CK);
    end;
    Len := (MPMSB(PrivKey.X) + 8) shr 3;
    GetMem(CK,Len);
    try
      MPIntToBase256(PrivKey.X,CK^,Len);
      S.Contents^.Items[5].SetContent(CK^,Len);
      ProtectClear(CK^,Len);
    finally
      FreeMem(CK);
    end;
    S.CalculateLength;
    MS := TSecureMemoryStream.Create;
    try
      ASN1ToStream(S,MS);
      GetMem(PW,PWLen + 16);
      try
        RawRandom(PW^,16*8);
        try
          Move(Password,PW[16],PWLen);
          GetMem(CK,64);
          try
            WeakPasswordToPrivKey(PW^,PWLen+16,$1000,CK^,64);
            try
              HMAC(CK^,64,MS.Memory^,MS.Size,haSHA1,B,16);
              MS.Write(B,16);
              WeakPasswordToPrivKey(PW^,PWLen+16,$8000,CK^,48);
              C := TRijndael_CTR.Create(CK^,48,1);
              try
                FillChar(B,16,0);
                Len := MS.Size;
                for I := 15 downto 12 do begin
                  B[I] := Len and $FF;
                  Len := Len shr 8;
                end;
                C.Encrypt(B,16);
                C.Encrypt(MS.Memory^,MS.Size);
              finally
                C.Free;
              end;
            finally
              ProtectClear(CK^,64);
            end;
          finally
            FreeMem(CK);
          end;
          AStream.Write(PW^,16);
          AStream.Write(B,16);
          AStream.CopyFrom(MS,0);
        finally
          ProtectClear(PW^,PWLen + 16);
        end;
      finally
        FreeMem(PW);
      end;
    finally
      MS.Free;
    end;
  finally
    DisposeASN1Struct(S);
  end;
end;

function RSAPrivateKeyFromStream(AStream: TStream;
                                 const Password; PWLen: Integer;
                                 var PrivKey: TIFPrivateKey): Boolean;
var
  C: TRijndael_CTR;
  CK, PW: PChar;
  MS: TSecureMemoryStream;
  B: array [0..15] of Byte;
  I, Len: Integer;
  S: TASN1Struct;
begin
  GetMem(PW,PWLen + 16);
  try
    AStream.Read(PW^,16);
    try
      Move(Password,PW[16],PWLen);
      GetMem(CK,64);
      try
        try
          WeakPasswordToPrivKey(PW^,PWLen + 16,$8000,CK^,48);
          C := TRijndael_CTR.Create(CK^,48,1);
          try
            AStream.Read(B,16);
            try
              C.Decrypt(B,16);
              Result := True;
              for I := 0 to 11 do
                Result := Result and (B[I] = 0);
              if Result then begin
                Len := 0;
                for I := 12 to 15 do
                  Len := (Len shl 8) + B[I];
                MS := TSecureMemoryStream.Create;
                try
                  MS.CopyFrom(AStream,Len);
                  try
                    C.Decrypt(MS.Memory^,MS.Size);
                    WeakPasswordToPrivKey(PW^,PWLen + 16,$1000,CK^,64);
                    HMAC(CK^,64,MS.Memory^,MS.Size - 16,haSHA1,B,16);
                    Result := CompareMem(Ptr(LongInt(MS.Memory) + MS.Size - 16),@B,16);
                    if Result then begin
                      MS.Position := 0;
                      FillChar(S,SizeOf(S),0);
                      ASN1FromStream(MS,S);
                      Result := (S.Tag = V_ASN1_SEQUENCE) and
                                (S.Cls = V_ASN1_UNIVERSAL) and
                                S.Constructed;
                      if Result then begin
                        Result := S.Contents^.ItemCount >= 3;
                        if Result then begin
                          Result := S.Contents^.Items[0].Length = 1;
                          for I := 0 to S.Contents^.ItemCount - 1 do
                            Result := Result and
                                      (S.Contents^.Items[I].Tag = V_ASN1_INTEGER) and
                                      (S.Contents^.Items[I].Cls = V_ASN1_UNIVERSAL) and
                                      not S.Contents^.Items[I].Constructed;
                          if Result then begin
                            B[0] := Byte(S.Contents^.Items[0].Content[0]);
                            Result := ((B[0] and $3) in [0..2]) and
                                      ((B[0] shr 2) in [0..2]) and
                                      (((B[0] and $3 = 0) and
                                        (S.Contents^.ItemCount = 3)) or
                                       ((B[0] and $3 = 1) and
                                        (S.Contents^.ItemCount = 6)) or
                                       ((B[0] and $3 = 2) and
                                        (S.Contents^.ItemCount = 4)));
                            if Result then begin
                              DisposeIFPrivateKey(PrivKey);
                              PrivKey.ReprType := B[0] and $3;
                              case B[0] shr 2 of
                                0: PrivKey.Scheme := ifRSA1;
                                1: PrivKey.Scheme := ifRSA2;
                                2: PrivKey.Scheme := ifRW;
                              end;
                              case B[0] and $3 of
                                0: begin
                                     S.Contents^.Items[1].ContentAsMPInt(PrivKey.zN);
                                     S.Contents^.Items[2].ContentAsMPInt(PrivKey.zD);
                                   end;
                                1: begin
                                     S.Contents^.Items[1].ContentAsMPInt(PrivKey.oP);
                                     S.Contents^.Items[2].ContentAsMPInt(PrivKey.oQ);
                                     S.Contents^.Items[3].ContentAsMPInt(PrivKey.oD1);
                                     S.Contents^.Items[4].ContentAsMPInt(PrivKey.oD2);
                                     S.Contents^.Items[5].ContentAsMPInt(PrivKey.oC);
                                   end;
                                2: begin
                                     S.Contents^.Items[1].ContentAsMPInt(PrivKey.tP);
                                     S.Contents^.Items[2].ContentAsMPInt(PrivKey.tQ);
                                     S.Contents^.Items[3].ContentAsMPInt(PrivKey.tD);
                                   end;
                              end;
                            end;
                          end;
                        end;
                      end;
                      DisposeASN1Struct(S);
                    end;
                  finally
                    ProtectClear(MS.Memory^,MS.Size);
                  end;
                finally
                  MS.Free;
                end;
              end;
            finally
              ProtectClear(B,SizeOf(B));
            end;
          finally
            C.Free;
          end;
        finally
          ProtectClear(CK^,64);
        end;
      finally
        FreeMem(CK);
      end;
    finally
      ProtectClear(PW^,PWLen + 16);
    end;
  finally
    FreeMem(PW);
  end;
end;               

function DLPrivateKeyFromStream(AStream: TStream;
                               const Password; PWLen: Integer;
                               var PrivKey: TDLPrivateKey): Boolean;
var
  C: TRijndael_CTR;
  CK, PW: PChar;
  MS: TSecureMemoryStream;
  B: array [0..15] of Byte;
  I, Len: Integer;
  S: TASN1Struct;
begin
  GetMem(PW,PWLen + 16);
  try
    AStream.Read(PW^,16);
    try
      Move(Password,PW[16],PWLen);
      GetMem(CK,64);
      try
        try
          WeakPasswordToPrivKey(PW^,PWLen + 16,$8000,CK^,48);
          C := TRijndael_CTR.Create(CK^,48,1);
          try
            AStream.Read(B,16);
            try
              C.Decrypt(B,16);
              Result := True;
              for I := 0 to 11 do
                Result := Result and (B[I] = 0);
              if Result then begin
                Len := 0;
                for I := 12 to 15 do
                  Len := (Len shl 8) + B[I];
                MS := TSecureMemoryStream.Create;
                try
                  MS.CopyFrom(AStream,Len);
                  try
                    C.Decrypt(MS.Memory^,MS.Size);
                    WeakPasswordToPrivKey(PW^,PWLen + 16,$1000,CK^,64);
                    HMAC(CK^,64,MS.Memory^,MS.Size - 16,haSHA1,B,16);
                    Result := CompareMem(Ptr(LongInt(MS.Memory) + MS.Size - 16),@B,16);
                    if Result then begin
                      MS.Position := 0;
                      FillChar(S,SizeOf(S),0);
                      ASN1FromStream(MS,S);
                      Result := (S.Tag = V_ASN1_SEQUENCE) and
                                (S.Cls = V_ASN1_UNIVERSAL) and
                                S.Constructed;
                      if Result then begin
                        Result := S.Contents^.ItemCount >= 3;
                        if Result then begin
                          Result := S.Contents^.Items[0].Length = 1;
                          for I := 0 to S.Contents^.ItemCount - 1 do
                            Result := Result and
                                      (S.Contents^.Items[I].Tag = V_ASN1_INTEGER) and
                                      (S.Contents^.Items[I].Cls = V_ASN1_UNIVERSAL) and
                                      not S.Contents^.Items[I].Constructed;
                          if Result then begin
                            B[0] := Byte(S.Contents^.Items[0].Content[0]);
                            Result := (Integer(B[0] and $7) <= Ord(High(TDLSignatureScheme))) and
                                      (((B[0] shr 3) and $7) <= Ord(High(TDLKeyAgreementScheme))) and
                                      (S.ItemCount = 6);
                            if Result then begin
                              DisposeDLPrivateKey(PrivKey);
                              PrivKey.SignScheme := TDLSignatureScheme(B[0] and $7);
                              PrivKey.KAScheme := TDLKeyAgreementScheme((B[0] shr 3) and $7);
                              S.Contents^.Items[1].ContentAsMPInt(PrivKey.Params.P);
                              S.Contents^.Items[2].ContentAsMPInt(PrivKey.Params.Q);
                              S.Contents^.Items[3].ContentAsMPInt(PrivKey.Params.G);
                              S.Contents^.Items[4].ContentAsMPInt(PrivKey.Params.J);
                              S.Contents^.Items[5].ContentAsMPInt(PrivKey.X);
                            end;
                          end;
                        end;
                      end;
                      DisposeASN1Struct(S);
                    end;
                  finally
                    ProtectClear(MS.Memory^,MS.Size);
                  end;
                finally
                  MS.Free;
                end;
              end;
            finally
              ProtectClear(B,SizeOf(B));
            end;
          finally
            C.Free;
          end;
        finally
          ProtectClear(CK^,64);
        end;
      finally
        FreeMem(CK);
      end;
    finally
      ProtectClear(PW^,PWLen + 16);
    end;
  finally
    FreeMem(PW);
  end;
end;
{$ENDIF SHA1}

function VerifyRSAPrivateKey(const Cert: TASN1Struct; const PrivKey: TIFPrivateKey): Boolean;
var
  PK: TIFPublicKey;
begin
  Result := False;
  FillChar(PK,SizeOf(PK),0);
  if ExtractSubjectRSAPublicKey(Cert,PK,True) = E_OK then begin
    try
      Result := ValidateIFPrivateKey(PrivKey,PK);
    finally
      MPDealloc(PK.N);
      MPDealloc(PK.E);
    end;
  end;
end;            

function VerifyDLPrivateKey(const Cert: TASN1Struct; const PrivKey: TDLPrivateKey): Boolean;
var
  PK: TDLPublicKey;
begin
  Result := False;
  FillChar(PK,SizeOf(PK),0);
  if ExtractSubjectDHPublicKey(Cert,PK) = E_OK then begin
    try
      Result := ValidateDLPrivateKey(PrivKey,PK);
    finally
      DisposeDLPublicKey(PK);
    end;
  end else if ExtractSubjectDSAPublicKey(Cert,PK) = E_OK then begin
    try
      Result := ValidateDLPrivateKey(PrivKey,PK);
    finally
      DisposeDLPublicKey(PK);
    end;
  end;
end;

function VerifyECPrivateKey(const Cert: TASN1Struct; const PrivKey: TECPrivateKey): Boolean;
var
  PK: TECPublicKey;
begin
  Result := False;
  FillChar(PK,SizeOf(PK),0);
  if ExtractSubjectECPublicKey(Cert,PK) = E_OK then begin
    try
      Result := ValidateECPrivateKey(PrivKey,PK);
    finally
      DisposeECPublicKey(PK);
    end;
  end;
end;

procedure RSASignStruct(var Signed: TASN1Struct;
//                        const CACert: TASN1Struct;
                        const CAPrivKey: TIFPrivateKey;
                        HashAlgorithm: THashAlgorithm;
                        MGFHashAlgorithm: THashAlgorithm;
                        Encoding: TSignEncoding);
var
  S, V, R: ^TASN1Struct;
  Alg, D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
begin
//  if not VerifyRSAPrivateKey(CACert,CAPrivKey) then
//    raise Exception.Create('Invalid CACert / Private key');

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    if Encoding = seEMSA3 then begin
      Alg := SignatureAlgEMSA3(HashAlgorithm);
      NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      V^.Contents^.Items[0].SetContent(D[1],Length(D));
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_NULL);
    end else if Encoding = seEMSA4 then begin
      Alg := id_RSASSA_PSS;
      NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      V^.Contents^.Items[0].SetContent(D[1],Length(D));
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      S := @V^.Contents^.Items[1];

      Alg := HashAlgorithmToOID(HashAlgorithm);

      AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,0);
      R := @S^.Contents^.Items[0];
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      R := @R^.Contents^.Items[0];
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      R^.Contents^.Items[0].SetContent(D[1],Length(D));
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,False,V_ASN1_NULL);

      Alg := HashAlgorithmToOID(MGFHashAlgorithm);

      AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,1);
      R := @S^.Contents^.Items[1];
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      R := @R^.Contents^.Items[0];
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(id_mgf1);
      R^.Contents^.Items[0].SetContent(D[1],Length(D));
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      R := @R^.Contents^.Items[1];
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      R^.Contents^.Items[0].SetContent(D[1],Length(D));
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,False,V_ASN1_NULL);
    end else
      raise Exception.Create('Encoding not supported');
    Signed.AllocContents(3);
    V^.CalculateLength;
    CopyASN1Struct(Signed.Contents^.Items[1],V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Signed.CalculateLength;
  MS := TSecureMemoryStream.Create;
  try
    ASN1ToStream(Signed.Contents^.Items[0],MS);
    SetLength(D,8192);
    Len := IFSSASignatureGeneration(CAPrivKey,
                                    MS.Memory^,MS.Size,
                                    HashAlgorithm,
                                    MGFHashAlgorithm,
                                    Encoding,
                                    D[1],Length(D));
    SetLength(D,Len);
    Signed.Contents^.Items[2].Cls := V_ASN1_UNIVERSAL;
    Signed.Contents^.Items[2].Constructed := False;
    Signed.Contents^.Items[2].Tag := V_ASN1_BIT_STRING;
    D := #0 + D;
    Signed.Contents^.Items[2].SetContent(D[1],Length(D));
  finally
    MS.Free;
  end;
  Signed.CalculateLength;
end;   

procedure RSASignCRL(var CRL: TASN1Struct;
                     const CACert: TASN1Struct;
                     const CAPrivKey: TIFPrivateKey;
                     HashAlgorithm: THashAlgorithm;
                     MGFHashAlgorithm: THashAlgorithm;
                     Encoding: TSignEncoding);
var
  Name: TX501Name;
  S, V, R: ^TASN1Struct;
  Ext: TX509Extension;
  Alg, D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
begin
  if not VerifyRSAPrivateKey(CACert,CAPrivKey) then
    raise Exception.Create('Invalid CACert / Private key');

  if not IsAllegedCRLIssuer(CRL,CACert) then begin
    ExtractSubject(CACert,Name);
    TranslateName(Name,CRL.Contents^.Items[0].Contents^.Items[3]);

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,Ext) = E_OK then begin
      Ext.Critical := False;
      Ext.extnID := id_ce_authorityKeyIdentifier;
      New(V);
      try
        NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        AddComposeASN1Field(V^,V_ASN1_CONTEXT_SPECIFIC,True,0);
        S := @V^.Contents^.Items[0];
        AddASN1Field(S^,Ext.extnValue);
        V^.CalculateLength;
        CopyASN1Struct(Ext.extnValue,V^);
        ImposeCRLExtension(Ext,CRL);
        DisposeASN1Struct(V^);
      finally
        Dispose(V);
      end;
    end;
    Ext.Free;

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectAltName,Ext) = E_OK then begin
      Ext.extnID := id_ce_issuerAltName;
      Ext.Critical := False;
      ImposeCRLExtension(Ext,CRL);
    end;
    Ext.Free;
  end;

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    if Encoding = seEMSA3 then begin
      Alg := SignatureAlgEMSA3(HashAlgorithm);
      NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      V^.Contents^.Items[0].SetContent(D[1],Length(D));
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_NULL);
    end else if Encoding = seEMSA4 then begin
      Alg := id_RSASSA_PSS;
      NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      V^.Contents^.Items[0].SetContent(D[1],Length(D));
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_SEQUENCE);
      S := @V^.Contents^.Items[1];

      Alg := HashAlgorithmToOID(HashAlgorithm);

      AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,0);
      R := @S^.Contents^.Items[0];
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      R := @R^.Contents^.Items[0];
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      R^.Contents^.Items[0].SetContent(D[1],Length(D));
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,False,V_ASN1_NULL);

      Alg := HashAlgorithmToOID(MGFHashAlgorithm);

      AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,1);
      R := @S^.Contents^.Items[1];
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      R := @R^.Contents^.Items[0];
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(id_mgf1);
      R^.Contents^.Items[0].SetContent(D[1],Length(D));
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      R := @R^.Contents^.Items[0];
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      R^.Contents^.Items[0].SetContent(D[1],Length(D));
      AddComposeASN1Field(R^,V_ASN1_UNIVERSAL,False,V_ASN1_NULL);
    end else
      raise Exception.Create('Encoding not supported');
    CopyASN1Struct(CRL.Contents^.Items[0].Contents^.Items[1],V^);
    CRL.AllocContents(3);
    CopyASN1Struct(CRL.Contents^.Items[1],V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  CRL.CalculateLength;
  MS := TSecureMemoryStream.Create;
  try
    ASN1ToStream(CRL.Contents^.Items[0],MS);
    SetLength(D,8192);
    Len := IFSSASignatureGeneration(CAPrivKey,
                                    MS.Memory^,MS.Size,
                                    HashAlgorithm,
                                    MGFHashAlgorithm,
                                    Encoding,
                                    D[1],Length(D));
    SetLength(D,Len);
    CRL.Contents^.Items[2].Cls := V_ASN1_UNIVERSAL;
    CRL.Contents^.Items[2].Constructed := False;
    CRL.Contents^.Items[2].Tag := V_ASN1_BIT_STRING;
    D := #0 + D;
    CRL.Contents^.Items[2].SetContent(D[1],Length(D));
  finally
    MS.Free;
  end;
  CRL.CalculateLength;
end;

procedure RSASignCertificate(var Cert: TASN1Struct;
                             const CACert: TASN1Struct;
                             const CAPrivKey: TIFPrivateKey;
                             HashAlgorithm: THashAlgorithm;
                             MGFHashAlgorithm: THashAlgorithm;
                             Encoding: TSignEncoding);
var
  Name: TX501Name;
  S, V: ^TASN1Struct;
  Ext: TX509Extension;
  D: string;
  MS: TSecureMemoryStream;
{$IFDEF MD5}
  H: THash;
{$ENDIF MD5}
  Len: Integer;
begin
  if not VerifyRSAPrivateKey(CACert,CAPrivKey) then
    raise Exception.Create('Invalid CACert / Private key');

  if not IsAllegedIssuer(Cert,CACert) then begin
    ExtractSubject(CACert,Name);
    TranslateName(Name,Cert.Contents^.Items[0].Contents^.Items[3]);

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,Ext) = E_OK then begin
      Ext.Critical := False;
      Ext.extnID := id_ce_authorityKeyIdentifier;
      New(V);
      try
        NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        AddComposeASN1Field(V^,V_ASN1_CONTEXT_SPECIFIC,True,0);
        S := @V^.Contents^.Items[0];
        AddASN1Field(S^,Ext.extnValue);
        V^.CalculateLength;
        CopyASN1Struct(Ext.extnValue,V^);
        ImposeExtension(Ext,Cert);
        DisposeASN1Struct(V^);
      finally
        Dispose(V);
      end;
    end;
    Ext.Free;

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectAltName,Ext) = E_OK then begin
      Ext.extnID := id_ce_issuerAltName;
      Ext.Critical := False;
      ImposeExtension(Ext,Cert);
    end;
    Ext.Free;
  end;

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    ComposeRSASignAlg(V^,HashAlgorithm,MGFHashAlgorithm,Encoding);
    Cert.Contents^.Items[0].Contents^.Items[2].Assign(V^);
    Cert.Contents^.Items[0].Contents^.Items[2].VarName := 'signature';
    Cert.AllocContents(3);
    Cert.Contents^.Items[1].Assign(V^);
    Cert.Contents^.Items[1].VarName := 'signatureAlgorithm';
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Cert.CalculateLength;
  MS := TSecureMemoryStream.Create;
  try
{$IFDEF MD5}
    if Cert.Items[0].Items[1].Length = 0 then begin
      ASN1ToStream(Cert.Contents^.Items[0],MS);
      H := TMD5.Create(MS.Memory^,MS.Size);
      try
        SetLength(D,H.DigestSize);
        H.Done(Pointer(D));
      finally
        H.Free;
      end;
      Cert.FindField('/tbsCertificate/serialNumber').SetContent(D[1],Length(D));
      Cert.CalculateLength;
    end;
{$ENDIF MD5}

    MS.Size := 0;
    ASN1ToStream(Cert.Contents^.Items[0],MS);
    SetLength(D,8192);
    Len := IFSSASignatureGeneration(CAPrivKey,
                                    MS.Memory^,MS.Size,
                                    HashAlgorithm,
                                    MGFHashAlgorithm,
                                    Encoding,
                                    D[1],Length(D));
    SetLength(D,Len);
    Cert.Items[2]^.Tag := V_ASN1_BIT_STRING;
    Cert.Items[2]^.VarName := 'signature';
    D := #0 + D;
    Cert.Contents^.Items[2].SetContent(D[1],Length(D));
  finally
    MS.Free;
  end;
  Cert.CalculateLength;
end;     

procedure DSASignStruct(var Signed: TASN1Struct;
//                        const CACert: TASN1Struct;
                        const CAPrivKey: TDLPrivateKey;
                        HashAlgorithm: THashAlgorithm);
var
  V: PASN1Struct;                              
{$IFDEF SHA1}
  Alg: string;                              
{$ENDIF SHA1}
  D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
begin                                  
{$IFDEF SHA1}
  Assert(HashAlgorithm = haSHA1,'DSA for X.509: Hash algorithm not supported');
{$ELSE  SHA1}                                                                  
  Assert(False,'DSA for X.509: Hash algorithm not supported');
{$ENDIF SHA1}
//  if not VerifyDLPrivateKey(CACert,CAPrivKey) then
//    raise Exception.Create('Invalid CACert / Private key');

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
{$IFDEF SHA1}
    if HashAlgorithm = haSHA1 then begin
      Alg := id_dsa_with_sha1;
      NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      V^.Contents^.Items[0].SetContent(D[1],Length(D));
    end else
{$ENDIF SHA1}
      raise Exception.Create('Algorithm not supported');
    Signed.AllocContents(3);
    V^.CalculateLength;
    CopyASN1Struct(Signed.Contents^.Items[1],V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Signed.CalculateLength;
  MS := TSecureMemoryStream.Create;
  try
    ASN1ToStream(Signed.Contents^.Items[0],MS);
    SetLength(D,8192);
    Len := DLSSASignatureGeneration(CAPrivKey,MS.Memory^,MS.Size,HashAlgorithm,True,D[1],Length(D));
    SetLength(D,Len);
    Signed.Contents^.Items[2].Cls := V_ASN1_UNIVERSAL;
    Signed.Contents^.Items[2].Constructed := False;
    Signed.Contents^.Items[2].Tag := V_ASN1_BIT_STRING;
    D := #0 + D;
    Signed.Contents^.Items[2].SetContent(D[1],Length(D));
  finally
    MS.Free;
  end;
  Signed.CalculateLength;
end;

procedure DSASignCRL(var CRL: TASN1Struct;
                     const CACert: TASN1Struct;
                     const CAPrivKey: TDLPrivateKey;
                     HashAlgorithm: THashAlgorithm);
var
  Name: TX501Name;
  S, V: ^TASN1Struct;
  Ext: TX509Extension;                              
{$IFDEF SHA1}
  Alg: string;                              
{$ENDIF SHA1}
  D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
begin
{$IFDEF SHA1}
  Assert(HashAlgorithm = haSHA1,'DSA for X.509: Hash algorithm not supported');
{$ELSE  SHA1}
  Assert(False,'DSA for X.509: Hash algorithm not supported');
{$ENDIF SHA1}
  if not VerifyDLPrivateKey(CACert,CAPrivKey) then
    raise Exception.Create('Invalid CACert / Private key');

  if not IsAllegedCRLIssuer(CRL,CACert) then begin
    ExtractSubject(CACert,Name);
    TranslateName(Name,CRL.Contents^.Items[0].Contents^.Items[3]);

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,Ext) = E_OK then begin
      Ext.Critical := False;
      Ext.extnID := id_ce_authorityKeyIdentifier;
      New(V);
      try
        NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        AddComposeASN1Field(V^,V_ASN1_CONTEXT_SPECIFIC,False,0);
        S := @V^.Contents^.Items[0];
        S^.SetContent(Ext.extnValue.Content^,Ext.extnValue.Length);
        V^.CalculateLength;
        CopyASN1Struct(Ext.extnValue,V^);
        ImposeCRLExtension(Ext,CRL);
        DisposeASN1Struct(V^);
      finally
        Dispose(V);
      end;
    end;
    Ext.Free;

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectAltName,Ext) = E_OK then begin
      Ext.extnID := id_ce_issuerAltName;
      Ext.Critical := False;
      ImposeCRLExtension(Ext,CRL);
    end;
    Ext.Free;
  end;

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
{$IFDEF SHA1}
    if HashAlgorithm = haSHA1 then begin
      Alg := id_dsa_with_sha1;
      NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      V^.Contents^.Items[0].SetContent(D[1],Length(D));
    end else                           
{$ENDIF SHA1}
      raise Exception.Create('Algorithm not supported');
    V^.CalculateLength;
    CopyASN1Struct(CRL.Contents^.Items[0].Contents^.Items[1],V^);
    CRL.AllocContents(3);
    CopyASN1Struct(CRL.Contents^.Items[1],V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  CRL.CalculateLength;
  MS := TSecureMemoryStream.Create;
  try
    ASN1ToStream(CRL.Contents^.Items[0],MS);
    SetLength(D,8192);
    Len := DLSSASignatureGeneration(CAPrivKey,MS.Memory^,MS.Size,HashAlgorithm,True,D[1],Length(D));
    SetLength(D,Len);
    CRL.Contents^.Items[2].Cls := V_ASN1_UNIVERSAL;
    CRL.Contents^.Items[2].Constructed := False;
    CRL.Contents^.Items[2].Tag := V_ASN1_BIT_STRING;
    D := #0 + D;
    CRL.Contents^.Items[2].SetContent(D[1],Length(D));
  finally
    MS.Free;
  end;
  CRL.CalculateLength;
end;
    
procedure DSASignCertificate(var Cert: TASN1Struct;
                             const CACert: TASN1Struct;
                             const CAPrivKey: TDLPrivateKey;
                             HashAlgorithm: THashAlgorithm);
var
  Name: TX501Name;
  S, V: ^TASN1Struct;
  Ext: TX509Extension;
  Alg, D: string;
  MS: TSecureMemoryStream;
{$IFDEF MD5}
  H: THash;
{$ENDIF MD5}
  Len: Integer;
begin
{$IFDEF SHA1}
  Assert(HashAlgorithm = haSHA1,'DSA for X.509: Hash algorithm not supported');
{$ELSE  SHA1}
  Assert(False,'DSA for X.509: Hash algorithm not supported');
{$ENDIF SHA1}
  if not VerifyDLPrivateKey(CACert,CAPrivKey) then
    raise Exception.Create('Invalid CACert / Private key');

  if not IsAllegedIssuer(Cert,CACert) then begin
    ExtractSubject(CACert,Name);
    TranslateName(Name,Cert.Contents^.Items[0].Contents^.Items[3]);

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,Ext) = E_OK then begin
      Ext.Critical := False;
      Ext.extnID := id_ce_authorityKeyIdentifier;
      New(V);
      try
        NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        AddComposeASN1Field(V^,V_ASN1_CONTEXT_SPECIFIC,False,0);
        S := @V^.Contents^.Items[0];
        S^.SetContent(Ext.extnValue.Content^,Ext.extnValue.Length);
        V^.CalculateLength;
        CopyASN1Struct(Ext.extnValue,V^);
        ImposeExtension(Ext,Cert);
        DisposeASN1Struct(V^);
      finally
        Dispose(V);
      end;
    end;
    Ext.Free;

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectAltName,Ext) = E_OK then begin
      Ext.extnID := id_ce_issuerAltName;
      Ext.Critical := False;
      ImposeExtension(Ext,Cert);
    end;
    Ext.Free;
  end;

{$IFDEF SHA1}
  if HashAlgorithm = haSHA1 then begin
    Alg := id_dsa_with_sha1;
  end else
{$ENDIF SHA1}
    raise Exception.Create('Algorithm not supported');
  Cert.Items[0].Items[2].AllocContents(1);
  Cert.Items[0].Items[2].Items[0].Tag := V_ASN1_OBJECT;
  Cert.Items[0].Items[2].Items[0].Cls := V_ASN1_UNIVERSAL;
  Cert.Items[0].Items[2].Items[0].Constructed := False;
  Cert.Items[0].Items[2].Items[0].EditContent(Alg);
  Cert.Items[0].Items[2].CalculateLength;
  Cert.AllocContents(3);
  CopyASN1Struct(Cert.Contents^.Items[1],Cert.Contents^.Items[0].Contents^.Items[2]);

  Cert.CalculateLength;
  MS := TSecureMemoryStream.Create;
  try
{$IFDEF MD5}
    if Cert.Items[0].Items[1].Length = 0 then begin
      ASN1ToStream(Cert.Contents^.Items[0],MS);
      H := TMD5.Create(MS.Memory^,MS.Size);
      try
        SetLength(D,H.DigestSize);
        H.Done(Pointer(D));
      finally
        H.Free;
      end;
      Cert.Contents^.Items[0].Contents^.Items[1].SetContent(D[1],Length(D));
      Cert.CalculateLength;
    end;
{$ENDIF MD5}

    MS.Size := 0;
    ASN1ToStream(Cert.Contents^.Items[0],MS);
    SetLength(D,8192);
    Len := DLSSASignatureGeneration(CAPrivKey,MS.Memory^,MS.Size,HashAlgorithm,True,D[2],Length(D)-1);
    Cert.Contents^.Items[2].Cls := V_ASN1_UNIVERSAL;
    Cert.Contents^.Items[2].Constructed := False;
    Cert.Contents^.Items[2].Tag := V_ASN1_BIT_STRING;
    D[1] := #0;
    Cert.Contents^.Items[2].SetContent(D[1],Len+1);
  finally
    MS.Free;
  end;
  Cert.CalculateLength;
end;

procedure ECDSASignStruct(var Signed: TASN1Struct;
//                          const CACert: TASN1Struct;
                          const CAPrivKey: TECPrivateKey;
                          // Allow for future expansion:
                          HashAlgorithm: THashAlgorithm);
var
  V: PASN1Struct;                              
{$IFDEF SHA1}
  Alg: string;                              
{$ENDIF SHA1}
  D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
begin
{$IFDEF SHA1}
  Assert(HashAlgorithm = haSHA1,'ECDSA for X.509: Hash algorithm not supported');
{$ELSE  SHA1}
  Assert(False,'ECDSA for X.509: Hash algorithm not supported');
{$ENDIF SHA1}
//  if not VerifyDLPrivateKey(CACert,CAPrivKey) then
//    raise Exception.Create('Invalid CACert / Private key');

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
{$IFDEF SHA1}
    if HashAlgorithm = haSHA1 then begin
      Alg := ecdsa_with_sha1;
      NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      V^.Contents^.Items[0].SetContent(D[1],Length(D));
    end else
{$ENDIF SHA1}
      raise Exception.Create('Algorithm not supported');
    Signed.AllocContents(3);
    V^.CalculateLength;
    CopyASN1Struct(Signed.Contents^.Items[1],V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  Signed.CalculateLength;
  MS := TSecureMemoryStream.Create;
  try
    ASN1ToStream(Signed.Contents^.Items[0],MS);
    SetLength(D,8192);
    Len := ECSSASignatureGeneration(CAPrivKey,MS.Memory^,MS.Size,HashAlgorithm,True,D[1],Length(D));
    SetLength(D,Len);
    Signed.Contents^.Items[2].Cls := V_ASN1_UNIVERSAL;
    Signed.Contents^.Items[2].Constructed := False;
    Signed.Contents^.Items[2].Tag := V_ASN1_BIT_STRING;
    D := #0 + D;
    Signed.Contents^.Items[2].SetContent(D[1],Length(D));
  finally
    MS.Free;
  end;
  Signed.CalculateLength;
end;

procedure ECDSASignCRL(var CRL: TASN1Struct;
                       const CACert: TASN1Struct;
                       const CAPrivKey: TECPrivateKey;
                       HashAlgorithm: THashAlgorithm);
var
  Name: TX501Name;
  S, V: ^TASN1Struct;
  Ext: TX509Extension;                              
{$IFDEF SHA1}
  Alg: string;                              
{$ENDIF SHA1}
  D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
begin                                 
{$IFDEF SHA1}
  Assert(HashAlgorithm = haSHA1,'ECDSA for X.509: Hash algorithm not supported');
{$ELSE  SHA1}
  Assert(False,'ECDSA for X.509: Hash algorithm not supported');
{$ENDIF SHA1}
  if not VerifyECPrivateKey(CACert,CAPrivKey) then
    raise Exception.Create('Invalid CACert / Private key');

  if not IsAllegedCRLIssuer(CRL,CACert) then begin
    ExtractSubject(CACert,Name);
    TranslateName(Name,CRL.Contents^.Items[0].Contents^.Items[3]);

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,Ext) = E_OK then begin
      Ext.Critical := False;
      Ext.extnID := id_ce_authorityKeyIdentifier;
      New(V);
      try
        NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        AddComposeASN1Field(V^,V_ASN1_CONTEXT_SPECIFIC,True,0);
        S := @V^.Contents^.Items[0];
        AddASN1Field(S^,Ext.extnValue);
        V^.CalculateLength;
        CopyASN1Struct(Ext.extnValue,V^);
        ImposeCRLExtension(Ext,CRL);
        DisposeASN1Struct(V^);
      finally
        Dispose(V);
      end;
    end;
    Ext.Free;

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectAltName,Ext) = E_OK then begin
      Ext.extnID := id_ce_issuerAltName;
      Ext.Critical := False;
      ImposeCRLExtension(Ext,CRL);
    end;
    Ext.Free;
  end;

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
{$IFDEF SHA1}
    if HashAlgorithm = haSHA1 then begin
      Alg := ecdsa_with_sha1;
      NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
      D := InterpretOIDtoBER(Alg);
      V^.Contents^.Items[0].SetContent(D[1],Length(D));
    end else
{$ENDIF SHA1}
      raise Exception.Create('Algorithm not supported');
    V^.CalculateLength;
    CopyASN1Struct(CRL.Contents^.Items[0].Contents^.Items[1],V^);
    CRL.AllocContents(3);
    CopyASN1Struct(CRL.Contents^.Items[1],V^);
    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;
  CRL.CalculateLength;
  MS := TSecureMemoryStream.Create;
  try
    ASN1ToStream(CRL.Contents^.Items[0],MS);
    SetLength(D,8192);
    Len := ECSSASignatureGeneration(CAPrivKey,MS.Memory^,MS.Size,HashAlgorithm,True,D[1],Length(D));
    SetLength(D,Len);
    CRL.Contents^.Items[2].Cls := V_ASN1_UNIVERSAL;
    CRL.Contents^.Items[2].Constructed := False;
    CRL.Contents^.Items[2].Tag := V_ASN1_BIT_STRING;
    D := #0 + D;
    CRL.Contents^.Items[2].SetContent(D[1],Length(D));
  finally
    MS.Free;
  end;
  CRL.CalculateLength;
end;
         
procedure ECDSASignCertificate(var Cert: TASN1Struct;
                               const CACert: TASN1Struct;
                               const CAPrivKey: TECPrivateKey;
                               // Allow for future expansion:
                               HashAlgorithm: THashAlgorithm);
var
  Name: TX501Name;
  S, V: ^TASN1Struct;
  Ext: TX509Extension;
  Alg, D: string;
  MS: TSecureMemoryStream;
{$IFDEF MD5}
  H: THash;
{$ENDIF MD5}
  Len: Integer;
begin
{$IFDEF SHA1}
  Assert(HashAlgorithm = haSHA1,'ECDSA for X.509: Hash algorithm not supported');
{$ELSE  SHA1}
  Assert(False,'ECDSA for X.509: Hash algorithm not supported');
{$ENDIF SHA1}
  if not VerifyECPrivateKey(CACert,CAPrivKey) then
    raise Exception.Create('Invalid CACert / Private key');

  if not IsAllegedIssuer(Cert,CACert) then begin
    ExtractSubject(CACert,Name);
    TranslateName(Name,Cert.Contents^.Items[0].Contents^.Items[3]);

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,Ext) = E_OK then begin
      Ext.Critical := False;
      Ext.extnID := id_ce_authorityKeyIdentifier;
      New(V);
      try
        NewComposeASN1Struct(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        AddComposeASN1Field(V^,V_ASN1_CONTEXT_SPECIFIC,True,0);
        S := @V^.Contents^.Items[0];
        AddASN1Field(S^,Ext.extnValue);
        V^.CalculateLength;
        CopyASN1Struct(Ext.extnValue,V^);
        ImposeExtension(Ext,Cert);
        DisposeASN1Struct(V^);
      finally
        Dispose(V);
      end;
    end;
    Ext.Free;

    Ext := nil;
    if ExtractNamedExtension(CACert,id_ce_subjectAltName,Ext) = E_OK then begin
      Ext.extnID := id_ce_issuerAltName;
      Ext.Critical := False;
      ImposeExtension(Ext,Cert);
    end;
    Ext.Free;
  end;

{$IFDEF SHA1}
  if HashAlgorithm = haSHA1 then begin
    Alg := ecdsa_with_sha1;
  end else
{$ENDIF SHA1}
    raise Exception.Create('Algorithm not supported');
  Cert.Items[0].Items[2].AllocContents(1);
  Cert.Items[0].Items[2].Items[0].Tag := V_ASN1_OBJECT;
  Cert.Items[0].Items[2].Items[0].Cls := V_ASN1_UNIVERSAL;
  Cert.Items[0].Items[2].Items[0].Constructed := False;
  Cert.Items[0].Items[2].Items[0].EditContent(Alg);
  Cert.Items[0].Items[2].CalculateLength;
  Cert.AllocContents(3);
  CopyASN1Struct(Cert.Contents^.Items[1],Cert.Contents^.Items[0].Contents^.Items[2]);

  Cert.CalculateLength;
  MS := TSecureMemoryStream.Create;
  try
{$IFDEF MD5}
    if Cert.Items[0].Items[1].Length = 0 then begin
      ASN1ToStream(Cert.Contents^.Items[0],MS);
      H := TMD5.Create(MS.Memory^,MS.Size);
      try
        SetLength(D,H.DigestSize);
        H.Done(Pointer(D));
      finally
        H.Free;
      end;
      Cert.Contents^.Items[0].Contents^.Items[1].SetContent(D[1],Length(D));
      Cert.CalculateLength;
    end;
{$ENDIF MD5}

    MS.Size := 0;
    ASN1ToStream(Cert.Contents^.Items[0],MS);
    SetLength(D,8192);
    Len := ECSSASignatureGeneration(CAPrivKey,MS.Memory^,MS.Size,HashAlgorithm,True,D[2],Length(D)-1);
    Cert.Contents^.Items[2].Cls := V_ASN1_UNIVERSAL;
    Cert.Contents^.Items[2].Constructed := False;
    Cert.Contents^.Items[2].Tag := V_ASN1_BIT_STRING;
    D[1] := #0;
    Cert.Contents^.Items[2].SetContent(D[1],Len+1);
  finally
    MS.Free;
  end;
  Cert.CalculateLength;
end;

procedure TranslateName(const Name: TX501Name; var Struct: TASN1Struct);

  procedure AddRDN(const Name: WideString; const OID: string; Tag: Cardinal);
  var
    R, A: PASN1Struct;
  begin
    R := Struct.AddField;
    R.Persistent := True;
    R := R^.AddField;
    R.Persistent := True;
    R^.FindField('name').EditContent(OID);
    A := R.FindField('value');
    A.Persistent := False;
    A.Tag := Tag;
    A.EditContent(Name);
    A.Persistent := True;
  end;

  {$IFNDEF D6UP}
  procedure AddRDNs(const Name: WideString; const OID: string; Tag: Cardinal);
  var
    N: string;
    P, E: PChar;
    OldDelim: Char;
  begin
    N := Name;
    UniqueString(N);
    P := PChar(N);
    while P^ <> #0 do begin
      while P^ in [#0..#31] do
        Inc(P);
      E := P;
      while not (E^ in [#0..#31]) do
        Inc(E);
      OldDelim := E^;
      E^ := #0;
      AddRDN(P,OID,Tag);
      E^ := OldDelim;
      P := E;
    end;
  end;
  {$ELSE}
  procedure AddRDNs(const Name: WideString; const OID: string; Tag: Cardinal);
  var
    N: WideString;
    P, E: PWideChar;
    OldDelim: WideChar;
  begin
    N := Name;
    UniqueString(N);
    P := PWideChar(N);
    while P^ <> #0 do begin
      while P^ in [WideChar(#0)..WideChar(#31)] do
        Inc(P);
      E := P;
      while not (E^ in [WideChar(#0)..WideChar(#31)]) do
        Inc(E);
      OldDelim := E^;
      E^ := #0;
      AddRDN(P,OID,Tag);
      E^ := OldDelim;
      P := E;
    end;
  end;
 {$ENDIF}

begin
  NewComposeASN1Struct(Struct,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  Struct.Persistent := True;
  Struct.TypeName := 'RDNSequence';
  Struct.Tag := V_ASN1_SEQUENCE;
  Struct.CreateOFTemplate;
  Struct.Template.TypeName := 'RelativeDistinguishedName';
  Struct.Template.Persistent := True;
  Struct.Template.Tag := V_ASN1_SET;
  Struct.Template.Constructed := True;
  Struct.Template.CreateOFTemplate;
  Struct.Template.Template.TypeName := 'AttributeTypeAndValue';
  Struct.Template.Template.Persistent := True;
  Struct.Template.Template.Tag := V_ASN1_SEQUENCE;
  Struct.Template.Template.Constructed := True;
  Struct.Template.Template.AddField('name','OBJECT','',True);
  Struct.Template.Template.AddField('value','ANY','',True);
  if Name.commonName.Str <> '' then
    AddRDNs(Name.commonName.Str,id_at_commonName,Name.commonName.Tag);
  if Name.surname.Str <> '' then
    AddRDN(Name.surname.Str,id_at_surname,Name.surName.Tag);
  if Name.countryName.Str <> '' then
    AddRDN(Name.countryName.Str,id_at_countryName,Name.countryName.Tag);
  if Name.localityName.Str <> '' then
    AddRDN(Name.localityName.Str,id_at_localityName,Name.localityName.Tag);
  if Name.stateOrProvinceName.Str <> '' then
    AddRDN(Name.stateOrProvinceName.Str,id_at_stateOrProvinceName,Name.stateOrProvinceName.Tag);
  if Name.organizationName.Str <> '' then
    AddRDNs(Name.organizationName.Str,id_at_organizationName,Name.organizationName.Tag);
  if Name.organizationalUnitName.Str <> '' then
    AddRDNs(Name.organizationalUnitName.Str,id_at_organizationalUnitName,Name.organizationalUnitName.Tag);
  if Name.title.Str <> '' then
    AddRDN(Name.title.Str,id_at_title,Name.title.Tag);
  if Name.name.Str <> '' then
    AddRDN(Name.name.Str,id_at_name,Name.name.Tag);
  if Name.givenName.Str <> '' then
    AddRDN(Name.givenName.Str,id_at_givenName,Name.givenName.Tag);
  if Name.initials.Str <> '' then
    AddRDN(Name.initials.Str,id_at_initials,Name.initials.Tag);
  if Name.generationQualifier.Str <> '' then
    AddRDN(Name.generationQualifier.Str,id_at_generationQualifier,Name.generationQualifier.Tag);
  if Name.dnQualifier.Str <> '' then
    AddRDN(Name.dnQualifier.Str,id_at_dnQualifier,Name.dnQualifier.Tag);
  if Name.emailAddress.Str <> '' then
{$IFDEF UNIQUENAMES}
    AddRDN(Name.emailAddress.Str,id_emailAddress,Name.emailAddress.Tag);
{$ELSE}
    AddRDN(Name.emailAddress.Str,emailAddress,Name.emailAddress.Tag);
{$ENDIF}
  Struct.CalculateLength;
end;

function InterpretName(const Struct: TASN1Struct;
                       var Name: TX501Name;
                       AdjustCase: Boolean;
                       Delim: WideChar): Integer;
var
  S, R, A, T: ^TASN1Struct;
  Id, Id1: Integer;
  Str, PrevStr: TX500String;
  OID, OName, N, PrevOName: string;

  procedure AssignStrCRLF(var DstStr: TX500String; const SrcStr: TX500String);
  begin
    if DstStr.Str = '' then
      DstStr := SrcStr
    else
      DstStr.Str := DstStr.Str + #13#10 + SrcStr.Str;
  end;

  procedure AssignStr(var DstStr: TX500String;
                      const SrcStr: TX500String;
                      Delim: WideChar);
  begin
    if Delim = #0013 then
      AssignStrCRLF(DstStr,SrcStr)
    else if DstStr.Str = '' then
      DstStr := SrcStr
    else
      DstStr.Str := DstStr.Str + Delim + SrcStr.Str;
  end;

begin
  Finalize(Name);
  FillChar(Name,SizeOf(Name),0);
  Result := E_SYNTAX;
  S := @Struct;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
    Result := E_OK;
    for Id := 0 to S^.Contents^.ItemCount - 1 do begin
      R := @S^.Contents^.Items[Id];
      if R^.Constructed and (R^.Tag = V_ASN1_SET) then begin
        if (R^.Contents = nil) or (R^.Contents^.ItemCount = 0) then
          Result := Result or E_EMPTY
        else begin
          if R^.Contents^.ItemCount > 1 then
            Result := Result or E_AMBIGUOUS_SET;
          Finalize(PrevStr);
          FillChar(PrevStr,SizeOf(PrevStr),0);
          PrevOName := '';
          for Id1 := 0 to R^.Contents^.ItemCount - 1 do begin
            A := @R^.Contents^.Items[Id1];
            if not A^.Constructed then
              Result := E_SYNTAX
            else begin
              if A^.Contents^.ItemCount <> 2 then
                Result := E_SYNTAX
              else begin
                T := @A^.Contents^.Items[1];
                if T^.Constructed then
                  Result := E_SYNTAX
                else begin
                  if T^.Tag = V_ASN1_PRINTABLESTRING then begin
                    if AdjustCase then
                      Str.Str := UpperCase(T^.ContentAsString)
                    else
                      Str.Str := T^.ContentAsString
                  end else if T^.Tag = V_ASN1_UTF8STRING then begin
                    SetLength(N,T^.Length);
                    if T^.Length > 0 then
                      Move(T^.Content[0],N[1],T^.Length);
                    Str.Str := Utf8ToUnicode(N);
                  end else if T^.Tag = V_ASN1_BMPSTRING then
                    Str.Str := BMPToUnicode(T^.Content,T^.Length)
                  else begin
                    Result := Result or E_STRING_FORMAT;
                    SetLength(N,T^.Length);
                    if T^.Length > 0 then
                      Move(T^.Content[0],N[1],T^.Length);
                    Str.Str := N;
                  end;
                  Str.Tag := T^.Tag;
                  T := @A^.Contents^.Items[0];
                  if (T^.Tag <> V_ASN1_OBJECT) or T^.Constructed then
                    Result := E_SYNTAX
                  else begin
                    OID := InterpretBERToOID(T^.ContentAsOctetString);
                    OName := GetObjectName(OID);
                    if (OName <> PrevOName) and (PrevOName <> '') then
                      Result := E_SYNTAX
                    else if (PrevStr.Tag = V_ASN1_BMPSTRING) or
                            (PrevStr.Tag = V_ASN1_UTF8STRING) then
                      Result := E_AMBIGUOUS_SET
                    else if OName = 'id-at-commonName' then
                      AssignStr(Name.commonName,Str,Delim)
                    else if OName = 'id-at-surname' then
                      AssignStr(Name.surname,Str,Delim)
                    else if OName = 'id-at-country' then
                      AssignStr(Name.countryName,Str,Delim)
                    else if OName = 'id-at-localityName' then
                      AssignStr(Name.localityName,Str,Delim)
                    else if OName = 'id-at-stateOrProvinceName' then
                      AssignStr(Name.stateOrProvinceName,Str,Delim)
                    else if OName = 'id-at-organizationName' then
                      AssignStr(Name.organizationName,Str,Delim)
                    else if OName = 'id-at-organizationalUnitName' then
                      AssignStr(Name.organizationalUnitName,Str,Delim)
                    else if OName = 'id-at-title' then
                      AssignStr(Name.title,Str,Delim)
                    else if OName = 'id-at-name' then
                      AssignStr(Name.name,Str,Delim)
                    else if OName = 'id-at-givenName' then
                      AssignStr(Name.givenName,Str,Delim)
                    else if OName = 'id-at-initials' then
                      AssignStr(Name.initials,Str,Delim)
                    else if OName = 'id-at-generationQualifier' then
                      AssignStr(Name.generationQualifier,Str,Delim)
                    else if OName = 'id-at-dnQualifier' then
                      AssignStr(Name.dnQualifier,Str,Delim)
                    else if OName = 'emailAddress' then
                      AssignStr(Name.emailAddress,Str,Delim)
                    else
                      Result := E_SYNTAX;
                    if (PrevStr.Tag <> V_ASN1_BMPSTRING) and
                       (PrevStr.Tag <> V_ASN1_UTF8STRING) then begin
                      PrevOName := OName;
                      PrevStr := Str;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end else
        Result := E_SYNTAX;
    end;
  end;
end;

function ExtractSerial(const Cert: TASN1Struct; var Serial: string): Integer; 
var
  S: ^TASN1Struct;
  P: PMPInteger;
begin
  Result := E_SYNTAX;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 1) then begin
      if S^.Contents^.Items[0].Cls = V_ASN1_CONTEXT_SPECIFIC then
        S := @S^.Contents^.Items[1]
      else
        S := @S^.Contents^.Items[0];
      if (S^.Tag = V_ASN1_INTEGER) and not S^.Constructed then begin
        Result := E_OK;
        P := nil;
        try
          S^.ContentAsUMPInt(P);
          Serial := MPIntToBase256(P);
        finally
          MPDealloc(P);
        end;
      end;
    end;
  end;
end;
        
function ExtractIssuerStruct(const Cert: TASN1Struct;
                             var NameStruct: PASN1Struct): Integer;
var
  S: PASN1Struct;
begin
  Result := E_SYNTAX;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.ItemCount > 0) then begin
    S := S^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.ItemCount > 3) then begin
      if S^.Items[0]^.Cls = V_ASN1_CONTEXT_SPECIFIC then
        S := S^.Items[3]
      else
        S := S^.Items[2];
      NameStruct := S;
      Result := E_OK;
    end;
  end;
end;

function ExtractIssuer(const Cert: TASN1Struct;
                       var Name: TX501Name;
                       AdjustCase: Boolean;
                       Delim: WideChar): Integer;
var
  S: ^TASN1Struct;
begin
  Result := E_SYNTAX;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 3) then begin
      if S^.Contents^.Items[0].Cls = V_ASN1_CONTEXT_SPECIFIC then
        S := @S^.Contents^.Items[3]
      else
        S := @S^.Contents^.Items[2];
      Result := InterpretName(S^,Name,AdjustCase,Delim);
    end;
  end;
end;   

function ExtractIssuerCRL(const CRL: TASN1Struct; var Name: TX501Name): Integer;
var
  S: ^TASN1Struct;
begin
  Result := E_SYNTAX;
  S := @CRL;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 1) then begin
      if S^.Contents^.Items[0].Tag = V_ASN1_SEQUENCE then begin
        S := @S^.Contents^.Items[1];
        Result := InterpretName(S^,Name);
      end else if (S^.Contents^.ItemCount > 2) and
                  (S^.Contents^.Items[2].Tag = V_ASN1_SEQUENCE) then begin
        S := @S^.Contents^.Items[2];
        Result := InterpretName(S^,Name);
      end;
    end;
  end;
end;

function ExtractSubjectStruct(const Cert: TASN1Struct;
                              var NameStruct: PASN1Struct): Integer;
var
  S: PASN1Struct;
begin
  Result := E_SYNTAX;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 5) then begin
      if S^.Contents^.Items[0].Cls = V_ASN1_CONTEXT_SPECIFIC then
        S := @S^.Contents^.Items[5]
      else
        S := @S^.Contents^.Items[4];
      NameStruct := S;
      Result := E_OK;
    end;
  end;
end;

function ExtractSubject(const Cert: TASN1Struct;
                        var Name: TX501Name;
                        AdjustCase: Boolean;
                        Delim: WideChar): Integer;
var
  S: ^TASN1Struct;
begin
  Assert(Assigned(Cert));
  Result := E_SYNTAX;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 5) then begin
      if S^.Contents^.Items[0].Cls = V_ASN1_CONTEXT_SPECIFIC then
        S := @S^.Contents^.Items[5]
      else
        S := @S^.Contents^.Items[4];
      Result := InterpretName(S^,Name,AdjustCase,Delim);
    end;
  end;
end;

{$IFNDEF D5UP}
function AnsiSameText(S0,S1: string): Boolean;
begin
  Result := AnsiCompareText(S0,S1) = 0;
end;
{$ENDIF}

function CompareName(const Name0, Name1: TX501Name): Boolean;
begin
  Result := (Name0.commonName.Tag = Name1.commonName.Tag) and
            (Name0.commonName.Str = Name1.commonName.Str) and
            (Name0.surname.Tag = Name1.surname.Tag) and
            (Name0.surname.Str = Name1.surname.Str) and
            (Name0.countryName.Tag = Name1.countryName.Tag) and
            (Name0.countryName.Str = Name1.countryName.Str) and
            (Name0.localityName.Tag = Name1.localityName.Tag) and
            (Name0.localityName.Str = Name1.localityName.Str) and
            (Name0.stateOrProvinceName.Tag = Name1.stateOrProvinceName.Tag) and
            (Name0.stateOrProvinceName.Str = Name1.stateOrProvinceName.Str) and
            (Name0.organizationName.Tag = Name1.organizationName.Tag) and
            (Name0.organizationName.Str = Name1.organizationName.Str) and
            (Name0.organizationalUnitName.Tag = Name1.organizationalUnitName.Tag) and
            (Name0.organizationalUnitName.Str = Name1.organizationalUnitName.Str) and
            (Name0.title.Tag = Name1.title.Tag) and
            (Name0.title.Str = Name1.title.Str) and
            (Name0.name.Tag = Name1.name.Tag) and
            (Name0.name.Str = Name1.name.Str) and
            (Name0.givenName.Tag = Name1.givenName.Tag) and
            (Name0.givenName.Str = Name1.givenName.Str) and
            (Name0.initials.Tag = Name1.initials.Tag) and
            (Name0.initials.Str = Name1.initials.Str) and
            (Name0.generationQualifier.Tag = Name1.generationQualifier.Tag) and
            (Name0.generationQualifier.Str = Name1.generationQualifier.Str) and
            (Name0.dnQualifier.Tag = Name1.dnQualifier.Tag) and
            (Name0.dnQualifier.Str = Name1.dnQualifier.Str);
  if Result then begin
    Result := (Name0.emailAddress.Tag = Name1.emailAddress.Tag) and
              AnsiSameText(Name0.emailAddress.Str,Name1.emailAddress.Str);
    if not Result then begin
      if Pos('<mailto:',AnsiLowerCase(Name0.emailAddress.Str)) > 0 then
        Result := AnsiSameText(Name0.emailAddress.Str,'<mailto:' + Name1.emailAddress.Str + '>')
      else
        Result := AnsiSameText(Name1.emailAddress.Str,'<mailto:' + Name0.emailAddress.Str + '>')
    end;
  end;
end;

procedure TranslateGeneralNames(const Name: TX509GeneralNames; var Struct: TASN1Struct);
var
  R, A, T: ^TASN1Struct;
  I, Idx: Integer;
  S: string;
begin        
  if Struct = nil then begin
    NewComposeASN1Struct(Struct,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  end else begin
    Struct.DisposeContent;
    Struct.Tag := V_ASN1_SEQUENCE;
    Struct.Cls := V_ASN1_UNIVERSAL;
    Struct.Constructed := True;
  end;
  for I := 0 to Length(Name) - 1 do begin
    if Struct.Contents <> nil then
      Idx := Struct.Contents^.ItemCount
    else
      Idx := 0;
    case Name[I].Tag of
      1: begin
           AddComposeASN1Field(Struct,V_ASN1_CONTEXT_SPECIFIC,False,1);
           R := @Struct.Contents^.Items[Idx];
           R^.SetContent(Name[I].rfc822Name[1],
                         Length(Name[I].rfc822Name));
         end;
      2: begin
           AddComposeASN1Field(Struct,V_ASN1_CONTEXT_SPECIFIC,False,2);
           R := @Struct.Contents^.Items[Idx];
           R^.SetContent(Name[I].dNSName[1],
                         Length(Name[I].dNSName));
         end;
      4: begin
           AddComposeASN1Field(Struct,V_ASN1_CONTEXT_SPECIFIC,True,4);
           R := @Struct.Contents^.Items[Idx];
           New(A);
           try
             FillChar(A^,SizeOf(TASN1Struct),0);
             TranslateName(Name[I].directoryName,A^);
             AddASN1Field(R^,A^);
             DisposeASN1Struct(A^);
           finally
             Dispose(A);
           end;
         end;
      5: begin
           AddComposeASN1Field(Struct,V_ASN1_CONTEXT_SPECIFIC,True,5);
           R := @Struct.Contents^.Items[Idx];
           New(A);
           try
             FillChar(A^,SizeOf(TASN1Struct),0);
             TranslateName(Name[I].ediPartyName.nameAssigner,A^);
             AddComposeASN1Field(R^,V_ASN1_CONTEXT_SPECIFIC,True,0);
             T := @R^.Contents^.Items[0];
             AddASN1Field(T^,A^);
             TranslateName(Name[I].ediPartyName.partyName,A^);
             AddComposeASN1Field(R^,V_ASN1_CONTEXT_SPECIFIC,True,1);
             T := @R^.Contents^.Items[1];
             AddASN1Field(T^,A^);
             DisposeASN1Struct(A^);
           finally
             Dispose(A);
           end;
           R^.CalculateLength;
           Struct.Contents^.Items[Idx].CalculateLength;
         end;
      6: begin
           AddComposeASN1Field(Struct,V_ASN1_CONTEXT_SPECIFIC,False,6);
           R := @Struct.Contents^.Items[Idx];
           R^.SetContent(Name[I].uniformResourceIdentifier[1],
                         Length(Name[I].uniformResourceIdentifier));
         end;
      7: begin
           AddComposeASN1Field(Struct,V_ASN1_CONTEXT_SPECIFIC,False,7);
           R := @Struct.Contents^.Items[Idx];
           if Name[I].ipMask = 0 then
             R^.SetContent(Name[I].ipAddress,4)
           else begin
             SetLength(S,4);
             Move(Name[I].ipAddress,S[1],4);
             case Name[I].ipMask of
               1: S := S + #255#0#0#0;
               2: S := S + #255#255#0#0;
               3: S := S + #255#255#255#0;
               4: S := S + #255#255#255#255;
             end;
             R^.SetContent(S[1],8);
           end;
         end;
      8: begin
           AddComposeASN1Field(Struct,V_ASN1_CONTEXT_SPECIFIC,False,8);
           R := @Struct.Contents^.Items[Idx];
           S := InterpretOIDtoBER(Name[I].registredID);
           R^.SetContent(S[1],Length(S));
         end;
    end;
  end;
  Struct.CalculateLength;
end;

function InterpretGeneralNames(const Struct: TASN1Struct; var Name: TX509GeneralNames): Integer;
var
  S, T, A, V: ^TASN1Struct;
  Idx, I: Integer;
begin
  Result := E_SYNTAX;
  S := @Struct;
  if S^.Constructed and
     ((S^.Tag = V_ASN1_SEQUENCE) or (S^.Cls <> V_ASN1_UNIVERSAL)) then begin
    if (S^.Contents = nil) or (S^.Contents^.ItemCount = 0) then
      Result := E_EMPTY
    else begin
      Result := E_OK;
      Idx := 0;
      for I := 0 to S^.Contents^.ItemCount - 1 do begin
        SetLength(Name,Idx + 1);
        T := @S^.Contents^.Items[I];
        if (T^.Cls <> V_ASN1_CONTEXT_SPECIFIC) then
          Result := E_SYNTAX
        else begin
          // The tagged types might be either implicit or explicit. Accept both
          if T^.Constructed then
            A := @T^.Contents^.Items[0]
          else
            A := T;
          if T^.Tag = 0 then begin
            Result := Result or E_NOT_SUPPORTED;
          end else if T^.Tag = 1 then begin
            if (A^.Tag <> V_ASN1_IA5STRING) and (A <> T) then
              Result := E_SYNTAX
            else begin
              Name[Idx].rfc822Name := A^.Content;
              Name[Idx].Tag := 1;
              Inc(Idx);
            end;
          end else if T^.Tag = 2 then begin
            if (A^.Tag <> V_ASN1_IA5STRING) and (A <> T) then
              Result := E_SYNTAX
            else begin
              Name[Idx].dNSName := A^.Content;
              Name[Idx].Tag := 2;
              Inc(Idx);
            end;
          end else if T^.Tag = 3 then begin
            Result := Result or E_NOT_SUPPORTED;
          end else if T^.Tag = 4 then begin
            if A^.Tag = V_ASN1_SET then
              A := T;
            if (A^.Tag <> V_ASN1_SEQUENCE) and (A <> T) then
              Result := E_SYNTAX
            else begin
              Result := Result or InterpretName(A^,Name[Idx].directoryName);
              Name[Idx].Tag := 4;
              Inc(Idx);
            end;
          end else if T^.Tag = 5 then begin
            if A^.Cls = V_ASN1_CONTEXT_SPECIFIC then
              A := T;
            if (A^.Tag <> V_ASN1_SEQUENCE) and (A <> T) then
              Result := E_SYNTAX
            else begin
              V := @A^.Contents^.Items[0];
              if V^.Cls <> V_ASN1_CONTEXT_SPECIFIC then
                Result := E_SYNTAX
              else if V^.Tag = 0 then begin
                Result := Result or InterpretName(V^.Contents^.Items[0],
                                                  Name[Idx].ediPartyName.nameAssigner);
                V := @A^.Contents^.Items[1];
                if (V^.Tag = 1) and (V.Cls = V_ASN1_CONTEXT_SPECIFIC) then begin
                  Result := Result or InterpretName(V^.Contents^.Items[0],
                                                    Name[Idx].ediPartyName.partyName);
                  Name[Idx].Tag := 5;
                  Inc(Idx);
                end else
                  Result := E_SYNTAX;
                Inc(Idx);
              end else if V^.Tag = 1 then begin
                Result := Result or InterpretName(V^.Contents^.Items[0],
                                                  Name[Idx].ediPartyName.partyName);
                Name[Idx].Tag := 5;
                Inc(Idx);
              end else
                Result := E_SYNTAX;
              Inc(Idx);
            end;
          end else if T^.Tag = 6 then begin
            if (A^.Tag <> V_ASN1_IA5STRING) and (A <> T) then
              Result := E_SYNTAX
            else begin
              Name[Idx].uniformResourceIdentifier := A^.Content;
              Name[Idx].Tag := 6;
              Inc(Idx);
            end;
          end else if T^.Tag = 7 then begin
            if ((A^.Tag <> V_ASN1_OCTET_STRING) and (A <> T)) or
               ((A^.Length <> 4) and (A^.Length <> 8)) then
              Result := E_SYNTAX
            else begin
              Move(A^.Content^,Name[Idx].ipAddress,4);
              if A^.Length = 4 then
                Name[Idx].ipMask := 0
              else begin
                if not ((A^.Content[4] = #$FF) and
                        (A^.Content[5] in [#0,#$FF]) and
                        (A^.Content[6] in [#0,#$FF]) and
                        (A^.Content[7] in [#0,#$FF])) then
                  Result := E_SYNTAX
                else begin
                  if A^.Content[5] = #0 then begin
                    Name[Idx].ipMask := 1;
                    if not ((A^.Content[6] = #0) and
                            (A^.Content[7] = #0)) then
                      Result := E_SYNTAX;
                  end else if A^.Content[6] = #0 then begin
                    Name[Idx].ipMask := 2;
                    if not (A^.Content[7] = #0) then
                      Result := E_SYNTAX;
                  end else if A^.Content[7] = #0 then
                    Name[Idx].ipMask := 3
                  else
                    Name[Idx].ipMask := 4;
                end;
              end;
              Name[Idx].Tag := 7;
              Inc(Idx);
            end;
          end else if T^.Tag = 8 then begin
            if (A^.Tag <> V_ASN1_OBJECT) and (A <> T) then
              Result := E_SYNTAX
            else begin
              Name[Idx].registredID := A^.ContentAsOID;
              Name[Idx].Tag := 8;
              Inc(Idx);
            end;
          end else
            Result := E_SYNTAX;
        end;
      end;
      SetLength(Name,Idx);
    end;
  end;
end;

function Extractv2IssuerID(const Cert: TASN1Struct; var UniqueID: string): Integer;
var
  S, T: ^TASN1Struct;
begin
  UniqueID := '';
  Result := E_SYNTAX;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
      if S^.Contents^.ItemCount > 0 then begin
        T := @S^.Contents^.Items[0];
        if (T^.Cls = V_ASN1_CONTEXT_SPECIFIC) and T^.Constructed and
           (T^.Tag = 0) and (T^.Contents^.ItemCount = 1) then begin
          T := @T^.Contents^.Items[0];
          if T^.Constructed then
            Result := E_SYNTAX
          else if T^.Content[0] = #0 then
            Result := E_NOT_SUPPORTED
          else begin
            if (S^.Contents^.ItemCount > 7) then begin
              S := @S^.Contents^.Items[7];
              if (S^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                 (S^.Tag = 1) and not S.Constructed then begin
                Result := E_OK;
                SetLength(UniqueID,S^.Length);
                Move(S^.Content^,UniqueID[1],S^.Length);
              end else
                Result := E_EMPTY
            end else
              Result := E_EMPTY
          end;
        end;
      end;
    end;
  end;
end;

function Extractv2SubjectID(const Cert: TASN1Struct; var UniqueID: string): Integer;
var
  S, T: ^TASN1Struct;
begin
  UniqueID := '';
  Result := E_SYNTAX;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
      if S^.Contents^.ItemCount > 0 then begin
        T := @S^.Contents^.Items[0];
        if (T^.Cls = V_ASN1_CONTEXT_SPECIFIC) and T^.Constructed and
           (T^.Tag = 0) and (T^.Contents^.ItemCount = 1) then begin
          T := @T^.Contents^.Items[0];
          if T^.Constructed then
            Result := E_SYNTAX
          else if T^.Content[0] = #0 then
            Result := E_NOT_SUPPORTED
          else begin
            if (S^.Contents^.ItemCount > 7) then begin
              T := @S^.Contents^.Items[7];
              if (T^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                 (T^.Tag = 2) and not T.Constructed then begin
                Result := E_OK;
                SetLength(UniqueID,T^.Length);
                Move(T^.Content^,UniqueID[1],T^.Length);
              end else if (S^.Contents^.ItemCount > 8) then begin
                T := @S^.Contents^.Items[8];
                if (T^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                   (T^.Tag = 2) and not T.Constructed then begin
                  Result := E_OK;
                  SetLength(UniqueID,T^.Length);
                  Move(T^.Content^,UniqueID[1],T^.Length);
                end else
                  Result := E_EMPTY
              end else
                Result := E_EMPTY
            end else
              Result := E_EMPTY
          end;
        end;
      end;
    end;
  end;
end;

function Comparev2UniqueID(const UID0, UID1: string): Boolean;
begin
  Result := UID0 = UID1;
end;

function ExtractValidity(const Cert: TASN1Struct; var Validity: TX509Validity): Integer;
var
  S, T: ^TASN1Struct;
begin
  Result := E_SYNTAX;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 4) then begin
      if S^.Contents^.Items[0].Cls = V_ASN1_CONTEXT_SPECIFIC then
        S := @S^.Contents^.Items[4]
      else
        S := @S^.Contents^.Items[3];
      if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
         (S^.Contents^.ItemCount = 2) then begin
        T := @S^.Contents^.Items[0];
        if T^.Tag = V_ASN1_UTCTIME then begin
          if UTCTimeToDateTime(T^.Content,T^.Length,Validity.notBefore) then
            Result := E_OK;
        end else if T^.Tag = V_ASN1_GENERALIZEDTIME then begin
          if X509GeneralizedTimeToDateTime(T^.Content,T^.Length,Validity.notBefore) then
            Result := E_OK;
        end;
        T := @S^.Contents^.Items[1];
        if T^.Tag = V_ASN1_UTCTIME then begin
          if not UTCTimeToDateTime(T^.Content,T^.Length,Validity.notAfter) then
            Result := E_SYNTAX;
        end else if T^.Tag = V_ASN1_GENERALIZEDTIME then begin
          if not X509GeneralizedTimeToDateTime(T^.Content,T^.Length,Validity.notAfter) then
            Result := E_SYNTAX;
        end;
      end;
    end;
  end;
end;

function CheckValidity(const Cert: TASN1Struct; HoursOffsetFromGMT: Double): Boolean;
var
  Validity: TX509Validity;
  NZ: TDateTime;
begin
  Assert(Abs(HoursOffsetFromGMT) < 24);
  Result := ExtractValidity(Cert,Validity) = E_OK;
  if Result then begin
    NZ := Now - HoursOffsetFromGMT/24;
    Result := (NZ > Validity.notBefore) and (NZ < Validity.notAfter);
  end;
end;

procedure TranslateExtension(const Ext: TX509Extension; var Struct: TASN1Struct);
var
  S: PASN1Struct;
  MS: TSecureMemoryStream;
begin
  if Struct = nil then begin
    NewComposeASN1Struct(Struct,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  end else begin
    Struct.DisposeContent;
    Struct.Tag := V_ASN1_SEQUENCE;
    Struct.Cls := V_ASN1_UNIVERSAL;
    Struct.Constructed := True;
  end;
  Struct.Persistent := True;
  Struct.TypeName := 'Extension';
  Struct.AddField('extnID','OBJECT',Ext.extnID,True);
  Struct.AddField('critical','BOOLEAN','False',True);
  S := Struct.FindField('critical');
  S.SetAsDefault;
  if Ext.Critical then
    S^.EditContent('True');
  S := Struct.AddField('extnValue','OCTET STRING','',True);
  MS := TSecureMemoryStream.Create;
  try
    if Ext.extnValue.Constructed then
      Ext.extnValue.CalculateLength;
    ASN1ToStream(Ext.extnValue,MS);
    S^.SetContent(MS.Memory^,MS.Size);
  finally
    MS.Free;
  end;
  Struct.CalculateLength;
end;

procedure TranslateAttribute(const Ext: TX509Extension; var Struct: TASN1Struct);
var
  S: PASN1Struct;
begin
  if Struct = nil then begin
    NewComposeASN1Struct(Struct,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  end else begin
    Struct.DisposeContent;
    Struct.Tag := V_ASN1_SEQUENCE;
    Struct.Cls := V_ASN1_UNIVERSAL;
    Struct.Constructed := True;
  end;
  Struct.Persistent := True;
  Struct.TypeName := 'Attribute';
  Struct.AddField('name','OBJECT',Ext.extnID,True);
  Struct.AddField('values','SET of ANY',nil);
  S := Struct.Items[1];
  S^.AddField('',Ext.extnValue.TypeName,Ext.extnValue);
  Struct.CalculateLength;
end;

function InterpretAttribute(const Struct: TASN1Struct; var Ext: TX509Extension): Integer;
var
  T, R: ^TASN1Struct;
begin
  if Ext = nil then
    Ext := TX509Extension.Create;
  Result := E_SYNTAX;
  T := @Struct;
  if T^.Constructed and (T^.Tag = V_ASN1_SEQUENCE) and
     (T^.Contents^.ItemCount > 1) then begin
    Ext.Free;
    Ext := TX509Extension.Create;
    R := @T^.Contents^.Items[0];
    if R^.Tag = V_ASN1_OBJECT then begin
      Result := E_OK;
      Ext.extnID := R^.ContentAsOID;
      Ext.extnIDName := GetObjectName(Ext.extnID);
      R := @T^.Contents^.Items[1];
      if (R^.Tag = V_ASN1_SET) and (R^.ItemCount >= 1) then begin
        Ext.extnValue := TASN1Struct.Create;
        Ext.extnValue.Assign(R^.Items[0]^);
      end else
        Result := E_SYNTAX;
    end;
  end;
end;

function InterpretExtension(const Struct: TASN1Struct; var Ext: TX509Extension): Integer;
var
  T, R: ^TASN1Struct;
  MS: TReadStream;
begin
  if Ext = nil then
    Ext := TX509Extension.Create;
  Result := E_SYNTAX;
  T := @Struct;
  if T^.Constructed and (T^.Tag = V_ASN1_SEQUENCE) and
     (T^.Contents^.ItemCount > 1) then begin
    Ext.Free;
    Ext := TX509Extension.Create;
    R := @T^.Contents^.Items[0];
    if R^.Tag = V_ASN1_OBJECT then begin
      Result := E_OK;
      Ext.extnID := R^.ContentAsOID;
      Ext.extnIDName := GetObjectName(Ext.extnID);
      R := @T^.Contents^.Items[1];
      if R^.Tag = V_ASN1_BOOLEAN then begin
        if R^.Length <> 1 then
          Result := E_SYNTAX
        else begin
          Ext.Critical := (R^.Content[0] <> #0);
          R := @T^.Contents^.Items[2];
          if R^.Tag = V_ASN1_OCTET_STRING then begin
            MS := TReadStream.Create(R^.Content^,R^.Length);
            try
              MS.Position := 0;
              ASN1FromStream(MS,Ext.extnValue);
            finally
              MS.Free;
            end;
          end else
            Result := E_SYNTAX;
        end;
      end else if R^.Tag = V_ASN1_OCTET_STRING then begin
        Ext.Critical := False;
        MS := TReadStream.Create(R^.Content^,R^.Length);
        try
          MS.Position := 0;
          ASN1FromStream(MS,Ext.extnValue);
        finally
          MS.Free;
        end;
      end else
        Result := E_SYNTAX;
    end;
  end;
end;

function InterpretNamedExtension(const Struct: TASN1Struct; BEROID: string; var Ext: TX509Extension): Integer;
var
  T, R: ^TASN1Struct;
  MS: TReadStream;
begin
  if Ext = nil then
    Ext := TX509Extension.Create;
  Result := E_SYNTAX;
  T := @Struct;
  if T^.Constructed and (T^.Tag = V_ASN1_SEQUENCE) and
     (T^.Contents^.ItemCount > 1) then begin
    Ext.Free;
    Ext := TX509Extension.Create;
    R := @T^.Contents^.Items[0];
    if R^.Tag = V_ASN1_OBJECT then begin
      if StrLComp(R^.Content,PChar(BEROID),R^.Length) <> 0 then
        Result := E_NOT_SUPPORTED
      else begin
        Result := E_OK;
        Ext.extnID := R^.ContentAsOID;
        Ext.extnIDName := GetObjectName(Ext.extnID);
        R := @T^.Contents^.Items[1];
        if R^.Tag = V_ASN1_BOOLEAN then begin
          if R^.Length <> 1 then
            Result := E_SYNTAX
          else begin
            Ext.Critical := (R^.Content[0] <> #0);
            R := @T^.Contents^.Items[2];
            if R^.Tag = V_ASN1_OCTET_STRING then begin
              MS := TReadStream.Create(R^.Content^,R^.Length);
              try
                MS.Position := 0;            
                DisposeASN1Struct(Ext.extnValue);
                ASN1FromStream(MS,Ext.extnValue);
              finally
                MS.Free;
              end;
            end else
              Result := E_SYNTAX;
          end;
        end else if R^.Tag = V_ASN1_OCTET_STRING then begin
          Ext.Critical := False;
          MS := TReadStream.Create(R^.Content^,R^.Length);
          try
            MS.Position := 0;
            DisposeASN1Struct(Ext.extnValue);
            ASN1FromStream(MS,Ext.extnValue);
          finally
            MS.Free;
          end;
        end else
          Result := E_SYNTAX;
      end;
    end;
  end;
end;

function ExtractExtension(const Cert: TASN1Struct; Index: Integer; var Ext: TX509Extension): Integer;
var
  S, T, E: ^TASN1Struct;
begin
  Ext.Free;
  Ext := TX509Extension.Create;
  Result := E_SYNTAX;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
      if S^.Contents^.ItemCount > 0 then begin
        T := @S^.Contents^.Items[0];
        if (T^.Cls = V_ASN1_CONTEXT_SPECIFIC) and T^.Constructed and
           (T^.Tag = 0) and (T^.Contents^.ItemCount = 1) then begin
          T := @T^.Contents^.Items[0];
          if T^.Constructed then
            Result := E_SYNTAX
          else if T^.Content[0] <> #2 then
            Result := E_NOT_SUPPORTED
          else begin
            E := nil;
            if (S^.Contents^.ItemCount > 7) then begin
              E := @S^.Contents^.Items[7];
              if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                      (E^.Tag = 3) and not E.Constructed) and
                 (S^.Contents^.ItemCount > 8) then begin
                E := @S^.Contents^.Items[8];
                if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                        (E^.Tag = 3) and not E^.Constructed) and
                   (S^.Contents^.ItemCount > 9) then begin
                  E := @S^.Contents^.Items[9];
                  if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                          (E^.Tag = 3) and E.Constructed) then
                    E := nil;
                end;
              end;
            end;

            Result := E_OK;
            if (E = nil) or (E^.Constructed and (E^.Contents^.ItemCount < 1)) or
               not E^.Constructed then begin
              Result := E_SYNTAX;
              E := nil;
            end else
              E := @E^.Contents^.Items[0];

            if (E = nil) or (E^.Constructed and (E^.Contents^.ItemCount <= Index)) then begin
              if Result <> E_SYNTAX then
                Result := E_EMPTY;
            end else if E^.Constructed then begin
              T := @E^.Contents^.Items[Index];
              Result := InterpretExtension(T^,Ext);
            end else
              Result := E_SYNTAX;
          end;
        end;
      end;
    end;
  end;
end;

function ExtractCRLExtension(const CRL: TASN1Struct; Index: Integer; var Ext: TX509Extension): Integer;
var
  S, T, E: ^TASN1Struct;
begin
  Ext.Free;
  Ext := TX509Extension.Create;
  Result := E_SYNTAX;
  S := @CRL;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
      E := nil;
      if (S^.Contents^.ItemCount > 3) then begin
        E := @S^.Contents^.Items[3];
        if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                (E^.Tag = 0) and E.Constructed) then begin
          if S^.Contents^.ItemCount > 4 then begin
            E := @S^.Contents^.Items[4];
            if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                    (E^.Tag = 0) and E^.Constructed) then begin
              if S^.Contents^.ItemCount > 5 then begin
                E := @S^.Contents^.Items[5];
                if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                        (E^.Tag = 0) and E.Constructed) then begin
                  if S^.Contents^.ItemCount > 6 then begin
                    E := @S^.Contents^.Items[6];
                    if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                            (E^.Tag = 0) and E.Constructed) then
                      E := nil;
                  end else
                    E := nil;
                end;
              end else
                E := nil;
            end;
          end else
            E := nil;
        end;
      end;

      Result := E_OK;
      if (E = nil) or (E^.Constructed and (E^.Contents^.ItemCount < 1)) or
         not E^.Constructed then begin
        Result := E_NOT_SUPPORTED;
        E := nil;
      end else
        E := @E^.Contents^.Items[0];

      if (E = nil) or (E^.Constructed and (E^.Contents^.ItemCount <= Index)) then begin
        if (Result <> E_SYNTAX) and (Result <> E_NOT_SUPPORTED) then
          Result := E_EMPTY;
      end else if E^.Constructed then begin
        T := @E^.Contents^.Items[Index];
        Result := InterpretExtension(T^,Ext);
      end else
        Result := E_SYNTAX;
    end;
  end;
end;

procedure ImposeExtension(const Ext: TX509Extension; var Cert: TASN1Struct);
var
  vExt: TX509Extension;
  Idx: Integer;
  Res: Integer;
  S, T, E: ^TASN1Struct;
begin
  FillChar(vExt,SizeOf(vExt),0);
  Idx := 0;
  repeat
    Res := ExtractExtension(Cert,Idx,vExt);
    Inc(Idx);
  until (Res <> E_OK) or (vExt.extnID = Ext.extnID);
  Dec(Idx);
  DisposeASN1Struct(vExt.extnValue);

  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
      if S^.Contents^.ItemCount > 0 then begin
        T := @S^.Contents^.Items[0];
        if (T^.Cls = V_ASN1_CONTEXT_SPECIFIC) and T^.Constructed and
           (T^.Tag = 0) and (T^.Contents^.ItemCount = 1) then begin
          T := @T^.Contents^.Items[0];
          if T^.Constructed then
            raise Exception.Create('Syntax error')
          else if T^.Content[0] <> #2 then begin
            T^.Content[0] := #2;
            raise Exception.Create('Wrong version');
          end else begin
            E := nil;
            if (S^.Contents^.ItemCount > 7) then begin
              E := @S^.Contents^.Items[7];
              if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                      (E^.Tag = 3) and not E.Constructed) and
                 (S^.Contents^.ItemCount > 8) then begin
                E := @S^.Contents^.Items[8];
                if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                        (E^.Tag = 3) and not E^.Constructed) and
                   (S^.Contents^.ItemCount > 9) then begin
                  E := @S^.Contents^.Items[9];
                  if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                          (E^.Tag = 3) and not E.Constructed) then
                    raise Exception.Create('Syntax error');
                end;
              end;
            end;

            if E = nil then begin
              AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,3);
              E := @S^.Contents^.Items[S^.Contents^.ItemCount - 1];
              AddComposeASN1Field(E^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
            end;
            if (E^.Constructed and (E^.Contents^.ItemCount <> 1)) or
               not E^.Constructed then
              raise Exception.Create('Syntax error')
            else begin
              if E^.Contents = nil then begin
                AddComposeASN1Field(E^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
              end;
              E := @E^.Contents^.Items[0];
              if (E^.Contents = nil) or (Idx = E^.Contents^.ItemCount) then
                E^.AllocContents(Idx + 1);
              TranslateExtension(Ext,E^.Contents^.Items[Idx]);
            end;
          end;
        end else
          raise Exception.Create('Syntax error')
      end else
        raise Exception.Create('Syntax error')
    end else
      raise Exception.Create('Syntax error')
  end else
    raise Exception.Create('Syntax error')
end;

procedure ImposeCRLExtension(const Ext: TX509Extension; var CRL: TASN1Struct);
var
  vExt: TX509Extension;
  Idx: Integer;
  Res: Integer;
  S, T, E: ^TASN1Struct;
begin
  FillChar(vExt,SizeOf(vExt),0);
  Idx := 0;
  repeat
    Res := ExtractCRLExtension(CRL,Idx,vExt);
    Inc(Idx);
  until (Res <> E_OK) or (vExt.extnID = Ext.extnID);
  Dec(Idx);
  DisposeASN1Struct(vExt.extnValue);

  S := @CRL;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
      if S^.Contents^.ItemCount > 0 then begin
        T := @S^.Contents^.Items[0];
        if (T^.Cls = V_ASN1_UNIVERSAL) and not T^.Constructed and
           (T^.Tag = V_ASN1_INTEGER) then begin
          if T^.Content[0] <> #1 then begin
            T^.Content[0] := #1;
            raise Exception.Create('Wrong version');
          end else begin
            E := nil;
            if (S^.Contents^.ItemCount > 3) then begin
              E := @S^.Contents^.Items[3];
              if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                      (E^.Tag = 0) and E.Constructed) then begin
                if S^.Contents^.ItemCount > 4 then begin
                  E := @S^.Contents^.Items[4];
                  if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                          (E^.Tag = 0) and E^.Constructed) then begin
                    if S^.Contents^.ItemCount > 5 then begin
                      E := @S^.Contents^.Items[5];
                      if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                              (E^.Tag = 0) and not E.Constructed) then begin
                        if S^.Contents^.ItemCount > 6 then begin
                          E := @S^.Contents^.Items[6];
                          if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                                  (E^.Tag = 0) and E.Constructed) then
                            raise Exception.Create('Syntax error');
                        end else
                          E := nil;
                      end;
                    end else
                      E := nil;
                  end;
                end else
                  E := nil;
              end;
            end;

            if E = nil then begin
              AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,0);
              E := @S^.Contents^.Items[S^.Contents^.ItemCount - 1];
              AddComposeASN1Field(E^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
            end;
            if (E^.Constructed and (E^.Contents^.ItemCount <> 1)) or
               not E^.Constructed then
              raise Exception.Create('Syntax error')
            else begin
              if E^.Contents = nil then begin
                AddComposeASN1Field(E^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
              end;
              E := @E^.Contents^.Items[0];
              if (E^.Contents = nil) or (Idx = E^.Contents^.ItemCount) then
                E^.AllocContents(Idx + 1);
              TranslateExtension(Ext,E^.Contents^.Items[Idx]);
            end;
          end;
        end else
          raise Exception.Create('Wrong version')
      end else
        raise Exception.Create('Syntax error')
    end else
      raise Exception.Create('Syntax error')
  end else
    raise Exception.Create('Syntax error')
end;

function CheckCriticalExtensions(const Cert: TASN1Struct; var Ext: TX509Extension): Integer;           
var
  Idx: Integer;
begin
  Idx := 0;
  repeat
    Result := ExtractExtension(Cert,Idx,Ext);
    if (Result = E_OK) and
       Ext.Critical and
       ((Ext.extnIDName[1] = '{') or
        ((Ext.extnID <> id_ce_keyUsage) and
         (Ext.extnID <> id_ce_extKeyUsage) and
         (Ext.extnID <> id_ce_primary_key_usage_restriction) and // deprecated
         (Ext.extnID <> id_ce_basicConstraints) and
         (Ext.extnID <> id_ce_cRLDistributionPoints) and
         (Ext.extnID <> id_ce_subjectAltName) and
         (Ext.extnID <> id_ce_issuerAltName) and
         (Ext.extnID <> id_ce_authorityKeyIdentifier) and
         (Ext.extnID <> id_ce_subjectKeyIdentifier) and
         (Ext.extnID <> id_ce_nameConstraints) and
         (Ext.extnID <> id_ce_policyMappings) and
         (Ext.extnID <> id_ce_policyConstraints) and
         (Ext.extnID <> netscape_cert_type))) then begin
      Result := E_NOT_SUPPORTED;
      Exit;
    end else if Result = E_NOT_SUPPORTED then
      Result := E_EMPTY;
    Inc(Idx);
  until (Result <> E_OK);
//  DisposeASN1Struct(Ext.extnValue);
//  FillChar(Ext,SizeOf(Ext),0);
  FreeAndNil(Ext);
end;

function ExtractNamedExtension(const Cert: TASN1Struct; OID: string; var Ext: TX509Extension): Integer;
var
  S, T, E: PASN1Struct;
  Idx: Integer;
  BEROID: string;
begin     
  Ext.Free;
  Ext := TX509Extension.Create;
  Result := E_SYNTAX;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
      if S^.Contents^.ItemCount > 0 then begin
        T := @S^.Contents^.Items[0];
        if (T^.Cls = V_ASN1_CONTEXT_SPECIFIC) and T^.Constructed and
           (T^.Tag = 0) and (T^.Contents^.ItemCount = 1) then begin
          T := @T^.Contents^.Items[0];
          if T^.Constructed then
            Result := E_SYNTAX
          else if (T.Length <> 1) or (T^.Content[0] <> #2) then
            Result := E_NOT_SUPPORTED
          else begin
            E := nil;
            if (S^.Contents^.ItemCount > 7) then begin
              E := @S^.Contents^.Items[7];
              if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                      (E^.Tag = 3) and E.Constructed) and
                 (S^.Contents^.ItemCount > 8) then begin
                E := @S^.Contents^.Items[8];
                if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                        (E^.Tag = 3) and E^.Constructed) and
                   (S^.Contents^.ItemCount > 9) then begin
                  E := @S^.Contents^.Items[9];
                  if not ((E^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
                          (E^.Tag = 3) and E.Constructed) then
                    E := nil;
                end;
              end;
            end;

            Result := E_OK;
            if (E = nil) or (E^.Constructed and (E^.Contents^.ItemCount < 1)) or
               not E^.Constructed then begin
              Result := E_SYNTAX;
              E := nil;
            end else
              E := @E^.Contents^.Items[0];

            if E = nil then begin
              if Result <> E_SYNTAX then
                Result := E_EMPTY;
            end else if E^.Constructed then begin
              Idx := 0;
              BEROID := InterpretOIDtoBER(OID);
              while Idx < E^.ItemCount do begin
                T := E^.Items[Idx];
                Result := InterpretNamedExtension(T^,BEROID,Ext);
                Inc(Idx);
                if Result = E_OK then
                  Break
              end;
            end else
              Result := E_SYNTAX;
          end;
        end else
          Result := E_NOT_SUPPORTED;
      end;
    end;
  end;
end;

function ExtractKeyUsage(const Cert: TASN1Struct): TKeyUsage;
var
  Ext: TX509Extension;
  R: Integer;
  P: PASN1Struct;
begin
  Ext := nil;
  try
    Result := [];
    R := ExtractNamedExtension(Cert,id_ce_keyUsage,Ext);
    if R = E_OK then begin
      if not Ext.extnValue.Constructed then begin
        if Ext.extnValue.Length = 2 then
          Move(Ext.extnValue.Content[1],Result,1)
        else if Ext.extnValue.Length >= 3 then
          Move(Ext.extnValue.Content[1],Result,2);
      end else
        raise Exception.Create('ExtractKeyUsage: Syntax error');
    end else if (R = E_EMPTY) or (R = E_NOT_SUPPORTED) then begin
      R := ExtractNamedExtension(Cert,id_ce_primary_key_usage_restriction,Ext);
      if R = E_OK then begin
        P := nil;
        if Ext.extnValue.Constructed and
           (Ext.extnValue.ItemCount > 0) then begin
          if (Ext.extnValue.ItemCount = 1) and
             (Ext.extnValue.Items[0].ActualTag = V_ASN1_BIT_STRING) then
            P := Ext.extnValue.Items[0]
          else if (Ext.extnValue.ItemCount > 1) and
                  (Ext.extnValue.Items[1].ActualTag = V_ASN1_BIT_STRING) then
            P := Ext.extnValue.Items[1];
        end;
        if Assigned(P) then begin
          if P.Length = 2 then
            Move(P.Content[1],Result,1)
          else if P.Length >= 3 then
            Move(P.Content[1],Result,2);
        end;
      end;
      if Result = [] then
        Result := [Low(TKeyUsageItem)..High(TKeyUsageItem)];
    end else
      raise Exception.Create('ExtractKeyUsage: Syntax error');
  finally
    Ext.Free;
  end;
end;

function ExtractSubjectKeyIdentifier(const Cert: TASN1Struct): string;
var
  Ext: TX509Extension;
  R: Integer;
begin        
  Ext := nil;
  try
    R := ExtractNamedExtension(Cert,id_ce_subjectKeyIdentifier,Ext);
    if R = E_OK then
      Result := Ext.extnValue.ContentAsOctetString
    else if (R = E_EMPTY) or (R = E_NOT_SUPPORTED) then
      Result := ''
    else
      raise Exception.Create(Format('Syntax error: %d',[R]));
  finally
    Ext.Free;
  end;
end;             

function ExtractIssuerKeyIdentifier(const Cert: TASN1Struct): string;
var
  Ext: TX509Extension;
  R: Integer;
  S: PASN1Struct;
begin
  Ext := nil;
  try
    R := ExtractNamedExtension(Cert,id_ce_authorityKeyIdentifier,Ext);
    if R <> E_OK then
      R := ExtractNamedExtension(Cert,id_ce_authorityKeyIdentifier_old,Ext);
    if R = E_OK then begin
      if Ext.extnValue.Constructed and (Ext.extnValue.Tag = V_ASN1_SEQUENCE) and
         (Ext.extnValue.Contents^.ItemCount > 0) then begin
        S := @Ext.extnValue.Contents^.Items[0];
        if S^.Tag <> 0 then
          S := nil;
      end else
        S := nil;
      if Assigned(S) then begin
        if S^.Constructed then begin
          if S^.ItemCount <> 1 then
            S := nil
          else if S^.Items[0].Tag <> V_ASN1_OCTET_STRING then
            S := nil
          else
            S := S^.Items[0];
        end;
      end;
      if Assigned(S) then
        Result := S^.ContentAsOctetString;
    end else if (R = E_EMPTY) or (R = E_NOT_SUPPORTED) then
      Result := ''
    else
      raise Exception.Create('Syntax error');
  finally
    Ext.Free;
  end;
end;

function ExtractPathLenConstraint(const Cert: TASN1Struct): Integer;
var
  Ext: TX509Extension;
  R: Integer;
begin
  Result := -1;
  if keyCertSign in ExtractKeyUsage(Cert) then begin
    Ext := nil;
    try
      R := ExtractNamedExtension(Cert,id_ce_basicConstraints,Ext);
      if R = E_OK then begin
        if ASN1StructAssigned(Ext.extnValue) then begin
          if Ext.extnValue.ItemCount > 0 then begin
            if Ext.extnValue.Items[0].Tag = V_ASN1_INTEGER then
              raise Exception.Create('Syntax error: End entity must not have pathLenConstraints')
            else if Ext.extnValue.Items[0].Tag = V_ASN1_BOOLEAN then begin
              if (Ext.extnValue.Items[0].Length = 1) and
                 (Ext.extnValue.Items[0].Content[0] = #$FF) then begin
                if Ext.extnValue.ItemCount > 1 then
                  Result := Ext.extnValue.Items[1].ContentAsInteger
                else
                  Result := MaxInt;
              end else
                Result := -1; // CA = False, this is definite
            end else
              raise Exception.Create('Syntax error: Illegal syntax for basicConstraints');
          end else
            Result := -1; // CA = False (default), this is definite
        end else
          Result := -1; // CA = False (default), this is definite
      end else if (R = E_EMPTY) or (R = E_NOT_SUPPORTED) then
        Result := -2 // Arguable - some CA certs don't have BasicConstraints
      else
        raise Exception.Create('Syntax error');
    finally
      Ext.Free;
    end;
  end else
    Result := -1; // keyCertSign is explicitly excluded, this is definite
end;

function ExtractCRLDistributionPoints(const Cert: TASN1Struct; var DPs: TX509CRLDistPoints): Integer;
var
  Ext: TX509Extension;
  I, J: Integer;
  S, T, R: PASN1Struct;
begin
  SetLength(DPs,0);
  Ext := TX509Extension.Create;
  Result := ExtractNamedExtension(Cert,id_ce_CRLDistributionPoints,Ext);
  if Result = E_OK then begin 
    S := @Ext.extnValue;
    for I := 0 to S^.Contents^.ItemCount - 1 do begin
      T := @S^.Contents^.Items[I];
      if not T.Constructed or (T^.Tag <> V_ASN1_SEQUENCE) then
        Result := E_SYNTAX
      else begin
        SetLength(DPs,Length(DPs) + 1);
        DPs[Length(DPs) - 1].reasons := [lowReasonFlag..High(TX509ReasonFlag)];
        for J := 0 to T^.Contents^.ItemCount - 1 do begin
          R := @T^.Contents^.Items[J];
          case R^.Tag of
            0: begin
                 if R^.Contents^.Items[0].Tag = 0 then begin
                   DPs[Length(DPs) - 1].distributionPoint.Tag := 0;
                   Result := Result or
                             InterpretGeneralNames(R^.Contents^.Items[0],
                                                   DPs[Length(DPs) - 1].distributionPoint.fullName);
                 end else if R^.Contents^.Items[0].Tag = 1 then begin
                   R := @R^.Contents^.Items[0];
                   if not T.Constructed or (R^.Contents^.ItemCount <> 1) then
                     Result := E_SYNTAX
                   else begin
                     DPs[Length(DPs) - 1].distributionPoint.Tag := 1;
                     Result := Result or
                               InterpretName(R^.Contents^.Items[0],
                                             DPs[Length(DPs) - 1].distributionPoint.nameRelativeToCRLIssuer);
                   end;
                 end;
               end;
            1: begin
                 if R^.Constructed and (R^.ItemCount = 1) then
                   R := R.Items[0];
                 if ((R^.Tag <> V_ASN1_BIT_STRING) and (R^.Cls = V_ASN1_UNIVERSAL)) or
                    R^.Constructed or
                    ((R^.Cls <> V_ASN1_UNIVERSAL) and (R^.Cls <> V_ASN1_CONTEXT_SPECIFIC)) or
                    (R^.Length < 1) then
                   Result := E_SYNTAX
                 else begin
                   if R^.Length < SizeOf(TX509ReasonFlags) + 1 then begin
                     DPs[Length(DPs) - 1].reasons := [];
                     Move(R^.Content[1],
                          DPs[Length(DPs) - 1].reasons,
                          R^.Length)
                   end else
                     Move(R^.Content[1],
                          DPs[Length(DPs) - 1].reasons,
                          SizeOf(TX509ReasonFlags));
                 end;
               end;
            2: begin
                 if not R.Constructed or (R^.Contents^.ItemCount <> 1) then
                   Result := E_SYNTAX
                 else begin
                   DPs[Length(DPs) - 1].distributionPoint.Tag := 0;
                   Result := Result or
                             InterpretGeneralNames(R^.Contents^.Items[0],
                                                   DPs[Length(DPs) - 1].distributionPoint.fullName);
                 end;
               end;
          else
            Result := E_SYNTAX;
          end;
        end;
      end;
    end;
  end;
  Ext.Free;
end;

function ExtractDNSName(Cert: TASN1Struct): string;
var
  Ext: TX509Extension;
  I: Integer;
  P: PASN1Struct;
begin
  Result := '';
  Ext := nil;
  try
    if ExtractNamedExtension(Cert,id_ce_subjectAltName,Ext) = E_OK then
      if Assigned(Ext.extnValue) then
        for I := 0 to Ext.extnValue.ItemCount - 1 do begin
          P := Ext.extnValue.Items[I];
          if (P^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
             (P^.Tag = 2) and not P^.Constructed then begin
            Result := P^.ContentAsOctetString;
            Break;
          end;
        end;
  finally
    Ext.Free;
  end;
end;

function CheckDNSName(Cert: TASN1Struct; const ADNSName: string): Boolean;
var
  Ext: TX509Extension;
  I: Integer;
  P: PASN1Struct;
begin
  Result := True;
  Ext := nil;
  try
    if ExtractNamedExtension(Cert,id_ce_subjectAltName,Ext) = E_OK then begin
      if Assigned(Ext.extnValue) then begin
        for I := 0 to Ext.extnValue.ItemCount - 1 do begin
          P := Ext.extnValue.Items[I];
          if (P^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
             (P^.Tag = 2) and not P^.Constructed then begin
            Result := CompareText(ADNSName,P^.ContentAsOctetString) = 0;
            if Result then Break;
          end;
        end;
      end;
    end;
  finally
    Ext.Free;
  end;
end;

function ExtractEmailAddress(Cert: TASN1Struct): string;
var
  Ext: TX509Extension;
  I: Integer;
  P: PASN1Struct;
begin
  Result := '';
  Ext := nil;
  try
    if ExtractNamedExtension(Cert,id_ce_subjectAltName,Ext) = E_OK then begin
      if Assigned(Ext.extnValue) then begin
        for I := 0 to Ext.extnValue.ItemCount - 1 do begin
          P := Ext.extnValue.Items[I];
          if (P^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
             (P^.Tag = 1) and not P^.Constructed then begin
            Result := P^.ContentAsOctetString;
            Break;
          end;
        end;
      end;
    end;
  finally
    Ext.Free;
  end;
end;

function CheckEmailAddress(Cert: TASN1Struct; const AEmailAddress: string): Boolean;
var
  Ext: TX509Extension;
  I: Integer;
  P: PASN1Struct;
begin
  Result := True;
  Ext := nil;
  try
    if ExtractNamedExtension(Cert,id_ce_subjectAltName,Ext) = E_OK then begin
      if Assigned(Ext.extnValue) then begin
        for I := 0 to Ext.extnValue.ItemCount - 1 do begin
          P := Ext.extnValue.Items[I];
          if (P^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
             (P^.Tag = 1) and not P^.Constructed then begin
            Result := CompareText(AEmailAddress,P^.ContentAsOctetString) = 0;
            if Result then Break;
          end;
        end;
      end;
    end;
  finally
    Ext.Free;
  end;
end;

function ExtractURI(Cert: TASN1Struct): string;
var
  Ext: TX509Extension;
  I: Integer;
  P: PASN1Struct;
begin
  Result := '';
  Ext := nil;
  try
    if ExtractNamedExtension(Cert,id_ce_subjectAltName,Ext) = E_OK then
      if Assigned(Ext.extnValue) then
        for I := 0 to Ext.extnValue.ItemCount - 1 do begin
          P := Ext.extnValue.Items[I];
          if (P^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
             (P^.Tag = 6) and not P^.Constructed then begin
            Result := P^.ContentAsOctetString;
            Break;
          end;
        end;
  finally
    Ext.Free;
  end;
end;

function CheckURI(Cert: TASN1Struct; const AURI: string): Boolean;
var
  Ext: TX509Extension;
  I, J, K, L, Res: Integer;
  P: PASN1Struct;
  URI: string;
  Found: Boolean;
  Name: TX501Name;
  SL: TStringList;
begin
  Result := True;
  Ext := nil;
  try
    Found := False;
    if ExtractNamedExtension(Cert,id_ce_subjectAltName,Ext) = E_OK then begin
      if Assigned(Ext.extnValue) then begin
        for I := 0 to Ext.extnValue.ItemCount - 1 do begin
          P := Ext.extnValue.Items[I];
          if (P^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
             (P^.Tag = 6) and not P^.Constructed then begin
            URI := P^.ContentAsOctetString;
            Found := (Length(URI) <= Length(AURI)) and
                     (StrLIComp(PChar(AURI),PChar(URI),Length(URI)) = 0);
            if Found then Break;
            Result := False;
          end;
        end;
      end;
    end;
    if Result and not Found then begin
      Res := ExtractSubject(Cert,Name,False);
      if Res and (E_OK or E_STRING_FORMAT) = Res then begin
        SL := TStringList.Create;
        try
          SL.Text := Name.commonName.Str;
          for I := 0 to SL.Count - 1 do begin
            URI := SL[I];
            J := Pos('://',AURI);
            if J = 0 then
              J := 1
            else
              J := J + 3;
            {handle wildchars}
            K := Pos('*',URI);
            if K = 0 then
              K := 1
            else
            begin
              K := K + 2;
              L := Pos('.',AURI);
              if L = 0 then
                K := 1
              else
                J := L + 1;
            end;
            Found := StrLIComp(PChar(@URI[K]),PChar(@AURI[J]),Length(PChar(@URI[K]))) = 0;
            if Found then Break;
            Result := False;
          end;
        finally
          SL.Free;
        end;
      end;
    end;
  finally
    Ext.Free;
  end;
end;

function ExtractIP(Cert: TASN1Struct): string;
type
  PWord = ^Word;
var
  Ext: TX509Extension;
  I: Integer;
  P: PASN1Struct;
  Addr: string;
  W: Word;
begin
  Addr := '';
  Ext := nil;
  try
    if ExtractNamedExtension(Cert,id_ce_subjectAltName,Ext) = E_OK then
      if Assigned(Ext.extnValue) then
        for I := 0 to Ext.extnValue.ItemCount - 1 do begin
          P := Ext.extnValue.Items[I];
          if (P^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
             (P^.Tag = 7) and not P^.Constructed then begin
            Addr := P^.ContentAsOctetString;
            Break;
          end;
        end;
  finally
    Ext.Free;
  end;
  Result := '';
  if Length(Addr) = 4 then
    for I := 1 to Length(Addr) do
      Result := Result + IntToStr(Byte(Addr[I])) + '.'
  else
    for I := 1 to Length(Addr) shr 1 do begin
      W := PWord(@Addr[I*2])^;
      Result := Result + IntToHex((W shr 8) + Word(W shl 8),1) + ':';
    end;
  if Result <> '' then
    Delete(Result,Length(Result),1);
end;

function CheckIP(Cert: TASN1Struct; const AIP: string): Boolean;
var
  Addr: string;
  Ext: TX509Extension;
  I, J: Integer;
  P: PASN1Struct;
  V: LongInt;
  IPv6: Boolean;
begin
  J := 1;
  I := 1;
  Addr := '';
  IPv6 := False;
  while I < Length(AIP) do begin
    case AIP[I] of
      '.':
        begin
          V := StrToIntDef(Copy(AIP,J,I-J),0);
          Addr := Addr + Char(V);
          J := I + 1;
        end;
      ':':
        begin
          V := StrToIntDef('$' + Copy(AIP,J,I-J),0);
          Addr := Addr + Char(V shr 8) + Char(V and $FF);
          J := I + 1;
          IPv6 := True;
        end;
    end;
    Inc(I);
  end;
  if IPv6 then
    V := StrToIntDef('$' + Copy(AIP,J,MaxInt),0)
  else
    V := StrToIntDef(Copy(AIP,J,MaxInt),0);
  Addr := Addr + Char(V);

  Result := True;
  Ext := nil;
  try
    if ExtractNamedExtension(Cert,id_ce_subjectAltName,Ext) = E_OK then begin
      if Assigned(Ext.extnValue) then begin
        for I := 0 to Ext.extnValue.ItemCount - 1 do begin
          P := Ext.extnValue.Items[I];
          if (P^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
             (P^.Tag = 7) and not P^.Constructed then begin
            Result := CompareText(Addr,P^.ContentAsOctetString) = 0;
            if Result then Break;
          end;
        end;
      end;
    end;
  finally
    Ext.Free;
  end;
end;

function CheckCertificatePolicy(const Cert: TASN1Struct;
                                const Policy: ObjectIdentifier): Boolean;
var
  Ext: TX509Extension;
  R, I: Integer;
begin
  Result := False;
  Ext := nil;
  try
    R := ExtractNamedExtension(Cert,id_ce_certificatePolicies,Ext);
    if R = E_OK then begin
      Result := False;
      for I := 0 to Ext.extnValue.ItemCount - 1 do
        if Ext.extnValue.Items[I].Items[0].ContentAsOID = Policy then begin
          Result := True;
          Break;
        end;
    end else if (R = E_EMPTY) or (R = E_NOT_SUPPORTED) then
      Result := False
    else
      raise Exception.Create('CheckCertificatePolicy: Syntax error');
  finally
    Ext.Free;
  end;
end;

function SuggestIssuingDistributionPoints(const CACert: TASN1Struct;
                                          const Reasons: TX509ReasonFlags;
                                          var IDPs: TX509issuingDistributionPoints): Integer;
var
  DPs: TX509CRLDistPoints;
  I, Res: Integer;
begin
  Result := 0;
  SetLength(IDPs,Result);

  SetLength(DPs,0);
  Res := ExtractCRLDistributionPoints(CACert,DPs);
  if Res = E_OK then
    for I := 0 to Length(DPs) - 1 do
      if DPs[I].reasons >= Reasons then begin
        SetLength(IDPs,Result + 1);
        IDPs[Result].distributionPoint := DPs[I].distributionPoint;
        IDPs[Result].onlySomeReasons := DPs[I].reasons;
        IDPs[Result].onlyContainsUserCerts := False;
        IDPs[Result].onlyContainsCACerts := False;
        IDPs[Result].indirectCRL := False;
        Inc(Result);
      end;
end;

function ExtractNamedCRLExtension(const CRL: TASN1Struct; OID: string; var Ext: TX509Extension): Integer;
var
  Idx: Integer;
begin  
  Idx := 0;
  repeat
    Result := ExtractCRLExtension(CRL,Idx,Ext);
    Inc(Idx);
  until (Result <> E_OK) or (Ext.extnID = OID);
end;

function ExtractCRLNumber(const CRL: TASN1Struct): Integer;
var
  Ext: TX509Extension;
  I: Integer;
begin
  Result := -1;
  Ext := TX509Extension.Create;
  if ExtractNamedCRLExtension(CRL,id_ce_CRLNumber,Ext) = E_OK then begin
    Result := 0;
    for I := 0 to Ext.extnValue.Length - 1 do
      Result := (Result shl 8) or Byte(Ext.extnValue.Content[I]);
  end;
  Ext.Free;
end;         

function ExtractBaseCRLNumber(const CRL: TASN1Struct): Integer;
var
  Ext: TX509Extension;
  I: Integer;
begin
  Result := -1;
  Ext := TX509Extension.Create;
  if ExtractNamedCRLExtension(CRL,id_ce_deltaCRLIndicator,Ext) = E_OK then begin
    Result := 0;
    for I := 0 to Ext.extnValue.Length - 1 do
      Result := (Result shl 8) or Byte(Ext.extnValue.Content[I]);
  end;
  Ext.Free;
end;

function ExtractIssuingDistributionPoint(const CRL: TASN1Struct; var IDP: TX509issuingDistributionPoint): Integer;
var
  Ext: TX509Extension;
  I: Integer;
  S, T: PASN1Struct;
begin
  Finalize(IDP);
  FillChar(IDP,SizeOf(IDP),0);
  IDP.distributionPoint.Tag := 255;
  IDP.onlyContainsUserCerts := False;
  IDP.onlyContainsCACerts := False;
  IDP.onlySomeReasons := [lowReasonFlag..High(TX509ReasonFlag)];
  IDP.indirectCRL := False;
  Ext := TX509Extension.Create;
  Result := ExtractNamedCRLExtension(CRL,id_ce_issuingDistributionPoint,Ext);
  if Result = E_OK then begin
    S := @Ext.extnValue;
    for I := 0 to S^.Contents^.ItemCount - 1 do begin
      T := @S^.Contents^.Items[I];
      case T^.Tag of
        0: begin
             if T^.Contents^.Items[0].Tag = 0 then begin
               IDP.distributionPoint.Tag := 0;
               Result := Result or InterpretGeneralNames(T^.Contents^.Items[0],IDP.distributionPoint.fullName);
             end else if T^.Contents^.Items[0].Tag = 1 then begin
               T := @T^.Contents^.Items[0];
               if not T.Constructed or (T^.Contents^.ItemCount <> 1) then
                 Result := E_SYNTAX
               else begin
                 IDP.distributionPoint.Tag := 1;
                 Result := Result or InterpretName(T^.Contents^.Items[0],IDP.distributionPoint.nameRelativeToCRLIssuer);
               end;
             end;
           end;
        1: begin
             if T^.Constructed then
               T := T^.Items[0];
             if ((T^.Tag <> V_ASN1_BOOLEAN) and (T^.Cls <> V_ASN1_CONTEXT_SPECIFIC)) or
                T^.Constructed or
                ((T^.Cls <> V_ASN1_UNIVERSAL) and (T^.Cls <> V_ASN1_CONTEXT_SPECIFIC)) or
                (T^.Length <> 1) then
               Result := E_SYNTAX
             else
               IDP.onlyContainsUserCerts := T^.Content[0] <> #0;
           end;
        2: begin
             if T^.Constructed then
               T := T^.Items[0];
             if ((T^.Tag <> V_ASN1_BOOLEAN) and (T^.Cls <> V_ASN1_CONTEXT_SPECIFIC)) or
                T^.Constructed or
                ((T^.Cls <> V_ASN1_UNIVERSAL) and (T^.Cls <> V_ASN1_CONTEXT_SPECIFIC)) or
                (T^.Length <> 1) then
               Result := E_SYNTAX
             else
               IDP.onlyContainsCACerts := T^.Content[0] <> #0;
           end;
        3: begin
             if T^.Constructed then
               T := T^.Items[0];
             if ((T^.Tag <> V_ASN1_BIT_STRING) and (T^.Cls <> V_ASN1_CONTEXT_SPECIFIC)) or
                T^.Constructed or
                ((T^.Cls <> V_ASN1_UNIVERSAL) and (T^.Cls <> V_ASN1_CONTEXT_SPECIFIC)) or
                (T^.Length < 1) then
               Result := E_SYNTAX
             else begin
               if T^.Length < SizeOf(TX509ReasonFlags) + 1 then
                 Move(T^.Content[1],
                      IDP.onlySomeReasons,
                      T^.Length)
               else
                 Move(T^.Content[1],
                      IDP.onlySomeReasons,
                      SizeOf(TX509ReasonFlags));
             end;
           end;
        4: begin
             if T^.Constructed then
               T := T^.Items[0];
             if ((T^.Tag <> V_ASN1_BOOLEAN) and (T^.Cls <> V_ASN1_CONTEXT_SPECIFIC)) or
                T^.Constructed or
                ((T^.Cls <> V_ASN1_UNIVERSAL) and (T^.Cls <> V_ASN1_CONTEXT_SPECIFIC)) or
                (T^.Length <> 1) then
               Result := E_SYNTAX
             else
               IDP.indirectCRL := T^.Content[0] <> #0;
           end;
      else
        Result := E_SYNTAX;
      end
    end;
  end;
  Ext.Free;
end;

function CompareIssuingDistributionPoints(const IDP0, IDP1: TX509issuingDistributionPoint): Integer;
begin
  if (IDP0.onlyContainsUserCerts = IDP1.onlyContainsUserCerts) and
     (IDP0.onlyContainsCACerts = IDP1.onlyContainsCACerts) then begin
    if IDP0.onlySomeReasons = IDP1.onlySomeReasons then
      Result := 0
    else if IDP0.onlySomeReasons <= IDP1.onlySomeReasons then
      Result := 1
    else
      Result := -1;
  end else
    Result := -1;
end;

function CertIsRevoked(const Cert, CRL: TASN1Struct): Integer;
var
  S, T, R, U: ^TASN1Struct;
  I: Integer;
  SR, UC: string;
begin
  {
  if not (IsAllegedIssuer(Cert,CACert) and
          IsAllegedCRLIssuer(CRL,CACert)) then
    Result := -1
  else
    Result := 0;
  }
  Result := 0;
  if Result >= 0 then begin
    Result := -1;
    ExtractSerial(Cert,SR);
    while (SR <> '') and (SR[1] = #0) do
      Delete(SR,1,1);
    S := @CRL;
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 0) then begin
      S := @S^.Contents^.Items[0];
      if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
        R := nil;
        if (S^.Contents^.ItemCount > 3) then begin
          R := @S^.Contents^.Items[3];
          if not ((R^.Cls = V_ASN1_UNIVERSAL) and
                  (R^.Tag = V_ASN1_SEQUENCE) and R.Constructed) then begin
            if S^.Contents^.ItemCount > 4 then begin
              R := @S^.Contents^.Items[4];
              if not ((R^.Cls = V_ASN1_UNIVERSAL) and
                      (R^.Tag = V_ASN1_SEQUENCE) and R^.Constructed) then begin
                if S^.Contents^.ItemCount > 5 then begin
                  R := @S^.Contents^.Items[5];
                  if not ((R^.Cls = V_ASN1_UNIVERSAL) and
                          (R^.Tag = V_ASN1_SEQUENCE) and R.Constructed) then
                    R := nil;
                end else
                  R := nil;
              end;
            end else
              R := nil;
          end;
        end;

        if (R = nil) or (R^.Constructed and (R^.Contents^.ItemCount <= 0)) then
          Result := -1
        else if R^.Constructed then begin
          Result := -1;
          for I := 0 to R^.Contents^.ItemCount - 1 do begin
            T := @R^.Contents^.Items[I];
            if T^.Contents^.ItemCount >= 2 then begin
              U := @T^.Contents^.Items[0];
              if (U^.Cls = V_ASN1_UNIVERSAL) and
                 (U^.Tag = V_ASN1_INTEGER) and not U^.Constructed then begin
                SetLength(UC,U^.Length);
                Move(U^.Content^,UC[1],U^.Length);
                while (UC <> '') and (UC[1] = #0) do
                  Delete(UC,1,1);
                if Length(SR) = Length(UC) then begin
                  if CompareMem(Pointer(SR),Pointer(UC),Length(SR)) then
                    Result := I;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function ExtractRevokedCertificate(const CRL: TASN1Struct; Index: Integer;
  var RC: TX509RevokedCertificate): Integer;
var
  S, T, R, U: ^TASN1Struct;
  J: Integer;
  Ext: TX509Extension;
begin
  Result := E_SYNTAX;
  S := @CRL;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
      R := nil;
      if (S^.Contents^.ItemCount > 3) then begin
        R := @S^.Contents^.Items[3];
        if not ((R^.Cls = V_ASN1_UNIVERSAL) and
                (R^.Tag = V_ASN1_SEQUENCE) and R.Constructed) then begin
          if S^.Contents^.ItemCount > 4 then begin
            R := @S^.Contents^.Items[4];
            if not ((R^.Cls = V_ASN1_UNIVERSAL) and
                    (R^.Tag = V_ASN1_SEQUENCE) and R^.Constructed) then begin
              if S^.Contents^.ItemCount > 5 then begin
                R := @S^.Contents^.Items[5];
                if not ((R^.Cls = V_ASN1_UNIVERSAL) and
                        (R^.Tag = V_ASN1_SEQUENCE) and R.Constructed) then
                  R := nil;
              end else
                R := nil;
            end;
          end else
            R := nil;
        end;
      end;

      if (R = nil) or (R^.Constructed and (R^.Contents^.ItemCount <= 0)) then
        Result := E_EMPTY
      else if R^.Constructed then begin
        Result := E_OK;
        if Index >= R^.Contents^.ItemCount then
          Result := E_EMPTY
        else begin
          T := @R^.Contents^.Items[Index];
          if T^.Contents^.ItemCount < 2 then
            Result := E_SYNTAX
          else begin
            U := @T^.Contents^.Items[0];
            if (U^.Cls = V_ASN1_UNIVERSAL) and
               (U^.Tag = V_ASN1_INTEGER) and not U^.Constructed then begin
              RC.Free;
              RC := TX509RevokedCertificate.Create;

              SetLength(RC.userCertificate,U^.Length);
              Move(U^.Content^,RC.userCertificate[1],U^.Length);
              U := @T^.Contents^.Items[1];
              if (U^.Cls = V_ASN1_UNIVERSAL) and
                 ((U^.Tag = V_ASN1_UTCTIME) or
                  (U^.Tag = V_ASN1_GENERALIZEDTIME)) and
                 not U^.Constructed then begin
                if U^.Tag = V_ASN1_UTCTIME then begin
                  if not UTCTimeToDateTime(U^.Content,U^.Length,RC.revocationDate) then
                    Result := E_SYNTAX;
                end else
                  if not X509GeneralizedTimeToDateTime(U^.Content,U^.Length,RC.revocationDate) then
                    Result := E_SYNTAX;
                if (Result = E_OK) and (T^.Contents^.ItemCount > 2) then begin
                  U := @T^.Contents^.Items[2];
                  if (U^.Cls = V_ASN1_UNIVERSAL) and
                     (U^.Tag = V_ASN1_SEQUENCE) and U^.Constructed then begin
                    Ext := nil;
                    RC.Count := U^.Contents^.ItemCount;
                    for J := 0 to U^.Contents^.ItemCount - 1 do begin
                      Result := InterpretExtension(U^.Contents^.Items[J],
                                                   Ext);
                      RC[J] := Ext;
                      if Result = E_SYNTAX then Break;
                    end;
                    Ext.Free;
                  end else
                    Result := E_SYNTAX;
                end;
              end else
                Result := E_SYNTAX;
            end else
              Result := E_SYNTAX;
          end;
        end;
      end;
    end;
  end;
end;

function ExtractRevokedCertificates(const CRL: TASN1Struct;
  var RCList: TX509RevokedCertificates): Integer;
var
  S, T, R, U: ^TASN1Struct;
  I, J: Integer;
  Ext: TX509Extension;
begin
  Result := E_SYNTAX;
  S := @CRL;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) then begin
      R := nil;
      if (S^.Contents^.ItemCount > 3) then begin
        R := @S^.Contents^.Items[3];
        if not ((R^.Cls = V_ASN1_UNIVERSAL) and
                (R^.Tag = V_ASN1_SEQUENCE) and R.Constructed) then begin
          if S^.Contents^.ItemCount > 4 then begin
            R := @S^.Contents^.Items[4];
            if not ((R^.Cls = V_ASN1_UNIVERSAL) and
                    (R^.Tag = V_ASN1_SEQUENCE) and R^.Constructed) then begin
              if S^.Contents^.ItemCount > 5 then begin
                R := @S^.Contents^.Items[5];
                if not ((R^.Cls = V_ASN1_UNIVERSAL) and
                        (R^.Tag = V_ASN1_SEQUENCE) and R.Constructed) then
                  R := nil;
              end else
                R := nil;
            end;
          end else
            R := nil;
        end;
      end;

      if (R = nil) or (R^.Constructed and (R^.Contents^.ItemCount <= 0)) then
        Result := E_EMPTY
      else if R^.Constructed then begin
        Result := E_OK;
        RCList.Free;
        RCList := TX509RevokedCertificates.Create;
        RCList.Count :=  R^.Contents^.ItemCount;
        for I := 0 to R^.Contents^.ItemCount - 1 do begin
          T := @R^.Contents^.Items[I];
          if T^.Contents^.ItemCount < 2 then
            Result := E_SYNTAX
          else begin
            U := @T^.Contents^.Items[0];
            if (U^.Cls = V_ASN1_UNIVERSAL) and
               (U^.Tag = V_ASN1_INTEGER) and not U^.Constructed then begin
              SetLength(RCList[I].userCertificate,U^.Length);
              Move(U^.Content^,RCList[I].userCertificate[1],U^.Length);
              U := @T^.Contents^.Items[1];
              if (U^.Cls = V_ASN1_UNIVERSAL) and
                 ((U^.Tag = V_ASN1_UTCTIME) or
                  (U^.Tag = V_ASN1_GENERALIZEDTIME)) and
                 not U^.Constructed then begin
                if U^.Tag = V_ASN1_UTCTIME then begin
                  if not UTCTimeToDateTime(U^.Content,U^.Length,RCList[I].revocationDate) then
                    Result := E_SYNTAX;
                end else
                  if not X509GeneralizedTimeToDateTime(U^.Content,U^.Length,RCList[I].revocationDate) then
                    Result := E_SYNTAX;
                if (Result = E_OK) and (T^.Contents^.ItemCount > 2) then begin
                  U := @T^.Contents^.Items[2];
                  if (U^.Cls = V_ASN1_UNIVERSAL) and
                     (U^.Tag = V_ASN1_SEQUENCE) and U^.Constructed then begin
                    Ext := nil;
                    RCList[I].Count := U^.Contents^.ItemCount;
                    for J := 0 to U^.Contents^.ItemCount - 1 do begin
                      Result := InterpretExtension(U^.Contents^.Items[J],
                                                   Ext);
                      RCList[I][J] := Ext;
                      if Result = E_SYNTAX then Break;
                    end;
                    Ext.Free;
                  end else
                    Result := E_SYNTAX;
                end;
              end else
                Result := E_SYNTAX;
            end else
              Result := E_SYNTAX;
          end;
          if Result = E_SYNTAX then begin
            RCList.Count := 0;
          end;
        end;
      end;
    end;
  end;
end;

function InterpretAlgorithmIdentifier(const Struct: TASN1Struct; var Alg: TX509AlgorithmIdentifier): Integer;
var
  S, T: ^TASN1Struct;
begin
  Result := E_SYNTAX;
  S := @Struct;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    T := @S^.Contents^.Items[0];
    if T^.Tag = V_ASN1_OBJECT then begin
      Alg.Free;
      Alg := TX509AlgorithmIdentifier.Create;
      Alg.Algorithm := T^.ContentAsOID;
      Alg.AlgorithmName := GetObjectName(Alg.Algorithm);
      if S^.Contents^.ItemCount > 1 then begin
        Alg.Parameters := TASN1Struct.Create;
        Alg.Parameters.Assign(S^.Contents^.Items[1]);
      end;
      Result := E_OK;
    end;
  end;
end;

function ExtractSignatureAlgorithmCertOrCRL(const Cert: TASN1Struct; var Alg: TX509AlgorithmIdentifier): Integer;
var
  S: ^TASN1Struct;
  A: TX509AlgorithmIdentifier;
begin
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 2) then begin
      if S^.Contents^.Items[0].Tag = V_ASN1_SEQUENCE then
        S := @S^.Contents^.Items[0]
      else if S^.Contents^.Items[1].Tag = V_ASN1_SEQUENCE then
        S := @S^.Contents^.Items[1]
      else
        S := @S^.Contents^.Items[2];
      A := nil;
      Result := InterpretAlgorithmIdentifier(S^,A);
      if Result = E_OK then begin

        S := @Cert;
        if S^.Contents^.ItemCount > 1 then begin
          S := @S^.Contents^.Items[1];
          Result := InterpretAlgorithmIdentifier(S^,Alg);
          if Result = E_OK then begin
            if Alg.AlgorithmName <> A.AlgorithmName then
              Result := E_MISMATCH
            else begin
              Result := E_OK;
              if ASN1StructAssigned(A.Parameters) and
                 ASN1StructAssigned(Alg.Parameters) and
                 not ASN1StructCompare(A.Parameters,Alg.Parameters) then
                Result := E_MISMATCH;
            end;
          end else
            Result := E_SYNTAX;
        end else
          Result := E_SYNTAX;
      end else
        Result := E_SYNTAX;
      A.Free;
    end else
      Result := E_SYNTAX;
  end else
    Result := E_SYNTAX;
end;

function ExtractSignatureAlgorithm(const Signed: TASN1Struct; var Alg: TX509AlgorithmIdentifier): Integer;
var
  S: ^TASN1Struct;
begin
  S := @Signed;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 1) then begin
    S := @S^.Contents^.Items[1];
    Result := InterpretAlgorithmIdentifier(S^,Alg);
  end else
    Result := E_SYNTAX;
end;

procedure TranslatePublicKey(const PK: TX509PublicKey; var Struct: TASN1Struct);
var
  MS: TSecureMemoryStream;
  B: Byte;
begin
  if Struct = nil then begin
    NewComposeASN1Struct(Struct,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  end else begin
    Struct.DisposeContent;
    Struct.Tag := V_ASN1_SEQUENCE;
    Struct.Cls := V_ASN1_UNIVERSAL;
    Struct.Constructed := True;
  end;
  Struct.AddField('algorithm','AlgorithmIdentifier',nil);
  Struct.FindField('algorithm').Tag := V_ASN1_SEQUENCE;
  Struct.FindField('algorithm').AddField('algorithm','OBJECT',PK.Algorithm.Algorithm,True);
  if ASN1StructAssigned(PK.Algorithm.Parameters) then
    Struct.FindField('algorithm').AddField('parameters',PK.Algorithm.Parameters.TypeName,PK.Algorithm.Parameters);
  MS := TSecureMemoryStream.Create;
  try
    Struct.AddField('subjectPublicKey','BIT STRING','',True);
    B := 0;
    MS.Write(B,1);
    if PK.KeyAlgorithmType = katEC then
      MS.Write(PK.PublicKey.Content^,PK.PublicKey.Length)
    else
      ASN1ToStream(PK.PublicKey,MS);
    MS.Position := 0;
    Struct.Contents^.Items[1].SetContent(MS.Memory^,MS.Size);
  finally
    MS.Free;
  end;
  Struct.CalculateLength;
end;

function ExtractSubjectPublicKeyStruct(const Cert: TASN1Struct; var PK: PASN1Struct): Integer;
var
  S, T, U: PASN1Struct;
begin
  PK := nil;
  S := @Cert;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 5) then begin
      T := @S^.Contents^.Items[0];
      if (T^.Cls = V_ASN1_CONTEXT_SPECIFIC) and T^.Constructed and
         (T^.Tag = 0) and (T^.Contents^.ItemCount = 1) then
        S := @S^.Contents^.Items[6]
      else
        S := @S^.Contents^.Items[5];
      if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
         (S^.Contents^.ItemCount = 2) then begin
        Result := E_OK;
        T := @S^.Contents^.Items[0];
        if T^.Constructed and (T^.Tag = V_ASN1_SEQUENCE) and
           (T^.Contents^.ItemCount > 0) then begin
          U := @T^.Contents^.Items[0];
          if U^.Tag <> V_ASN1_OBJECT then
            Result := E_SYNTAX;
        end else
          Result := E_SYNTAX;
        if Result = E_OK then
          PK := S;
      end else
        Result := E_SYNTAX;
    end else
      Result := E_SYNTAX;
  end else
    Result := E_SYNTAX;
end;

function ExtractSubjectPublicKey(const Cert: TASN1Struct; var PK: TX509PublicKey): Integer;
var
  S, T, U: PASN1Struct;
begin
  Result := ExtractSubjectPublicKeyStruct(Cert,S);
  if Result = E_OK then begin
    if PK = nil then
      PK := TX509PublicKey.Create;
    U := S^.Items[1];
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 0) then begin
      T := @S^.Contents^.Items[0];
      if T^.Tag = V_ASN1_OBJECT then begin
        PK.Algorithm.Algorithm := T^.ContentAsOID;
        PK.Algorithm.AlgorithmName := GetObjectName(PK.Algorithm.Algorithm);
        if S^.Contents^.ItemCount > 1 then begin
          if PK.Algorithm.Parameters = nil then
            PK.Algorithm.Parameters := TASN1Struct.Create;
          PK.Algorithm.Parameters.Assign(S^.Contents^.Items[1]);
        end;
        Result := E_OK;
      end else
        Result := E_SYNTAX;
    end else
      Result := E_SYNTAX;
    if Result = E_OK then begin
      if PK.KeyAlgorithmType = katEC then begin
        PK.PublicKey := TASN1Struct.Create;
        PK.PublicKey.Tag := V_ASN1_OCTET_STRING;
        PK.PublicKey.SetContent(U^.Content[1],U^.Length);
      end else
        U^.ContentAsASN1Struct(PK.PublicKey);
    end;
  end;
end;

function ExtractSubjectPublicKeyAlg(const Cert: TASN1Struct; var OID: string): Integer;
var
  PK: TX509PublicKey;
begin
  PK := nil;
  try
    Result := ExtractSubjectPublicKey(Cert,PK);
    if Result = E_OK then
      OID := PK.Algorithm.Algorithm;
  finally
    PK.Free;
  end;
end;

function ExtractSubjectPublicKeySize(const Cert: TASN1Struct; var KeySize: Integer): Integer;
var
  RSAKey: TIFPublicKey;
  DLKey: TDLPublicKey;
  ECKey: TECPublicKey;
begin
  FillChar(RSAKey,SizeOf(RSAKey),0);
  FillChar(DLKey,SizeOf(DLKey),0);  
  FillChar(ECKey,SizeOf(ECKey),0);
  Result := ExtractSubjectRSAPublicKey(Cert,RSAKey,True);
  if Result = E_OK then begin
    KeySize := MPMSB(RSAKey.N);
  end else begin
    Result := ExtractSubjectDHPublicKey(Cert,DLKey);
    if Result = E_OK then begin
      KeySize := MPMSB(DLKey.Params.P);
      DisposeDLPublicKey(DLKey);
    end else begin
      Result := ExtractSubjectDSAPublicKey(Cert,DLKey);
      if Result = E_OK then begin
        KeySize := MPMSB(DLKey.Params.P);
      end else begin
        Result := ExtractSubjectECPublicKey(Cert,ECKey);
        if Result = E_OK then begin
          KeySize := MPMSB(ECKey.Params.Q);
          if (KeySize >= 163) and (KeySize < 283) then
            KeySize := Trunc((3072-1024)*(KeySize-163)/(283-163)+1024)
          else if (KeySize >= 283) and (KeySize < 409) then
            KeySize := Trunc((7680-3072)*(KeySize-283)/(409-283)+3072)
          else if (KeySize >= 409) and (KeySize < 571) then
            KeySize := Trunc((15360-7680)*(KeySize-409)/(571-409)+7680);
        end;
      end;
    end;
  end;
  DisposeIFPublicKey(RSAKey);
  DisposeDLPublicKey(DLKey);
  DisposeECPublicKey(ECKey);
end;

function IsAllegedIssuer(const Cert, CACert: TASN1Struct): Boolean;
var
  UKI, SKI, AKI: TX509Extension;
  S: PASN1Struct;
  A, N: PASN1Struct;
begin
  Result := False;
  UKI := nil;
  if ExtractNamedExtension(CACert,id_ce_keyUsage,UKI) = E_OK then begin
    if not UKI.extnValue.Constructed and
       (UKI.extnValue.Tag = V_ASN1_BIT_STRING) and
       (UKI.extnValue.Length > 1) then begin
      if Byte(UKI.extnValue.Content[1]) and keyCertSignMask = 0 then begin
        DisposeASN1Struct(UKI.extnValue);
        Exit;
      end;
    end;
  end;
  UKI.Free;

  SKI := nil;
  AKI := nil;
  if ExtractNamedExtension(Cert,id_ce_authorityKeyIdentifier,AKI) = E_OK then begin
    if AKI.extnValue.Constructed and (AKI.extnValue.Tag = V_ASN1_SEQUENCE) and
       (AKI.extnValue.Contents^.ItemCount > 0) then begin
      S := @AKI.extnValue.Contents^.Items[0];
      if S^.Tag <> 0 then
        S := nil;
    end else
      S := nil;
    if Assigned(S) then begin
      if S^.Constructed then begin
        if S^.ItemCount <> 1 then
          S := nil
        else if S^.Items[0].Tag <> V_ASN1_OCTET_STRING then
          S := nil
        else
          S := S^.Items[0];
      end;
      if Assigned(S) then begin
        if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,SKI) = E_OK then begin
          Result := (S^.Length = SKI.extnValue.Length) and
                    CompareMem(S^.Content,SKI.extnValue.Content,S^.Length);
          SKI.Free;
          AKI.Free;
          if ExtractIssuerStruct(Cert,A) = E_OK then begin
            if ExtractSubjectStruct(CACert,N) = E_OK then
              Result := Result and ASN1StructCompare(A^,N^)
            else
              Result := False;
          end else
            Result := False;
          Exit;
        end;
      end;
    end;
  end;
  AKI.Free;
  SKI.Free;
  if ExtractIssuerStruct(Cert,A) = E_OK then
    if ExtractSubjectStruct(CACert,N) = E_OK then
      Result := ASN1StructCompare(A^,N^);
end;

function IsAllegedCRLIssuer(const CRL, CACert: TASN1Struct): Boolean;
var
  AName, SName: TX501Name;
  UKI, SKI, AKI: TX509Extension;
  S: ^TASN1Struct;
begin
  Result := False;
  UKI := nil;
  if ExtractNamedExtension(CACert,id_ce_keyUsage,UKI) = E_OK then begin
    if not UKI.extnValue.Constructed and
       (UKI.extnValue.Tag = V_ASN1_BIT_STRING) then begin
      if Byte(UKI.extnValue.Content[0]) and cRLSignMask = 0 then begin
        DisposeASN1Struct(UKI.extnValue);
        Exit;
      end;
    end;
  end;
  UKI.Free;

  SKI := nil;
  AKI := nil;
  if ExtractNamedCRLExtension(CRL,id_ce_authorityKeyIdentifier,AKI) = E_OK then begin
    if AKI.extnValue.Constructed and (AKI.extnValue.Tag = V_ASN1_SEQUENCE) and
       (AKI.extnValue.Contents^.ItemCount > 0) then begin
      S := @AKI.extnValue.Contents^.Items[0];
      if (S^.Tag <> 0) or (not S^.Constructed) or
         (S^.contents^.ItemCount <> 1) then S := nil;
    end else
      S := nil;
    if Assigned(S) then begin
      S := @S^.Contents^.Items[0];
      if S^.Tag = V_ASN1_OCTET_STRING then begin
        if ExtractNamedExtension(CACert,id_ce_subjectKeyIdentifier,SKI) = E_OK then begin
          Result := (S^.Length = SKI.extnValue.Length) and
                    CompareMem(S^.Content,SKI.extnValue.Content,S^.Length);
          SKI.Free;
          AKI.Free;
          if ExtractIssuerCRL(CRL,AName) >= 0 then begin
            if ExtractSubject(CACert,SName) >= 0 then
              Result := Result and CompareName(AName,SName)
            else
              Result := False;
          end else
            Result := False;
          Exit;
        end;
      end;
    end;
  end;
  AKI.Free;
  SKI.Free;
  if ExtractIssuerCRL(CRL,AName) >= 0 then
    if ExtractSubject(CACert,SName) >= 0 then
      Result := CompareName(AName,SName);
end;

procedure TranslateRSAPublicKey(const RSAKey: TIFPublicKey; var PK: TX509PublicKey);
var
  S: string;
begin
  Assert(RSAKey.Scheme in [ifRSA1,ifRSA2]);
  if PK = nil then
    PK := TX509PublicKey.Create;
  PK.Algorithm.Parameters.Free;
  PK.Algorithm.Parameters := nil;
  PK.PublicKey.Free;
  PK.PublicKey := nil;
  PK.Algorithm.Algorithm := rsaEncryption;
  NewComposeASN1Struct(PK.Algorithm.Parameters,V_ASN1_UNIVERSAL,False,V_ASN1_NULL);
  NewComposeASN1Struct(PK.PublicKey,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  AddComposeASN1Field(PK.PublicKey,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  S := MPIntToBase256(RSAKey.N);
  PK.PublicKey.Contents^.Items[0].SetContent(S[1],Length(S));
  AddComposeASN1Field(PK.PublicKey,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  S := MPIntToBase256(RSAKey.E);
  PK.PublicKey.Contents^.Items[1].SetContent(S[1],Length(S));
  PK.PublicKey.CalculateLength;
end;          

procedure TranslateRSA_OAEPPublicKey(const RSAKey: TIFPublicKey;
                                     HashAlgorithm,
                                     MGFHashAlgorithm: THashAlgorithm;
                                     const P: string;
                                     var PK: TX509PublicKey);
var
  N: PASN1Struct;
  S: string;
begin
  Assert(RSAKey.Scheme in [ifRSA1,ifRSA2]);
  if PK = nil then
    PK := TX509PublicKey.Create;
  PK.Algorithm.Parameters.Free;
  PK.Algorithm.Parameters := nil;
  PK.PublicKey.Free;
  PK.PublicKey := nil;

  PK.Algorithm.Algorithm := id_RSAES_OAEP;

  NewComposeASN1Struct(PK.Algorithm.Parameters,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  PK.Algorithm.Parameters.Persistent := True;
  PK.Algorithm.Parameters.TypeName := 'RSAES-OAEP-params';

{$IFDEF SHA1}
  if HashAlgorithm <> haSHA1 then begin
{$ENDIF SHA1}
    N := PK.Algorithm.Parameters.AddField('hashFunc','[0] AlgorithmIdentifier',nil);
    N.Persistent := True;
    N.Constructed := True;
    N.Tag := V_ASN1_SEQUENCE;
    N.AddField('algorithm','OBJECT',HashAlgorithmToOID(HashAlgorithm),True);
    N.AddField('parameters','NULL','',True);
{$IFDEF SHA1}
  end;       
{$ENDIF SHA1}
       
{$IFDEF SHA1}
  if MGFHashAlgorithm <> haSHA1 then begin
{$ENDIF SHA1}
    N := PK.Algorithm.Parameters.AddField('maskGenFunc','[1] AlgorithmIdentifier-mgf',nil);
    N.Persistent := True;
    N.Constructed := True;
    N.Tag := V_ASN1_SEQUENCE;
    N.AddField('algorithm','OBJECT',id_mgf1,True);
    N := N.AddField('parameters','AlgorithmIdentifier',nil);
    N.Persistent := True;
    N.Constructed := True;
    N.Tag := V_ASN1_SEQUENCE;
    N.AddField('algorithm','OBJECT',HashAlgorithmToOID(MGFHashAlgorithm),True);
    N.AddField('parameters','NULL','',True);
{$IFDEF SHA1}
  end;       
{$ENDIF SHA1}

  if P <> '' then begin
    N := PK.Algorithm.Parameters.AddField('pSourceFunc','[2] AlgorithmIdentifier-p',nil);
    N.Persistent := True;
    N.Constructed := True;
    N.Tag := V_ASN1_SEQUENCE;
    N.AddField('algorithm','OBJECT',id_pSpecified,True);
    N := N.AddField('parameters','OCTET STRING','',True);
    N.SetContent(P[1],Length(P));
  end;
  PK.Algorithm.Parameters.CalculateLength;

  NewComposeASN1Struct(PK.PublicKey,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  AddComposeASN1Field(PK.PublicKey,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  S := MPIntToBase256(RSAKey.N);
  PK.PublicKey.Contents^.Items[0].SetContent(S[1],Length(S));
  AddComposeASN1Field(PK.PublicKey,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  S := MPIntToBase256(RSAKey.E);
  PK.PublicKey.Contents^.Items[1].SetContent(S[1],Length(S));
  PK.PublicKey.CalculateLength;
end;

procedure TranslateDHPublicKey(const DLKey: TDLPublicKey; var PK: TX509PublicKey);
var
  N: PASN1Struct;
  S: string;
begin
  if PK = nil then
    PK := TX509PublicKey.Create;
  PK.Algorithm.Algorithm := dhpublicnumber;
  PK.Algorithm.Parameters.Free;
  PK.Algorithm.Parameters := nil;
  NewComposeASN1Struct(PK.Algorithm.Parameters,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  PK.Algorithm.Parameters.Persistent := True;
  PK.Algorithm.Parameters.TypeName := 'DomainParameters';
  N := PK.Algorithm.Parameters.AddField('p','INTEGER','',True);
  S := MPIntToBase256(DLKey.Params.P);
  while S[1] = #0 do Delete(S,1,1);
  N.SetContent(S[1],Length(S));
  N := PK.Algorithm.Parameters.AddField('g','INTEGER','',True);
  S := MPIntToBase256(DLKey.Params.G);
  while S[1] = #0 do Delete(S,1,1);
  N.SetContent(S[1],Length(S));
  N := PK.Algorithm.Parameters.AddField('q','INTEGER','',True);
  S := MPIntToBase256(DLKey.Params.Q);
  while S[1] = #0 do Delete(S,1,1);
  N.SetContent(S[1],Length(S));
  if Assigned(DLKey.Params.J) then begin
    N := PK.Algorithm.Parameters.AddField('j','INTEGER OPTIONAL','',True);
    S := MPIntToBase256(DLKey.Params.J);
    while S[1] = #0 do Delete(S,1,1);
    N.SetContent(S[1],Length(S));
  end;

  PK.Algorithm.Parameters.CalculateLength;

  PK.PublicKey.Free;
  PK.PublicKey := nil;
  NewComposeASN1Struct(PK.PublicKey,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  S := MPIntToBase256(DLKey.Y);
  PK.PublicKey.SetContent(S[1],Length(S));
end;            

procedure TranslateDSAPublicKey(const DLKey: TDLPublicKey; var PK: TX509PublicKey);
var
  N: PASN1Struct;
begin
  if PK = nil then
    PK := TX509PublicKey.Create;
  PK.Algorithm.Algorithm := id_dsa;
  PK.Algorithm.Parameters.Free;
  PK.Algorithm.Parameters := nil;
  NewComposeASN1Struct(PK.Algorithm.Parameters,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  PK.Algorithm.Parameters.Persistent := True;
  PK.Algorithm.Parameters.TypeName := 'DSS-Parms';
  N := PK.Algorithm.Parameters.AddField('p','INTEGER','',True);
  N.EditContent(DLKey.Params.P,False);
  N := PK.Algorithm.Parameters.AddField('q','INTEGER','',True);
  N.EditContent(DLKey.Params.Q,False);
  N := PK.Algorithm.Parameters.AddField('g','INTEGER','',True);
  N.EditContent(DLKey.Params.G,False);

  PK.Algorithm.Parameters.CalculateLength;

  PK.PublicKey.Free;
  PK.PublicKey := nil;
  NewComposeASN1Struct(PK.PublicKey,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  PK.PublicKey.EditContent(DLKey.Y,False);
end;

procedure TranslateECPublicKey(const ECKey: TECPublicKey; var PK: TX509PublicKey);
var
  N: PASN1Struct;
  A: PMPInteger;
begin
  if PK = nil then
    PK := TX509PublicKey.Create;
  PK.Algorithm.Parameters.Free;
  PK.Algorithm.Parameters := nil;
  PK.PublicKey.Free;
  PK.PublicKey := nil;
  PK.Algorithm.Algorithm := id_ecPublicKey;
  NewComposeASN1Struct(PK.Algorithm.Parameters,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  if TryIsECurveP192_1(ECKey.Params) then begin
    PK.Algorithm.Parameters.TypeName := 'OBJECT';
    PK.Algorithm.Parameters.EditContent(prime192v1);
  end else if TryIsECurveP192_2(ECKey.Params) then begin
    PK.Algorithm.Parameters.TypeName := 'OBJECT';
    PK.Algorithm.Parameters.EditContent(prime192v2);
  end else if TryIsECurveP192_3(ECKey.Params) then begin
    PK.Algorithm.Parameters.TypeName := 'OBJECT';
    PK.Algorithm.Parameters.EditContent(prime192v3);
  end else if TryIsECurveP239_1(ECKey.Params) then begin
    PK.Algorithm.Parameters.TypeName := 'OBJECT';
    PK.Algorithm.Parameters.EditContent(prime239v1);
  end else if TryIsECurveP239_2(ECKey.Params) then begin
    PK.Algorithm.Parameters.TypeName := 'OBJECT';
    PK.Algorithm.Parameters.EditContent(prime239v2);
  end else if TryIsECurveP239_3(ECKey.Params) then begin
    PK.Algorithm.Parameters.TypeName := 'OBJECT';
    PK.Algorithm.Parameters.EditContent(prime239v3);
  end else if TryIsECurveP256_1(ECKey.Params) then begin
    PK.Algorithm.Parameters.TypeName := 'OBJECT';
    PK.Algorithm.Parameters.EditContent(prime256v1);
  end else begin
    PK.Algorithm.Parameters.Persistent := True;
    PK.Algorithm.Parameters.TypeName := 'EcpkParameters';
    N := PK.Algorithm.Parameters.AddField('version','INTEGER','',True);
    N.EditContent(Integer(1));
    N := PK.Algorithm.Parameters.AddField('fieldID','FieldID',nil);
    N.Tag := V_ASN1_SEQUENCE;
    N.Constructed := True;
    N^.AddField('fieldType','OBJECT',prime_field,True);
    N.AddField('parameters','INTEGER','',True)^.EditContent(ECKey.Params.Q,True);
    N := PK.Algorithm.Parameters.AddField('curve','Curve',nil);
    N.Tag := V_ASN1_SEQUENCE;
    N.Constructed := True;
    A := MPCopy(ECKey.Params.A);
    try
      MPMod(A,ECKey.Params.Q);
      N.AddField('a','OCTET STRING','',True)^.EditContent(A,True);
    finally
      MPDealloc(A);
    end;
    N.AddField('b','OCTET STRING','',True)^.EditContent(ECKey.params.B,True);
    N := PK.Algorithm.Parameters.AddField('base','OCTET STRING','',True);
    N.EditContent(ECKey.Params.G,ECKey.Params,cHybrid);
    N := PK.Algorithm.Parameters.AddField('order','INTEGER','',True);
    N.EditContent(ECKey.Params.R,True);
  end;

  NewComposeASN1Struct(PK.PublicKey,V_ASN1_UNIVERSAL,False,V_ASN1_OCTET_STRING);
  PK.PublicKey.EditContent(ECKey.W,ECKey.Params,cHybrid);
end;

function InterpretSubjectRSAPublicKey(PK: TX509PublicKey;
                                      var RSAKey: TIFPublicKey;
                                      AcceptOAEP: Boolean): Integer;
begin
  Result := E_OK;
  if Result = E_OK then begin
    if (PK.Algorithm.Algorithm <> rsaEncryption) and
       ((PK.Algorithm.Algorithm <> id_RSAES_OAEP) or not AcceptOAEP) then
      Result := E_NOT_SUPPORTED
    else if (PK.PublicKey.Tag <> V_ASN1_SEQUENCE) or
            not PK.PublicKey.Constructed or
            (PK.PublicKey.Contents^.ItemCount <> 2) then
      Result := E_SYNTAX
    else begin
      RSAKey.Scheme := ifRSA1;
      PK.PublicKey.Contents^.Items[0].ContentAsUMPInt(RSAKey.N);
      PK.PublicKey.Contents^.Items[1].ContentAsUMPInt(RSAKey.E);
    end;
  end;
end;

function InterpretSubjectRSA_OAEPPublicKey(PK: TX509PublicKey;
                                           var RSAKey: TIFPublicKey;
                                           var HashFunc: THashAlgorithm;
                                           var MGFHashFunc: THashAlgorithm;
                                           var P: string): Integer;
var
  I, J: Cardinal;
  S: PASN1Struct;
begin
  Result := E_OK;
  if Result = E_OK then begin
    if PK.Algorithm.Algorithm <> id_RSAES_OAEP then
      Result := E_NOT_SUPPORTED
    else if (PK.PublicKey.Tag <> V_ASN1_SEQUENCE) or
            not PK.Algorithm.Parameters.Constructed or
            not PK.PublicKey.Constructed or
            (PK.PublicKey.Contents^.ItemCount <> 2) then
      Result := E_SYNTAX
    else begin
      RSAKey.Scheme := ifRSA1;
      PK.PublicKey.Contents^.Items[0].ContentAsUMPInt(RSAKey.N);
      PK.PublicKey.Contents^.Items[1].ContentAsUMPInt(RSAKey.E);
{$IFDEF SHA1}
      HashFunc := haSHA1;
      MGFHashFunc := haSHA1;
{$ELSE  SHA1}
      HashFunc := haReserved0;
      HashFunc := haReserved0;
{$ENDIF SHA1}
      P := '';
      if PK.Algorithm.Parameters.ItemCount > 0 then begin
        I := 0;
        for J := 0 to PK.Algorithm.Parameters.ItemCount - 1 do begin
          S := PK.Algorithm.Parameters.Items[J];
          if (S.Cls <> V_ASN1_CONTEXT_SPECIFIC) or
             (S.Tag < I) or
             (S.Tag > 2) or
             not S.Constructed then begin
            Result := E_SYNTAX;
            Exit;
          end else if S.Tag = 0 then begin
            I := 1;
            if not OIDToHashAlgorithm(S.Items[0].ContentAsOID,HashFunc) then begin
              Result := E_NOT_SUPPORTED;
              Exit;
            end;
          end else if S.Tag = 1 then begin
            S := S.Items[0];
            if S.Items[0].ContentAsOID <> id_mgf1 then begin
              Result := E_NOT_SUPPORTED;
              Exit;
            end;
            S := S.Items[1];
            if not OIDToHashAlgorithm(S.Items[0].ContentAsOID,MGFHashFunc) then begin
              Result := E_NOT_SUPPORTED;
              Exit;
            end;
            I := 2;
          end else if S.Tag = 2 then begin
            S := S.Items[0];
            if S.Items[0].ContentAsOID <> id_pSpecified then begin
              Result := E_NOT_SUPPORTED;
              Exit;
            end;
            SetLength(P,S.Items[1].Length);
            Move(S.Items[1].Content^,P[1],Length(P));
            I := 3;
          end;
        end;
      end;
    end;
  end;
end;

function InterpretSubjectDHPublicKey(PK: TX509PublicKey;
                                     var DHKey: TDLPublicKey): Integer;
var
  P: PMPInteger;
begin
  Result := E_OK;
  if Result = E_OK then begin
    if PK.Algorithm.Algorithm <> dhPublicNumber then
      Result := E_NOT_SUPPORTED
    else if (PK.PublicKey.Tag <> V_ASN1_INTEGER) or
            PK.PublicKey.Constructed then
      Result := E_SYNTAX
    else begin
      PK.PublicKey.ContentAsMPInt(DHKey.Y);
      if Assigned(PK.Algorithm.Parameters) and
         (PK.Algorithm.Parameters.ItemCount >= 3) then begin
        PK.Algorithm.Parameters.Items[0].ContentAsUMPInt(DHKey.Params.P);
        PK.Algorithm.Parameters.Items[1].ContentAsUMPInt(DHKey.Params.G);
        PK.Algorithm.Parameters.Items[2].ContentAsUMPInt(DHKey.Params.Q);
        if PK.Algorithm.Parameters.ItemCount >= 4 then
          PK.Algorithm.Parameters.Items[3].ContentAsUMPInt(DHKey.Params.J)
        else begin
          P := MPCopy(DHKey.Params.P);
          MPDiv(P,DHKey.Params.Q,DHKey.Params.J);
          MPDealloc(P);
        end;
      end;
    end;
  end;
end;

function InterpretSubjectDSAPublicKey(PK: TX509PublicKey;
                                      var DSAKey: TDLPublicKey): Integer;
var
  OID: ObjectIdentifier;
  P: PMPInteger;
begin
  Result := E_OK;
  if Result = E_OK then begin
    OID := PK.Algorithm.Algorithm;
    if (OID <> id_dsa) and (OID <> id_nr) then
      Result := E_NOT_SUPPORTED
    else if (PK.PublicKey.Tag <> V_ASN1_INTEGER) or
            PK.PublicKey.Constructed then
      Result := E_SYNTAX
    else begin
      PK.PublicKey.ContentAsUMPInt(DSAKey.Y);
      if Assigned(PK.Algorithm.Parameters) and
         (PK.Algorithm.Parameters.ItemCount >= 3) then begin
        PK.Algorithm.Parameters.Items[0].ContentAsUMPInt(DSAKey.Params.P);
        PK.Algorithm.Parameters.Items[1].ContentAsUMPInt(DSAKey.Params.Q);
        PK.Algorithm.Parameters.Items[2].ContentAsUMPInt(DSAKey.Params.G);
        P := MPCopy(DSAKey.Params.P);
        MPDiv(P,DSAKey.Params.Q,DSAKey.Params.J);
        MPDealloc(P);
      end;
      if OID = id_nr then
        DSAKey.SignScheme := dlssaNR
      else
        DSAKey.SignScheme := dlssaDSA;
    end;
  end;
end;

function InterpretSubjectECPublicKey(PK: TX509PublicKey;
                                    var ECKey: TECPublicKey): Integer;
var
  F: TASN1Struct;
  OID: string;
begin
  Result := E_OK;
  if Result = E_OK then begin
    OID := PK.Algorithm.Algorithm;
    if (OID <> id_ecPublicKey) and
       (OID <> id_ecnr) then
      Result := E_NOT_SUPPORTED
    else if (PK.PublicKey.Tag <> V_ASN1_OCTET_STRING) or
            PK.PublicKey.Constructed then
      Result := E_SYNTAX
    else if Assigned(PK.Algorithm.Parameters) and
            (not PK.Algorithm.Parameters.Constructed or
             (PK.Algorithm.Parameters.ItemCount > 0)) then begin
      if not PK.Algorithm.Parameters.Constructed then begin
        if PK.Algorithm.Parameters.Tag = V_ASN1_OBJECT then begin
          OID := PK.Algorithm.Parameters.ContentAsOID;
          if OID = prime192v1 then
            ECurveP192_1(ECKey.Params)
          else if OID = prime192v2 then
            ECurveP192_2(ECKey.Params)
          else if OID = prime192v3 then
            ECurveP192_3(ECKey.Params)
          else if OID = prime239v1 then
            ECurveP239_1(ECKey.Params)
          else if OID = prime239v2 then
            ECurveP239_2(ECKey.Params)
          else if OID = prime239v3 then
            ECurveP239_3(ECKey.Params)
          else if OID = prime256v1 then
            ECurveP256_1(ECKey.Params)
          else if OID = prime384 then
            ECurveP384(ECKey.Params)
          else if OID = prime521 then
            ECurveP521(ECKey.Params)
          else
            Result := E_NOT_SUPPORTED;
          if Result = E_OK then
            PK.PublicKey.ContentAsECPoint(ECKey.W,ECKey.Params);
        end else
          Result := E_SYNTAX
      end else if PK.Algorithm.Parameters.ItemCount >= 5 then begin
        F := PK.Algorithm.Parameters;
        if (F.Items[0]^.Tag = V_ASN1_INTEGER) and
           (F.Items[0]^.ContentAsInteger = 1) and
           (F.Items[1]^.Tag = V_ASN1_SEQUENCE) and
           (F.Items[1]^.ItemCount = 2) and
           (F.Items[2]^.Tag = V_ASN1_SEQUENCE) and
           (F.Items[2]^.ItemCount = 2) and
           (F.Items[1]^.Items[0]^.Tag = V_ASN1_OBJECT) and
           (F.Items[1]^.Items[0]^.ContentAsOID = prime_field) then begin
          F.Items[1].Items[1].ContentAsUMPInt(ECKey.Params.Q);
          F.Items[2].Items[0].ContentAsUMPInt(ECKey.Params.A);
          F.Items[2].Items[1].ContentAsUMPInt(ECKey.Params.B);
          F.Items[4].ContentAsUMPInt(ECKey.Params.R);
          if (F.ItemCount > 5) and
             (F.Items[5].Cls = V_ASN1_UNIVERSAL) and
             (F.Items[5].Tag = V_ASN1_INTEGER) then
            F.Items[5].ContentAsUMPInt(ECKey.Params.K)
          else begin
            MPDealloc(ECKey.Params.K);
            ECKey.Params.K := IntToMPInt(1);
          end;
          if not IsECurveP224(ECKey.Params) then
            if not IsECurveP384(ECKey.Params) then
              IsECurveP521(ECKey.Params);
          F.Items[3].ContentAsECPoint(ECKey.Params.G,ECKey.Params);

          PK.PublicKey.ContentAsECPoint(ECKey.W,ECKey.Params);
        end else
          Result := E_NOT_SUPPORTED;
      end;
    end else if Assigned(ECKey.Params.Q) then
      PK.PublicKey.ContentAsECPoint(ECKey.W,ECKey.Params)
    else
      Result := E_NO_PARAMETERS;
  end;
end;

function ExtractSubjectRSAPublicKey(const Cert: TASN1Struct;
                                    var RSAKey: TIFPublicKey;
                                    AcceptOAEP: Boolean): Integer;
var
  PK: TX509PublicKey;
begin
  PK := TX509PublicKey.Create;
  try
    Result := ExtractSubjectPublicKey(Cert,PK);
    if Result = E_OK then
      Result := InterpretSubjectRSAPublicKey(PK,RSAKey,AcceptOAEP);
  finally
    PK.Free;
  end;
end;

function ExtractSubjectRSA_OAEPPublicKey(const Cert: TASN1Struct;
                                         var RSAKey: TIFPublicKey;
                                         var HashFunc: THashAlgorithm;
                                         var MGFHashFunc: THashAlgorithm;
                                         var P: string): Integer;
var
  PK: TX509PublicKey;
begin
  PK := TX509PublicKey.Create;
  Result := ExtractSubjectPublicKey(Cert,PK);
  if Result = E_OK then
    Result := InterpretSubjectRSA_OAEPPublicKey(PK,RSAKey,HashFunc,MGFHashFunc,P);
  PK.Free;
end;

function ExtractSubjectDHPublicKey(const Cert: TASN1Struct; var DHKey: TDLPublicKey): Integer;
var
  PK: TX509PublicKey;
begin
  PK := TX509PublicKey.Create;
  Result := ExtractSubjectPublicKey(Cert,PK);
  if Result = E_OK then
    Result := InterpretSubjectDHPublicKey(PK,DHKey);
  PK.Free;
end;

function ExtractSubjectDSAPublicKey(const Cert: TASN1Struct; var DSAKey: TDLPublicKey): Integer;
var
  PK: TX509PublicKey;
begin
  PK := TX509PublicKey.Create;
  Result := ExtractSubjectPublicKey(Cert,PK);
  if Result = E_OK then
    Result := InterpretSubjectDSAPublicKey(PK,DSAKey);
  PK.Free;
end;

function ExtractSubjectECPublicKey(const Cert: TASN1Struct; var ECKey: TECPublicKey): Integer;
var
  PK: TX509PublicKey;
begin
  PK := TX509PublicKey.Create;
  Result := ExtractSubjectPublicKey(Cert,PK);
  if Result = E_OK then
    Result := InterpretSubjectECPublicKey(PK,ECKey);
  PK.Free;
end;

function CheckSignature(const Signed, CACert: TASN1Struct; RaiseBitAlignError: Boolean): Boolean;
var
  Alg: TX509AlgorithmIdentifier;
  RSAKey: TIFPublicKey;
  DLKey: TDLPublicKey;
  ECKey: TECPublicKey;
  MS: TSecureMemoryStream;
  HA, MHA: THashAlgorithm;
  EM: TSignEncoding;
begin
  Alg := nil;
  EM := seEMSA3;
{$IFDEF SHA1}
  HA := haSHA1;
{$ELSE  SHA1}
  HA := haReserved0;
{$ENDIF SHA1}
  if ExtractSignatureAlgorithm(Signed,Alg) = E_OK then begin
    if (Alg.Algorithm = id_DSA_with_SHA1) or
       (Alg.Algorithm = ecdsa_with_SHA1) then
      Result := True
    else
      Result := InterpretRSASignatureAlgorithm(Alg,HA,MHA,EM);
    Alg.Free;
  end else
    Result := False;
  if Result then begin
    FillChar(RSAKey,SizeOf(RSAKey),0);
    FillChar(DLKey,SizeOf(DLKey),0);
    FillChar(ECKey,SizeOf(ECKey),0);
    if ExtractSubjectRSAPublicKey(CACert,RSAKey,True) = E_OK then begin
      MS := TSecureMemoryStream.Create;
      try
        ASN1ToStream(Signed.Contents^.Items[0],MS);
        MS.Position := 0;
        try
          Result := IFSSASignatureVerification(RSAKey,MS.Memory^,MS.Size,
                                               Signed.Contents^.Items[2].Content^,
                                               Signed.Contents^.Items[2].Length,
                                               HA,MHA,EM);
        except
          on E: EBitAlignment do begin
            if RaiseBitAlignError then
              raise;
            Result := IFSSASignatureVerification(RSAKey,MS.Memory^,MS.Size,
                                                 Signed.Contents^.Items[2].Content^,
                                                 Signed.Contents^.Items[2].Length,
                                                 HA,HA,EM);
          end;
        end;
      finally
        DisposeIFPublicKey(RSAKey);
        MS.Free;
      end;
    end else if ExtractSubjectDSAPublicKey(CACert,DLKey) = E_OK then begin
      MS := TSecureMemoryStream.Create;
      try
        ASN1ToStream(Signed.Contents^.Items[0],MS);
        MS.Position := 0;
        Result := DLSSASignatureVerification(DLKey,MS.Memory^,MS.Size,
                                             Signed.Contents^.Items[2].Content[1],
                                             Signed.Contents^.Items[2].Length - 1,
                                             True,HA);
      finally
        DisposeDLPublicKey(DLKey);
        MS.Free;
      end;
    end else if ExtractSubjectECPublicKey(CACert,ECKey) = E_OK then begin
      MS := TSecureMemoryStream.Create;
      try
        ASN1ToStream(Signed.Contents^.Items[0],MS);
        MS.Position := 0;
        Result := ECSSASignatureVerification(ECKey,MS.Memory^,MS.Size,
                                             Signed.Contents^.Items[2].Content[1],
                                             Signed.Contents^.Items[2].Length - 1,
                                             True,HA);
      finally
        DisposeECPublicKey(ECKey);
        MS.Free;
      end;
    end else
      Result := False;
  end;
end;

function CheckSignatureCertOrCRL(const Struct, CACert: TASN1Struct): Boolean;
var
  Alg: TX509AlgorithmIdentifier;
  RSAKey: TIFPublicKey;
  DLKey: TDLPublicKey;
  ECKey: TECPublicKey;
  MS: TSecureMemoryStream;
  HA,MHA: THashAlgorithm;
  EM: TSignEncoding;
begin
  Alg := nil;
  EM := seEMSA3;
{$IFDEF SHA1}
  HA := haSHA1;
  MHA := haSHA1;
{$ELSE  SHA1}
  HA := haReserved0;
  MHA := haReserved0;
{$ENDIF SHA1}
  if ExtractSignatureAlgorithmCertOrCRL(Struct,Alg) = E_OK then begin
    if (Alg.Algorithm = id_DSA_with_SHA1) or
       (Alg.Algorithm = ecdsa_with_SHA1) or
       (Alg.Algorithm = id_nr_with_sha1) or
       (Alg.Algorithm = id_ecnr_with_sha1) then
      Result := True
    else
      Result := InterpretRSASignatureAlgorithm(Alg,HA,MHA,EM);
    Alg.Free;
  end else
    Result := False;
  if Result then begin
    FillChar(RSAKey,SizeOf(RSAKey),0);
    FillChar(DLKey,SizeOf(DLKey),0);
    FillChar(ECKey,SizeOf(ECKey),0);
    if ExtractSubjectRSAPublicKey(CACert,RSAKey,True) = E_OK then begin
      MS := TSecureMemoryStream.Create;
      try
        Struct.Items[0]^.SaveToStream(MS,fmtDER);
        MS.Position := 0;
        Result := IFSSASignatureVerification(RSAKey,MS.Memory^,MS.Size,
                                             Struct.Contents^.Items[2].Content^,
                                             Struct.Contents^.Items[2].Length,
                                             HA,MHA,EM);
      finally
        MS.Free;
      end;
      DisposeIFPublicKey(RSAKey);
    end else if ExtractSubjectDSAPublicKey(CACert,DLKey) = E_OK then begin
      MS := TSecureMemoryStream.Create;
      try
        ASN1ToStream(Struct.Contents^.Items[0],MS);
        MS.Position := 0;
        Result := DLSSASignatureVerification(DLKey,MS.Memory^,MS.Size,
                                             Struct.Contents^.Items[2].Content[1],
                                             Struct.Contents^.Items[2].Length - 1,
                                             True,HA);
      finally
        MS.Free;
      end;
      DisposeDLPublicKey(DLKey);
    end else if ExtractSubjectECPublicKey(CACert,ECKey) = E_OK then begin
      MS := TSecureMemoryStream.Create;
      try
        ASN1ToStream(Struct.Contents^.Items[0],MS);
        MS.Position := 0;
        Result := ECSSASignatureVerification(ECKey,MS.Memory^,MS.Size,
                                             Struct.Contents^.Items[2].Content[1],
                                             Struct.Contents^.Items[2].Length - 1,
                                             True,HA);
      finally
        MS.Free;
      end;
      DisposeECPublicKey(ECKey);
    end else
      Result := False;
  end;
end;

function CheckSignatureCert(const Cert, CACert: TASN1Struct): Boolean;
begin
  Result := IsAllegedIssuer(Cert,CACert);
  if Result then
    Result := CheckSignatureCertOrCRL(Cert,CACert);
end;

function CheckSignatureCRL(const CRL, CACert: TASN1Struct): Boolean;
begin
  Result := IsAllegedCRLIssuer(CRL,CACert);
  if Result then
    Result := CheckSignatureCertOrCRL(CRL,CACert);
end;

{ TX509AlgorithmIdentifier }

destructor TX509AlgorithmIdentifier.Destroy;
begin
  DisposeASN1Struct(Parameters);
  inherited;
end;

{ TX509PublicKey }

procedure TX509PublicKey.AsDLKey(var AKey: TDLPublicKey);
begin
  Assert(KeyAlgorithmType = katDL);
  if FAlgorithm.Algorithm = dhPublicNumber then
    InterpretSubjectDHPublicKey(Self,AKey)
  else
    InterpretSubjectDSAPublicKey(Self,AKey);
end;

procedure TX509PublicKey.AsECKey(var AKey: TECPublicKey);
begin
  Assert(KeyAlgorithmType = katEC);
  InterpretSubjectECPublicKey(Self,AKey);
end;

procedure TX509PublicKey.AsIFKey(var AKey: TIFPublicKey);
begin
  Assert(KeyAlgorithmType = katIF);
  InterpretSubjectRSAPublicKey(Self,AKey,True);
end;

constructor TX509PublicKey.Create;
begin
  FAlgorithm := TX509AlgorithmIdentifier.Create;
end;

constructor TX509PublicKey.CreateFromDLKey(const AKey: TDLPublicKey;
  AsDSA: Boolean);
begin
  Create;
  if AsDSA then
    TranslateDSAPublicKey(AKey,Self)
  else
    TranslateDHPublicKey(AKey,Self);
end;

constructor TX509PublicKey.CreateFromECKey(const AKey: TECPublicKey);
begin
  Create;
  TranslateECPublicKey(AKey,Self);
end;

constructor TX509PublicKey.CreateFromIFKey(const AKey: TIFPublicKey;
  AsOAEP: Boolean; HA: THashAlgorithm; MGFHA: THashAlgorithm; P: string);
begin
  Create;
  if AsOAEP then
    TranslateRSA_OAEPPublicKey(AKey,HA,MGFHA,P,Self)
  else
    TranslateRSAPublicKey(AKey,Self);
end;

destructor TX509PublicKey.Destroy;
begin
  FAlgorithm.Free;
  DisposeASN1Struct(PublicKey);
  inherited;
end;

function TX509PublicKey.GetKeyAlgorithmType: TKeyAlgorithmType;
var
  OID: string;
begin
  OID := FAlgorithm.Algorithm;
  if (OID = rsaEncryption) or
     (OID = id_RSAES_OAEP) or
     (OID = id_RSASSA_PSS) or
     (OID = id_rw) then
    Result := katIF
  else if (OID = dhPublicNumber) or
          (OID = id_dsa) or
          (OID = id_nr) then
    Result := katDL
  else if (OID = id_ecPublicKey) or
          (OID = id_ecnr) then
    Result := katEC
  else
    Result := katUnknown;
end;

procedure TX509PublicKey.SetAlgorithm(
  const Value: TX509AlgorithmIdentifier);
begin
  FAlgorithm := Value;
end;

{ TX509Extension }

destructor TX509Extension.Destroy;
begin
  DisposeASN1Struct(extnValue);
  inherited;
end;

{ TX509RevokedCertificate }

function TX509RevokedCertificate.Add: TX509Extension;
begin
  Result := TX509Extension.Create;
  FList.Add(Result);
end;

function TX509RevokedCertificate.Append(Item: TX509Extension): Integer;
begin
  Result := Count;
  Add;
  crlEntryExtensions[Result] := Item;
end;

procedure TX509RevokedCertificate.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TX509Extension(FList[I]).Free;
  FList.Clear;
end;

constructor TX509RevokedCertificate.Create;
begin
  FList := TList.Create;
end;

procedure TX509RevokedCertificate.Delete(index: Integer);
begin
  FList.Delete(index);
end;

destructor TX509RevokedCertificate.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TX509RevokedCertificate.GetCI: TX509GeneralNames;
var
  I: Integer;
begin
  SetLength(Result,0);
  for I := 0 to Count - 1 do
    if crlEntryExtensions[I].extnID = id_ce_certificateIssuer then begin
      InterpretGeneralNames(crlEntryExtensions[I].extnValue,Result);
      Exit;
    end;
end;

function TX509RevokedCertificate.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TX509RevokedCertificate.GetcrlEntryExtensions(
  index: Integer): TX509Extension;
begin
  Result := FList[index];
end;

function TX509RevokedCertificate.GetHI: string;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if crlEntryExtensions[I].extnID = id_ce_holdInstructionCode then begin
      Result := crlEntryExtensions[I].extnValue.ContentAsOID;
      Exit;
    end;
end;

function TX509RevokedCertificate.GetInvDate: TDateTime;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if crlEntryExtensions[I].extnID = id_ce_invalidityDate then begin
      if crlEntryExtensions[I].extnValue.Length = 15 then
        X509GeneralizedTimeToDateTime(crlEntryExtensions[I].extnValue.Content,
                                      crlEntryExtensions[I].extnValue.Length,
                                      Result)
      else
        UTCTimeToDateTime(crlEntryExtensions[I].extnValue.Content,
                          crlEntryExtensions[I].extnValue.Length,
                          Result);
      Exit;
    end;
end;

function TX509RevokedCertificate.GetReasonCode: CRLReason;
var
  I, J, RF: Integer;
begin
  Result := unspecified0;
  for I := 0 to Count - 1 do
    if crlEntryExtensions[I].extnID = id_ce_cRLReason then begin
      RF := 0;
      for J := 0 to crlEntryExtensions[I].extnValue.Length - 1 do begin
        RF := RF shl 8;
        RF := RF + Byte(crlEntryExtensions[I].extnValue.Content[J]);
      end;
      Result := CRLReason(RF);
      Exit;
    end;
end;

procedure TX509RevokedCertificate.SetCI(const Value: TX509GeneralNames);
var
  I: Integer;
  Ext: TX509Extension;
begin
  for I := 0 to Count - 1 do
    if crlEntryExtensions[I].extnID = id_ce_certificateIssuer then begin
      TranslateGeneralNames(Value,crlEntryExtensions[I].extnValue);
      Exit;
    end;
  Ext := TX509Extension.Create;
  Ext.extnID := id_ce_certificateIssuer;
  Ext.extnIDName := 'id-ce-certificateIssuer';
  Ext.Critical := True;
  TranslateGeneralNames(Value,Ext.extnValue);
  Append(Ext);
  Ext.Free;
end;

procedure TX509RevokedCertificate.SetCount(const Value: Integer);
var
  I: Integer;
begin
  if Value > Count then begin
    for I := Count to Value - 1 do
      Add;
  end else if Value < Count then begin
    for I := Value to Count - 1 do
      TX509Extension(FList[I]).Free;
    FList.Count := Value;
  end;
end;

procedure TX509RevokedCertificate.SetcrlEntryExtensions(index: Integer;
  const Value: TX509Extension);
var
  Ext: TX509Extension;
begin
  Ext := FList[index];
  Ext.extnID := Value.extnID;
  Ext.extnIDName := Value.extnIDName;
  Ext.Critical := Value.Critical;
  CopyASN1Struct(Ext.extnValue,Value.extnValue);
end;

procedure TX509RevokedCertificate.SetHI(const Value: string);
var
  I: Integer;
  Ext: TX509Extension;
  S: string;
begin
  if Value = '' then begin  
    for I := 0 to Count - 1 do
      if crlEntryExtensions[I].extnID = id_ce_holdInstructionCode then begin
        TObject(FList[I]).Free;
        FList.Delete(I);
        Break;
      end;
    Exit;
  end;
  S := InterpretOIDtoBER(Value);
  for I := 0 to Count - 1 do
    if crlEntryExtensions[I].extnID = id_ce_holdInstructionCode then begin
      crlEntryExtensions[I].extnValue.SetContent(S[1],Length(S));
      Exit;
    end;
  Ext := TX509Extension.Create;
  Ext.extnID := id_ce_holdInstructionCode;
  Ext.extnIDName := 'id-ce-holdInstructionCode';
  Ext.Critical := True;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
  Ext.extnValue.SetContent(S[1],Length(S));
  Append(Ext);
  Ext.Free;
end;

procedure TX509RevokedCertificate.SetInvDate(const Value: TDateTime);
var
  I: Integer;
  Ext: TX509Extension;
begin
  for I := 0 to Count - 1 do
    if crlEntryExtensions[I].extnID = id_ce_invalidityDate then begin
      DateTimeToASN1(Value,crlEntryExtensions[I].extnValue);
      Exit;
    end;
  Ext := TX509Extension.Create;
  Ext.extnID := id_ce_invalidityDate;
  Ext.extnIDName := 'id-ce-invalidityDate';
  Ext.Critical := True;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_GENERALIZEDTIME);
  DateTimeToASN1(Value,Ext.extnValue);
  Ext.extnValue.Tag := V_ASN1_GENERALIZEDTIME;
  Append(Ext);
  Ext.Free;
end;

procedure TX509RevokedCertificate.SetReasonCode(
  const Value: CRLReason);
var
  V, I: Integer;
  Ext: TX509Extension;
  S: string;
begin
  V := Ord(Value);
  S := '';
  while V > 0 do begin
    S := Char(Byte(V)) + S;
    V := V shr 8;
  end;
  if S = '' then
    S := #0;
  for I := 0 to Count - 1 do
    if crlEntryExtensions[I].extnID = id_ce_cRLReason then begin
      crlEntryExtensions[I].extnValue.SetContent(S[1],Length(S));
      Exit;
    end;
  Ext := TX509Extension.Create;
  Ext.extnID := id_ce_cRLReason;
  Ext.extnIDName := 'id-ce-cRLReason';
  Ext.Critical := True;
  NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_ENUMERATED);
  Ext.extnValue.SetContent(S[1],Length(S));
  Append(Ext);
  Ext.Free;
end;

{ TX509RevokedCertificates }

function TX509RevokedCertificates.Add: TX509RevokedCertificate;
begin
  Result := TX509RevokedCertificate.Create;
  FList.Add(Result);
end;

function TX509RevokedCertificates.Append(
  Item: TX509RevokedCertificate): Integer;
begin
  Result := Count;
  Add;
  Items[Result] := Item;
end;

procedure TX509RevokedCertificates.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TX509RevokedCertificate(FList[I]).Free;
  FList.Clear;
end;

constructor TX509RevokedCertificates.Create;
begin
  FList := TList.Create;
end;

procedure TX509RevokedCertificates.Delete(index: Integer);
begin
  FList.Delete(index);
end;

destructor TX509RevokedCertificates.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TX509RevokedCertificates.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TX509RevokedCertificates.GetItems(
  index: Integer): TX509RevokedCertificate;
begin
  Result := FList[index];
end;

procedure TX509RevokedCertificates.SetCount(const Value: Integer);
var
  I: Integer;
begin
  if Value > Count then begin
    for I := Count to Value - 1 do
      Add;
  end else if Value < Count then begin
    for I := Value to Count - 1 do
      TX509RevokedCertificate(FList[I]).Free;
    FList.Count := Value;
  end;
end;

procedure TX509RevokedCertificates.SetItems(index: Integer;
  const Value: TX509RevokedCertificate);
var
  RC: TX509RevokedCertificate;
  I: Integer;
begin
  RC := FList[index];
  RC.userCertificate := Value.userCertificate;
  RC.revocationDate := Value.revocationDate;
  RC.Clear;
  for I := 0 to Value.Count - 1 do begin
    RC.Add;
    RC[I] := Value[I];
  end;
end;

end.
