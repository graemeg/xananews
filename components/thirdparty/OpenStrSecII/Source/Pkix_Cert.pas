{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     PKIX_Cert Unit                                    }
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
unit Pkix_Cert;

interface

uses
  Classes, Asn1, SecUtils, MpPK, MpIF, MpDL, MpEC, Pkix;

const
  crlf = #13#10;
{$NODEFINE ASNModule}
  ASNModule =
    'PKIX-Cert DEFINITIONS ::=' + crlf +
    'BEGIN' + crlf + crlf +
    'Certificate ::= SEQUENCE {' + crlf +
    '     tbsCertificate'#9'TBSCertificate,' + crlf +
    '     signatureAlgorithm'#9'AlgorithmIdentifier,' + crlf +
    '     signature'#9'BIT STRING}' + crlf + crlf +
    'TBSCertificate ::= SEQUENCE {' + crlf +
    '     version'#9'[0] IMPLICIT Version DEFAULT A003020100,' + crlf +
    '     serialNumber'#9'INTEGER,' + crlf +
    '     signature'#9'AlgorithmIdentifier,' + crlf +
    '     issuer'#9'Name,' + crlf +
    '     validity'#9'Validity,' + crlf +
    '     subject'#9'Name,' + crlf +
    '     subjectPublicKeyInfo'#9'SubjectPublicKeyInfo,' + crlf +
    '     issuerUniqueID'#9'[1] IMPLICIT BIT STRING OPTIONAL,' + crlf +
    '     subjectUniqueID'#9'[2] IMPLICIT BIT STRING OPTIONAL,' + crlf +
    '     extensions'#9'[3] IMPLICIT ImplExtensions OPTIONAL}' + crlf + crlf +
    'Version ::= SEQUENCE {' + crlf +
    '     version'#9'INTEGER}' + crlf + crlf +
    'AlgorithmIdentifier ::= SEQUENCE {' + crlf +
    '     algorithm'#9'OBJECT,' + crlf +
    '     parameters'#9'AlgorithmID.&Type(&algorithm) OPTIONAL}' + crlf + crlf +
    'AlgorithmID ::= TYPE-IDENTIFIER {' + crlf +
    '     {NULL'#9'IDENTIFIED BY rsaEncryption}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY md5WithRSAEncryption}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY sha1WithRSAEncryption}|' + crlf +
    '     {Dss-Params'#9'IDENTIFIED BY id-dsa}|' + crlf +            
    '     {Dss-Params'#9'IDENTIFIED BY id-nr}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY id-dsa-with-sha1}|' + crlf +
    '     {DomainParameters'#9'IDENTIFIED BY dhpublicnumber}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY sha256WithRSAEncryption}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY sha384WithRSAEncryption}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY sha512WithRSAEncryption}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY md2WithRSAEncryption}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY ecdsa-with-SHA1}|' + crlf +
    '     {OBJECT'#9'IDENTIFIED BY id-ecPublicKey}|' + crlf +
    '     {RSAES-OAEP-params'#9'IDENTIFIED BY id-RSAES-OAEP}|' + crlf + 
    '     {RSASSA-PSS-params'#9'IDENTIFIED BY id-RSASSA-PSS}|' + crlf +
    '     {RSASSA-PSS-params'#9'IDENTIFIED BY id-rw}|' + crlf +
    '     {OBJECT'#9'IDENTIFIED BY id-ecnr}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY id-nr-with-sha1}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY id-ecnr-with-sha1}}' + crlf + crlf +
    'RSAES-OAEP-params ::= SEQUENCE {' + crlf +
    '     hashFunc'#9'[0] DigestAlgorithmIdentifier OPTIONAL,' + crlf +
    '     maskGenFunc'#9'[1] MGFAlgorithmIdentifier OPTIONAL,' + crlf +
    '     pSourceFunc'#9'[2] PSAlgorithmIdentifier OPTIONAL}' + crlf + crlf +
    'DigestAlgorithmIdentifier ::= SEQUENCE {' + crlf +
    '     algorithm'#9'OBJECT DEFAULT 06052B0E03021A,' + crlf +
    '     parameters'#9'DigParams.&Type(&algorithm) OPTIONAL}' + crlf + crlf +
    'DigParams ::= TYPE-IDENTIFIER {' + crlf +
    '     {NULL'#9'IDENTIFIED BY sha-1}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY md2}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY md5}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY id-ripeMD160}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY sha256}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY sha384}|' + crlf +
    '     {NULL'#9'IDENTIFIED BY sha512}}' + crlf + crlf +
    'sha-1 OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) ansi-X9-42(10046) algorithm(1) 26 }' + crlf + crlf +
    'md5 OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) digestAlgorithm(2) 5 }' + crlf + crlf +
    'md2 OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) digestAlgorithm(2) 2 }' + crlf + crlf +
    'id-ripeMD160 OBJECT IDENTIFIER ::= {  iso(1) 3 36 3 2 1 }' + crlf + crlf +
    'sha256 OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) (2) 1 }' + crlf + crlf +
    'sha384 OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) (2) 2 }' + crlf + crlf +
    'sha512 OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) (2) 3 }' + crlf + crlf + crlf +
    'MGFAlgorithmIdentifier ::= SEQUENCE {' + crlf +
    '     algorithm'#9'OBJECT DEFAULT 06092A864886F70D010108,' + crlf +
    '     parameters'#9'MGFParams.&Type(&algorithm) OPTIONAL}' + crlf + crlf +
    'MGFParams ::= TYPE-IDENTIFIER {' + crlf +
    '     {DigestAlgorithmIdentifier'#9'IDENTIFIED BY id-mgf1}}' + crlf + crlf +
    'id-mgf1 OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 8 }' + crlf + crlf +
    'PSAlgorithmIdentifier ::= SEQUENCE {' + crlf +
    '     algorithm'#9'OBJECT DEFAULT 06092A864886F70D010109,' + crlf +
    '     parameters'#9'PSParams.&Type(&algorithm) OPTIONAL}' + crlf + crlf +
    'PSParams ::= TYPE-IDENTIFIER {' + crlf +
    '     {PEmptyString'#9'IDENTIFIED BY id-pSpecified}}' + crlf + crlf +
    'PEmptyString ::= OCTET STRING' + crlf + crlf +
    'id-pSpecified OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 9 }' + crlf + crlf +
    'id-RSAES-OAEP OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 7 }' + crlf + crlf +
    'RSASSA-PSS-params ::= SEQUENCE {' + crlf +
    '     hashFunc'#9'[0] DigestAlgorithmIdentifier OPTIONAL,' + crlf +
    '     maskGenFunc'#9'[1] MGFAlgorithmIdentifier OPTIONAL}' + crlf + crlf +
    'id-RSASSA-PSS OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 10 }' + crlf + crlf +
    'id-rw  OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) private(4) enterprise(1) streamsec(13085) ssec(4) ssa(1) rw(1) }' + crlf + crlf +
    'DomainParameters ::= SEQUENCE {' + crlf +
    '     p'#9'INTEGER,' + crlf +
    '     g'#9'INTEGER,' + crlf +
    '     q'#9'INTEGER,' + crlf +
    '     j'#9'INTEGER OPTIONAL,' + crlf +
    '     validationParms'#9'ValidationParms OPTIONAL}' + crlf + crlf +
    'ValidationParms ::= SEQUENCE {' + crlf +
    '     seed'#9'BIT STRING,' + crlf +
    '     pgenCounter'#9'INTEGER}' + crlf + crlf +
    'dhpublicnumber OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) ansi-X9-42(10046) numberType(2) 1 }' + crlf + crlf +
    'rsaEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 1 }' + crlf + crlf +
    'sha1WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 5 }' + crlf + crlf +
    'sha256WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 11 }' + crlf + crlf +
    'sha384WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 12 }' + crlf + crlf +
    'sha512WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 13 }' + crlf + crlf +
    'md2WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 2 }' + crlf + crlf +
    'md5WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 4 }' + crlf + crlf +
    'Dss-Params ::= SEQUENCE {' + crlf +
    '     p'#9'INTEGER,' + crlf +
    '     q'#9'INTEGER,' + crlf +
    '     g'#9'INTEGER}' + crlf + crlf +
    'id-dsa OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) ansi-X9-57(10040) (4) 1 }' + crlf + crlf +
    'id-nr OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) private(4) enterprise(1) streamsec(13085) ssec(4) ssa(1) nr(2) }' + crlf + crlf +
    'id-ecnr OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) private(4) enterprise(1) streamsec(13085) ssec(4) ssa(1) ecnr(3) }' + crlf + crlf +
    'id-nr-with-sha1 OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) private(4) enterprise(1) streamsec(13085) ssec(4) ssa(1) nr(2) 1 }' + crlf + crlf +
    'id-ecnr-with-sha1 OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) private(4) enterprise(1) streamsec(13085) ssec(4) ssa(1) ecnr(3) 1 }' + crlf + crlf +
    'id-dsa-with-sha1 OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) ansi-X9-57(10040) (4) 3 }' + crlf + crlf +
    'ecdsa-with-SHA1 OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) ansi-X9-62(10045) signatures(4) 1 }' + crlf + crlf +
    'id-ecPublicKey OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) ansi-X9-62(10045) id-publicKeyType(2) 1 }' + crlf + crlf +
    'Name ::= CHOICE {' + crlf +
    '     rdnSequence'#9'RDNSequence}' + crlf + crlf +
    'RDNSequence ::= SEQUENCE OF RelativeDistinguishedName' + crlf + crlf +
    'RelativeDistinguishedName ::= SET OF AttributeTypeAndValue' + crlf + crlf +
    'AttributeTypeAndValue ::= SEQUENCE {' + crlf +
    '     attrType'#9'OBJECT,' + crlf +
    '     attrValue'#9'AttrValue.&Type(&attrType)}' + crlf + crlf +
    'AttrValue ::= TYPE-IDENTIFIER {' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY id-at-name}|' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY id-at-surname}|' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY id-at-givenName}|' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY id-at-initials}|' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY id-at-generationQualifier}|' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY id-at-commonName}|' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY id-at-localityName}|' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY id-at-stateOrProvinceName}|' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY id-at-organizationName}|' + crlf + 
    '     {DirectoryString'#9'IDENTIFIED BY id-at-organizationalUnitName}|' + crlf + 
    '     {DirectoryString'#9'IDENTIFIED BY id-at-title}|' + crlf + 
    '     {ANSIString'#9'IDENTIFIED BY id-at-dnQualifier}|' + crlf + 
    '     {ANSIString'#9'IDENTIFIED BY id-at-country}|' + crlf + 
    '     {EmailAddress'#9'IDENTIFIED BY emailAddress}}' + crlf + crlf + 
    'DirectoryString ::= CHOICE {' + crlf + 
    '     teletexString'#9'TeletexString,' + crlf + 
    '     printableString'#9'PrintableString,' + crlf + 
    '     universalString'#9'UniversalString,' + crlf + 
    '     utf8String'#9'UTF8STRING,' + crlf +
    '     bmpString'#9'BMPString}' + crlf + crlf + 
    'id-at-name OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 41 }' + crlf + crlf + 
    'id-at-surname OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 4 }' + crlf + crlf + 
    'id-at-givenName OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 42 }' + crlf + crlf + 
    'id-at-initials OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 43 }' + crlf + crlf + 
    'id-at-generationQualifier OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 44 }' + crlf + crlf + 
    'id-at-commonName OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 3 }' + crlf + crlf + 
    'id-at-localityName OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 7 }' + crlf + crlf + 
    'id-at-stateOrProvinceName OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 8 }' + crlf + crlf + 
    'id-at-organizationName OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 10 }' + crlf + crlf + 
    'id-at-organizationalUnitName OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 11 }' + crlf + crlf + 
    'id-at-title OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 12 }' + crlf + crlf +
    'ANSIString ::= CHOICE {' + crlf + 
    '     printableString'#9'PrintableString,' + crlf + 
    '     utf8String'#9'UTF8STRING}' + crlf + crlf + 
    'id-at-dnQualifier OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 46 }' + crlf + crlf + 
    'id-at-country OBJECT IDENTIFIER ::= {  (2) (5) id-at(4) 6 }' + crlf + crlf + 
    'EmailAddress ::= CHOICE {' + crlf + 
    '     ia5String'#9'IA5String,' + crlf + 
    '     utf8String'#9'UTF8STRING}' + crlf + crlf + 
    'emailAddress OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) 1 }' + crlf + crlf + 
    'Validity ::= SEQUENCE {' + crlf + 
    '     notBefore'#9'Time,' + crlf + 
    '     notAfter'#9'Time}' + crlf + crlf + 
    'Time ::= CHOICE {' + crlf + 
    '     utcTime'#9'UTCTime,' + crlf + 
    '     generalTime'#9'GeneralizedTime}' + crlf + crlf + 
    'SubjectPublicKeyInfo ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'AlgorithmIdentifier,' + crlf + 
    '     subjectPublicKey'#9'EncapsulatedPublicKey.&Type(&/algorithm/algorithm)}' + crlf + crlf + 
    'EncapsulatedPublicKey ::= TYPE-IDENTIFIER {' + crlf + 
    '     {EncapsulatedRSAKey'#9'IDENTIFIED BY rsaEncryption}|' + crlf +
    '     {EncapsulatedInteger'#9'IDENTIFIED BY dhpublicnumber}|' + crlf +
    '     {EncapsulatedInteger'#9'IDENTIFIED BY id-dsa}|' + crlf +        
    '     {EncapsulatedInteger'#9'IDENTIFIED BY id-nr}|' + crlf +
    '     {ECPublicKey'#9'IDENTIFIED BY id-ecPublicKey}|' + crlf +
    '     {EncapsulatedRSAKey'#9'IDENTIFIED BY id-RSAES-OAEP}|' + crlf +
    '     {EncapsulatedRSAKey'#9'IDENTIFIED BY id-RSASSA-PSS}|' + crlf +
    '     {EncapsulatedRSAKey'#9'IDENTIFIED BY id-rw}|' + crlf +
    '     {ECPublicKey'#9'IDENTIFIED BY id-ecnr}}' + crlf + crlf +
    'EncapsulatedRSAKey ::= BIT STRING ENCAPSULATES RSAKey' + crlf + crlf +
    'RSAKey ::= SEQUENCE {' + crlf + 
    '     modulus'#9'INTEGER,' + crlf + 
    '     publicExponent'#9'INTEGER}' + crlf + crlf + crlf + 
    'EncapsulatedInteger ::= BIT STRING ENCAPSULATES INTEGER' + crlf + crlf + 
    'ECPublicKey ::= BIT STRING' + crlf + crlf + 
    'ImplExtensions ::= SEQUENCE {' + crlf + 
    '     extensions'#9'Extensions}' + crlf + crlf + 
    'Extensions ::= SEQUENCE OF Extension' + crlf + crlf + 
    'Extension ::= SEQUENCE {' + crlf + 
    '     extnID'#9'OBJECT,' + crlf + 
    '     critical'#9'BOOLEAN DEFAULT 010100,' + crlf +
    '     extnValue'#9'ExtnValue.&Type(&extnID)}' + crlf + crlf + 
    'ExtnValue ::= TYPE-IDENTIFIER {' + crlf + 
    '     {EncapsulatedAuthorityKeyIdentifier'#9'IDENTIFIED BY id-ce-authorityKeyIdentifier}|' + crlf + 
    '     {EncapsulatedKeyIdentifier'#9'IDENTIFIED BY id-ce-subjectKeyIdentifier}|' + crlf + 
    '     {EncapsulatedBitString'#9'IDENTIFIED BY id-ce-keyUsage}|' + crlf + 
    '     {EncapsulatedExtendedKeyUsage'#9'IDENTIFIED BY id-ce-extKeyUsage}|' + crlf + 
    '     {EncapsulatedPrivateKeyUsagePeriod'#9'IDENTIFIED BY id-ce-privateKeyUsagePeriod}|' + crlf + 
    '     {EncapsulatedCertificatePolicies'#9'IDENTIFIED BY id-ce-certificatePolicies}|' + crlf + 
    '     {EncapsulatedPolicyMappings'#9'IDENTIFIED BY id-ce-policyMappings}|' + crlf + 
    '     {EncapsulatedGeneralNames'#9'IDENTIFIED BY id-ce-subjectAltName}|' + crlf + 
    '     {EncapsulatedGeneralNames'#9'IDENTIFIED BY id-ce-issuerAltName}|' + crlf + 
    '     {EncapsulatedBasicConstraints'#9'IDENTIFIED BY id-ce-basicConstraints}|' + crlf + 
    '     {EncapsulatedNameConstraints'#9'IDENTIFIED BY id-ce-nameConstraints}|' + crlf + 
    '     {EncapsulatedPolicyConstraints'#9'IDENTIFIED BY id-ce-policyConstraints}|' + crlf + 
    '     {EncapsulatedCRLDistributionPoints'#9'IDENTIFIED BY id-ce-cRLDistributionPoints}|' + crlf + 
    '     {EncapsulatedSubjectDirectoryAttributes'#9'IDENTIFIED BY id-ce-subjectDirectoryAttributes}|' + crlf + 
    '     {EncapsulatedAuthorityInfoAccess'#9'IDENTIFIED BY id-pe-authorityInfoAccess}|' + crlf + 
    '     {EncapsulatedBitString'#9'IDENTIFIED BY netscape-cert-type}}' + crlf + crlf + 
    'EncapsulatedAuthorityKeyIdentifier ::= OCTET STRING ENCAPSULATES AuthorityKeyIdentifier' + crlf + crlf + 
    'AuthorityKeyIdentifier ::= SEQUENCE {' + crlf +
    '     keyIdentifier'#9'[0] IMPLICIT OCTET STRING OPTIONAL,' + crlf + 
    '     authorityCertIssuer'#9'[1] IMPLICIT GeneralNames OPTIONAL,' + crlf + 
    '     authorityCertSerialNumber'#9'[2] IMPLICIT INTEGER OPTIONAL}' + crlf + crlf + 
    'GeneralNames ::= SEQUENCE OF GeneralName' + crlf + crlf + 
    'GeneralName ::= CHOICE {' + crlf + 
    '     otherName'#9'[0] IMPLICIT AnotherName,' + crlf + 
    '     rfc822Name'#9'[1] IMPLICIT IA5String,' + crlf + 
    '     dNSName'#9'[2] IMPLICIT IA5String,' + crlf + 
    '     x400Address'#9'[3] IMPLICIT ORAddress,' + crlf +
    '     directoryName'#9'[4] Name,' + crlf +
    '     ediPartyName'#9'[5] IMPLICIT EDIPartyName,' + crlf +
    '     uniformResourceIdentifier'#9'[6] IMPLICIT IA5String,' + crlf +
    '     iPAddress'#9'[7] IMPLICIT OCTET STRING,' + crlf +
    '     registeredID'#9'[8] IMPLICIT OBJECT}' + crlf + crlf +
    'GNName ::= SEQUENCE{' + crlf +
    '     name'#9'Name}' + crlf + crlf +
    'AnotherName ::= SEQUENCE {' + crlf +
    '     type-id'#9'OBJECT,' + crlf +
    '     value'#9'[0] ANY}' + crlf + crlf +
    'ORAddress ::= SEQUENCE {' + crlf + 
    '     built-in-standard-attributes'#9'BuiltInStandardAttributes,' + crlf +
    '     built-in-domain-defined-attributes'#9'BuiltInDomainDefinedAttributes OPTIONAL,' + crlf + 
    '     extension-attributes'#9'ExtensionAttributes OPTIONAL}' + crlf + crlf + 
    'BuiltInStandardAttributes ::= SEQUENCE {' + crlf + 
    '     country-name'#9'[APPLICATION 1] CountryName OPTIONAL,' + crlf + 
    '     administration-domain-name'#9'[APPLICATION 2] AdministrationDomainName OPTIONAL,' + crlf + 
    '     network-address'#9'[0] NumericString OPTIONAL,' + crlf + 
    '     terminal-identifier'#9'[1] PrintableString OPTIONAL,' + crlf + 
    '     private-domain-name'#9'[2] PrivateDomainName OPTIONAL,' + crlf + 
    '     organization-name'#9'[3] PrintableString OPTIONAL,' + crlf + 
    '     numeric-user-identifier'#9'[4] NumericString OPTIONAL,' + crlf + 
    '     personal-name'#9'[5] PersonalName OPTIONAL,' + crlf + 
    '     organizational-unit-names'#9'[6] SEQUENCE OF PrintableString OPTIONAL}' + crlf + crlf + 
    'CountryName ::= CHOICE {' + crlf + 
    '     x121-dcc-code'#9'NumericString,' + crlf +
    '     iso-3166-alpha2-code'#9'PrintableString}' + crlf + crlf + 
    'AdministrationDomainName ::= CHOICE {' + crlf + 
    '     numeric'#9'NumericString,' + crlf + 
    '     printable'#9'PrintableString}' + crlf + crlf + 
    'PrivateDomainName ::= CHOICE {' + crlf + 
    '     numeric'#9'NumericString,' + crlf + 
    '     printable'#9'PrintableString}' + crlf + crlf + 
    'PersonalName ::= SET {' + crlf + 
    '     surname'#9'[0] PrintableString,' + crlf + 
    '     given-name'#9'[1] PrintableString OPTIONAL,' + crlf + 
    '     initials'#9'[2] PrintableString OPTIONAL,' + crlf + 
    '     generation-qualifier'#9'[3] PrintableString OPTIONAL}' + crlf + crlf + 
    'BuiltInDomainDefinedAttributes ::= SEQUENCE OF BuiltInDomainDefinedAttribute' + crlf + crlf + 
    'BuiltInDomainDefinedAttribute ::= SEQUENCE {' + crlf + 
    '     typeName'#9'PrintableString,' + crlf + 
    '     value'#9'PrintableString}' + crlf + crlf + crlf + 
    'ExtensionAttributes ::= SET OF ExtensionAttribute' + crlf + crlf + 
    'ExtensionAttribute ::= SEQUENCE {' + crlf + 
    '     extension-attribute-type'#9'[0] INTEGER,' + crlf + 
    '     extension-attribute-value'#9'[1] ExtnAttr.&Type(&/extension-attribute-type/)}' + crlf + crlf + 
    'ExtnAttr ::= TYPE-IDENTIFIER {' + crlf + 
    '     {CommonName'#9'IDENTIFIED BY 1}|' + crlf + 
    '     {TeletexCommonName'#9'IDENTIFIED BY 2}|' + crlf + 
    '     {TeletexOrganizationName'#9'IDENTIFIED BY 3}|' + crlf + 
    '     {TeletexPersonalName'#9'IDENTIFIED BY 4}|' + crlf + 
    '     {TeletexOrganizationalUnitNames'#9'IDENTIFIED BY 5}|' + crlf +
    '     {TeletexDomainDefinedAttributes'#9'IDENTIFIED BY 6}|' + crlf + 
    '     {PDSName'#9'IDENTIFIED BY 7}|' + crlf + 
    '     {PhysicalDeliveryCountryName'#9'IDENTIFIED BY 8}|' + crlf + 
    '     {PostalCode'#9'IDENTIFIED BY 9}|' + crlf + 
    '     {PhysicalDeliveryOfficeName'#9'IDENTIFIED BY 10}|' + crlf + 
    '     {PhysicalDeliveryOfficeNumber'#9'IDENTIFIED BY 11}|' + crlf + 
    '     {ExtensionORAddressComponents'#9'IDENTIFIED BY 12}|' + crlf + 
    '     {PhysicalDeliveryPersonalName'#9'IDENTIFIED BY 13}|' + crlf + 
    '     {PhysicalDeliveryOrganizationName'#9'IDENTIFIED BY 14}|' + crlf +
    '     {ExtensionPhysicalDeliveryAddressComponents'#9'IDENTIFIED BY 15}|' + crlf + 
    '     {UnformattedPostalAddress'#9'IDENTIFIED BY 16}|' + crlf + 
    '     {StreetAddress'#9'IDENTIFIED BY 17}|' + crlf + 
    '     {PostOfficeBoxAddress'#9'IDENTIFIED BY 18}|' + crlf + 
    '     {PosteRestanteAddress'#9'IDENTIFIED BY 19}|' + crlf + 
    '     {UniquePostalName'#9'IDENTIFIED BY 20}|' + crlf + 
    '     {LocalPostalAttributes'#9'IDENTIFIED BY 21}|' + crlf + 
    '     {ExtendedNetworkAddress'#9'IDENTIFIED BY 22}|' + crlf + 
    '     {TerminalType'#9'IDENTIFIED BY 23}}' + crlf + crlf + 
    'CommonName ::= PrintableString' + crlf + crlf + 
    'TeletexCommonName ::= TeletexString' + crlf + crlf + 
    'TeletexOrganizationName ::= TeletexString' + crlf + crlf + 
    'TeletexPersonalName ::= SET {' + crlf + 
    '     surname'#9'[0] TeletexString,' + crlf + 
    '     given-name'#9'[1] TeletexString OPTIONAL,' + crlf + 
    '     initials'#9'[2] TeletexString OPTIONAL,' + crlf + 
    '     generation-qualifier'#9'[3] TeletexString OPTIONAL}' + crlf + crlf + 
    'TeletexOrganizationalUnitNames ::= SEQUENCE OF TeletexString' + crlf + crlf + 
    'TeletexDomainDefinedAttributes ::= SEQUENCE OF TeletexDomainDefinedAttribute' + crlf + crlf + 
    'TeletexDomainDefinedAttribute ::= SEQUENCE {' + crlf + 
    '     typeName'#9'TeletexString,' + crlf + 
    '     value'#9'TeletexString}' + crlf + crlf + crlf + 
    'PDSName ::= PrintableString' + crlf + crlf + 
    'PhysicalDeliveryCountryName ::= CHOICE {' + crlf + 
    '     x121-dcc-code'#9'NumericString,' + crlf + 
    '     iso-3166-alpha2-code'#9'PrintableString}' + crlf + crlf + 
    'PostalCode ::= CHOICE {' + crlf + 
    '     numeric-code'#9'NumericString,' + crlf + 
    '     printable-code'#9'PrintableString}' + crlf + crlf + 
    'PhysicalDeliveryOfficeName ::= PDSParameter' + crlf + crlf + 
    'PDSParameter ::= SET {' + crlf +
    '     printable-string'#9'PrintableString OPTIONAL,' + crlf + 
    '     teletex-string'#9'TeletexString OPTIONAL}' + crlf + crlf + 
    'PhysicalDeliveryOfficeNumber ::= PDSParameter' + crlf + crlf + 
    'ExtensionORAddressComponents ::= PDSParameter' + crlf + crlf +
    'PhysicalDeliveryPersonalName ::= PDSParameter' + crlf + crlf + 
    'PhysicalDeliveryOrganizationName ::= PDSParameter' + crlf + crlf + 
    'ExtensionPhysicalDeliveryAddressComponents ::= PDSParameter' + crlf + crlf + 
    'UnformattedPostalAddress ::= SET {' + crlf + 
    '     printable-address'#9'SEQUENCE OF PrintableString OPTIONAL,' + crlf + 
    '     teletex-string'#9'TeletexString OPTIONAL}' + crlf + crlf + 
    'StreetAddress ::= PDSParameter' + crlf + crlf + 
    'PostOfficeBoxAddress ::= PDSParameter' + crlf + crlf + 
    'PosteRestanteAddress ::= PDSParameter' + crlf + crlf + 
    'UniquePostalName ::= PDSParameter' + crlf + crlf + 
    'LocalPostalAttributes ::= PDSParameter' + crlf + crlf + 
    'ExtendedNetworkAddress ::= CHOICE {' + crlf + 
    '     e163-4-address'#9'E163_4Address,' + crlf + 
    '     psap-address'#9'[0] PresentationAddress}' + crlf + crlf + 
    'E163_4Address ::= SEQUENCE {' + crlf + 
    '     number'#9'[0] NumericString,' + crlf + 
    '     sub-address'#9'[1] NumericString OPTIONAL}' + crlf + crlf + 
    'PresentationAddress ::= SEQUENCE {' + crlf + 
    '     pSelector'#9'[0] OCTET STRING OPTIONAL,' + crlf + 
    '     sSelector'#9'[1] OCTET STRING OPTIONAL,' + crlf + 
    '     tSelector'#9'[2] OCTET STRING OPTIONAL,' + crlf + 
    '     nAddresses'#9'[3] SET OF OCTET STRING}' + crlf + crlf + 
    'TerminalType ::= INTEGER' + crlf + crlf + 
    'EDIPartyName ::= SEQUENCE {' + crlf + 
    '     nameAssigner'#9'[0] IMPLICIT DirectoryString OPTIONAL,' + crlf + 
    '     partyName'#9'[1] IMPLICIT DirectoryString}' + crlf + crlf + crlf + 
    'id-ce-authorityKeyIdentifier OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 35 }' + crlf + crlf + 
    'EncapsulatedKeyIdentifier ::= OCTET STRING ENCAPSULATES OCTET STRING' + crlf + crlf + 
    'id-ce-subjectKeyIdentifier OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 14 }' + crlf + crlf + 
    'EncapsulatedBitString ::= OCTET STRING ENCAPSULATES BIT STRING' + crlf + crlf + 
    'id-ce-keyUsage OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 15 }' + crlf + crlf + 
    'EncapsulatedExtendedKeyUsage ::= OCTET STRING ENCAPSULATES ExtKeyUsageSyntax' + crlf + crlf + 
    'ExtKeyUsageSyntax ::= SEQUENCE OF OBJECT' + crlf + crlf + crlf + 
    'id-ce-extKeyUsage OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 37 }' + crlf + crlf + 
    'EncapsulatedPrivateKeyUsagePeriod ::= OCTET STRING ENCAPSULATES PrivateKeyUsagePeriod' + crlf + crlf +
    'PrivateKeyUsagePeriod ::= SEQUENCE {' + crlf +
    '     notBefore'#9'[0] IMPLICIT GeneralizedTime OPTIONAL,' + crlf + 
    '     notAfter'#9'[1] IMPLICIT GeneralizedTime OPTIONAL}' + crlf + crlf + crlf + 
    'id-ce-privateKeyUsagePeriod OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 16 }' + crlf + crlf + 
    'EncapsulatedCertificatePolicies ::= OCTET STRING ENCAPSULATES CertificatePolicies' + crlf + crlf + 
    'CertificatePolicies ::= SEQUENCE OF PolicyInformation' + crlf + crlf + 
    'PolicyInformation ::= SEQUENCE {' + crlf + 
    '     policyIdentifier'#9'OBJECT,' + crlf + 
    '     policyQualifiers'#9'SEQUENCE OF PolicyQualifierInfo OPTIONAL}' + crlf + crlf + 
    'PolicyQualifierInfo ::= SEQUENCE {' + crlf + 
    '     policyQualifierId'#9'OBJECT,' + crlf + 
    '     qualifier'#9'Qualifier.&Type(&policyQualifierID)}' + crlf + crlf + 
    'Qualifier ::= TYPE-IDENTIFIER {' + crlf + 
    '     {IA5String'#9'IDENTIFIED BY id-qt-cps}|' + crlf + 
    '     {UserNotice'#9'IDENTIFIED BY id-qt-unotice}}' + crlf + crlf + 
    'id-qt-cps OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) (2) 1 }' + crlf + crlf + 
    'UserNotice ::= SEQUENCE {' + crlf + 
    '     noticeRef'#9'NoticeReference OPTIONAL,' + crlf + 
    '     explicitText'#9'DisplayText OPTIONAL}' + crlf + crlf + 
    'NoticeReference ::= SEQUENCE {' + crlf + 
    '     organization'#9'DisplayText,' + crlf + 
    '     noticeNumbers'#9'SEQUENCE OF INTEGER}' + crlf + crlf + 
    'DisplayText ::= CHOICE {' + crlf + 
    '     visibleString'#9'VisibleString,' + crlf + 
    '     bmpString'#9'BMPString,' + crlf + 
    '     utf8String'#9'UTF8STRING}' + crlf + crlf + 
    'id-qt-unotice OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) (2) 2 }' + crlf + crlf + crlf + crlf + 
    'id-ce-certificatePolicies OBJECT IDENTIFIER ::= {  (2) (5) (29) 32 }' + crlf + crlf +
    'EncapsulatedPolicyMappings ::= OCTET STRING ENCAPSULATES PolicyMappingsSyntax' + crlf + crlf + 
    'PolicyMappingsSyntax ::= SEQUENCE OF PolicyMapping' + crlf + crlf + 
    'PolicyMapping ::= SEQUENCE {' + crlf + 
    '     issuerDomainPolicy'#9'OBJECT,' + crlf + 
    '     subjectDomainPolicy'#9'OBJECT}' + crlf + crlf + crlf + crlf + 
    'id-ce-policyMappings OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 33 }' + crlf + crlf + 
    'EncapsulatedGeneralNames ::= OCTET STRING ENCAPSULATES GeneralNames' + crlf + crlf +
    'id-ce-subjectAltName OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 17 }' + crlf + crlf + 
    'id-ce-issuerAltName OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 18 }' + crlf + crlf + 
    'EncapsulatedBasicConstraints ::= OCTET STRING ENCAPSULATES BasicConstraintsSyntax' + crlf + crlf + 
    'BasicConstraintsSyntax ::= SEQUENCE {' + crlf + 
    '     cA'#9'BOOLEAN DEFAULT 010100,' + crlf + 
    '     pathLenConstraint'#9'INTEGER OPTIONAL}' + crlf + crlf + crlf +
    'id-ce-basicConstraints OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 19 }' + crlf + crlf + 
    'EncapsulatedNameConstraints ::= OCTET STRING ENCAPSULATES NameConstraintsSyntax' + crlf + crlf + 
    'NameConstraintsSyntax ::= SEQUENCE {' + crlf + 
    '     permittedSubtrees'#9'[0] IMPLICIT GeneralSubtrees OPTIONAL,' + crlf + 
    '     excludedSubtrees'#9'[1] IMPLICIT GeneralSubtrees OPTIONAL}' + crlf + crlf + 
    'GeneralSubtrees ::= SEQUENCE OF GeneralSubtree' + crlf + crlf + 
    'GeneralSubtree ::= SEQUENCE {' + crlf + 
    '     base'#9'GeneralName,' + crlf + 
    '     minimum'#9'[0] IMPLICIT INTEGER DEFAULT 020100,' + crlf + 
    '     maximum'#9'[1] IMPLICIT INTEGER OPTIONAL}' + crlf + crlf + crlf + 
    'id-ce-nameConstraints OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 30 }' + crlf + crlf + 
    'EncapsulatedPolicyConstraints ::= OCTET STRING ENCAPSULATES PolicyConstraintsSyntax' + crlf + crlf + 
    'PolicyConstraintsSyntax ::= SEQUENCE {' + crlf + 
    '     requireExplicitPolicy'#9'[0] IMPLICIT INTEGER OPTIONAL,' + crlf + 
    '     inhibitPolicyMapping'#9'[1] IMPLICIT INTEGER OPTIONAL}' + crlf + crlf + crlf + 
    'id-ce-policyConstraints OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 36 }' + crlf + crlf + 
    'EncapsulatedCRLDistributionPoints ::= OCTET STRING ENCAPSULATES CRLDistributionPointsSyntax' + crlf + crlf + 
    'CRLDistributionPointsSyntax ::= SEQUENCE OF DistributionPoint' + crlf + crlf + 
    'DistributionPoint ::= SEQUENCE {' + crlf +
    '     distributionPoint'#9'[0] DistributionPointName OPTIONAL,' + crlf +
    '     reasons'#9'[1] IMPLICIT BIT STRING OPTIONAL,' + crlf + 
    '     cRLIssuer'#9'[2] IMPLICIT GeneralNames OPTIONAL}' + crlf + crlf + 
    'DistributionPointName ::= CHOICE {' + crlf + 
    '     fullName'#9'[0] IMPLICIT GeneralNames,' + crlf + 
    '     nameRelativeToCRLIssuer'#9'[1] IMPLICIT RelativeDistinguishedName}' + crlf + crlf + crlf + crlf + 
    'id-ce-cRLDistributionPoints OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 31 }' + crlf + crlf + 
    'EncapsulatedSubjectDirectoryAttributes ::= OCTET STRING ENCAPSULATES AttributesSyntax' + crlf + crlf + 
    'AttributesSyntax ::= SEQUENCE OF Attribute' + crlf + crlf + 
    'Attribute ::= SEQUENCE {' + crlf +
    '     attributeType'#9'OBJECT,' + crlf + 
    '     attributeValue'#9'SET OF AttrValue.&Type(&attributeType)}' + crlf + crlf + crlf + crlf + 
    'id-ce-subjectDirectoryAttributes OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 9 }' + crlf + crlf + 
    'EncapsulatedAuthorityInfoAccess ::= OCTET STRING ENCAPSULATES AuthorityInfoAccessSyntax' + crlf + crlf + 
    'AuthorityInfoAccessSyntax ::= SEQUENCE OF AccessDescription' + crlf + crlf + 
    'AccessDescription ::= SEQUENCE {' + crlf + 
    '     accessMethod'#9'OBJECT,' + crlf + 
    '     accessLocation'#9'GeneralName}' + crlf + crlf + crlf + crlf + 
    'id-pe-authorityInfoAccess OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) id-pe(1) 1 }' + crlf + crlf +
    'netscape OBJECT IDENTIFIER ::= {  2 16 840 1 113730 }' + crlf +
    'netscape-cert-extension OBJECT IDENTIFIER ::= {  netscape 1 }' + crlf +
    'netscape-cert-type OBJECT IDENTIFIER ::= {  netscape-cert-extension 1 }' + crlf +
    'END' + crlf + crlf;
const
  digitalSignatureVal = 0;
  nonRepudiationVal   = 1;
  keyEnciphermentVal  = 2;
  dataEnciphermentVal = 3;
  keyAgreementVal     = 4;
  keyCertSignVal      = 5;
  cRLSignVal          = 6;
  encipherOnlyVal     = 7;
  decipherOnlyVal     = 8;

type
{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TDomainParameters
         TValidationParms}

  TValidationParms = class(TASNConstructedWrapper)
  private
    function GetSeed: TBitString;
    procedure SetSeed(const Value: TBitString);
    function GetPgenCounter: TIntegerWrapper;
    procedure SetPgenCounter(const Value: TIntegerWrapper);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Seed: TBitString read GetSeed write SetSeed;
    property PgenCounter: TIntegerWrapper read GetPgenCounter write SetPgenCounter;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TDomainParameters}

  TDomainParameters = class(TASNConstructedWrapper)
  private
    function GetP: TIntegerWrapper;
    procedure SetP(const Value: TIntegerWrapper);
    function GetG: TIntegerWrapper;
    procedure SetG(const Value: TIntegerWrapper);
    function GetQ: TIntegerWrapper;
    procedure SetQ(const Value: TIntegerWrapper);
    function GetJ: TIntegerWrapper;
    procedure SetJ(const Value: TIntegerWrapper);
    function GetValidationParms: TValidationParms;
    procedure SetValidationParms(const Value: TValidationParms);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property P: TIntegerWrapper read GetP write SetP;
    property G: TIntegerWrapper read GetG write SetG;
    property Q: TIntegerWrapper read GetQ write SetQ;
    property J: TIntegerWrapper read GetJ write SetJ;
    property ValidationParms: TValidationParms read GetValidationParms write SetValidationParms;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TDss_Params}

  TDss_Params = class(TASNConstructedWrapper)
  private
    function GetP: TIntegerWrapper;
    procedure SetP(const Value: TIntegerWrapper);
    function GetQ: TIntegerWrapper;
    procedure SetQ(const Value: TIntegerWrapper);
    function GetG: TIntegerWrapper;
    procedure SetG(const Value: TIntegerWrapper);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property P: TIntegerWrapper read GetP write SetP;
    property Q: TIntegerWrapper read GetQ write SetQ;
    property G: TIntegerWrapper read GetG write SetG;
  end;
 
{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TRsaesOaepParams
         TDigestAlgorithmIdentifier
          TDigParams}

  TDigParamsEnum = (
    dpeUndefined, dpeSha1, dpeMd2, dpeMd5, dpeIdRipeMd160, dpeSha256, dpeSha384,
    dpeSha512);

  TDigParams = class(TASNChoiceWrapper)
  private
    function GetChoice: TDigParamsEnum;
    procedure SetChoice(const Value: TDigParamsEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Choice: TDigParamsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TRsaesOaepParams
         TDigestAlgorithmIdentifier}

  TDigestAlgorithmIdentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TDigParams;
    procedure SetParameters(const Value: TDigParams);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TDigParamsEnum): Boolean;
    procedure SetType(Id: TDigParamsEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TDigParams read GetParameters write SetParameters;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TRsaesOaepParams
         TMgfAlgorithmIdentifier
          TMgfParams}

  TMgfParamsEnum = (
    mpeUndefined, mpeIdMgf1);

  TMgfParams = class(TASNChoiceWrapper)
  private
    function GetChoice: TMgfParamsEnum;
    procedure SetChoice(const Value: TMgfParamsEnum);
    function GetAsMgf1: TDigestAlgorithmIdentifier;
    procedure SetAsMgf1(const Value: TDigestAlgorithmIdentifier);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsMgf1: TDigestAlgorithmIdentifier read GetAsMgf1 write SetAsMgf1;
  published
    property Choice: TMgfParamsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TRsaesOaepParams
         TMgfAlgorithmIdentifier}

  TMgfAlgorithmIdentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TMgfParams;
    procedure SetParameters(const Value: TMgfParams);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TMgfParamsEnum): Boolean;
    procedure SetType(Id: TMgfParamsEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TMgfParams read GetParameters write SetParameters;
  end;                   

{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TRsaesOaepParams
         TPsAlgorithmIdentifier
          TPsParams}

  TPsParamsEnum = (
    ppeUndefined, ppeIdPSpecified);

  TPsParams = class(TASNChoiceWrapper)
  private
    function GetChoice: TPsParamsEnum;
    procedure SetChoice(const Value: TPsParamsEnum);
    function GetAsPSpecified: TOctetString;
    procedure SetAsPSpecified(const Value: TOctetString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsPSpecified: TOctetString read GetAsPSpecified write SetAsPSpecified;
  published
    property Choice: TPsParamsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TRsaesOaepParams
         TPsAlgorithmIdentifier}

  TPsAlgorithmIdentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TPsParams;
    procedure SetParameters(const Value: TPsParams);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TPsParamsEnum): Boolean;
    procedure SetType(Id: TPsParamsEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TPsParams read GetParameters write SetParameters;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TRsassaPssParams}

  TRsassaPssParams = class(TASNConstructedWrapper)
  private
    function GetHashFunc: TDigestAlgorithmIdentifier;
    procedure SetHashFunc(const Value: TDigestAlgorithmIdentifier);
    function GetMgfFunc: TMgfAlgorithmIdentifier;
    procedure SetMgfFunc(const Value: TMgfAlgorithmIdentifier);
  protected
    procedure InheritedCreate(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property HashFunc: TDigestAlgorithmIdentifier read GetHashFunc write SetHashFunc;
    property MgfFunc: TMgfAlgorithmIdentifier read GetMgfFunc write SetMgfFunc;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID
        TRsaesOaepParams}

  TRsaesOaepParams = class(TRsassaPssParams)
  private
    function GetPSourceFunc: TPsAlgorithmIdentifier;
    procedure SetPSourceFunc(const Value: TPsAlgorithmIdentifier);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property PSourceFunc: TPsAlgorithmIdentifier read GetPSourceFunc write SetPSourceFunc;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier
       TAlgorithmID}

  TAlgorithmIDEnum = (
    aideUndefined, aideRsaEncryption, aideMd5WithRSAEncryption,
    aideSha1WithRSAEncryption, aideIdDsa, aideIdNR, aideIdDsaWithSha1,
    aideDhpublicnumber, aideSha256WithRSAEncryption,
    aideSha384WithRSAEncryption, aideSha512WithRSAEncryption,
    aideMd2WithRSAEncryption, aideEcdsaWithSHA1, aideIdEcPublicKey,
    aideIdRsaesOaep, aideIdRsassaPss, aideIdRW, aideIdEcNr,
    aideIdNrWithSha1,aideIdEcNrWithSha1);

  TAlgorithmID = class(TASNChoiceWrapper)
  private
    function GetAsDhpublicnumber: TDomainParameters;
    procedure SetAsDhpublicnumber(const Value: TDomainParameters);
    function GetAsDsa: TDss_Params;
    procedure SetAsDsa(const Value: TDss_Params);
    function GetAsEcPublicKey: ObjectIdentifier;
    procedure SetAsEcPublicKey(const Value: ObjectIdentifier);
    function GetChoice: TAlgorithmIDEnum;
    procedure SetChoice(const Value: TAlgorithmIDEnum);
    function GetAsRsaesOaep: TRsaesOaepParams;
    procedure SetAsRsaesOaep(const Value: TRsaesOaepParams);
    function GetAsRsassaPss: TRsassaPssParams;
    procedure SetAsRsassaPss(const Value: TRsassaPssParams);
    function GetAsNr: TDss_Params;
    procedure SetAsNr(const Value: TDss_Params);
    function GetAsRw: TRsassaPssParams;
    procedure SetAsRw(const Value: TRsassaPssParams);
    function GetAsEcNr: ObjectIdentifier;
    procedure SetAsEcNr(const Value: ObjectIdentifier);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsDhpublicnumber: TDomainParameters read GetAsDhpublicnumber write SetAsDhpublicnumber;
    property AsDsa: TDss_Params read GetAsDsa write SetAsDsa;
    property AsNR: TDss_Params read GetAsNr write SetAsNr;
    property AsEcPublicKey: ObjectIdentifier read GetAsEcPublicKey write SetAsEcPublicKey;
    property AsRsaesOaep: TRsaesOaepParams read GetAsRsaesOaep write SetAsRsaesOaep;
    property AsRsassaPss: TRsassaPssParams read GetAsRsassaPss write SetAsRsassaPss;
    property AsRw: TRsassaPssParams read GetAsRw write SetAsRw;
    property AsEcNr: ObjectIdentifier read GetAsEcNr write SetAsEcNr;
  published
    property Choice: TAlgorithmIDEnum read GetChoice write SetChoice;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TAlgorithmIdentifier}

  TAlgorithmIdentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TAlgorithmID;
    procedure SetParameters(const Value: TAlgorithmID);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TAlgorithmIDEnum): Boolean;
    procedure SetType(Id: TAlgorithmIDEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TAlgorithmID read GetParameters write SetParameters;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TName
       TRdnsequence
        TRelativeDistinguishedName
         TAttributeTypeAndValue
          TAttrValue
           TDirectoryString}

  TDirectoryStringEnum = (
    dseUndefined, dseTeletexString, dsePrintableString, dseUniversalString,
    dseUtf8String, dseBmpString);

  TDirectoryString = class(TASNChoiceWrapper)
  private
    function GetAsTeletexString: TeletexString;
    procedure SetAsTeletexString(const Value: TeletexString);
    function GetAsPrintableString: PrintableString;
    procedure SetAsPrintableString(const Value: PrintableString);
    function GetAsUniversalString: UniversalString;
    procedure SetAsUniversalString(const Value: UniversalString);
    function GetAsUtf8String: WideString;
    procedure SetAsUtf8String(const Value: WideString);
    function GetAsBmpString: WideString;
    procedure SetAsBmpString(const Value: WideString);
    function GetChoice: TDirectoryStringEnum;
    procedure SetChoice(const Value: TDirectoryStringEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsTeletexString: TeletexString read GetAsTeletexString write SetAsTeletexString;
    property AsPrintableString: PrintableString read GetAsPrintableString write SetAsPrintableString;
    property AsUniversalString: UniversalString read GetAsUniversalString write SetAsUniversalString;
    property AsUtf8String: WideString read GetAsUtf8String write SetAsUtf8String;
    property AsBmpString: WideString read GetAsBmpString write SetAsBmpString;
  published
    property Choice: TDirectoryStringEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TName
       TRdnsequence
        TRelativeDistinguishedName
         TAttributeTypeAndValue
          TAttrValue
           TAnsistring}

  TAnsistringEnum = (
    aeUndefined, aePrintableString, aeUtf8String);

  TAnsistring = class(TASNChoiceWrapper)
  private
    function GetAsPrintableString: PrintableString;
    procedure SetAsPrintableString(const Value: PrintableString);
    function GetAsUtf8String: WideString;
    procedure SetAsUtf8String(const Value: WideString);
    function GetChoice: TAnsistringEnum;
    procedure SetChoice(const Value: TAnsistringEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsPrintableString: PrintableString read GetAsPrintableString write SetAsPrintableString;
    property AsUtf8String: WideString read GetAsUtf8String write SetAsUtf8String;
  published
    property Choice: TAnsistringEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TName
       TRdnsequence
        TRelativeDistinguishedName
         TAttributeTypeAndValue
          TAttrValue
           TEmailAddress}

  TEmailAddressEnum = (
    eaeUndefined, eaeIa5String, eaeUtf8String);

  TEmailAddress = class(TASNChoiceWrapper)
  private
    function GetAsIa5String: IA5String;
    procedure SetAsIa5String(const Value: IA5String);
    function GetAsUtf8String: WideString;
    procedure SetAsUtf8String(const Value: WideString);
    function GetChoice: TEmailAddressEnum;
    procedure SetChoice(const Value: TEmailAddressEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsIa5String: IA5String read GetAsIa5String write SetAsIa5String;
    property AsUtf8String: WideString read GetAsUtf8String write SetAsUtf8String;
  published
    property Choice: TEmailAddressEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TName
       TRdnsequence
        TRelativeDistinguishedName
         TAttributeTypeAndValue
          TAttrValue}

  TAttrValueEnum = (
    aveUndefined, aveIdAtName, aveIdAtSurname, aveIdAtGivenName,
    aveIdAtInitials, aveIdAtGenerationQualifier, aveIdAtCommonName,
    aveIdAtLocalityName, aveIdAtStateOrProvinceName, aveIdAtOrganizationName,
    aveIdAtOrganizationalUnitName, aveIdAtTitle, aveIdAtDnQualifier,
    aveIdAtCountry, aveEmailAddress);

  TAttrValue = class(TASNChoiceWrapper)
  private
    function GetAsAt_Name: TDirectoryString;
    procedure SetAsAt_Name(const Value: TDirectoryString);
    function GetAsAt_Surname: TDirectoryString;
    procedure SetAsAt_Surname(const Value: TDirectoryString);
    function GetAsAt_GivenName: TDirectoryString;
    procedure SetAsAt_GivenName(const Value: TDirectoryString);
    function GetAsAt_Initials: TDirectoryString;
    procedure SetAsAt_Initials(const Value: TDirectoryString);
    function GetAsAt_GenerationQualifier: TDirectoryString;
    procedure SetAsAt_GenerationQualifier(const Value: TDirectoryString);
    function GetAsAt_CommonName: TDirectoryString;
    procedure SetAsAt_CommonName(const Value: TDirectoryString);
    function GetAsAt_LocalityName: TDirectoryString;
    procedure SetAsAt_LocalityName(const Value: TDirectoryString);
    function GetAsAt_StateOrProvinceName: TDirectoryString;
    procedure SetAsAt_StateOrProvinceName(const Value: TDirectoryString);
    function GetAsAt_OrganizationName: TDirectoryString;
    procedure SetAsAt_OrganizationName(const Value: TDirectoryString);
    function GetAsAt_OrganizationalUnitName: TDirectoryString;
    procedure SetAsAt_OrganizationalUnitName(const Value: TDirectoryString);
    function GetAsAt_Title: TDirectoryString;
    procedure SetAsAt_Title(const Value: TDirectoryString);
    function GetAsAt_DnQualifier: TAnsistring;
    procedure SetAsAt_DnQualifier(const Value: TAnsistring);
    function GetAsAt_Country: TAnsistring;
    procedure SetAsAt_Country(const Value: TAnsistring);
    function GetAsEmailAddress: TEmailAddress;
    procedure SetAsEmailAddress(const Value: TEmailAddress);
    function GetChoice: TAttrValueEnum;
    procedure SetChoice(const Value: TAttrValueEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsAt_Name: TDirectoryString read GetAsAt_Name write SetAsAt_Name;
    property AsAt_Surname: TDirectoryString read GetAsAt_Surname write SetAsAt_Surname;
    property AsAt_GivenName: TDirectoryString read GetAsAt_GivenName write SetAsAt_GivenName;
    property AsAt_Initials: TDirectoryString read GetAsAt_Initials write SetAsAt_Initials;
    property AsAt_GenerationQualifier: TDirectoryString read GetAsAt_GenerationQualifier write SetAsAt_GenerationQualifier;
    property AsAt_CommonName: TDirectoryString read GetAsAt_CommonName write SetAsAt_CommonName;
    property AsAt_LocalityName: TDirectoryString read GetAsAt_LocalityName write SetAsAt_LocalityName;
    property AsAt_StateOrProvinceName: TDirectoryString read GetAsAt_StateOrProvinceName write SetAsAt_StateOrProvinceName;
    property AsAt_OrganizationName: TDirectoryString read GetAsAt_OrganizationName write SetAsAt_OrganizationName;
    property AsAt_OrganizationalUnitName: TDirectoryString read GetAsAt_OrganizationalUnitName write SetAsAt_OrganizationalUnitName;
    property AsAt_Title: TDirectoryString read GetAsAt_Title write SetAsAt_Title;
    property AsAt_DnQualifier: TAnsistring read GetAsAt_DnQualifier write SetAsAt_DnQualifier;
    property AsAt_Country: TAnsistring read GetAsAt_Country write SetAsAt_Country;
    property AsEmailAddress: TEmailAddress read GetAsEmailAddress write SetAsEmailAddress;
  published
    property Choice: TAttrValueEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TName
       TRdnsequence
        TRelativeDistinguishedName
         TAttributeTypeAndValue}

  TAttributeTypeAndValue = class(TASNConstructedWrapper)
  private
    function GetAttrType: ObjectIdentifier;
    procedure SetAttrType(const Value: ObjectIdentifier);
    function GetAttrValue: TAttrValue;
    procedure SetAttrValue(const Value: TAttrValue);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TAttrValueEnum): Boolean;
    procedure SetType(Id: TAttrValueEnum);
  published
    property AttrType: ObjectIdentifier read GetAttrType write SetAttrType;
    property AttrValue: TAttrValue read GetAttrValue write SetAttrValue;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TName
       TRdnsequence
        TRelativeDistinguishedName}

  TRelativeDistinguishedName = class(TASNCustomOFFieldWrapper)
  private
    function GetUniqueItem(Id: TAttrValueEnum): TAttributeTypeAndValue;
    function GetItems(Index: Integer): TAttributeTypeAndValue;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function AddUniqueItem(Id: TAttrValueEnum): TAttributeTypeAndValue;
    function Add: TAttributeTypeAndValue;
    property Items[Index: Integer]: TAttributeTypeAndValue read GetItems;
    property UniqueItem[Id: TAttrValueEnum]: TAttributeTypeAndValue read GetUniqueItem; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TName
       TRdnsequence}

  TRdnsequence = class(TASNCustomOFFieldWrapper)
  private
    FLineSeparator: string;
    function GetUniqueItem(Id: TAttrValueEnum): TAttributeTypeAndValue;
    function GetItems(Index: Integer): TRelativeDistinguishedName;
    function GetAsAt_CommonName: WideString;
    function GetAsAt_Country: string;
    function GetAsAt_DnQualifier: string;
    function GetAsAt_GenerationQualifier: WideString;
    function GetAsAt_GivenName: WideString;
    function GetAsAt_Initials: WideString;
    function GetAsAt_LocalityName: WideString;
    function GetAsAt_OrganizationalUnitName: WideString;
    function GetAsAt_OrganizationName: WideString;
    function GetAsAt_StateOrProvinceName: WideString;
    function GetAsAt_Surname: WideString;
    function GetAsAt_Title: WideString;
    procedure SetAsAt_CommonName(const Value: WideString);
    procedure SetAsAt_Country(const Value: string);
    procedure SetAsAt_DnQualifier(const Value: string);
    procedure SetAsAt_GenerationQualifier(const Value: WideString);
    procedure SetAsAt_GivenName(const Value: WideString);
    procedure SetAsAt_Initials(const Value: WideString);
    procedure SetAsAt_LocalityName(const Value: WideString);
    procedure SetAsAt_OrganizationalUnitName(const Value: WideString);
    procedure SetAsAt_OrganizationName(const Value: WideString);
    procedure SetAsAt_StateOrProvinceName(const Value: WideString);
    procedure SetAsAt_Surname(const Value: WideString);
    procedure SetAsAt_Title(const Value: WideString);
    procedure SetLineSeparator(const Value: string);
    function GetAsEmailAddress: string;
    procedure SetAsEmailAddress(const Value: string);
  protected                       
    function GetNextValue(Id: TAttrValueEnum; var I, J: Integer): WideString;
    function GetValues(Id: TAttrValueEnum): WideString;
    function UniqueValue(Id: TAttrValueEnum): WideString;
    procedure SetUniqueValue(Id: TAttrValueEnum; Value: WideString);
    procedure SetValue(Id: TAttrValueEnum; Value: WideString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;                                      
    function AddItem(Id: TAttrValueEnum): TAttributeTypeAndValue;
    function AddUniqueItem(Id: TAttrValueEnum): TAttributeTypeAndValue;
    function Add: TRelativeDistinguishedName;
    function Compare(AName: TRdnSequence): Boolean;
    property Items[Index: Integer]: TRelativeDistinguishedName read GetItems;
    property UniqueItem[Id: TAttrValueEnum]: TAttributeTypeAndValue read GetUniqueItem; default;
    property Values[Id: TAttrValueEnum]: WideString read GetValues write SetValue;
  published
    property Surname: WideString read GetAsAt_Surname write SetAsAt_Surname;
    property GivenName: WideString read GetAsAt_GivenName write SetAsAt_GivenName;
    property Initials: WideString read GetAsAt_Initials write SetAsAt_Initials;
    property GenerationQualifier: WideString read GetAsAt_GenerationQualifier write SetAsAt_GenerationQualifier;
    property CommonName: WideString read GetAsAt_CommonName write SetAsAt_CommonName;
    property LocalityName: WideString read GetAsAt_LocalityName write SetAsAt_LocalityName;
    property StateOrProvinceName: WideString read GetAsAt_StateOrProvinceName write SetAsAt_StateOrProvinceName;
    property OrganizationName: WideString read GetAsAt_OrganizationName write SetAsAt_OrganizationName;
    property OrganizationalUnitName: WideString read GetAsAt_OrganizationalUnitName write SetAsAt_OrganizationalUnitName;
    property Title: WideString read GetAsAt_Title write SetAsAt_Title;
    property DnQualifier: string read GetAsAt_DnQualifier write SetAsAt_DnQualifier;
    property Country: string read GetAsAt_Country write SetAsAt_Country;
    property AsEmailAddress: string read GetAsEmailAddress write SetAsEmailAddress;
    property LineSeparator: string read FLineSeparator write SetLineSeparator;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TName}

  TNameEnum = (
    neUndefined, neRdnSequence);

  TName = class(TASNChoiceWrapper)
  private
    function GetAsRdnSequence: TRdnsequence;
    procedure SetAsRdnSequence(const Value: TRdnsequence);
    function GetChoice: TNameEnum;
    procedure SetChoice(const Value: TNameEnum);
    function GetAsAt_CommonName: WideString;
    function GetAsAt_Country: string;
    function GetAsAt_DnQualifier: string;
    function GetAsAt_GenerationQualifier: WideString;
    function GetAsAt_GivenName: WideString;
    function GetAsAt_Initials: WideString;
    function GetAsAt_LocalityName: WideString;
    function GetAsAt_OrganizationalUnitName: WideString;
    function GetAsAt_OrganizationName: WideString;
    function GetAsAt_StateOrProvinceName: WideString;
    function GetAsAt_Surname: WideString;
    function GetAsAt_Title: WideString;
    procedure SetAsAt_CommonName(const Value: WideString);
    procedure SetAsAt_Country(const Value: string);
    procedure SetAsAt_DnQualifier(const Value: string);
    procedure SetAsAt_GenerationQualifier(const Value: WideString);
    procedure SetAsAt_GivenName(const Value: WideString);
    procedure SetAsAt_Initials(const Value: WideString);
    procedure SetAsAt_LocalityName(const Value: WideString);
    procedure SetAsAt_OrganizationalUnitName(const Value: WideString);
    procedure SetAsAt_OrganizationName(const Value: WideString);
    procedure SetAsAt_StateOrProvinceName(const Value: WideString);
    procedure SetAsAt_Surname(const Value: WideString);
    procedure SetAsAt_Title(const Value: WideString);
    procedure SetLineSeparator(const Value: string);
    function GetLineSeparator: string;
  protected
    function GetNextValue(Id: TAttrValueEnum; var I, J: Integer): WideString;
    function GetValues(Id: TAttrValueEnum): WideString;
    function UniqueValue(Id: TAttrValueEnum): WideString;
    procedure SetUniqueValue(Id: TAttrValueEnum; Value: WideString);
    procedure SetValue(Id: TAttrValueEnum; Value: WideString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Compare(Name: TName): Boolean;
    property AsRdnSequence: TRdnsequence read GetAsRdnSequence write SetAsRdnSequence;
    property Choice: TNameEnum read GetChoice write SetChoice;
  published
    property Surname: WideString read GetAsAt_Surname write SetAsAt_Surname;
    property GivenName: WideString read GetAsAt_GivenName write SetAsAt_GivenName;
    property Initials: WideString read GetAsAt_Initials write SetAsAt_Initials;
    property GenerationQualifier: WideString read GetAsAt_GenerationQualifier write SetAsAt_GenerationQualifier;
    property CommonName: WideString read GetAsAt_CommonName write SetAsAt_CommonName;
    property LocalityName: WideString read GetAsAt_LocalityName write SetAsAt_LocalityName;
    property StateOrProvinceName: WideString read GetAsAt_StateOrProvinceName write SetAsAt_StateOrProvinceName;
    property OrganizationName: WideString read GetAsAt_OrganizationName write SetAsAt_OrganizationName;
    property OrganizationalUnitName: WideString read GetAsAt_OrganizationalUnitName write SetAsAt_OrganizationalUnitName;
    property Title: WideString read GetAsAt_Title write SetAsAt_Title;
    property DnQualifier: string read GetAsAt_DnQualifier write SetAsAt_DnQualifier;
    property Country: string read GetAsAt_Country write SetAsAt_Country;
    property LineSeparator: string read GetLineSeparator write SetLineSeparator;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TValidity
       TTime}

  TTimeEnum = (
    teUndefined, teUtcTime, teGeneralTime);

  TTime = class(TASNChoiceWrapper)
  private
    function GetAsUtcTime: TUTCTime;
    procedure SetAsUtcTime(const Value: TUTCTime);
    function GetAsGeneralTime: TGeneralizedTime;
    procedure SetAsGeneralTime(const Value: TGeneralizedTime);
    function GetChoice: TTimeEnum;
    procedure SetChoice(const Value: TTimeEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsUtcTime: TUTCTime read GetAsUtcTime write SetAsUtcTime;
    property AsGeneralTime: TGeneralizedTime read GetAsGeneralTime write SetAsGeneralTime;
  published
    property Choice: TTimeEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TValidity}

  TValidity = class(TASNConstructedWrapper)
  private
    function GetNotBefore: TTime;
    procedure SetNotBefore(const Value: TTime);
    function GetNotAfter: TTime;
    procedure SetNotAfter(const Value: TTime);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property NotBefore: TTime read GetNotBefore write SetNotBefore;
    property NotAfter: TTime read GetNotAfter write SetNotAfter;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TSubjectPublicKeyInfo
       TEncapsulatedPublicKey
        TRsakey}

  TRsakey = class(TASNConstructedWrapper)
  private
    function GetModulus: TIntegerWrapper;
    procedure SetModulus(const Value: TIntegerWrapper);
    function GetPublicExponent: TIntegerWrapper;
    procedure SetPublicExponent(const Value: TIntegerWrapper);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Modulus: TIntegerWrapper read GetModulus write SetModulus;
    property PublicExponent: TIntegerWrapper read GetPublicExponent write SetPublicExponent;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TSubjectPublicKeyInfo
       TEncapsulatedPublicKey}

  TEncapsulatedPublicKeyEnum = (
    epkeUndefined, epkeRsaEncryption, epkeDhpublicnumber, epkeIdDsa, epkeIdNR,
    epkeIdEcPublicKey,epkeIdRsaesOaep,epkeIdRsassaPss,epkeIdRw,epkeIdEcNr);

  TEncapsulatedPublicKey = class(TASNChoiceWrapper)
  private
    function GetAsRsaEncryption: TRsakey;
    procedure SetAsRsaEncryption(const Value: TRsakey);
    function GetAsDhpublicnumber: TIntegerWrapper;
    procedure SetAsDhpublicnumber(const Value: TIntegerWrapper);
    function GetAsDsa: TIntegerWrapper;
    procedure SetAsDsa(const Value: TIntegerWrapper);
    function GetAsEcPublicKey: TBitString;
    procedure SetAsEcPublicKey(const Value: TBitString);
    function GetChoice: TEncapsulatedPublicKeyEnum;
    procedure SetChoice(const Value: TEncapsulatedPublicKeyEnum);
    function GetAsRsaesOaep: TRsakey;
    procedure SetAsRsaesOaep(const Value: TRsakey);
    function GetAsRsassaPss: TRsakey;
    procedure SetAsRsassaPss(const Value: TRsakey);
    function GetAsNR: TIntegerWrapper;
    procedure SetAsNR(const Value: TIntegerWrapper);
    function GetAsRW: TRsakey;
    procedure SetAsRW(const Value: TRsakey);
    function GetAsEcNr: TBitString;
    procedure SetAsEcNr(const Value: TBitString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsRsaEncryption: TRsakey read GetAsRsaEncryption write SetAsRsaEncryption;
    property AsDhpublicnumber: TIntegerWrapper read GetAsDhpublicnumber write SetAsDhpublicnumber;
    property AsDsa: TIntegerWrapper read GetAsDsa write SetAsDsa;
    property AsNR: TIntegerWrapper read GetAsNR write SetAsNR;
    property AsEcPublicKey: TBitString read GetAsEcPublicKey write SetAsEcPublicKey;
    property AsRsaesOaep: TRsakey read GetAsRsaesOaep write SetAsRsaesOaep;
    property AsRsassaPss: TRsakey read GetAsRsassaPss write SetAsRsassaPss;
    property AsRW: TRsakey read GetAsRW write SetAsRW;
    property AsEcNr: TBitString read GetAsEcNr write SetAsEcNr;
  published
    property Choice: TEncapsulatedPublicKeyEnum read GetChoice write SetChoice;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TSubjectPublicKeyInfo}

  IReadOnlyPublicKeyInfo = interface
    ['{FB92AB94-509E-4507-9780-DF4F7754F42F}']
    function GetAlgorithm: TAlgorithmIdentifier;
    function ExtractSubjectRSAPublicKey(var RSAKey: TIFPublicKey): Boolean;
    function ExtractSubjectRSAES_OAEPPublicKey(var RSAKey: TIFPublicKey;
                                               var HA: THashAlgorithm;
                                               var MGFHA: THashAlgorithm;
                                               var P: string): Boolean;
    function ExtractSubjectDHPublicKey(var DHKey: TDLPublicKey): Boolean;
    function ExtractSubjectDSAPublicKey(var DSAKey: TDLPublicKey): Boolean;
    function ExtractSubjectECPublicKey(var ECKey: TECPublicKey): Boolean;
    function PublicKeyIdentifier: string;
  end;

  IWriteOnlyPublicKeyInfo = interface
    ['{9FED1AF5-D786-4DEB-85FD-60023636F907}']
    procedure ImposeSubjectRSAPublicKey(const RSAKey: TIFPublicKey);
    procedure ImposeSubjectRSAES_OAEPPublicKey(const RSAKey: TIFPublicKey;
                                               HA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               MGFHA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               P: OctetString = '');
    procedure ImposeSubjectDHPublicKey(const DHKey: TDLPublicKey);
    procedure ImposeSubjectDSAPublicKey(const DSAKey: TDLPublicKey);
    procedure ImposeSubjectECPublicKey(var ECKey: TECPublicKey);
  end;

  IPublicKeyInfo = interface
    ['{3BA9B41E-ECF0-4655-9D24-62038E2136D5}']
    function GetAlgorithm: TAlgorithmIdentifier;
    function ExtractSubjectRSAPublicKey(var RSAKey: TIFPublicKey): Boolean;
    function ExtractSubjectRSAES_OAEPPublicKey(var RSAKey: TIFPublicKey;
                                               var HA: THashAlgorithm;
                                               var MGFHA: THashAlgorithm;
                                               var P: string): Boolean;
    function ExtractSubjectDHPublicKey(var DHKey: TDLPublicKey): Boolean;
    function ExtractSubjectDSAPublicKey(var DSAKey: TDLPublicKey): Boolean;
    function ExtractSubjectECPublicKey(var ECKey: TECPublicKey): Boolean;
    function PublicKeyIdentifier: string;
    procedure ImposeSubjectRSAPublicKey(const RSAKey: TIFPublicKey);
    procedure ImposeSubjectRSAES_OAEPPublicKey(const RSAKey: TIFPublicKey;
                                               HA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               MGFHA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               P: OctetString = '');
    procedure ImposeSubjectDHPublicKey(const DHKey: TDLPublicKey);
    procedure ImposeSubjectDSAPublicKey(const DSAKey: TDLPublicKey);
    procedure ImposeSubjectECPublicKey(var ECKey: TECPublicKey);
  end;


  TSubjectPublicKeyInfo = class(TASNConstructedWrapper,
                                IReadOnlyPublicKeyInfo,
                                IWriteOnlyPublicKeyInfo,
                                IPublicKeyInfo,
                                IPKPublicKeyInfo,              // From MPPK
                                IDLPublicKeyInfo,              // From MPDL
                                IIFPublicKeyInfo,              // From MPIF
                                IECPublicKeyInfo)              // From MPEC
  private
    procedure SetAlgorithm(const Value: TAlgorithmIdentifier);
    function GetSubjectPublicKey: TEncapsulatedPublicKey;
    procedure SetSubjectPublicKey(const Value: TEncapsulatedPublicKey);
  protected
    function AlgorithmIdentifier: string;
    function GetAlgorithm: TAlgorithmIdentifier;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    constructor CreateFromCert(ACert: TASN1Struct);
    destructor Destroy; override;
    function IsType(Id: TEncapsulatedPublicKeyEnum): Boolean;
    procedure SetType(Id: TEncapsulatedPublicKeyEnum);
    function ExtractSubjectRSAPublicKey(var RSAKey: TIFPublicKey): Boolean;
    function ExtractSubjectRSAES_OAEPPublicKey(var RSAKey: TIFPublicKey;
                                               var HA: THashAlgorithm;
                                               var MGFHA: THashAlgorithm;
                                               var P: string): Boolean;
    function ExtractSubjectDSAPublicKey(var DSAKey: TDLPublicKey): Boolean;
    function ExtractSubjectDHPublicKey(var DHKey: TDLPublicKey): Boolean;   
    function ExtractSubjectNRPublicKey(var NRKey: TDLPublicKey): Boolean;
    function ExtractSubjectECPublicKey(var ECKey: TECPublicKey): Boolean;
    procedure ImposeSubjectRSAPublicKey(const RSAKey: TIFPublicKey);
    procedure ImposeSubjectRSAES_OAEPPublicKey(const RSAKey: TIFPublicKey;
                                               HA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               MGFHA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               P: OctetString = '');
    procedure ImposeSubjectDSAPublicKey(const DSAKey: TDLPublicKey);
    procedure ImposeSubjectDHPublicKey(const DHKey: TDLPublicKey);
    procedure ImposeSubjectNRPublicKey(const NRKey: TDLPublicKey);
    procedure ImposeSubjectECPublicKey(var ECKey: TECPublicKey);
    function PublicKeyIdentifier: string;
    function PublicKeyIdentifier2: string;
  published
    property Algorithm: TAlgorithmIdentifier read GetAlgorithm write SetAlgorithm;
    property SubjectPublicKey: TEncapsulatedPublicKey read GetSubjectPublicKey write SetSubjectPublicKey;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TAnotherName
             TAny}

  TAny = class(TASNConstructedWrapper)
  private
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TAnotherName}

  TAnotherName = class(TASNConstructedWrapper)
  private
    function GetType_Id: ObjectIdentifier;
    procedure SetType_Id(const Value: ObjectIdentifier);
    function GetValue: TAny;
    procedure SetValue(const Value: TAny);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Type_Id: ObjectIdentifier read GetType_Id write SetType_Id;
    property Value: TAny read GetValue write SetValue;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TBuiltInStandardAttributes
              TCountryName}

  TCountryNameEnum = (
    cneUndefined, cneX121DccCode, cneIso3166Alpha2Code);

  TCountryName = class(TASNChoiceWrapper)
  private
    function GetAsX121_Dcc_Code: NumericString;
    procedure SetAsX121_Dcc_Code(const Value: NumericString);
    function GetAsIso_3166_Alpha2_Code: PrintableString;
    procedure SetAsIso_3166_Alpha2_Code(const Value: PrintableString);
    function GetChoice: TCountryNameEnum;
    procedure SetChoice(const Value: TCountryNameEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsX121_Dcc_Code: NumericString read GetAsX121_Dcc_Code write SetAsX121_Dcc_Code;
    property AsIso_3166_Alpha2_Code: PrintableString read GetAsIso_3166_Alpha2_Code write SetAsIso_3166_Alpha2_Code;
  published
    property Choice: TCountryNameEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TBuiltInStandardAttributes
              TAdministrationDomainName}

  TAdministrationDomainNameEnum = (
    adneUndefined, adneNumeric, adnePrintable);

  TAdministrationDomainName = class(TASNChoiceWrapper)
  private
    function GetAsNumeric: NumericString;
    procedure SetAsNumeric(const Value: NumericString);
    function GetAsPrintable: PrintableString;
    procedure SetAsPrintable(const Value: PrintableString);
    function GetChoice: TAdministrationDomainNameEnum;
    procedure SetChoice(const Value: TAdministrationDomainNameEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsNumeric: NumericString read GetAsNumeric write SetAsNumeric;
    property AsPrintable: PrintableString read GetAsPrintable write SetAsPrintable;
  published
    property Choice: TAdministrationDomainNameEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TBuiltInStandardAttributes
              TPrivateDomainName}

  TPrivateDomainNameEnum = (
    pdneUndefined, pdneNumeric, pdnePrintable);

  TPrivateDomainName = class(TASNChoiceWrapper)
  private
    function GetAsNumeric: NumericString;
    procedure SetAsNumeric(const Value: NumericString);
    function GetAsPrintable: PrintableString;
    procedure SetAsPrintable(const Value: PrintableString);
    function GetChoice: TPrivateDomainNameEnum;
    procedure SetChoice(const Value: TPrivateDomainNameEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsNumeric: NumericString read GetAsNumeric write SetAsNumeric;
    property AsPrintable: PrintableString read GetAsPrintable write SetAsPrintable;
  published
    property Choice: TPrivateDomainNameEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TBuiltInStandardAttributes
              TPersonalName}

  TPersonalName = class(TASNConstructedWrapper)
  private
    function GetSurname: PrintableString;
    procedure SetSurname(const Value: PrintableString);
    function GetGiven_Name: PrintableString;
    procedure SetGiven_Name(const Value: PrintableString);
    function GetInitials: PrintableString;
    procedure SetInitials(const Value: PrintableString);
    function GetGeneration_Qualifier: PrintableString;
    procedure SetGeneration_Qualifier(const Value: PrintableString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Surname: PrintableString read GetSurname write SetSurname;
    property Given_Name: PrintableString read GetGiven_Name write SetGiven_Name;
    property Initials: PrintableString read GetInitials write SetInitials;
    property Generation_Qualifier: PrintableString read GetGeneration_Qualifier write SetGeneration_Qualifier;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TBuiltInStandardAttributes
              TSequenceOfPrintableString}

  TSequenceOfPrintableString = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TStringWrapper;
    function GetValues(Index: Integer): PrintableString;
    procedure SetValues(Index: Integer; const Value: PrintableString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TStringWrapper;
    procedure AddValue(const Value: PrintableString);
    property Values[Index: Integer]: PrintableString read GetValues write SetValues; default;
    property Items[Index: Integer]: TStringWrapper read GetItems;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TBuiltInStandardAttributes}

  TBuiltInStandardAttributes = class(TASNConstructedWrapper)
  private
    function GetCountry_Name: TCountryName;
    procedure SetCountry_Name(const Value: TCountryName);
    function GetAdministration_Domain_Name: TAdministrationDomainName;
    procedure SetAdministration_Domain_Name(const Value: TAdministrationDomainName);
    function GetNetwork_Address: NumericString;
    procedure SetNetwork_Address(const Value: NumericString);
    function GetTerminal_Identifier: PrintableString;
    procedure SetTerminal_Identifier(const Value: PrintableString);
    function GetPrivate_Domain_Name: TPrivateDomainName;
    procedure SetPrivate_Domain_Name(const Value: TPrivateDomainName);
    function GetOrganization_Name: PrintableString;
    procedure SetOrganization_Name(const Value: PrintableString);
    function GetNumeric_User_Identifier: NumericString;
    procedure SetNumeric_User_Identifier(const Value: NumericString);
    function GetPersonal_Name: TPersonalName;
    procedure SetPersonal_Name(const Value: TPersonalName);
    function GetOrganizational_Unit_Names: TSequenceOfPrintableString;
    procedure SetOrganizational_Unit_Names(const Value: TSequenceOfPrintableString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Country_Name: TCountryName read GetCountry_Name write SetCountry_Name;
    property Administration_Domain_Name: TAdministrationDomainName read GetAdministration_Domain_Name write SetAdministration_Domain_Name;
    property Network_Address: NumericString read GetNetwork_Address write SetNetwork_Address;
    property Terminal_Identifier: PrintableString read GetTerminal_Identifier write SetTerminal_Identifier;
    property Private_Domain_Name: TPrivateDomainName read GetPrivate_Domain_Name write SetPrivate_Domain_Name;
    property Organization_Name: PrintableString read GetOrganization_Name write SetOrganization_Name;
    property Numeric_User_Identifier: NumericString read GetNumeric_User_Identifier write SetNumeric_User_Identifier;
    property Personal_Name: TPersonalName read GetPersonal_Name write SetPersonal_Name;
    property Organizational_Unit_Names: TSequenceOfPrintableString read GetOrganizational_Unit_Names write SetOrganizational_Unit_Names;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TBuiltInDomainDefinedAttributes
              TBuiltInDomainDefinedAttribute}

  TBuiltInDomainDefinedAttribute = class(TASNConstructedWrapper)
  private
    function GetTypeName: PrintableString;
    procedure SetTypeName(const Value: PrintableString);
    function GetValue: PrintableString;
    procedure SetValue(const Value: PrintableString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property TypeName: PrintableString read GetTypeName write SetTypeName;
    property Value: PrintableString read GetValue write SetValue;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TBuiltInDomainDefinedAttributes}

  TBuiltInDomainDefinedAttributes = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TBuiltInDomainDefinedAttribute;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TBuiltInDomainDefinedAttribute;
    property Items[Index: Integer]: TBuiltInDomainDefinedAttribute read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TTeletexPersonalName}

  TTeletexPersonalName = class(TASNConstructedWrapper)
  private
    function GetSurname: TeletexString;
    procedure SetSurname(const Value: TeletexString);
    function GetGiven_Name: TeletexString;
    procedure SetGiven_Name(const Value: TeletexString);
    function GetInitials: TeletexString;
    procedure SetInitials(const Value: TeletexString);
    function GetGeneration_Qualifier: TeletexString;
    procedure SetGeneration_Qualifier(const Value: TeletexString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Surname: TeletexString read GetSurname write SetSurname;
    property Given_Name: TeletexString read GetGiven_Name write SetGiven_Name;
    property Initials: TeletexString read GetInitials write SetInitials;
    property Generation_Qualifier: TeletexString read GetGeneration_Qualifier write SetGeneration_Qualifier;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TTeletexOrganizationalUnitNames}

  TTeletexOrganizationalUnitNames = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TStringWrapper;
    function GetValues(Index: Integer): TeletexString;
    procedure SetValues(Index: Integer; const Value: TeletexString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TStringWrapper;
    procedure AddValue(const Value: TeletexString);
    property Values[Index: Integer]: TeletexString read GetValues write SetValues; default;
    property Items[Index: Integer]: TStringWrapper read GetItems;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TTeletexDomainDefinedAttributes
                 TTeletexDomainDefinedAttribute}

  TTeletexDomainDefinedAttribute = class(TASNConstructedWrapper)
  private
    function GetTypeName: TeletexString;
    procedure SetTypeName(const Value: TeletexString);
    function GetValue: TeletexString;
    procedure SetValue(const Value: TeletexString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property TypeName: TeletexString read GetTypeName write SetTypeName;
    property Value: TeletexString read GetValue write SetValue;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TTeletexDomainDefinedAttributes}

  TTeletexDomainDefinedAttributes = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TTeletexDomainDefinedAttribute;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TTeletexDomainDefinedAttribute;
    property Items[Index: Integer]: TTeletexDomainDefinedAttribute read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TPhysicalDeliveryCountryName}

  TPhysicalDeliveryCountryNameEnum = (
    pdcneUndefined, pdcneX121DccCode, pdcneIso3166Alpha2Code);

  TPhysicalDeliveryCountryName = class(TASNChoiceWrapper)
  private
    function GetAsX121_Dcc_Code: NumericString;
    procedure SetAsX121_Dcc_Code(const Value: NumericString);
    function GetAsIso_3166_Alpha2_Code: PrintableString;
    procedure SetAsIso_3166_Alpha2_Code(const Value: PrintableString);
    function GetChoice: TPhysicalDeliveryCountryNameEnum;
    procedure SetChoice(const Value: TPhysicalDeliveryCountryNameEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsX121_Dcc_Code: NumericString read GetAsX121_Dcc_Code write SetAsX121_Dcc_Code;
    property AsIso_3166_Alpha2_Code: PrintableString read GetAsIso_3166_Alpha2_Code write SetAsIso_3166_Alpha2_Code;
  published
    property Choice: TPhysicalDeliveryCountryNameEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TPostalCode}

  TPostalCodeEnum = (
    pceUndefined, pceNumericCode, pcePrintableCode);

  TPostalCode = class(TASNChoiceWrapper)
  private
    function GetAsNumeric_Code: NumericString;
    procedure SetAsNumeric_Code(const Value: NumericString);
    function GetAsPrintable_Code: PrintableString;
    procedure SetAsPrintable_Code(const Value: PrintableString);
    function GetChoice: TPostalCodeEnum;
    procedure SetChoice(const Value: TPostalCodeEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsNumeric_Code: NumericString read GetAsNumeric_Code write SetAsNumeric_Code;
    property AsPrintable_Code: PrintableString read GetAsPrintable_Code write SetAsPrintable_Code;
  published
    property Choice: TPostalCodeEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TPdsparameter}

  TPdsparameter = class(TASNConstructedWrapper)
  private
    function GetPrintable_String: PrintableString;
    procedure SetPrintable_String(const Value: PrintableString);
    function GetTeletex_String: TeletexString;
    procedure SetTeletex_String(const Value: TeletexString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Printable_String: PrintableString read GetPrintable_String write SetPrintable_String;
    property Teletex_String: TeletexString read GetTeletex_String write SetTeletex_String;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TUnformattedPostalAddress}

  TUnformattedPostalAddress = class(TASNConstructedWrapper)
  private
    function GetPrintable_Address: TSequenceOfPrintableString;
    procedure SetPrintable_Address(const Value: TSequenceOfPrintableString);
    function GetTeletex_String: TeletexString;
    procedure SetTeletex_String(const Value: TeletexString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Printable_Address: TSequenceOfPrintableString read GetPrintable_Address write SetPrintable_Address;
    property Teletex_String: TeletexString read GetTeletex_String write SetTeletex_String;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TExtendedNetworkAddress
                 TE163_4Address}

  TE163_4Address = class(TASNConstructedWrapper)
  private
    function GetNumber: NumericString;
    procedure SetNumber(const Value: NumericString);
    function GetSub_Address: NumericString;
    procedure SetSub_Address(const Value: NumericString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Number: NumericString read GetNumber write SetNumber;
    property Sub_Address: NumericString read GetSub_Address write SetSub_Address;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TExtendedNetworkAddress
                 TPresentationAddress
                  TSetOfOctetString}

  TSetOfOctetString = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TOctetString;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TOctetString;
    property Items[Index: Integer]: TOctetString read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TExtendedNetworkAddress
                 TPresentationAddress}

  TPresentationAddress = class(TASNConstructedWrapper)
  private
    function GetPSelector: TOctetString;
    procedure SetPSelector(const Value: TOctetString);
    function GetSSelector: TOctetString;
    procedure SetSSelector(const Value: TOctetString);
    function GetTSelector: TOctetString;
    procedure SetTSelector(const Value: TOctetString);
    function GetNAddresses: TSetOfOctetString;
    procedure SetNAddresses(const Value: TSetOfOctetString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property PSelector: TOctetString read GetPSelector write SetPSelector;
    property SSelector: TOctetString read GetSSelector write SetSSelector;
    property TSelector: TOctetString read GetTSelector write SetTSelector;
    property NAddresses: TSetOfOctetString read GetNAddresses write SetNAddresses;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr
                TExtendedNetworkAddress}

  TExtendedNetworkAddressEnum = (
    enaeUndefined, enaeE1634Address, enaePsapAddress);

  TExtendedNetworkAddress = class(TASNChoiceWrapper)
  private
    function GetAsE163_4_Address: TE163_4Address;
    procedure SetAsE163_4_Address(const Value: TE163_4Address);
    function GetAsPsap_Address: TPresentationAddress;
    procedure SetAsPsap_Address(const Value: TPresentationAddress);
    function GetChoice: TExtendedNetworkAddressEnum;
    procedure SetChoice(const Value: TExtendedNetworkAddressEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsE163_4_Address: TE163_4Address read GetAsE163_4_Address write SetAsE163_4_Address;
    property AsPsap_Address: TPresentationAddress read GetAsPsap_Address write SetAsPsap_Address;
  published
    property Choice: TExtendedNetworkAddressEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute
               TExtnAttr}

  TExtnAttrEnum = (
    exaeUndefined, exaeCommonName, exaeTeletexCommonName,
    exaeTeletexOrganizationName, exaeTeletexPersonalName,
    exaeTeletexOrganizationalUnitNames, exaeTeletexDomainDefinedAttributes,
    exaePDSName, exaePhysicalDeliveryCountryName, exaePostalCode,
    exaePhysicalDeliveryOfficeName, exaePhysicalDeliveryOfficeNumber,
    exaeExtensionORAddressComponents, exaePhysicalDeliveryPersonalName,
    exaePhysicalDeliveryOrganizationName,
    exaeExtensionPhysicalDeliveryAddressComponents,
    exaeUnformattedPostalAddress, exaeStreetAddress, exaePostOfficeBoxAddress,
    exaePosteRestanteAddress, exaeUniquePostalName, exaeLocalPostalAttributes,
    exaeExtendedNetworkAddress, exaeTerminalType);

  TExtnAttr = class(TASNChoiceWrapper)
  private
    function GetAsCommonName: PrintableString;
    procedure SetAsCommonName(const Value: PrintableString);
    function GetAsTeletexCommonName: TeletexString;
    procedure SetAsTeletexCommonName(const Value: TeletexString);
    function GetAsTeletexOrganizationName: TeletexString;
    procedure SetAsTeletexOrganizationName(const Value: TeletexString);
    function GetAsTeletexPersonalName: TTeletexPersonalName;
    procedure SetAsTeletexPersonalName(const Value: TTeletexPersonalName);
    function GetAsTeletexOrganizationalUnitNames: TTeletexOrganizationalUnitNames;
    procedure SetAsTeletexOrganizationalUnitNames(const Value: TTeletexOrganizationalUnitNames);
    function GetAsTeletexDomainDefinedAttributes: TTeletexDomainDefinedAttributes;
    procedure SetAsTeletexDomainDefinedAttributes(const Value: TTeletexDomainDefinedAttributes);
    function GetAsPdsname: PrintableString;
    procedure SetAsPdsname(const Value: PrintableString);
    function GetAsPhysicalDeliveryCountryName: TPhysicalDeliveryCountryName;
    procedure SetAsPhysicalDeliveryCountryName(const Value: TPhysicalDeliveryCountryName);
    function GetAsPostalCode: TPostalCode;
    procedure SetAsPostalCode(const Value: TPostalCode);
    function GetAsPhysicalDeliveryOfficeName: TPdsparameter;
    procedure SetAsPhysicalDeliveryOfficeName(const Value: TPdsparameter);
    function GetAsPhysicalDeliveryOfficeNumber: TPdsparameter;
    procedure SetAsPhysicalDeliveryOfficeNumber(const Value: TPdsparameter);
    function GetAsExtensionORAddressComponents: TPdsparameter;
    procedure SetAsExtensionORAddressComponents(const Value: TPdsparameter);
    function GetAsPhysicalDeliveryPersonalName: TPdsparameter;
    procedure SetAsPhysicalDeliveryPersonalName(const Value: TPdsparameter);
    function GetAsPhysicalDeliveryOrganizationName: TPdsparameter;
    procedure SetAsPhysicalDeliveryOrganizationName(const Value: TPdsparameter);
    function GetAsExtensionPhysicalDeliveryAddressComponents: TPdsparameter;
    procedure SetAsExtensionPhysicalDeliveryAddressComponents(const Value: TPdsparameter);
    function GetAsUnformattedPostalAddress: TUnformattedPostalAddress;
    procedure SetAsUnformattedPostalAddress(const Value: TUnformattedPostalAddress);
    function GetAsStreetAddress: TPdsparameter;
    procedure SetAsStreetAddress(const Value: TPdsparameter);
    function GetAsPostOfficeBoxAddress: TPdsparameter;
    procedure SetAsPostOfficeBoxAddress(const Value: TPdsparameter);
    function GetAsPosteRestanteAddress: TPdsparameter;
    procedure SetAsPosteRestanteAddress(const Value: TPdsparameter);
    function GetAsUniquePostalName: TPdsparameter;
    procedure SetAsUniquePostalName(const Value: TPdsparameter);
    function GetAsLocalPostalAttributes: TPdsparameter;
    procedure SetAsLocalPostalAttributes(const Value: TPdsparameter);
    function GetAsExtendedNetworkAddress: TExtendedNetworkAddress;
    procedure SetAsExtendedNetworkAddress(const Value: TExtendedNetworkAddress);
    function GetAsTerminalType: TIntegerWrapper;
    procedure SetAsTerminalType(const Value: TIntegerWrapper);
    function GetChoice: TExtnAttrEnum;
    procedure SetChoice(const Value: TExtnAttrEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsCommonName: PrintableString read GetAsCommonName write SetAsCommonName;
    property AsTeletexCommonName: TeletexString read GetAsTeletexCommonName write SetAsTeletexCommonName;
    property AsTeletexOrganizationName: TeletexString read GetAsTeletexOrganizationName write SetAsTeletexOrganizationName;
    property AsTeletexPersonalName: TTeletexPersonalName read GetAsTeletexPersonalName write SetAsTeletexPersonalName;
    property AsTeletexOrganizationalUnitNames: TTeletexOrganizationalUnitNames read GetAsTeletexOrganizationalUnitNames write SetAsTeletexOrganizationalUnitNames;
    property AsTeletexDomainDefinedAttributes: TTeletexDomainDefinedAttributes read GetAsTeletexDomainDefinedAttributes write SetAsTeletexDomainDefinedAttributes;
    property AsPdsname: PrintableString read GetAsPdsname write SetAsPdsname;
    property AsPhysicalDeliveryCountryName: TPhysicalDeliveryCountryName read GetAsPhysicalDeliveryCountryName write SetAsPhysicalDeliveryCountryName;
    property AsPostalCode: TPostalCode read GetAsPostalCode write SetAsPostalCode;
    property AsPhysicalDeliveryOfficeName: TPdsparameter read GetAsPhysicalDeliveryOfficeName write SetAsPhysicalDeliveryOfficeName;
    property AsPhysicalDeliveryOfficeNumber: TPdsparameter read GetAsPhysicalDeliveryOfficeNumber write SetAsPhysicalDeliveryOfficeNumber;
    property AsExtensionORAddressComponents: TPdsparameter read GetAsExtensionORAddressComponents write SetAsExtensionORAddressComponents;
    property AsPhysicalDeliveryPersonalName: TPdsparameter read GetAsPhysicalDeliveryPersonalName write SetAsPhysicalDeliveryPersonalName;
    property AsPhysicalDeliveryOrganizationName: TPdsparameter read GetAsPhysicalDeliveryOrganizationName write SetAsPhysicalDeliveryOrganizationName;
    property AsExtensionPhysicalDeliveryAddressComponents: TPdsparameter read GetAsExtensionPhysicalDeliveryAddressComponents write SetAsExtensionPhysicalDeliveryAddressComponents;
    property AsUnformattedPostalAddress: TUnformattedPostalAddress read GetAsUnformattedPostalAddress write SetAsUnformattedPostalAddress;
    property AsStreetAddress: TPdsparameter read GetAsStreetAddress write SetAsStreetAddress;
    property AsPostOfficeBoxAddress: TPdsparameter read GetAsPostOfficeBoxAddress write SetAsPostOfficeBoxAddress;
    property AsPosteRestanteAddress: TPdsparameter read GetAsPosteRestanteAddress write SetAsPosteRestanteAddress;
    property AsUniquePostalName: TPdsparameter read GetAsUniquePostalName write SetAsUniquePostalName;
    property AsLocalPostalAttributes: TPdsparameter read GetAsLocalPostalAttributes write SetAsLocalPostalAttributes;
    property AsExtendedNetworkAddress: TExtendedNetworkAddress read GetAsExtendedNetworkAddress write SetAsExtendedNetworkAddress;
    property AsTerminalType: TIntegerWrapper read GetAsTerminalType write SetAsTerminalType;
  published
    property Choice: TExtnAttrEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes
              TExtensionAttribute}

  TExtensionAttribute = class(TASNConstructedWrapper)
  private
    function GetExtension_Attribute_Type: TIntegerWrapper;
    procedure SetExtension_Attribute_Type(const Value: TIntegerWrapper);
    function GetExtension_Attribute_Value: TExtnAttr;
    procedure SetExtension_Attribute_Value(const Value: TExtnAttr);
    function GetTypeName: string;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TExtnAttrEnum): Boolean;
    procedure SetType(Id: TExtnAttrEnum);
    property TypeName: string read GetTypeName;
  published
    property Extension_Attribute_Type: TIntegerWrapper read GetExtension_Attribute_Type write SetExtension_Attribute_Type;
    property Extension_Attribute_Value: TExtnAttr read GetExtension_Attribute_Value write SetExtension_Attribute_Value;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress
             TExtensionAttributes}

  TExtensionAttributes = class(TASNCustomOFFieldWrapper)
  private
    function GetUniqueItem(Id: TExtnAttrEnum): TExtensionAttribute;
    function GetItems(Index: Integer): TExtensionAttribute;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function AddUniqueItem(Id: TExtnAttrEnum): TExtensionAttribute;
    function Add: TExtensionAttribute;
    property Items[Index: Integer]: TExtensionAttribute read GetItems;
    property UniqueItem[Id: TExtnAttrEnum]: TExtensionAttribute read GetUniqueItem; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TOraddress}

  TOraddress = class(TASNConstructedWrapper)
  private
    function GetBuilt_In_Standard_Attributes: TBuiltInStandardAttributes;
    procedure SetBuilt_In_Standard_Attributes(const Value: TBuiltInStandardAttributes);
    function GetBuilt_In_Domain_Defined_Attributes: TBuiltInDomainDefinedAttributes;
    procedure SetBuilt_In_Domain_Defined_Attributes(const Value: TBuiltInDomainDefinedAttributes);
    function GetExtension_Attributes: TExtensionAttributes;
    procedure SetExtension_Attributes(const Value: TExtensionAttributes);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Built_In_Standard_Attributes: TBuiltInStandardAttributes read GetBuilt_In_Standard_Attributes write SetBuilt_In_Standard_Attributes;
    property Built_In_Domain_Defined_Attributes: TBuiltInDomainDefinedAttributes read GetBuilt_In_Domain_Defined_Attributes write SetBuilt_In_Domain_Defined_Attributes;
    property Extension_Attributes: TExtensionAttributes read GetExtension_Attributes write SetExtension_Attributes;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName
            TEdipartyName}

  TEdipartyName = class(TASNConstructedWrapper)
  private
    function GetNameAssigner: TDirectoryString;
    procedure SetNameAssigner(const Value: TDirectoryString);
    function GetPartyName: TDirectoryString;
    procedure SetPartyName(const Value: TDirectoryString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property NameAssigner: TDirectoryString read GetNameAssigner write SetNameAssigner;
    property PartyName: TDirectoryString read GetPartyName write SetPartyName;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames
           TGeneralName}

  TGeneralNameEnum = (
    gneUndefined, gneOtherName, gneRfc822Name, gneDNSName, gneX400Address,
    gneDirectoryName, gneEdiPartyName, gneUniformResourceIdentifier,
    gneIPAddress, gneRegisteredID);

  TGeneralName = class(TASNChoiceWrapper)
  private
    function GetAsOtherName: TAnotherName;
    procedure SetAsOtherName(const Value: TAnotherName);
    function GetAsRfc822Name: IA5String;
    procedure SetAsRfc822Name(const Value: IA5String);
    function GetAsDNSName: IA5String;
    procedure SetAsDNSName(const Value: IA5String);
    function GetAsX400Address: TOraddress;
    procedure SetAsX400Address(const Value: TOraddress);
    function GetAsDirectoryName: TName;
    procedure SetAsDirectoryName(const Value: TName);
    function GetAsEdiPartyName: TEdipartyName;
    procedure SetAsEdiPartyName(const Value: TEdipartyName);
    function GetAsUniformResourceIdentifier: IA5String;
    procedure SetAsUniformResourceIdentifier(const Value: IA5String);
    function GetAsIPAddress: TOctetString;
    procedure SetAsIPAddress(const Value: TOctetString);
    function GetAsRegisteredID: ObjectIdentifier;
    procedure SetAsRegisteredID(const Value: ObjectIdentifier);
    function GetChoice: TGeneralNameEnum;
    procedure SetChoice(const Value: TGeneralNameEnum);
    function GetText: WideString;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsOtherName: TAnotherName read GetAsOtherName write SetAsOtherName;
    property AsRfc822Name: IA5String read GetAsRfc822Name write SetAsRfc822Name;
    property AsDNSName: IA5String read GetAsDNSName write SetAsDNSName;
    property AsX400Address: TOraddress read GetAsX400Address write SetAsX400Address;
    property AsDirectoryName: TName read GetAsDirectoryName write SetAsDirectoryName;
    property AsEdiPartyName: TEdipartyName read GetAsEdiPartyName write SetAsEdiPartyName;
    property AsUniformResourceIdentifier: IA5String read GetAsUniformResourceIdentifier write SetAsUniformResourceIdentifier;
    property AsIPAddress: TOctetString read GetAsIPAddress write SetAsIPAddress;
    property AsRegisteredID: ObjectIdentifier read GetAsRegisteredID write SetAsRegisteredID;
    property Text: WideString read GetText;
  published
    property Choice: TGeneralNameEnum read GetChoice write SetChoice;
  end;

{ Declaration tree:
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier
          TGeneralNames}

  TGeneralNames = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TGeneralName;
    function GetText: WideString;
    function GetURI: string;
    procedure SetURI(const Value: string);
    function GetDnsName: string;
    function GetIPAddress: string;
    function GetRfc822Name: string;
    procedure SetDnsName(const Value: string);
    procedure SetIPAddress(const Value: string);
    procedure SetRfc822Name(const Value: string);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TGeneralName;
    function Compare(AltName: TGeneralNames): Boolean;
    property Items[Index: Integer]: TGeneralName read GetItems; default;
    property Text: WideString read GetText;
    property URI: string read GetURI write SetURI;
    property Rfc822Name: string read GetRfc822Name write SetRfc822Name;
    property DnsName: string read GetDnsName write SetDnsName;
    property IPAddress: string read GetIPAddress write SetIPAddress;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityKeyIdentifier}

  TAuthorityKeyIdentifier = class(TASNConstructedWrapper)
  private
    function GetKeyIdentifier: TOctetString;
    procedure SetKeyIdentifier(const Value: TOctetString);
    function GetAuthorityCertIssuer: TGeneralNames;
    procedure SetAuthorityCertIssuer(const Value: TGeneralNames);
    function GetAuthorityCertSerialNumber: TIntegerWrapper;
    procedure SetAuthorityCertSerialNumber(const Value: TIntegerWrapper);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property KeyIdentifier: TOctetString read GetKeyIdentifier write SetKeyIdentifier;
    property AuthorityCertIssuer: TGeneralNames read GetAuthorityCertIssuer write SetAuthorityCertIssuer;
    property AuthorityCertSerialNumber: TIntegerWrapper read GetAuthorityCertSerialNumber write SetAuthorityCertSerialNumber;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TExtKeyUsageSyntax}

  TExtKeyUsageSyntax = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TStringWrapper;
    function GetValues(Index: Integer): ObjectIdentifier;
    procedure SetValues(Index: Integer; const Value: ObjectIdentifier);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TStringWrapper;
    procedure AddValue(const Value: ObjectIdentifier);
    property Values[Index: Integer]: ObjectIdentifier read GetValues write SetValues; default;
    property Items[Index: Integer]: TStringWrapper read GetItems;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TPrivateKeyUsagePeriod}

  TPrivateKeyUsagePeriod = class(TASNConstructedWrapper)
  private
    function GetNotBefore: TGeneralizedTime;
    procedure SetNotBefore(const Value: TGeneralizedTime);
    function GetNotAfter: TGeneralizedTime;
    procedure SetNotAfter(const Value: TGeneralizedTime);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property NotBefore: TGeneralizedTime read GetNotBefore write SetNotBefore;
    property NotAfter: TGeneralizedTime read GetNotAfter write SetNotAfter;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCertificatePolicies
          TPolicyInformation
           TSequenceOfPolicyQualifierInfo
            TPolicyQualifierInfo
             TQualifier
              TUserNotice
               TNoticeReference
                TDisplayText}

  TDisplayTextEnum = (
    dteUndefined, dteVisibleString, dteBmpString, dteUtf8String);

  TDisplayText = class(TASNChoiceWrapper)
  private
    function GetAsVisibleString: VisibleString;
    procedure SetAsVisibleString(const Value: VisibleString);
    function GetAsBmpString: WideString;
    procedure SetAsBmpString(const Value: WideString);
    function GetAsUtf8String: WideString;
    procedure SetAsUtf8String(const Value: WideString);
    function GetChoice: TDisplayTextEnum;
    procedure SetChoice(const Value: TDisplayTextEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsVisibleString: VisibleString read GetAsVisibleString write SetAsVisibleString;
    property AsBmpString: WideString read GetAsBmpString write SetAsBmpString;
    property AsUtf8String: WideString read GetAsUtf8String write SetAsUtf8String;
  published
    property Choice: TDisplayTextEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCertificatePolicies
          TPolicyInformation
           TSequenceOfPolicyQualifierInfo
            TPolicyQualifierInfo
             TQualifier
              TUserNotice
               TNoticeReference
                TSequenceOfInteger}

  TSequenceOfInteger = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TIntegerWrapper;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TIntegerWrapper;
    property Items[Index: Integer]: TIntegerWrapper read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCertificatePolicies
          TPolicyInformation
           TSequenceOfPolicyQualifierInfo
            TPolicyQualifierInfo
             TQualifier
              TUserNotice
               TNoticeReference}

  TNoticeReference = class(TASNConstructedWrapper)
  private
    function GetOrganization: TDisplayText;
    procedure SetOrganization(const Value: TDisplayText);
    function GetNoticeNumbers: TSequenceOfInteger;
    procedure SetNoticeNumbers(const Value: TSequenceOfInteger);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Organization: TDisplayText read GetOrganization write SetOrganization;
    property NoticeNumbers: TSequenceOfInteger read GetNoticeNumbers write SetNoticeNumbers;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCertificatePolicies
          TPolicyInformation
           TSequenceOfPolicyQualifierInfo
            TPolicyQualifierInfo
             TQualifier
              TUserNotice}

  TUserNotice = class(TASNConstructedWrapper)
  private
    function GetNoticeRef: TNoticeReference;
    procedure SetNoticeRef(const Value: TNoticeReference);
    function GetExplicitText: TDisplayText;
    procedure SetExplicitText(const Value: TDisplayText);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property NoticeRef: TNoticeReference read GetNoticeRef write SetNoticeRef;
    property ExplicitText: TDisplayText read GetExplicitText write SetExplicitText;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCertificatePolicies
          TPolicyInformation
           TSequenceOfPolicyQualifierInfo
            TPolicyQualifierInfo
             TQualifier}

  TQualifierEnum = (
    qeUndefined, qeIdQtCps, qeIdQtUnotice);

  TQualifier = class(TASNChoiceWrapper)
  private
    function GetAsQt_Cps: IA5String;
    procedure SetAsQt_Cps(const Value: IA5String);
    function GetAsQt_Unotice: TUserNotice;
    procedure SetAsQt_Unotice(const Value: TUserNotice);
    function GetChoice: TQualifierEnum;
    procedure SetChoice(const Value: TQualifierEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsQt_Cps: IA5String read GetAsQt_Cps write SetAsQt_Cps;
    property AsQt_Unotice: TUserNotice read GetAsQt_Unotice write SetAsQt_Unotice;
  published
    property Choice: TQualifierEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCertificatePolicies
          TPolicyInformation
           TSequenceOfPolicyQualifierInfo
            TPolicyQualifierInfo}

  TPolicyQualifierInfo = class(TASNConstructedWrapper)
  private
    function GetPolicyQualifierId: ObjectIdentifier;
    procedure SetPolicyQualifierId(const Value: ObjectIdentifier);
    function GetQualifier: TQualifier;
    procedure SetQualifier(const Value: TQualifier);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TQualifierEnum): Boolean;
    procedure SetType(Id: TQualifierEnum);
  published
    property PolicyQualifierId: ObjectIdentifier read GetPolicyQualifierId write SetPolicyQualifierId;
    property Qualifier: TQualifier read GetQualifier write SetQualifier;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCertificatePolicies
          TPolicyInformation
           TSequenceOfPolicyQualifierInfo}

  TSequenceOfPolicyQualifierInfo = class(TASNCustomOFFieldWrapper)
  private
    function GetUniqueItem(Id: TQualifierEnum): TPolicyQualifierInfo;
    function GetItems(Index: Integer): TPolicyQualifierInfo;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function AddUniqueItem(Id: TQualifierEnum): TPolicyQualifierInfo;
    function Add: TPolicyQualifierInfo;
    property Items[Index: Integer]: TPolicyQualifierInfo read GetItems;
    property UniqueItem[Id: TQualifierEnum]: TPolicyQualifierInfo read GetUniqueItem; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCertificatePolicies
          TPolicyInformation}

  TPolicyInformation = class(TASNConstructedWrapper)
  private
    function GetPolicyIdentifier: ObjectIdentifier;
    procedure SetPolicyIdentifier(const Value: ObjectIdentifier);
    function GetPolicyQualifiers: TSequenceOfPolicyQualifierInfo;
    procedure SetPolicyQualifiers(const Value: TSequenceOfPolicyQualifierInfo);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property PolicyIdentifier: ObjectIdentifier read GetPolicyIdentifier write SetPolicyIdentifier;
    property PolicyQualifiers: TSequenceOfPolicyQualifierInfo read GetPolicyQualifiers write SetPolicyQualifiers;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCertificatePolicies}

  TCertificatePolicies = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TPolicyInformation;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TPolicyInformation;
    property Items[Index: Integer]: TPolicyInformation read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TPolicyMappingsSyntax
          TPolicyMapping}

  TPolicyMapping = class(TASNConstructedWrapper)
  private
    function GetIssuerDomainPolicy: ObjectIdentifier;
    procedure SetIssuerDomainPolicy(const Value: ObjectIdentifier);
    function GetSubjectDomainPolicy: ObjectIdentifier;
    procedure SetSubjectDomainPolicy(const Value: ObjectIdentifier);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property IssuerDomainPolicy: ObjectIdentifier read GetIssuerDomainPolicy write SetIssuerDomainPolicy;
    property SubjectDomainPolicy: ObjectIdentifier read GetSubjectDomainPolicy write SetSubjectDomainPolicy;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TPolicyMappingsSyntax}

  TPolicyMappingsSyntax = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TPolicyMapping;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TPolicyMapping;
    property Items[Index: Integer]: TPolicyMapping read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TBasicConstraintsSyntax}

  TBasicConstraintsSyntax = class(TASNConstructedWrapper)
  private
    function GetCA: Boolean;
    procedure SetCA(const Value: Boolean);
    function GetPathLenConstraint: TIntegerWrapper;
    procedure SetPathLenConstraint(const Value: TIntegerWrapper);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property CA: Boolean read GetCA write SetCA;
    property PathLenConstraint: TIntegerWrapper read GetPathLenConstraint write SetPathLenConstraint;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TNameConstraintsSyntax
          TGeneralSubtrees
           TGeneralSubtree}

  TGeneralSubtree = class(TASNConstructedWrapper)
  private
    function GetBase: TGeneralName;
    procedure SetBase(const Value: TGeneralName);
    function GetMinimum: TIntegerWrapper;
    procedure SetMinimum(const Value: TIntegerWrapper);
    function GetMaximum: TIntegerWrapper;
    procedure SetMaximum(const Value: TIntegerWrapper);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function AppliesBase(ABase: TGeneralName): Boolean;
    function Applies(Name: TName;
                     AltName: TGeneralNames;
                     out Missing: Boolean): Boolean;
  published
    property Base: TGeneralName read GetBase write SetBase;
    property Minimum: TIntegerWrapper read GetMinimum write SetMinimum;
    property Maximum: TIntegerWrapper read GetMaximum write SetMaximum;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TNameConstraintsSyntax
          TGeneralSubtrees}

  TGeneralSubtrees = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TGeneralSubtree;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TGeneralSubtree;
    procedure Difference(ADifference: TGeneralSubTrees);
    procedure Intersect(AIntersection: TGeneralSubTrees);
    procedure Union(AUnion: TGeneralSubTrees);
    property Items[Index: Integer]: TGeneralSubtree read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TNameConstraintsSyntax}

  TNameConstraintsSyntax = class(TASNConstructedWrapper)
  private
    function GetPermittedSubtrees: TGeneralSubtrees;
    procedure SetPermittedSubtrees(const Value: TGeneralSubtrees);
    function GetExcludedSubtrees: TGeneralSubtrees;
    procedure SetExcludedSubtrees(const Value: TGeneralSubtrees);
    function GetText: WideString;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    procedure Combine(ACombination: TNameConstraintsSyntax);
    procedure Compress(ACombination: TNameConstraintsSyntax);
    function Verify(Name: TName;
                    AltName: TGeneralNames): Boolean;
    property Text: WideString read GetText;
  published
    property PermittedSubtrees: TGeneralSubtrees read GetPermittedSubtrees write SetPermittedSubtrees;
    property ExcludedSubtrees: TGeneralSubtrees read GetExcludedSubtrees write SetExcludedSubtrees;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TPolicyConstraintsSyntax}

  TPolicyConstraintsSyntax = class(TASNConstructedWrapper)
  private
    function GetRequireExplicitPolicy: TIntegerWrapper;
    procedure SetRequireExplicitPolicy(const Value: TIntegerWrapper);
    function GetInhibitPolicyMapping: TIntegerWrapper;
    procedure SetInhibitPolicyMapping(const Value: TIntegerWrapper);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property RequireExplicitPolicy: TIntegerWrapper read GetRequireExplicitPolicy write SetRequireExplicitPolicy;
    property InhibitPolicyMapping: TIntegerWrapper read GetInhibitPolicyMapping write SetInhibitPolicyMapping;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCrldistributionPointsSyntax
          TDistributionPoint
           TDistributionPointName}

  TDistributionPointNameEnum = (
    dpneUndefined, dpneFullName, dpneNameRelativeToCRLIssuer);

  TDistributionPointName = class(TASNChoiceWrapper)
  private
    function GetAsFullName: TGeneralNames;
    procedure SetAsFullName(const Value: TGeneralNames);
    function GetAsNameRelativeToCRLIssuer: TRelativeDistinguishedName;
    procedure SetAsNameRelativeToCRLIssuer(const Value: TRelativeDistinguishedName);
    function GetChoice: TDistributionPointNameEnum;
    procedure SetChoice(const Value: TDistributionPointNameEnum);
    function GetText: WideString;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsFullName: TGeneralNames read GetAsFullName write SetAsFullName;
    property AsNameRelativeToCRLIssuer: TRelativeDistinguishedName read GetAsNameRelativeToCRLIssuer write SetAsNameRelativeToCRLIssuer;
    property Text: WideString read GetText;
  published
    property Choice: TDistributionPointNameEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCrldistributionPointsSyntax
          TDistributionPoint}

  TDistributionPoint = class(TASNConstructedWrapper)
  private
    function GetDistributionPoint: TDistributionPointName;
    procedure SetDistributionPoint(const Value: TDistributionPointName);
    function GetReasons: TBitString;
    procedure SetReasons(const Value: TBitString);
    function GetCRLIssuer: TGeneralNames;
    procedure SetCRLIssuer(const Value: TGeneralNames);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property DistributionPoint: TDistributionPointName read GetDistributionPoint write SetDistributionPoint;
    property Reasons: TBitString read GetReasons write SetReasons;
    property CRLIssuer: TGeneralNames read GetCRLIssuer write SetCRLIssuer;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TCrldistributionPointsSyntax}

  TCrldistributionPointsSyntax = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TDistributionPoint;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TDistributionPoint;
    property Items[Index: Integer]: TDistributionPoint read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAttributesSyntax
          TAttribute
           TSetOfAttrValue}

  TSetOfAttrValue = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TAttrValue;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TAttrValue;
    property Items[Index: Integer]: TAttrValue read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAttributesSyntax
          TAttribute}

  TAttribute = class(TASNConstructedWrapper)
  private
    function GetAttributeType: ObjectIdentifier;
    procedure SetAttributeType(const Value: ObjectIdentifier);
    function GetAttributeValue: TSetOfAttrValue;
    procedure SetAttributeValue(const Value: TSetOfAttrValue);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TAttrValueEnum): Boolean;
  published
    property AttributeType: ObjectIdentifier read GetAttributeType write SetAttributeType;
    property AttributeValue: TSetOfAttrValue read GetAttributeValue write SetAttributeValue;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAttributesSyntax}

  TAttributesSyntax = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TAttribute;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TAttribute;
    property Items[Index: Integer]: TAttribute read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityInfoAccessSyntax
          TAccessDescription}

  TAccessDescription = class(TASNConstructedWrapper)
  private
    function GetAccessMethod: ObjectIdentifier;
    procedure SetAccessMethod(const Value: ObjectIdentifier);
    function GetAccessLocation: TGeneralName;
    procedure SetAccessLocation(const Value: TGeneralName);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property AccessMethod: ObjectIdentifier read GetAccessMethod write SetAccessMethod;
    property AccessLocation: TGeneralName read GetAccessLocation write SetAccessLocation;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue
         TAuthorityInfoAccessSyntax}

  TAuthorityInfoAccessSyntax = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TAccessDescription;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TAccessDescription;
    property Items[Index: Integer]: TAccessDescription read GetItems; default;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension
        TExtnValue}

  TExtnValueEnum = (
    eveUndefined, eveIdCeAuthorityKeyIdentifier, eveIdCeSubjectKeyIdentifier,
    eveIdCeKeyUsage, eveIdCeExtKeyUsage, eveIdCePrivateKeyUsagePeriod,
    eveIdCeCertificatePolicies, eveIdCePolicyMappings, eveIdCeSubjectAltName,
    eveIdCeIssuerAltName, eveIdCeBasicConstraints, eveIdCeNameConstraints,
    eveIdCePolicyConstraints, eveIdCeCRLDistributionPoints,
    eveIdCeSubjectDirectoryAttributes, eveIdPeAuthorityInfoAccess,
    eveNetscapeCertType);

  TExtnValue = class(TASNChoiceWrapper)
  private
    function GetAsCe_AuthorityKeyIdentifier: TAuthorityKeyIdentifier;
    procedure SetAsCe_AuthorityKeyIdentifier(const Value: TAuthorityKeyIdentifier);
    function GetAsCe_SubjectKeyIdentifier: TOctetString;
    procedure SetAsCe_SubjectKeyIdentifier(const Value: TOctetString);
    function GetAsCe_KeyUsage: TBitString;
    procedure SetAsCe_KeyUsage(const Value: TBitString);
    function GetAsCe_ExtKeyUsage: TExtKeyUsageSyntax;
    procedure SetAsCe_ExtKeyUsage(const Value: TExtKeyUsageSyntax);
    function GetAsCe_PrivateKeyUsagePeriod: TPrivateKeyUsagePeriod;
    procedure SetAsCe_PrivateKeyUsagePeriod(const Value: TPrivateKeyUsagePeriod);
    function GetAsCe_CertificatePolicies: TCertificatePolicies;
    procedure SetAsCe_CertificatePolicies(const Value: TCertificatePolicies);
    function GetAsCe_PolicyMappings: TPolicyMappingsSyntax;
    procedure SetAsCe_PolicyMappings(const Value: TPolicyMappingsSyntax);
    function GetAsCe_SubjectAltName: TGeneralNames;
    procedure SetAsCe_SubjectAltName(const Value: TGeneralNames);
    function GetAsCe_IssuerAltName: TGeneralNames;
    procedure SetAsCe_IssuerAltName(const Value: TGeneralNames);
    function GetAsCe_BasicConstraints: TBasicConstraintsSyntax;
    procedure SetAsCe_BasicConstraints(const Value: TBasicConstraintsSyntax);
    function GetAsCe_NameConstraints: TNameConstraintsSyntax;
    procedure SetAsCe_NameConstraints(const Value: TNameConstraintsSyntax);
    function GetAsCe_PolicyConstraints: TPolicyConstraintsSyntax;
    procedure SetAsCe_PolicyConstraints(const Value: TPolicyConstraintsSyntax);
    function GetAsCe_CRLDistributionPoints: TCrldistributionPointsSyntax;
    procedure SetAsCe_CRLDistributionPoints(const Value: TCrldistributionPointsSyntax);
    function GetAsCe_SubjectDirectoryAttributes: TAttributesSyntax;
    procedure SetAsCe_SubjectDirectoryAttributes(const Value: TAttributesSyntax);
    function GetAsPe_AuthorityInfoAccess: TAuthorityInfoAccessSyntax;
    procedure SetAsPe_AuthorityInfoAccess(const Value: TAuthorityInfoAccessSyntax);
    function GetChoice: TExtnValueEnum;
    procedure SetChoice(const Value: TExtnValueEnum);
    function GetAsNetscapeCertType: TBitString;
    procedure SetAsNetscapeCertType(const Value: TBitString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsCe_AuthorityKeyIdentifier: TAuthorityKeyIdentifier read GetAsCe_AuthorityKeyIdentifier write SetAsCe_AuthorityKeyIdentifier;
    property AsCe_SubjectKeyIdentifier: TOctetString read GetAsCe_SubjectKeyIdentifier write SetAsCe_SubjectKeyIdentifier;
    property AsCe_KeyUsage: TBitString read GetAsCe_KeyUsage write SetAsCe_KeyUsage;
    property AsCe_ExtKeyUsage: TExtKeyUsageSyntax read GetAsCe_ExtKeyUsage write SetAsCe_ExtKeyUsage;
    property AsCe_PrivateKeyUsagePeriod: TPrivateKeyUsagePeriod read GetAsCe_PrivateKeyUsagePeriod write SetAsCe_PrivateKeyUsagePeriod;
    property AsCe_CertificatePolicies: TCertificatePolicies read GetAsCe_CertificatePolicies write SetAsCe_CertificatePolicies;
    property AsCe_PolicyMappings: TPolicyMappingsSyntax read GetAsCe_PolicyMappings write SetAsCe_PolicyMappings;
    property AsCe_SubjectAltName: TGeneralNames read GetAsCe_SubjectAltName write SetAsCe_SubjectAltName;
    property AsCe_IssuerAltName: TGeneralNames read GetAsCe_IssuerAltName write SetAsCe_IssuerAltName;
    property AsCe_BasicConstraints: TBasicConstraintsSyntax read GetAsCe_BasicConstraints write SetAsCe_BasicConstraints;
    property AsCe_NameConstraints: TNameConstraintsSyntax read GetAsCe_NameConstraints write SetAsCe_NameConstraints;
    property AsCe_PolicyConstraints: TPolicyConstraintsSyntax read GetAsCe_PolicyConstraints write SetAsCe_PolicyConstraints;
    property AsCe_CRLDistributionPoints: TCrldistributionPointsSyntax read GetAsCe_CRLDistributionPoints write SetAsCe_CRLDistributionPoints;
    property AsCe_SubjectDirectoryAttributes: TAttributesSyntax read GetAsCe_SubjectDirectoryAttributes write SetAsCe_SubjectDirectoryAttributes;
    property AsPe_AuthorityInfoAccess: TAuthorityInfoAccessSyntax read GetAsPe_AuthorityInfoAccess write SetAsPe_AuthorityInfoAccess;
    property AsNetscapeCertType: TBitString read GetAsNetscapeCertType write SetAsNetscapeCertType;
  published
    property Choice: TExtnValueEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions
       TExtension}

  TExtension = class(TASNConstructedWrapper)
  private
    function GetExtnID: ObjectIdentifier;
    procedure SetExtnID(const Value: ObjectIdentifier);
    function GetCritical: Boolean;
    procedure SetCritical(const Value: Boolean);
    function GetExtnValue: TExtnValue;
    procedure SetExtnValue(const Value: TExtnValue);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TExtnValueEnum): Boolean;
    procedure SetType(Id: TExtnValueEnum);
  published
    property ExtnID: ObjectIdentifier read GetExtnID write SetExtnID;
    property Critical: Boolean read GetCritical write SetCritical;
    property ExtnValue: TExtnValue read GetExtnValue write SetExtnValue;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate
      TExtensions}

  TExtensions = class(TASNCustomOFFieldWrapper)
  private
    function GetUniqueItem(Id: TExtnValueEnum): TExtension;
    function GetItems(Index: Integer): TExtension;
    function GetKeyUsage: TKeyUsage;
    procedure SetKeyUsage(const Value: TKeyUsage);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function AddUniqueItem(Id: TExtnValueEnum): TExtension;
    function Add: TExtension;
    property Items[Index: Integer]: TExtension read GetItems;
    property UniqueItem[Id: TExtnValueEnum]: TExtension read GetUniqueItem; default;
  published
    property KeyUsage: TKeyUsage read GetKeyUsage write SetKeyUsage;
  end;

{ Declaration tree: 
    TCertificate
     TTbscertificate}

  TTbscertificate = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetSerialNumber: TIntegerWrapper;
    procedure SetSerialNumber(const Value: TIntegerWrapper);
    function GetSignature: TAlgorithmIdentifier;
    procedure SetSignature(const Value: TAlgorithmIdentifier);
    function GetIssuer: TName;
    procedure SetIssuer(const Value: TName);
    function GetValidity: TValidity;
    procedure SetValidity(const Value: TValidity);
    function GetSubject: TName;
    procedure SetSubject(const Value: TName);
    function GetSubjectPublicKeyInfo: TSubjectPublicKeyInfo;
    procedure SetSubjectPublicKeyInfo(const Value: TSubjectPublicKeyInfo);
    function GetIssuerUniqueID: TBitString;
    procedure SetIssuerUniqueID(const Value: TBitString);
    function GetSubjectUniqueID: TBitString;
    procedure SetSubjectUniqueID(const Value: TBitString);
    function GetExtensions: TExtensions;
    procedure SetExtensions(const Value: TExtensions);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property SerialNumber: TIntegerWrapper read GetSerialNumber write SetSerialNumber;
    property Signature: TAlgorithmIdentifier read GetSignature write SetSignature;
    property Issuer: TName read GetIssuer write SetIssuer;
    property Validity: TValidity read GetValidity write SetValidity;
    property Subject: TName read GetSubject write SetSubject;
    property SubjectPublicKeyInfo: TSubjectPublicKeyInfo read GetSubjectPublicKeyInfo write SetSubjectPublicKeyInfo;
    property IssuerUniqueID: TBitString read GetIssuerUniqueID write SetIssuerUniqueID;
    property SubjectUniqueID: TBitString read GetSubjectUniqueID write SetSubjectUniqueID;
    property Extensions: TExtensions read GetExtensions write SetExtensions;
  end;

{ Declaration tree:
    TCertificate}

  TCertificate = class;

  ISigned = interface
    ['{24639F3B-0A3A-4317-B361-F567476C56FB}']
    function CheckSignature(CACert: TCertificate): Boolean; overload;
    function CheckSignature(PublicKey: IReadOnlyPublicKeyInfo): Boolean; overload;
    function RSASign(const RSAKey: TIFPrivateKey;
                     HA: THashAlgorithm;
                     CACert: TCertificate): Boolean; overload;
    function RSASSA_PSSSign(const RSAKey: TIFPrivateKey;
                            CACert: TCertificate): Boolean; overload;
    function DSASign(const DSAKey: TDLPrivateKey;
                     CACert: TCertificate): Boolean; overload;
    function ECDSASign(const ECDSAKey: TECPrivateKey;
                       CACert: TCertificate): Boolean; overload;
    function RSASign(const RSAKey: TIFPrivateKey;
                     HA: THashAlgorithm;
                     CACert: TASN1Struct): Boolean; overload;
    function RSASSA_PSSSign(const RSAKey: TIFPrivateKey;
                            CACert: TASN1Struct): Boolean; overload;
    function DSASign(const DSAKey: TDLPrivateKey;
                     CACert: TASN1Struct): Boolean; overload;
    function ECDSASign(const ECDSAKey: TECPrivateKey;
                       CACert: TASN1Struct): Boolean; overload;
    function RSASign(const RSAKey: TIFPrivateKey;
                     HA: THashAlgorithm): Boolean; overload;
    function RSASSA_PSSSign(const RSAKey: TIFPrivateKey): Boolean; overload;
    function DSASign(const DSAKey: TDLPrivateKey): Boolean; overload;
    function ECDSASign(const ECDSAKey: TECPrivateKey): Boolean; overload;
  end;

  TSigned = class(TASNConstructedWrapper,ISigned)
  private
    function GetSignatureAlgorithm: TAlgorithmIdentifier;
    procedure SetSignatureAlgorithm(const Value: TAlgorithmIdentifier);
    function GetSignature: TBitString;
    procedure SetSignature(const Value: TBitString);
  protected
    function BeforeSign(var CACert: TCertificate;
                        SignAlg: ObjectIdentifier;
                        out Params: TASNCustomWrapper): Boolean; virtual; abstract;
    function CheckSignAlgCoherence: Boolean; virtual; abstract;
    function InternalCheckSignature(CACert: IReadOnlyPublicKeyInfo): Boolean; dynamic;
    function InterpretRSASignatureAlgorithm(var HA: THashAlgorithm;
                                            var MHA: THashAlgorithm;
                                            var EM: TSignEncoding): Boolean;
    function IsAllegedIssuer(CACert: TCertificate): Boolean; virtual; abstract;
  public
    function CheckSignature(CACert: TCertificate): Boolean; overload;
    function CheckSignature(PublicKey: IReadOnlyPublicKeyInfo): Boolean; overload;
    function RSASign(const RSAKey: TIFPrivateKey;
                     HA: THashAlgorithm;
                     CACert: TCertificate): Boolean; overload;
    function RSASSA_PSSSign(const RSAKey: TIFPrivateKey;
                            CACert: TCertificate): Boolean; overload;
    function DSASign(const DSAKey: TDLPrivateKey;
                     CACert: TCertificate): Boolean; overload;
    function ECDSASign(const ECDSAKey: TECPrivateKey;
                       CACert: TCertificate): Boolean; overload;
    function RSASign(const RSAKey: TIFPrivateKey;
                     HA: THashAlgorithm;
                     CACert: TASN1Struct): Boolean; overload;
    function RSASSA_PSSSign(const RSAKey: TIFPrivateKey;
                            CACert: TASN1Struct): Boolean; overload;
    function DSASign(const DSAKey: TDLPrivateKey;
                     CACert: TASN1Struct): Boolean; overload;
    function ECDSASign(const ECDSAKey: TECPrivateKey;
                       CACert: TASN1Struct): Boolean; overload;
    function RSASign(const RSAKey: TIFPrivateKey;
                     HA: THashAlgorithm): Boolean; overload;
    function RSASSA_PSSSign(const RSAKey: TIFPrivateKey): Boolean; overload;
    function DSASign(const DSAKey: TDLPrivateKey): Boolean; overload;
    function ECDSASign(const ECDSAKey: TECPrivateKey): Boolean; overload;
  published
    property SignatureAlgorithm: TAlgorithmIdentifier read GetSignatureAlgorithm write SetSignatureAlgorithm;
    property Signature: TBitString read GetSignature write SetSignature;
  end;

  TCertificate = class(TSigned,
                       IWriteOnlyPublicKeyInfo,
                       IReadOnlyPublicKeyInfo,
                       IPublicKeyInfo,
                       IPKPublicKeyInfo,
                       IIFPublicKeyInfo,
                       IDLPublicKeyInfo,
                       IECPublicKeyInfo)
  private
    function GetTbsCertificate: TTbscertificate;
    procedure SetTbsCertificate(const Value: TTbscertificate);
    function GetIssuer: TName;
    function GetSubject: TName;
    function GetValidity: TValidity;
    procedure SetIssuer(const Value: TName);
    procedure SetSubject(const Value: TName);
    procedure SetValidity(const Value: TValidity);
    function GetSerialNumber: TIntegerWrapper;
    function GetSubjectPublicKeyInfo: TSubjectPublicKeyInfo;
    function GetVersion: Integer;
    procedure SetSerialNumber(const Value: TIntegerWrapper);
    procedure SetSubjectPublicKeyInfo(const Value: TSubjectPublicKeyInfo);
    procedure SetVersion(const Value: Integer);
  protected                          
    function AlgorithmIdentifier: string;
    function BeforeSign(var CACert: TCertificate;
                        SignAlg: ObjectIdentifier;
                        out Params: TASNCustomWrapper): Boolean; override;
    function CheckSignAlgCoherence: Boolean; override;
    function GetAlgorithm: TAlgorithmIdentifier;
    function IsAllegedIssuer(CACert: TCertificate): Boolean; override;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function ExtractSubjectRSAPublicKey(var RSAKey: TIFPublicKey): Boolean;
    function ExtractSubjectDSAPublicKey(var DSAKey: TDLPublicKey): Boolean;
    function ExtractSubjectDHPublicKey(var DHKey: TDLPublicKey): Boolean;
    function ExtractSubjectNRPublicKey(var NRKey: TDLPublicKey): Boolean;
    function ExtractSubjectECPublicKey(var ECKey: TECPublicKey): Boolean;
    function ExtractSubjectRSAES_OAEPPublicKey(var RSAKey: TIFPublicKey;
                                               var HA: THashAlgorithm;
                                               var MGFHA: THashAlgorithm;
                                               var P: string): Boolean;
    procedure ImposeSubjectRSAPublicKey(const RSAKey: TIFPublicKey);
    procedure ImposeSubjectDSAPublicKey(const DSAKey: TDLPublicKey);
    procedure ImposeSubjectDHPublicKey(const DHKey: TDLPublicKey);
    procedure ImposeSubjectNRPublicKey(const NRKey: TDLPublicKey);
    procedure ImposeSubjectECPublicKey(var ECKey: TECPublicKey);
    procedure ImposeSubjectRSAES_OAEPPublicKey(const RSAKey: TIFPublicKey;
                                               HA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               MGFHA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               P: OctetString = '');
    function PublicKeyIdentifier: string;
  published
    property TbsCertificate: TTbscertificate read GetTbsCertificate write SetTbsCertificate;
    property Issuer: TName read GetIssuer write SetIssuer;
    property SerialNumber: TIntegerWrapper read GetSerialNumber write SetSerialNumber;
    property Subject: TName read GetSubject write SetSubject;
    property SubjectPublicKeyInfo: TSubjectPublicKeyInfo read GetSubjectPublicKeyInfo
                                                         write SetSubjectPublicKeyInfo
                                                         implements IReadOnlyPublicKeyInfo;
    property Validity: TValidity read GetValidity write SetValidity;
    property Version: Integer read GetVersion write SetVersion;
  end;

function FormatIPAddress(OctetString: TOctetString): string;
procedure ParseIPAddress(const IP: string; OctetString: TOctetString);

{$IFNDEF INIT_SECTIONS}
procedure Initialize;
{$ENDIF}
{$IFNDEF FINI_SECTIONS}
procedure Finalize;
{$ENDIF}

implementation

uses
  SysUtils, ReadStrm, MpArith, MpEC_NISTCurves, MpEC_X9_62Curves, X509Base;

var
  GlobalObject: TASN1Struct;

procedure InitGlobalObject;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(ASNModule);
  try
    GlobalObject := TASN1Struct.Create;
    GlobalObject._AddRef;
    GlobalObject.LoadFromStream(SS,fmtASN1);
  finally
    SS.Free;
  end;
end;

procedure Initialize;
begin
  if GlobalObject = nil then begin
    InitGlobalObject;
  end;
end;

procedure Finalize;
var
  F: TASN1Struct;
begin
  if Assigned(GlobalObject) then begin
    F := GlobalObject;
    GlobalObject := nil;
    F._Release;
  end;
end;

function FormatIPAddress(OctetString: TOctetString): string;
var
  OS: string;
begin
  OS := OctetString.AsString;
  if OctetString.Length = 4 then
    Result := IntToStr(Byte(OS[1])) + '.' +
              IntToStr(Byte(OS[2])) + '.' +
              IntToStr(Byte(OS[3])) + '.' +
              IntToStr(Byte(OS[4]))
  else if OctetString.Length = 8 then
    Result := IntToStr(Byte(OS[1])) + '.' +
              IntToStr(Byte(OS[2])) + '.' +
              IntToStr(Byte(OS[3])) + '.' +
              IntToStr(Byte(OS[4])) + '/' +
              IntToStr(Byte(OS[5])) + '.' +
              IntToStr(Byte(OS[6])) + '.' +
              IntToStr(Byte(OS[7])) + '.' +
              IntToStr(Byte(OS[8]))
  else
    Result := '';
end;

procedure ParseIPAddress(const IP: string; OctetString: TOctetString);
var
  S, OS: string;
  I: Integer;
begin
  OS := '';
  S := '';
  for I := 1 to Length(IP) do
    if IP[I] in ['0'..'9'] then
      S := S + IP[I]
    else if IP[I] in ['.','/'] then begin
      OS := OS + Char(StrToInt(S));
      S := '';
    end;
  if S <> '' then
    OS := OS + Char(StrToInt(S));
  OctetString.AsString := OS;
end;

{ TValidationParms }

constructor TValidationParms.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[0]^;
    F := F.FindField('validationParms')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TBitString,nil,FData.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[1]^,nil^);
end;

destructor TValidationParms.Destroy;
begin
  inherited Destroy;
end;

function TValidationParms.GetSeed: TBitString;
begin
  Result := FItems[0];
end;

procedure TValidationParms.SetSeed(const Value: TBitString);
begin
  TBitString(FItems[0]).Assign(Value);
end;

function TValidationParms.GetPgenCounter: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TValidationParms.SetPgenCounter(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

{ TDomainParameters }

constructor TDomainParameters.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[3]^,nil^);
  FieldFactory(TValidationParms,nil,FData.Items[4]^,nil^);
end;

destructor TDomainParameters.Destroy;
begin
  inherited Destroy;
end;

function TDomainParameters.GetP: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TDomainParameters.SetP(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TDomainParameters.GetG: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TDomainParameters.SetG(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

function TDomainParameters.GetQ: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TDomainParameters.SetQ(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

function TDomainParameters.GetJ: TIntegerWrapper;
begin
  Result := FItems[3];
end;

procedure TDomainParameters.SetJ(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[3]).Assign(Value);
end;

function TDomainParameters.GetValidationParms: TValidationParms;
begin
  Result := FItems[4];
end;

procedure TDomainParameters.SetValidationParms(const Value: TValidationParms);
begin
  TValidationParms(FItems[4]).Assign(Value);
end;

{ TDss_Params }

constructor TDss_Params.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[8]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
end;

destructor TDss_Params.Destroy;
begin
  inherited Destroy;
end;

function TDss_Params.GetP: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TDss_Params.SetP(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TDss_Params.GetQ: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TDss_Params.SetQ(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

function TDss_Params.GetG: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TDss_Params.SetG(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

{ TAlgorithmID }

constructor TAlgorithmID.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(TDss_Params);
  FChoiceList.Add(TDss_Params);
  FChoiceList.Add(nil);
  FChoiceList.Add(TDomainParameters);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TRsaesOaepParams);
  FChoiceList.Add(TRsassaPssParams);
  FChoiceList.Add(TRsassaPssParams);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  if FData.TypeName <> '' then
    Update;
end;

destructor TAlgorithmID.Destroy;
begin
  inherited Destroy;
end;

function TAlgorithmID.GetAsDhpublicnumber: TDomainParameters;
begin
  Result := (FSelected as TDomainParameters);
end;

procedure TAlgorithmID.SetAsDhpublicnumber(const Value: TDomainParameters);
begin
  InternalSelectChoice(Ord(aideDhpublicnumber)-1);
  (FSelected as TDomainParameters).Assign(Value);
end;

function TAlgorithmID.GetAsDsa: TDss_Params;
begin
  Result := (FSelected as TDss_Params);
end;

procedure TAlgorithmID.SetAsDsa(const Value: TDss_Params);
begin
  InternalSelectChoice(Ord(aideIdDsa)-1);
  (FSelected as TDss_Params).Assign(Value);
end;

function TAlgorithmID.GetAsEcPublicKey: ObjectIdentifier;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TAlgorithmID.SetAsEcPublicKey(const Value: ObjectIdentifier);
begin
  InternalSelectChoice(Ord(aideIdEcPublicKey)-1);
  (FSelected as TStringWrapper).Value := Value;
end;

function TAlgorithmID.GetChoice: TAlgorithmIDEnum;
begin
  Result := TAlgorithmIDEnum(InternalGetChoice);
end;

procedure TAlgorithmID.SetChoice(const Value: TAlgorithmIDEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

function TAlgorithmID.GetAsRsaesOaep: TRsaesOaepParams;
begin
  Result := (FSelected as TRsaesOaepParams);
end;

procedure TAlgorithmID.SetAsRsaesOaep(const Value: TRsaesOaepParams);
begin
  InternalSelectChoice(Ord(aideIdRsaesOaep)-1);
  (FSelected as TRsaesOaepParams).Assign(Value);
end;

function TAlgorithmID.GetAsRsassaPss: TRsassaPssParams;
begin
  Result := (FSelected as TRsassaPssParams);
end;

procedure TAlgorithmID.SetAsRsassaPss(const Value: TRsassaPssParams);
begin
  InternalSelectChoice(Ord(aideIdRsassaPss)-1);
  (FSelected as TRsassaPssParams).Assign(Value);
end;

function TAlgorithmID.GetAsNr: TDss_Params;
begin
  Result := (FSelected as TDss_Params);
end;

procedure TAlgorithmID.SetAsNr(const Value: TDss_Params);
begin
  InternalSelectChoice(Ord(aideIdNr)-1);
  (FSelected as TDss_Params).Assign(Value);
end;

function TAlgorithmID.GetAsRw: TRsassaPssParams;
begin
  Result := (FSelected as TRsassaPssParams);
end;

procedure TAlgorithmID.SetAsRw(const Value: TRsassaPssParams);
begin
  InternalSelectChoice(Ord(aideIdRw)-1);
  (FSelected as TRsassaPssParams).Assign(Value);
end;

function TAlgorithmID.GetAsEcNr: ObjectIdentifier;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TAlgorithmID.SetAsEcNr(const Value: ObjectIdentifier);
begin
  InternalSelectChoice(Ord(aideIdEcNr)-1);
  (FSelected as TStringWrapper).Value := Value;
end;

{ TAlgorithmIdentifier }

constructor TAlgorithmIdentifier.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TAlgorithmID,nil,FData.Items[1]^,nil^);
end;

destructor TAlgorithmIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TAlgorithmIdentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TAlgorithmIdentifier.SetAlgorithm(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
  Parameters.Update;
end;

function TAlgorithmIdentifier.GetParameters: TAlgorithmID;
begin
  Result := FItems[1];
end;

procedure TAlgorithmIdentifier.SetParameters(const Value: TAlgorithmID);
begin
  TAlgorithmID(FItems[1]).Assign(Value);
end;

function TAlgorithmIdentifier.IsType(Id: TAlgorithmIDEnum): Boolean;
begin
  Result := TAlgorithmID(FItems[1]).Choice = Id;
end;

procedure TAlgorithmIdentifier.SetType(Id: TAlgorithmIDEnum);
begin
  TAlgorithmID(FItems[1]).Choice := Id;
end;

{ TDirectoryString }

constructor TDirectoryString.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/issuer')^.Choices[0]^.Template.Template.FindField('attrValue')^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TWideStringWrapper);
  FChoiceList.Add(TWideStringWrapper);
end;

destructor TDirectoryString.Destroy;
begin
  inherited Destroy;
end;

function TDirectoryString.GetAsTeletexString: TeletexString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TDirectoryString.SetAsTeletexString(const Value: TeletexString);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TDirectoryString.GetAsPrintableString: PrintableString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TDirectoryString.SetAsPrintableString(const Value: PrintableString);
begin
  InternalSelectChoice(1);
  (FSelected as TStringWrapper).Value := Value;
end;

function TDirectoryString.GetAsUniversalString: UniversalString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TDirectoryString.SetAsUniversalString(const Value: UniversalString);
begin
  InternalSelectChoice(2);
  (FSelected as TStringWrapper).Value := Value;
end;

function TDirectoryString.GetAsUtf8String: WideString;
begin
  Result := (FSelected as TWideStringWrapper).Value;
end;

procedure TDirectoryString.SetAsUtf8String(const Value: WideString);
begin
  InternalSelectChoice(3);
  (FSelected as TWideStringWrapper).Value := Value;
end;

function TDirectoryString.GetAsBmpString: WideString;
begin
  Result := (FSelected as TWideStringWrapper).Value;
end;

procedure TDirectoryString.SetAsBmpString(const Value: WideString);
begin
  InternalSelectChoice(4);
  (FSelected as TWideStringWrapper).Value := Value;
end;

function TDirectoryString.GetChoice: TDirectoryStringEnum;
begin
  Result := TDirectoryStringEnum(InternalGetChoice);
end;

procedure TDirectoryString.SetChoice(const Value: TDirectoryStringEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TAnsistring }

constructor TAnsistring.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/issuer')^.Choices[0]^.Template.Template.FindField('attrValue')^.Choices[11]^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TWideStringWrapper);
end;

destructor TAnsistring.Destroy;
begin
  inherited Destroy;
end;

function TAnsistring.GetAsPrintableString: PrintableString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TAnsistring.SetAsPrintableString(const Value: PrintableString);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TAnsistring.GetAsUtf8String: WideString;
begin
  Result := (FSelected as TWideStringWrapper).Value;
end;

procedure TAnsistring.SetAsUtf8String(const Value: WideString);
begin
  InternalSelectChoice(1);
  (FSelected as TWideStringWrapper).Value := Value;
end;

function TAnsistring.GetChoice: TAnsistringEnum;
begin
  Result := TAnsistringEnum(InternalGetChoice);
end;

procedure TAnsistring.SetChoice(const Value: TAnsistringEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TEmailAddress }

constructor TEmailAddress.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/issuer')^.Choices[0]^.Template.Template.FindField('attrValue')^.Choices[13]^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TWideStringWrapper);
end;

destructor TEmailAddress.Destroy;
begin
  inherited Destroy;
end;

function TEmailAddress.GetAsIa5String: IA5String;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TEmailAddress.SetAsIa5String(const Value: IA5String);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TEmailAddress.GetAsUtf8String: WideString;
begin
  Result := (FSelected as TWideStringWrapper).Value;
end;

procedure TEmailAddress.SetAsUtf8String(const Value: WideString);
begin
  InternalSelectChoice(1);
  (FSelected as TWideStringWrapper).Value := Value;
end;

function TEmailAddress.GetChoice: TEmailAddressEnum;
begin
  Result := TEmailAddressEnum(InternalGetChoice);
end;

procedure TEmailAddress.SetChoice(const Value: TEmailAddressEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TAttrValue }

constructor TAttrValue.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/issuer')^.Choices[0]^.Template.Template;
    F := F.FindField('attrValue')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TAnsistring);
  FChoiceList.Add(TAnsistring);
  FChoiceList.Add(TEmailAddress);
end;

destructor TAttrValue.Destroy;
begin
  inherited Destroy;
end;

function TAttrValue.GetAsAt_Name: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_Name(const Value: TDirectoryString);
begin
  InternalSelectChoice(0);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_Surname: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_Surname(const Value: TDirectoryString);
begin
  InternalSelectChoice(1);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_GivenName: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_GivenName(const Value: TDirectoryString);
begin
  InternalSelectChoice(2);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_Initials: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_Initials(const Value: TDirectoryString);
begin
  InternalSelectChoice(3);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_GenerationQualifier: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_GenerationQualifier(const Value: TDirectoryString);
begin
  InternalSelectChoice(4);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_CommonName: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_CommonName(const Value: TDirectoryString);
begin
  InternalSelectChoice(5);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_LocalityName: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_LocalityName(const Value: TDirectoryString);
begin
  InternalSelectChoice(6);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_StateOrProvinceName: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_StateOrProvinceName(const Value: TDirectoryString);
begin
  InternalSelectChoice(7);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_OrganizationName: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_OrganizationName(const Value: TDirectoryString);
begin
  InternalSelectChoice(8);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_OrganizationalUnitName: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_OrganizationalUnitName(const Value: TDirectoryString);
begin
  InternalSelectChoice(9);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_Title: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TAttrValue.SetAsAt_Title(const Value: TDirectoryString);
begin
  InternalSelectChoice(10);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TAttrValue.GetAsAt_DnQualifier: TAnsistring;
begin
  Result := (FSelected as TAnsistring);
end;

procedure TAttrValue.SetAsAt_DnQualifier(const Value: TAnsistring);
begin
  InternalSelectChoice(11);
  (FSelected as TAnsistring).Assign(Value);
end;

function TAttrValue.GetAsAt_Country: TAnsistring;
begin
  Result := (FSelected as TAnsistring);
end;

procedure TAttrValue.SetAsAt_Country(const Value: TAnsistring);
begin
  InternalSelectChoice(12);
  (FSelected as TAnsistring).Assign(Value);
end;

function TAttrValue.GetAsEmailAddress: TEmailAddress;
begin
  Result := (FSelected as TEmailAddress);
end;

procedure TAttrValue.SetAsEmailAddress(const Value: TEmailAddress);
begin
  InternalSelectChoice(13);
  (FSelected as TEmailAddress).Assign(Value);
end;

function TAttrValue.GetChoice: TAttrValueEnum;
begin
  Result := TAttrValueEnum(InternalGetChoice);
end;

procedure TAttrValue.SetChoice(const Value: TAttrValueEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TAttributeTypeAndValue }

constructor TAttributeTypeAndValue.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/issuer')^.Choices[0]^.Template.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TAttrValue,nil,FData.Items[1]^,nil^);
end;

destructor TAttributeTypeAndValue.Destroy;
begin
  inherited Destroy;
end;

function TAttributeTypeAndValue.GetAttrType: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TAttributeTypeAndValue.SetAttrType(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TAttributeTypeAndValue.GetAttrValue: TAttrValue;
begin
  Result := FItems[1];
end;

procedure TAttributeTypeAndValue.SetAttrValue(const Value: TAttrValue);
begin
  TAttrValue(FItems[1]).Assign(Value);
end;

type
  THack = class(TASN1Struct);

function TAttributeTypeAndValue.IsType(Id: TAttrValueEnum): Boolean;
begin
  Result := TStringWrapper(FItems[0]).Value =
            TAttrValue(FItems[1]).FTemplate.Choices[Ord(Id)-1].IdentifiedBy;
end;

procedure TAttributeTypeAndValue.SetType(Id: TAttrValueEnum);
begin
  TAttrValue(FItems[1]).Choice := Id;
end;

{ TRelativeDistinguishedName }

constructor TRelativeDistinguishedName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TAttributeTypeAndValue;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/issuer')^.Choices[0]^.Template;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TRelativeDistinguishedName.Destroy;
begin
  inherited Destroy;
end;

function TRelativeDistinguishedName.GetUniqueItem(Id: TAttrValueEnum): TAttributeTypeAndValue;
var
  I: Integer;
begin
  for I := ItemCount - 1 downto 0 do begin
    Result := Items[I];
    if Result.IsType(Id) then
      Exit;
  end;
  Result := nil;
end;

function TRelativeDistinguishedName.AddUniqueItem(Id: TAttrValueEnum): TAttributeTypeAndValue;
begin
  Result := GetUniqueItem(Id);
  if Result = nil then begin
    Result := Add;
    Result.SetType(Id);
  end;
end;

function TRelativeDistinguishedName.Add: TAttributeTypeAndValue;
begin
  Result := TAttributeTypeAndValue(InternalAdd);
end;

function TRelativeDistinguishedName.GetItems(Index: Integer): TAttributeTypeAndValue;
begin
  Result := TAttributeTypeAndValue(InternalGetItems(Index));
end;

{ TRdnsequence }

constructor TRdnsequence.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TRelativeDistinguishedName;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/issuer')^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TRdnsequence.Destroy;
begin
  inherited Destroy;
end;

function TRdnsequence.GetUniqueItem(Id: TAttrValueEnum): TAttributeTypeAndValue;
var
  I: Integer;
begin
  for I := ItemCount - 1 downto 0 do begin
    Result := Items[I].UniqueItem[Id];
    if Assigned(Result) then
      Exit;
  end;
  Result := nil;
end;

function TRdnsequence.AddUniqueItem(Id: TAttrValueEnum): TAttributeTypeAndValue;
begin
  Result := GetUniqueItem(Id);
  if Result = nil then
    Result := Add.AddUniqueItem(Id);
end;

function TRdnsequence.Add: TRelativeDistinguishedName;
begin
  Result := TRelativeDistinguishedName(InternalAdd);
end;

function TRdnsequence.GetItems(Index: Integer): TRelativeDistinguishedName;
begin
  Result := TRelativeDistinguishedName(InternalGetItems(Index));
end;

function TRdnsequence.GetAsAt_CommonName: WideString;
begin
  Result := GetValues(aveIdAtCommonName);
end;

function TRdnsequence.GetAsAt_Country: string;
begin
  Result := GetValues(aveIdAtCountry);
end;

function TRdnsequence.GetAsAt_DnQualifier: string;
begin
  Result := GetValues(aveIdAtDnQualifier);
end;

function TRdnsequence.GetAsAt_GenerationQualifier: WideString;
begin
  Result := GetValues(aveIdAtGenerationQualifier);
end;

function TRdnsequence.GetAsAt_GivenName: WideString;
begin
  Result := GetValues(aveIdAtGivenName);
end;

function TRdnsequence.GetAsAt_Initials: WideString;
begin
  Result := GetValues(aveIdAtInitials);
end;

function TRdnsequence.GetAsAt_LocalityName: WideString;
begin
  Result := GetValues(aveIdAtLocalityName);
end;

function TRdnsequence.GetAsAt_OrganizationalUnitName: WideString;
begin
  Result := GetValues(aveIdAtOrganizationalUnitName);
end;

function TRdnsequence.GetAsAt_OrganizationName: WideString;
begin
  Result := GetValues(aveIdAtOrganizationName);
end;

function TRdnsequence.GetAsAt_StateOrProvinceName: WideString;
begin
  Result := GetValues(aveIdAtStateOrProvinceName);
end;

function TRdnsequence.GetAsAt_Surname: WideString;
begin
  Result := GetValues(aveIdAtSurname);
end;

function TRdnsequence.GetAsAt_Title: WideString;
begin
  Result := GetValues(aveIdAtTitle);
end;

function TRdnsequence.GetAsEmailAddress: string;
begin
  Result := GetValues(aveEmailAddress);
end;

procedure TRdnsequence.SetAsAt_CommonName(const Value: WideString);
begin
  SetValue(aveIdAtCommonName,Value);
end;

procedure TRdnsequence.SetAsAt_Country(const Value: string);
begin
  SetValue(aveIdAtCountry,Value);
end;

procedure TRdnsequence.SetAsAt_DnQualifier(const Value: string);
begin
  SetValue(aveIdAtDnQualifier,Value);
end;

procedure TRdnsequence.SetAsAt_GenerationQualifier(const Value: WideString);
begin
  SetValue(aveIdAtGenerationQualifier,Value);
end;

procedure TRdnsequence.SetAsAt_GivenName(const Value: WideString);
begin
  SetValue(aveIdAtGivenName,Value);
end;

procedure TRdnsequence.SetAsAt_Initials(const Value: WideString);
begin
  SetValue(aveIdAtInitials,Value);
end;

procedure TRdnsequence.SetAsAt_LocalityName(const Value: WideString);
begin
  SetValue(aveIdAtLocalityName,Value);
end;

procedure TRdnsequence.SetAsAt_OrganizationalUnitName(const Value: WideString);
begin
  SetValue(aveIdAtOrganizationalUnitName,Value);
end;

procedure TRdnsequence.SetAsAt_OrganizationName(const Value: WideString);
begin
  SetValue(aveIdAtOrganizationName,Value);
end;

procedure TRdnsequence.SetAsAt_StateOrProvinceName(const Value: WideString);
begin
  SetValue(aveIdAtStateOrProvinceName,Value);
end;

procedure TRdnsequence.SetAsAt_Surname(const Value: WideString);
begin
  SetValue(aveIdAtSurname,Value);
end;

procedure TRdnsequence.SetAsAt_Title(const Value: WideString);
begin
  SetValue(aveIdAtTitle,Value);
end;

procedure TRdnsequence.SetAsEmailAddress(const Value: string);
begin
  SetValue(aveEmailAddress,Value);
end;

procedure TRdnsequence.SetUniqueValue(Id: TAttrValueEnum;
  Value: WideString);
var
  Attr: TAttributeTypeAndValue;
  I, J: Integer;
begin
  if Value = '' then begin
    for I := Count - 1 downto 0 do begin
      for J := Items[I].Count - 1 downto 0 do
        if Items[I].Items[J].IsType(Id) then
          Items[I].Delete(J);
      if Items[I].Count = 0 then
        Delete(I);
    end;
  end else begin
    Attr := AddUniqueItem(Id);
    if Attr.AttrValue.Selected is TDirectoryString then
      TDirectoryString(Attr.AttrValue.Selected).Choice := dseUtf8String
    else
      TANSIString(Attr.AttrValue.Selected).Choice := aePrintableString;
    Attr.AttrValue.Value := Value;
  end;
end;

function TRdnsequence.UniqueValue(Id: TAttrValueEnum): WideString;
var
  Attr: TAttributeTypeAndValue;
begin
  Attr := UniqueItem[Id];
  if Assigned(Attr) then
    Result := Attr.AttrValue.Value
  else
    Result := '';
end;

function TRdnsequence.GetNextValue(Id: TAttrValueEnum; var I,
  J: Integer): WideString;
var
  Attr: TAttributeTypeAndValue;
begin
  Result := '';
  while (Result = '') and (I < Count) do begin
    while (Result = '') and (J < Items[I].Count) do begin
      Attr := Items[I].Items[J];
      if Attr.IsType(Id) then
        Result := Attr.AttrValue.Value;
      Inc(J);
    end;
    if Result = '' then begin
      Inc(I);
      J := 0;
    end;
  end;
end;

function TRdnsequence.GetValues(Id: TAttrValueEnum): WideString;
var
  I, J: Integer;
  Separator: string;
  Value: WideString;
begin
  if LineSeparator = '' then
    Result := UniqueValue(Id)
  else begin
    Separator := '';
    Result := '';
    I := 0;
    J := 0;
    repeat
      Value := GetNextValue(Id,I,J);
      if I < Count then begin
        Result := Result + Separator + Value;
        Separator := LineSeparator;
      end;
    until I = Count;
  end;
end;

procedure TRdnsequence.SetLineSeparator(const Value: string);
begin
  FLineSeparator := Value;
end;

function TRdnsequence.AddItem(Id: TAttrValueEnum): TAttributeTypeAndValue;
begin
  Result := Add.AddUniqueItem(Id);
end;

procedure TRdnsequence.SetValue(Id: TAttrValueEnum; Value: WideString);
var
  Attr: TAttributeTypeAndValue;
  I, J, P: Integer;
  Str: WideString;
begin
  if LineSeparator = '' then
    SetUniqueValue(Id,Value)
  else begin
    for I := Count - 1 downto 0 do begin
      for J := Items[I].Count - 1 downto 0 do
        if Items[I].Items[J].IsType(Id) then
          Items[I].Delete(J);
      if Items[I].Count = 0 then
        Delete(I);
    end;
    while Value <> '' do begin
      P := Pos(LineSeparator,Value);
      if P = 0 then begin
        Str := Value;
        Value := '';
      end else begin
        Str := Copy(Value,1,P-1);
        Value := Copy(Value,P + Length(LineSeparator),MaxInt);
      end;
      Attr := AddItem(Id);
      if Attr.AttrValue.Selected is TDirectoryString then
        TDirectoryString(Attr.AttrValue.Selected).Choice := dseUtf8String
      else
        TANSIString(Attr.AttrValue.Selected).Choice := aePrintableString;
      Attr.AttrValue.Value := Str;
    end;
  end;
end;

function TRdnsequence.Compare(AName: TRdnSequence): Boolean;
begin
  Result := ASN1StructCompare(FData,AName.FData);
end;

{ TName }

constructor TName.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/issuer')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TRdnsequence);
  Choice := neRdnSequence;
end;

destructor TName.Destroy;
begin
  inherited Destroy;
end;

function TName.GetAsRdnSequence: TRdnsequence;
begin
  if FSelected = nil then
    FieldFactory(FChoiceList[0],FData.Choices[0]^,FData,FSelected);
  Result := (FSelected as TRdnsequence);
end;

procedure TName.SetAsRdnSequence(const Value: TRdnsequence);
begin
  InternalSelectChoice(0);
  (FSelected as TRdnsequence).Assign(Value);
end;

function TName.GetChoice: TNameEnum;
begin
  Result := TNameEnum(InternalGetChoice);
end;

procedure TName.SetChoice(const Value: TNameEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

function TName.GetAsAt_CommonName: WideString;
begin
  Result := GetValues(aveIdAtCommonName);
end;

function TName.GetAsAt_Country: string;
begin
  Result := GetValues(aveIdAtCountry);
end;

function TName.GetAsAt_DnQualifier: string;
begin
  Result := GetValues(aveIdAtDnQualifier);
end;

function TName.GetAsAt_GenerationQualifier: WideString;
begin
  Result := GetValues(aveIdAtGenerationQualifier);
end;

function TName.GetAsAt_GivenName: WideString;
begin
  Result := GetValues(aveIdAtGivenName);
end;

function TName.GetAsAt_Initials: WideString;
begin
  Result := GetValues(aveIdAtInitials);
end;

function TName.GetAsAt_LocalityName: WideString;
begin
  Result := GetValues(aveIdAtLocalityName);
end;

function TName.GetAsAt_OrganizationalUnitName: WideString;
begin
  Result := GetValues(aveIdAtOrganizationalUnitName);
end;

function TName.GetAsAt_OrganizationName: WideString;
begin
  Result := GetValues(aveIdAtOrganizationName);
end;

function TName.GetAsAt_StateOrProvinceName: WideString;
begin
  Result := GetValues(aveIdAtStateOrProvinceName);
end;

function TName.GetAsAt_Surname: WideString;
begin
  Result := GetValues(aveIdAtSurname);
end;

function TName.GetAsAt_Title: WideString;
begin
  Result := GetValues(aveIdAtTitle);
end;

procedure TName.SetAsAt_CommonName(const Value: WideString);
begin
  SetValue(aveIdAtCommonName,Value);
end;

procedure TName.SetAsAt_Country(const Value: string);
begin
  SetValue(aveIdAtCountry,Value);
end;

procedure TName.SetAsAt_DnQualifier(const Value: string);
begin
  SetValue(aveIdAtDnQualifier,Value);
end;

procedure TName.SetAsAt_GenerationQualifier(const Value: WideString);
begin
  SetValue(aveIdAtGenerationQualifier,Value);
end;

procedure TName.SetAsAt_GivenName(const Value: WideString);
begin
  SetValue(aveIdAtGivenName,Value);
end;

procedure TName.SetAsAt_Initials(const Value: WideString);
begin
  SetValue(aveIdAtInitials,Value);
end;

procedure TName.SetAsAt_LocalityName(const Value: WideString);
begin
  SetValue(aveIdAtLocalityName,Value);
end;

procedure TName.SetAsAt_OrganizationalUnitName(const Value: WideString);
begin
  SetValue(aveIdAtOrganizationalUnitName,Value);
end;

procedure TName.SetAsAt_OrganizationName(const Value: WideString);
begin
  SetValue(aveIdAtOrganizationName,Value);
end;

procedure TName.SetAsAt_StateOrProvinceName(const Value: WideString);
begin
  SetValue(aveIdAtStateOrProvinceName,Value);
end;

procedure TName.SetAsAt_Surname(const Value: WideString);
begin
  SetValue(aveIdAtSurname,Value);
end;

procedure TName.SetAsAt_Title(const Value: WideString);
begin
  SetValue(aveIdAtTitle,Value);
end;

function TName.Compare(Name: TName): Boolean;
begin
  Result := ASN1StructCompare(FData,Name.FData);
end;

procedure TName.SetUniqueValue(Id: TAttrValueEnum; Value: WideString);
var
  Attr: TAttributeTypeAndValue;
  I, J: Integer;
begin
  if Value = '' then begin
    for I := AsRdnSequence.Count - 1 downto 0 do begin
      for J := AsRdnSequence.Items[I].Count - 1 downto 0 do
        if AsRdnSequence.Items[I].Items[J].IsType(Id) then
          AsRdnSequence.Items[I].Delete(J);
      if AsRdnSequence.Items[I].Count = 0 then
        AsRdnSequence.Delete(I);
    end;
  end else begin
    Attr := AsRdnSequence.AddUniqueItem(Id);
    if Attr.AttrValue.Selected is TDirectoryString then
      TDirectoryString(Attr.AttrValue.Selected).Choice := dseUtf8String
    else
      TANSIString(Attr.AttrValue.Selected).Choice := aePrintableString;
    Attr.AttrValue.Value := Value;
  end;
end;

function TName.UniqueValue(Id: TAttrValueEnum): WideString;
var
  Attr: TAttributeTypeAndValue;
begin
  Attr := AsRdnSequence.UniqueItem[Id];
  if Assigned(Attr) then
    Result := Attr.AttrValue.Value
  else
    Result := '';
end;

procedure TName.SetLineSeparator(const Value: string);
begin
  AsRdnsequence.LineSeparator := Value;
end;

function TName.GetNextValue(Id: TAttrValueEnum;
  var I, J: Integer): WideString;
begin
  Result := AsRdnSequence.GetNextValue(Id,I,J);
end;

function TName.GetValues(Id: TAttrValueEnum): WideString;
begin
  Result := AsRdnSequence.GetValues(Id);
end;

procedure TName.SetValue(Id: TAttrValueEnum; Value: WideString);
begin
  AsRdnsequence.SetValue(Id,Value);
end;

function TName.GetLineSeparator: string;
begin
  Result := AsRdnSequence.LineSeparator;
end;

{ TTime }

constructor TTime.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/validity/notBefore')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TUTCTime);
  FChoiceList.Add(TGeneralizedTime);
end;

destructor TTime.Destroy;
begin
  inherited Destroy;
end;

function TTime.GetAsUtcTime: TUTCTime;
begin                    
  InternalSelectChoice(0);
  Result := (FSelected as TUTCTime);
end;

procedure TTime.SetAsUtcTime(const Value: TUTCTime);
begin
  InternalSelectChoice(0);
  (FSelected as TUTCTime).Assign(Value);
end;

function TTime.GetAsGeneralTime: TGeneralizedTime;
begin          
  InternalSelectChoice(1);
  Result := (FSelected as TGeneralizedTime);
end;

procedure TTime.SetAsGeneralTime(const Value: TGeneralizedTime);
begin
  InternalSelectChoice(1);
  (FSelected as TGeneralizedTime).Assign(Value);
end;

function TTime.GetChoice: TTimeEnum;
begin
  Result := TTimeEnum(InternalGetChoice);
end;

procedure TTime.SetChoice(const Value: TTimeEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TValidity }

constructor TValidity.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/validity')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TTime,nil,FData.Items[0]^,nil^);
  FieldFactory(TTime,nil,FData.Items[1]^,nil^);
end;

destructor TValidity.Destroy;
begin
  inherited Destroy;
end;

function TValidity.GetNotBefore: TTime;
begin
  Result := FItems[0];
end;

procedure TValidity.SetNotBefore(const Value: TTime);
begin
  TTime(FItems[0]).Assign(Value);
end;

function TValidity.GetNotAfter: TTime;
begin
  Result := FItems[1];
end;

procedure TValidity.SetNotAfter(const Value: TTime);
begin
  TTime(FItems[1]).Assign(Value);
end;

{ TRsakey }

constructor TRsakey.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/subjectPublicKeyInfo/subjectPublicKey')^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Encapsulated.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Encapsulated.Items[1]^,nil^);
end;

destructor TRsakey.Destroy;
begin
  inherited Destroy;
end;

function TRsakey.GetModulus: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TRsakey.SetModulus(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TRsakey.GetPublicExponent: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TRsakey.SetPublicExponent(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

{ TEncapsulatedPublicKey }

constructor TEncapsulatedPublicKey.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/subjectPublicKeyInfo/subjectPublicKey')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TRsakey);
  FChoiceList.Add(TIntegerWrapper);
  FChoiceList.Add(TIntegerWrapper);
  FChoiceList.Add(TIntegerWrapper);
  FChoiceList.Add(TBitString);
  FChoiceList.Add(TRsakey);
  FChoiceList.Add(TRsakey);    
  FChoiceList.Add(TRsakey);
  FChoiceList.Add(TBitString);
end;

destructor TEncapsulatedPublicKey.Destroy;
begin
  inherited Destroy;
end;

function TEncapsulatedPublicKey.GetAsRsaEncryption: TRsakey;
begin
  if FSelected is TOctetString then
    Choice := epkeRsaEncryption;
  Result := (FSelected as TRsakey);
end;

procedure TEncapsulatedPublicKey.SetAsRsaEncryption(const Value: TRsakey);
begin
  InternalSelectChoice(Ord(epkeRsaEncryption)-1);
  (FSelected as TRsakey).Assign(Value);
end;

function TEncapsulatedPublicKey.GetAsDhpublicnumber: TIntegerWrapper;
begin
  Result := (FSelected as TIntegerWrapper);
end;

procedure TEncapsulatedPublicKey.SetAsDhpublicnumber(const Value: TIntegerWrapper);
begin
  InternalSelectChoice(Ord(epkeDhPublicNumber)-1);
  (FSelected as TIntegerWrapper).Assign(Value);
end;

function TEncapsulatedPublicKey.GetAsDsa: TIntegerWrapper;
begin
  Result := (FSelected as TIntegerWrapper);
end;

procedure TEncapsulatedPublicKey.SetAsDsa(const Value: TIntegerWrapper);
begin
  InternalSelectChoice(Ord(epkeIdDsa)-1);
  (FSelected as TIntegerWrapper).Assign(Value);
end;

function TEncapsulatedPublicKey.GetAsEcPublicKey: TBitString;
begin
  Result := (FSelected as TBitString);
end;

procedure TEncapsulatedPublicKey.SetAsEcPublicKey(const Value: TBitString);
begin
  InternalSelectChoice(Ord(epkeIdEcPublicKey)-1);
  (FSelected as TBitString).Assign(Value);
end;

function TEncapsulatedPublicKey.GetChoice: TEncapsulatedPublicKeyEnum;
begin
  Result := TEncapsulatedPublicKeyEnum(InternalGetChoice);
end;

procedure TEncapsulatedPublicKey.SetChoice(const Value: TEncapsulatedPublicKeyEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

function TEncapsulatedPublicKey.GetAsRsaesOaep: TRsakey;
begin
  if FSelected is TOctetString then
    Choice := epkeIdRsaesOaep;
  Result := (FSelected as TRsakey);
end;

procedure TEncapsulatedPublicKey.SetAsRsaesOaep(const Value: TRsakey);
begin
  InternalSelectChoice(Ord(epkeIdRsaesOaep)-1);
  (FSelected as TRsakey).Assign(Value);
end;

function TEncapsulatedPublicKey.GetAsRsassaPss: TRsakey;
begin
  if FSelected is TOctetString then
    Choice := epkeIdRsassaPss;
  Result := (FSelected as TRsakey);
end;

procedure TEncapsulatedPublicKey.SetAsRsassaPss(const Value: TRsakey);
begin
  InternalSelectChoice(Ord(epkeIdRsassaPss)-1);
  (FSelected as TRsakey).Assign(Value);
end;

function TEncapsulatedPublicKey.GetAsNR: TIntegerWrapper;
begin
  Result := (FSelected as TIntegerWrapper);
end;

procedure TEncapsulatedPublicKey.SetAsNR(const Value: TIntegerWrapper);
begin
  InternalSelectChoice(Ord(epkeIdDsa)-1);
  (FSelected as TIntegerWrapper).Assign(Value);
end;

function TEncapsulatedPublicKey.GetAsRW: TRsakey;
begin
  if FSelected is TOctetString then
    Choice := epkeIdRw;
  Result := (FSelected as TRsakey);
end;

procedure TEncapsulatedPublicKey.SetAsRW(const Value: TRsakey);
begin
  InternalSelectChoice(Ord(epkeIdRw)-1);
  (FSelected as TRsakey).Assign(Value);
end;

function TEncapsulatedPublicKey.GetAsEcNr: TBitString;
begin
  if FSelected is TOctetString then
    Choice := epkeIdEcNr;
  Result := (FSelected as TBitString);
end;

procedure TEncapsulatedPublicKey.SetAsEcNr(const Value: TBitString);
begin
  InternalSelectChoice(Ord(epkeIdEcNr)-1);
  (FSelected as TBitString).Assign(Value);
end;

{ TSubjectPublicKeyInfo }

constructor TSubjectPublicKeyInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited Create(AOwner,AData,ATemplate);
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/subjectPublicKeyInfo')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[0]^,nil^);
  FieldFactory(TEncapsulatedPublicKey,nil,FData.Items[1]^,nil^);
end;

constructor TSubjectPublicKeyInfo.CreateFromCert(ACert: TASN1Struct);
var
  PK: PASN1Struct;
  Res: Integer;
begin
  Res := X509Base.ExtractSubjectPublicKeyStruct(ACert,PK);
  Assert(Res = E_OK);
  Create(nil,nil);
  AssignStruct(PK^);
end;

destructor TSubjectPublicKeyInfo.Destroy;
begin
  inherited Destroy;
end;

function TSubjectPublicKeyInfo.GetAlgorithm: TAlgorithmIdentifier;
begin
  Result := FItems[0];
end;

procedure TSubjectPublicKeyInfo.SetAlgorithm(const Value: TAlgorithmIdentifier);
begin
  TAlgorithmIdentifier(FItems[0]).Assign(Value);
end;

function TSubjectPublicKeyInfo.GetSubjectPublicKey: TEncapsulatedPublicKey;
begin
  Result := FItems[1];
end;

procedure TSubjectPublicKeyInfo.SetSubjectPublicKey(const Value: TEncapsulatedPublicKey);
begin
  TEncapsulatedPublicKey(FItems[1]).Assign(Value);
end;

function TSubjectPublicKeyInfo.IsType(Id: TEncapsulatedPublicKeyEnum): Boolean;
begin
  Result := TEncapsulatedPublicKey(FItems[1]).Choice = Id;
end;

procedure TSubjectPublicKeyInfo.SetType(Id: TEncapsulatedPublicKeyEnum);
begin
  TEncapsulatedPublicKey(FItems[1]).Choice := Id;
end;        

function TSubjectPublicKeyInfo.ExtractSubjectDHPublicKey(
  var DHKey: TDLPublicKey): Boolean;
var
  Params: TDomainParameters;
  Key: TIntegerWrapper;
begin
  Result := SubjectPublicKey.Choice = epkeDhPublicNumber;
  if Result then begin
    Params := Algorithm.Parameters.AsDhPublicNumber;
    Params.P.GetMPInt(DHKey.Params.P);
    Params.G.GetMPInt(DHKey.Params.G);
    Params.Q.GetMPInt(DHKey.Params.Q);
    if Params.J.Length > 0 then
      Params.J.GetMPInt(DHKey.Params.J)
    else begin
      MPDealloc(DHKey.Params.J);
      DHKey.Params.J := nil;
    end;
    Key := SubjectPublicKey.AsDhPublicNumber;
    Key.GetMPInt(DHKey.Y);
    DHKey.KAScheme := dlkasDH1;
  end;
end;

function TSubjectPublicKeyInfo.ExtractSubjectDSAPublicKey(
  var DSAKey: TDLPublicKey): Boolean;
var
  Params: TDSS_Params;
  Key: TIntegerWrapper;
begin
  Result := SubjectPublicKey.Choice = epkeIdDsa;
  if Result then begin
    Params := Algorithm.Parameters.AsDsa;
    Params.P.GetMPInt(DSAKey.Params.P);
    Params.G.GetMPInt(DSAKey.Params.G);
    Params.Q.GetMPInt(DSAKey.Params.Q);
    MPDealloc(DSAKey.Params.J);
    DSAKey.Params.J := nil;
    Key := SubjectPublicKey.AsDsa;
    Key.GetMPInt(DSAKey.Y);
    DSAKey.SignScheme := dlssaDSA;
  end;
end;

function TSubjectPublicKeyInfo.ExtractSubjectECPublicKey(
  var ECKey: TECPublicKey): Boolean;
var
  OID: string;
begin
  Result := True;
  case SubjectPublicKey.Choice of
    epkeIdEcPublicKey: ECKey.SignScheme := ecssaDSA;
    epkeIdEcNr:        ECKey.SignScheme := ecssaNR;
  else
    Result := False;
  end;
  if Result then begin
    OID := Algorithm.Parameters.AsEcPublicKey;
    Result := OIDToCurve(OID,ECKey.Params);
    if Result then begin
      Result := SubjectPublicKey.FData.ContentAsECPoint(ECKey.W,ECKey.Params);
      ECKey.KAScheme := eckasDH1;
    end;
  end;
end;

function TSubjectPublicKeyInfo.ExtractSubjectRSAPublicKey(
  var RSAKey: TIFPublicKey): Boolean;
var
  Key: TRSAKey;
begin
  Result := SubjectPublicKey.Choice = epkeRsaEncryption;
  if Result then begin
    Key := SubjectPublicKey.AsRsaEncryption;
    Key.Modulus.GetMPInt(RSAKey.N);
    Key.PublicExponent.GetMPInt(RSAKey.E);
    RSAKey.Scheme := ifRSA1;
  end;
end;

function TSubjectPublicKeyInfo.ExtractSubjectRSAES_OAEPPublicKey(
  var RSAKey: TIFPublicKey; var HA, MGFHA: THashAlgorithm;
  var P: string): Boolean;
var
  Key: TRSAKey;
  HashFunc, MgfFunc: ObjectIdentifier;
begin
  Result := SubjectPublicKey.Choice = epkeIdRsaesOaep;
  if Result then begin
    Key := SubjectPublicKey.AsRsaesOaep;
    RSAKey.Scheme := ifRSA1;
    HashFunc := Algorithm.Parameters.AsRsaesOaep.HashFunc.Algorithm;
    MgfFunc := Algorithm.Parameters.AsRsaesOaep.MgfFunc.Parameters.AsMgf1.Algorithm;
    with Algorithm.Parameters.AsRsaesOaep.PSourceFunc do
      if Parameters.Choice = ppeIdPSpecified then
        P := Parameters.AsPSpecified.AsString
      else
        P := '';
  end else begin
    Result := SubjectPublicKey.Choice = epkeIdRsassaPss;
    if Result then begin
      Key := SubjectPublicKey.AsRsassaPss;
      RSAKey.Scheme := ifRSA1;
      HashFunc := Algorithm.Parameters.AsRsassaPss.HashFunc.Algorithm;
      MgfFunc := Algorithm.Parameters.AsRsassaPss.MgfFunc.Parameters.AsMgf1.Algorithm;
      P := '';
    end else begin
      Result := SubjectPublicKey.Choice = epkeIdRw;
      if Result then begin
        Key := SubjectPublicKey.AsRw;
        RSAKey.Scheme := ifRW;
        HashFunc := Algorithm.Parameters.AsRsassaPss.HashFunc.Algorithm;
        MgfFunc := Algorithm.Parameters.AsRsassaPss.MgfFunc.Parameters.AsMgf1.Algorithm;
        P := '';
      end else
        Key := nil;
    end;
  end;
  Result := OIDToHashAlgorithm(HashFunc,HA);
{$IFDEF SHA1}
  if not Result then HA := haSHA1;
  Result := True;
{$ENDIF}
  if Result then begin
    Result := OIDToHashAlgorithm(Algorithm.Parameters.AsRsaesOaep.MgfFunc.Parameters.AsMgf1.Algorithm,MGFHA);
{$IFDEF SHA1}
    if not Result then MGFHA := haSHA1;
    Result := True;
{$ENDIF}
  end;
  Key.Modulus.GetMPInt(RSAKey.N);
  if (Key.PublicExponent.Length = 0) and (RSAKey.Scheme = ifRW) then begin
    MPDealloc(RSAKey.E);
    RSAKey.E := IntToMPInt(2);
  end else
    Key.PublicExponent.GetMPInt(RSAKey.E);
end;

procedure TSubjectPublicKeyInfo.ImposeSubjectDHPublicKey(
  const DHKey: TDLPublicKey);
begin
  if FData.ReadOnly then
    FData.ReadOnly := False
  else
    THack(FData).DoModifiedChild;
  Algorithm.Algorithm := dhPublicNumber;
  Algorithm.Parameters.AsDhpublicnumber.P.SetMPInt(DHKey.Params.P);
  Algorithm.Parameters.AsDhpublicnumber.G.SetMPInt(DHKey.Params.G);
  Algorithm.Parameters.AsDhpublicnumber.Q.SetMPInt(DHKey.Params.Q);
  if Assigned(DHKey.Params.J) then
    Algorithm.Parameters.AsDhpublicnumber.J.SetMPInt(DHKey.Params.J);
  SubjectPublicKey.Choice := epkeDhPublicNumber;
  SubjectPublicKey.AsDhpublicnumber.SetMPInt(DHKey.Y);
end;

procedure TSubjectPublicKeyInfo.ImposeSubjectDSAPublicKey(
  const DSAKey: TDLPublicKey);
begin
  if FData.ReadOnly then
    FData.ReadOnly := False
  else
    THack(FData).DoModifiedChild;
  Algorithm.Parameters.Choice := aideIdDsa;
  Algorithm.Parameters.AsDsa.P.SetMPInt(DSAKey.Params.P);
  Algorithm.Parameters.AsDsa.G.SetMPInt(DSAKey.Params.G);
  Algorithm.Parameters.AsDsa.Q.SetMPInt(DSAKey.Params.Q);
  SubjectPublicKey.Choice := epkeIdDsa;
  SubjectPublicKey.AsDsa.SetMPInt(DSAKey.Y);
end;

procedure TSubjectPublicKeyInfo.ImposeSubjectECPublicKey(
  var ECKey: TECPublicKey);
begin
  if FData.ReadOnly then
    FData.ReadOnly := False
  else
    THack(FData).DoModifiedChild;
  if ECKey.SignScheme = ecssaDSA then
    Algorithm.Algorithm := id_ecPublicKey
  else
    Algorithm.Algorithm := id_ecnr;
  if IsECurveP192_1(ECKey.Params) then
    Algorithm.Parameters.AsEcPublicKey := prime192v1
  else if IsECurveP192_2(ECKey.Params) then
    Algorithm.Parameters.AsEcPublicKey := prime192v2
  else if IsECurveP192_3(ECKey.Params) then
    Algorithm.Parameters.AsEcPublicKey := prime192v3
  else if IsECurveP224(ECKey.Params) then
    Algorithm.Parameters.AsEcPublicKey := prime224
  else if IsECurveP239_1(ECKey.Params) then
    Algorithm.Parameters.AsEcPublicKey := prime239v1
  else if IsECurveP239_2(ECKey.Params) then
    Algorithm.Parameters.AsEcPublicKey := prime239v2
  else if IsECurveP239_3(ECKey.Params) then
    Algorithm.Parameters.AsEcPublicKey := prime239v3
  else if IsECurveP256_1(ECKey.Params) then
    Algorithm.Parameters.AsEcPublicKey := prime256v1
  else if IsECurveP384(ECKey.Params) then
    Algorithm.Parameters.AsEcPublicKey := prime384
  else if IsECurveP521(ECKey.Params) then
    Algorithm.Parameters.AsEcPublicKey := prime521
  else
    raise Exception.Create('Selected elliptic curve not supported');   
  if ECKey.SignScheme = ecssaDSA then
    SubjectPublicKey.Choice := epkeIdEcPublicKey
  else
    SubjectPublicKey.Choice := epkeIdEcNr;
  SubjectPublicKey.AsEcPublicKey.Data.EditContent(ECKey.W,ECKey.Params,cHybrid);
end;

procedure TSubjectPublicKeyInfo.ImposeSubjectRSAPublicKey(
  const RSAKey: TIFPublicKey);
begin
  if FData.ReadOnly then
    FData.ReadOnly := False
  else
    THack(FData).DoModifiedChild;
  Algorithm.Algorithm := rsaEncryption;
  SubjectPublicKey.Choice := epkeRsaEncryption;
  SubjectPublicKey.AsRsaEncryption.Modulus.SetMPInt(RSAKey.N);
  SubjectPublicKey.AsRsaEncryption.PublicExponent.SetMPInt(RSAKey.E);
end;

procedure TSubjectPublicKeyInfo.ImposeSubjectRSAES_OAEPPublicKey(
  const RSAKey: TIFPublicKey; HA, MGFHA: THashAlgorithm; P: OctetString);
begin
  if FData.ReadOnly then
    FData.ReadOnly := False
  else
    THack(FData).DoModifiedChild;
  if RSAKey.Scheme = ifRW then begin
    Algorithm.Algorithm := id_rw;
    SubjectPublicKey.Choice := epkeIdRw;
    Algorithm.Parameters.AsRw.HashFunc.Algorithm := HashAlgorithmToOID(HA);
    Algorithm.Parameters.AsRw.MgfFunc.Parameters.AsMgf1.Algorithm :=
      HashAlgorithmToOID(MGFHA);
    SubjectPublicKey.AsRw.Modulus.SetMPInt(RSAKey.N);
    SubjectPublicKey.AsRw.PublicExponent.SetMPInt(RSAKey.E);
  end else begin
    Algorithm.Algorithm := id_RSAES_OAEP;
    SubjectPublicKey.Choice := epkeIdRsaesOaep;
    Algorithm.Parameters.AsRsaesOaep.HashFunc.Algorithm := HashAlgorithmToOID(HA);
    Algorithm.Parameters.AsRsaesOaep.MgfFunc.Parameters.AsMgf1.Algorithm :=
      HashAlgorithmToOID(MGFHA);
    Algorithm.Parameters.AsRsaesOaep.PSourceFunc.Parameters.AsPSpecified.AsString := P;
    SubjectPublicKey.AsRsaesOaep.Modulus.SetMPInt(RSAKey.N);
    SubjectPublicKey.AsRsaesOaep.PublicExponent.SetMPInt(RSAKey.E);
  end;
end;

function TSubjectPublicKeyInfo.PublicKeyIdentifier: string;
begin
{$IFDEF SHA1}
  Result := DigestString(haSHA1,SubjectPublicKey.FData.ContentAsOctetString);
{$ELSE  SHA1}
  Result := '';
{$ENDIF SHA1}
end;

function TSubjectPublicKeyInfo.PublicKeyIdentifier2: string;
{$IFDEF SHA1}
var
  F: TAsn1Struct;
  S: string;
  SS: TStringStream;
{$ENDIF SHA1}
begin
{$IFDEF SHA1}
  SS := TStringStream.Create('');
  try
    F := TASN1Struct.Create;
    try
      F.Tag := V_ASN1_BIT_STRING;
      F.Constructed := False;
      S := #0 + SubjectPublicKey.FData.ContentAsOctetString;
      F.SetContent(Pointer(S)^,Length(S));
      F.CalculateLength;
      F.SaveToStream(SS,fmtDER);
    finally
      F.Free;
    end;
    Result := DigestString(haSHA1,SS.DataString);
    Delete(Result,1,12);
    Result[1] := Char($40 or (Byte(Result[1]) and $0F));
  finally
    SS.Free;
  end;
{$ELSE  SHA1}
  Result := '';
{$ENDIF SHA1}
end;

function TSubjectPublicKeyInfo.AlgorithmIdentifier: string;
begin
  Result := Algorithm.Algorithm;
end;

function TSubjectPublicKeyInfo.ExtractSubjectNRPublicKey(
  var NRKey: TDLPublicKey): Boolean;
var
  Params: TDSS_Params;
  Key: TIntegerWrapper;
begin
  Result := SubjectPublicKey.Choice = epkeIdNr;
  if Result then begin
    Params := Algorithm.Parameters.AsDsa;
    Params.P.GetMPInt(NRKey.Params.P);
    Params.G.GetMPInt(NRKey.Params.G);
    Params.Q.GetMPInt(NRKey.Params.Q);
    MPDealloc(NRKey.Params.J);
    NRKey.Params.J := nil;
    Key := SubjectPublicKey.AsNR;
    Key.GetMPInt(NRKey.Y);
    NRKey.SignScheme := dlssaNR;
  end;
end;

procedure TSubjectPublicKeyInfo.ImposeSubjectNRPublicKey(
  const NRKey: TDLPublicKey);
begin
  if FData.ReadOnly then
    FData.ReadOnly := False
  else
    THack(FData).DoModifiedChild;
  Algorithm.Parameters.Choice := aideIdNR;
  Algorithm.Parameters.AsNR.P.SetMPInt(NRKey.Params.P);
  Algorithm.Parameters.AsNR.G.SetMPInt(NRKey.Params.G);
  Algorithm.Parameters.AsNR.Q.SetMPInt(NRKey.Params.Q);
  SubjectPublicKey.Choice := epkeIdNR;
  SubjectPublicKey.AsNR.SetMPInt(NRKey.Y);
end;

{ TAny }

constructor TAny.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[0]^;
    F := F.FindField('/value/')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TAny.Destroy;
begin
  inherited Destroy;
end;

{ TAnotherName }

constructor TAnotherName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TAny,nil,FData.Items[1]^.Items[0]^,nil^);
end;

destructor TAnotherName.Destroy;
begin
  inherited Destroy;
end;

function TAnotherName.GetType_Id: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TAnotherName.SetType_Id(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TAnotherName.GetValue: TAny;
begin
  Result := FItems[1];
end;

procedure TAnotherName.SetValue(const Value: TAny);
begin
  TAny(FItems[1]).Assign(Value);
end;

{ TCountryName }

constructor TCountryName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^;
    F := F.FindField('/built-in-standard-attributes/country-name/')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TStringWrapper);
end;

destructor TCountryName.Destroy;
begin
  inherited Destroy;
end;

function TCountryName.GetAsX121_Dcc_Code: NumericString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TCountryName.SetAsX121_Dcc_Code(const Value: NumericString);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TCountryName.GetAsIso_3166_Alpha2_Code: PrintableString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TCountryName.SetAsIso_3166_Alpha2_Code(const Value: PrintableString);
begin
  InternalSelectChoice(1);
  (FSelected as TStringWrapper).Value := Value;
end;

function TCountryName.GetChoice: TCountryNameEnum;
begin
  Result := TCountryNameEnum(InternalGetChoice);
end;

procedure TCountryName.SetChoice(const Value: TCountryNameEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TAdministrationDomainName }

constructor TAdministrationDomainName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^;
    F := F.FindField('/built-in-standard-attributes/administration-domain-name/')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TStringWrapper);
end;

destructor TAdministrationDomainName.Destroy;
begin
  inherited Destroy;
end;

function TAdministrationDomainName.GetAsNumeric: NumericString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TAdministrationDomainName.SetAsNumeric(const Value: NumericString);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TAdministrationDomainName.GetAsPrintable: PrintableString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TAdministrationDomainName.SetAsPrintable(const Value: PrintableString);
begin
  InternalSelectChoice(1);
  (FSelected as TStringWrapper).Value := Value;
end;

function TAdministrationDomainName.GetChoice: TAdministrationDomainNameEnum;
begin
  Result := TAdministrationDomainNameEnum(InternalGetChoice);
end;

procedure TAdministrationDomainName.SetChoice(const Value: TAdministrationDomainNameEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TPrivateDomainName }

constructor TPrivateDomainName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^;
    F := F.FindField('/built-in-standard-attributes/private-domain-name/')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TStringWrapper);
end;

destructor TPrivateDomainName.Destroy;
begin
  inherited Destroy;
end;

function TPrivateDomainName.GetAsNumeric: NumericString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TPrivateDomainName.SetAsNumeric(const Value: NumericString);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TPrivateDomainName.GetAsPrintable: PrintableString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TPrivateDomainName.SetAsPrintable(const Value: PrintableString);
begin
  InternalSelectChoice(1);
  (FSelected as TStringWrapper).Value := Value;
end;

function TPrivateDomainName.GetChoice: TPrivateDomainNameEnum;
begin
  Result := TPrivateDomainNameEnum(InternalGetChoice);
end;

procedure TPrivateDomainName.SetChoice(const Value: TPrivateDomainNameEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TPersonalName }

constructor TPersonalName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^;
    F := F.FindField('/built-in-standard-attributes/personal-name/')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[1]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[2]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[3]^.Items[0]^,nil^);
end;

destructor TPersonalName.Destroy;
begin
  inherited Destroy;
end;

function TPersonalName.GetSurname: PrintableString;
begin
  Result := PrintableString(TStringWrapper(FItems[0]).Value);
end;

procedure TPersonalName.SetSurname(const Value: PrintableString);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TPersonalName.GetGiven_Name: PrintableString;
begin
  Result := PrintableString(TStringWrapper(FItems[1]).Value);
end;

procedure TPersonalName.SetGiven_Name(const Value: PrintableString);
begin
  TStringWrapper(FItems[1]).Value := Value;
end;

function TPersonalName.GetInitials: PrintableString;
begin
  Result := PrintableString(TStringWrapper(FItems[2]).Value);
end;

procedure TPersonalName.SetInitials(const Value: PrintableString);
begin
  TStringWrapper(FItems[2]).Value := Value;
end;

function TPersonalName.GetGeneration_Qualifier: PrintableString;
begin
  Result := PrintableString(TStringWrapper(FItems[3]).Value);
end;

procedure TPersonalName.SetGeneration_Qualifier(const Value: PrintableString);
begin
  TStringWrapper(FItems[3]).Value := Value;
end;

{ TSequenceOfPrintableString }

constructor TSequenceOfPrintableString.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TStringWrapper;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^;
    F := F.FindField('/built-in-standard-attributes/organizational-unit-names/')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSequenceOfPrintableString.Destroy;
begin
  inherited Destroy;
end;

function TSequenceOfPrintableString.Add: TStringWrapper;
begin
  Result := TStringWrapper(InternalAdd);
end;

procedure TSequenceOfPrintableString.AddValue(const Value: PrintableString);
begin
  TStringWrapper(InternalAdd).Value := Value;
end;

function TSequenceOfPrintableString.GetItems(Index: Integer): TStringWrapper;
begin
  Result := TStringWrapper(InternalGetItems(Index));
end;

function TSequenceOfPrintableString.GetValues(Index: Integer): PrintableString;
begin
  Result := PrintableString(TStringWrapper(InternalGetItems(Index)).Value);
end;

procedure TSequenceOfPrintableString.SetValues(Index: Integer; const Value: PrintableString);
begin
  TStringWrapper(FItems[Index]).Value := Value;
end;

{ TBuiltInStandardAttributes }

constructor TBuiltInStandardAttributes.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^;
    F := F.FindField('built-in-standard-attributes')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TCountryName,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TAdministrationDomainName,nil,FData.Items[1]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[2]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[3]^.Items[0]^,nil^);
  FieldFactory(TPrivateDomainName,nil,FData.Items[4]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[5]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[6]^.Items[0]^,nil^);
  FieldFactory(TPersonalName,nil,FData.Items[7]^.Items[0]^,nil^);
  FieldFactory(TSequenceOfPrintableString,nil,FData.Items[8]^.Items[0]^,nil^);
end;

destructor TBuiltInStandardAttributes.Destroy;
begin
  inherited Destroy;
end;

function TBuiltInStandardAttributes.GetCountry_Name: TCountryName;
begin
  Result := FItems[0];
end;

procedure TBuiltInStandardAttributes.SetCountry_Name(const Value: TCountryName);
begin
  TCountryName(FItems[0]).Assign(Value);
end;

function TBuiltInStandardAttributes.GetAdministration_Domain_Name: TAdministrationDomainName;
begin
  Result := FItems[1];
end;

procedure TBuiltInStandardAttributes.SetAdministration_Domain_Name(const Value: TAdministrationDomainName);
begin
  TAdministrationDomainName(FItems[1]).Assign(Value);
end;

function TBuiltInStandardAttributes.GetNetwork_Address: NumericString;
begin
  Result := NumericString(TStringWrapper(FItems[2]).Value);
end;

procedure TBuiltInStandardAttributes.SetNetwork_Address(const Value: NumericString);
begin
  TStringWrapper(FItems[2]).Value := Value;
end;

function TBuiltInStandardAttributes.GetTerminal_Identifier: PrintableString;
begin
  Result := PrintableString(TStringWrapper(FItems[3]).Value);
end;

procedure TBuiltInStandardAttributes.SetTerminal_Identifier(const Value: PrintableString);
begin
  TStringWrapper(FItems[3]).Value := Value;
end;

function TBuiltInStandardAttributes.GetPrivate_Domain_Name: TPrivateDomainName;
begin
  Result := FItems[4];
end;

procedure TBuiltInStandardAttributes.SetPrivate_Domain_Name(const Value: TPrivateDomainName);
begin
  TPrivateDomainName(FItems[4]).Assign(Value);
end;

function TBuiltInStandardAttributes.GetOrganization_Name: PrintableString;
begin
  Result := PrintableString(TStringWrapper(FItems[5]).Value);
end;

procedure TBuiltInStandardAttributes.SetOrganization_Name(const Value: PrintableString);
begin
  TStringWrapper(FItems[5]).Value := Value;
end;

function TBuiltInStandardAttributes.GetNumeric_User_Identifier: NumericString;
begin
  Result := NumericString(TStringWrapper(FItems[6]).Value);
end;

procedure TBuiltInStandardAttributes.SetNumeric_User_Identifier(const Value: NumericString);
begin
  TStringWrapper(FItems[6]).Value := Value;
end;

function TBuiltInStandardAttributes.GetPersonal_Name: TPersonalName;
begin
  Result := FItems[7];
end;

procedure TBuiltInStandardAttributes.SetPersonal_Name(const Value: TPersonalName);
begin
  TPersonalName(FItems[7]).Assign(Value);
end;

function TBuiltInStandardAttributes.GetOrganizational_Unit_Names: TSequenceOfPrintableString;
begin
  Result := FItems[8];
end;

procedure TBuiltInStandardAttributes.SetOrganizational_Unit_Names(const Value: TSequenceOfPrintableString);
begin
  TSequenceOfPrintableString(FItems[8]).Assign(Value);
end;

{ TBuiltInDomainDefinedAttribute }

constructor TBuiltInDomainDefinedAttribute.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('built-in-domain-defined-attributes')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[1]^,nil^);
end;

destructor TBuiltInDomainDefinedAttribute.Destroy;
begin
  inherited Destroy;
end;

function TBuiltInDomainDefinedAttribute.GetTypeName: PrintableString;
begin
  Result := PrintableString(TStringWrapper(FItems[0]).Value);
end;

procedure TBuiltInDomainDefinedAttribute.SetTypeName(const Value: PrintableString);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TBuiltInDomainDefinedAttribute.GetValue: PrintableString;
begin
  Result := PrintableString(TStringWrapper(FItems[1]).Value);
end;

procedure TBuiltInDomainDefinedAttribute.SetValue(const Value: PrintableString);
begin
  TStringWrapper(FItems[1]).Value := Value;
end;

{ TBuiltInDomainDefinedAttributes }

constructor TBuiltInDomainDefinedAttributes.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TBuiltInDomainDefinedAttribute;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^;
    F := F.FindField('built-in-domain-defined-attributes')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TBuiltInDomainDefinedAttributes.Destroy;
begin
  inherited Destroy;
end;

function TBuiltInDomainDefinedAttributes.Add: TBuiltInDomainDefinedAttribute;
begin
  Result := TBuiltInDomainDefinedAttribute(InternalAdd);
end;

function TBuiltInDomainDefinedAttributes.GetItems(Index: Integer): TBuiltInDomainDefinedAttribute;
begin
  Result := TBuiltInDomainDefinedAttribute(InternalGetItems(Index));
end;

{ TTeletexPersonalName }

constructor TTeletexPersonalName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[3]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[1]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[2]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[3]^.Items[0]^,nil^);
end;

destructor TTeletexPersonalName.Destroy;
begin
  inherited Destroy;
end;

function TTeletexPersonalName.GetSurname: TeletexString;
begin
  Result := TeletexString(TStringWrapper(FItems[0]).Value);
end;

procedure TTeletexPersonalName.SetSurname(const Value: TeletexString);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TTeletexPersonalName.GetGiven_Name: TeletexString;
begin
  Result := TeletexString(TStringWrapper(FItems[1]).Value);
end;

procedure TTeletexPersonalName.SetGiven_Name(const Value: TeletexString);
begin
  TStringWrapper(FItems[1]).Value := Value;
end;

function TTeletexPersonalName.GetInitials: TeletexString;
begin
  Result := TeletexString(TStringWrapper(FItems[2]).Value);
end;

procedure TTeletexPersonalName.SetInitials(const Value: TeletexString);
begin
  TStringWrapper(FItems[2]).Value := Value;
end;

function TTeletexPersonalName.GetGeneration_Qualifier: TeletexString;
begin
  Result := TeletexString(TStringWrapper(FItems[3]).Value);
end;

procedure TTeletexPersonalName.SetGeneration_Qualifier(const Value: TeletexString);
begin
  TStringWrapper(FItems[3]).Value := Value;
end;

{ TTeletexOrganizationalUnitNames }

constructor TTeletexOrganizationalUnitNames.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TStringWrapper;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[4]^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TTeletexOrganizationalUnitNames.Destroy;
begin
  inherited Destroy;
end;

function TTeletexOrganizationalUnitNames.Add: TStringWrapper;
begin
  Result := TStringWrapper(InternalAdd);
end;

procedure TTeletexOrganizationalUnitNames.AddValue(const Value: TeletexString);
begin
  TStringWrapper(InternalAdd).Value := Value;
end;

function TTeletexOrganizationalUnitNames.GetItems(Index: Integer): TStringWrapper;
begin
  Result := TStringWrapper(InternalGetItems(Index));
end;

function TTeletexOrganizationalUnitNames.GetValues(Index: Integer): TeletexString;
begin
  Result := TeletexString(TStringWrapper(InternalGetItems(Index)).Value);
end;

procedure TTeletexOrganizationalUnitNames.SetValues(Index: Integer; const Value: TeletexString);
begin
  TStringWrapper(FItems[Index]).Value := Value;
end;

{ TTeletexDomainDefinedAttribute }

constructor TTeletexDomainDefinedAttribute.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[5]^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[1]^,nil^);
end;

destructor TTeletexDomainDefinedAttribute.Destroy;
begin
  inherited Destroy;
end;

function TTeletexDomainDefinedAttribute.GetTypeName: TeletexString;
begin
  Result := TeletexString(TStringWrapper(FItems[0]).Value);
end;

procedure TTeletexDomainDefinedAttribute.SetTypeName(const Value: TeletexString);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TTeletexDomainDefinedAttribute.GetValue: TeletexString;
begin
  Result := TeletexString(TStringWrapper(FItems[1]).Value);
end;

procedure TTeletexDomainDefinedAttribute.SetValue(const Value: TeletexString);
begin
  TStringWrapper(FItems[1]).Value := Value;
end;

{ TTeletexDomainDefinedAttributes }

constructor TTeletexDomainDefinedAttributes.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TTeletexDomainDefinedAttribute;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[5]^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TTeletexDomainDefinedAttributes.Destroy;
begin
  inherited Destroy;
end;

function TTeletexDomainDefinedAttributes.Add: TTeletexDomainDefinedAttribute;
begin
  Result := TTeletexDomainDefinedAttribute(InternalAdd);
end;

function TTeletexDomainDefinedAttributes.GetItems(Index: Integer): TTeletexDomainDefinedAttribute;
begin
  Result := TTeletexDomainDefinedAttribute(InternalGetItems(Index));
end;

{ TPhysicalDeliveryCountryName }

constructor TPhysicalDeliveryCountryName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[7]^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TStringWrapper);
end;

destructor TPhysicalDeliveryCountryName.Destroy;
begin
  inherited Destroy;
end;

function TPhysicalDeliveryCountryName.GetAsX121_Dcc_Code: NumericString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TPhysicalDeliveryCountryName.SetAsX121_Dcc_Code(const Value: NumericString);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TPhysicalDeliveryCountryName.GetAsIso_3166_Alpha2_Code: PrintableString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TPhysicalDeliveryCountryName.SetAsIso_3166_Alpha2_Code(const Value: PrintableString);
begin
  InternalSelectChoice(1);
  (FSelected as TStringWrapper).Value := Value;
end;

function TPhysicalDeliveryCountryName.GetChoice: TPhysicalDeliveryCountryNameEnum;
begin
  Result := TPhysicalDeliveryCountryNameEnum(InternalGetChoice);
end;

procedure TPhysicalDeliveryCountryName.SetChoice(const Value: TPhysicalDeliveryCountryNameEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TPostalCode }

constructor TPostalCode.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[8]^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TStringWrapper);
end;

destructor TPostalCode.Destroy;
begin
  inherited Destroy;
end;

function TPostalCode.GetAsNumeric_Code: NumericString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TPostalCode.SetAsNumeric_Code(const Value: NumericString);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TPostalCode.GetAsPrintable_Code: PrintableString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TPostalCode.SetAsPrintable_Code(const Value: PrintableString);
begin
  InternalSelectChoice(1);
  (FSelected as TStringWrapper).Value := Value;
end;

function TPostalCode.GetChoice: TPostalCodeEnum;
begin
  Result := TPostalCodeEnum(InternalGetChoice);
end;

procedure TPostalCode.SetChoice(const Value: TPostalCodeEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TPdsparameter }

constructor TPdsparameter.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[9]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[1]^,nil^);
end;

destructor TPdsparameter.Destroy;
begin
  inherited Destroy;
end;

function TPdsparameter.GetPrintable_String: PrintableString;
begin
  Result := PrintableString(TStringWrapper(FItems[0]).Value);
end;

procedure TPdsparameter.SetPrintable_String(const Value: PrintableString);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TPdsparameter.GetTeletex_String: TeletexString;
begin
  Result := TeletexString(TStringWrapper(FItems[1]).Value);
end;

procedure TPdsparameter.SetTeletex_String(const Value: TeletexString);
begin
  TStringWrapper(FItems[1]).Value := Value;
end;

{ TUnformattedPostalAddress }

constructor TUnformattedPostalAddress.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[15]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TSequenceOfPrintableString,nil,FData.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[1]^,nil^);
end;

destructor TUnformattedPostalAddress.Destroy;
begin
  inherited Destroy;
end;

function TUnformattedPostalAddress.GetPrintable_Address: TSequenceOfPrintableString;
begin
  Result := FItems[0];
end;

procedure TUnformattedPostalAddress.SetPrintable_Address(const Value: TSequenceOfPrintableString);
begin
  TSequenceOfPrintableString(FItems[0]).Assign(Value);
end;

function TUnformattedPostalAddress.GetTeletex_String: TeletexString;
begin
  Result := TeletexString(TStringWrapper(FItems[1]).Value);
end;

procedure TUnformattedPostalAddress.SetTeletex_String(const Value: TeletexString);
begin
  TStringWrapper(FItems[1]).Value := Value;
end;

{ TE163_4Address }

constructor TE163_4Address.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[21]^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[1]^.Items[0]^,nil^);
end;

destructor TE163_4Address.Destroy;
begin
  inherited Destroy;
end;

function TE163_4Address.GetNumber: NumericString;
begin
  Result := NumericString(TStringWrapper(FItems[0]).Value);
end;

procedure TE163_4Address.SetNumber(const Value: NumericString);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TE163_4Address.GetSub_Address: NumericString;
begin
  Result := NumericString(TStringWrapper(FItems[1]).Value);
end;

procedure TE163_4Address.SetSub_Address(const Value: NumericString);
begin
  TStringWrapper(FItems[1]).Value := Value;
end;

{ TSetOfOctetString }

constructor TSetOfOctetString.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TOctetString;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[21]^.Choices[1]^;
    F := F.FindField('/nAddresses/')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSetOfOctetString.Destroy;
begin
  inherited Destroy;
end;

function TSetOfOctetString.Add: TOctetString;
begin
  Result := TOctetString(InternalAdd);
end;

function TSetOfOctetString.GetItems(Index: Integer): TOctetString;
begin
  Result := TOctetString(InternalGetItems(Index));
end;

{ TPresentationAddress }

constructor TPresentationAddress.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[21]^.Choices[1]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TOctetString,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[1]^.Items[0]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[2]^.Items[0]^,nil^);
  FieldFactory(TSetOfOctetString,nil,FData.Items[3]^.Items[0]^,nil^);
end;

destructor TPresentationAddress.Destroy;
begin
  inherited Destroy;
end;

function TPresentationAddress.GetPSelector: TOctetString;
begin
  Result := FItems[0];
end;

procedure TPresentationAddress.SetPSelector(const Value: TOctetString);
begin
  TOctetString(FItems[0]).Assign(Value);
end;

function TPresentationAddress.GetSSelector: TOctetString;
begin
  Result := FItems[1];
end;

procedure TPresentationAddress.SetSSelector(const Value: TOctetString);
begin
  TOctetString(FItems[1]).Assign(Value);
end;

function TPresentationAddress.GetTSelector: TOctetString;
begin
  Result := FItems[2];
end;

procedure TPresentationAddress.SetTSelector(const Value: TOctetString);
begin
  TOctetString(FItems[2]).Assign(Value);
end;

function TPresentationAddress.GetNAddresses: TSetOfOctetString;
begin
  Result := FItems[3];
end;

procedure TPresentationAddress.SetNAddresses(const Value: TSetOfOctetString);
begin
  TSetOfOctetString(FItems[3]).Assign(Value);
end;

{ TExtendedNetworkAddress }

constructor TExtendedNetworkAddress.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template.FindField('/extension-attribute-value/')^.Choices[21]^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TE163_4Address);
  FChoiceList.Add(TPresentationAddress);
end;

destructor TExtendedNetworkAddress.Destroy;
begin
  inherited Destroy;
end;

function TExtendedNetworkAddress.GetAsE163_4_Address: TE163_4Address;
begin
  Result := (FSelected as TE163_4Address);
end;

procedure TExtendedNetworkAddress.SetAsE163_4_Address(const Value: TE163_4Address);
begin
  InternalSelectChoice(0);
  (FSelected as TE163_4Address).Assign(Value);
end;

function TExtendedNetworkAddress.GetAsPsap_Address: TPresentationAddress;
begin
  Result := (FSelected as TPresentationAddress);
end;

procedure TExtendedNetworkAddress.SetAsPsap_Address(const Value: TPresentationAddress);
begin
  InternalSelectChoice(1);
  (FSelected as TPresentationAddress).Assign(Value);
end;

function TExtendedNetworkAddress.GetChoice: TExtendedNetworkAddressEnum;
begin
  Result := TExtendedNetworkAddressEnum(InternalGetChoice);
end;

procedure TExtendedNetworkAddress.SetChoice(const Value: TExtendedNetworkAddressEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TExtnAttr }

constructor TExtnAttr.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template;
    F := F.FindField('/extension-attribute-value/')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TTeletexPersonalName);
  FChoiceList.Add(TTeletexOrganizationalUnitNames);
  FChoiceList.Add(TTeletexDomainDefinedAttributes);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TPhysicalDeliveryCountryName);
  FChoiceList.Add(TPostalCode);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TUnformattedPostalAddress);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TPdsparameter);
  FChoiceList.Add(TExtendedNetworkAddress);
  FChoiceList.Add(TIntegerWrapper);
end;

destructor TExtnAttr.Destroy;
begin
  inherited Destroy;
end;

function TExtnAttr.GetAsCommonName: PrintableString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TExtnAttr.SetAsCommonName(const Value: PrintableString);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TExtnAttr.GetAsTeletexCommonName: TeletexString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TExtnAttr.SetAsTeletexCommonName(const Value: TeletexString);
begin
  InternalSelectChoice(1);
  (FSelected as TStringWrapper).Value := Value;
end;

function TExtnAttr.GetAsTeletexOrganizationName: TeletexString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TExtnAttr.SetAsTeletexOrganizationName(const Value: TeletexString);
begin
  InternalSelectChoice(2);
  (FSelected as TStringWrapper).Value := Value;
end;

function TExtnAttr.GetAsTeletexPersonalName: TTeletexPersonalName;
begin
  Result := (FSelected as TTeletexPersonalName);
end;

procedure TExtnAttr.SetAsTeletexPersonalName(const Value: TTeletexPersonalName);
begin
  InternalSelectChoice(3);
  (FSelected as TTeletexPersonalName).Assign(Value);
end;

function TExtnAttr.GetAsTeletexOrganizationalUnitNames: TTeletexOrganizationalUnitNames;
begin
  Result := (FSelected as TTeletexOrganizationalUnitNames);
end;

procedure TExtnAttr.SetAsTeletexOrganizationalUnitNames(const Value: TTeletexOrganizationalUnitNames);
begin
  InternalSelectChoice(4);
  (FSelected as TTeletexOrganizationalUnitNames).Assign(Value);
end;

function TExtnAttr.GetAsTeletexDomainDefinedAttributes: TTeletexDomainDefinedAttributes;
begin
  Result := (FSelected as TTeletexDomainDefinedAttributes);
end;

procedure TExtnAttr.SetAsTeletexDomainDefinedAttributes(const Value: TTeletexDomainDefinedAttributes);
begin
  InternalSelectChoice(5);
  (FSelected as TTeletexDomainDefinedAttributes).Assign(Value);
end;

function TExtnAttr.GetAsPdsname: PrintableString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TExtnAttr.SetAsPdsname(const Value: PrintableString);
begin
  InternalSelectChoice(6);
  (FSelected as TStringWrapper).Value := Value;
end;

function TExtnAttr.GetAsPhysicalDeliveryCountryName: TPhysicalDeliveryCountryName;
begin
  Result := (FSelected as TPhysicalDeliveryCountryName);
end;

procedure TExtnAttr.SetAsPhysicalDeliveryCountryName(const Value: TPhysicalDeliveryCountryName);
begin
  InternalSelectChoice(7);
  (FSelected as TPhysicalDeliveryCountryName).Assign(Value);
end;

function TExtnAttr.GetAsPostalCode: TPostalCode;
begin
  Result := (FSelected as TPostalCode);
end;

procedure TExtnAttr.SetAsPostalCode(const Value: TPostalCode);
begin
  InternalSelectChoice(8);
  (FSelected as TPostalCode).Assign(Value);
end;

function TExtnAttr.GetAsPhysicalDeliveryOfficeName: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsPhysicalDeliveryOfficeName(const Value: TPdsparameter);
begin
  InternalSelectChoice(9);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsPhysicalDeliveryOfficeNumber: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsPhysicalDeliveryOfficeNumber(const Value: TPdsparameter);
begin
  InternalSelectChoice(10);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsExtensionORAddressComponents: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsExtensionORAddressComponents(const Value: TPdsparameter);
begin
  InternalSelectChoice(11);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsPhysicalDeliveryPersonalName: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsPhysicalDeliveryPersonalName(const Value: TPdsparameter);
begin
  InternalSelectChoice(12);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsPhysicalDeliveryOrganizationName: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsPhysicalDeliveryOrganizationName(const Value: TPdsparameter);
begin
  InternalSelectChoice(13);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsExtensionPhysicalDeliveryAddressComponents: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsExtensionPhysicalDeliveryAddressComponents(const Value: TPdsparameter);
begin
  InternalSelectChoice(14);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsUnformattedPostalAddress: TUnformattedPostalAddress;
begin
  Result := (FSelected as TUnformattedPostalAddress);
end;

procedure TExtnAttr.SetAsUnformattedPostalAddress(const Value: TUnformattedPostalAddress);
begin
  InternalSelectChoice(15);
  (FSelected as TUnformattedPostalAddress).Assign(Value);
end;

function TExtnAttr.GetAsStreetAddress: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsStreetAddress(const Value: TPdsparameter);
begin
  InternalSelectChoice(16);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsPostOfficeBoxAddress: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsPostOfficeBoxAddress(const Value: TPdsparameter);
begin
  InternalSelectChoice(17);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsPosteRestanteAddress: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsPosteRestanteAddress(const Value: TPdsparameter);
begin
  InternalSelectChoice(18);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsUniquePostalName: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsUniquePostalName(const Value: TPdsparameter);
begin
  InternalSelectChoice(19);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsLocalPostalAttributes: TPdsparameter;
begin
  Result := (FSelected as TPdsparameter);
end;

procedure TExtnAttr.SetAsLocalPostalAttributes(const Value: TPdsparameter);
begin
  InternalSelectChoice(20);
  (FSelected as TPdsparameter).Assign(Value);
end;

function TExtnAttr.GetAsExtendedNetworkAddress: TExtendedNetworkAddress;
begin
  Result := (FSelected as TExtendedNetworkAddress);
end;

procedure TExtnAttr.SetAsExtendedNetworkAddress(const Value: TExtendedNetworkAddress);
begin
  InternalSelectChoice(21);
  (FSelected as TExtendedNetworkAddress).Assign(Value);
end;

function TExtnAttr.GetAsTerminalType: TIntegerWrapper;
begin
  Result := (FSelected as TIntegerWrapper);
end;

procedure TExtnAttr.SetAsTerminalType(const Value: TIntegerWrapper);
begin
  InternalSelectChoice(22);
  (FSelected as TIntegerWrapper).Assign(Value);
end;

function TExtnAttr.GetChoice: TExtnAttrEnum;
begin
  Result := TExtnAttrEnum(InternalGetChoice);
end;

procedure TExtnAttr.SetChoice(const Value: TExtnAttrEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TExtensionAttribute }

constructor TExtensionAttribute.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^.FindField('extension-attributes')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TExtnAttr,nil,FData.Items[1]^.Items[0]^,nil^);
end;

destructor TExtensionAttribute.Destroy;
begin
  inherited Destroy;
end;

function TExtensionAttribute.GetExtension_Attribute_Type: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TExtensionAttribute.SetExtension_Attribute_Type(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TExtensionAttribute.GetExtension_Attribute_Value: TExtnAttr;
begin
  Result := FItems[1];
end;

procedure TExtensionAttribute.SetExtension_Attribute_Value(const Value: TExtnAttr);
begin
  TExtnAttr(FItems[1]).Assign(Value);
end;

function TExtensionAttribute.IsType(Id: TExtnAttrEnum): Boolean;
begin
  Result := TExtnAttr(FItems[1]).Choice = Id;
end;

procedure TExtensionAttribute.SetType(Id: TExtnAttrEnum);
begin
  TExtnAttr(FItems[1]).Choice := Id;
end;

function TExtensionAttribute.GetTypeName: string;
begin
  Result := TExtnAttr(FItems[1]).FData.ChoiceTypeName;
end;

{ TExtensionAttributes }

constructor TExtensionAttributes.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TExtensionAttribute;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[3]^;
    F := F.FindField('extension-attributes')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TExtensionAttributes.Destroy;
begin
  inherited Destroy;
end;

function TExtensionAttributes.GetUniqueItem(Id: TExtnAttrEnum): TExtensionAttribute;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do begin
    Result := Items[I];
    if Result.IsType(Id) then
      Exit;
  end;
  Result := nil;
end;

function TExtensionAttributes.AddUniqueItem(Id: TExtnAttrEnum): TExtensionAttribute;
begin
  Result := GetUniqueItem(Id);
  if Result = nil then begin
    Result := Add;
    Result.SetType(Id);
  end;
end;

function TExtensionAttributes.Add: TExtensionAttribute;
begin
  Result := TExtensionAttribute(InternalAdd);
end;

function TExtensionAttributes.GetItems(Index: Integer): TExtensionAttribute;
begin
  Result := TExtensionAttribute(InternalGetItems(Index));
end;

{ TOraddress }

constructor TOraddress.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[5]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TBuiltInStandardAttributes,nil,FData.Items[0]^,nil^);
  FieldFactory(TBuiltInDomainDefinedAttributes,nil,FData.Items[1]^,nil^);
  FieldFactory(TExtensionAttributes,nil,FData.Items[2]^,nil^);
end;

destructor TOraddress.Destroy;
begin
  inherited Destroy;
end;

function TOraddress.GetBuilt_In_Standard_Attributes: TBuiltInStandardAttributes;
begin
  Result := FItems[0];
end;

procedure TOraddress.SetBuilt_In_Standard_Attributes(const Value: TBuiltInStandardAttributes);
begin
  TBuiltInStandardAttributes(FItems[0]).Assign(Value);
end;

function TOraddress.GetBuilt_In_Domain_Defined_Attributes: TBuiltInDomainDefinedAttributes;
begin
  Result := FItems[1];
end;

procedure TOraddress.SetBuilt_In_Domain_Defined_Attributes(const Value: TBuiltInDomainDefinedAttributes);
begin
  TBuiltInDomainDefinedAttributes(FItems[1]).Assign(Value);
end;

function TOraddress.GetExtension_Attributes: TExtensionAttributes;
begin
  Result := FItems[2];
end;

procedure TOraddress.SetExtension_Attributes(const Value: TExtensionAttributes);
begin
  TExtensionAttributes(FItems[2]).Assign(Value);
end;

{ TEdipartyName }

constructor TEdipartyName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template.Choices[8]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TDirectoryString,nil,FData.Items[0]^,nil^);
  FieldFactory(TDirectoryString,nil,FData.Items[1]^,nil^);
end;

destructor TEdipartyName.Destroy;
begin
  inherited Destroy;
end;

function TEdipartyName.GetNameAssigner: TDirectoryString;
begin
  Result := FItems[0];
end;

procedure TEdipartyName.SetNameAssigner(const Value: TDirectoryString);
begin
  TDirectoryString(FItems[0]).Assign(Value);
end;

function TEdipartyName.GetPartyName: TDirectoryString;
begin
  Result := FItems[1];
end;

procedure TEdipartyName.SetPartyName(const Value: TDirectoryString);
begin
  TDirectoryString(FItems[1]).Assign(Value);
end;

{ TGeneralName }

constructor TGeneralName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if AData = nil then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated.FindField('authorityCertIssuer')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TAnotherName);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TOraddress);
  FChoiceList.Add(TName);
  FChoiceList.Add(TEdipartyName);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TOctetString);
  FChoiceList.Add(TStringWrapper);
  if FData.ChoiceVarName <> '' then
    Update;
end;

destructor TGeneralName.Destroy;
begin
  inherited Destroy;
end;

function TGeneralName.GetAsOtherName: TAnotherName;
begin
  Result := (FSelected as TAnotherName);
end;

procedure TGeneralName.SetAsOtherName(const Value: TAnotherName);
begin
  InternalSelectChoice(0);
  (FSelected as TAnotherName).Assign(Value);
end;

function TGeneralName.GetAsRfc822Name: IA5String;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TGeneralName.SetAsRfc822Name(const Value: IA5String);
begin
  InternalSelectChoice(1);
  (FSelected as TStringWrapper).Value := Value;
end;

function TGeneralName.GetAsDNSName: IA5String;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TGeneralName.SetAsDNSName(const Value: IA5String);
begin
  InternalSelectChoice(2);
  (FSelected as TStringWrapper).Value := Value;
end;

function TGeneralName.GetAsX400Address: TOraddress;
begin
  Result := (FSelected as TOraddress);
end;

procedure TGeneralName.SetAsX400Address(const Value: TOraddress);
begin
  InternalSelectChoice(3);
  (FSelected as TOraddress).Assign(Value);
end;

function TGeneralName.GetAsDirectoryName: TName;
begin
  if FSelected = nil then
    FieldFactory(FChoiceList[4],FData.Items[0]^,FData.Items[0]^,FSelected);
  Result := (FSelected as TName);
end;

procedure TGeneralName.SetAsDirectoryName(const Value: TName);
begin
  InternalSelectChoice(4);
  (FSelected as TName).Assign(Value);
end;

function TGeneralName.GetAsEdiPartyName: TEdipartyName;
begin
  Result := (FSelected as TEdipartyName);
end;

procedure TGeneralName.SetAsEdiPartyName(const Value: TEdipartyName);
begin
  InternalSelectChoice(5);
  (FSelected as TEdipartyName).Assign(Value);
end;

function TGeneralName.GetAsUniformResourceIdentifier: IA5String;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TGeneralName.SetAsUniformResourceIdentifier(const Value: IA5String);
begin
  InternalSelectChoice(6);
  (FSelected as TStringWrapper).Value := Value;
end;

function TGeneralName.GetAsIPAddress: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TGeneralName.SetAsIPAddress(const Value: TOctetString);
begin
  InternalSelectChoice(7);
  (FSelected as TOctetString).Assign(Value);
end;

function TGeneralName.GetAsRegisteredID: ObjectIdentifier;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TGeneralName.SetAsRegisteredID(const Value: ObjectIdentifier);
begin
  InternalSelectChoice(8);
  (FSelected as TStringWrapper).Value := Value;
end;

function TGeneralName.GetChoice: TGeneralNameEnum;
begin
  Result := TGeneralNameEnum(InternalGetChoice);
end;

procedure TGeneralName.SetChoice(const Value: TGeneralNameEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

function TGeneralName.GetText: WideString;
var
  J, K: Integer;
begin
  if FSelected = nil then
    case Choice of
      gneUndefined:;
      gneDirectoryName: GetAsDirectoryName;
    else
      FieldFactory(FChoiceList[Ord(Choice)-1],FData.Choices[Ord(Choice)-1]^,FData,FSelected);
    end;
  Result := '';
    case Choice of
      gneOtherName:
        Result := Format('  Other name (%s): %s' + crlf,
          [AsOtherName.Type_Id,
           OSToHex(AsOtherName.Value.FData.ContentAsOctetString)]);
      gneRfc822Name:
        Result := '  RFC 822 name: ' + AsRfc822Name + crlf;
      gneDNSName:
        Result := '  DNS name: ' + AsDNSName + crlf;
      gneX400Address:
        begin
          Result := '  X400 address:' + crlf;
          Result := Result + '    Country name: ' +
            AsX400Address.Built_In_Standard_Attributes.Country_Name.Value + crlf;
          Result := Result + '    Administration domain name: ' +
            AsX400Address.Built_In_Standard_Attributes.Administration_Domain_Name.Value + crlf;
          Result := Result + '    Network address: ' +
            AsX400Address.Built_In_Standard_Attributes.Network_Address + crlf;
          Result := Result + '    Terminal identifier: ' +
            AsX400Address.Built_In_Standard_Attributes.Terminal_Identifier + crlf;
          Result := Result + '    Private domain name: ' +
            AsX400Address.Built_In_Standard_Attributes.Private_Domain_Name.Value + crlf;
          Result := Result + '    Organization name: ' +
            AsX400Address.Built_In_Standard_Attributes.Organization_Name + crlf;
          Result := Result + '    Numeric user identifier: ' +
            AsX400Address.Built_In_Standard_Attributes.Numeric_User_Identifier + crlf;
          Result := Result + '    Network address: ' +
            AsX400Address.Built_In_Standard_Attributes.Personal_Name.Given_Name + ' ' +
            AsX400Address.Built_In_Standard_Attributes.Personal_Name.Initials + ' ' +
            AsX400Address.Built_In_Standard_Attributes.Personal_Name.Surname + ' ' +
            AsX400Address.Built_In_Standard_Attributes.Personal_Name.Generation_Qualifier + crlf;
          Result := Result + '    Organizational unit names: ';
          for J := 0 to AsX400Address.Built_In_Standard_Attributes.Organizational_Unit_Names.Count - 1 do
            Result := Result + '      ' + AsX400Address.Built_In_Standard_Attributes.Organizational_Unit_Names[J];
          for J := 0 to AsX400Address.Built_In_Domain_Defined_Attributes.Count - 1 do
            Result := Result + '      ' + AsX400Address.Built_In_Domain_Defined_Attributes.Items[J].TypeName + ': ' +
              AsX400Address.Built_In_Domain_Defined_Attributes.Items[J].Value + crlf;
          for J := 0 to AsX400Address.Extension_Attributes.Count - 1 do
            Result := Result + '      ' + AsX400Address.Extension_Attributes.Items[J].TypeName + ': ' +
              AsX400Address.Extension_Attributes.Items[J].Extension_Attribute_Value.Value + crlf;
        end;
    gneDirectoryName:
      begin
       for J := 0 to AsDirectoryName.AsRdnSequence.Count - 1 do
         for K := 0 to AsDirectoryName.AsRdnSequence.Items[J].ItemCount - 1 do
           Result := Result + '    ' +  GetObjectName(AsDirectoryName.AsRdnSequence.Items[J].Items[K].AttrType) + ': ' +
             AsDirectoryName.AsRdnSequence.Items[J].Items[K].AttrValue.Value + crlf;
      end;
    gneEdiPartyName:
      Result := '  EDI party name: ' + crlf +
                '    Name assigner: ' +
        AsEdiPartyName.NameAssigner.Value + crlf +
                '    Party name: ' +
        AsEdiPartyName.PartyName.Value + crlf;
    gneUniformResourceIdentifier:
      Result := '  URI: ' + AsUniformResourceIdentifier + crlf;
    gneIPAddress:
      Result := '  IP address: ' + FormatIPAddress(AsIPAddress) + crlf;
    gneRegisteredID:
      Result := '  Registred ID: ' + AsRegisteredID + crlf;
  else
    Result := '  (Unknown field)' + crlf;
  end;
end;

{ TGeneralNames }

constructor TGeneralNames.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TGeneralName;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated;
    F := F.FindField('authorityCertIssuer')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TGeneralNames.Destroy;
begin
  inherited Destroy;
end;

function TGeneralNames.Add: TGeneralName;
begin
  Result := TGeneralName(InternalAdd);
end;

function TGeneralNames.GetItems(Index: Integer): TGeneralName;
begin
  Result := TGeneralName(InternalGetItems(Index));
end;

function TGeneralNames.Compare(AltName: TGeneralNames): Boolean;
begin
  Result := ASN1StructCompare(FData,AltName.FData);
end;

function TGeneralNames.GetText: WideString;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + Items[I].GetText;
end;

function TGeneralNames.GetURI: string;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Choice = gneUniformResourceIdentifier then begin
      Result := Items[I].AsUniformResourceIdentifier;
      Exit;
    end;
  Result := '';
end;

procedure TGeneralNames.SetURI(const Value: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Choice = gneUniformResourceIdentifier then begin
      Items[I].AsUniformResourceIdentifier := Value;
      Exit;
    end;
  with Add do begin
    Choice := gneUniformResourceIdentifier;
    AsUniformResourceIdentifier := Value;
  end;
end;

function TGeneralNames.GetDnsName: string;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Choice = gneDNSName then begin
      Result := Items[I].GetAsDNSName;
      Exit;
    end;
  Result := '';
end;

function TGeneralNames.GetIPAddress: string;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Choice = gneIPAddress then begin
      Result := FormatIPAddress(Items[I].AsIPAddress);
      Exit;
    end;
  Result := '';
end;

function TGeneralNames.GetRfc822Name: string;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Choice = gneRfc822Name then begin
      Result := Items[I].GetAsRfc822Name;
      Exit;
    end;
  Result := '';
end;

procedure TGeneralNames.SetDnsName(const Value: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Choice = gneDnsName then begin
      Items[I].AsDNSName := Value;
      Exit;
    end;
  with Add do begin
    Choice := gneDnsName;
    AsDnsName := Value;
  end;
end;

procedure TGeneralNames.SetIPAddress(const Value: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Choice = gneIPAddress then begin
      ParseIPAddress(Value,Items[I].AsIPAddress);
      Exit;
    end;
  with Add do begin
    Choice := gneIPAddress;
    ParseIPAddress(Value,AsIPAddress);
  end;
end;

procedure TGeneralNames.SetRfc822Name(const Value: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Choice = gneRfc822Name then begin
      Items[I].AsRfc822Name := Value;
      Exit;
    end;
  with Add do begin
    Choice := gneRfc822Name;
    AsRfc822Name := Value;
  end;
end;

{ TAuthorityKeyIdentifier }

constructor TAuthorityKeyIdentifier.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end else if FData.Encapsulated = nil then begin
    FData.CreateEncapsulated;
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[0]^.Encapsulated;
    FData.Encapsulated.CopyTypeInfo(F);
  end;
  FieldFactory(TOctetString,nil,FData.Encapsulated.Items[0]^,nil^);
  FieldFactory(TGeneralNames,nil,FData.Encapsulated.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Encapsulated.Items[2]^,nil^);
end;

destructor TAuthorityKeyIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TAuthorityKeyIdentifier.GetKeyIdentifier: TOctetString;
begin
  Result := FItems[0];
end;

procedure TAuthorityKeyIdentifier.SetKeyIdentifier(const Value: TOctetString);
begin
  TOctetString(FItems[0]).AsString := Value.AsString;
end;

function TAuthorityKeyIdentifier.GetAuthorityCertIssuer: TGeneralNames;
begin
  Result := FItems[1];
end;

procedure TAuthorityKeyIdentifier.SetAuthorityCertIssuer(const Value: TGeneralNames);
begin
  TGeneralNames(FItems[1]).Assign(Value);
end;

function TAuthorityKeyIdentifier.GetAuthorityCertSerialNumber: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TAuthorityKeyIdentifier.SetAuthorityCertSerialNumber(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

{ TExtKeyUsageSyntax }

constructor TExtKeyUsageSyntax.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TStringWrapper;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[3]^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TExtKeyUsageSyntax.Destroy;
begin
  inherited Destroy;
end;

function TExtKeyUsageSyntax.Add: TStringWrapper;
begin
  Result := TStringWrapper(InternalAdd);
end;

procedure TExtKeyUsageSyntax.AddValue(const Value: ObjectIdentifier);
var
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  for I := 0 to Count - 1 do begin
    Found := Values[I] = Value;
    if Found then Break;
  end;
  if not Found then
    TStringWrapper(InternalAdd).Value := Value;
end;

function TExtKeyUsageSyntax.GetItems(Index: Integer): TStringWrapper;
begin
  Result := TStringWrapper(InternalGetItems(Index));
end;

function TExtKeyUsageSyntax.GetValues(Index: Integer): ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(InternalGetItems(Index)).Value);
end;

procedure TExtKeyUsageSyntax.SetValues(Index: Integer; const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[Index]).Value := Value;
end;

{ TPrivateKeyUsagePeriod }

constructor TPrivateKeyUsagePeriod.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[4]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TGeneralizedTime,nil,FData.Encapsulated.Items[0]^,nil^);
  FieldFactory(TGeneralizedTime,nil,FData.Encapsulated.Items[1]^,nil^);
end;

destructor TPrivateKeyUsagePeriod.Destroy;
begin
  inherited Destroy;
end;

function TPrivateKeyUsagePeriod.GetNotBefore: TGeneralizedTime;
begin
  Result := FItems[0];
end;

procedure TPrivateKeyUsagePeriod.SetNotBefore(const Value: TGeneralizedTime);
begin
  TGeneralizedTime(FItems[0]).Assign(Value);
end;

function TPrivateKeyUsagePeriod.GetNotAfter: TGeneralizedTime;
begin
  Result := FItems[1];
end;

procedure TPrivateKeyUsagePeriod.SetNotAfter(const Value: TGeneralizedTime);
begin
  TGeneralizedTime(FItems[1]).Assign(Value);
end;

{ TDisplayText }

constructor TDisplayText.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[5]^.Encapsulated.Template.FindField('policyQualifiers')^.Template.FindField('qualifier')^.Choices[1]^;
    F := F.FindField('/noticeRef/organization')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TWideStringWrapper);
  FChoiceList.Add(TWideStringWrapper);
end;

destructor TDisplayText.Destroy;
begin
  inherited Destroy;
end;

function TDisplayText.GetAsVisibleString: VisibleString;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TDisplayText.SetAsVisibleString(const Value: VisibleString);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TDisplayText.GetAsBmpString: WideString;
begin
  Result := (FSelected as TWideStringWrapper).Value;
end;

procedure TDisplayText.SetAsBmpString(const Value: WideString);
begin
  InternalSelectChoice(1);
  (FSelected as TWideStringWrapper).Value := Value;
end;

function TDisplayText.GetAsUtf8String: WideString;
begin
  Result := (FSelected as TWideStringWrapper).Value;
end;

procedure TDisplayText.SetAsUtf8String(const Value: WideString);
begin
  InternalSelectChoice(2);
  (FSelected as TWideStringWrapper).Value := Value;
end;

function TDisplayText.GetChoice: TDisplayTextEnum;
begin
  Result := TDisplayTextEnum(InternalGetChoice);
end;

procedure TDisplayText.SetChoice(const Value: TDisplayTextEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TSequenceOfInteger }

constructor TSequenceOfInteger.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TIntegerWrapper;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[5]^.Encapsulated.Template.FindField('policyQualifiers')^.Template.FindField('qualifier')^.Choices[1]^;
    F := F.FindField('/noticeRef/noticeNumbers')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSequenceOfInteger.Destroy;
begin
  inherited Destroy;
end;

function TSequenceOfInteger.Add: TIntegerWrapper;
begin
  Result := TIntegerWrapper(InternalAdd);
end;

function TSequenceOfInteger.GetItems(Index: Integer): TIntegerWrapper;
begin
  Result := TIntegerWrapper(InternalGetItems(Index));
end;

{ TNoticeReference }

constructor TNoticeReference.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[5]^.Encapsulated.Template.FindField('policyQualifiers')^.Template.FindField('qualifier')^.Choices[1]^;
    F := F.FindField('noticeRef')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TDisplayText,nil,FData.Items[0]^,nil^);
  FieldFactory(TSequenceOfInteger,nil,FData.Items[1]^,nil^);
end;

destructor TNoticeReference.Destroy;
begin
  inherited Destroy;
end;

function TNoticeReference.GetOrganization: TDisplayText;
begin
  Result := FItems[0];
end;

procedure TNoticeReference.SetOrganization(const Value: TDisplayText);
begin
  TDisplayText(FItems[0]).Assign(Value);
end;

function TNoticeReference.GetNoticeNumbers: TSequenceOfInteger;
begin
  Result := FItems[1];
end;

procedure TNoticeReference.SetNoticeNumbers(const Value: TSequenceOfInteger);
begin
  TSequenceOfInteger(FItems[1]).Assign(Value);
end;

{ TUserNotice }

constructor TUserNotice.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[5]^.Encapsulated.Template.FindField('policyQualifiers')^.Template.FindField('qualifier')^.Choices[1]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TNoticeReference,nil,FData.Items[0]^,nil^);
  FieldFactory(TDisplayText,nil,FData.Items[1]^,nil^);
end;

destructor TUserNotice.Destroy;
begin
  inherited Destroy;
end;

function TUserNotice.GetNoticeRef: TNoticeReference;
begin
  Result := FItems[0];
end;

procedure TUserNotice.SetNoticeRef(const Value: TNoticeReference);
begin
  TNoticeReference(FItems[0]).Assign(Value);
end;

function TUserNotice.GetExplicitText: TDisplayText;
begin
  Result := FItems[1];
end;

procedure TUserNotice.SetExplicitText(const Value: TDisplayText);
begin
  TDisplayText(FItems[1]).Assign(Value);
end;

{ TQualifier }

constructor TQualifier.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[5]^.Encapsulated.Template.FindField('policyQualifiers')^.Template;
    F := F.FindField('qualifier')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TUserNotice);
end;

destructor TQualifier.Destroy;
begin
  inherited Destroy;
end;

function TQualifier.GetAsQt_Cps: IA5String;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TQualifier.SetAsQt_Cps(const Value: IA5String);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TQualifier.GetAsQt_Unotice: TUserNotice;
begin
  Result := (FSelected as TUserNotice);
end;

procedure TQualifier.SetAsQt_Unotice(const Value: TUserNotice);
begin
  InternalSelectChoice(1);
  (FSelected as TUserNotice).Assign(Value);
end;

function TQualifier.GetChoice: TQualifierEnum;
begin
  Result := TQualifierEnum(InternalGetChoice);
end;

procedure TQualifier.SetChoice(const Value: TQualifierEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TPolicyQualifierInfo }

constructor TPolicyQualifierInfo.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[5]^.Encapsulated.Template.FindField('policyQualifiers')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TQualifier,nil,FData.Items[1]^,nil^);
end;

destructor TPolicyQualifierInfo.Destroy;
begin
  inherited Destroy;
end;

function TPolicyQualifierInfo.GetPolicyQualifierId: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TPolicyQualifierInfo.SetPolicyQualifierId(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TPolicyQualifierInfo.GetQualifier: TQualifier;
begin
  Result := FItems[1];
end;

procedure TPolicyQualifierInfo.SetQualifier(const Value: TQualifier);
begin
  TQualifier(FItems[1]).Assign(Value);
end;

function TPolicyQualifierInfo.IsType(Id: TQualifierEnum): Boolean;
begin
  Result := TQualifier(FItems[1]).Choice = Id;
end;

procedure TPolicyQualifierInfo.SetType(Id: TQualifierEnum);
begin
  TQualifier(FItems[1]).Choice := Id;
end;

{ TSequenceOfPolicyQualifierInfo }

constructor TSequenceOfPolicyQualifierInfo.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TPolicyQualifierInfo;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[5]^.Encapsulated.Template;
    F := F.FindField('policyQualifiers')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSequenceOfPolicyQualifierInfo.Destroy;
begin
  inherited Destroy;
end;

function TSequenceOfPolicyQualifierInfo.GetUniqueItem(Id: TQualifierEnum): TPolicyQualifierInfo;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do begin
    Result := Items[I];
    if Result.IsType(Id) then
      Exit;
  end;
  Result := nil;
end;

function TSequenceOfPolicyQualifierInfo.AddUniqueItem(Id: TQualifierEnum): TPolicyQualifierInfo;
begin
  Result := GetUniqueItem(Id);
  if Result = nil then begin
    Result := Add;
    Result.SetType(Id);
  end;
end;

function TSequenceOfPolicyQualifierInfo.Add: TPolicyQualifierInfo;
begin
  Result := TPolicyQualifierInfo(InternalAdd);
end;

function TSequenceOfPolicyQualifierInfo.GetItems(Index: Integer): TPolicyQualifierInfo;
begin
  Result := TPolicyQualifierInfo(InternalGetItems(Index));
end;

{ TPolicyInformation }

constructor TPolicyInformation.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[5]^.Encapsulated.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TSequenceOfPolicyQualifierInfo,nil,FData.Items[1]^,nil^);
end;

destructor TPolicyInformation.Destroy;
begin
  inherited Destroy;
end;

function TPolicyInformation.GetPolicyIdentifier: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TPolicyInformation.SetPolicyIdentifier(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TPolicyInformation.GetPolicyQualifiers: TSequenceOfPolicyQualifierInfo;
begin
  Result := FItems[1];
end;

procedure TPolicyInformation.SetPolicyQualifiers(const Value: TSequenceOfPolicyQualifierInfo);
begin
  TSequenceOfPolicyQualifierInfo(FItems[1]).Assign(Value);
end;

{ TCertificatePolicies }

constructor TCertificatePolicies.Create;
var
  F: TASN1Struct;
begin
  FFieldType := TPolicyInformation;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[5]^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TCertificatePolicies.Destroy;
begin
  inherited Destroy;
end;

function TCertificatePolicies.Add: TPolicyInformation;
begin
  Result := TPolicyInformation(InternalAdd);
end;

function TCertificatePolicies.GetItems(Index: Integer): TPolicyInformation;
begin
  Result := TPolicyInformation(InternalGetItems(Index));
end;

{ TPolicyMapping }

constructor TPolicyMapping.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[6]^.Encapsulated.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TStringWrapper,nil,FData.Items[1]^,nil^);
end;

destructor TPolicyMapping.Destroy;
begin
  inherited Destroy;
end;

function TPolicyMapping.GetIssuerDomainPolicy: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TPolicyMapping.SetIssuerDomainPolicy(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TPolicyMapping.GetSubjectDomainPolicy: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[1]).Value);
end;

procedure TPolicyMapping.SetSubjectDomainPolicy(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[1]).Value := Value;
end;

{ TPolicyMappingsSyntax }

constructor TPolicyMappingsSyntax.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TPolicyMapping;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[6]^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TPolicyMappingsSyntax.Destroy;
begin
  inherited Destroy;
end;

function TPolicyMappingsSyntax.Add: TPolicyMapping;
begin
  Result := TPolicyMapping(InternalAdd);
end;

function TPolicyMappingsSyntax.GetItems(Index: Integer): TPolicyMapping;
begin
  Result := TPolicyMapping(InternalGetItems(Index));
end;

{ TBasicConstraintsSyntax }

constructor TBasicConstraintsSyntax.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[9]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TBooleanWrapper,nil,FData.Encapsulated.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Encapsulated.Items[1]^,nil^);
end;

destructor TBasicConstraintsSyntax.Destroy;
begin
  inherited Destroy;
end;

function TBasicConstraintsSyntax.GetCA: Boolean;
begin
  Result := Boolean(TBooleanWrapper(FItems[0]).Value);
end;

procedure TBasicConstraintsSyntax.SetCA(const Value: Boolean);
begin
  TBooleanWrapper(FItems[0]).Value := Value;
end;

function TBasicConstraintsSyntax.GetPathLenConstraint: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TBasicConstraintsSyntax.SetPathLenConstraint(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

{ TGeneralSubtree }

constructor TGeneralSubtree.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[10]^.Encapsulated.FindField('permittedSubtrees')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TGeneralName,nil,FData.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
end;

destructor TGeneralSubtree.Destroy;
begin
  inherited Destroy;
end;

function TGeneralSubtree.GetBase: TGeneralName;
begin
  Result := FItems[0];
end;

procedure TGeneralSubtree.SetBase(const Value: TGeneralName);
begin
  TGeneralName(FItems[0]).Assign(Value);
end;

function TGeneralSubtree.GetMinimum: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TGeneralSubtree.SetMinimum(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

function TGeneralSubtree.GetMaximum: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TGeneralSubtree.SetMaximum(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;
                                 
type
  TCharSet = set of Char;

function CompareURI(S0, S1: string; const Delim: TCharSet): Boolean;
begin
  if S1 = '' then
    Result := True
  else if Length(S0) = Length(S1) then
    Result := StrIComp(PChar(S0),PChar(S1)) = 0
  else if Length(S0) < Length(S1) then
    Result := False
  else begin
    if (S0[Length(S0) - Length(S1)] in Delim) or
       (S1[1] = '.') then
      Result := StrLIComp(PChar(S1),
                          PChar(S0) + Length(S0) - Length(S1),
                          Length(S1)) = 0
    else
      Result := False
  end;
end;

function TGeneralSubtree.Applies(Name: TName; AltName: TGeneralNames;
  out Missing: Boolean): Boolean;
var
  I, J, K: Integer;
  ATV: TAttributeTypeAndValue;
  S0, S1: string;
  ExtnAttr: TExtensionAttribute;
  AttrValue, AttrValue0: TAttributeTypeAndValue;
begin
  Result := False;
  case Base.Choice of
    gneUndefined,
    gneOtherName,
    gneEdiPartyName,
    gneRegisteredID:
      begin
        Missing := False;
        Result := False; // Not defined by X.509.
      end;
    gneRfc822Name:
      begin
        Result := False;
        Missing := True;
        // Email address constraint.
        ATV := Name.AsRdnSequence.UniqueItem[aveEmailAddress];
        if Assigned(ATV) then begin
          Missing := False;
          S0 := ATV.AttrValue.AsEmailAddress.Value;
          S1 := Base.AsRfc822Name;
          Result := CompareURI(S0,S1,['.','@']);
          Exit;
        end;
        if AltName = nil then Exit;
        for I := 0 to AltName.Count - 1 do
          if AltName.Items[I].Choice = gneRfc822Name then begin
            Missing := False;
            S0 := AltName.Items[I].AsRfc822Name;
            S1 := Base.AsRfc822Name;
            Result := CompareURI(S0,S1,['.','@']);
            Break;
          end;
      end;
    gneDNSName:
      begin
        Result := False;
        Missing := True;
        if AltName = nil then Exit;
        for I := 0 to AltName.Count - 1 do
          if AltName.Items[I].Choice = gneDNSName then begin
            Missing := False;
            S0 := AltName.Items[I].AsDNSName;
            S1 := Base.AsDNSName;
            Result := CompareURI(S0,S1,['.']);
            Break;
          end;
      end;
    gneX400Address:
      begin
        Result := False;
        Missing := True;
        if AltName = nil then Exit;
        for I := 0 to AltName.Count - 1 do
          if AltName.Items[I].Choice = gneX400Address then begin
            Missing := False;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Country_Name.Value;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Country_Name.Value;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Administration_Domain_Name.Value;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Administration_Domain_Name.Value;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Network_Address;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Network_Address;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Terminal_Identifier;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Terminal_Identifier;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Private_Domain_Name.Value;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Private_Domain_Name.Value;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Organization_Name;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Organization_Name;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Numeric_User_Identifier;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Numeric_User_Identifier;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Personal_Name.Surname;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Surname;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Personal_Name.Given_Name;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Given_Name;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Personal_Name.Initials;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Initials;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Personal_Name.Generation_Qualifier;
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Generation_Qualifier;
            if S1 <> '' then begin
              Result := S0 = S1;
              if not Result then Exit;
            end;
            for J := 0 to AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Organizational_Unit_Names.Count - 1 do begin
              S0 := AltName.Items[I].AsX400Address.Built_In_Standard_Attributes.Organizational_Unit_Names.Values[J];
              S1 := '';
              for K := 0 to Base.AsX400Address.Built_In_Standard_Attributes.Organizational_Unit_Names.Count - 1 do begin
                S1 := Base.AsX400Address.Built_In_Standard_Attributes.Organizational_Unit_Names.Values[K];
                Result := S0 = S1;
                if Result then Break;
              end;
              if S1 = '' then Break;
              if not Result then Exit;
            end;
            for J := 0 to AltName.Items[I].AsX400Address.Built_In_Domain_Defined_Attributes.Count - 1 do begin
              S0 := AltName.Items[I].AsX400Address.Built_In_Domain_Defined_Attributes.Items[J].TypeName;
              S1 := AltName.Items[I].AsX400Address.Built_In_Domain_Defined_Attributes.Items[J].Value;
              for K := 0 to Base.AsX400Address.Built_In_Domain_Defined_Attributes.Count - 1 do
                if Base.AsX400Address.Built_In_Domain_Defined_Attributes.Items[K].TypeName = S0 then begin
                  Result := S1 = Base.AsX400Address.Built_In_Domain_Defined_Attributes.Items[K].Value;
                  Break;
                end;
              if not Result then Exit;
            end;
            for J := 0 to AltName.Items[I].AsX400Address.Extension_Attributes.Count - 1 do begin
              S0 := AltName.Items[I].AsX400Address.Extension_Attributes.Items[J].Extension_Attribute_Value.Value;
              ExtnAttr := Base.AsX400Address.Extension_Attributes.UniqueItem[AltName.Items[I].AsX400Address.Extension_Attributes.Items[J].Extension_Attribute_Value.Choice];
              if Assigned(ExtnAttr) then begin
                S1 := ExtnAttr.Extension_Attribute_Value.Value;
                Result := S0 = S1;
                if not Result then Exit;
              end;
            end;
            Break;
          end;
      end;
    gneDirectoryName:
      begin
        Result := False;
        Missing := False;
        for I := 0 to Name.AsRdnSequence.Count - 1 do begin
          for J := 0 to Name.AsRdnSequence.Items[I].Count - 1 do begin
            AttrValue0 := Name.AsRdnSequence.Items[I].Items[J];
            S0 := AttrValue0.AttrValue.Value;
            AttrValue := Base.AsDirectoryName.AsRdnSequence.UniqueItem[AttrValue0.AttrValue.Choice];
            if Assigned(AttrValue) then begin
              S1 := AttrValue.AttrValue.Value;
              if (AttrValue.AttrValue.Data.Tag = V_ASN1_PRINTABLESTRING) or
                 (AttrValue0.AttrValue.Data.Tag = V_ASN1_PRINTABLESTRING) then
                Result := CompareText(TrimPrintable(S0),TrimPrintable(S1)) = 0
              else
                Result := S0 = S1;
              if not Result then Exit;
            end;
          end;
        end;
        if AltName = nil then Exit;
        for I := 0 to AltName.Count - 1 do
          if AltName.Items[I].Choice = gneDirectoryName then begin
            for J := 0 to AltName.Items[I].AsDirectoryName.AsRdnSequence.Count - 1 do begin
              for K := 0 to AltName.Items[I].AsDirectoryName.AsRdnSequence.Items[J].Count - 1 do begin
                AttrValue0 := AltName.Items[I].AsDirectoryName.AsRdnSequence.Items[J].Items[K];
                S0 := AttrValue0.AttrValue.Value;
                AttrValue := Base.AsDirectoryName.AsRdnSequence.UniqueItem[AttrValue0.AttrValue.Choice];
                if Assigned(AttrValue) then begin
                  S1 := AttrValue.AttrValue.Value;
                  if (AttrValue.AttrValue.Data.Tag = V_ASN1_PRINTABLESTRING) or
                     (AttrValue0.AttrValue.Data.Tag = V_ASN1_PRINTABLESTRING) then
                    Result := CompareText(TrimPrintable(S0),TrimPrintable(S1)) = 0
                  else
                    Result := S0 = S1;
                  if not Result then Exit;
                end;
              end;
            end;
          end;
      end;
    gneUniformResourceIdentifier:
      begin
        Result := False;
        Missing := True;
        if AltName = nil then Exit;
        for I := 0 to AltName.Count - 1 do
          if AltName.Items[I].Choice = gneUniformResourceIdentifier then begin
            Missing := False;
            S0 := AltName.Items[I].AsUniformResourceIdentifier;
            S1 := Base.AsUniformResourceIdentifier;
            for J := 1 to Length(S0) do
              if S0[J] = ':' then begin
                K := J;
                while (K < Length(S0)) and (S0[K] = '/') do
                  Inc(K);
                Delete(S0,1,K-1);
                K := 1;
                while (K <= Length(S0)) and not (S0[K] in [':','/']) do
                  Inc(K);
                S0 := Copy(S0,1,K-1);
                Break;
              end;
            Result := CompareURI(S0,S1,['.']);
            Break;
          end;
      end;
    gneIPAddress:
      begin
        Result := False;
        Missing := True;
        if AltName = nil then Exit;
        for I := 0 to AltName.Count - 1 do
          if AltName.Items[I].Choice = gneIPAddress then begin
            Missing := False;
            S0 := AltName.Items[I].AsIPAddress.AsString;
            S1 := Base.AsIPAddress.AsString;
            Result := (Length(S0) = 4) and (Length(S1) = 8);
            if Result then
              for J := 1 to 4 do begin
                if S1[J + 4] = #$FF then
                  Result := S0[J] = S1[J]
                else if S1[J + 4] = #0 then
                  Break;
                if not Result then Exit;
              end;
            Break;
          end;
      end;
  end;
end;

function TGeneralSubtree.AppliesBase(ABase: TGeneralName): Boolean;
var
  J, K: Integer;
  S0, S1: string;
  ExtnAttr: TExtensionAttribute;
  AttrValue, AttrValue0: TAttributeTypeAndValue;
begin
  Result := Base.Choice = ABase.Choice;
  if Result then case Base.Choice of
    gneUndefined,
    gneOtherName,
    gneEdiPartyName,
    gneRegisteredID:
      begin
        Result := False; // Not defined by X.509.
      end;
    gneRfc822Name:
      begin
        // Email address constraint.
        S0 := ABase.AsRfc822Name;
        S1 := Base.AsRfc822Name;
        Result := CompareURI(S0,S1,['.','@']);
      end;
    gneDNSName:
      begin
        S0 := ABase.AsDNSName;
        S1 := Base.AsDNSName;
        Result := CompareURI(S0,S1,['.']);
      end;
    gneX400Address:
      begin
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Country_Name.Value;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Country_Name.Value;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Administration_Domain_Name.Value;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Administration_Domain_Name.Value;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Network_Address;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Network_Address;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Terminal_Identifier;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Terminal_Identifier;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Private_Domain_Name.Value;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Private_Domain_Name.Value;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Organization_Name;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Organization_Name;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Numeric_User_Identifier;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Numeric_User_Identifier;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Surname;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Surname;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Given_Name;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Given_Name;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Initials;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Initials;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Generation_Qualifier;
        S1 := Base.AsX400Address.Built_In_Standard_Attributes.Personal_Name.Generation_Qualifier;
        if S1 <> '' then begin
          Result := S0 = S1;
          if not Result then Exit;
        end;
        for J := 0 to ABase.AsX400Address.Built_In_Standard_Attributes.Organizational_Unit_Names.Count - 1 do begin
          S0 := ABase.AsX400Address.Built_In_Standard_Attributes.Organizational_Unit_Names.Values[J];
          S1 := '';
          for K := 0 to Base.AsX400Address.Built_In_Standard_Attributes.Organizational_Unit_Names.Count - 1 do begin
            S1 := Base.AsX400Address.Built_In_Standard_Attributes.Organizational_Unit_Names.Values[K];
            Result := S0 = S1;
            if Result then Break;
          end;
          if S1 = '' then Break;
          if not Result then Exit;
        end;
        for J := 0 to ABase.AsX400Address.Built_In_Domain_Defined_Attributes.Count - 1 do begin
          S0 := ABase.AsX400Address.Built_In_Domain_Defined_Attributes.Items[J].TypeName;
          S1 := ABase.AsX400Address.Built_In_Domain_Defined_Attributes.Items[J].Value;
          for K := 0 to Base.AsX400Address.Built_In_Domain_Defined_Attributes.Count - 1 do
            if Base.AsX400Address.Built_In_Domain_Defined_Attributes.Items[K].TypeName = S0 then begin
              Result := S1 = Base.AsX400Address.Built_In_Domain_Defined_Attributes.Items[K].Value;
              Break;
            end;
          if not Result then Exit;
        end;
        for J := 0 to ABase.AsX400Address.Extension_Attributes.Count - 1 do begin
          S0 := ABase.AsX400Address.Extension_Attributes.Items[J].Extension_Attribute_Value.Value;
          ExtnAttr := Base.AsX400Address.Extension_Attributes.UniqueItem[ABase.AsX400Address.Extension_Attributes.Items[J].Extension_Attribute_Value.Choice];
          if Assigned(ExtnAttr) then begin
            S1 := ExtnAttr.Extension_Attribute_Value.Value;
            Result := S0 = S1;
            if not Result then Exit;
          end;
        end;
      end;
    gneDirectoryName:
      begin
        Result := False;
        for J := 0 to ABase.AsDirectoryName.AsRdnSequence.Count - 1 do begin
          for K := 0 to ABase.AsDirectoryName.AsRdnSequence.Items[J].Count - 1 do begin
            AttrValue0 := ABase.AsDirectoryName.AsRdnSequence.Items[J].Items[K];
            S0 := AttrValue0.AttrValue.Value;
            AttrValue := Base.AsDirectoryName.AsRdnSequence.UniqueItem[AttrValue0.AttrValue.Choice];
            if Assigned(AttrValue) then begin
              S1 := AttrValue.AttrValue.Value;
              if (AttrValue.AttrValue.Data.Tag = V_ASN1_PRINTABLESTRING) or
                 (AttrValue0.AttrValue.Data.Tag = V_ASN1_PRINTABLESTRING) then
                Result := CompareText(TrimPrintable(S0),TrimPrintable(S1)) = 0
              else
                Result := S0 = S1;
              if not Result then Exit;
            end;
          end;
        end;
      end;
    gneUniformResourceIdentifier:
      begin
        S0 := ABase.AsUniformResourceIdentifier;
        S1 := Base.AsUniformResourceIdentifier;
        for J := 1 to Length(S0) do
          if S0[J] = ':' then begin
            K := J;
            while (K < Length(S0)) and (S0[K] = '/') do
              Inc(K);
            Delete(S0,1,K-1);
            K := 1;
            while (K <= Length(S0)) and not (S0[K] in [':','/']) do
              Inc(K);
            S0 := Copy(S0,1,K-1);
            Break;
          end;
        Result := CompareURI(S0,S1,['.']);
      end;
    gneIPAddress:
      begin
        S0 := ABase.AsIPAddress.AsString;
        S1 := Base.AsIPAddress.AsString;
        Result := (Length(S0) = 4) and (Length(S1) = 8);
        if Result then
          for J := 1 to 4 do begin
            if S1[J + 4] = #$FF then
              Result := S0[J] = S1[J]
            else if S1[J + 4] = #0 then
              Break;
            if not Result then Exit;
          end
        else begin
          Result := (Length(S0) = 8) and (Length(S1) = 8);
          if Result then
            for J := 1 to 4 do begin
              if S1[J + 4] = #$FF then
                Result := (S0[J] = S1[J]) and (S0[J + 4] = #$FF)
              else if S1[J + 4] = #0 then
                Break;
              if not Result then Exit;
            end;
        end;
      end;
  end;
end;

{ TGeneralSubtrees }

constructor TGeneralSubtrees.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TGeneralSubtree;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[10]^.Encapsulated;
    F := F.FindField('permittedSubtrees')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TGeneralSubtrees.Destroy;
begin
  inherited Destroy;
end;

function TGeneralSubtrees.Add: TGeneralSubtree;
begin
  Result := TGeneralSubtree(InternalAdd);
end;

procedure TGeneralSubtrees.Difference(ADifference: TGeneralSubTrees);
var
  I, J: Integer;
  Match: Boolean;
begin
  for I := ADifference.Count - 1 downto 0 do begin
    Match := False;
    for J := 0 to Count - 1 do begin
      Match := Items[J].AppliesBase(ADifference[I].Base) or
               ASN1StructCompare(ADifference[J].FData,Items[I].FData);
      if Match then Break;
    end;
    if Match then
      ADifference.Delete(I);
  end;
end;

function TGeneralSubtrees.GetItems(Index: Integer): TGeneralSubtree;
begin
  Result := TGeneralSubtree(InternalGetItems(Index));
end;

procedure TGeneralSubtrees.Intersect(AIntersection: TGeneralSubTrees);
var
  I, J: Integer;
  Match: Boolean;
begin
  for I := AIntersection.Count - 1 downto 0 do begin
    Match := False;
    for J := 0 to Count - 1 do begin
      Match := Items[J].AppliesBase(AIntersection[I].Base) or
               ASN1StructCompare(AIntersection[J].FData,Items[I].FData);
      if Match then Break;
    end;
    if not Match then
      AIntersection.Delete(I);
  end;
end;

procedure TGeneralSubtrees.Union(AUnion: TGeneralSubTrees);
var
  I, J, MaxJ: Integer;
  Match: Boolean;
begin
  MaxJ := AUnion.Count;
  for I := Count - 1 downto 0 do begin
    Match := False;
    for J := 0 to MaxJ - 1 do begin
      Match := AUnion[J].AppliesBase(Items[I].Base) or
               ASN1StructCompare(AUnion[J].FData,Items[I].FData);
      if Match then Break;
    end;
    if not Match then
      AUnion.Add.Assign(Items[I]);
  end;
end;

{ TNameConstraintsSyntax }

constructor TNameConstraintsSyntax.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[10]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TGeneralSubtrees,nil,FData.Encapsulated.Items[0]^,nil^);
  FieldFactory(TGeneralSubtrees,nil,FData.Encapsulated.Items[1]^,nil^);
end;

destructor TNameConstraintsSyntax.Destroy;
begin
  inherited Destroy;
end;

function TNameConstraintsSyntax.GetPermittedSubtrees: TGeneralSubtrees;
begin
  Result := FItems[0];
end;

procedure TNameConstraintsSyntax.SetPermittedSubtrees(const Value: TGeneralSubtrees);
begin
  TGeneralSubtrees(FItems[0]).Assign(Value);
end;

function TNameConstraintsSyntax.GetExcludedSubtrees: TGeneralSubtrees;
begin
  Result := FItems[1];
end;

procedure TNameConstraintsSyntax.SetExcludedSubtrees(const Value: TGeneralSubtrees);
begin
  TGeneralSubtrees(FItems[1]).Assign(Value);
end;

function TNameConstraintsSyntax.Verify(Name: TName;
  AltName: TGeneralNames): Boolean;
var
  I: Integer;
  Missing: Boolean;
begin
  Result := (ExcludedSubTrees.Count = 0) =
            (PermittedSubTrees.Count = 0);
  for I := 0 to PermittedSubTrees.Count - 1 do begin
    Result := PermittedSubTrees.Items[I].Applies(Name,AltName,Missing);
    Result := Result or Missing;
    if not Result then Exit;
  end;
  if not Result then Exit;
  for I := 0 to ExcludedSubTrees.Count - 1 do begin
    Result := not ExcludedSubTrees.Items[I].Applies(Name,AltName,Missing);
    Result := Result or Missing;
    if not Result then Exit;
  end;
end;

procedure TNameConstraintsSyntax.Combine(
  ACombination: TNameConstraintsSyntax);
begin
  ExcludedSubTrees.Union(ACombination.ExcludedSubtrees);
  PermittedSubTrees.Intersect(ACombination.PermittedSubtrees);
end;

function TNameConstraintsSyntax.GetText: WideString;
var
  I: Integer;
begin
  Result := '';
  if ExcludedSubTrees.Count > 0 then begin
    Result := '  Excluded sub trees:' + crlf;
    for I := 0 to ExcludedSubTrees.Count - 1 do
      Result := Result + ExcludedSubTrees.Items[I].Base.GetText;
  end;
  if PermittedSubTrees.Count > 0 then begin
    Result := '  Permitted sub trees:' + crlf;
    for I := 0 to PermittedSubTrees.Count - 1 do
      Result := Result + PermittedSubTrees.Items[I].Base.GetText;
  end;
end;

procedure TNameConstraintsSyntax.Compress(
  ACombination: TNameConstraintsSyntax);
begin
  ExcludedSubTrees.Difference(ACombination.ExcludedSubtrees);
  PermittedSubTrees.Intersect(ACombination.PermittedSubtrees);
end;

{ TPolicyConstraintsSyntax }

constructor TPolicyConstraintsSyntax.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[11]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Encapsulated.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Encapsulated.Items[1]^,nil^);
end;

destructor TPolicyConstraintsSyntax.Destroy;
begin
  inherited Destroy;
end;

function TPolicyConstraintsSyntax.GetRequireExplicitPolicy: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TPolicyConstraintsSyntax.SetRequireExplicitPolicy(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TPolicyConstraintsSyntax.GetInhibitPolicyMapping: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TPolicyConstraintsSyntax.SetInhibitPolicyMapping(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

{ TDistributionPointName }

constructor TDistributionPointName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[12]^.Encapsulated.Template;
    F := F.FindField('distributionPoint')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TGeneralNames);
  FChoiceList.Add(TRelativeDistinguishedName);
  if FData.ChoiceCount = 0 then
    FieldFactory(TGeneralNames,nil,FData,FSelected)
  else
    Update;
end;

destructor TDistributionPointName.Destroy;
begin
  inherited Destroy;
end;

function TDistributionPointName.GetAsFullName: TGeneralNames;
begin
  InternalSelectChoice(0);
  Result := (FSelected as TGeneralNames);
end;

procedure TDistributionPointName.SetAsFullName(const Value: TGeneralNames);
begin
  InternalSelectChoice(0);
  (FSelected as TGeneralNames).Assign(Value);
end;

function TDistributionPointName.GetAsNameRelativeToCRLIssuer: TRelativeDistinguishedName;
begin
  InternalSelectChoice(1);
  Result := (FSelected as TRelativeDistinguishedName);
end;

procedure TDistributionPointName.SetAsNameRelativeToCRLIssuer(const Value: TRelativeDistinguishedName);
begin
  InternalSelectChoice(1);
  (FSelected as TRelativeDistinguishedName).Assign(Value);
end;

function TDistributionPointName.GetChoice: TDistributionPointNameEnum;
begin
  if FData.ChoiceCount = 0 then
    Result := dpneFullName
  else
    Result := TDistributionPointNameEnum(InternalGetChoice);
end;

procedure TDistributionPointName.SetChoice(const Value: TDistributionPointNameEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

function TDistributionPointName.GetText: WideString;
var
  I: Integer;
begin
  if Choice = dpneFullName then
    Result := AsFullName.Text
  else begin
    Result := '';
    for I := 0 to AsNameRelativeToCRLIssuer.Count - 1 do
      Result := Result + GetObjectName(AsNameRelativeToCRLIssuer.Items[I].AttrType) + ': ' +
        AsNameRelativeToCRLIssuer.Items[I].AttrValue.Value + crlf;
  end;
end;

{ TDistributionPoint }

constructor TDistributionPoint.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[12]^.Encapsulated.Template;
    FData.CopyTypeInfo(F);
  end;
  F := FData.Items[0]^.Items[0]^;
  if (F.ChoiceCount = 0) and not F.IsEmpty then
    F := F.Items[0]^;
  FieldFactory(TDistributionPointName,nil,F,nil^);
  FieldFactory(TBitString,nil,FData.Items[1]^,nil^);
  FieldFactory(TGeneralNames,nil,FData.Items[2]^,nil^);
end;

destructor TDistributionPoint.Destroy;
begin
  inherited Destroy;
end;

function TDistributionPoint.GetDistributionPoint: TDistributionPointName;
begin
  Result := FItems[0];
end;

procedure TDistributionPoint.SetDistributionPoint(const Value: TDistributionPointName);
begin
  TDistributionPointName(FItems[0]).Assign(Value);
end;

function TDistributionPoint.GetReasons: TBitString;
begin
  Result := FItems[1];
end;

procedure TDistributionPoint.SetReasons(const Value: TBitString);
begin
  TBitString(FItems[1]).Assign(Value);
end;

function TDistributionPoint.GetCRLIssuer: TGeneralNames;
begin
  Result := FItems[2];
end;

procedure TDistributionPoint.SetCRLIssuer(const Value: TGeneralNames);
begin
  TGeneralNames(FItems[2]).Assign(Value);
end;

{ TCrldistributionPointsSyntax }

constructor TCrldistributionPointsSyntax.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TDistributionPoint;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[12]^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TCrldistributionPointsSyntax.Destroy;
begin
  inherited Destroy;
end;

function TCrldistributionPointsSyntax.Add: TDistributionPoint;
begin
  Result := TDistributionPoint(InternalAdd);
end;

function TCrldistributionPointsSyntax.GetItems(Index: Integer): TDistributionPoint;
begin
  Result := TDistributionPoint(InternalGetItems(Index));
end;

{ TSetOfAttrValue }

constructor TSetOfAttrValue.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TAttrValue;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[13]^.Encapsulated.Template;
    F := F.FindField('attributeValue')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSetOfAttrValue.Destroy;
begin
  inherited Destroy;
end;

function TSetOfAttrValue.Add: TAttrValue;
begin
  Result := TAttrValue(InternalAdd);
end;

function TSetOfAttrValue.GetItems(Index: Integer): TAttrValue;
begin
  Result := TAttrValue(InternalGetItems(Index));
end;

{ TAttribute }

constructor TAttribute.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[13]^.Encapsulated.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TSetOfAttrValue,nil,FData.Items[1]^,nil^);
end;

destructor TAttribute.Destroy;
begin
  inherited Destroy;
end;

function TAttribute.GetAttributeType: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TAttribute.SetAttributeType(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TAttribute.GetAttributeValue: TSetOfAttrValue;
begin
  Result := FItems[1];
end;

procedure TAttribute.SetAttributeValue(const Value: TSetOfAttrValue);
begin
  TSetOfAttrValue(FItems[1]).Assign(Value);
end;

function TAttribute.IsType(Id: TAttrValueEnum): Boolean;
begin
  if TASNCustomOFFieldWrapper(FItems[1]).ItemCount > 0 then
    Result := TAttrValue(FItems[1]).Choice = Id
  else
    Result := False;
end;

{ TAttributesSyntax }

constructor TAttributesSyntax.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TAttribute;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[13]^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TAttributesSyntax.Destroy;
begin
  inherited Destroy;
end;

function TAttributesSyntax.Add: TAttribute;
begin
  Result := TAttribute(InternalAdd);
end;

function TAttributesSyntax.GetItems(Index: Integer): TAttribute;
begin
  Result := TAttribute(InternalGetItems(Index));
end;

{ TAccessDescription }

constructor TAccessDescription.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[14]^.Encapsulated.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TGeneralName,nil,FData.Items[1]^,nil^);
end;

destructor TAccessDescription.Destroy;
begin
  inherited Destroy;
end;

function TAccessDescription.GetAccessMethod: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TAccessDescription.SetAccessMethod(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TAccessDescription.GetAccessLocation: TGeneralName;
begin
  Result := FItems[1];
end;

procedure TAccessDescription.SetAccessLocation(const Value: TGeneralName);
begin
  TGeneralName(FItems[1]).Assign(Value);
end;

{ TAuthorityInfoAccessSyntax }

constructor TAuthorityInfoAccessSyntax.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TAccessDescription;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template.FindField('extnValue')^.Choices[14]^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TAuthorityInfoAccessSyntax.Destroy;
begin
  inherited Destroy;
end;

function TAuthorityInfoAccessSyntax.Add: TAccessDescription;
begin
  Result := TAccessDescription(InternalAdd);
end;

function TAuthorityInfoAccessSyntax.GetItems(Index: Integer): TAccessDescription;
begin
  Result := TAccessDescription(InternalGetItems(Index));
end;

{ TExtnValue }

constructor TExtnValue.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template;
    F := F.FindField('extnValue')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TAuthorityKeyIdentifier);
  FChoiceList.Add(TOctetString);
  FChoiceList.Add(TBitString);
  FChoiceList.Add(TExtKeyUsageSyntax);
  FChoiceList.Add(TPrivateKeyUsagePeriod);
  FChoiceList.Add(TCertificatePolicies);
  FChoiceList.Add(TPolicyMappingsSyntax);
  FChoiceList.Add(TGeneralNames);
  FChoiceList.Add(TGeneralNames);
  FChoiceList.Add(TBasicConstraintsSyntax);
  FChoiceList.Add(TNameConstraintsSyntax);
  FChoiceList.Add(TPolicyConstraintsSyntax);
  FChoiceList.Add(TCrldistributionPointsSyntax);
  FChoiceList.Add(TAttributesSyntax);
  FChoiceList.Add(TAuthorityInfoAccessSyntax);
  FChoiceList.Add(TBitString);
end;

destructor TExtnValue.Destroy;
begin
  inherited Destroy;
end;

function TExtnValue.GetAsCe_AuthorityKeyIdentifier: TAuthorityKeyIdentifier;
begin
  Result := (FSelected as TAuthorityKeyIdentifier);
end;

procedure TExtnValue.SetAsCe_AuthorityKeyIdentifier(const Value: TAuthorityKeyIdentifier);
begin
  InternalSelectChoice(0);
  (FSelected as TAuthorityKeyIdentifier).Assign(Value);
end;

function TExtnValue.GetAsCe_SubjectKeyIdentifier: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TExtnValue.SetAsCe_SubjectKeyIdentifier(const Value: TOctetString);
begin
  InternalSelectChoice(1);
  (FSelected as TOctetString).Assign(Value);
end;

function TExtnValue.GetAsCe_KeyUsage: TBitString;
begin
  Result := (FSelected as TBitString);
end;

procedure TExtnValue.SetAsCe_KeyUsage(const Value: TBitString);
begin
  InternalSelectChoice(2);
  (FSelected as TBitString).Assign(Value);
end;

function TExtnValue.GetAsCe_ExtKeyUsage: TExtKeyUsageSyntax;
begin
  Result := (FSelected as TExtKeyUsageSyntax);
end;

procedure TExtnValue.SetAsCe_ExtKeyUsage(const Value: TExtKeyUsageSyntax);
begin
  InternalSelectChoice(3);
  (FSelected as TExtKeyUsageSyntax).Assign(Value);
end;

function TExtnValue.GetAsCe_PrivateKeyUsagePeriod: TPrivateKeyUsagePeriod;
begin
  Result := (FSelected as TPrivateKeyUsagePeriod);
end;

procedure TExtnValue.SetAsCe_PrivateKeyUsagePeriod(const Value: TPrivateKeyUsagePeriod);
begin
  InternalSelectChoice(4);
  (FSelected as TPrivateKeyUsagePeriod).Assign(Value);
end;

function TExtnValue.GetAsCe_CertificatePolicies: TCertificatePolicies;
begin
  Result := (FSelected as TCertificatePolicies);
end;

procedure TExtnValue.SetAsCe_CertificatePolicies(const Value: TCertificatePolicies);
begin
  InternalSelectChoice(5);
  (FSelected as TCertificatePolicies).Assign(Value);
end;

function TExtnValue.GetAsCe_PolicyMappings: TPolicyMappingsSyntax;
begin
  Result := (FSelected as TPolicyMappingsSyntax);
end;

procedure TExtnValue.SetAsCe_PolicyMappings(const Value: TPolicyMappingsSyntax);
begin
  InternalSelectChoice(6);
  (FSelected as TPolicyMappingsSyntax).Assign(Value);
end;

function TExtnValue.GetAsCe_SubjectAltName: TGeneralNames;
begin
  Result := (FSelected as TGeneralNames);
end;

procedure TExtnValue.SetAsCe_SubjectAltName(const Value: TGeneralNames);
begin
  InternalSelectChoice(7);
  (FSelected as TGeneralNames).Assign(Value);
end;

function TExtnValue.GetAsCe_IssuerAltName: TGeneralNames;
begin
  Result := (FSelected as TGeneralNames);
end;

procedure TExtnValue.SetAsCe_IssuerAltName(const Value: TGeneralNames);
begin
  InternalSelectChoice(8);
  (FSelected as TGeneralNames).Assign(Value);
end;

function TExtnValue.GetAsCe_BasicConstraints: TBasicConstraintsSyntax;
begin
  Result := (FSelected as TBasicConstraintsSyntax);
end;

procedure TExtnValue.SetAsCe_BasicConstraints(const Value: TBasicConstraintsSyntax);
begin
  InternalSelectChoice(9);
  (FSelected as TBasicConstraintsSyntax).Assign(Value);
end;

function TExtnValue.GetAsCe_NameConstraints: TNameConstraintsSyntax;
begin
  Result := (FSelected as TNameConstraintsSyntax);
end;

procedure TExtnValue.SetAsCe_NameConstraints(const Value: TNameConstraintsSyntax);
begin
  InternalSelectChoice(10);
  (FSelected as TNameConstraintsSyntax).Assign(Value);
end;

function TExtnValue.GetAsCe_PolicyConstraints: TPolicyConstraintsSyntax;
begin
  Result := (FSelected as TPolicyConstraintsSyntax);
end;

procedure TExtnValue.SetAsCe_PolicyConstraints(const Value: TPolicyConstraintsSyntax);
begin
  InternalSelectChoice(11);
  (FSelected as TPolicyConstraintsSyntax).Assign(Value);
end;

function TExtnValue.GetAsCe_CRLDistributionPoints: TCrldistributionPointsSyntax;
begin
  Result := (FSelected as TCrldistributionPointsSyntax);
end;

procedure TExtnValue.SetAsCe_CRLDistributionPoints(const Value: TCrldistributionPointsSyntax);
begin
  InternalSelectChoice(12);
  (FSelected as TCrldistributionPointsSyntax).Assign(Value);
end;

function TExtnValue.GetAsCe_SubjectDirectoryAttributes: TAttributesSyntax;
begin
  Result := (FSelected as TAttributesSyntax);
end;

procedure TExtnValue.SetAsCe_SubjectDirectoryAttributes(const Value: TAttributesSyntax);
begin
  InternalSelectChoice(13);
  (FSelected as TAttributesSyntax).Assign(Value);
end;

function TExtnValue.GetAsPe_AuthorityInfoAccess: TAuthorityInfoAccessSyntax;
begin
  Result := (FSelected as TAuthorityInfoAccessSyntax);
end;

procedure TExtnValue.SetAsPe_AuthorityInfoAccess(const Value: TAuthorityInfoAccessSyntax);
begin
  InternalSelectChoice(14);
  (FSelected as TAuthorityInfoAccessSyntax).Assign(Value);
end;

function TExtnValue.GetChoice: TExtnValueEnum;
begin
  Result := TExtnValueEnum(InternalGetChoice);
end;

procedure TExtnValue.SetChoice(const Value: TExtnValueEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

function TExtnValue.GetAsNetscapeCertType: TBitString;
begin
  Result := FSelected as TBitString;
end;

procedure TExtnValue.SetAsNetscapeCertType(const Value: TBitString);
begin
  InternalSelectChoice(15);
  (FSelected as TBitString).Assign(Value);
end;

{ TExtension }

constructor TExtension.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TExtnValue,nil,FData.Items[2]^,nil^);
end;

destructor TExtension.Destroy;
begin
  inherited Destroy;
end;

function TExtension.GetExtnID: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TExtension.SetExtnID(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TExtension.GetCritical: Boolean;
begin
  Result := Boolean(TBooleanWrapper(FItems[1]).Value);
end;

procedure TExtension.SetCritical(const Value: Boolean);
begin
  TBooleanWrapper(FItems[1]).Value := Value;
end;

function TExtension.GetExtnValue: TExtnValue;
begin
  Result := FItems[2];
end;

procedure TExtension.SetExtnValue(const Value: TExtnValue);
begin
  TExtnValue(FItems[2]).Assign(Value);
end;

function TExtension.IsType(Id: TExtnValueEnum): Boolean;
begin
  Result := TExtnValue(FItems[2]).Choice = Id;
end;

procedure TExtension.SetType(Id: TExtnValueEnum);
begin
  TExtnValue(FItems[2]).Choice := Id;
end;

{ TExtensions }

constructor TExtensions.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  FFieldType := TExtension;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/extensions/extensions')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TExtensions.Destroy;
begin
  inherited Destroy;
end;

function TExtensions.GetUniqueItem(Id: TExtnValueEnum): TExtension;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do begin
    Result := Items[I];
    if Result.IsType(Id) then
      Exit;
  end;
  Result := nil;
end;

function TExtensions.AddUniqueItem(Id: TExtnValueEnum): TExtension;
begin
  Result := GetUniqueItem(Id);
  if Result = nil then begin
    Result := Add;
    Result.SetType(Id);
    case Id of
      eveIdCeAuthorityKeyIdentifier: Result.Critical := False;// MUST
      eveIdCeSubjectKeyIdentifier:   Result.Critical := False;// MUST
      eveIdCeKeyUsage:               Result.Critical := True; // MUST
      eveIdCeExtKeyUsage:            Result.Critical := True; // Optional
      eveIdCePrivateKeyUsagePeriod:  Result.Critical := False;// Should not be used
      eveIdCeCertificatePolicies:    Result.Critical := False;// Optional
      eveIdCePolicyMappings:         Result.Critical := False;// MUST
      eveIdCeSubjectAltName:         Result.Critical := True; // MUST, if Subject is empty
      eveIdCeIssuerAltName:          Result.Critical := False;// Should
      eveIdCeBasicConstraints:       Result.Critical := True; // MUST in CA certs; should be absent otherwise
      eveIdCeNameConstraints:        Result.Critical := True; // MUST; MUST be absent from non-CA certs
      eveIdCePolicyConstraints:      Result.Critical := True; // Optional
      eveIdCeCRLDistributionPoints:  Result.Critical := False;// Should
      eveIdCeSubjectDirectoryAttributes:
                                     Result.Critical := False;// MUST
      eveIdPeAuthorityInfoAccess:    Result.Critical := False;// MUST
    end;
  end;
end;

function TExtensions.Add: TExtension;
begin
  Result := TExtension(InternalAdd);
end;

function TExtensions.GetItems(Index: Integer): TExtension;
begin
  Result := TExtension(InternalGetItems(Index));
end;

function TExtensions.GetKeyUsage: TKeyUsage;
var
  Ext: TExtension;
  Dummy: packed record
           Dummy: Byte;
           KU: TKeyUsage;
         end;
begin
  Ext := GetUniqueItem(eveIdCeKeyUsage);
  if Ext = nil then
    Result := [Low(TKeyUsageItem)..High(TKeyUsageItem)]
  else begin
    if Ext.ExtnValue.AsCe_KeyUsage.Length < 2 then
      Result := []
    else if Ext.ExtnValue.AsCe_KeyUsage.Length < SizeOf(Dummy) then begin
      FillChar(Dummy,SizeOf(Dummy),0);
      Ext.ExtnValue.AsCe_KeyUsage.GetBinary(Dummy,Ext.ExtnValue.AsCe_KeyUsage.Length);
      Result := Dummy.KU;
    end else begin
      Ext.ExtnValue.AsCe_KeyUsage.GetBinary(Dummy,SizeOf(Dummy));
      Result := Dummy.KU;
    end;
  end;
end;

procedure TExtensions.SetKeyUsage(const Value: TKeyUsage);
var
  Ext: TExtension;                      
  Dummy: packed record
           Dummy: Byte;
           KU: TKeyUsage;
         end;
begin
  Ext := AddUniqueItem(eveIdCeKeyUsage);
  Dummy.Dummy := 7;
  Dummy.KU := Value;
  Ext.ExtnValue.AsCe_KeyUsage.SetBinary(Dummy,SizeOf(Dummy));
end;

{ TTbscertificate }

constructor TTbscertificate.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('tbsCertificate')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[2]^,nil^);
  FieldFactory(TName,nil,FData.Items[3]^,nil^);
  FieldFactory(TValidity,nil,FData.Items[4]^,nil^);
  FieldFactory(TName,nil,FData.Items[5]^,nil^);
  FieldFactory(TSubjectPublicKeyInfo,nil,FData.Items[6]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[7]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[8]^,nil^);
  FieldFactory(TExtensions,nil,FData.Items[9]^.Items[0]^,nil^);
end;

destructor TTbscertificate.Destroy;
begin
  inherited Destroy;
end;

function TTbscertificate.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TTbscertificate.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TTbscertificate.GetSerialNumber: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TTbscertificate.SetSerialNumber(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

function TTbscertificate.GetSignature: TAlgorithmIdentifier;
begin
  Result := FItems[2];
end;

procedure TTbscertificate.SetSignature(const Value: TAlgorithmIdentifier);
begin
  TAlgorithmIdentifier(FItems[2]).Assign(Value);
end;

function TTbscertificate.GetIssuer: TName;
begin
  Result := FItems[3];
end;

procedure TTbscertificate.SetIssuer(const Value: TName);
begin
  TName(FItems[3]).Assign(Value);
end;

function TTbscertificate.GetValidity: TValidity;
begin
  Result := FItems[4];
end;

procedure TTbscertificate.SetValidity(const Value: TValidity);
begin
  TValidity(FItems[4]).Assign(Value);
end;

function TTbscertificate.GetSubject: TName;
begin
  Result := FItems[5];
end;

procedure TTbscertificate.SetSubject(const Value: TName);
begin
  TName(FItems[5]).Assign(Value);
end;

function TTbscertificate.GetSubjectPublicKeyInfo: TSubjectPublicKeyInfo;
begin
  Result := FItems[6];
end;

procedure TTbscertificate.SetSubjectPublicKeyInfo(const Value: TSubjectPublicKeyInfo);
begin
  TSubjectPublicKeyInfo(FItems[6]).Assign(Value);
end;

function TTbscertificate.GetIssuerUniqueID: TBitString;
begin
  Result := FItems[7];
end;

procedure TTbscertificate.SetIssuerUniqueID(const Value: TBitString);
begin
  TBitString(FItems[7]).Assign(Value);
end;

function TTbscertificate.GetSubjectUniqueID: TBitString;
begin
  Result := FItems[8];
end;

procedure TTbscertificate.SetSubjectUniqueID(const Value: TBitString);
begin
  TBitString(FItems[8]).Assign(Value);
end;

function TTbscertificate.GetExtensions: TExtensions;
begin
  Result := FItems[9];
end;

procedure TTbscertificate.SetExtensions(const Value: TExtensions);
begin
  TExtensions(FItems[9]).Assign(Value);
end;   

{ TSigned }

function TSigned.CheckSignature(CACert: TCertificate): Boolean;
var
  PublKey: IReadOnlyPublicKeyInfo;
begin
  if CACert = nil then begin
    if QueryInterface(IReadOnlyPublicKeyInfo,PublKey) = 0 then
      Result := InternalCheckSignature(PublKey)
    else
      Result := False;
  end else begin
    Result := IsAllegedIssuer(CACert);
    if Result then
      Result := InternalCheckSignature(CACert);
  end;
end;

function TSigned.DSASign(const DSAKey: TDLPrivateKey;
  CACert: TCertificate): Boolean;
{$IFDEF SHA1}                    
var
  OID, D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
  Params: TASNCustomWrapper;
{$ENDIF SHA1}
begin
{$IFDEF SHA1}
  if DSAKey.SignScheme = dlssaDSA then
    OID := id_dsa_with_sha1
  else if DSAKey.SignScheme = dlssaNR then
    OID := id_nr_with_sha1
  else
    OID := '';
  Result := BeforeSign(CACert,OID,Params);
  if Result then begin
    SignatureAlgorithm.Algorithm := OID;
    MS := TSecureMemoryStream.Create;
    try
      FData.Items[0].SaveToStream(MS,fmtDER);
      SetLength(D,8192);
      Len := DLSSASignatureGeneration(DSAKey,MS.Memory^,MS.Size,haSHA1,True,D[1],Length(D));
      SetLength(D,Len);
      D := #0 + D;
      Signature.SetBinary(D[1],Length(D));
    finally
      MS.Free;
    end;         
    FData.CalculateLength;
  end;
{$ELSE  SHA1}
  Result := False;
{$ENDIF SHA1}
end;

function TSigned.ECDSASign(const ECDSAKey: TECPrivateKey;
  CACert: TCertificate): Boolean;
{$IFDEF SHA1}
var
  OID, D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
  Params: TASNCustomWrapper;     
{$ENDIF SHA1}
begin  
{$IFDEF SHA1}
  OID := ecdsa_with_sha1;
  Result := BeforeSign(CACert,OID,Params);
  if Result then begin
    SignatureAlgorithm.Algorithm := OID;
    MS := TSecureMemoryStream.Create;
    try
      FData.Items[0].SaveToStream(MS,fmtDER);
      SetLength(D,8192);
      Len := ECSSASignatureGeneration(ECDSAKey,MS.Memory^,MS.Size,haSHA1,True,D[1],Length(D));
      SetLength(D,Len);
      D := #0 + D;
      Signature.SetBinary(D[1],Length(D));
    finally
      MS.Free;
    end;
    FData.CalculateLength;
  end;
{$ELSE  SHA1}
  Result := False;
{$ENDIF SHA1}
end;

function TSigned.ECDSASign(const ECDSAKey: TECPrivateKey;
  CACert: TASN1Struct): Boolean;
var
  Crt: TCertificate;
begin
  Crt := TCertificate.Create(nil,nil);
  try
    Crt.Data.ReadOnly := False;
    Crt.AssignStruct(CACert);
    Result := ECDSASign(ECDSAkey,Crt);
  finally
    Crt.Free;
  end;
end;

function TSigned.GetSignature: TBitString;
begin
  Result := FItems[2];
end;

function TSigned.GetSignatureAlgorithm: TAlgorithmIdentifier;
begin
  Result := FItems[1];
end;

function TSigned.InternalCheckSignature(CACert: IReadOnlyPublicKeyInfo): Boolean;
var
  RSAKey: TIFPublicKey;
  DLKey: TDLPublicKey;
  ECKey: TECPublicKey;
  MS: TSecureMemoryStream;
  HA,MHA: THashAlgorithm;
  EM: TSignEncoding;
begin
  Result := False;
  if not CheckSignAlgCoherence then
    Exit;

  EM := seEMSA3;
{$IFDEF SHA1}
  HA := haSHA1;
  MHA := haSHA1;
{$ELSE  SHA1}   
  HA := haReserved0;
  MHA := haReserved0;
{$ENDIF SHA1}
  if (SignatureAlgorithm.Algorithm = id_DSA_with_SHA1) or
     (SignatureAlgorithm.Algorithm = ecdsa_with_SHA1) then
    Result := True
  else
    Result := InterpretRSASignatureAlgorithm(HA,MHA,EM);
  if Result then begin
    FillChar(RSAKey,SizeOf(RSAKey),0);
    FillChar(DLKey,SizeOf(DLKey),0);
    FillChar(ECKey,SizeOf(ECKey),0);
    if CACert.ExtractSubjectRSAPublicKey(RSAKey) then begin
      MS := TSecureMemoryStream.Create;
      try
        FData.Items[0]^.SaveToStream(MS,fmtDER);
        MS.Position := 0;
        Result := IFSSASignatureVerification(RSAKey,MS.Memory^,MS.Size,
                                             FData.Items[2]^.Content^,
                                             FData.Items[2]^.Length,
                                             HA,MHA,EM);
      finally
        MS.Free;
      end;
      DisposeIFPublicKey(RSAKey);
    end else if CACert.ExtractSubjectDSAPublicKey(DLKey) then begin
      MS := TSecureMemoryStream.Create;
      try
        FData.Items[0]^.SaveToStream(MS,fmtDER);
        MS.Position := 0;
        Result := DLSSASignatureVerification(DLKey,MS.Memory^,MS.Size,
                                             FData.Items[2].Content[1],
                                             FData.Items[2].Length - 1,
                                             True,HA);
      finally
        MS.Free;
      end;
      DisposeDLPublicKey(DLKey);
    end else if CACert.ExtractSubjectECPublicKey(ECKey) then begin
      MS := TSecureMemoryStream.Create;
      try
        FData.Items[0]^.SaveToStream(MS,fmtDER);
        MS.Position := 0;
        Result := ECSSASignatureVerification(ECKey,MS.Memory^,MS.Size,
                                             FData.Items[2].Content[1],
                                             FData.Items[2].Length - 1,
                                             True,HA);
      finally
        MS.Free;
      end;
      DisposeECPublicKey(ECKey);
    end else
      Result := False;
  end;
end;

function TSigned.InterpretRSASignatureAlgorithm(var HA,
  MHA: THashAlgorithm; var EM: TSignEncoding): Boolean;
begin
  Result := True;
  if ExtractHashAlgEMSA3(SignatureAlgorithm.Algorithm,HA) then
    EM := seEMSA3
  else if SignatureAlgorithm.Algorithm = id_RSASSA_PSS then begin
    EM := seEMSA4;
    Result := OIDToHashAlgorithm(SignatureAlgorithm.Parameters.AsRsassaPss.HashFunc.Algorithm,HA);
{$IFDEF SHA1}
    if not Result then HA := haSHA1;
    Result := True;
{$ENDIF SHA1}
    if Result then begin
      Result := OIDToHashAlgorithm(SignatureAlgorithm.Parameters.AsRsassaPss.MgfFunc.Parameters.AsMgf1.Algorithm,MHA);
{$IFDEF SHA1}
      if not Result then MHA := haSHA1;
      Result := True;
{$ENDIF}
    end; 
  end;
end;

function TSigned.RSASign(const RSAKey: TIFPrivateKey; HA: THashAlgorithm;
  CACert: TCertificate): Boolean;
var
  OID, D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
  Params: TASNCustomWrapper;
begin
  try
    OID := SignatureAlgEMSA3(HA);
  except
    Result := False;
    Exit;
  end;
  Result := BeforeSign(CACert,OID,Params);
  if Result then begin
    SignatureAlgorithm.Algorithm := OID;
    MS := TSecureMemoryStream.Create;
    try
      FData.CalculateLength;
      FData.Items[0].SaveToStream(MS,fmtDER);
      SetLength(D,8192);
      Len := IFSSASignatureGeneration(RSAKey,
                                      MS.Memory^,MS.Size,
                                      HA,HA,seEMSA3,
                                      D[1],Length(D));
      SetLength(D,Len);
      D := #0 + D;
      Signature.SetBinary(D[1],Length(D));
    finally
      MS.Free;
    end;
    FData.CalculateLength;
  end;
end;

function TSigned.RSASign(const RSAKey: TIFPrivateKey; HA: THashAlgorithm;
  CACert: TASN1Struct): Boolean;
var
  Crt: TCertificate;
begin
  Crt := TCertificate.Create(nil,nil);
  try
    Crt.Data.ReadOnly := False;
    Crt.AssignStruct(CACert);
    Result := RSASign(RSAkey,HA,Crt);
  finally
    Crt.Free;
  end;
end;

procedure TSigned.SetSignature(const Value: TBitString);
begin
  TBitString(FItems[2]).Assign(Value);
end;

procedure TSigned.SetSignatureAlgorithm(const Value: TAlgorithmIdentifier);
begin
  TAlgorithmIdentifier(FItems[1]).Assign(Value);
end;

function TSigned.DSASign(const DSAKey: TDLPrivateKey;
  CACert: TASN1Struct): Boolean;
var
  Crt: TCertificate;
begin
  Crt := TCertificate.Create(nil,nil);
  try
    Crt.Data.ReadOnly := False;
    Crt.AssignStruct(CACert);
    Result := DSASign(DSAkey,Crt);
  finally
    Crt.Free;
  end;
end;

function TSigned.DSASign(const DSAKey: TDLPrivateKey): Boolean;
begin
  Result := DSASign(DSAKey,TCertificate(nil));
end;

function TSigned.ECDSASign(const ECDSAKey: TECPrivateKey): Boolean;
begin
  Result := ECDSASign(ECDSAKey,TCertificate(nil));
end;

function TSigned.RSASign(const RSAKey: TIFPrivateKey;
  HA: THashAlgorithm): Boolean;
begin
  Result := RSASign(RSAKey,HA,TCertificate(nil));
end;

function TSigned.RSASSA_PSSSign(const RSAKey: TIFPrivateKey;
  CACert: TCertificate): Boolean;
var
  OID, D: string;
  HA, MGFHA: THashAlgorithm;
  MS: TSecureMemoryStream;
  Len: Integer;
  Params: TASNCustomWrapper;
begin
  OID := id_RSASSA_PSS;
  Result := BeforeSign(CACert,OID,Params);
  if Result then begin
    SignatureAlgorithm.Algorithm := OID;
    SignatureAlgorithm.Parameters.AssignStruct(Params.Data);
    Result := OIDToHashAlgorithm(SignatureAlgorithm.Parameters.AsRsassaPss.HashFunc.Algorithm,HA);
{$IFDEF SHA1}
    if not Result then HA := haSHA1;
    Result := True;
{$ENDIF SHA1}
    if Result then begin
      Result := OIDToHashAlgorithm(SignatureAlgorithm.Parameters.AsRsassaPss.MgfFunc.Parameters.AsMgf1.Algorithm,MGFHA);
{$IFDEF SHA1}
      if not Result then MGFHA := haSHA1;
      Result := True;
{$ENDIF SHA1}
      if Result then begin
        MS := TSecureMemoryStream.Create;
        try
          FData.Items[0].SaveToStream(MS,fmtDER);
          SetLength(D,8192);
          Len := IFSSASignatureGeneration(RSAKey,
                                          MS.Memory^,MS.Size,
                                          HA,MGFHA,seEMSA4,
                                          D[1],Length(D));
          SetLength(D,Len);
          D := #0 + D;
          Signature.SetBinary(D[1],Length(D));
        finally
          MS.Free;
        end;
      end;
    end;
    FData.CalculateLength;
  end;
end;

function TSigned.RSASSA_PSSSign(const RSAKey: TIFPrivateKey;
  CACert: TASN1Struct): Boolean;
var
  Crt: TCertificate;
begin
  Crt := TCertificate.Create(nil,nil);
  try
    Crt.Data.ReadOnly := False;
    Crt.AssignStruct(CACert);
    Result := RSASSA_PSSSign(RSAkey,Crt);
  finally
    Crt.Free;
  end;
end;

function TSigned.RSASSA_PSSSign(const RSAKey: TIFPrivateKey): Boolean;
begin
  Result := RSASSA_PSSSign(RSAKey,TCertificate(nil));
end;

function TSigned.CheckSignature(PublicKey: IReadOnlyPublicKeyInfo): Boolean;
begin
  Result := InternalCheckSignature(PublicKey);
end;

{ TCertificate }

function TCertificate.AlgorithmIdentifier: string;
begin
  Result := SubjectPublicKeyInfo.AlgorithmIdentifier;
end;

function TCertificate.BeforeSign(var CACert: TCertificate;
                                 SignAlg: ObjectIdentifier;
                                 out Params: TASNCustomWrapper): Boolean;
var
  Ext, Ext0: TExtension;
begin
  if CACert = nil then
    CACert := Self;

  Ext := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeBasicConstraints];
  if Assigned(Ext) then begin
    Result := Ext.ExtnValue.AsCe_BasicConstraints.CA;
    if not Result then Exit;
  end else begin
    Result := False;
    Exit;
  end;

  Ext := CACert.TBSCertificate.Extensions.UniqueItem[eveIdCeKeyUsage];
  if Assigned(Ext) then
    if not Ext.ExtnValue.AsCe_KeyUsage.Bits[keyCertSignVal] then begin
      Result := False;
      Exit;
    end;

  TBSCertificate.Signature.Algorithm := SignAlg;
  Params := TBSCertificate.Signature.Parameters.Selected;
  if SignAlg = id_RSASSA_PSS then begin
    if CACert.SubjectPublicKeyInfo.Algorithm.Parameters.Choice = aideIdRsaesOaep then begin
      TRsassaPssParams(Params).HashFunc.Algorithm :=
        CACert.SubjectPublicKeyInfo.Algorithm.Parameters.AsRsaesOaep.HashFunc.Algorithm;
      TRsassaPssParams(Params).MgfFunc.Algorithm := id_mgf1;
      TRsassaPssParams(Params).MgfFunc.Parameters.AsMgf1.Algorithm :=
        CACert.SubjectPublicKeyInfo.Algorithm.Parameters.AsRsaesOaep.MgfFunc.Parameters.AsMgf1.Algorithm;
    end else if CACert.SubjectPublicKeyInfo.Algorithm.Parameters.Choice = aideIdRsassaPss then begin
      TRsassaPssParams(Params).HashFunc.Algorithm :=
        CACert.SubjectPublicKeyInfo.Algorithm.Parameters.AsRsassaPss.HashFunc.Algorithm;
      TRsassaPssParams(Params).MgfFunc.Algorithm := id_mgf1;
      TRsassaPssParams(Params).MgfFunc.Parameters.AsMgf1.Algorithm :=
        CACert.SubjectPublicKeyInfo.Algorithm.Parameters.AsRsassaPss.MgfFunc.Parameters.AsMgf1.Algorithm;
    end else begin
      Result := False;
      Exit;
    end;
  end;

  TBSCertificate.Issuer := CACert.TbsCertificate.Subject;

  Ext := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];
  if Assigned(Ext) then begin
    Ext0 := TBSCertificate.Extensions.AddUniqueItem(eveIdCeIssuerAltName);
    Ext0.ExtnValue.AsCe_IssuerAltName := Ext.ExtnValue.AsCe_SubjectAltName;
  end;

  Ext := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectKeyIdentifier];
  if Assigned(Ext) then begin
    Ext0 := TBSCertificate.Extensions.AddUniqueItem(eveIdCeAuthorityKeyIdentifier);
    Ext0.ExtnValue.AsCe_AuthorityKeyIdentifier.KeyIdentifier.AsString :=
      Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString;
  end;

  if TbsCertificate.Extensions.Count > 0 then
    Version := 3;

  FData.SortSET;
end;

function TCertificate.CheckSignAlgCoherence: Boolean;
begin
  Result := SignatureAlgorithm.Algorithm = TBSCertificate.Signature.Algorithm;
end;

constructor TCertificate.Create;
begin                        
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then
    FData.Assign(GlobalObject);
  FieldFactory(TTbscertificate,nil,FData.Items[0]^,nil^);
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[2]^,nil^);
end;

destructor TCertificate.Destroy;
begin
  inherited Destroy;
end;

function TCertificate.ExtractSubjectDHPublicKey(
  var DHKey: TDLPublicKey): Boolean;
begin
  Result := TBSCertificate.SubjectPublicKeyInfo.ExtractSubjectDHPublicKey(DHKey);
end;

function TCertificate.ExtractSubjectDSAPublicKey(
  var DSAKey: TDLPublicKey): Boolean;
begin
  Result := SubjectPublicKeyInfo.ExtractSubjectDSAPublicKey(DSAKey);
end;

function TCertificate.ExtractSubjectECPublicKey(
  var ECKey: TECPublicKey): Boolean;
begin
  Result := SubjectPublicKeyInfo.ExtractSubjectECPublicKey(ECKey);
end;

function TCertificate.ExtractSubjectNRPublicKey(
  var NRKey: TDLPublicKey): Boolean;
begin
  Result := SubjectPublicKeyInfo.ExtractSubjectNRPublicKey(NRKey);
end;

function TCertificate.ExtractSubjectRSAES_OAEPPublicKey(
  var RSAKey: TIFPublicKey; var HA, MGFHA: THashAlgorithm;
  var P: string): Boolean;
begin
  Result := SubjectPublicKeyInfo.ExtractSubjectRSAES_OAEPPublicKey(RSAKey,HA,MGFHA,P);
end;

function TCertificate.ExtractSubjectRSAPublicKey(
  var RSAKey: TIFPublicKey): Boolean;
begin
  Result := SubjectPublicKeyInfo.ExtractSubjectRSAPublicKey(RSAKey);
end;

function TCertificate.GetAlgorithm: TAlgorithmIdentifier;
begin
  Result := TbsCertificate.SubjectPublicKeyInfo.Algorithm;
end;

function TCertificate.GetIssuer: TName;
begin
  Result := TbsCertificate.Issuer;
end;

function TCertificate.GetSerialNumber: TIntegerWrapper;
begin
  Result := TbsCertificate.SerialNumber;
end;

function TCertificate.GetSubject: TName;
begin
  Result := TbsCertificate.Subject;
end;

function TCertificate.GetSubjectPublicKeyInfo: TSubjectPublicKeyInfo;
begin
  Result := TbsCertificate.SubjectPublicKeyInfo;
end;

function TCertificate.GetTbsCertificate: TTbscertificate;
begin
  Result := FItems[0];
end;

function TCertificate.GetValidity: TValidity;
begin
  Result := TbsCertificate.Validity;
end;

function TCertificate.GetVersion: Integer;
begin
  Result := TbsCertificate.Version.AsInteger + 1;
end;

procedure TCertificate.ImposeSubjectDHPublicKey(const DHKey: TDLPublicKey);
var
  Ext: TExtension;
begin
  TbsCertificate.SubjectPublicKeyInfo.ImposeSubjectDHPublicKey(DHKey);
  Ext := TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    TbsCertificate.SubjectPublicKeyInfo.PublicKeyIdentifier;
end;

procedure TCertificate.ImposeSubjectDSAPublicKey(
  const DSAKey: TDLPublicKey);
var
  Ext: TExtension;
begin
  TbsCertificate.SubjectPublicKeyInfo.ImposeSubjectDSAPublicKey(DSAKey);
  Ext := TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    TbsCertificate.SubjectPublicKeyInfo.PublicKeyIdentifier;
end;

procedure TCertificate.ImposeSubjectECPublicKey(var ECKey: TECPublicKey);
var
  Ext: TExtension;
begin
  TbsCertificate.SubjectPublicKeyInfo.ImposeSubjectECPublicKey(ECKey);
  Ext := TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    TbsCertificate.SubjectPublicKeyInfo.PublicKeyIdentifier;
end;

procedure TCertificate.ImposeSubjectNRPublicKey(const NRKey: TDLPublicKey);
var
  Ext: TExtension;
begin
  TbsCertificate.SubjectPublicKeyInfo.ImposeSubjectNRPublicKey(NRKey);
  Ext := TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    TbsCertificate.SubjectPublicKeyInfo.PublicKeyIdentifier;
end;

procedure TCertificate.ImposeSubjectRSAES_OAEPPublicKey(
  const RSAKey: TIFPublicKey; HA, MGFHA: THashAlgorithm; P: OctetString);
var
  Ext: TExtension;
begin
  SubjectPublicKeyInfo.ImposeSubjectRSAES_OAEPPublicKey(RSAKey,HA,MGFHA,P);
  Ext := TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    SubjectPublicKeyInfo.PublicKeyIdentifier;
end;

procedure TCertificate.ImposeSubjectRSAPublicKey(
  const RSAKey: TIFPublicKey);
var
  Ext: TExtension;
begin
  TbsCertificate.SubjectPublicKeyInfo.ImposeSubjectRSAPublicKey(RSAKey);
  Ext := TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    TbsCertificate.SubjectPublicKeyInfo.PublicKeyIdentifier;
end;

function TCertificate.IsAllegedIssuer(CACert: TCertificate): Boolean;
var
  UKI, ABC, SBC, NCE, SAN, SKI, AKI: TExtension;
  AltName: TGeneralNames;
  A, S: TOctetString;
begin
  Result := False;

  UKI := CACert.TBSCertificate.Extensions.UniqueItem[eveIdCeKeyUsage];
  if Assigned(UKI) then
    if not UKI.ExtnValue.AsCe_KeyUsage.Bits[keyCertSignMask] then
      Exit;

  ABC := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeBasicConstraints];
  if Assigned(ABC) then begin
    if not (ABC.Critical and ABC.ExtnValue.AsCe_BasicConstraints.CA) then
      Exit;
    SBC := TbsCertificate.Extensions.UniqueItem[eveIdCeBasicConstraints];
    if Assigned(SBC) then
      if SBC.ExtnValue.AsCe_BasicConstraints.CA and
         (SBC.ExtnValue.AsCe_BasicConstraints.PathLenConstraint.AsInteger >=
          ABC.ExtnValue.AsCe_BasicConstraints.PathLenConstraint.AsInteger) then
        Exit;
  end else
    Exit;

  NCE := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeNameConstraints];
  if Assigned(NCE) then begin
    SAN := TBSCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];
    if Assigned(SAN) then
      AltName := SAN.ExtnValue.AsCe_SubjectAltName
    else
      AltName := nil;
    if not NCE.ExtnValue.AsCe_NameConstraints.Verify(
             TBSCertificate.Subject,AltName) then
      Exit;
  end;

  AKI := TBSCertificate.Extensions.UniqueItem[eveIdCeAuthorityKeyIdentifier];
  if Assigned(AKI) then begin
    A := AKI.ExtnValue.AsCe_AuthorityKeyIdentifier.KeyIdentifier;
    SKI := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectKeyIdentifier];
    Result := Assigned(SKI);
    if Result then begin
      S := SKI.ExtnValue.AsCe_SubjectKeyIdentifier;
      Result := (A.Length = S.Length) and
                (A.AsString = S.AsString);
      if Result then begin
        Result := TBSCertificate.Issuer.Compare(CACert.TbsCertificate.Subject);
        if Result then begin
          AKI := TBSCertificate.Extensions.UniqueItem[eveIdCeIssuerAltName];
          SKI := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];
          Result := Assigned(AKI) = Assigned(SKI);
          if Result and Assigned(AKI) then
            Result := AKI.GetExtnValue.AsCe_IssuerAltName.Compare(SKI.ExtnValue.AsCe_SubjectAltName);
        end;
      end;
    end;
  end else begin
    Result := TBSCertificate.Issuer.Compare(CACert.TbsCertificate.Subject);
    if Result then begin
      AKI := TBSCertificate.Extensions.UniqueItem[eveIdCeIssuerAltName];
      SKI := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];
      Result := Assigned(AKI) = Assigned(SKI);
      if Result and Assigned(AKI) then
        Result := AKI.GetExtnValue.AsCe_IssuerAltName.Compare(SKI.ExtnValue.AsCe_SubjectAltName);
    end;
  end;
end;

function TCertificate.PublicKeyIdentifier: string;
var
  Ext: TExtension;
begin
  Ext := TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectKeyIdentifier];
  if Assigned(Ext) then
    Result := Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString
  else
    Result := '';
end;

procedure TCertificate.SetIssuer(const Value: TName);
begin
  TbsCertificate.Issuer.Assign(Value);
end;

procedure TCertificate.SetSerialNumber(const Value: TIntegerWrapper);
begin
  TbsCertificate.SerialNumber.Assign(Value);
end;

procedure TCertificate.SetSubject(const Value: TName);
begin
  TbsCertificate.Subject.Assign(Value);
end;

procedure TCertificate.SetSubjectPublicKeyInfo(
  const Value: TSubjectPublicKeyInfo);
begin
  TbsCertificate.SubjectPublicKeyInfo.Assign(Value);
end;

procedure TCertificate.SetTbsCertificate(const Value: TTbscertificate);
begin
  TTbscertificate(FItems[0]).Assign(Value);
end;

procedure TCertificate.SetValidity(const Value: TValidity);
begin
  TbsCertificate.Validity.Assign(Value);
end;

procedure TCertificate.SetVersion(const Value: Integer);
begin
  if (Value > 0) and (Value <= 3) then
    TbsCertificate.Version.AsInteger := Value - 1;
end;

{ TRsaesOaepParams }

constructor TRsaesOaepParams.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  InheritedCreate(AOwner,AData,ATemplate);
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[9]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TDigestAlgorithmIdentifier,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TMgfAlgorithmIdentifier,nil,FData.Items[1]^.Items[0]^,nil^);
  FieldFactory(TPsAlgorithmIdentifier,nil,FData.Items[2]^.Items[0]^,nil^);
end;

destructor TRsaesOaepParams.Destroy;
begin
  inherited Destroy;
end;

function TRsaesOaepParams.GetPSourceFunc: TPsAlgorithmIdentifier;
begin
  Result := FItems[2];
end;

procedure TRsaesOaepParams.SetPSourceFunc(
  const Value: TPsAlgorithmIdentifier);
begin
  TPsAlgorithmIdentifier(FItems[2]).Assign(Value);
end;

{ TDigParams }

constructor TDigParams.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[9]^.Items[0]^.Items[0]^;
    F := F.FindField('parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
end;

destructor TDigParams.Destroy;
begin
  inherited Destroy;
end;

function TDigParams.GetChoice: TDigParamsEnum;
begin
  Result := TDigParamsEnum(InternalGetChoice);
end;

procedure TDigParams.SetChoice(const Value: TDigParamsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TDigestAlgorithmIdentifier }

constructor TDigestAlgorithmIdentifier.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[9]^.Items[0]^.Items[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TDigParams,nil,FData.Items[1]^,nil^);
end;

destructor TDigestAlgorithmIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TDigestAlgorithmIdentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TDigestAlgorithmIdentifier.SetAlgorithm(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TDigestAlgorithmIdentifier.GetParameters: TDigParams;
begin
  Result := FItems[1];
end;

procedure TDigestAlgorithmIdentifier.SetParameters(const Value: TDigParams);
begin
  TDigParams(FItems[1]).Assign(Value);
end;

function TDigestAlgorithmIdentifier.IsType(Id: TDigParamsEnum): Boolean;
begin
  Result := TDigParams(FItems[1]).Choice = Id;
end;

procedure TDigestAlgorithmIdentifier.SetType(Id: TDigParamsEnum);
begin
  TDigParams(FItems[1]).Choice := Id;
end;

{ TMgfParams }

constructor TMgfParams.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[9]^.Items[1]^.Items[0]^;
    F := F.FindField('parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TDigestAlgorithmIdentifier);
end;

destructor TMgfParams.Destroy;
begin
  inherited Destroy;
end;

function TMgfParams.GetAsMgf1: TDigestAlgorithmIdentifier;
begin
  InternalSelectChoice(0);
  Result := (FSelected as TDigestAlgorithmIdentifier);
end;

function TMgfParams.GetChoice: TMgfParamsEnum;
begin
  Result := TMgfParamsEnum(InternalGetChoice);
end;

procedure TMgfParams.SetAsMgf1(const Value: TDigestAlgorithmIdentifier);
begin
  InternalSelectChoice(0);
  (FSelected as TDigestAlgorithmIdentifier).Assign(Value);
end;

procedure TMgfParams.SetChoice(const Value: TMgfParamsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TMgfAlgorithmIdentifier }

constructor TMgfAlgorithmIdentifier.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[9]^.Items[1]^.Items[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TMgfParams,nil,FData.Items[1]^,nil^);
end;

destructor TMgfAlgorithmIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TMgfAlgorithmIdentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := TStringWrapper(FItems[0]).Value;
end;

function TMgfAlgorithmIdentifier.GetParameters: TMgfParams;
begin
  Result := FItems[1];
end;

function TMgfAlgorithmIdentifier.IsType(Id: TMgfParamsEnum): Boolean;
begin
  Result := TMgfParams(FItems[1]).Choice = Id;
end;

procedure TMgfAlgorithmIdentifier.SetAlgorithm(
  const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

procedure TMgfAlgorithmIdentifier.SetParameters(const Value: TMgfParams);
begin
  TMgfParams(FItems[1]).Assign(Value);
end;

procedure TMgfAlgorithmIdentifier.SetType(Id: TMgfParamsEnum);
begin
  TMgfParams(FItems[1]).Choice := Id;
end;

{ TPsParams }

constructor TPsParams.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[9]^.Items[2]^.Items[0]^;
    F := F.FindField('parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TOctetString);
end;

destructor TPsParams.Destroy;
begin
  inherited Destroy;
end;

function TPsParams.GetAsPSpecified: TOctetString;
begin
  InternalSelectChoice(0);
  Result := (FSelected as TOctetString);
end;

function TPsParams.GetChoice: TPsParamsEnum;
begin
  Result := TPsParamsEnum(InternalGetChoice);
end;

procedure TPsParams.SetAsPSpecified(const Value: TOctetString);
begin
  InternalSelectChoice(0);
  (FSelected as TOctetString).Assign(Value);
end;

procedure TPsParams.SetChoice(const Value: TPsParamsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TPsAlgorithmIdentifier }

constructor TPsAlgorithmIdentifier.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[9]^.Items[2]^.Items[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TPsParams,nil,FData.Items[1]^,nil^);
end;

destructor TPsAlgorithmIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TPsAlgorithmIdentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := TStringWrapper(FItems[0]).Value;
end;

function TPsAlgorithmIdentifier.GetParameters: TPsParams;
begin
  Result := FItems[1];
end;

function TPsAlgorithmIdentifier.IsType(Id: TPsParamsEnum): Boolean;
begin
  Result := TPsParams(FItems[1]).Choice = Id;
end;

procedure TPsAlgorithmIdentifier.SetAlgorithm(
  const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

procedure TPsAlgorithmIdentifier.SetParameters(const Value: TPsParams);
begin
  TPsParams(FItems[1]).Assign(Value);
end;

procedure TPsAlgorithmIdentifier.SetType(Id: TPsParamsEnum);
begin
  TPsParams(FItems[1]).Choice := Id;
end;

{ TRsassaPssParams }

constructor TRsassaPssParams.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_Cert not initialized: Call StrSecInit.InitPKIX_Cert to correct.');
  InheritedCreate(AOwner,AData,ATemplate);
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertificate/signature/parameters')^.Choices[10]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TDigestAlgorithmIdentifier,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TMgfAlgorithmIdentifier,nil,FData.Items[1]^.Items[0]^,nil^);
end;

destructor TRsassaPssParams.Destroy;
begin
  inherited Destroy;
end;

function TRsassaPssParams.GetHashFunc: TDigestAlgorithmIdentifier;
begin
  Result := FItems[0];
end;

function TRsassaPssParams.GetMgfFunc: TMgfAlgorithmIdentifier;
begin
  Result := FItems[1];
end;

procedure TRsassaPssParams.InheritedCreate(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
begin
  inherited Create(AOwner,AData,ATemplate);
end;

procedure TRsassaPssParams.SetHashFunc(
  const Value: TDigestAlgorithmIdentifier);
begin
  TDigestAlgorithmIdentifier(FItems[0]).Assign(Value);
end;

procedure TRsassaPssParams.SetMgfFunc(
  const Value: TMgfAlgorithmIdentifier);
begin
  TMgfAlgorithmIdentifier(FItems[1]).Assign(Value);
end;

{$IFDEF INITIALIZATION}
initialization
{$ENDIF}
{$IFDEF INIT_SECTIONS}
  Initialize;
{$ENDIF}
{$IFDEF FINI_SECTIONS}
finalization
  Finalize;
{$ENDIF}
end.
