{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     CMS Unit                                          }
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
unit Cms;

interface

uses
  Classes, 
{$IFDEF BCB}
  SecUtils,
{$ENDIF}
  Asn1, Pkix_Cert, Pkix_CRL;

const
  crlf = #13#10;
{$NODEFINE ASNModule}
  ASNModule =
    'CryptographicMessageSyntax {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) smime(16) modules(0) cms(1) } DEFINITIONS ::=' + crlf + 
    'BEGIN' + crlf + crlf + 
    'IMPORTS CertificateList FROM PKIX-CRL;' + crlf + 
    'IMPORTS Certificate, Name, AlgorithmIdentifier FROM PKIX-Cert;' + crlf + crlf + 
    'ContentInfo ::= SEQUENCE {' + crlf + 
    '     contentType'#9'ContentType,' + crlf + 
    '     content'#9'[0] Content.&Type(&contentType)}' + crlf + crlf + 
    'ContentType ::= OBJECT' + crlf + crlf + 
    'Content ::= TYPE-IDENTIFIER {' + crlf + 
    '     {OCTET STRING'#9'IDENTIFIED BY id-data}|' + crlf + 
    '     {SignedData'#9'IDENTIFIED BY id-signedData}|' + crlf + 
    '     {EnvelopedData'#9'IDENTIFIED BY id-envelopedData}|' + crlf + 
    '     {SignedAndEnvelopedData'#9'IDENTIFIED BY id-signedAndEnvelopedData}|' + crlf + 
    '     {DigestedData'#9'IDENTIFIED BY id-digestedData}|' + crlf + 
    '     {EncryptedData'#9'IDENTIFIED BY id-encryptedData}|' + crlf + 
    '     {AuthenticatedData'#9'IDENTIFIED BY id-ct-authData}}' + crlf + crlf + 
    'id-data OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-7(7) 1 }' + crlf + crlf + 
    'SignedData ::= SEQUENCE {' + crlf + 
    '     version'#9'CMSVersion,' + crlf + 
    '     digestAlgorithms'#9'DigestAlgorithmIdentifiers,' + crlf + 
    '     encapContentInfo'#9'EncapsulatedContentInfo,' + crlf + 
    '     certificates'#9'[0] IMPLICIT CertificateSet OPTIONAL,' + crlf + 
    '     crls'#9'[1] IMPLICIT CertificateRevocationLists OPTIONAL,' + crlf + 
    '     signerInfos'#9'SignerInfos}' + crlf + crlf + 
    'CMSVersion ::= INTEGER' + crlf + crlf + 
    'DigestAlgorithmIdentifiers ::= SET OF DigestAlgorithmIdentifier' + crlf + crlf + 
    'DigestAlgorithmIdentifier ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'OBJECT,' + crlf +
    '     parameters'#9'DigParams.&Type(&algorithm) OPTIONAL}' + crlf + crlf + 
    'DigParams ::= TYPE-IDENTIFIER {' + crlf + 
    '     {NULL'#9'IDENTIFIED BY sha-1}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY md5}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY sha256}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY sha384}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY sha512}}' + crlf + crlf + 
    'sha-1 OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) ansi-X9-42(10046) algorithm(1) 26 }' + crlf + crlf + 
    'md5 OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) digestAlgorithm(2) 5 }' + crlf + crlf + 
    'sha256 OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) (2) 1 }' + crlf + crlf + 
    'sha384 OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) (2) 2 }' + crlf + crlf + 
    'sha512 OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) (2) 3 }' + crlf + crlf + crlf + 
    'EncapsulatedContentInfo ::= SEQUENCE {' + crlf + 
    '     eContentType'#9'ContentType,' + crlf + 
    '     eContent'#9'[0] OCTET STRING OPTIONAL}' + crlf + crlf +
    'CertificateSet ::= SET OF CertificateChoices' + crlf + crlf + 
    'CertificateChoices ::= CHOICE {' + crlf + 
    '     certificate'#9'Certificate,' + crlf + 
    '     extendedCertificate'#9'[0] IMPLICIT ExtendedCertificate}' + crlf + crlf + 
    'ExtendedCertificate ::= SEQUENCE {' + crlf + 
    '     extendedCertificateInfo'#9'ExtendedCertificateInfo,' + crlf + 
    '     signatureAlgorithm'#9'SignatureAlgorithmIdentifier,' + crlf + 
    '     signature'#9'Signature}' + crlf + crlf + 
    'ExtendedCertificateInfo ::= SEQUENCE {' + crlf + 
    '     version'#9'CMSVersion,' + crlf + 
    '     certificate'#9'Certificate,' + crlf + 
    '     attributes'#9'UnauthAttributes}' + crlf + crlf + 
    'UnauthAttributes ::= SET OF Attribute' + crlf + crlf + 
    'Attribute ::= SEQUENCE {' + crlf + 
    '     attrType'#9'OBJECT,' + crlf + 
    '     attrValues'#9'SET OF AttributeValue.&Type(&attrType)}' + crlf + crlf + 
    'AttributeValue ::= TYPE-IDENTIFIER {' + crlf + 
    '     {OBJECT'#9'IDENTIFIED BY id-contentType}|' + crlf + 
    '     {MessageDigest'#9'IDENTIFIED BY id-messageDigest}|' + crlf + 
    '     {SigningTime'#9'IDENTIFIED BY id-signingTime}|' + crlf +
    '     {SignerInfo'#9'IDENTIFIED BY id-countersignature}|' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY id-signingDescription}}' + crlf + crlf +
    'id-contentType OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) 3 }' + crlf + crlf +
    'MessageDigest ::= OCTET STRING' + crlf + crlf +
    'id-messageDigest OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) 4 }' + crlf + crlf +
    'SigningTime ::= Time' + crlf + crlf +
    'id-signingTime OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) 5 }' + crlf + crlf +
    'id-signingDescription OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) 13 }' + crlf + crlf +
    'CounterSignature ::= SignerInfo' + crlf + crlf +
    'SignerInfo ::= SEQUENCE {' + crlf + 
    '     version'#9'CMSVersion,' + crlf + 
    '     sid'#9'SignerIdentifier,' + crlf + 
    '     digestAlgorithm'#9'DigestAlgorithmIdentifier,' + crlf + 
    '     signedAttrs'#9'[0] IMPLICIT SignedAttributes OPTIONAL,' + crlf + 
    '     signatureAlgorithm'#9'SignatureAlgorithmIdentifier,' + crlf + 
    '     signature'#9'SignatureValue,' + crlf + 
    '     unsignedAttrs'#9'[1] IMPLICIT UnsignedAttributes OPTIONAL}' + crlf + crlf + 
    'SignerIdentifier ::= CHOICE {' + crlf + 
    '     issuerAndSerialNumber'#9'IssuerAndSerialNumber,' + crlf + 
    '     subjectKeyIdentifier'#9'[0] SubjectKeyIdentifier}' + crlf + crlf + 
    'IssuerAndSerialNumber ::= SEQUENCE {' + crlf + 
    '     issuer'#9'Name,' + crlf + 
    '     serialNumber'#9'INTEGER}' + crlf + crlf + 
    'SubjectKeyIdentifier ::= OCTET STRING' + crlf + crlf + 
    'SignedAttributes ::= SET OF Attribute' + crlf + crlf + 
    'SignatureAlgorithmIdentifier ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'OBJECT,' + crlf + 
    '     parameters'#9'SigParams.&Type(&algorithm) OPTIONAL}' + crlf + crlf + 
    'SigParams ::= TYPE-IDENTIFIER {' + crlf + 
    '     {NULL'#9'IDENTIFIED BY md2WithRSAEncryption}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY md5WithRSAEncryption}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY sha1WithRSAEncryption}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY sha256WithRSAEncryption}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY sha384WithRSAEncryption}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY sha512WithRSAEncryption}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY id-dsa-with-sha1}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY rsaEncryption}}' + crlf + crlf + 
    'md2WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 2 }' + crlf + crlf + 
    'md5WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 4 }' + crlf + crlf +
    'sha1WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 5 }' + crlf + crlf + 
    'sha256WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 11 }' + crlf + crlf + 
    'sha384WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 12 }' + crlf + crlf + 
    'sha512WithRSAEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 13 }' + crlf + crlf + 
    'id-dsa-with-sha1 OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) ansi-X9-57(10040) (4) 3 }' + crlf + crlf + 
    'rsaEncryption OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 1 }' + crlf + crlf + 
    'SignatureValue ::= OCTET STRING' + crlf + crlf + 
    'UnsignedAttributes ::= SET OF Attribute' + crlf + crlf + 
    'id-countersignature OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) 6 }' + crlf + crlf + 
    'Signature ::= BIT STRING' + crlf + crlf + crlf + 
    'CertificateRevocationLists ::= SET OF CertificateList' + crlf + crlf + 
    'SignerInfos ::= SET OF SignerInfo' + crlf + crlf + 
    'id-signedData OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-7(7) 2 }' + crlf + crlf + 
    'EnvelopedData ::= SEQUENCE {' + crlf + 
    '     version'#9'CMSVersion,' + crlf + 
    '     originatorInfo'#9'[0] IMPLICIT OriginatorInfo OPTIONAL,' + crlf + 
    '     recipientInfos'#9'RecipientInfos,' + crlf + 
    '     encryptedContentInfo'#9'EncryptedContentInfo,' + crlf + 
    '     unprotectedAttrs'#9'[1] IMPLICIT UnprotectedAttributes OPTIONAL}' + crlf + crlf + 
    'OriginatorInfo ::= SEQUENCE {' + crlf + 
    '     certs'#9'[0] IMPLICIT CertificateSet OPTIONAL,' + crlf + 
    '     crls'#9'[1] IMPLICIT CertificateRevocationLists OPTIONAL}' + crlf + crlf + 
    'RecipientInfos ::= SET OF RecipientInfo' + crlf + crlf + 
    'RecipientInfo ::= CHOICE {' + crlf + 
    '     ktri'#9'KeyTransRecipientInfo,' + crlf + 
    '     kari'#9'[1] IMPLICIT KeyAgreeRecipientInfo,' + crlf + 
    '     kekri'#9'[2] IMPLICIT KEKRecipientInfo}' + crlf + crlf + 
    'KeyTransRecipientInfo ::= SEQUENCE {' + crlf + 
    '     version'#9'CMSVersion,' + crlf + 
    '     rid'#9'RecipientIdentifier,' + crlf + 
    '     keyEncryptionAlgorithm'#9'KeyEncryptionAlgorithmIdentifier,' + crlf + 
    '     encryptedKey'#9'EncryptedKey}' + crlf + crlf + 
    'RecipientIdentifier ::= CHOICE {' + crlf + 
    '     issuerAndSerialNumber'#9'IssuerAndSerialNumber,' + crlf + 
    '     subjectKeyIdentifier'#9'[0] IMPLICIT SubjectKeyIdentifier}' + crlf + crlf + 
    'KeyEncryptionAlgorithmIdentifier ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'OBJECT,' + crlf +
    '     parameters'#9'KEParams.&Type(&algorithm) OPTIONAL}' + crlf + crlf + 
    'KEParams ::= TYPE-IDENTIFIER {' + crlf + 
    '     {NULL'#9'IDENTIFIED BY rsaEncryption}|' + crlf + 
    '     {KeyWrapAlgorithmIdentifier'#9'IDENTIFIED BY id-alg-ESDH}|' + crlf + 
    '     {RSAES-OAEP-params'#9'IDENTIFIED BY id-RSAES-OAEP}}' + crlf + crlf + 
    'KeyWrapAlgorithmIdentifier ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'OBJECT,' + crlf + 
    '     parameters'#9'KWParams.&Type(&algorithm) OPTIONAL}' + crlf + crlf + 
    'KWParams ::= TYPE-IDENTIFIER {' + crlf + 
    '     {NULL'#9'IDENTIFIED BY id-alg-CMS3DESwrap}|' + crlf + 
    '     {RC2wrapParameter'#9'IDENTIFIED BY id-alg-CMSRC2wrap}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY id-aes128-wrap}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY id-aes192-wrap}|' + crlf + 
    '     {NULL'#9'IDENTIFIED BY id-aes256-wrap}}' + crlf + crlf + 
    'id-alg-CMS3DESwrap OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) smime(16) alg(3) 6 }' + crlf + crlf + 
    'RC2wrapParameter ::= RC2ParameterVersion' + crlf + crlf + 
    'RC2ParameterVersion ::= INTEGER' + crlf + crlf + 
    'id-alg-CMSRC2wrap OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) smime(16) alg(3) 7 }' + crlf + crlf + 
    'id-aes128-wrap OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) id-aes(1) 5 }' + crlf + crlf + 
    'id-aes192-wrap OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) id-aes(1) 25 }' + crlf + crlf + 
    'id-aes256-wrap OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) id-aes(1) 45 }' + crlf + crlf + 
    'id-alg-ESDH OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) smime(16) alg(3) 5 }' + crlf + crlf + 
    'RSAES-OAEP-params ::= SEQUENCE {' + crlf + 
    '     hashFunc'#9'[0] DigestAlgorithmIdentifier OPTIONAL,' + crlf + 
    '     maskGenFunc'#9'[1] MGFAlgorithmIdentifier OPTIONAL,' + crlf + 
    '     pSourceFunc'#9'[2] PSFAlgorithmIdentifier OPTIONAL}' + crlf + crlf + 
    'MGFAlgorithmIdentifier ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'OBJECT,' + crlf + 
    '     parameters'#9'PKCS1MGFAlgorithms.&Type(&algorithm)}' + crlf + crlf + 
    'PKCS1MGFAlgorithms ::= TYPE-IDENTIFIER {' + crlf + 
    '     {DigestAlgorithmIdentifier'#9'IDENTIFIED BY id-mgf1}}' + crlf + crlf + 
    'id-mgf1 OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 8 }' + crlf + crlf + 
    'PSFAlgorithmIdentifier ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'OBJECT,' + crlf + 
    '     parameters'#9'PKCS1pSourceAlgorithms.&Type(&algorithm)}' + crlf + crlf + 
    'PKCS1pSourceAlgorithms ::= TYPE-IDENTIFIER {' + crlf + 
    '     {PSource'#9'IDENTIFIED BY id-pSpecified}}' + crlf + crlf +
    'PSource ::= OCTET STRING' + crlf + crlf + 
    'id-pSpecified OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 9 }' + crlf + crlf + 
    'id-RSAES-OAEP OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-1(1) 7 }' + crlf + crlf + 
    'EncryptedKey ::= OCTET STRING' + crlf + crlf + 
    'KeyAgreeRecipientInfo ::= SEQUENCE {' + crlf + 
    '     version'#9'CMSVersion,' + crlf + 
    '     originator'#9'[0] OriginatorIdentifierOrKey,' + crlf + 
    '     ukm'#9'[1] UserKeyingMaterial OPTIONAL,' + crlf + 
    '     keyEncryptionAlgorithm'#9'KeyEncryptionAlgorithmIdentifier,' + crlf + 
    '     recipientEncryptedKeys'#9'RecipientEncryptedKeys}' + crlf + crlf + 
    'OriginatorIdentifierOrKey ::= CHOICE {' + crlf + 
    '     issuerAndSerialNumber'#9'IssuerAndSerialNumber,' + crlf + 
    '     subjectKeyIdentifier'#9'[0] IMPLICIT SubjectKeyIdentifier,' + crlf + 
    '     originatorKey'#9'[1] IMPLICIT OriginatorPublicKey}' + crlf + crlf + 
    'OriginatorPublicKey ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'AlgorithmIdentifier,' + crlf + 
    '     publicKey'#9'BIT STRING}' + crlf + crlf + 
    'UserKeyingMaterial ::= OCTET STRING' + crlf + crlf + 
    'RecipientEncryptedKeys ::= SEQUENCE OF RecipientEncryptedKey' + crlf + crlf + 
    'RecipientEncryptedKey ::= SEQUENCE {' + crlf + 
    '     rid'#9'KeyAgreeRecipientIdentifier,' + crlf + 
    '     encryptedKey'#9'EncryptedKey}' + crlf + crlf + 
    'KeyAgreeRecipientIdentifier ::= CHOICE {' + crlf + 
    '     issuerAndSerialNumber'#9'IssuerAndSerialNumber,' + crlf + 
    '     rKeyId'#9'[0] IMPLICIT RecipientKeyIdentifier}' + crlf + crlf + 
    'RecipientKeyIdentifier ::= SEQUENCE {' + crlf + 
    '     subjectKeyIdentifier'#9'SubjectKeyIdentifier,' + crlf + 
    '     date'#9'GeneralizedTime OPTIONAL,' + crlf + 
    '     other'#9'OtherKeyAttribute OPTIONAL}' + crlf + crlf + 
    'OtherKeyAttribute ::= SEQUENCE {' + crlf + 
    '     keyAttrId'#9'OBJECT,' + crlf + 
    '     keyAttr'#9'ANY OPTIONAL}' + crlf + crlf + 
    'KEKRecipientInfo ::= SEQUENCE {' + crlf + 
    '     version'#9'CMSVersion,' + crlf + 
    '     kekid'#9'KEKIdentifier,' + crlf + 
    '     keyEncryptionAlgorithm'#9'KeyEncryptionAlgorithmIdentifier,' + crlf + 
    '     encryptedKey'#9'EncryptedKey}' + crlf + crlf +
    'KEKIdentifier ::= SEQUENCE {' + crlf + 
    '     keyIdentifier'#9'OCTET STRING,' + crlf + 
    '     date'#9'GeneralizedTime OPTIONAL,' + crlf + 
    '     other'#9'OtherKeyAttribute OPTIONAL}' + crlf + crlf + crlf + 
    'EncryptedContentInfo ::= SEQUENCE {' + crlf + 
    '     contentType'#9'ContentType,' + crlf + 
    '     contentEncryptionAlgorithm'#9'ContentEncryptionAlgorithmIdentifier,' + crlf + 
    '     encryptedContent'#9'[0] IMPLICIT EncryptedContent OPTIONAL}' + crlf + crlf + 
    'ContentEncryptionAlgorithmIdentifier ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'OBJECT,' + crlf +
    '     parameters'#9'CEParams.&Type(&algorithm) OPTIONAL}' + crlf + crlf +
    'CEParams ::= TYPE-IDENTIFIER {' + crlf + 
    '     {IV'#9'IDENTIFIED BY des-ede3-cbc}|' + crlf + 
    '     {RC2CBCParameter'#9'IDENTIFIED BY rc2-cbc}|' + crlf + 
    '     {IV'#9'IDENTIFIED BY id-aes128-CBC}|' + crlf + 
    '     {IV'#9'IDENTIFIED BY id-aes192-CBC}|' + crlf + 
    '     {IV'#9'IDENTIFIED BY id-aes256-CBC}}' + crlf + crlf + 
    'IV ::= OCTET STRING' + crlf + crlf + 
    'des-ede3-cbc OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) encryptionAlgorithm(3) 7 }' + crlf + crlf + 
    'RC2CBCParameter ::= SEQUENCE {' + crlf + 
    '     rc2ParameterVersion'#9'INTEGER,' + crlf + 
    '     iv'#9'OCTET STRING}' + crlf + crlf + 
    'rc2-cbc OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) encryptionAlgorithm(3) 2 }' + crlf + crlf + 
    'id-aes128-CBC OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) id-aes(1) 2 }' + crlf + crlf + 
    'id-aes192-CBC OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) id-aes(1) 22 }' + crlf + crlf + 
    'id-aes256-CBC OBJECT IDENTIFIER ::= {  (2) (16) (840) (1) (101) (3) (4) id-aes(1) 42 }' + crlf + crlf +
    'EncryptedContent ::= OCTET STRING' + crlf + crlf +
    'UnprotectedAttributes ::= SET OF Attribute' + crlf + crlf + 
    'id-envelopedData OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-7(7) 3 }' + crlf + crlf + 
    'SignedAndEnvelopedData ::= SEQUENCE {' + crlf + 
    '     version'#9'INTEGER,' + crlf + 
    '     recipientInfos'#9'RecipientInfos,' + crlf + 
    '     digestAlgorithms'#9'DigestAlgorithmIdentifiers,' + crlf + 
    '     encryptedContentInfo'#9'EncryptedContentInfo,' + crlf + 
    '     certificates'#9'[0] IMPLICIT CertificateSet OPTIONAL,' + crlf + 
    '     cRLs'#9'[1] IMPLICIT CertificateRevocationLists OPTIONAL,' + crlf + 
    '     signerInfos'#9'SignerInfos}' + crlf + crlf +
    'id-signedAndEnvelopedData OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-7(7) 4 }' + crlf + crlf + 
    'DigestedData ::= SEQUENCE {' + crlf + 
    '     version'#9'CMSVersion,' + crlf + 
    '     digestAlgorithm'#9'DigestAlgorithmIdentifier,' + crlf + 
    '     encapContentInfo'#9'EncapsulatedContentInfo,' + crlf + 
    '     digest'#9'Digest}' + crlf + crlf + 
    'Digest ::= OCTET STRING' + crlf + crlf + 
    'id-digestedData OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-7(7) 5 }' + crlf + crlf + 
    'EncryptedData ::= SEQUENCE {' + crlf + 
    '     version'#9'CMSVersion,' + crlf + 
    '     encryptedContentInfo'#9'EncryptedContentInfo,' + crlf + 
    '     unprotectedAttrs'#9'[1] IMPLICIT UnprotectedAttributes OPTIONAL}' + crlf + crlf + 
    'id-encryptedData OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-7(7) 6 }' + crlf + crlf + 
    'AuthenticatedData ::= SEQUENCE {' + crlf + 
    '     version'#9'CMSVersion,' + crlf + 
    '     originatorInfo'#9'[0] IMPLICIT OriginatorInfo OPTIONAL,' + crlf + 
    '     recipientInfos'#9'RecipientInfos,' + crlf + 
    '     macAlgorithm'#9'MessageAuthenticationCodeAlgorithm,' + crlf + 
    '     digestAlgorithm'#9'[1] IMPLICIT DigestAlgorithmIdentifier OPTIONAL,' + crlf + 
    '     encapContentInfo'#9'EncapsulatedContentInfo,' + crlf + 
    '     authenticatedAttributes'#9'[2] IMPLICIT AuthAttributes OPTIONAL,' + crlf + 
    '     mac'#9'MessageAuthenticationCode,' + crlf + 
    '     unauthenticatedAttributes'#9'[3] IMPLICIT UnauthAttributes OPTIONAL}' + crlf + crlf + 
    'MessageAuthenticationCodeAlgorithm ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'OBJECT,' + crlf + 
    '     parameters'#9'MACParams.&Type(&algorithm) OPTIONAL}' + crlf + crlf + 
    'MACParams ::= TYPE-IDENTIFIER {' + crlf + 
    '     {NULL'#9'IDENTIFIED BY hMAC-SHA1}}' + crlf + crlf + 
    'hMAC-SHA1 OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) (8) (1) 2 }' + crlf + crlf + 
    'AuthAttributes ::= SET OF Attribute' + crlf + crlf + 
    'MessageAuthenticationCode ::= OCTET STRING' + crlf + crlf + 
    'id-ct-authData OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) smime(16) ct(1) 2 }' + crlf + crlf + 
    'END' + crlf + crlf;

type
{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TDigestAlgorithmIdentifiers
        TDigestAlgorithmIdentifier
         TDigParams}

  TDigParamsEnum = (
    dpeUndefined, dpeSha1, dpeMd5, dpeSha256, dpeSha384, dpeSha512);

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
    TContentInfo
     TContent
      TSignedData
       TDigestAlgorithmIdentifiers
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
    TContentInfo
     TContent
      TSignedData
       TDigestAlgorithmIdentifiers}

  TDigestAlgorithmIdentifiers = class(TASNCustomOFFieldWrapper)
  private
    function GetUniqueItem(Id: TDigParamsEnum): TDigestAlgorithmIdentifier;
    function GetItems(Index: Integer): TDigestAlgorithmIdentifier;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function AddUniqueItem(Id: TDigParamsEnum): TDigestAlgorithmIdentifier;
    function Add: TDigestAlgorithmIdentifier;
    property Items[Index: Integer]: TDigestAlgorithmIdentifier read GetItems;
    property UniqueItem[Id: TDigParamsEnum]: TDigestAlgorithmIdentifier read GetUniqueItem; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TEncapsulatedContentInfo}

  TEncapsulatedContentInfo = class(TASNConstructedWrapper)
  private
    function GetEContentType: ObjectIdentifier;
    procedure SetEContentType(const Value: ObjectIdentifier);
    function GetEContent: TOctetString;
    procedure SetEContent(const Value: TOctetString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property EContentType: ObjectIdentifier read GetEContentType write SetEContentType;
    property EContent: TOctetString read GetEContent write SetEContent;
  end;

  TSignerInfo = class;

{ Declaration tree:
    TContentInfo
     TContent
      TSignedData
       TCertificateSet
        TCertificateChoices
         TExtendedCertificate
          TExtendedCertificateInfo
           TUnauthAttributes
            TCmsattribute
             TSetOfCmsattributeValue
              TCmsattributeValue}

  TCmsattributeValueEnum = (
    cveUndefined, cveIdContentType, cveIdMessageDigest, cveIdSigningTime,
    cveIdCountersignature);

  TCmsattributeValue = class(TASNChoiceWrapper)
  private
    function GetAsContentType: ObjectIdentifier;
    procedure SetAsContentType(const Value: ObjectIdentifier);
    function GetAsMessageDigest: TOctetString;
    procedure SetAsMessageDigest(const Value: TOctetString);
    function GetAsSigningTime: TTime;
    procedure SetAsSigningTime(const Value: TTime);
    function GetAsCountersignature: TSignerInfo;
    procedure SetAsCountersignature(const Value: TSignerInfo);
    function GetChoice: TCmsattributeValueEnum;
    procedure SetChoice(const Value: TCmsattributeValueEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsContentType: ObjectIdentifier read GetAsContentType write SetAsContentType;
    property AsMessageDigest: TOctetString read GetAsMessageDigest write SetAsMessageDigest;
    property AsSigningTime: TTime read GetAsSigningTime write SetAsSigningTime;
    property AsCountersignature: TSignerInfo read GetAsCountersignature write SetAsCountersignature;
  published
    property Choice: TCmsattributeValueEnum read GetChoice write SetChoice;
  end;

{ Declaration tree:
    TContentInfo
     TContent
      TSignedData
       TCertificateSet
        TCertificateChoices
         TExtendedCertificate
          TExtendedCertificateInfo
           TUnauthAttributes
            TCmsattribute
             TSetOfCmsattributeValue}

  TSetOfCmsattributeValue = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TCmsattributeValue;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TCmsattributeValue;
    property Items[Index: Integer]: TCmsattributeValue read GetItems; default;
  end;

{ Declaration tree:
    TContentInfo
     TContent
      TSignedData
       TCertificateSet
        TCertificateChoices
         TExtendedCertificate
          TExtendedCertificateInfo
           TUnauthAttributes
            TCmsattribute}

  TCmsattribute = class(TASNConstructedWrapper)
  private
    function GetAttrType: ObjectIdentifier;
    procedure SetAttrType(const Value: ObjectIdentifier);
    function GetAttrValues: TSetOfCmsattributeValue;
    procedure SetAttrValues(const Value: TSetOfCmsattributeValue);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TCmsattributeValueEnum): Boolean;
  published
    property AttrType: ObjectIdentifier read GetAttrType write SetAttrType;
    property AttrValues: TSetOfCmsattributeValue read GetAttrValues write SetAttrValues;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TCertificateSet
        TCertificateChoices
         TExtendedCertificate
          TExtendedCertificateInfo
           TUnauthAttributes}

  TUnauthAttributes = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TCMSAttribute;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TCMSAttribute;
    property Items[Index: Integer]: TCMSAttribute read GetItems; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TCertificateSet
        TCertificateChoices
         TExtendedCertificate
          TExtendedCertificateInfo}

  TExtendedCertificateInfo = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetCertificate: TCertificate;
    procedure SetCertificate(const Value: TCertificate);
    function GetAttributes: TUnauthAttributes;
    procedure SetAttributes(const Value: TUnauthAttributes);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property Certificate: TCertificate read GetCertificate write SetCertificate;
    property Attributes: TUnauthAttributes read GetAttributes write SetAttributes;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TCertificateSet
        TCertificateChoices
         TExtendedCertificate
          TSignatureAlgorithmIdentifier
           TSigParams}

  TSigParamsEnum = (
    speUndefined, speMd2WithRSAEncryption, speMd5WithRSAEncryption,
    speSha1WithRSAEncryption, speSha256WithRSAEncryption,
    speSha384WithRSAEncryption, speSha512WithRSAEncryption, speIdDsaWithSha1,
    speRsaEncryption);

  TSigParams = class(TASNChoiceWrapper)
  private
    function GetChoice: TSigParamsEnum;
    procedure SetChoice(const Value: TSigParamsEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Choice: TSigParamsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TCertificateSet
        TCertificateChoices
         TExtendedCertificate
          TSignatureAlgorithmIdentifier}

  TSignatureAlgorithmIdentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TSigParams;
    procedure SetParameters(const Value: TSigParams);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TSigParamsEnum): Boolean;
    procedure SetType(Id: TSigParamsEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TSigParams read GetParameters write SetParameters;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TCertificateSet
        TCertificateChoices
         TExtendedCertificate}

  TExtendedCertificate = class(TASNConstructedWrapper)
  private
    function GetExtendedCertificateInfo: TExtendedCertificateInfo;
    procedure SetExtendedCertificateInfo(const Value: TExtendedCertificateInfo);
    function GetSignatureAlgorithm: TSignatureAlgorithmIdentifier;
    procedure SetSignatureAlgorithm(const Value: TSignatureAlgorithmIdentifier);
    function GetSignature: TBitString;
    procedure SetSignature(const Value: TBitString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property ExtendedCertificateInfo: TExtendedCertificateInfo read GetExtendedCertificateInfo write SetExtendedCertificateInfo;
    property SignatureAlgorithm: TSignatureAlgorithmIdentifier read GetSignatureAlgorithm write SetSignatureAlgorithm;
    property Signature: TBitString read GetSignature write SetSignature;
  end;

{ Declaration tree:
    TContentInfo
     TContent
      TSignedData
       TCertificateSet
        TCertificateChoices}

  TCertificateChoicesEnum = (
    cceUndefined, cceCertificate, cceExtendedCertificate);

  TCertificateChoices = class(TASNChoiceWrapper)
  private
    function GetAsCertificate: TCertificate;
    procedure SetAsCertificate(const Value: TCertificate);
    function GetAsExtendedCertificate: TExtendedCertificate;
    procedure SetAsExtendedCertificate(const Value: TExtendedCertificate);
    function GetChoice: TCertificateChoicesEnum;
    procedure SetChoice(const Value: TCertificateChoicesEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsCertificate: TCertificate read GetAsCertificate write SetAsCertificate;
    property AsExtendedCertificate: TExtendedCertificate read GetAsExtendedCertificate write SetAsExtendedCertificate;
  published
    property Choice: TCertificateChoicesEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TCertificateSet}

  TCertificateSet = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TCertificateChoices;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TCertificateChoices;
    property Items[Index: Integer]: TCertificateChoices read GetItems; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TCertificateRevocationLists}

  TCertificateRevocationLists = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TCertificateList;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TCertificateList;
    property Items[Index: Integer]: TCertificateList read GetItems; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TSignerInfos
        TSignerInfo
         TSignerIdentifier
          TIssuerAndSerialNumber}

  TIssuerAndSerialNumber = class(TASNConstructedWrapper)
  private
    function GetIssuer: TName;
    procedure SetIssuer(const Value: TName);
    function GetSerialNumber: TIntegerWrapper;
    procedure SetSerialNumber(const Value: TIntegerWrapper);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Issuer: TName read GetIssuer write SetIssuer;
    property SerialNumber: TIntegerWrapper read GetSerialNumber write SetSerialNumber;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TSignerInfos
        TSignerInfo
         TSignerIdentifier}

  TSignerIdentifierEnum = (
    sieUndefined, sieIssuerAndSerialNumber, sieSubjectKeyIdentifier);

  TSignerIdentifier = class(TASNChoiceWrapper)
  private
    function GetAsIssuerAndSerialNumber: TIssuerAndSerialNumber;
    procedure SetAsIssuerAndSerialNumber(const Value: TIssuerAndSerialNumber);
    function GetAsSubjectKeyIdentifier: TOctetString;
    procedure SetAsSubjectKeyIdentifier(const Value: TOctetString);
    function GetChoice: TSignerIdentifierEnum;
    procedure SetChoice(const Value: TSignerIdentifierEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsIssuerAndSerialNumber: TIssuerAndSerialNumber read GetAsIssuerAndSerialNumber write SetAsIssuerAndSerialNumber;
    property AsSubjectKeyIdentifier: TOctetString read GetAsSubjectKeyIdentifier write SetAsSubjectKeyIdentifier;
  published
    property Choice: TSignerIdentifierEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TSignerInfos
        TSignerInfo
         TSignedAttributes}

  TSignedAttributes = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TAttribute;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TAttribute;
    property Items[Index: Integer]: TAttribute read GetItems; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TSignerInfos
        TSignerInfo
         TUnsignedAttributes}

  TUnsignedAttributes = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TCMSAttribute;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TCMSAttribute;
    property Items[Index: Integer]: TCMSAttribute read GetItems; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TSignerInfos
        TSignerInfo}

  TSignerInfo = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetSid: TSignerIdentifier;
    procedure SetSid(const Value: TSignerIdentifier);
    function GetDigestAlgorithm: TDigestAlgorithmIdentifier;
    procedure SetDigestAlgorithm(const Value: TDigestAlgorithmIdentifier);
    function GetSignedAttrs: TSignedAttributes;
    procedure SetSignedAttrs(const Value: TSignedAttributes);
    function GetSignatureAlgorithm: TSignatureAlgorithmIdentifier;
    procedure SetSignatureAlgorithm(const Value: TSignatureAlgorithmIdentifier);
    function GetSignature: TOctetString;
    procedure SetSignature(const Value: TOctetString);
    function GetUnsignedAttrs: TUnsignedAttributes;
    procedure SetUnsignedAttrs(const Value: TUnsignedAttributes);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property Sid: TSignerIdentifier read GetSid write SetSid;
    property DigestAlgorithm: TDigestAlgorithmIdentifier read GetDigestAlgorithm write SetDigestAlgorithm;
    property SignedAttrs: TSignedAttributes read GetSignedAttrs write SetSignedAttrs;
    property SignatureAlgorithm: TSignatureAlgorithmIdentifier read GetSignatureAlgorithm write SetSignatureAlgorithm;
    property Signature: TOctetString read GetSignature write SetSignature;
    property UnsignedAttrs: TUnsignedAttributes read GetUnsignedAttrs write SetUnsignedAttrs;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData
       TSignerInfos}

  TSignerInfos = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TSignerInfo;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TSignerInfo;
    property Items[Index: Integer]: TSignerInfo read GetItems; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedData}

  TSignedData = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetDigestAlgorithms: TDigestAlgorithmIdentifiers;
    procedure SetDigestAlgorithms(const Value: TDigestAlgorithmIdentifiers);
    function GetEncapContentInfo: TEncapsulatedContentInfo;
    procedure SetEncapContentInfo(const Value: TEncapsulatedContentInfo);
    function GetCertificates: TCertificateSet;
    procedure SetCertificates(const Value: TCertificateSet);
    function GetCrls: TCertificateRevocationLists;
    procedure SetCrls(const Value: TCertificateRevocationLists);
    function GetSignerInfos: TSignerInfos;
    procedure SetSignerInfos(const Value: TSignerInfos);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property DigestAlgorithms: TDigestAlgorithmIdentifiers read GetDigestAlgorithms write SetDigestAlgorithms;
    property EncapContentInfo: TEncapsulatedContentInfo read GetEncapContentInfo write SetEncapContentInfo;
    property Certificates: TCertificateSet read GetCertificates write SetCertificates;
    property Crls: TCertificateRevocationLists read GetCrls write SetCrls;
    property SignerInfos: TSignerInfos read GetSignerInfos write SetSignerInfos;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TOriginatorInfo}

  TOriginatorInfo = class(TASNConstructedWrapper)
  private
    function GetCerts: TCertificateSet;
    procedure SetCerts(const Value: TCertificateSet);
    function GetCrls: TCertificateRevocationLists;
    procedure SetCrls(const Value: TCertificateRevocationLists);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Certs: TCertificateSet read GetCerts write SetCerts;
    property Crls: TCertificateRevocationLists read GetCrls write SetCrls;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo
          TRecipientIdentifier}

  TRecipientIdentifierEnum = (
    rieUndefined, rieIssuerAndSerialNumber, rieSubjectKeyIdentifier);

  TRecipientIdentifier = class(TASNChoiceWrapper)
  private
    function GetAsIssuerAndSerialNumber: TIssuerAndSerialNumber;
    procedure SetAsIssuerAndSerialNumber(const Value: TIssuerAndSerialNumber);
    function GetChoice: TRecipientIdentifierEnum;
    procedure SetChoice(const Value: TRecipientIdentifierEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsIssuerAndSerialNumber: TIssuerAndSerialNumber read GetAsIssuerAndSerialNumber write SetAsIssuerAndSerialNumber;
  published
    property Choice: TRecipientIdentifierEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo
          TKeyEncryptionAlgorithmIdentifier
           TKeparams
            TKeyWrapAlgorithmIdentifier
             TKwparams}

  TKwparamsEnum = (
    keUndefined, keIdAlgCMS3DESwrap, keIdAlgCMSRC2wrap, keIdAes128Wrap,
    keIdAes192Wrap, keIdAes256Wrap);

  TKwparams = class(TASNChoiceWrapper)
  private
    function GetAsAlg_CMSRC2wrap: TIntegerWrapper;
    procedure SetAsAlg_CMSRC2wrap(const Value: TIntegerWrapper);
    function GetChoice: TKwparamsEnum;
    procedure SetChoice(const Value: TKwparamsEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsAlg_CMSRC2wrap: TIntegerWrapper read GetAsAlg_CMSRC2wrap write SetAsAlg_CMSRC2wrap;
  published
    property Choice: TKwparamsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo
          TKeyEncryptionAlgorithmIdentifier
           TKeparams
            TKeyWrapAlgorithmIdentifier}

  TKeyWrapAlgorithmIdentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TKwparams;
    procedure SetParameters(const Value: TKwparams);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TKwparamsEnum): Boolean;
    procedure SetType(Id: TKwparamsEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TKwparams read GetParameters write SetParameters;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo
          TKeyEncryptionAlgorithmIdentifier
           TKeparams
            TRsaes_OAEP_Params
             TMgfalgorithmIdentifier
              TPkcs1MGFAlgorithms}

  TPkcs1MGFAlgorithmsEnum = (
    pmgfaeUndefined, pmgfaeIdMgf1);

  TPkcs1MGFAlgorithms = class(TASNChoiceWrapper)
  private
    function GetAsMgf1: TDigestAlgorithmIdentifier;
    procedure SetAsMgf1(const Value: TDigestAlgorithmIdentifier);
    function GetChoice: TPkcs1MGFAlgorithmsEnum;
    procedure SetChoice(const Value: TPkcs1MGFAlgorithmsEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsMgf1: TDigestAlgorithmIdentifier read GetAsMgf1 write SetAsMgf1;
  published
    property Choice: TPkcs1MGFAlgorithmsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo
          TKeyEncryptionAlgorithmIdentifier
           TKeparams
            TRsaes_OAEP_Params
             TMgfalgorithmIdentifier}

  TMgfalgorithmIdentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TPkcs1MGFAlgorithms;
    procedure SetParameters(const Value: TPkcs1MGFAlgorithms);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TPkcs1MGFAlgorithmsEnum): Boolean;
    procedure SetType(Id: TPkcs1MGFAlgorithmsEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TPkcs1MGFAlgorithms read GetParameters write SetParameters;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo
          TKeyEncryptionAlgorithmIdentifier
           TKeparams
            TRsaes_OAEP_Params
             TPsfalgorithmIdentifier
              TPkcs1pSourceAlgorithms}

  TPkcs1pSourceAlgorithmsEnum = (
    psaeUndefined, psaeIdPSpecified);

  TPkcs1pSourceAlgorithms = class(TASNChoiceWrapper)
  private
    function GetAsPSpecified: TOctetString;
    procedure SetAsPSpecified(const Value: TOctetString);
    function GetChoice: TPkcs1pSourceAlgorithmsEnum;
    procedure SetChoice(const Value: TPkcs1pSourceAlgorithmsEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsPSpecified: TOctetString read GetAsPSpecified write SetAsPSpecified;
  published
    property Choice: TPkcs1pSourceAlgorithmsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo
          TKeyEncryptionAlgorithmIdentifier
           TKeparams
            TRsaes_OAEP_Params
             TPsfalgorithmIdentifier}

  TPsfalgorithmIdentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TPkcs1pSourceAlgorithms;
    procedure SetParameters(const Value: TPkcs1pSourceAlgorithms);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TPkcs1pSourceAlgorithmsEnum): Boolean;
    procedure SetType(Id: TPkcs1pSourceAlgorithmsEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TPkcs1pSourceAlgorithms read GetParameters write SetParameters;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo
          TKeyEncryptionAlgorithmIdentifier
           TKeparams
            TRsaes_OAEP_Params}

  TRsaes_OAEP_Params = class(TASNConstructedWrapper)
  private
    function GetHashFunc: TDigestAlgorithmIdentifier;
    procedure SetHashFunc(const Value: TDigestAlgorithmIdentifier);
    function GetMaskGenFunc: TMgfalgorithmIdentifier;
    procedure SetMaskGenFunc(const Value: TMgfalgorithmIdentifier);
    function GetPSourceFunc: TPsfalgorithmIdentifier;
    procedure SetPSourceFunc(const Value: TPsfalgorithmIdentifier);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property HashFunc: TDigestAlgorithmIdentifier read GetHashFunc write SetHashFunc;
    property MaskGenFunc: TMgfalgorithmIdentifier read GetMaskGenFunc write SetMaskGenFunc;
    property PSourceFunc: TPsfalgorithmIdentifier read GetPSourceFunc write SetPSourceFunc;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo
          TKeyEncryptionAlgorithmIdentifier
           TKeparams}

  TKeparamsEnum = (
    keeUndefined, keeRsaEncryption, keeIdAlgESDH, keeIdRSAESOAEP);

  TKeparams = class(TASNChoiceWrapper)
  private
    function GetAsAlg_ESDH: TKeyWrapAlgorithmIdentifier;
    procedure SetAsAlg_ESDH(const Value: TKeyWrapAlgorithmIdentifier);
    function GetAsRsaes_OAEP: TRsaes_OAEP_Params;
    procedure SetAsRsaes_OAEP(const Value: TRsaes_OAEP_Params);
    function GetChoice: TKeparamsEnum;
    procedure SetChoice(const Value: TKeparamsEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsAlg_ESDH: TKeyWrapAlgorithmIdentifier read GetAsAlg_ESDH write SetAsAlg_ESDH;
    property AsRsaes_OAEP: TRsaes_OAEP_Params read GetAsRsaes_OAEP write SetAsRsaes_OAEP;
  published
    property Choice: TKeparamsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo
          TKeyEncryptionAlgorithmIdentifier}

  TKeyEncryptionAlgorithmIdentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TKeparams;
    procedure SetParameters(const Value: TKeparams);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TKeparamsEnum): Boolean;
    procedure SetType(Id: TKeparamsEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TKeparams read GetParameters write SetParameters;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyTransRecipientInfo}

  TKeyTransRecipientInfo = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetRid: TRecipientIdentifier;
    procedure SetRid(const Value: TRecipientIdentifier);
    function GetKeyEncryptionAlgorithm: TKeyEncryptionAlgorithmIdentifier;
    procedure SetKeyEncryptionAlgorithm(const Value: TKeyEncryptionAlgorithmIdentifier);
    function GetEncryptedKey: TOctetString;
    procedure SetEncryptedKey(const Value: TOctetString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property Rid: TRecipientIdentifier read GetRid write SetRid;
    property KeyEncryptionAlgorithm: TKeyEncryptionAlgorithmIdentifier read GetKeyEncryptionAlgorithm write SetKeyEncryptionAlgorithm;
    property EncryptedKey: TOctetString read GetEncryptedKey write SetEncryptedKey;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyAgreeRecipientInfo
          TOriginatorIdentifierOrKey
           TOriginatorPublicKey}

  TOriginatorPublicKey = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: TAlgorithmIdentifier;
    procedure SetAlgorithm(const Value: TAlgorithmIdentifier);
    function GetPublicKey: TBitString;
    procedure SetPublicKey(const Value: TBitString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Algorithm: TAlgorithmIdentifier read GetAlgorithm write SetAlgorithm;
    property PublicKey: TBitString read GetPublicKey write SetPublicKey;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyAgreeRecipientInfo
          TOriginatorIdentifierOrKey}

  TOriginatorIdentifierOrKeyEnum = (
    oiokeUndefined, oiokeIssuerAndSerialNumber, oiokeSubjectKeyIdentifier,
    oiokeOriginatorKey);

  TOriginatorIdentifierOrKey = class(TASNChoiceWrapper)
  private
    function GetAsIssuerAndSerialNumber: TIssuerAndSerialNumber;
    procedure SetAsIssuerAndSerialNumber(const Value: TIssuerAndSerialNumber);
    function GetAsOriginatorKey: TOriginatorPublicKey;
    procedure SetAsOriginatorKey(const Value: TOriginatorPublicKey);
    function GetChoice: TOriginatorIdentifierOrKeyEnum;
    procedure SetChoice(const Value: TOriginatorIdentifierOrKeyEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsIssuerAndSerialNumber: TIssuerAndSerialNumber read GetAsIssuerAndSerialNumber write SetAsIssuerAndSerialNumber;
    property AsOriginatorKey: TOriginatorPublicKey read GetAsOriginatorKey write SetAsOriginatorKey;
  published
    property Choice: TOriginatorIdentifierOrKeyEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyAgreeRecipientInfo
          TRecipientEncryptedKeys
           TRecipientEncryptedKey
            TKeyAgreeRecipientIdentifier
             TRecipientKeyIdentifier
              TOtherKeyAttribute}

  TOtherKeyAttribute = class(TASNConstructedWrapper)
  private
    function GetKeyAttrId: ObjectIdentifier;
    procedure SetKeyAttrId(const Value: ObjectIdentifier);
    function GetKeyAttr: TAny;
    procedure SetKeyAttr(const Value: TAny);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property KeyAttrId: ObjectIdentifier read GetKeyAttrId write SetKeyAttrId;
    property KeyAttr: TAny read GetKeyAttr write SetKeyAttr;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyAgreeRecipientInfo
          TRecipientEncryptedKeys
           TRecipientEncryptedKey
            TKeyAgreeRecipientIdentifier
             TRecipientKeyIdentifier}

  TRecipientKeyIdentifier = class(TASNConstructedWrapper)
  private
    function GetSubjectKeyIdentifier: TOctetString;
    procedure SetSubjectKeyIdentifier(const Value: TOctetString);
    function GetDate: TGeneralizedTime;
    procedure SetDate(const Value: TGeneralizedTime);
    function GetOther: TOtherKeyAttribute;
    procedure SetOther(const Value: TOtherKeyAttribute);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property SubjectKeyIdentifier: TOctetString read GetSubjectKeyIdentifier write SetSubjectKeyIdentifier;
    property Date: TGeneralizedTime read GetDate write SetDate;
    property Other: TOtherKeyAttribute read GetOther write SetOther;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyAgreeRecipientInfo
          TRecipientEncryptedKeys
           TRecipientEncryptedKey
            TKeyAgreeRecipientIdentifier}

  TKeyAgreeRecipientIdentifierEnum = (
    karieUndefined, karieIssuerAndSerialNumber, karieRKeyId);

  TKeyAgreeRecipientIdentifier = class(TASNChoiceWrapper)
  private
    function GetAsIssuerAndSerialNumber: TIssuerAndSerialNumber;
    procedure SetAsIssuerAndSerialNumber(const Value: TIssuerAndSerialNumber);
    function GetAsRKeyId: TRecipientKeyIdentifier;
    procedure SetAsRKeyId(const Value: TRecipientKeyIdentifier);
    function GetChoice: TKeyAgreeRecipientIdentifierEnum;
    procedure SetChoice(const Value: TKeyAgreeRecipientIdentifierEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsIssuerAndSerialNumber: TIssuerAndSerialNumber read GetAsIssuerAndSerialNumber write SetAsIssuerAndSerialNumber;
    property AsRKeyId: TRecipientKeyIdentifier read GetAsRKeyId write SetAsRKeyId;
  published
    property Choice: TKeyAgreeRecipientIdentifierEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyAgreeRecipientInfo
          TRecipientEncryptedKeys
           TRecipientEncryptedKey}

  TRecipientEncryptedKey = class(TASNConstructedWrapper)
  private
    function GetRid: TKeyAgreeRecipientIdentifier;
    procedure SetRid(const Value: TKeyAgreeRecipientIdentifier);
    function GetEncryptedKey: TOctetString;
    procedure SetEncryptedKey(const Value: TOctetString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Rid: TKeyAgreeRecipientIdentifier read GetRid write SetRid;
    property EncryptedKey: TOctetString read GetEncryptedKey write SetEncryptedKey;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyAgreeRecipientInfo
          TRecipientEncryptedKeys}

  TRecipientEncryptedKeys = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TRecipientEncryptedKey;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TRecipientEncryptedKey;
    property Items[Index: Integer]: TRecipientEncryptedKey read GetItems; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKeyAgreeRecipientInfo}

  TKeyAgreeRecipientInfo = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetOriginator: TOriginatorIdentifierOrKey;
    procedure SetOriginator(const Value: TOriginatorIdentifierOrKey);
    function GetUkm: TOctetString;
    procedure SetUkm(const Value: TOctetString);
    function GetKeyEncryptionAlgorithm: TKeyEncryptionAlgorithmIdentifier;
    procedure SetKeyEncryptionAlgorithm(const Value: TKeyEncryptionAlgorithmIdentifier);
    function GetRecipientEncryptedKeys: TRecipientEncryptedKeys;
    procedure SetRecipientEncryptedKeys(const Value: TRecipientEncryptedKeys);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property Originator: TOriginatorIdentifierOrKey read GetOriginator write SetOriginator;
    property Ukm: TOctetString read GetUkm write SetUkm;
    property KeyEncryptionAlgorithm: TKeyEncryptionAlgorithmIdentifier read GetKeyEncryptionAlgorithm write SetKeyEncryptionAlgorithm;
    property RecipientEncryptedKeys: TRecipientEncryptedKeys read GetRecipientEncryptedKeys write SetRecipientEncryptedKeys;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKekrecipientInfo
          TKekidentifier}

  TKekidentifier = class(TASNConstructedWrapper)
  private
    function GetKeyIdentifier: TOctetString;
    procedure SetKeyIdentifier(const Value: TOctetString);
    function GetDate: TGeneralizedTime;
    procedure SetDate(const Value: TGeneralizedTime);
    function GetOther: TOtherKeyAttribute;
    procedure SetOther(const Value: TOtherKeyAttribute);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property KeyIdentifier: TOctetString read GetKeyIdentifier write SetKeyIdentifier;
    property Date: TGeneralizedTime read GetDate write SetDate;
    property Other: TOtherKeyAttribute read GetOther write SetOther;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo
         TKekrecipientInfo}

  TKekrecipientInfo = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetKekid: TKekidentifier;
    procedure SetKekid(const Value: TKekidentifier);
    function GetKeyEncryptionAlgorithm: TKeyEncryptionAlgorithmIdentifier;
    procedure SetKeyEncryptionAlgorithm(const Value: TKeyEncryptionAlgorithmIdentifier);
    function GetEncryptedKey: TOctetString;
    procedure SetEncryptedKey(const Value: TOctetString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property Kekid: TKekidentifier read GetKekid write SetKekid;
    property KeyEncryptionAlgorithm: TKeyEncryptionAlgorithmIdentifier read GetKeyEncryptionAlgorithm write SetKeyEncryptionAlgorithm;
    property EncryptedKey: TOctetString read GetEncryptedKey write SetEncryptedKey;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos
        TRecipientInfo}

  TRecipientInfoEnum = (
    reieUndefined, reieKtri, reieKari, reieKekri);

  TRecipientInfo = class(TASNChoiceWrapper)
  private
    function GetAsKtri: TKeyTransRecipientInfo;
    procedure SetAsKtri(const Value: TKeyTransRecipientInfo);
    function GetAsKari: TKeyAgreeRecipientInfo;
    procedure SetAsKari(const Value: TKeyAgreeRecipientInfo);
    function GetAsKekri: TKekrecipientInfo;
    procedure SetAsKekri(const Value: TKekrecipientInfo);
    function GetChoice: TRecipientInfoEnum;
    procedure SetChoice(const Value: TRecipientInfoEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsKtri: TKeyTransRecipientInfo read GetAsKtri write SetAsKtri;
    property AsKari: TKeyAgreeRecipientInfo read GetAsKari write SetAsKari;
    property AsKekri: TKekrecipientInfo read GetAsKekri write SetAsKekri;
  published
    property Choice: TRecipientInfoEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TRecipientInfos}

  TRecipientInfos = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TRecipientInfo;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TRecipientInfo;
    property Items[Index: Integer]: TRecipientInfo read GetItems; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TEncryptedContentInfo
        TContentEncryptionAlgorithmIdentifier
         TCeparams
          TRc2CBCParameter}

  TRc2CBCParameter = class(TASNConstructedWrapper)
  private
    function GetRc2ParameterVersion: TIntegerWrapper;
    procedure SetRc2ParameterVersion(const Value: TIntegerWrapper);
    function GetIv: TOctetString;
    procedure SetIv(const Value: TOctetString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Rc2ParameterVersion: TIntegerWrapper read GetRc2ParameterVersion write SetRc2ParameterVersion;
    property Iv: TOctetString read GetIv write SetIv;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TEncryptedContentInfo
        TContentEncryptionAlgorithmIdentifier
         TCeparams}

  TCeparamsEnum = (
    ceUndefined, ceDesEde3Cbc, ceRc2Cbc, ceIdAes128CBC, ceIdAes192CBC,
    ceIdAes256CBC);

  TCeparams = class(TASNChoiceWrapper)
  private
    function GetAsDes_Ede3_Cbc: TOctetString;
    procedure SetAsDes_Ede3_Cbc(const Value: TOctetString);
    function GetAsRc2_Cbc: TRc2CBCParameter;
    procedure SetAsRc2_Cbc(const Value: TRc2CBCParameter);
    function GetAsAes128_CBC: TOctetString;
    procedure SetAsAes128_CBC(const Value: TOctetString);
    function GetAsAes192_CBC: TOctetString;
    procedure SetAsAes192_CBC(const Value: TOctetString);
    function GetAsAes256_CBC: TOctetString;
    procedure SetAsAes256_CBC(const Value: TOctetString);
    function GetChoice: TCeparamsEnum;
    procedure SetChoice(const Value: TCeparamsEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsDes_Ede3_Cbc: TOctetString read GetAsDes_Ede3_Cbc write SetAsDes_Ede3_Cbc;
    property AsRc2_Cbc: TRc2CBCParameter read GetAsRc2_Cbc write SetAsRc2_Cbc;
    property AsAes128_CBC: TOctetString read GetAsAes128_CBC write SetAsAes128_CBC;
    property AsAes192_CBC: TOctetString read GetAsAes192_CBC write SetAsAes192_CBC;
    property AsAes256_CBC: TOctetString read GetAsAes256_CBC write SetAsAes256_CBC;
  published
    property Choice: TCeparamsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TEncryptedContentInfo
        TContentEncryptionAlgorithmIdentifier}

  TContentEncryptionAlgorithmIdentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TCeparams;
    procedure SetParameters(const Value: TCeparams);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TCeparamsEnum): Boolean;
    procedure SetType(Id: TCeparamsEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TCeparams read GetParameters write SetParameters;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TEncryptedContentInfo}

  TEncryptedContentInfo = class(TASNConstructedWrapper)
  private
    function GetContentType: ObjectIdentifier;
    procedure SetContentType(const Value: ObjectIdentifier);
    function GetContentEncryptionAlgorithm: TContentEncryptionAlgorithmIdentifier;
    procedure SetContentEncryptionAlgorithm(const Value: TContentEncryptionAlgorithmIdentifier);
    function GetEncryptedContent: TOctetString;
    procedure SetEncryptedContent(const Value: TOctetString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property ContentType: ObjectIdentifier read GetContentType write SetContentType;
    property ContentEncryptionAlgorithm: TContentEncryptionAlgorithmIdentifier read GetContentEncryptionAlgorithm write SetContentEncryptionAlgorithm;
    property EncryptedContent: TOctetString read GetEncryptedContent write SetEncryptedContent;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData
       TUnprotectedAttributes}

  TUnprotectedAttributes = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TCMSAttribute;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TCMSAttribute;
    property Items[Index: Integer]: TCMSAttribute read GetItems; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEnvelopedData}

  TEnvelopedData = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetOriginatorInfo: TOriginatorInfo;
    procedure SetOriginatorInfo(const Value: TOriginatorInfo);
    function GetRecipientInfos: TRecipientInfos;
    procedure SetRecipientInfos(const Value: TRecipientInfos);
    function GetEncryptedContentInfo: TEncryptedContentInfo;
    procedure SetEncryptedContentInfo(const Value: TEncryptedContentInfo);
    function GetUnprotectedAttrs: TUnprotectedAttributes;
    procedure SetUnprotectedAttrs(const Value: TUnprotectedAttributes);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property OriginatorInfo: TOriginatorInfo read GetOriginatorInfo write SetOriginatorInfo;
    property RecipientInfos: TRecipientInfos read GetRecipientInfos write SetRecipientInfos;
    property EncryptedContentInfo: TEncryptedContentInfo read GetEncryptedContentInfo write SetEncryptedContentInfo;
    property UnprotectedAttrs: TUnprotectedAttributes read GetUnprotectedAttrs write SetUnprotectedAttrs;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TSignedAndEnvelopedData}

  TSignedAndEnvelopedData = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetRecipientInfos: TRecipientInfos;
    procedure SetRecipientInfos(const Value: TRecipientInfos);
    function GetDigestAlgorithms: TDigestAlgorithmIdentifiers;
    procedure SetDigestAlgorithms(const Value: TDigestAlgorithmIdentifiers);
    function GetEncryptedContentInfo: TEncryptedContentInfo;
    procedure SetEncryptedContentInfo(const Value: TEncryptedContentInfo);
    function GetCertificates: TCertificateSet;
    procedure SetCertificates(const Value: TCertificateSet);
    function GetCRLs: TCertificateRevocationLists;
    procedure SetCRLs(const Value: TCertificateRevocationLists);
    function GetSignerInfos: TSignerInfos;
    procedure SetSignerInfos(const Value: TSignerInfos);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property RecipientInfos: TRecipientInfos read GetRecipientInfos write SetRecipientInfos;
    property DigestAlgorithms: TDigestAlgorithmIdentifiers read GetDigestAlgorithms write SetDigestAlgorithms;
    property EncryptedContentInfo: TEncryptedContentInfo read GetEncryptedContentInfo write SetEncryptedContentInfo;
    property Certificates: TCertificateSet read GetCertificates write SetCertificates;
    property CRLs: TCertificateRevocationLists read GetCRLs write SetCRLs;
    property SignerInfos: TSignerInfos read GetSignerInfos write SetSignerInfos;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TDigestedData}

  TDigestedData = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetDigestAlgorithm: TDigestAlgorithmIdentifier;
    procedure SetDigestAlgorithm(const Value: TDigestAlgorithmIdentifier);
    function GetEncapContentInfo: TEncapsulatedContentInfo;
    procedure SetEncapContentInfo(const Value: TEncapsulatedContentInfo);
    function GetDigest: TOctetString;
    procedure SetDigest(const Value: TOctetString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property DigestAlgorithm: TDigestAlgorithmIdentifier read GetDigestAlgorithm write SetDigestAlgorithm;
    property EncapContentInfo: TEncapsulatedContentInfo read GetEncapContentInfo write SetEncapContentInfo;
    property Digest: TOctetString read GetDigest write SetDigest;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TEncryptedData}

  TEncryptedData = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetEncryptedContentInfo: TEncryptedContentInfo;
    procedure SetEncryptedContentInfo(const Value: TEncryptedContentInfo);
    function GetUnprotectedAttrs: TUnprotectedAttributes;
    procedure SetUnprotectedAttrs(const Value: TUnprotectedAttributes);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property EncryptedContentInfo: TEncryptedContentInfo read GetEncryptedContentInfo write SetEncryptedContentInfo;
    property UnprotectedAttrs: TUnprotectedAttributes read GetUnprotectedAttrs write SetUnprotectedAttrs;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TAuthenticatedData
       TMessageAuthenticationCodeAlgorithm
        TMacparams}

  TMacparamsEnum = (
    meUndefined, meHMACSHA1);

  TMacparams = class(TASNChoiceWrapper)
  private
    function GetChoice: TMacparamsEnum;
    procedure SetChoice(const Value: TMacparamsEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Choice: TMacparamsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TAuthenticatedData
       TMessageAuthenticationCodeAlgorithm}

  TMessageAuthenticationCodeAlgorithm = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TMacparams;
    procedure SetParameters(const Value: TMacparams);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TMacparamsEnum): Boolean;
    procedure SetType(Id: TMacparamsEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TMacparams read GetParameters write SetParameters;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TAuthenticatedData
       TAuthAttributes}

  TAuthAttributes = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TCMSAttribute;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TCMSAttribute;
    property Items[Index: Integer]: TCMSAttribute read GetItems; default;
  end;

{ Declaration tree: 
    TContentInfo
     TContent
      TAuthenticatedData}

  TAuthenticatedData = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetOriginatorInfo: TOriginatorInfo;
    procedure SetOriginatorInfo(const Value: TOriginatorInfo);
    function GetRecipientInfos: TRecipientInfos;
    procedure SetRecipientInfos(const Value: TRecipientInfos);
    function GetMacAlgorithm: TMessageAuthenticationCodeAlgorithm;
    procedure SetMacAlgorithm(const Value: TMessageAuthenticationCodeAlgorithm);
    function GetDigestAlgorithm: TDigestAlgorithmIdentifier;
    procedure SetDigestAlgorithm(const Value: TDigestAlgorithmIdentifier);
    function GetEncapContentInfo: TEncapsulatedContentInfo;
    procedure SetEncapContentInfo(const Value: TEncapsulatedContentInfo);
    function GetAuthenticatedAttributes: TAuthAttributes;
    procedure SetAuthenticatedAttributes(const Value: TAuthAttributes);
    function GetMac: TOctetString;
    procedure SetMac(const Value: TOctetString);
    function GetUnauthenticatedAttributes: TUnauthAttributes;
    procedure SetUnauthenticatedAttributes(const Value: TUnauthAttributes);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property OriginatorInfo: TOriginatorInfo read GetOriginatorInfo write SetOriginatorInfo;
    property RecipientInfos: TRecipientInfos read GetRecipientInfos write SetRecipientInfos;
    property MacAlgorithm: TMessageAuthenticationCodeAlgorithm read GetMacAlgorithm write SetMacAlgorithm;
    property DigestAlgorithm: TDigestAlgorithmIdentifier read GetDigestAlgorithm write SetDigestAlgorithm;
    property EncapContentInfo: TEncapsulatedContentInfo read GetEncapContentInfo write SetEncapContentInfo;
    property AuthenticatedAttributes: TAuthAttributes read GetAuthenticatedAttributes write SetAuthenticatedAttributes;
    property Mac: TOctetString read GetMac write SetMac;
    property UnauthenticatedAttributes: TUnauthAttributes read GetUnauthenticatedAttributes write SetUnauthenticatedAttributes;
  end;

{ Declaration tree: 
    TContentInfo
     TContent}

  TContentEnum = (
    coeUndefined, coeIdData, coeIdSignedData, coeIdEnvelopedData,
    coeIdSignedAndEnvelopedData, coeIdDigestedData, coeIdEncryptedData,
    coeIdCtAuthData);

  TContent = class(TASNChoiceWrapper)
  private
    function GetAsData: TOctetString;
    procedure SetAsData(const Value: TOctetString);
    function GetAsSignedData: TSignedData;
    procedure SetAsSignedData(const Value: TSignedData);
    function GetAsEnvelopedData: TEnvelopedData;
    procedure SetAsEnvelopedData(const Value: TEnvelopedData);
    function GetAsSignedAndEnvelopedData: TSignedAndEnvelopedData;
    procedure SetAsSignedAndEnvelopedData(const Value: TSignedAndEnvelopedData);
    function GetAsDigestedData: TDigestedData;
    procedure SetAsDigestedData(const Value: TDigestedData);
    function GetAsEncryptedData: TEncryptedData;
    procedure SetAsEncryptedData(const Value: TEncryptedData);
    function GetAsCt_AuthData: TAuthenticatedData;
    procedure SetAsCt_AuthData(const Value: TAuthenticatedData);
    function GetChoice: TContentEnum;
    procedure SetChoice(const Value: TContentEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsData: TOctetString read GetAsData write SetAsData;
    property AsSignedData: TSignedData read GetAsSignedData write SetAsSignedData;
    property AsEnvelopedData: TEnvelopedData read GetAsEnvelopedData write SetAsEnvelopedData;
    property AsSignedAndEnvelopedData: TSignedAndEnvelopedData read GetAsSignedAndEnvelopedData write SetAsSignedAndEnvelopedData;
    property AsDigestedData: TDigestedData read GetAsDigestedData write SetAsDigestedData;
    property AsEncryptedData: TEncryptedData read GetAsEncryptedData write SetAsEncryptedData;
    property AsCt_AuthData: TAuthenticatedData read GetAsCt_AuthData write SetAsCt_AuthData;
  published
    property Choice: TContentEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TContentInfo}

  TContentInfo = class(TASNConstructedWrapper)
  private
    function GetContentType: ObjectIdentifier;
    procedure SetContentType(const Value: ObjectIdentifier);
    function GetContent: TContent;
    procedure SetContent(const Value: TContent);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TContentEnum): Boolean;
    procedure SetType(Id: TContentEnum);
  published
    property ContentType: ObjectIdentifier read GetContentType write SetContentType;
    property Content: TContent read GetContent write SetContent;
  end;
         
type
  TPKCS7DataType = (pNone,pData,pSignedData,pEnvelopedData,pSignedAndEnvelopedData,
                    pDigestedData,pEncryptedData,pAuthenticatedData,
                    pAuthenticode);
  TPKCS7DataTypes = set of TPKCS7DataType;
const
  AllDataTypes = [pData..High(TPKCS7DataType)];

function OIDToContentType(OID: string; var AContentType: TPKCS7DataType): Boolean;
function ContentTypeToOID(AContentType: TPKCS7DataType): string;

procedure NewDataStruct(var ContentInfo: TASN1Struct);
procedure NewSignedDataStruct(var ContentInfo: TASN1Struct);
procedure NewEnvelopedDataStruct(var ContentInfo: TASN1Struct);
procedure NewSignedAndEnvelopedDataStruct(var ContentInfo: TASN1Struct);
procedure NewDigestedDataStruct(var ContentInfo: TASN1Struct);
procedure NewEncryptedDataStruct(var ContentInfo: TASN1Struct);
procedure NewAuthenticatedDataStruct(var ContentInfo: TASN1Struct);

function CheckCMSSyntax(var ContentInfo: TASN1Struct;
                        var AContentType: TPKCS7DataType;
                        ValidTypes: TPKCS7DataTypes = AllDataTypes): Boolean;

{$IFNDEF INIT_SECTIONS}
procedure Initialize;
{$ENDIF}
{$IFNDEF FINI_SECTIONS}
procedure Finalize;
{$ENDIF}

implementation

uses
  Pkix, ReadStrm;

var
  GlobalObject: TASN1Struct;

procedure InitGlobalObject;
var
  SS: TStringStream;
begin
  Assert(PKIX_Cert.ASNModule <> '');
  Assert(PKIX_CRL.ASNModule <> '');
  SS := TStringStream.Create(ASNModule);
  try
    GlobalObject := TASN1Struct.Create;
    GlobalObject._AddRef;
    GlobalObject.LoadFromStream(SS,fmtASN1);
  finally
    SS.Free;
  end;
end;

type
  THack = class(TASN1Struct);

var
  DataTmpl,
  SignedDataTmpl,
  EnvelopedDataTmpl,
  SignedAndEnvelopedDataTmpl,
  DigestedDataTmpl,
  EncryptedDataTmpl,
  AuthenticatedDataTmpl: TASN1Struct;

procedure SetUpTemplates;
begin
  DataTmpl := TASN1Struct.Create;
  SignedDataTmpl := TASN1Struct.Create;
  EnvelopedDataTmpl := TASN1Struct.Create;
  SignedAndEnvelopedDataTmpl := TASN1Struct.Create;
  DigestedDataTmpl := TASN1Struct.Create;
  EncryptedDataTmpl := TASN1Struct.Create;
  AuthenticatedDataTmpl := TASN1Struct.Create;
  DataTmpl._AddRef;
  SignedDataTmpl._AddRef;
  EnvelopedDataTmpl._AddRef;
  SignedAndEnvelopedDataTmpl._AddRef;
  DigestedDataTmpl._AddRef;
  EncryptedDataTmpl._AddRef;
  AuthenticatedDataTmpl._AddRef;

  DataTmpl.CopyTypeInfo(GlobalObject);
  SignedDataTmpl.CopyTypeInfo(GlobalObject);
  EnvelopedDataTmpl.CopyTypeInfo(GlobalObject);
  SignedAndEnvelopedDataTmpl.CopyTypeInfo(GlobalObject);
  DigestedDataTmpl.CopyTypeInfo(GlobalObject);
  EncryptedDataTmpl.CopyTypeInfo(GlobalObject);
  AuthenticatedDataTmpl.CopyTypeInfo(GlobalObject);

  DataTmpl.EditField('contentType',id_data);
  SignedDataTmpl.EditField('contentType',id_signedData);
  EnvelopedDataTmpl.EditField('contentType',id_envelopedData);
  SignedAndEnvelopedDataTmpl.EditField('contentType',id_signedAndEnvelopedData);
  DigestedDataTmpl.EditField('contentType',id_digestedData);
  EncryptedDataTmpl.EditField('contentType',id_encryptedData);
  AuthenticatedDataTmpl.EditField('contentType',id_ct_authData);
end;

procedure CleanUpTemplates;
begin
  ASN1.ReleaseASN1Struct(DataTmpl);
  ASN1.ReleaseASN1Struct(SignedDataTmpl);
  ASN1.ReleaseASN1Struct(EnvelopedDataTmpl);
  ASN1.ReleaseASN1Struct(SignedAndEnvelopedDataTmpl);
  ASN1.ReleaseASN1Struct(DigestedDataTmpl);
  ASN1.ReleaseASN1Struct(EncryptedDataTmpl);
  ASN1.ReleaseASN1Struct(AuthenticatedDataTmpl);
end;

procedure Initialize;
begin
  if GlobalObject = nil then begin
    InitGlobalObject;
    SetUpTemplates;
  end;
end;

procedure Finalize;
begin
  if Assigned(GlobalObject) then begin
    CleanUpTemplates;
    ASN1.ReleaseASN1Struct(GlobalObject);
  end;
end;

function ContentTypeToOID(AContentType: TPKCS7DataType): string;
begin
  case AContentType of
    pData: Result := id_data;
    pSignedData: Result := id_signedData;
    pEnvelopedData: Result := id_envelopedData;
    pSignedAndEnvelopedData: Result := id_signedAndEnvelopedData;
    pDigestedData: Result := id_digestedData;
    pEncryptedData: Result := id_encryptedData;
    pAuthenticatedData: Result := id_ct_authData;
    pAuthenticode: Result := '1.3.6.1.4.1.311.2.1.4';
  else
    Result := '';
  end;
end;

function OIDToContentType(OID: string; var AContentType: TPKCS7DataType): Boolean;
begin
  Result := True;
  if OID = id_data then
    AContentType := pData
  else if OID = id_signedData then
    AContentType := pSignedData
  else if OID = id_envelopedData then
    AContentType := pEnvelopedData
  else if OID = id_signedAndEnvelopedData then
    AContentType := pSignedAndEnvelopedData
  else if OID = id_digestedData then
    AContentType := pDigestedData
  else if OID = id_encryptedData then
    AContentType := pEncryptedData
  else if OID = id_digestedData then
    AContentType := pDigestedData
  else if OID = id_ct_authData then
    AContentType := pAuthenticatedData
  else if OID = '1.3.6.1.4.1.311.2.1.4' then
    AContentType := pAuthenticode
  else begin
    AContentType := pNone;
    Result := False;
  end;
end;
    
procedure NewDataStruct(var ContentInfo: TASN1Struct);
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  if not Assigned(ContentInfo) then
    ContentInfo := TASN1Struct.Create;
  ContentInfo.NoChangeEvent := True;
  ContentInfo.Assign(DataTmpl);
end;

procedure NewSignedDataStruct(var ContentInfo: TASN1Struct);
begin                         
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  if not Assigned(ContentInfo) then
    ContentInfo := TASN1Struct.Create;
  ContentInfo.NoChangeEvent := True;
  ContentInfo.Assign(SignedDataTmpl);                                                   
  ContentInfo.FindField('/content//version').EditContent(Integer(1));
  ContentInfo.FindField('/content//encapContentInfo/eContentType').EditContent(id_data);
  ContentInfo.ForgetOptionalDefault := True;
end;

procedure NewEnvelopedDataStruct(var ContentInfo: TASN1Struct);
begin                            
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  if not Assigned(ContentInfo) then
    ContentInfo := TASN1Struct.Create;
  ContentInfo.NoChangeEvent := True;
  ContentInfo.Assign(EnvelopedDataTmpl);
  ContentInfo.ForgetOptionalDefault := True;
end;

procedure NewSignedAndEnvelopedDataStruct(var ContentInfo: TASN1Struct);
begin                                
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  if not Assigned(ContentInfo) then
    ContentInfo := TASN1Struct.Create;
  ContentInfo.NoChangeEvent := True;
  ContentInfo.Assign(SignedAndEnvelopedDataTmpl);
  ContentInfo.ForgetOptionalDefault := True;
end;

procedure NewDigestedDataStruct(var ContentInfo: TASN1Struct);
begin                           
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  if not Assigned(ContentInfo) then
    ContentInfo := TASN1Struct.Create;
  ContentInfo.NoChangeEvent := True;
  ContentInfo.Assign(DigestedDataTmpl);
end;

procedure NewEncryptedDataStruct(var ContentInfo: TASN1Struct);
begin                         
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  if not Assigned(ContentInfo) then
    ContentInfo := TASN1Struct.Create;
  ContentInfo.NoChangeEvent := True;
  ContentInfo.Assign(EncryptedDataTmpl);
end;

procedure NewAuthenticatedDataStruct(var ContentInfo: TASN1Struct);
begin                      
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  if not Assigned(ContentInfo) then
    ContentInfo := TASN1Struct.Create;
  ContentInfo.NoChangeEvent := True;
  ContentInfo.Assign(AuthenticatedDataTmpl);
  ContentInfo.ForgetOptionalDefault := True;
end;

function CheckCMSSyntax(var ContentInfo: TASN1Struct;
                        var AContentType: TPKCS7DataType;
                        ValidTypes: TPKCS7DataTypes = AllDataTypes): Boolean;
var
  C: TASN1Struct;
  F: PASN1Struct;
  MS: TSecureMemoryStream;
begin                
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  AContentType := pNone;
  if (ContentInfo.ModuleName = DataTmpl.ModuleName) and
     (ContentInfo.TypeName = DataTmpl.TypeName)then begin
    F := ContentInfo.FindField('contentType');
    if Assigned(F) then begin
      Result := OIDToContentType(F.ContentAsOID,AContentType);
      if Result then
        Result := AContentType in ValidTypes;
    end else
      Result := False;
  end else if (ContentInfo.ItemCount >= 2) and
              (ContentInfo.Items[0].ActualTag = V_ASN1_OBJECT) and
              (ContentInfo.Items[1].Tag = 0) and
              ContentInfo.Items[1].Constructed and
              (ContentInfo.Items[1].Cls = V_ASN1_CONTEXT_SPECIFIC) then begin
    F := ContentInfo.Items[0];
    Result := OIDToContentType(F.ContentAsOID,AContentType);
    if Result then begin
      Result := AContentType in ValidTypes;
      if Result then begin
        C := nil;
        try
          NewDataStruct(C);
          MS := TSecureMemoryStream.Create;
          try
            ContentInfo.SaveToStream(MS,fmtDER);
            try
              MS.Position := 0;
              C.LoadFromStream(MS,fmtDER);
              if not ContentInfo.ReadOnly then begin
                if ContentInfo.Owner = nil then begin
                  ContentInfo._Release;
                  ContentInfo := C;
                  C._AddRef;
                  C := nil;
                end else
                  ContentInfo.Assign(C);
              end;
            except
              Result := False;
            end;
          finally
            MS.Free;
          end;
        finally
          C.Free;
        end;
      end;
    end;
  end else
    Result := False;
end;

{ TDigParams }

constructor TDigParams.Create;
var
  F: TASN1Struct;
begin       
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('digestAlgorithms')^.Template;
    F := F.FindField('parameters')^;
    FData.CopyTypeInfo(F);
  end;
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

constructor TDigestAlgorithmIdentifier.Create;
var
  F: TASN1Struct;
begin       
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('digestAlgorithms')^.Template;
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

{ TDigestAlgorithmIdentifiers }

constructor TDigestAlgorithmIdentifiers.Create;
var
  F: TASN1Struct;
begin                           
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TDigestAlgorithmIdentifier;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^;
    F := F.FindField('digestAlgorithms')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TDigestAlgorithmIdentifiers.Destroy;
begin
  inherited Destroy;
end;

function TDigestAlgorithmIdentifiers.GetUniqueItem(Id: TDigParamsEnum): TDigestAlgorithmIdentifier;
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

function TDigestAlgorithmIdentifiers.AddUniqueItem(Id: TDigParamsEnum): TDigestAlgorithmIdentifier;
begin
  Result := GetUniqueItem(Id);
  if Result = nil then begin
    Result := Add;
    Result.SetType(Id);
  end;
end;

function TDigestAlgorithmIdentifiers.Add: TDigestAlgorithmIdentifier;
begin
  Result := TDigestAlgorithmIdentifier(InternalAdd);
end;

function TDigestAlgorithmIdentifiers.GetItems(Index: Integer): TDigestAlgorithmIdentifier;
begin
  Result := TDigestAlgorithmIdentifier(InternalGetItems(Index));
end;

{ TEncapsulatedContentInfo }

constructor TEncapsulatedContentInfo.Create;
var
  F: TASN1Struct;
begin                    
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^;
    F := F.FindField('encapContentInfo')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[1]^.Items[0]^,nil^);
end;

destructor TEncapsulatedContentInfo.Destroy;
begin
  inherited Destroy;
end;

function TEncapsulatedContentInfo.GetEContentType: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TEncapsulatedContentInfo.SetEContentType(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TEncapsulatedContentInfo.GetEContent: TOctetString;
begin
  Result := FItems[1];
end;

procedure TEncapsulatedContentInfo.SetEContent(const Value: TOctetString);
begin
  TOctetString(FItems[1]).Assign(Value);
end;

{ TUnauthAttributes }

constructor TUnauthAttributes.Create;
var
  F: TASN1Struct;
begin                  
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TAttribute;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('certificates')^.Template.Choices[1]^;
    F := F.FindField('/extendedCertificateInfo/attributes')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TUnauthAttributes.Destroy;
begin
  inherited Destroy;
end;

function TUnauthAttributes.Add: TCMSAttribute;
begin
  Result := TCMSAttribute(InternalAdd);
end;

function TUnauthAttributes.GetItems(Index: Integer): TCMSAttribute;
begin
  Result := TCMSAttribute(InternalGetItems(Index));
end;

{ TExtendedCertificateInfo }

constructor TExtendedCertificateInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('certificates')^.Template.Choices[1]^;
    F := F.FindField('extendedCertificateInfo')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TCertificate,nil,FData.Items[1]^,nil^);
  FieldFactory(TUnauthAttributes,nil,FData.Items[2]^,nil^);
end;

destructor TExtendedCertificateInfo.Destroy;
begin
  inherited Destroy;
end;

function TExtendedCertificateInfo.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TExtendedCertificateInfo.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TExtendedCertificateInfo.GetCertificate: TCertificate;
begin
  Result := FItems[1];
end;

procedure TExtendedCertificateInfo.SetCertificate(const Value: TCertificate);
begin
  TCertificate(FItems[1]).Assign(Value);
end;

function TExtendedCertificateInfo.GetAttributes: TUnauthAttributes;
begin
  Result := FItems[2];
end;

procedure TExtendedCertificateInfo.SetAttributes(const Value: TUnauthAttributes);
begin
  TUnauthAttributes(FItems[2]).Assign(Value);
end;

{ TSigParams }

constructor TSigParams.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('certificates')^.Template.Choices[1]^;
    F := F.FindField('/signatureAlgorithm/parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
end;

destructor TSigParams.Destroy;
begin
  inherited Destroy;
end;

function TSigParams.GetChoice: TSigParamsEnum;
begin
  Result := TSigParamsEnum(InternalGetChoice);
end;

procedure TSigParams.SetChoice(const Value: TSigParamsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TSignatureAlgorithmIdentifier }

constructor TSignatureAlgorithmIdentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('certificates')^.Template.Choices[1]^;
    F := F.FindField('signatureAlgorithm')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TSigParams,nil,FData.Items[1]^,nil^);
end;

destructor TSignatureAlgorithmIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TSignatureAlgorithmIdentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TSignatureAlgorithmIdentifier.SetAlgorithm(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TSignatureAlgorithmIdentifier.GetParameters: TSigParams;
begin
  Result := FItems[1];
end;

procedure TSignatureAlgorithmIdentifier.SetParameters(const Value: TSigParams);
begin
  TSigParams(FItems[1]).Assign(Value);
end;

function TSignatureAlgorithmIdentifier.IsType(Id: TSigParamsEnum): Boolean;
begin
  Result := TSigParams(FItems[1]).Choice = Id;
end;

procedure TSignatureAlgorithmIdentifier.SetType(Id: TSigParamsEnum);
begin
  TSigParams(FItems[1]).Choice := Id;
end;

{ TExtendedCertificate }

constructor TExtendedCertificate.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('certificates')^.Template.Choices[1]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TExtendedCertificateInfo,nil,FData.Items[0]^,nil^);
  FieldFactory(TSignatureAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[2]^,nil^);
end;

destructor TExtendedCertificate.Destroy;
begin
  inherited Destroy;
end;

function TExtendedCertificate.GetExtendedCertificateInfo: TExtendedCertificateInfo;
begin
  Result := FItems[0];
end;

procedure TExtendedCertificate.SetExtendedCertificateInfo(const Value: TExtendedCertificateInfo);
begin
  TExtendedCertificateInfo(FItems[0]).Assign(Value);
end;

function TExtendedCertificate.GetSignatureAlgorithm: TSignatureAlgorithmIdentifier;
begin
  Result := FItems[1];
end;

procedure TExtendedCertificate.SetSignatureAlgorithm(const Value: TSignatureAlgorithmIdentifier);
begin
  TSignatureAlgorithmIdentifier(FItems[1]).Assign(Value);
end;

function TExtendedCertificate.GetSignature: TBitString;
begin
  Result := FItems[2];
end;

procedure TExtendedCertificate.SetSignature(const Value: TBitString);
begin
  TBitString(FItems[2]).Assign(Value);
end;

{ TCertificateChoices }

constructor TCertificateChoices.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('certificates')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TCertificate);
  FChoiceList.Add(TExtendedCertificate);
end;

destructor TCertificateChoices.Destroy;
begin
  inherited Destroy;
end;

function TCertificateChoices.GetAsCertificate: TCertificate;
begin
  Result := (FSelected as TCertificate);
end;

procedure TCertificateChoices.SetAsCertificate(const Value: TCertificate);
begin
  InternalSelectChoice(0);
  (FSelected as TCertificate).Assign(Value);
end;

function TCertificateChoices.GetAsExtendedCertificate: TExtendedCertificate;
begin
  Result := (FSelected as TExtendedCertificate);
end;

procedure TCertificateChoices.SetAsExtendedCertificate(const Value: TExtendedCertificate);
begin
  InternalSelectChoice(1);
  (FSelected as TExtendedCertificate).Assign(Value);
end;

function TCertificateChoices.GetChoice: TCertificateChoicesEnum;
begin
  Result := TCertificateChoicesEnum(InternalGetChoice);
end;

procedure TCertificateChoices.SetChoice(const Value: TCertificateChoicesEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TCertificateSet }

constructor TCertificateSet.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TCertificateChoices;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^;
    F := F.FindField('certificates')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TCertificateSet.Destroy;
begin
  inherited Destroy;
end;

function TCertificateSet.Add: TCertificateChoices;
begin
  Result := TCertificateChoices(InternalAdd);
end;

function TCertificateSet.GetItems(Index: Integer): TCertificateChoices;
begin
  Result := TCertificateChoices(InternalGetItems(Index));
end;

{ TCertificateRevocationLists }

constructor TCertificateRevocationLists.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TCertificateList;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^;
    F := F.FindField('crls')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TCertificateRevocationLists.Destroy;
begin
  inherited Destroy;
end;

function TCertificateRevocationLists.Add: TCertificateList;
begin
  Result := TCertificateList(InternalAdd);
end;

function TCertificateRevocationLists.GetItems(Index: Integer): TCertificateList;
begin
  Result := TCertificateList(InternalGetItems(Index));
end;

{ TIssuerAndSerialNumber }

constructor TIssuerAndSerialNumber.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('signerInfos')^.Template.FindField('sid')^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TName,nil,FData.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[1]^,nil^);
end;

destructor TIssuerAndSerialNumber.Destroy;
begin
  inherited Destroy;
end;

function TIssuerAndSerialNumber.GetIssuer: TName;
begin
  Result := FItems[0];
end;

procedure TIssuerAndSerialNumber.SetIssuer(const Value: TName);
begin
  TName(FItems[0]).Assign(Value);
end;

function TIssuerAndSerialNumber.GetSerialNumber: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TIssuerAndSerialNumber.SetSerialNumber(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

{ TSignerIdentifier }

constructor TSignerIdentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('signerInfos')^.Template;
    F := F.FindField('sid')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TIssuerAndSerialNumber);
  FChoiceList.Add(TOctetString);
end;

destructor TSignerIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TSignerIdentifier.GetAsIssuerAndSerialNumber: TIssuerAndSerialNumber;
begin
  Result := (FSelected as TIssuerAndSerialNumber);
end;

procedure TSignerIdentifier.SetAsIssuerAndSerialNumber(const Value: TIssuerAndSerialNumber);
begin
  InternalSelectChoice(0);
  (FSelected as TIssuerAndSerialNumber).Assign(Value);
end;

function TSignerIdentifier.GetAsSubjectKeyIdentifier: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TSignerIdentifier.SetAsSubjectKeyIdentifier(const Value: TOctetString);
begin
  InternalSelectChoice(1);
  (FSelected as TOctetString).Assign(Value);
end;

function TSignerIdentifier.GetChoice: TSignerIdentifierEnum;
begin
  Result := TSignerIdentifierEnum(InternalGetChoice);
end;

procedure TSignerIdentifier.SetChoice(const Value: TSignerIdentifierEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TSignedAttributes }

constructor TSignedAttributes.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TAttribute;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('signerInfos')^.Template;
    F := F.FindField('signedAttrs')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSignedAttributes.Destroy;
begin
  inherited Destroy;
end;

function TSignedAttributes.Add: TAttribute;
begin
  Result := TAttribute(InternalAdd);
end;

function TSignedAttributes.GetItems(Index: Integer): TAttribute;
begin
  Result := TAttribute(InternalGetItems(Index));
end;

{ TUnsignedAttributes }

constructor TUnsignedAttributes.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TAttribute;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('signerInfos')^.Template;
    F := F.FindField('unsignedAttrs')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TUnsignedAttributes.Destroy;
begin
  inherited Destroy;
end;

function TUnsignedAttributes.Add: TCMSAttribute;
begin
  Result := TCMSAttribute(InternalAdd);
end;

function TUnsignedAttributes.GetItems(Index: Integer): TCMSAttribute;
begin
  Result := TCMSAttribute(InternalGetItems(Index));
end;

{ TSignerInfo }

constructor TSignerInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('signerInfos')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TSignerIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TDigestAlgorithmIdentifier,nil,FData.Items[2]^,nil^);
  FieldFactory(TSignedAttributes,nil,FData.Items[3]^,nil^);
  FieldFactory(TSignatureAlgorithmIdentifier,nil,FData.Items[4]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[5]^,nil^);
  FieldFactory(TUnsignedAttributes,nil,FData.Items[6]^,nil^);
end;

destructor TSignerInfo.Destroy;
begin
  inherited Destroy;
end;

function TSignerInfo.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TSignerInfo.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TSignerInfo.GetSid: TSignerIdentifier;
begin
  Result := FItems[1];
end;

procedure TSignerInfo.SetSid(const Value: TSignerIdentifier);
begin
  TSignerIdentifier(FItems[1]).Assign(Value);
end;

function TSignerInfo.GetDigestAlgorithm: TDigestAlgorithmIdentifier;
begin
  Result := FItems[2];
end;

procedure TSignerInfo.SetDigestAlgorithm(const Value: TDigestAlgorithmIdentifier);
begin
  TDigestAlgorithmIdentifier(FItems[2]).Assign(Value);
end;

function TSignerInfo.GetSignedAttrs: TSignedAttributes;
begin
  Result := FItems[3];
end;

procedure TSignerInfo.SetSignedAttrs(const Value: TSignedAttributes);
begin
  TSignedAttributes(FItems[3]).Assign(Value);
end;

function TSignerInfo.GetSignatureAlgorithm: TSignatureAlgorithmIdentifier;
begin
  Result := FItems[4];
end;

procedure TSignerInfo.SetSignatureAlgorithm(const Value: TSignatureAlgorithmIdentifier);
begin
  TSignatureAlgorithmIdentifier(FItems[4]).Assign(Value);
end;

function TSignerInfo.GetSignature: TOctetString;
begin
  Result := FItems[5];
end;

procedure TSignerInfo.SetSignature(const Value: TOctetString);
begin
  TOctetString(FItems[5]).Assign(Value);
end;

function TSignerInfo.GetUnsignedAttrs: TUnsignedAttributes;
begin
  Result := FItems[6];
end;

procedure TSignerInfo.SetUnsignedAttrs(const Value: TUnsignedAttributes);
begin
  TUnsignedAttributes(FItems[6]).Assign(Value);
end;

{ TSignerInfos }

constructor TSignerInfos.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TSignerInfo;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^;
    F := F.FindField('signerInfos')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSignerInfos.Destroy;
begin
  inherited Destroy;
end;

function TSignerInfos.Add: TSignerInfo;
begin
  Result := TSignerInfo(InternalAdd);
end;

function TSignerInfos.GetItems(Index: Integer): TSignerInfo;
begin
  Result := TSignerInfo(InternalGetItems(Index));
end;

{ TSignedData }

constructor TSignedData.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TDigestAlgorithmIdentifiers,nil,FData.Items[1]^,nil^);
  FieldFactory(TEncapsulatedContentInfo,nil,FData.Items[2]^,nil^);
  FieldFactory(TCertificateSet,nil,FData.Items[3]^,nil^);
  FieldFactory(TCertificateRevocationLists,nil,FData.Items[4]^,nil^);
  FieldFactory(TSignerInfos,nil,FData.Items[5]^,nil^);
end;

destructor TSignedData.Destroy;
begin
  inherited Destroy;
end;

function TSignedData.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TSignedData.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TSignedData.GetDigestAlgorithms: TDigestAlgorithmIdentifiers;
begin
  Result := FItems[1];
end;

procedure TSignedData.SetDigestAlgorithms(const Value: TDigestAlgorithmIdentifiers);
begin
  TDigestAlgorithmIdentifiers(FItems[1]).Assign(Value);
end;

function TSignedData.GetEncapContentInfo: TEncapsulatedContentInfo;
begin
  Result := FItems[2];
end;

procedure TSignedData.SetEncapContentInfo(const Value: TEncapsulatedContentInfo);
begin
  TEncapsulatedContentInfo(FItems[2]).Assign(Value);
end;

function TSignedData.GetCertificates: TCertificateSet;
begin
  Result := FItems[3];
end;

procedure TSignedData.SetCertificates(const Value: TCertificateSet);
begin
  TCertificateSet(FItems[3]).Assign(Value);
end;

function TSignedData.GetCrls: TCertificateRevocationLists;
begin
  Result := FItems[4];
end;

procedure TSignedData.SetCrls(const Value: TCertificateRevocationLists);
begin
  TCertificateRevocationLists(FItems[4]).Assign(Value);
end;

function TSignedData.GetSignerInfos: TSignerInfos;
begin
  Result := FItems[5];
end;

procedure TSignedData.SetSignerInfos(const Value: TSignerInfos);
begin
  TSignerInfos(FItems[5]).Assign(Value);
end;

{ TOriginatorInfo }

constructor TOriginatorInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^;
    F := F.FindField('originatorInfo')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TCertificateSet,nil,FData.Items[0]^,nil^);
  FieldFactory(TCertificateRevocationLists,nil,FData.Items[1]^,nil^);
end;

destructor TOriginatorInfo.Destroy;
begin
  inherited Destroy;
end;

function TOriginatorInfo.GetCerts: TCertificateSet;
begin
  Result := FItems[0];
end;

procedure TOriginatorInfo.SetCerts(const Value: TCertificateSet);
begin
  TCertificateSet(FItems[0]).Assign(Value);
end;

function TOriginatorInfo.GetCrls: TCertificateRevocationLists;
begin
  Result := FItems[1];
end;

procedure TOriginatorInfo.SetCrls(const Value: TCertificateRevocationLists);
begin
  TCertificateRevocationLists(FItems[1]).Assign(Value);
end;

{ TRecipientIdentifier }

constructor TRecipientIdentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^;
    F := F.FindField('rid')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TIssuerAndSerialNumber);
  FChoiceList.Add(nil);
end;

destructor TRecipientIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TRecipientIdentifier.GetAsIssuerAndSerialNumber: TIssuerAndSerialNumber;
begin
  Result := (FSelected as TIssuerAndSerialNumber);
end;

procedure TRecipientIdentifier.SetAsIssuerAndSerialNumber(const Value: TIssuerAndSerialNumber);
begin
  InternalSelectChoice(0);
  (FSelected as TIssuerAndSerialNumber).Assign(Value);
end;

function TRecipientIdentifier.GetChoice: TRecipientIdentifierEnum;
begin
  Result := TRecipientIdentifierEnum(InternalGetChoice);
end;

procedure TRecipientIdentifier.SetChoice(const Value: TRecipientIdentifierEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TKwparams }

constructor TKwparams.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^.FindField('/keyEncryptionAlgorithm/parameters')^.Choices[1]^;
    F := F.FindField('parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(nil);
  FChoiceList.Add(TIntegerWrapper);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
  FChoiceList.Add(nil);
end;

destructor TKwparams.Destroy;
begin
  inherited Destroy;
end;

function TKwparams.GetAsAlg_CMSRC2wrap: TIntegerWrapper;
begin
  Result := (FSelected as TIntegerWrapper);
end;

procedure TKwparams.SetAsAlg_CMSRC2wrap(const Value: TIntegerWrapper);
begin
  InternalSelectChoice(1);
  (FSelected as TIntegerWrapper).Assign(Value);
end;

function TKwparams.GetChoice: TKwparamsEnum;
begin
  Result := TKwparamsEnum(InternalGetChoice);
end;

procedure TKwparams.SetChoice(const Value: TKwparamsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TKeyWrapAlgorithmIdentifier }

constructor TKeyWrapAlgorithmIdentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^.FindField('/keyEncryptionAlgorithm/parameters')^.Choices[1]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TKwparams,nil,FData.Items[1]^,nil^);
end;

destructor TKeyWrapAlgorithmIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TKeyWrapAlgorithmIdentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TKeyWrapAlgorithmIdentifier.SetAlgorithm(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TKeyWrapAlgorithmIdentifier.GetParameters: TKwparams;
begin
  Result := FItems[1];
end;

procedure TKeyWrapAlgorithmIdentifier.SetParameters(const Value: TKwparams);
begin
  TKwparams(FItems[1]).Assign(Value);
end;

function TKeyWrapAlgorithmIdentifier.IsType(Id: TKwparamsEnum): Boolean;
begin
  Result := TKwparams(FItems[1]).Choice = Id;
end;

procedure TKeyWrapAlgorithmIdentifier.SetType(Id: TKwparamsEnum);
begin
  TKwparams(FItems[1]).Choice := Id;
end;

{ TPkcs1MGFAlgorithms }

constructor TPkcs1MGFAlgorithms.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^.FindField('/keyEncryptionAlgorithm/parameters')^.Choices[2]^;
    F := F.FindField('/maskGenFunc//parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TDigestAlgorithmIdentifier);
end;

destructor TPkcs1MGFAlgorithms.Destroy;
begin
  inherited Destroy;
end;

function TPkcs1MGFAlgorithms.GetAsMgf1: TDigestAlgorithmIdentifier;
begin
  Result := (FSelected as TDigestAlgorithmIdentifier);
end;

procedure TPkcs1MGFAlgorithms.SetAsMgf1(const Value: TDigestAlgorithmIdentifier);
begin
  InternalSelectChoice(0);
  (FSelected as TDigestAlgorithmIdentifier).Assign(Value);
end;

function TPkcs1MGFAlgorithms.GetChoice: TPkcs1MGFAlgorithmsEnum;
begin
  Result := TPkcs1MGFAlgorithmsEnum(InternalGetChoice);
end;

procedure TPkcs1MGFAlgorithms.SetChoice(const Value: TPkcs1MGFAlgorithmsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TMgfalgorithmIdentifier }

constructor TMgfalgorithmIdentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^.FindField('/keyEncryptionAlgorithm/parameters')^.Choices[2]^;
    F := F.FindField('/maskGenFunc/')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TPkcs1MGFAlgorithms,nil,FData.Items[1]^,nil^);
end;

destructor TMgfalgorithmIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TMgfalgorithmIdentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TMgfalgorithmIdentifier.SetAlgorithm(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TMgfalgorithmIdentifier.GetParameters: TPkcs1MGFAlgorithms;
begin
  Result := FItems[1];
end;

procedure TMgfalgorithmIdentifier.SetParameters(const Value: TPkcs1MGFAlgorithms);
begin
  TPkcs1MGFAlgorithms(FItems[1]).Assign(Value);
end;

function TMgfalgorithmIdentifier.IsType(Id: TPkcs1MGFAlgorithmsEnum): Boolean;
begin
  Result := TPkcs1MGFAlgorithms(FItems[1]).Choice = Id;
end;

procedure TMgfalgorithmIdentifier.SetType(Id: TPkcs1MGFAlgorithmsEnum);
begin
  TPkcs1MGFAlgorithms(FItems[1]).Choice := Id;
end;

{ TPkcs1pSourceAlgorithms }

constructor TPkcs1pSourceAlgorithms.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^.FindField('/keyEncryptionAlgorithm/parameters')^.Choices[2]^;
    F := F.FindField('/pSourceFunc//parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TOctetString);
end;

destructor TPkcs1pSourceAlgorithms.Destroy;
begin
  inherited Destroy;
end;

function TPkcs1pSourceAlgorithms.GetAsPSpecified: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TPkcs1pSourceAlgorithms.SetAsPSpecified(const Value: TOctetString);
begin
  InternalSelectChoice(0);
  (FSelected as TOctetString).Assign(Value);
end;

function TPkcs1pSourceAlgorithms.GetChoice: TPkcs1pSourceAlgorithmsEnum;
begin
  Result := TPkcs1pSourceAlgorithmsEnum(InternalGetChoice);
end;

procedure TPkcs1pSourceAlgorithms.SetChoice(const Value: TPkcs1pSourceAlgorithmsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TPsfalgorithmIdentifier }

constructor TPsfalgorithmIdentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^.FindField('/keyEncryptionAlgorithm/parameters')^.Choices[2]^;
    F := F.FindField('/pSourceFunc/')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TPkcs1pSourceAlgorithms,nil,FData.Items[1]^,nil^);
end;

destructor TPsfalgorithmIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TPsfalgorithmIdentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TPsfalgorithmIdentifier.SetAlgorithm(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TPsfalgorithmIdentifier.GetParameters: TPkcs1pSourceAlgorithms;
begin
  Result := FItems[1];
end;

procedure TPsfalgorithmIdentifier.SetParameters(const Value: TPkcs1pSourceAlgorithms);
begin
  TPkcs1pSourceAlgorithms(FItems[1]).Assign(Value);
end;

function TPsfalgorithmIdentifier.IsType(Id: TPkcs1pSourceAlgorithmsEnum): Boolean;
begin
  Result := TPkcs1pSourceAlgorithms(FItems[1]).Choice = Id;
end;

procedure TPsfalgorithmIdentifier.SetType(Id: TPkcs1pSourceAlgorithmsEnum);
begin
  TPkcs1pSourceAlgorithms(FItems[1]).Choice := Id;
end;

{ TRsaes_OAEP_Params }

constructor TRsaes_OAEP_Params.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^.FindField('/keyEncryptionAlgorithm/parameters')^.Choices[2]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TDigestAlgorithmIdentifier,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TMgfalgorithmIdentifier,nil,FData.Items[1]^.Items[0]^,nil^);
  FieldFactory(TPsfalgorithmIdentifier,nil,FData.Items[2]^.Items[0]^,nil^);
end;

destructor TRsaes_OAEP_Params.Destroy;
begin
  inherited Destroy;
end;

function TRsaes_OAEP_Params.GetHashFunc: TDigestAlgorithmIdentifier;
begin
  Result := FItems[0];
end;

procedure TRsaes_OAEP_Params.SetHashFunc(const Value: TDigestAlgorithmIdentifier);
begin
  TDigestAlgorithmIdentifier(FItems[0]).Assign(Value);
end;

function TRsaes_OAEP_Params.GetMaskGenFunc: TMgfalgorithmIdentifier;
begin
  Result := FItems[1];
end;

procedure TRsaes_OAEP_Params.SetMaskGenFunc(const Value: TMgfalgorithmIdentifier);
begin
  TMgfalgorithmIdentifier(FItems[1]).Assign(Value);
end;

function TRsaes_OAEP_Params.GetPSourceFunc: TPsfalgorithmIdentifier;
begin
  Result := FItems[2];
end;

procedure TRsaes_OAEP_Params.SetPSourceFunc(const Value: TPsfalgorithmIdentifier);
begin
  TPsfalgorithmIdentifier(FItems[2]).Assign(Value);
end;

{ TKeparams }

constructor TKeparams.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^;
    F := F.FindField('/keyEncryptionAlgorithm/parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(nil);
  FChoiceList.Add(TKeyWrapAlgorithmIdentifier);
  FChoiceList.Add(TRsaes_OAEP_Params);
end;

destructor TKeparams.Destroy;
begin
  inherited Destroy;
end;

function TKeparams.GetAsAlg_ESDH: TKeyWrapAlgorithmIdentifier;
begin
  Result := (FSelected as TKeyWrapAlgorithmIdentifier);
end;

procedure TKeparams.SetAsAlg_ESDH(const Value: TKeyWrapAlgorithmIdentifier);
begin
  InternalSelectChoice(1);
  (FSelected as TKeyWrapAlgorithmIdentifier).Assign(Value);
end;

function TKeparams.GetAsRsaes_OAEP: TRsaes_OAEP_Params;
begin
  Result := (FSelected as TRsaes_OAEP_Params);
end;

procedure TKeparams.SetAsRsaes_OAEP(const Value: TRsaes_OAEP_Params);
begin
  InternalSelectChoice(2);
  (FSelected as TRsaes_OAEP_Params).Assign(Value);
end;

function TKeparams.GetChoice: TKeparamsEnum;
begin
  Result := TKeparamsEnum(InternalGetChoice);
end;

procedure TKeparams.SetChoice(const Value: TKeparamsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TKeyEncryptionAlgorithmIdentifier }

constructor TKeyEncryptionAlgorithmIdentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^;
    F := F.FindField('keyEncryptionAlgorithm')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TKeparams,nil,FData.Items[1]^,nil^);
end;

destructor TKeyEncryptionAlgorithmIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TKeyEncryptionAlgorithmIdentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TKeyEncryptionAlgorithmIdentifier.SetAlgorithm(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TKeyEncryptionAlgorithmIdentifier.GetParameters: TKeparams;
begin
  Result := FItems[1];
end;

procedure TKeyEncryptionAlgorithmIdentifier.SetParameters(const Value: TKeparams);
begin
  TKeparams(FItems[1]).Assign(Value);
end;

function TKeyEncryptionAlgorithmIdentifier.IsType(Id: TKeparamsEnum): Boolean;
begin
  Result := TKeparams(FItems[1]).Choice = Id;
end;

procedure TKeyEncryptionAlgorithmIdentifier.SetType(Id: TKeparamsEnum);
begin
  TKeparams(FItems[1]).Choice := Id;
end;

{ TKeyTransRecipientInfo }

constructor TKeyTransRecipientInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TRecipientIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TKeyEncryptionAlgorithmIdentifier,nil,FData.Items[2]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[3]^,nil^);
end;

destructor TKeyTransRecipientInfo.Destroy;
begin
  inherited Destroy;
end;

function TKeyTransRecipientInfo.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TKeyTransRecipientInfo.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TKeyTransRecipientInfo.GetRid: TRecipientIdentifier;
begin
  Result := FItems[1];
end;

procedure TKeyTransRecipientInfo.SetRid(const Value: TRecipientIdentifier);
begin
  TRecipientIdentifier(FItems[1]).Assign(Value);
end;

function TKeyTransRecipientInfo.GetKeyEncryptionAlgorithm: TKeyEncryptionAlgorithmIdentifier;
begin
  Result := FItems[2];
end;

procedure TKeyTransRecipientInfo.SetKeyEncryptionAlgorithm(const Value: TKeyEncryptionAlgorithmIdentifier);
begin
  TKeyEncryptionAlgorithmIdentifier(FItems[2]).Assign(Value);
end;

function TKeyTransRecipientInfo.GetEncryptedKey: TOctetString;
begin
  Result := FItems[3];
end;

procedure TKeyTransRecipientInfo.SetEncryptedKey(const Value: TOctetString);
begin
  TOctetString(FItems[3]).Assign(Value);
end;

{ TOriginatorPublicKey }

constructor TOriginatorPublicKey.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[1]^.FindField('/originator/')^.Choices[2]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[0]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[1]^,nil^);
end;

destructor TOriginatorPublicKey.Destroy;
begin
  inherited Destroy;
end;

function TOriginatorPublicKey.GetAlgorithm: TAlgorithmIdentifier;
begin
  Result := FItems[0];
end;

procedure TOriginatorPublicKey.SetAlgorithm(const Value: TAlgorithmIdentifier);
begin
  TAlgorithmIdentifier(FItems[0]).Assign(Value);
end;

function TOriginatorPublicKey.GetPublicKey: TBitString;
begin
  Result := FItems[1];
end;

procedure TOriginatorPublicKey.SetPublicKey(const Value: TBitString);
begin
  TBitString(FItems[1]).Assign(Value);
end;

{ TOriginatorIdentifierOrKey }

constructor TOriginatorIdentifierOrKey.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[1]^;
    F := F.FindField('/originator/')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TIssuerAndSerialNumber);
  FChoiceList.Add(nil);
  FChoiceList.Add(TOriginatorPublicKey);
end;

destructor TOriginatorIdentifierOrKey.Destroy;
begin
  inherited Destroy;
end;

function TOriginatorIdentifierOrKey.GetAsIssuerAndSerialNumber: TIssuerAndSerialNumber;
begin
  Result := (FSelected as TIssuerAndSerialNumber);
end;

procedure TOriginatorIdentifierOrKey.SetAsIssuerAndSerialNumber(const Value: TIssuerAndSerialNumber);
begin
  InternalSelectChoice(0);
  (FSelected as TIssuerAndSerialNumber).Assign(Value);
end;

function TOriginatorIdentifierOrKey.GetAsOriginatorKey: TOriginatorPublicKey;
begin
  Result := (FSelected as TOriginatorPublicKey);
end;

procedure TOriginatorIdentifierOrKey.SetAsOriginatorKey(const Value: TOriginatorPublicKey);
begin
  InternalSelectChoice(2);
  (FSelected as TOriginatorPublicKey).Assign(Value);
end;

function TOriginatorIdentifierOrKey.GetChoice: TOriginatorIdentifierOrKeyEnum;
begin
  Result := TOriginatorIdentifierOrKeyEnum(InternalGetChoice);
end;

procedure TOriginatorIdentifierOrKey.SetChoice(const Value: TOriginatorIdentifierOrKeyEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TOtherKeyAttribute }

constructor TOtherKeyAttribute.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[1]^.FindField('recipientEncryptedKeys')^.Template.FindField('rid')^.Choices[1]^;
    F := F.FindField('other')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TAny,nil,FData.Items[1]^,nil^);
end;

destructor TOtherKeyAttribute.Destroy;
begin
  inherited Destroy;
end;

function TOtherKeyAttribute.GetKeyAttrId: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TOtherKeyAttribute.SetKeyAttrId(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TOtherKeyAttribute.GetKeyAttr: TAny;
begin
  Result := FItems[1];
end;

procedure TOtherKeyAttribute.SetKeyAttr(const Value: TAny);
begin
  TAny(FItems[1]).Assign(Value);
end;

{ TRecipientKeyIdentifier }

constructor TRecipientKeyIdentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[1]^.FindField('recipientEncryptedKeys')^.Template.FindField('rid')^.Choices[1]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TOctetString,nil,FData.Items[0]^,nil^);
  FieldFactory(TGeneralizedTime,nil,FData.Items[1]^,nil^);
  FieldFactory(TOtherKeyAttribute,nil,FData.Items[2]^,nil^);
end;

destructor TRecipientKeyIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TRecipientKeyIdentifier.GetSubjectKeyIdentifier: TOctetString;
begin
  Result := FItems[0];
end;

procedure TRecipientKeyIdentifier.SetSubjectKeyIdentifier(const Value: TOctetString);
begin
  TOctetString(FItems[0]).Assign(Value);
end;

function TRecipientKeyIdentifier.GetDate: TGeneralizedTime;
begin
  Result := FItems[1];
end;

procedure TRecipientKeyIdentifier.SetDate(const Value: TGeneralizedTime);
begin
  TGeneralizedTime(FItems[1]).Assign(Value);
end;

function TRecipientKeyIdentifier.GetOther: TOtherKeyAttribute;
begin
  Result := FItems[2];
end;

procedure TRecipientKeyIdentifier.SetOther(const Value: TOtherKeyAttribute);
begin
  TOtherKeyAttribute(FItems[2]).Assign(Value);
end;

{ TKeyAgreeRecipientIdentifier }

constructor TKeyAgreeRecipientIdentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[1]^.FindField('recipientEncryptedKeys')^.Template;
    F := F.FindField('rid')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TIssuerAndSerialNumber);
  FChoiceList.Add(TRecipientKeyIdentifier);
end;

destructor TKeyAgreeRecipientIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TKeyAgreeRecipientIdentifier.GetAsIssuerAndSerialNumber: TIssuerAndSerialNumber;
begin
  Result := (FSelected as TIssuerAndSerialNumber);
end;

procedure TKeyAgreeRecipientIdentifier.SetAsIssuerAndSerialNumber(const Value: TIssuerAndSerialNumber);
begin
  InternalSelectChoice(0);
  (FSelected as TIssuerAndSerialNumber).Assign(Value);
end;

function TKeyAgreeRecipientIdentifier.GetAsRKeyId: TRecipientKeyIdentifier;
begin
  Result := (FSelected as TRecipientKeyIdentifier);
end;

procedure TKeyAgreeRecipientIdentifier.SetAsRKeyId(const Value: TRecipientKeyIdentifier);
begin
  InternalSelectChoice(1);
  (FSelected as TRecipientKeyIdentifier).Assign(Value);
end;

function TKeyAgreeRecipientIdentifier.GetChoice: TKeyAgreeRecipientIdentifierEnum;
begin
  Result := TKeyAgreeRecipientIdentifierEnum(InternalGetChoice);
end;

procedure TKeyAgreeRecipientIdentifier.SetChoice(const Value: TKeyAgreeRecipientIdentifierEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TRecipientEncryptedKey }

constructor TRecipientEncryptedKey.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[1]^.FindField('recipientEncryptedKeys')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TKeyAgreeRecipientIdentifier,nil,FData.Items[0]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[1]^,nil^);
end;

destructor TRecipientEncryptedKey.Destroy;
begin
  inherited Destroy;
end;

function TRecipientEncryptedKey.GetRid: TKeyAgreeRecipientIdentifier;
begin
  Result := FItems[0];
end;

procedure TRecipientEncryptedKey.SetRid(const Value: TKeyAgreeRecipientIdentifier);
begin
  TKeyAgreeRecipientIdentifier(FItems[0]).Assign(Value);
end;

function TRecipientEncryptedKey.GetEncryptedKey: TOctetString;
begin
  Result := FItems[1];
end;

procedure TRecipientEncryptedKey.SetEncryptedKey(const Value: TOctetString);
begin
  TOctetString(FItems[1]).Assign(Value);
end;

{ TRecipientEncryptedKeys }

constructor TRecipientEncryptedKeys.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TRecipientEncryptedKey;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[1]^;
    F := F.FindField('recipientEncryptedKeys')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TRecipientEncryptedKeys.Destroy;
begin
  inherited Destroy;
end;

function TRecipientEncryptedKeys.Add: TRecipientEncryptedKey;
begin
  Result := TRecipientEncryptedKey(InternalAdd);
end;

function TRecipientEncryptedKeys.GetItems(Index: Integer): TRecipientEncryptedKey;
begin
  Result := TRecipientEncryptedKey(InternalGetItems(Index));
end;

{ TKeyAgreeRecipientInfo }

constructor TKeyAgreeRecipientInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[1]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TOriginatorIdentifierOrKey,nil,FData.Items[1]^.Items[0]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[2]^.Items[0]^,nil^);
  FieldFactory(TKeyEncryptionAlgorithmIdentifier,nil,FData.Items[3]^,nil^);
  FieldFactory(TRecipientEncryptedKeys,nil,FData.Items[4]^,nil^);
end;

destructor TKeyAgreeRecipientInfo.Destroy;
begin
  inherited Destroy;
end;

function TKeyAgreeRecipientInfo.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TKeyAgreeRecipientInfo.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TKeyAgreeRecipientInfo.GetOriginator: TOriginatorIdentifierOrKey;
begin
  Result := FItems[1];
end;

procedure TKeyAgreeRecipientInfo.SetOriginator(const Value: TOriginatorIdentifierOrKey);
begin
  TOriginatorIdentifierOrKey(FItems[1]).Assign(Value);
end;

function TKeyAgreeRecipientInfo.GetUkm: TOctetString;
begin
  Result := FItems[2];
end;

procedure TKeyAgreeRecipientInfo.SetUkm(const Value: TOctetString);
begin
  TOctetString(FItems[2]).Assign(Value);
end;

function TKeyAgreeRecipientInfo.GetKeyEncryptionAlgorithm: TKeyEncryptionAlgorithmIdentifier;
begin
  Result := FItems[3];
end;

procedure TKeyAgreeRecipientInfo.SetKeyEncryptionAlgorithm(const Value: TKeyEncryptionAlgorithmIdentifier);
begin
  TKeyEncryptionAlgorithmIdentifier(FItems[3]).Assign(Value);
end;

function TKeyAgreeRecipientInfo.GetRecipientEncryptedKeys: TRecipientEncryptedKeys;
begin
  Result := FItems[4];
end;

procedure TKeyAgreeRecipientInfo.SetRecipientEncryptedKeys(const Value: TRecipientEncryptedKeys);
begin
  TRecipientEncryptedKeys(FItems[4]).Assign(Value);
end;

{ TKekidentifier }

constructor TKekidentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[2]^;
    F := F.FindField('kekid')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TOctetString,nil,FData.Items[0]^,nil^);
  FieldFactory(TGeneralizedTime,nil,FData.Items[1]^,nil^);
  FieldFactory(TOtherKeyAttribute,nil,FData.Items[2]^,nil^);
end;

destructor TKekidentifier.Destroy;
begin
  inherited Destroy;
end;

function TKekidentifier.GetKeyIdentifier: TOctetString;
begin
  Result := FItems[0];
end;

procedure TKekidentifier.SetKeyIdentifier(const Value: TOctetString);
begin
  TOctetString(FItems[0]).Assign(Value);
end;

function TKekidentifier.GetDate: TGeneralizedTime;
begin
  Result := FItems[1];
end;

procedure TKekidentifier.SetDate(const Value: TGeneralizedTime);
begin
  TGeneralizedTime(FItems[1]).Assign(Value);
end;

function TKekidentifier.GetOther: TOtherKeyAttribute;
begin
  Result := FItems[2];
end;

procedure TKekidentifier.SetOther(const Value: TOtherKeyAttribute);
begin
  TOtherKeyAttribute(FItems[2]).Assign(Value);
end;

{ TKekrecipientInfo }

constructor TKekrecipientInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template.Choices[2]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TKekidentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TKeyEncryptionAlgorithmIdentifier,nil,FData.Items[2]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[3]^,nil^);
end;

destructor TKekrecipientInfo.Destroy;
begin
  inherited Destroy;
end;

function TKekrecipientInfo.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TKekrecipientInfo.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TKekrecipientInfo.GetKekid: TKekidentifier;
begin
  Result := FItems[1];
end;

procedure TKekrecipientInfo.SetKekid(const Value: TKekidentifier);
begin
  TKekidentifier(FItems[1]).Assign(Value);
end;

function TKekrecipientInfo.GetKeyEncryptionAlgorithm: TKeyEncryptionAlgorithmIdentifier;
begin
  Result := FItems[2];
end;

procedure TKekrecipientInfo.SetKeyEncryptionAlgorithm(const Value: TKeyEncryptionAlgorithmIdentifier);
begin
  TKeyEncryptionAlgorithmIdentifier(FItems[2]).Assign(Value);
end;

function TKekrecipientInfo.GetEncryptedKey: TOctetString;
begin
  Result := FItems[3];
end;

procedure TKekrecipientInfo.SetEncryptedKey(const Value: TOctetString);
begin
  TOctetString(FItems[3]).Assign(Value);
end;

{ TRecipientInfo }

constructor TRecipientInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('recipientInfos')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TKeyTransRecipientInfo);
  FChoiceList.Add(TKeyAgreeRecipientInfo);
  FChoiceList.Add(TKekrecipientInfo);
end;

destructor TRecipientInfo.Destroy;
begin
  inherited Destroy;
end;

function TRecipientInfo.GetAsKtri: TKeyTransRecipientInfo;
begin
  Result := (FSelected as TKeyTransRecipientInfo);
end;

procedure TRecipientInfo.SetAsKtri(const Value: TKeyTransRecipientInfo);
begin
  InternalSelectChoice(0);
  (FSelected as TKeyTransRecipientInfo).Assign(Value);
end;

function TRecipientInfo.GetAsKari: TKeyAgreeRecipientInfo;
begin
  Result := (FSelected as TKeyAgreeRecipientInfo);
end;

procedure TRecipientInfo.SetAsKari(const Value: TKeyAgreeRecipientInfo);
begin
  InternalSelectChoice(1);
  (FSelected as TKeyAgreeRecipientInfo).Assign(Value);
end;

function TRecipientInfo.GetAsKekri: TKekrecipientInfo;
begin
  Result := (FSelected as TKekrecipientInfo);
end;

procedure TRecipientInfo.SetAsKekri(const Value: TKekrecipientInfo);
begin
  InternalSelectChoice(2);
  (FSelected as TKekrecipientInfo).Assign(Value);
end;

function TRecipientInfo.GetChoice: TRecipientInfoEnum;
begin
  Result := TRecipientInfoEnum(InternalGetChoice);
end;

procedure TRecipientInfo.SetChoice(const Value: TRecipientInfoEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TRecipientInfos }

constructor TRecipientInfos.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TRecipientInfo;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^;
    F := F.FindField('recipientInfos')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TRecipientInfos.Destroy;
begin
  inherited Destroy;
end;

function TRecipientInfos.Add: TRecipientInfo;
begin
  Result := TRecipientInfo(InternalAdd);
end;

function TRecipientInfos.GetItems(Index: Integer): TRecipientInfo;
begin
  Result := TRecipientInfo(InternalGetItems(Index));
end;

{ TRc2CBCParameter }

constructor TRc2CBCParameter.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^.FindField('/encryptedContentInfo/contentEncryptionAlgorithm/parameters')^.Choices[1]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[1]^,nil^);
end;

destructor TRc2CBCParameter.Destroy;
begin
  inherited Destroy;
end;

function TRc2CBCParameter.GetRc2ParameterVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TRc2CBCParameter.SetRc2ParameterVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TRc2CBCParameter.GetIv: TOctetString;
begin
  Result := FItems[1];
end;

procedure TRc2CBCParameter.SetIv(const Value: TOctetString);
begin
  TOctetString(FItems[1]).Assign(Value);
end;

{ TCeparams }

constructor TCeparams.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^;
    F := F.FindField('/encryptedContentInfo/contentEncryptionAlgorithm/parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TOctetString);
  FChoiceList.Add(TRc2CBCParameter);
  FChoiceList.Add(TOctetString);
  FChoiceList.Add(TOctetString);
  FChoiceList.Add(TOctetString);
end;

destructor TCeparams.Destroy;
begin
  inherited Destroy;
end;

function TCeparams.GetAsDes_Ede3_Cbc: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TCeparams.SetAsDes_Ede3_Cbc(const Value: TOctetString);
begin
  InternalSelectChoice(0);
  (FSelected as TOctetString).Assign(Value);
end;

function TCeparams.GetAsRc2_Cbc: TRc2CBCParameter;
begin
  Result := (FSelected as TRc2CBCParameter);
end;

procedure TCeparams.SetAsRc2_Cbc(const Value: TRc2CBCParameter);
begin
  InternalSelectChoice(1);
  (FSelected as TRc2CBCParameter).Assign(Value);
end;

function TCeparams.GetAsAes128_CBC: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TCeparams.SetAsAes128_CBC(const Value: TOctetString);
begin
  InternalSelectChoice(2);
  (FSelected as TOctetString).Assign(Value);
end;

function TCeparams.GetAsAes192_CBC: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TCeparams.SetAsAes192_CBC(const Value: TOctetString);
begin
  InternalSelectChoice(3);
  (FSelected as TOctetString).Assign(Value);
end;

function TCeparams.GetAsAes256_CBC: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TCeparams.SetAsAes256_CBC(const Value: TOctetString);
begin
  InternalSelectChoice(4);
  (FSelected as TOctetString).Assign(Value);
end;

function TCeparams.GetChoice: TCeparamsEnum;
begin
  Result := TCeparamsEnum(InternalGetChoice);
end;

procedure TCeparams.SetChoice(const Value: TCeparamsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TContentEncryptionAlgorithmIdentifier }

constructor TContentEncryptionAlgorithmIdentifier.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^;
    F := F.FindField('/encryptedContentInfo/contentEncryptionAlgorithm')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TCeparams,nil,FData.Items[1]^,nil^);
end;

destructor TContentEncryptionAlgorithmIdentifier.Destroy;
begin
  inherited Destroy;
end;

function TContentEncryptionAlgorithmIdentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TContentEncryptionAlgorithmIdentifier.SetAlgorithm(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TContentEncryptionAlgorithmIdentifier.GetParameters: TCeparams;
begin
  Result := FItems[1];
end;

procedure TContentEncryptionAlgorithmIdentifier.SetParameters(const Value: TCeparams);
begin
  TCeparams(FItems[1]).Assign(Value);
end;

function TContentEncryptionAlgorithmIdentifier.IsType(Id: TCeparamsEnum): Boolean;
begin
  Result := TCeparams(FItems[1]).Choice = Id;
end;

procedure TContentEncryptionAlgorithmIdentifier.SetType(Id: TCeparamsEnum);
begin
  TCeparams(FItems[1]).Choice := Id;
end;

{ TEncryptedContentInfo }

constructor TEncryptedContentInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^;
    F := F.FindField('encryptedContentInfo')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TContentEncryptionAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[2]^,nil^);
end;

destructor TEncryptedContentInfo.Destroy;
begin
  inherited Destroy;
end;

function TEncryptedContentInfo.GetContentType: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TEncryptedContentInfo.SetContentType(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TEncryptedContentInfo.GetContentEncryptionAlgorithm: TContentEncryptionAlgorithmIdentifier;
begin
  Result := FItems[1];
end;

procedure TEncryptedContentInfo.SetContentEncryptionAlgorithm(const Value: TContentEncryptionAlgorithmIdentifier);
begin
  TContentEncryptionAlgorithmIdentifier(FItems[1]).Assign(Value);
end;

function TEncryptedContentInfo.GetEncryptedContent: TOctetString;
begin
  Result := FItems[2];
end;

procedure TEncryptedContentInfo.SetEncryptedContent(const Value: TOctetString);
begin
  TOctetString(FItems[2]).Assign(Value);
end;

{ TUnprotectedAttributes }

constructor TUnprotectedAttributes.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TAttribute;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^;
    F := F.FindField('unprotectedAttrs')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TUnprotectedAttributes.Destroy;
begin
  inherited Destroy;
end;

function TUnprotectedAttributes.Add: TCMSAttribute;
begin
  Result := TCMSAttribute(InternalAdd);
end;

function TUnprotectedAttributes.GetItems(Index: Integer): TCMSAttribute;
begin
  Result := TCMSAttribute(InternalGetItems(Index));
end;

{ TEnvelopedData }

constructor TEnvelopedData.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[2]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TOriginatorInfo,nil,FData.Items[1]^,nil^);
  FieldFactory(TRecipientInfos,nil,FData.Items[2]^,nil^);
  FieldFactory(TEncryptedContentInfo,nil,FData.Items[3]^,nil^);
  FieldFactory(TUnprotectedAttributes,nil,FData.Items[4]^,nil^);
end;

destructor TEnvelopedData.Destroy;
begin
  inherited Destroy;
end;

function TEnvelopedData.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TEnvelopedData.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TEnvelopedData.GetOriginatorInfo: TOriginatorInfo;
begin
  Result := FItems[1];
end;

procedure TEnvelopedData.SetOriginatorInfo(const Value: TOriginatorInfo);
begin
  TOriginatorInfo(FItems[1]).Assign(Value);
end;

function TEnvelopedData.GetRecipientInfos: TRecipientInfos;
begin
  Result := FItems[2];
end;

procedure TEnvelopedData.SetRecipientInfos(const Value: TRecipientInfos);
begin
  TRecipientInfos(FItems[2]).Assign(Value);
end;

function TEnvelopedData.GetEncryptedContentInfo: TEncryptedContentInfo;
begin
  Result := FItems[3];
end;

procedure TEnvelopedData.SetEncryptedContentInfo(const Value: TEncryptedContentInfo);
begin
  TEncryptedContentInfo(FItems[3]).Assign(Value);
end;

function TEnvelopedData.GetUnprotectedAttrs: TUnprotectedAttributes;
begin
  Result := FItems[4];
end;

procedure TEnvelopedData.SetUnprotectedAttrs(const Value: TUnprotectedAttributes);
begin
  TUnprotectedAttributes(FItems[4]).Assign(Value);
end;

{ TSignedAndEnvelopedData }

constructor TSignedAndEnvelopedData.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[3]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TRecipientInfos,nil,FData.Items[1]^,nil^);
  FieldFactory(TDigestAlgorithmIdentifiers,nil,FData.Items[2]^,nil^);
  FieldFactory(TEncryptedContentInfo,nil,FData.Items[3]^,nil^);
  FieldFactory(TCertificateSet,nil,FData.Items[4]^,nil^);
  FieldFactory(TCertificateRevocationLists,nil,FData.Items[5]^,nil^);
  FieldFactory(TSignerInfos,nil,FData.Items[6]^,nil^);
end;

destructor TSignedAndEnvelopedData.Destroy;
begin
  inherited Destroy;
end;

function TSignedAndEnvelopedData.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TSignedAndEnvelopedData.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TSignedAndEnvelopedData.GetRecipientInfos: TRecipientInfos;
begin
  Result := FItems[1];
end;

procedure TSignedAndEnvelopedData.SetRecipientInfos(const Value: TRecipientInfos);
begin
  TRecipientInfos(FItems[1]).Assign(Value);
end;

function TSignedAndEnvelopedData.GetDigestAlgorithms: TDigestAlgorithmIdentifiers;
begin
  Result := FItems[2];
end;

procedure TSignedAndEnvelopedData.SetDigestAlgorithms(const Value: TDigestAlgorithmIdentifiers);
begin
  TDigestAlgorithmIdentifiers(FItems[2]).Assign(Value);
end;

function TSignedAndEnvelopedData.GetEncryptedContentInfo: TEncryptedContentInfo;
begin
  Result := FItems[3];
end;

procedure TSignedAndEnvelopedData.SetEncryptedContentInfo(const Value: TEncryptedContentInfo);
begin
  TEncryptedContentInfo(FItems[3]).Assign(Value);
end;

function TSignedAndEnvelopedData.GetCertificates: TCertificateSet;
begin
  Result := FItems[4];
end;

procedure TSignedAndEnvelopedData.SetCertificates(const Value: TCertificateSet);
begin
  TCertificateSet(FItems[4]).Assign(Value);
end;

function TSignedAndEnvelopedData.GetCRLs: TCertificateRevocationLists;
begin
  Result := FItems[5];
end;

procedure TSignedAndEnvelopedData.SetCRLs(const Value: TCertificateRevocationLists);
begin
  TCertificateRevocationLists(FItems[5]).Assign(Value);
end;

function TSignedAndEnvelopedData.GetSignerInfos: TSignerInfos;
begin
  Result := FItems[6];
end;

procedure TSignedAndEnvelopedData.SetSignerInfos(const Value: TSignerInfos);
begin
  TSignerInfos(FItems[6]).Assign(Value);
end;

{ TDigestedData }

constructor TDigestedData.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[4]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TDigestAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TEncapsulatedContentInfo,nil,FData.Items[2]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[3]^,nil^);
end;

destructor TDigestedData.Destroy;
begin
  inherited Destroy;
end;

function TDigestedData.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TDigestedData.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TDigestedData.GetDigestAlgorithm: TDigestAlgorithmIdentifier;
begin
  Result := FItems[1];
end;

procedure TDigestedData.SetDigestAlgorithm(const Value: TDigestAlgorithmIdentifier);
begin
  TDigestAlgorithmIdentifier(FItems[1]).Assign(Value);
end;

function TDigestedData.GetEncapContentInfo: TEncapsulatedContentInfo;
begin
  Result := FItems[2];
end;

procedure TDigestedData.SetEncapContentInfo(const Value: TEncapsulatedContentInfo);
begin
  TEncapsulatedContentInfo(FItems[2]).Assign(Value);
end;

function TDigestedData.GetDigest: TOctetString;
begin
  Result := FItems[3];
end;

procedure TDigestedData.SetDigest(const Value: TOctetString);
begin
  TOctetString(FItems[3]).Assign(Value);
end;

{ TEncryptedData }

constructor TEncryptedData.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[5]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TEncryptedContentInfo,nil,FData.Items[1]^,nil^);
  FieldFactory(TUnprotectedAttributes,nil,FData.Items[2]^,nil^);
end;

destructor TEncryptedData.Destroy;
begin
  inherited Destroy;
end;

function TEncryptedData.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TEncryptedData.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TEncryptedData.GetEncryptedContentInfo: TEncryptedContentInfo;
begin
  Result := FItems[1];
end;

procedure TEncryptedData.SetEncryptedContentInfo(const Value: TEncryptedContentInfo);
begin
  TEncryptedContentInfo(FItems[1]).Assign(Value);
end;

function TEncryptedData.GetUnprotectedAttrs: TUnprotectedAttributes;
begin
  Result := FItems[2];
end;

procedure TEncryptedData.SetUnprotectedAttrs(const Value: TUnprotectedAttributes);
begin
  TUnprotectedAttributes(FItems[2]).Assign(Value);
end;

{ TMacparams }

constructor TMacparams.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[6]^;
    F := F.FindField('/macAlgorithm/parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(nil);
end;

destructor TMacparams.Destroy;
begin
  inherited Destroy;
end;

function TMacparams.GetChoice: TMacparamsEnum;
begin
  Result := TMacparamsEnum(InternalGetChoice);
end;

procedure TMacparams.SetChoice(const Value: TMacparamsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TMessageAuthenticationCodeAlgorithm }

constructor TMessageAuthenticationCodeAlgorithm.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[6]^;
    F := F.FindField('macAlgorithm')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TMacparams,nil,FData.Items[1]^,nil^);
end;

destructor TMessageAuthenticationCodeAlgorithm.Destroy;
begin
  inherited Destroy;
end;

function TMessageAuthenticationCodeAlgorithm.GetAlgorithm: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TMessageAuthenticationCodeAlgorithm.SetAlgorithm(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TMessageAuthenticationCodeAlgorithm.GetParameters: TMacparams;
begin
  Result := FItems[1];
end;

procedure TMessageAuthenticationCodeAlgorithm.SetParameters(const Value: TMacparams);
begin
  TMacparams(FItems[1]).Assign(Value);
end;

function TMessageAuthenticationCodeAlgorithm.IsType(Id: TMacparamsEnum): Boolean;
begin
  Result := TMacparams(FItems[1]).Choice = Id;
end;

procedure TMessageAuthenticationCodeAlgorithm.SetType(Id: TMacparamsEnum);
begin
  TMacparams(FItems[1]).Choice := Id;
end;

{ TAuthAttributes }

constructor TAuthAttributes.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TAttribute;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[6]^;
    F := F.FindField('authenticatedAttributes')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TAuthAttributes.Destroy;
begin
  inherited Destroy;
end;

function TAuthAttributes.Add: TCMSAttribute;
begin
  Result := TCMSAttribute(InternalAdd);
end;

function TAuthAttributes.GetItems(Index: Integer): TCMSAttribute;
begin
  Result := TCMSAttribute(InternalGetItems(Index));
end;

{ TAuthenticatedData }

constructor TAuthenticatedData.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[6]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TOriginatorInfo,nil,FData.Items[1]^,nil^);
  FieldFactory(TRecipientInfos,nil,FData.Items[2]^,nil^);
  FieldFactory(TMessageAuthenticationCodeAlgorithm,nil,FData.Items[3]^,nil^);
  FieldFactory(TDigestAlgorithmIdentifier,nil,FData.Items[4]^,nil^);
  FieldFactory(TEncapsulatedContentInfo,nil,FData.Items[5]^,nil^);
  FieldFactory(TAuthAttributes,nil,FData.Items[6]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[7]^,nil^);
  FieldFactory(TUnauthAttributes,nil,FData.Items[8]^,nil^);
end;

destructor TAuthenticatedData.Destroy;
begin
  inherited Destroy;
end;

function TAuthenticatedData.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TAuthenticatedData.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TAuthenticatedData.GetOriginatorInfo: TOriginatorInfo;
begin
  Result := FItems[1];
end;

procedure TAuthenticatedData.SetOriginatorInfo(const Value: TOriginatorInfo);
begin
  TOriginatorInfo(FItems[1]).Assign(Value);
end;

function TAuthenticatedData.GetRecipientInfos: TRecipientInfos;
begin
  Result := FItems[2];
end;

procedure TAuthenticatedData.SetRecipientInfos(const Value: TRecipientInfos);
begin
  TRecipientInfos(FItems[2]).Assign(Value);
end;

function TAuthenticatedData.GetMacAlgorithm: TMessageAuthenticationCodeAlgorithm;
begin
  Result := FItems[3];
end;

procedure TAuthenticatedData.SetMacAlgorithm(const Value: TMessageAuthenticationCodeAlgorithm);
begin
  TMessageAuthenticationCodeAlgorithm(FItems[3]).Assign(Value);
end;

function TAuthenticatedData.GetDigestAlgorithm: TDigestAlgorithmIdentifier;
begin
  Result := FItems[4];
end;

procedure TAuthenticatedData.SetDigestAlgorithm(const Value: TDigestAlgorithmIdentifier);
begin
  TDigestAlgorithmIdentifier(FItems[4]).Assign(Value);
end;

function TAuthenticatedData.GetEncapContentInfo: TEncapsulatedContentInfo;
begin
  Result := FItems[5];
end;

procedure TAuthenticatedData.SetEncapContentInfo(const Value: TEncapsulatedContentInfo);
begin
  TEncapsulatedContentInfo(FItems[5]).Assign(Value);
end;

function TAuthenticatedData.GetAuthenticatedAttributes: TAuthAttributes;
begin
  Result := FItems[6];
end;

procedure TAuthenticatedData.SetAuthenticatedAttributes(const Value: TAuthAttributes);
begin
  TAuthAttributes(FItems[6]).Assign(Value);
end;

function TAuthenticatedData.GetMac: TOctetString;
begin
  Result := FItems[7];
end;

procedure TAuthenticatedData.SetMac(const Value: TOctetString);
begin
  TOctetString(FItems[7]).Assign(Value);
end;

function TAuthenticatedData.GetUnauthenticatedAttributes: TUnauthAttributes;
begin
  Result := FItems[8];
end;

procedure TAuthenticatedData.SetUnauthenticatedAttributes(const Value: TUnauthAttributes);
begin
  TUnauthAttributes(FItems[8]).Assign(Value);
end;

{ TContent }

constructor TContent.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TOctetString);
  FChoiceList.Add(TSignedData);
  FChoiceList.Add(TEnvelopedData);
  FChoiceList.Add(TSignedAndEnvelopedData);
  FChoiceList.Add(TDigestedData);
  FChoiceList.Add(TEncryptedData);
  FChoiceList.Add(TAuthenticatedData);
end;

destructor TContent.Destroy;
begin
  inherited Destroy;
end;

function TContent.GetAsData: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TContent.SetAsData(const Value: TOctetString);
begin
  InternalSelectChoice(0);
  (FSelected as TOctetString).Assign(Value);
end;

function TContent.GetAsSignedData: TSignedData;
begin
  Result := (FSelected as TSignedData);
end;

procedure TContent.SetAsSignedData(const Value: TSignedData);
begin
  InternalSelectChoice(1);
  (FSelected as TSignedData).Assign(Value);
end;

function TContent.GetAsEnvelopedData: TEnvelopedData;
begin
  Result := (FSelected as TEnvelopedData);
end;

procedure TContent.SetAsEnvelopedData(const Value: TEnvelopedData);
begin
  InternalSelectChoice(2);
  (FSelected as TEnvelopedData).Assign(Value);
end;

function TContent.GetAsSignedAndEnvelopedData: TSignedAndEnvelopedData;
begin
  Result := (FSelected as TSignedAndEnvelopedData);
end;

procedure TContent.SetAsSignedAndEnvelopedData(const Value: TSignedAndEnvelopedData);
begin
  InternalSelectChoice(3);
  (FSelected as TSignedAndEnvelopedData).Assign(Value);
end;

function TContent.GetAsDigestedData: TDigestedData;
begin
  Result := (FSelected as TDigestedData);
end;

procedure TContent.SetAsDigestedData(const Value: TDigestedData);
begin
  InternalSelectChoice(4);
  (FSelected as TDigestedData).Assign(Value);
end;

function TContent.GetAsEncryptedData: TEncryptedData;
begin
  Result := (FSelected as TEncryptedData);
end;

procedure TContent.SetAsEncryptedData(const Value: TEncryptedData);
begin
  InternalSelectChoice(5);
  (FSelected as TEncryptedData).Assign(Value);
end;

function TContent.GetAsCt_AuthData: TAuthenticatedData;
begin
  Result := (FSelected as TAuthenticatedData);
end;

procedure TContent.SetAsCt_AuthData(const Value: TAuthenticatedData);
begin
  InternalSelectChoice(6);
  (FSelected as TAuthenticatedData).Assign(Value);
end;

function TContent.GetChoice: TContentEnum;
begin
  Result := TContentEnum(InternalGetChoice);
end;

procedure TContent.SetChoice(const Value: TContentEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TContentInfo }

constructor TContentInfo.Create;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  FData.Assign(GlobalObject);
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TContent,nil,FData.Items[1]^.Items[0]^,nil^);
end;

destructor TContentInfo.Destroy;
begin
  inherited Destroy;
end;

function TContentInfo.GetContentType: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TContentInfo.SetContentType(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TContentInfo.GetContent: TContent;
begin
  Result := FItems[1];
end;

procedure TContentInfo.SetContent(const Value: TContent);
begin
  TContent(FItems[1]).Assign(Value);
end;

function TContentInfo.IsType(Id: TContentEnum): Boolean;
begin
  Result := TContent(FItems[1]).Choice = Id;
end;

procedure TContentInfo.SetType(Id: TContentEnum);
begin
  TContent(FItems[1]).Choice := Id;
end;

{ TCmsattributeValue }

constructor TCmsattributeValue.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('certificates')^.Template.Choices[1]^.FindField('/extendedCertificateInfo/attributes')^.Template.FindField('attrValues')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TOctetString);
  FChoiceList.Add(TTime);
  FChoiceList.Add(TSignerInfo);
  if FData.ChoiceVarName <> '' then
    Update;
end;

destructor TCmsattributeValue.Destroy;
begin
  inherited Destroy;
end;

function TCmsattributeValue.GetAsContentType: ObjectIdentifier;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TCmsattributeValue.SetAsContentType(const Value: ObjectIdentifier);
begin
  InternalSelectChoice(0);
  (FSelected as TStringWrapper).Value := Value;
end;

function TCmsattributeValue.GetAsMessageDigest: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TCmsattributeValue.SetAsMessageDigest(const Value: TOctetString);
begin
  InternalSelectChoice(1);
  (FSelected as TOctetString).Assign(Value);
end;

function TCmsattributeValue.GetAsSigningTime: TTime;
begin
  Result := (FSelected as TTime);
end;

procedure TCmsattributeValue.SetAsSigningTime(const Value: TTime);
begin
  InternalSelectChoice(2);
  (FSelected as TTime).Assign(Value);
end;

function TCmsattributeValue.GetAsCountersignature: TSignerInfo;
begin
  Result := (FSelected as TSignerInfo);
end;

procedure TCmsattributeValue.SetAsCountersignature(const Value: TSignerInfo);
begin
  InternalSelectChoice(3);
  (FSelected as TSignerInfo).Assign(Value);
end;

function TCmsattributeValue.GetChoice: TCmsattributeValueEnum;
begin
  Result := TCmsattributeValueEnum(InternalGetChoice);
end;

procedure TCmsattributeValue.SetChoice(const Value: TCmsattributeValueEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TSetOfCmsattributeValue }

constructor TSetOfCmsattributeValue.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  FFieldType := TCmsattributeValue;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('certificates')^.Template.Choices[1]^.FindField('/extendedCertificateInfo/attributes')^.Template;
    F := F.FindField('attrValues')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSetOfCmsattributeValue.Destroy;
begin
  inherited Destroy;
end;

function TSetOfCmsattributeValue.Add: TCmsattributeValue;
begin
  Result := TCmsattributeValue(InternalAdd);
end;

function TSetOfCmsattributeValue.GetItems(Index: Integer): TCmsattributeValue;
begin
  Result := TCmsattributeValue(InternalGetItems(Index));
end;

{ TCmsattribute }

constructor TCmsattribute.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'CMS not initialized: Call StrSecInit.InitCMS to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/content/')^.Choices[1]^.FindField('certificates')^.Template.Choices[1]^.FindField('/extendedCertificateInfo/attributes')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TSetOfCmsattributeValue,nil,FData.Items[1]^,nil^);
end;

destructor TCmsattribute.Destroy;
begin
  inherited Destroy;
end;

function TCmsattribute.GetAttrType: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TCmsattribute.SetAttrType(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TCmsattribute.GetAttrValues: TSetOfCmsattributeValue;
begin
  Result := FItems[1];
end;

procedure TCmsattribute.SetAttrValues(const Value: TSetOfCmsattributeValue);
begin
  TSetOfCmsattributeValue(FItems[1]).Assign(Value);
end;

function TCmsattribute.IsType(Id: TCmsattributeValueEnum): Boolean;
begin
  if TASNCustomOFFieldWrapper(FItems[1]).ItemCount > 0 then
    Result := TCmsattributeValue(FItems[1]).Choice = Id
  else
    Result := False;
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
