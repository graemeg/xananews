{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     StreamSecII Unit                                  }
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
{$B-,Q-,R-}
unit StreamSecII;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Controls,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  SysUtils, Classes, ResourceFile, MpArithTypes, MpPK,
  MpDL, MpIF, MpEC, Asn1, X509Base, MpX509, Cms, SecUtils, ReadStrm,
  Tls, TlsClass, SyncObjs, Pkix_Cert, Pkcs_10, SecComp;

const
  StrSecIIPrivateContentInfoASN1Module =
    'ASN.1 Module' + crlf +
    'BEGIN' + crlf +
    crlf +
    'PrivateContentInfo ::= SEQUENCE {' + crlf +
    '  contentEncryptionKey'#9'PCIContentEncryptionKey,' + crlf +
    '  encryptionAlgorithm'#9'PCIEncryptionAlgorithmIdentifier,' + crlf +
    '  contentID'#9'[1] IMPLICIT OCTET STRING OPTIONAL,' + crlf +
    '  encryptedContent'#9'OCTET STRING,' + crlf +
    '  hMAC'#9'[0] IMPLICIT OCTET STRING OPTIONAL}' + crlf +
    crlf +
    'PCIContentEncryptionKey ::= SEQUENCE {' + crlf +
    '  kekid'#9'KEKIdentifier,' + crlf +
    '  keyEncryptionAlgorithm'#9'PCIEncryptionAlgorithmIdentifier,' + crlf +
    '  encryptedKey'#9'OCTET STRING}' + crlf +
    crlf +
    'KEKIdentifier ::= SEQUENCE {' + crlf +
    '  kdfScheme'#9'ENUMERATED,' + crlf +
    '  kdfParameter'#9'OCTET STRING,' + crlf +
    '  kdfIterations'#9'INTEGER DEFAULT 020101,' + crlf +
    '  keyValidation'#9'KeyValidation OPTIONAL}' + crlf +
    crlf +
    'KeyValidation ::= SEQUENCE {' + crlf +
    '  kdfScheme'#9'ENUMERATED,' + crlf +
    '  kdfParameter'#9'OCTET STRING,' + crlf +
    '  kdfIterations'#9'INTEGER DEFAULT 020101,' + crlf +
    '  hmac'#9'OCTET STRING}' + crlf +
    crlf +
    'PCIEncryptionAlgorithmIdentifier ::= SEQUENCE {' + crlf +
    '  algorithm'#9'OBJECT,' + crlf +
    '  iv'#9'OCTET STRING OPTIONAL}' + crlf +
    crlf +
    'END';

{$NODEFINE StrSecIIPrivateKeyRingASN1Module}
  StrSecIIPrivateKeyRingASN1Module =
    'ASN.1 Module' + crlf +
    'BEGIN' + crlf +
    'IMPORTS SubjectPublicKeyInfo FROM PKIX-Cert;' + crlf +
    crlf +
    'PrivateKeyRing ::= SEQUENCE {' + crlf +
    '  sequenceNumber'#9'INTEGER,' + crlf +
    '  longTermKeys'#9'[0] IMPLICIT SEQUENCE OF PrivateKeyInfo OPTIONAL,' + crlf +
    '  sessionKeys'#9'[1] IMPLICIT SEQUENCE OF SessionKeyInfo OPTIONAL,' + crlf +
    '  signedEncryptedKeys'#9'[2] IMPLICIT SEQUENCE OF SignedEncryptedKey OPTIONAL,' + crlf +
    '  encryptedKeys'#9'[3] IMPLICIT SEQUENCE OF EncryptedKeyInfo OPTIONAL,' + crlf +
    '  keySignKey'#9'[4] IMPLICIT KeySignKey OPTIONAL}' + crlf +
    crlf +
    'SignedEncryptedKey ::= SEQUENCE {' + crlf +
    '  privateKey'#9'EncryptedKeyInfo,' + crlf +
    '  signature'#9'OCTET STRING}' + crlf +
    crlf +
    'EncryptedKeyInfo ::= SEQUENCE {' + crlf +
    '  privateKeyType'#9'OBJECT,' + crlf +
    '  identifier'#9'OCTET STRING OPTIONAL,' + crlf +
    '  privateKeyAlg'#9'OBJECT OPTIONAL,' + crlf +
    '  encryptedKey'#9'OCTET STRING,' + crlf +
    '  signatureDigestAlg'#9'OBJECT OPTIONAL}' + crlf +
    crlf +
    'KeySignKey ::= SEQUENCE {' + crlf +
    '  publicKey'#9'SubjectPublicKeyInfo,' + crlf +
    '  privateKey'#9'EncryptedKeyInfo}' + crlf +
    crlf +
    'PrivateKeyInfo ::= SEQUENCE {' + crlf +
    '  privateKeyType'#9'OBJECT,' + crlf +
    '  identifier'#9'OCTET STRING OPTIONAL,' + crlf +
    '  privateKeyAlg'#9'OBJECT OPTIONAL,' + crlf +
    '  privateKey'#9'[0] EXPLICIT PrivateKey.&Type(&privateKeyType) OPTIONAL}' + crlf +
    crlf +
    'PrivateKey ::= TYPE-IDENTIFIER {' + crlf +
    '  {RSAKey'#9'IDENTIFIED BY rsaEncryption}|' + crlf +
    '  {RSAKey'#9'IDENTIFIED BY id-RSAES-OAEP}|' + crlf +
    '  {DHKey'#9'IDENTIFIED BY dhPublicNumber}|' + crlf +
    '  {DSAKey'#9'IDENTIFIED BY id-dsa}|' + crlf +
    '  {ECKey'#9'IDENTIFIED BY id-ecPublicKey}|' + crlf +
    '  {SymmetricKey'#9'IDENTIFIED BY id-tls}|' + crlf +    
    '  {SymmetricKey'#9'IDENTIFIED BY id-mastersecret}|' + crlf +
    '  {SymmetricKey'#9'IDENTIFIED BY tripleDES}|' + crlf +
    '  {SymmetricKey'#9'IDENTIFIED BY id-aes}|' + crlf +
    '  {SymmetricKey'#9'IDENTIFIED BY arcfour}|' + crlf +
    '  {SymmetricKey'#9'IDENTIFIED BY id-twofish}|' + crlf +
    '  {SymmetricKey'#9'IDENTIFIED BY id-blowfish}}' + crlf +
    crlf +
    'pkcs-1         OBJECT IDENTIFIER ::= ' +
    '{ iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) 1 }' + crlf +
    'rsaEncryption  OBJECT IDENTIFIER ::= { pkcs-1 1 }' + crlf +
    'id-RSAES-OAEP  OBJECT IDENTIFIER ::= { pkcs-1 7 }' + crlf +
    'ansi-X9-42     OBJECT IDENTIFIER ::= { iso(1) member-body(2) us(840) 10046 }' + crlf +
    'algorithm      OBJECT IDENTIFIER ::= { ansi-X9-42 1 }' + crlf +
    'tripleDES      OBJECT IDENTIFIER ::= { algorithm 2 }' + crlf +
    'numberType     OBJECT IDENTIFIER ::= { ansi-X9-42 2 }' + crlf +
    'dhPublicNumber OBJECT IDENTIFIER ::= { numberType 1 }' + crlf +
    'ansi-X9-57     OBJECT IDENTIFIER ::= { iso(1) member-body(2) us(840) 10040 }' + crlf +
    'id-dsa         OBJECT IDENTIFIER ::= { ansi-X9-57 4 1 }' + crlf +
    'id-aes         OBJECT IDENTIFIER ::= { 2 16 840 1 101 3 4 1 }' + crlf +
    'pkcs-3         OBJECT IDENTIFIER ::= ' +
    '{ iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) 3 }' + crlf +
    'arcfour        OBJECT IDENTIFIER ::= { pkcs-3 4 }' + crlf +
    'DES-EDE3-CBC   OBJECT IDENTIFIER ::= { pkcs-3 7 }' + crlf +
    'id-streamsec   OBJECT IDENTIFIER ::= { iso(1) org(3) dod(6) internet(1) private(4) enterprise(1) streamsec(13085) }' + crlf +
    'id-ciphers     OBJECT IDENTIFIER ::= { id-streamsec ciphers(2) }' + crlf +
    'id-twofish     OBJECT IDENTIFIER ::= { id-ciphers 2 }' + crlf +           
    'id-blowfish     OBJECT IDENTIFIER ::= { id-ciphers 3 }' + crlf +
    'id-nr  OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) private(4) enterprise(1) streamsec(13085) ssec(4) ssa(1) nr(2) }' + crlf + crlf +
    'id-ecnr  OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) private(4) enterprise(1) streamsec(13085) ssec(4) ssa(1) ecnr(3) }' + crlf + crlf +
    'certicom-arc   OBJECT IDENTIFIER ::= { iso(1) identified-organization(3) certicom(132) }' + crlf +
    'ellipticCurve  OBJECT IDENTIFIER ::= { certicom-arc curve(0) }' + crlf +
    'primeCurve     OBJECT IDENTIFIER ::= {iso(1) member-body(2) us(840) 10045 3 1 }' + crlf +
    'prime192       OBJECT IDENTIFIER  ::=  { primeCurve 1 }' + crlf +
    'prime224       OBJECT IDENTIFIER  ::=  { ellipticCurve 33 }' + crlf +
    'prime256       OBJECT IDENTIFIER  ::=  { primeCurve 7 }' + crlf +
    'prime384       OBJECT IDENTIFIER  ::=  { ellipticCurve 34 }' + crlf +
    'prime521       OBJECT IDENTIFIER  ::=  { ellipticCurve 35 }' + crlf +
    'ansi-X9-62       OBJECT IDENTIFIER ::= { 1 2 840 10045 }' + crlf +
    'id-publicKeyType OBJECT IDENTIFIER ::= { ansi-X9-62 2 }' + crlf +
    'id-ecPublicKey   OBJECT IDENTIFIER ::= { id-publicKeyType 1 }' + crlf +
    'id-aes         OBJECT IDENTIFIER ::= { 2 16 840 1 101 3 4 1 }' + crlf +
    'id-aes128-ECB  OBJECT IDENTIFIER ::= { id-aes 1 }' + crlf +
    'id-aes128-CBC  OBJECT IDENTIFIER ::= { id-aes 2 }' + crlf +
    'id-aes128-OFB  OBJECT IDENTIFIER ::= { id-aes 3 }' + crlf +
    'id-aes128-CFB  OBJECT IDENTIFIER ::= { id-aes 4 }' + crlf +
    'id-aes128-wrap OBJECT IDENTIFIER ::= { id-aes 5 }' + crlf +
    'id-aes192-ECB  OBJECT IDENTIFIER ::= { id-aes 21 }' + crlf +
    'id-aes192-CBC  OBJECT IDENTIFIER ::= { id-aes 22 }' + crlf +
    'id-aes192-OFB  OBJECT IDENTIFIER ::= { id-aes 23 }' + crlf +
    'id-aes192-CFB  OBJECT IDENTIFIER ::= { id-aes 24 }' + crlf +
    'id-aes192-wrap OBJECT IDENTIFIER ::= { id-aes 25 }' + crlf +
    'id-aes256-ECB  OBJECT IDENTIFIER ::= { id-aes 41 }' + crlf +
    'id-aes256-CBC  OBJECT IDENTIFIER ::= { id-aes 42 }' + crlf +
    'id-aes256-OFB  OBJECT IDENTIFIER ::= { id-aes 43 }' + crlf +
    'id-aes256-CFB  OBJECT IDENTIFIER ::= { id-aes 44 }' + crlf +
    'id-aes256-wrap OBJECT IDENTIFIER ::= { id-aes 45 }' + crlf +
    'id-tls         OBJECT IDENTIFIER ::= { id-streamsec 5 }' + crlf +
    'id-mastersecret OBJECT IDENTIFIER ::= { id-streamsec 6 }' + crlf +
    'id-kivciphers OBJECT IDENTIFIER ::= { id-streamsec kivciphers(1) }' + crlf +
    'id-ssec-aes128-ctr OBJECT IDENTIFIER ::= { id-kivciphers 1 }' + crlf +
    'id-ssec-aes192-ctr OBJECT IDENTIFIER ::= { id-kivciphers 2 }' + crlf +
    'id-ssec-aes256-ctr OBJECT IDENTIFIER ::= { id-kivciphers 3 }' + crlf +
    'id-ssec-aes128-abc OBJECT IDENTIFIER ::= { id-kivciphers 4 }' + crlf +
    'id-ssec-aes192-abc OBJECT IDENTIFIER ::= { id-kivciphers 5 }' + crlf +
    'id-ssec-aes256-abc OBJECT IDENTIFIER ::= { id-kivciphers 6 }' + crlf +
    'id-ssec-aes128-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 7 }' + crlf +
    'id-ssec-aes192-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 8 }' + crlf +
    'id-ssec-aes256-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 9 }' + crlf +
    'id-ssec-aes128-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 10 }' + crlf +
    'id-ssec-aes192-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 11 }' + crlf +
    'id-ssec-aes256-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 12 }' + crlf +
    'id-ssec-aes128-cbc OBJECT IDENTIFIER ::= { id-kivciphers 16 }' + crlf +
    'id-ssec-aes192-cbc OBJECT IDENTIFIER ::= { id-kivciphers 17 }' + crlf +
    'id-ssec-aes256-cbc OBJECT IDENTIFIER ::= { id-kivciphers 18 }' + crlf +
    'id-ssec-aes128-ofb OBJECT IDENTIFIER ::= { id-kivciphers 19 }' + crlf +
    'id-ssec-aes192-ofb OBJECT IDENTIFIER ::= { id-kivciphers 20 }' + crlf +
    'id-ssec-aes256-ofb OBJECT IDENTIFIER ::= { id-kivciphers 21 }' + crlf +
    'id-ssec-aes128-cfb OBJECT IDENTIFIER ::= { id-kivciphers 22 }' + crlf +
    'id-ssec-aes192-cfb OBJECT IDENTIFIER ::= { id-kivciphers 23 }' + crlf +
    'id-ssec-aes256-cfb OBJECT IDENTIFIER ::= { id-kivciphers 24 }' + crlf +
    'id-ssec-twofish128-ctr OBJECT IDENTIFIER ::= { id-kivciphers 31 }' + crlf +
    'id-ssec-twofish192-ctr OBJECT IDENTIFIER ::= { id-kivciphers 32 }' + crlf +
    'id-ssec-twofish256-ctr OBJECT IDENTIFIER ::= { id-kivciphers 33 }' + crlf +
    'id-ssec-twofish128-abc OBJECT IDENTIFIER ::= { id-kivciphers 34 }' + crlf +
    'id-ssec-twofish192-abc OBJECT IDENTIFIER ::= { id-kivciphers 35 }' + crlf +
    'id-ssec-twofish256-abc OBJECT IDENTIFIER ::= { id-kivciphers 36 }' + crlf +
    'id-ssec-twofish128-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 37 }' + crlf +
    'id-ssec-twofish192-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 38 }' + crlf +
    'id-ssec-twofish256-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 39 }' + crlf +
    'id-ssec-twofish128-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 40 }' + crlf +
    'id-ssec-twofish192-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 41 }' + crlf +
    'id-ssec-twofish256-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 42 }' + crlf +
    'id-ssec-twofish128-cbc OBJECT IDENTIFIER ::= { id-kivciphers 46 }' + crlf +
    'id-ssec-twofish192-cbc OBJECT IDENTIFIER ::= { id-kivciphers 47 }' + crlf +
    'id-ssec-twofish256-cbc OBJECT IDENTIFIER ::= { id-kivciphers 48 }' + crlf +
    'id-ssec-twofish128-ofb OBJECT IDENTIFIER ::= { id-kivciphers 49 }' + crlf +
    'id-ssec-twofish192-ofb OBJECT IDENTIFIER ::= { id-kivciphers 50 }' + crlf +
    'id-ssec-twofish256-ofb OBJECT IDENTIFIER ::= { id-kivciphers 51 }' + crlf +
    'id-ssec-twofish128-cfb OBJECT IDENTIFIER ::= { id-kivciphers 52 }' + crlf +
    'id-ssec-twofish192-cfb OBJECT IDENTIFIER ::= { id-kivciphers 53 }' + crlf +
    'id-ssec-twofish256-cfb OBJECT IDENTIFIER ::= { id-kivciphers 54 }' + crlf +
    'id-ssec-des-ctr OBJECT IDENTIFIER ::= { id-kivciphers 61 }' + crlf +
    'id-ssec-3des112-ctr OBJECT IDENTIFIER ::= { id-kivciphers 62 }' + crlf +
    'id-ssec-3des168-ctr OBJECT IDENTIFIER ::= { id-kivciphers 63 }' + crlf +
    'id-ssec-des-abc OBJECT IDENTIFIER ::= { id-kivciphers 64 }' + crlf +
    'id-ssec-3des112-abc OBJECT IDENTIFIER ::= { id-kivciphers 65 }' + crlf +
    'id-ssec-3des168-abc OBJECT IDENTIFIER ::= { id-kivciphers 66 }' + crlf +
    'id-ssec-des-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 67 }' + crlf +
    'id-ssec-3des112-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 68 }' + crlf +
    'id-ssec-3des168-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 69 }' + crlf +
    'id-ssec-des-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 70 }' + crlf +
    'id-ssec-3des112-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 71 }' + crlf +
    'id-ssec-3des168-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 72 }' + crlf +
    'id-ssec-des-cbc OBJECT IDENTIFIER ::= { id-kivciphers 76 }' + crlf +
    'id-ssec-3des112-cbc OBJECT IDENTIFIER ::= { id-kivciphers 77 }' + crlf +
    'id-ssec-3des168-cbc OBJECT IDENTIFIER ::= { id-kivciphers 78 }' + crlf +
    'id-ssec-des-ofb OBJECT IDENTIFIER ::= { id-kivciphers 79 }' + crlf +
    'id-ssec-3des112-ofb OBJECT IDENTIFIER ::= { id-kivciphers 80 }' + crlf +
    'id-ssec-3des168-ofb OBJECT IDENTIFIER ::= { id-kivciphers 81 }' + crlf +
    'id-ssec-des-cfb OBJECT IDENTIFIER ::= { id-kivciphers 82 }' + crlf +
    'id-ssec-3des112-cfb OBJECT IDENTIFIER ::= { id-kivciphers 83 }' + crlf +
    'id-ssec-3des168-cfb OBJECT IDENTIFIER ::= { id-kivciphers 84 }' + crlf +
    'id-ssec-blowfish128-ctr OBJECT IDENTIFIER ::= { id-kivciphers 91 }' + crlf +
    'id-ssec-blowfish192-ctr OBJECT IDENTIFIER ::= { id-kivciphers 92 }' + crlf +
    'id-ssec-blowfish256-ctr OBJECT IDENTIFIER ::= { id-kivciphers 93 }' + crlf +
    'id-ssec-blowfish320-ctr OBJECT IDENTIFIER ::= { id-kivciphers 94 }' + crlf +
    'id-ssec-blowfish384-ctr OBJECT IDENTIFIER ::= { id-kivciphers 95 }' + crlf +
    'id-ssec-blowfish448-ctr OBJECT IDENTIFIER ::= { id-kivciphers 96 }' + crlf +
    'id-ssec-blowfish128-abc OBJECT IDENTIFIER ::= { id-kivciphers 97 }' + crlf +
    'id-ssec-blowfish192-abc OBJECT IDENTIFIER ::= { id-kivciphers 98 }' + crlf +
    'id-ssec-blowfish256-abc OBJECT IDENTIFIER ::= { id-kivciphers 99 }' + crlf +
    'id-ssec-blowfish320-abc OBJECT IDENTIFIER ::= { id-kivciphers 100 }' + crlf +
    'id-ssec-blowfish384-abc OBJECT IDENTIFIER ::= { id-kivciphers 101 }' + crlf +
    'id-ssec-blowfish448-abc OBJECT IDENTIFIER ::= { id-kivciphers 102 }' + crlf +
    'id-ssec-blowfish128-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 103 }' + crlf +
    'id-ssec-blowfish192-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 104 }' + crlf +
    'id-ssec-blowfish256-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 105 }' + crlf +
    'id-ssec-blowfish320-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 106 }' + crlf +
    'id-ssec-blowfish384-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 107 }' + crlf +
    'id-ssec-blowfish448-pcfb OBJECT IDENTIFIER ::= { id-kivciphers 108 }' + crlf +
    'id-ssec-blowfish128-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 109 }' + crlf +
    'id-ssec-blowfish192-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 110 }' + crlf +
    'id-ssec-blowfish256-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 111 }' + crlf +
    'id-ssec-blowfish320-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 112 }' + crlf +
    'id-ssec-blowfish384-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 113 }' + crlf +
    'id-ssec-blowfish448-ppcfb OBJECT IDENTIFIER ::= { id-kivciphers 114 }' + crlf +
    'id-ssec-blowfish128-cbc OBJECT IDENTIFIER ::= { id-kivciphers 115 }' + crlf +
    'id-ssec-blowfish192-cbc OBJECT IDENTIFIER ::= { id-kivciphers 116 }' + crlf +
    'id-ssec-blowfish256-cbc OBJECT IDENTIFIER ::= { id-kivciphers 117 }' + crlf +
    'id-ssec-blowfish320-cbc OBJECT IDENTIFIER ::= { id-kivciphers 118 }' + crlf +
    'id-ssec-blowfish384-cbc OBJECT IDENTIFIER ::= { id-kivciphers 119 }' + crlf +
    'id-ssec-blowfish448-cbc OBJECT IDENTIFIER ::= { id-kivciphers 120 }' + crlf +
    'id-ssec-blowfish128-ofb OBJECT IDENTIFIER ::= { id-kivciphers 121 }' + crlf +
    'id-ssec-blowfish192-ofb OBJECT IDENTIFIER ::= { id-kivciphers 122 }' + crlf +
    'id-ssec-blowfish256-ofb OBJECT IDENTIFIER ::= { id-kivciphers 123 }' + crlf +
    'id-ssec-blowfish320-ofb OBJECT IDENTIFIER ::= { id-kivciphers 124 }' + crlf +
    'id-ssec-blowfish384-ofb OBJECT IDENTIFIER ::= { id-kivciphers 125 }' + crlf +
    'id-ssec-blowfish448-ofb OBJECT IDENTIFIER ::= { id-kivciphers 126 }' + crlf +
    'id-ssec-blowfish128-cfb OBJECT IDENTIFIER ::= { id-kivciphers 127 }' + crlf +
    'id-ssec-blowfish192-cfb OBJECT IDENTIFIER ::= { id-kivciphers 128 }' + crlf +
    'id-ssec-blowfish256-cfb OBJECT IDENTIFIER ::= { id-kivciphers 129 }' + crlf +
    'id-ssec-blowfish320-cfb OBJECT IDENTIFIER ::= { id-kivciphers 130 }' + crlf +
    'id-ssec-blowfish384-cfb OBJECT IDENTIFIER ::= { id-kivciphers 131 }' + crlf +
    'id-ssec-blowfish448-cfb OBJECT IDENTIFIER ::= { id-kivciphers 132 }' + crlf +

    crlf +
    'hMAC-SHA1      OBJECT IDENTIFIER ::= ' +
    '{ iso(1) identified-organization(3) dod(6) internet(1) security(5) mechanisms(5) 8 1 2 }' + crlf +
    crlf +
    'RSAKey ::= SEQUENCE {' + crlf +
    '  publicKeyIdentifier'#9'OCTET STRING,' + crlf +
    '  privateKey'#9'RSAPrivateKey,' + crlf +
    '  signatureDigestAlg'#9'OBJECT OPTIONAL}' + crlf +
    crlf +
    'RSAPrivateKey ::= CHOICE {' + crlf +
    '  privKey0'#9'[0] IMPLICIT RSAPrivKey0,' + crlf +
    '  privKey1'#9'[1] IMPLICIT RSAPrivKey1,' + crlf +
    '  privKey2'#9'[2] IMPLICIT RSAPrivKey2,' + crlf +
    '  extrnKey'#9'[3] IMPLICIT OCTET STRING}' + crlf +
    crlf +
    'RSAPrivKey0 ::= SEQUENCE {' + crlf +
    '  n'#9'INTEGER,' + crlf +
    '  d'#9'INTEGER}' + crlf +
    crlf +
    'RSAPrivKey1 ::= SEQUENCE {' + crlf +
    '  p'#9'INTEGER,' + crlf +
    '  q'#9'INTEGER,' + crlf +
    '  d1'#9'INTEGER,' + crlf +
    '  d2'#9'INTEGER,' + crlf +
    '  c'#9'INTEGER}' + crlf +
    crlf +
    'RSAPrivKey2 ::= SEQUENCE {' + crlf +
    '  p'#9'INTEGER,' + crlf +
    '  q'#9'INTEGER,' + crlf +
    '  d'#9'INTEGER}' + crlf +
    crlf +
    'DSAKey ::= SEQUENCE {' + crlf +
    '  publicKeyIdentifier'#9'OCTET STRING,' + crlf +
    '  privateKey'#9'DSAPrivateKey,' + crlf +
    '  signatureDigestAlg'#9'OBJECT OPTIONAL}' + crlf +
    crlf +
    'DSAPrivateKey ::= SEQUENCE {' + crlf +
    '  privateKey'#9'OCTET STRING,' + crlf +
    '  params'#9'DSS-Params}' + crlf +
    crlf +
    'DSS-Params ::= SEQUENCE {' + crlf +
    '  p'#9'INTEGER,' + crlf +
    '  q'#9'INTEGER,' + crlf +
    '  g'#9'INTEGER}' + crlf +
    crlf +
    'DHKey ::= SEQUENCE {' + crlf +
    '  publicKeyIdentifier'#9'OCTET STRING,' + crlf +
    '  privateKey'#9'DHPrivateKey}' + crlf +
    crlf +
    'DHPrivateKey ::= SEQUENCE {' + crlf +
    '  privateKey'#9'OCTET STRING,' + crlf +
    '  params'#9'DomainParams}' + crlf +
    crlf +
    'DomainParams ::= SEQUENCE {' + crlf +
    '  p'#9'INTEGER,' + crlf +
    '  g'#9'INTEGER,' + crlf +
    '  q'#9'INTEGER,' + crlf +
    '  j'#9'INTEGER OPTIONAL}' + crlf +
    crlf +
    'DSAKey ::= SEQUENCE {' + crlf +
    '  publicKeyIdentifier'#9'OCTET STRING,' + crlf +
    '  privateKey'#9'DSAPrivateKey}' + crlf +
    crlf +
    'ECKey ::= SEQUENCE {' + crlf +
    '  publicKeyIdentifier'#9'OCTET STRING,' + crlf +
    '  privateKey'#9'ECPrivateKey,' + crlf +
    '  signatureDigestAlg'#9'OBJECT OPTIONAL}' + crlf +
    crlf +
    'ECPrivateKey ::= SEQUENCE {' + crlf +
    '  privateKey'#9'OCTET STRING,' + crlf +
    '  params'#9'EcpkParameters}' + crlf +
    crlf +
    'EcpkParameters ::= CHOICE {' + crlf +
    '  ecParameters'#9'ECParameters,' + crlf +
    '  namedCurve'#9'OBJECT}' + crlf +
    crlf +
    'ECParameters  ::= SEQUENCE {' + crlf +
    '  version'#9'INTEGER,' + crlf +
    '  fieldID'#9'FieldID,' + crlf +
    '  curve'#9'Curve,' + crlf +
    '  base'#9'OCTET STRING,' + crlf +
    '  order'#9'INTEGER,' + crlf +
    '  cofactor'#9'INTEGER OPTIONAL}' + crlf +
    crlf +
    'FieldID ::= SEQUENCE {' + crlf +
    '  fieldType'#9'OBJECT IDENTIFIER,' + crlf +
    '  parameters'#9'INTEGER}' + crlf +
    crlf +
    'Curve ::= SEQUENCE {' + crlf +
    '  a'#9'OCTET STRING,' + crlf +
    '  b'#9'OCTET STRING,' + crlf +
    '  seed'#9'BIT STRING OPTIONAL}' + crlf +
    crlf +
    'SessionKeyInfo ::= SEQUENCE {' + crlf +
    '  recipientInfo'#9'RecipientInfo OPTIONAL,' + crlf +
    '  expires'#9'GeneralizedTime,' + crlf +
    '  originator'#9'BOOLEAN,' + crlf +
    '  contentEncryptionKey'#9'PrivateKeyInfo OPTIONAL,' + crlf +
    '  keyEncryptionKey'#9'[0] IMPLICIT PrivateKeyInfo OPTIONAL,' + crlf +
    '  ephemeralKey'#9'[1] IMPLICIT PrivateKeyInfo OPTIONAL}' + crlf +
    crlf +
    'RecipientInfo ::= CHOICE {' + crlf +
    '  ktri'#9'KeyTransportRecipientInfo,' + crlf +
    '  kari'#9'[1] IMPLICIT KeyAgreeRecipientInfo,' + crlf +
    '  kekri'#9'[2] IMPLICIT KEKRecipientInfo,' + crlf +
    '  other'#9'[3] IMPLICIT OCTET STRING}' + crlf +
    crlf +
    'KeyTransportRecipientInfo ::= SEQUENCE {' + crlf +
    '  version'#9'INTEGER,' + crlf +
    '  issuerAndSerialNumber'#9'IssuerAndSerialNumber,' + crlf +
    '  keyEncryptionAlgorithm'#9'AlgorithmIdentifier,' + crlf +
    '  encryptedKey'#9'OCTET STRING}' + crlf +
    crlf +
    'IssuerAndSerialNumber ::= SEQUENCE {' + crlf +
    '  issuer'#9'Name,' + crlf +
    '  serialNumber'#9'INTEGER}' + crlf +
    crlf +
    'AlgorithmIdentifier ::= SEQUENCE {' + crlf +
    '  algorithm'#9'OBJECT,' + crlf +
    '  parameters'#9'ANY OPTIONAL}' + crlf +
    crlf +
    'KeyAgreeRecipientInfo ::= SEQUENCE {' + crlf +
    '  version'#9'INTEGER,' + crlf +
    '  originator'#9'[0] OriginatorIdentifierOrKey,' + crlf +
    '  ukm'#9'[1] UserKeyingMaterial OPTIONAL,' + crlf +
    '  keyEncryptionAlgorithm'#9'AlgorithmIdentifier,' + crlf +
    '  recipientEncryptedKeys'#9'RecipientEncryptedKeys}' + crlf +
    crlf +
    'OriginatorIdentifierOrKey ::= CHOICE {' + crlf +
    '  issuerAndSerialNumber'#9'IssuerAndSerialNumber,' + crlf +
    '  subjectKeyIdentifier'#9'[0] OCTET STRING,' + crlf +
    '  originatorKey'#9'[1] OriginatorPublicKey}' + crlf +
    crlf +
    'OriginatorPublicKey ::= SEQUENCE {' + crlf +
    '  algorithm'#9'AlgorithmIdentifier,' + crlf +
    '  publicKey'#9'BIT STRING}' + crlf +
    crlf +
    'RecipientEncryptedKeys ::= SEQUENCE OF RecipientEncryptedKey' + crlf +
    crlf +
    'RecipientEncryptedKey ::= SEQUENCE {' + crlf +
    '  rid'#9'KeyAgreeRecipientIdentifier,' + crlf +
    '  encryptedKey'#9'EncryptedKey}' + crlf +
    crlf +
    'KeyAgreeRecipientIdentifier ::= CHOICE {' + crlf +
    '  issuerAndSerialNumber'#9'IssuerAndSerialNumber,' + crlf +
    '  rKeyId'#9'[0] IMPLICIT RecipientKeyIdentifier}' + crlf +
    crlf +
    'RecipientKeyIdentifier ::= SEQUENCE {' + crlf +
    '  subjectKeyIdentifier'#9'SubjectKeyIdentifier,' + crlf +
    '  date'#9'GeneralizedTime OPTIONAL,' + crlf +
    '  other'#9'OtherKeyAttribute OPTIONAL}' + crlf +
    crlf +
    'SubjectKeyIdentifier ::= OCTET STRING' + crlf +
    crlf +
    'EncryptedContentInfo ::= SEQUENCE {' + crlf +
    '  contentType'#9'OBJECT,' + crlf +
    '  contentEncryptionAlgorithm'#9'AlgorithmIdentifier,' + crlf +
    '  encryptedContent'#9'[0] IMPLICIT OCTET STRING OPTIONAL}' + crlf +
    crlf +
    'KEKRecipientInfo ::= SEQUENCE {' + crlf +
    '  version'#9'INTEGER,' + crlf +
    '  kekid'#9'KEKIdentifier,' + crlf +
    '  keyEncryptionAlgorithm'#9'AlgorithmIdentifier,' + crlf +
    '  encryptedKey'#9'EncryptedKey}' + crlf +
    crlf +
    'KEKIdentifier ::= SEQUENCE {' + crlf +
    '  keyIdentifier'#9'OCTET STRING,' + crlf +
    '  date'#9'GeneralizedTime OPTIONAL,' + crlf +
    '  other'#9'OtherKeyAttribute OPTIONAL}' + crlf +
    crlf +
    'OtherKeyAttribute ::= SEQUENCE {' + crlf +
    '  keyAttrId'#9'OBJECT,' + crlf +
    '  keyAttr'#9'ANY OPTIONAL}' + crlf +
    crlf +
    'SymmetricKey ::= OCTET STRING' + crlf +
    crlf +
    'END';

type
  TPreference = (prNotAllowed,prAllowed,prPrefer);

  TECKeySize = (ecsAuto,ecs192,ecs224,ecs256,ecs384,ecs521);

  TVerifyServerNameEnum = (vsnIP,vsnDNS,vsnURI);
  TVerifyServerName = set of TVerifyServerNameEnum;
                
{$IFDEF SHA1_AND_MD5}
  TTLSOptions = class(TPersistent)
  private
    FOwner: TComponent;
    FExport40Bit: TPreference;
    FBulkCipherAES128: TPreference;
    FSignatureECDSA: TPreference;
    FKeyAgreementRSA: TPreference;
    FBulkCipherDES: TPreference;
    FSignatureDSS: TPreference;
    FExport56Bit: TPreference;
    FBulkCipherARC4: TPreference;
    FBulkCipherTripleDES: TPreference;
    FKeyAgreementECDH: TPreference;
    FKeyAgreementDH: TPreference;
    FSignatureRSA: TPreference;
    FSignatureAnon: TPreference;
    FKeyAgreementDHE: TPreference;
    FEphemeralDHQSize: Integer;
    FEphemeralDHKeySize: Integer;
    FEphemeralRSAKeySize: Integer;
    FRequireClientCertificate: Boolean;
    FRequestClientCertificate: Boolean;
    FHashAlgorithmSHA1: TPreference;
    FHashAlgorithmMD5: TPreference;
    FBulkCipherAES256: TPreference;
    FCipherSuites: TCipherSuites;
    FCipherSuiteNames: TStrings;
    FEphemeralECDHKeySize: TECKeySize;
    FOnChange: TNotifyEvent;
    FUpdateCount: Integer;
    FChange: Boolean;
    FDualSockets: Boolean;
    FOnlyStandardCipherSuites: Boolean;
    FHashAlgorithmSHA384: TPreference;
    FHashAlgorithmSHA512: TPreference;
    FHashAlgorithmSHA256: TPreference;
    FHashAlgorithmRipeMD160: TPreference;
    FBulkCipherAES192: TPreference;
    FBulkCipherBlowFish128: TPreference;
    FBulkCipherTwoFish128: TPreference;
    FBulkCipherTwoFish256: TPreference;
    FBulkCipherTwoFish192: TPreference;
    FKeyAgreementECDHE: TPreference;
    FBulkCipherTwoFish192CTR: TPreference;
    FBulkCipherAES192CTR: TPreference;
    FBulkCipherTwoFish128CTR: TPreference;
    FBulkCipherAES256CTR: TPreference;
    FBulkCipherTwoFish256CTR: TPreference;
    FBulkCipherAES128CTR: TPreference;
    FBulkCipherBlowFish128CTR: TPreference;
    FVerifyServerName: TVerifyServerName;
    FBulkCipherHelix: TPreference;
    FBulkCipherRC2: TPreference;
    procedure SetBulkCipherAES128(const Value: TPreference);
    procedure SetBulkCipherARC4(const Value: TPreference);
    procedure SetBulkCipherDES(const Value: TPreference);
    procedure SetBulkCipherTripleDES(const Value: TPreference);
    procedure SetExport40Bit(const Value: TPreference);
    procedure SetExport56Bit(const Value: TPreference);
    procedure SetKeyAgreementDH(const Value: TPreference);
    procedure SetKeyAgreementDHE(const Value: TPreference);
    procedure SetKeyAgreementECDH(const Value: TPreference);
    procedure SetKeyAgreementRSA(const Value: TPreference);
    procedure SetSignatureAnon(const Value: TPreference);
    procedure SetSignatureDSS(const Value: TPreference);
    procedure SetSignatureECDSA(const Value: TPreference);
    procedure SetSignatureRSA(const Value: TPreference);
    procedure SetEphemeralDHKeySize(const Value: Integer);
    procedure SetEphemeralDHQSize(const Value: Integer);
    procedure SetEphemeralRSAKeySize(const Value: Integer);
    procedure SetRequestClientCertificate(const Value: Boolean);
    procedure SetRequireClientCertificate(const Value: Boolean);
    procedure SetHashAlgorithmMD5(const Value: TPreference);
    procedure SetHashAlgorithmSHA1(const Value: TPreference);
    procedure SetBulkCipherAES256(const Value: TPreference);
    procedure SetCipherSuites(const Value: TCipherSuites);
    procedure SetCipherSuiteNames(const Value: TStrings);
    procedure SetEphemeralECDHKeySize(const Value: TECKeySize);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetDualSockets(const Value: Boolean);
    procedure SetOnlyStandardCipherSuites(const Value: Boolean);
    procedure SetHashAlgorithmRipeMD160(const Value: TPreference);
    procedure SetHashAlgorithmSHA256(const Value: TPreference);
    procedure SetHashAlgorithmSHA384(const Value: TPreference);
    procedure SetHashAlgorithmSHA512(const Value: TPreference);
    procedure SetBulkCipherAES192(const Value: TPreference);
    procedure SetBulkCipherBlowFish128(const Value: TPreference);
    procedure SetBulkCipherTwoFish128(const Value: TPreference);
    procedure SetBulkCipherTwoFish192(const Value: TPreference);
    procedure SetBulkCipherTwoFish256(const Value: TPreference);
    procedure SetKeyAgreementECDHE(const Value: TPreference);
    procedure SetBulkCipherAES128CTR(const Value: TPreference);
    procedure SetBulkCipherAES192CTR(const Value: TPreference);
    procedure SetBulkCipherAES256CTR(const Value: TPreference);
    procedure SetBulkCipherTwoFish128CTR(const Value: TPreference);
    procedure SetBulkCipherTwoFish192CTR(const Value: TPreference);
    procedure SetBulkCipherTwoFish256CTR(const Value: TPreference);
    procedure SetBulkCipherBlowFish128CTR(const Value: TPreference);
    procedure SetVerifyServerName(const Value: TVerifyServerName);
    procedure SetBulkCipherHelix(const Value: TPreference);
    procedure SetBulkCipherRC2(const Value: TPreference);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ComputeCipherSuites: TCipherSuites;
    procedure DoChange;
    function GetOwner: TPersistent; override;
    procedure RefreshCipherSuiteNames;
    procedure RefreshCipherSuites;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetNamedPropValue(const APropName: string): LongInt;
    procedure SetNamedPropValue(const APropName: string; AValue: LongInt);
    property CipherSuites: TCipherSuites read FCipherSuites write SetCipherSuites;
  published
    property AllowDualSockets: Boolean read FDualSockets write SetDualSockets default False;
    property BulkCipherAES128: TPreference read FBulkCipherAES128 write SetBulkCipherAES128 default prNotAllowed;
    property BulkCipherAES192: TPreference read FBulkCipherAES192 write SetBulkCipherAES192 default prNotAllowed;
    property BulkCipherAES256: TPreference read FBulkCipherAES256 write SetBulkCipherAES256 default prNotAllowed;
    property BulkCipherAES128CTR: TPreference read FBulkCipherAES128CTR write SetBulkCipherAES128CTR default prNotAllowed;
    property BulkCipherAES192CTR: TPreference read FBulkCipherAES192CTR write SetBulkCipherAES192CTR default prNotAllowed;
    property BulkCipherAES256CTR: TPreference read FBulkCipherAES256CTR write SetBulkCipherAES256CTR default prNotAllowed;
    property BulkCipherTwoFish128: TPreference read FBulkCipherTwoFish128 write SetBulkCipherTwoFish128 default prNotAllowed;
    property BulkCipherTwoFish192: TPreference read FBulkCipherTwoFish192 write SetBulkCipherTwoFish192 default prNotAllowed;
    property BulkCipherTwoFish256: TPreference read FBulkCipherTwoFish256 write SetBulkCipherTwoFish256 default prNotAllowed;
    property BulkCipherTwoFish128CTR: TPreference read FBulkCipherTwoFish128CTR write SetBulkCipherTwoFish128CTR default prNotAllowed;
    property BulkCipherTwoFish192CTR: TPreference read FBulkCipherTwoFish192CTR write SetBulkCipherTwoFish192CTR default prNotAllowed;
    property BulkCipherTwoFish256CTR: TPreference read FBulkCipherTwoFish256CTR write SetBulkCipherTwoFish256CTR default prNotAllowed;
    property BulkCipherBlowFish128: TPreference read FBulkCipherBlowFish128 write SetBulkCipherBlowFish128 default prNotAllowed;
    property BulkCipherBlowFish128CTR: TPreference read FBulkCipherBlowFish128CTR write SetBulkCipherBlowFish128CTR default prNotAllowed;
    property BulkCipherARC4: TPreference read FBulkCipherARC4 write SetBulkCipherARC4 default prPrefer;
    property BulkCipherDES: TPreference read FBulkCipherDES write SetBulkCipherDES default prNotAllowed;
    property BulkCipherTripleDES: TPreference read FBulkCipherTripleDES write SetBulkCipherTripleDES default prAllowed;
    property BulkCipherHelix: TPreference read FBulkCipherHelix write SetBulkCipherHelix default prNotAllowed;
    property BulkCipherRC2: TPreference read FBulkCipherRC2 write SetBulkCipherRC2 default prNotAllowed;
    property CipherSuiteNames: TStrings read FCipherSuiteNames write SetCipherSuiteNames stored False;
    property EphemeralDHKeySize: Integer read FEphemeralDHKeySize write SetEphemeralDHKeySize default 1024;
    property EphemeralDHQSize: Integer read FEphemeralDHQSize write SetEphemeralDHQSize default 160;
    property EphemeralECDHKeySize: TECKeySize read FEphemeralECDHKeySize write SetEphemeralECDHKeySize;
    property EphemeralRSAKeySize: Integer read FEphemeralRSAKeySize write SetEphemeralRSAKeySize default 512;
    property Export40Bit: TPreference read FExport40Bit write SetExport40Bit default prNotAllowed;
    property Export56Bit: TPreference read FExport56Bit write SetExport56Bit default prNotAllowed;
    property HashAlgorithmMD5: TPreference read FHashAlgorithmMD5 write SetHashAlgorithmMD5 default prNotAllowed;
    property HashAlgorithmSHA1: TPreference read FHashAlgorithmSHA1 write SetHashAlgorithmSHA1 default prPrefer;
    property HashAlgorithmRipeMD160: TPreference read FHashAlgorithmRipeMD160 write SetHashAlgorithmRipeMD160 default prNotAllowed;
    property HashAlgorithmSHA256: TPreference read FHashAlgorithmSHA256 write SetHashAlgorithmSHA256 default prNotAllowed;
    property HashAlgorithmSHA384: TPreference read FHashAlgorithmSHA384 write SetHashAlgorithmSHA384 default prNotAllowed;
    property HashAlgorithmSHA512: TPreference read FHashAlgorithmSHA512 write SetHashAlgorithmSHA512 default prNotAllowed;
    property KeyAgreementDH: TPreference read FKeyAgreementDH write SetKeyAgreementDH default prAllowed;
    property KeyAgreementDHE: TPreference read FKeyAgreementDHE write SetKeyAgreementDHE default prPrefer;
    property KeyAgreementECDH: TPreference read FKeyAgreementECDH write SetKeyAgreementECDH default prNotAllowed;
    property KeyAgreementECDHE: TPreference read FKeyAgreementECDHE write SetKeyAgreementECDHE default prNotAllowed;
    property KeyAgreementRSA: TPreference read FKeyAgreementRSA write SetKeyAgreementRSA default prAllowed;
    property OnlyStandardCipherSuites: Boolean read FOnlyStandardCipherSuites write SetOnlyStandardCipherSuites default True;
    property RequestClientCertificate: Boolean read FRequestClientCertificate write SetRequestClientCertificate default True;
    property RequireClientCertificate: Boolean read FRequireClientCertificate write SetRequireClientCertificate default True;
    property SignatureAnon: TPreference read FSignatureAnon write SetSignatureAnon default prNotAllowed;
    property SignatureDSS: TPreference read FSignatureDSS write SetSignatureDSS default prAllowed;
    property SignatureECDSA: TPreference read FSignatureECDSA write SetSignatureECDSA default prNotAllowed;
    property SignatureRSA: TPreference read FSignatureRSA write SetSignatureRSA default prPrefer;
    property VerifyServerName: TVerifyServerName read FVerifyServerName write SetVerifyServerName;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
{$ENDIF SHA1_AND_MD5}

  TKeyDerivation = (kdfWeakPassword,kdfP1636_2_sha1,kdfX9_42Concat_sha1,
                    kdfWeakPassword256,kdfP1636_2_sha256,kdfX9_42Concat_sha256,
                    kdfX9_42ASN1DER_sha1,kdfX9_42ASN1DER_sha256,
                    kdfWPv2SHA1,
                    kdfWPv2MD5,
                    kdfWPv2RipeMD160,
                    kdfWPv2SHA256,
                    kdfWPv2SHA384,
                    kdfWPv2SHA512);

  TKeyAgreement = (kaOnePassDH,kaStaticDH,kaHybridOnePass,kaMQV1);

  {$NODEFINE TCipherAlgorithm}
  {$HPPEMIT 'namespace Streamsecii {'}
  {$HPPEMIT '#pragma option push -b-'}
  {$HPPEMIT 'enum TCipherAlgorithm { caAES128CBC, caAES128OFB, caAES128CFB,'}
  {$HPPEMIT '                        caAES192CBC, caAES192OFB, caAES192CFB,'}
  {$HPPEMIT '                        caAES256CBC, caAES256OFB, caAES256CFB,'}
  {$HPPEMIT '                        ca3DESCBC, caARC4,'}
  {$HPPEMIT '                        caAES128KIVctr, caAES192KIVctr, caAES256KIVctr,'}
  {$HPPEMIT '                        caAES128KIVabc, caAES192KIVabc, caAES256KIVabc,'}
  {$HPPEMIT '                        caAES128KIVpcfb, caAES192KIVpcfb, caAES256KIVpcfb,'}
  {$HPPEMIT '                        caAES128KIVppcfb, caAES192KIVppcfb, caAES256KIVppcfb,'}
  {$HPPEMIT '                        caAES128KIVcbc, caAES192KIVcbc, caAES256KIVcbc,'}
  {$HPPEMIT '                        caAES128KIVofb, caAES192KIVofb, caAES256KIVofb,'}
  {$HPPEMIT '                        caAES128KIVcfb, caAES192KIVcfb, caAES256KIVcfb,'}
  {$HPPEMIT '                        caTwoFish128KIVctr, caTwoFish192KIVctr, caTwoFish256KIVctr,'}
  {$HPPEMIT '                        caTwoFish128KIVabc, caTwoFish192KIVabc, caTwoFish256KIVabc,'}
  {$HPPEMIT '                        caTwoFish128KIVpcfb, caTwoFish192KIVpcfb, caTwoFish256KIVpcfb,'}
  {$HPPEMIT '                        caTwoFish128KIVppcfb, caTwoFish192KIVppcfb, caTwoFish256KIVppcfb,'}
  {$HPPEMIT '                        caTwoFish128KIVcbc, caTwoFish192KIVcbc, caTwoFish256KIVcbc,'}
  {$HPPEMIT '                        caTwoFish128KIVofb, caTwoFish192KIVofb, caTwoFish256KIVofb,'}
  {$HPPEMIT '                        caTwoFish128KIVcfb, caTwoFish192KIVcfb, caTwoFish256KIVcfb,'}
  {$HPPEMIT '                        caDESKIVctr, ca3DES112KIVctr, ca3DES168KIVctr,'}
  {$HPPEMIT '                        caDESKIVabc, ca3DES112KIVabc, ca3DES168KIVabc,'}
  {$HPPEMIT '                        caDESKIVpcfb, ca3DES112KIVpcfb, ca3DES168KIVpcfb,'}
  {$HPPEMIT '                        caDESKIVppcfb, ca3DES112KIVppcfb, ca3DES168KIVppcfb,'}
  {$HPPEMIT '                        caDESKIVcbc, ca3DES112KIVcbc, ca3DES168KIVcbc,'}
  {$HPPEMIT '                        caDESKIVofb, ca3DES112KIVofb, ca3DES168KIVofb,'}
  {$HPPEMIT '                        caDESKIVcfb, ca3DES112KIVcfb, ca3DES168KIVcfb,'}
  {$HPPEMIT '                        caBlowFish128KIVctr, caBlowFish192KIVctr, caBlowFish256KIVctr,'}
  {$HPPEMIT '                        caBlowFish320KIVctr, caBlowFish384KIVctr, caBlowFish448KIVctr,'}
  {$HPPEMIT '                        caBlowFish128KIVabc, caBlowFish192KIVabc, caBlowFish256KIVabc,'}
  {$HPPEMIT '                        caBlowFish320KIVabc, caBlowFish384KIVabc, caBlowFish448KIVabc,'}
  {$HPPEMIT '                        caBlowFish128KIVpcfb, caBlowFish192KIVpcfb, caBlowFish256KIVpcfb,'}
  {$HPPEMIT '                        caBlowFish320KIVpcfb, caBlowFish384KIVpcfb, caBlowFish448KIVpcfb,'}
  {$HPPEMIT '                        caBlowFish128KIVppcfb, caBlowFish192KIVppcfb, caBlowFish256KIVppcfb,'}
  {$HPPEMIT '                        caBlowFish320KIVppcfb, caBlowFish384KIVppcfb, caBlowFish448KIVppcfb,'}
  {$HPPEMIT '                        caBlowFish128KIVcbc, caBlowFish192KIVcbc, caBlowFish256KIVcbc,'}
  {$HPPEMIT '                        caBlowFish320KIVcbc, caBlowFish384KIVcbc, caBlowFish448KIVcbc,'}
  {$HPPEMIT '                        caBlowFish128KIVofb, caBlowFish192KIVofb, caBlowFish256KIVofb,'}
  {$HPPEMIT '                        caBlowFish320KIVofb, caBlowFish384KIVofb, caBlowFish448KIVofb,'}
  {$HPPEMIT '	                     caBlowFish128KIVcfb, caBlowFish192KIVcfb, caBlowFish256KIVcfb,'}
  (*$HPPEMIT '                        caBlowFish320KIVcfb, caBlowFish384KIVcfb, caBlowFish448KIVcfb };'*)
  {$HPPEMIT '#pragma option pop'}
  (*$HPPEMIT '};'*)
  {$HPPEMIT ''}
  TCipherAlgorithm = (caAES128CBC,caAES128OFB,caAES128CFB,
                      caAES192CBC,caAES192OFB,caAES192CFB,
                      caAES256CBC,caAES256OFB,caAES256CFB,
                      ca3DESCBC,caARC4,
                      caAES128KIVctr, caAES192KIVctr, caAES256KIVctr,
                      caAES128KIVabc, caAES192KIVabc, caAES256KIVabc,
                      caAES128KIVpcfb, caAES192KIVpcfb, caAES256KIVpcfb,
                      caAES128KIVppcfb, caAES192KIVppcfb, caAES256KIVppcfb,
                      caAES128KIVcbc, caAES192KIVcbc, caAES256KIVcbc,
                      caAES128KIVofb, caAES192KIVofb, caAES256KIVofb,
                      caAES128KIVcfb, caAES192KIVcfb, caAES256KIVcfb,
                      caTwoFish128KIVctr, caTwoFish192KIVctr, caTwoFish256KIVctr,
                      caTwoFish128KIVabc, caTwoFish192KIVabc, caTwoFish256KIVabc,
                      caTwoFish128KIVpcfb, caTwoFish192KIVpcfb, caTwoFish256KIVpcfb,
                      caTwoFish128KIVppcfb, caTwoFish192KIVppcfb, caTwoFish256KIVppcfb,
                      caTwoFish128KIVcbc, caTwoFish192KIVcbc, caTwoFish256KIVcbc,
                      caTwoFish128KIVofb, caTwoFish192KIVofb, caTwoFish256KIVofb,
                      caTwoFish128KIVcfb, caTwoFish192KIVcfb, caTwoFish256KIVcfb,
                      caDESKIVctr, ca3DES112KIVctr, ca3DES168KIVctr,
                      caDESKIVabc, ca3DES112KIVabc, ca3DES168KIVabc,
                      caDESKIVpcfb, ca3DES112KIVpcfb, ca3DES168KIVpcfb,
                      caDESKIVppcfb, ca3DES112KIVppcfb, ca3DES168KIVppcfb,
                      caDESKIVcbc, ca3DES112KIVcbc, ca3DES168KIVcbc,
                      caDESKIVofb, ca3DES112KIVofb, ca3DES168KIVofb,
                      caDESKIVcfb, ca3DES112KIVcfb, ca3DES168KIVcfb,
                      caBlowFish128KIVctr, caBlowFish192KIVctr, caBlowFish256KIVctr,
                      caBlowFish320KIVctr, caBlowFish384KIVctr, caBlowFish448KIVctr,
                      caBlowFish128KIVabc, caBlowFish192KIVabc, caBlowFish256KIVabc,
                      caBlowFish320KIVabc, caBlowFish384KIVabc, caBlowFish448KIVabc,
                      caBlowFish128KIVpcfb, caBlowFish192KIVpcfb, caBlowFish256KIVpcfb,
                      caBlowFish320KIVpcfb, caBlowFish384KIVpcfb, caBlowFish448KIVpcfb,
                      caBlowFish128KIVppcfb, caBlowFish192KIVppcfb, caBlowFish256KIVppcfb,
                      caBlowFish320KIVppcfb, caBlowFish384KIVppcfb, caBlowFish448KIVppcfb,
                      caBlowFish128KIVcbc, caBlowFish192KIVcbc, caBlowFish256KIVcbc,
                      caBlowFish320KIVcbc, caBlowFish384KIVcbc, caBlowFish448KIVcbc,
                      caBlowFish128KIVofb, caBlowFish192KIVofb, caBlowFish256KIVofb,
                      caBlowFish320KIVofb, caBlowFish384KIVofb, caBlowFish448KIVofb,
                      caBlowFish128KIVcfb, caBlowFish192KIVcfb, caBlowFish256KIVcfb,
                      caBlowFish320KIVcfb, caBlowFish384KIVcfb, caBlowFish448KIVcfb
                      );

  TEncodeType = (eEncrypt,eDigest,eDigestThenEncrypt,eEncryptThenDigest);
  TInternalBuffer = array [0..65535] of Byte;

{$IFDEF SHA1_AND_MD5}
  TTLSAlertEvent = procedure (Sender: TObject;
                              Client: TCustomTLS_ContentLayer;
                              var Fatal: Boolean;
                              AlertCode: Integer) of object;
  TTLSSelectCompressionEvent = procedure (Sender: TObject;
                                          Client: TCustomTLS_ContentLayer;
                                          const CompMethods: array of TCompressionMethod;
                                          var CompMethod: TCompressionMethod) of object;
  TTLSRenegotiateEvent = procedure (Sender: TObject;
                                    Client: TCustomTLS_ContentLayer;
                                    const SessionID: TSessionID;
                                    var Allow: Boolean) of object;

  TTLSCompressEvent = procedure (Sender: TObject;
                                 Client: TCustomTLS_ContentLayer;
                                 Src: PTLSPlainText; PTLen: Integer;
                                 Method: Byte;
                                 var Dst: PTLSCompressed; var CLen: Integer;
                                 var Error: Integer) of object;

  TTLSDecompressEvent = procedure (Sender: TObject;
                                   Client: TCustomTLS_ContentLayer;
                                   Src: PTLSCompressed; CLen: Integer;
                                   Method: Byte;
                                   var Dst: PTLSPlainText; var PTLen: Integer;
                                   var Error: Integer) of object;

  TTLSChangeCipherSpec = procedure (Sender: TObject;
                                    Client: TCustomTLS_ContentLayer) of object;
{$ENDIF SHA1_AND_MD5}

  {$IFDEF MSWINDOWS}
  {$NODEFINE TStreamSecIITime}
  {$NODEFINE TTime}
  {$HPPEMIT 'namespace Streamsecii {'}
  {$HPPEMIT '  typedef Controls::TTime TStreamSecIITime;'}
  {$HPPEMIT '  typedef TStreamSecIITime TTime;'}
  (*$HPPEMIT '};'*)
  {$HPPEMIT ''}
  TStreamSecIITime = type Controls.TTime;
  TTime = TStreamSecIITime;
  {$ELSE}
  TTime = TDateTime;
  {$ENDIF}

  ICipher = interface(IKey)
    function GetAlgorithmName: PChar;
    function GetBlockSize: Integer;
    function GetCipherAlgorithm: TCipherAlg;
    function GetCipherAlg: ObjectIdentifier;
    function GetMode: TCipherMode;
    function Decrypt(var Buf; Count: Integer): Boolean;
    function DecryptPtr(Buf: Pointer; Count: Integer): Boolean;
    function DecryptStr(const Buf: string): string;
    function DecryptToPtr(Src, Dst: Pointer; Count: Integer): Boolean;
    function Encrypt(var Buf; Count: Integer): Boolean;
    function EncryptPtr(Buf: Pointer; Count: Integer): Boolean;
    function EncryptStr(const Buf: string): string;
    function EncryptToPtr(Src, Dst: Pointer; Count: Integer): Boolean;
    function SetVectorBuf(const IV; Count: Integer): Boolean;
    property Algorithm: TCipherAlg read GetCipherAlgorithm;
    property AlgorithmName: PChar read GetAlgorithmName;
    property BlockSize: Integer read GetBlockSize;
    property CipherAlg: ObjectIdentifier read GetCipherAlg;
    property Mode: TCipherMode read GetMode;
  end;

  TCipherKeyHandler = class(TKeyHandler)
  protected
    procedure InitStruct(F: TASN1Struct); override;
  end;

  TMPCipher = class(TMPSecretKey,ICipher)
  private
    FCipher: TCipher;
    FCipherAlg: ObjectIdentifier;
    FKey: ISecretKey;
  protected
    function DecodeASNKey(K: TASN1Struct): Boolean; override;
    procedure DisposeKey; override;
    function EncodeASNKey(K: TASN1Struct): Boolean; override;
    function GetAlgorithmName: PChar;
    function GetBlockSize: Integer;
    function GetCipherAlgorithm: TCipherAlg;
    function GetCipherAlg: ObjectIdentifier;
    function GetMode: TCipherMode;
    function GetPKI: string; override;
  public
    constructor Create(AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler); override;
    constructor CreateKey(AKey: ISecretKey;
                          AKeyStruct: TASN1Struct;
                          AKeyHandler: IKeyHandler);
    function Decrypt(var Buf; Count: Integer): Boolean; dynamic;
    function DecryptPtr(Buf: Pointer; Count: Integer): Boolean; dynamic;
    function DecryptStr(const Buf: string): string; dynamic;
    function DecryptToPtr(Src, Dst: Pointer; Count: Integer): Boolean; dynamic;
    function Encrypt(var Buf; Count: Integer): Boolean; dynamic;
    function EncryptPtr(Buf: Pointer; Count: Integer): Boolean; dynamic;
    function EncryptStr(const Buf: string): string; dynamic;
    function EncryptToPtr(Src, Dst: Pointer; Count: Integer): Boolean; dynamic;
    function SetVectorBuf(const IV; Count: Integer): Boolean;
    property Algorithm: TCipherAlg read GetCipherAlgorithm;
    property AlgorithmName: PChar read GetAlgorithmName;
    property BlockSize: Integer read GetBlockSize;
    property CipherAlg: ObjectIdentifier read GetCipherAlg;
    property Mode: TCipherMode read GetMode;
  end;

  TMPKeyData = class(TMPSecretKey,ISecretKey)
  private
    FKey: ISecretKey;
  protected
    function DecodeASNKey(K: TASN1Struct): Boolean; override;
    procedure DisposeKey; override;
    function EncodeASNKey(K: TASN1Struct): Boolean; override;
    function GetKey: Pointer;
    function GetKeyBytes: PByteArray;
    function GetKeyLen: Integer;
    function GetPKI: string; override;
    function GetVectorSize: Integer;
    procedure SetAlgorithm(const Value: ObjectIdentifier); override;
    procedure SetVectorSize(const Value: Integer);
  public
    constructor Create(AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler); override;
    procedure AddTruncKeyAt(AKey: ISecretKey; APos: Integer; IncVal: Byte = 0);
    procedure SetKey(AKey: Pointer; AKeyLen: Integer; AVectorSize: Integer);
    procedure SetKeyAt(AKey: ISecretKey; APos: Integer);
    procedure SetKeyStr(const AKeyStr: string; ConvertToBMPStr: Boolean);
    procedure SetKeyStrAt(const AKeyStr: string; APos: Integer);
    procedure SetLength(AKeyLen: Integer);
    procedure SetTruncKeyAt(AKey: ISecretKey; APos: Integer);
    procedure XORKeyAt(AKey: ISecretKey; APos: Integer);
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Key: Pointer read GetKey;
    property KeyLen: Integer read GetKeyLen;
    property VectorSize: Integer read GetVectorSize write SetVectorSize;
  end;

{$IFDEF SHA1_AND_MD5}
  TMPMasterSecret = class(TMPKeyData)
  private
    FSessionID: TSessionID;
    procedure SetSessionID(const Value: TSessionID);
  protected
    function GetPKI: string; override;
  public
    constructor Create(AKeyStruct: TASN1Struct; AKeyHandler: IKeyHandler); override;
    destructor Destroy; override;
    property SessionID: TSessionID read FSessionID write SetSessionID;
  end;                             
{$ENDIF SHA1_AND_MD5}

  TKeyRingEnum = (krLongTerm,krSession,krSignedEncrypted,krEncrypted);
  TKeyRings = set of TKeyRingEnum;

  TPasswordEvent = procedure (Sender: TObject; Password: ISecretKey) of object;

  TSsPrivateKeyRingComponent = class;

  TPrivateKeyRing = class
  private
    FOwner: TSsPrivateKeyRingComponent;
    FPrivateKeyRing: TASN1Struct;
    FPlainTextKeyHandler: IKeyHandler;
    FEncryptedKeyHandler: IKeyHandler;
    FSignedEncryptedKeyHandler: ISignedEncryptedKeyHandler;
    FCID: OctetString;
    FAdminLoggedIn: Boolean;
    FManagedKeys: IInterfaceList;
    FManagedIndex: TStringList;
    FManagedIndexLock: TCriticalSection;
    FHoursOffsetFromGMT: Double;
    FSessionKeyLifeSpan: TTime;
    FAdminPubl: TSubjectPublicKeyInfo;    
    FAllowPlainTextKeys: Boolean;
    FExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass;
    FCacheKeyInterfaces: Boolean;
    procedure SetHoursOffsetFromGMT(const Value: Double);
    procedure SetSessionKeyLifeSpan(const Value: TTime);
    procedure SetAllowPlainTextKeys(const Value: Boolean);
    function GetLongTermKeyCount: Integer;
    function GetLongTermKeys(index: Integer): TASN1Struct;
    function GetSequenceNumber: Integer;
    function GetSessionKeyCount: Integer;
    function GetSessionKeys(index: Integer): TASN1Struct;
    procedure SetExternalIFPrivateKeyClass(
      const Value: TExternalIFPrivateKeyClass);
    procedure SetLongTermKeys(index: Integer; const Value: TASN1Struct);
    procedure SetSequenceNumber(const Value: Integer);
    procedure SetSessionKeys(index: Integer; const Value: TASN1Struct);
    procedure SetCacheKeyInterfaces(const Value: Boolean);
  protected
    procedure AddToIndex(Cert: TASN1Struct); overload;
    procedure AddToIndex(PublicKeyIdentifier: Pointer; PKILen: Integer); overload;
    function AllocatePrivateKey(KeyRing: TKeyRingEnum;
                                var KeyHandler: IKeyHandler;
                                var Index: Integer): PASN1Struct;
    function CheckAdminLogin: Boolean;
    function CheckLogin: Boolean;
    procedure FreeManagedKeys;
    function InternalFindPrivateKey(Cert: TASN1Struct;
                                    var KeyRing: TKeyRings;
                                    var Key: PASN1Struct): Boolean; overload;
    function InternalFindPrivateKey(PublicKeyIdentifier: Pointer; PKILen: Integer;
                                    var Key: PASN1Struct;
                                    var Index: Integer;
                                    var KeyRing: TKeyRings): Boolean; overload;
    function InternalFindPrivateKey(PublicKeyIdentifier: Pointer; PKILen: Integer;
                                    const Algorithm: ObjectIdentifier;
                                    var Key: PASN1Struct;
                                    var Index: Integer;
                                    KeyRing: TKeyRingEnum): Boolean; overload;
    function InternalIndexFind(Cert: TASN1Struct; TLS: Boolean = False): IMPPrivateKey; overload;
    function InternalIndexFind(PublicKeyIdentifier: Pointer;
                               PKILen: Integer;
                               TLS: Boolean = False): IKey; overload;
  public
    constructor Create(AOwner: TSsPrivateKeyRingComponent);
    destructor Destroy; override;
    function AddKey(Key: TMPSecretKey;
                    KeyRing: TKeyRingEnum;
                    var Index: Integer): Boolean;
    function AddPrivateDLKey(const PrivKey: TDLPrivateKey;
                             PublicKeyIdentifier: Pointer; PKILen: Integer;
                             IsDSAKey,
                             IsLongTermKey: Boolean): Integer;
    function AddPrivateECKey(const PrivKey: TECPrivateKey;
                             PublicKeyIdentifier: Pointer; PKILen: Integer;
                             IsLongTermKey: Boolean): Integer;
    function AddPrivateRSAKey(const PrivKey: TIFPrivateKey;
                              PublicKeyIdentifier: Pointer; PKILen: Integer;
                              IsLongTermKey: Boolean;
                              RSAES_OAEP: Boolean = False): Integer;
    function AdminLogin(Password: ISecretKey): Boolean;
    procedure AdminLogout;
    function CreateAdminKeyPairRSA(Password: ISecretKey;
                                   KeySize: Cardinal{$IFDEF SHA1} = 2048{$ENDIF};
                                   SignatureDigestAlg: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF}): Boolean;
    function CreateAdminKeyPairDSA(Password: ISecretKey;
                                   PSize: Cardinal{$IFDEF SHA1} = 2048{$ENDIF};
                                   QSize: Cardinal{$IFDEF SHA1} = 160{$ENDIF};
                                   SignatureDigestAlg: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF}): Boolean;
    function CreateAdminKeyPairECDSA(Password: ISecretKey;
                                     Curve: ObjectIdentifier;
                                     SignatureDigestAlg: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF}): Boolean;
    function CreateKeyPairIF(const Algorithm: ObjectIdentifier;
                             KeySize: Cardinal;
                             PublKey: IIFPublicKeyInfo;
                             KeyRing: TKeyRingEnum;
                             SignatureDigestAlg: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                             Exportable: Boolean = False): IMPIFPrivateKey;
    function CreateKeyPairDL(const Algorithm: ObjectIdentifier;
                             PSize: Cardinal;
                             QSize: Cardinal;
                             PublKey: IDLPublicKeyInfo;
                             KeyRing: TKeyRingEnum;
                             SignatureDigestAlg: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF}): IMPDLPrivateKey;
    function CreateKeyPairEC(const Algorithm: ObjectIdentifier;
                             Curve: ObjectIdentifier;
                             PublKey: IECPublicKeyInfo;
                             KeyRing: TKeyRingEnum;
                             SignatureDigestAlg: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF}): IMPECPrivateKey;
    procedure DeleteSessionKey(index: Integer);
    function FindCreatePrivateKey(Cert: TASN1Struct;
                                  var KeyRing: TKeyRings): IMPPrivateKey; overload;
    function FindCreatePrivateKey(PublicKeyIdentifier: Pointer; PKILen: Integer;
                                  var Index: Integer;
                                  var KeyRing: TKeyRings): IKey; overload;
    function FindCreatePrivateKeyObj(Cert: TASN1Struct;
                                     var KeyRing: TKeyRings;
                                     TLS: Boolean = False): TMPPrivateKey; overload;
    function FindCreatePrivateKeyObj(PublicKeyIdentifier: Pointer; PKILen: Integer;
                                     var Index: Integer;
                                     var KeyRing: TKeyRings;
                                     TLS: Boolean = False): TMPSecretKey; overload;
{$IFDEF SHA1_AND_MD5}
    function FindTLSMasterSecret(const SessionID: TSessionID;
                                 var Index: Integer): ISecretKey;
{$ENDIF SHA1_AND_MD5}
    function FreePrivateKey(APrivKey: IKey): Boolean;
    function LoadPrivateKeyRingFromStream(AStream: TStream;
                                          Password: ISecretKey): Boolean;
    procedure SavePrivateKeyRingToStream(Stream: TStream;
                                         Password: ISecretKey;
                                         KDF: TKeyDerivation;
                                         KDFIterations: Integer;
                                         KeyValidation: Boolean;
                                         KEKAlgorithm: string;
                                         CipherClass: TCipherClass);
    property AllowPlainTextKeys: Boolean read FAllowPlainTextKeys
                                         write SetAllowPlainTextKeys;
    property CacheKeyInterfaces: Boolean read FCacheKeyInterfaces write SetCacheKeyInterfaces;
    property ExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass read FExternalIFPrivateKeyClass
                                                                   write SetExternalIFPrivateKeyClass;
    property HoursOffsetFromGMT: Double read FHoursOffsetFromGMT
                                        write SetHoursOffsetFromGMT;
    property SessionKeyLifeSpan: TTime read FSessionKeyLifeSpan
                                       write SetSessionKeyLifeSpan;
    property LongTermKeyCount: Integer read GetLongTermKeyCount;
    property LongTermKeys[index: Integer]: TASN1Struct read GetLongTermKeys
                                                       write SetLongTermKeys;
    property SequenceNumber: Integer read GetSequenceNumber
                                     write SetSequenceNumber;
    property SessionKeyCount: Integer read GetSessionKeyCount;
    property SessionKeys[index: Integer]: TASN1Struct read GetSessionKeys
                                                      write SetSessionKeys;
  end;

  TSsPrivateKeyRingComponent = class(TSsKeyRingComponent)
  private                        
    FPrivateKeyRing: TPrivateKeyRing;
    FMyPrivateKeyRing: TPrivateKeyRing;
    FOnPassword: TPasswordEvent;
    FOnAdminPassword: TPasswordEvent;
    FDefaultHashAlgorithm: THashAlgorithm;
    FExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass;
    FPKRComp: TSsPrivateKeyRingComponent;
    FPrivateKeyRingFileName: TFileName;
    FPrivateKeyRingFile: TResourceFile;
    FExceptions: Boolean;       
    FPassword: ISecretKey; // Design time only
    procedure SetOnAdminPassword(const Value: TPasswordEvent);
    procedure SetOnPassword(const Value: TPasswordEvent);
    function GetHoursOffsetFromGMT: Double;
    procedure SetHoursOffsetFromGMT(const Value: Double);
    function GetSessionKeyCount: Integer;
    function GetSessionKeys(index: Integer): TASN1Struct;
    procedure SetSessionKeys(index: Integer; const Value: TASN1Struct);
    function GetLongTermKeyCount: Integer;
    function GetLongTermKeys(index: Integer): TASN1Struct;
    procedure SetLongTermKeys(index: Integer; const Value: TASN1Struct);
    function GetAllowPlainTextKeys: Boolean;
    function GetCacheKeyInterfaces: Boolean;
    procedure SetAllowPlainTextKeys(const Value: Boolean);
    procedure SetCacheKeyInterfaces(const Value: Boolean);
    procedure SetDefaultHashAlgorithm(const Value: THashAlgorithm);
    procedure SetExternalIFPrivateKeyClass(
      const Value: TExternalIFPrivateKeyClass);
    function GetSessionKeyLifeSpan: TTime;
    procedure SetSessionKeyLifeSpan(const Value: TTime);
    procedure SetPrivateKeyRing(const Value: TSsPrivateKeyRingComponent);
    procedure SetPrivateKeyRingFileName(const Value: TFileName);
    procedure SetPrivateKeyRingFile(const Value: TResourceFile);
    procedure SetExceptions(const Value: Boolean);
  protected
    FCreationThreadID: LongWord;
    FHijackCount: Integer;
    function CheckThreadID: Boolean;
    procedure DoAdminPassword(var Password: ISecretKey);
    procedure DoLoadPrivateKeyRingFile(Sender: TObject);
    function InterpretDLPrivateKey(F: PASN1Struct; var Key: TDLPrivateKey): Boolean;
    function InterpretECPrivateKey(F: PASN1Struct; var Key: TECPrivateKey): Boolean;
    function InterpretIFPrivateKey(F: PASN1Struct; var Key: TIFPrivateKey): Boolean;
    procedure KeyLock(AAlways: Boolean = False);
    procedure KeyUnlock(AAlways: Boolean = False);
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure TranslatePrivateDLKey(F: PASN1Struct;
                                    const PrivKey: TDLPrivateKey;
                                    PublicKeyIdentifier: Pointer; PKILen: Integer;
                                    IsDSAKey: Boolean);
    procedure TranslatePrivateECKey(F: PASN1Struct;
                                    const PrivKey: TECPrivateKey;
                                    PublicKeyIdentifier: Pointer; PKILen: Integer);
    procedure TranslatePrivateRSAKey(F: PASN1Struct;
                                     const PrivKey: TIFPrivateKey;
                                     PublicKeyIdentifier: Pointer; PKILen: Integer;
                                     RSAES_OAEP: Boolean = False);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddKey(Key: TMPSecretKey;
                    KeyRing: TKeyRingEnum;
                    var Index: Integer): Boolean;
    function AddPrivateDLKey(const PrivKey: TDLPrivateKey;
                             PublicKeyIdentifier: Pointer; PKILen: Integer;
                             IsDSAKey,
                             IsLongTermKey: Boolean): Integer;
    function AddPrivateECKey(const PrivKey: TECPrivateKey;
                             PublicKeyIdentifier: Pointer; PKILen: Integer;
                             IsLongTermKey: Boolean): Integer;
    function AddPrivateRSAKey(const PrivKey: TIFPrivateKey;
                              PublicKeyIdentifier: Pointer; PKILen: Integer;
                              IsLongTermKey: Boolean;
                              RSAES_OAEP: Boolean = False): Integer;
    function AddPrivateAESKey(const PrivKey; PrivKeyLen: Integer;
                              IsLongTermKey: Boolean;
                              KeyIdentifier: string = ''): Integer; override;
    function AddPrivate3DESKey(const PrivKey; PrivKeyLen: Integer;
                               IsLongTermKey: Boolean;
                               KeyIdentifier: string = ''): Integer; override;
    function AddPrivateARC4Key(const PrivKey; PrivKeyLen: Integer;
                               IsLongTermKey: Boolean;
                               KeyIdentifier: string = ''): Integer; override;
    function AddPrivateTwoFishKey(const PrivKey; PrivKeyLen: Integer;
                                  IsLongTermKey: Boolean;
                                  KeyIdentifier: string = ''): Integer; override;
    function AddPrivateBlowFishKey(const PrivKey; PrivKeyLen: Integer;
                                   IsLongTermKey: Boolean;
                                   KeyIdentifier: string = ''): Integer; override;               
{$IFDEF SHA1_AND_MD5}
    function AddTLSMasterSecret(const SessionID: TSessionID;
                                const MasterSecret: TMasterSecret): Integer;
{$ENDIF SHA1_AND_MD5}
    function AdminLogin(const Password: string): Boolean; overload;
    function AdminLogin(Password: ISecretKey = nil): Boolean; overload;
    procedure AdminLogout;
    procedure ClearExpiredSessionKeys; virtual;
    procedure ComputePublicKey(Index: Integer;
                               LongTermKey: Boolean;
                               var PK: TX509PublicKey);
    function CreateAdminKeyPair(Password: ISecretKey;
                                const Algorithm: string;
                                ABitSize: Integer;
                                const CurveOID: string = ''): Boolean;
    function CreateKeyPair(DstCert: TASN1Struct;
                           const Algorithm: string;
                           ABitSize: Integer;
                           const CurveOID: string = '';
                           Exportable: Boolean = False): Boolean; overload;
    function CreateKeyPair(DstCert: IPublicKeyInfo;
                           const Algorithm: string;
                           ABitSize: Integer;
                           const CurveOID: string = '';
                           Exportable: Boolean = False): Boolean; overload;
    procedure DeleteSessionKey(index: Integer);
    procedure DoPassword(var Password: ISecretKey);
    function FindCreateCipher(const KeyIdentifier: OctetString;
                              Alg: TCipherAlg; Mode: TCipherMode;
                              var KeyedIV: Boolean;
                              var Param: OctetString;
                              MACKey: ISecretKey = nil): TCipher; override;
    function FindCreatePrivateKey(Cert: TASN1Struct): TMPPrivateKey; overload;
    function FindCreatePrivateKey(PublicKeyIdentifier: Pointer; PKILen: Integer;
                                  out IsLongTermKey: Boolean;
                                  var Index: Integer): TMPSecretKey; overload;
    function FindPrivateKey(Cert: TASN1Struct; var Key: PASN1Struct): Boolean; overload;
    function FindPrivateKey(Cert: TASN1Struct; var Key: TIFPrivateKey): Boolean; overload;
    function FindPrivateKey(Cert: TASN1Struct; var Key: TDLPrivateKey): Boolean; overload;
    function FindPrivateKey(Cert: TASN1Struct; var Key: TECPrivateKey): Boolean; overload;
    function FindPrivateKey(PublicKeyIdentifier: Pointer; PKILen: Integer;
                            var Key: PASN1Struct;
                            out IsLongTermKey: Boolean;
                            var Index: Integer): Boolean; overload;
    function FindPrivateKey(PublicKeyIdentifier: Pointer; PKILen: Integer; var Key: PASN1Struct; var Index: Integer; IsLongTermKey: Boolean): Boolean; overload;
    function FindPrivateKey(PublicKeyIdentifier: Pointer; PKILen: Integer; var Key: PASN1Struct): Boolean; overload;
    function FindPrivateKey(PublicKeyIdentifier: Pointer; PKILen: Integer; var Key: TIFPrivateKey): Boolean; overload;
    function FindPrivateKey(PublicKeyIdentifier: Pointer; PKILen: Integer; var Key: TDLPrivateKey): Boolean; overload;
    function FindPrivateKey(PublicKeyIdentifier: Pointer; PKILen: Integer; var Key: TECPrivateKey): Boolean; overload;
{$IFDEF SHA1_AND_MD5}
    function FindTLSMasterSecret(const SessionID: TSessionID;
                                 var MasterSecret: TMasterSecret;
                                 var Index: Integer): Boolean; overload;
    function FindTLSMasterSecret(const SessionID: TSessionID;
                                 var Index: Integer): ISecretKey; overload;               
{$ENDIF SHA1_AND_MD5}
    function FreePrivateKey(APrivKey: IKey): Boolean;
    function LoadPrivateContentFromStream(Src: TStream;
                                          Dst: TSecureMemoryStream;
                                          const Password: string): Boolean; overload;
    function LoadPrivateContentFromStream(Src: TStream;
                                          Dst: TSecureMemoryStream;
                                          const Password: string;
                                          var ContentID: OctetString): Boolean; overload;
    function LoadPrivateContentFromStream(Src: TStream;
                                          Dst: TSecureMemoryStream;
                                          Password: ISecretKey): Boolean; overload;
    function LoadPrivateContentFromStream(Src: TStream;
                                          Dst: TSecureMemoryStream;
                                          Password: ISecretKey;
                                          var ContentID: OctetString): Boolean; overload;
    function LoadPrivateKeyRingFromStream(Stream: TStream;
                                          const Password: string): Boolean; overload;
    function LoadPrivateKeyRingFromStream(Stream: TStream;
                                          Password: ISecretKey): Boolean; overload;  
    function LoadPrivateKeyRingFromFile(AFileName: TFileName;
                                        Password: ISecretKey): Boolean;              
    function LoadPrivateKeyFromFile(AFileName: TFileName;
                                    Password: ISecretKey = nil): Boolean;
    procedure SavePrivateContentToStream(Dst: TStream;
                                         Src: TSecureMemoryStream;
                                         const Password: string;
                                         KDF: TKeyDerivation;
                                         KDFIterations: Integer;
                                         KeyValidation: Boolean;
                                         KEKAlgorithm: string;
                                         CipherClass: TCipherClass); overload;
    procedure SavePrivateContentToStream(Dst: TStream;
                                         Src: TSecureMemoryStream;
                                         Password: ISecretKey;
                                         KDF: TKeyDerivation;
                                         KDFIterations: Integer;
                                         KeyValidation: Boolean;
                                         KEKAlgorithm: string;
                                         CipherClass: TCipherClass); overload;
    procedure SavePrivateContentToStream(Dst: TStream;
                                         Src: TSecureMemoryStream;
                                         const Password: string;      
                                         var ContentID: OctetString;
                                         KDF: TKeyDerivation;
                                         KDFIterations: Integer;
                                         KeyValidation: Boolean;
                                         KEKAlgorithm: string;
                                         CipherClass: TCipherClass); overload;
    procedure SavePrivateContentToStream(Dst: TStream;
                                         Src: TSecureMemoryStream;
                                         Password: ISecretKey;
                                         var ContentID: OctetString;
                                         KDF: TKeyDerivation;
                                         KDFIterations: Integer;
                                         KeyValidation: Boolean;
                                         KEKAlgorithm: string;
                                         CipherClass: TCipherClass); overload;
    procedure SavePrivateKeyRingToStream(Stream: TStream;
                                         const Password: string;
                                         KDF: TKeyDerivation;
                                         KDFIterations: Integer;
                                         KeyValidation: Boolean;
                                         KEKAlgorithm: string;
                                         CipherClass: TCipherClass); overload;
    procedure SavePrivateKeyRingToStream(Stream: TStream;
                                         Password: ISecretKey;
                                         KDF: TKeyDerivation;
                                         KDFIterations: Integer;
                                         KeyValidation: Boolean;
                                         KEKAlgorithm: string;
                                         CipherClass: TCipherClass); overload;
    function SavePrivateKeyRingToFile(const FileName: string;
                                      const Password: string;
                                      KDF: TKeyDerivation;
                                      KDFIterations: Integer;
                                      KeyValidation: Boolean;
                                      KEKAlgorithm: string;
                                      CipherClass: TCipherClass): Boolean; overload;
    function SavePrivateKeyRingToFile(const FileName: string;
                                      Password: ISecretKey;
                                      KDF: TKeyDerivation;
                                      KDFIterations: Integer;
                                      KeyValidation: Boolean;
                                      KEKAlgorithm: string;
                                      CipherClass: TCipherClass): Boolean; overload;
    function SavePrivateKeyToFile(Cert: TASN1Struct;
                                  const FileName: string;
                                  Password: ISecretKey = nil): Boolean;
    function Sign(Signed: ISigned; PublicKey: IPublicKeyInfo;
                  HA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF}): Boolean;
    function SignBuf(const Buf; BufLen: Integer;
                     PKI: Pointer; PKILen: Integer;
                     HA: THashAlgorithm;
                     var Sign; var SignLen: Integer): Boolean;
    function SignCertificate(DstCert, CACert: TASN1Struct;
                             HA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF}): Boolean; overload;
    function SignCertificate(DstCert: TCertificate;
                             CACert: TASN1Struct;
                             HA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF}): Boolean; overload;
    function SignSigned(Signed: TSigned; CACert: IPublicKeyInfo;
                        HA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF}): Boolean;
    function ValidateMyCert(const Cert: TASN1Struct): Boolean;
    property Exceptions: Boolean read FExceptions write SetExceptions;
    property ExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass read FExternalIFPrivateKeyClass write SetExternalIFPrivateKeyClass;
    property LongTermKeyCount: Integer read GetLongTermKeyCount;
    property LongTermKeys[index: Integer]: TASN1Struct read GetLongTermKeys write SetLongTermKeys;
    property SessionKeyCount: Integer read GetSessionKeyCount;
    property SessionKeys[index: Integer]: TASN1Struct read GetSessionKeys write SetSessionKeys;
  published
    property AllowPlainTextKeys: Boolean read GetAllowPlainTextKeys write SetAllowPlainTextKeys;
    property CacheKeyInterfaces: Boolean read GetCacheKeyInterfaces write SetCacheKeyInterfaces;
    property DefaultHashAlgorithm: THashAlgorithm read FDefaultHashAlgorithm write SetDefaultHashAlgorithm;
    property HoursOffsetFromGMT: Double read GetHoursOffsetFromGMT write SetHoursOffsetFromGMT{$IFDEF MSWINDOWS} stored False{$ENDIF};
    property PrivateKeyRing: TSsPrivateKeyRingComponent read FPKRComp write SetPrivateKeyRing;
    property PrivateKeyRingFile: TResourceFile read FPrivateKeyRingFile write SetPrivateKeyRingFile;
    property PrivateKeyRingFileName: TFileName read FPrivateKeyRingFileName write SetPrivateKeyRingFileName;
    property SessionKeyLifeSpan: TTime read GetSessionKeyLifeSpan write SetSessionKeyLifeSpan;
    property OnAdminPassword: TPasswordEvent read FOnAdminPassword write SetOnAdminPassword;
    property OnPassword: TPasswordEvent read FOnPassword write SetOnPassword;
  end;

  TNewCertWizardEnum = (ncwAdvanced,ncwCA,ncwServer,ncwClient,ncwCodeSigning,
                        ncwEMail,ncwPeerToPeer);
  TNewCertCreateEnum = (nccRoot,nccCert,nccCertReq,nccCertFromReq,nccTemplate);

  INewCertDlg = interface;
  ITLSServer = interface
  ['{288086E0-B277-40DA-9619-015F213AD838}']
    procedure ExportToPFX(AFileName: string;
                          Password: ISecretKey = nil;
                          ExportCert: TASN1Struct = nil;
                          FriendlyName: WideString = '';
                          ProviderName: WideString = '');
    procedure KeyAndSign(ADialog: INewCertDlg);
    function SaveMyCertsToSCLFile(AFileName: TFileName): Boolean;
    function SaveMyCertsToSCLStream(AStream: TStream): Boolean;
    procedure SavePrivateKeyRing(AFileName: TFileName);
    function SaveRootCertsToSCLFile(AFileName: TFileName): Boolean;
    function SaveRootCertsToSCLStream(AStream: TStream): Boolean;
    function SaveTrustedCertsToSCLFile(AFileName: TFileName): Boolean;
    function SaveTrustedCertsToSCLStream(AStream: TStream): Boolean;
  end;

  IUserList = interface
  ['{68FDFC03-0A76-4BB7-BF30-911D0EDCD9AB}']
    function VerifyUser(ASubjectName: TRdnSequence;
                        ASubjectAltName: TGeneralNames;
                        const ARegToken: WideString): Boolean;
  end;

  INewCertDlg = interface
  ['{386C933B-63AC-4AF9-8647-E54C27C6DDAF}']
    function GetTemplateWizard: Boolean;
    function GetIssuedCerts: TX509TrustedCertificates;
    function GetMyCerts: TX509TrustedCertificates;
    function GetPrivateKeyRing: TSsPrivateKeyRingComponent;
    function GetRootCerts: TX509TrustedCertificates;
    function GetExportData: TStream;
    function GetImportData: TStream;
    function GetCACert: TASN1Struct;
    function GetCert: TCertificate;
    function GetP10: TCertificationRequest;
    function GetWizard: TNewCertWizardEnum;
    function GetCreateOpt: TNewCertCreateEnum;
    function GetSignatureDigestAlg: THashAlgorithm;
    function GetPrivateKeyAlg: ObjectIdentifier;
    function GetKeySize: Integer;
    function GetExportableRSA: Boolean;
    function GetSubjectPublicKeyInfo: TSubjectPublicKeyInfo;
    function GetUserList: IUserList;

    procedure SetTemplateWizard(const Value: Boolean);
    procedure SetIssuedCerts(const Value: TX509TrustedCertificates);
    procedure SetMyCerts(const Value: TX509TrustedCertificates);
    procedure SetPrivateKeyRing(const Value: TSsPrivateKeyRingComponent);
    procedure SetRootCerts(const Value: TX509TrustedCertificates);
    procedure SetExportData(const Value: TStream);
    procedure SetImportData(const Value: TStream);
    procedure SetCACert(const Value: TASN1Struct);
    procedure SetUserList(const Value: IUserList);

    function Execute(ATLSServer: ITLSServer = nil): Boolean;
    procedure SelectWizard(Index: TNewCertWizardEnum;
                           GenerateRequest: Boolean = False);
    property TemplateWizard: Boolean read GetTemplateWizard write SetTemplateWizard;
    property MyCerts: TX509TrustedCertificates read GetMyCerts write SetMyCerts;
    property RootCerts: TX509TrustedCertificates read GetRootCerts write SetRootCerts;
    property IssuedCerts: TX509TrustedCertificates read GetIssuedCerts write SetIssuedCerts;
    property PrivateKeyRing: TSsPrivateKeyRingComponent read GetPrivateKeyRing write SetPrivateKeyRing;
    property ExportData: TStream read GetExportData write SetExportData;
    property ImportData: TStream read GetImportData write SetImportData;
    property CACert: TASN1Struct read GetCACert write SetCACert;
    property Cert: TCertificate read GetCert;
    property P10: TCertificationRequest read GetP10;
    property Wizard: TNewCertWizardEnum read GetWizard;
    property CreateOpt: TNewCertCreateEnum read GetCreateOpt;
    property SignatureDigestAlg: THashAlgorithm read GetSignatureDigestAlg;
    property PrivateKeyAlg: ObjectIdentifier read GetPrivateKeyAlg;
    property KeySize: Integer read GetKeySize;
    property ExportableRSA: Boolean read GetExportableRSA;
    property SubjectPublicKeyInfo: TSubjectPublicKeyInfo read GetSubjectPublicKeyInfo;
    property UserList: IUserList read GetUserList write SetUserList;
  end;

  INewCertReqDlg = interface(INewCertDlg)
  ['{0FDC4A65-3995-40C3-AE40-D8C9D4812EA5}']
    function GetChallengePassword: WideString;
    function GetRegToken: WideString;

    procedure SetChallengePassword(const Value: WideString);
    procedure SetRegToken(const Value: WideString);

    property RegToken: WideString read GetRegToken write SetRegToken;
    property ChallengePassword: WideString read GetChallengePassword write SetChallengePassword;
  end;

  TCRMKind = (crmCRMF,crmPKCS10,crmCert);

  TNewCertDlgObject = class(TInterfacedObject,INewCertDlg)
  private
    FTemplateWizard: Boolean;
    FIssuedCerts: TX509TrustedCertificates;
    FMyCerts: TX509TrustedCertificates;
    FPrivateKeyRing: TSsPrivateKeyRingComponent;
    FRootCerts: TX509TrustedCertificates;
    FExportData: TStream;
    FImportData: TStream;
    FCACert: TASN1Struct;
    FCert: TCertificate;
    FP10: TCertificationRequest;
    FWizard: TNewCertWizardEnum;
    FCreateOpt: TNewCertCreateEnum;
    FSignatureDigestAlg: THashAlgorithm;
    FPrivateKeyAlg: ObjectIdentifier;
    FKeySize: Integer;
    FExportableRSA: Boolean;
    FNewCert: TCertificate;
    FChallengePassword: WideString;
    FRegToken: WideString;
    FUserList: IUserList;
    FVerified: Boolean;
    procedure SetExportableRSA(const Value: Boolean);
    procedure SetKeySize(const Value: Integer);
    procedure SetPrivateKeyAlg(const Value: ObjectIdentifier);
    procedure SetSignatureDigestAlg(const Value: THashAlgorithm);
  protected
    function GetChallengePassword: WideString;
    function GetRegToken: WideString;
    procedure SetChallengePassword(const Value: WideString);
    procedure SetRegToken(const Value: WideString);
  protected
    function LoadCRM(const FileName: string): Boolean;
    function LoadCRMStream(Stream: TStream; AKind: TCRMKind): Boolean;
    property RegToken: WideString read GetRegToken write SetRegToken;
    property ChallengePassword: WideString read GetChallengePassword write SetChallengePassword;
    property NewCert: TCertificate read FNewCert;
    property Verified: Boolean read FVerified;

    { To be overridden: }
    procedure ComposeCertificate; virtual;
    procedure AfterExecute(ATLSServer: ITLSServer); virtual;

    { INewCertDlg }
    function GetTemplateWizard: Boolean;
    function GetIssuedCerts: TX509TrustedCertificates;
    function GetMyCerts: TX509TrustedCertificates;
    function GetPrivateKeyRing: TSsPrivateKeyRingComponent;
    function GetRootCerts: TX509TrustedCertificates;
    function GetExportData: TStream;
    function GetImportData: TStream;
    function GetCACert: TASN1Struct;
    function GetCert: TCertificate;
    function GetP10: TCertificationRequest;
    function GetWizard: TNewCertWizardEnum;
    function GetCreateOpt: TNewCertCreateEnum;
    function GetSignatureDigestAlg: THashAlgorithm;
    function GetPrivateKeyAlg: ObjectIdentifier;
    function GetKeySize: Integer;
    function GetExportableRSA: Boolean;
    function GetSubjectPublicKeyInfo: TSubjectPublicKeyInfo;     
    function GetUserList: IUserList;

    procedure SetTemplateWizard(const Value: Boolean);
    procedure SetIssuedCerts(const Value: TX509TrustedCertificates);
    procedure SetMyCerts(const Value: TX509TrustedCertificates);
    procedure SetPrivateKeyRing(const Value: TSsPrivateKeyRingComponent);
    procedure SetRootCerts(const Value: TX509TrustedCertificates);
    procedure SetExportData(const Value: TStream);
    procedure SetImportData(const Value: TStream);
    procedure SetCACert(const Value: TASN1Struct);
    procedure SetUserList(const Value: IUserList);

    function Execute(ATLSServer: ITLSServer = nil): Boolean;
    procedure SelectWizard(Index: TNewCertWizardEnum;
                           GenerateRequest: Boolean = False);
    property TemplateWizard: Boolean read GetTemplateWizard write SetTemplateWizard;
    property MyCerts: TX509TrustedCertificates read GetMyCerts write SetMyCerts;
    property RootCerts: TX509TrustedCertificates read GetRootCerts write SetRootCerts;
    property IssuedCerts: TX509TrustedCertificates read GetIssuedCerts write SetIssuedCerts;
    property PrivateKeyRing: TSsPrivateKeyRingComponent read GetPrivateKeyRing write SetPrivateKeyRing;
    property ExportData: TStream read GetExportData write SetExportData;
    property ImportData: TStream read GetImportData write SetImportData;
    property CACert: TASN1Struct read GetCACert write SetCACert;
    property Cert: TCertificate read GetCert;
    property P10: TCertificationRequest read GetP10;
    property Wizard: TNewCertWizardEnum read GetWizard;
    property CreateOpt: TNewCertCreateEnum read GetCreateOpt;
    property SignatureDigestAlg: THashAlgorithm read GetSignatureDigestAlg
                                                write SetSignatureDigestAlg;
    property PrivateKeyAlg: ObjectIdentifier read GetPrivateKeyAlg
                                             write SetPrivateKeyAlg;
    property KeySize: Integer read GetKeySize write SetKeySize;
    property ExportableRSA: Boolean read GetExportableRSA write SetExportableRSA;
    property SubjectPublicKeyInfo: TSubjectPublicKeyInfo read GetSubjectPublicKeyInfo;
    property UserList: IUserList read GetUserList write SetUserList;
  end;

  TStreamSecII = class(TSsPrivateKeyRingComponent, IThreadIdentifier)
  private
    FTrustedCertificates: TX509TrustedCertificates;
    FDefaultKeyAgreement: TKeyAgreement;
    FDefaultCipherAlgorithm: TCipherAlgorithm;
    FCurrentSrc: TStream;
    FCurrentDst: TStream;
    FCurrentAct: TEncodeType;
    FCurrentEnc: Boolean;
    FCurrentKey: TASN1Struct;
    FCurrentCipher: TCipher;
    FCurrentHash: THash;
    FCurrentData: TASN1Struct;
    FCurrentDigData: TASN1Struct;
    FCurrentDigDataCount: Int64;
    FCurrentEncData: TASN1Struct;
    FCurrentEncDataCount: Int64;
    FCurrentPadLen: Byte;
    FInternalBuffer: TInternalBuffer;
    FCurrentDigest: string;
    FVerified: Boolean;
{$IFDEF SHA1_AND_MD5}
    FEphemeralKeyGenThreads: TList;
{$ENDIF SHA1_AND_MD5}
    FCollectedCertificates: TX509TrustedCertificates;
    FMyCertificates: TX509TrustedCertificates;
    FCACertificates: TX509TrustedCertificates;
{$IFDEF SHA1_AND_MD5}
    FTLSSessionsID: TList;
    FTLSSessionsUD: TList;
    FTLSOptions: TTLSOptions;
    FOnOutgoingAlert: TTLSAlertEvent;
    FOnIncomingAlert: TTLSAlertEvent;
    FOnCompress: TTLSCompressEvent;
    FOnDecompress: TTLSDecompressEvent;
    FOnRenegotiate: TTLSRenegotiateEvent;
    FOnSelectCompression: TTLSSelectCompressionEvent;
    FOnTLSChangeCipherSpec: TTLSChangeCipherSpec;
    FTLS_RSACertificate: PTLSHandshake;
    FRSACert: PASN1Struct;
    FRSAPriv: IMPIFPrivateKey;
    FTLS_DSSCertificate: PTLSHandshake;
    FDSSCert: PASN1Struct;
    FDSSPriv: IMPDLPrivateKey;
    FTLS_DH_RSACertificate: PTLSHandshake;
    FDH_RSACert: PASN1Struct;
    FDH_RSAPriv: IMPDLPrivateKey;
    FTLS_DH_DSSCertificate: PTLSHandshake;
    FDH_DSSCert: PASN1Struct;
    FDH_DSSPriv: IMPDLPrivateKey;
    FTLS_ECDH_RSACertificate: PTLSHandshake;
    FECDH_RSACert: PASN1Struct;
    FECDH_RSAPriv: IMPECPrivateKey;
    FTLS_ECDH_ECDSACertificate: PTLSHandshake;
    FECDH_ECDSACert: PASN1Struct;
    FECDH_ECDSAPriv: IMPECPrivateKey;
    FDHCertReq: PTLSHandshake;
    FECDHCertReq: PTLSHandshake;
    FOtherCertReq: PTLSHandshake;
    FTLSLock: TCriticalSection;                       
{$ENDIF SHA1_AND_MD5}
    procedure SetTrustedCertificates(
      const Value: TX509TrustedCertificates);
    function GetSequenceNumber: Integer;
    procedure SetSequenceNumber(const Value: Integer);
    procedure SetDefaultKeyAgreement(const Value: TKeyAgreement);
    procedure SetDefaultCipherAlgorithm(const Value: TCipherAlgorithm);
    procedure SetCACertificates(const Value: TX509TrustedCertificates);
    procedure SetCollectedCertificates(const Value: TX509TrustedCertificates);
    procedure SetMyCertificates(const Value: TX509TrustedCertificates);               
{$IFDEF SHA1_AND_MD5}
    function GetTLSSessions(index: Integer): TCustomTLS_ContentLayer;
    function GetTLSSessionCount: Integer;
    procedure SetTLSOptions(const Value: TTLSOptions);
    procedure SetOnIncomingAlert(const Value: TTLSAlertEvent);
    procedure SetOnOutgoingAlert(const Value: TTLSAlertEvent);
    procedure SetOnRenegotiate(const Value: TTLSRenegotiateEvent);
    procedure SetOnSelectCompression(
      const Value: TTLSSelectCompressionEvent);
    procedure SetOnTLSChangeCipherSpec(const Value: TTLSChangeCipherSpec);
{$ENDIF SHA1_AND_MD5}
  protected         
{$IFDEF SHA1_AND_MD5}
    procedure ClearTLSSessions;
{$ENDIF SHA1_AND_MD5}
    procedure DataLoad(Sender: TObject; Field: TASN1Struct);
    procedure DHKeyGenDone(Sender: TObject;
                           const APriv: TDLPrivateKey;
                           const APubl: TDLPublicKey);
    procedure DigDataCaptureLoad(Sender: TObject;
                                 Field: TASN1Struct;
                                 Stream: TStream;
                                 Length: Integer;
                                 var Count: Integer;
                                 var Done: Boolean);
    procedure DigDataCaptureSave(Sender: TObject;
                                 Field: TASN1Struct;
                                 Stream: TStream;
                                 Length: Integer;
                                 var Count: Integer;
                                 var Done: Boolean);      
{$IFDEF SHA1_AND_MD5}
    procedure DoTLSChangeCipherSpec(Sender: TObject);
    procedure DoTLSCompress(Sender: TObject;
                            Src: PTLSPlainText; PTLen: Integer;
                            Method: Byte;
                            var Dst: PTLSCompressed; var CLen: Integer;
                            var Error: Integer);
    procedure DoTLSDecompress(Sender: TObject;
                              Src: PTLSCompressed; CLen: Integer;
                              Method: Byte;
                              var Dst: PTLSPlainText; var PTLen: Integer;
                              var Error: Integer);
    procedure DoTLSDestroy(Sender: TObject);
    procedure DoTLSGetCertificate(Sender: TObject;
                                  KEA: TKeyExchangeAlgorithm;
                                  SignAlg: TSignatureAlgorithm;
                                  var H: PTLSHandshake;
                                  var Len: Integer;
                                  var C: PASN1Struct;
                                  var Found: Boolean);

    procedure DoTLSGetCertReq(Sender: TObject;
                              KEA: TKeyExchangeAlgorithm;
                              var H: PTLSHandshake;
                              var Len: Integer;
                              var Found: Boolean);
    procedure DoTLSIncomingAlert(Sender: TObject;
                                 var Fatal: Boolean;
                                 AlertCode: Integer);
    procedure DoTLSOutgoingAlert(Sender: TObject;
                                 var Fatal: Boolean;
                                 AlertCode: Integer);
    procedure DoTLSRenegotiate(Sender: TObject;
                               const SessionID: TSessionID;
                               var MasterSecret: ShortString);
    procedure DoTLSSelectCompression(Sender: TObject;
                                     const CompMethods: array of TCompressionMethod;
                                     var CompMethod: TCompressionMethod);
    function TLSSessionsUDFind(UserData: Pointer;
                               var Index: Integer): TCustomTLS_ContentLayer;
    function TLSSessionsIDFind(SessionID: TSessionID;
                               var Index: Integer): TCustomTLS_ContentLayer;
{$ENDIF SHA1_AND_MD5}
    procedure EncDataCaptureLoad(Sender: TObject;
                                 Field: TASN1Struct;
                                 Stream: TStream;
                                 Length: Integer;
                                 var Count: Integer;
                                 var Done: Boolean);
    procedure EncDataCaptureSave(Sender: TObject;
                                 Field: TASN1Struct;
                                 Stream: TStream;
                                 Length: Integer;
                                 var Count: Integer;
                                 var Done: Boolean);
    function InternalNewData: TASN1Struct;
    function InternalNewDigestedData: TASN1Struct;
    function InternalNewEncryptedData(OID: string; IV: string): TASN1Struct; overload;
    function InternalNewEncryptedData: TASN1Struct; overload;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RSAKeyGenDone(Sender: TObject;
                            const APriv: TIFPrivateKey;
                            const APubl: TIFPublicKey);
{$IFDEF SHA1_AND_MD5}
    procedure TLSLock;
    procedure TLSUnlock;
{$ENDIF SHA1_AND_MD5}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearExpiredSessionKeys; override;
    function DecodeStream(Src: TStream; Dst: TStream;
                          UseLongTermKey: Boolean;
                          var Action: TEncodeType;
                          KeyIndex: Integer): Boolean;
{$IFDEF SHA1_AND_MD5}
    function FindCreatePrivateKeyTLS(Cert: TASN1Struct): TMPPrivateKey; overload;
{$ENDIF SHA1_AND_MD5}
    procedure EncodeStream(Src: TStream; Dst: TStream;
                           Action: TEncodeType;
                           UseLongTermKey: Boolean;
                           var KeyIndex: Integer;
                           IVector: string);    
{$IFDEF SHA1_AND_MD5}
    function FindTLSSession(const SessionID: TSessionID;
                            var Client: TCustomTLS_ContentLayer;
                            Extract: Boolean = False): Integer; overload;
    function FindTLSSession(UserData: Pointer;
                            var Client: TCustomTLS_ContentLayer;
                            Extract: Boolean = False): Integer; overload;
    function RenewEphemeralDHKey(DHBitSize, DHQBitSize: Integer): Boolean;
    procedure StartEphemeralKeyGen(const RSABitSizes: array of Integer;
                                   const DHBitSizes: array of Integer;
                                   const DHQBitSizes: array of Integer;
                                   RunOnce: Boolean);
    procedure StopEphemeralKeyGen;
    function TLSAddClientSession: TCustomTLS_ContentLayer;
    function TLSAddServerSession(UserData: Pointer = nil): TCustomTLS_ContentLayer;
    function TLSClose(const SessionID: TSessionID;
                      Response: TStream;
                      ErrorCode: PInteger = nil): Integer; overload;
    function TLSDecodeData(var SessionID: TSessionID;
                           Src, Data, Response: TStream;
                           ErrorCode: PInteger = nil): Integer; overload;
    function TLSEncodeData(const SessionID: TSessionID;
                           Data, Response: TStream;
                           ErrorCode: PInteger = nil): Integer; overload;
    function TLSAccept(UserData: Pointer;
                       Src, Response: TStream;
                       ErrorCode: PInteger = nil): Integer;
    function TLSClose(UserData: Pointer;
                      Response: TStream;
                      ErrorCode: PInteger = nil): Integer; overload;
    function TLSConnect(UserData: Pointer;
                        Response: TStream;
                        ErrorCode: PInteger = nil): Integer; overload;
    function TLSConnect(const SessionID: TSessionID;
                        Response: TStream;
                        ErrorCode: PInteger = nil): Integer; overload;
    function TLSConnect(UserData: Pointer;
                        const SessionID: TSessionID;
                        Response: TStream;
                        ErrorCode: PInteger = nil): Integer; overload;
    function TLSConnect(UserData: Pointer;
                        const SessionID: TSessionID;
                        const CipherSuites: TCipherSuites;
                        Response: TStream;
                        ErrorCode: PInteger): Integer; overload;
    function TLSConnectAsServer(UserData: Pointer;
                                Response: TStream;
                                ErrorCode: PInteger = nil): Integer;
    function TLSDecodeData(UserData: Pointer;
                           Src, Data, Response: TStream;
                           ErrorCode: PInteger = nil): Integer; overload;
    function TLSEncodeData(UserData: Pointer;
                           Data, Response: TStream;
                           ErrorCode: PInteger = nil): Integer; overload;
    procedure TLSSetupServer;
    property TLSSessionCount: Integer read GetTLSSessionCount;
    property TLSSessions[index: Integer]: TCustomTLS_ContentLayer read GetTLSSessions;
{$ENDIF SHA1_AND_MD5}
    property SequenceNumber: Integer read GetSequenceNumber write SetSequenceNumber;
  published
    property DefaultCipherAlgorithm: TCipherAlgorithm read FDefaultCipherAlgorithm write SetDefaultCipherAlgorithm;
    property DefaultKeyAgreement: TKeyAgreement read FDefaultKeyAgreement write SetDefaultKeyAgreement;
    property TrustedCertificates: TX509TrustedCertificates read FTrustedCertificates write SetTrustedCertificates;
    property MyCertificates: TX509TrustedCertificates read FMyCertificates write SetMyCertificates;
    property CollectedCertificates: TX509TrustedCertificates read FCollectedCertificates write SetCollectedCertificates;
    property CACertificates: TX509TrustedCertificates read FCACertificates write SetCACertificates;
{$IFDEF SHA1_AND_MD5}
    property TLSOptions: TTLSOptions read FTLSOptions write SetTLSOptions;
    property OnTLSChangeCipherSpec: TTLSChangeCipherSpec read FOnTLSChangeCipherSpec write SetOnTLSChangeCipherSpec;
    property OnTLSCompress: TTLSCompressEvent read FOnCompress write FOnCompress;
    property OnTLSDecompress: TTLSDecompressEvent read FOnDecompress write FOnDecompress;
    property OnTLSIncomingAlert: TTLSAlertEvent read FOnIncomingAlert write SetOnIncomingAlert;
    property OnTLSOutgoingAlert: TTLSAlertEvent read FOnOutgoingAlert write SetOnOutgoingAlert;
    property OnTLSRenegotiate: TTLSRenegotiateEvent read FOnRenegotiate write SetOnRenegotiate;
    property OnTLSSelectCompression: TTLSSelectCompressionEvent read FOnSelectCompression write SetOnSelectCompression;
{$ENDIF SHA1_AND_MD5}
  end;

  TDigestStream = class(TStream)
  private
    FHash: THash;
    FDataStream: TStream;
    procedure SetDataStream(const Value: TStream);
    procedure SetHash(const Value: THash);
    function GetDigest: string;
  public
    constructor Create(ADataStream: TStream; AHash: THash);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Done(ADigest: Pointer);
    property DataStream: TStream read FDataStream write SetDataStream;
    property Digest: string read GetDigest;
    property Hash: THash read FHash write SetHash;
  end;

  TEncryptStream = class(TStream)
  private
    FBlock: Pointer;
    FBlockCount: Integer;
    FBlockSize: Integer;
    FCipher: TCipher;
    FDataStream: TStream;
    procedure SetCipher(const Value: TCipher);
    procedure SetDataStream(const Value: TStream);
  public
    constructor Create(ADataStream: TStream; ACipher: TCipher);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Done: Integer;
    property Cipher: TCipher read FCipher write SetCipher;
    property DataStream: TStream read FDataStream write SetDataStream;
  end;

  TDecryptStream = class(TStream)
  private
    FBlock: PByteArray;
    FBlockCount: Integer;
    FBlockSize: Integer;
    FCipher: TCipher;
    FDataStream: TStream;
    FOK: Boolean;
    FDecBuffer: array [0..255] of Byte;
    FDecLen: Integer;
    procedure SetCipher(const Value: TCipher);
    procedure SetDataStream(const Value: TStream);
  public
    constructor Create(ADataStream: TStream; ACipher: TCipher);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Done: Integer; virtual;
    property Cipher: TCipher read FCipher write SetCipher;
    property DataStream: TStream read FDataStream write SetDataStream;
    property OK: Boolean read FOK;
  end;

  TKeyGenThread = class(TThread)
  private
    FRandom: Pointer;
    FRandomLen: Integer;
    FRunOnce: Boolean;
    FSleepTime: Cardinal;
    procedure SetRunOnce(const Value: Boolean);
    procedure SetSleepTime(const Value: Cardinal);
  protected
    procedure Execute; override;
    procedure Generate; virtual; abstract;
    procedure GenerateRandom;
    procedure GenerateRandomBound(var X: PMPInteger; XMin, XUBound: PMPInteger);
    procedure GenerateRandomPrime(var P: PMPInteger;
                                  BitSize: Integer;
                                  Params: Pointer = nil);
    procedure GeneratePrimeSubGroup(var P, Q: PMPInteger;
                                    BitSize, QBitSize: Integer);
    property RunOnce: Boolean read FRunOnce write SetRunOnce;
    property SleepTime: Cardinal read FSleepTime write SetSleepTime;
  end;

  TRSAKeyEvent = procedure (Sender: TObject;
                            const PrivKey: TIFPrivateKey;
                            const PublKey: TIFPublicKey) of object;

  TRSAKeyGen = class(TKeyGenThread)
  private
    FPriv: TIFPrivateKey;
    FPubl: TIFPublicKey;
    FKeyBitSize: Integer;
    FOnKeyDone: TRSAKeyEvent;
    procedure SetKeyBitSize(const Value: Integer);
    procedure SetOnKeyDone(const Value: TRSAKeyEvent);
  protected
    procedure DoKeyDone;
    procedure Generate; override;
  public
    property KeyBitSize: Integer read FKeyBitSize write SetKeyBitSize;
    property OnKeyDone: TRSAKeyEvent read FOnKeyDone write SetOnKeyDone;
  end;

  TDHKeyEvent = procedure (Sender: TObject;
                           const PrivKey: TDLPrivateKey;
                           const PublKey: TDLPublicKey) of object;

  TDHKeyGen = class(TKeyGenThread)
  private
    FPriv: TDLPrivateKey;
    FPubl: TDLPublicKey;
    FKeyBitSize: Integer;
    FOnKeyDone: TDHKeyEvent;
    FQBitSize: Integer;
    procedure SetKeyBitSize(const Value: Integer);
    procedure SetOnKeyDone(const Value: TDHKeyEvent);
    procedure SetQBitSize(const Value: Integer);
  protected
    procedure DoKeyDone;
    procedure Generate; override;
  public
    property KeyBitSize: Integer read FKeyBitSize write SetKeyBitSize;
    property QBitSize: Integer read FQBitSize write SetQBitSize;
    property OnKeyDone: TDHKeyEvent read FOnKeyDone write SetOnKeyDone;
  end;

procedure CipherAlgorithmToCipherClass(CipherAlgorithm: TCipherAlgorithm;
                                       var KeyedIV: Boolean;
                                       var KeyLength: Integer;
                                       var CipherClass: TCipherClass);
function CipherClassToCipherAlgorithm(KeyLength: Integer;
                                      CipherClass: TCipherClass;
                                      KeyedIV: Boolean;
                                      var CipherAlgorithm: TCipherAlgorithm): Boolean;
function CipherClassToOID(KeyLength: Integer;     // Keysize, if KeyedIV incl IV
                          CipherClass: TCipherClass;
                          KeyedIV: Boolean;
                          var OID: string): Boolean;
function OIDToCipherClass(OID: string;            // Alg + mode + keysize
                          var KeyedIV: Boolean;
                          var KeyLength: Integer; // Keysize, if KeyedIV incl IV
                          var CipherClass: TCipherClass): Boolean;

{$IFNDEF INIT_SECTIONS}
procedure Initialize;
{$ENDIF}
{$IFNDEF FINI_SECTIONS}
procedure Finalize;
{$ENDIF}
               
implementation

uses
  {$IFDEF GUI_APP}
  {$IFDEF DEMO}
  Dialogs,
  {$ENDIF}
  PasswordMain,
  {$ENDIF}
  MpArith, MpYarrow, SsRijndael, SsTwoFish, SsDes, SsArc4, SsBlowFish, Pkix,
  MpECArith, MpEC_NISTCurves, MpEC_X9_62Curves, TlsUtils, TlsLayer, Pkix_CRMF,
  TypInfo, MpIFConversion;

var
  GlobalObject: TASN1Struct;

procedure InitGlobalObject;
var
  SS: TStringStream;
begin
  GlobalObject := TASN1Struct.Create;
  SS := TStringStream.Create(StrSecIIPrivateKeyRingASN1Module);
  try
    GlobalObject.StrictTagging := True;
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
                      
{$IFDEF SHA1_AND_MD5}
function TLSSessionsSortCompare(Item1,Item2: Pointer): Integer;
begin
  Result := CompareStr(TCustomTLS_ContentLayer(Item1).SessionID,
                       TCustomTLS_ContentLayer(Item2).SessionID);
end;

function TLSSessionsUDSortCompare(Item1,Item2: Pointer): Integer;
begin
  Result := LongInt(TCustomTLS_ContentLayer(Item1).UserData) -
            LongInt(TCustomTLS_ContentLayer(Item2).UserData);
end;   
{$ENDIF SHA1_AND_MD5}

procedure CipherAlgorithmToCipherClass(CipherAlgorithm: TCipherAlgorithm;
                                       var KeyedIV: Boolean;
                                       var KeyLength: Integer;
                                       var CipherClass: TCipherClass);
begin
  if CipherAlgorithm in [caAES128CBC..caAES256CFB] then begin
    KeyedIV := False;
    case CipherAlgorithm of
      caAES128CBC,caAES192CBC,caAES256CBC:
        CipherClass := TRijndael_CBC;
      caAES128OFB,caAES192OFB,caAES256OFB:
        CipherClass := TRijndael_OFB;
      caAES128CFB,caAES192CFB,caAES256CFB:
        CipherClass := TRijndael_CFB;
    end;
    case CipherAlgorithm of
      caAES128CBC,caAES128OFB,caAES128CFB:
        KeyLength := 16;
      caAES192CBC,caAES192OFB,caAES192CFB:
        KeyLength := 24;
      caAES256CBC,caAES256OFB,caAES256CFB:
        KeyLength := 32;
    end;
  end else if CipherAlgorithm = ca3DESCBC then begin
    KeyedIV := False;
    CipherClass := T3DES_CBC;
    KeyLength := 24;
  end else if CipherAlgorithm = caARC4 then begin
    KeyedIV := False;
    CipherClass := TARC4;
    KeyLength := 32;
  end else if CipherAlgorithm in [caDESKIVctr..ca3DES168KIVcfb] then begin
    KeyedIV := True;
    case CipherAlgorithm of
      caDESKIVctr, ca3DES112KIVctr, ca3DES168KIVctr:
        CipherClass := T3DES_CTR;
      caDESKIVabc, ca3DES112KIVabc, ca3DES168KIVabc:
        CipherClass := T3DES_ABC;
      caDESKIVpcfb, ca3DES112KIVpcfb, ca3DES168KIVpcfb:
        CipherClass := T3DES_PCFB;
      caDESKIVppcfb, ca3DES112KIVppcfb, ca3DES168KIVppcfb:
        CipherClass := T3DES_PipedPCFB;
      caDESKIVcbc, ca3DES112KIVcbc, ca3DES168KIVcbc:
        CipherClass := T3DES_CBC;
      caDESKIVofb, ca3DES112KIVofb, ca3DES168KIVofb:
        CipherClass := T3DES_OFB;
      caDESKIVcfb, ca3DES112KIVcfb, ca3DES168KIVcfb:
        CipherClass := T3DES_CFB;
    end;
    case CipherAlgorithm of
      caDESKIVctr, caDESKIVpcfb, caDESKIVcbc, caDESKIVofb, caDESKIVcfb:
        KeyLength := 16;
      ca3DES112KIVctr, ca3DES112KIVpcfb,
      ca3DES112KIVcbc, ca3DES112KIVofb, ca3DES112KIVcfb:
        KeyLength := 20;
      ca3DES168KIVctr, ca3DES168KIVpcfb,
      ca3DES168KIVcbc, ca3DES168KIVofb, ca3DES168KIVcfb:
        KeyLength := 24;
      caDESKIVabc, caDESKIVppcfb:
        KeyLength := 24;
      ca3DES112KIVabc, ca3DES112KIVppcfb:
        KeyLength := 28;
      ca3DES168KIVabc, ca3DES168KIVppcfb:
        KeyLength := 32;
    end;
  end else if CipherAlgorithm in [caBlowFish128KIVctr..caBlowFish448KIVcfb] then begin
    KeyedIV := True;
    case CipherAlgorithm of
      caBlowFish128KIVctr, caBlowFish192KIVctr, caBlowFish256KIVctr,
      caBlowFish320KIVctr, caBlowFish384KIVctr, caBlowFish448KIVctr:
        CipherClass := TBlowFish_CTR;
      caBlowFish128KIVabc, caBlowFish192KIVabc, caBlowFish256KIVabc,
      caBlowFish320KIVabc, caBlowFish384KIVabc, caBlowFish448KIVabc:
        CipherClass := TBlowFish_ABC;
      caBlowFish128KIVpcfb, caBlowFish192KIVpcfb, caBlowFish256KIVpcfb,
      caBlowFish320KIVpcfb, caBlowFish384KIVpcfb, caBlowFish448KIVpcfb:
        CipherClass := TBlowFish_PCFB;
      caBlowFish128KIVppcfb, caBlowFish192KIVppcfb, caBlowFish256KIVppcfb,
      caBlowFish320KIVppcfb, caBlowFish384KIVppcfb, caBlowFish448KIVppcfb:
        CipherClass := TBlowFish_PipedPCFB;
      caBlowFish128KIVcbc, caBlowFish192KIVcbc, caBlowFish256KIVcbc,
      caBlowFish320KIVcbc, caBlowFish384KIVcbc, caBlowFish448KIVcbc:
        CipherClass := TBlowFish_CBC;
      caBlowFish128KIVofb, caBlowFish192KIVofb, caBlowFish256KIVofb,
      caBlowFish320KIVofb, caBlowFish384KIVofb, caBlowFish448KIVofb:
        CipherClass := TBlowFish_OFB;
      caBlowFish128KIVcfb, caBlowFish192KIVcfb, caBlowFish256KIVcfb,
      caBlowFish320KIVcfb, caBlowFish384KIVcfb, caBlowFish448KIVcfb:
        CipherClass := TBlowFish_CFB;
    end;
    case CipherAlgorithm of
      caBlowFish128KIVctr, caBlowFish128KIVpcfb,
      caBlowFish128KIVcbc, caBlowFish128KIVofb, caBlowFish128KIVcfb:
        KeyLength := 24;
      caBlowFish192KIVctr, caBlowFish192KIVpcfb,
      caBlowFish192KIVcbc, caBlowFish192KIVofb, caBlowFish192KIVcfb:
        KeyLength := 32;
      caBlowFish256KIVctr, caBlowFish256KIVpcfb,
      caBlowFish256KIVcbc, caBlowFish256KIVofb, caBlowFish256KIVcfb:
        KeyLength := 40;                  
      caBlowFish320KIVctr, caBlowFish320KIVpcfb,
      caBlowFish320KIVcbc, caBlowFish320KIVofb, caBlowFish320KIVcfb:
        KeyLength := 48;
      caBlowFish384KIVctr, caBlowFish384KIVpcfb,
      caBlowFish384KIVcbc, caBlowFish384KIVofb, caBlowFish384KIVcfb:
        KeyLength := 56;
      caBlowFish448KIVctr, caBlowFish448KIVpcfb,
      caBlowFish448KIVcbc, caBlowFish448KIVofb, caBlowFish448KIVcfb:
        KeyLength := 64;
      caBlowFish128KIVabc, caBlowFish128KIVppcfb:
        KeyLength := 32;
      caBlowFish192KIVabc, caBlowFish192KIVppcfb:
        KeyLength := 40;
      caBlowFish256KIVabc, caBlowFish256KIVppcfb:
        KeyLength := 48;
      caBlowFish320KIVabc, caBlowFish320KIVppcfb:
        KeyLength := 56;
      caBlowFish384KIVabc, caBlowFish384KIVppcfb:
        KeyLength := 64;
      caBlowFish448KIVabc, caBlowFish448KIVppcfb:
        KeyLength := 72;
    end;
  end else if CipherAlgorithm in [caTwoFish128KIVctr..caTwoFish256KIVcfb] then begin
    KeyedIV := True;
    case CipherAlgorithm of
      caTwoFish128KIVctr, caTwoFish192KIVctr, caTwoFish256KIVctr:
        CipherClass := TTwoFish_CTR;
      caTwoFish128KIVabc, caTwoFish192KIVabc, caTwoFish256KIVabc:
        CipherClass := TTwoFish_ABC;
      caTwoFish128KIVpcfb, caTwoFish192KIVpcfb, caTwoFish256KIVpcfb:
        CipherClass := TTwoFish_PCFB;
      caTwoFish128KIVppcfb, caTwoFish192KIVppcfb, caTwoFish256KIVppcfb:
        CipherClass := TTwoFish_PipedPCFB;
      caTwoFish128KIVcbc, caTwoFish192KIVcbc, caTwoFish256KIVcbc:
        CipherClass := TTwoFish_CBC;
      caTwoFish128KIVofb, caTwoFish192KIVofb, caTwoFish256KIVofb:
        CipherClass := TTwoFish_OFB;
      caTwoFish128KIVcfb, caTwoFish192KIVcfb, caTwoFish256KIVcfb:
        CipherClass := TTwoFish_CFB;
    end;
    case CipherAlgorithm of
      caTwoFish128KIVctr, caTwoFish128KIVpcfb,
      caTwoFish128KIVcbc, caTwoFish128KIVofb, caTwoFish128KIVcfb:
        KeyLength := 32;
      caTwoFish192KIVctr, caTwoFish192KIVpcfb,
      caTwoFish192KIVcbc, caTwoFish192KIVofb, caTwoFish192KIVcfb:
        KeyLength := 40;
      caTwoFish256KIVctr, caTwoFish256KIVpcfb,
      caTwoFish256KIVcbc, caTwoFish256KIVofb, caTwoFish256KIVcfb:
        KeyLength := 48;
      caTwoFish128KIVabc, caTwoFish128KIVppcfb:
        KeyLength := 48;
      caTwoFish192KIVabc, caTwoFish192KIVppcfb:
        KeyLength := 56;
      caTwoFish256KIVabc, caTwoFish256KIVppcfb:
        KeyLength := 64;
    end;
  end else if CipherAlgorithm in [caAES128KIVctr..caAES256KIVcfb] then begin
    KeyedIV := True;
    case CipherAlgorithm of
      caAES128KIVctr, caAES192KIVctr, caAES256KIVctr:
        CipherClass := TRijndael_CTR;
      caAES128KIVabc, caAES192KIVabc, caAES256KIVabc:
        CipherClass := TRijndael_ABC;
      caAES128KIVpcfb, caAES192KIVpcfb, caAES256KIVpcfb:
        CipherClass := TRijndael_PCFB;
      caAES128KIVppcfb, caAES192KIVppcfb, caAES256KIVppcfb:
        CipherClass := TRijndael_PipedPCFB;
      caAES128KIVcbc, caAES192KIVcbc, caAES256KIVcbc:
        CipherClass := TRijndael_CBC;
      caAES128KIVofb, caAES192KIVofb, caAES256KIVofb:
        CipherClass := TRijndael_OFB;
      caAES128KIVcfb, caAES192KIVcfb, caAES256KIVcfb:
        CipherClass := TRijndael_CFB;
    end;
    case CipherAlgorithm of
      caAES128KIVctr, caAES128KIVpcfb, caAES128KIVcbc, caAES128KIVofb, caAES128KIVcfb:
        KeyLength := 32;
      caAES192KIVctr, caAES192KIVpcfb, caAES192KIVcbc, caAES192KIVofb, caAES192KIVcfb:
        KeyLength := 40;
      caAES256KIVctr, caAES256KIVpcfb, caAES256KIVcbc, caAES256KIVofb, caAES256KIVcfb:
        KeyLength := 48;
      caAES128KIVabc, caAES128KIVppcfb:
        KeyLength := 48;
      caAES192KIVabc, caAES192KIVppcfb:
        KeyLength := 56;
      caAES256KIVabc, caAES256KIVppcfb:
        KeyLength := 64;
    end;
  end;
end;

function CipherClassToCipherAlgorithm(KeyLength: Integer;
                                      CipherClass: TCipherClass;
                                      KeyedIV: Boolean;
                                      var CipherAlgorithm: TCipherAlgorithm): Boolean;
begin
  if KeyedIV then begin
    if CipherClass.InheritsFrom(TRijndael_ECB) then begin
      case CipherClass.Mode of  
        cmCTR:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caAES128KIVCTR;
              24: CipherAlgorithm := caAES192KIVCTR;
              32: CipherAlgorithm := caAES256KIVCTR;
            else
              Result := False;
            end;
          end;
        cmABC:
          begin
            Result := True;
            case KeyLength-32 of
              16: CipherAlgorithm := caAES128KIVABC;
              24: CipherAlgorithm := caAES192KIVABC;
              32: CipherAlgorithm := caAES256KIVABC;
            else
              Result := False;
            end;
          end;   
        cmPCFB:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caAES128KIVPCFB;
              24: CipherAlgorithm := caAES192KIVPCFB;
              32: CipherAlgorithm := caAES256KIVPCFB;
            else
              Result := False;
            end;
          end;
        cmPipedPCFB:
          begin
            Result := True;
            case KeyLength-32 of
              16: CipherAlgorithm := caAES128KIVPPCFB;
              24: CipherAlgorithm := caAES192KIVPPCFB;
              32: CipherAlgorithm := caAES256KIVPPCFB;
            else
              Result := False;
            end;
          end;
        cmCBC:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caAES128KIVCBC;
              24: CipherAlgorithm := caAES192KIVCBC;
              32: CipherAlgorithm := caAES256KIVCBC;
            else
              Result := False;
            end;
          end;
        cmCFB:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caAES128KIVCFB;
              24: CipherAlgorithm := caAES192KIVCFB;
              32: CipherAlgorithm := caAES256KIVCFB;
            else
              Result := False;
            end;
          end;
        cmOFB:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caAES128KIVOFB;
              24: CipherAlgorithm := caAES192KIVOFB;
              32: CipherAlgorithm := caAES256KIVOFB;
            else
              Result := False;
            end;
          end;
      else
        Result := False;
      end
    end else if CipherClass.InheritsFrom(TBlowFish_ECB) then begin
      case CipherClass.Mode of
        cmCTR:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caBlowFish128KIVCTR;
              24: CipherAlgorithm := caBlowFish192KIVCTR;
              32: CipherAlgorithm := caBlowFish256KIVCTR;
              40: CipherAlgorithm := caBlowFish320KIVCTR;
              48: CipherAlgorithm := caBlowFish384KIVCTR;
              56: CipherAlgorithm := caBlowFish448KIVCTR;
            else
              Result := False;
            end;
          end;
        cmABC:
          begin
            Result := True;
            case KeyLength-32 of
              16: CipherAlgorithm := caBlowFish128KIVABC;
              24: CipherAlgorithm := caBlowFish192KIVABC;
              32: CipherAlgorithm := caBlowFish256KIVABC;
              40: CipherAlgorithm := caBlowFish320KIVABC;
              48: CipherAlgorithm := caBlowFish384KIVABC;
              56: CipherAlgorithm := caBlowFish448KIVABC;
            else
              Result := False;
            end;
          end;
        cmPCFB:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caBlowFish128KIVPCFB;
              24: CipherAlgorithm := caBlowFish192KIVPCFB;
              32: CipherAlgorithm := caBlowFish256KIVPCFB;
              40: CipherAlgorithm := caBlowFish320KIVPCFB;
              48: CipherAlgorithm := caBlowFish384KIVPCFB;
              56: CipherAlgorithm := caBlowFish448KIVPCFB;
            else
              Result := False;
            end;
          end;
        cmPipedPCFB:
          begin
            Result := True;
            case KeyLength-32 of
              16: CipherAlgorithm := caBlowFish128KIVPPCFB;
              24: CipherAlgorithm := caBlowFish192KIVPPCFB;
              32: CipherAlgorithm := caBlowFish256KIVPPCFB;
              40: CipherAlgorithm := caBlowFish320KIVPPCFB;
              48: CipherAlgorithm := caBlowFish384KIVPPCFB;
              56: CipherAlgorithm := caBlowFish448KIVPPCFB;
            else
              Result := False;
            end;
          end;
        cmCBC:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caBlowFish128KIVCBC;
              24: CipherAlgorithm := caBlowFish192KIVCBC;
              32: CipherAlgorithm := caBlowFish256KIVCBC;
              40: CipherAlgorithm := caBlowFish320KIVCBC;
              48: CipherAlgorithm := caBlowFish384KIVCBC;
              56: CipherAlgorithm := caBlowFish448KIVCBC;
            else
              Result := False;
            end;
          end;
        cmCFB:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caBlowFish128KIVCFB;
              24: CipherAlgorithm := caBlowFish192KIVCFB;
              32: CipherAlgorithm := caBlowFish256KIVCFB;
              40: CipherAlgorithm := caBlowFish320KIVCFB;
              48: CipherAlgorithm := caBlowFish384KIVCFB;
              56: CipherAlgorithm := caBlowFish448KIVCFB;
            else
              Result := False;
            end;
          end;
        cmOFB:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caBlowFish128KIVOFB;
              24: CipherAlgorithm := caBlowFish192KIVOFB;
              32: CipherAlgorithm := caBlowFish256KIVOFB;
              40: CipherAlgorithm := caBlowFish320KIVOFB;
              48: CipherAlgorithm := caBlowFish384KIVOFB;
              56: CipherAlgorithm := caBlowFish448KIVOFB;
            else
              Result := False;
            end;
          end;
      else
        Result := False;
      end
    end else if CipherClass.InheritsFrom(TTwoFish_ECB) then begin
      case CipherClass.Mode of
        cmCTR:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caTwoFish128KIVCTR;
              24: CipherAlgorithm := caTwoFish192KIVCTR;
              32: CipherAlgorithm := caTwoFish256KIVCTR;
            else
              Result := False;
            end;
          end;
        cmABC:
          begin
            Result := True;
            case KeyLength-32 of
              16: CipherAlgorithm := caTwoFish128KIVABC;
              24: CipherAlgorithm := caTwoFish192KIVABC;
              32: CipherAlgorithm := caTwoFish256KIVABC;
            else
              Result := False;
            end;
          end;
        cmPCFB:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caTwoFish128KIVPCFB;
              24: CipherAlgorithm := caTwoFish192KIVPCFB;
              32: CipherAlgorithm := caTwoFish256KIVPCFB;
            else
              Result := False;
            end;
          end;
        cmPipedPCFB:
          begin
            Result := True;
            case KeyLength-32 of
              16: CipherAlgorithm := caTwoFish128KIVPPCFB;
              24: CipherAlgorithm := caTwoFish192KIVPPCFB;
              32: CipherAlgorithm := caTwoFish256KIVPPCFB;
            else
              Result := False;
            end;
          end;
        cmCBC:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caTwoFish128KIVCBC;
              24: CipherAlgorithm := caTwoFish192KIVCBC;
              32: CipherAlgorithm := caTwoFish256KIVCBC;
            else
              Result := False;
            end;
          end;
        cmCFB:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caTwoFish128KIVCFB;
              24: CipherAlgorithm := caTwoFish192KIVCFB;
              32: CipherAlgorithm := caTwoFish256KIVCFB;
            else
              Result := False;
            end;
          end;
        cmOFB:
          begin
            Result := True;
            case KeyLength-16 of
              16: CipherAlgorithm := caTwoFish128KIVOFB;
              24: CipherAlgorithm := caTwoFish192KIVOFB;
              32: CipherAlgorithm := caTwoFish256KIVOFB;
            else
              Result := False;
            end;
          end;
      else
        Result := False;
      end
    end else if CipherClass.InheritsFrom(T3DES_ECB) then begin
      case CipherClass.Mode of
        cmCTR:
          begin
            Result := True;
            case KeyLength-8 of
              8: CipherAlgorithm := caDESKIVCTR;
              16: CipherAlgorithm := ca3DES112KIVCTR;
              24: CipherAlgorithm := ca3DES168KIVCTR;
            else
              Result := False;
            end;
          end;
        cmABC:
          begin
            Result := True;
            case KeyLength-16 of
              8: CipherAlgorithm := caDESKIVABC;
              16: CipherAlgorithm := ca3DES112KIVABC;
              24: CipherAlgorithm := ca3DES168KIVABC;
            else
              Result := False;
            end;
          end;
        cmPCFB:
          begin
            Result := True;
            case KeyLength-8 of
              8: CipherAlgorithm := caDESKIVPCFB;
              16: CipherAlgorithm := ca3DES112KIVPCFB;
              24: CipherAlgorithm := ca3DES168KIVPCFB;
            else
              Result := False;
            end;
          end;
        cmPipedPCFB:
          begin
            Result := True;
            case KeyLength-16 of
              8: CipherAlgorithm := caDESKIVPPCFB;
              16: CipherAlgorithm := ca3DES112KIVPPCFB;
              24: CipherAlgorithm := ca3DES168KIVPPCFB;
            else
              Result := False;
            end;
          end;
        cmCBC:
          begin
            Result := True;
            case KeyLength-8 of
              8: CipherAlgorithm := caDESKIVCBC;
              16: CipherAlgorithm := ca3DES112KIVCBC;
              24: CipherAlgorithm := ca3DES168KIVCBC;
            else
              Result := False;
            end;
          end;
        cmCFB:
          begin
            Result := True;
            case KeyLength-8 of
              8: CipherAlgorithm := caDESKIVCFB;
              16: CipherAlgorithm := ca3DES112KIVCFB;
              24: CipherAlgorithm := ca3DES168KIVCFB;
            else
              Result := False;
            end;
          end;
        cmOFB:
          begin
            Result := True;
            case KeyLength-8 of
              8: CipherAlgorithm := caDESKIVOFB;
              16: CipherAlgorithm := ca3DES112KIVOFB;
              24: CipherAlgorithm := ca3DES168KIVOFB;
            else
              Result := False;
            end;
          end;
      else
        Result := False;
      end
    end else
      Result := False
  end else begin
    if CipherClass.InheritsFrom(TRijndael_ECB) then begin
      case CipherClass.Mode of
        cmCBC:
          begin
            Result := True;
            case KeyLength of
              16: CipherAlgorithm := caAES128CBC;
              24: CipherAlgorithm := caAES192CBC;
              32: CipherAlgorithm := caAES256CBC;
            else
              Result := False;
            end;
          end;
        cmCFB:
          begin
            Result := True;
            case KeyLength of
              16: CipherAlgorithm := caAES128CFB;
              24: CipherAlgorithm := caAES192CFB;
              32: CipherAlgorithm := caAES256CFB;
            else
              Result := False;
            end;
          end;
        cmOFB:
          begin
            Result := True;
            case KeyLength of
              16: CipherAlgorithm := caAES128OFB;
              24: CipherAlgorithm := caAES192OFB;
              32: CipherAlgorithm := caAES256OFB;
            else
              Result := False;
            end;
          end;
      else
        Result := False;
      end;
    end else if CipherClass = TARC4 then begin
      if KeyLength <= 256 then begin
        Result := True;
        CipherAlgorithm := caARC4;
      end else
        Result := False;
    end else if CipherClass = T3DES_CBC then begin
      if KeyLength = 24 then begin
        Result := True;
        CipherAlgorithm := ca3DESCBC;
      end else
        Result := False;
    end else
      Result := False;
  end;
end;

function OIDToCipherClass(OID: string;
                          var KeyedIV: Boolean;
                          var KeyLength: Integer;
                          var CipherClass: TCipherClass): Boolean;
var
  I: Integer;
  SI: Cardinal;
begin
  KeyedIV := False;
  if OID = DES_EDE3_CBC then begin
    CipherClass := FindCipherClass(caDES,cmCBC);
    KeyLength := 24;
  end else if OID = arcfour then begin
    CipherClass := FindCipherClass(SecUtils.caARC4,cmOFB);
    KeyLength := 32;
  end else if OID = id_aes128_ecb then begin
    CipherClass := FindCipherClass(caRijndael,cmECB);
    KeyLength := 16;
  end else if OID = id_aes128_cbc then begin
    CipherClass := FindCipherClass(caRijndael,cmCBC);
    KeyLength := 16;
  end else if OID = id_aes128_ofb then begin
    CipherClass := FindCipherClass(caRijndael,cmOFB);
    KeyLength := 16;
  end else if OID = id_aes128_cfb then begin
    CipherClass := FindCipherClass(caRijndael,cmCFB);
    KeyLength := 16;
  end else if OID = id_aes128_wrap then begin
    CipherClass := FindCipherClass(caRijndael,cmAESWrap);
    KeyLength := 16;
  end else if OID = id_aes192_ecb then begin
    CipherClass := FindCipherClass(caRijndael,cmECB);
    KeyLength := 24;
  end else if OID = id_aes192_cbc then begin
    CipherClass := FindCipherClass(caRijndael,cmCBC);
    KeyLength := 24;
  end else if OID = id_aes192_ofb then begin
    CipherClass := FindCipherClass(caRijndael,cmOFB);
    KeyLength := 24;
  end else if OID = id_aes192_cfb then begin
    CipherClass := FindCipherClass(caRijndael,cmCFB);
    KeyLength := 24;
  end else if OID = id_aes192_wrap then begin
    CipherClass := FindCipherClass(caRijndael,cmAESWrap);
    KeyLength := 24;
  end else if OID = id_aes256_ecb then begin
    CipherClass := FindCipherClass(caRijndael,cmECB);
    KeyLength := 32;
  end else if OID = id_aes256_cbc then begin
    CipherClass := FindCipherClass(caRijndael,cmCBC);
    KeyLength := 32;
  end else if OID = id_aes256_ofb then begin
    CipherClass := FindCipherClass(caRijndael,cmOFB);
    KeyLength := 32;
  end else if OID = id_aes256_cfb then begin
    CipherClass := FindCipherClass(caRijndael,cmCFB);
    KeyLength := 32;
  end else if OID = id_aes256_wrap then begin
    CipherClass := FindCipherClass(caRijndael,cmAESWrap);
    KeyLength := 32;
  end else if Copy(OID,1,Length(id_kivCipher)) = id_kivCipher then begin
    I := Length(id_kivCipher) + 2;
    SI := ExtractSubIden(OID,I);
    case SI of
      1..3:   CipherClass := FindCipherClass(caRijndael,cmCTR);
      4..6:   CipherClass := FindCipherClass(caRijndael,cmABC);
      7..9:   CipherClass := FindCipherClass(caRijndael,cmPCFB);
      10..12: CipherClass := FindCipherClass(caRijndael,cmPipedPCFB);
      16..18: CipherClass := FindCipherClass(caRijndael,cmCBC);
      19..21: CipherClass := FindCipherClass(caRijndael,cmOFB);
      22..24: CipherClass := FindCipherClass(caRijndael,cmCFB);
      31..33: CipherClass := FindCipherClass(caTwoFish,cmCTR);
      34..36: CipherClass := FindCipherClass(caTwoFish,cmABC);
      37..39: CipherClass := FindCipherClass(caTwoFish,cmPCFB);
      40..42: CipherClass := FindCipherClass(caTwoFish,cmPipedPCFB);
      46..48: CipherClass := FindCipherClass(caTwoFish,cmCBC);
      49..51: CipherClass := FindCipherClass(caTwoFish,cmOFB);
      52..54: CipherClass := FindCipherClass(caTwoFish,cmCFB);
      61..63: CipherClass := FindCipherClass(caDES,cmCTR);
      64..66: CipherClass := FindCipherClass(caDES,cmABC);
      67..69: CipherClass := FindCipherClass(caDES,cmPCFB);
      70..72: CipherClass := FindCipherClass(caDES,cmPipedPCFB);
      76..78: CipherClass := FindCipherClass(caDES,cmCBC);
      79..81: CipherClass := FindCipherClass(caDES,cmOFB);
      82..84: CipherClass := FindCipherClass(caDES,cmCFB);
      91..96: CipherClass := FindCipherClass(caBlowFish,cmCTR);
      97..102: CipherClass := FindCipherClass(caBlowFish,cmABC);
      103..108: CipherClass := FindCipherClass(caBlowFish,cmPCFB);
      109..114: CipherClass := FindCipherClass(caBlowFish,cmPipedPCFB);
      115..120: CipherClass := FindCipherClass(caBlowFish,cmCBC);
      121..126: CipherClass := FindCipherClass(caBlowFish,cmOFB);
      127..132: CipherClass := FindCipherClass(caBlowFish,cmCFB);
    end;
    if Assigned(CipherClass) then begin
      if SI < 61 then begin
        KeyLength := ((SI-1) mod 3)*8 + 16;
        if CipherClass.Mode in [cmABC,cmPipedPCFB] then
          KeyLength := KeyLength + 32
        else
          KeyLength := KeyLength + 16;
      end else if SI < 91 then begin
        KeyLength := ((SI-1) mod 3)*8 + 8;
        if CipherClass.Mode in [cmABC,cmPipedPCFB] then
          KeyLength := KeyLength + 16
        else
          KeyLength := KeyLength + 8;
      end else begin
        KeyLength := ((SI-1) mod 6)*8 + 16;
        if CipherClass.Mode in [cmABC,cmPipedPCFB] then
          KeyLength := KeyLength + 16
        else
          KeyLength := KeyLength + 8;
      end;
    end;
    KeyedIV := True;
  end else
    CipherClass := nil;
  Result := Assigned(CipherClass);
end;

function CipherClassToOID(KeyLength: Integer;
                          CipherClass: TCipherClass;
                          KeyedIV: Boolean;
                          var OID: string): Boolean;
begin
  if KeyedIV then begin
    OID := CipherClass.KeyedIVOID(KeyLength -
                                  TBlockCipherClass(CipherClass).BlockSize*
                                  TBlockCipherClass(CipherClass).BlockVectorSize);
    Result := OID <> '';
  end else begin
    OID := CipherClass.OID(KeyLength);
    Result := OID <> '';
  end;
end;

procedure _KDF(Password: ISecretKey;
               KDF: TKeyDerivation;
               KDFIterations: Integer;
               EncryptionAlgorithmID: string;
               PartyAInfo: ISecretKey;
               SuppPublInfo: string;
               KeyLen: Integer;
               var Key: ISecretKey); overload;
var
  BERAlgID: string;
  Params: ISecretKey;
  I, Idx: Integer;
  HA: THashAlgorithm;
begin
  case KDF of
{$IFDEF SHA1}
     kdfWPv2SHA1:
                      WeakPasswordKDFv2(Password,
                                        PartyAInfo,
                                        KDFIterations,
                                        haSHA1,
                                        KeyLen,Key);
{$ENDIF SHA1}
{$IFDEF MD5}
     kdfWPv2MD5:
                      WeakPasswordKDFv2(Password,
                                        PartyAInfo,
                                        KDFIterations,
                                        haMD5,
                                        KeyLen,Key);
{$ENDIF MD5}
{$IFDEF RIPEMD160}
     kdfWPv2RipeMD160:
                      WeakPasswordKDFv2(Password,
                                        PartyAInfo,
                                        KDFIterations,
                                        haRipeMD160,
                                        KeyLen,Key);
{$ENDIF RIPEMD160}
{$IFDEF SHA256}
     kdfWPv2SHA256:
                      WeakPasswordKDFv2(Password,
                                        PartyAInfo,
                                        KDFIterations,
                                        haSHA256,
                                        KeyLen,Key);
{$ENDIF SHA256}
{$IFDEF SHA512}
     kdfWPv2SHA384:
                      WeakPasswordKDFv2(Password,
                                        PartyAInfo,
                                        KDFIterations,
                                        haSHA384,
                                        KeyLen,Key);
     kdfWPv2SHA512:
                      WeakPasswordKDFv2(Password,
                                        PartyAInfo,
                                        KDFIterations,
                                        haSHA512,
                                        KeyLen,Key);
{$ENDIF SHA512}
     kdfWeakPassword,
     kdfWeakPassword256:
                      begin
{$IFDEF SHA1}
                        if KDF = kdfWeakPassword then
                          WeakPasswordToPrivKey(Password,
                                                PartyAInfo,
                                                KDFIterations,
                                                KeyLen,Key)
                        else                               
{$ENDIF SHA1}
{$IFDEF SHA256}
                          WeakPasswordToPrivKey256(Password,
                                                   PartyAInfo,
                                                   KDFIterations,
                                                   KeyLen,Key);
{$ENDIF SHA256}
                      end;
     kdfP1636_2_sha1,
     kdfP1636_2_sha256:
                      begin
                        if KDF = kdfP1636_2_sha1 then
{$IFDEF SHA1}
                          HA := haSHA1
{$ELSE  SHA1}
                          Exit
{$ENDIF SHA1}
                        else
{$IFDEF SHA256}
                          HA := haSHA256;
{$ELSE  SHA256}
                          Exit;
{$ENDIF SHA256}
                        if KDFIterations > 1 then begin
                          KDF2(Password,
                               PartyAInfo,
                               HA,
                               KeyLen,Key);
                          for I := 1 to KDFIterations - 1 do
                            KDF2(Password,
                                 Key,
                                 HA,
                                 KeyLen,Key);
                        end else begin
                          KDF2(Password,
                               PartyAInfo,
                               HA,
                               KeyLen,Key);
                        end;
                      end;
     kdfX9_42Concat_sha1,
     kdfX9_42Concat_sha256:
                      begin
                        if KDF = kdfX9_42Concat_sha1 then
{$IFDEF SHA1}
                          HA := haSHA1
{$ELSE  SHA1}
                          Exit
{$ENDIF SHA1}
                        else
{$IFDEF SHA256}
                          HA := haSHA256;
{$ELSE  SHA256}
                          Exit;
{$ENDIF SHA256}
                        BERAlgID := InterpretOIDToBER(EncryptionAlgorithmID);
                        Params := TSecretKey.Create('');
                        Params.SetLength(Length(BERAlgID) + 4 + KeyLen + Length(SuppPublInfo));
                        Params.SetKeyStrAt(BERAlgID,0);
                        Params.SetKeyStrAt(#0#0#0#0,Length(BERAlgID));
                        Params.SetKeyStrAt(SuppPublInfo,Length(BERAlgID) + 4 + KeyLen);
                        Params.SetKeyAt(PartyAInfo,Length(BERAlgID) + 4);
                        KDFX9_42Concatenation(Password,
                                              Params,
                                              HA,
                                              KeyLen,Key);
                        if Assigned(PartyAInfo) and (KDFIterations > 1) then
                          for I := 1 to KDFIterations - 1 do begin
                            if Key.KeyLen < PartyAInfo.KeyLen then
                              Params.SetKeyAt(Key,Length(BERAlgID) + 4)
                            else
                              Move(Key.Key^,Params.KeyBytes^[Length(BERAlgID) + 4],PartyAInfo.KeyLen);
                            KDFX9_42Concatenation(Password,
                                                  Params,
                                                  HA,
                                                  KeyLen,Key);
                          end;
                      end;
     kdfX9_42ASN1DER_sha1,
     kdfX9_42ASN1DER_sha256:
                      begin
                        if KDF = kdfX9_42ASN1DER_sha1 then
{$IFDEF SHA1}
                          HA := haSHA1
{$ELSE  SHA1}
                          Exit
{$ENDIF SHA1}
                        else
{$IFDEF SHA256}
                          HA := haSHA256;
{$ELSE  SHA256}
                          Exit;
{$ENDIF SHA256}
                        BERAlgID := InterpretOIDToBER(EncryptionAlgorithmID);
                        Params := TSecretKey.Create('');
                        if PartyAInfo.KeyLen > 0 then begin
                          if SuppPublInfo <> '' then
                            Params.SetLength(2 +
                                               2 +
                                                 2 + Length(BERAlgID) +
                                                 2 + 4 +
                                               2 +
                                                 2 + PartyAInfo.KeyLen +
                                               2 +
                                                 2 + Length(SuppPublInfo))
                          else
                            Params.SetLength(2 +
                                               2 +
                                                 2 + Length(BERAlgID) +
                                                 2 + 4 +
                                               2 +
                                                 2 + PartyAInfo.KeyLen);
                        end else begin
                          if SuppPublInfo <> '' then
                            Params.SetLength(2 +
                                               2 +
                                                 2 + Length(BERAlgID) +
                                                 2 + 4 +
                                               2 +
                                                 2 + Length(SuppPublInfo))
                          else
                            Params.SetLength(2 +
                                               2 +
                                                 2 + Length(BERAlgID) +
                                                 2 + 4);
                        end;
                        if Params.KeyLen > 129 then
                          raise Exception.Create('Too large paramater for key derivation function');
                        Params.KeyBytes[0] := $30;
                        Params.KeyBytes[1] := Byte(Params.KeyLen - 2);
                        Params.KeyBytes[2] := $30;
                        Params.KeyBytes[3] := Byte(Length(BERAlgID) + 8);
                        Params.KeyBytes[4] := $06;
                        Params.KeyBytes[5] := Byte(Length(BERAlgID));
                        Params.SetKeyStrAt(BERAlgID,6);
                        Idx := 6 + Length(BERAlgID);
                        Params.KeyBytes[Idx] := $04;
                        Params.KeyBytes[Idx + 1] := $04;
                        Params.SetKeyStrAt(#0#0#0#0,Idx + 2);
                        Idx := Idx + 6;
                        if Assigned(PartyAInfo) and (PartyAInfo.KeyLen > 0) then begin
                          Params.KeyBytes[Idx] := $A0;
                          Params.KeyBytes[Idx + 1] := Byte(KeyLen + 2);
                          Params.KeyBytes[Idx + 2] := $04;
                          Params.KeyBytes[Idx + 3] := Byte(PartyAInfo.KeyLen);
                          Params.SetKeyAt(PartyAInfo,Idx + 4);
                          Idx := Idx + 4 + PartyAInfo.KeyLen;
                        end;
                        if SuppPublInfo <> '' then begin
                          Params.KeyBytes[Idx] := $A2;
                          Params.KeyBytes[Idx + 1] := Byte(Length(SuppPublInfo) + 2);
                          Params.KeyBytes[Idx + 2] := $04;
                          Params.KeyBytes[Idx + 3] := Byte(Length(SuppPublInfo));
                          Params.SetKeyStrAt(SuppPublInfo,Idx + 4);
                        end;
                        KDFX9_42ASN1DER(Password,
                                        Params,
                                        HA,
                                        KeyLen,Key);
                        if KDFIterations > 1 then begin
                          for I := 1 to KDFIterations - 1 do begin
                            if Key.KeyLen < PartyAInfo.KeyLen then
                              Params.SetKeyAt(Key,Length(BERAlgID) + 16)
                            else
                              Move(Key.Key^,Params.KeyBytes^[Length(BERAlgID) + 16],PartyAInfo.KeyLen);
                            KDFX9_42ASN1DER(Password,
                                            Params,
                                            HA,
                                            Key.KeyLen,Key);
                          end;
                        end;
                      end;
  end;
end;

procedure _KDF(const Password: string;
               KDF: TKeyDerivation;
               KDFIterations: Integer;
               EncryptionAlgorithmID: string;
               const PartyAInfo; PartyAInfoLen: Integer;
               SuppPublInfo: string;
               var Key: string); overload;
var
  PW, PAI, K: ISecretKey;
begin
  PW := TSecretKey.Create('');
  PW.SetKey(Pointer(Password),Length(Password),0);
  PAI := TSecretKey.Create('');
  PAI.SetKey(@PartyAInfo,PartyAInfoLen,0);
  _KDF(PW,KDF,KDFIterations,EncryptionAlgorithmID,PAI,SuppPublInfo,Length(Key),K);
  Move(K.Key^,Pointer(Key)^,K.KeyLen);
end;

procedure _KDF(Password: ISecretKey;
               KDF: TKeyDerivation;
               KDFIterations: Integer;
               EncryptionAlgorithmID: string;
               SuppPublInfo: string;
               Struct: TASN1Struct;
               KeyLen: Integer;
               var Key: ISecretKey); overload;
var
  Params: ISecretKey;
  F: TASN1Struct;
begin
  if Assigned(Struct) then begin
    Struct.EditField('kdfScheme',Ord(KDF));
    Struct.EditField('kdfIterations',KDFIterations);
  end;
  Params := TSecretKey.Create('');
  case KDF of
     kdfP1636_2_sha1,
     kdfP1636_2_sha256,
     kdfX9_42Concat_sha1,
     kdfX9_42Concat_sha256,
     kdfX9_42ASN1DER_sha1,
     kdfX9_42ASN1DER_sha256:
                      begin
                        Params.SetLength(KeyLen);
                        RawRandom(Params.Key^,KeyLen*8);
                      end;
  else
    Params.SetLength(32);
    RawRandom(Params.Key^,256);
  end;
  _KDF(Password,
       KDF,KDFIterations,
       EncryptionAlgorithmID,
       Params,
       SuppPublInfo,
       KeyLen,
       Key);
  if Assigned(Struct) then begin
    F := Struct.FindField('kdfParameter')^;
    F.SetContent(Params.Key^,Params.KeyLen);
  end;
end;

procedure _KDF(const Password: string;
               KDF: TKeyDerivation;
               KDFIterations: Integer;
               EncryptionAlgorithmID: string;
               SuppPublInfo: string;
               Struct: TASN1Struct;
               var Key: string); overload;
var
  PW, K: ISecretKey;
begin
  PW := TSecretKey.Create('');
  PW.SetKey(Pointer(Password),Length(Password),0);
  _KDF(PW,KDF,KDFIterations,EncryptionAlgorithmID,SuppPublInfo,Struct,Length(Key),K);
  Move(K.Key^,Pointer(Key)^,K.KeyLen);
end;

procedure _KDF(Password: ISecretKey;
               KDF: TKeyDerivation;
               KDFIterations: Integer;
               EncryptionAlgorithmID: string;
               SuppPublInfo: string;
               var Params: string;
               KeyLen: Integer;
               var Key: ISecretKey); overload;
var
  Prm: ISecretKey;
begin
  Prm := TSecretKey.Create('');
  case KDF of
     kdfWPv2SHA1,
     kdfWPv2MD5,
     kdfWPv2RipeMD160,
     kdfWPv2SHA256,
     kdfWPv2SHA384,
     kdfWPv2SHA512,
     kdfWeakPassword,
     kdfWeakPassword256:
                      begin
                        Prm.SetLength(32);
                        RawRandom(Prm.Key^,256);
                        _KDF(Password,KDF,KDFIterations,'',Prm,'',KeyLen,Key);
                      end;
     kdfP1636_2_sha1,
     kdfP1636_2_sha256:
                      begin
                        Prm.SetLength(KeyLen);
                        RawRandom(Prm.Key^,KeyLen*8);
                        _KDF(Password,
                             KDF,KDFIterations,
                             '',
                             Prm,
                             '',
                             KeyLen,Key);
                      end;
     kdfX9_42Concat_sha1,
     kdfX9_42Concat_sha256,
     kdfX9_42ASN1DER_sha1,
     kdfX9_42ASN1DER_sha256:
                      begin
                        Prm.SetLength(KeyLen);
                        RawRandom(Prm.Key^,KeyLen*8);
                        _KDF(Password,
                             KDF,KDFIterations,
                             EncryptionAlgorithmID,
                             Prm,
                             SuppPublInfo,
                             KeyLen,Key);
                      end;
  end;
  SetLength(Params,Prm.KeyLen);
  Move(Prm.Key^,Pointer(Params)^,Prm.KeyLen);
end;

procedure _KDF(const Password: string;
               KDF: TKeyDerivation;
               KDFIterations: Integer;
               EncryptionAlgorithmID: string;
               SuppPublInfo: string;
               var Params: string;
               var Key: string); overload;
var
  PW, K: ISecretKey;
begin
  PW := TSecretKey.Create('');
  PW.SetKey(Pointer(Password),Length(Password),0);
  _KDF(PW,KDF,KDFIterations,EncryptionAlgorithmID,SuppPublInfo,Params,Length(Key),K);
  Move(K.Key^,Pointer(Key)^,K.KeyLen);
end;

function _KDFVerification(Password: ISecretKey;
                          EncryptionAlgorithmID: string;
                          SuppPublInfo: string;
                          Struct: TASN1Struct;
                          KeyLen: Integer;
                          var Key: ISecretKey): Boolean; overload;
var
  cKDF: Integer;
  KDF: TKeyDerivation;
  KDFIterations: Integer;
  BERAlgID, Params: string;
  HA: THashAlgorithm;
  F: TASN1Struct;
  I, Idx: Integer;
begin
  F := Struct.FindField('kdfScheme')^;
  cKDF := F.ContentAsInteger;
  Result := (cKDF >= Ord(Low(TKeyDerivation))) and
            (cKDF <= Ord(High(TKeyDerivation)));
  if not Result then Exit;
  KDF := TKeyDerivation(cKDF);
  F := Struct.FindField('kdfIterations')^;
  KDFIterations := F.ContentAsInteger;
  try
    case KDF of
{$IFDEF SHA1}
     kdfWPv2SHA1:
                  begin
                    F := Struct.FindField('kdfParameter')^;
                    Params := F.ContentAsOctetString;
                    Result := Length(Params) > 0;
                    if Result then
                      WeakPasswordKDFv2(Password.Key^,Password.KeyLen,
                                        Pointer(Params)^,Length(Params),
                                        KDFIterations,
                                        haSHA1,
                                        KeyLen,Key);
                  end;
{$ENDIF SHA1}
{$IFDEF MD5}
     kdfWPv2MD5:
                  begin
                    F := Struct.FindField('kdfParameter')^;
                    Params := F.ContentAsOctetString;
                    Result := Length(Params) > 0;
                    if Result then
                      WeakPasswordKDFv2(Password.Key^,Password.KeyLen,
                                        Pointer(Params)^,Length(Params),
                                        KDFIterations,
                                        haMD5,
                                        KeyLen,Key);
                  end;
{$ENDIF MD5}
{$IFDEF RIPEMD160}
     kdfWPv2RipeMD160:
                  begin
                    F := Struct.FindField('kdfParameter')^;
                    Params := F.ContentAsOctetString;
                    Result := Length(Params) > 0;
                    if Result then
                      WeakPasswordKDFv2(Password.Key^,Password.KeyLen,
                                        Pointer(Params)^,Length(Params),
                                        KDFIterations,
                                        haRipeMD160,
                                        KeyLen,Key);
                  end;
{$ENDIF RIPEMD160}
{$IFDEF SHA256}
     kdfWPv2SHA256:
                  begin
                    F := Struct.FindField('kdfParameter')^;
                    Params := F.ContentAsOctetString;
                    Result := Length(Params) > 0;
                    if Result then
                      WeakPasswordKDFv2(Password.Key^,Password.KeyLen,
                                        Pointer(Params)^,Length(Params),
                                        KDFIterations,
                                        haSHA256,
                                        KeyLen,Key);
                  end;
{$ENDIF SHA256}
{$IFDEF SHA512}
     kdfWPv2SHA384:
                  begin
                    F := Struct.FindField('kdfParameter')^;
                    Params := F.ContentAsOctetString;
                    Result := Length(Params) > 0;
                    if Result then
                      WeakPasswordKDFv2(Password.Key^,Password.KeyLen,
                                        Pointer(Params)^,Length(Params),
                                        KDFIterations,
                                        haSHA384,
                                        KeyLen,Key);
                  end;
     kdfWPv2SHA512:
                  begin
                    F := Struct.FindField('kdfParameter')^;
                    Params := F.ContentAsOctetString;
                    Result := Length(Params) > 0;
                    if Result then
                      WeakPasswordKDFv2(Password.Key^,Password.KeyLen,
                                        Pointer(Params)^,Length(Params),
                                        KDFIterations,
                                        haSHA512,
                                        KeyLen,Key);
                  end;
{$ENDIF SHA512}
       kdfWeakPassword,
       kdfWeakPassword256:
                        begin
                          F := Struct.FindField('kdfParameter')^;
                          Params := F.ContentAsOctetString;
                          Result := Length(Params) > 0;
                          if Result then
                            if KDF = kdfWeakPassword then
{$IFDEF SHA1}
                              WeakPasswordToPrivKey(Password.Key^,Password.KeyLen,
                                                    Pointer(Params)^,Length(Params),
                                                    KDFIterations,
                                                    KeyLen,
                                                    Key)
                            else
{$ENDIF SHA1}
{$IFDEF SHA256}
                              WeakPasswordToPrivKey256(Password.Key^,Password.KeyLen,
                                                       Pointer(Params)^,Length(Params),
                                                       KDFIterations,
                                                       KeyLen,
                                                       Key);
{$ENDIF SHA256}
                        end;
       kdfP1636_2_sha1,
       kdfP1636_2_sha256:
                        begin
                          if KDF = kdfP1636_2_sha1 then
{$IFDEF SHA1}
                            HA := haSHA1
{$ELSE  SHA1}
                            Exit
{$ENDIF SHA1}
                          else
{$IFDEF SHA256}
                            HA := haSHA256;
{$ELSE  SHA256}
                            Exit;
{$ENDIF SHA256}
                          F := Struct.FindField('kdfParameter')^;
                          Params := F.ContentAsOctetString;
                          Result := Length(Params) > 0;
                          if Result then begin
                            if KDFIterations > 1 then begin
                              KDF2(Password.Key^,Password.KeyLen,
                                   Pointer(Params)^,Length(Params),
                                   HA,
                                   KeyLen,
                                   Key);
                              for I := 1 to KDFIterations - 1 do
                                KDF2(Password.Key^,Password.KeyLen,
                                     Key.Key^,Key.KeyLen,
                                     HA,
                                     KeyLen,
                                     Key);
                            end else begin
                              KDF2(Password.Key^,Password.KeyLen,
                                   Pointer(Params)^,Length(Params),
                                   HA,
                                   KeyLen,
                                   Key);
                            end;
                          end;
                        end;
       kdfX9_42Concat_sha1,
       kdfX9_42Concat_sha256:
                        begin    
                          if KDF = kdfP1636_2_sha1 then
{$IFDEF SHA1}
                            HA := haSHA1
{$ELSE  SHA1}
                            Exit
{$ENDIF SHA1}
                          else
{$IFDEF SHA256}
                            HA := haSHA256;
{$ELSE  SHA256}
                            Exit;
{$ENDIF SHA256}
                          if KDFIterations > 1 then begin
                            BERAlgID := InterpretOIDToBER(EncryptionAlgorithmID);
                            SetLength(Params,Length(BERAlgID) + 4 + Length(SuppPublInfo) + KeyLen);
                            F := Struct.FindField('kdfParameter')^;
                            Result := F.Length = KeyLen;
                            if Result then begin
                              Move(F.Content^,Params[5 + Length(BERAlgID)],KeyLen);
                              Move(Pointer(BERAlgID)^,Pointer(Params)^,Length(BERAlgID));
                              FillChar(Params[1 + Length(BERAlgID)],4,0);
                              Move(Pointer(SuppPublInfo)^,Params[5 + Length(BERAlgID) + KeyLen],Length(SuppPublInfo));
                              Key := TSecretKey.Create('');
                              Key.SetLength(KeyLen);
                              KDFX9_42Concatenation(Password.Key^,Password.KeyLen,
                                                    Pointer(Params)^,Length(Params),
                                                    HA,
                                                    Key.Key^,KeyLen);
                              for I := 1 to KDFIterations - 1 do begin
                                Move(Key.Key^,Params[5 + Length(BERAlgID)],KeyLen);
                                KDFX9_42Concatenation(Password.Key^,Password.KeyLen,
                                                      Pointer(Params)^,Length(Params),
                                                      HA,
                                                      Key.Key^,KeyLen);
                              end;
                            end;
                          end else begin
                            BERAlgID := InterpretOIDToBER(EncryptionAlgorithmID);
                            SetLength(Params,Length(BERAlgID) + 4 + Length(SuppPublInfo) + KeyLen);
                            F := Struct.FindField('kdfParameter')^;
                            Result := F.Length = KeyLen;
                            if Result then begin
                              Move(F.Content^,Params[5 + Length(BERAlgID)],KeyLen);
                              Move(Pointer(BERAlgID)^,Pointer(Params)^,Length(BERAlgID));
                              FillChar(Params[1 + Length(BERAlgID)],4,0);
                              Move(Pointer(SuppPublInfo)^,Params[5 + Length(BERAlgID)],Length(SuppPublInfo));
                              Key := TSecretKey.Create('');
                              Key.SetLength(KeyLen);
                              KDFX9_42Concatenation(Password.Key^,Password.KeyLen,
                                                    Pointer(Params)^,Length(Params),
                                                    HA,
                                                    Key.Key^,KeyLen);
                            end;
                          end;
                        end;
       kdfX9_42ASN1DER_sha1,
       kdfX9_42ASN1DER_sha256:
                        begin
                          F := Struct.FindField('kdfParameter')^;
                          Result := F.Length = KeyLen;
                          if Result then begin
                            if KDF = kdfX9_42ASN1DER_sha1 then
{$IFDEF SHA1}
                              HA := haSHA1
{$ELSE  SHA1}
                              Exit
{$ENDIF SHA1}
                            else
{$IFDEF SHA256}
                              HA := haSHA256;
{$ELSE  SHA256}
                              Exit;
{$ENDIF SHA256}
                            BERAlgID := InterpretOIDToBER(EncryptionAlgorithmID);
                            if SuppPublInfo <> '' then
                              SetLength(Params,2 +
                                                 2 +
                                                   2 + Length(BERAlgID) +
                                                   2 + 4 +
                                                 2 +
                                                   2 + KeyLen +
                                                 2 +
                                                   2 + Length(SuppPublInfo))
                            else
                              SetLength(Params,2 +
                                                 2 +
                                                   2 + Length(BERAlgID) +
                                                   2 + 4 +
                                                 2 +
                                                   2 + KeyLen);
                            if Length(Params) > 129 then
                              raise Exception.Create('Too large paramater for key derivation function');
                            Params[1] := #$30;
                            Params[2] := Char(Byte(Length(Params) - 2));
                            Params[3] := #$30;
                            Params[4] := Char(Byte(Length(BERAlgID) + 8));
                            Params[5] := #$06;
                            Params[6] := Char(Byte(Length(BERAlgID)));
                            Move(Pointer(BERAlgID)^,Params[7],Length(BERAlgID));
                            Idx := 7 + Length(BERAlgID);
                            Params[Idx] := #$04;
                            Params[Idx + 1] := #$04;
                            FillChar(Params[Idx + 2],4,0);
                            Idx := Idx + 6;
                            Params[Idx] := #$A0;
                            Params[Idx + 1] := Char(Byte(KeyLen + 2));
                            Params[Idx + 2] := #$04;
                            Params[Idx + 3] := Char(Byte(KeyLen));
                            Move(F.Content^,Params[Idx + 4],KeyLen);
                            Idx := Idx + 4 + KeyLen;
                            if SuppPublInfo <> '' then begin
                              Params[Idx] := #$A2;
                              Params[Idx + 1] := Char(Byte(Length(SuppPublInfo) + 2));
                              Params[Idx + 2] := #$04;
                              Params[Idx + 3] := Char(Byte(Length(SuppPublInfo)));
                              Move(Pointer(SuppPublInfo)^,Params[Idx + 4],Length(SuppPublInfo));
                            end;
                            Key := TSecretKey.Create('');
                            Key.SetLength(KeyLen);
                            KDFX9_42ASN1DER(Password.Key^,Password.KeyLen,
                                            Pointer(Params)^,Length(Params),
                                            HA,
                                            Key.Key^,KeyLen);
                            if KDFIterations > 1 then begin
                              for I := 1 to KDFIterations - 1 do begin
                                Move(Key.Key^,Params[17 + Length(BERAlgID)],KeyLen);
                                KDFX9_42ASN1DER(Password.Key^,Password.KeyLen,
                                                Pointer(Params)^,Length(Params),
                                                HA,
                                                Key.Key^,KeyLen);
                              end;
                            end;
                          end;
                        end;
    end;
  finally
    ProtectClear(Pointer(Params)^,Length(Params));
  end;
end;
    
function _KDFVerification(const Password: string;
                          EncryptionAlgorithmID: string;
                          SuppPublInfo: string;
                          Struct: TASN1Struct;
                          var Key: string): Boolean; overload;
var
  PW: ISecretKey;
  K: ISecretKey;
begin
  PW := TSecretKey.Create('');
  PW.SetKey(Pointer(Password),Length(Password),0);
  Result := _KDFVerification(PW,EncryptionAlgorithmID,SuppPublInfo,Struct,Length(Key),K);
  if Result then
    Move(K.Key^,Pointer(Key)^,Length(Key));
end;

{ TStreamSecII }

{$IFDEF SHA1_AND_MD5}
procedure TStreamSecII.ClearTLSSessions;
var
  I: Integer;
  Client: TCustomTLS_ContentLayer;
begin
  try
    for I := TLSSessionCount - 1 downto 0 do begin
      Client := TLSSessions[I];
      Client.Release;
    end;
  finally
    FTLSSessionsUD.Clear;
    FTLSSessionsID.Clear;
  end;
  FreeMem(FTLS_RSACertificate);
  FTLS_RSACertificate := nil;
  FreeMem(FTLS_DSSCertificate);
  FTLS_DSSCertificate := nil;
  FreeMem(FTLS_DH_RSACertificate);
  FTLS_DH_RSACertificate := nil;
  FreeMem(FTLS_DH_DSSCertificate);
  FTLS_DH_DSSCertificate := nil;
  FreeMem(FTLS_ECDH_RSACertificate);
  FTLS_ECDH_RSACertificate := nil;
  FreeMem(FTLS_ECDH_ECDSACertificate);
  FTLS_ECDH_ECDSACertificate := nil;
  FRSAPriv := nil;
  FDSSPriv := nil;
  FDH_RSAPriv := nil;
  FDH_DSSPriv := nil;
  FECDH_RSAPriv := nil;
  FECDH_ECDSAPriv := nil;
  FreeMem(FDHCertReq);
  FreeMem(FECDHCertReq);
  FreeMem(FOtherCertReq);
  FDHCertReq := nil;
  FECDHCertReq := nil;
  FOtherCertReq := nil;
end;
{$ENDIF SHA1_AND_MD5}

constructor TStreamSecII.Create(AOwner: TComponent);
begin
{$IFDEF SHA1_AND_MD5}
  FTLSLock := TCriticalSection.Create;
{$ENDIF SHA1_AND_MD5}
  inherited;         
{$IFDEF SHA1_AND_MD5}
  FTLSSessionsID := TList.Create;
  FTLSSessionsUD := TList.Create;
  FTLSOptions := TTLSOptions.Create(Self);
{$ENDIF SHA1_AND_MD5}
  FCollectedCertificates := TX509TrustedCertificates.Create(Self);
  FCollectedCertificates.Name := Name + 'CollectedCerts';
end;

procedure TStreamSecII.DataLoad(Sender: TObject; Field: TASN1Struct);
var
  Result: TASN1Struct;
  F: PASN1Struct;
begin
  Result := Field.Owner.Owner;
  F := Result.FindField('contentType');
  if F^.ContentAsOID = id_digestedData then begin
    FCurrentDigData := Result;
    Result.AllowResume := True;
    F := Result.FindField('/content//encapContentInfo/eContent/');           
    F^.AllowResume := True;
    F^.CaptureContent := True;
    F^.OnCaptureLoad := DigDataCaptureLoad;
    FCurrentDigDataCount := 0;
    FCurrentAct := eDigest;
  end else if F^.ContentAsOID = id_encryptedData then begin
    FCurrentEncData := Result;
    Result.AllowResume := True;
    F := Result.FindField('/content//encryptedContentInfo/encryptedContent');
    F^.AllowResume := True;
    F^.CaptureContent := True;
    F^.OnCaptureLoad := EncDataCaptureLoad;
    FCurrentEncDataCount := 0;
    FCurrentAct := eEncrypt;
  end;
  Field.OnLoad := nil;
end;

function TStreamSecII.DecodeStream(Src, Dst: TStream;
  UseLongTermKey: Boolean; var Action: TEncodeType;
  KeyIndex: Integer): Boolean;
var
  F: TASN1Struct;
begin
  InternalNewData;
  try
    if UseLongTermKey then
      F := LongTermKeys[KeyIndex]
    else
      F := SessionKeys[KeyIndex].FindField('contentEncryptionKey')^;
    if F = nil then
      raise Exception.Create('Key not found');
    FCurrentKey := F.FindField('/privateKey/')^;
    FCurrentDst := Dst;
    FCurrentSrc := Src;
    FCurrentEnc := False;
    FCurrentData.LoadFromStream(Src,fmtDER);
    Action := FCurrentAct;
    if Action in [eDigest,eEncryptThenDigest] then
      FVerified := FVerified and
                   (CompareStr(FCurrentDigest,FCurrentDigData.FindField('/content//digest')^.ContentAsOctetString) = 0);
  finally
    FCurrentData.Free;
    FCurrentData := nil;
    FCurrentDigData := nil;
    FCurrentEncData := nil;
  end;
  Result := FVerified;
end;

destructor TStreamSecII.Destroy;
var
  Debug: string;
begin
  try                              
{$IFDEF SHA1_AND_MD5}
    Debug := 'StopEphemeralKeyGen';
    StopEphemeralKeyGen;
    Debug := 'FTLSLock.Free';
    FTLSLock.Free;
    Debug := 'ClearTLSSessions';
    ClearTLSSessions;
    Debug := 'FTLSSessionsID.Free';
    FTLSSessionsID.Free;
    Debug := 'FTLSSessionsUD.Free';
    FTLSSessionsUD.Free;
    Debug := 'FTLSOptions.Free';
    FTLSOptions.Free;   
{$ENDIF SHA1_AND_MD5}
    Debug := 'inherited';
    inherited;
  except
    on E: Exception do
      raise Exception.Create('TStreamSecII.Destroy: ' + Debug + #13#10 +
                             E.Message);
  end;
end;

procedure TStreamSecII.DHKeyGenDone(Sender: TObject;
  const APriv: TDLPrivateKey; const APubl: TDLPublicKey);
var
  PKI: string;
begin
  PKI := Format('DH key %d %d',[MPMSB(APubl.Params.P),MPMSB(APubl.Params.Q)]);
  KeyLock(True);
  try
    AddPrivateDLKey(APriv,Pointer(PKI),Length(PKI),False,False);
  finally
    KeyUnlock(True);
  end;
end;

procedure TStreamSecII.DigDataCaptureLoad(Sender: TObject;
  Field: TASN1Struct; Stream: TStream; Length: Integer; var Count: Integer;
  var Done: Boolean);
var
  OID: string;
  HA: THashAlgorithm;
  HC: THashClass;
  DS: TDigestStream;
  dCount: Integer;
begin
  OID := FCurrentDigData.FindField('/content//encapContentInfo/eContentType').ContentAsOID;
  if OID = id_EncryptedData then
    FCurrentAct := eEncryptThenDigest;

  OID := FCurrentDigData.FindField('/content//digestAlgorithm/algorithm')^.ContentAsOID;
  if not OIDToHashAlgorithm(OID,HA) then
    Abort;
  HC := FindHashClass(HA);
  if Assigned(HC) then
    FCurrentHash := HC.Create(Pointer(nil)^,0)
  else
    Abort;
  try
    DS := TDigestStream.Create(Stream,FCurrentHash);
    try
      if FCurrentAct in [eDigest,eDigestThenEncrypt] then begin
        repeat
          if Count + SizeOf(FInternalBuffer) <= Field.Length then
            dCount := DS.Read(FInternalBuffer,SizeOf(FInternalBuffer))
         else
            dCount := DS.Read(FInternalBuffer,Field.Length - Count);
          if dCount = 0 then Break;
          Count := Count + dCount;
          Done := Count = Field.Length;
          FCurrentDst.Write(FInternalBuffer,dCount);
        until Done;
        FVerified := Done;
      end else begin
        InternalNewEncryptedData;
        try
          Count := FCurrentEncData.FindField('/content/')^.LoadFromStream(DS,fmtDER);
          Done := FCurrentEncData.FindField('/content/')^.DoneLoading;
        finally
          FCurrentEncData.Free;
          FCurrentEncData := nil;
        end;
      end;
      if Done then begin
        SetLength(FCurrentDigest,FCurrentHash.DigestSize);
        FCurrentHash.Done(Pointer(FCurrentDigest));
      end;
    finally
      DS.Free;
    end;
  finally
    FCurrentHash.Free;
    FCurrentHash := nil;
  end;
end;

procedure TStreamSecII.DigDataCaptureSave(Sender: TObject;
  Field: TASN1Struct; Stream: TStream; Length: Integer; var Count: Integer;
  var Done: Boolean);
var
  DS: TDigestStream;
  dCount: Integer;
  F: TASN1Struct;
begin
  DS := TDigestStream.Create(Stream,FCurrentHash);
  try
    if FCurrentAct in [eDigest,eDigestThenEncrypt] then begin
      repeat
        dCount := FCurrentSrc.Read(FInternalBuffer,SizeOf(FInternalBuffer));
        Done := dCount = 0;
        Count := Count + dCount;
        if not Done then
          DS.Write(FInternalBuffer,dCount);
      until Done;
    end else begin
      Count := FCurrentEncData.FindField('/content/')^.SaveToStream(DS,fmtDER);
      Done := FCurrentEncData.FindField('/content/')^.DoneSaving;
    end;
    if Done then begin
      F := FCurrentDigData.FindField('/content//digest')^;
      FCurrentHash.Done(F.Content);
    end;
  finally
    DS.Free;
  end;
end;
                                            
{$IFDEF SHA1_AND_MD5}
procedure TStreamSecII.DoTLSChangeCipherSpec(Sender: TObject);
var
  Client: TCustomTLS_ContentLayer;
begin
  Client := TCustomTLS_ContentLayer(Sender);
  TLSLock;
  try
    AddTLSMasterSecret(Client.SessionID,Client.Context^.master_secret);

    if not Client.Unmanaged then begin
      FTLSSessionsID.Add(Client);
      FTLSSessionsID.Sort(TLSSessionsSortCompare);
    end;
  finally
    TLSUnlock;
  end;
  if Assigned(FOnTLSChangeCipherSpec) then
    FOnTLSChangeCipherSpec(Self,Client);
end;

procedure TStreamSecII.DoTLSCompress(Sender: TObject; Src: PTLSPlainText;
  PTLen: Integer; Method: Byte; var Dst: PTLSCompressed; var CLen,
  Error: Integer);
begin
  if Assigned(FOnCompress) then
    FOnCompress(Self,TCustomTLS_ContentLayer(Sender),Src,PTLen,Method,Dst,CLen,Error);
end;

procedure TStreamSecII.DoTLSDecompress(Sender: TObject;
  Src: PTLSCompressed; CLen: Integer; Method: Byte; var Dst: PTLSPlainText;
  var PTLen, Error: Integer);
begin
  if Assigned(FOnDecompress) then
    FOnDecompress(Self,TCustomTLS_ContentLayer(Sender),Src,CLen,Method,Dst,PTLen,Error);
end;

procedure TStreamSecII.DoTLSDestroy(Sender: TObject);
begin
  TLSLock;
  try
    FTLSSessionsID.Remove(Sender);
    FTLSSessionsUD.Remove(Sender);
  finally
    TLSUnlock;
  end;
end;

procedure TStreamSecII.DoTLSIncomingAlert(Sender: TObject;
  var Fatal: Boolean; AlertCode: Integer);
begin
  if Assigned(FOnIncomingAlert) then
    FOnIncomingAlert(Self,TCustomTLS_ContentLayer(Sender),Fatal,AlertCode);
end;

procedure TStreamSecII.DoTLSOutgoingAlert(Sender: TObject;
  var Fatal: Boolean; AlertCode: Integer);
begin
  if Assigned(FOnOutgoingAlert) then
    FOnOutgoingAlert(Self,TCustomTLS_ContentLayer(Sender),Fatal,AlertCode);
end;

procedure TStreamSecII.DoTLSRenegotiate(Sender: TObject;
  const SessionID: TSessionID; var MasterSecret: ShortString);
var
  Allowed: Boolean;
  MastSec: TMasterSecret;
  Idx: Integer;
begin
  MasterSecret := '';
  if FindTLSMasterSecret(SessionID,MastSec,Idx) then begin
    Allowed := True;
    if Assigned(FOnRenegotiate) then
      FOnRenegotiate(Self,TCustomTLS_ContentLayer(Sender),SessionID,Allowed);
    if Allowed then begin
      SetLength(MasterSecret,48);
      Move(MastSec,MasterSecret[1],48);
    end;
    ProtectClear(MastSec,48);
  end;
end;

procedure TStreamSecII.DoTLSSelectCompression(Sender: TObject;
  const CompMethods: array of TCompressionMethod;
  var CompMethod: TCompressionMethod);
begin
  if Assigned(FOnSelectCompression) then
    FOnSelectCompression(Self,TCustomTLS_ContentLayer(Sender),CompMethods,CompMethod);
end;
{$ENDIF SHA1_AND_MD5}

procedure TStreamSecII.EncDataCaptureLoad(Sender: TObject;
  Field: TASN1Struct; Stream: TStream; Length: Integer; var Count: Integer;
  var Done: Boolean);
var
  OID, IVector: string;
  CipherClass: TCipherClass;
  KeySize: Integer;
  KeyedIV: Boolean;
  K: TASN1Struct;
  DS: TDecryptStream;
  dCount, Pad: Integer;
begin
  OID := FCurrentEncData.FindField('/content//encryptedContentInfo/contentType')^.ContentAsOID;
  if OID = id_digestedData then
    FCurrentAct := eDigestThenEncrypt;

  OID := FCurrentEncData.FindField('/content//encryptedContentInfo/contentEncryptionAlgorithm/algorithm')^.ContentAsOID;
  IVector := FCurrentEncData.FindField('/content//encryptedContentInfo/contentEncryptionAlgorithm/parameters')^.ContentAsOctetString;
  CipherClass := nil;
  if not OIDToCipherClass(OID,KeyedIV,KeySize,CipherClass) then
    Abort;           
  K := FCurrentKey;
  if K.ChoiceTypeName = 'SymmetricKeyInfo' then
    K := K.FindField('keyData')^;
  if K.Length < KeySize then
    Abort;
  FCurrentCipher := CreateCipher(K.Content^,KeySize,CipherClass,KeyedIV,IVector);
  try
    if FCurrentCipher = nil then
      Abort;

    DS := TDecryptStream.Create(Stream,FCurrentCipher);
    try
      if FCurrentAct in [eEncrypt,eEncryptThenDigest] then begin
        repeat
          if Count + SizeOf(FInternalBuffer) <= Field.Length then
            dCount := DS.Read(FInternalBuffer,SizeOf(FInternalBuffer))
          else
            dCount := DS.Read(FInternalBuffer,Field.Length - Count);
          Count := Count + dCount;
          Done := Count = Field.Length;
          if Done then begin
            Pad := FInternalBuffer[dCount-1];
            if Pad <= FCurrentCipher.BlockSize then begin
              Move(FInternalBuffer[dCount - Pad],DS.FDecBuffer,Pad);
              DS.FDecLen := Pad;
              DS.Done;
              if DS.OK then
                dCount := dCount - Pad;
            end;
          end;
          FCurrentDst.Write(FInternalBuffer,dCount);
        until Done or (dCount = 0);
        FVerified := Done;
        if Done then
          Count := Field.Length;
      end else begin
        InternalNewDigestedData;
        try
          try
            Count := FCurrentDigData.FindField('/content/')^.LoadFromStream(DS,fmtDER);
            Done := FCurrentDigData.FindField('/content/')^.DoneLoading;
            if Done then
              FVerified := FVerified and
                           (CompareStr(FCurrentDigest,
                                       FCurrentDigData.FindField('/content//digest')^.ContentAsOctetString) = 0);
          except
            FVerified := False;
          end;
        finally
          FCurrentDigData.Free;
          FCurrentDigData := nil;
        end;
        DS.Done;
        FVerified := FVerified and DS.OK;
        if FVerified then
          Count := Count + 16 - (Count and $F);
      end;
    finally
      DS.Free;
    end;
  finally
    FCurrentCipher.Free;
    FCurrentCipher := nil;
  end;
end;

procedure TStreamSecII.EncDataCaptureSave(Sender: TObject;
  Field: TASN1Struct; Stream: TStream; Length: Integer; var Count: Integer;
  var Done: Boolean);
var
  ES: TEncryptStream;
  dCount: Integer;
begin
  ES := TEncryptStream.Create(Stream,FCurrentCipher);
  try
    if FCurrentAct in [eEncrypt,eEncryptThenDigest] then begin
      repeat
        dCount := FCurrentSrc.Read(FInternalBuffer,SizeOf(FInternalBuffer));
        if dCount = 0 then Break;
        Count := ES.Write(FInternalBuffer,dCount) + Count;
      until dCount = 0;
      Count := ES.Done + Count;
      Done := Field.Length <= Count;
    end else begin
      Count := FCurrentDigData.FindField('/content/')^.SaveToStream(ES,fmtDER);
      Count := ES.Done + Count;
      Done := (Field.Length <= Count) and
              FCurrentDigData.FindField('/content/')^.DoneSaving;
    end;
  finally
    ES.Free;
  end;
end;

procedure TStreamSecII.EncodeStream(Src, Dst: TStream; Action: TEncodeType;
  UseLongTermKey: Boolean; var KeyIndex: Integer; IVector: string);
var
  F, A: TASN1Struct;
  CipherClass: TCipherClass;
  C: TCipher;
  HC: THashClass;
  H: THash;
  Key: array [0..63] of Byte;
  KeySize: Integer;
  KeyedIV: Boolean;
  OID: string;
begin
  try
    FCurrentAct := Action;
    FCurrentDst := Dst;
    FCurrentSrc := Src;
    FCurrentEnc := True;
    if Action <> eDigest then begin
      CipherAlgorithmToCipherClass(DefaultCipherAlgorithm,KeyedIV,KeySize,CipherClass);
      if KeyIndex < 0 then begin
        RawRandom(Key,KeySize*8);
        if CipherClass.Algorithm = caDES then begin
          DESKeyParity(Key,KeySize);
          KeyIndex := AddPrivate3DESKey(Key,KeySize,UseLongTermKey);
        end else if DefaultCipherAlgorithm = caARC4  then
          KeyIndex := AddPrivateARC4Key(Key,KeySize,UseLongTermKey)
        else if CipherClass.Algorithm = caRijndael then
          KeyIndex := AddPrivateAESKey(Key,KeySize,UseLongTermKey)
        else if CipherClass.Algorithm = caTwoFish then
          KeyIndex := AddPrivateTwoFishKey(Key,KeySize,UseLongTermKey)
        else
          Abort;
        if (IVector = '') and CipherClass.InheritsFrom(TBlockCipher) {and not KeyedIV} then begin
          SetLength(IVector,CipherClass.BlockSize);
          RawRandom(Pointer(IVector)^,Length(IVector)*8);
        end;
        C := CreateCipher(Key,KeySize,CipherClass,KeyedIV,IVector);
      end else begin
        if UseLongTermKey then
          F := LongTermKeys[KeyIndex]
        else
          F := SessionKeys[KeyIndex].FindField('contentEncryptionKey')^;
        if F = nil then
          raise Exception.Create('Key not found');
        A := F.FindField('privateKeyType')^;
        if A.ContentAsOID = tripleDES then begin
          if CipherClass.Algorithm <> caDES then
            Abort;
        end else if A.ContentAsOID = arcfour then begin
          if CipherClass.Algorithm <> SecUtils.caARC4 then
            Abort;
        end else if A.ContentAsOID = id_aes then begin
          if CipherClass.Algorithm <> caRijndael then
            Abort;
        end else if A.ContentAsOID = id_twofish then begin
          if CipherClass.Algorithm <> caTwoFish then
            Abort;
        end else
          raise Exception.Create('Not a symmetric encryption key');
        F := F.FindField('/privateKey/')^;
        KeySize := F.Length;
        if (IVector = '') and CipherClass.InheritsFrom(TBlockCipher) {and not KeyedIV} then begin
          SetLength(IVector,CipherClass.BlockSize);
          RawRandom(Pointer(IVector)^,Length(IVector)*8);
        end;
        C := CreateCipher(F.Content^,KeySize,CipherClass,KeyedIV,IVector);
      end;
      FCurrentCipher := C;
    end;
    if Action <> eEncrypt then begin
      HC := FindHashClass(DefaultHashAlgorithm);
      if Assigned(HC) then
        H := HC.Create(Pointer(nil)^,0)
      else
        raise Exception.Create('TStreamSecII.EncodeStream: Unsupported hash algorithm');
      FCurrentHash := H;
    end;

    if Action in [eDigest,eEncryptThenDigest] then begin
      if Action <> eDigest then begin
        CipherClassToOID(KeySize,CipherClass,KeyedIV,OID);
        InternalNewEncryptedData(OID,IVector);
      end;
      InternalNewDigestedData;
      FCurrentDigData.SaveToStream(Dst,fmtDER)
    end else if Action in [eEncrypt,eDigestThenEncrypt] then begin
      if Action <> eEncrypt then
        InternalNewDigestedData;
      CipherClassToOID(KeySize,CipherClass,KeyedIV,OID);
      InternalNewEncryptedData(OID,IVector);
      FCurrentEncData.SaveToStream(Dst,fmtDER);
    end;
  finally
    FCurrentEncData.Free;
    FCurrentEncData := nil;
    FCurrentCipher.Free;
    FCurrentCipher := nil;
    FCurrentDigData.Free;
    FCurrentDigData := nil;
    FCurrentHash.Free;
    FCurrentHash := nil;
    ProtectClear(Key,64);
  end;
end;
                
{$IFDEF SHA1_AND_MD5}
function TStreamSecII.FindTLSSession(const SessionID: TSessionID;
  var Client: TCustomTLS_ContentLayer; Extract: Boolean): Integer;
var
  Dummy: Integer;
begin
  TLSLock;
  try
    Client := TLSSessionsIDFind(SessionID,Dummy);
    if Assigned(Client) then
      TLSSessionsUDFind(Client.UserData,Result)
    else
      Result := -1;
    if Assigned(Client) and Extract then begin
      FTLSSessionsID.Remove(Client);
      FTLSSessionsUD.Remove(Client);  
      Client.Unmanaged := True;  
      Client.OnDestroy := nil;
    end;
  finally
    TLSUnlock;
  end;
end;   
{$ENDIF SHA1_AND_MD5}

function TStreamSecII.GetSequenceNumber: Integer;
begin
  Result := FPrivateKeyRing.SequenceNumber;
end;
                                      
{$IFDEF SHA1_AND_MD5}
function TStreamSecII.GetTLSSessionCount: Integer;
begin
  Result := FTLSSessionsUD.Count;
end;

function TStreamSecII.GetTLSSessions(index: Integer): TCustomTLS_ContentLayer;
begin
  Result := FTLSSessionsUD[index];
end;   
{$ENDIF SHA1_AND_MD5}

function TStreamSecII.InternalNewData: TASN1Struct;
var
  F: PASN1Struct;
begin
  Result := nil;
  NewDataStruct(Result);
  FCurrentData := Result;
  Result.AllowResume := True;
  F := Result.FindField('/content/');
  F.OnLoad := DataLoad;
end;

function TStreamSecII.InternalNewDigestedData: TASN1Struct;
var
  F, D: PASN1Struct;
begin
  Result := nil;
  NewDigestedDataStruct(Result);
  FCurrentDigData := Result;
  Result.AllowResume := True;
  F := Result.FindField('/content//encapContentInfo/eContent/');
  F^.CaptureContent := True;
  if FCurrentEnc then begin
    F^.OnCaptureSave := DigDataCaptureSave;
    Result.EditField('/content//digestAlgorithm/algorithm',
                     HashAlgorithmToOID(DefaultHashAlgorithm));
    if (FCurrentAct in [eDigest,eDigestThenEncrypt]) then begin
      Result.EditField('/content//version',Integer(0));
      Result.EditField('/content//encapContentInfo/eContentType',id_data);
      F^.Length := FCurrentSrc.Size - FCurrentSrc.Position
    end else begin
      Result.EditField('/content//version',Integer(2));
      Result.EditField('/content//encapContentInfo/eContentType',id_encryptedData);
      FCurrentEncData.CalculateLength;
      D := FCurrentEncData.FindField('content');
      F^.Length := D^.Length;
    end;
    F := Result.FindField('/content//digest');
    F^.Length := FCurrentHash.DigestSize;
    Result.CalculateLength;
  end else begin
    F^.OnCaptureLoad := DigDataCaptureLoad;
  end;
  FCurrentDigDataCount := 0;
end;

function TStreamSecII.InternalNewEncryptedData: TASN1Struct;
var
  F: PASN1Struct;
begin
  Result := nil;
  NewEncryptedDataStruct(Result);
  FCurrentEncData := Result;
  Result.AllowResume := True;
  F := Result.FindField('/content//encryptedContentInfo/encryptedContent');
  F^.CaptureContent := True;
  F^.OnCaptureLoad := EncDataCaptureLoad;
  FCurrentEncDataCount := 0;
end;

function TStreamSecII.InternalNewEncryptedData(OID: string;
  IV: string): TASN1Struct;
var
  F, D: PASN1Struct;
  BS: Integer;
begin
  Result := nil;
  NewEncryptedDataStruct(Result);
  FCurrentEncData := Result;
  Result.AllowResume := True;
  F := Result.FindField('/content//encryptedContentInfo/encryptedContent');
  F^.CaptureContent := True;
  F^.OnCaptureSave := EncDataCaptureSave;
  if (FCurrentAct in [eEncrypt,eEncryptThenDigest]) then begin
    Result.EditField('/content//encryptedContentInfo/contentType',id_data);
    F^.Length := FCurrentSrc.Size;
  end else begin
    Result.EditField('/content//encryptedContentInfo/contentType',id_digestedData);
    FCurrentDigData.CalculateLength;
    D := FCurrentDigData.FindField('content');
    F^.Length := D^.Length;
  end;
  if FCurrentCipher is TBlockCipher then begin
    BS := FCurrentCipher.BlockSize;
    FCurrentPadLen := BS - (F^.Length mod BS);
    F^.Length := F^.Length + FCurrentPadLen;
  end else
    FCurrentPadLen := 0;
  Result.EditField('/content//encryptedContentInfo/contentEncryptionAlgorithm/algorithm',
                   OID);
  if IV <> '' then begin
    F := Result.FindField('/content//encryptedContentInfo/contentEncryptionAlgorithm/parameters');
    F.Tag := V_ASN1_OCTET_STRING;
    F.Constructed := False;
    F.SetContent(Pointer(IV)^,Length(IV));
  end;
  Result.CalculateLength;
  FCurrentEncDataCount := 0;
end;

procedure TStreamSecII.Loaded;
begin
  inherited;    
{$IFDEF SHA1_AND_MD5}
  FTLSOptions.RefreshCipherSuites;
{$ENDIF SHA1_AND_MD5}
  if FCollectedCertificates.Owner = Self then
    FCollectedCertificates.Name := Name + 'CollectedCerts';
end;

procedure TStreamSecII.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FTrustedCertificates then
      FTrustedCertificates := nil;
    if AComponent = FCollectedCertificates then
      FCollectedCertificates := nil;
    if AComponent = FMyCertificates then
      FMyCertificates := nil;
    if AComponent = FCACertificates then
      FCACertificates := nil;
  end;
end;

procedure TStreamSecII.RSAKeyGenDone(Sender: TObject;
  const APriv: TIFPrivateKey; const APubl: TIFPublicKey);
var
  PKI: string;
begin
  PKI := Format('RSA key %d',[MPMSB(APubl.N)]);
  KeyLock(True);
  try
    AddPrivateRSAKey(APriv,Pointer(PKI),Length(PKI),False);
  finally
    KeyUnlock(True);
  end;
end;

procedure TStreamSecII.SetCACertificates(const Value: TX509TrustedCertificates);
begin
  if (FCACertificates = FCollectedCertificates.TrustedCACertificates) then
    FCollectedCertificates.TrustedCACertificates := nil;
  FCACertificates := Value;
  if Assigned(Value) then begin
    FCollectedCertificates.TrustedCACertificates := Value;
    FreeNotification(Value);
  end else if Assigned(FCollectedCertificates.TrustedCACertificates) then
    SetCACertificates(FCollectedCertificates.TrustedCACertificates);
end;

procedure TStreamSecII.SetCollectedCertificates(
  const Value: TX509TrustedCertificates);
begin
  if Value = FCollectedCertificates then
    Exit;
  if Assigned(FCollectedCertificates) and (FCollectedCertificates.Owner = Self) then
    FCollectedCertificates.Free;
  FCollectedCertificates := Value;
  if Value = nil then begin
    FCollectedCertificates := TX509TrustedCertificates.Create(Self);
    FCollectedCertificates.TrustedCACertificates := FCACertificates;
  end else begin
    FreeNotification(Value);
    if FCollectedCertificates.TrustedCACertificates = nil then
      FCollectedCertificates.TrustedCACertificates := FCACertificates
    else
      CACertificates := FCollectedCertificates.TrustedCACertificates;
  end;
  SetTrustedCertificates(FTrustedCertificates);
end;

procedure TStreamSecII.SetDefaultCipherAlgorithm(
  const Value: TCipherAlgorithm);
begin
  FDefaultCipherAlgorithm := Value;
end;

procedure TStreamSecII.SetDefaultKeyAgreement(const Value: TKeyAgreement);
begin
  FDefaultKeyAgreement := Value;
end;

procedure TStreamSecII.SetMyCertificates(const Value: TX509TrustedCertificates);
begin
  FMyCertificates := Value;
  if Assigned(Value) then
    FreeNotification(Value);
end;
                
{$IFDEF SHA1_AND_MD5}
procedure TStreamSecII.SetOnIncomingAlert(const Value: TTLSAlertEvent);
begin
  FOnIncomingAlert := Value;
end;

procedure TStreamSecII.SetOnOutgoingAlert(const Value: TTLSAlertEvent);
begin
  FOnOutgoingAlert := Value;
end;

procedure TStreamSecII.SetOnRenegotiate(const Value: TTLSRenegotiateEvent);
begin
  FOnRenegotiate := Value;
end;

procedure TStreamSecII.SetOnSelectCompression(
  const Value: TTLSSelectCompressionEvent);
begin
  FOnSelectCompression := Value;
end;

procedure TStreamSecII.SetOnTLSChangeCipherSpec(
  const Value: TTLSChangeCipherSpec);
begin
  FOnTLSChangeCipherSpec := Value;
end;   
{$ENDIF SHA1_AND_MD5}

procedure TStreamSecII.SetSequenceNumber(const Value: Integer);
begin
  FPrivateKeyRing.SequenceNumber := Value;
end;
                                    
{$IFDEF SHA1_AND_MD5}
procedure TStreamSecII.SetTLSOptions(const Value: TTLSOptions);
begin
  if Assigned(Value) then
    FTLSOptions.Assign(Value);
end;   
{$ENDIF SHA1_AND_MD5}

procedure TStreamSecII.SetTrustedCertificates(
  const Value: TX509TrustedCertificates);
var
  I: Integer;
  Status: TCertStatusCode;
begin
  FTrustedCertificates := Value;
  if Assigned(Value) then begin
    FreeNotification(Value);
    HoursOffsetFromGMT := Value.HoursOffsetFromGMT;
  end;
  if Assigned(FTrustedCertificates) then
    for I := 0 to FTrustedCertificates.Count - 1 do
      CollectedCertificates.AddCertificate(FTrustedCertificates.Certs[I],True,Status);
end;
                  
{$IFDEF SHA1_AND_MD5}
procedure TStreamSecII.StartEphemeralKeyGen(const RSABitSizes, DHBitSizes,
  DHQBitSizes: array of Integer; RunOnce: Boolean);
var
  T: TKeyGenThread;
  I: Integer;
begin
  if FEphemeralKeyGenThreads = nil then
    FEphemeralKeyGenThreads := TList.Create;

  Assert(Length(DHBitSizes) = Length(DHQBitSizes));

  for I := Low(RSABitSizes) to High(RSABitSizes) do begin
    T := TRSAKeyGen.Create(True);
    {$IFDEF MSWINDOWS}
    T.Priority := tpIdle;
    {$ENDIF}
    T.FRunOnce := RunOnce;
    T.SleepTime := Trunc(0.95 * SessionKeyLifeSpan * 24 * 3600 * 1000);
    TRSAKeyGen(T).OnKeyDone := RSAKeyGenDone;
    TRSAKeyGen(T).KeyBitSize := RSABitSizes[I];
    FEphemeralKeyGenThreads.Add(T);  
    T.FreeOnTerminate := True;
    T.Resume;
  end;

  for I := Low(DHBitSizes) to High(DHBitSizes) do begin
    T := TDHKeyGen.Create(True);
    {$IFDEF MSWINDOWS}
    T.Priority := tpIdle;
    {$ENDIF}
    T.FRunOnce := RunOnce;
    T.SleepTime := Trunc(0.95 * SessionKeyLifeSpan * 24 * 3600 * 1000);
    TDHKeyGen(T).OnKeyDone := DHKeyGenDone;
    TDHKeyGen(T).KeyBitSize := DHBitSizes[I];
    TDHKeyGen(T).QBitSize := DHQBitSizes[I];
    FEphemeralKeyGenThreads.Add(T);
    T.FreeOnTerminate := True;
    T.Resume;
  end;
end;

procedure TStreamSecII.StopEphemeralKeyGen;
var
  I: Integer;
  T: TThread;
begin
  if FEphemeralKeyGenThreads = nil then
    Exit;

  for I := FEphemeralKeyGenThreads.Count - 1 downto 0 do begin
    T := FEphemeralKeyGenThreads[I];
    if Assigned(T) then try
      if not T.Suspended then
        T.Suspend;
      {$IFDEF MSWINDOWS}
      T.Priority := tpHigher;
      {$ENDIF}
      if T is TDHKeyGen then
        TDHKeyGen(T).OnKeyDone := nil
      else if T is TRSAKeyGen then
        TRSAKeyGen(T).OnKeyDone := nil;
      T.Terminate;
      T.Resume;
    except
      T.Free;
    end;
  end;
  FEphemeralKeyGenThreads.Free;
  FEphemeralKeyGenThreads := nil;
end;

function TStreamSecII.TLSAccept(UserData: Pointer; Src,
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  if FindTLSSession(UserData,Client) < 0 then begin
    try
      Client := TLSAddServerSession(UserData);
    except
      on E: Exception do
        raise;
    end;
  end;
  try
    Result := Client.Accept(Src,Response);
  finally
    if Assigned(ErrorCode) then
      ErrorCode^ := Client.LastAlertCode;
    if not Client.Active then
      Client.Release;
  end;
end;

function TStreamSecII.TLSAddClientSession: TCustomTLS_ContentLayer;
var
  CMs: TCompressionMethods;
begin
  Result := TTLS_ContentLayer.Create(False);
  TLSLock;
  try
    FTLSSessionsUD.Add(Result);
  finally
    TLSUnlock;
  end;
  TTLS_ContentLayer(Result).PrivateKeyRing := Self;
  Result.CipherPreference := FTLSOptions.CipherSuites;
  Result.DHEphemeralKeySize := FTLSOptions.EphemeralDHKeySize;
  Result.DHEphemeralQSize := FTLSOptions.EphemeralDHQSize;
  case FTLSOptions.EphemeralECDHKeySize of
    ecsAuto: Result.ECDHEphemeralKeySize := 0;
    ecs192:  Result.ECDHEphemeralKeySize := 192;
    ecs224:  Result.ECDHEphemeralKeySize := 224;
    ecs256:  Result.ECDHEphemeralKeySize := 256;
    ecs384:  Result.ECDHEphemeralKeySize := 384;
    ecs521:  Result.ECDHEphemeralKeySize := 521;
  end;
  Result.RequestClientAuth := FTLSOptions.RequestClientCertificate;
  Result.RequireClientAuth := FTLSOptions.RequireClientCertificate;
  Result.CACerts := CACertificates;
  Result.CollectedCerts := CollectedCertificates;
  Result.MyCerts := MyCertificates;

  SetLength(CMs,1);
  CMs[0] := 0;
  Result.CompressionPreference := CMs;
  Result.OnCompress := DoTLSCompress;
  Result.OnDecompress := DoTLSDecompress;
  Result.OnIncomingAlert := DoTLSIncomingAlert;
  Result.OnOutgoingAlert := DoTLSOutgoingAlert;
  Result.OnRenegotiate := DoTLSRenegotiate;
  Result.OnSelectCompression := DoTLSSelectCompression;
  Result.OnDestroy := DoTLSDestroy;
  Result.OnChangeCipherSpec := DoTLSChangeCipherSpec;
  Result.OnGetCertificate := DoTLSGetCertificate;
end;

function TStreamSecII.TLSAddServerSession(UserData: Pointer): TCustomTLS_ContentLayer;
var
  CMs: TCompressionMethods;
begin                                      
  TLSLock;
  try
    Result := TTLS_ContentLayer.Create(True);
    Result.UserData := UserData;
    FTLSSessionsUD.Add(Result);
    FTLSSessionsUD.Sort(TLSSessionsUDSortCompare);
  finally
    TLSUnlock;
  end;
  TTLS_ContentLayer(Result).PrivateKeyRing := Self;
  Result.AcceptHelloRequest := FTLSOptions.AllowDualSockets;
  if Result.AcceptHelloRequest then
    Result.CipherPreference := FTLSOptions.CipherSuites;
  Result.AcceptedCiphers := FTLSOptions.CipherSuites;
  Result.DHEphemeralKeySize := FTLSOptions.EphemeralDHKeySize;
  Result.DHEphemeralQSize := FTLSOptions.EphemeralDHQSize;
  case FTLSOptions.EphemeralECDHKeySize of
    ecsAuto: Result.ECDHEphemeralKeySize := 0;
    ecs192:  Result.ECDHEphemeralKeySize := 192;
    ecs224:  Result.ECDHEphemeralKeySize := 224;
    ecs256:  Result.ECDHEphemeralKeySize := 256;
    ecs384:  Result.ECDHEphemeralKeySize := 384;
    ecs521:  Result.ECDHEphemeralKeySize := 521;
  end;
  Result.RequestClientAuth := FTLSOptions.RequestClientCertificate;
  Result.RequireClientAuth := FTLSOptions.RequireClientCertificate;
  Result.CACerts := CACertificates;
  Result.CollectedCerts := CollectedCertificates;
  Result.MyCerts := MyCertificates;

  SetLength(CMs,1);
  CMs[0] := 0;
  Result.CompressionPreference := CMs;
  Result.OnCompress := DoTLSCompress;
  Result.OnDecompress := DoTLSDecompress;
  Result.OnIncomingAlert := DoTLSIncomingAlert;
  Result.OnOutgoingAlert := DoTLSOutgoingAlert;
  Result.OnRenegotiate := DoTLSRenegotiate;
  Result.OnSelectCompression := DoTLSSelectCompression;
  Result.OnDestroy := DoTLSDestroy;
  Result.OnChangeCipherSpec := DoTLSChangeCipherSpec;
  Result.OnGetCertificate := DoTLSGetCertificate;
  Result.OnGetCertReq := DoTLSGetCertReq;
end;

function TStreamSecII.TLSClose(const SessionID: TSessionID;
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  Result := 0;
  if FindTLSSession(SessionID,Client) >= 0 then begin
    if Assigned(Response) then
      Result := Client.Close(Response);
    if Assigned(ErrorCode) then
      ErrorCode^ := Client.LastAlertCode;
    Client.Release;
  end;
end;

function TStreamSecII.TLSDecodeData(var SessionID: TSessionID; Src, Data,
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  Result := 0;
  if FindTLSSession(SessionID,Client) >= 0 then begin
    Result := Client.DecodeData(Src,Data,Response);
    if Assigned(ErrorCode) then
      ErrorCode^ := Client.LastAlertCode;
    if not Client.Active then
      Client.Release;
  end;
end;

function TStreamSecII.TLSEncodeData(const SessionID: TSessionID; Data,
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  Result := 0;
  if FindTLSSession(SessionID,Client) >= 0 then begin
    Result := Client.EncodeData(Data,Response);
    if Assigned(ErrorCode) then
      ErrorCode^ := Client.LastAlertCode;
  end;
end;

function TStreamSecII.TLSClose(UserData: Pointer;
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  Result := 0;
  if FindTLSSession(UserData,Client) >= 0 then begin
    if Assigned(Response) then
      Result := Client.Close(Response);
    if Assigned(ErrorCode) then
      ErrorCode^ := Client.LastAlertCode;
    Client.Release;
  end;
end;

function TStreamSecII.TLSConnect(UserData: Pointer;
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  if FindTLSSession(UserData,Client) < 0 then begin
    Client := TLSAddClientSession;
    Client.UserData := UserData;
    TLSLock;
    try
      FTLSSessionsUD.Sort(TLSSessionsUDSortCompare);
    finally
      TLSUnlock;
    end;
  end;
  Result := Client.Connect(Response);
  if Assigned(ErrorCode) then
    ErrorCode^ := Client.LastAlertCode;
end;

function TStreamSecII.TLSConnectAsServer(UserData: Pointer;
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  if FindTLSSession(UserData,Client) < 0 then begin
    Client := TLSAddServerSession;
    Client.UserData := UserData;
    TLSLock;
    try
      FTLSSessionsUD.Sort(TLSSessionsUDSortCompare);
    finally
      TLSUnlock;
    end;
  end;
  Result := Client.Accept(nil,Response);
  if Assigned(ErrorCode) then
    ErrorCode^ := Client.LastAlertCode;
end;

function TStreamSecII.TLSDecodeData(UserData: Pointer; Src, Data,
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  if FindTLSSession(UserData,Client) >= 0 then begin
    Result := Client.DecodeData(Src,Data,Response);
    if Assigned(ErrorCode) then
      ErrorCode^ := Client.LastAlertCode;
    if not Client.Active then
      Client.Release;
  end else
    Result := TLSAccept(UserData,Src,Response,ErrorCode);
end;

function TStreamSecII.TLSEncodeData(UserData: Pointer; Data,
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  Result := 0;
  if FindTLSSession(UserData,Client) >= 0 then begin
    Result := Client.EncodeData(Data,Response);
    if Assigned(ErrorCode) then
      ErrorCode^ := Client.LastAlertCode;
  end;
end;

function TStreamSecII.FindTLSSession(UserData: Pointer;
  var Client: TCustomTLS_ContentLayer; Extract: Boolean): Integer;
begin
  TLSLock;
  try
    Client := TLSSessionsUDFind(UserData, Result);
    if Client = nil then
      Result := -1
    else if Extract then begin
      FTLSSessionsID.Remove(Client);
      FTLSSessionsUD.Remove(Client);
      Client.Unmanaged := True;
      Client.OnDestroy := nil;
    end;
  finally
    TLSUnlock;
  end;
end;

procedure TStreamSecII.TLSSetupServer;
var
  Idx, Len, I: Integer;
  C: PASN1Struct;
  IFPubl: TIFPublicKey;
  ICR: TInternalCertificateRequest;
  PrivKey: TMPPrivateKey;
begin
  with TLSOptions do begin
    if [SignatureAnon,SignatureRSA,SignatureECDSA,SignatureDSS] = [prNotAllowed] then
      raise Exception.Create(Name + '.TLSSetupServer: All signatures schemes are disabled.');
    if [KeyAgreementDH,KeyAgreementDHE,KeyAgreementECDH,KeyAgreementRSA] = [prNotAllowed] then
      raise Exception.Create(Name + '.TLSSetupServer: All key agreement schemes are disabled.');
  end;
  TLSLock;
  try
  if Assigned(FMyCertificates) then begin
    // Compile all supported certificate handshake messages:
    Idx := 0;
    while FMyCertificates.FindCert([md2WithRSAEncryption,
                                    md5WithRSAEncryption,
                                    sha1WithRSAEncryption,
                                    sha256WithRSAEncryption,
                                    sha384WithRSAEncryption,
                                    sha512WithRSAEncryption],
                                   [rsaEncryption],
                                   [digitalSignature,keyEncipherment],
                                   Idx,C) do begin
      FRSACert := nil;
      PrivKey := FindCreatePrivateKeyTLS(C^);
      if Assigned(PrivKey) then begin
        FRSAPriv := PrivKey as TMPIFPrivateKey;
        FillChar(IFPubl,SizeOf(IFPubl),0);
        try
          if ExtractSubjectRSAPublicKey(C^,IFPubl) = E_OK then begin
            if FRSAPriv.ValidatePublicKey(IFPubl) then begin
              FRSACert := C;
              if Assigned(FTLS_RSACertificate) then
                Len := FTLS_RSACertificate.length[0]*65536 +
                       FTLS_RSACertificate.length[1]*256 +
                       FTLS_RSACertificate.length[2]
              else
                Len := 0;
              FMyCertificates.ExportChainToTLS(FTLS_RSACertificate,Len,Idx);
              Break;
            end else
              FRSAPriv := nil;
          end;
        finally
          DisposeIFPublicKey(IFPubl);
        end;
      end;
    end;
    Idx := 0;
    while FMyCertificates.FindCert([md2WithRSAEncryption,
                                    md5WithRSAEncryption,
                                    sha1WithRSAEncryption,
                                    sha256WithRSAEncryption,
                                    sha384WithRSAEncryption,
                                    sha512WithRSAEncryption],
                                   [dhPublicNumber],
                                   [keyAgreement],
                                   Idx,C) do begin
      FDH_RSACert := nil;
      PrivKey := FindCreatePrivateKeyTLS(C^);
      if Assigned(PrivKey) then begin
        FDH_RSAPriv := PrivKey as TMPDLPrivateKey;
        FDH_RSACert := C;
        if Assigned(FTLS_DH_RSACertificate) then
          Len := FTLS_DH_RSACertificate.length[0]*65536 +
                 FTLS_DH_RSACertificate.length[1]*256 +
                 FTLS_DH_RSACertificate.length[2]
        else
          Len := 0;
        FMyCertificates.ExportChainToTLS(FTLS_DH_RSACertificate,Len,Idx);
        Break;
      end;
    end;
    Idx := 0;
    while FMyCertificates.FindCert([md2WithRSAEncryption,
                                    md5WithRSAEncryption,
                                    sha1WithRSAEncryption,
                                    sha256WithRSAEncryption,
                                    sha384WithRSAEncryption,
                                    sha512WithRSAEncryption],
                                   [id_ecPublicKey],
                                   [keyAgreement],
                                   Idx,C) do begin
      FECDH_RSACert := nil;
      PrivKey := FindCreatePrivateKeyTLS(C^);
      if Assigned(PrivKey) then begin
        FECDH_RSAPriv := PrivKey as TMPECPrivateKey;
        FECDH_RSACert := C;
        if Assigned(FTLS_ECDH_RSACertificate) then
          Len := FTLS_ECDH_RSACertificate.length[0]*65536 +
                 FTLS_ECDH_RSACertificate.length[1]*256 +
                 FTLS_ECDH_RSACertificate.length[2]
        else
          Len := 0;
        FMyCertificates.ExportChainToTLS(FTLS_ECDH_RSACertificate,Len,Idx);
        Break;
      end;
    end;
    Idx := 0;
    while FMyCertificates.FindCert([id_dsa_with_sha1],
                                   [dhPublicNumber],
                                   [keyAgreement],
                                   Idx,C) do begin
      FDH_DSSCert := nil;
      PrivKey := FindCreatePrivateKeyTLS(C^);
      if Assigned(PrivKey) then begin
        FDH_DSSPriv := PrivKey as TMPDLPrivateKey;
        FDH_DSSCert := C;
        if Assigned(FTLS_DH_DSSCertificate) then
          Len := FTLS_DH_DSSCertificate.length[0]*65536 +
                 FTLS_DH_DSSCertificate.length[1]*256 +
                 FTLS_DH_DSSCertificate.length[2]
        else
          Len := 0;
        FMyCertificates.ExportChainToTLS(FTLS_DH_DSSCertificate,Len,Idx);
        Break;
      end;
    end;
    Idx := 0;
    while FMyCertificates.FindCert([],
                                   [id_dsa],
                                   [digitalSignature],
                                   Idx,C) do begin
      FDSSCert := nil;
      PrivKey := FindCreatePrivateKeyTLS(C^);
      if Assigned(PrivKey) then begin
        FDSSPriv := PrivKey as TMPDLPrivateKey;
        FDSSCert := C;
        if Assigned(FTLS_DSSCertificate) then
          Len := FTLS_DSSCertificate.length[0]*65536 +
                 FTLS_DSSCertificate.length[1]*256 +
                 FTLS_DSSCertificate.length[2]
        else
          Len := 0;
        FMyCertificates.ExportChainToTLS(FTLS_DSSCertificate,Len,Idx);
        Break;
      end;
    end;
    Idx := 0;
    while FMyCertificates.FindCert([ecdsa_with_sha1],
                                   [id_ecPublicKey],
                                   [keyAgreement],
                                   Idx,C) do begin
      FECDH_ECDSACert := nil;
      PrivKey := FindCreatePrivateKeyTLS(C^);
      if Assigned(PrivKey) then begin
        FECDH_ECDSAPriv := PrivKey as TMPECPrivateKey;
        FECDH_ECDSACert := C;
        if Assigned(FTLS_ECDH_ECDSACertificate) then
          Len := FTLS_ECDH_ECDSACertificate.length[0]*65536 +
                 FTLS_ECDH_ECDSACertificate.length[1]*256 +
                 FTLS_ECDH_ECDSACertificate.length[2]
        else
          Len := 0;
        FMyCertificates.ExportChainToTLS(FTLS_ECDH_ECDSACertificate,Len,Idx);
        Break;
      end;
    end;
  end;

    if Assigned(FCACertificates) then begin
      // Compile all certificate request messages:
      SetLength(ICR.certificate_authorities,FCACertificates.Count);
      for I := 0 to FCACertificates.Count - 1 do
        ExtractSubject(FCACertificates.Certs[I],
                       ICR.certificate_authorities[I]);

      SetLength(ICR.certificate_types,4);
      ICR.certificate_types[0] := 1;
      ICR.certificate_types[1] := 2;
      ICR.certificate_types[2] := 3;
      ICR.certificate_types[3] := 4;
      Len := TLS_EncodeCertificateRequest(ICR,FDHCertReq);

      SetLength(ICR.certificate_types,3);
      ICR.certificate_types[0] := 5;
      ICR.certificate_types[1] := 6;
      ICR.certificate_types[2] := 7;
      TLS_EncodeCertificateRequest(ICR,FECDHCertReq);

      SetLength(ICR.certificate_types,2);
      ICR.certificate_types[0] := 1;
      ICR.certificate_types[1] := 2;
      TLS_EncodeCertificateRequest(ICR,FOtherCertReq);
    end;
  finally
    TLSUnlock;
  end;
end;

procedure TStreamSecII.DoTLSGetCertificate(Sender: TObject;
  KEA: TKeyExchangeAlgorithm; SignAlg: TSignatureAlgorithm;
  var H: PTLSHandshake; var Len: Integer; var C: PASN1Struct;
  var Found: Boolean);
begin
  TLSLock;
  try
    if ((KEA in [keaRSA,keaRSA_Export,keaRSA1024]) or
        (KEA in [keaDHE,keaDHE_Export,keaDHE1024,keaECDHE])) and
       (SignAlg = saRSA) then begin
      Found := Assigned(FTLS_RSACertificate);
      if Found then begin
        Len := CopyTLSHandshake(FTLS_RSACertificate,H);
        C := FRSACert;
      end;
    end else if (KEA = keaDH) and (SignAlg = saRSA) then begin
      Found := Assigned(FTLS_DH_RSACertificate);
      if Found then begin
        Len := CopyTLSHandshake(FTLS_DH_RSACertificate,H);
        C := FDH_RSACert;
      end;
    end else if (KEA = keaECDH) and (SignAlg = saRSA) then begin
      Found := Assigned(FTLS_ECDH_RSACertificate);
      if Found then begin
        Len := CopyTLSHandshake(FTLS_ECDH_RSACertificate,H);
        C := FECDH_RSACert;
      end;
    end else if (KEA in [keaDHE,keaDHE_Export,keaDHE1024,keaECDHE]) and
                (SignAlg = saDSS) then begin
      Found := Assigned(FTLS_DSSCertificate);
      if Found then begin
        Len := CopyTLSHandshake(FTLS_DSSCertificate,H);
        C := FDSSCert;
      end;
    end else if (KEA = keaDH) and (SignAlg = saDSS) then begin
      Found := Assigned(FTLS_DH_DSSCertificate);
      if Found then begin
        Len := CopyTLSHandshake(FTLS_DH_DSSCertificate,H);
        C := FDH_DSSCert;
      end;
    end else if (KEA = keaECDH) and (SignAlg = saECDSA) then begin
      Found := Assigned(FTLS_ECDH_ECDSACertificate);
      if Found then begin
        Len := CopyTLSHandshake(FTLS_ECDH_ECDSACertificate,H);
        C := FECDH_ECDSACert;
      end;
    end;
  finally
    TLSUnlock;
  end;
end;

procedure TStreamSecII.DoTLSGetCertReq(Sender: TObject;
  KEA: TKeyExchangeAlgorithm; var H: PTLSHandshake; var Len: Integer;
  var Found: Boolean);
begin
  case KEA of
    keaDH:
      begin
        Found := Assigned(FDHCertReq);
        if Found then
          Len := CopyTLSHandshake(FDHCertReq,H);
      end;
    keaECDH:
      begin
        Found := Assigned(FECDHCertReq);
        if Found then
          Len := CopyTLSHandshake(FECDHCertReq,H);
      end;
  else
    Found := Assigned(FOtherCertReq);
    if Found then
      Len := CopyTLSHandshake(FOtherCertReq,H);
  end;
end;

function TStreamSecII.TLSConnect(const SessionID: TSessionID;
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  if FindTLSSession(SessionId,Client) < 0 then begin
    Client := TLSAddClientSession;
    Client.SessionID := SessionID;
    // The client will be added to the ID list after the change cipher spec
  end;
  Result := Client.Connect(Response);
  if Assigned(ErrorCode) then
    ErrorCode^ := Client.LastAlertCode;
end;

function TStreamSecII.TLSConnect(UserData: Pointer;
  const SessionID: TSessionID; Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  if (FindTLSSession(UserData,Client) < 0) {and
     (FindTLSSession(SessionId,Client) < 0)} then begin
    Client := TLSAddClientSession;
    Client.SessionID := SessionID;
    Client.UserData := UserData;
    TLSLock;
    try
      FTLSSessionsUD.Sort(TLSSessionsUDSortCompare);
    finally
      TLSUnlock;
    end;
  end;
  Result := Client.Connect(Response);
  if Assigned(ErrorCode) then
    ErrorCode^ := Client.LastAlertCode;
end;

function TStreamSecII.TLSConnect(UserData: Pointer;
  const SessionID: TSessionID; const CipherSuites: TCipherSuites;
  Response: TStream; ErrorCode: PInteger): Integer;
var
  Client: TCustomTLS_ContentLayer;
begin
  if (FindTLSSession(UserData,Client) < 0) {and
     (FindTLSSession(SessionId,Client) < 0)} then begin
    Client := TLSAddClientSession;
    Client.SessionID := SessionID;
    Client.UserData := UserData;
    Client.CipherPreference := CipherSuites;
    TLSLock;
    try
      FTLSSessionsUD.Sort(TLSSessionsUDSortCompare);
    finally
      TLSUnlock;
    end;
  end;
  Result := Client.Connect(Response);
  if Assigned(ErrorCode) then
    ErrorCode^ := Client.LastAlertCode;
end;

procedure TStreamSecII.TLSLock;
begin
  if not CheckThreadID then
    FTLSLock.Acquire;
end;

procedure TStreamSecII.TLSUnlock;
begin
  if not CheckThreadID then
    FTLSLock.Release;
end;

function TStreamSecII.TLSSessionsIDFind(
  SessionID: TSessionID; var Index: Integer): TCustomTLS_ContentLayer;
var
  L, H, I, C: Integer;
begin
  L := 0;
  H := FTLSSessionsID.Count - 1;
  Result := nil;
  C := -1;
  I := 0;
  while L <= H do begin
    I := (L + H) shr 1;
    Result := FTLSSessionsID[I];
    C := CompareStr(Result.SessionID, SessionID);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then
        L := I;
    end;
  end;    
  if C <> 0 then
    Result := nil;
  Index := I;
end;

function TStreamSecII.TLSSessionsUDFind(
  UserData: Pointer; var Index: Integer): TCustomTLS_ContentLayer;
var
  L, H, I, C: Integer;
begin
  L := 0;
  H := FTLSSessionsUD.Count - 1;
  Result := nil;
  C := -1;
  I := 0;
  while L <= H do begin
    I := (L + H) shr 1;
    Result := FTLSSessionsUD[I];
    C := LongInt(Result.UserData) - LongInt(UserData);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then
        L := I;
    end;
  end;
  if C <> 0 then
    Result := nil;
  Index := I;
end;

function TStreamSecII.FindCreatePrivateKeyTLS(
  Cert: TASN1Struct): TMPPrivateKey;
var
  KR: TKeyRings;
begin
  Result := nil;
  TLSLock;
  try
    if Assigned(FRSACert) and (Cert = FRSACert^) then
      Result := FRSAPriv.Instance as TMPPrivateKey
    else if Assigned(FDSSCert) and (Cert = FDSSCert^) then
      Result := FDSSPriv.Instance as TMPPrivateKey
    else if Assigned(FDH_DSSCert) and (Cert = FDH_DSSCert^) then
      Result := FDH_DSSPriv.Instance as TMPPrivateKey
    else if Assigned(FDH_RSACert) and (Cert = FDH_RSACert^) then
      Result := FDH_RSAPriv.Instance as TMPPrivateKey
    else if Assigned(FECDH_ECDSACert) and (Cert = FECDH_ECDSACert^) then
      Result := FECDH_ECDSAPriv.Instance as TMPPrivateKey
    else if Assigned(FECDH_RSACert) and (Cert = FECDH_RSACert^) then
      Result := FECDH_RSAPriv.Instance as TMPPrivateKey;
  finally
    TLSUnlock;
  end;
  if Result = nil then begin
    KR := [krSession,krLongTerm,krEncrypted,krSignedEncrypted];
    Result := FPrivateKeyRing.FindCreatePrivateKeyObj(Cert,KR,True);
  end;
end;                 
{$ENDIF SHA1_AND_MD5}

procedure TStreamSecII.ClearExpiredSessionKeys;
var
  I: Integer;
  NowGMT: TDateTime;
begin
  KeyLock;
  try
    NowGMT := Now - HoursOffsetFromGMT/24;
    for I := SessionKeyCount - 1 downto 0 do
      if SessionKeys[I].FindField('expires').ContentAsDateTime < NowGMT then
        DeleteSessionKey(I);
  finally
    KeyUnlock;
  end;
end;
                     
{$IFDEF SHA1_AND_MD5}
function TStreamSecII.RenewEphemeralDHKey(DHBitSize,
  DHQBitSize: Integer): Boolean;
var
  PKI: string;
  DLPriv: TDLPrivateKey;
begin
  PKI := Format('DH key %d %d',[DHBitSize,DHQBitSize]);
  FillChar(DLPriv,SizeOf(DLPriv),0);
  try
    KeyLock;
    try
      Result := FindPrivateKey(Pointer(PKI),Length(PKI),DLPriv);
      if Result then begin
        MPRandomBound(DLPriv.X,nil,DLPriv.Params.Q);
        Result := AddPrivateDLKey(DLPriv,Pointer(PKI),Length(PKI),False,False) >= 0;
      end;
    finally
      KeyUnlock;
    end;
  finally
    DisposeDLPrivateKey(DLPriv);
  end;
end;   
{$ENDIF SHA1_AND_MD5}

{ TDigestStream }

constructor TDigestStream.Create(ADataStream: TStream; AHash: THash);
begin
  inherited Create;
  FDataStream := ADataStream;
  FHash := AHash;
end;

procedure TDigestStream.Done(ADigest: Pointer);
begin
  if Assigned(FHash) then
    FHash.Done(ADigest);
end;

function TDigestStream.GetDigest: string;
begin
  if Assigned(FHash) then
    Result := FHash.Digest
  else
    Result := '';
end;

function TDigestStream.Read(var Buffer; Count: Integer): Longint;
begin
  if Assigned(FDataStream) then begin
    Result := FDataStream.Read(Buffer,Count);
    if Assigned(FHash) then
      FHash.HashData(Buffer,Result);
  end else
    Result := 0;
end;

function TDigestStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if Assigned(FDataStream) then
    Result := FDataStream.Seek(Offset,Origin)
  else
    Result := 0;
end;

procedure TDigestStream.SetDataStream(const Value: TStream);
begin
  FDataStream := Value;
end;

procedure TDigestStream.SetHash(const Value: THash);
begin
  FHash := Value;
end;

function TDigestStream.Write(const Buffer; Count: Integer): Longint;
begin
  if Assigned(FDataStream) then begin  
    Result := FDataStream.Write(Buffer,Count);
    if Assigned(FHash) then
      FHash.HashData(Buffer,Result);
  end else
    Result := 0;
end;

{ TEncryptStream }

constructor TEncryptStream.Create(ADataStream: TStream; ACipher: TCipher);
begin
  inherited Create;
  FCipher := ACipher;
  if ACipher is TBlockCipher then begin
    FBlockSize := ACipher.BlockSize;
    GetMem(FBlock,FBlockSize);
  end;
  FDataStream := ADataStream;
end;

destructor TEncryptStream.Destroy;
begin
  ProtectClear(FBlock^,FBlockSize);
  FreeMem(FBlock);
  FBlock := nil;
  inherited;
end;

function TEncryptStream.Done: Integer;
var
  P: PChar;
  I, PadSize: Integer;
begin
  if Assigned(FBlock) then begin
    PadSize := FBlockSize - FBlockCount;
    P := FBlock;
    for I := FBlockSize - 1 downto FBlockCount do
      P[I] := Char(PadSize);
    FCipher.Encrypt(FBlock^,FBlockSize);
    FDataStream.Write(FBlock^,FBlockSize);
    Result := PadSize;
    FBlockCount := 0;
  end else
    Result := 0;
end;

function TEncryptStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

function TEncryptStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if Assigned(FDataStream) then
    Result := FDataStream.Seek(Offset,Origin)
  else
    Result := 0;
end;

procedure TEncryptStream.SetCipher(const Value: TCipher);
begin
  FCipher := Value;
end;

procedure TEncryptStream.SetDataStream(const Value: TStream);
begin
  FDataStream := Value;
end;

function TEncryptStream.Write(const Buffer; Count: Integer): Longint;
var
  P: PChar;
  InternalBuffer: array [0..1023] of Byte;
  InBuffer: Integer;
begin
  if Assigned(FDataStream) then begin
    if Assigned(FCipher) then begin
      Result := 0;
      P := @Buffer;
      Count := Count + FBlockCount;
      while Count > 0 do begin
        if FBlockCount > 0 then
          Move(FBlock^,InternalBuffer,FBlockCount);
        if Count >= 1024 then begin
          Move(P^,InternalBuffer[FBlockCount],1024 - FBlockCount);
          P := P + 1024 - FBlockCount;
          Result := Result + 1024 - FBlockCount;
          InBuffer := 1024;
          Count := Count - 1024;
        end else begin
          Move(P^,InternalBuffer[FBlockCount],Count - FBlockCount);
          P := P + Count - FBlockCount;
          Result := Result + Count - FBlockCount;
          InBuffer := Count;
          Count := 0;
        end;
        if FBlockSize > 0 then begin
          FBlockCount := InBuffer mod FBlockSize;
          InBuffer := InBuffer - FBlockCount;
          Count := Count + FBlockCount;
          Move(InternalBuffer[InBuffer],FBlock^,FBlockCount);
        end else
          FBlockCount := 0;
        if InBuffer = 0 then
          Break;
        FCipher.Encrypt(InternalBuffer,InBuffer);
        FDataStream.Write(InternalBuffer,InBuffer);
      end;
    end else
      Result := FDataStream.Write(Buffer,Count);
  end else
    Result := 0;
end;

{ TDecryptStream }

constructor TDecryptStream.Create(ADataStream: TStream; ACipher: TCipher);
begin    
  inherited Create;
  FCipher := ACipher;
  if ACipher is TBlockCipher then begin
    FBlockSize := ACipher.BlockSize;
    GetMem(FBlock,FBlockSize);
  end;
  FDataStream := ADataStream;
end;

destructor TDecryptStream.Destroy;
begin
  inherited;
  ProtectClear(FBlock^,FBlockCount);
  FreeMem(FBlock);
  ProtectClear(FDecBuffer,FDecLen);
end;

function TDecryptStream.Done: Integer;
var
  I: Integer;
begin
  Result := 0;
  FOK := FDecLen <= FBlockSize;
  if FOK and (FDecLen > 0) then begin
    Result := FDecBuffer[FDecLen - 1];
    if Result > FBlockSize then
      FOK := False
    else if Result <> FDecLen then
      FOK := False
    else
      for I := 0 to Result - 2 do
        if FDecBuffer[I] <> Result then
          FOK := False;
  end;
end;

function TDecryptStream.Read(var Buffer; Count: Integer): Longint;
var
  P: PChar;
  InBuffer, PLen, DLen: Integer;
begin
  if Assigned(FDataStream) then begin
    if Assigned(FCipher) then begin
      Result := 0;
      InBuffer := 0;
      P := @Buffer;
      if FDecLen > 0 then begin
        if FDecLen > Count then begin
          Move(FDecBuffer,Buffer,Count);
          Move(FDecBuffer[Count],FDecBuffer,FDecLen - Count);
          FDecLen := FDecLen - Count;
          Result := Count;
          Count := 0;
        end else begin
          Move(FDecBuffer,Buffer,FDecLen);
          P := P + FDecLen;
          Result := FDecLen;
          Count := Count - FDecLen;
          FDecLen := 0;
        end;
      end else if FBlockCount > 0 then begin
        Move(FBlock^,P^,FBlockCount);
        InBuffer := FBlockCount;
        P := P + InBuffer;
        FBlockCount := 0;
      end;
      if Count > 0 then begin
        InBuffer := InBuffer + FDataStream.Read(P^,Count);
        Result := Result + InBuffer;
        DLen := InBuffer mod FBlockSize;
        PLen := InBuffer - DLen;
        if PLen > 0 then FCipher.Decrypt(P^,PLen);
        Move(P[PLen],FBlock^,DLen);
        FBlockCount := FDataStream.Read(FBlock^[DLen],FBlockSize - DLen) + DLen;
        if FBlockCount = FBlockSize then begin
          FCipher.DecryptToPtr(FBlock,@FDecBuffer,FBlockCount);
          Move(FDecBuffer,P[PLen],DLen);
          FBlockCount := 0;
          FDecLen := FBlockSize - DLen;
          Move(FDecBuffer[DLen],FDecBuffer,FDecLen);
        end else
          Result := Result - DLen;
      end;
    end else
      Result := FDataStream.Read(Buffer,Count);
  end else
    Result := 0;
end;

function TDecryptStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if Assigned(FDataStream) then begin
    Result := FDataStream.Seek(Offset,Origin);
  end else
    Result := 0;
end;

procedure TDecryptStream.SetCipher(const Value: TCipher);
begin
  FCipher := Value;
end;

procedure TDecryptStream.SetDataStream(const Value: TStream);
begin
  FDataStream := Value;
end;

function TDecryptStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

{ TKeyGenThread }

procedure TKeyGenThread.Execute;
var
  Start: Cardinal;
begin
  try
    MPArith.SetThrdMPPoolSize(512);
    try
      while not Terminated do begin
        Start := GetTickCount;
        Generate;
        if RunOnce then Break;
        while (Start + SleepTime > GetTickCount) and not Terminated do
          Sleep(50);
      end;
    finally
      MPArith.SetThrdMPPoolSize(0);
    end;
  except
  end;
end;

procedure TKeyGenThread.GeneratePrimeSubGroup(var P, Q: PMPInteger;
                                              BitSize, QBitSize: Integer);
var
  SMin, SMax, S: PMPInteger;
  OK: Boolean;
  Attempts: Integer;
begin
  OK := False;
  while not (OK or Terminated) do begin
    GenerateRandomPrime(Q,QBitSize);
    SMin := nil;
    SMax := nil;
    try
      Attempts := 0;
      while (Attempts < 4096) and not (OK or Terminated) do begin
        Inc(Attempts);
        S := IntToMPInt(1);
        try
          MPShl(S,BitSize);
          MPDiv(S,Q,SMax);
        finally
          MPDealloc(S);
        end;
        S := IntToMPInt(1);
        try
          MPShl(S,BitSize-1);
          MPDiv(S,Q,SMin);
        finally
          MPDealloc(S);
        end;
        S := nil;
        try
          GenerateRandomBound(S,SMin,SMax);
          MPMul2(S,Q,P);
          MPInc(P);
        finally
          MPDealloc(S);
        end;
        OK := MPMillerRabin(P,1);
      end;
    finally
      MPDealloc(SMax);
      MPDealloc(SMin);
    end;
  end;
end;

procedure TKeyGenThread.GenerateRandom;
begin
  if FRandomLen > 0 then begin
    RawRandom(FRandom^,FRandomLen*8);
    PLongWord(FRandom)^ := PLongWord(FRandom)^ or $80000000;
  end;
end;

procedure TKeyGenThread.GenerateRandomBound(var X: PMPInteger; XMin,
  XUBound: PMPInteger);
var
  D: PMPInteger;
  BSize, Len: Integer;
begin
  // Generate random X such that XMin <= X < XUBound
  if Assigned(XMin) then begin
    D := MPCopy(XUBound);
    MPSub(D,XMin);
    GenerateRandomBound(X,nil,D);
    MPDealloc(D);
    MPAdd(X,XMin);
  end else begin
    BSize := MPMSB(XUBound);
    Len := ((2*BSize + 31) shr 5)*4;
    MPRealloc(X,Len shr 2);
    X^.Data[0] := 0;
    FRandom := @X^.Data;
    X^.Sign := 1;
    Synchronize(GenerateRandom);
    FRandomLen := Len;
    MPMul(X,XUBound);
    Move(X^.Data[0],X^.Data[Len shr 2],Integer(X^.Size) - (Len shr 2));
    FillChar(X^.Data[0],Len,0);
    MPRealloc(X,(MPMSB(X) + 31) shr 5);
    if MPCmpOffset(X,XUBound) >= 0 then
      raise Exception.Create('');
  end;
end;

procedure TKeyGenThread.GenerateRandomPrime(var P: PMPInteger;
                                            BitSize: Integer;
                                            Params: Pointer = nil);
var
  Len: Integer;
  D: PMPInteger;
  PrParams: PPrimeParams;
begin
  Len := (BitSize + 7) shr 3;
  MPRealloc(P,(Len + 3) shr 2);
  P^.Sign := 1;
  P^.Data[0] := 0;
  FRandom := Ptr(LongInt(@P^.Data) + ((4 - Len) mod 4));
  FRandomLen := Len;
  if Assigned(Params) then begin
    D := nil;
    PrParams := Params;
    if Assigned(PrParams^.PMin) and Assigned(PrParams^.PUBound) then begin
      repeat
        GenerateRandomBound(P,PrParams^.PMin,PrParams^.PUBound);
        if Assigned(PrParams^.F) then begin
          P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] and not 1;
          if PrParams^.Mod8 < 8 then
            if (P^.Data[P^.Size - 1] and 8) <> PrParams^.Mod8 then Continue;
          MPBinaryGCD(P,PrParams^.F,D);
          if Terminated then Break;
          if MPMSB(D) <> 1 then Continue;
        end;
        P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 1;
      until MPMillerRabin(P,1) or Terminated;
    end else
      repeat
        Synchronize(GenerateRandom);
        if Assigned(PrParams^.F) then begin
          P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] and not 1;
          if PrParams^.Mod8 < 8 then
            if (P^.Data[P^.Size - 1] and 8) <> PrParams^.Mod8 then Continue;
          MPBinaryGCD(P,PrParams^.F,D);
          if Terminated then Break;
          if MPMSB(D) <> 1 then Continue;
        end;
        P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 1;
      until MPMillerRabin(P,1) or Terminated;
    MPDealloc(D);
  end else
    repeat
      Synchronize(GenerateRandom);
      P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 1;
    until MPMillerRabin(P,1) or Terminated;
end;

procedure TKeyGenThread.SetRunOnce(const Value: Boolean);
begin
  FRunOnce := Value;
end;

procedure TKeyGenThread.SetSleepTime(const Value: Cardinal);
begin
  FSleepTime := Value;
end;

{ TRSAKeyGen }

procedure TRSAKeyGen.DoKeyDone;
begin
  if Assigned(FOnKeyDone) then
    FOnKeyDone(Self,FPriv,FPubl);
end;

procedure TRSAKeyGen.Generate;  
var
  P, Q, N, X, Y, M, G, L, D: PMPInteger;
  Params: TStrongPrimeParams;
begin
  FillChar(FPriv,SizeOf(FPriv),0);
  FillChar(FPubl,SizeOf(FPubl),0);
  try
    FPriv.ReprType := 1;
    FPriv.Scheme := ifRSA1;
    FPubl.Scheme := ifRSA1;

    FPubl.E := IntToMPInt(3);

    P := nil;
    try
      Params.PMin := nil;
      Params.PUBound := nil;
      Params.F := FPubl.E;
      Params.Mod8 := 255;
      GenerateRandomPrime(P,(FKeyBitSize + 1) shr 1,@Params);
      MPCopy2(P,FPriv.oP);
      Q := nil;
      try
        X := IntToMPInt(1);
        try
          MPShl(X,FKeyBitSize-1);
          MPDiv(X,P,Params.PMin);
          MPInc(Params.PMin);
        finally
          MPDealloc(X);
        end;
        X := IntToMPInt(1);
        try
          MPShl(X,FKeyBitSize);
          MPDiv(X,P,Params.PUBound);
          MPInc(Params.PUBound);
        finally
          MPDealloc(X);
        end;
        Params.F := FPubl.E;
        Params.Mod8 := 255;
        GenerateRandomPrime(Q,0,@Params);
        MPDealloc(Params.PMin);
        MPDealloc(Params.PUBound);
        MPCopy2(Q,FPriv.oQ);

        N := nil;
        try
          MPMul2(P,Q,N);
          MPRealloc(N,(MPMSB(N) + 31) shr 5);
          MPCopy2(N,FPubl.N);
          X := nil;
          try
            MPDec2(P,X);
            Y := nil;
            try
              MPDec2(Q,Y);
              M := nil;
              try
                MPMul2(X,Y,M);
                G := nil;
                try
                  MPBinaryGCD(X,Y,G);
                  L := nil;
                  try
                    MPDiv(M,G,L);
                    D := nil;
                    try
                      MPInvMod(FPubl.E,L,D);
                      MPCopy2(D,FPriv.oD1);
                      MPMod(FPriv.oD1,X);

                      MPCopy2(D,FPriv.oD2);
                      MPMod(FPriv.oD2,Y);

                      MPInvMod(Q,P,FPriv.oC);

                      MPCopy2(Q,M);
                      MPMulMod(M,FPriv.oC,P);
                      Assert(MPMSB(M) = 1);
                    finally
                      MPDealloc(D);
                    end;
                  finally
                    MPDealloc(L);
                  end;
                finally
                  MPDealloc(G);
                end;
              finally
                MPDealloc(M);
              end;
            finally
              MPDealloc(Y);
            end;
          finally
            MPDealloc(X);
          end;
        finally
          MPDealloc(N);
        end;
      finally
        MPDealloc(Q);
      end;
    finally
      MPDealloc(P);
    end;
    Synchronize(DoKeyDone);
  finally
    DisposeIFPublicKey(FPubl);
    DisposeIFPrivateKey(FPriv);
  end;
end;

procedure TRSAKeyGen.SetKeyBitSize(const Value: Integer);
begin
  FKeyBitSize := Value;
end;

procedure TRSAKeyGen.SetOnKeyDone(const Value: TRSAKeyEvent);
begin
  FOnKeyDone := Value;
end;

{ TDHKeyGen }

procedure TDHKeyGen.DoKeyDone;
begin
  if Assigned(FOnKeyDone) then
    FOnKeyDone(Self,FPriv,FPubl);
end;

procedure TDHKeyGen.Generate;
var
  T: PMPInteger;
begin
  T := nil;
  FillChar(FPriv,SizeOf(FPriv),0);
  FillChar(FPubl,SizeOf(FPubl),0);
  try
    repeat
      GeneratePrimeSubGroup(FPriv.Params.P,FPriv.Params.Q,FKeyBitSize,FQBitSize);
      T := MPCopy(FPriv.Params.P);
      MPDiv(T,FPriv.Params.Q,FPriv.Params.J);
      MPDealloc(T);
      MPPow2Mod(FPriv.Params.J,FPriv.Params.P,FPriv.Params.G);
    until (MPMSB(FPriv.Params.G) > 1) or Terminated;
    MPCopy2(FPriv.Params.P,FPubl.Params.P);
    MPCopy2(FPriv.Params.Q,FPubl.Params.Q);
    MPCopy2(FPriv.Params.G,FPubl.Params.G);
    MPCopy2(FPriv.Params.J,FPubl.Params.J);
    GenerateRandomBound(FPriv.X,nil,FPriv.Params.Q);
    MPExpMod(FPriv.Params.G,FPriv.X,FPriv.Params.P,FPubl.Y);
    Synchronize(DoKeyDone);
  finally
    DisposeDLPublicKey(FPubl);
    DisposeDLPrivateKey(FPriv);
  end;
end;

procedure TDHKeyGen.SetKeyBitSize(const Value: Integer);
begin
  FKeyBitSize := Value;
end;

procedure TDHKeyGen.SetOnKeyDone(const Value: TDHKeyEvent);
begin
  FOnKeyDone := Value;
end;

procedure TDHKeyGen.SetQBitSize(const Value: Integer);
begin
  FQBitSize := Value;
end;
              
{$IFDEF SHA1_AND_MD5}
{ TTLSOptions }

procedure TTLSOptions.AssignTo(Dest: TPersistent);
var
  D: TTLSOptions;
begin
  if Dest is TTLSOptions then begin
    D := TTLSOptions(Dest);
    D.BeginUpdate;
    try
      D.Export40Bit := FExport40Bit;
      D.BulkCipherAES128 := FBulkCipherAES128;
      D.SignatureECDSA := FSignatureECDSA;
      D.KeyAgreementRSA := FKeyAgreementRSA;
      D.BulkCipherDES := FBulkCipherDES;
      D.SignatureDSS := FSignatureDSS;
      D.Export56Bit := FExport56Bit;
      D.BulkCipherARC4 := FBulkCipherARC4;
      D.BulkCipherTripleDES := FBulkCipherTripleDES;
      D.KeyAgreementECDH := FKeyAgreementECDH;
      D.KeyAgreementDH := FKeyAgreementDH;
      D.SignatureRSA := FSignatureRSA;
      D.SignatureAnon := FSignatureAnon;
      D.KeyAgreementDHE := FKeyAgreementDHE;
      D.KeyAgreementECDHE := FKeyAgreementECDHE;
      D.EphemeralDHQSize := FEphemeralDHQSize;
      D.EphemeralDHKeySize := FEphemeralDHKeySize;
      D.EphemeralRSAKeySize := FEphemeralRSAKeySize;
      D.RequireClientCertificate := FRequireClientCertificate;
      D.RequestClientCertificate := FRequestClientCertificate;
      D.HashAlgorithmSHA1 := FHashAlgorithmSHA1;
      D.HashAlgorithmSHA256 := FHashAlgorithmSHA256;
      D.HashAlgorithmSHA384 := FHashAlgorithmSHA384;
      D.HashAlgorithmSHA512 := FHashAlgorithmSHA512;
      D.HashAlgorithmMD5 := FHashAlgorithmMD5;
      D.HashAlgorithmRipeMD160 := FHashAlgorithmRipeMD160;
      D.BulkCipherAES256 := FBulkCipherAES256;
      D.BulkCipherAES192 := FBulkCipherAES192;
      D.BulkCipherAES128CTR := FBulkCipherAES128CTR;
      D.BulkCipherAES192CTR := FBulkCipherAES192CTR;
      D.BulkCipherAES256CTR := FBulkCipherAES256CTR;
      D.BulkCipherTwoFish128 := FBulkCipherTwoFish128;
      D.BulkCipherTwoFish256 := FBulkCipherTwoFish256;
      D.BulkCipherTwoFish192 := FBulkCipherTwoFish192;
      D.BulkCipherTwoFish128CTR := FBulkCipherTwoFish128CTR;
      D.BulkCipherTwoFish192CTR := FBulkCipherTwoFish192CTR;
      D.BulkCipherTwoFish256CTR := FBulkCipherTwoFish256CTR;
      D.BulkCipherBlowFish128 := FBulkCipherBlowFish128;
      D.BulkCipherBlowFish128CTR := FBulkCipherBlowFish128CTR;
      D.BulkCipherHelix := FBulkCipherHelix;
      D.OnlyStandardCipherSuites := FOnlyStandardCipherSuites;
      D.VerifyServerName := FVerifyServerName;
    finally
      D.EndUpdate;
    end;
  end else
    inherited;
end;

procedure TTLSOptions.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TTLSOptions.ComputeCipherSuites: TCipherSuites;
var
  I: Integer;
  Standard, OK: Boolean;
  CS: TCipherSuite;
  CipherAlg:      TBulkCipherAlgorithm;
  HashAlg:        THashAlgorithm;
  KeyExchangeAlg: TKeyExchangeAlgorithm;
  SignAlg:        TSignatureAlgorithm;
  Pts: Integer;
  Pt: Integer;
  Pr: TPreference;
  Taken: set of Byte;
begin
  CS[0] := 0;
  SetLength(Result,0);
  Taken := [];
  for Pt := 8 downto 4 do begin
    for Standard := Low(Boolean) to High(Boolean) do begin
      if Standard then
        CS[0] := 0
      else begin
        if OnlyStandardCipherSuites then Continue;
        CS[0] := $FF;
      end;
      for I := 0 to $FF do begin
        if I in Taken then Continue;
        CS[1] := I;
        if Standard then
          OK := TLS_DecodeCipherSuite(CS,CipherAlg,HashAlg,KeyExchangeAlg,SignAlg)
        else
          OK := TLS_DecodeXCipherSuite(CS,CipherAlg,HashAlg,KeyExchangeAlg,SignAlg);
        if OK then begin
          Pts := 0;
          case CipherAlg of
            bcaRC4:
              Pr := BulkCipherARC4;
            bcaRC4_40:
              begin
                Pr := BulkCipherARC4;
                if Pr > Export40Bit then
                  Pr := Export40Bit;
              end;
            bcaRC4_56:
              begin
                Pr := BulkCipherARC4;
                if Pr > Export56Bit then
                  Pr := Export56Bit;
              end;
            bcaDES:
              Pr := BulkCipherDES;
            bcaDES40:
              begin
                Pr := BulkCipherDes;
                if Pr > Export40Bit then
                  Pr := Export40Bit;
              end;
            bcaDES56:
              begin
                Pr := BulkCipherDes;
                if Pr > Export56Bit then
                  Pr := Export56Bit;
              end;
            bca3DES:
              Pr := BulkCipherTripleDES;
            bcaAES128:
              Pr := BulkCipherAES128;
            bcaAES256:
              Pr := BulkCipherAES256;
            bcaAES192:
              Pr := BulkCipherAES192;
            bcaTwoFish128:
              Pr := BulkCipherTwoFish128;
            bcaTwoFish192:
              Pr := BulkCipherTwoFish192;
            bcaTwoFish256:
              Pr := BulkCipherTwoFish256;
            bcaBlowFish128:
              Pr := BulkCipherBlowFish128;
            bcaAES128_CTR:
              Pr := BulkCipherAES128CTR;
            bcaAES256_CTR:
              Pr := BulkCipherAES256CTR;
            bcaAES192_CTR:
              Pr := BulkCipherAES192CTR;
            bcaTwoFish128_CTR:
              Pr := BulkCipherTwoFish128CTR;
            bcaTwoFish192_CTR:
              Pr := BulkCipherTwoFish192CTR;
            bcaTwoFish256_CTR:
              Pr := BulkCipherTwoFish256CTR;
            bcaBlowFish128_CTR:
              Pr := BulkCipherBlowFish128CTR;
            bcaHelix:
              Pr := BulkCipherHelix;
            bcaRC2_40:
              Pr := BulkCipherRC2;
          else
            Pr := prNotAllowed;
          end;
          if Pr = prAllowed then
            Pts := Pts + 1
          else if Pr = prPrefer then
            Pts := Pts + 2;
          if (Pr <> prNotAllowed) and (Pts <= Pt - 3) and (Pts >= Pt - 6) then begin
            if CipherAlg <> bcaHelix then case HashAlg of
              haMD5:
                Pr := HashAlgorithmMD5;
              haSHA1:
                Pr := HashAlgorithmSHA1;
              {$IFDEF RIPEMD160}
              haRipeMd160:
                Pr := HashAlgorithmRipeMD160;
              {$ENDIF}
              {$IFDEF SHA256}
              haSHA256:
                Pr := HashAlgorithmSHA256;
              {$ENDIF}
              {$IFDEF SHA512}
              haSHA384:
                Pr := HashAlgorithmSHA384;
              haSHA512:
                Pr := HashAlgorithmSHA512;
              {$ENDIF}
            else
              Pr := prNotAllowed;
            end;
            if Pr = prAllowed then
              Pts := Pts + 1
            else if Pr = prPrefer then
              Pts := Pts + 2;
            if (Pr <> prNotAllowed) and (Pts <= Pt - 2) and (Pts >= Pt - 4) then begin
              case KeyExchangeAlg of
                keaRSA:
                  Pr := KeyAgreementRSA;
                keaRSA_Export:
                  begin
                    Pr := KeyAgreementRSA;
                    if Pr > Export40Bit then
                      Pr := Export40Bit;
                  end;
                keaRSA1024:
                  begin
                    Pr := KeyAgreementRSA;
                    if Pr > Export56Bit then
                      Pr := Export56Bit;
                  end;
                keaDH:
                  Pr := KeyAgreementDH;
                keaDHE:
                  Pr := KeyAgreementDHE;
                keaDHE_Export:
                  begin
                    Pr := KeyAgreementDHE;
                    if Pr > Export40Bit then
                      Pr := Export40Bit;
                  end;
                keaDHE1024:
                  begin
                    Pr := KeyAgreementDHE;
                    if Pr > Export56Bit then
                      Pr := Export56Bit;
                  end;
                keaECDH:
                  Pr := KeyAgreementECDH;
                keaECDHE:
                  Pr := KeyAgreementECDHE;
              else
                Pr := prNotAllowed;
              end;
              if Pr = prAllowed then
                Pts := Pts + 1
              else if Pr = prPrefer then
                Pts := Pts + 2;
              if (Pr <> prNotAllowed) and (Pts <= Pt - 1) and (Pts >= Pt - 2) then begin
                case SignAlg of
                  saAnon:   Pr := SignatureAnon;
                  saRSA:    Pr := SignatureRSA;
                  saDSS:    Pr := SignatureDSS;
                  saECDSA:  Pr := SignatureECDSA;
                else
                  Pr := prNotAllowed;
                end;
                if Pr = prAllowed then
                  Pts := Pts + 1
                else if Pr = prPrefer then
                  Pts := Pts + 2;
                if (Pr <> prNotAllowed) and (Pts = Pt) then begin
                  SetLength(Result,Length(Result) + 1);
                  Result[Length(Result) - 1] := CS;
                  Include(Taken,I);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

constructor TTLSOptions.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FBulkCipherAES128 := prNotAllowed;
  FBulkCipherAES256 := prNotAllowed;
  FBulkCipherARC4 := prPrefer;
  FBulkCipherDES := prNotAllowed;
  FBulkCipherTripleDES := prAllowed;
  FEphemeralDHKeySize := 1024;
  FEphemeralDHQSize := 160;
  FEphemeralRSAKeySize := 512;
  FExport40Bit := prNotAllowed;
  FExport56Bit := prNotAllowed;
  FKeyAgreementDH := prAllowed;
  FKeyAgreementDHE := prPrefer;
  FKeyAgreementECDH := prNotAllowed;
  FKeyAgreementRSA := prAllowed;
  FSignatureAnon := prNotAllowed;
  FSignatureDSS := prAllowed;
  FSignatureECDSA := prNotAllowed;
  FSignatureRSA := prPrefer;
  FHashAlgorithmMD5 := prNotAllowed;
  FHashAlgorithmSHA1 := prPrefer;
  FHashAlgorithmRipeMD160 := prNotAllowed;
  FHashAlgorithmSHA256 := prNotAllowed;   
  FHashAlgorithmSHA384 := prNotAllowed;
  FHashAlgorithmSHA512 := prNotAllowed;
  FRequestClientCertificate := True;
  FRequireClientCertificate := True;
  FOnlyStandardCipherSuites := True;
  FVerifyServerName := [vsnIP,vsnDNS,vsnURI];
  FCipherSuiteNames := TStringList.Create;
end;

destructor TTLSOptions.Destroy;
begin
  FCipherSuiteNames.Free;
  inherited;
end;

procedure TTLSOptions.DoChange;
begin
  if FUpdateCount > 0 then
    FChange := True
  else if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TTLSOptions.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount <= 0) and FChange then begin
    FChange := False;
    DoChange;
  end;
end;

function TTLSOptions.GetNamedPropValue(const APropName: string): LongInt;
begin
{$IFDEF D4}
  Result := GetOrdProp(Self,GetPropInfo(ClassInfo,APropName));
{$ELSE}
  Result := TypInfo.GetOrdProp(Self,APropName);
{$ENDIF}
end;

function TTLSOptions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TTLSOptions.RefreshCipherSuiteNames;
var
  I: Integer;
  OK: Boolean;
  CipherAlg:      TBulkCipherAlgorithm;
  HashAlg:        THashAlgorithm;
  KeyExchangeAlg: TKeyExchangeAlgorithm;
  SignAlg:        TSignatureAlgorithm;
  CSName: string;
begin
  FCipherSuiteNames.Clear;
  if Length(FCipherSuites) = 0 then begin
    DoChange;
    Exit;
  end;

  for I := 0 to Length(FCipherSuites) - 1 do begin
    OK := TLS_DecodeCipherSuite(FCipherSuites[I],CipherAlg,HashAlg,KeyExchangeAlg,SignAlg);
    if not (OK or OnlyStandardCipherSuites) then
      OK := (FCipherSuites[I,0] = 255) and
            TLS_DecodeXCipherSuite(FCipherSuites[I],CipherAlg,HashAlg,KeyExchangeAlg,SignAlg);
    if OK then begin
      if FCipherSuites[I,0] = 255 then
        CSName := 'X_TLS_'
      else
        CSName := 'TLS_';
      case KeyExchangeAlg of
        keaNULL:
          CSName := CSName + 'NULL_';
        keaRSA,
        keaRSA_Export,
        keaRSA1024:    CSName := CSName + 'RSA_';
        keaDH:
          case SignAlg of
            saAnon:  CSName := CSName + 'DH_anon_';
            saRSA:   CSName := CSName + 'DH_RSA_';
            saDSS:   CSName := CSName + 'DH_DSS_';
          end;
        keaDHE,
        keaDHE_Export,
        keaDHE1024:
          case SignAlg of
            saRSA:   CSName := CSName + 'DHE_RSA_';
            saDSS:   CSName := CSName + 'DHE_DSS_';
            saECDSA: CSName := CSName + 'DHE_ECDSA_';
          end;
        keaECDH:
          case SignAlg of
            saRSA:   CSName := CSName + 'ECDH_RSA_';
            saECDSA: CSName := CSName + 'ECDH_ECDSA_';
          end;
        keaECDHE:
          case SignAlg of
            saRSA:   CSName := CSName + 'ECDHE_RSA_';
            saDSS:   CSName := CSName + 'ECDHE_DSS_';
            saECDSA: CSName := CSName + 'ECDHE_ECDSA_';
          end;
      end;
      case CipherAlg of
        bcaNull:        CSName := CSName + 'WITH_NULL_';
        bcaRC4:         CSName := CSName + 'WITH_RC4_128_';
        bcaRC4_40:      CSName := CSName + 'EXPORT_WITH_RC4_40_';
        bcaRC4_56:      CSName := CSName + 'EXPORT1024_WITH_RC4_56_';
        bcaDES:         CSName := CSName + 'WITH_DES_CBC_';
        bcaDES40:       CSName := CSName + 'EXPORT_WITH_DES_CBC_';
        bcaDES56:       CSName := CSName + 'EXPORT1024_WITH_DES_CBC_';
        bca3DES:        CSName := CSName + 'WITH_3DES_EDE_CBC_';
        bcaAES128:      CSName := CSName + 'WITH_AES_128_CBC_';
        bcaAES256:      CSName := CSName + 'WITH_AES_256_CBC_';
        bcaAES192:      CSName := CSName + 'WITH_AES_192_CBC_';
        bcaTwoFish128:  CSName := CSName + 'WITH_TWOFISH_128_CBC_';
        bcaTwoFish192:  CSName := CSName + 'WITH_TWOFISH_192_CBC_';
        bcaTwoFish256:  CSName := CSName + 'WITH_TWOFISH_256_CBC_';
        bcaBlowFish128: CSName := CSName + 'WITH_BLOWFISH_128_CBC_';
        bcaAES128_CTR:  CSName := CSName + 'WITH_AES_128_CTR_';
        bcaAES192_CTR:  CSName := CSName + 'WITH_AES_192_CTR_';
        bcaAES256_CTR:  CSName := CSName + 'WITH_AES_256_CTR_';
        bcaTwoFish128_CTR:  CSName := CSName + 'WITH_TWOFISH_128_CTR_';
        bcaTwoFish192_CTR:  CSName := CSName + 'WITH_TWOFISH_192_CTR_';
        bcaTwoFish256_CTR:  CSName := CSName + 'WITH_TWOFISH_256_CTR_';
        bcaBlowFish128_CTR: CSName := CSName + 'WITH_BLOWFISH_128_CTR_';
        bcaHelix:       CSName := CSName + 'WITH_HELIX_128';
        bcaRC2_40:       CSName := CSName + 'EXPORT_WITH_RC2_CBC_40_';
      end;
      if CipherAlg <> bcaHelix then case HashAlg of
        haMD5:       CSName := CSName + 'MD5';
        haSHA1:      CSName := CSName + 'SHA';
        {$IFDEF RIPEMD160}
        haRipeMD160: CSName := CSName + 'RIPEMD160';
        {$ENDIF}
        {$IFDEF SHA256}
        haSHA256:    CSName := CSName + 'SHA256';
        {$ENDIF}
        {$IFDEF SHA512}
        haSHA384:    CSName := CSName + 'SHA384';
        haSHA512:    CSName := CSName + 'SHA512';
        {$ENDIF}
      end;
      FCipherSuiteNames.Add(Format('%s (%x,%x)',[CSName,FCipherSuites[I,0],FCipherSuites[I,1]]))
    end else
      FCipherSuiteNames.Add(Format('unknown (%x,%x)',[FCipherSuites[I,0],FCipherSuites[I,1]]))
  end;
  DoChange;
end;

procedure TTLSOptions.RefreshCipherSuites;
begin
  if not (csLoading in FOwner.ComponentState) then begin
    FCipherSuites := ComputeCipherSuites;
    RefreshCipherSuiteNames;
  end;
end;

procedure TTLSOptions.SetBulkCipherAES128(const Value: TPreference);
begin
  if Value <> FBulkCipherAES128 then begin
    FBulkCipherAES128 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherAES128CTR(const Value: TPreference);
begin 
  if Value <> FBulkCipherAES128CTR then begin
    FBulkCipherAES128CTR := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherAES192(const Value: TPreference);
begin
  if Value <> FBulkCipherAES192 then begin
    FBulkCipherAES192 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherAES192CTR(const Value: TPreference);
begin 
  if Value <> FBulkCipherAES192CTR then begin
    FBulkCipherAES192CTR := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherAES256(const Value: TPreference);
begin
  if Value <> FBulkCipherAES256 then begin
    FBulkCipherAES256 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherAES256CTR(const Value: TPreference);
begin 
  if Value <> FBulkCipherAES256CTR then begin
    FBulkCipherAES256CTR := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherARC4(const Value: TPreference);
begin
  if Value <> FBulkCipherARC4 then begin
    FBulkCipherARC4 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherBlowFish128(const Value: TPreference);
begin
  if Value <> FBulkCipherBlowFish128 then begin
    FBulkCipherBlowFish128 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherBlowFish128CTR(
  const Value: TPreference);
begin 
  if Value <> FBulkCipherBlowFish128CTR then begin
    FBulkCipherBlowFish128CTR := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherDES(const Value: TPreference);
begin
  if Value <> FBulkCipherDES then begin
    FBulkCipherDES := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherHelix(const Value: TPreference);
begin
  if Value <> FBulkCipherHelix then begin
    FBulkCipherHelix := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherRC2(const Value: TPreference);
begin
  if Value <> FBulkCipherRC2 then begin
    FBulkCipherRC2 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherTripleDES(const Value: TPreference);
begin
  if Value <> FBulkCipherTripleDES then begin
    FBulkCipherTripleDES := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherTwoFish128(const Value: TPreference);
begin
  if Value <> FBulkCipherTwoFish128 then begin
    FBulkCipherTwoFish128 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherTwoFish128CTR(const Value: TPreference);
begin 
  if Value <> FBulkCipherTwoFish128CTR then begin
    FBulkCipherTwoFish128CTR := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherTwoFish192(const Value: TPreference);
begin
  if Value <> FBulkCipherTwoFish192 then begin
    FBulkCipherTwoFish192 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherTwoFish192CTR(const Value: TPreference);
begin 
  if Value <> FBulkCipherTwoFish192CTR then begin
    FBulkCipherTwoFish192CTR := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherTwoFish256(const Value: TPreference);
begin
  if Value <> FBulkCipherTwoFish256 then begin
    FBulkCipherTwoFish256 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetBulkCipherTwoFish256CTR(const Value: TPreference);
begin 
  if Value <> FBulkCipherTwoFish256CTR then begin
    FBulkCipherTwoFish256CTR := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetCipherSuiteNames(const Value: TStrings);
begin
  // Not implemented
end;

procedure TTLSOptions.SetCipherSuites(const Value: TCipherSuites);
begin
  FCipherSuites := Value;
  RefreshCipherSuiteNames;
end;

procedure TTLSOptions.SetDualSockets(const Value: Boolean);
begin
  FDualSockets := Value;
end;

procedure TTLSOptions.SetEphemeralDHKeySize(const Value: Integer);
begin
  if Value <> FEphemeralDHKeySize then begin
    FEphemeralDHKeySize := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetEphemeralDHQSize(const Value: Integer);
begin
  if Value <> FEphemeralDHQSize then begin
    FEphemeralDHQSize := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetEphemeralECDHKeySize(const Value: TECKeySize);
begin
  if Value <> FEphemeralECDHKeySize then begin
    FEphemeralECDHKeySize := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetEphemeralRSAKeySize(const Value: Integer);
begin
  if Value <> FEphemeralRSAKeySize then begin
    FEphemeralRSAKeySize := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetExport40Bit(const Value: TPreference);
begin
  if Value <> FExport40Bit then begin
    FExport40Bit := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetExport56Bit(const Value: TPreference);
begin
  if Value <> FExport56Bit then begin
    FExport56Bit := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetHashAlgorithmMD5(const Value: TPreference);
begin
  if Value <> FHashAlgorithmMD5 then begin
    FHashAlgorithmMD5 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetHashAlgorithmRipeMD160(const Value: TPreference);
begin                             
  if Value <> FHashAlgorithmRipeMD160 then begin
    FHashAlgorithmRipeMD160 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetHashAlgorithmSHA1(const Value: TPreference);
begin
  if Value <> FHashAlgorithmSHA1 then begin
    FHashAlgorithmSHA1 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetHashAlgorithmSHA256(const Value: TPreference);
begin
  if Value <> FHashAlgorithmSHA256 then begin
    FHashAlgorithmSHA256 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetHashAlgorithmSHA384(const Value: TPreference);
begin
  if Value <> FHashAlgorithmSHA384 then begin
    FHashAlgorithmSHA384 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetHashAlgorithmSHA512(const Value: TPreference);
begin
  if Value <> FHashAlgorithmSHA512 then begin
    FHashAlgorithmSHA512 := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetKeyAgreementDH(const Value: TPreference);
begin
  if Value <> FKeyAgreementDH then begin
    FKeyAgreementDH := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetKeyAgreementDHE(const Value: TPreference);
begin
  if Value <> FKeyAgreementDHE then begin
    FKeyAgreementDHE := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetKeyAgreementECDH(const Value: TPreference);
begin
  if Value <> FKeyAgreementECDH then begin
    FKeyAgreementECDH := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetKeyAgreementECDHE(const Value: TPreference);
begin
  if Value <> FKeyAgreementECDHE then begin
    FKeyAgreementECDHE := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetKeyAgreementRSA(const Value: TPreference);
begin
  if Value <> FKeyAgreementRSA then begin
    FKeyAgreementRSA := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetNamedPropValue(const APropName: string;
  AValue: Integer);
begin
{$IFDEF D4}
  SetOrdProp(Self,GetPropInfo(ClassInfo,APropName),AValue);
{$ELSE}
  TypInfo.SetOrdProp(Self,APropName,AValue);
{$ENDIF}
end;

procedure TTLSOptions.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TTLSOptions.SetOnlyStandardCipherSuites(const Value: Boolean);
begin
  if Value <> FOnlyStandardCipherSuites then begin
    FOnlyStandardCipherSuites := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetRequestClientCertificate(const Value: Boolean);
begin
  if Value <> FRequestClientCertificate then begin
    FRequestClientCertificate := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetRequireClientCertificate(const Value: Boolean);
begin
  if Value <> FRequireClientCertificate then begin
    FRequireClientCertificate := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetSignatureAnon(const Value: TPreference);
begin
  if Value <> FSignatureAnon then begin
    FSignatureAnon := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetSignatureDSS(const Value: TPreference);
begin
  if Value <> FSignatureDSS then begin
    FSignatureDSS := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetSignatureECDSA(const Value: TPreference);
begin
  if Value <> FSignatureECDSA then begin
    FSignatureECDSA := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetSignatureRSA(const Value: TPreference);
begin
  if Value <> FSignatureRSA then begin
    FSignatureRSA := Value;
    RefreshCipherSuites;
  end;
end;

procedure TTLSOptions.SetVerifyServerName(const Value: TVerifyServerName);
begin
  FVerifyServerName := Value;
end;
{$ENDIF SHA1_AND_MD5}

function IsPublicKeyAlg(const OID: ObjectIdentifier): Boolean;
begin
  Result := (OID = rsaEncryption) or
            (OID = id_rsaes_oaep) or
            (OID = id_rsassa_pss) or
            (OID = id_rw) or
            (OID = dhPublicNumber) or
            (OID = id_dsa) or
            (OID = id_nr) or
            (OID = id_ecPublicKey) or
            (OID = id_ecnr);
end;

{ TPrivateKeyRing }

function TPrivateKeyRing.AddKey(Key: TMPSecretKey; KeyRing: TKeyRingEnum;
  var Index: Integer): Boolean;
var
  PKI: string;
  F, A: PASN1Struct;
  KeyRings: TKeyRings;
  KeyHandler: IKeyHandler;
  T: TDateTime;
  TempKey: TMPSecretKey;                               
{$IFDEF SHA1_AND_MD5}
  SessionID: string[32];
{$ENDIF SHA1_AND_MD5}
begin
  FManagedIndexLock.Acquire;
  try
    PKI := Key.PublicKeyIdentifier;
    KeyRings := [KeyRing];
    if InternalFindPrivateKey(Pointer(PKI),Length(PKI),F,Index,KeyRings) then
      Result := False
    else begin
      F := AllocatePrivateKey(KeyRing,KeyHandler,Index);
      Result := Assigned(KeyHandler);
      if Result then begin
        if KeyRing = krSession then begin
          T := Now + SessionKeyLifeSpan - HoursOffsetFromGMT/24;
          F^.EditField('expires',T);
          A := F^.FindField('recipientInfo');
          A^.TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,False,3);
{$IFDEF SHA1_AND_MD5}
          if (Key is TMPMasterSecret) then begin
            SessionID := TMPMasterSecret(Key).SessionID;
            A^.SetContent(SessionID[1],Length(SessionID));
            F := F^.FindField('contentEncryptionKey');
          end else
{$ENDIF SHA1_AND_MD5}
          if IsPublicKeyAlg(Key.Algorithm) then
            F := F^.FindField('ephemeralKey')
          else
            F := F^.FindField('contentEncryptionKey');
        end;
        TempKey := TMPSecretKeyClass(Key.ClassType).Create(F^,KeyHandler);
        try
          Result := TempKey.Assign(Key);
        finally
          TempKey.Free;
        end;
      end;
    end;
  finally
    FManagedIndexLock.Release;
  end;
end;

function TPrivateKeyRing.AddPrivateDLKey(const PrivKey: TDLPrivateKey;
  PublicKeyIdentifier: Pointer; PKILen: Integer; IsDSAKey,
  IsLongTermKey: Boolean): Integer;
var
  F: PASN1Struct;
  T: TDateTime;
  KR: TKeyRings;
begin
  if (FAllowPlainTextKeys or FAdminLoggedIn) or not IsLongTermKey then begin
    if PKILen > 0 then begin
      if IsLongTermKey then
        KR := [krLongTerm]
      else
        KR := [krSession];
      if InternalFindPrivateKey(PublicKeyIdentifier,PKILen,F,Result,KR) then begin
        if not IsLongTermKey then begin
          F := FPrivateKeyRing.FindField('sessionKeys')^.Items[Result];
          F.FindField('recipientInfo').TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,False,3);
          T := Now + SessionKeyLifeSpan - HoursOffsetFromGMT/24;
          F.EditField('expires',T);       
          F.EditField('originator',True);
          Result := F.Owner.IndexOf(F^);
          F := F.FindField('ephemeralKey');
        end;
      end else
        F := nil;
    end;

    if F = nil then begin
      if IsLongTermKey then begin
        F := FPrivateKeyRing.FindField('longTermKeys')^.AddField;
        Result := F.Owner.IndexOf(F^);
      end else begin
        F := FPrivateKeyRing.FindField('sessionKeys')^.AddField;  
        F.FindField('recipientInfo').TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,False,3);
        T := Now + SessionKeyLifeSpan - HoursOffsetFromGMT/24;
        F.EditField('expires',T);        
        F.EditField('originator',True);
        Result := F.Owner.IndexOf(F^);
        F := F.FindField('ephemeralKey');
      end;
    end;
    FOwner.TranslatePrivateDLKey(F,PrivKey,PublicKeyIdentifier,PKILen,IsDSAKey);
  end else
    Result := -1;
end;

function TPrivateKeyRing.AddPrivateECKey(const PrivKey: TECPrivateKey;
  PublicKeyIdentifier: Pointer; PKILen: Integer;
  IsLongTermKey: Boolean): Integer;
var
  F: PASN1Struct;
  T: TDateTime;
  KR: TKeyRings;
begin
  if (FAllowPlainTextKeys or FAdminLoggedIn) or not IsLongTermKey then begin
    if PKILen > 0 then begin
      if IsLongTermKey then
        KR := [krLongTerm]
      else
        KR := [krSession];
      if InternalFindPrivateKey(PublicKeyIdentifier,PKILen,F,Result,KR) then begin
        if not IsLongTermKey then begin
          F := FPrivateKeyRing.FindField('sessionKeys')^.Items[Result];
          T := Now + SessionKeyLifeSpan - HoursOffsetFromGMT/24;
          F.EditField('expires',T);
          Result := F.Owner.IndexOf(F^);
          F := F.FindField('ephemeralKey');
        end;
      end else
        F := nil;
    end;

    if F = nil then begin
      if IsLongTermKey then begin
        F := FPrivateKeyRing.FindField('longTermKeys')^.AddField;
        Result := F.Owner.IndexOf(F^);
      end else begin
        F := FPrivateKeyRing.FindField('sessionKeys')^.AddField;
        T := Now + SessionKeyLifeSpan - HoursOffsetFromGMT/24;
        F.EditField('expires',T);
        Result := F.Owner.IndexOf(F^);
        F := F.FindField('ephemeralKey');
      end;
    end;
    FOwner.TranslatePrivateECKey(F,PrivKey,PublicKeyIdentifier,PKILen);
  end else
    Result := -1;
end;

function TPrivateKeyRing.AddPrivateRSAKey(const PrivKey: TIFPrivateKey;
  PublicKeyIdentifier: Pointer; PKILen: Integer; IsLongTermKey,
  RSAES_OAEP: Boolean): Integer;
var
  F: PASN1Struct;
  T: Double;
  KR: TKeyRings;
begin
  Assert(PrivKey.Scheme in [ifRSA1, ifRSA2],'Not a RSA key');
  if (FAllowPlainTextKeys or FAdminLoggedIn) or not IsLongTermKey then begin
    if PKILen > 0 then begin
      if IsLongTermKey then
        KR := [krLongTerm]
      else
        KR := [krSession];
      if InternalFindPrivateKey(PublicKeyIdentifier,PKILen,F,Result,KR) then begin
        if not IsLongTermKey then begin
          F := FPrivateKeyRing.FindField('sessionKeys')^.Items[Result];
          F.FindField('recipientInfo').TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,False,3);
          T := Now + SessionKeyLifeSpan - HoursOffsetFromGMT/24;
          F.EditField('expires',T);
          F.EditField('originator',True);
          Result := F.Owner.IndexOf(F^);
          F := F.FindField('ephemeralKey');
        end;
      end else
        F := nil;
    end;

    if F = nil then begin
      if IsLongTermKey then begin
        F := FPrivateKeyRing.FindField('longTermKeys')^.AddField;
        Result := F.Owner.IndexOf(F^);
      end else begin
        F := FPrivateKeyRing.FindField('sessionKeys')^.AddField;
        F.FindField('recipientInfo').TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,False,3);
        T := Now + SessionKeyLifeSpan - HoursOffsetFromGMT/24;
        F.EditField('expires',T);        
        F.EditField('originator',True);
        Result := F.Owner.IndexOf(F^);
        F := F.FindField('ephemeralKey');
      end;
    end;
    FOwner.TranslatePrivateRSAKey(F,PrivKey,PublicKeyIdentifier,PKILen,RSAES_OAEP);
  end else
    Result := -1;
end;

procedure TPrivateKeyRing.AddToIndex(PublicKeyIdentifier: Pointer;
  PKILen: Integer);
var
  S: string;
begin
  SetLength(S,PKILen);
  Move(PublicKeyIdentifier^,Pointer(S)^,PKILen);
  FManagedIndex.Add(S);
end;

procedure TPrivateKeyRing.AddToIndex(Cert: TASN1Struct);
var
  S: string;
begin
  S := ExtractSubjectKeyIdentifier(Cert);
  UniqueString(S);
  FManagedIndex.Add(S);
end;

function TPrivateKeyRing.AdminLogin(Password: ISecretKey): Boolean;
var
  P, A: PASN1Struct;
  OID: string;
  AEKH: TEncryptedKeyHandler;
begin
  FManagedIndexLock.Acquire;
  try
    Result := CheckLogin;
    Result := Result and FAdminLoggedIn;
    if Assigned(Password) and not Result then begin
      P := FPrivateKeyRing.FindField('keySignKey');
      Result := Assigned(P);
      if Result then begin
        Assert(Assigned(P^));
        A := P^.FindField('/publicKey/algorithm/algorithm');
        Result := Assigned(A) and Assigned(A^) and (A^.Length > 0);
        if not Result then begin
          A := P^.FindField('/privateKey/privateKeyType');
          Result := Assigned(A) and Assigned(A^) and (A^.Length > 0);
        end;
        if Result then begin
          OID := A^.ContentAsOID;
          A := P^.FindField('privateKey');
          Result := Assigned(A) and Assigned(A^) and (A^.Length > 8);
        end;
        if Result then begin
          AEKH := TEncryptedKeyHandler.CreateIntf('AdminKey',Password);
          if (OID = rsaEncryption) or
             (OID = id_rsassa_pss) or
             (OID = id_rsaes_oaep) or
             (OID = id_rw) then
            FSignedEncryptedKeyHandler.SignaturePrivateKey :=
              MpIF.TMPIFPrivateKey.Create(A^,AEKH)
          else if (OID = id_dsa) or
                  (OID = id_nr) then
            FSignedEncryptedKeyHandler.SignaturePrivateKey :=
              MpDL.TMPDLPrivateKey.Create(A^,AEKH)
          else if (OID = id_ecPublicKey) or
                  (OID = id_ecnr) then
            FSignedEncryptedKeyHandler.SignaturePrivateKey :=
              MpEC.TMPECPrivateKey.Create(A^,AEKH)
          else
            Result := False;
          if Result then
            Result := FSignedEncryptedKeyHandler.SignaturePrivateKey.SignatureLength > 0;
          if Result and (FAdminPubl = nil) then begin
            FAdminPubl := TSubjectPublicKeyInfo.Create(nil,nil);
            FSignedEncryptedKeyHandler.SignaturePrivateKey.PublKey := FAdminPubl;
            if FAdminPubl.Algorithm.Algorithm <> '' then begin
              P^.FindField('publicKey')^.Assign(FAdminPubl.Data);
              FSignedEncryptedKeyHandler.SignaturePublicKey :=
                CreateMPPublicKey(FAdminPubl);
            end;
          end;
        end;
      end;
      FAdminLoggedIn := Result;
    end;
  finally
    FManagedIndexLock.Release;
  end;
end;

procedure TPrivateKeyRing.AdminLogout;
begin
  FManagedIndexLock.Acquire;
  try
    if FAdminLoggedIn then begin
      FAdminLoggedIn := False;
      if Assigned(FSignedEncryptedKeyHandler) then
        FSignedEncryptedKeyHandler.SignaturePrivateKey := nil;
    end;
  finally
    FManagedIndexLock.Release;
  end;
end;

function TPrivateKeyRing.AllocatePrivateKey(
  KeyRing: TKeyRingEnum; var KeyHandler: IKeyHandler;
  var Index: Integer): PASN1Struct;
begin
  case KeyRing of
    krLongTerm:        Result := FPrivateKeyRing.FindField('longTermKeys')^.AddField;
    krSession:         Result := FPrivateKeyRing.FindField('sessionKeys')^.AddField;
    krEncrypted:       Result := FPrivateKeyRing.FindField('encryptedKeys')^.AddField;
    krSignedEncrypted: Result := FPrivateKeyRing.FindField('signedEncryptedKeys')^.AddField;
  else
    Result := nil;
  end;
  if Result <> nil then
    Index := Result^.Owner.IndexOf(Result^);
  case KeyRing of
    krLongTerm:        KeyHandler := FPlainTextKeyHandler;
    krSession:         KeyHandler := FPlainTextKeyHandler;
    krEncrypted:       KeyHandler := FEncryptedKeyHandler;
    krSignedEncrypted: KeyHandler := FSignedEncryptedKeyHandler;
  else
    KeyHandler := nil;
  end;
end;

constructor TPrivateKeyRing.Create;
begin
  Assert(Assigned(GlobalObject),'StreamSecII not initialized: Call StrSecInit.InitStreamSecII to correct.');
  FOwner := AOwner;
  FPrivateKeyRing := TASN1Struct.Create;
  FPrivateKeyRing.CopyTypeInfo(GlobalObject);
  FPlainTextKeyHandler := TKeyHandler.Create;
  FManagedKeys := TInterfaceList.Create;
  FManagedIndex := TStringList.Create;
  FManagedIndexLock := TCriticalSection.Create;
end;

function TPrivateKeyRing.CreateAdminKeyPairDSA(Password: ISecretKey; PSize,
  QSize: Cardinal; SignatureDigestAlg: THashAlgorithm): Boolean;
var
  F: PASN1Struct;
  SPK: TMPPrivateKey;
  AEKH: TEncryptedKeyHandler;
  APK: TSubjectPublicKeyInfo;
begin
  FManagedIndexLock.Acquire;
  try
    Result := (FAdminPubl = nil) and Assigned(FSignedEncryptedKeyHandler);
    if Result then begin
      F := FPrivateKeyRing.FindField('keySignKey');
      APK := TSubjectPublicKeyInfo.Create(nil,nil);
      FAdminPubl := APK;
      AEKH := TEncryptedKeyHandler.CreateIntf('AdminKey',Password);
      SPK := MpDL.TMPDLPrivateKey.CreateNew(id_dsa,PSize,QSize,
                                            F^.FindField('privateKey')^,
                                            AEKH,
                                            SignatureDigestAlg,
                                            APK);
      F^.FindField('publicKey')^.Assign(APK.Data);
      SPK.SignatureDigestAlg := SignatureDigestAlg;
      FSignedEncryptedKeyHandler.SignaturePrivateKey := SPK;
      FSignedEncryptedKeyHandler.SignaturePublicKey := MpDL.TMPDLPublicKey.Create(APK);
      FAdminLoggedIn := True;
    end;
  finally
    FManagedIndexLock.Release;
  end;
end;

function TPrivateKeyRing.CreateAdminKeyPairECDSA(Password: ISecretKey;
  Curve: ObjectIdentifier; SignatureDigestAlg: THashAlgorithm): Boolean;
var
  F: PASN1Struct;
  SPK: TMPPrivateKey;
  AEKH: TEncryptedKeyHandler;
  APK: TSubjectPublicKeyInfo;
begin
  FManagedIndexLock.Acquire;
  try
    Result := (FAdminPubl = nil) and Assigned(FSignedEncryptedKeyHandler);
    if Result then begin
      F := FPrivateKeyRing.FindField('keySignKey');
      APK := TSubjectPublicKeyInfo.Create(nil,nil);
      FAdminPubl := APK;
      AEKH := TEncryptedKeyHandler.CreateIntf('AdminKey',Password);
      SPK := MpEC.TMPECPrivateKey.CreateNew(id_ecPublicKey,Curve,
                                            F^.FindField('privateKey')^,
                                            AEKH,
                                            SignatureDigestAlg,
                                            APK);
      F^.FindField('publicKey')^.Assign(APK.Data);
      FSignedEncryptedKeyHandler.SignaturePrivateKey := SPK;
      FSignedEncryptedKeyHandler.SignaturePublicKey := MpEC.TMPECPublicKey.Create(APK);
      FAdminLoggedIn := True;
    end;
  finally
    FManagedIndexLock.Release;
  end;
end;

function TPrivateKeyRing.CreateAdminKeyPairRSA(Password: ISecretKey;
  KeySize: Cardinal; SignatureDigestAlg: THashAlgorithm): Boolean;
var
  F: PASN1Struct;
  SPK: TMPPrivateKey;
  AEKH: TEncryptedKeyHandler;
  APK: TSubjectPublicKeyInfo;
begin
  FManagedIndexLock.Acquire;
  try
    Result := (FAdminPubl = nil) and Assigned(FSignedEncryptedKeyHandler);
    if Result then begin
      F := FPrivateKeyRing.FindField('keySignKey');
      APK := TSubjectPublicKeyInfo.Create(nil,nil);
      FAdminPubl := APK;
      AEKH := TEncryptedKeyHandler.CreateIntf('AdminKey',Password);
      SPK := MpIF.TMPIFPrivateKey.CreateNew(id_rsassa_pss,KeySize,
                                            F^.FindField('privateKey')^,
                                            AEKH,
                                            SignatureDigestAlg,
                                            APK);
      F^.FindField('publicKey')^.Assign(APK.Data);
      FSignedEncryptedKeyHandler.SignaturePrivateKey := SPK;
      FSignedEncryptedKeyHandler.SignaturePublicKey := MpIF.TMPIFPublicKey.Create(APK);
      FAdminLoggedIn := True;
    end;
  finally
    FManagedIndexLock.Release;
  end;
end;

function TPrivateKeyRing.CreateKeyPairDL(const Algorithm: ObjectIdentifier;
  PSize, QSize: Cardinal;
  PublKey: IDLPublicKeyInfo;
  KeyRing: TKeyRingEnum;
  SignatureDigestAlg: THashAlgorithm): IMPDLPrivateKey;
var
  F, A: PASN1Struct;
  KeyHandler: IKeyHandler;
  T: TDateTime;
  Index: Integer;
begin
  if CheckAdminLogin then begin
    FManagedIndexLock.Acquire;
    try
      F := AllocatePrivateKey(KeyRing,KeyHandler,Index);
      if Assigned(KeyHandler) then begin
        if KeyRing = krSession then begin
          T := Now + SessionKeyLifeSpan - HoursOffsetFromGMT/24;
          F^.EditField('expires',T);
          A := F^.FindField('recipientInfo');
          A^.TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,False,3);
          F := F^.FindField('ephemeralKey');
        end;
        Result := TMPDLPrivateKey.CreateNew(Algorithm,PSize,QSize,
                                            F^,KeyHandler,
                                            SignatureDigestAlg,PublKey);
      end else
        Result := nil;
    finally
      FManagedIndexLock.Release;
    end;
  end else
    Result := nil;
end;

function TPrivateKeyRing.CreateKeyPairEC(const Algorithm: ObjectIdentifier;
  Curve: ObjectIdentifier;
  PublKey: IECPublicKeyInfo;
  KeyRing: TKeyRingEnum;
  SignatureDigestAlg: THashAlgorithm): IMPECPrivateKey;
var
  F, A: PASN1Struct;
  KeyHandler: IKeyHandler;
  T: TDateTime;
  Index: Integer;
begin                      
  if CheckAdminLogin then begin
    FManagedIndexLock.Acquire;
    try
      F := AllocatePrivateKey(KeyRing,KeyHandler,Index);
      if Assigned(KeyHandler) then begin
        if KeyRing = krSession then begin
          T := Now + SessionKeyLifeSpan - HoursOffsetFromGMT/24;
          F^.EditField('expires',T);
          A := F^.FindField('recipientInfo');
          A^.TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,False,3);
          F := F^.FindField('ephemeralKey');
        end;
        Result := TMPECPrivateKey.CreateNew(Algorithm,Curve,
                                            F^,KeyHandler,
                                            SignatureDigestAlg,PublKey);
      end else
        Result := nil;
    finally
      FManagedIndexLock.Release;
    end;
  end else
    Result := nil;
end;

function TPrivateKeyRing.CreateKeyPairIF(const Algorithm: ObjectIdentifier;
  KeySize: Cardinal; PublKey: IIFPublicKeyInfo;
  KeyRing: TKeyRingEnum;
  SignatureDigestAlg: THashAlgorithm; Exportable: Boolean): IMPIFPrivateKey;
var
  F, A: PASN1Struct;
  KeyHandler: IKeyHandler;
  T: TDateTime;
  Index: Integer;
begin
  if CheckAdminLogin then begin
    FManagedIndexLock.Acquire;
    try
      F := AllocatePrivateKey(KeyRing,KeyHandler,Index);
      if Assigned(KeyHandler) then begin
        if KeyRing = krSession then begin
          T := Now + SessionKeyLifeSpan - HoursOffsetFromGMT/24;
          F^.EditField('expires',T);
          A := F^.FindField('recipientInfo');
          A^.TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,False,3);
          F := F^.FindField('ephemeralKey');
        end;
        Result := MpIF.TMPIFPrivateKey.CreateNew(Algorithm,KeySize,
                                                 F^,KeyHandler,
                                                 SignatureDigestAlg,
                                                 PublKey,
                                                 Exportable);
      end else if FOwner.Exceptions then
        raise Exception.Create('No key handler assigned')
      else
        Result := nil;
    finally
      FManagedIndexLock.Release;
    end;
  end else if FOwner.Exceptions then
    raise Exception.Create('Could not login as admin')
  else
    Result := nil;
end;

procedure TPrivateKeyRing.DeleteSessionKey(index: Integer);
begin
  FPrivateKeyRing.FindField('sessionKeys').DeleteItem(index);
end;

destructor TPrivateKeyRing.Destroy;
var
  Debug: string;
begin
  try                                
    Debug := 'FOwner.FMyPrivateKeyRing := nil';
    if Assigned(FOwner) then
      FOwner.FMyPrivateKeyRing := nil;
    {$IFDEF FreeManagedKeys}
    if CacheKeyInterfaces then                               
      Debug := 'FreeManagedKeys (CacheKeyInterfaces = True)'
    else
      Debug := 'FreeManagedKeys (CacheKeyInterfaces = False)';
    FreeManagedKeys;
    {$ENDIF}                   
    Debug := 'FPrivateKeyRing.Free';
    FPrivateKeyRing.Free;
    Debug := 'FAdminPubl := nil';
    FAdminPubl := nil;
    Debug := 'FPlainTextKeyHandler := nil';
    FPlainTextKeyHandler := nil;
    Debug := 'FEncryptedKeyHandler := nil';
    FEncryptedKeyHandler := nil;
    Debug := 'FSignedEncryptedKeyHandler := nil';
    FSignedEncryptedKeyHandler := nil;
    Debug := 'FManagedKeys := nil';
    FManagedKeys := nil;
    Debug := 'FManagedIndexLock.Free';
    FManagedIndexLock.Free;
    Debug := 'FManagedIndex.Free';
    FManagedIndex.Free;
    Debug := 'inherited';
    inherited;
  except
    on E: Exception do
      raise Exception.Create('TPrivateKeyRing.Destroy: ' + Debug + #13#10 +
                             E.Message);
  end;
end;

function TPrivateKeyRing.FindCreatePrivateKey(Cert: TASN1Struct;
  var KeyRing: TKeyRings): IMPPrivateKey;
begin
  Result := FindCreatePrivateKeyObj(Cert,KeyRing);
end;

function TPrivateKeyRing.FindCreatePrivateKey(PublicKeyIdentifier: Pointer;
  PKILen: Integer; var Index: Integer;
  var KeyRing: TKeyRings): IKey;
begin
  Result := FindCreatePrivateKeyObj(PublicKeyIdentifier,PKILen,Index,KeyRing);
end;

function TPrivateKeyRing.FindCreatePrivateKeyObj(Cert: TASN1Struct;
  var KeyRing: TKeyRings; TLS: Boolean): TMPPrivateKey;
var
  Key, K: PASN1Struct;
  OID: ObjectIdentifier;
  KeyHandler: IKeyHandler;
  Intf: IMPPrivateKey;
begin
  FManagedIndexLock.Acquire;
  try
    if InternalFindPrivateKey(Cert,KeyRing,Key) then begin
      Intf := InternalIndexFind(Cert,TLS);
      if (Intf = nil) or (Intf.TLS <> TLS) then begin
        if KeyRing = [krSignedEncrypted] then begin
          K := Key^.FindField('privateKey');
          KeyHandler := FSignedEncryptedKeyHandler;
        end else begin
          K := Key;
          if KeyRing = [krEncrypted] then
            KeyHandler := FEncryptedKeyHandler
          else
            KeyHandler := FPlainTextKeyHandler;
        end;
        OID := K.FindField('privateKeyType').ContentAsOID;
        if (OID = rsaEncryption) or
           (OID = id_rsaes_oaep) or
           (OID = id_rsassa_pss) or
           (OID = id_rw) then begin
          if TLS then
            Result := MpIF.TMPIFPrivateKeyTLS.Create(Key^,KeyHandler)
          else
            Result := MpIF.TMPIFPrivateKey.Create(Key^,KeyHandler);
          TMPIFPrivateKey(Result).ExternalIFPrivateKeyClass := ExternalIFPrivateKeyClass;
        end else if (OID = dhPublicNumber) or
                    (OID = id_dsa) or
                    (OID = id_nr) then begin
          if TLS then
            Result := MpDL.TMPDLPrivateKeyTLS.Create(Key^,KeyHandler)
          else
            Result := MpDL.TMPDLPrivateKey.Create(Key^,KeyHandler);
        end else if (OID = id_ecPublicKey) or
                    (OID = id_ecnr) then
          Result := MpEC.TMPECPrivateKey.Create(Key^,KeyHandler)
        else
          Result := nil;
        if CacheKeyInterfaces and Assigned(Result) then begin
          Result.TLS := TLS;
          FManagedKeys.Add(Result);
          AddToIndex(Cert);
        end;
      end else
        Result := Intf.Instance as TMPPrivateKey;
    end else
      Result := nil;
  finally
    FManagedIndexLock.Release;
  end;
end;

function TPrivateKeyRing.FindCreatePrivateKeyObj(
  PublicKeyIdentifier: Pointer; PKILen: Integer; var Index: Integer;
  var KeyRing: TKeyRings; TLS: Boolean): TMPSecretKey;
var
  Key, K: PASN1Struct;
  OID, AlgOID: ObjectIdentifier;
  KeyHandler: IKeyHandler;
  NoCache: Boolean;
  Intf: IKey;
begin
  FManagedIndexLock.Acquire;
  try
    if InternalFindPrivateKey(PublicKeyIdentifier,PKILen,Key,Index,KeyRing) then begin
      Intf := InternalIndexFind(PublicKeyIdentifier,PKILen,TLS);
      if Intf = nil then begin
        if KeyRing = [krSignedEncrypted] then begin
          K := Key^.FindField('privateKey');
          KeyHandler := FSignedEncryptedKeyHandler;
        end else begin
          K := Key;
          if KeyRing = [krEncrypted] then
            KeyHandler := FEncryptedKeyHandler
          else
            KeyHandler := FPlainTextKeyHandler;
        end;
        OID := K.FindField('privateKeyType').ContentAsOID;
        AlgOID := K.FindField('privateKeyAlg').ContentAsOID;
        NoCache := False;
        if (OID = rsaEncryption) or
           (OID = id_rsaes_oaep) or
           (OID = id_rsassa_pss) or
           (OID = id_rw) then begin
          if TLS then
            Result := MpIF.TMPIFPrivateKeyTLS.Create(Key^,KeyHandler)
          else
            Result := MpIF.TMPIFPrivateKey.Create(Key^,KeyHandler);
        end else if (OID = dhPublicNumber) or
                    (OID = id_dsa) or
                    (OID = id_nr) then begin
          if TLS then
            Result := MpDL.TMPDLPrivateKeyTLS.Create(Key^,KeyHandler)
          else
            Result := MpDL.TMPDLPrivateKey.Create(Key^,KeyHandler);
        end else if (OID = id_ecPublicKey) or
                    (OID = id_ecnr) then
          Result := MpEC.TMPECPrivateKey.Create(Key^,KeyHandler)
        else if (OID <> '') and (AlgOID <> '') then begin
          NoCache := True; // Can't reuse
          Result := TMPCipher.Create(Key^,KeyHandler)
        end else
          Result := TMPKeyData.Create(Key^,KeyHandler);
        if CacheKeyInterfaces and Assigned(Result) and not NoCache then begin
          if Result is TMPPrivateKey then
            TMPPrivateKey(Result).TLS := TLS;
          FManagedKeys.Add(Result);
          AddToIndex(PublicKeyIdentifier,PKILen);
        end;
      end else
        Result := Intf.Instance as TMPSecretKey;
    end else
      Result := nil;
  finally
    FManagedIndexLock.Release;
  end;
end;
                 
{$IFDEF SHA1_AND_MD5}
function TPrivateKeyRing.FindTLSMasterSecret(const SessionID: TSessionID;
  var Index: Integer): ISecretKey;
var
  I: Integer;
  F, K, A: PASN1Struct;
  OK: Boolean;
begin
  Result := nil;
  if SessionID = '' then
    Index := -1
  else begin
    FManagedIndexLock.Acquire;
    try
      Index := -1;
      F := FPrivateKeyRing.FindField('sessionKeys');
      if Assigned(F) then begin
        for I := 0 to F^.ItemCount - 1 do begin
          K := F^.Items[I];
          A := K.FindField('recipientInfo');
          if Assigned(A) then begin
            if A^.Tag = 3 then begin
              OK := A^.Length = Length(SessionID);
              if OK then
                OK := CompareMem(@SessionID[1],A^.Content,Length(SessionID));
              if OK then begin
                F := K^.FindField('expires');
                OK := Assigned(F) and
                      (F^.ContentAsDateTime > Now - HoursOffsetFromGMT/24);
                if OK then begin
                  A := K^.FindField('contentEncryptionKey');
                  F := A^.FindField('/privateKey/');
                  OK := Assigned(F) and (F^.Length = 48);
                  if OK then begin
                    Index := I;
                    Result := TMPMasterSecret.Create(A^,FPlainTextKeyHandler);
                  end;
                end;
                Break;
              end;
            end;
          end;
        end;
      end;
    finally
      FManagedIndexLock.Release;
    end;
  end;
end;   
{$ENDIF SHA1_AND_MD5}

procedure TPrivateKeyRing.FreeManagedKeys;
begin
  if FManagedKeys.Count > 0 then
    FManagedKeys.Clear;
end;

function TPrivateKeyRing.FreePrivateKey(APrivKey: IKey): Boolean;
var
  Idx: Integer;
begin
  FManagedIndexLock.Acquire;
  try
    Idx := FManagedKeys.Remove(APrivKey);
    Result := Idx >= 0;
    if Result then
      FManagedIndex.Delete(Idx);
  finally
    FManagedIndexLock.Release;
  end;
end;

function CompareIden(PKI: Pointer; PKILen: Integer; Key: PASN1Struct): Boolean;
var
  K: PASN1Struct;
  Iden, Iden2: string;
begin
  K := Key.FindField('/privateKey//publicKeyIdentifier');
  Result := Assigned(K) and (K.Length > 0);
  if Result then
    Iden := K^.ContentAsOctetString
  else
    Iden := '';
  Iden2 := '';
  K := Key.FindField('identifier');
  if Assigned(K) and (K.Length > 0) then begin
    Result := True;
    if Iden = '' then
      Iden := K.ContentAsOctetString
    else
      Iden2 := K.ContentAsOctetString;
  end;
  if Result then begin
    Result := (Length(Iden) = PKILen) and
              CompareMem(Pointer(Iden),PKI,PKILen);
    if (Iden2 <> '') and not Result then
      Result := (Length(Iden2) = PKILen) and
                CompareMem(Pointer(Iden2),PKI,PKILen);
  end;
end;

function TPrivateKeyRing.InternalFindPrivateKey(Cert: TASN1Struct;
  var KeyRing: TKeyRings; var Key: PASN1Struct): Boolean;
var
  S: string;
  PK: TX509PublicKey;
  KR: TKeyRingEnum;
  I: Integer;
begin
  Key := nil;
  PK := nil;
  try
    Result := ExtractSubjectPublicKey(Cert,PK) = E_OK;
    if Result then begin
      S := ExtractSubjectKeyIdentifier(Cert);
{$IFDEF SHA1}
      if S = '' then
        S := DigestString(haSHA1,PK.PublicKey.ContentAsOctetString);
{$ELSE}
      if S = '' then
        raise Exception.Create('No subject key identifier');
{$ENDIF SHA1}
      for KR := Low(TKeyRingEnum) to High(TKeyRingEnum) do
        if KR in KeyRing then begin
          Result := InternalFindPrivateKey(Pointer(S),Length(S),
                                           PK.Algorithm.Algorithm,
                                           Key,I,KR);
          if Result then begin
            KeyRing := [KR];
            Break;
          end;
        end;
    end;
  finally
    PK.Free;
  end;
end;

function TPrivateKeyRing.InternalFindPrivateKey(
  PublicKeyIdentifier: Pointer; PKILen: Integer; var Key: PASN1Struct;
  var Index: Integer; var KeyRing: TKeyRings): Boolean;
var
  KR: TKeyRingEnum;
begin
  Result := False;
  for KR := Low(TKeyRingEnum) to High(TKeyRingEnum) do
    if KR in KeyRing then begin
      Result := InternalFindPrivateKey(PublicKeyIdentifier,PKILen,
                                       '',
                                       Key,Index,KR);
      if Result then begin
        KeyRing := [KR];
        Break;
      end;
    end;
end;

function TPrivateKeyRing.GetLongTermKeyCount: Integer;
begin
  Result := FPrivateKeyRing.FindField('longTermKeys').ItemCount;
end;

function TPrivateKeyRing.GetLongTermKeys(index: Integer): TASN1Struct;
begin
  Result := FPrivateKeyRing.FindField('longTermKeys').Items[index]^;
end;

function TPrivateKeyRing.GetSequenceNumber: Integer;
begin
  Result := FPrivateKeyRing.FindField('sequenceNumber').ContentAsInteger;
end;

function TPrivateKeyRing.GetSessionKeyCount: Integer;
begin
  Result := FPrivateKeyRing.FindField('sessionKeys').ItemCount;
end;

function TPrivateKeyRing.GetSessionKeys(index: Integer): TASN1Struct;
begin
  Result := FPrivateKeyRing.FindField('sessionKeys').Items[index]^;
end;

function TPrivateKeyRing.InternalFindPrivateKey(
  PublicKeyIdentifier: Pointer; PKILen: Integer;
  const Algorithm: ObjectIdentifier; var Key: PASN1Struct;
  var Index: Integer; KeyRing: TKeyRingEnum): Boolean;
var
  F, K, T: PASN1Struct;
  I: Integer;
begin
  case KeyRing of
    krLongTerm:        F := FPrivateKeyRing.FindField('longTermKeys');
    krSession:         F := FPrivateKeyRing.FindField('sessionKeys');
    krSignedEncrypted: F := FPrivateKeyRing.FindField('signedEncryptedKeys');
    krEncrypted:       F := FPrivateKeyRing.FindField('encryptedKeys');
  else
    F := nil;
  end;
  Result := Assigned(F);
  if Result then begin
    Result := False;
    for I := 0 to F^.ItemCount - 1 do begin
      Key := F^.Items[I];
      if KeyRing = krSignedEncrypted then
        K := Key.FindField('privateKey')      // Return item, check this field
      else if KeyRing = krSession then begin
        T := nil;
        if not IsPublicKeyAlg(Algorithm) then
          T := Key.FindField('contentEncryptionKey'); // Return and check this field;
        if (T = nil) or T.IsEmpty then
          T := Key.FindField('ephemeralKey'); // Return and check this field
        Key := T;
        K := T;
      end else
        K := Key;                              // Return and check item
      Assert(Assigned(K));
      if (Algorithm = '') or
         (K^.FindField('privateKeyType').ContentAsOID = Algorithm) then begin
        Result := CompareIden(PublicKeyIdentifier,PKILen,K);
        if Result then begin
          Index := I;
          Break;
        end;
      end;
      Key := nil;
    end;
  end;
end;

function TPrivateKeyRing.InternalIndexFind(PublicKeyIdentifier: Pointer;
  PKILen: Integer; TLS: Boolean): IKey;
var
  S: string;
  Idx: Integer;
begin
  SetLength(S,PKILen);
  Move(PublicKeyIdentifier^,Pointer(S)^,PKILen);
  Idx := FManagedIndex.IndexOf(S);
  if Idx >= 0 then begin
    FManagedKeys[Idx].QueryInterface(IMPPrivateKey,Result);
    if (Result as IMPPrivateKey).TLS <> TLS then
      while Idx < FManagedIndex.Count - 1 do begin
        Inc(Idx);
        if FManagedIndex[Idx] = S then begin
          FManagedKeys[Idx].QueryInterface(IMPPrivateKey,Result);
          if (Result as IMPPrivateKey).TLS = TLS then Break;
        end;
      end;
  end else
    Result := nil;
end;

function TPrivateKeyRing.InternalIndexFind(Cert: TASN1Struct; TLS: Boolean): IMPPrivateKey;
var
  Idx: Integer;
  Iden: string;
begin
  Iden := ExtractSubjectKeyIdentifier(Cert);
  Idx := FManagedIndex.IndexOf(Iden);
  if Idx >= 0 then begin
    FManagedKeys[Idx].QueryInterface(IMPPrivateKey,Result);
    if Result.TLS <> TLS then
      while Idx < FManagedIndex.Count - 1 do begin
        Inc(Idx);
        if FManagedIndex[Idx] = Iden then begin
          FManagedKeys[Idx].QueryInterface(IMPPrivateKey,Result);
          if Result.TLS = TLS then Break;
        end;
      end;
  end else
    Result := nil;
end;

function TPrivateKeyRing.LoadPrivateKeyRingFromStream(AStream: TStream;
  Password: ISecretKey): Boolean;
var
  MS: TSecureMemoryStream;
  S, P: PASN1Struct;
begin
  if Password.KeyLen <= 0 then
    raise Exception.Create('Password is empty');
  Assert(Assigned(GlobalObject),'StreamSecII not initialized: Call StrSecInit.InitStreamSecII to correct.');

  FSignedEncryptedKeyHandler := nil;
  FEncryptedKeyHandler := nil;
  FEncryptedKeyHandler := TEncryptedKeyHandler.CreateIntf('Key Cipher',Password);
  FSignedEncryptedKeyHandler := TSignedEncryptedKeyHandler.CreateIntf('Key Cipher',Password);

  MS := TSecureMemoryStream.Create;
  try
    Result := FOwner.LoadPrivateContentFromStream(AStream,MS,
                                                  Password,
                                                  FCID);
    if Result then begin
      MS.Position := 0;
      FPrivateKeyRing.Free;
      FPrivateKeyRing := TASN1Struct.Create;
      FPrivateKeyRing.CopyTypeInfo(GlobalObject);
      FPrivateKeyRing.LoadFromStream(MS,fmtDER);

      P := FPrivateKeyRing.FindField('/keySignKey/publicKey');
      FAdminPubl := nil;
      if Assigned(P) and not P^.IsEmpty then begin
        FAdminPubl := TSubjectPublicKeyInfo.Create(nil,nil);
        FAdminPubl.AssignStruct(P^);
        FSignedEncryptedKeyHandler.SignaturePublicKey :=
          CreateMPPublicKey(FAdminPubl);
      end else begin
        S := FPrivateKeyRing.FindField('/keySignKey/privateKey');
        if not S^.IsEmpty then begin
          if CheckAdminLogin then
            AdminLogout;
        end else
          FAdminPubl := nil;
      end;
    end;
  finally
    MS.Free;
  end;
end;

procedure TPrivateKeyRing.SavePrivateKeyRingToStream(Stream: TStream;
  Password: ISecretKey; KDF: TKeyDerivation; KDFIterations: Integer;
  KeyValidation: Boolean; KEKAlgorithm: string; CipherClass: TCipherClass);
var
  MS: TSecureMemoryStream;
  KeyedIV: Boolean;
  EncAlg: string;
  Res: Boolean;
begin
  Assert(KDFIterations > 0);
  Assert(Password <> nil);
  Assert(CipherClass <> nil);
  if CipherClass.InheritsFrom(TBlockCipher) then begin
    KeyedIV := CipherClass.Mode in [cmABC,cmPCFB,cmPipedPCFB,cmCTR];
    if KeyedIV then
      Res := CipherClassToOID(TBlockCipherClass(CipherClass).MaxKeySize +
                              TBlockCipherClass(CipherClass).BlockVectorSize *
                              TBlockCipherClass(CipherClass).BlockSize ,
                              CipherClass,KeyedIV,
                              EncAlg)
    else
      Res := CipherClassToOID(32,CipherClass,False,EncAlg);
  end else
    Res := CipherClassToOID(32,CipherClass,False,EncAlg);
  Assert(Res,'Not a supported encryption algorithm');

  MS := TSecureMemoryStream.Create;
  try
    FPrivateKeyRing.CalculateLength;
    FPrivateKeyRing.SaveToStream(MS,fmtDER);
    MS.Position := 0;
    FOwner.SavePrivateContentToStream(Stream,MS,
                                      Password,
                                      FCID,
                                      KDF,KDFIterations,KeyValidation,
                                      KEKAlgorithm,
                                      CipherClass);
  finally
    MS.Free;
  end;
end;

procedure TPrivateKeyRing.SetAllowPlainTextKeys(const Value: Boolean);
begin
  FAllowPlainTextKeys := Value;
end;

procedure TPrivateKeyRing.SetExternalIFPrivateKeyClass(
  const Value: TExternalIFPrivateKeyClass);
begin
  FExternalIFPrivateKeyClass := Value;
end;

procedure TPrivateKeyRing.SetHoursOffsetFromGMT(const Value: Double);
begin
  FHoursOffsetFromGMT := Value;
end;

procedure TPrivateKeyRing.SetLongTermKeys(index: Integer;
  const Value: TASN1Struct);
begin
  CopyASN1Struct(FPrivateKeyRing.FindField('longTermKeys').Items[index]^,
                 Value);
end;

procedure TPrivateKeyRing.SetSequenceNumber(const Value: Integer);
begin
  FPrivateKeyRing.EditField('sequenceNumber',Value);
end;

procedure TPrivateKeyRing.SetSessionKeyLifeSpan(const Value: TTime);
begin
  FSessionKeyLifeSpan := Value;
end;

procedure TPrivateKeyRing.SetSessionKeys(index: Integer;
  const Value: TASN1Struct);
begin
  CopyASN1Struct(FPrivateKeyRing.FindField('sessionKeys').Items[index]^,
                 Value);
end;

function TPrivateKeyRing.CheckAdminLogin: Boolean;
var
  PW: ISecretKey;
begin
  Result := AdminLogin(nil);
  if not Result then begin
    FOwner.DoAdminPassword(PW);
    Result := Assigned(PW) and (PW.KeyLen > 0);
    if Result then begin
      Result := AdminLogin(PW);
{$IFDEF SHA1}
      if not Result then
        Result := CreateAdminKeyPairRSA(PW);
{$ENDIF SHA1}
    end;
  end;
end;

function TPrivateKeyRing.CheckLogin: Boolean;
var
  PW: ISecretKey;
begin
  Result := Assigned(FSignedEncryptedKeyHandler);
  if not Result then begin
    FOwner.DoPassword(PW);
    Result := Assigned(PW) and (PW.KeyLen > 0);
    if Result then begin
      FEncryptedKeyHandler := TEncryptedKeyHandler.CreateIntf('Key Cipher',PW);
      FSignedEncryptedKeyHandler := TSignedEncryptedKeyHandler.CreateIntf('Key Cipher',PW);
    end;
  end;
end;

procedure TPrivateKeyRing.SetCacheKeyInterfaces(const Value: Boolean);
begin
  FCacheKeyInterfaces := Value;
end;

{ TCipherKeyHandler }

procedure TCipherKeyHandler.InitStruct(F: TASN1Struct);
begin
  F.Tag := V_ASN1_SEQUENCE;
  F.Constructed := True;
  F.AddField('privateKeyType','OBJECT',nil);
  F.AddField('identifier','OCTET STRING OPTIONAL',nil);
  F.AddField('privateKeyAlg','OBJECT OPTIONAL',nil);
  F.AddField('privateKey','[0] OCTET STRING OPTIONAL',nil);
end;

{ TMPCipher }

constructor TMPCipher.Create(AKeyStruct: TASN1Struct;
  AKeyHandler: IKeyHandler);
var
  vKeyStruct: TASN1Struct;
begin
  if AKeyStruct = nil then begin
    vKeyStruct := TASN1Struct.Create;
    if AKeyHandler = nil then
      AKeyHandler := TCipherKeyHandler.Create;
    AKeyHandler.InitStruct(vKeyStruct);
  end else begin
    vKeyStruct := AKeyStruct;
    Assert(Assigned(AKeyHandler));
  end;
  inherited Create(vKeyStruct,AKeyHandler);
end;

constructor TMPCipher.CreateKey(AKey: ISecretKey; AKeyStruct: TASN1Struct;
  AKeyHandler: IKeyHandler);
begin
  FKey := AKey;
  Create(AKeyStruct,AKeyHandler);
  if Assigned(AKeyHandler) then
    if not EncodeNewKey then
      raise Exception.Create('TMPCipher.CreateKey: Could not encode key');
end;

function TMPCipher.DecodeASNKey(K: TASN1Struct): Boolean;
var
  CC: TCipherClass;
  KeyedIV: Boolean;
  KeyLength: Integer;
  OID, fOID: string;
begin
  FCipherAlg := KeyStruct.FindField('privateKeyAlg').ContentAsOID;
  Result := OIDToCipherClass(FCipherAlg,KeyedIV,KeyLength,CC);
  if Result then begin
    if KeyedIV then
      Result := K.Length = KeyLength +
                           TBlockCipherClass(CC).BlockVectorSize * CC.BlockSize
    else
      Result := KeyLength = K.Length;
    if Result then begin
      fOID := KeyStruct.FindField('privateKeyType').ContentAsOID;
      if CC.InheritsFrom(TRijndael_ECB) then
        OID := id_aes
      else if CC.InheritsFrom(TTwoFish_ECB) then
        OID := id_twofish
      else if CC.InheritsFrom(TBlowFish_ECB) then
        OID := id_blowfish
      else if CC.InheritsFrom(T3DES_ECB) then
        OID := tripleDES
      else if CC.InheritsFrom(TARC4) then
        OID := arcfour
      else
        OID := '';
      Result := OID = fOID;
      if Result then begin
        SetAlgorithm(OID);
        if KeyedIV then
          FCipher := CC.Create(K.Content^,K.Length,TBlockCipherClass(CC).BlockVectorSize)
        else
          FCipher := CC.Create(K.Content^,K.Length,0);
      end;
    end;
  end;
end;

function TMPCipher.Decrypt(var Buf; Count: Integer): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      FCipher.Decrypt(Buf,Count);
  finally
    UnlockKey;
  end;
end;

function TMPCipher.DecryptPtr(Buf: Pointer; Count: Integer): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      FCipher.DecryptPtr(Buf,Count);
  finally
    UnlockKey;
  end;
end;

function TMPCipher.DecryptStr(const Buf: string): string;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FCipher.DecryptStr(Buf)
    else
      Result := '';
  finally
    UnlockKey;
  end;
end;

function TMPCipher.DecryptToPtr(Src, Dst: Pointer; Count: Integer): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      FCipher.DecryptToPtr(Src,Dst,Count);
  finally
    UnlockKey;
  end;
end;

procedure TMPCipher.DisposeKey;
begin
  inherited;
  FCipher.Free;
  FCipher := nil;
  FKey := nil;
end;

function TMPCipher.EncodeASNKey(K: TASN1Struct): Boolean;
var
  CC: TCipherClass;
  KeyedIV: Boolean;
  KeyLength: Integer;
  OID: string;
begin
  if OIDToCipherClass(FKey.Algorithm,KeyedIV,KeyLength,CC) then begin
    Result := KeyLength = FKey.KeyLen;
    if Result then begin
      FCipherAlg := FKey.Algorithm;
      FCipher := CC.CreateIntf(FKey);
      if CC.InheritsFrom(TRijndael_ECB) then
        OID := id_aes
      else if CC.InheritsFrom(TTwoFish_ECB) then
        OID := id_twofish
      else if CC.InheritsFrom(TBlowFish_ECB) then
        OID := id_blowfish
      else if CC.InheritsFrom(T3DES_ECB) then
        OID := tripleDES
      else if CC.InheritsFrom(TARC4) then
        OID := arcfour
      else
        OID := '';
      SetAlgorithm(OID);
      KeyStruct.EditField('privateKeyType',OID);
      KeyStruct.EditField('privateKeyAlg',FCipherAlg);
      K.SetContent(FKey.Key^,FKey.KeyLen);
      FKey := nil;
    end;
  end else
    Result := False;
end;

function TMPCipher.Encrypt(var Buf; Count: Integer): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      FCipher.Encrypt(Buf,Count);
  finally
    UnlockKey;
  end;
end;

function TMPCipher.EncryptPtr(Buf: Pointer; Count: Integer): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      FCipher.EncryptPtr(Buf,Count);
  finally
    UnlockKey;
  end;
end;

function TMPCipher.EncryptStr(const Buf: string): string;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FCipher.EncryptStr(Buf)
    else
      Result := '';
  finally
    UnlockKey;
  end;
end;

function TMPCipher.EncryptToPtr(Src, Dst: Pointer; Count: Integer): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      FCipher.EncryptToPtr(Src,Dst,Count);
  finally
    UnlockKey;
  end;
end;

function TMPCipher.GetAlgorithmName: PChar;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FCipher.AlgorithmName
    else
      Result := nil;
  finally
    UnlockKey;
  end;
end;

function TMPCipher.GetBlockSize: Integer;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FCipher.BlockSize
    else
      Result := 0;
  finally
    UnlockKey;
  end;
end;

function TMPCipher.GetCipherAlg: ObjectIdentifier;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FCipherAlg
    else
      Result := '';
  finally
    UnlockKey;
  end;
end;

function TMPCipher.GetCipherAlgorithm: TCipherAlg;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FCipher.Algorithm
    else
      Result := caUnknown;
  finally
    UnlockKey;
  end;
end;

function TMPCipher.GetMode: TCipherMode;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FCipher.Mode
    else
      Result := cmECB;
  finally
    UnlockKey;
  end;
end;

function TMPCipher.GetPKI: string;
begin
  Result := inherited GetPKI;
  if Result = '' then
    Result := Identifier;
end;

function TMPCipher.SetVectorBuf(const IV; Count: Integer): Boolean;
begin
  LockKey;
  try
    Result := KeyInitialized;
    if Result then
      FCipher.SetVectorBuf(IV,Count);
  finally
    UnlockKey;
  end;
end;

{ TMPKeyData }

procedure TMPKeyData.AddTruncKeyAt(AKey: ISecretKey; APos: Integer;
  IncVal: Byte);
begin
  FKey.AddTruncKeyAt(AKey,APos,IncVal);
  if KeyHandler <> nil then EncodeNewKey;
end;

constructor TMPKeyData.Create(AKeyStruct: TASN1Struct;
  AKeyHandler: IKeyHandler);
var
  vKeyStruct: TASN1Struct;
begin
  if AKeyStruct = nil then begin
    vKeyStruct := TASN1Struct.Create;
    if AKeyHandler = nil then
      AKeyHandler := TCipherKeyHandler.Create;
    AKeyHandler.InitStruct(vKeyStruct);
  end else begin
    vKeyStruct := AKeyStruct;
    Assert(Assigned(AKeyHandler));
  end;
  inherited Create(vKeyStruct,AKeyHandler);
end;

function TMPKeyData.DecodeASNKey(K: TASN1Struct): Boolean;
begin
  Result := K.Length > 0;
  if Result then begin
    FKey := TSecretKey.Create(KeyStruct.FindField('privateKeyType')^.ContentAsOID);
    FKey.SetKey(K.Content,K.Length,0);
  end else
    FKey := nil;
end;

procedure TMPKeyData.DisposeKey;
begin
  inherited;
  FKey := nil;
end;

function TMPKeyData.EncodeASNKey(K: TASN1Struct): Boolean;
begin
  Result := True;
  K.SetContent(FKey.Key^,FKey.KeyLen);
end;

function TMPKeyData.GetKey: Pointer;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FKey.Key
    else
      Result := nil;
  finally
    UnlockKey;
  end;
end;

function TMPKeyData.GetKeyBytes: SecUtils.PByteArray;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FKey.KeyBytes
    else
      Result := nil;
  finally
    UnlockKey;
  end;
end;

function TMPKeyData.GetKeyLen: Integer;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FKey.KeyLen
    else
      Result := -1;
  finally
    UnlockKey;
  end;
end;

function TMPKeyData.GetPKI: string;
begin
  Result := inherited GetPKI;
  if Result = '' then
    Result := Identifier;
end;

function TMPKeyData.GetVectorSize: Integer;
begin
  LockKey;
  try
    if KeyInitialized then
      Result := FKey.VectorSize
    else
      Result := -1;
  finally
    UnlockKey;
  end;
end;

procedure TMPKeyData.SetAlgorithm(const Value: ObjectIdentifier);
var
  CC: TCipherClass;
  KeyedIV: Boolean;
  KeyLen: Integer;
  VectorSize: Integer;
begin
  if OIDToCipherClass(Value,KeyedIV,KeyLen,CC) then begin
    if KeyedIV then
      VectorSize := TBlockCipherClass(CC).BlockVectorSize
    else
      VectorSize := 0;
    inherited SetAlgorithm(Value);
    if KeyedIV then
      SetKey(FKey.Key,FKey.KeyLen,VectorSize);
  end else                        
    inherited SetAlgorithm(Value);
end;

procedure TMPKeyData.SetKey(AKey: Pointer; AKeyLen, AVectorSize: Integer);
begin
  FKey := TSecretKey.Create(Algorithm);
  FKey.SetKey(AKey,AKeyLen,AVectorSize);
  if KeyHandler <> nil then EncodeNewKey;
end;

procedure TMPKeyData.SetKeyAt(AKey: ISecretKey; APos: Integer);
begin
  FKey.SetKeyAt(AKey,APos);
  if KeyHandler <> nil then EncodeNewKey;
end;

procedure TMPKeyData.SetKeyStr(const AKeyStr: string; ConvertToBMPStr: Boolean);
var
  W: WideString;
begin
  if ConvertToBMPStr then begin
    W := AKeyStr;
    FKey := TSecretKey.CreateBMPStr(PWideChar(W),Length(AKeyStr))
  end else
    FKey := TSecretKey.CreateStr(Pointer(AKeyStr),Length(AKeyStr));
  if KeyHandler <> nil then EncodeNewKey;
end;

procedure TMPKeyData.SetKeyStrAt(const AKeyStr: string; APos: Integer);
begin
  FKey.SetKeyStrAt(AKeyStr,APos);
  if KeyHandler <> nil then EncodeNewKey;
end;

procedure TMPKeyData.SetLength(AKeyLen: Integer);
begin
  FKey := TSecretKey.Create(Algorithm);
  FKey.SetLength(AKeyLen);
end;

procedure TMPKeyData.SetTruncKeyAt(AKey: ISecretKey; APos: Integer);
begin
  FKey.SetTruncKeyAt(AKey,APos);
  if KeyHandler <> nil then EncodeNewKey;
end;

procedure TMPKeyData.SetVectorSize(const Value: Integer);
begin
  FKey.VectorSize := Value;
end;

procedure TMPKeyData.XORKeyAt(AKey: ISecretKey; APos: Integer);
begin
  FKey.XORKeyAt(AKey,APos);
  if KeyHandler <> nil then EncodeNewKey;
end;

{$IFDEF SHA1_AND_MD5}
{ TMPMasterSecret }

constructor TMPMasterSecret.Create(AKeyStruct: TASN1Struct;
  AKeyHandler: IKeyHandler);
begin
  SetAlgorithm(id_tls);
  inherited;
end;

destructor TMPMasterSecret.Destroy;
begin
  ProtectClear(FSessionID,SizeOf(FSessionID));
  inherited;
end;

function TMPMasterSecret.GetPKI: string;
begin
  Result := 'TLS MasterSecret ' + SessionID;
end;

procedure TMPMasterSecret.SetSessionID(const Value: TSessionID);
begin
  FSessionID := Value;
end;   
{$ENDIF SHA1_AND_MD5}

{ TSsPrivateKeyRingComponent }

function TSsPrivateKeyRingComponent.AddKey(Key: TMPSecretKey;
  KeyRing: TKeyRingEnum; var Index: Integer): Boolean;
begin
  Result := FPrivateKeyRing.AddKey(Key,KeyRing,Index);
end;

function TSsPrivateKeyRingComponent.AddPrivate3DESKey(const PrivKey;
  PrivKeyLen: Integer; IsLongTermKey: Boolean;
  KeyIdentifier: string): Integer;
var
  K: TMPKeyData;
  OK: Boolean;
begin
  Assert(PrivKeyLen in [8,16,24,32,40],'Wrong key size for DES');
  K := TMPKeyData.Create(nil,nil);
  try
    K.Algorithm := tripleDES;
    K.SetLength(PrivKeyLen);
    K.Identifier := KeyIdentifier;
    K.SetKey(@PrivKey,PrivKeyLen,0);
    if IsLongTermKey then
      OK := FPrivateKeyRing.AddKey(K,krLongTerm,Result)
    else
      OK := FPrivateKeyRing.AddKey(K,krSession,Result);
    if not OK then Result := -1;
  finally
    K.Free;
  end;
end;

function TSsPrivateKeyRingComponent.AddPrivateAESKey(const PrivKey;
  PrivKeyLen: Integer; IsLongTermKey: Boolean;
  KeyIdentifier: string): Integer;
var
  K: TMPKeyData;
  OK: Boolean;
begin
  // Allow IV to be saved as key data (max two blocks)
  Assert(PrivKeyLen in [16,24,32,40,48,56,64],'Wrong key size for AES');
  K := TMPKeyData.Create(nil,nil);
  try
    K.Algorithm := id_aes;
    K.SetLength(PrivKeyLen);
    K.Identifier := KeyIdentifier;
    K.SetKey(@PrivKey,PrivKeyLen,0);
    if IsLongTermKey then
      OK := FPrivateKeyRing.AddKey(K,krLongTerm,Result)
    else
      OK := FPrivateKeyRing.AddKey(K,krSession,Result);
    if not OK then Result := -1;
  finally
    K.Free;
  end;
end;

function TSsPrivateKeyRingComponent.AddPrivateARC4Key(const PrivKey;
  PrivKeyLen: Integer; IsLongTermKey: Boolean;
  KeyIdentifier: string): Integer;
var
  K: TMPKeyData;
  OK: Boolean;
begin
  Assert(PrivKeyLen <= 256,'Wrong key size for ARC4');
  K := TMPKeyData.Create(nil,nil);
  try
    K.Algorithm := arcfour;
    K.SetLength(PrivKeyLen);
    K.Identifier := KeyIdentifier;
    K.SetKey(@PrivKey,PrivKeyLen,0);
    if IsLongTermKey then
      OK := FPrivateKeyRing.AddKey(K,krLongTerm,Result)
    else
      OK := FPrivateKeyRing.AddKey(K,krSession,Result);
    if not OK then Result := -1;
  finally
    K.Free;
  end;
end;

function TSsPrivateKeyRingComponent.AddPrivateBlowFishKey(const PrivKey;
  PrivKeyLen: Integer; IsLongTermKey: Boolean;
  KeyIdentifier: string): Integer;
var
  K: TMPKeyData;
  OK: Boolean;
begin
  // Allow IV to be saved as key data (max two blocks)
  Assert(PrivKeyLen in [8,16,24,32,40,48,56,64,72],'Wrong key size for BlowFish');
  K := TMPKeyData.Create(nil,nil);
  try
    K.Algorithm := id_blowfish;
    K.SetLength(PrivKeyLen);
    K.Identifier := KeyIdentifier;
    K.SetKey(@PrivKey,PrivKeyLen,0);
    if IsLongTermKey then
      OK := FPrivateKeyRing.AddKey(K,krLongTerm,Result)
    else
      OK := FPrivateKeyRing.AddKey(K,krSession,Result);
    if not OK then Result := -1;
  finally
    K.Free;
  end;
end;

function TSsPrivateKeyRingComponent.AddPrivateDLKey(
  const PrivKey: TDLPrivateKey; PublicKeyIdentifier: Pointer;
  PKILen: Integer; IsDSAKey, IsLongTermKey: Boolean): Integer;
begin
  Result := FPrivateKeyRing.AddPrivateDLKey(PrivKey,
                                            PublicKeyIdentifier,PKILen,
                                            IsDSAKey,IsLongTermKey);
end;

function TSsPrivateKeyRingComponent.AddPrivateECKey(
  const PrivKey: TECPrivateKey; PublicKeyIdentifier: Pointer;
  PKILen: Integer; IsLongTermKey: Boolean): Integer;
begin
  Result := FPrivateKeyRing.AddPrivateECKey(PrivKey,
                                            PublicKeyIdentifier,PKILen,
                                            IsLongTermKey);
end;

function TSsPrivateKeyRingComponent.AddPrivateRSAKey(
  const PrivKey: TIFPrivateKey; PublicKeyIdentifier: Pointer;
  PKILen: Integer; IsLongTermKey, RSAES_OAEP: Boolean): Integer;
begin
  Result := FPrivateKeyRing.AddPrivateRSAKey(PrivKey,
                                             PublicKeyIdentifier,PKILen,
                                             IsLongTermKey,RSAES_OAEP);
end;

function TSsPrivateKeyRingComponent.AddPrivateTwoFishKey(const PrivKey;
  PrivKeyLen: Integer; IsLongTermKey: Boolean;
  KeyIdentifier: string): Integer;
var
  K: TMPKeyData;
  OK: Boolean;
begin
  // Allow IV to be saved as key data (max two blocks)
  Assert(PrivKeyLen in [16,24,32,40,48,56,64],'Wrong key size for TwoFish');
  K := TMPKeyData.Create(nil,nil);
  try
    K.Algorithm := id_twofish;
    K.SetLength(PrivKeyLen);
    K.Identifier := KeyIdentifier;
    K.SetKey(@PrivKey,PrivKeyLen,0);
    if IsLongTermKey then
      OK := FPrivateKeyRing.AddKey(K,krLongTerm,Result)
    else
      OK := FPrivateKeyRing.AddKey(K,krSession,Result);
    if not OK then Result := -1;
  finally
    K.Free;
  end;
end;
                   
{$IFDEF SHA1_AND_MD5}
function TSsPrivateKeyRingComponent.AddTLSMasterSecret(
  const SessionID: TSessionID; const MasterSecret: TMasterSecret): Integer;
var
  K: TMPMasterSecret;
begin
  K := TMPMasterSecret.Create(nil,nil);
  K.SetLength(SizeOf(MasterSecret));
  K.SetKey(@MasterSecret,SizeOf(MasterSecret),0);
  K.SessionID := SessionID;
  if not FPrivateKeyRing.AddKey(K,krSession,Result) then
    Result := -1;
  K.Free;
end;
{$ENDIF SHA1_AND_MD5}

function TSsPrivateKeyRingComponent.AdminLogin(
  Password: ISecretKey): Boolean;
begin
  if Assigned(Password) then
    Result := FPrivateKeyRing.AdminLogin(Password)
  else
    Result := FPrivateKeyRing.CheckAdminLogin;
end;

function TSsPrivateKeyRingComponent.AdminLogin(
  const Password: string): Boolean;
var
  PW: ISecretKey;
begin
  PW := TSecretKey.Create('');
  PW.SetLength(Length(Password));
  PW.SetKeyStrAt(Password,0);
  Result := FPrivateKeyRing.AdminLogin(PW);
end;

procedure TSsPrivateKeyRingComponent.AdminLogout;
begin
  if Assigned(FPrivateKeyRing) then
    FPrivateKeyRing.AdminLogout;
end;

function TSsPrivateKeyRingComponent.CheckThreadID: Boolean;
var
  CTID: LongWord;
begin
  CTID := GetCurrentThreadID;
  Result := CTID = FCreationThreadID;
  if (FCreationThreadID <> MainThreadID) and not Result then
    raise Exception.Create('TStreamSecII.CheckThreadID: Called from the wrong thread.');
end;

procedure TSsPrivateKeyRingComponent.ClearExpiredSessionKeys;
var
  I: Integer;
  NowGMT: TDateTime;
begin
  KeyLock;
  try
    NowGMT := Now - HoursOffsetFromGMT/24;
    for I := SessionKeyCount - 1 downto 0 do
      if SessionKeys[I].FindField('expires').ContentAsDateTime < NowGMT then
        DeleteSessionKey(I);
  finally
    KeyUnlock;
  end;
end;

procedure TSsPrivateKeyRingComponent.ComputePublicKey(Index: Integer;
  LongTermKey: Boolean; var PK: TX509PublicKey);
var
  F, Idf: TASN1Struct;
  P: PASN1Struct;
  DLPriv: TDLPrivateKey;
  DLPubl: TDLPublicKey;
  IFPriv: TIFPrivateKey;
  IFPubl: TIFPublicKey;
  ECPriv: TECPrivateKey;
  ECPubl: TECPublicKey;
begin
  PK.Free;
  PK := TX509PublicKey.Create;
  if LongTermKey then
    F := LongTermKeys[Index]
  else begin
    F := SessionKeys[Index].FindField('keyEncryptionKey')^;
    if F.FindField('privateKeyType')^.Length = 0 then
      F := SessionKeys[Index].FindField('ephemeralKey')^
  end;

  PK.Algorithm.Algorithm := F.FindField('privateKeyType')^.ContentAsOID;
  P := F.FindField('/privateKey//privateKey/params');
  if Assigned(P) then begin
    PK.Algorithm.Parameters := TASN1Struct.Create;
    PK.Algorithm.Parameters.Assign(P^);
  end;
  if (PK.Algorithm.Algorithm = id_dsa) or
     (PK.Algorithm.Algorithm = dhPublicNumber) then begin
    FillChar(DLPriv,SizeOf(DLPriv),0);
    Idf := F.FindField('/privateKey//publicKeyIdentifier')^;
    FindPrivateKey(Idf.Content,Idf.Length,DLPriv);
    FillChar(DLPubl,SizeOf(DLPubl),0);
    DLPublicKey(DLPriv,DLPubl);
    NewComposeASN1Struct(PK.PublicKey,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
    PK.PublicKey.EditContent(DLPubl.Y,True);
    PK.PublicKey.CalculateLength;
    DisposeDLPublicKey(DLPubl);
    DisposeDLPrivateKey(DLPriv);
  end else if (PK.Algorithm.Algorithm = rsaEncryption) or
              (PK.Algorithm.Algorithm = id_RSAES_OAEP) then begin
    FillChar(IFPriv,SizeOf(IFPriv),0);
    Idf := F.FindField('/privateKey//publicKeyIdentifier')^;
    FindPrivateKey(Idf.Content,Idf.Length,IFPriv);
    FillChar(IFPubl,SizeOf(IFPubl),0);
    IFRSAPublicKey(IFPriv,IFPubl);
    NewComposeASN1Struct(PK.PublicKey,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
    PK.PublicKey.AddField('','INTEGER',nil)^.EditContent(IFPubl.N,True);
    PK.PublicKey.AddField('','INTEGER',nil)^.EditContent(IFPubl.E,True);
    PK.PublicKey.CalculateLength;
    DisposeIFPublicKey(IFPubl);
    DisposeIFPrivateKey(IFPriv);
  end else if PK.Algorithm.Algorithm = id_ecPublicKey then begin
    FillChar(ECPriv,SizeOf(ECPriv),0);
    Idf := F.FindField('/privateKey//publicKeyIdentifier')^;
    FindPrivateKey(Idf.Content,Idf.Length,ECPriv);
    FillChar(ECPubl,SizeOf(ECPubl),0);
    ECPublicKey(ECPriv,ECPubl);
    NewComposeASN1Struct(PK.PublicKey,V_ASN1_UNIVERSAL,False,V_ASN1_OCTET_STRING);
    PK.PublicKey.EditContent(ECPubl.W,ECPubl.Params,cHybrid);
    DisposeECPublicKey(ECPubl);
    DisposeECPrivateKey(ECPriv);
  end;
end;

constructor TSsPrivateKeyRingComponent.Create(AOwner: TComponent);
begin
  FCreationThreadID := GetCurrentThreadID;
  inherited;
  FMyPrivateKeyRing := TPrivateKeyRing.Create(Self);
  FPrivateKeyRing := FMyPrivateKeyRing;
  SessionKeyLifeSpan := 1/48;
  AllowPlainTextKeys := True;
  {$IFDEF MSWINDOWS}
  SetHoursOffsetFromGMT(OffsetFromUTC);
  {$ENDIF}
end;

function TSsPrivateKeyRingComponent.CreateAdminKeyPair(
  Password: ISecretKey; const Algorithm: string; ABitSize: Integer;
  const CurveOID: string): Boolean;
begin
{$IFDEF SHA1}
  if (Algorithm = rsaEncryption) or
     (Algorithm = id_rsaes_oaep) or
     (Algorithm = id_rsassa_pss) then
    Result := FPrivateKeyRing.CreateAdminKeyPairRSA(Password,ABitSize)
  else if (Algorithm = id_dsa) then
    Result := FPrivateKeyRing.CreateAdminKeyPairDSA(Password,ABitSize)
  else if (Algorithm = id_ecPublicKey) then
    Result := FPrivateKeyRing.CreateAdminKeyPairECDSA(Password,CurveOID)
  else
{$ENDIF}
    Result := False;
end;

function TSsPrivateKeyRingComponent.CreateKeyPair(DstCert: TASN1Struct;
  const Algorithm: string; ABitSize: Integer;
  const CurveOID: string; Exportable: Boolean): Boolean;
var
  Crt: TSubjectPublicKeyInfo;
begin
  Crt := TSubjectPublicKeyInfo.CreateFromCert(DstCert);
  try
    Result := CreateKeyPair(Crt,Algorithm,ABitSize,CurveOID,Exportable);
    if Result then
      DstCert.Assign(Crt.Data);
  finally
    Crt.Free;
  end;
end;

function TSsPrivateKeyRingComponent.CreateKeyPair(DstCert: IPublicKeyInfo;
  const Algorithm: string; ABitSize: Integer;
  const CurveOID: string; Exportable: Boolean): Boolean;
var
  IFPubl: IIFPublicKeyInfo;
  DLPubl: IDLPublicKeyInfo;
  ECPubl: IECPublicKeyInfo;
  KR: TKeyRingEnum;
begin
  Result := True;
  if FPrivateKeyRing.CheckAdminLogin then
    KR := krSignedEncrypted
  else begin
    KR := krLongTerm;
    Result := AllowPlainTextKeys;
  end;
  if not Result then begin
    if Exceptions then
      raise Exception.Create('Could not login as admin and plain text keys are forbidden');
  end else if (Algorithm = rsaEncryption) or
              (Algorithm = id_rsaes_oaep) or
              (Algorithm = id_rsassa_pss) or
              (Algorithm = id_rw) then begin
    Result := DstCert.QueryInterface(IIFPublicKeyInfo,IFPubl) = 0;
    Result := Result and
              (FPrivateKeyRing.CreateKeyPairIF(Algorithm,
                                               ABitSize,
                                               IFPubl,
                                               KR,
                                               DefaultHashAlgorithm,
                                               Exportable) <> nil);
  end else if (Algorithm = id_dsa) or
              (Algorithm = id_nr) or
              (Algorithm = dhPublicNumber) then begin
    Result := DstCert.QueryInterface(IDLPublicKeyInfo,DLPubl) = 0;
    if ABitSize < 2048 then
      Result := Result and
                (FPrivateKeyRing.CreateKeyPairDL(Algorithm,
                                                 ABitSize,160,
                                                 DLPubl,
                                                 KR,
                                                 DefaultHashAlgorithm) <> nil)
    else if ABitSize < 3072 then
      Result := Result and
                (FPrivateKeyRing.CreateKeyPairDL(Algorithm,
                                                 ABitSize,256,
                                                 DLPubl,
                                                 KR,
                                                 DefaultHashAlgorithm) <> nil)
    else if ABitSize < 4096 then
      Result := Result and
                (FPrivateKeyRing.CreateKeyPairDL(Algorithm,
                                                 ABitSize,384,
                                                 DLPubl,
                                                 KR,
                                                 DefaultHashAlgorithm) <> nil)
    else
      Result := Result and
                (FPrivateKeyRing.CreateKeyPairDL(Algorithm,
                                                 ABitSize,512,
                                                 DLPubl,
                                                 KR,
                                                 DefaultHashAlgorithm) <> nil);
  end else if (Algorithm = id_ecPublicKey) or
              (Algorithm = id_ecnr) then begin
    Result := DstCert.QueryInterface(IECPublicKeyInfo,ECPubl) = 0;
    Result := Result and
              (FPrivateKeyRing.CreateKeyPairEC(Algorithm,
                                               CurveOID,
                                               ECPubl,
                                               KR,
                                               DefaultHashAlgorithm) <> nil);
  end;
end;

procedure TSsPrivateKeyRingComponent.DeleteSessionKey(index: Integer);
begin
  FPrivateKeyRing.DeleteSessionKey(index);
end;

destructor TSsPrivateKeyRingComponent.Destroy;
var
  Debug: string;
begin
  try
    Debug := 'SetPrivateKeyRing(nil)';
    SetPrivateKeyRing(nil);
    Debug := 'AdminLogout';
    AdminLogout;
    Debug := 'inherited Destroy';
    inherited Destroy;
    Debug := 'if FMyPrivateKeyRing.FOwner = Self then';
    if Assigned(FMyPrivateKeyRing) and
       (FMyPrivateKeyRing.FOwner = Self) then begin
      Debug := 'FMyPrivateKeyRing.Free';
      FMyPrivateKeyRing.Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('TSsPrivateKeyRingComponent.Destroy: ' + Debug + #13#10 +
                             E.Message);
  end;
end;

procedure TSsPrivateKeyRingComponent.DoAdminPassword(
  var Password: ISecretKey);
begin                                     
  Password := TSecretKey.Create('');
  if (not (csDesigning in ComponentState)) and Assigned(FOnAdminPassword) then
    FOnAdminPassword(Self,Password);
{$IFDEF GUI_APP}
  if Password.KeyLen <= 0 then
    PasswordDlg('Private Key Ring Admin',Password);
{$ENDIF GUI_APP}
end;

procedure TSsPrivateKeyRingComponent.DoLoadPrivateKeyRingFile(
  Sender: TObject);
var
  Dummy: ISecretKey;
begin
  if Sender = FPrivateKeyRingFile then begin
    try
      if Assigned(FPrivateKeyRingFile) and
         Assigned(FPrivateKeyRingFile.DataStream) and
         not (csDesigning in ComponentState) then begin
        LoadPrivateKeyRingFromStream(FPrivateKeyRingFile.DataStream,Dummy);
        FPrivateKeyRingFileName := '';
      end;
    except
      FPrivateKeyRingFile.RemoveLoadNotification(Self,DoLoadPrivateKeyRingFile);
      FPrivateKeyRingFile := nil;
    end;
  end;
end;

procedure TSsPrivateKeyRingComponent.DoPassword(var Password: ISecretKey);
begin
  if Assigned(FPassword) then begin
    if Assigned(Password) and (Password <> FPassword) then
      Password.SetKey(FPassword.Key,FPassword.KeyLen,FPassword.VectorSize)
    else
      Password := FPassword;
  end else begin
    Password := TSecretKey.Create('');
    if Assigned(FOnPassword) then
      FOnPassword(Self,Password); 
{$IFDEF GUI_APP}
    if Password.KeyLen = 0 then
      PasswordDlg('Private Key Ring',Password);
{$ENDIF GUI_APP}
    if csDesigning in ComponentState then
      FPassword := Password;
  end;
end;

function TSsPrivateKeyRingComponent.FindCreateCipher(
  const KeyIdentifier: OctetString; Alg: TCipherAlg; Mode: TCipherMode;
  var KeyedIV: Boolean; var Param: OctetString;
  MACKey: ISecretKey): TCipher;
var
  Err: Boolean;
  OID: string;
  F, A: PASN1Struct;
  Idx: Integer;
  CC: TCipherClass;
  KLen: Integer;
begin
  Result := nil;
  Err := False;
  case Alg of
    caUnknown:       OID := '';
    caDES:           OID := tripleDES;
    caRijndael:      OID := id_aes;
    SecUtils.caARC4: OID := arcfour;
    caTwoFish:       OID := id_twofish;
    caBlowFish:      OID := id_blowfish;
  else
    Err := True;
  end;
  if not Err then begin
    Err := not FPrivateKeyRing.InternalFindPrivateKey(Pointer(KeyIdentifier),Length(KeyIdentifier),
                                                      OID,
                                                      F,
                                                      Idx,krLongTerm);
    if Err then
      Err := not FPrivateKeyRing.InternalFindPrivateKey(Pointer(KeyIdentifier),Length(KeyIdentifier),
                                                        OID,
                                                        F,
                                                        Idx,krSession);
    if not Err then begin
      if OID = '' then begin
        A := F.FindField('privateKeyType');
        if A.ContentAsOID = tripleDES then
          Alg := caDES
        else if A.ContentAsOID = arcfour then
          Alg := SecUtils.caARC4
        else if A.ContentAsOID = id_aes then
          Alg := caRijndael
        else if A.ContentAsOID = id_twofish then
          Alg := caTwoFish
        else if A.ContentAsOID = id_blowfish then
          Alg := caBlowFish
        else
          Err := True;
      end;
      if not Err then begin
        A := F.FindField('privateKeyAlg');
        if A.IsEmpty then begin
          CC := FindCipherClass(Alg,Mode);
          Err := CC = nil;
          KLen := -1;
        end else
          Err := not OIDToCipherClass(A^.ContentAsOID,KeyedIV,KLen,CC);
        if not Err then begin
          F := F.FindField('/privateKey/');
          if KLen < 0 then
            KLen := F.Length
          else
            Err := KLen <> F.Length;
          if not Err then begin
            if (Param = '') and CC.InheritsFrom(TBlockCipher) {and not KeyedIV} then begin
              SetLength(Param,CC.BlockSize);
              RawRandom(Pointer(Param)^,Length(Param)*8);
            end;
            Result := CreateCipher_v2(F.Content^,KLen,CC,KeyedIV,Param,MACKey);
          end;
        end;
      end;
    end;
  end;
end;

function TSsPrivateKeyRingComponent.FindCreatePrivateKey(
  PublicKeyIdentifier: Pointer; PKILen: Integer;
  out IsLongTermKey: Boolean; var Index: Integer): TMPSecretKey;
var
  KR: TKeyRings;
begin                                                        
  KR := [krSession,krLongTerm];
  Result := FPrivateKeyRing.FindCreatePrivateKeyObj(PublicKeyIdentifier,PKILen,Index,KR);
  IsLongTermKey := KR = [krLongTerm];
  if Assigned(FExternalIFPrivateKeyClass) and (Result is TMPIFPrivateKey) then
    TMPIFPrivateKey(Result).ExternalIFPrivateKeyClass := FExternalIFPrivateKeyClass;
end;

function TSsPrivateKeyRingComponent.FindCreatePrivateKey(
  Cert: TASN1Struct): TMPPrivateKey;
var
  KR: TKeyRings;
begin
  KR := [krSession,krLongTerm,krEncrypted,krSignedEncrypted];
  Result := FPrivateKeyRing.FindCreatePrivateKeyObj(Cert,KR);
  if Assigned(FExternalIFPrivateKeyClass) and (Result is TMPIFPrivateKey) then
    TMPIFPrivateKey(Result).ExternalIFPrivateKeyClass := FExternalIFPrivateKeyClass;
end;

function TSsPrivateKeyRingComponent.FindPrivateKey(Cert: TASN1Struct;
  var Key: TECPrivateKey): Boolean;
var
  F: PASN1Struct;
  KR: TKeyRings;
begin
  KeyLock;
  try
    F := nil;
    KR := [krLongTerm];
    Result := FPrivateKeyRing.InternalFindPrivateKey(Cert,KR,F);
    if Result then begin
      Result := InterpretECPrivateKey(F,Key);
      if F^.RefCount = 0 then begin
        F^.Free;
        Dispose(F);
      end;
    end;
  finally
    KeyUnlock;
  end;
end;

function TSsPrivateKeyRingComponent.FindPrivateKey(
  PublicKeyIdentifier: Pointer; PKILen: Integer; var Key: PASN1Struct;
  var Index: Integer; IsLongTermKey: Boolean): Boolean;
var
  KR: TKeyRings;
begin
  KeyLock;
  try
    if IsLongTermKey then
      KR := [krLongTerm]
    else
      KR := [krSession];
    Result := FPrivateKeyRing.InternalFindPrivateKey(PublicKeyIdentifier,PKILen,
                                                     Key,Index,KR);
  finally
    KeyUnlock;
  end;
end;

function TSsPrivateKeyRingComponent.FindPrivateKey(
  PublicKeyIdentifier: Pointer; PKILen: Integer; var Key: PASN1Struct;
  out IsLongTermKey: Boolean; var Index: Integer): Boolean;
var
  KR: TKeyRings;
begin
  KeyLock;
  try       
    KR := [krSession,krLongTerm];
    Result := FPrivateKeyRing.InternalFindPrivateKey(PublicKeyIdentifier,PKILen,
                                                     Key,Index,KR);
    IsLongTermKey := KR = [krLongTerm];
  finally
    KeyUnlock;
  end;
end;

function TSsPrivateKeyRingComponent.FindPrivateKey(Cert: TASN1Struct;
  var Key: PASN1Struct): Boolean;
var
  KR: TKeyRings;
begin
  KR := [krLongTerm];
  Result := FPrivateKeyRing.InternalFindPrivateKey(Cert,KR,Key);
end;

function TSsPrivateKeyRingComponent.FindPrivateKey(Cert: TASN1Struct;
  var Key: TIFPrivateKey): Boolean;
var
  F: PASN1Struct;
  KR: TKeyRings;
begin
  KeyLock;
  try
    KR := [krLongTerm];
    Result := FPrivateKeyRing.InternalFindPrivateKey(Cert,KR,F);
    if Result then begin
      Result := InterpretIFPrivateKey(F,Key);
      if F^.RefCount = 0 then begin
        F^.Free;
        Dispose(F);
      end;
    end;
  finally
    KeyUnlock;
  end;
end;

function TSsPrivateKeyRingComponent.FindPrivateKey(Cert: TASN1Struct;
  var Key: TDLPrivateKey): Boolean;
var
  F: PASN1Struct;
  KR: TKeyRings;
begin
  KeyLock;
  try
    KR := [krLongTerm];
    Result := FPrivateKeyRing.InternalFindPrivateKey(Cert,KR,F);
    if Result then begin
      Result := InterpretDLPrivateKey(F,Key);
      if F^.RefCount = 0 then begin
        F^.Free;
        Dispose(F);
      end;
    end;
  finally
    KeyUnlock;
  end;
end;

function TSsPrivateKeyRingComponent.FindPrivateKey(
  PublicKeyIdentifier: Pointer; PKILen: Integer;
  var Key: TDLPrivateKey): Boolean;
var
  F: PASN1Struct;
  Dummy: Integer;
  KR: TKeyRings;
begin
  KeyLock;
  try
    KR := [krSession,krLongTerm];
    Result := FPrivateKeyRing.InternalFindPrivateKey(PublicKeyIdentifier,PKILen,
                                                     F,Dummy,KR);
    if Result then begin
      Result := InterpretDLPrivateKey(F,Key);
      if F^.RefCount = 0 then begin
        F^.Free;
        Dispose(F);
      end;
    end;
  finally
    KeyUnlock;
  end;
end;

function TSsPrivateKeyRingComponent.FindPrivateKey(
  PublicKeyIdentifier: Pointer; PKILen: Integer;
  var Key: TECPrivateKey): Boolean;
var
  F: PASN1Struct;
  Dummy: Integer;
  KR: TKeyRings;
begin
  KeyLock;
  try
    KR := [krSession,krLongTerm];
    Result := FPrivateKeyRing.InternalFindPrivateKey(PublicKeyIdentifier,PKILen,
                                                     F,Dummy,KR);
    if Result then begin
      Result := InterpretECPrivateKey(F,Key);
      if F^.RefCount = 0 then begin
        F^.Free;
        Dispose(F);
      end;
    end;
  finally
    KeyUnlock;
  end;
end;

function TSsPrivateKeyRingComponent.FindPrivateKey(
  PublicKeyIdentifier: Pointer; PKILen: Integer;
  var Key: PASN1Struct): Boolean;
var
  Index: Integer;
  LongTerm: Boolean;
begin
  Result := FindPrivateKey(PublicKeyIdentifier,PKILen,Key,LongTerm,Index);
end;

function TSsPrivateKeyRingComponent.FindPrivateKey(
  PublicKeyIdentifier: Pointer; PKILen: Integer;
  var Key: TIFPrivateKey): Boolean;
var
  F: PASN1Struct;
  Dummy: Integer;
  KR: TKeyRings;
begin
  KeyLock;
  try
    KR := [krSession,krLongTerm];
    Result := FPrivateKeyRing.InternalFindPrivateKey(PublicKeyIdentifier,PKILen,
                                                     F,Dummy,KR);
    if Result then begin
      Result := InterpretIFPrivateKey(F,Key);
      if F^.RefCount = 0 then begin
        F^.Free;
        Dispose(F);
      end;
    end;
  finally
    KeyUnlock;
  end;
end;
                
{$IFDEF SHA1_AND_MD5}
function TSsPrivateKeyRingComponent.FindTLSMasterSecret(
  const SessionID: TSessionID; var Index: Integer): ISecretKey;
begin
  Result := FPrivateKeyRing.FindTLSMasterSecret(SessionID,Index);
end;

function TSsPrivateKeyRingComponent.FindTLSMasterSecret(
  const SessionID: TSessionID; var MasterSecret: TMasterSecret;
  var Index: Integer): Boolean;
var
  K: ISecretKey;
begin
  K := FPrivateKeyRing.FindTLSMasterSecret(SessionID,Index);
  Result := Assigned(K);
  if Result then
    Move(K.Key^,MasterSecret,K.KeyLen);
end;   
{$ENDIF SHA1_AND_MD5}

function TSsPrivateKeyRingComponent.FreePrivateKey(
  APrivKey: IKey): Boolean;
begin
  Result := FPrivateKeyRing.FreePrivateKey(APrivKey);
end;

function TSsPrivateKeyRingComponent.GetAllowPlainTextKeys: Boolean;
begin
  Result := FPrivateKeyRing.AllowPlainTextKeys;
end;

function TSsPrivateKeyRingComponent.GetCacheKeyInterfaces: Boolean;
begin
  Result := FPrivateKeyRing.CacheKeyInterfaces;
end;

function TSsPrivateKeyRingComponent.GetHoursOffsetFromGMT: Double;
begin
  Result := FPrivateKeyRing.HoursOffsetFromGMT;
end;

function TSsPrivateKeyRingComponent.GetLongTermKeyCount: Integer;
begin
  Result := FPrivateKeyRing.GetLongTermKeyCount;
end;

function TSsPrivateKeyRingComponent.GetLongTermKeys(
  index: Integer): TASN1Struct;
begin
  Result := FPrivateKeyRing.GetLongTermKeys(index);
end;

function TSsPrivateKeyRingComponent.GetSessionKeyCount: Integer;
begin
  Result := FPrivateKeyRing.GetSessionKeyCount;
end;

function TSsPrivateKeyRingComponent.GetSessionKeyLifeSpan: TTime;
begin
  Result := FPrivateKeyRing.SessionKeyLifeSpan;
end;

function TSsPrivateKeyRingComponent.GetSessionKeys(
  index: Integer): TASN1Struct;
begin
  Result := FPrivateKeyRing.SessionKeys[index];
end;

function TSsPrivateKeyRingComponent.InterpretDLPrivateKey(F: PASN1Struct;
  var Key: TDLPrivateKey): Boolean;
var
  OID: string;
  A: PASN1Struct;
begin
  Result := True;
  OID := F.FindField('privateKeyType')^.ContentAsOID;
  A := F.FindField('/privateKey//privateKey');
  if Assigned(A) then begin
    if OID = id_dsa then begin
      A.FindField('privateKey')^.ContentAsUMPInt(Key.X);
      A.FindField('/params/p')^.ContentAsUMPInt(Key.params.P);
      A.FindField('/params/q')^.ContentAsUMPInt(Key.params.Q);
      A.FindField('/params/g')^.ContentAsUMPInt(Key.params.G);
      MPDealloc(Key.params.J);
      Key.Params.J := nil;
    end else if OID = dhPublicNumber then begin
      A.FindField('privateKey')^.ContentAsUMPInt(Key.X);
      A.FindField('/params/p')^.ContentAsUMPInt(Key.params.P);
      A.FindField('/params/q')^.ContentAsUMPInt(Key.params.Q);
      A.FindField('/params/g')^.ContentAsUMPInt(Key.params.G);
      A.FindField('/params/j')^.ContentAsUMPInt(Key.params.J);
    end else
      Result := False;
  end else
    Result := False;
end;

function TSsPrivateKeyRingComponent.InterpretECPrivateKey(F: PASN1Struct;
  var Key: TECPrivateKey): Boolean;
var
  OID: string;
  A, P: PASN1Struct;
  T: Cardinal;
begin
  if F.FindField('privateKeyType')^.ContentAsOID = id_ecPublicKey then begin
    Result := True;
    A := F.FindField('/privateKey//privateKey');
    if Assigned(A) then begin
      T := A.FindField('params').Tag;
      if T = V_ASN1_SEQUENCE then begin
        OID := A.FindField('/params/fieldID/fieldType')^.ContentAsOID;
        if OID = prime_field then begin
          A.FindField('privateKey')^.ContentAsUMPInt(Key.S);
          A.FindField('/params/fieldID/parameters')^.ContentAsUMPInt(Key.params.Q);
          A.FindField('/params/curve/a')^.ContentAsUMPInt(Key.params.A);
          A.FindField('/params/curve/b')^.ContentAsUMPInt(Key.params.B);
          A.FindField('/params/order')^.ContentAsUMPInt(Key.params.R);
          P := A.FindField('/params/coFactor');
          if P.Length > 0 then
            P.ContentAsUMPInt(Key.Params.K)
          else begin
            MPDealloc(Key.Params.K);
            Key.Params.K := IntToMPInt(1);
          end;
          A.FindField('/params/base')^.ContentAsECPoint(Key.params.G,Key.Params);
        end else
          Result := False;
      end else if T = V_ASN1_OBJECT then begin
        OID := A.FindField('params')^.ContentAsOID;
        if OID = prime192v1 then
          ECurveP192_1(Key.params)
        else if OID = prime192v2 then
          ECurveP192_2(Key.params)
        else if OID = prime192v3 then
          ECurveP192_3(Key.params)
        else if OID = prime239v1 then
          ECurveP239_1(Key.params)
        else if OID = prime239v2 then
          ECurveP239_2(Key.params)
        else if OID = prime239v3 then
          ECurveP239_3(Key.params)
        else if OID = prime256v1 then
          ECurveP256_1(Key.params)
        else if OID = GetRegistredOID('prime224') then
          ECurveP224(Key.params)
        else if OID = GetRegistredOID('prime384') then
          ECurveP384(Key.params)
        else if OID = GetRegistredOID('prime521') then
          ECurveP521(Key.params)
        else
          Result := False;
        if Result then
          A.FindField('privateKey')^.ContentAsUMPInt(Key.S);
      end else
        Result := False;
    end else
      Result := False;
  end else
    Result := False;
end;

function TSsPrivateKeyRingComponent.InterpretIFPrivateKey(F: PASN1Struct;
  var Key: TIFPrivateKey): Boolean;
var
  OID, S: string;
  A: PASN1Struct;
  T: Cardinal;
begin
  OID := F.FindField('privateKeyType')^.ContentAsOID;
  if ((OID = rsaEncryption) or
      (OID = id_RSAES_OAEP) or
      (OID = id_RSASSA_PSS)) then begin
    Result := True;
    A := F.FindField('/privateKey//privateKey');
    if Assigned(A) then begin
      T := A^.Tag;
      if T = 0 then begin
        Key.ReprType := 0;
        A.FindField('n')^.ContentAsUMPInt(Key.zN);
        A.FindField('d')^.ContentAsUMPInt(Key.zD);
      end else if T = 1 then begin
        Key.ReprType := 1;
        A.FindField('p')^.ContentAsUMPInt(Key.oP);
        A.FindField('q')^.ContentAsUMPInt(Key.oQ);
        A.FindField('d1')^.ContentAsUMPInt(Key.oD1);
        A.FindField('d2')^.ContentAsUMPInt(Key.oD2);
        A.FindField('c')^.ContentAsUMPInt(Key.oC);
      end else if T = 2 then begin
        Key.ReprType := 2;
        A.FindField('p')^.ContentAsUMPInt(Key.tP);
        A.FindField('q')^.ContentAsUMPInt(Key.tQ);
        A.FindField('d')^.ContentAsUMPInt(Key.tD);
      end else if T = 3 then begin
        Key.ReprType := 3;
        Result := Assigned(FExternalIFPrivateKeyClass);
        if Result then begin
          S := F.FindField('/privateKey//publicKeyIdentifier')^.ContentAsOctetString;
          Key.ExternalKey := FExternalIFPrivateKeyClass.Create;
          Result := Key.ExternalKey.ImportKey(Pointer(S)^,Length(S));
        end;
      end else
        Result := False;
    end else
      Result := False;
  end else
    Result := False;
end;

procedure TSsPrivateKeyRingComponent.KeyLock;
begin
  if AAlways or not CheckThreadID then
    FPrivateKeyRing.FManagedIndexLock.Acquire;
end;

procedure TSsPrivateKeyRingComponent.KeyUnlock;
begin
  if AAlways or not CheckThreadID then
    FPrivateKeyRing.FManagedIndexLock.Release;
end;

procedure TSsPrivateKeyRingComponent.Loaded;
var
  Dummy: ISecretKey;
begin
  inherited;
  if FPrivateKeyRingFileName <> '' then
    LoadPrivateKeyRingFromFile(FPrivateKeyRingFileName,nil)
  else if Assigned(FPrivateKeyRingFile) and
          Assigned(FPrivateKeyRingFile.DataStream) then
    if not LoadPrivateKeyRingFromStream(FPrivateKeyRingFile.DataStream,Dummy) then
      raise Exception.Create('Unable to load private key ring from resource ' +
                             FPrivateKeyRingFile.Name);
end;

function TSsPrivateKeyRingComponent.LoadPrivateContentFromStream(
  Src: TStream; Dst: TSecureMemoryStream; Password: ISecretKey;
  var ContentID: OctetString): Boolean;
var
  PKR: TASN1Struct;
  F, A, D: TASN1Struct;
  SS: TStringStream;
  CipherClass, KEKCipherClass: TCipherClass;
  C: TCipher;
  KeyedIV, KEKeyedIV: Boolean;
  KEKKey, HMacKey: ISecretKey;
  Key, EncKey, IV: string;
{$IFDEF SHA1}
  Mac, PadKey, Param: string;
  EKPadSize: Integer;
  Pad: Byte;
{$ENDIF SHA1}
  KEKAlgorithm, EncAlg: string;
  PadSize, KeyLength, KEKKeyLength: Integer;
  I: Integer;
  HA: THashAlgorithm;
  H: THash;
  HC: THashClass;
begin
  KEKKey := nil;
  HMacKey := nil;
  PKR := TASN1Struct.Create;
  try
    SS := TStringStream.Create(StrSecIIPrivateContentInfoASN1Module);
    try
      PKR.LoadFromStream(SS,fmtASN1);
    finally
      SS.Free;
    end;
    PKR.LoadFromStream(Src,fmtDER);
    ContentID := PKR.FindField('contentID')^.ContentAsOctetString;
    F := PKR.FindField('/encryptionAlgorithm/algorithm')^;
    EncAlg := F.ContentAsOID;
    Result := OIDToCipherClass(EncAlg,KeyedIV,KeyLength,CipherClass);
    if Result then begin
      F := PKR.FindField('encryptedContent')^;
      Result := (F.Length > 0) and
                ((F.Length mod CipherClass.BlockSize) = 0);
    end;
    if Result then begin
      F := PKR.FindField('/contentEncryptionKey/keyEncryptionAlgorithm/algorithm')^;
      KEKAlgorithm := F.ContentAsOID;
      Result := OIDToCipherClass(KEKAlgorithm,KEKeyedIV,KEKKeyLength,KEKCipherClass);
      if Result then begin
        F := PKR.FindField('/contentEncryptionKey/kekid')^;
        Result := _KDFVerification(Password,
                                   KEKAlgorithm,
                                   #0 + Char(Byte((KeyLength*8) shr 16)) + Char(Byte((KeyLength*8) shr 8)) + Char(Byte(KeyLength*8)),
                                   F,KEKKeyLength,KEKKey);
      end;
      if Result then try
        if KEKCipherClass.InheritsFrom(T3DES_ECB) then
          DESKeyParity(KEKKey.Key^,KEKKey.KeyLen);

{$IFDEF SHA1}
        F := F.FindField('keyValidation')^;
        if not F.IsEmpty then begin
          SetLength(Mac,8);
          Result := _KDFVerification(KEKKey,GetRegistredOID('hMAC-SHA1'),'',F,64,HMacKey);
          if Result then begin
            HMac(HMacKey.Key^,HMacKey.KeyLen,
                 KEKKey.Key^,KEKKey.KeyLen,
                 haSHA1,
                 Pointer(Mac)^,Length(Mac));
            F := F.FindField('hmac')^;
            Result := CompareMem(Pointer(Mac),F.Content,8);
          end;
        end;
{$ENDIF SHA1}
        if Result then begin
          if KEKeyedIV then
            C := KEKCipherClass.Create(KEKKey.Key^,KEKKey.KeyLen,
                                       TBlockCipherClass(KEKCipherClass).BlockVectorSize)
          else
            C := KEKCipherClass.Create(KEKKey.Key^,KEKKey.KeyLen,0);
          try
            F := PKR.FindField('/contentEncryptionKey/encryptedKey')^;
            EncKey := F.ContentAsOctetString;
            try

              if C is TAES_Wrap then begin
                Result := Length(EncKey) = 8 + KeyLength;
                if Result then begin
                  C.IVector := Copy(EncKey,1,8);
                  C.Decrypt(EncKey[9],KeyLength);
                  Result := C.IVector = #$A6#$A6#$A6#$A6#$A6#$A6#$A6#$A6;
                end;
                if Result then begin
                  SetLength(Key,KeyLength);
                  Move(EncKey[9],Pointer(Key)^,KeyLength);
                end;
              end else begin
{$IFDEF SHA1}
                if C is TBlockCipher then begin
                  F := PKR.FindField('/contentEncryptionKey/keyEncryptionAlgorithm/iv')^;
                  C.IVector := F.ContentAsOctetString;
                end;

                C.Decrypt(Pointer(EncKey)^,Length(EncKey));

                SetLength(PadKey,Length(EncKey));
                try
                  WeakPasswordToPrivKey(Password.Key^,Password.KeyLen,
                                        KEKKey.Key^,KEKKey.KeyLen,
                                        1,
                                        Pointer(PadKey)^,Length(PadKey));
                  for I := 1 to Length(EncKey) do
                    EncKey[I] := Char(Byte(EncKey[I]) xor Byte(PadKey[I]));
                finally
                  ProtectClear(Pointer(PadKey)^,Length(PadKey));
                end;

                if C is TBlockCipher then begin
                  EKPadSize := C.BlockSize - (KeyLength mod C.BlockSize);
                  Result := EKPadSize = (Length(EncKey) - KeyLength);
                  if Result then begin
                    Pad := EKPadSize;
                    for I := Length(EncKey) downto Length(EncKey) - EKPadSize + 1 do
                      Result := Result and (EncKey[I] = Char(Pad));
                  end;
                end;
                if Result then begin
                  SetLength(Key,KeyLength);
                  Move(Pointer(EncKey)^,Pointer(Key)^,KeyLength);
                end;
{$ELSE  SHA1}
                Result := False;
{$ENDIF SHA1}
              end;
            finally
              ProtectClear(Pointer(EncKey)^,Length(EncKey));
            end;
          finally
            C.Free;
          end;

{$IFDEF SHA1}
{$IFDEF SHA256}
          F := PKR.FindField('hMAC')^;
          if Result and not F.IsEmpty then begin
            SetLength(EncKey,Length(Key));
            try
              Move(Pointer(Key)^,Pointer(EncKey)^,Length(Key));
              Param := 'Content Encryption Key';
              WeakPasswordToPrivKey256(Pointer(EncKey)^,Length(EncKey),
                                       Pointer(Param)^,Length(Param),
                                       1,
                                       Pointer(Key)^,Length(Key));
              try
                Param := 'Authentication Key';
                WeakPasswordToPrivKey256(Pointer(EncKey)^,Length(EncKey),
                                         Pointer(Param)^,Length(Param),
                                         1,
                                         Length(EncKey),HMacKey);
                SetLength(Mac,F.Length);
                try
                  D := PKR.FindField('encryptedContent')^;
                  HMac(HMacKey.Key^,HMacKey.KeyLen,
                       D.Content^,D.Length,
                       haSHA1,
                       Pointer(Mac)^,Length(Mac));
                  Result := CompareMem(Pointer(Mac),F.Content,F.Length);
                finally
                  ProtectClear(Pointer(Mac)^,Length(Mac));
                end;
              finally
                HMacKey := nil;
              end;
            finally
              ProtectClear(Pointer(EncKey)^,Length(EncKey));
            end;
          end;
{$ENDIF SHA256}
{$ENDIF SHA1}
          if Result then begin
            F := PKR.FindField('encryptedContent')^;
            if KeyedIV then
              C := CipherClass.Create(Pointer(Key)^,Length(Key),
                                      TBlockCipherClass(CipherClass).BlockVectorSize)
            else
              C := CipherClass.Create(Pointer(Key)^,Length(Key),0);
            try
              ProtectClear(Pointer(Key)^,Length(Key));

              if (C is TBlockCipher) and not KeyedIV then begin
                A := PKR.FindField('/encryptionAlgorithm/iv')^;
                IV := A.ContentAsOctetString;
                Result := Length(IV) = C.BlockSize;
                if Result then
                  C.IVector := IV;
              end;
              C.Decrypt(F.Content^,F.Length);
              PadSize := Byte(F.Content[F.Length-1]);
              Result := PadSize <= C.BlockSize;
              if Result then
                for I := F.Length - PadSize to F.Length - 1 do
                  Result := Result and (Byte(F.Content[I]) = PadSize);
              if Result then begin
                D := nil;
                try
                  NewDigestedDataStruct(D);
                  F.ContentAsASN1Struct(D.FindField('/content/')^);
                  A := D.FindField('/content//digestAlgorithm/algorithm')^;
                  OIDToHashAlgorithm(A.ContentAsOID,HA);
                  A := D.FindField('/content//encapContentInfo/eContent/')^;
                  HC := FindHashClass(HA);
                  if Assigned(HC) then
                    H := HC.Create(A.Content^,A.Length)
                  else
                    raise Exception.Create('TStreamSecII.LoadPrivateContentFromStream: Unsupported hash algorithm');
                  try
                    H.Done(nil);
                    A := D.FindField('/content//digest')^;
                    Result := CompareStr(H.Digest,A.ContentAsOctetString) = 0;
                  finally
                    H.Free;
                  end;
                  if Result then begin
                    A := D.FindField('/content//encapContentInfo/eContent/')^;
                    Dst.Write(A.Content^,A.Length);
                    ProtectClear(F.Content^,F.Length);
                    ProtectClear(A.Content^,A.Length);
                  end;
                finally
                  D.Free;
                end;
              end;
            finally
              C.Free;
            end;
          end;
        end;
      finally
        ProtectClear(Pointer(Key)^,Length(Key));
      end;
    end;
  finally
    PKR.Free;
  end;
end;

function TSsPrivateKeyRingComponent.LoadPrivateContentFromStream(
  Src: TStream; Dst: TSecureMemoryStream; Password: ISecretKey): Boolean;
var
  Dummy: OctetString;
begin
  Result := LoadPrivateContentFromStream(Src,Dst,Password,Dummy);
end;

function TSsPrivateKeyRingComponent.LoadPrivateContentFromStream(
  Src: TStream; Dst: TSecureMemoryStream; const Password: string): Boolean;
var
  Dummy: OctetString;
begin
  Result := LoadPrivateContentFromStream(Src,Dst,Password,Dummy);
end;

function TSsPrivateKeyRingComponent.LoadPrivateContentFromStream(
  Src: TStream; Dst: TSecureMemoryStream; const Password: string;
  var ContentID: OctetString): Boolean;
var
  PW: ISecretKey;
begin
  PW := TSecretKey.Create('');
  PW.SetKey(Pointer(Password),Length(Password),0);
  Result := LoadPrivateContentFromStream(Src,Dst,PW,ContentID);
end;

function TSsPrivateKeyRingComponent.LoadPrivateKeyRingFromFile(
  AFileName: TFileName; Password: ISecretKey): Boolean;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmOpenRead);
  try
    Result := LoadPrivateKeyRingFromStream(FS,Password);
  finally
    FS.Free;
  end;
end;

function TSsPrivateKeyRingComponent.LoadPrivateKeyRingFromStream(
  Stream: TStream; Password: ISecretKey): Boolean;
begin
  if Password = nil then
    DoPassword(Password);
  Result := FPrivateKeyRing.LoadPrivateKeyRingFromStream(Stream,Password);
end;

function TSsPrivateKeyRingComponent.LoadPrivateKeyRingFromStream(
  Stream: TStream; const Password: string): Boolean;
var
  PW: ISecretKey;
begin
  Assert(Password <> '');
  PW := TSecretKey.Create('');
  PW.SetLength(Length(Password));
  PW.SetKeyStrAt(Password,0);
  Result := FPrivateKeyRing.LoadPrivateKeyRingFromStream(Stream,PW);
end;

procedure TSsPrivateKeyRingComponent.SavePrivateContentToStream(
  Dst: TStream; Src: TSecureMemoryStream; const Password: string;
  KDF: TKeyDerivation; KDFIterations: Integer; KeyValidation: Boolean;
  KEKAlgorithm: string; CipherClass: TCipherClass);
var
  Dummy: OctetString;
begin
  Dummy := '';
  SavePrivateContentToStream(Dst,Src,
                             Password,
                             Dummy,
                             KDF,KDFIterations,KeyValidation,KEKAlgorithm,
                             CipherClass);
end;

procedure TSsPrivateKeyRingComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FPKRComp then begin
      FPKRComp.KeyLock;
      try
        FPrivateKeyRing := FMyPrivateKeyRing;
      finally
        FPKRComp.KeyUnlock;
      end;
      FPKRComp := nil;
    end;
    if AComponent = FPrivateKeyRingFile then
      FPrivateKeyRingFile := nil;
  end;
end;

procedure TSsPrivateKeyRingComponent.SavePrivateContentToStream(
  Dst: TStream; Src: TSecureMemoryStream; Password: ISecretKey;
  var ContentID: OctetString; KDF: TKeyDerivation; KDFIterations: Integer;
  KeyValidation: Boolean; KEKAlgorithm: string; CipherClass: TCipherClass);
var
  PKR: TASN1Struct;
  F, A, D: TASN1Struct;
  SS: TStringStream;
  C: TCipher;
  KEKKey, Key, EncKey, IV, Param: ISecretKey;
{$IFDEF SHA1}
  HMacKey, Mac, PadKey: ISecretKey;
  EKPadSize: Integer;
{$ENDIF SHA1}
  EncAlg: string;
  PadSize, KeyLength, KEKeyLength: Integer;
  KeyedIV, KEKeyedIV: Boolean;
  Pad: Byte;
  I: Integer;
  Res: Boolean;
  KECipherClass: TCipherClass;
  H: THash;
  MS: TSecureMemoryStream;
begin  
{$IFNDEF SHA256}
  raise Exception.Create('TSsPrivateKeyRingComponent.SavePrivateContentToStream: Action requires SHA-256');
{$ENDIF  SHA256}
  Assert(KDFIterations > 0);
  Assert(Password <> nil);
  Assert(CipherClass <> nil);
  if CipherClass.InheritsFrom(TBlockCipher) then begin
    KeyLength := TBlockCipherClass(CipherClass).MaxKeySize;
    KeyedIV := CipherClass.Mode in [cmABC,cmPCFB,cmPipedPCFB,cmCTR];
    if KeyedIV then
      KeyLength := KeyLength + TBlockCipherClass(CipherClass).BlockVectorSize *
                               TBlockCipherClass(CipherClass).BlockSize;
  end else begin
    KeyLength := 32;
    KeyedIV := False;
  end;
  Res := CipherClassToOID(KeyLength,CipherClass,KeyedIV,EncAlg);
  Assert(Res,'Not a supported encryption algorithm');

  Res := OIDToCipherClass(KEKAlgorithm,KEKeyedIV,KEKeyLength,KECipherClass);
  Assert(Res);

  PKR := TASN1Struct.Create;
  try
    SS := TStringStream.Create(StrSecIIPrivateContentInfoASN1Module);
    try
      PKR.LoadFromStream(SS,fmtASN1);
    finally
      SS.Free;
    end;
    D := nil;
    try
      NewDigestedDataStruct(D);
      A := D.FindField('/content//digestAlgorithm/algorithm')^;
      A.EditContent(HashAlgorithmToOID(DefaultHashAlgorithm));
      A := D.FindField('/content//encapContentInfo/eContent/')^;
      A.SetContent(Src.Memory^,Src.Size);
      H := FindHashClass(DefaultHashAlgorithm).Create(A.Content^,A.Length);
      try
        A := D.FindField('/content//digest')^;
        A.Length := H.DigestSize;
        H.Done(A.Content);
      finally
        H.Free;
      end;
      MS := TSecureMemoryStream.Create;
      try
        D.CalculateLength;
        D.FindField('/content/').SaveToStream(MS,fmtDER);

        if CipherClass.InheritsFrom(TBlockCipher) then
          PadSize := CipherClass.BlockSize - (MS.Size mod CipherClass.BlockSize)
        else
          PadSize := 0;

        F := PKR.FindField('encryptedContent')^;
        F.Length := MS.Size + PadSize;
        Move(MS.Memory^,F.Content^,MS.Size);
        Pad := PadSize;
        for I := MS.Size to MS.Size + PadSize - 1 do
          F.Content[I] := Char(Pad);
      finally
        MS.Free;
      end;
      ProtectClear(A.Content^,A.Length);
    finally
      D.Free;
    end;
    F := PKR.FindField('/contentEncryptionKey/kekid')^;
    _KDF(Password,KDF,KDFIterations,
         KEKAlgorithm,
         #0 + Char(Byte((KeyLength*8) shr 16)) + Char(Byte((KeyLength*8) shr 8)) + Char(Byte(KeyLength*8)),
         F,KEKeyLength,KEKKey);
    if KECipherClass.InheritsFrom(T3DES_ECB) then
      DESKeyParity(KEKKey.Key^,KEKKey.KeyLen);
{$IFDEF SHA1}
    if KeyValidation then begin
      F := F.FindField('keyValidation')^;
      _KDF(KEKKey,KDF,KDFIterations,GetRegistredOID('hMAC-SHA1'),'',F,64,HMacKey);
      HMac(HMacKey,
           KEKKey.Key^,KEKKey.KeyLen,
           haSHA1,
           8,Mac);
      F := F.FindField('hmac')^;
      F.SetContent(Mac.Key^,Mac.KeyLen);
    end;
{$ENDIF SHA1}
    if KEKeyedIV then
      KEKKey.VectorSize := TBlockCipherClass(KECipherClass).BlockVectorSize;
    C := KECipherClass.CreateIntf(KEKKey);
    try
      RawRandomIntf(Key,KeyLength*8);
      if C is T3DES_ECB then
        DESKeyParity(Pointer(Key)^,KeyLength);
      F := PKR.FindField('/contentEncryptionKey/keyEncryptionAlgorithm/algorithm')^;
      F.EditContent(KEKAlgorithm);
      EncKey := TSecretKey.Create('');
      if C is TAES_Wrap then begin
        EncKey.SetLength(KeyLength + 8);
        EncKey.SetKeyAt(Key,8);
        C.Encrypt(EncKey.KeyBytes^[8],KeyLength);
        C.GetVectorBuf(EncKey.Key^,8);
      end else begin
{$IFDEF SHA1}
        if C is TBlockCipher then begin
          RawRandomIntf(IV,Length(C.IVector)*8);
          C.SetVectorBuf(IV.Key^,IV.KeyLen);
          F := PKR.FindField('/contentEncryptionKey/keyEncryptionAlgorithm/iv')^;
          F.SetContent(IV.Key^,IV.KeyLen);
          EKPadSize := C.BlockSize - (Key.KeyLen mod C.BlockSize);
          EncKey.SetLength(KeyLength + EKPadSize);
          Pad := EKPadSize;
          for I := KeyLength + EKPadSize - 1 downto KeyLength do
            EncKey.KeyBytes[I] := Byte(Pad);
        end else
          EncKey.SetLength(KeyLength);
        EncKey.SetKeyAt(Key,0);

        WeakPasswordToPrivKey(Password,
                              KEKKey,
                              1,
                              EncKey.KeyLen,PadKey);
        for I := 0 to EncKey.KeyLen - 1 do
          EncKey.KeyBytes[I] := EncKey.KeyBytes[I] xor PadKey.KeyBytes[I];

        C.EncryptIntf(EncKey);
{$ELSE  SHA1}
        raise Exception.Create('TSsPrivateKeyRingComponent.SavePrivateContentToStream: Action requires SHA-1');
{$ENDIF SHA1}
      end;
      F := PKR.FindField('/contentEncryptionKey/encryptedKey')^;
      F.SetContent(EncKey.Key^,EncKey.KeyLen);
    finally
      C.Free;
    end;
    EncKey.SetLength(KeyLength);

    EncKey.SetKeyAt(Key,0);
    Param := TSecretKey.CreateStr('Content Encryption Key');
{$IFDEF SHA256}
    WeakPasswordToPrivKey256(EncKey,
                             Param,
                             1,
                             KeyLength,Key);
{$ENDIF SHA256}
    F := PKR.FindField('encryptedContent')^;
    if KeyedIV then
      Key.VectorSize := TBlockCipherClass(CipherClass).BlockVectorSize
    else
      Key.VectorSize := 0;
    C := CipherClass.CreateIntf(Key);
    try
      PKR.EditField('/encryptionAlgorithm/algorithm',EncAlg);
      if (C is TBlockCipher) and not KeyedIV then begin
        RawRandomIntf(IV,Length(C.IVector)*8);
        C.SetVectorBuf(IV.Key^,IV.KeyLen);
        A := PKR.FindField('/encryptionAlgorithm/iv')^;
        A.SetContent(IV.Key^,IV.KeyLen);
      end;
      C.Encrypt(F.Content^,F.Length);
    finally
      C.Free;
    end;

{$IFDEF SHA1}
{$IFDEF SHA256}
    Param := TSecretKey.CreateStr('Authentication Key');
    WeakPasswordToPrivKey256(EncKey,
                             Param,
                             1,
                             KeyLength,HMacKey);
    D := PKR.FindField('encryptedContent')^;
    HMac(HMacKey,D.Content^,D.Length,haSHA1,20,Mac);
    F := PKR.FindField('hMAC')^;
    F.SetContent(Mac.Key^,20);
{$ENDIF SHA256}
{$ENDIF SHA1}

    if ContentID = '' then begin
      SetLength(ContentID,16);
      RawRandom(Pointer(ContentID)^,Length(ContentID)*8);
    end;
    PKR.FindField('contentID')^.SetContent(Pointer(ContentID)^,Length(ContentID));

    PKR.SaveToStream(Dst,fmtDER);
  finally
    PKR.Free;
  end;
end;

procedure TSsPrivateKeyRingComponent.SavePrivateContentToStream(
  Dst: TStream; Src: TSecureMemoryStream; const Password: string;
  var ContentID: OctetString; KDF: TKeyDerivation; KDFIterations: Integer;
  KeyValidation: Boolean; KEKAlgorithm: string; CipherClass: TCipherClass);
var
  PW: ISecretKey;
begin
  PW := TSecretKey.Create('');
  PW.SetKey(Pointer(Password),Length(Password),0);
  SavePrivateContentToStream(Dst,Src,PW,KDF,KDFIterations,KeyValidation,KEKAlgorithm,CipherClass);
end;

procedure TSsPrivateKeyRingComponent.SavePrivateContentToStream(
  Dst: TStream; Src: TSecureMemoryStream; Password: ISecretKey;
  KDF: TKeyDerivation; KDFIterations: Integer; KeyValidation: Boolean;
  KEKAlgorithm: string; CipherClass: TCipherClass);
var
  Dummy: OctetString;
begin
  Dummy := '';
  SavePrivateContentToStream(Dst,Src,
                             Password,
                             Dummy,
                             KDF,KDFIterations,KeyValidation,KEKAlgorithm,
                             CipherClass);
end;

function TSsPrivateKeyRingComponent.SavePrivateKeyRingToFile(
  const FileName: string; Password: ISecretKey; KDF: TKeyDerivation;
  KDFIterations: Integer; KeyValidation: Boolean; KEKAlgorithm: string;
  CipherClass: TCipherClass): Boolean;
var
  Dst: TFileStream;
  MS: TSecureMemoryStream;
  CID: OctetString;
begin
  if FileExists(FileName) then begin
    Dst := TFileStream.Create(FileName,fmOpenReadWrite or fmShareExclusive);
    try
      MS := TSecureMemoryStream.Create;
      try
        Result := LoadPrivateContentFromStream(Dst,MS,Password,CID);
        if Result then
          Result := (Length(CID) = Length(FPrivateKeyRing.FCID)) and
                    CompareMem(Pointer(CID),Pointer(FPrivateKeyRing.FCID),Length(CID));
      finally
        MS.Free;
      end;
      if Result then begin
        Dst.Size := 0;
        Dst.Position := 0;
        SavePrivateKeyRingToStream(Dst,
                                   Password,
                                   KDF,KDFIterations,KeyValidation,KEKAlgorithm,
                                   CipherClass);
      end;
    finally
      Dst.Free;
    end;
  end else begin
    Result := True;
    Dst := TFileStream.Create(FileName,fmCreate);
    try
      SavePrivateKeyRingToStream(Dst,
                                 Password,
                                 KDF,KDFIterations,KeyValidation,KEKAlgorithm,
                                 CipherClass);
    finally
      Dst.Free;
    end;
  end;
end;

function TSsPrivateKeyRingComponent.SavePrivateKeyRingToFile(
  const FileName, Password: string; KDF: TKeyDerivation;
  KDFIterations: Integer; KeyValidation: Boolean; KEKAlgorithm: string;
  CipherClass: TCipherClass): Boolean;
var
  Dst: TFileStream;
  MS: TSecureMemoryStream;
  CID: OctetString;
begin
  if FileExists(FileName) then begin
    Dst := TFileStream.Create(FileName,fmOpenReadWrite or fmShareExclusive);
    try
      MS := TSecureMemoryStream.Create;
      try
        Result := LoadPrivateContentFromStream(Dst,MS,Password,CID);
        if Result then
          Result := (Length(CID) = Length(FPrivateKeyRing.FCID)) and
                    CompareMem(Pointer(CID),Pointer(FPrivateKeyRing.FCID),Length(CID));
      finally
        MS.Free;
      end;
      if Result then begin
        Dst.Size := 0;
        Dst.Position := 0;
        SavePrivateKeyRingToStream(Dst,
                                   Password,
                                   KDF,KDFIterations,KeyValidation,KEKAlgorithm,
                                   CipherClass);
      end;
    finally
      Dst.Free;
    end;
  end else begin
    Result := True;
    Dst := TFileStream.Create(FileName,fmCreate);
    try
      SavePrivateKeyRingToStream(Dst,
                                 Password,
                                 KDF,KDFIterations,KeyValidation,KEKAlgorithm,
                                 CipherClass);
    finally
      Dst.Free;
    end;
  end;
end;

procedure TSsPrivateKeyRingComponent.SavePrivateKeyRingToStream(
  Stream: TStream; const Password: string; KDF: TKeyDerivation;
  KDFIterations: Integer; KeyValidation: Boolean; KEKAlgorithm: string;
  CipherClass: TCipherClass);
var
  PW: ISecretKey;
begin
  Assert(Password <> '');
  PW := TSecretKey.Create('');
  PW.SetLength(Length(Password));
  PW.SetKeyStrAt(Password,0);
  FPrivateKeyRing.SavePrivateKeyRingToStream(Stream,PW,
                                             KDF,KDFIterations,KeyValidation,KEKAlgorithm,
                                             CipherClass);
end;

procedure TSsPrivateKeyRingComponent.SavePrivateKeyRingToStream(
  Stream: TStream; Password: ISecretKey; KDF: TKeyDerivation;
  KDFIterations: Integer; KeyValidation: Boolean; KEKAlgorithm: string;
  CipherClass: TCipherClass);
begin
  FPrivateKeyRing.SavePrivateKeyRingToStream(Stream,Password,
                                             KDF,KDFIterations,KeyValidation,KEKAlgorithm,
                                             CipherClass);
end;

procedure TSsPrivateKeyRingComponent.SetAllowPlainTextKeys(
  const Value: Boolean);
begin
  FPrivateKeyRing.AllowPlainTextKeys := Value;
end;

procedure TSsPrivateKeyRingComponent.SetCacheKeyInterfaces(
  const Value: Boolean);
begin
  FPrivateKeyRing.CacheKeyInterfaces := Value;
end;

procedure TSsPrivateKeyRingComponent.SetDefaultHashAlgorithm(
  const Value: THashAlgorithm);
begin
  FDefaultHashAlgorithm := Value;
end;

procedure TSsPrivateKeyRingComponent.SetExternalIFPrivateKeyClass(
  const Value: TExternalIFPrivateKeyClass);
begin
  FExternalIFPrivateKeyClass := Value;
end;

procedure TSsPrivateKeyRingComponent.SetHoursOffsetFromGMT(
  const Value: Double);
begin
  FPrivateKeyRing.HoursOffsetFromGMT := Value;
end;

procedure TSsPrivateKeyRingComponent.SetLongTermKeys(index: Integer;
  const Value: TASN1Struct);
begin
  FPrivateKeyRing.SetLongTermKeys(index,Value);
end;

procedure TSsPrivateKeyRingComponent.SetOnAdminPassword(
  const Value: TPasswordEvent);
begin
  FOnAdminPassword := Value;
end;

procedure TSsPrivateKeyRingComponent.SetOnPassword(
  const Value: TPasswordEvent);
begin
  FOnPassword := Value;
end;

procedure TSsPrivateKeyRingComponent.SetPrivateKeyRing(
  const Value: TSsPrivateKeyRingComponent);
begin
  if Assigned(FPKRComp) then
    InterlockedDecrement(FPKRComp.FHijackCount);
  FPKRComp := Value;
  if Assigned(Value) then begin
    Value.FreeNotification(Self);
    if FHijackCount > 0 then begin
      FPKRComp := nil;
      raise Exception.Create('TSsPrivateKeyRingComponent.SetPrivateKeyRing: Multiple chaining not allowed');
    end else if Value = Self then begin
      FPKRComp := nil;
      raise Exception.Create('TSsPrivateKeyRingComponent.SetPrivateKeyRing: Circular reference not allowed');
    end else if Assigned(Value.FPKRComp) then
      SetPrivateKeyRing(FPKRComp)
    else begin
      InterlockedIncrement(Value.FHijackCount);
      FPrivateKeyRing := Value.FPrivateKeyRing;
      SetPrivateKeyRingFile(nil);
      SetPrivateKeyRingFileName('');
    end;
    OnAdminPassword := Value.OnAdminPassword;
    OnPassword := Value.OnPassword;
  end else begin
    FPrivateKeyRing := FMyPrivateKeyRing;
  end;
end;

procedure TSsPrivateKeyRingComponent.SetPrivateKeyRingFile(
  const Value: TResourceFile);
var
  Loading: Boolean;
  Dummy: ISecretKey;
begin
  if Value = nil then begin
    if Assigned(FPrivateKeyRingFile) then
      FPrivateKeyRingFile.RemoveLoadNotification(Self,DoLoadPrivateKeyRingFile);
    FPrivateKeyRingFile := nil;
  end else if (Value <> FPrivateKeyRingFile) and
              Assigned(Value.DataStream) and
              (Value.DataStream.Size > 0) then begin
    try
      Value.FreeNotification(Self);
      SetPrivateKeyRing(nil);
      FPrivateKeyRingFile := Value;
      Value.AddLoadNotification(Self,DoLoadPrivateKeyRingFile);
      Loading := csLoading in ComponentState;
      if Assigned(Owner) and not Loading then
        Loading := csLoading in Owner.ComponentState;
      if not Loading then begin
        Dummy := nil;
        if not LoadPrivateKeyRingFromStream(Value.DataStream,Dummy) then
          raise Exception.Create('Unable to load private key ring from ' +
                                 Value.Name);
        FPrivateKeyRingFileName := '';
      end;
      {$IFDEF DEMO}
      MessageDlg('Saving private key rings in the module resource may '#13#10 +
                 'jeopardize the security of your system. It is '#13#10 +
                 'essential that the password is entered at run time'#13#10 +
                 'or that the application it self is kept strictly'#13#10 +
                 'confidential.'#13#10#13#10 +
                 'This message has been added to help you evaluate '#13#10 +
                 'StrSecII and is shown only in the trial version.',
                 mtWarning,[mbOK],0);
      {$ENDIF}
    except
      SetPrivateKeyRingFile(nil);
      raise;
    end;
  end;
end;

procedure TSsPrivateKeyRingComponent.SetPrivateKeyRingFileName(
  const Value: TFileName);
var
  Loading: Boolean;
begin
  if Value = '' then
    FPrivateKeyRingFileName := ''
  else if Value <> FPrivateKeyRingFileName then begin
    try
      SetPrivateKeyRing(nil);
      FPrivateKeyRingFileName := Value;
      Loading := csLoading in ComponentState;
      if Assigned(Owner) and not Loading then
        Loading := csLoading in Owner.ComponentState;
      if not Loading then begin
        FPassword := nil;
        if LoadPrivateKeyRingFromFile(Value,nil) then
          SetPrivateKeyRingFile(nil)
        else
          FPrivateKeyRingFileName := '';
      end;
    except
      FPrivateKeyRingFileName := '';
    end;
  end;
end;

procedure TSsPrivateKeyRingComponent.SetSessionKeyLifeSpan(
  const Value: TTime);
begin
  FPrivateKeyRing.SetSessionKeyLifeSpan(Value);
end;

procedure TSsPrivateKeyRingComponent.SetSessionKeys(index: Integer;
  const Value: TASN1Struct);
begin
  FPrivateKeyRing.SetSessionKeys(index,Value);
end;

function TSsPrivateKeyRingComponent.Sign(Signed: ISigned;
  PublicKey: IPublicKeyInfo; HA: THashAlgorithm): Boolean;
var
  OID, PKI: string;
  CADSAKey: TDLPrivateKey;
  CAECKey: TECPrivateKey;
  CARSAKey: TIFPrivateKey;
begin
  Result := False;
  OID := PublicKey.GetAlgorithm.Algorithm;
  if OID <> '' then begin
    PKI := PublicKey.PublicKeyIdentifier;
    if OID = rsaEncryption then begin
      FillChar(CARSAKey,SizeOf(CARSAKey),0);
      try
        if FindPrivateKey(Pointer(PKI),Length(PKI),CARSAKey) then
          Result := Signed.RSASign(CARSAKey,HA);
      finally
        DisposeIFPrivateKey(CARSAKey);
      end;
    end else if (OID = id_rsassa_pss) or (OID = id_rsaes_oaep) then begin
      FillChar(CARSAKey,SizeOf(CARSAKey),0);
      try
        if FindPrivateKey(Pointer(PKI),Length(PKI),CARSAKey) then
          Result := Signed.RSASSA_PSSSign(CARSAKey);
      finally
        DisposeIFPrivateKey(CARSAKey);
      end;
    end else if OID = id_dsa then begin
      FillChar(CADSAKey,SizeOf(CADSAKey),0);
      try
        if FindPrivateKey(Pointer(PKI),Length(PKI),CADSAKey) then
          Result := Signed.DSASign(CADSAKey);
      finally
        DisposeDLPrivateKey(CADSAKey);
      end;
    end else if OID = id_ecPublicKey then begin
      FillChar(CAECKey,SizeOf(CAECKey),0);
      try
        if FindPrivateKey(Pointer(PKI),Length(PKI),CAECKey) then
          Result := Signed.ECDSASign(CAECKey);
      finally
        DisposeECPrivateKey(CAECKey);
      end;
    end
  end;
end;

function TSsPrivateKeyRingComponent.SignBuf(const Buf; BufLen: Integer;
  PKI: Pointer; PKILen: Integer; HA: THashAlgorithm; var Sign;
  var SignLen: Integer): Boolean;
var
  SKey: IKey;
  PrivKey: IMPPrivateKey;
  Dummy: Integer;
  KR: TKeyRings;
begin
  KR := [krSession,krLongTerm,krEncrypted,krSignedEncrypted];
  SKey := FPrivateKeyRing.FindCreatePrivateKey(PKI,PKILen,Dummy,KR);
  Result := Assigned(SKey) and (SKey.QueryInterface(IMPPrivateKey,PrivKey) = 0);
  if Result then begin
    PrivKey.SignatureDigestAlg := HA;
    SignLen := PrivKey.SignBuf(Buf,BufLen,Sign,SignLen);
    Result := SignLen > 0;
  end;
end;

function TSsPrivateKeyRingComponent.SignCertificate(DstCert,
  CACert: TASN1Struct; HA: THashAlgorithm): Boolean;
var
  PrivKey: IMPPrivateKey;
  Crt: TCertificate;
  CrtIntf: ISigned;
begin
  if CACert = nil then
    raise Exception.Create('Cannot sign without a CA certificate');
  PrivKey := FindCreatePrivateKey(CACert);
  Result := Assigned(PrivKey);
  if Result then begin
    Crt := TCertificate.Create(nil,nil);
    try
      Crt.AssignStruct(DstCert);
      PrivKey.SignatureDigestAlg := HA;
      CrtIntf := Crt;
      Result := PrivKey.SignSigned(CrtIntf,CACert);
      if Result then
        DstCert.Assign(Crt.Data);
    finally
      Crt.Free;
    end;
  end;
end;

function TSsPrivateKeyRingComponent.SignCertificate(DstCert: TCertificate;
  CACert: TASN1Struct; HA: THashAlgorithm): Boolean;
var
  PrivKey: IMPPrivateKey;
  CrtIntf: ISigned;
begin      
  if CACert = nil then
    raise Exception.Create('Cannot sign without a CA certificate');
  PrivKey := FindCreatePrivateKey(CACert);
  Result := Assigned(PrivKey);
  if Result then begin
    PrivKey.SignatureDigestAlg := HA;
    DstCert.Data.CalculateLength;
    CrtIntf := DstCert;
    Result := PrivKey.SignSigned(CrtIntf,CACert);
  end;
end;

function TSsPrivateKeyRingComponent.SignSigned(Signed: TSigned;
  CACert: IPublicKeyInfo; HA: THashAlgorithm): Boolean;
var
  PKI: string;
  KR: TKeyRings;
  Index: Integer;
  SK: IKey;
  PrivKey: IMPPrivateKey;
  CrtIntf: ISigned;
begin
  PKI := CACert.PublicKeyIdentifier;
  KR := [krLongTerm,krSignedEncrypted];
  SK := FPrivateKeyRing.FindCreatePrivateKey(Pointer(PKI),Length(PKI),Index,KR);
  Result := Assigned(SK);
  if Result then begin
    Result := SK.QueryInterface(IMPPrivateKey,PrivKey) = 0;
    if Result then begin
      PrivKey.SignatureDigestAlg := HA;
      CrtIntf := Signed;
      Result := PrivKey.SignSigned(CrtIntf);
    end;
  end;
{
var
  OID, PKI: string;
  CADSAKey: TDLPrivateKey;
  CAECKey: TECPrivateKey;
  CARSAKey: TIFPrivateKey;
begin
  Result := False;
  OID := CACert.GetAlgorithm.Algorithm;
  if OID <> '' then begin
    PKI := CACert.PublicKeyIdentifier;
    if OID = rsaEncryption then begin
      FillChar(CARSAKey,SizeOf(CARSAKey),0);
      try
        if FindPrivateKey(Pointer(PKI),Length(PKI),CARSAKey) then
          Result := Signed.RSASign(CARSAKey,HA);
      finally
        DisposeIFPrivateKey(CARSAKey);
      end;
    end else if OID = id_dsa then begin
      FillChar(CADSAKey,SizeOf(CADSAKey),0);
      try
        if FindPrivateKey(Pointer(PKI),Length(PKI),CADSAKey) then
          Result := Signed.DSASign(CADSAKey);
      finally
        DisposeDLPrivateKey(CADSAKey);
      end;
    end else if OID = id_ecPublicKey then begin
      FillChar(CAECKey,SizeOf(CAECKey),0);
      try
        if FindPrivateKey(Pointer(PKI),Length(PKI),CAECKey) then
          Result := Signed.ECDSASign(CAECKey);
      finally
        DisposeECPrivateKey(CAECKey);
      end;
    end
  end;}
end;

procedure TSsPrivateKeyRingComponent.TranslatePrivateDLKey(F: PASN1Struct;
  const PrivKey: TDLPrivateKey; PublicKeyIdentifier: Pointer;
  PKILen: Integer; IsDSAKey: Boolean);
var
  A: PASN1Struct;
begin
  if IsDSAKey then begin
    F.EditField('privateKeyType',id_dsa);
    A := F.FindField('/privateKey//privateKey');
    A.EditField('privateKey',PrivKey.X,True);
    A.EditField('/params/p',PrivKey.Params.P,True);
    A.EditField('/params/q',PrivKey.Params.Q,True);
    A.EditField('/params/g',PrivKey.Params.G,True);
  end else begin
    F.EditField('privateKeyType',dhPublicNumber);
    A := F.FindField('/privateKey//privateKey');
    A.EditField('privateKey',PrivKey.X,True);
    A.EditField('/params/p',PrivKey.Params.P,True);
    A.EditField('/params/g',PrivKey.Params.G,True);
    A.EditField('/params/q',PrivKey.Params.Q,True);
    A.EditField('/params/j',PrivKey.Params.J,True);
  end;
  if Assigned(PublicKeyIdentifier) then begin
    A := F.FindField('/privateKey//publicKeyIdentifier');
    A.SetContent(PublicKeyIdentifier^,PKILen);
  end;
  F.CalculateLength;
end;

procedure TSsPrivateKeyRingComponent.TranslatePrivateECKey(F: PASN1Struct;
  const PrivKey: TECPrivateKey; PublicKeyIdentifier: Pointer;
  PKILen: Integer);
var
  A: PASN1Struct;
  OID: string;
begin
  F.EditField('privateKeyType',id_ecPublicKey);
  A := F.FindField('/privateKey//privateKey');

  A.EditField('privateKey',PrivKey.S,True);

  OID := '';
  if TryIsECurveP192_1(PrivKey.params) then
    OID := prime192v1
  else if TryIsECurveP192_2(PrivKey.params) then
    OID := prime192v2
  else if TryIsECurveP192_3(PrivKey.params) then
    OID := prime192v3
  else if TryIsECurveP239_1(PrivKey.params) then
    OID := prime239v1
  else if TryIsECurveP239_2(PrivKey.params) then
    OID := prime239v2
  else if TryIsECurveP239_3(PrivKey.params) then
    OID := prime239v3
  else if TryIsECurveP256_1(PrivKey.params) then
    OID := prime256v1
  else if TryIsECurveP224(PrivKey.params) then
    OID := GetRegistredOID('prime224')
  else if TryIsECurveP384(PrivKey.params) then
    OID := GetRegistredOID('prime384')
  else if TryIsECurveP521(PrivKey.params) then
    OID := GetRegistredOID('prime521');

  if OID <> '' then begin
    A.FindField('params')^.SelectChoice(1);
    A.EditField('params',OID);
  end else begin
    A.FindField('params')^.SelectChoice(0);
    A.EditField('/params/version',Integer(1));
    A.EditField('/params/fieldID/fieldType',prime_field);
    A.EditField('/params/fieldID/parameters',PrivKey.params.Q,True);
    A.EditField('/params/curve/a',PrivKey.params.A,True);
    A.EditField('/params/curve/b',PrivKey.params.B,True);
    A.EditField('/params/base',PrivKey.params.G,PrivKey.params,cHybrid);
    A.EditField('/params/order',PrivKey.params.R,True);
    A.EditField('/params/coFactor',PrivKey.params.K,True);
  end;

  if Assigned(PublicKeyIdentifier) then begin
    A := F.FindField('/privateKey//publicKeyIdentifier');
    A.SetContent(PublicKeyIdentifier^,PKILen);
  end;
  F.CalculateLength;
end;

procedure TSsPrivateKeyRingComponent.TranslatePrivateRSAKey(F: PASN1Struct;
  const PrivKey: TIFPrivateKey; PublicKeyIdentifier: Pointer;
  PKILen: Integer; RSAES_OAEP: Boolean);
var
  A: PASN1Struct;
  S: string;
begin
  if RSAES_OAEP then
    F.EditField('privateKeyType',id_rsaes_oaep)
  else
    F.EditField('privateKeyType',rsaEncryption);
  A := F.FindField('/privateKey//privateKey');
  A.SelectChoice(PrivKey.ReprType);
  if PrivKey.ReprType = 0 then begin
    A.EditField('n',PrivKey.zN,True);
    A.EditField('d',PrivKey.zD,True);
  end else if PrivKey.ReprType = 1 then begin
    A.EditField('p',PrivKey.oP,True);
    A.EditField('q',PrivKey.oQ,True);
    A.EditField('d1',PrivKey.oD1,True);
    A.EditField('d2',PrivKey.oD2,True);
    A.EditField('c',PrivKey.oC,True);
  end else if PrivKey.ReprType = 2 then begin
    A.EditField('p',PrivKey.tP,True);
    A.EditField('q',PrivKey.tQ,True);
    A.EditField('d',PrivKey.tD,True);
  end else if (PrivKey.ReprType = 3) and Assigned(PrivKey.ExternalKey) then begin
    A := F.FindField('identifier');
    S := PrivKey.ExternalKey.Identifier;
    A.SetContent(Pointer(S)^,Length(S));
  end;
  if Assigned(PublicKeyIdentifier) then begin
    A := F.FindField('/privateKey//publicKeyIdentifier');
    A.SetContent(PublicKeyIdentifier^,PKILen);
  end;
  F.CalculateLength;
end;
                       
function TSsPrivateKeyRingComponent.ValidateMyCert(
  const Cert: TASN1Struct): Boolean;
var
  Crt: TSubjectPublicKeyInfo;
  PrivKey: IMPPrivateKey;
begin
  Result := False;
  try
    Crt := TSubjectPublicKeyInfo.CreateFromCert(Cert);
  except
    Crt := nil;
  end;
  try
    try
      PrivKey := FindCreatePrivateKey(Cert);
    except
      raise Exception.Create('ValidateMyCert: Exception in FindCreatePrivateKey');
    end;
    Result := Assigned(PrivKey);
    if Result then begin
      PrivKey.PublKey := Crt;
      if Assigned(Crt) then
        Result := PrivKey.PublKey <> nil;
    end;
  finally
    if not Result then
      Crt.Free;
  end;
end;

procedure TSsPrivateKeyRingComponent.SetExceptions(const Value: Boolean);
begin
  FExceptions := Value;
end;

function TSsPrivateKeyRingComponent.LoadPrivateKeyFromFile(
  AFileName: TFileName; Password: ISecretKey): Boolean;
var
  PrivKey: TIFPrivateKey;
  Iden: OctetString;
  Dummy: Boolean;
  PWDummy: ISecretKey;
  Idx: Integer;
begin
  FillChar(PrivKey,SizeOf(PrivKey),0);
  try
    if LowerCase(ExtractFileExt(AFileName)) = '.pvk' then begin
      Result := LoadRSAPrivateKeyFromPVKFile(AFileName,Password,PrivKey);
{$IFDEF GUI_APP}
      if (Password = nil) and not Result then begin
        PasswordDlg('Private Key ' + AFileName,Password);
        Result := LoadRSAPrivateKeyFromPVKFile(AFileName,Password,PrivKey);
      end;
{$ENDIF GUI_APP}
    end else
      Result := LoadRSAPrivateKeyFromKEYFile(AFileName,PrivKey);
    if Result then begin
      Iden := MpIF.KeyIdentifierFromPrivateKey(PrivKey);
      if FindCreatePrivateKey(Pointer(Iden),Length(Iden),Dummy,Idx) = nil then begin
        if not AllowPlainTextKeys then begin
          PWDummy := nil;
          AdminLogin(PWDummy);
        end;
        Result := AddPrivateRSAKey(PrivKey,
                                   Pointer(Iden),Length(Iden),
                                   True) >= 0;
      end;
    end;
  finally
    DisposeIFPrivateKey(PrivKey);
  end;
end;

function TSsPrivateKeyRingComponent.SavePrivateKeyToFile(Cert: TASN1Struct;
  const FileName: string; Password: ISecretKey): Boolean;
var
  Priv: TMPPrivateKey;
  Alg: LongWord;
  P: Pointer;
  K: PRSAKeyBlob;
  Len: Integer;
begin
  Priv := FindCreatePrivateKey(Cert);
  Result := Assigned(Priv) and (Priv is TMPIFPrivateKey);
  if Result then begin
    if keyEncipherment in ExtractKeyUsage(Cert) then
      Alg := CALG_RSA_KEYX
    else
      Alg := CALG_RSA_SIGN;
    Result := (Priv as TMPIFPrivateKey).ExportToPRIVATEKEYBLOB(P,Len,Alg,True);
    if Result then try
      K := P;
      Result := SavePRIVATEKEYBLOBToPVKFile(FileName,Password,K,Len);
    finally
      ProtectClear(P^,Len);
      FreeMem(P);
    end;
  end;
end;

{ TNewCertDlgObject }

procedure TNewCertDlgObject.AfterExecute(ATLSServer: ITLSServer);
begin
{
  Override this function and add code that will execute after FCert has been
  created and signed, e.g. to save it to a file.
}
  if Assigned(FExportData) then begin
    if Assigned(FP10) then
      FP10.SaveToStream(FExportData);
    if Assigned(FCert) then
      FCert.SaveToStream(FExportData);
  end;
end;

procedure TNewCertDlgObject.ComposeCertificate;
begin
{
  Override this function and add code that will check and/or fill
  the fields of FCert.
}
end;

function TNewCertDlgObject.Execute(ATLSServer: ITLSServer): Boolean;
begin
  try
    FNewCert := nil;
    if Assigned(FImportData) then begin
      FImportData.Position := 0;
      if not LoadCRMStream(FImportData,crmPKCS10) then begin
        FImportData.Position := 0;
        if not LoadCRMStream(FImportData,crmCert) then begin
          FImportData := nil;
          raise Exception.Create('Wrong or Unknown format');
        end;
      end;
    end;
    try
      if FCreateOpt = nccCertReq then
        FP10 := TCertificationRequest.Create(nil,nil)
      else
        FCert := TCertificate.Create(nil,nil);
      try
        ComposeCertificate;
        ATLSServer.KeyAndSign(Self);
        AfterExecute(ATLSServer);
        Result := True;
      finally
        FP10.Free;
        FCert.Free;
      end;
    finally
      FNewCert.Free;
    end;
  except
    Result := False;
  end;
end;

function TNewCertDlgObject.GetCACert: TASN1Struct;
begin
  Result := FCACert;
end;

function TNewCertDlgObject.GetCert: TCertificate;
begin
  Result := FCert;
end;

function TNewCertDlgObject.GetChallengePassword: WideString;
begin
  Result := FChallengePassword;
end;

function TNewCertDlgObject.GetCreateOpt: TNewCertCreateEnum;
begin
  Result := FCreateOpt;
end;

function TNewCertDlgObject.GetExportableRSA: Boolean;
begin
  Result := FExportableRSA;
end;

function TNewCertDlgObject.GetExportData: TStream;
begin
  Result := FExportData;
end;

function TNewCertDlgObject.GetImportData: TStream;
begin
  Result := FImportData;
end;

function TNewCertDlgObject.GetIssuedCerts: TX509TrustedCertificates;
begin
  Result := FIssuedCerts;
end;

function TNewCertDlgObject.GetKeySize: Integer;
begin
  Result := FKeySize;
end;

function TNewCertDlgObject.GetMyCerts: TX509TrustedCertificates;
begin
  Result := FMyCerts;
end;

function TNewCertDlgObject.GetP10: TCertificationRequest;
begin
  Result := FP10;
end;

function TNewCertDlgObject.GetPrivateKeyAlg: ObjectIdentifier;
begin
  Result := FPrivateKeyAlg;
end;

function TNewCertDlgObject.GetPrivateKeyRing: TSsPrivateKeyRingComponent;
begin
  Result := FPrivateKeyRing;
end;

function TNewCertDlgObject.GetRegToken: WideString;
begin
  Result := FRegToken;
end;

function TNewCertDlgObject.GetRootCerts: TX509TrustedCertificates;
begin
  Result := FRootCerts;
end;

function TNewCertDlgObject.GetSignatureDigestAlg: THashAlgorithm;
begin
  Result := FSignatureDigestAlg;
end;

function TNewCertDlgObject.GetSubjectPublicKeyInfo: TSubjectPublicKeyInfo;
begin
  Result := FNewCert.SubjectPublicKeyInfo;
end;

function TNewCertDlgObject.GetTemplateWizard: Boolean;
begin
  Result := FTemplateWizard;
end;

function TNewCertDlgObject.GetUserList: IUserList;
begin
  Result := FUserList;
end;

function TNewCertDlgObject.GetWizard: TNewCertWizardEnum;
begin
  Result := FWizard;
end;

function TNewCertDlgObject.LoadCRM(const FileName: string): Boolean;
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

function TNewCertDlgObject.LoadCRMStream(Stream: TStream;
  AKind: TCRMKind): Boolean;
var
  Cert: TCertificate;
  P10: TCertificationRequest;
  CRM: TCertReqMsg;
  Ext: TExtension;

  procedure ExtractExtensions(Extensions: TExtensions);
  begin
    if Extensions <> nil then
      FNewCert.TbsCertificate.Extensions.Assign(Extensions);
  end;

  procedure ExtractSubject(Subject: TRdnSequence);
  begin
    FNewCert.Subject.AsRdnSequence.Assign(Subject);
  end;

begin
  FVerified := False;
  Result := True;
  if AKind = crmCert then begin
    Cert := TCertificate.Create(nil,nil);
    try
      try
        Cert.LoadFromStream(Stream);
        if (CreateOpt = nccCertFromReq) and Cert.CheckSignature(nil) then begin
          FNewCert.Free;
          FNewCert := TCertificate.Create(nil,nil);
          if not Cert.SubjectPublicKeyInfo.Data.IsEmpty then
            FNewCert.SubjectPublicKeyInfo.AssignStruct(Cert.SubjectPublicKeyInfo.Data);

          ExtractSubject(Cert.Subject.AsRdnSequence);

          ExtractExtensions(Cert.TbsCertificate.Extensions);

          FNewCert.Validity.Assign(Cert.Validity);

          ChallengePassword := '';
          RegToken := '';
        end else
          Result := False;
      except
        Result := False;
      end;
    finally
      Cert.Free;
    end;
  end else if AKind = crmPKCS10 then begin
    P10 := TCertificationRequest.Create(nil,nil);
    try
      try
        P10.LoadFromStream(Stream);
        if P10.CheckSignature(nil) then begin
          FNewCert.Free;
          FNewCert := TCertificate.Create(nil,nil);
          FNewCert.SubjectPublicKeyInfo.AssignStruct(P10.CertificationRequestInfo.SubjectPKInfo.Data);

          ExtractSubject(P10.CertificationRequestInfo.Subject);

          ExtractExtensions(P10.CertificationRequestInfo.Extensions);

          ChallengePassword := P10.CertificationRequestInfo.Attributes.ChallengePassword;
          RegToken := P10.CertificationRequestInfo.Attributes.RegToken;
          if Assigned(FUserList) then begin
            Ext := FNewCert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];
            if Assigned(Ext) then
              Result := FUserList.VerifyUser(P10.Subject,Ext.ExtnValue.AsCe_SubjectAltName,RegToken)
            else
              Result := FUserList.VerifyUser(P10.Subject,nil,RegToken);
            FVerified := Result;
          end;
        end else
          Result := False;
      except
        Result := False;
      end;
    finally
      P10.Free;
    end;
  end else begin
    CRM := TCertReqMsg.Create(nil,nil);
    try
      try
        CRM.LoadFromStream(Stream);
        if CRM.CheckSignature(nil) then begin
          FNewCert.Free;
          FNewCert := TCertificate.Create(nil,nil);
          FNewCert.SubjectPublicKeyInfo.AssignStruct(CRM.CertReq.CertTemplate.PublicKey.Data);

          ExtractSubject(CRM.CertReq.CertTemplate.Subject.AsRdnSequence);

          ExtractExtensions(CRM.CertReq.CertTemplate.Extensions);

          ChallengePassword := '';
          if CRM.CertReq.Controls.UniqueItem[cveIdRegCtrl] <> nil then begin
            RegToken := CRM.CertReq.Controls.UniqueItem[cveIdRegCtrl].Value.AsRegCtrl;
            if Assigned(FUserList) then begin
              Ext := FNewCert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];
              Result := FUserList.VerifyUser(FNewCert.Subject.AsRdnSequence,Ext.ExtnValue.AsCe_SubjectAltName,RegToken);
              FVerified := Result;
            end;
          end;
        end else
          Result := False;
      except
        Result := False;
      end;
    finally
      CRM.Free;
    end;
  end;
end;

procedure TNewCertDlgObject.SelectWizard(Index: TNewCertWizardEnum;
  GenerateRequest: Boolean);
begin
  FWizard := Index;
  if GenerateRequest then
    FCreateOpt := nccCertReq
  else if Assigned(ImportData) then
    FCreateOpt := nccCertFromReq
  else
    FCreateOpt := nccCert;
end;

procedure TNewCertDlgObject.SetCACert(const Value: TASN1Struct);
begin
  FCACert := Value;
end;

procedure TNewCertDlgObject.SetChallengePassword(const Value: WideString);
begin
  FChallengePassword := Value;
end;

procedure TNewCertDlgObject.SetExportableRSA(const Value: Boolean);
begin
  FExportableRSA := Value;
end;

procedure TNewCertDlgObject.SetExportData(const Value: TStream);
begin
  FExportData := Value;
end;

procedure TNewCertDlgObject.SetImportData(const Value: TStream);
begin
  FImportData := Value;
end;

procedure TNewCertDlgObject.SetIssuedCerts(
  const Value: TX509TrustedCertificates);
begin
  FIssuedCerts := Value;
end;

procedure TNewCertDlgObject.SetKeySize(const Value: Integer);
begin
  FKeySize := Value;
end;

procedure TNewCertDlgObject.SetMyCerts(
  const Value: TX509TrustedCertificates);
begin
  FMyCerts := Value;
end;

procedure TNewCertDlgObject.SetPrivateKeyAlg(
  const Value: ObjectIdentifier);
begin
  FPrivateKeyAlg := Value;
end;

procedure TNewCertDlgObject.SetPrivateKeyRing(
  const Value: TSsPrivateKeyRingComponent);
begin
  FPrivateKeyRing := Value;
end;

procedure TNewCertDlgObject.SetRegToken(const Value: WideString);
begin
  FRegToken := Value;
end;

procedure TNewCertDlgObject.SetRootCerts(
  const Value: TX509TrustedCertificates);
begin
  FRootCerts := Value;
end;

procedure TNewCertDlgObject.SetSignatureDigestAlg(
  const Value: THashAlgorithm);
begin
  FSignatureDigestAlg := Value;
end;

procedure TNewCertDlgObject.SetTemplateWizard(const Value: Boolean);
begin
  FTemplateWizard := Value;
end;

procedure TNewCertDlgObject.SetUserList(const Value: IUserList);
begin
  FUserList := Value;
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
