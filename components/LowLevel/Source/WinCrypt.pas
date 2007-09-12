(*======================================================================*
 | unitParser                                                           |
 |                                                                      |
 | Interface unit for the WinCrypt APIs                                 |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2005  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 10.0     08/03/2006  CPWW  BDS 2006 release version                  |
 *======================================================================*)

 {Partial interface}

unit WinCrypt;

interface

uses Windows;

const
// Algorithm classes
  ALG_CLASS_ANY                   = (0);
  ALG_CLASS_SIGNATURE             = (1 shl 13);
  ALG_CLASS_MSG_ENCRYPT           = (2 shl 13);
  ALG_CLASS_DATA_ENCRYPT          = (3 shl 13);
  ALG_CLASS_HASH                  = (4 shl 13);
  ALG_CLASS_KEY_EXCHANGE          = (5 shl 13);
  ALG_CLASS_ALL                   = (7 shl 13);

// Algorithm types
  ALG_TYPE_ANY                    = (0);
  ALG_TYPE_DSS                    = (1 shl 9);
  ALG_TYPE_RSA                    = (2 shl 9);
  ALG_TYPE_BLOCK                  = (3 shl 9);
  ALG_TYPE_STREAM                 = (4 shl 9);
  ALG_TYPE_DH                     = (5 shl 9);
  ALG_TYPE_SECURECHANNEL          = (6 shl 9);

// Generic sub-ids
  ALG_SID_ANY                     = (0);

// Some RSA sub-ids
  ALG_SID_RSA_ANY                 = 0;
  ALG_SID_RSA_PKCS                = 1;
  ALG_SID_RSA_MSATWORK            = 2;
  ALG_SID_RSA_ENTRUST             = 3;
  ALG_SID_RSA_PGP                 = 4;

// Some DSS sub-ids
//
  ALG_SID_DSS_ANY                 = 0;
  ALG_SID_DSS_PKCS                = 1;
  ALG_SID_DSS_DMS                 = 2;

// Block cipher sub ids
// DES sub_ids
  ALG_SID_DES                     = 1;
  ALG_SID_3DES                    = 3;
  ALG_SID_DESX                    = 4;
  ALG_SID_IDEA                    = 5;
  ALG_SID_CAST                    = 6;
  ALG_SID_SAFERSK64               = 7;
  ALG_SID_SAFERSK128              = 8;
  ALG_SID_3DES_112                = 9;
  ALG_SID_CYLINK_MEK              = 12;
  ALG_SID_RC5                     = 13;
  ALG_SID_AES_128                 = 14;
  ALG_SID_AES_192                 = 15;
  ALG_SID_AES_256                 = 16;
  ALG_SID_AES                     = 17;

// Fortezza sub-ids
  ALG_SID_SKIPJACK                = 10;
  ALG_SID_TEK                     = 11;

// KP_MODE
  CRYPT_MODE_CBCI                 = 6;       // ANSI CBC Interleaved
  CRYPT_MODE_CFBP                 = 7;       // ANSI CFB Pipelined
  CRYPT_MODE_OFBP                 = 8;       // ANSI OFB Pipelined
  CRYPT_MODE_CBCOFM               = 9;       // ANSI CBC + OF Masking
  CRYPT_MODE_CBCOFMI              = 10;      // ANSI CBC + OFM Interleaved

// RC2 sub-ids
  ALG_SID_RC2                     = 2;

// Stream cipher sub-ids
  ALG_SID_RC4                     = 1;
  ALG_SID_SEAL                    = 2;

// Diffie-Hellman sub-ids
  ALG_SID_DH_SANDF                = 1;
  ALG_SID_DH_EPHEM                = 2;
  ALG_SID_AGREED_KEY_ANY          = 3;
  ALG_SID_KEA                     = 4;

// Hash sub ids
  ALG_SID_MD2                     = 1;
  ALG_SID_MD4                     = 2;
  ALG_SID_MD5                     = 3;
  ALG_SID_SHA                     = 4;
  ALG_SID_SHA1                    = 4;
  ALG_SID_MAC                     = 5;
  ALG_SID_RIPEMD                  = 6;
  ALG_SID_RIPEMD160               = 7;
  ALG_SID_SSL3SHAMD5              = 8;
  ALG_SID_HMAC                    = 9;
  ALG_SID_TLS1PRF                 = 10;
  ALG_SID_HASH_REPLACE_OWF        = 11;

// secure channel sub ids
  ALG_SID_SSL3_MASTER             = 1;
  ALG_SID_SCHANNEL_MASTER_HASH    = 2;
  ALG_SID_SCHANNEL_MAC_KEY        = 3;
  ALG_SID_PCT1_MASTER             = 4;
  ALG_SID_SSL2_MASTER             = 5;
  ALG_SID_TLS1_MASTER             = 6;
  ALG_SID_SCHANNEL_ENC_KEY        = 7;

// Our silly example sub-id
  ALG_SID_EXAMPLE                 = 80;

// algorithm identifier definitions
  CALG_MD2                = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_MD2);
  CALG_MD4                = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_MD4);
  CALG_MD5                = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_MD5);
  CALG_SHA                = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_SHA);
  CALG_SHA1               = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_SHA1);
  CALG_MAC                = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_MAC);
  CALG_RSA_SIGN           = (ALG_CLASS_SIGNATURE  or  ALG_TYPE_RSA  or  ALG_SID_RSA_ANY);
  CALG_DSS_SIGN           = (ALG_CLASS_SIGNATURE  or  ALG_TYPE_DSS  or  ALG_SID_DSS_ANY);
  CALG_NO_SIGN            = (ALG_CLASS_SIGNATURE  or  ALG_TYPE_ANY  or  ALG_SID_ANY);
  CALG_RSA_KEYX           = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_RSA or ALG_SID_RSA_ANY);
  CALG_DES                = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_DES);
  CALG_3DES_112           = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_3DES_112);
  CALG_3DES               = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_3DES);
  CALG_DESX               = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_DESX);
  CALG_RC2                = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_RC2);
  CALG_RC4                = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_STREAM or ALG_SID_RC4);
  CALG_SEAL               = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_STREAM or ALG_SID_SEAL);
  CALG_DH_SF              = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_DH_SANDF);
  CALG_DH_EPHEM           = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_DH_EPHEM);
  CALG_AGREEDKEY_ANY      = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_AGREED_KEY_ANY);
  CALG_KEA_KEYX           = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_KEA);
  CALG_HUGHES_MD5         = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_ANY or ALG_SID_MD5);
  CALG_SKIPJACK           = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_SKIPJACK);
  CALG_TEK                = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_TEK);
  CALG_CYLINK_MEK         = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_CYLINK_MEK);
  CALG_SSL3_SHAMD5        = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_SSL3SHAMD5);
  CALG_SSL3_MASTER        = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SSL3_MASTER);
  CALG_SCHANNEL_MASTER_HASH  = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_MASTER_HASH);
  CALG_SCHANNEL_MAC_KEY   = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_MAC_KEY);
  CALG_SCHANNEL_ENC_KEY   = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_ENC_KEY);
  CALG_PCT1_MASTER        = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_PCT1_MASTER);
  CALG_SSL2_MASTER        = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SSL2_MASTER);
  CALG_TLS1_MASTER        = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_TLS1_MASTER);
  CALG_RC5                = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_RC5);
  CALG_HMAC               = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_HMAC);
  CALG_TLS1PRF            = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_TLS1PRF);
  CALG_HASH_REPLACE_OWF   = (ALG_CLASS_HASH  or  ALG_TYPE_ANY  or  ALG_SID_HASH_REPLACE_OWF);
  CALG_AES_128            = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_128);
  CALG_AES_192            = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_192);
  CALG_AES_256            = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_256);
  CALG_AES                = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES);

  PROV_RSA_FULL           = 1;
  PROV_RSA_SIG            = 2;
  PROV_DSS                = 3;
  PROV_FORTEZZA           = 4;
  PROV_MS_EXCHANGE        = 5;
  PROV_SSL                = 6;
  PROV_RSA_SCHANNEL       = 12;
  PROV_DSS_DH             = 13;
  PROV_EC_ECDSA_SIG       = 14;
  PROV_EC_ECNRA_SIG       = 15;
  PROV_EC_ECDSA_FULL      = 16;
  PROV_EC_ECNRA_FULL      = 17;
  PROV_DH_SCHANNEL        = 18;
  PROV_SPYRUS_LYNKS       = 20;
  PROV_RNG                = 21;
  PROV_INTEL_SEC          = 22;
  PROV_REPLACE_OWF        = 23;
  PROV_RSA_AES            = 24;

  CRYPT_EXPORTABLE        = $00000001;
  CRYPT_USER_PROTECTED    = $00000002;
  CRYPT_CREATE_SALT       = $00000004;
  CRYPT_UPDATE_KEY        = $00000008;
  CRYPT_NO_SALT           = $00000010;
  CRYPT_PREGEN            = $00000040;
  CRYPT_RECIPIENT         = $00000010;
  CRYPT_INITIATOR         = $00000040;
  CRYPT_ONLINE            = $00000080;
  CRYPT_SF                = $00000100;
  CRYPT_CREATE_IV         = $00000200;
  CRYPT_KEK               = $00000400;
  CRYPT_DATA_KEY          = $00000800;
  CRYPT_VOLATILE          = $00001000;
  CRYPT_SGCKEY            = $00002000;
  CRYPT_ARCHIVABLE        = $00004000;

  RSA1024BIT_KEY          = $04000000;

// dwFlags definitions for CryptDeriveKey
  CRYPT_SERVER            = $00000400;

  KEY_LENGTH_MASK         = $FFFF0000;

// dwFlag definitions for CryptExportKey
  CRYPT_Y_ONLY            = $00000001;
  CRYPT_SSL2_FALLBACK     = $00000002;
  CRYPT_DESTROYKEY        = $00000004;
  CRYPT_OAEP              = $00000040;  // used with RSA encryptions/decryptions
                                            // CryptExportKey, CryptImportKey,
                                            // CryptEncrypt and CryptDecrypt

  CRYPT_BLOB_VER3         = $00000080;  // export version 3 of a blob type
  CRYPT_IPSEC_HMAC_KEY    = $00000100;  // CryptImportKey only

// dwFlags definitions for CryptCreateHash
  CRYPT_SECRETDIGEST      = $00000001;

// dwFlags definitions for CryptHashData
  CRYPT_OWF_REPL_LM_HASH  = $00000001;  // this is only for the OWF replacement CSP

// dwFlags definitions for CryptHashSessionKey
  CRYPT_LITTLE_ENDIAN     = $00000001;

// dwFlags definitions for CryptSignHash and CryptVerifySignature
  CRYPT_NOHASHOID         = $00000001;
  CRYPT_TYPE2_FORMAT      = $00000002;
  CRYPT_X931_FORMAT       = $00000004;

// dwFlag definitions for CryptSetProviderEx and CryptGetDefaultProvider
  CRYPT_MACHINE_DEFAULT   = $00000001;
  CRYPT_USER_DEFAULT      = $00000002;
  CRYPT_DELETE_DEFAULT    = $00000004;

// dwFlag definitions for CryptAcquireContext
  CRYPT_NEWKEYSET         = $00000008;
  CRYPT_MACHINE_KEYSET    = $00000020;


  MS_DEF_PROV              = 'Microsoft Base Cryptographic Provider v1.0';
  MS_ENHANCED_PROV         = 'Microsoft Enhanced Cryptographic Provider v1.0';
  MS_STRONG_PROV           = 'Microsoft Strong Cryptographic Provider';
  MS_DEF_RSA_SIG_PROV      = 'Microsoft RSA Signature Cryptographic Provider';
  MS_DEF_RSA_SCHANNEL_PROV = 'Microsoft RSA SChannel Cryptographic Provider';
  MS_DEF_DSS_PROV          = 'Microsoft Base DSS Cryptographic Provider';
  MS_DEF_DSS_DH_PROV       = 'Microsoft Base DSS and Diffie-Hellman Cryptographic Provider';
  MS_ENH_DSS_DH_PROV       = 'Microsoft Enhanced DSS and Diffie-Hellman Cryptographic Provider';
  MS_DEF_DH_SCHANNEL_PROV  = 'Microsoft DH SChannel Cryptographic Provider';
  MS_SCARD_PROV            = 'Microsoft Base Smart Card Crypto Provider';

//
// CryptGetProvParam
//
  PP_ENUMALGS             = 1;
  PP_ENUMCONTAINERS       = 2;
  PP_IMPTYPE              = 3;
  PP_NAME                 = 4;
  PP_VERSION              = 5;
  PP_CONTAINER            = 6;
  PP_CHANGE_PASSWORD      = 7;
  PP_KEYSET_SEC_DESCR     = 8;       // get/set security descriptor of keyset
  PP_CERTCHAIN            = 9;       // for retrieving certificates from tokens
  PP_KEY_TYPE_SUBTYPE     = 10;
  PP_PROVTYPE             = 16;
  PP_KEYSTORAGE           = 17;
  PP_APPLI_CERT           = 18;
  PP_SYM_KEYSIZE          = 19;
  PP_SESSION_KEYSIZE      = 20;
  PP_UI_PROMPT            = 21;
  PP_ENUMALGS_EX          = 22;
  PP_ENUMMANDROOTS	  = 25;
  PP_ENUMELECTROOTS	  = 26;
  PP_KEYSET_TYPE	  = 27;
  PP_ADMIN_PIN            = 31;
  PP_KEYEXCHANGE_PIN      = 32;
  PP_SIGNATURE_PIN        = 33;
  PP_SIG_KEYSIZE_INC      = 34;
  PP_KEYX_KEYSIZE_INC     = 35;
  PP_UNIQUE_CONTAINER     = 36;
  PP_SGC_INFO             = 37;
  PP_USE_HARDWARE_RNG     = 38;
  PP_KEYSPEC              = 39;
  PP_ENUMEX_SIGNING_PROT  = 40;

  CRYPT_FIRST             = 1;
  CRYPT_NEXT              = 2;
  CRYPT_SGC_ENUM	  = 4;

  CRYPT_IMPL_HARDWARE     = 1;
  CRYPT_IMPL_SOFTWARE     = 2;
  CRYPT_IMPL_MIXED        = 3;
  CRYPT_IMPL_UNKNOWN      = 4;
  CRYPT_IMPL_REMOVABLE    = 8;

type
  HCRYPTPROV = THandle;
  HCRYPTKEY = THandle;
  HCRYPTHASH = THandle;
  ALG_ID = LongWord;

  TEnumAlgs = packed record
    algID : ALG_ID;
    dwBits : DWORD;
    dwNameLen : DWORD;
    szName : array [0..0] of char;
  end;
  PEnumAlgs = ^TEnumAlgs;

  TCryptoAPIBlob = record
    cbData : DWORD;
    pbData : PBYTE;
  end;
  PCryptoAPIBlob = ^TCryptoAPIBlob;

  TCryptIntegerBlob = TCryptoAPIBlob; PCryptIntegerBlob = ^TCryptIntegerBlob;
  TCryptUINTBlob    = TCryptoAPIBlob; PCryptUINTBlob    = ^TCryptUINTBlob;
  TCryptOBJIDBlob   = TCryptoAPIBlob; PCryptOBJIDBlob   = ^TCryptOBJIDBlob;
  TCertNameBlob     = TCryptoAPIBlob; PCertNameBlob     = ^TCertNameBlob;
  TCertRDNValueBlob = TCryptoAPIBlob; PCertRDNValueBlob = ^TCertRDNValueBlob;
  TCertBlob         = TCryptoAPIBlob; PCertBlob         = ^TCertBlob;
  TCRLBlob          = TCryptoAPIBlob; PCRLBlob          = ^TCRLBlob;
  TDataBlob         = TCryptoAPIBlob; PDataBlob         = ^TDataBlob;
  TCryptDataBlob    = TCryptoAPIBlob; PCryptDataBlob    = ^TCryptDataBlob;
  TCryptHashBlob    = TCryptoAPIBlob; PCryptHashBlob    = ^TCryptHashBlob;
  TCryptDigestBlob  = TCryptoAPIBlob; PCryptDigestBlob  = ^TCryptDigestBlob;
  TCryptDerBlob     = TCryptoAPIBlob; PCryptDerBlob     = ^TCryptDerBlob;
  TCryptAttrBlob    = TCryptoAPIBlob; PCryptAttrBlob    = ^TCryptAttrBlob;

//+-------------------------------------------------------------------------
//  Cryptographic Key Provider Information
//
//  CRYPT_KEY_PROV_INFO defines the CERT_KEY_PROV_INFO_PROP_ID's pvData.
//
//  The CRYPT_KEY_PROV_INFO fields are passed to CryptAcquireContext
//  to get a HCRYPTPROV handle. The optional CRYPT_KEY_PROV_PARAM fields are
//  passed to CryptSetProvParam to further initialize the provider.
//
//  The dwKeySpec field identifies the private key to use from the container
//  For example, AT_KEYEXCHANGE or AT_SIGNATURE.
//--------------------------------------------------------------------------
  TCryptKeyProvParam = record
    dwParam : DWORD;
    pbData : PBYTE;
    cbData : DWORD;
    dwFlags : DWORD;
  end;
  PCryptKeyProvParam = ^TCryptKeyProvParam;

  TCryptKeyProvInfo = record
    pwszContainerName : PWideChar;
    pwszProvName : PWideChar;
    dwProvType : DWORD;
    dwFlags : DWORD;
    cProvParam : DWORD;
    rgProvParam : PCryptKeyProvParam;
    dwKeySpec : DWORD;
  end;
  PCryptKeyProvInfo = ^TCryptKeyProvInfo;

  TCryptAlgorithmIdentifier = record
    pszObjId : PChar;
    Parameters : TCryptObjIDBlob;
  end;
  PCryptAlgorithmIdentifier = ^TCryptAlgorithmIdentifier;

// Following are the definitions of various algorithm object identifiers
// RSA

const
  szOID_RSA               = '1.2.840.113549';
  szOID_PKCS              = '1.2.840.113549.1';
  szOID_RSA_HASH          = '1.2.840.113549.2';
  szOID_RSA_ENCRYPT       = '1.2.840.113549.3';

  szOID_PKCS_1            = '1.2.840.113549.1.1';
  szOID_PKCS_2            = '1.2.840.113549.1.2';
  szOID_PKCS_3            = '1.2.840.113549.1.3';
  szOID_PKCS_4            = '1.2.840.113549.1.4';
  szOID_PKCS_5            = '1.2.840.113549.1.5';
  szOID_PKCS_6            = '1.2.840.113549.1.6';
  szOID_PKCS_7            = '1.2.840.113549.1.7';
  szOID_PKCS_8            = '1.2.840.113549.1.8';
  szOID_PKCS_9            = '1.2.840.113549.1.9';
  szOID_PKCS_10           = '1.2.840.113549.1.10';
  szOID_PKCS_12           = '1.2.840.113549.1.12';

  szOID_RSA_RSA           = '1.2.840.113549.1.1.1';
  szOID_RSA_MD2RSA        = '1.2.840.113549.1.1.2';
  szOID_RSA_MD4RSA        = '1.2.840.113549.1.1.3';
  szOID_RSA_MD5RSA        = '1.2.840.113549.1.1.4';
  szOID_RSA_SHA1RSA       = '1.2.840.113549.1.1.5';
  szOID_RSA_SETOAEP_RSA   = '1.2.840.113549.1.1.6';

  szOID_RSA_DH            = '1.2.840.113549.1.3.1';

  szOID_RSA_data          = '1.2.840.113549.1.7.1';
  szOID_RSA_signedData    = '1.2.840.113549.1.7.2';
  szOID_RSA_envelopedData = '1.2.840.113549.1.7.3';
  szOID_RSA_signEnvData   = '1.2.840.113549.1.7.4';
  szOID_RSA_digestedData  = '1.2.840.113549.1.7.5';
  szOID_RSA_hashedData    = '1.2.840.113549.1.7.5';
  szOID_RSA_encryptedData = '1.2.840.113549.1.7.6';

  szOID_RSA_emailAddr     = '1.2.840.113549.1.9.1';
  szOID_RSA_unstructName  = '1.2.840.113549.1.9.2';
  szOID_RSA_contentType   = '1.2.840.113549.1.9.3';
  szOID_RSA_messageDigest = '1.2.840.113549.1.9.4';
  szOID_RSA_signingTime   = '1.2.840.113549.1.9.5';
  szOID_RSA_counterSign   = '1.2.840.113549.1.9.6';
  szOID_RSA_challengePwd  = '1.2.840.113549.1.9.7';
  szOID_RSA_unstructAddr  = '1.2.840.113549.1.9.8';
  szOID_RSA_extCertAttrs  = '1.2.840.113549.1.9.9';
  szOID_RSA_certExtensions = '1.2.840.113549.1.9.14';
  szOID_RSA_SMIMECapabilities = '1.2.840.113549.1.9.15';
  szOID_RSA_preferSignedData = '1.2.840.113549.1.9.15.1';

  szOID_RSA_SMIMEalg              = '1.2.840.113549.1.9.16.3';
  szOID_RSA_SMIMEalgESDH          = '1.2.840.113549.1.9.16.3.5';
  szOID_RSA_SMIMEalgCMS3DESwrap   = '1.2.840.113549.1.9.16.3.6';
  szOID_RSA_SMIMEalgCMSRC2wrap    = '1.2.840.113549.1.9.16.3.7';

  szOID_RSA_MD2           = '1.2.840.113549.2.2';
  szOID_RSA_MD4           = '1.2.840.113549.2.4';
  szOID_RSA_MD5           = '1.2.840.113549.2.5';

  szOID_RSA_RC2CBC        = '1.2.840.113549.3.2';
  szOID_RSA_RC4           = '1.2.840.113549.3.4';
  szOID_RSA_DES_EDE3_CBC  = '1.2.840.113549.3.7';
  szOID_RSA_RC5_CBCPad    = '1.2.840.113549.3.9';


  szOID_ANSI_X942         = '1.2.840.10046';
  szOID_ANSI_X942_DH      = '1.2.840.10046.2.1';

  szOID_X957              = '1.2.840.10040';
  szOID_X957_DSA          = '1.2.840.10040.4.1';
  szOID_X957_SHA1DSA      = '1.2.840.10040.4.3';

// ITU-T UsefulDefinitions
  szOID_DS                = '2.5';
  szOID_DSALG             = '2.5.8';
  szOID_DSALG_CRPT        = '2.5.8.1';
  szOID_DSALG_HASH        = '2.5.8.2';
  szOID_DSALG_SIGN        = '2.5.8.3';
  szOID_DSALG_RSA         = '2.5.8.1.1';
// NIST OSE Implementors' Workshop (OIW)
// http://nemo.ncsl.nist.gov/oiw/agreements/stable/OSI/12s_9506.w51
// http://nemo.ncsl.nist.gov/oiw/agreements/working/OSI/12w_9503.w51
  szOID_OIW               = '1.3.14';
// NIST OSE Implementors' Workshop (OIW) Security SIG algorithm identifiers
  szOID_OIWSEC            = '1.3.14.3.2';
  szOID_OIWSEC_md4RSA     = '1.3.14.3.2.2';
  szOID_OIWSEC_md5RSA     = '1.3.14.3.2.3';
  szOID_OIWSEC_md4RSA2    = '1.3.14.3.2.4';
  szOID_OIWSEC_desECB     = '1.3.14.3.2.6';
  szOID_OIWSEC_desCBC     = '1.3.14.3.2.7';
  szOID_OIWSEC_desOFB     = '1.3.14.3.2.8';
  szOID_OIWSEC_desCFB     = '1.3.14.3.2.9';
  szOID_OIWSEC_desMAC     = '1.3.14.3.2.10';
  szOID_OIWSEC_rsaSign    = '1.3.14.3.2.11';
  szOID_OIWSEC_dsa        = '1.3.14.3.2.12';
  szOID_OIWSEC_shaDSA     = '1.3.14.3.2.13';
  szOID_OIWSEC_mdc2RSA    = '1.3.14.3.2.14';
  szOID_OIWSEC_shaRSA     = '1.3.14.3.2.15';
  szOID_OIWSEC_dhCommMod  = '1.3.14.3.2.16';
  szOID_OIWSEC_desEDE     = '1.3.14.3.2.17';
  szOID_OIWSEC_sha        = '1.3.14.3.2.18';
  szOID_OIWSEC_mdc2       = '1.3.14.3.2.19';
  szOID_OIWSEC_dsaComm    = '1.3.14.3.2.20';
  szOID_OIWSEC_dsaCommSHA = '1.3.14.3.2.21';
  szOID_OIWSEC_rsaXchg    = '1.3.14.3.2.22';
  szOID_OIWSEC_keyHashSeal = '1.3.14.3.2.23';
  szOID_OIWSEC_md2RSASign = '1.3.14.3.2.24';
  szOID_OIWSEC_md5RSASign = '1.3.14.3.2.25';
  szOID_OIWSEC_sha1       = '1.3.14.3.2.26';
  szOID_OIWSEC_dsaSHA1    = '1.3.14.3.2.27';
  szOID_OIWSEC_dsaCommSHA1 = '1.3.14.3.2.28';
  szOID_OIWSEC_sha1RSASign = '1.3.14.3.2.29';
// NIST OSE Implementors' Workshop (OIW) Directory SIG algorithm identifiers
  szOID_OIWDIR            = '1.3.14.7.2';
  szOID_OIWDIR_CRPT       = '1.3.14.7.2.1';
  szOID_OIWDIR_HASH       = '1.3.14.7.2.2';
  szOID_OIWDIR_SIGN       = '1.3.14.7.2.3';
  szOID_OIWDIR_md2        = '1.3.14.7.2.2.1';
  szOID_OIWDIR_md2RSA     = '1.3.14.7.2.3.1';


// INFOSEC Algorithms
// joint-iso-ccitt(2) country(16) us(840) organization(1) us-government(101) dod(2) id-infosec(1)
  szOID_INFOSEC                       = '2.16.840.1.101.2.1';
  szOID_INFOSEC_sdnsSignature         = '2.16.840.1.101.2.1.1.1';
  szOID_INFOSEC_mosaicSignature       = '2.16.840.1.101.2.1.1.2';
  szOID_INFOSEC_sdnsConfidentiality   = '2.16.840.1.101.2.1.1.3';
  szOID_INFOSEC_mosaicConfidentiality = '2.16.840.1.101.2.1.1.4';
  szOID_INFOSEC_sdnsIntegrity         = '2.16.840.1.101.2.1.1.5';
  szOID_INFOSEC_mosaicIntegrity       = '2.16.840.1.101.2.1.1.6';
  szOID_INFOSEC_sdnsTokenProtection   = '2.16.840.1.101.2.1.1.7';
  szOID_INFOSEC_mosaicTokenProtection = '2.16.840.1.101.2.1.1.8';
  szOID_INFOSEC_sdnsKeyManagement     = '2.16.840.1.101.2.1.1.9';
  szOID_INFOSEC_mosaicKeyManagement   = '2.16.840.1.101.2.1.1.10';
  szOID_INFOSEC_sdnsKMandSig          = '2.16.840.1.101.2.1.1.11';
  szOID_INFOSEC_mosaicKMandSig        = '2.16.840.1.101.2.1.1.12';
  szOID_INFOSEC_SuiteASignature       = '2.16.840.1.101.2.1.1.13';
  szOID_INFOSEC_SuiteAConfidentiality = '2.16.840.1.101.2.1.1.14';
  szOID_INFOSEC_SuiteAIntegrity       = '2.16.840.1.101.2.1.1.15';
  szOID_INFOSEC_SuiteATokenProtection = '2.16.840.1.101.2.1.1.16';
  szOID_INFOSEC_SuiteAKeyManagement   = '2.16.840.1.101.2.1.1.17';
  szOID_INFOSEC_SuiteAKMandSig        = '2.16.840.1.101.2.1.1.18';
  szOID_INFOSEC_mosaicUpdatedSig      = '2.16.840.1.101.2.1.1.19';
  szOID_INFOSEC_mosaicKMandUpdSig     = '2.16.840.1.101.2.1.1.20';
  szOID_INFOSEC_mosaicUpdatedInteg    = '2.16.840.1.101.2.1.1.21';

type
//+-------------------------------------------------------------------------
//  Type used for an extension to an encoded content
//
//  Where the Value's CRYPT_OBJID_BLOB is in its encoded representation.
//--------------------------------------------------------------------------
  TCertExtension = record
    pszObjId : PChar;
    fCritical : BOOL;
    Value : TCryptObjIDBlob;
  end;
  PCertExtension = ^TCertExtension;

//+-------------------------------------------------------------------------
//  X509_EXTENSIONS
//  szOID_CERT_EXTENSIONS
//
//  pvStructInfo points to following CERT_EXTENSIONS.
//--------------------------------------------------------------------------
  TCertExtensions = record
    cExtensions : DWORD;
    rgExtensions : PCertExtension;
  end;
  PCertExtensions = ^TCertExtensions;



//+=========================================================================
//  Certificate Store Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//              In its most basic implementation, a cert store is simply a
//              collection of certificates and/or CRLs. This is the case when
//              a cert store is opened with all of its certificates and CRLs
//              coming from a PKCS #7 encoded cryptographic message.
//
//              Nonetheless, all cert stores have the following properties:
//               - A public key may have more than one certificate in the store.
//                 For example, a private/public key used for signing may have a
//                 certificate issued for VISA and another issued for
//                 Mastercard. Also, when a certificate is renewed there might
//                 be more than one certificate with the same subject and
//                 issuer.
//               - However, each certificate in the store is uniquely
//                 identified by its Issuer and SerialNumber.
//               - There's an issuer of subject certificate relationship. A
//                 certificate's issuer is found by doing a match of
//                 pSubjectCert->Issuer with pIssuerCert->Subject.
//                 The relationship is verified by using
//                 the issuer's public key to verify the subject certificate's
//                 signature. Note, there might be X.509 v3 extensions
//                 to assist in finding the issuer certificate.
//               - Since issuer certificates might be renewed, a subject
//                 certificate might have more than one issuer certificate.
//               - There's an issuer of CRL relationship. An
//                 issuer's CRL is found by doing a match of
//                 pIssuerCert->Subject with pCrl->Issuer.
//                 The relationship is verified by using
//                 the issuer's public key to verify the CRL's
//                 signature. Note, there might be X.509 v3 extensions
//                 to assist in finding the CRL.
//               - Since some issuers might support the X.509 v3 delta CRL
//                 extensions, an issuer might have more than one CRL.
//               - The store shouldn't have any redundant certificates or
//                 CRLs. There shouldn't be two certificates with the same
//                 Issuer and SerialNumber. There shouldn't be two CRLs with
//                 the same Issuer, ThisUpdate and NextUpdate.
//               - The store has NO policy or trust information. No
//                 certificates are tagged as being "root". Its up to
//                 the application to maintain a list of CertIds (Issuer +
//                 SerialNumber) for certificates it trusts.
//               - The store might contain bad certificates and/or CRLs.
//                 The issuer's signature of a subject certificate or CRL may
//                 not verify. Certificates or CRLs may not satisfy their
//                 time validity requirements. Certificates may be
//                 revoked.
//
//              In addition to the certificates and CRLs, properties can be
//              stored. There are two predefined property IDs for a user
//              certificate: CERT_KEY_PROV_HANDLE_PROP_ID and
//              CERT_KEY_PROV_INFO_PROP_ID. The CERT_KEY_PROV_HANDLE_PROP_ID
//              is a HCRYPTPROV handle to the private key assoicated
//              with the certificate. The CERT_KEY_PROV_INFO_PROP_ID contains
//              information to be used to call
//              CryptAcquireContext and CryptSetProvParam to get a handle
//              to the private key associated with the certificate.
//
//              There exists two more predefined property IDs for certificates
//              and CRLs, CERT_SHA1_HASH_PROP_ID and CERT_MD5_HASH_PROP_ID.
//              If these properties don't already exist, then, a hash of the
//              content is computed. (CERT_HASH_PROP_ID maps to the default
//              hash algorithm, currently, CERT_SHA1_HASH_PROP_ID).
//
//              There are additional APIs for creating certificate and CRL
//      contexts not in a store (CertCreateCertificateContext and
//      CertCreateCRLContext).
//
//--------------------------------------------------------------------------


  HCERTSTORE = pointer;

//+-------------------------------------------------------------------------
//  In a CRYPT_BIT_BLOB the last byte may contain 0-7 unused bits. Therefore, the
//  overall bit length is cbData * 8 - cUnusedBits.
//--------------------------------------------------------------------------
  TCryptBitBlob = record
    cbData : DWORD;
    pbData : PBYTE;
    cUnusedBits : DWORD;
  end;
  PCryptBitBlob = ^TCryptBitBlob;

//+-------------------------------------------------------------------------
//  Public Key Info
//
//  The PublicKey is the encoded representation of the information as it is
//  stored in the bit string
//--------------------------------------------------------------------------
  TCertPublicKeyInfo = record
    Algorithm : TCryptAlgorithmIdentifier;
    PublicKey : TCryptBitBlob;
  end;
  PCertPublicKeyInfo = ^TCertPublicKeyInfo;

//+-------------------------------------------------------------------------
//  Information stored in a certificate
//
//  The Issuer, Subject, Algorithm, PublicKey and Extension BLOBs are the
//  encoded representation of the information.
//--------------------------------------------------------------------------

  TCertInfo = record
    dwVersion : DWORD;
    SerialNumber : TCryptIntegerBlob;
    SignatureAlgorithm : TCryptAlgorithmIdentifier;
    Issuer : TCertNameBlob;
    NotBefore : TFileTime;
    NotAfter : TFileTime;
    Subject : TCertNameBlob;
    SubjectPublicKeyInfo : TCertPublicKeyInfo;
    IssuerUniqueId : TCryptBitBlob;
    SubjectUniqueId : TCryptBitBlob;
    cExtension : DWORD;
    rgExtension : PCertExtension;
  end;
  PCertInfo = ^TCertInfo;

//+-------------------------------------------------------------------------
//  An entry in a CRL
//
//  The Extension BLOBs are the encoded representation of the information.
//--------------------------------------------------------------------------
  TCRLEntry = record
    SerialNumber : TCryptIntegerBlob;
    RevocationDate : TFileTime;
    cExtension : DWORD;
    rgExtension : PCertExtension;
  end;
  PCRLEntry = ^TCRLEntry;

//+-------------------------------------------------------------------------
//  Information stored in a CRL
//
//  The Issuer, Algorithm and Extension BLOBs are the encoded
//  representation of the information.
//--------------------------------------------------------------------------
  TCRLInfo = record
    dwVersion : DWORD;
    SignatureAlgorithm : TCryptAlgorithmIdentifier;
    Issuer : TCertNameBlob;
    ThisUpdate : TFileTime;
    NextUpdate : TFileTime;
    cCRLEntry : DWORD;
    rgCRLEntry : PCRLEntry;
    cExtension : DWORD;
    rgExtension : PCertExtension;
  end;
  PCRLInfo = ^TCRLInfo;

//+-------------------------------------------------------------------------
//  CRL versions
//--------------------------------------------------------------------------
const
  CRL_V1 = 0;
  CRL_V2 = 1;

type

//+-------------------------------------------------------------------------
//  Certificate context.
//
//  A certificate context contains both the encoded and decoded representation
//  of a certificate. A certificate context returned by a cert store function
//  must be freed by calling the CertFreeCertificateContext function. The
//  CertDuplicateCertificateContext function can be called to make a duplicate
//  copy (which also must be freed by calling CertFreeCertificateContext).
//--------------------------------------------------------------------------

  TCertContext = record
    dwCertEncodingType : DWORD;
    pbCertEncoded : PBYTE;
    cbCertEncoded : DWORD;
    certInfo : PCertInfo;
    certStore : HCERTSTORE;
  end;
  PCertContext = ^TCertContext;
  PCCertContext = ^TCertContext;

//+-------------------------------------------------------------------------
//  CRL context.
//
//  A CRL context contains both the encoded and decoded representation
//  of a CRL. A CRL context returned by a cert store function
//  must be freed by calling the CertFreeCRLContext function. The
//  CertDuplicateCRLContext function can be called to make a duplicate
//  copy (which also must be freed by calling CertFreeCRLContext).
//--------------------------------------------------------------------------
  TCRLContext = record
    dwCertEncodingType : DWORD;
    pbCrlEncoded : PBYTE;
    cbCrlEncoded : DWORD;
    crlInfo : PCrlInfo;
    certStore : HCertStore;
  end;
  PCRLContext = ^TCRLContext;

  TCTLUsage = record
    cUsageIdentifier : DWORD;
    rgpszUsageIdentifier : PPChar;
  end;
  PCTLUsage = ^TCTLUsage;
  TCertEnhkeyUsage = TCTLUsage; PCertEnhkeyUsage = ^TCertEnhkeyUsage;


//+-------------------------------------------------------------------------
//  Attributes
//
//  Where the Value's PATTR_BLOBs are in their encoded representation.
//--------------------------------------------------------------------------
  TCryptAttribute = record
    pszObjId : PChar;
    cValue : DWORD;
    rgValue : PCryptAttrBlob;
  end;
  PCryptAttribute = ^TCryptAttribute;

//+-------------------------------------------------------------------------
//  An entry in a CTL
//--------------------------------------------------------------------------
  TCTLEntry = record
    SubjectIdentifier : TCryptDataBlob;          // For example, its hash
    cAttribute : DWORD;
    rgAttribute : PCryptAttribute;
  end;
  PCTLEntry = ^TCTLEntry;

//+-------------------------------------------------------------------------
//  Information stored in a CTL
//--------------------------------------------------------------------------
  TCTLInfo = record
    dwVersion : DWORD;
    SubjectUsage : TCTLUsage;
    ListIdentifier : TCryptDataBlob;     // OPTIONAL
    SequenceNumber : TCryptIntegerBlob;     // OPTIONAL
    ThisUpdate : TFileTime;
    NextUpdate : TFileTime;         // OPTIONAL
    SubjectAlgorithm : TCryptAlgorithmIdentifier;
    cCTLEntry : DWORD;
    rgCTLEntry : PCTLEntry;         // OPTIONAL
    cExtension : DWORD;
    rgExtension : PCertExtension;        // OPTIONAL
  end;
  PCTLInfo = ^TCTLInfo;

//+-------------------------------------------------------------------------
//  CTL versions
//--------------------------------------------------------------------------
const
  CTL_V1 = 0;

type
  HCRYPTMSG = pointer;
//+-------------------------------------------------------------------------
//  Certificate Trust List (CTL) context.
//
//  A CTL context contains both the encoded and decoded representation
//  of a CTL. Also contains an opened HCRYPTMSG handle to the decoded
//  cryptographic signed message containing the CTL_INFO as its inner content.
//  pbCtlContent is the encoded inner content of the signed message.
//
//  The CryptMsg APIs can be used to extract additional signer information.
//--------------------------------------------------------------------------
  TCTLContext = record
    dwMsgAndCertEncodingType : DWORD;
    pbCtlEncoded : PBYTE;
    cbCtlEncoded : DWORD;
    ctlInfo : PCTLInfo;
    certStore : HCERTSTORE;
    cryptMsg : HCRYPTMSG;
    pbCtlContent : PBYTE;
    cbCtlContent : DWORD;
  end;
  PCTLContext = ^TCTLContext;

//+-------------------------------------------------------------------------
//  Certificate, CRL and CTL property IDs
//
//  See CertSetCertificateContextProperty or CertGetCertificateContextProperty
//  for usage information.
//--------------------------------------------------------------------------
const
  CERT_KEY_PROV_HANDLE_PROP_ID        = 1;
  CERT_KEY_PROV_INFO_PROP_ID          = 2;
  CERT_SHA1_HASH_PROP_ID              = 3;
  CERT_MD5_HASH_PROP_ID               = 4;
  CERT_HASH_PROP_ID                   = CERT_SHA1_HASH_PROP_ID;
  CERT_KEY_CONTEXT_PROP_ID            = 5;
  CERT_KEY_SPEC_PROP_ID               = 6;
  CERT_IE30_RESERVED_PROP_ID          = 7;
  CERT_PUBKEY_HASH_RESERVED_PROP_ID   = 8;
  CERT_ENHKEY_USAGE_PROP_ID           = 9;
  CERT_CTL_USAGE_PROP_ID              = CERT_ENHKEY_USAGE_PROP_ID;
  CERT_NEXT_UPDATE_LOCATION_PROP_ID   = 10;
  CERT_FRIENDLY_NAME_PROP_ID          = 11;
  CERT_PVK_FILE_PROP_ID               = 12;
  CERT_DESCRIPTION_PROP_ID            = 13;
  CERT_ACCESS_STATE_PROP_ID           = 14;
  CERT_SIGNATURE_HASH_PROP_ID         = 15;
  CERT_SMART_CARD_DATA_PROP_ID        = 16;
  CERT_EFS_PROP_ID                    = 17;
  CERT_FORTEZZA_DATA_PROP_ID          = 18;
  CERT_ARCHIVED_PROP_ID               = 19;
  CERT_KEY_IDENTIFIER_PROP_ID         = 20;
  CERT_AUTO_ENROLL_PROP_ID            = 21;
  CERT_PUBKEY_ALG_PARA_PROP_ID        = 22;
  CERT_CROSS_CERT_DIST_POINTS_PROP_ID = 23;
  CERT_ISSUER_PUBLIC_KEY_MD5_HASH_PROP_ID     = 24;
  CERT_SUBJECT_PUBLIC_KEY_MD5_HASH_PROP_ID    = 25;
  CERT_ENROLLMENT_PROP_ID             = 26;
  CERT_DATE_STAMP_PROP_ID             = 27;
  CERT_ISSUER_SERIAL_NUMBER_MD5_HASH_PROP_ID  = 28;
  CERT_SUBJECT_NAME_MD5_HASH_PROP_ID  = 29;
  CERT_EXTENDED_ERROR_INFO_PROP_ID    = 30;

// Note, 32 - 35 are reserved for the CERT, CRL, CTL and KeyId file element IDs.
//       36 - 63 are reserved for future element IDs.

  CERT_RENEWAL_PROP_ID                = 64;
  CERT_ARCHIVED_KEY_HASH_PROP_ID      = 65;
  CERT_AUTO_ENROLL_RETRY_PROP_ID      = 66;
  CERT_AIA_URL_RETRIEVED_PROP_ID      = 67;
  CERT_FIRST_RESERVED_PROP_ID         = 68;

  CERT_LAST_RESERVED_PROP_ID          = $00007FFF;
  CERT_FIRST_USER_PROP_ID             = $00008000;
  CERT_LAST_USER_PROP_ID              = $0000FFFF;

  CERT_CREATE_SELFSIGN_NO_SIGN     = 1;
  CERT_CREATE_SELFSIGN_NO_KEY_INFO = 2;

//+-------------------------------------------------------------------------
//  Certificate Store Provider Types
//--------------------------------------------------------------------------
  CERT_STORE_PROV_MSG               = PChar (1);
  CERT_STORE_PROV_MEMORY            = PChar (2);
  CERT_STORE_PROV_FILE              = PChar (3);
  CERT_STORE_PROV_REG               = PChar (4);

  CERT_STORE_PROV_PKCS7             = PChar (5);
  CERT_STORE_PROV_SERIALIZED        = PChar (6);
  CERT_STORE_PROV_FILENAME_A        = PChar (7);
  CERT_STORE_PROV_FILENAME_W        = PChar (8);
  CERT_STORE_PROV_FILENAME          = CERT_STORE_PROV_FILENAME_W;
  CERT_STORE_PROV_SYSTEM_A          = PChar (9);
  CERT_STORE_PROV_SYSTEM_W          = PChar (10);
  CERT_STORE_PROV_SYSTEM            = CERT_STORE_PROV_SYSTEM_W;

  CERT_STORE_PROV_COLLECTION        = PChar (11);
  CERT_STORE_PROV_SYSTEM_REGISTRY_A = PChar (12);
  CERT_STORE_PROV_SYSTEM_REGISTRY_W = PChar (13);
  CERT_STORE_PROV_SYSTEM_REGISTRY   = CERT_STORE_PROV_SYSTEM_REGISTRY_W;
  CERT_STORE_PROV_PHYSICAL_W        = PChar (14);
  CERT_STORE_PROV_PHYSICAL          = CERT_STORE_PROV_PHYSICAL_W;
  CERT_STORE_PROV_SMART_CARD_W      = PChar (15);
  CERT_STORE_PROV_SMART_CARD        = CERT_STORE_PROV_SMART_CARD_W;
  CERT_STORE_PROV_LDAP_W            = PChar (16);
  CERT_STORE_PROV_LDAP              = CERT_STORE_PROV_LDAP_W;

  sz_CERT_STORE_PROV_MEMORY            = 'Memory';
  sz_CERT_STORE_PROV_FILENAME_W        = 'File';
  sz_CERT_STORE_PROV_FILENAME          = sz_CERT_STORE_PROV_FILENAME_W;
  sz_CERT_STORE_PROV_SYSTEM_W          = 'System';
  sz_CERT_STORE_PROV_SYSTEM            = sz_CERT_STORE_PROV_SYSTEM_W;
  sz_CERT_STORE_PROV_PKCS7             = 'PKCS7';
  sz_CERT_STORE_PROV_SERIALIZED        = 'Serialized';

  sz_CERT_STORE_PROV_COLLECTION        = 'Collection';
  sz_CERT_STORE_PROV_SYSTEM_REGISTRY_W = 'SystemRegistry';
  sz_CERT_STORE_PROV_SYSTEM_REGISTRY   = sz_CERT_STORE_PROV_SYSTEM_REGISTRY_W;
  sz_CERT_STORE_PROV_PHYSICAL_W        = 'Physical';
  sz_CERT_STORE_PROV_PHYSICAL          = sz_CERT_STORE_PROV_PHYSICAL_W;
  sz_CERT_STORE_PROV_SMART_CARD_W      = 'SmartCard';
  sz_CERT_STORE_PROV_SMART_CARD        = sz_CERT_STORE_PROV_SMART_CARD_W;
  sz_CERT_STORE_PROV_LDAP_W            = 'Ldap';
  sz_CERT_STORE_PROV_LDAP              = sz_CERT_STORE_PROV_LDAP_W;

//+-------------------------------------------------------------------------
//  Certificate Store verify/results flags
//--------------------------------------------------------------------------
  CERT_STORE_SIGNATURE_FLAG           = $00000001;
  CERT_STORE_TIME_VALIDITY_FLAG       = $00000002;
  CERT_STORE_REVOCATION_FLAG          = $00000004;
  CERT_STORE_NO_CRL_FLAG              = $00010000;
  CERT_STORE_NO_ISSUER_FLAG           = $00020000;

  CERT_STORE_BASE_CRL_FLAG            = $00000100;
  CERT_STORE_DELTA_CRL_FLAG           = $00000200;


//+-------------------------------------------------------------------------
//  Certificate Store open/property flags
//--------------------------------------------------------------------------
  CERT_STORE_NO_CRYPT_RELEASE_FLAG                = $00000001;
  CERT_STORE_SET_LOCALIZED_NAME_FLAG              = $00000002;
  CERT_STORE_DEFER_CLOSE_UNTIL_LAST_FREE_FLAG     = $00000004;
  CERT_STORE_DELETE_FLAG                          = $00000010;
  CERT_STORE_UNSAFE_PHYSICAL_FLAG                 = $00000020;
  CERT_STORE_SHARE_STORE_FLAG                     = $00000040;
  CERT_STORE_SHARE_CONTEXT_FLAG                   = $00000080;
  CERT_STORE_MANIFOLD_FLAG                        = $00000100;
  CERT_STORE_ENUM_ARCHIVED_FLAG                   = $00000200;
  CERT_STORE_UPDATE_KEYID_FLAG                    = $00000400;
  CERT_STORE_BACKUP_RESTORE_FLAG                  = $00000800;
  CERT_STORE_READONLY_FLAG                        = $00008000;
  CERT_STORE_OPEN_EXISTING_FLAG                   = $00004000;
  CERT_STORE_CREATE_NEW_FLAG                      = $00002000;
  CERT_STORE_MAXIMUM_ALLOWED_FLAG                 = $00001000;

//+-------------------------------------------------------------------------
//  Certificate System Store Flag Values
//--------------------------------------------------------------------------
// Includes flags and location

  CERT_SYSTEM_STORE_MASK = $FFFF0000;
  CERT_SYSTEM_STORE_RELOCATE_FLAG = $80000000;
  CERT_SYSTEM_STORE_UNPROTECTED_FLAG = $40000000;

// Location of the system store:
  CERT_SYSTEM_STORE_LOCATION_MASK  = $00FF0000;
  CERT_SYSTEM_STORE_LOCATION_SHIFT = 16;


//  Registry: HKEY_CURRENT_USER or HKEY_LOCAL_MACHINE
  CERT_SYSTEM_STORE_CURRENT_USER_ID       = 1;
  CERT_SYSTEM_STORE_LOCAL_MACHINE_ID      = 2;
//  Registry: HKEY_LOCAL_MACHINE\Software\Microsoft\Cryptography\Services
  CERT_SYSTEM_STORE_CURRENT_SERVICE_ID    = 4;
  CERT_SYSTEM_STORE_SERVICES_ID           = 5;
//  Registry: HKEY_USERS
  CERT_SYSTEM_STORE_USERS_ID              = 6;

//  Registry: HKEY_CURRENT_USER\Software\Policies\Microsoft\SystemCertificates
  CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY_ID    = 7;
//  Registry: HKEY_LOCAL_MACHINE\Software\Policies\Microsoft\SystemCertificates
  CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY_ID   = 8;

//  Registry: HKEY_LOCAL_MACHINE\Software\Microsoft\EnterpriseCertificates
  CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE_ID     = 9;

  CERT_SYSTEM_STORE_CURRENT_USER    = (CERT_SYSTEM_STORE_CURRENT_USER_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  CERT_SYSTEM_STORE_LOCAL_MACHINE   = (CERT_SYSTEM_STORE_LOCAL_MACHINE_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  CERT_SYSTEM_STORE_CURRENT_SERVICE = (CERT_SYSTEM_STORE_CURRENT_SERVICE_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  CERT_SYSTEM_STORE_SERVICES        = (CERT_SYSTEM_STORE_SERVICES_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  CERT_SYSTEM_STORE_USERS           = (CERT_SYSTEM_STORE_USERS_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY  = (CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY = (CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE   = (CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);

  CERT_NAME_EMAIL_TYPE            = 1;
  CERT_NAME_RDN_TYPE              = 2;
  CERT_NAME_ATTR_TYPE             = 3;
  CERT_NAME_SIMPLE_DISPLAY_TYPE   = 4;
  CERT_NAME_FRIENDLY_DISPLAY_TYPE = 5;
  CERT_NAME_DNS_TYPE              = 6;
  CERT_NAME_URL_TYPE              = 7;
  CERT_NAME_UPN_TYPE              = 8;

//+-------------------------------------------------------------------------
//  Certificate name flags
//--------------------------------------------------------------------------
  CERT_NAME_ISSUER_FLAG           = $1;
  CERT_NAME_DISABLE_IE4_UTF8_FLAG = $00010000;
//
// MessageId: CRYPT_E_MSG_ERROR
//
// MessageText:
//
//  An error occurred while performing an operation on a cryptographic message.
//
  CRYPT_E_MSG_ERROR                = $80091001;

//
// MessageId: CRYPT_E_UNKNOWN_ALGO
//
// MessageText:
//
//  Unknown cryptographic algorithm.
//
  CRYPT_E_UNKNOWN_ALGO             = $80091002;

//
// MessageId: CRYPT_E_OID_FORMAT
//
// MessageText:
//
//  The object identifier is poorly formatted.
//
  CRYPT_E_OID_FORMAT               = $80091003;

//
// MessageId: CRYPT_E_INVALID_MSG_TYPE
//
// MessageText:
//
//  Invalid cryptographic message type.
//
  CRYPT_E_INVALID_MSG_TYPE         = $80091004;

//
// MessageId: CRYPT_E_UNEXPECTED_ENCODING
//
// MessageText:
//
//  Unexpected cryptographic message encoding.
//
  CRYPT_E_UNEXPECTED_ENCODING      = $80091005;

//
// MessageId: CRYPT_E_AUTH_ATTR_MISSING
//
// MessageText:
//
//  The cryptographic message does not contain an expected authenticated attribute.
//
  CRYPT_E_AUTH_ATTR_MISSING        = $80091006;

//
// MessageId: CRYPT_E_HASH_VALUE
//
// MessageText:
//
//  The hash value is not correct.
//
  CRYPT_E_HASH_VALUE               = $80091007;

//
// MessageId: CRYPT_E_INVALID_INDEX
//
// MessageText:
//
//  The index value is not valid.
//
  CRYPT_E_INVALID_INDEX            = $80091008;

//
// MessageId: CRYPT_E_ALREADY_DECRYPTED
//
// MessageText:
//
//  The content of the cryptographic message has already been decrypted.
//
  CRYPT_E_ALREADY_DECRYPTED        = $80091009;

//
// MessageId: CRYPT_E_NOT_DECRYPTED
//
// MessageText:
//
//  The content of the cryptographic message has not been decrypted yet.
//
  CRYPT_E_NOT_DECRYPTED            = $8009100A;

//
// MessageId: CRYPT_E_RECIPIENT_NOT_FOUND
//
// MessageText:
//
//  The enveloped-data message does not contain the specified recipient.
//
  CRYPT_E_RECIPIENT_NOT_FOUND      = $8009100B;

//
// MessageId: CRYPT_E_CONTROL_TYPE
//
// MessageText:
//
//  Invalid control type.
//
  CRYPT_E_CONTROL_TYPE             = $8009100C;

//
// MessageId: CRYPT_E_ISSUER_SERIALNUMBER
//
// MessageText:
//
//  Invalid issuer and/or serial number.
//
  CRYPT_E_ISSUER_SERIALNUMBER      = $8009100D;

//
// MessageId: CRYPT_E_SIGNER_NOT_FOUND
//
// MessageText:
//
//  Cannot find the original signer.
//
  CRYPT_E_SIGNER_NOT_FOUND         = $8009100E;

//
// MessageId: CRYPT_E_ATTRIBUTES_MISSING
//
// MessageText:
//
//  The cryptographic message does not contain all of the requested attributes.
//
  CRYPT_E_ATTRIBUTES_MISSING       = $8009100F;

//
// MessageId: CRYPT_E_STREAM_MSG_NOT_READY
//
// MessageText:
//
//  The streamed cryptographic message is not ready to return data.
//
  CRYPT_E_STREAM_MSG_NOT_READY     = $80091010;

//
// MessageId: CRYPT_E_STREAM_INSUFFICIENT_DATA
//
// MessageText:
//
//  The streamed cryptographic message requires more data to complete the decode operation.
//
  CRYPT_E_STREAM_INSUFFICIENT_DATA = $80091011;

//
// MessageId: CRYPT_I_NEW_PROTECTION_REQUIRED
//
// MessageText:
//
//  The protected data needs to be re-protected.
//
  CRYPT_I_NEW_PROTECTION_REQUIRED  = $00091012;

//
// MessageId: CRYPT_E_BAD_LEN
//
// MessageText:
//
//  The length specified for the output data was insufficient.
//
  CRYPT_E_BAD_LEN                  = $80092001;

//
// MessageId: CRYPT_E_BAD_ENCODE
//
// MessageText:
//
//  An error occurred during encode or decode operation.
//
  CRYPT_E_BAD_ENCODE               = $80092002;

//
// MessageId: CRYPT_E_FILE_ERROR
//
// MessageText:
//
//  An error occurred while reading or writing to a file.
//
  CRYPT_E_FILE_ERROR               = $80092003;

//
// MessageId: CRYPT_E_NOT_FOUND
//
// MessageText:
//
//  Cannot find object or property.
//
  CRYPT_E_NOT_FOUND                = $80092004;

//
// MessageId: CRYPT_E_EXISTS
//
// MessageText:
//
//  The object or property already exists.
//
  CRYPT_E_EXISTS                   = $80092005;

//
// MessageId: CRYPT_E_NO_PROVIDER
//
// MessageText:
//
//  No provider was specified for the store or object.
//
  CRYPT_E_NO_PROVIDER              = $80092006;

//
// MessageId: CRYPT_E_SELF_SIGNED
//
// MessageText:
//
//  The specified certificate is self signed.
//
  CRYPT_E_SELF_SIGNED              = $80092007;

//
// MessageId: CRYPT_E_DELETED_PREV
//
// MessageText:
//
//  The previous certificate or CRL context was deleted.
//
  CRYPT_E_DELETED_PREV             = $80092008;

//
// MessageId: CRYPT_E_NO_MATCH
//
// MessageText:
//
//  Cannot find the requested object.
//
  CRYPT_E_NO_MATCH                 = $80092009;

//
// MessageId: CRYPT_E_UNEXPECTED_MSG_TYPE
//
// MessageText:
//
//  The certificate does not have a property that references a private key.
//
  CRYPT_E_UNEXPECTED_MSG_TYPE      = $8009200A;

//
// MessageId: CRYPT_E_NO_KEY_PROPERTY
//
// MessageText:
//
//  Cannot find the certificate and private key for decryption.
//
  CRYPT_E_NO_KEY_PROPERTY          = $8009200B;

//
// MessageId: CRYPT_E_NO_DECRYPT_CERT
//
// MessageText:
//
//  Cannot find the certificate and private key to use for decryption.
//
  CRYPT_E_NO_DECRYPT_CERT          = $8009200C;

//
// MessageId: CRYPT_E_BAD_MSG
//
// MessageText:
//
//  Not a cryptographic message or the cryptographic message is not formatted correctly.
//
  CRYPT_E_BAD_MSG                  = $8009200D;

//
// MessageId: CRYPT_E_NO_SIGNER
//
// MessageText:
//
//  The signed cryptographic message does not have a signer for the specified signer index.
//
  CRYPT_E_NO_SIGNER                = $8009200E;

//
// MessageId: CRYPT_E_PENDING_CLOSE
//
// MessageText:
//
//  Final closure is pending until additional frees or closes.
//
  CRYPT_E_PENDING_CLOSE            = $8009200F;

//
// MessageId: CRYPT_E_REVOKED
//
// MessageText:
//
//  The certificate is revoked.
//
  CRYPT_E_REVOKED                  = $80092010;

//
// MessageId: CRYPT_E_NO_REVOCATION_DLL
//
// MessageText:
//
//  No Dll or exported function was found to verify revocation.
//
  CRYPT_E_NO_REVOCATION_DLL        = $80092011;

//
// MessageId: CRYPT_E_NO_REVOCATION_CHECK
//
// MessageText:
//
//  The revocation function was unable to check revocation for the certificate.
//
  CRYPT_E_NO_REVOCATION_CHECK      = $80092012;

//
// MessageId: CRYPT_E_REVOCATION_OFFLINE
//
// MessageText:
//
//  The revocation function was unable to check revocation because the revocation server was offline.
//
  CRYPT_E_REVOCATION_OFFLINE       = $80092013;

//
// MessageId: CRYPT_E_NOT_IN_REVOCATION_DATABASE
//
// MessageText:
//
//  The certificate is not in the revocation server's database.
//
  CRYPT_E_NOT_IN_REVOCATION_DATABASE = $80092014;

//
// MessageId: CRYPT_E_INVALID_NUMERIC_STRING
//
// MessageText:
//
//  The string contains a non-numeric character.
//
  CRYPT_E_INVALID_NUMERIC_STRING   = $80092020;

//
// MessageId: CRYPT_E_INVALID_PRINTABLE_STRING
//
// MessageText:
//
//  The string contains a non-printable character.
//
  CRYPT_E_INVALID_PRINTABLE_STRING = $80092021;

//
// MessageId: CRYPT_E_INVALID_IA5_STRING
//
// MessageText:
//
//  The string contains a character not in the 7 bit ASCII character set.
//
  CRYPT_E_INVALID_IA5_STRING       = $80092022;

//
// MessageId: CRYPT_E_INVALID_X500_STRING
//
// MessageText:
//
//  The string contains an invalid X500 name attribute key, oid, value or delimiter.
//
  CRYPT_E_INVALID_X500_STRING      = $80092023;

//
// MessageId: CRYPT_E_NOT_CHAR_STRING
//
// MessageText:
//
//  The dwValueType for the CERT_NAME_VALUE is not one of the character strings.  Most likely it is either a CERT_RDN_ENCODED_BLOB or CERT_TDN_OCTED_STRING.
//
  CRYPT_E_NOT_CHAR_STRING          = $80092024;

//
// MessageId: CRYPT_E_FILERESIZED
//
// MessageText:
//
//  The Put operation can not continue.  The file needs to be resized.  However, there is already a signature present.  A complete signing operation must be done.
//
  CRYPT_E_FILERESIZED              = $80092025;

//
// MessageId: CRYPT_E_SECURITY_SETTINGS
//
// MessageText:
//
//  The cryptographic operation failed due to a local security option setting.
//
  CRYPT_E_SECURITY_SETTINGS        = $80092026;

//
// MessageId: CRYPT_E_NO_VERIFY_USAGE_DLL
//
// MessageText:
//
//  No DLL or exported function was found to verify subject usage.
//
  CRYPT_E_NO_VERIFY_USAGE_DLL      = $80092027;

//
// MessageId: CRYPT_E_NO_VERIFY_USAGE_CHECK
//
// MessageText:
//
//  The called function was unable to do a usage check on the subject.
//
  CRYPT_E_NO_VERIFY_USAGE_CHECK    = $80092028;

//
// MessageId: CRYPT_E_VERIFY_USAGE_OFFLINE
//
// MessageText:
//
//  Since the server was offline, the called function was unable to complete the usage check.
//
  CRYPT_E_VERIFY_USAGE_OFFLINE     = $80092029;

//
// MessageId: CRYPT_E_NOT_IN_CTL
//
// MessageText:
//
//  The subject was not found in a Certificate Trust List (CT;.
//
  CRYPT_E_NOT_IN_CTL               = $8009202A;

//
// MessageId: CRYPT_E_NO_TRUSTED_SIGNER
//
// MessageText:
//
//  None of the signers of the cryptographic message or certificate trust list is trusted.
//
  CRYPT_E_NO_TRUSTED_SIGNER        = $8009202B;

//
// MessageId: CRYPT_E_MISSING_PUBKEY_PARA
//
// MessageText:
//
//  The public key's algorithm parameters are missing.
//
  CRYPT_E_MISSING_PUBKEY_PARA      = $8009202C;

//
// MessageId: CRYPT_E_OSS_ERROR
//
// MessageText:
//
//  OSS Certificate encode/decode error code base
//
//  See asn1code.h for a definition of the OSS runtime errors. The OSS
//  error values are offset by CRYPT_E_OSS_ERROR.
//
  CRYPT_E_OSS_ERROR                = $80093000;

function CryptAcquireContext(var hProv : HCRYPTPROV; pscContainer, pszProvider : PChar; dwProvType, dwFlags : DWORD) : BOOL; stdcall;
function CryptCreateHash(hProv : HCRYPTPROV; Algid : ALG_ID; hKey : HCRYPTKEY; dwFlags : DWORD; var hHash : HCRYPTHASH) : BOOL; stdcall;
function CryptHashData (hHash : HCRYPTHASH; data : PChar; dwDataLen : DWORD; dwFlags : DWORD) : BOOL; stdcall;
function CryptDeriveKey (hProv : HCRYPTPROV; Algid : ALG_ID; hBaseData : HCRYPTHASH; dwFlags : DWORD; var hKey : HCRYPTKEY) : BOOL; stdcall;
function CryptDestroyHash(hHash : HCRYPTHASH) : BOOL; stdcall;
function CryptDestroyKey(hKey : HCRYPTKEY) : BOOL; stdcall;
function CryptReleaseContext (hProv : HCRYPTPROV; dwFlags : DWORD) : BOOL; stdcall;
function CryptEncrypt(hKey : HCRYPTKEY; hHash : HCRYPTHASH; final : BOOL; dwFlags : DWORD; pbData : Pointer; var pdwDataLen : DWORD; dwBufLen : DWORD) : BOOL; stdcall;
function CryptDecrypt(hKey : HCRYPTKEY; hHash : HCRYPTHASH; Final : BOOL; dwFlags : DWORD; pbData : pointer; var pdwDataLen : DWORD) : BOOL; stdcall;
function CryptImportKey(hProv : HCRYPTPROV; pbData : Pointer; dwDataLen : DWORD; hPubKey : HCRYPTKEY; dwFlags : DWORD; var phKey : HCRYPTKEY) : BOOL; stdcall;
function CryptGetProvParam (hProv : HCRYPTPROV; dwparam : DWORD; pbData : Pointer; var pdwDataLen : DWORD; dwFlags : DWORD) : BOOL; stdcall;

function CertOpenStore (lpszStoreProvider : PChar;
                        dwEncodingType : DWORD;
                        hProv : HCRYPTPROV;
                        dwFlags : DWORD;
                        pvPara : pointer) : HCERTSTORE; stdcall;

function CertCloseStore (certStore : HCERTSTORE; dwFlags : DWORD) : BOOL; stdcall;

function CertCreateSelfSignCertificate (hProv : HCRYPTPROV;
                                        pSubjectIssuerBlob : PCertNameBlob;
                                        dwFlags : DWORD;
                                        pKeyProvInfo : PCryptKeyProvInfo;
                                        pSignatureAlgorithm : PCryptAlgorithmIdentifier;
                                        pStartTime :PSystemTime;
                                        pEndTime : PSystemTime;
                                        pExtensions : PCertExtensions) : PCCertContext; stdcall;

function CertEnumCertificatesInStore (certStore : HCERTSTORE; pPrevCertContext : PCCertContext) : PCCertContext; stdcall;

function CertFreeCertificateContext(pCertContext : PCCertContext) : BOOL; stdcall;

function CertGetNameStringA(certContext : pCCertContext;
                            dwType : DWORD;
                            dwFlags : DWORD;
                            pvTypePara : pointer;
                            pszNameString : PChar;
                            cchNameString : DWORD) : DWORD; stdcall;

function CertGetNameStringW(certContext : pCCertContext;
                            dwType : DWORD;
                            dwFlags : DWORD;
                            pvTypePara : pointer;
                            pszNameString : PWideChar;
                            cchNameString : DWORD) : DWORD; stdcall;

function CertGetNameString (certContext : pCCertContext;
                            dwType : DWORD;
                            dwFlags : DWORD;
                            pvTypePara : pointer;
                            pszNameString : PChar;
                            cchNameString : DWORD) : DWORD; stdcall;

implementation

const
  cryptdll = 'advapi32.dll';
  certdll = 'crypt32.dll';

function CryptAcquireContext; external cryptdll name 'CryptAcquireContextA';
function CryptCreateHash; external cryptdll;
function CryptHashData; external cryptdll;
function CryptDeriveKey; external cryptdll;
function CryptDestroyHash; external cryptdll;
function CryptDestroyKey; external cryptdll;
function CryptReleaseContext; external cryptdll;
function CryptEncrypt; external cryptdll;
function CryptDecrypt; external cryptdll;
function CryptImportKey; external cryptdll;
function CryptGetProvParam; external cryptdll;

function CertOpenStore; external certdll;
function CertCloseStore; external certdll;
function CertCreateSelfSignCertificate; external certdll;
function CertEnumCertificatesInStore; external certdll;
function CertFreeCertificateContext; external certdll;
function CertGetNameStringA; external certdll;
function CertGetNameStringW; external certdll;
function CertGetNameString; external certdll name 'CertGetNameStringA';
end.
