{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     TLSConst Unit                                     }
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
unit TlsConst;

interface

const
  MaxPlainTextLength = 16384;
  MaxCompressedLength = 16384 + 1024;
  MaxCipherTextLength = 16384 + 2048;

  change_cipher_spec = 20;
  alert              = 21;
  handshake          = 22;
  application_data   = 23;

  {*** ALERT ***}
  warning = 1;
  fatal   = 2;

  close_notify            = 0;
{   unexpected_message
       An inappropriate message was received. This alert is always fatal
       and should never be observed in communication between proper
       implementations.}  
  unexpected_message      = 10;

{   bad_record_mac
       This alert is returned if a record is received with an incorrect  |
       This alert is returned if a record is received with an incorrect  |
       MAC. This alert also SHOULD be returned if a TLSCiphertext        |
       decrypted in an invalid way: either it wasn't an even multiple of |
       the block length, or its padding values, when checked, weren't    |
       correct. This message is always fatal.}
  bad_record_mac          = 20;

{   decryption_failed
       This alert MAY be returned if a TLSCiphertext decrypted in an     |
       invalid way: either it wasn't an even multiple of the block       |
       length, or its padding values, when checked, weren't correct.     |
       This message is always fatal.                                     |

       NB: Differentiating between bad_record_mac and decryption_failed  |
       alerts may permit certain attacks against CBC mode as used in     |
       TLS. It is preferable to uniformly use the bad_record_mac alert   |
       to hide the specific type of the error.}
  decryption_failed       = 21;

{   record_overflow
       A TLSCiphertext record was received which had a length more than
       2^14+2048 bytes, or a record decrypted to a TLSCompressed record
       with more than 2^14+1024 bytes. This message is always fatal.}
  record_overflow         = 22;

{   decompression_failure
       The decompression function received improper input (e.g. data
       that would expand to excessive length). This message is always
       fatal.}
  decompression_failure   = 30;

{   handshake_failure
       Reception of a handshake_failure alert message indicates that the
       sender was unable to negotiate an acceptable set of security
       parameters given the options available. This is a fatal error.}
  handshake_failure       = 40;

{   no_certificate_RESERVED                                               |
       This alert was used in SSLv3 but not in TLS. It should not be     |
       sent by compliant implementations.}
  no_certificate_RESERVED = 41;

{   bad_certificate
       A certificate was corrupt, contained signatures that did not
       verify correctly, etc.}
  bad_certificate         = 42;

{   unsupported_certificate
       A certificate was of an unsupported type.}
  unsupported_certificate = 43;

{   certificate_revoked
       A certificate was revoked by its signer.}
  certificate_revoked     = 44;

{   certificate_expired
       A certificate has expired or is not currently valid.}
  certificate_expired     = 45;

{   certificate_unknown
       Some other (unspecified) issue arose in processing the
       certificate, rendering it unacceptable.}
  certificate_unknown     = 46;

{   illegal_parameter
       A field in the handshake was out of range or inconsistent with
       other fields. This is always fatal.}
  illegal_parameter       = 47;

{   unknown_ca
       A valid certificate chain or partial chain was received, but the
       certificate was not accepted because the CA certificate could not |
       be located or couldn't be matched with a known, trusted CA.  This
       message is always fatal.}
  unknown_ca              = 48;

{   access_denied
       A valid certificate was received, but when access control was
       applied, the sender decided not to proceed with negotiation.
       This message is always fatal.}
  access_denied           = 49;

{   decode_error
       A message could not be decoded because some field was out of the
       specified range or the length of the message was incorrect. This
       message is always fatal.}
  decode_error            = 50;

{   decrypt_error
       A handshake cryptographic operation failed, including being
       unable to correctly verify a signature, decrypt a key exchange,
       or validate a finished message.}
  decrypt_error           = 51;

{   export_restriction
       A negotiation not in compliance with export restrictions was
       detected; for example, attempting to transfer a 1024 bit
       ephemeral RSA key for the RSA_EXPORT handshake method. This
       message is always fatal.}
  export_restriction      = 60;

{   protocol_version
       The protocol version the client has attempted to negotiate is
       recognized, but not supported. (For example, old protocol
       versions might be avoided for security reasons). This message is
       always fatal.}
  protocol_version        = 70;

{   insufficient_security
       Returned instead of handshake_failure when a negotiation has
       failed specifically because the server requires ciphers more
       secure than those supported by the client. This message is always
       fatal.}
  insufficient_security   = 71;

{   internal_error
       An internal error unrelated to the peer or the correctness of the
       protocol makes it impossible to continue (such as a memory
       allocation failure). This message is always fatal.}
  internal_error          = 80;

{   user_canceled
       This handshake is being canceled for some reason unrelated to a
       protocol failure. If the user cancels an operation after the
       handshake is complete, just closing the connection by sending a
       close_notify is more appropriate. This alert should be followed
       by a close_notify. This message is generally a warning.}
  user_canceled           = 90;

{   no_renegotiation
       Sent by the client in response to a hello request or by the
       server in response to a client hello after initial handshaking.
       Either of these would normally lead to renegotiation; when that
       is not appropriate, the recipient should respond with this alert
       at that point, the original requester can decide whether to
       proceed with the connection. One case where this would be
       appropriate would be where a server has spawned a process to
       satisfy a request; the process might receive security parameters
       (key length, authentication, etc.) at startup and it might be
       difficult to communicate changes to these parameters after that
       point. This message is always a warning.}
  no_renegotiation        = 100;

  {*** Handshake Types ***}
  hello_request           = 0;
  client_hello            = 1;
  server_hello            = 2;
  certificate             = 11;
  server_key_exchange     = 12;
  certificate_request     = 13;
  server_hello_done       = 14;
  certificate_verify      = 15;
  client_key_exchange     = 16;
  finished                = 20;

  {*** Client Certificate Types ***}
  rsa_sign                  = 1;
  dss_sign                  = 2;
  rsa_fixed_dh              = 3;
  dss_fixed_dh              = 4;
{  rsa_ephemeral_dh_RESERVED = 5; // From SSL, deprecated }
{  dss_ephemeral_dh_RESERVED = 6; // From SSL, deprecated }
  ecdsa_sign                = 5;
  rsa_fixed_ecdh            = 6;
  ecdsa_fixed_ecdh          = 7;
  fortezza_dms_RESERVED     = 20;

  {*** Compression methods ***}
  cmNull                    = 0;

{*** Extended Error Codes ***}
  {** Server side: **}
  client_hello_format       = 1 * 256;
  unknown_compression       = 2 * 256;
  no_common_cipher_suite    = 3 * 256;
  cipher_suite_unknown      = 4 * 256;
  server_cert_not_found     = 5 * 256;
  server_key_not_found      = 6 * 256;
  client_key_exch_format    = 7 * 256;
  {** Client side: **}
  server_cert_ip            = 1 * 65536;
  server_cert_uri           = 2 * 65536;
  server_cert_name          = 3 * 65536;
  wrong_netscape_key_usage  = 4 * 65536;
  extension_format          = 5 * 65536;
  wrong_netscape_cert_type  = 6 * 65536;
  wrong_key_usage           = 7 * 65536;
  client_cert_not_found     = 8 * 65536;

function AlertMsg(AlertCode: Integer): string;

implementation

function AlertMsg(AlertCode: Integer): string;
begin
  case AlertCode and $FF of
    0:  Result := 'close_notify: The remote host has terminated the connection.';
    10: Result := 'unexpected_message:'#13#10 +
       'An inappropriate message was received. This alert is always fatal'#13#10 +
       'and should never be observed in communication between proper'#13#10 +
       'implementations. Please notify StreamSec if you get this message.';
    20: Result := 'bad_record_mac:'#13#10 +
       'This alert is returned if a record is received with an incorrect'#13#10 +
       'MAC. This alert also SHOULD be returned if a TLSCiphertext'#13#10 +
       'decrypted in an invalid way: either it wasn''t an even multiple of'#13#10 +
       'the block length, or its padding values, when checked, weren''t'#13#10 +
       'correct. This message is always fatal.';
    21: Result := 'decryption_failed:'#13#10 +
       'This alert MAY be returned if a TLSCiphertext decrypted in an'#13#10 +
       'invalid way: either it wasn''t an even multiple of the block'#13#10 +
       'length, or its padding values, when checked, weren''t correct.'#13#10 +
       'This message is always fatal.'#13#10 +
       #13#10 +
       'NB: Differentiating between bad_record_mac and decryption_failed'#13#10 +
       'alerts may permit certain attacks against CBC mode as used in'#13#10 +
       'TLS. It is preferable to uniformly use the bad_record_mac alert'#13#10 +
       'to hide the specific type of the error.';
    22: Result := 'record_overflow:'#13#10 +
       'A TLSCiphertext record was received which had a length more than'#13#10 +
       '2^14+2048 bytes, or a record decrypted to a TLSCompressed record'#13#10 +
       'with more than 2^14+1024 bytes. This message is always fatal.';
    30: Result := 'decompression_failure:'#13#10 +
       'The decompression function received improper input (e.g. data'#13#10 +
       'that would expand to excessive length). This message is always'#13#10 +
       'fatal.';
    40: Result := 'handshake_failure:'#13#10 +
       'Reception of a handshake_failure alert message indicates that the'#13#10 +
       'sender was unable to negotiate an acceptable set of security'#13#10 +
       'parameters given the options available. This is a fatal error.';
    41: Result := 'no_certificate_RESERVED:'#13#10 +
       'This alert was used in SSLv3 but not in TLS. It should not be'#13#10 +
       'sent by compliant implementations.';
    42: Result := 'bad_certificate:'#13#10 +
       'A certificate was corrupt, contained signatures that did not'#13#10 +
       'verify correctly, etc. This alert code is e.g. generated by the'#13#10 +
       'client if the server certificate is not allowed to be used for'#13#10 +
       'server authentication';
    43: Result := 'unsupported_certificate:'#13#10 +
       'A certificate was of an unsupported type.';
    44: Result := 'certificate_revoked:'#13#10 +
       'A certificate was revoked by its signer.';
    45: Result := 'certificate_expired:'#13#10 +
       'A certificate has expired or is not currently valid.';
    46: Result := 'certificate_unknown:'#13#10 +
       'Some other (unspecified) issue arose in processing the'#13#10 +
       'certificate, rendering it unacceptable.';
    47: Result := 'illegal_parameter:'#13#10 +
       'A field in the handshake was out of range or inconsistent with'#13#10 +
       'other fields. This is always fatal.';
    48: Result := 'unknown_ca:'#13#10 +
       'A valid certificate chain or partial chain was received, but the'#13#10 +
       'certificate was not accepted because the CA certificate could not'#13#10 +
       'be located or couldn''t be matched with a known, trusted CA.  This'#13#10 +
       'message is always fatal.';
    49: Result := 'access_denied:'#13#10 +
       'A valid certificate was received, but when access control was'#13#10 +
       'applied, the sender decided not to proceed with negotiation.'#13#10 +
       'This message is always fatal.';
    50: Result := 'decode_error:'#13#10 +
       'A message could not be decoded because some field was out of the'#13#10 +
       'specified range or the length of the message was incorrect. This'#13#10 +
       'message is always fatal.';
    51: Result := 'decrypt_error:'#13#10 +
       'A handshake cryptographic operation failed, including being'#13#10 +
       'unable to correctly verify a signature, decrypt a key exchange,'#13#10 +
       'or validate a finished message.';
    60: Result := 'export_restriction:'#13#10 +
       'A negotiation not in compliance with export restrictions was'#13#10 +
       'detected; for example, attempting to transfer a 1024 bit'#13#10 +
       'ephemeral RSA key for the RSA_EXPORT handshake method. This'#13#10 +
       'message is always fatal.';
    70: Result := 'protocol_version:'#13#10 +
       'The protocol version the client has attempted to negotiate is'#13#10 +
       'recognized, but not supported. (For example, old protocol'#13#10 +
       'versions might be avoided for security reasons). This message is'#13#10 +
       'always fatal.';
    71: Result := 'insufficient_security:'#13#10 +
       'Returned instead of handshake_failure when a negotiation has'#13#10 +
       'failed specifically because the server requires ciphers more'#13#10 +
       'secure than those supported by the client. This message is always'#13#10 +
       'fatal.';
    80: Result := 'internal_error:'#13#10 +
       'An internal error unrelated to the peer or the correctness of the'#13#10 +
       'protocol makes it impossible to continue (such as a memory'#13#10 +
       'allocation failure). This message is always fatal.';
    90: Result := 'user_canceled:'#13#10 +
       'This handshake is being canceled for some reason unrelated to a'#13#10 +
       'protocol failure. If the user cancels an operation after the'#13#10 +
       'handshake is complete, just closing the connection by sending a'#13#10 +
       'close_notify is more appropriate. This alert should be followed'#13#10 +
       'by a close_notify. This message is generally a warning.';
    100: Result := 'no_renegotiation:'#13#10 +
       'Sent by the client in response to a hello request or by the'#13#10 +
       'server in response to a client hello after initial handshaking.'#13#10 +
       'Either of these would normally lead to renegotiation; when that'#13#10 +
       'is not appropriate, the recipient should respond with this alert'#13#10 +
       'at that point, the original requester can decide whether to'#13#10 +
       'proceed with the connection. One case where this would be'#13#10 +
       'appropriate would be where a server has spawned a process to'#13#10 +
       'satisfy a request; the process might receive security parameters'#13#10 +
       '(key length, authentication, etc.) at startup and it might be'#13#10 +
       'difficult to communicate changes to these parameters after that'#13#10 +
       'point. This message is always a warning.';
  else
    Result := 'Unknown alert code: No description available.';
  end;
  case AlertCode and $FFFF00 of
    client_hello_format:
      Result := Result + #13#10'Extended information:'#13#10 +
       'Problem parsing the client hello message.';
    unknown_compression:                                      
      Result := Result + #13#10'Extended information:'#13#10 +
       'The selected compression suite was not requested by the client.';
    no_common_cipher_suite:
      Result := Result + #13#10'Extended information:'#13#10 +
       'The client and the server do not have any cipher suites in common.';
    cipher_suite_unknown:
      Result := Result + #13#10'Extended information:'#13#10 +
       'The selected cipher suite could not be decoded. This is likely to'#13#10 +
       'be caused by an internal error.';
    server_cert_not_found:
      Result := Result + #13#10'Extended information:'#13#10 +
       'Unable to find a server certificate appropriate for the selected'#13#10 +
       'cipher suite.';
    server_key_not_found:
      Result := Result + #13#10'Extended information:'#13#10 +
       'Unable to find the private key corresponding to the selected certificate.';
    client_key_exch_format:
      Result := Result + #13#10'Extended information:'#13#10 +
       'The client key exchange message could not be processed correctly.';
    server_cert_ip:
      Result := Result + #13#10'Extended information:'#13#10 +
       'Wrong IP address for server certificate.';
    server_cert_uri:
      Result := Result + #13#10'Extended information:'#13#10 +
       'Wrong URI for server certificate.';
    server_cert_name:
      Result := Result + #13#10'Extended information:'#13#10 +
       'Wrong host name for server certificate.';
    wrong_netscape_key_usage:
      Result := Result + #13#10'Extended information:'#13#10 +
       'Wrong Netscape Key Usage for server certificate.';
    extension_format:
      Result := Result + #13#10'Extended information:'#13#10 +
       'The server certificate has an invalid extension.';
    wrong_netscape_cert_type:
      Result := Result + #13#10'Extended information:'#13#10 +
       'Wrong Netscape certificate type for server certificate.';
    wrong_key_usage:
      Result := Result + #13#10'Extended information:'#13#10 +
       'The server certificate does not have have the expected key usage for'#13#10 +
       'the negotiated cipher suite.';
    client_cert_not_found:
      Result := Result + #13#10'Extended information:'#13#10 +
       'Unable to find a client certificate conforming of a type supported'#13#10 +
       'by the server.';
  end;
end;

end.
