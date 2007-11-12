{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     CryptGlobal Unit                                  }
{     Globals used by the CryptUtils unit               }
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
unit CryptGlobal;

interface

uses
  Classes, SecUtils, SsRijndael;

type
  TKeySize = 8..56;

  // In many cases cryptographic operations may be performed with
  // algorithms and parameters that are fixed by the application.
  //
  // The routines in CryptUtils will use the algorithms and parameters set
  // below. No information about the algorithms and parameters are included in
  // the cipher text. The application may change the values of these variables
  // rather freely, but is responsible for carrying information about them
  // from the sender to the recipient.
  //
  // IMPORTANT: The routines in CryptUtils will authenticate the plain text,
  // not the cipher text. This means that it is not recommendable to use
  // any other cipher mode than ABC with these routines.
var
  DefaultCipher: TCipherClass;
  DefaultHash: THashClass;
  // Rijndael key sizes are: 16, 24, 32
  // TwoFish key sizes are: 16, 24, 32
  // BlowFish key sizes are: 1..56
  // DES/3DES key sizes are: 8, 16, 24
  // ARC4 key sizes are: 1..256
  DefaultKeySize: TKeySize = 32;
  // The value of DefaultKDFIter determines the number of key derivation
  // iterations performed when a password is transformed into key data. The
  // higher the value, the slower the operation (which is good since it may
  // hinder brute force attacks on the password).
  DefaultKDFIter: Integer = $1000;


implementation

initialization
  DefaultCipher := FindCipherClass(caRijndael,cmABC);
{$IFDEF SHA512}
  DefaultHash := FindHashClass(haSHA512);
{$ENDIF SHA512}
end.
