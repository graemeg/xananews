{$Q-,R-}
{$I ver.inc}

{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Rijndael Unit                                     }
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

unit SsRijndael;

interface

uses
  SecUtils;

{$I Rijndael.Inc}
{$I RijndaelEncTblConst.inc}
{$I RijndaelDcrTblConst.inc}

const
  BC= 4;
  MAXROUNDS= 14;
  fMaxKeySize = 32;

type
  PWord= ^Word;
  PDWord= ^LongWord;
  PDWordArray= ^TDWordArray;
  TDWordArray= array[0..1023] of LongWord;
  PByteArray= ^TByteArray;
  TByteArray= array[0..4095] of byte;

  TRijndael_ECB = class(TBlockCipher)
  protected
    IV: array[0..47] of byte;
    numrounds: longint;
    rk, drk: array[0..MAXROUNDS,0..7] of LongWord;
    procedure CleanUp; override;
    procedure DecryptBlock(var Buf); override;
    procedure DecryptBlockToDst(var Dst; const Src); override;
    procedure EncryptBlock(var Buf); override;
    procedure EncryptBlockToDst(var Dst; const Src); override;
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    constructor CreateClone(ACipher: TCipher); override;
    class function BlockSize: Integer; override;
    class function Mode: TCipherMode; override;   
    class function Algorithm: TCipherAlg; override;
    class function AlgorithmName: PChar; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    class function MaxKeySize: Integer; override;
    class function MinKeySize: Integer; override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
    procedure SetUp(const AKey; Count, VectorSize: Integer); override;
  end;

  TRijndael_CFB = class(TRijndael_ECB)
  public
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    property ModeRatio;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TRijndael_OFB = class(TRijndael_CFB)
  public
    class function Mode: TCipherMode; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;   
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TRijndael_PCFB = class(TRijndael_CFB)
  public        
    constructor Create(const AKey; Count, VectorSize: Integer); override;
    class function Mode: TCipherMode; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override; 
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;               

  TRijndael_PipedPCFB = class(TRijndael_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;  
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TRijndael_ABC = class(TRijndael_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TRijndael_CTR = class(TRijndael_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;   
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TRijndael_CBC = class(TRijndael_ECB)
  public
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;   
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;

  TAES_Wrap = class(TRijndael_ECB)
  protected
    procedure CleanUp; override;
  public
    class function BlockSize: Integer; override;         
    class function Mode: TCipherMode; override;
    class function BlockVectorSize: Integer; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;    
    class function OID(KeySize: Integer): PChar; override;
    class function KeyedIVOID(KeySize: Integer): PChar; override;
  end;


function KeyWrapDecrypt(const InBuf; InBufLen: Integer;
                        const Key; KeyLen: Integer;
                        var OutBuf; OutBufLen: Integer): Integer; overload;
function KeyWrapEncrypt(const InBuf; InBufLen: Integer;
                        const Key; KeyLen: Integer;
                        var OutBuf; OutBufLen: Integer): Integer; overload;

function KeyWrapDecrypt(InBuf: ISecretKey;
                        Key: ISecretKey;
                        var OutBuf: ISecretKey): Boolean; overload;
function KeyWrapEncrypt(InBuf: ISecretKey;
                        Key: ISecretKey;
                        var OutBuf: ISecretKey): Boolean; overload;

implementation

uses
  SysUtils;

function KeyWrapDecrypt(const InBuf; InBufLen: Integer;
                        const Key; KeyLen: Integer;
                        var OutBuf; OutBufLen: Integer): Integer;
var
  C: TAES_Wrap;
  P: PChar;
begin
  if (OutBufLen < InBufLen - 8) or (InBufLen mod 8 <> 0) then
    Result := 0
  else begin
    C := TAES_Wrap.Create(Key,KeyLen,0);
    try
      Move(InBuf,C.IV,8);
      P := @InBuf;
      Result := InBufLen - 8;
      Move((P+8)^,OutBuf,Result);
      C.Decrypt(OutBuf,Result);
      if C.IVector <> #$A6#$A6#$A6#$A6#$A6#$A6#$A6#$A6 then
        Result := 0;
    finally
      C.Free;
    end;
  end;
end;

function KeyWrapEncrypt(const InBuf; InBufLen: Integer;
                        const Key; KeyLen: Integer;
                        var OutBuf; OutBufLen: Integer): Integer;
var
  C: TAES_Wrap;
  P: PChar;
begin
  if (InBufLen > OutBufLen - 8) or (InBufLen mod 8 <> 0) then
    Result := 0
  else begin
    P := @OutBuf;
    Move(InBuf,(P+8)^,InBufLen);
    C := TAES_Wrap.Create(Key,KeyLen,0);
    try
      C.Encrypt((P+8)^,InBufLen);
      Move(C.IV,OutBuf,8);
      Result := 8 + InBufLen;
    finally
      C.Free;
    end;
  end;
end;

function KeyWrapDecrypt(InBuf: ISecretKey;
                        Key: ISecretKey;
                        var OutBuf: ISecretKey): Boolean;
begin
  if OutBuf = nil then OutBuf := TSecretKey.Create('');
  OutBuf.SetLength(InBuf.KeyLen - 8);
  Result := KeyWrapDecrypt(InBuf.Key^,InBuf.KeyLen,
                           Key.Key^,Key.KeyLen,
                           OutBuf.Key^,OutBuf.KeyLen) > 0;
end;

function KeyWrapEncrypt(InBuf: ISecretKey;
                        Key: ISecretKey;
                        var OutBuf: ISecretKey): Boolean;
begin
  if OutBuf = nil then OutBuf := TSecretKey.Create('');
  OutBuf.SetLength(InBuf.KeyLen + 8);
  Result := KeyWrapEncrypt(InBuf.Key^,InBuf.KeyLen,
                           Key.Key^,Key.KeyLen,
                           OutBuf.Key^,OutBuf.KeyLen) > 0;
end;

const
  MAXBC= 8;
  MAXKC= 8;

{ TRijndael_ECB }

procedure TRijndael_ECB.CleanUp;
begin
  ProtectClear(rk,Sizeof(rk));
  ProtectClear(drk,Sizeof(drk));
  ProtectClear(IV,Sizeof(IV));
  NumRounds:= 0;
end;

{$IFNDEF ECB}
procedure TRijndael_ECB.Decrypt(var Buf; Count: Integer);
begin
  Assert(False);
end;
{$ELSE}
{$IFDEF PIC}
procedure TRijndael_ECB.Decrypt(var Buf; Count: Integer);
begin
  inherited Decrypt(Buf,Count);
end;
{$ELSE}
{$IFDEF AES_BG}
procedure TRijndael_ECB.Decrypt(var Buf; Count: Integer);
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@Exit
@@DcrMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  {$I RijndaelDcrBG.inc}
  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@DcrMain

@@Exit:
  add ESP,12

  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;
{$ELSE}
procedure TRijndael_ECB.Decrypt(var Buf; Count: Integer);
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@Exit
@@DcrMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  {$I RijndaelDcr.inc}
  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@DcrMain

@@Exit:
  add ESP,12

  pop EBP
  pop EDI
  pop ESI
  pop EBX
  ret

  DB 0,0,0
  {$I RijndaelDcrTbl.inc}
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF PIC}
procedure TRijndael_ECB.DecryptBlock(var Buf);
type
  TBlock = array [0..3] of LongWord;
var
  P: ^TBlock;
  A, B, C, D, E, F, G, H: LongWord;
  I: Integer;
begin
  P := @Buf;
  A := P[0] xor drk[0,0];
  B := P[1] xor drk[0,1];
  C := P[2] xor drk[0,2];
  D := P[3] xor drk[0,3];
  for I := 1 to numrounds-1 do begin
    E := A;
    G := B;
    H := D;

    A := drk[I,0];
    B := drk[I,1];
    F := Lo(E);
    A := A xor LongWord(T4[0,F]);
    D := drk[I,3];
    F := Hi(E);
    E := E shr 16;
    B := B xor LongWord(T4[1,F]);
    F := Lo(E);
    E := Hi(E);
    D := D xor LongWord(T4[3,E]);
    E := C;
    C := drk[I,2];
    C := C xor LongWord(T4[2,F]);

    F := Lo(E);
    C := C xor LongWord(T4[0,F]);
    F := Hi(E);
    E := E shr 16;
    D := D xor LongWord(T4[1,F]);
    F := Lo(E);
    E := Hi(E);
    A := A xor LongWord(T4[2,F]);
    B := B xor LongWord(T4[3,E]);

    F := Lo(G);
    B := B xor LongWord(T4[0,F]);
    F := Hi(G);
    G := G shr 16;
    C := C xor LongWord(T4[1,F]);
    F := Lo(G);
    E := Hi(G);
    D := D xor LongWord(T4[2,F]);
    A := A xor LongWord(T4[3,E]);

    F := Lo(H);
    D := D xor LongWord(T4[0,F]);
    F := Hi(H);
    H := H shr 16;
    A := A xor LongWord(T4[1,F]);
    F := Lo(H);
    E := Hi(H);
    B := B xor LongWord(T4[2,F]);
    C := C xor LongWord(T4[3,E]);
  end;
  E := A;
  G := B;
  H := D;

  I := numrounds;

  A := drk[I,0];
  B := drk[I,1];
  F := Lo(E);
  A := A xor LongWord(S4[0,F]);
  D := drk[I,3];
  F := Hi(E);
  E := E shr 16;
  B := B xor LongWord(S4[1,F]);
  F := Lo(E);
  E := Hi(E);
  D := D xor LongWord(S4[3,E]);
  E := C;
  C := drk[I,2];
  C := C xor LongWord(S4[2,F]);

  F := Lo(E);
  C := C xor LongWord(S4[0,F]);
  F := Hi(E);
  E := E shr 16;
  D := D xor LongWord(S4[1,F]);
  F := Lo(E);
  E := Hi(E);
  A := A xor LongWord(S4[2,F]);
  B := B xor LongWord(S4[3,E]);

  F := Lo(G);
  B := B xor LongWord(S4[0,F]);
  F := Hi(G);
  G := G shr 16;
  C := C xor LongWord(S4[1,F]);
  F := Lo(G);
  E := Hi(G);
  D := D xor LongWord(S4[2,F]);
  A := A xor LongWord(S4[3,E]);

  F := Lo(H);
  D := D xor LongWord(S4[0,F]);
  F := Hi(H);
  H := H shr 16;
  A := A xor LongWord(S4[1,F]);
  F := Lo(H);
  E := Hi(H);
  B := B xor LongWord(S4[2,F]);
  C := C xor LongWord(S4[3,E]);

  P[0] := A;
  P[1] := B;
  P[2] := C;
  P[3] := D;
{$WARNINGS ON}
end;
{$ELSE}
{$IFDEF AES_BG}
procedure TRijndael_ECB.DecryptBlock(var Buf);
asm
  push EBX
  push ESI
  push EDI
  push EBP
  {$I RijndaelDcrBG.inc}
  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;
{$ELSE}
procedure TRijndael_ECB.DecryptBlock(var Buf);
asm
  push EBX
  push ESI
  push EDI
  push EBP
  {$I RijndaelDcr.inc}
  pop EBP
  pop EDI
  pop ESI
  pop EBX
  ret

  DB 0,0,0
  {$I RijndaelDcrTbl.inc}
end;
{$ENDIF}
{$ENDIF}

{$IFDEF ECB}
{$IFDEF PIC}
procedure TRijndael_ECB.Encrypt(var Buf; Count: Integer);
begin
  inherited Encrypt(Buf,Count);
end;
{$ELSE}
{$IFDEF AES_BG}
procedure TRijndael_ECB.Encrypt(var Buf; Count: Integer);
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@Exit
@@EncMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  {$I RijndaelEncBG.inc}
  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@Exit:
  add ESP,12

  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;
{$ELSE}
procedure TRijndael_ECB.Encrypt(var Buf; Count: Integer);
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@Exit
@@EncMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  {$I RijndaelEnc.inc}
  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@Exit:
  add ESP,12

  pop EBP
  pop EDI
  pop ESI
  pop EBX
  ret

  DB 0,0,0,0,0,0,0
  {$I RijndaelEncTbl.inc}
end;
{$ENDIF}
{$ENDIF}
{$ELSE}
procedure TRijndael_ECB.Encrypt(var Buf; Count: Integer);
begin
  Assert(False);
end;
{$ENDIF}

{$IFDEF PIC}
procedure TRijndael_ECB.EncryptBlock(var Buf);
type
  TBlock = array [0..3] of LongWord;
var
  P: ^TBlock;
  A, B, C, D, E, F, G, H: LongWord;
  I: Integer;
begin
  P := @Buf;
  A := P[0] xor rk[0,0];
  B := P[1] xor rk[0,1];
  C := P[2] xor rk[0,2];
  D := P[3] xor rk[0,3];
  for I := 1 to numrounds-1 do begin
    E := A;
    G := B;
    H := D;

    A := rk[I,0];
    D := rk[I,3];
    F := Lo(E);
    A := A xor LongWord(T0[0,F]);
    B := rk[I,1];
    F := Hi(E);
    E := E shr 16;
    D := D xor LongWord(T0[1,F]);
    F := Lo(E);
    E := Hi(E);
    B := B xor LongWord(T0[3,E]);
    E := C;
    C := rk[I,2];
    C := C xor LongWord(T0[2,F]);

    F := Lo(E);
    C := C xor LongWord(T0[0,F]);
    F := Hi(E);
    E := E shr 16;
    B := B xor LongWord(T0[1,F]);
    F := Lo(E);
    E := Hi(E);
    A := A xor LongWord(T0[2,F]);
    D := D xor LongWord(T0[3,E]);

    F := Lo(G);
    B := B xor LongWord(T0[0,F]);
    F := Hi(G);
    G := G shr 16;
    A := A xor LongWord(T0[1,F]);
    F := Lo(G);
    E := Hi(G);
    D := D xor LongWord(T0[2,F]);
    C := C xor LongWord(T0[3,E]);

    F := Lo(H);
    D := D xor LongWord(T0[0,F]);
    F := Hi(H);
    H := H shr 16;
    C := C xor LongWord(T0[1,F]);
    F := Lo(H);
    E := Hi(H);
    B := B xor LongWord(T0[2,F]);
    A := A xor LongWord(T0[3,E]);
  end;
  E := A;
  G := B;
  H := D;

  I := numrounds;

  A := rk[I,0];
  D := rk[I,3];
  F := Lo(E);
  A := A xor LongWord(S0[0,F]);
  B := rk[I,1];
  F := Hi(E);
  E := E shr 16;
  D := D xor LongWord(S0[1,F]);
  F := Lo(E);
  E := Hi(E);
  B := B xor LongWord(S0[3,E]);
  E := C;
  C := rk[I,2];
  C := C xor LongWord(S0[2,F]);

  F := Lo(E);
  C := C xor LongWord(S0[0,F]);
  F := Hi(E);
  E := E shr 16;
  B := B xor LongWord(S0[1,F]);
  F := Lo(E);
  E := Hi(E);
  A := A xor LongWord(S0[2,F]);
  D := D xor LongWord(S0[3,E]);

  F := Lo(G);
  B := B xor LongWord(S0[0,F]);
  F := Hi(G);
  G := G shr 16;
  A := A xor LongWord(S0[1,F]);
  F := Lo(G);
  E := Hi(G);
  D := D xor LongWord(S0[2,F]);
  C := C xor LongWord(S0[3,E]);

  F := Lo(H);
  D := D xor LongWord(S0[0,F]);
  F := Hi(H);
  H := H shr 16;
  C := C xor LongWord(S0[1,F]);
  F := Lo(H);
  E := Hi(H);
  B := B xor LongWord(S0[2,F]);
  A := A xor LongWord(S0[3,E]);

  P[0] := A;
  P[1] := B;
  P[2] := C;
  P[3] := D;
{$WARNINGS ON}
end;
{$ELSE}
{$IFDEF AES_BG}
procedure TRijndael_ECB.EncryptBlock(var Buf);
asm
  push EBX
  push ESI
  push EDI
  push EBP
  {$I RijndaelEncBG.inc}
  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;
{$ELSE}
procedure TRijndael_ECB.EncryptBlock(var Buf);
asm
  push EBX
  push ESI
  push EDI
  push EBP
  {$I RijndaelEnc.inc}
  pop EBP
  pop EDI
  pop ESI
  pop EBX
  ret

  DB 0,0,0,0,0,0
  {$I RijndaelEncTbl.inc}
end;
{$ENDIF}
{$ENDIF}

procedure InvMixColumn(a: PByteArray; BC: byte);
var
  j: longint;
begin
  for j:= 0 to (BC-1) do
    PDWord(@(a^[j*4]))^ := PDWord(@U1[a^[j*4+0]])^ xor
                           PDWord(@U2[a^[j*4+1]])^ xor
                           PDWord(@U3[a^[j*4+2]])^ xor
                           PDWord(@U4[a^[j*4+3]])^;
end;

class function TRijndael_ECB.Mode: TCipherMode;
begin
  Result := cmECB;
end;

procedure TRijndael_ECB.SetUp(const AKey; Count, VectorSize: Integer);
var
  KC, ROUNDS, j, r, t, rconpointer: longint;
  tk: array[0..MAXKC-1,0..3] of byte;
  Size: LongInt;
begin
  Nag(Count);
  CleanUp;
  if (Count<= 0) then
    raise Exception.Create(Format('Rijndael: Invalid key size - %d',[Count]));

  Size := Count;

  FillChar(tk,Sizeof(tk),0);
  Move(AKey,tk,Size);
  if Size <= 16 then begin
    KC := 4;
    Rounds := 10;
  end else if Size <= 24 then begin
    KC := 6;
    Rounds := 12;
  end else begin
    KC := 8;
    Rounds := 14;
  end;
  numrounds := rounds;
  r := 0;
  t := 0;
  j := 0;
  while (j < KC) and (r < rounds+1) do begin
    while (j < KC) and (t < BC) do begin
      rk[r,t] := PDWord(@tk[j])^;
      Inc(j);
      Inc(t);
    end;
    if t = BC then begin
      t := 0;
      Inc(r);
    end;
  end;
  rconpointer := 0;
  while r < rounds+1 do begin
    tk[0,0] := tk[0,0] xor S[tk[KC-1,1]];
    tk[0,1] := tk[0,1] xor S[tk[KC-1,2]];
    tk[0,2] := tk[0,2] xor S[tk[KC-1,3]];
    tk[0,3] := tk[0,3] xor S[tk[KC-1,0]];
    tk[0,0] := tk[0,0] xor rcon[rconpointer];
    Inc(rconpointer);
    if KC <> 8 then begin
      for j := 1 to KC-1 do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end else begin
      for j:= 1 to ((KC div 2)-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
      tk[KC shr 1,0]:= tk[KC shr 1,0] xor S[tk[KC shr 1 - 1,0]];
      tk[KC shr 1,1]:= tk[KC shr 1,1] xor S[tk[KC shr 1 - 1,1]];
      tk[KC shr 1,2]:= tk[KC shr 1,2] xor S[tk[KC shr 1 - 1,2]];
      tk[KC shr 1,3]:= tk[KC shr 1,3] xor S[tk[KC shr 1 - 1,3]];
      for j:= (KC shr 1) + 1 to KC-1 do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end;
    j:= 0;
    while (j < KC) and (r < rounds+1) do begin
      while (j < KC) and (t < BC) do begin
        rk[r,t] := PDWord(@tk[j])^;
        Inc(j);
        Inc(t);
      end;
      if t = BC then begin
        Inc(r);
        t := 0;
      end;
    end;
  end;
  ProtectClear(tk,SizeOf(tk));
  drk[0] := rk[numrounds];
  drk[numrounds] := rk[0];
  for r:= 1 to (numrounds-1) do begin
    drk[r] := rk[numrounds-r];
    InvMixColumn(@drk[r],BC);
  end;
end;

class function TRijndael_ECB.BlockVectorSize: Integer;
begin
  Result := 0;
end;

class function TRijndael_ECB.BlockSize: Integer;
begin
  Result := 16;
end;

constructor TRijndael_ECB.Create(const AKey; Count, VectorSize: Integer);
begin
  FIV := @IV;
  inherited;
end;

class function TRijndael_ECB.MaxKeySize: Integer;
begin
  Result := 32;
end;

class function TRijndael_ECB.MinKeySize: Integer;
begin
  Result := 16;
end;

class function TRijndael_ECB.KeyedIVOID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

class function TRijndael_ECB.OID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '2.16.840.1.101.3.4.1.1';
    24: Result := '2.16.840.1.101.3.4.1.21';
    32: Result := '2.16.840.1.101.3.4.1.41';
  else
    Result := nil;
  end;
end;

class function TRijndael_ECB.Algorithm: TCipherAlg;
begin
  Result := caRijndael;
end;

class function TRijndael_ECB.AlgorithmName: PChar;
begin
  Result := 'Rijndael';
end;

{$IFDEF PIC}
procedure TRijndael_ECB.DecryptBlockToDst(var Dst; const Src);
type
  TBlock = array [0..3] of LongWord;
var
  P: ^TBlock;
  A, B, C, D, E, F, G, H: LongWord;
  I: Integer;
begin
  P := @Src;
  A := P[0] xor drk[0,0];
  B := P[1] xor drk[0,1];
  C := P[2] xor drk[0,2];
  D := P[3] xor drk[0,3];
  for I := 1 to numrounds-1 do begin
    E := A;
    G := B;
    H := D;

    A := drk[I,0];
    B := drk[I,1];
    F := Lo(E);
    A := A xor LongWord(T4[0,F]);
    D := drk[I,3];
    F := Hi(E);
    E := E shr 16;
    B := B xor LongWord(T4[1,F]);
    F := Lo(E);
    E := Hi(E);
    D := D xor LongWord(T4[3,E]);
    E := C;
    C := drk[I,2];
    C := C xor LongWord(T4[2,F]);

    F := Lo(E);
    C := C xor LongWord(T4[0,F]);
    F := Hi(E);
    E := E shr 16;
    D := D xor LongWord(T4[1,F]);
    F := Lo(E);
    E := Hi(E);
    A := A xor LongWord(T4[2,F]);
    B := B xor LongWord(T4[3,E]);

    F := Lo(G);
    B := B xor LongWord(T4[0,F]);
    F := Hi(G);
    G := G shr 16;
    C := C xor LongWord(T4[1,F]);
    F := Lo(G);
    E := Hi(G);
    D := D xor LongWord(T4[2,F]);
    A := A xor LongWord(T4[3,E]);

    F := Lo(H);
    D := D xor LongWord(T4[0,F]);
    F := Hi(H);
    H := H shr 16;
    A := A xor LongWord(T4[1,F]);
    F := Lo(H);
    E := Hi(H);
    B := B xor LongWord(T4[2,F]);
    C := C xor LongWord(T4[3,E]);
  end;
  E := A;
  G := B;
  H := D;

  I := numrounds;

  A := drk[I,0];
  B := drk[I,1];
  F := Lo(E);
  A := A xor LongWord(S4[0,F]);
  D := drk[I,3];
  F := Hi(E);
  E := E shr 16;
  B := B xor LongWord(S4[1,F]);
  F := Lo(E);
  E := Hi(E);
  D := D xor LongWord(S4[3,E]);
  E := C;
  C := drk[I,2];
  C := C xor LongWord(S4[2,F]);

  F := Lo(E);
  C := C xor LongWord(S4[0,F]);
  F := Hi(E);
  E := E shr 16;
  D := D xor LongWord(S4[1,F]);
  F := Lo(E);
  E := Hi(E);
  A := A xor LongWord(S4[2,F]);
  B := B xor LongWord(S4[3,E]);

  F := Lo(G);
  B := B xor LongWord(S4[0,F]);
  F := Hi(G);
  G := G shr 16;
  C := C xor LongWord(S4[1,F]);
  F := Lo(G);
  E := Hi(G);
  D := D xor LongWord(S4[2,F]);
  A := A xor LongWord(S4[3,E]);

  F := Lo(H);
  D := D xor LongWord(S4[0,F]);
  F := Hi(H);
  H := H shr 16;
  A := A xor LongWord(S4[1,F]);
  F := Lo(H);
  E := Hi(H);
  B := B xor LongWord(S4[2,F]);
  C := C xor LongWord(S4[3,E]);

  P := @Dst;
  P[0] := A;
  P[1] := B;
  P[2] := C;
  P[3] := D;
end;
{$ELSE}
{$IFDEF AES_BG}
procedure TRijndael_ECB.DecryptBlockToDst(var Dst; const Src);
asm
  push EBX
  push ESI
  push EDI
  push EBP
  {$I RijndaelDcr2DBG.inc}
  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;
{$ELSE}
procedure TRijndael_ECB.DecryptBlockToDst(var Dst; const Src);
asm
  push EBX
  push ESI
  push EDI
  push EBP
  {$I RijndaelDcr2D.inc}
  pop EBP
  pop EDI
  pop ESI
  pop EBX
  ret
  {$I RijndaelDcrTbl.inc}
end;
{$ENDIF}
{$ENDIF}

{$IFDEF PIC}
procedure TRijndael_ECB.EncryptBlockToDst(var Dst; const Src);
type
  TBlock = array [0..3] of LongWord;
var
  P: ^TBlock;
  A, B, C, D, E, F, G, H: LongWord;
  I: Integer;
begin
  P := @Src;
  A := P[0] xor rk[0,0];
  B := P[1] xor rk[0,1];
  C := P[2] xor rk[0,2];
  D := P[3] xor rk[0,3];
  for I := 1 to numrounds-1 do begin
    E := A;
    G := B;
    H := D;

    A := rk[I,0];
    D := rk[I,3];
    F := Lo(E);
    A := A xor LongWord(T0[0,F]);
    B := rk[I,1];
    F := Hi(E);
    E := E shr 16;
    D := D xor LongWord(T0[1,F]);
    F := Lo(E);
    E := Hi(E);
    B := B xor LongWord(T0[3,E]);
    E := C;
    C := rk[I,2];
    C := C xor LongWord(T0[2,F]);

    F := Lo(E);
    C := C xor LongWord(T0[0,F]);
    F := Hi(E);
    E := E shr 16;
    B := B xor LongWord(T0[1,F]);
    F := Lo(E);
    E := Hi(E);
    A := A xor LongWord(T0[2,F]);
    D := D xor LongWord(T0[3,E]);

    F := Lo(G);
    B := B xor LongWord(T0[0,F]);
    F := Hi(G);
    G := G shr 16;
    A := A xor LongWord(T0[1,F]);
    F := Lo(G);
    E := Hi(G);
    D := D xor LongWord(T0[2,F]);
    C := C xor LongWord(T0[3,E]);

    F := Lo(H);
    D := D xor LongWord(T0[0,F]);
    F := Hi(H);
    H := H shr 16;
    C := C xor LongWord(T0[1,F]);
    F := Lo(H);
    E := Hi(H);
    B := B xor LongWord(T0[2,F]);
    A := A xor LongWord(T0[3,E]);
  end;
  E := A;
  G := B;
  H := D;

  I := numrounds;

  A := rk[I,0];
  D := rk[I,3];
  F := Lo(E);
  A := A xor LongWord(S0[0,F]);
  B := rk[I,1];
  F := Hi(E);
  E := E shr 16;
  D := D xor LongWord(S0[1,F]);
  F := Lo(E);
  E := Hi(E);
  B := B xor LongWord(S0[3,E]);
  E := C;
  C := rk[I,2];
  C := C xor LongWord(S0[2,F]);

  F := Lo(E);
  C := C xor LongWord(S0[0,F]);
  F := Hi(E);
  E := E shr 16;
  B := B xor LongWord(S0[1,F]);
  F := Lo(E);
  E := Hi(E);
  A := A xor LongWord(S0[2,F]);
  D := D xor LongWord(S0[3,E]);

  F := Lo(G);
  B := B xor LongWord(S0[0,F]);
  F := Hi(G);
  G := G shr 16;
  A := A xor LongWord(S0[1,F]);
  F := Lo(G);
  E := Hi(G);
  D := D xor LongWord(S0[2,F]);
  C := C xor LongWord(S0[3,E]);

  F := Lo(H);
  D := D xor LongWord(S0[0,F]);
  F := Hi(H);
  H := H shr 16;
  C := C xor LongWord(S0[1,F]);
  F := Lo(H);
  E := Hi(H);
  B := B xor LongWord(S0[2,F]);
  A := A xor LongWord(S0[3,E]);

  P := @Dst;
  P[0] := A;
  P[1] := B;
  P[2] := C;
  P[3] := D;
{$WARNINGS ON}
end;
{$ELSE}
{$IFDEF AES_BG}
procedure TRijndael_ECB.EncryptBlockToDst(var Dst; const Src);
asm
  push EBX
  push ESI
  push EDI
  push EBP
  {$I RijndaelEnc2DBG.inc}
  pop EBP
  pop EDI
  pop ESI
  pop EBX
end;
{$ELSE}
procedure TRijndael_ECB.EncryptBlockToDst(var Dst; const Src);
asm
  push EBX
  push ESI
  push EDI
  push EBP
  {$I RijndaelEnc2D.inc}
  pop EBP
  pop EDI
  pop ESI
  pop EBX
  ret

  DB 0,0,0,0,0,0
  {$I RijndaelEncTbl.inc}
end;
{$ENDIF}
{$ENDIF}

constructor TRijndael_ECB.CreateClone(ACipher: TCipher);
begin         
  VirtualLock;
  if ACipher is TRijndael_ECB then begin   
    FIV := @IV;
    numrounds := TRijndael_ECB(ACipher).numrounds;
    rk := TRijndael_ECB(ACipher).rk;
    drk := TRijndael_ECB(ACipher).drk;
    FFBIndex := TRijndael_ECB(ACipher).FFBIndex;
    IV := TRijndael_ECB(ACipher).IV;
    ModeRatio := TRijndael_ECB(ACipher).ModeRatio;
  end else
    inherited;
end;

{ TRijndael_CFB }

procedure TRijndael_CFB.Decrypt(var Buf; Count: Integer);
begin
  Decrypt_CFB(Buf,Count);
end;

procedure TRijndael_CFB.Encrypt(var Buf; Count: Integer);
begin
  Encrypt_CFB(Buf,Count);
end;

class function TRijndael_CFB.Mode: TCipherMode;
begin
  Result := cmCFB;
end;

class function TRijndael_CFB.BlockVectorSize: Integer;
begin
  Result := 1;
end;

constructor TRijndael_CFB.Create(const AKey; Count, VectorSize: Integer);
begin
  ModeRatio := 16; 
  inherited;
end;

class function TRijndael_CFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.22';
    24: Result := '1.3.6.1.4.1.13085.1.23';
    32: Result := '1.3.6.1.4.1.13085.1.24';
  else
    Result := nil;
  end;
end;

class function TRijndael_CFB.OID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '2.16.840.1.101.3.4.1.4';
    24: Result := '2.16.840.1.101.3.4.1.24';
    32: Result := '2.16.840.1.101.3.4.1.44'; 
  else
    Result := nil;
  end;
end;

{ TRijndael_PCFB }

constructor TRijndael_PCFB.Create(const AKey; Count, VectorSize: Integer);
begin
  inherited;
  ModeRatio := 1;
end;

procedure TRijndael_PCFB.Decrypt(var Buf; Count: Integer);
var
  i, j, c: longint;
  CT: array[0..15] of byte;
begin
  c := Count div ModeRatio;
  for i:= 0 to c-1 do begin 
    Move(PByteArray(@Buf)^[i*ModeRatio],CT,ModeRatio);
    EncryptBlock(IV);
    for j := 0 to ModeRatio - 1 do
      PByteArray(@Buf)^[i*ModeRatio + j]:=
        PByteArray(@Buf)^[i*ModeRatio + j] xor IV[j];
    Move(IV[ModeRatio],IV[0],16 - ModeRatio);
    Move(CT,IV[16 - ModeRatio],ModeRatio);
  end;
  if (Count mod ModeRatio) <> 0 then begin
    Move(PByteArray(@Buf)^[c*ModeRatio],CT,Count mod ModeRatio);
    EncryptBlock(IV);
    for j := 0 to (Count mod ModeRatio) - 1 do
      PByteArray(@Buf)^[c*ModeRatio + j]:=
        PByteArray(@Buf)^[c*ModeRatio + j] xor IV[j];
    Move(IV[Count mod ModeRatio],IV[0],16 - (Count mod ModeRatio));
    Move(CT,IV[16 - (Count mod ModeRatio)],(Count mod ModeRatio));
  end;
end;

procedure TRijndael_PCFB.Encrypt(var Buf; Count: Integer);
var
  i, j, c: longint;
begin                      
  c := Count div ModeRatio;
  for i:= 0 to c-1 do begin
    EncryptBlock(IV);
    for j := 0 to ModeRatio - 1 do
      PByteArray(@Buf)^[i*ModeRatio + j]:=
        PByteArray(@Buf)^[i*ModeRatio + j] xor IV[j];
    Move(IV[ModeRatio],IV[0],16 - ModeRatio);
    Move(PByteArray(@Buf)^[i*ModeRatio],IV[16 - ModeRatio],ModeRatio);
  end;      
  if (Count mod ModeRatio) <> 0 then begin
    EncryptBlock(IV);
    for j := 0 to (Count mod ModeRatio) - 1 do
      PByteArray(@Buf)^[c*ModeRatio + j]:=
        PByteArray(@Buf)^[c*ModeRatio + j] xor IV[j];
    Move(IV[Count mod ModeRatio],IV[0],16 - (Count mod ModeRatio));
    Move(PByteArray(@Buf)^[c*ModeRatio],IV[16 - (Count mod ModeRatio)],Count mod ModeRatio);
  end;
end;

class function TRijndael_PCFB.KeyedIVOID(KeySize: Integer): PChar;
begin   
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.7';
    24: Result := '1.3.6.1.4.1.13085.1.8';
    32: Result := '1.3.6.1.4.1.13085.1.9';
  else
    Result := nil;
  end;
end;

class function TRijndael_PCFB.Mode: TCipherMode;
begin
  Result := cmPCFB;
end;

class function TRijndael_PCFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TRijndael_ABC }

procedure TRijndael_ABC.Decrypt(var Buf; Count: Integer);
{$IFDEF ABC}
{$IFDEF PIC}
begin
  Decrypt_ABC(Buf,Count);
{$ELSE}
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push ECX
  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@LastBlock
@@DcrMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  // B[i] := C[i] xor H[i-1]
  mov EBX,[ESI]
  mov [ESI+32],EBX
  xor EBX,[EDX]
  mov [ESI],EBX
  mov EBX,[ESI+4]
  mov [ESI+36],EBX
  xor EBX,[EDX+4]
  mov [ESI+4],EBX
  mov EBX,[ESI+8]
  mov [ESI+40],EBX
  xor EBX,[EDX+8]
  mov [ESI+8],EBX
  mov EBX,[ESI+12]
  mov [ESI+44],EBX
  xor EBX,[EDX+12]
  mov [ESI+12],EBX
  // A[i] := D(B[i]);
  mov EDX,ESI
{$IFDEF AES_BG}
  {$I RijndaelDcrBG.inc}
{$ELSE}
  {$I RijndaelDcr.inc}
{$ENDIF}
  // H[i] := A[i] xor C[i - 1]
  // P[i] := H[i] xor H[i-1]
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  mov EBX,[ESI+16]
  mov ECX,[EDX]
  mov [ESI+16],ECX
  xor [ESI],EBX
  mov EBX,[ESI]
  xor EBX,[ESI+32]
  mov [EDX],EBX

  mov EBX,[ESI+20]
  mov ECX,[EDX+4]
  mov [ESI+20],ECX
  xor [ESI+4],EBX
  mov EBX,[ESI+4]
  xor EBX,[ESI+36]
  mov [EDX+4],EBX
  mov EBX,[ESI+24]
  mov ECX,[EDX+8]
  mov [ESI+24],ECX
  xor [ESI+8],EBX
  mov EBX,[ESI+8]
  xor EBX,[ESI+40]
  mov [EDX+8],EBX
  mov EBX,[ESI+28]
  mov ECX,[EDX+12]
  mov [ESI+28],ECX
  xor [ESI+12],EBX
  mov EBX,[ESI+12]
  xor EBX,[ESI+44]
  mov [EDX+12],EBX


  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@DcrMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  // XORBlock128(IV[0],IV[16]);
  mov EBX,[ESI+16]
  xor [ESI],EBX
  mov EBX,[ESI+20]
  xor [ESI+4],EBX
  mov EBX,[ESI+24]
  xor [ESI+8],EBX
  mov EBX,[ESI+28]
  xor [ESI+12],EBX
  // EncryptBlock(IV[0]);
  mov EDX,ESI
{$IFDEF AES_BG}
  {$I RijndaelEnc2BG.inc}
{$ELSE}
  {$I RijndaelEnc2.inc}
{$ENDIF}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  mov ECX,[ESP+12]
  and ECX,15
@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop EDI
  pop ESI
  pop EBX
{$IFNDEF AES_BG}
  ret

  DB 0,0
  {$I RijndaelDcrTbl.inc}
  {$I RijndaelEncTbl.inc}
{$ENDIF}
{$ENDIF}
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;
{var
  BlockCount, I: longint;
begin
  BlockCount := Count div 16;
  for I := 0 to BlockCount - 1 do begin
    Move(IV[0],IV[32],16);
    // H[i] := D(C[i] xor H[i-1]) xor C[i-1]
    XORBlock128(IV[0],Ptr(LongInt(@Buf) + I*16)^);
    DecryptBlock(IV[0]);
    XORBlock128(IV[0],IV[16]);

    Move(Ptr(LongInt(@Buf) + I*16)^,IV[16],16);
    // P[i] := H[i] xor H[i-1]
    Move(IV[0],Ptr(LongInt(@Buf) + I*16)^,16);
    XORBlock128(Ptr(LongInt(@Buf) + I*16)^,IV[32]);
  end;

  if BlockCount * 16 < Count then begin
    XORBlock128(IV[0],IV[16]);
    EncryptBlock(IV[0]);
    for I := 0 to Count - BlockCount * 16 - 1 do
      Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) :=
        Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) xor IV[I];
  end;
end;}

procedure TRijndael_ABC.Encrypt(var Buf; Count: Integer);
{$IFDEF ABC}
{$IFDEF PIC}
begin
  Encrypt_ABC(Buf,Count);
{$ELSE}
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push ECX
  push EAX
  push EDX

  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@LastBlock

@@EncMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  // H[i] := P[i] xor H[i-1];
  // A[i] := H[i] xor C[i-1];
  mov EBX,dword [ESI]
  mov dword [ESI+32],EBX
  xor EBX,dword [EDX]
  mov dword [ESI],EBX
  xor dword [ESI+16],EBX
  mov EBX,dword [ESI+4]
  mov dword [ESI+36],EBX
  xor EBX,dword [EDX+4]
  mov dword [ESI+4],EBX
  xor dword [ESI+20],EBX
  mov EBX,dword [ESI+8]
  mov dword [ESI+40],EBX
  xor EBX,dword [EDX+8]
  mov dword [ESI+8],EBX
  xor dword [ESI+24],EBX
  mov EBX,dword [ESI+12]
  mov dword [ESI+44],EBX
  xor EBX,dword [EDX+12]
  mov dword [ESI+12],EBX
  xor dword [ESI+28],EBX
  // B[i] := E(A[i]);
  lea EDX,[ESI + 16]
{$IFDEF AES_BG}
  {$I RijndaelEncBG.inc}
{$ELSE}
  {$I RijndaelEnc.inc}
{$ENDIF}
  // C[i] := B[i] xor H[i-1]
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  mov EBX,dword [ESI+16]
  xor EBX,dword [ESI+32]
  mov dword [EDX],EBX
  mov dword [ESI+16],EBX
  mov EBX,dword [ESI+20]
  xor EBX,dword [ESI+36]
  mov dword [EDX+4],EBX
  mov dword [ESI+20],EBX
  mov EBX,dword [ESI+24]
  xor EBX,dword [ESI+40]
  mov dword [EDX+8],EBX
  mov dword [ESI+24],EBX
  mov EBX,dword [ESI+28]
  xor EBX,dword [ESI+44]
  mov dword [EDX+12],EBX
  mov dword [ESI+28],EBX

  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit
  // XORBlock128(IV[0],IV[16]);
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  mov EBX,[ESI+16]
  xor [ESI],EBX
  mov EBX,[ESI+20]
  xor [ESI+4],EBX
  mov EBX,[ESI+24]
  xor [ESI+8],EBX
  mov EBX,[ESI+28]
  xor [ESI+12],EBX
  // EncryptBlock(IV[0]);
  mov EDX,ESI       
{$IFDEF AES_BG}
  {$I RijndaelEnc2BG.inc}
{$ELSE}
  {$I RijndaelEnc2.inc}
{$ENDIF}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  mov ECX,[ESP+12]
  and ECX,15
@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop EDI
  pop ESI
  pop EBX
{$IFNDEF AES_BG}
  ret

  DB 0,0,0,0,0
  {$I RijndaelEncTbl.inc}
{$ENDIF}
{$ENDIF}
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;
{var
  BlockCount, I: longint;
begin
  BlockCount := Count div 16;
  for I := 0 to BlockCount - 1 do begin
    Move(IV[0],IV[32],16);
    // H[i] := P[i] xor H[i-1]
    XORBlock128(IV[0],Ptr(LongInt(@Buf) + I*16)^);
    Move(IV[0],Ptr(LongInt(@Buf) + I*16)^,16);
    // C[i] := E(C[i-1] xor H[i]) xor H[i-1]
    XORBlock128(Ptr(LongInt(@Buf) + I*16)^,IV[16]);
    EncryptBlock(Ptr(LongInt(@Buf) + I*16)^);
    XORBlock128(Ptr(LongInt(@Buf) + I*16)^,IV[32]);

    Move(Ptr(LongInt(@Buf) + I*16)^,IV[16],16);
  end;

  if BlockCount * 16 < Count then begin
    XORBlock128(IV[0],IV[16]);
    EncryptBlock(IV[0]);
    for I := 0 to Count - BlockCount * 16 - 1 do
      Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) :=
        Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) xor IV[I];
  end;
end;}

class function TRijndael_ABC.Mode: TCipherMode;
begin
  Result := cmABC;
end;

class function TRijndael_ABC.BlockVectorSize: Integer;
begin
  Result := 2;
end;

class function TRijndael_ABC.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.4';
    24: Result := '1.3.6.1.4.1.13085.1.5';
    32: Result := '1.3.6.1.4.1.13085.1.6';
  else
    Result := nil;
  end;
end;

class function TRijndael_ABC.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TRijndael_PipedPCFB }

class function TRijndael_PipedPCFB.BlockVectorSize: Integer;
begin
  Result := 2;
end;

procedure TRijndael_PipedPCFB.Decrypt(var Buf; Count: Integer);
var
  BlockCount, I: longint;
begin
  BlockCount := Count div 16;
  for I := 0 to BlockCount - 1 do begin
    Move(IV[0],IV[32],16);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock128(IV[32],IV[16]);
    EncryptBlock(IV[32]);

    Move(Ptr(LongInt(@Buf) + I*16)^,IV[16],16);
    // P[i] := C[i] xor E(V[i] + V[i-1]);
    AddBlock128(IV[0],IV[32]);
    EncryptBlock(IV[0]);
    XORBlock128(Ptr(LongInt(@Buf) + I*16)^,IV[0]);

    Move(IV[32],IV[0],16);
  end;

  if BlockCount * 16 < Count then begin
    Move(IV[0],IV[32],16);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock128(IV[32],IV[16]);
    EncryptBlock(IV[32]);
    // P[i] := C[i] xor E(V[i] + V[i-1]);
    AddBlock128(IV[0],IV[32]);
    EncryptBlock(IV[0]);
    for I := 0 to Count - BlockCount * 16 - 1 do
      Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) :=
        Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) xor IV[I];
  end;
end;

procedure TRijndael_PipedPCFB.Encrypt(var Buf; Count: Integer);
var
  BlockCount, I: longint;
begin
  BlockCount := Count div 16;
  for I := 0 to BlockCount - 1 do begin
    Move(IV[0],IV[32],16);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock128(IV[32],IV[16]);
    EncryptBlock(IV[32]);

    // C[i] := P[i] xor E(V[i] + V[i-1]);
    AddBlock128(IV[0],IV[32]);
    EncryptBlock(IV[0]);
    XORBlock128(Ptr(LongInt(@Buf) + I*16)^,IV[0]);

    Move(Ptr(LongInt(@Buf) + I*16)^,IV[16],16);
    Move(IV[32],IV[0],16);
  end;

  if BlockCount * 16 < Count then begin
    Move(IV[0],IV[32],16);
    // V[i] := E(C[i-1] + V[i-1]);
    AddBlock128(IV[32],IV[16]);
    EncryptBlock(IV[32]);
    // P[i] := C[i] xor E(V[i] + V[i-1]);
    AddBlock128(IV[0],IV[32]);
    EncryptBlock(IV[0]);
    for I := 0 to Count - BlockCount * 16 - 1 do
      Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) :=
        Byte(Ptr(LongInt(@Buf) + I + BlockCount*16)^) xor IV[I];
  end;
end;


class function TRijndael_PipedPCFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.10';
    24: Result := '1.3.6.1.4.1.13085.1.11';
    32: Result := '1.3.6.1.4.1.13085.1.12';
  else
    Result := nil;
  end;
end;

class function TRijndael_PipedPCFB.Mode: TCipherMode;
begin
  Result := cmPipedPCFB
end;

class function TRijndael_PipedPCFB.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TRijndael_CTR }

class function TRijndael_CTR.BlockVectorSize: Integer;
begin
  Result := 1;
end;

procedure TRijndael_CTR.Decrypt(var Buf; Count: Integer);
{$IFDEF CTR}
{$IFDEF PIC}
begin
  Encrypt_CTR(Buf,Count);
{$ELSE}
asm
  push EBX
  push EDI
  push ESI
  push EBP

  push ECX
  push EAX
  push EDX

  lea ESI,dword ptr [EAX].TRijndael_ECB.IV

  add ESI,16
  mov ECX,EDX
  mov EDI,16
  mov EBX,dword [ESP+8]
  {$I FBEncFirst.inc}
  mov dword [ESP+8],EBX
  mov dword [ESP],EDX
  shr EBX,4
  push EBX
  sub ESI,16
  mov EAX,dword [ESP+8]
  mov EDX,dword [ESP+4]

  mov ECX,dword [ESP]
  test ECX,ECX
  jz @@LastBlock

@@EncMain:
  // IV := IV + 1;
  mov EBX,[ESI + 12]
  bswap EBX
  add EBX,1
  bswap EBX
  mov [ESI + 12],EBX
  mov EBX,[ESI + 8]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 8],EBX
  mov EBX,[ESI + 4]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 4],EBX
  mov EBX,[ESI + 0]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 0],EBX
  // EncryptBlock(IV);
  lea EDX,[ESI+16]
  lea ECX,[ESI]
{$IFDEF AES_BG}
  {$I RijndaelEnc2DBG.inc}
{$ELSE}
  {$I RijndaelEnc2D.inc}
{$ENDIF}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  mov EBX,[ESI+16]
  xor [EDX],EBX
  mov EBX,[ESI+20]
  xor [EDX+4],EBX
  mov EBX,[ESI+24]
  xor [EDX+8],EBX
  mov EBX,[ESI+28]
  xor [EDX+12],EBX


  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit

  // IV := IV + 1;
  mov EBX,[ESI + 12]
  bswap EBX
  add EBX,1
  bswap EBX
  mov [ESI + 12],EBX
  mov [ESI + 28],EBX
  mov EBX,[ESI + 8]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 8],EBX
  mov [ESI + 24],EBX
  mov EBX,[ESI + 4]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 4],EBX
  mov [ESI + 20],EBX
  mov EBX,[ESI + 0]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 0],EBX
  mov [ESI + 16],EBX
  // EncryptBlock(IV);
  lea EDX,[ESI+16]
{$IFDEF AES_BG}
  {$I RijndaelEnc2BG.inc}
{$ELSE}
  {$I RijndaelEnc2.inc}
{$ENDIF}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  add ESI,16
  mov ECX,[ESP+12]
  and ECX,15

  mov dword [EAX].TBlockCipher.FFBIndex,ECX

@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop ESI
  pop EDI
  pop EBX
{$IFNDEF AES_BG}
  ret

  db 0,0
  {$I RijndaelEncTbl.inc}
{$ENDIF}
{$ENDIF}
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;

procedure TRijndael_CTR.Encrypt(var Buf; Count: Integer);
{$IFDEF CTR}  
{$IFDEF PIC}
begin
  Encrypt_CTR(Buf,Count);
{$ELSE}
asm
  push EBX
  push EDI
  push ESI
  push EBP

  push ECX
  push EAX
  push EDX

  lea ESI,dword ptr [EAX].TRijndael_ECB.IV

  add ESI,16
  mov ECX,EDX
  mov EDI,16
  mov EBX,dword [ESP+8]
  {$I FBEncFirst.inc}
  mov dword [ESP+8],EBX
  mov dword [ESP],EDX
  shr EBX,4
  push EBX
  sub ESI,16
  mov EAX,dword [ESP+8]
  mov EDX,dword [ESP+4]

  mov ECX,dword [ESP]
  test ECX,ECX
  jz @@LastBlock

@@EncMain:
  // IV := IV + 1;
  mov EBX,[ESI + 12]
  bswap EBX
  add EBX,1
  bswap EBX
  mov [ESI + 12],EBX
  mov EBX,[ESI + 8]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 8],EBX
  mov EBX,[ESI + 4]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 4],EBX
  mov EBX,[ESI + 0]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 0],EBX
  // EncryptBlock(IV);
  lea EDX,[ESI+16]
  lea ECX,[ESI]
{$IFDEF AES_BG}
  {$I RijndaelEnc2DBG.inc}
{$ELSE}
  {$I RijndaelEnc2D.inc}
{$ENDIF}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  mov EBX,[ESI+16]
  xor [EDX],EBX
  mov EBX,[ESI+20]
  xor [EDX+4],EBX
  mov EBX,[ESI+24]
  xor [EDX+8],EBX
  mov EBX,[ESI+28]
  xor [EDX+12],EBX


  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit

  // IV := IV + 1;
  mov EBX,[ESI + 12]
  bswap EBX
  add EBX,1
  bswap EBX
  mov [ESI + 12],EBX
  mov [ESI + 28],EBX
  mov EBX,[ESI + 8]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 8],EBX
  mov [ESI + 24],EBX
  mov EBX,[ESI + 4]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 4],EBX
  mov [ESI + 20],EBX
  mov EBX,[ESI + 0]
  bswap EBX
  adc EBX,0
  bswap EBX
  mov [ESI + 0],EBX
  mov [ESI + 16],EBX
  // EncryptBlock(IV);
  lea EDX,[ESI+16]
{$IFDEF AES_BG}
  {$I RijndaelEnc2BG.inc}
{$ELSE}
  {$I RijndaelEnc2.inc}
{$ENDIF}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  add ESI,16
  mov ECX,[ESP+12]
  and ECX,15

  mov dword [EAX].TBlockCipher.FFBIndex,ECX

@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop ESI
  pop EDI
  pop EBX
{$IFNDEF AES_BG}
  ret

  db 0,0
  {$I RijndaelEncTbl.inc}
{$ENDIF}
{$ENDIF}
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;

class function TRijndael_CTR.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.1';
    24: Result := '1.3.6.1.4.1.13085.1.2';
    32: Result := '1.3.6.1.4.1.13085.1.3';
  else
    Result := nil;
  end;
end;

class function TRijndael_CTR.Mode: TCipherMode;
begin
  Result := cmCTR;
end;

class function TRijndael_CTR.OID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

{ TRijndael_CBC }

class function TRijndael_CBC.BlockVectorSize: Integer;
begin
  Result := 1;
end;

procedure TRijndael_CBC.Decrypt(var Buf; Count: Integer);
{$IFDEF CBC}
{$IFDEF PIC}
begin
  Decrypt_CBC(Buf,Count);
{$ELSE}
asm
  push EBX
  push ESI
  push EDI
  push EBP

  push ECX
  push EAX
  push EDX
  shr ECX,4
  push ECX
  test ECX,ECX
  jz @@LastBlock
@@EncMain:
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  // P[i] := D(C[i]) xor C[i-1];
  mov EBX,[EDX]
  mov [ESI+16],EBX
  mov EBX,[EDX+4]
  mov [ESI+20],EBX
  mov EBX,[EDX+8]
  mov [ESI+24],EBX
  mov EBX,[EDX+12]
  mov [ESI+28],EBX
{$IFDEF AES_BG}       
  {$I RijndaelDcrBG.inc}
{$ELSE}
  {$I RijndaelDcr.inc}
{$ENDIF}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  mov EBX,dword [ESI+16]
  mov ECX,dword [ESI]
  mov dword [ESI],EBX
  xor dword [EDX],ECX
  mov EBX,dword [ESI+20]
  mov ECX,dword [ESI+4]
  mov dword [ESI+4],EBX
  xor dword [EDX+4],ECX
  mov EBX,dword [ESI+24]
  mov ECX,dword [ESI+8]
  mov dword [ESI+8],EBX
  xor dword [EDX+8],ECX
  mov EBX,dword [ESI+28]
  mov ECX,dword [ESI+12]
  mov dword [ESI+12],EBX
  xor dword [EDX+12],ECX

  add dword [ESP+4],16
  dec dword [ESP]
  jnz @@EncMain

@@LastBlock:
  mov ECX,[ESP+12]
  and ECX,15
  jz @@Exit
  // EncryptBlock(IV[0]);
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  mov EDX,ESI     
{$IFDEF AES_BG}       
  {$I RijndaelEnc2BG.inc}
{$ELSE}
  {$I RijndaelEnc2.inc}
{$ENDIF}
  mov EAX,[ESP+8]
  mov EDX,[ESP+4]
  lea ESI,dword ptr [EAX].TRijndael_ECB.IV
  mov ECX,[ESP+12]
  and ECX,15
@@LastLoop:
  mov AL,[ESI]
  xor [EDX],AL
  inc ESI
  inc EDX
  loop @@LastLoop

@@Exit:
  add ESP,16

  pop EBP
  pop EDI
  pop ESI
  pop EBX
{$IFNDEF AES_BG}
  ret

  db 0,0,0,0
  {$I RijndaelDcrTbl.inc}
  {$I RijndaelEncTbl.inc}
{$ENDIF}
{$ENDIF}
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;

procedure TRijndael_CBC.Encrypt(var Buf; Count: Integer);
begin
  Encrypt_CBC(Buf,Count);
end;

class function TRijndael_CBC.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.16';
    24: Result := '1.3.6.1.4.1.13085.1.17';
    32: Result := '1.3.6.1.4.1.13085.1.18';
  else
    Result := nil;
  end;
end;

class function TRijndael_CBC.Mode: TCipherMode;
begin
  Result := cmCBC;
end;

class function TRijndael_CBC.OID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '2.16.840.1.101.3.4.1.2';
    24: Result := '2.16.840.1.101.3.4.1.22';
    32: Result := '2.16.840.1.101.3.4.1.42'; 
  else
    Result := nil;
  end;
end;

{ TRijndael_OFB }

procedure TRijndael_OFB.Decrypt(var Buf; Count: Integer);
begin
  Encrypt_OFB(Buf,Count);
end;

procedure TRijndael_OFB.Encrypt(var Buf; Count: Integer);
begin
  Encrypt_OFB(Buf,Count);
end;

class function TRijndael_OFB.KeyedIVOID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '1.3.6.1.4.1.13085.1.19';
    24: Result := '1.3.6.1.4.1.13085.1.20';
    32: Result := '1.3.6.1.4.1.13085.1.21';
  else
    Result := nil;
  end;
end;

class function TRijndael_OFB.Mode: TCipherMode;
begin
  Result := cmOFB;
end;

class function TRijndael_OFB.OID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '2.16.840.1.101.3.4.1.3';
    24: Result := '2.16.840.1.101.3.4.1.23';
    32: Result := '2.16.840.1.101.3.4.1.43';
  else
    Result := nil;
  end;
end;

{ TAES_Wrap }

class function TAES_Wrap.BlockSize: Integer;
begin
  Result := 8;
end;

class function TAES_Wrap.BlockVectorSize: Integer;
begin
  Result := 1;
end;

procedure TAES_Wrap.CleanUp;
begin
  inherited;
  IVector := #$A6#$A6#$A6#$A6#$A6#$A6#$A6#$A6;
end;

procedure XorIndex(var Buf; Index: Integer);
asm
  bswap EDX
  xor [EAX + 4],EDX
end;

procedure TAES_Wrap.Decrypt(var Buf; Count: Integer);
var
  Block: packed array [0..15] of Byte;
  I, J, N, T: Integer;
begin
  N := Count div 8;
  T := 6*n;
  for J := 5 downto 0 do
    for I := N - 1 downto 0 do begin
      Move(IV,Block,8);
      XorIndex(Block,T);
      Dec(T);
      Move(Ptr(LongInt(@Buf) + I*8)^,Block[8],8);
      DecryptBlock(Block);
      Move(Block[8],Ptr(LongInt(@Buf) + I*8)^,8);
      Move(Block,IV,8);
    end;
end;

procedure TAES_Wrap.Encrypt(var Buf; Count: Integer);
var
  Block: packed array [0..15] of Byte;
  I, J, N, T: Integer;
begin
  N := Count div 8;
  T := 1;
  for J := 0 to 5 do
    for I := 0 to N - 1 do begin
      Move(IV,Block,8);
      Move(Ptr(LongInt(@Buf) + I*8)^,Block[8],8);
      EncryptBlock(Block);                       
      Move(Block[8],Ptr(LongInt(@Buf) + I*8)^,8);
      XorIndex(Block,T);   
      Inc(T);
      Move(Block,IV,8);
    end;
end;

class function TAES_Wrap.KeyedIVOID(KeySize: Integer): PChar;
begin
  Result := nil;
end;

class function TAES_Wrap.Mode: TCipherMode;
begin
  Result := cmAESWrap;
end;

class function TAES_Wrap.OID(KeySize: Integer): PChar;
begin
  case KeySize of
    16: Result := '2.16.840.1.101.3.4.1.5';
    24: Result := '2.16.840.1.101.3.4.1.25';
    32: Result := '2.16.840.1.101.3.4.1.45';
  else
    Result := nil;
  end;
end;

{$IFDEF AES_BG}
const
  BrianGladmanCopyrightNotice =
    #13#10 +
    '/* This is an independent implementation of the encryption algorithm:   */'#13#10 +
    '/*                                                                      */'#13#10 +
    '/*         RIJNDAEL by Joan Daemen and Vincent Rijmen                   */'#13#10 +
    '/*                                                                      */'#13#10 +
    '/* which is a candidate algorithm in the Advanced Encryption Standard   */'#13#10 +
    '/* programme of the US National Institute of Standards and Technology.  */'#13#10 +
    '/*                                                                      */'#13#10 +
    '/* Copyright in this implementation is held by Dr B R Gladman but I     */'#13#10 +
    '/* hereby give permission for its free direct or derivative use subject */'#13#10 +
    '/* to acknowledgment of its origin and compliance with any conditions   */'#13#10 +
    '/* that the originators of the algorithm place on its exploitation.     */'#13#10 +
    '/*                                                                      */'#13#10 +
    '/* Dr Brian Gladman (gladman@seven77.demon.co.uk) 14th January 1999     */'#13#10;
var
  CompilerFooler: string;
{$ENDIF AES_BG}

{$IFDEF RIJNDAEL}
initialization                                 
{$IFDEF AES_BG}
  CompilerFooler := BrianGladmanCopyrightNotice;
{$ENDIF AES_BG}
  {$IFDEF ECB}       RegisterCipherClass( TRijndael_ECB );       {$ENDIF}
  {$IFDEF ABC}       RegisterCipherClass( TRijndael_ABC );       {$ENDIF}
  {$IFDEF CBC}       RegisterCipherClass( TRijndael_CBC );       {$ENDIF}
  {$IFDEF CFB}       RegisterCipherClass( TRijndael_CFB );       {$ENDIF}
  {$IFDEF CTR}       RegisterCipherClass( TRijndael_CTR );       {$ENDIF}
  {$IFDEF OFB}       RegisterCipherClass( TRijndael_OFB );       {$ENDIF}
  {$IFDEF PCFB}      RegisterCipherClass( TRijndael_PCFB );      {$ENDIF}
  {$IFDEF PIPEDPCFB} RegisterCipherClass( TRijndael_PipedPCFB ); {$ENDIF}
  {$IFDEF AESWRAP}   RegisterCipherClass( TAES_Wrap );           {$ENDIF}
finalization
{$IFDEF AES_BG}
  CompilerFooler := ''; // fixes memory leak
{$ENDIF AES_BG}
{$ENDIF}
end.
