{$Q-,R-}
{$I ver.inc}

{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     ARC4 Unit                                         }
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
unit SsArc4;

interface

uses
  SecUtils;

type
  TARC4 = class(TCipher)
  private
    FData: packed array [0..255] of Byte;
    FI: Byte;
    FJ: Byte;
  protected
    procedure CleanUp; override;
    function GetVector: OctetString; override;
    procedure SetVector(const Value: OctetString); override;
  public                         
    constructor CreateClone(ACipher: TCipher); override;
    class function BlockSize: Integer; override;
    class function Mode: TCipherMode; override;
    class function Algorithm: TCipherAlg; override;
    class function AlgorithmName: PChar; override;
    class function OID(KeySize: Integer): PChar; override;
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    procedure SetUp(const AKey; Count, VectorSize: Integer); override;
  end;

implementation

uses
  SysUtils;

{ TARC4 }

class function TARC4.Algorithm: TCipherAlg;
begin
  Result := caARC4;
end;

class function TARC4.AlgorithmName: PChar;
begin
  Result := 'ARC4';
end;

class function TARC4.BlockSize: Integer;
begin
  Result := 1;
end;

procedure TARC4.CleanUp;
begin
  ProtectClear(FData,256);
end;

constructor TARC4.CreateClone(ACipher: TCipher); 
begin      
  VirtualLock;                                       
  if ACipher is TARC4 then begin
    Move(TARC4(ACipher).FData,FData,256);
    FI := TARC4(ACipher).FI;
    FJ := TARC4(ACIpher).FJ;
  end else
    inherited;
end;

procedure TARC4.Decrypt(var Buf; Count: Integer);
{$IFDEF ARCFOUR}
asm
  push EBX
  push EDI
  push ESI

  mov EDI,EDX
  lea ESI,dword ptr [EAX].TARC4.FData
  movzx EBX,byte [EAX].TARC4.FI
  movzx EDX,byte [EAX].TARC4.FJ
  push EAX
@@Main:
  // i := (i + 1) and $FF
  inc BL
  // j := (j + FData[i]) and $FF
  mov AL,byte [ESI + EBX]
  add DL,AL
  // Swap(FData[i],FData[j])
  mov AH,byte [ESI + EDX]
  mov [ESI + EBX],AH
  mov [ESI + EDX],AL
  // p := c xor FData[(FData[i] + FData[j]) and $FF]
  add AL,AH
  and EAX,$FF
  movzx EAX,byte [ESI + EAX]
  xor byte [EDI],AL

  inc EDI
  loop @@Main

  pop EAX
  mov byte [EAX].TARC4.FI,BL
  mov byte [EAX].TARC4.FJ,DL

  pop ESI
  pop EDI
  pop EBX
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;

procedure TARC4.Encrypt(var Buf; Count: Integer);
{$IFDEF ARCFOUR}
asm
  push EBX
  push EDI
  push ESI

  mov EDI,EDX
  lea ESI,dword ptr [EAX].TARC4.FData
  movzx EBX,byte [EAX].TARC4.FI
  movzx EDX,byte [EAX].TARC4.FJ
  push EAX
@@Main:
  // i := (i + 1) and $FF
  inc BL
  // j := (j + FData[i]) and $FF
  mov AL,byte [ESI + EBX]
  add DL,AL
  // Swap(FData[i],FData[j])
  mov AH,byte [ESI + EDX]
  mov [ESI + EBX],AH
  mov [ESI + EDX],AL
  // c := p xor FData[(FData[i] + FData[j]) and $FF]
  add AL,AH
  and EAX,$FF
  movzx EAX,byte [ESI + EAX]
  xor byte [EDI],AL

  inc EDI
  loop @@Main

  pop EAX
  mov byte [EAX].TARC4.FI,BL
  mov byte [EAX].TARC4.FJ,DL

  pop ESI
  pop EDI
  pop EBX
{$ELSE}
begin
  Assert(False);
{$ENDIF}
end;

function TARC4.GetVector: OctetString;
begin
  SetLength(Result,256);
  Move(FData,Result[1],256);
end;

class function TARC4.Mode: TCipherMode;
begin
  Result := cmOFB;
end;

procedure TARC4.SetUp(const AKey; Count, VectorSize: Integer);
{$IFDEF DEMO}
begin
  Nag(Count);
{$ENDIF}
asm
  push EBX
  push EDI
  push ESI

  mov byte [EAX].TARC4.FI,0
  mov byte [EAX].TARC4.FJ,0

  mov EDX,AKey
  mov EBX,Count
  cmp EBX,256
  jb @@Cont
  xor EBX,EBX

@@Cont:
  lea EDI,dword ptr [EAX].TARC4.FData
  xor ECX,ECX
@@InitData:
  mov [EDI + ECX],CL
  inc ECX
  cmp ECX,256
  jnz @@InitData

  mov ESI,EDX
  xor EDX,EDX
  xor ECX,ECX
@@Permute:
  mov EAX,ECX
  test BL,BL
  jz @@DoPermute
  div BL
  movzx EAX,AH
@@DoPermute:
  movzx EAX,byte [ESI + EAX]
  add EDX,EAX
  movzx EAX,byte [EDI + ECX]
  add EDX,EAX
  and EDX,$FF
  mov AH,byte [EDI+ EDX]
  mov byte [EDI + ECX],AH
  mov byte [EDI + EDX],AL

  inc ECX
  cmp ECX,256
  jnz @@Permute

  pop ESI
  pop EDI
  pop EBX
{$IFDEF DEMO}
end;
{$ENDIF}
end;

class function TARC4.OID(KeySize: Integer): PChar;
begin
  Result := '1.2.840.113549.1.3.4';
end;

procedure TARC4.SetVector(const Value: OctetString);
begin
  if Length(Value) > 256 then
    Move(Value[1],FData,256)
  else if Length(Value) = 256 then
    Move(Value[1],FData,Length(Value))
  else
    raise Exception.Create('The vector for ARC4 must be a 256 byte permutation.');
  FI := 0;
  FJ := 0;
end;

{$IFDEF ARCFOUR}
initialization
  RegisterCipherClass(TARC4);
{$ENDIF}
end.
