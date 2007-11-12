{$B-,O+,Q-,R-}
{$I ver.inc}
{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPArith Unit                                      }
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

unit MpArith;

interface

uses
  MpArithTypes;

{*** MP procedure paramater conventions ***                                    }
{    1. Standard convention:                                                   }
{       Parameters are ordered from left to right in the same order as in the  }
{       corresponding arithmetic expression. The result is returned in the     }
{       first parameter. Example: MPSub(A,B) <=> A := A - B                    }
{    2. "To" convention:                                                       }
{       Parameters are ordered from left to right in the same order as in the  }
{       corresponding arithmetic expression. The result is returned in the     }
{       last parameter. The last parameter *must* be initialized by the calling}
{       procedure. The last parameter *must not* be identical to any of the    }
{       other parameters. The name of the procedure ends with the              }
{       character "2". Example: MPSub2(A,B,C) <=> C := A - B                   }
{    3. Exceptions from 1 and 2:                                               }
{       a) MPDiv(A,B,C) <=> C := A div B; A := A mod B                         }
{       b) MPExpMod(G,E,M,R) <=> R := (G**E) mod M            ("To" Convention)}
{       c) MPMontgomeryExpMod(G,E,M,R) <=> R := (G**E) mod M  ("To" Convention)}
{       d) MPPow2Mod(E,M,R) <=> R := (2**E) mod M             ("To" Convention)}
{       e) MPPow4Mod(E,M,R) <=> R := (4**E) mod M             ("To" Convention)}
{       f) MPBinaryGCD(A,B,AGCD) <=> AGCD := gcd(A,B)         ("To" Convention)}
{       g) MPInvMod(V,M,AInv) <=> AInv := (V**-1) mod M       ("To" Convention)}
{       h) MPShr: Standard convention except that the first parameter is       }
{          a value parameter and not a var parameter.                          }
{    4. Other exceptions:                                                      }
{       Confer the documentation or source code for instruction on how to call }
{       functions and procedures that do not correspond to simple arithmetic   }
{       expressions in any obvious way. Example: MPMontgomeryParams            }

const
  DefaultPoolSize = 1024;

type
  TPrimeAlgo = (paStandard,          // HAC 4.44 / P1363 A.15.6
                paStandardFast,
                paSafePrime,
                paSubGroup,
                paDSAPrime,          // HAC 4.56
                paGordonStrongPrime, // HAC 4.53
                paMaurerPrime,       // HAC 4.62
                paStrongPrime        // P1363 A.15.8
                );

  PPrimeParams = ^TPrimeParams;
  TPrimeParams = record
    PMin: PMPInteger;
    PUBound: PMPInteger;
    F: PMPInteger;
    Mod8: Byte;
  end;

  PStrongPrimeParams = ^TStrongPrimeParams;
  TStrongPrimeParams = record
    PMin: PMPInteger;
    PUBound: PMPInteger;
    F: PMPInteger;
    Mod8: Byte;
    LS, LT, LR: Cardinal;
  end;

  PSubGroupParams = ^TSubGroupParams;
  TSubGroupParams = record
    PMin: PMPInteger;
    PUBound: PMPInteger;
    c: PMPInteger;
    q: PMPInteger;
    l: Cardinal;
  end;

  PDSAParams = ^TDSAParams;
  TDSAParams = record
    Seed: string;
    q: PMPInteger;
    l: Cardinal;
  end;

  PMaurerParams = ^TMaurerParams;
  TMaurerParams = record
    q: PMPInteger;
  end;

{MPIntegers are allocated from a memory pool. The size in bytes of this pool
 equals PoolSize * PoolBlockSize * 4. Each MPInteger occupies an integer
 number of blocks from the pool.}
procedure SetMPPoolSize(BlockCount: Cardinal);
procedure SetMPPoolBlockSize(BlockSize: Cardinal);

procedure EnablePoolExceptions;
procedure DisablePoolExceptions;

procedure EnableMemoryCheck;
procedure DisableMemoryCheck;

procedure EnableClearOnDealloc;
procedure DisableClearOnDealloc;
                        
procedure LockMPPoolAllocations(ALock: Boolean);

{The default memory pool is not thread safe. If you are using MP arithmetics
 in a thread, the execute method should look something like:

   procedure TMyThread.Execute;
   begin
     SetThrdMPPoolSize(256);
     try
       ...
     finally
       SetThrdMPPoolSize(0);
     end;
   end;}
procedure SetThrdMPPoolSize(BlockCount: Cardinal; BlockSize: Cardinal = 0);

function MPCopy(Source: PMPInteger): PMPInteger;
procedure MPRealloc(var A: PMPInteger; NewSize: Cardinal; Protect: Boolean = True);
procedure MPDealloc(A: PMPInteger);

procedure MPCopy2(Source: PMPInteger; var Dest: PMPInteger);

procedure MPAdd(var A: PMPInteger; B: PMPInteger);
procedure MPAdd2(A, B: PMPInteger; var C: PMPInteger);

procedure MPInc(var A: PMPInteger);
procedure MPInc2(A: PMPInteger; var B: PMPInteger);

procedure MPSub(var A: PMPInteger; B: PMPInteger);
procedure MPSub2(A, B: PMPInteger; var C: PMPInteger);

procedure MPDec(var A: PMPInteger);
procedure MPDec2(A: PMPInteger; var B: PMPInteger);

procedure MPMul(var A: PMPInteger; B: PMPInteger);
procedure MPMul2(A, B: PMPInteger; var R: PMPInteger);

procedure MPDiv(var A: PMPInteger; M: PMPInteger; var Q: PMPInteger; AdjustSize: Boolean = True);

function MPDivByInt2(V: PMPInteger; M: LongWord; var Q: PMPInteger): LongWord;

procedure MPMod(var A: PMPInteger; M: PMPInteger);
procedure MPMod2(A, M: PMPInteger; var R: PMPInteger);

function MPModByInt(N: PMPInteger; P: LongWord): Longword;

procedure MPMulMod(var A: PMPInteger; B, M: PMPInteger);
procedure MPMulMod2(A, B, M: PMPInteger; var R: PMPInteger);

procedure MPMulByInt(var X: PMPInteger; Y: LongInt); 
procedure MPMulByInt2(X: PMPInteger; Y: LongInt; var R: PMPInteger);

procedure MPExpMod(G, E, M: PMPInteger; var R: PMPInteger);
procedure MPPow2Mod(E, M: PMPInteger; var R: PMPInteger);
procedure MPPow4Mod(E, M: PMPInteger; var R: PMPInteger);  
procedure MPPowBMod(Base: LongWord; E, M: PMPInteger; var R: PMPInteger);
procedure MPMontgomeryExpMod(G, E, M: PMPInteger; var R: PMPInteger);

procedure MPMontgomeryParams(M: PMPInteger; var R, RQuad: PMPInteger; var MPrime: LongWord);
procedure MPMontgomeryMul(X,Y,M: PMPInteger; MPrime: LongWord; var A: PMPInteger);

procedure MPGarnerCRT(var X: PMPInteger; const M, V: array of PMPInteger);

procedure MPShl(var A: PMPInteger; boffs: Integer);
procedure MPShr(A: PMPInteger; boffs: Integer);

procedure MPShl2(A: PMPInteger; boffs: Integer; var R: PMPInteger);

procedure MPMask(var A: PMPInteger; MaskBits: Integer);

procedure MPBinaryGCD(X, Y: PMPInteger; var AGCD: PMPInteger);  
procedure MPBinaryInvMod(Y, M: PMPInteger; var AInv: PMPInteger);
procedure MPInvMod(V, M: PMPInteger; var AInv: PMPInteger);

procedure MPDivMod(var A: PMPInteger; B, M: PMPInteger);
procedure MPDivMod2(A, B, M: PMPInteger; var Q: PMPInteger);

function MPLegendreSymbol(X, P: PMPInteger): Integer;
function MPJacobiSymbol(A, N: PMPInteger): Integer;

function MPSqrtModPrime2(G, P: PMPInteger; var Z: PMPInteger): Boolean;

function MPMSB(A: PMPInteger): Integer;
function MPCmpOffset(A, B: PMPInteger; Offset: Cardinal = 0): Integer;

function MPMillerRabin(N: PMPInteger; T: Integer): Boolean;

procedure MPRandom(var X: PMPInteger; HighBit: Cardinal);   
procedure MPRandomBound(var X: PMPInteger; XMin, XUBound: PMPInteger);
procedure MPPrime(var P: PMPInteger; BitSize: Cardinal; Algo: TPrimeAlgo; Params: Pointer = nil);

function IntToMPInt(Value: Integer): PMPInteger;         
function IntegersToMPInt(const Value: array of LongWord;
                         Negative: Boolean = False): PMPInteger;

function Base16ToMPInt(var A: PMPInteger; const HexStr: string): Boolean;
function MPIntToBase16(A: PMPInteger): string;

procedure Base256ToMPInt(var A: PMPInteger; const Str: string); overload;
procedure Base256ToMPInt(var A: PMPInteger; const Str: string; MaxBitSize: Cardinal); overload;
procedure Base256ToMPInt(var A: PMPInteger; const Buf; Count: Cardinal; MaxBitSize: Cardinal); overload;
function MPIntToBase256(A: PMPInteger): string; overload;
function MPIntToBase256(A: PMPInteger; FixedOutputLen: Cardinal): string; overload;
procedure MPIntToBase256(A: PMPInteger; var Buf; Count: Cardinal); overload;

procedure Base256ToUMPInt(var A: PMPInteger; const Buf; Count: Cardinal; MaxBitSize: Cardinal);
procedure UMPIntToBase256(A: PMPInteger; var Buf; Count: Cardinal);

function BaseBToMPInt(var A: PMPInteger; B: Cardinal; const Digits, Str: string): Boolean;
function MPIntToBaseB(A: PMPInteger; B: Cardinal; const Digits: string): string;

function Base10ToMPInt(var A: PMPInteger; const Str: string): Boolean;
function MPIntToBase10(A: PMPInteger): string;
           
function ByteSwap(Value: LongWord): Longword; overload;
function ByteSwap(Value: Int64): Int64; overload;

implementation

uses
  SysUtils, SmallPrimes, SecUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  Classes, MpYarrow, Math, SyncObjs, MpKaratsuba;

function MPCopy(Source: PMPInteger): PMPInteger;
begin
  MPRealloc(Source,Source^.Size,False);
  Result := Source;
end;

procedure MPCopy2(Source: PMPInteger; var Dest: PMPInteger);
begin
  MPRealloc(Source,Source^.Size,False);
  MPDealloc(Dest);
  Dest := Source;
end;

type
  TSet32 = set of 0..31;
  TPoolMap = array [0..MaxListSize-1] of TSet32;
  PPoolMap = ^TPoolMap;
  TExtPoolMap = array [0..MaxListSize-1] of LongWord;
  PExtPoolMap = ^TExtPoolMap;

  TMPPool = class
  private
    FData: Pointer;
    FPool: PChar;
    FPoolMap: PPoolMap;
    FPoolSize: Cardinal;
    FPoolBlockSize: Cardinal;
    FPoolBlockSizeSize: Byte;
    FFirstAvail: Cardinal;
    FPoolExceptions: Boolean;
    FPoolMemoryCheck: Boolean;
    FExtPoolMap: PExtPoolMap;
    FCreationThreadID: LongWord;
    FLock: TCriticalSection;
    // Synchronized parameters and results:
    procedure DoCache(X: PMPInteger);
    procedure DoIsUsed(var AIsUsed: Boolean);
    procedure DoObtain(Size: Cardinal; var X: PMPInteger);
    function CheckThreadID: Boolean; virtual;
    function IsUsed: Boolean;
    procedure Cache(X: PMPInteger);
    function InternalCheck(X: PMPInteger; Remove: Boolean): Boolean;
    function Obtain(Size: Cardinal): PMPInteger;
    constructor Create(APoolSize, APoolBlockSize: Cardinal);
    destructor Destroy; override;
  protected                        
    procedure Check(X: PMPInteger);
  end;

  TMPThreadPool = class(TMPPool)
  private
    function CheckThreadID: Boolean; override;
  protected
    constructor Create(APoolSize, APoolBlockSize: Cardinal);
    destructor Destroy; override;
  end;
var
  Pool: TMPPool;
  PoolSize: Cardinal = DefaultPoolSize;
  PoolBlockSize: Cardinal = 32;
  PoolExceptions: Boolean = True;
  PoolMemoryCheck: Boolean = True;
  ClearOnDealloc: Boolean = True;
  ThreadLockCount: Integer = 0;
threadvar
  PoolLocked: Boolean;

procedure LockMPPoolAllocations(ALock: Boolean);
begin
  if ALock then
    InterlockedIncrement(ThreadLockCount)
  else
    InterlockedDecrement(ThreadLockCount);
end;

procedure SetMPPoolSize(BlockCount: Cardinal);
begin
  if Assigned(Pool) and Pool.IsUsed then
    raise Exception.Create('Cannot change pool size while the pool is in use.')
  else begin
    PoolSize := BlockCount;
    if PoolSize = 0 then
      PoolBlockSize := 1;
    Pool.Free;
    {$IFDEF INIT_SECTIONS}
    Pool := TMPPool.Create(PoolSize,PoolBlockSize);
    {$ELSE}
    if PoolSize = 0 then
      Pool := nil
    else
      Pool := TMPPool.Create(PoolSize,PoolBlockSize);
    {$ENDIF}
  end;
end; 

threadvar
  ThrdLocalPool: TMPPool;
var
  ThrdLocalPoolCount: Integer = 0;

procedure SetMPPoolBlockSize(BlockSize: Cardinal);
var
  I: Cardinal;
begin
  if Pool.IsUsed or (ThrdLocalPoolCount > 0) then
    raise Exception.Create('Cannot change block size while the pool is in use.')
  else begin
    I := BlockSize;
    while (I > 0) and not Odd(I) do
      I := I shr 1;
    if I <> 1 then
      raise Exception.Create('Block size must be a power of 2.');
    PoolBlockSize := BlockSize;
    Pool.Free;
    {$IFDEF INIT_SECTIONS}
    Pool := TMPPool.Create(PoolSize,PoolBlockSize);
    {$ELSE}
    if PoolSize = 0 then
      Pool := nil
    else
      Pool := TMPPool.Create(PoolSize,PoolBlockSize);
    {$ENDIF}
  end;
end;
    
procedure EnablePoolExceptions;
begin
  PoolExceptions := True;
end;

procedure DisablePoolExceptions;
begin
  PoolExceptions := False;
end;
      
procedure EnableMemoryCheck;
begin
  PoolMemoryCheck := True;
end;

procedure DisableMemoryCheck;
begin
  PoolMemoryCheck := False;
end;

procedure EnableClearOnDealloc;
begin
  ClearOnDealloc := True;
end;

procedure DisableClearOnDealloc;
begin
  ClearOnDealloc := False;
end;

procedure SetThrdMPPoolSize(BlockCount: Cardinal; BlockSize: Cardinal = 0);
var
  vPool: TMPPool;
begin
  if Assigned(ThrdLocalPool) and ThrdLocalPool.IsUsed and (BlockCount > 0) then
    raise Exception.Create('Cannot change pool size while the pool is in use.')
  else begin
    if BlockSize = 0 then
      BlockSize := PoolBlockSize
    else if BlockSize <> PoolBlockSize then
      raise Exception.Create('Cannot change pool block size');
    vPool := ThrdLocalPool;
    ThrdLocalPool := nil;
    vPool.Free;
    if BlockCount > 0 then begin
      LockMPPoolAllocations(True);    
      InterlockedIncrement(ThrdLocalPoolCount);
      ThrdLocalPool := TMPPool.Create(BlockCount,BlockSize)
    end else begin
      ThrdLocalPool := nil;                    
      InterlockedDecrement(ThrdLocalPoolCount);
      LockMPPoolAllocations(False);
    end;
  end;
end;

{ TMPPool }

procedure TMPPool.Cache(X: PMPInteger);
begin
  if Assigned(X) then begin
    if not CheckThreadID then begin
      FLock.Acquire;
      try
        DoCache(X);
      finally
        FLock.Release;
      end;
    end else begin
      DoCache(X);
    end;
  end;
end;

procedure TMPPool.Check(X: PMPInteger);
begin                               
  if FPoolExceptions then
    if not InternalCheck(X,False) then
      raise Exception.Create('TMPPool.Check: Invalid pointer');
end;

function TMPPool.CheckThreadID: Boolean;
var
  CTID: LongWord;
begin
  Result := ThreadLockCount = 0;
  if Result then begin
    CTID := GetCurrentThreadID;
    Result := CTID = FCreationThreadID;
    if (FCreationThreadID <> MainThreadID) and not Result then
      raise Exception.Create('TMPPool.CheckThreadID: Called from the wrong thread.');
  end;
end;

constructor TMPPool.Create;
begin
  FCreationThreadID := GetCurrentThreadID;
//  if FCreationThreadID = MainThreadID then
    FLock := TCriticalSection.Create;
  GetMem(FData,(APoolSize + 1) * APoolBlockSize * 4 - 1);
  FPool := FData;
  while LongWord(FPool) mod (APoolBlockSize * 4) > 0 do
    Inc(LongWord(FPool));
  {$IFDEF MSWINDOWS}
  PoolLocked := VirtualLock(FPool,APoolSize * APoolBlockSize * 4);
  {$ENDIF}
  GetMem(FPoolMap,(APoolSize div 32)*4 + 4);
  FillChar(FPoolMap^,(APoolSize div 32)*4,0);
  FPoolMemoryCheck := PoolMemoryCheck;
  if FPoolMemoryCheck then begin
    GetMem(FExtPoolMap,APoolSize*4 + 4);
    FillChar(FExtPoolMap^,APoolSize*4,0);
  end;
  FPoolSize := APoolSize;
  FPoolBlockSize := APoolBlockSize;
  while APoolBlockSize > 1 do begin
    Inc(FPoolBlockSizeSize);
    APoolBlockSize := APoolBlockSize shr 1;
  end;
  FPoolExceptions := PoolExceptions;
  inherited Create;
end;

destructor TMPPool.Destroy;
begin
  ProtectClear(FPool^,FPoolSize * FPoolBlockSize * 4);
  {$IFDEF MSWINDOWS}
  VirtualUnlock(FPool,FPoolSize * FPoolBlockSize * 4);
  {$ENDIF}
  FreeMem(FData);
  FData := nil;
  // This will detect stray PMPIntegers.
  try
    Assert(not IsUsed,'There are still allocated PMPIntegers in the pool.');
  finally
    FLock.Free;
    FreeMem(FPoolMap);
    FreeMem(FExtPoolMap);
    FPoolSize := 0;
    FPoolBlockSize := 0;
    FPool := nil;
  end;
end;

procedure TMPPool.DoCache(X: PMPInteger);
var
  BlockCount, I, J, C: Cardinal;
  Map: TSet32;
begin
  if Assigned(X) then begin
    if InternalCheck(X,True) then begin
      I := (LongInt(X) - LongInt(FPool)) shr (2 + FPoolBlockSizeSize);
      BlockCount := (X^.Size + 2 + FPoolBlockSize-1) shr FPoolBlockSizeSize;
      if BlockCount <= 0 then Exit;
      if ClearOnDealloc then
        ProtectClear(X^,BlockCount * FPoolBlockSize * 4);
      Map := FPoolMap^[I shr 5];
      if BlockCount + (I and 31) <= 32 then
        FPoolMap^[I shr 5] := FPoolMap^[I shr 5] -
                              [I and 31..(I and 31) + BlockCount - 1]
      else begin
        J := I shr 5;
        FPoolMap^[J] := FPoolMap^[J] - [I and 31..31];
        C := BlockCount + (I and 31) - 32;
        Inc(J);
        while C > 32 do begin
          FPoolMap^[J] := [];
          Dec(C,32);
          Inc(J);
        end;
        if C > 0 then
          FPoolMap^[J] := FPoolMap^[J] - [0..C-1];
      end;
      if I < FFirstAvail then FFirstAvail := I;
    end else if PoolExceptions and FPoolExceptions then
      raise Exception.Create('TMPPool.Cache: Invalid pointer')
    else begin
      I := (X^.Size + 2) * 4;
      if ClearOnDealloc then
        ProtectClear(X^,I);
      FreeMem(X);
    end;
  end;
end;

procedure TMPPool.DoIsUsed;
var
  I: Integer;
  Map: TSet32;
begin
  AIsUsed := False;
  for I := 0 to (FPoolSize div 32) - 1 do begin
    Map := FPoolMap^[I];
    AIsUsed := Map <> [];
    if AIsUsed then Break;
  end;
end;

procedure TMPPool.DoObtain;
var
  Result: PMPInteger;
  I, J, C: Cardinal;
  BlockCount: Cardinal;
  Map: TSet32;
begin
  BlockCount := (Size + 2 + FPoolBlockSize-1) shr FPoolBlockSizeSize;
  I := FFirstAvail;
  if FPoolSize > I then
    Map := FPoolMap^[I shr 5];
  while I + BlockCount < FPoolSize do begin
    if (I and 31) in Map then begin
      Inc(I);
      if I and 31 = 0 then begin
        if I >= FPoolSize then Break;
        Map := FPoolMap^[I shr 5];
      end;
    end else begin
      J := I + 1;
      if J and 31 = 0 then
        Map := FPoolMap^[J shr 5];
      while (J - I < BlockCount) and (J < FPoolSize) and
            not ((J and 31) in Map) do begin
        Inc(J);
        if J and 31 = 0 then
          Map := FPoolMap^[J shr 5];
      end;
      if J - I = BlockCount then begin
        Result := Pointer(FPool + I * FPoolBlockSize * 4);
        if FPoolMemoryCheck then
          FExtPoolMap^[I] := BlockCount;
        if BlockCount + (I and 31) <= 32 then
          FPoolMap^[I shr 5] := FPoolMap^[I shr 5] +
                                [I and 31..(I and 31) + BlockCount - 1]
        else begin
          J := I shr 5;
          FPoolMap^[J] := FPoolMap^[J] + [I and 31..31];
          C := BlockCount + (I and 31) - 32;
          Inc(J);
          while C > 32 do begin
            FPoolMap^[J] := [0..31];
            Dec(C,32);
            Inc(J);
          end;
          if C > 0 then
            FPoolMap^[J] := FPoolMap^[J] + [0..C-1];
        end;
        if I = FFirstAvail then begin
          I := I + BlockCount;
          if I >= FPoolSize then
            FFirstAvail := 0
          else begin
            Map := FPoolMap^[I shr 5];
            while (I < FPoolSize) and (I and 31 in Map) do begin
              Inc(I);
              if I and 31 = 0 then begin
                if I >= FPoolSize then Break;
                Map := FPoolMap^[I shr 5];
              end;
            end;
            FFirstAvail := I;
          end;
        end;
        X := Result;
        Exit;
      end else begin
        I := J + 1;
        if I < FPoolSize then
          Map := FPoolMap^[I shr 5]
        else
          Break;
      end;
    end;
  end;
  if PoolExceptions and FPoolExceptions then
    raise Exception.Create('TMPPool.Obtain: Out of memory')
  else
    GetMem(Result,BlockCount * FPoolBlockSize * 4);
  X := Result;
end;

function TMPPool.InternalCheck(X: PMPInteger; Remove: Boolean): Boolean;
var
  Idx: Cardinal;
  BlockCount: Cardinal;

  function DivMod(A, B: LongWord): Longword;
  asm
    mov ECX,EDX
    xor EDX,EDX
    div ECX
    test EDX,EDX
    jz @@Exit
    mov EAX,$FFFFFFFF
  @@Exit:
  end;

begin
  Idx := LongInt(X) - LongInt(FPool);
  Result := (Idx <= Cardinal(MaxInt)) and
            (Idx < FPoolSize * FPoolBlockSize * 4);
  if Result and FPoolMemoryCheck then begin
    if (Idx and $3) = 0 then begin
      Idx := Idx shr 2;
      Idx := DivMod(Idx,FPoolBlockSize);
      if Idx < $FFFFFFFF then begin
        BlockCount := (X^.Size + 2 + FPoolBlockSize-1) shr FPoolBlockSizeSize;
        if BlockCount <> FExtPoolMap^[Idx] then
          raise Exception.Create('TMPPool.InternalCheck: Invalid pointer');
        if Remove then
          FExtPoolMap[Idx] := 0;
      end else
        raise Exception.Create('TMPPool.InternalCheck: Invalid pointer');
    end else
      raise Exception.Create('TMPPool.InternalCheck: Invalid pointer');
  end;
end;

function TMPPool.IsUsed: Boolean;
begin
  if not CheckThreadID then begin
    FLock.Acquire;
    try
      DoIsUsed(Result);
    finally
      FLock.Release;
    end;
  end else begin
    DoIsUsed(Result);
  end;
end;

function TMPPool.Obtain(Size: Cardinal): PMPInteger;
begin
  if not CheckThreadID then begin
    FLock.Acquire;
    try
      Result := nil;
      DoObtain(Size, Result);
    finally
      FLock.Release;
    end;
  end else begin
    Result := nil;
    DoObtain(Size, Result);
  end;
end;   

{ TMPThreadPool }

function TMPThreadPool.CheckThreadID: Boolean;
begin
  Result := True;
end;

constructor TMPThreadPool.Create(APoolSize, APoolBlockSize: Cardinal);
begin
  inherited;
end;

destructor TMPThreadPool.Destroy;
begin
  InterlockedDecrement(ThrdLocalPoolCount);
  inherited;
end;

procedure ClearLong(Mem: Pointer; LongCount: Integer);
asm
  push EDI
  mov EDI,EAX
  mov ECX,EDX
  xor EAX,EAX
  cld
  rep stosd
  pop EDI
end;

procedure MoveLongDown(Src, Dst: Pointer; LongCount: Integer);
asm
  push EDI
  push ESI
  mov ESI,EAX
  mov EDI,EDX
  cld
  rep movsd
  pop ESI
  pop EDI
end;
          
procedure MoveLongUp(Src, Dst: Pointer; LongCount: Integer);
asm
  push EDI
  push ESI
  lea ESI,[EAX + ECX*4 - 4]
  lea EDI,[EDX + ECX*4 - 4]
  std
  rep movsd
  cld
  pop ESI
  pop EDI
end;

procedure MPRealloc(var A: PMPInteger; NewSize: Cardinal; Protect: Boolean = True);
var
  OldSize, OldBlockSize, NewBlockSize: Cardinal;
  I: Cardinal;
  T: PMPInteger;
begin
  if Assigned(A) and Protect then begin
    OldSize := A^.Size;
    if OldSize = NewSize then Exit;
    if PoolBlockSize > 1 then begin
      NewBlockSize := (NewSize + 2 + PoolBlockSize-1) div PoolBlockSize;
      OldBlockSize := (OldSize + 2 + PoolBlockSize-1) div PoolBlockSize;
      if NewBlockSize = OldBlockSize then begin
        if NewSize > OldSize then begin
          if OldSize > 0 then
            {$IFDEF NOMOVEFILL}
            for I := 1 to OldSize do
              A^.Data[NewSize - I] := A^.Data[OldSize - I];
            {$ELSE}
            MoveLongUp(@A^.Data[0],@A^.Data[NewSize-OldSize],OldSize);
            {$ENDIF}
          {$IFDEF NOMOVEFILL}
          for I := 0 to NewSize - OldSize - 1 do
            A^.Data[I] := 0;
          {$ELSE}
          ClearLong(@A^.Data,NewSize - OldSize);
          {$ENDIF}
        end else if NewSize > 0 then begin
          {$IFDEF NOMOVEFILL}
          for I := 0 to NewSize - 1 do
            A^.Data[I] := A^.Data[I + OldSize - NewSize];
          {$ELSE}
          MoveLongDown(@A^.Data[OldSize-NewSize],@A^.Data[0],NewSize);
          {$ENDIF}
        end;
        A^.Size := NewSize;
        Exit;
      end;
    end;
  end;
  if (ThrdLocalPoolCount > 0) and Assigned(ThrdLocalPool) then
    T := ThrdLocalPool.Obtain(NewSize)
  else
    T := Pool.Obtain(NewSize);
  T^.Size := NewSize;
  if Assigned(A) then begin
    OldSize := A^.Size;
    T^.Sign := A^.Sign;
    if OldSize < NewSize then begin
      if Protect then
        {$IFDEF NOMOVEFILL}
        for I := 1 to OldSize do begin
          T^.Data[NewSize - I] := A^.Data[OldSize - I];
          A^.Data[OldSize - I] := 0;
        end
        {$ELSE}
        begin
        MoveLongDown(@A^.Data[0],@T^.Data[NewSize-OldSize],OldSize);
        ClearLong(@A^.Data[0],OldSize);
        end
        {$ENDIF}
      else
        {$IFDEF NOMOVEFILL}
        for I := 1 to OldSize do
          T^.Data[NewSize - I] := A^.Data[OldSize - I];
        {$ELSE}
        MoveLongDown(@A^.Data[0],@T^.Data[NewSize-OldSize],OldSize);
        {$ENDIF}
      {$IFDEF NOMOVEFILL}
      for I := OldSize + 1 to NewSize do
        T^.Data[NewSize - I] := 0;
      {$ELSE}
      ClearLong(@T^.Data[0],NewSize - OldSize);
      {$ENDIF}
    end else begin
      {$IFDEF NOMOVEFILL}
      if Protect then begin
        for I := 1 to NewSize do begin
          T^.Data[NewSize - I] := A^.Data[OldSize - I];
          A^.Data[OldSize - I] := 0;
        end;
        if (OldSize > NewSize) then
          for I := NewSize + 1 to OldSize do
            A^.Data[OldSize - I] := 0;
      end else
        for I := 1 to NewSize do
          T^.Data[NewSize - I] := A^.Data[OldSize - I];
      {$ELSE}
      MoveLongDown(@A^.Data[OldSize - NewSize],@T^.Data[0],NewSize);
      if Protect then
        ClearLong(@A^.Data,OldSize);
      {$ENDIF}
    end;
    if Protect then begin
      if (ThrdLocalPoolCount > 0) and Assigned(ThrdLocalPool) then
        ThrdLocalPool.Cache(A)
      else
        Pool.Cache(A);
    end;
  end else begin
    T^.Sign := 0;
    if NewSize > 0 then
      for I := 1 to NewSize do
        T^.Data[NewSize - I] := 0;
  end;
  A := T;
end;

procedure MPMask(var A: PMPInteger; MaskBits: Integer);
var
  Mask: LongWord;
begin
  MPRealloc(A,(MaskBits + 31) shr 5);
  MaskBits := MaskBits and $1F;
  if MaskBits > 0 then begin
    Mask := (1 shl MaskBits) - 1;
    A^.Data[0] := A^.Data[0] and Mask;
  end;
end;

procedure MPDealloc(A: PMPInteger);
begin
  if (ThrdLocalPoolCount > 0) and Assigned(ThrdLocalPool) then
    ThrdLocalPool.Cache(A)
  else
    Pool.Cache(A);
end;

procedure MPAdd(var A: PMPInteger; B: PMPInteger);
var
  I, Size, ASize: Integer;
  C: LongWord;
  AGreatest, NewB: Boolean;
begin
  {A <- A + B}
  if A = B then begin
    if A^.Data[0] and $80000000 > 0 then
      MPRealloc(A,A^.Size + 1);
    Size := A^.Size;
    asm
      mov EAX,[A]
      mov EAX,[EAX]
      mov ECX,Size
      clc
    @@1:
      mov EDX,[EAX + ECX*4 + 4].dword
      adc [EAX + ECX*4 + 4].dword,EDX
      loop @@1
    end;
  end else begin
    if B^.Sign = 0 then Exit;
    NewB := False;
    Size := B^.Size;
    ASize := A^.Size;
    if ASize < Size then begin
      MPRealloc(A,Size);
      ASize := Size;
    end else if ASize > Size then begin
      MPRealloc(B,ASize,False);
      NewB := True;
      Size := ASize;
    end;
    I := 0;
    while (A^.Data[I] = B^.Data[I]) and (I < Size) do
      Inc(I);
    if (I < Size) or (A^.Sign = B^.Sign) then begin
      if I = Size then
        AGreatest := True
      else
        AGreatest := A^.Data[I] > B^.Data[I];
      if A^.Sign = B^.Sign then begin
        C := 0;
        asm
          push EBX
          mov EBX,[B]
          mov EAX,[A]
          mov EAX,[EAX]
          mov ECX,Size
          clc
        @@1:
          mov EDX,[EBX + ECX*4 + 4].dword
          adc [EAX + ECX*4 + 4].dword,EDX
          loop @@1
          adc C,0
          pop EBX
        end;
        if C = 1 then begin
          MPRealloc(A,ASize + 1);
          A^.Data[0] := 1;
        end;
      end else if AGreatest then begin
        asm
          push EBX
          mov EBX,[B]
          mov EAX,[A]
          mov EAX,[EAX]
          mov ECX,Size
          clc
        @@2:
          mov EDX,[EBX + ECX*4 + 4].dword
          sbb [EAX + ECX*4 + 4].dword,EDX
          loop @@2
          pop EBX
        end;
      end else begin
        asm
          push EBX
          mov EBX,[B]
          mov EAX,[A]
          mov EAX,[EAX]
          mov ECX,Size
          clc
        @@3:
          mov EDX,[EBX + ECX*4 + 4].dword
          sbb EDX,[EAX + ECX*4 + 4].dword
          mov [EAX + ECX*4 + 4].dword,EDX
          loop @@3
          pop EBX
        end;
        A^.Sign := B^.Sign;
      end;
    end else begin
      FillChar(A^.Data,A^.Size*4,0);
      A^.Sign := 0;
    end;
    if NewB then MPDealloc(B);
  end;
end;

procedure MPAdd2(A, B: PMPInteger; var C: PMPInteger);
begin
  if (C = A) and (C <> B) then
    MPAdd(C,B)
  else if A = B then begin
    if C = A then begin
      C := MPCopy(A);
      MPAdd(C,C);
      MPDealloc(A);
    end else begin
      MPDealloc(C);
      C := MPCopy(A);
      MPAdd(C,C);
    end;
  end else if C = B then begin
    C := MPCopy(A);
    MPAdd(C,B);
    MPDealloc(B);
  end else begin
    MPDealloc(C);
    C := MPCopy(A);
    MPAdd(C,B);
  end;
end;

procedure MPAddInt(A: PMPInteger; B: LongWord);
asm
  push EBX
  mov ECX,[EAX]
  dec ECX
  xor EBX,EBX
  add [EAX + ECX*4 + 8],EDX
  adc EBX,0
  test EBX,EBX
  jz @@1
  test ECX,ECX
  jz @@1
  stc
@@MainAddInt:
  adc dword [EAX + ECX*4 + 4],0
  loop @@MainAddInt
@@1:
  mov ECX,[EAX+4]
  test ECX,ECX
  jnz @@2
  mov dword [EAX+4],1
@@2:
  pop EBX
end;

procedure MPSubInt(A: PMPInteger; B: LongWord);
asm
  mov ECX,[EAX]
  dec ECX
  sub dword [EAX + ECX*4 + 8],EDX
  jnc @@1
  test ECX,ECX
  jz @@1
  stc
@@MainAddInt:
  sbb dword [EAX + ECX*4 + 4],0
  loop @@MainAddInt
@@1:
  mov ECX,[EAX+4]
  test ECX,ECX
  jnz @@2
  mov dword [EAX+4],-1
@@2:
end;

procedure MPInc(var A: PMPInteger);
begin
  if A.Sign > 0 then
    MPAddInt(A,1)
  else if A.Sign < 0 then begin
    if MPMSB(A) = 1 then begin
      MPDealloc(A);
      A := IntToMPInt(0);
    end else
      MPSubInt(A,1)
  end else begin
    MPDealloc(A);
    A := IntToMPInt(1);
  end;
end;  

procedure MPInc2(A: PMPInteger; var B: PMPInteger);
begin
  MPCopy2(A,B);
  MPInc(B);
end;

procedure MPDec(var A: PMPInteger);
begin
  if A.Sign < 0 then
    MPAddInt(A,1)
  else if A.Sign > 0 then begin
    if MPMSB(A) = 1 then begin
      MPDealloc(A);
      A := IntToMPInt(0);
    end else
      MPSubInt(A,1)
  end else begin
    MPDealloc(A);
    A := IntToMPInt(-1);
  end;
end;

procedure MPDec2(A: PMPInteger; var B: PMPInteger);
begin
  MPCopy2(A,B);
  MPDec(B);
end;

procedure MPSub(var A: PMPInteger; B: PMPInteger);
begin
  {A <- A - B}
  B^.Sign := -B^.Sign;
  MPAdd(A,B);
  B^.Sign := -B^.Sign;
end;

procedure MPSub2(A, B: PMPInteger; var C: PMPInteger);
var
  R: PMPInteger;
begin
  {C <- A - B}
  if A = C then
    MPSub(C,B)
  else if B = C then begin
    R := MPCopy(A);     
    B^.Sign := -B^.Sign;
    MPAdd(R,B);
    B^.Sign := -B^.Sign;
    MPDealloc(C);
    C := R;
  end else begin
    MPCopy2(A,C);
    B^.Sign := -B^.Sign;
    MPAdd(C,B);
    B^.Sign := -B^.Sign;
  end;
end;

procedure RawSqr(A, C: PMPInteger);
asm
  push EBX
  push EDI
  push ESI
  push EBP

  mov ESI,EAX

  push EDX

  mov EAX,[ESI]
  push EAX

@@Outer:
  mov EDI,dword [ESP + 4]
  mov ECX,dword [ESP]
  mov EBP,dword [ESI + ECX*4 + 4]
  lea EDI,[EDI + ECX*8 + 4]
  xor EBX,EBX

  mov EAX,EBP
  mul EBP
  add dword [EDI],EAX
  adc dword [EDI - 4],EDX
  adc EBX,0
  sub EDI,4

  dec ECX
  jz @@OuterDone

@@Inner:
  shl EBX,8
  mov EAX,dword [ESI + ECX*4 + 4]
  mul EBP
  add EAX,EAX
  adc EDX,EDX
  adc EBX,0
  add dword [EDI],EAX
  adc EDX,dword [EDI - 4] 
  adc EBX,0
  movzx EAX,BH
  add EDX,EAX
  adc EBX,0
  mov dword [EDI - 4],EDX
  sub EDI,4
  loop @@Inner

  dec dword [ESP]
  jz @@OuterDone
  
  movzx EAX,BL
  mov dword [EDI - 4],EAX

  jmp @@Outer
@@OuterDone:

  add ESP,8

  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

procedure RawMul(A: PMPInteger; B: PMPInteger; C: PMPInteger);
asm
  push EBX
  push EDI
  push ESI
  push EBP

  mov EBP,EAX
  mov ESI,EDX

  push ECX

  mov EAX,[EBP]
  push EAX
  lea EBP,[EBP + EAX*4 + 4]
  push EBP
@@Outer:
  mov EDI,dword [ESP + 8]
  mov EAX,dword [ESP + 4]
  mov EBP,dword [ESP]
  mov EBP,dword [EBP]
  mov ECX,[ESI]
  add EAX,ECX
  lea EDI,[EDI + EAX*4 + 4]
  xor EBX,EBX
@@Inner:
  mov EAX,[ESI + ECX*4 + 4]
  mul EBP
  add [EDI],EAX
  adc EDX,[EDI - 4]
  mov EAX,0
  adc EAX,0
  add EDX,EBX
  adc EAX,0
  mov [EDI - 4],EDX
  mov EBX,EAX
  sub EDI,4
  loop @@Inner

  sub dword [ESP],4
  sub dword [ESP + 4],1
  jnz @@Outer

  add ESP,12

  pop EBP
  pop ESI
  pop EDI
  pop EBX
end;

procedure RawMulByInt(X: PMPInteger; Y: LongWord; A: PMPInteger);
asm
  push EBX
  push EDI
  push ESI

  mov ESI,EAX
  mov EDI,ECX
  mov EBX,EDX
  mov ECX,[ESI].dword
  mov dword [EDI + ECX*4 + 8],0
  xor EAX,EAX
@@MainRawMulByInt:
  mov [EDI + ECX*4 + 4].dword,EAX
  mov EAX,[ESI + ECX*4 + 4].dword
  mul EBX
  add [EDI + ECX*4 + 8].dword,EAX
  adc [EDI + ECX*4 + 4].dword,EDX
  mov EAX,0
  adc EAX,0
  loop @@MainRawMulByInt

  pop ESI
  pop EDI
  pop EBX
end;        

procedure RawAdd(A, B: Pointer; Count: Integer);
asm
  push EBX
  clc
@@MainRawAdd:
  mov EBX,[EDX + ECX*4 - 4]
  adc [EAX + ECX*4 - 4],EBX
  loop @@MainRawAdd
  adc dword [EAX - 4],0
  pop EBX
end;

procedure MPMul(var A: PMPInteger; B: PMPInteger);
var
  C: PMPInteger;
begin
  {A <- A * B}
  if A.Sign*B.Sign = 0 then begin
    MPRealloc(A,1);
    A^.Sign := 0;
    A^.Data[0] := 0;
  end else if A^.Size = 1 then begin
    C := nil;
    MPRealloc(C,B^.Size + 1);
    RawMulByInt(B,A^.Data[0],C);
    C^.Sign := A^.Sign * B^.Sign;
    if C^.Sign = 0 then
      raise Exception.Create('');
    MPDealloc(A);
    A := C;
  end else if B^.Size = 1 then begin
    C := nil;
    MPRealloc(C,A^.Size + 1);
    RawMulByInt(A,B^.Data[0],C);
    C^.Sign := A^.Sign * B^.Sign;
    if C^.Sign = 0 then
      raise Exception.Create('');
    MPDealloc(A);
    A := C;
  end else begin
    C := nil;
    MPRealloc(C,A^.Size + B^.Size);
    RawMul(A,B,C);
    C^.Sign := A^.Sign * B^.Sign;
    if C^.Sign = 0 then
      raise Exception.Create('');
    MPDealloc(A);
    A := C;
  end;
end;

procedure MPMul2(A, B: PMPInteger; var R: PMPInteger);
begin
  if A = R then
    MPMul(R,B)
  else if B = R then
    MPMul(R,A)
  else begin
    MPDealloc(R);
    R := nil;
    MPRealloc(R,A^.Size + B^.Size);
    RawMul(A,B,R);
    R^.Sign := A^.Sign * B^.Sign;
  end;
end;

procedure RawShl(A: Pointer; boffs, Size: Integer);
asm
  push ESI
  push EDI
  push EBX

  mov EBX,ECX
  mov ECX,EDX
  and ECX,31
  shr EDX,5

  mov EDI,EAX
  lea ESI,[EAX + EDX*4]

  dec EBX
  jz @@2
  mov EDX,[ESI]
@@1:
  mov EAX,EDX
  mov EDX,[ESI+4]
  shld EAX,EDX,CL
  mov [EDI],EAX
  add ESI,4
  add EDI,4
  dec EBX
  jnz @@1
@@2:
  mov EAX,[ESI]
  shl EAX,CL
  mov [EDI],EAX

  cmp ESI,EDI
  je @@Exit
  xor EDX,EDX
@@3:
  add EDI,4
  mov [EDI],EDX
  cmp ESI,EDI
  jne @@3

@@Exit:
  pop EBX
  pop EDI
  pop ESI
end;

procedure MPShl(var A: PMPInteger; boffs: Integer);
var
  NewBitSize, OldBitSize, OldNetSize: Integer;
begin
  if boffs = 0 then Exit;
  OldBitSize := MPMSB(A);
  if OldBitSize = 0 then Exit;
  NewBitSize := boffs + OldBitSize;
  if NewBitSize > Integer(A^.Size*32) then
    MPRealloc(A,(NewBitSize+31) shr 5);
  OldNetSize := Integer(A^.Size) - (boffs shr 5);
  RawShl(@A^.Data,boffs,OldNetSize);
end;
{$IFDEF NEVER}
asm
  push ESI
  push EDI
  push EBX

  push EAX
  mov EAX,[EAX]
  push EAX

  mov ECX,EDX

  cmp ECX,0
  jle @@Done

  and ECX,31
  shr EDX,5
  mov EBX,[EAX]
  push EDX
  push ECX
  push EAX
  mov EAX,ESP

  add EDX,EBX
  inc EDX
  mov ECX,True
{$IFDEF LINUX}
  push EBX
  call MPRealloc
  pop EBX
{$ELSE}
  call MPRealloc
{$ENDIF}

  pop EAX // The stack has been modified by MPRealloc. This is a PMPInteger.
  mov EBX,[EAX]
  pop ECX
  pop EDX

  dec EBX

  lea EDI,[EAX + 8]
  lea ESI,[EAX + EDX*4 + 8]
  mov [ESP],EAX

  sub BL,DL
  mov BH,DL

  test BL,BL
  jz @@LastShift

  xor EDX,EDX
@@Main:
  mov EAX,[ESI]
  mov EDX,[ESI + 4]
  shld EAX,EDX,CL
  mov [EDI],EAX
  add ESI,4
  add EDI,4
  dec BL
  jnz @@Main

@@LastShift:
  mov EAX,[ESI]
  shl EAX,CL
  mov [EDI],EAX

  add EDI,4

  test BH,BH
  jz @@Done
  xor EDX,EDX
  movzx ECX,BH
@@PadZero:
  mov [EDI],EDX
  add EDI,4
  loop @@PadZero

@@Done:
  pop EAX
  pop EDX
  mov [EDX],EAX
  mov EAX,EDX

  pop EBX
  pop EDI
  pop ESI
end;
{$ENDIF} //MPShl IFDEF NEVER

procedure MPShl2(A: PMPInteger; boffs: Integer; var R: PMPInteger);
begin
  if LongInt(R) <> LongInt(A) then begin
    MPDealloc(R);
    R := MPCopy(A);
  end;
  MPShl(R,boffs);
end;

procedure RawShr(A: pointer; boffs, Size: Integer);
asm
  push ESI
  push EDI
  push EBX

  mov EBX,ECX
  mov ECX,EDX
  and ECX,31
  shr EDX,5

  dec EBX

  lea ESI,[EAX + EBX*4]
  lea EDI,[ESI + EDX*4]

  jz @@2
@@1:
  mov EAX,[ESI]
  mov EDX,[ESI-4]
  shrd EAX,EDX,CL
  mov [EDI],EAX
  sub ESI,4
  sub EDI,4
  dec EBX
  jnz @@1
@@2:
  mov EAX,[ESI]
  shr EAX,CL
  mov [EDI],EAX

  cmp ESI,EDI
  jge @@Exit
  xor EDX,EDX
@@3:
  sub EDI,4
  mov [EDI],EDX
  cmp ESI,EDI
  jl @@3

@@Exit:
  pop EBX
  pop EDI
  pop ESI
end;

procedure MPShr(A: PMPInteger; boffs: Integer);
var
  NewBitSize, OldBitSize, OldNetSize, OldSize: Integer;
begin
  if boffs = 0 then Exit;
  OldBitSize := MPMSB(A);
  NewBitSize := OldBitSize - boffs;
  if NewBitSize <= 0 then begin
    FillChar(A^.Data,A^.Size*4,0);
    A^.Sign := 0;
  end else begin
    OldSize := (OldBitSize + 31) shr 5;
    OldNetSize := OldSize - (boffs shr 5);                           
    if A^.Sign = 0 then
      raise Exception.Create('');
    RawShr(@A^.Data[Integer(A^.Size) - OldSize],boffs,OldNetSize);
    if A^.Sign = 0 then
      raise Exception.Create('');
  end;
end;
{
asm
  push ESI
  push EDI
  push EBX
  mov ECX,EDX

  cmp ECX,0
  jle @@Exit

  and ECX,31
  shr EDX,5
  shl EDX,2
  mov EBX,[EAX]
  dec EBX
  lea ESI,[EAX + 4*EBX + 8]
  sub ESI,EDX
  shr EDX,2

  cmp EDX,EBX
  ja  @@Exit

  lea EDI,[EAX + 4*EBX + 8]
  sub BL,DL
  mov BH,DL

  cmp BL,0
  jz @@LastShift
  jl @@BeginPad

  xor EDX,EDX
@@Main:
  mov EAX,[ESI]
  mov EDX,[ESI - 4]
  shrd EAX,EDX,CL
  mov [EDI],EAX
  sub ESI,4
  sub EDI,4
  dec BL
  jnz @@Main

@@LastShift:
  mov EAX,[ESI]
  shr EAX,CL
  mov [EDI],EAX

@@BeginPad:
  sub EDI,4

  xor EDX,EDX
  test BH,BH
  jz @@Exit
  movzx ECX,BH
@@PadZero:
  mov [EDI],EDX
  sub EDI,4
  loop @@PadZero

@@Exit:
  pop EBX
  pop EDI
  pop ESI
end;
 }
function MPMSB(A: PMPInteger): Integer;
const
  BitsPerInt = 32;
asm
  push EBX
  test EAX,EAX
  jz @@ExitMSB

  push EAX

  mov ECX,BitsPerInt

  mov EBX,[EAX]
  test EBX,EBX
  jz @@DoneMSB
  shl EBX,5
  add EAX,8
@@ZeroScan:
  mov EDX,[EAX]
  test EDX,EDX
  jnz @@BitScan
  add EAX,4
  sub EBX,BitsPerInt
  jnz @@ZeroScan
  jmp @@DoneMSB

@@BitScan:
  mov EDX,$80000000
  mov ECX,BitsPerInt
@@InnerMSB:
  test [EAX],EDX
  jnz @@DoneMSB
  shr EDX,1
  loop @@InnerMSB

@@DoneMSB:
  pop EDX
  lea EAX,[EBX + ECX - BitsPerInt]
  test EAX,EAX
  jnz @@ExitMSB
  mov dword [EDX + 4],0
@@ExitMSB:
  pop EBX
end;

function MPCmpOffset(A, B: PMPInteger; Offset: Cardinal = 0): Integer;
var
  I, bcA, bcB, C, DA, DB: Cardinal;
begin
  if Assigned(A) xor Assigned(B) then begin
    Result := MaxInt;
    Exit;
  end;
  bcA := A^.Size;
  while (bcA > 0) and (A^.Data[A^.Size - bcA] = 0) do
    Dec(bcA);
  if bcA = 0 then
    A^.Sign := 0;
  bcB := B^.Size;
  while (bcB > 0) and (B^.Data[B^.Size - bcB] = 0) do
    Dec(bcB);    
  if bcB = 0 then
    B^.Sign := 0;
  if A^.Sign < B^.Sign then
    Result := -1
  else if A^.Sign > B^.Sign then
    Result := 1
  else if A^.Sign = 0 then
    Result := 0
  else if bcA < bcB + Offset then
    Result := -A^.Sign
  else if bcA > bcB + Offset then
    Result := A^.Sign
  else begin
    DA := A^.Size - bcA;
    DB := B^.Size - bcB;
    C := A^.Size - DA;
    if C > B^.Size - DB then
      C := B^.Size - DB;
    for I := 0 to C - 1 do
      if A^.Data[I + DA] <> B^.Data[I + DB] then begin
        if A^.Data[I + DA] < B^.Data[I + DB] then
          Result := -A^.Sign
        else
          Result := A^.Sign;
        Exit;
      end;
    Result := 0;
  end;
end;

function MPCmpAbsOffset(A, B: PMPInteger; Offset: Cardinal = 0): Integer;
var
  I, bcA, bcB, C, DA, DB: Cardinal;
begin
  bcA := A^.Size;
  while (bcA > 0) and (A^.Data[A^.Size - bcA] = 0) do
    Dec(bcA);
  bcB := B^.Size;
  while (bcB > 0) and (B^.Data[B^.Size - bcB] = 0) do
    Dec(bcB);
  if bcA < bcB + Offset then
    Result := -1
  else if bcA > bcB + Offset then
    Result := 1
  else begin
    DA := A^.Size - bcA;
    DB := B^.Size - bcB;
    C := A^.Size - DA;
    if C > B^.Size - DB then
      C := B^.Size - DB;
    for I := 0 to C - 1 do
      if A^.Data[I + DA] <> B^.Data[I + DB] then begin
        if A^.Data[I + DA] < B^.Data[I + DB] then
          Result := -1
        else
          Result := 1;
        Exit;
      end;
    Result := 0;
  end;
end;

procedure RawSub(A, B: Pointer; Count: Integer); overload;
asm
  push EBX
  clc
@@MainRawSub:
  mov EBX,[EDX + ECX*4 - 4]
  sbb [EAX + ECX*4 - 4],EBX
  loop @@MainRawSub
  pop EBX
end;

procedure RawSub(A, B: Pointer; Count: Integer; LastSub: Boolean); overload;
asm
  push EBX
  clc
@@MainRawSub:
  mov EBX,[EDX + ECX*4 - 4]
  sbb [EAX + ECX*4 - 4],EBX
  loop @@MainRawSub
  sbb dword [EAX - 4],0
  pop EBX
end;

procedure RawSub(A, B: Pointer; Count: Integer; qd: LongWord); overload;
asm
  push EBX
  push ESI
  push EDI
  mov ESI,EDX
  mov EDI,EAX
  xor EBX,EBX
  xor EDX,EDX
@@MainRawSub:  
  sub [EDI + ECX*4 - 4],EDX
  adc EBX,0
  mov EAX,[ESI + ECX*4 - 4]
  mul qd
  sub [EDI + ECX*4 - 4],EAX
  adc EDX,EBX
  mov EBX,0
  setc BL
  loop @@MainRawSub
  test EDX,EDX
  jz @@ExitRawSub
  sub [EDI - 4],EDX
@@ExitRawSub:
  pop EDI
  pop ESI
  pop EBX
end;

function MPDivByInt2(V: PMPInteger; M: LongWord; var Q: PMPInteger): LongWord;
asm
  push ESI
  push EDI
  push EBX

  mov EBX,EDX
  mov EDI,ECX
  mov ESI,EAX

  cmp ESI,[EDI]
  je @@1
  mov EAX,EDI
  mov EDX,[ESI]
  mov ECX,True
{$IFDEF LINUX}
  push EBX
  mov EBX,dword [ESP + 4]
  call MPRealloc
  pop EBX
{$ELSE}
  call MPRealloc
{$ENDIF}
@@1:
  mov EDI,[EDI]

  mov EDX,[ESI + 4]
  mov [EDI + 4],EDX
  add EDI,8

  xor EDX,EDX
  mov ECX,[ESI]
  add ESI,8
@@2:
  mov EAX,[ESI]
  div EBX
  mov [EDI],EAX
  add ESI,4
  add EDI,4
  loop @@2
  mov EAX,EDX
  pop EBX
  pop EDI
  pop ESI
end;

procedure MPDiv(var A: PMPInteger; M: PMPInteger; var Q: PMPInteger; AdjustSize: Boolean = True);
type
  Int128 = array [0..3] of Longword;
var
  n, t, i, boffs: Cardinal;
  qd, bit, md: LongWord;
  X96, Y96, P96: Int128;

  function TopDiv(const X, Y, P: Int128): LongWord;
  asm
    push EBX
    push ESI
    push EDI
    push EBP

    mov ESI,EAX
    mov EDI,EDX
    mov EBP,ECX

    mov EAX,[ESI + 4]
    mov EDX,[ESI + 8]
    mov ECX,[EDI + 4]

    div ECX

    mov EBX,EAX

    mov EAX,[EDI]
    mul EBX
    mov [EBP],EAX
    mov [EBP + 4],EDX
    mov dword [EBP + 8],0
    mov dword [EBP + 12],0

    mov EAX,[EDI + 4]
    mul EBX
    add [EBP + 4],EAX
    adc [EBP + 8],EDX
    adc dword [EBP + 12],0
    mov EAX,[EBP]   
    mov ECX,[EBP + 8]
    mov EDX,[EBP + 12]
    mov EBP,[EBP + 4]
@@Loop:
    test EDX,EDX
    jnz @@Dec
    cmp ECX,[ESI + 8]
    jb @@Exit
    ja @@Dec
    cmp EBP,[ESI + 4]
    jb @@Exit
    ja @@Dec
    cmp EAX,[ESI]
    jb @@Exit
@@Dec:
    dec EBX
    sub EAX,[EDI]
    sbb EBP,[EDI + 4]
    sbb ECX,0
    sbb EDX,0

    jmp @@Loop
@@Exit:
    mov EAX,EBX

    pop EBP
    pop EDI
    pop ESI
    pop EBX
  end;

begin

  t := M^.Size - 1;
  while (t > 0) and (M^.Data[M^.Size - 1 - t] = 0) do Dec(t);

  if t = 0 then begin
    if M^.Data[M^.Size - 1] = 0 then
      raise Exception.Create('Division by zero in LargeDiv.')
    else begin
      qd := MPDivByInt2(A,M^.Data[M^.Size - 1],Q);
      if AdjustSize then
        MPRealloc(A,1)
      else if A^.Size > 1 then
        for i := 1 to A^.Size do
          A^.Data[A^.Size - i] := 0;
      A^.Data[A^.Size - 1] := qd;
      if A^.Sign = M^.Sign then
        Q^.Sign := 1
      else
        Q^.Sign := -1;
      if A^.Sign = -1 then begin
        MPAdd(A,M);
        if MPMSB(Q) = 0 then begin
          Q^.Data[Q^.Size - 1] := 1;
          Q^.Sign := -1;
        end else if Q^.Sign = -1 then
          MPAddInt(Q,1)
        else
          MPSubInt(Q,1);
      end;
      Exit;
    end;
  end;

  n := A^.Size - 1;
  while (A^.Data[A^.Size - 1 - n] = 0) and (n > 0) do Dec(n);

  if n < t then begin
    if A^.Sign < 0 then
      MPAdd(A,M);
    if AdjustSize then
      MPRealloc(A,(MPMSB(A) + 31) shr 5);
    MPDealloc(Q);
    Q := IntToMPInt(0);
    Exit;
  end;

  // Normalize:
  boffs := 0;
  bit := $80000000;
  md := M^.Data[M^.Size - 1 - t];
  while md < bit do begin
    bit := bit shr 1;
    Inc(boffs);
  end;
  if boffs > 0 then begin
    M := MPCopy(M);
    MPShl(M,boffs);
    MPShl(A,boffs);
    n := A^.Size - 1;
    while (A^.Data[A^.Size - 1 - n] = 0) and (n > 0) do Dec(n);
    md := M^.Data[M^.Size - 1 - t];
  end;

  MPRealloc(Q,n-t+1);
  FillChar(Q^.Data,Q^.Size*4,0);
  Q^.Sign := 0;
  if md <= A^.Data[A^.Size - 1 - n] then begin
    if md < A^.Data[A^.Size - 1 - n] then begin
      qd := A^.Data[A^.Size - 1 - n] div (md + 1);
      if qd > 0 then begin
        Q^.Data[0] := qd;
        RawSub(@A^.Data[A^.Size - 1 - n],@M^.Data[M^.Size - 1 - t],t+1,qd);
      end;
    end;
    while MPCmpAbsOffset(A,M,n-t) > 0 do begin
      MPAddInt(Q,1);
      RawSub(@A^.Data[A^.Size - 1 - n],@M^.Data[M^.Size - 1 - t],t+1);
    end;
  end;

  if (M^.Size > 2) and
     (M^.Data[M^.Size - 1 - (t-2)] = $FFFFFFFF) and
     (M^.Data[M^.Size - 1 - (t-1)] = $FFFFFFFF) and
     (M^.Data[M^.Size - 1 - t] = $FFFFFFFF) then begin
    for i := n downto t + 1 do begin
      if A^.Data[A^.Size - 1 - i] = $FFFFFFFF then begin
        qd := $FFFFFFFF;
      end else begin
        qd := A^.Data[A^.Size - 1 - i];
        if (A^.Data[A^.Size - 1 - (i-1)] = $FFFFFFFF) and
           (A^.Data[A^.Size - 1 - (i-2)] = $FFFFFFFF) then
          Inc(qd);
      end;

      if qd = 1 then
        RawSub(@A^.Data[A^.Size - 1 - (i - 1)],@M^.Data[M^.Size - 1 - t],t+1,True)
      else
        RawSub(@A^.Data[A^.Size - 1 - (i - 1)],@M^.Data[M^.Size - 1 - t],t+1,qd);

      if A^.Data[A^.Size - 1 - i] <> 0 then begin
        RawAdd(@A^.Data[A^.Size - 1 - (i - 1)],@M^.Data[M^.Size - 1 - t],t+1);
        A^.Data[A^.Size - 1 - i] := 0;
        Dec(qd);
      end;
      Q^.Data[Q^.Size - 1 - (i-t-1)] := qd;
    end;
  end else begin
    Y96[0] := M^.Data[M^.Size - 1 - (t-1)];
    Y96[1] := M^.Data[M^.Size - 1 - t];
    Y96[2] := 0;
    Y96[3] := 0;
    X96[3] := 0;
    P96[3] := 0;
    P96[2] := 0;
    for i := n downto t + 1 do begin
      if A^.Data[A^.Size - 1 - i] = M^.Data[M^.Size - 1 - t] then begin
        qd := $FFFFFFFF;
      end else begin
        X96[0] := A^.Data[A^.Size - 1 - (i-2)];
        X96[1] := A^.Data[A^.Size - 1 - (i-1)];
        X96[2] := A^.Data[A^.Size - 1 - i];
        qd := TopDiv(X96,Y96,P96);
      end;

      if qd = 1 then
        RawSub(@A^.Data[A^.Size - 1 - (i - 1)],@M^.Data[M^.Size - 1 - t],t+1,True)
      else
        RawSub(@A^.Data[A^.Size - 1 - (i - 1)],@M^.Data[M^.Size - 1 - t],t+1,qd);

      if A^.Data[A^.Size - 1 - i] <> 0 then begin
        RawAdd(@A^.Data[A^.Size - 1 - (i - 1)],@M^.Data[M^.Size - 1 - t],t+1);
        A^.Data[A^.Size - 1 - i] := 0;
        Dec(qd);
      end;
      Q^.Data[Q^.Size - 1 - (i-t-1)] := qd;
    end;
  end;
  if A^.Sign = M^.Sign then
    Q^.Sign := 1
  else
    Q^.Sign := -1;

  if (A.Sign = -1) and (MPMSB(A) > 0) then begin
    MPAdd(A,M);
    if MPMSB(Q) = 0 then begin
      Q^.Data[Q^.Size - 1] := 1;
      Q^.Sign := -1;
    end else if Q^.Sign = -1 then
      MPAddInt(Q,1)
    else
      MPSubInt(Q,1);
  end;
  if boffs > 0 then begin
  // Denormalize:
    MPDealloc(M);
    MPShr(A,boffs);
  end;
  if AdjustSize then begin
    boffs := MPMSB(A);
    if boffs = 0 then boffs := 1;
    if A^.Size * 32 > boffs then
      MPRealloc(A,(boffs + 31) shr 5);
    boffs := MPMSB(Q);
    if boffs = 0 then boffs := 1;
    if Q^.Size * 32 > boffs then
      MPRealloc(Q,(boffs + 31) shr 5);
  end;
end;

procedure MPMod(var A: PMPInteger; M: PMPInteger);
var
  Dummy: PMPInteger;
begin
  Dummy := nil;
  MPDiv(A,M,Dummy);
  MPDealloc(Dummy);
end;

procedure MPMod2(A, M: PMPInteger; var R: PMPInteger);
begin
  MPCopy2(A,R);
  MPMod(R,M);
end;

procedure MPMulMod(var A: PMPInteger; B, M: PMPInteger);
var
  Dummy: PMPInteger;
begin
  MPMul(A,B);
  Dummy := nil;
  MPDiv(A,M,Dummy);
  MPDealloc(Dummy);
end;

procedure MPMulMod2(A, B, M: PMPInteger; var R: PMPInteger);
var
  Dummy: PMPInteger;
begin
  MPMul2(A,B,R);
  Dummy := nil;
  MPDiv(R,M,Dummy);
  MPDealloc(Dummy);
end;

procedure MPExpMod(G, E, M: PMPInteger; var R: PMPInteger);
var
  LW, Bit, EW: LongWord;
  i, j, c, k, l: Integer;
  G2: PMPInteger;
  HasMult: Boolean;
  WG: array of PMPInteger;

  procedure SqrMod;
  var
    R2, Dummy, Tmp: PMPInteger;
  begin
    if (R^.Size = 1) and (R^.Data[0] = 1) then begin
      Exit
    end else if R^.Size = M^.Size then begin
      R2 := nil;
      MPRealloc(R2,M^.Size*2);
      ClearLong(@R2^.Data,R2^.Size);
      RawSqr(R,R2);
      R2^.Sign := 1;
      Dummy := nil;
      MPDiv(R2,M,Dummy);
      MPDealloc(Dummy);
      Tmp := R;
      R := R2;
      MPDealloc(Tmp);
    end else
      MPMulMod(R,R,M);
  end;

begin
  c := MPMSB(E);
  case c of
    0..18:     k := 1;
    19..48:    k := 3;
    49..160:   k := 4;
    161..480:  k := 5;
    481..1344: k := 6;
    1345..3584:k := 7;
  else
    k := 8;
  end;
  SetLength(WG,1 shl (k-1));
  G2 := nil;
  MPMulMod2(G,G,M,G2);
  WG[0] := nil;
  MPMod2(G,M,WG[0]);
  if k > 1 then
    for i := 1 to (1 shl (k-1)) - 1 do begin
      WG[i] := nil;
      MPMulMod2(WG[i-1],G2,M,WG[i]);
    end;
  MPDealloc(G2);

  MPRealloc(R,1);
  R^.Data[0] := 1;
  R^.Sign := 1;
  try
    EW := 0;
    HasMult := False;
    for i := 0 to E^.Size - 1 do begin
      LW := E^.Data[i];
      Bit := $80000000;
      for j := 31 downto 0 do begin
        EW := EW shl 1;
        if (Bit and LW) = Bit then
          EW := EW + 1
        else if (EW = 0) and HasMult then
          SqrMod;
        if (EW shr (k-1)) = 1 then begin
          l := 0;
          while not Odd(EW) do begin
            Inc(l);
            EW := EW shr 1;
          end;
          HasMult := True;
          c := k - l;
          while c > 0 do begin
            SqrMod;
            Dec(c);
          end;
          MPMulMod(R,WG[EW shr 1],M);
          while l > 0 do begin
            SqrMod;
            Dec(l);
          end;
          EW := 0;
        end;
        Bit := Bit shr 1;
      end;
    end;

    if EW <> 0 then begin
      i := k;
      while (EW shr (i-1)) = 0 do
        Dec(i);
      l := 0;
      while not Odd(EW) do begin
        Inc(l);
        EW := EW shr 1;
      end;
      c := i - l;
      while c > 0 do begin
        SqrMod;
        Dec(c);
      end;
      MPMulMod(R,WG[EW shr 1],M);
      while l > 0 do begin
        SqrMod;
        Dec(l);
      end;
    end;
  finally
    for i := 0 to (1 shl (k-1)) - 1 do
      MPDealloc(WG[i]);
    WG := nil;
  end;
end;

procedure MPPowBMod(Base: LongWord; E, M: PMPInteger; var R: PMPInteger);
var
  LW: LongWord;
  i, j, k, highk: Cardinal;
  R2, Dummy: PMPInteger;
  HasMult: Boolean;
  Divs: array [0..31] of PMPInteger;
begin
  MPRealloc(R,M^.Size);
  ClearLong(@R^.Data[0],R^.Size-1);
  R^.Data[R^.Size - 1] := 1;
  R^.Sign := 1;
  HasMult := False;
  R2 := nil;
  MPRealloc(R2,M^.Size*2);
  R2^.Sign := 1;
  Dummy := nil;
  highk := 0;
  try
    LW := 2;
    while LW < Base do begin
      Inc(highk);
      LW := LW + LW;
    end;
    FillChar(Divs,SizeOf(Divs),0);
    Divs[0] := M;
    for k := 1 to highk do
      MPAdd2(Divs[k-1],Divs[k-1],Divs[k]);
    for i := 0 to E^.Size - 1 do begin
      LW := E^.Data[i];

      if (LW = 0) and not HasMult then Continue;
      HasMult := True;
      for j := 31 downto 0 do begin
        MPRealloc(R2,M^.Size * 2);
        ClearLong(@R2^.Data,R2^.Size);
        RawSqr(R,R2);
        MPDiv(R2,M,Dummy,False);
        Assert(R^.Size = M^.Size);
        MoveLongUp(@R2^.Data[M^.Size],@R^.Data,M^.Size);
        if LW and $80000000 > 0 then begin
          MPMulByInt(R,Base);
          for k := highk downto 0 do
            if MPCmpAbsOffset(R,Divs[k]) >= 0 then
              MPSub(R,Divs[k]);
          MPRealloc(R,M^.Size);
        end;
        LW := LW shl 1;
      end;
    end;
  finally
    for k := 1 to highk do
      MPDealloc(Divs[k]);
    MPDealloc(Dummy);
    MPDealloc(R2);
  end;
end;


procedure MPPow2Mod(E, M: PMPInteger; var R: PMPInteger);
var
  LW: LongWord;
  i, j: Cardinal;
  R2, Dummy: PMPInteger;
  HasMult: Boolean;
begin
  MPRealloc(R,M^.Size);
  ClearLong(@R^.Data,R^.Size-1);
  R^.Data[R^.Size - 1] := 1;
  R^.Sign := 1;
  HasMult := False;
  R2 := nil;
  MPRealloc(R2,M^.Size*2);
  R2^.Sign := 1;
  Dummy := nil;
  try
    for i := 0 to E^.Size - 1 do begin
      LW := E^.Data[i];

      if (LW = 0) and not HasMult then Continue;
      HasMult := True;
      for j := 31 downto 0 do begin
        if R2^.Size <> M^.Size*2 then MPRealloc(R2,M^.Size*2);
        ClearLong(@R2^.Data,M^.Size*2);
        RawSqr(R,R2);
        MPDiv(R2,M,Dummy,False);
        MoveLongUp(@R2^.Data[M^.Size],@R^.Data,M^.Size);
        if LW and $80000000 > 0 then begin
          MPAdd(R,R);
          if MPCmpOffset(R,M) >= 0 then
            MPSub(R,M);
          MPRealloc(R,M^.Size);
        end;
        LW := LW shl 1;
      end;
    end;
  finally
    MPDealloc(Dummy);
    MPDealloc(R2);
  end;
end;                 

procedure MPPow4Mod(E, M: PMPInteger; var R: PMPInteger);
var
  LW: LongWord;
  i, j, k, n: Cardinal;
  R2, Dummy: PMPInteger;
  HasMult: Boolean;
begin
  MPRealloc(R,M^.Size);
  R^.Data[R^.Size - 1] := 1;
  R^.Sign := 1;
  HasMult := False;
  R2 := nil;
  MPRealloc(R2,M^.Size*2);
  R2^.Sign := 1;
  Dummy := nil;
  try
    for i := 0 to E^.Size - 1 do begin
      LW := E^.Data[i];

      if (LW = 0) and not HasMult then Continue;
      HasMult := True;
      for k := 0 to 3 do begin
        for j := 0 to 7 do begin
          MPRealloc(R2,M^.Size * 2);
          for n := 1 to R2^.Size do
            R2^.Data[R2^.Size - n] := 0;
          RawMul(R,R,R2);
          MPDiv(R2,M,Dummy,False);
          for n := 1 to R^.Size do
            R^.Data[R^.Size - n] := R2^.Data[R2^.Size - n];
        end;
        MPShl(R,(LW shr 24)*2);
        MPDiv(R,M,Dummy);
        MPRealloc(R,M^.Size);
        LW := LW shl 8;
      end;
    end;
  finally
    MPDealloc(Dummy);
    MPDealloc(R2);
  end;
end;

function MPModByInt(N: PMPInteger; P: LongWord): Longword;
asm
  push EBX
  push EDI

  lea EDI,[EAX + 8]
  mov EBX,EDX

  xor EDX,EDX
  mov ECX,[EAX]
@@Main:
  mov EAX,[EDI]
  div EBX
  add EDI,4
  loop @@Main

  mov EAX,EDX

  pop EDI
  pop EBX
end;

procedure MPBinaryGCD(X, Y: PMPInteger; var AGCD: PMPInteger);
const
  BitsPerInt = 32;
var
  U, V: PMPInteger;
  i, j, GBits: Cardinal;
  Bit: LongWord;

  function IsZero(X: PMPInteger): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to X^.Size - 1 do begin
      Result := X^.Data[I] = 0;
      if not Result then Break;
    end;
  end;

begin
  // Input: Two positive integers with U >= V.
  if (X^.Sign <> 1) or (Y^.Sign <> 1) then
    raise Exception.Create('Invalid argument for MPBinaryGCD');
  if MPCmpOffset(X,Y,0) >= 0 then begin
    U := MPCopy(X);
    V := MPCopy(Y);
  end else begin
    U := MPCopy(Y);
    V := MPCopy(X);
  end;
//  try
    // while X and Y are both even do: U <- U/2, V <- V/2, G <- 2G.
    i := 0;
    while (U^.Data[U^.Size-1 - i] = 0) and (V^.Data[V^.Size-1 - i] = 0) and
          (i < U^.Size-1) and (i < V^.Size-1) do
      Inc(i);
    GBits := i * BitsPerInt;
    Bit := 1;
    while ((U^.Data[U^.Size-1 - i] and Bit) = 0) and
          ((V^.Data[V^.Size-1 - i] and Bit) = 0) do begin
      Inc(GBits);
      Bit := Bit shl 1;
    end;
    if GBits > 0 then begin
      MPShr(U,GBits);
      MPShr(V,GBits);
    end;

    // While U <> 0 do the following:
    while not IsZero(U) do begin
      // While U is even do: U <- U/2
      i := 0;
      while U^.Data[U^.Size-1 - i] = 0 do Inc(i);
      j := i * BitsPerInt;
      Bit := 1;
      while (U^.Data[U^.Size-1 - i] and Bit) = 0 do begin
        Inc(j);
        Bit := Bit shl 1;
      end;
      if j > 0 then MPShr(U,j);

      // While V is even do: V <- V/2
      if not IsZero(V) then begin
        i := 0;
        while V^.Data[V^.Size-1 - i] = 0 do Inc(i);
        j := i * BitsPerInt;
        Bit := 1;
        while (V^.Data[V^.Size-1 - i] and Bit) = 0 do begin
          Inc(j);
          Bit := Bit shl 1;
        end;
        if j > 0 then MPShr(V,j);
      end;

      // T <- |U-V|/2,
      // If U >= V then U <-T, else V <- T.
      if MPCmpOffset(U,V,0) >= 0 then begin
        MPSub(U,V);
        MPShr(U,1);
      end else begin
        MPSub(V,U);
        MPShr(V,1);
      end;
    end;

    // Return(V*G).
    MPDealloc(AGCD);
    AGCD := MPCopy(V);
    if GBits > 0 then
      MPShl(AGCD,GBits);

//  finally
    MPDealloc(U);
    MPDealloc(V);
//  end;
end;

procedure MPBinaryInvMod(Y, M: PMPInteger; var AInv: PMPInteger);
var
  u, v, B, D: PMPInteger;
  Bit: LongWord;
  Bits, J: Cardinal;
begin
  u := MPCopy(M);
  v := MPCopy(Y);
  if v^.Sign = -1 then
    MPAdd(v,M);
  Assert(v^.Sign > 0);
  B := IntToMPInt(0);
  D := IntToMPInt(1);

  while MPMSB(u) > 0 do begin
    Bits := 0;
    J := 1;
    Bit := 1;
    while (J <= u^.Size) and ((u^.Data[u^.Size - J] and Bit) = 0) do begin
      Inc(Bits);
      if (B^.Data[B^.Size - 1] and 1) > 0 then
        MPSub(B,M);
      MPShr(B,1);
      Bit := Bit + Bit;
      if Bit = 0 then begin
        Inc(J);
        Bit := 1;
      end;
    end;
    if Bits > 0 then
      MPShr(u,Bits);

    Bits := 0;
    J := 1;
    Bit := 1;
    while (J <= v^.Size) and ((v^.Data[v^.Size - J] and Bit) = 0) do begin
      Inc(Bits);
      if (D^.Data[D^.Size - 1] and 1) > 0 then
        MPSub(D,M);
      MPShr(D,1);
      Bit := Bit + Bit;
      if Bit = 0 then begin
        Inc(J);
        Bit := 1;
      end;
    end;
    if Bits > 0 then
      MPShr(v,Bits);

    if MPCmpOffset(u,v) >= 0 then begin
      MPSub(u,v);
      MPSub(B,D);
    end else begin
      MPSub(v,u);
      MPSub(D,B);
    end;
  end;

  MPCopy2(D,AInv);
  if AInv^.Sign = -1 then
    MPAdd(AInv,M);
  MPRealloc(AInv,(MPMSB(AInv) + 31) shr 5);

  MPDealloc(D);
  MPDealloc(B);
  MPDealloc(v);
  MPDealloc(u);
end;

procedure MPInvMod(V, M: PMPInteger; var AInv: PMPInteger);
var
  a, b, q, k, x, x1, x2: PMPInteger;
begin
  b := MPCopy(V);
  a := MPCopy(M);

  q := nil;
  if MPCmpOffset(b,a,0) > 0 then
    MPDiv(b,a,q);

  x1 := IntToMPInt(1);
  x2 := IntToMPInt(0);
  k := nil;
  x := nil;

  while MPMSB(b) > 0 do begin
    k := a;
    MPDiv(k,b,q);
    a := b;
    b := k;
    x := x2;
    // q <- (q * x1) :
    MPMulMod(q,x1,M);
    // x <- (x2 - q) :
    MPSub(x,q);
    while x^.Sign = -1 do MPAdd(x,M);
    x2 := x1;
    x1 := x;
  end;

  if MPMSB(a) = 1 then begin
    if X2^.Sign = -1 then MPAdd(x2,M);
    MPDiv(x2,M,q);
    if Assigned(AInv) then MPDealloc(AInv);
    AInv := x2;
  end else if Assigned(AInv) then begin
    AInv^.Sign := 0;
    FillChar(AInv^.Data,AInv^.Size*4,0);
    MPDealloc(x2);
  end else                              
    MPDealloc(x2);

  MPDealloc(a);
  MPDealloc(b);
  MPDealloc(q);
  MPDealloc(x1);
//  MPDealloc(x2); // x2 = AInv
//  MPDealloc(k);  // k = b
//  MPDealloc(x);  // x = x1
end;

procedure MPDivMod(var A: PMPInteger; B, M: PMPInteger);
var
  BInv: PMPInteger;
begin
  BInv := nil;
  MPBinaryInvMod(B,M,BInv);
  MPMulMod(A,BInv,M);
  MPDealloc(BInv);
end;

procedure MPDivMod2(A, B, M: PMPInteger; var Q: PMPInteger);
var
  BInv: PMPInteger;
begin
  BInv := nil;
  MPBinaryInvMod(B,M,BInv);
  MPMulMod2(A,BInv,M,Q);
  MPDealloc(BInv);
end;

procedure MPMulByInt(var X: PMPInteger; Y: LongInt);
var
  R: PMPInteger;
begin
  R := nil;
  MPRealloc(R,X.Size+1);
  if Y < 0 then begin
    RawMulByInt(X,-Y,R);
    R.Sign := -X.Sign;
  end else begin
    RawMulByInt(X,Y,R);
    R.Sign := X.Sign;
  end;
  MPDealloc(X);
  X := R;
end;

procedure MPMulByInt2(X: PMPInteger; Y: LongInt; var R: PMPInteger);
begin
  if X = R then
    MPMulByInt(R,Y)
  else begin
    MPRealloc(R,X.Size+1);
    if Y < 0 then begin
      RawMulByInt(X,-Y,R);
      R.Sign := -X.Sign;
    end else begin
      RawMulByInt(X,Y,R);
      R.Sign := X.Sign;
    end;
  end;
end;

procedure MPMontgomeryParams(M: PMPInteger; var R, RQuad: PMPInteger; var MPrime: LongWord);
var
  Dummy: PMPInteger;


  function InvMod(V: LongWord): LongWord;
  var
    a, b, q, k, x, x1, x2: LongWord;
  begin
    b := V;

    x1 := 1;
    x2 := 0;

    k := $100000000 mod b;
    q := $100000000 div b;
    a := b;
    b := k;
    // q <- (q * x1) :
    q := q * x1;
    // x <- (x2 - q) :
    x := x2 - q;
    x2 := x1;
    x1 := x;
    while b > 0 do begin
      k := a mod b;
      q := a div b;
      a := b;
      b := k;
      // q <- (q * x1) :
      q := q * x1;
      // x <- (x2 - q) :
      x := x2 - q;
      x2 := x1;
      x1 := x;
    end;

    Assert(a = 1);
    Result := x2;
  end;

begin
  MPRealloc(R,M^.Size + 1);
  FillChar(R^.Data[0],R^.Size * 4,0);
  R^.Data[R^.Size - 2] := 1;
  R^.Sign := 1;
  Dummy := nil;
  MPrime := -InvMod(M^.Data[M^.Size - 1]);

  R^.Data[0] := 1;
  R^.Data[R^.Size - 2] := 0;
  MPDiv(R,M,Dummy);
  MPDealloc(RQuad);
  RQuad := MPCopy(R);
  MPMul(RQuad,R);
  MPDiv(RQuad,M,Dummy);
  MPDealloc(Dummy);
end;

procedure MPMontgomeryRed(var X: PMPInteger; M: PMPInteger; MPrime: LongWord);
var
  i: Cardinal;
  ui: LongWord;
  um: PMPInteger;
begin
  if X^.Size <= M^.Size then
    MPRealloc(X,M^.Size + 1);
  um := nil;
  MPRealloc(um,M^.Size + 1);
  for i := 0 to M^.Size - 1 do begin
    ui := MPrime * X^.Data[X^.Size - 1];
    um^.Sign := 1;
    RawMulByInt(M,ui,um);
    asm
      push EBX
      push EDI
      mov EBX,[um]
      mov EAX,[X]
      mov EAX,[EAX]
      mov EDI,[EAX]
      lea EAX,[EAX + EDI*4]
      mov ECX,[EBX]
      sub EDI,ECX
      lea EBX,[EBX + ECX*4]
      mov EDX,dword [EBX + 4]
      add EDX,dword [EAX + 4]
      dec ECX
    @@1:
      mov EDX,dword [EBX]
      adc EDX,dword [EAX]
      mov dword [EAX + 4],EDX
      lea EBX,[EBX - 4]
      lea EAX,[EAX - 4]
      loop @@1
      setc DL
      test EDI,EDI
      jz @@3
      mov ECX,EDI
      add DL,$FF
    @@2:
      mov EDX,0
      adc EDX,dword [EAX]
      mov dword [EAX + 4],EDX
      lea EAX,[EAX - 4]
      loop @@2
      setc DL
    @@3:
      add DL,$FF
      mov dword [EAX + 4],0
      adc dword [EAX + 4],0
      pop EDI
      pop EBX
    end;
  end;
  if MPCmpOffset(X,M) >= 0 then
    MPSub(X,M);
  MPDealloc(um);
end;

procedure MPMontgomeryMul(X,Y,M: PMPInteger; MPrime: LongWord; var A: PMPInteger);
var
  i,n: Cardinal;
  ui,xi: LongWord;
  xy,um: PMPInteger;
  TempY, TempX: Boolean;
begin
  n := M^.Size;
  MPRealloc(A,n + 1);
  FillChar(A^.Data,(n + 1)*4,0);
  A^.Sign := 1;
  TempX := X^.Size <> n;
  if TempX then
    MPRealloc(X,n,False);
  TempY := Y^.Size <> n;
  if TempY then
    MPRealloc(Y,n,False);
  xy := nil;
  MPRealloc(xy,n + 1);
  xy^.Sign := 1;
  um := nil;
  MPRealloc(um,n + 1);
  um^.Sign := 1;
  for i := 1 to n do begin
    xi := X^.Data[n-i];
    ui := (A^.Data[n] + xi * Y^.Data[n-1]) * MPrime;
    RawMulByInt(Y,xi,xy);
    RawMulByInt(M,ui,um);
    RawAdd(@A^.Data,@xy^.Data,n+1);
    RawAdd(@A^.Data,@um^.Data,n+1);
    MoveLongUp(@A^.Data[0],@A^.Data[1],n);
//    for j := 1 to n do
//      A^.Data[n-j+1] := A^.Data[n-j];
    A^.Data[0] := A^.Sign - 1;
    A^.Sign := 1;
  end;
  if MPCmpOffset(A,M) >= 0 then
    MPSub(A,M);
  MPDealloc(xy);
  MPDealloc(um);
  if TempX then
    MPDealloc(X);
  if TempY then
    MPDealloc(Y);
end;

procedure MPMontgomeryExpMod(G, E, M: PMPInteger; var R: PMPInteger);
var
  LW, Bit, EW, MPrime: LongWord;
  i, j, c, k, l: Integer;
  G2: PMPInteger;
  FreeG: Boolean;
  WG: array of PMPInteger;
  RQuad, X, R2: PMPInteger;

  procedure Mont(var X: PMPInteger; Y,M: PMPInteger; MPrime: LongWord);
  var
    A: PMPInteger;
  begin
    A := nil;
    if ((X = Y) and MPKaratsubaSqr2(X,A)) or
       ((X <> Y) and MPKaratsubaMul2(X,Y,A)) then begin
      MPMontgomeryRed(A,M,MPrime);
      MPDealloc(X);
      X := A;
    end else begin
      MPMontgomeryMul(X,Y,M,MPrime,A);
      MPDealloc(X);
      X := A;
    end;
  end;
              
  procedure Mont2(X,Y,M: PMPInteger; MPrime: LongWord; var A: PMPInteger);
  begin
    if ((X = Y) and MPKaratsubaSqr2(X,A)) or
       ((X <> Y) and MPKaratsubaMul2(X,Y,A)) then begin
      MPMontgomeryRed(A,M,MPrime);
    end else begin
      MPMontgomeryMul(X,Y,M,MPrime,A);
    end;
  end;

begin
  c := MPMSB(G);
  FreeG := (G^.Size > LongWord((c + 31) shr 5)) or (G = R);
  if FreeG then
    MPRealloc(G,(c + 31) shr 5,False);

  c := MPMSB(E);
  case c of
    0..18:     k := 1;
    19..48:    k := 3;
    49..160:   k := 4;
    161..480:  k := 5;
    481..1344: k := 6;
    1345..3584:k := 7;
  else
    k := 8;
  end;

  R2 := nil;
  RQuad := nil;
  MPMontgomeryParams(M,R,RQuad,MPrime);
  X := nil;
  Mont2(G,RQuad,M,MPrime,X);
  try
    SetLength(WG,1 shl (k-1));
    G2 := nil;
    Mont2(X,X,M,MPrime,G2);
    WG[0] := X;
    for i := 1 to (1 shl (k-1)) - 1 do begin
      WG[i] := nil;
      Mont2(WG[i-1],G2,M,MPrime,WG[i]);
    end;
    MPDealloc(G2);

    EW := 0;
    for i := 0 to E^.Size - 1 do begin
      LW := E^.Data[i];
      Bit := $80000000;
      for j := 31 downto 0 do begin
        EW := EW shl 1;
        if (Bit and LW) = Bit then
          EW := EW + 1
        else if EW = 0 then
          Mont(R,R,M,MPrime);
        if (EW shr (k-1)) = 1 then begin
          l := 0;
          while not Odd(EW) do begin
            Inc(l);
            EW := EW shr 1;
          end;
          c := k - l;
          while c > 0 do begin
            Mont(R,R,M,MPrime);
            Dec(c);
          end;
          Mont(R,WG[EW shr 1],M,MPrime);
          while l > 0 do begin
            Mont(R,R,M,MPrime);
            Dec(l);
          end;
          EW := 0;
        end;
        Bit := Bit shr 1;
      end;
    end;
    if EW <> 0 then begin
      i := k;
      while (EW shr (i - 1)) = 0 do
        Dec(i);
      l := 0;
      while not Odd(EW) do begin
        Inc(l);
        EW := EW shr 1;
      end;
      c := i - l;
      while c > 0 do begin
        Mont(R,R,M,MPrime);
        Dec(c);
      end;
      Mont(R,WG[EW shr 1],M,MPrime);
      while l > 0 do begin
        Mont(R,R,M,MPrime);
        Dec(l);
      end;
    end;
    R2 := R;
    R := nil;
    MPRealloc(X,1);
    X^.Data[0] := 1;
    X^.Sign := 1;
    Mont2(R2,X,M,MPrime,R);
  finally
    for i := 1 to (1 shl (k-1)) - 1 do
      MPDealloc(WG[i]);
    WG := nil;
    if FreeG then MPDealloc(G);
    MPDealloc(X);
    MPDealloc(R2);
    MPDealloc(RQuad);
  end;
end;

function MPLegendreSymbol(X, P: PMPInteger): Integer;
var
  E, Y: PMPInteger;
begin
  E := nil;
  try
    MPDec2(P,E);
    MPShr(E,1);
    Y := nil;
    try
      MPExpMod(X,E,P,Y);
      if MPMSB(Y) = 0 then
        Result := 0
      else if MPMSB(Y) = 1 then
        Result := 1
      else begin
        MPSub2(P,Y,E);
        Assert(MPMSB(E) = 1);
        Result := -1;
      end;
    finally
      MPDealloc(Y);
    end;
  finally
    MPDealloc(E);
  end;
end;

function MPJacobiSymbol(A, N: PMPInteger): Integer;
var
  X, Y, T, tmp: PMPInteger;
begin
  X := MPCopy(A);
  Y := MPCopy(N);
  T := nil;
  Result := 1;
  while MPMSB(Y) > 1 do begin
    MPMod(X,Y);
    MPSub2(Y,X,T);
    if MPCmpOffset(X,T) > 0 then begin
      MPCopy2(T,X);
      if Y^.Data[Y^.Size - 1] and 3 = 3 then
        Result := -Result;
    end;
    if MPMSB(X) = 0 then begin
      Result := 0;
      Break;
    end else begin
      while X^.Data[X^.Size - 1] and 3 = 0 do
        MPShr(X,2);
      if X^.Data[X^.Size - 1] and 1 = 0 then begin
        MPShr(X,1);
        if (Y^.Data[Y^.Size - 1] and 7) in [3,5] then
          Result := -Result;
      end;
      if (X^.Data[X^.Size - 1] and 3 = 3) and
         (Y^.Data[Y^.Size - 1] and 3 = 3) then
        Result := -Result;
    end;
    tmp := X;
    X := Y;
    Y := tmp;
  end;
  MPDealloc(X);
  MPDealloc(Y);
  MPDealloc(T);
end;

procedure MPLucasSequence(N, P, Q, K: PMPInteger; var V, QR: PMPInteger);
var
  v0,v1,q0,q1,t: PMPInteger;
  Bit, LW: LongWord;
  I, J: Cardinal;
begin
  Integer(I) := MPMSB(K);
  if I = 0 then
    raise Exception.Create('K must be non-zero');
  Bit := 1 shl ((I - 1) and $1F);
  I := ((I - 1) shr 5) + 1;

  v0 := IntToMPInt(2);
  v1 := MPCopy(P);
  q0 := IntToMPInt(1);
  q1 := IntToMPInt(1);
  t := nil;

  for J := I downto 1 do begin
    LW := K^.Data[K^.Size - J];
    repeat
      MPMulMod(q0,q1,N);
      if (LW and Bit) = Bit then begin
        // q1 <- q0*Q mod N
        MPMulMod2(q0,Q,N,q1);
        // v0 <- v0*v1 - P*q0 mod N
        MPMul2(P,q0,t);
        MPMul(v0,v1);
        MPSub(v0,t);
        MPMod(v0,N);
        // v1 <- v1*v1 - 2*q1 mod N
        MPShl2(q1,1,t);
        MPMul(v1,v1);
        MPSub(v1,t);
        MPMod(v1,N);
      end else begin
        MPCopy2(q0,q1);
        // v1 <- v0*v1 - P*q0 mod N
        MPMul2(P,q0,t);
        MPMul(v1,v0);
        MPSub(v1,t);
        MPMod(v1,N);
        // v0 <- v0*v0 - 2*q0 mod N
        MPShl2(q0,1,t);
        MPMul(v0,v0);
        MPSub(v0,t);
        MPMod(v0,N);
      end;
      Bit := Bit shr 1;
    until Bit = 0;
    Bit := $80000000;
  end;     

  MPDealloc(V);
  V := v0;
  MPDealloc(QR);
  QR := q0;

  MPDealloc(q1);
  MPDealloc(v1);
  MPDealloc(t);
end;

function MPSqrtModPrime2(G, P: PMPInteger; var Z: PMPInteger): Boolean;
var
  k, t, gamma, iota, V, Q: PMPInteger;
  i: Integer;
begin
  if (P^.Data[P^.Size - 1] and $3) = 3 then begin
    k := MPCopy(P);
    MPShr(k,2);
    MPAddInt(k,1);
    MPExpMod(G,k,P,Z);
    MPDealloc(k);
    Result := True;
  end else if (P^.Data[P^.Size - 1] and $7) = 5 then begin
    k := MPCopy(P);
    MPShr(k,3);
    // gamma <- (2g)**k
    t := nil;
    MPShl2(G,1,t);
    MPExpMod(t,k,P,gamma);
    // iota <- 2g*gamma*gamma mod P
    iota := nil;
    MPMulMod2(gamma,gamma,P,iota);
    MPShl(iota,1);
    MPMulMod(iota,G,P);
    // Z <- g*gamma*(iota-1) mod P
    MPCopy2(iota,Z);
    MPSubInt(Z,1);
    MPMulMod(Z,gamma,P);
    MPMulMod(Z,G,P);
    MPDealloc(iota);
    MPDealloc(gamma);
    MPDealloc(t);
    MPDealloc(k);
    Result := True;
  end else if (P^.Data[P^.Size - 1] and $7) = 1 then begin
    k := MPCopy(P);
    MPAddInt(k,1);
    MPShr(k,1);
    V := nil;
    Q := nil;
    i := 2;
    repeat
      t := IntToMPInt(i);
      MPLucasSequence(P,t,G,k,V,Q);
      MPDealloc(t);
      // Z <- (V div 2) mod P
      MPCopy2(V,Z);
      if (Z^.Data[Z^.Size - 1] and 1) = 1 then
        MPAdd(Z,P);
      MPShr(Z,1);
      // if z*z = G (mod P) then done
      MPMul2(Z,Z,V);
      MPSub(V,G);
      MPMod(V,P);
      Result := (MPMSB(V) = 0);
      if Result then Break;
      if MPMSB(Q) > 1 then begin
        MPSub(Q,P);
        if MPMSB(Q) > 1 then begin
          Z^.Sign := 0;
          Break;
        end;
      end;
      Inc(i);
    until False;
    MPDealloc(k);
    MPDealloc(Q);
    MPDealloc(V);
  end else
    raise Exception.Create('MPSqrtModPrime2: An even integer cannot be a prime.');
end;

procedure MPGarnerCRT(var X: PMPInteger; const M, V: array of PMPInteger);
var
  C: array of PMPInteger;
  I, J: Integer;
  Ci, U, MP: PMPInteger;
begin
  if High(M) - Low(M) <> High(V) - Low(V) then
    raise Exception.Create('MPGarnerCRT: Operand size mismatch');

  SetLength(C,High(M) - Low(M) + 1);
  FillChar(C[0],Length(C)*4,0);
  U := nil;
  MP := nil;
  Ci := nil;
  try
    for I := Low(M) + 1 to High(M) do begin
      MPRealloc(Ci,1);
      Ci^.Data[0] := 1;
      Ci^.Sign := 1;
      for J := Low(M) to I - 1 do begin
        MPInvMod(M[J],M[I],U);
        if (U = nil) or (U^.Sign = 0) then
          raise Exception.Create(Format('MPGarnerCRT: Residue number %d and %d have a divisor in common',[I,J]));
        MPMulMod(Ci,U,M[I]);
      end;
      C[I - Low(M)] := Ci;
      Ci := nil;
    end;

    X := MPCopy(V[Low(V)]);

    MPRealloc(MP,1);
    MP^.Data[0] := 1;
    MP^.Sign := 1;
    for I := Low(M) + 1 to High(M) do begin
      MPDealloc(U);
      U := MPCopy(V[I - Low(M) + Low(V)]);
      MPSub(U,X);
      MPMulMod(U,C[I - Low(M)],M[I]);

      MPMul(MP,M[I-1]);
      MPMul(U,MP);
      MPAdd(X,U);
    end;

  finally
    MPDealloc(MP);
    MPDealloc(U);
    MPDealloc(Ci);
    for I := 1 to Length(C) - 1 do
      MPDealloc(C[I]);
    C := nil;
  end;
end;

procedure MPRandom(var X: PMPInteger; HighBit: Cardinal);
var
  Size: Cardinal;
  BC: LongWord;
begin
  if HighBit = 0 then
    raise Exception.Create('HighBit must be non-zero.');

  MPRawRandom(X,HighBit - 1);

  Size := (HighBit + 31) shr 5;
  if Size <> X^.Size then
    MPRealloc(X,Size);
  BC := 1 shl ((HighBit - 1) and $1F);
  X^.Data[X^.Size - Size] := X^.Data[X^.Size - Size] or BC;
end;

procedure MPRandomBound(var X: PMPInteger; XMin, XUBound: PMPInteger);
var
  D: PMPInteger;
  BSize: Cardinal;
begin
  // Generate random X such that XMin <= X < XUBound
  if Assigned(XMin) then begin
    Assert(MPCmpOffset(XMin,XUBound,0) < 0,'XMin must be less than XUBound');
    D := MPCopy(XUBound);
    MPSub(D,XMin);
    MPRandomBound(X,nil,D);
    MPDealloc(D);
    MPAdd(X,XMin);
  end else begin
    BSize := MPMSB(XUBound);
    MPRawRandom(X,2*BSize);
    MPMul(X,XUBound);
    MPShr(X,2*BSize);
    MPRealloc(X,(BSize + 31) shr 5);
  end;
end;

function ByteSwap(Value: LongWord): Longword;
asm
  bswap EAX
end;     

function ByteSwap(Value: Int64): Int64;
asm
  mov EAX,dword ptr [Value + 4]
  mov EDX,dword ptr [Value]
  bswap EAX
  bswap EDX
end;

procedure MPDSARandom(var X: PMPInteger; Seed: PMPInteger; var j: Cardinal; l: Integer);
{$IFDEF SHA1}
var
  S, W: PMPInteger;
  n, LL, i: Cardinal;
  H: TSHA1;
{$ENDIF SHA1}
begin
{$IFDEF SHA1}
  LL := 512 + 64*l;
  n := (LL - 1) div 160;
  S := MPCopy(Seed);
  try
    MPAddInt(S,j);
    W := nil;
    MPRealloc(W,(n + 1) * 5);
    try
      for i := 0 to n do begin
        H := TSHA1.Create(S^.Data,S^.Size*4);
        try
          H.Done(@W^.Data[i*5]);
        finally
          H.Free;
        end;
        MPAddInt(S,1);
      end;
      j := j + n + 1;

      n := LL shr 5;
      MPRealloc(X,n);
      for i := 0 to n-1 do
        X^.Data[i] := ByteSwap(W^.Data[i]);
      X^.Data[0] := X^.Data[0] or $80000000;
      X^.Sign := 1;
    finally
      MPDealloc(W);
    end;
  finally
    MPDealloc(S);
  end;      
{$ELSE  SHA1}
  raise Exception.Create('The implementation made an attempt to use a feature that requires SHA-1');
{$ENDIF SHA1}
end;

function MPTrialDiv(N: PMPInteger; T: Integer): Boolean;
var
  J: Integer;
  Q: PMPInteger;
  P: LongWord;
begin
  // Trial division:
  Result := False;
  if T >= -1 then begin
    if (T < 0) or (Cardinal(T) > SPrimeCount) then T := SPrimeCount;
    for J := 0 to T-1 do begin
      if MPModByInt(N,Primes[J]) = 0 then
        Exit;
    end;
    Result := True;
  end else if T = -2 then begin
    T := SPrimeCount;
    Q := MPCopy(N);
    try
      MPShr(Q,1);
      for J := 0 to T-1 do begin
        P := Primes[J];
        Result := (MPModByInt(N,P) <> 0) and
                  (MPModByInt(Q,P) <> 0);
        if not Result then Break;
      end;
    finally
      MPDealloc(Q);
    end;
  end;
end;

function MPTrialDivBound(N: PMPInteger; B: Cardinal): Boolean;
var
  J: Integer;
  P: LongWord;
begin
  // Trial division:
  Result := False;
  for J := 0 to SPrimeCount - 1 do begin
    P := Primes[J];
    if P >= B then Break;
    if MPModByInt(N,P) = 0 then
      Exit;
  end;
  Result := True;
end;

function MillerRabinTest(var N, Y, NMin1: PMPInteger; S: Integer): Boolean;
var
  J: Integer;
  Dummy: PMPInteger;
begin
  Dummy := nil;
  try
    if (MPMSB(Y) <> 1) and (MPCmpOffset(Y,Nmin1) <> 0) then begin
      Result := False;
      if S = 1 then Exit;
      J := 1;
      while (J < S) and (MPCmpOffset(Y,Nmin1) <> 0) do begin
        MPMul(Y,Y);
        MPDiv(Y,N,Dummy);
        if MPMSB(Y) = 1 then Exit;
        Inc(J);
      end;
      Result := (MPCmpOffset(Y,Nmin1) = 0);
    end else
      Result := True;
  finally
    MPDealloc(Dummy);
  end;
end;

function MPMillerRabin(N: PMPInteger; T: Integer): Boolean;
const
  BitsPerInt = 32;
var
  Nmin1, R, Y, A: PMPInteger;
  S, J, I: Cardinal;
  Bit: LongWord;
begin
  Result := (N^.Data[N^.Size - 1] and 1) = 1;
  if not Result then Exit;
  Result := MPTrialDiv(N,2048);
  if (T = 0) or not Result then Exit;

  // Write N-1 = (2**s)*R such that R is odd.
  NMin1 := nil;
  MPDec2(N,NMin1);
  J := 0;
  Bit := 1;
  S := 0;
  while ((NMin1^.Data[NMin1^.Size - 1 - J] and Bit) = 0) and (J < NMin1^.Size) do begin
    Inc(S);
    if (S and 31) = 0 then begin
      Inc(J);
      Bit := 1;
    end else
      Bit := Bit + Bit;
  end;
  R := nil;
  MPRealloc(R,N^.Size);
  R^.Sign := 1;
  for I := 0 to NMin1^.Size-J-1 do
    R^.Data[I+J] := NMin1^.Data[I];
  MPShr(R,S-J*BitsPerInt);

  // Compute Y = 2**R mod N
  Y := nil;
  A := IntToMPInt(2);
  try
    MPPow2Mod(R,N,Y);
  finally
    MPDealloc(A);
  end;
  Result := MillerRabinTest(N,Y,Nmin1,S);
  if not Result then begin
    MPDealloc(Y);
    MPDealloc(NMin1);
    MPDealloc(R);
    Exit;
  end;
  if T > 1 then
    for I := 1 to T-1 do begin
      A := nil;
      try
        MPRandom(A,MPMSB(N)-1);
        MPExpMod(A,R,N,Y);
      finally
        MPDealloc(A);
      end;
      Result := MillerRabinTest(N,Y,Nmin1,S);
      if not Result then begin
        MPDealloc(Y);
        MPDealloc(NMin1);
        MPDealloc(R);
        Exit;
      end;
    end;
  MPDealloc(Y);
  MPDealloc(NMin1);
  MPDealloc(R);
end;

function MPFastMillerRabin(N: PMPInteger; T: Integer): Boolean;
const
  BitsPerInt = 32;
var
  Nmin1, R, Y: PMPInteger;
  S, J, I: Cardinal;
  Bit: LongWord;
begin
  Result := (N^.Data[N^.Size - 1] and 1) = 1;
  if (T = 0) or not Result then Exit;

  // Write N-1 = (2**s)*R such that R is odd.
  NMin1 := nil;
  MPRealloc(NMin1,N^.Size);
  for J := 0 to N^.Size - 1 do
    NMin1^.Data[J] := N^.Data[J];
  NMin1^.Sign := 1;
  MPSubInt(NMin1,1);
  J := 0;
  Bit := 1;
  S := 0;
  while ((NMin1^.Data[NMin1^.Size - 1 - J] and Bit) = 0) and (J < NMin1^.Size) do begin
    Inc(S);
    if (S mod 32) = 0 then begin
      Inc(J);
      Bit := 1;
    end else
      Bit := Bit shl 1;
  end;
  R := nil;
  MPRealloc(R,N^.Size);
  R^.Sign := 1;
  for I := 0 to NMin1^.Size-J-1 do
    R^.Data[I+J] := NMin1^.Data[I];
  MPShr(R,S-J*BitsPerInt);

  // Compute Y = 2**R mod N
  Y := nil;
  MPKaratsubaPow2Mod(R,N,Y);
  Result := MillerRabinTest(N,Y,Nmin1,S);
  if not Result then begin
    MPDealloc(Y);
    MPDealloc(NMin1);
    MPDealloc(R);
    Exit;
  end;
  if T > 1 then
    for I := 1 to T-1 do begin
      MPKaratsubaPowBMod(Primes[I-1],R,N,Y);
      Result := MillerRabinTest(N,Y,Nmin1,S);
      if not Result then begin
        MPDealloc(Y);
        MPDealloc(NMin1);
        MPDealloc(R);
        Exit;
      end;
    end;
  MPDealloc(Y);
  MPDealloc(NMin1);
  MPDealloc(R);
end;

procedure MPMaurerPrime(var P: PMPInteger; BitSize: Cardinal; Params: PMaurerParams);
var
  Bound: Cardinal;
  d: Extended;
  q, x, y, a, b, r: PMPInteger;
  Success: Boolean;

  function SmallPrime(BitSize: Cardinal): LongWord;
  var
    P: LongWord;
    I, J, K: Integer;
  begin
    repeat
      P := Random(1 shl BitSize) or (1 shl (BitSize-1)) or 1;
      J := 0;
      I := SPrimeCount - 1;
      while (I > J) and (Primes[I] <> P) do begin
        if Primes[J] = P then
          I := J
        else begin
          K := (I + J) shr 1;
          if Primes[K] > P then
            I := K - 1
          else if Primes[J] < P then
            J := K + 1
          else
            I := K;
        end;
      end;
    until Primes[I] = P;
    Result := P;
  end;

begin
  if BitSize <= SPrimeBoundBitSize then begin
    MPRealloc(P,1);
    P^.Data[0] := SmallPrime(BitSize);
    P^.Sign := 1;
  end else if BitSize <= 2*SPrimeBoundBitSize then begin
    repeat
      MPRandom(P,BitSize);
      P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 1;
    until MPTrialDiv(P,-1);
  end else begin
    Bound := Trunc(0.1 * BitSize * BitSize) + 1;
    if BitSize <= 2*SPrimeBoundBitSize then
      d := 0.5
    else
      repeat
        d := Exp((Random-1)*ln(2));
      until BitSize - d*BitSize > SPrimeBoundBitSize;
    q := nil;
    MPMaurerPrime(q,Trunc(d*BitSize) + 1,nil);
    y := nil;
    MPRealloc(y,1);
    y^.Data[0] := 1;
    y^.Sign := 1;
    MPShl(y,BitSize-2);
    x := nil;
    if MPMSB(q) <= 32 then
      MPDivByInt2(y,q^.Data[q^.Size - 1],x)
    else
      MPDiv(y,q,x);
    a := nil;
    r := nil;
    b := nil;
    Success := False;
    repeat
      MPRandom(r,BitSize);
      MPMul(r,x);
      MPShr(r,BitSize-1);
      MPDealloc(P);
      P := MPCopy(r);
      MPMul(P,q);
      MPShl(P,1);
      MPAddInt(P,1);
      MPRealloc(P,(BitSize + 31) shr 5);
      if MPTrialDivBound(P,Bound) then begin
        MPDealloc(y);
        y := MPCopy(P);
        MPSubInt(y,1);
        repeat
          MPRandom(a,BitSize+1);
          MPMul(a,y);
          MPShr(a,BitSize);
          MPSub(a,y);
        until MPMSB(a) >= 2;
        MPExpMod(a,y,P,b);
        if MPMSB(b) = 1 then begin
          MPDealloc(y);
          y := MPCopy(r);
          MPShl(y,1);
          MPExpMod(a,y,P,b);
          MPSubInt(b,1);
          if (MPMSB(b) > 0) and (b^.Sign > 0) then begin
            MPBinaryGCD(b,p,y);
            Success := (MPMSB(y) = 1);
          end;
        end;
      end;
    until Success;

    if Params = nil then
      MPDealloc(q)
    else
      Params^.q := q;
    MPDealloc(x);
    MPDealloc(y);
    MPDealloc(a);
    MPDealloc(b);
    MPDealloc(r);
  end;
end;

procedure MPPrimeCongruenceCondition(var P: PMPInteger; BitSize: Cardinal; A, R: PMPInteger; Params: PPrimeParams = nil);
var
  B, K, KMin, KMax, D: PMPInteger;
  T: Integer;
begin 
  // Generate random prime P such that P = A (mod R).
  // Algorithm IEEE P1363: A.15.7
  case BitSize of
    1..163: T := 34;
    164..256: T := 32;
    257..512: T := 16;
    513..768: T := 8;
    769..1024: T := 6;
    1025..1536: T := 4;
    1537..2048: T := 3;
  else
    T := 2;
  end;
  if Assigned(A) then
    B := MPCopy(A)
  else begin
    B := nil;
    MPRealloc(B,1);
    B^.Sign := 1;
    B^.Data[0] := 1;
  end;
  if (B^.Data[B^.Size - 1] and 1) = 0 then
    MPAdd(B,R);
  if Assigned(Params) and
     Assigned(Params^.PMin) and Assigned(Params^.PUBound) then begin
    K := MPCopy(Params^.PMin);
    MPSubInt(K,1);
    MPShr(K,1);
    KMin := nil;
    MPDiv(K,R,KMin);
    if MPMSB(K) > 0 then
      MPAddInt(KMin,1);
    MPDealloc(K);
    K := MPCopy(Params^.PUBound);
    MPSubInt(K,1);
    MPShr(K,1);
    KMax := nil;
    MPDiv(K,R,KMax);
    MPAddInt(KMax,1);
  end else begin
    K := nil;
    MPRealloc(K,1);
    K^.Data[0] := 1;
    K^.Sign := 1;
    MPShl(K,BitSize-1);
    MPSubInt(K,1);
    MPShr(K,1);
    KMin := nil;
    MPDiv(K,R,KMin);
    if MPMSB(K) > 0 then
      MPAddInt(KMin,1);

    MPRealloc(K,1);
    K^.Data[0] := 1;
    K^.Sign := 1;
    MPShl(K,BitSize);
    MPSubInt(K,1);
    MPShr(K,1);
    KMax := nil;
    MPDiv(K,R,KMax);
    MPAddInt(KMax,1);
  end;
  D := nil;
  repeat
    MPRandomBound(K,KMin,KMax);
    MPMul2(K,R,P);
    MPShl(P,1);
    MPAdd(P,B);
    if Assigned(Params) and Assigned(Params^.F) then begin
      MPCopy2(P,K);
      MPSubInt(K,1);
      MPBinaryGCD(K,Params^.F,D);
      if MPMSB(D) <> 1 then Continue;
    end;
    MPRealloc(P,(MPMSB(P) + 31) shr 5);
  until MPMillerRabin(P,T);
  MPDealloc(D);
  MPDealloc(K);
  MPDealloc(KMin);
  MPDealloc(KMax);
  MPDealloc(B);
end;

procedure MPGordonStrongPrime(var P: PMPInteger; BitSize: Cardinal; Params: PPrimeParams = nil);
var
  S, C, R, P0, JMin, JMax, J0, D: PMPInteger;
  T, T0: Integer;
  j: Cardinal;
  i: LongWord;
  Found: Boolean;
begin
  case BitSize of
    1..163: T := 34;
    164..256: T := 32;
    257..512: T := 16;
    513..768: T := 8;
    769..1024: T := 6;
    1025..1536: T := 4;
    1537..2048: T := 3;
  else
    T := 2;
  end;

  S := nil;
  MPPrime(S,(BitSize shr 1) - 15,paStandard);
  C := nil;
  MPPrime(C,(BitSize shr 1) - 47,paStandard);

  RawRandom(i,30);
  i := (ByteSwap(i) or $40000000) shl 1;
  case (BitSize shr 1) - 16 of
    1..256: T0 := 32;
    257..512: T0 := 16;
    513..768: T0 := 8;
    769..1024: T0 := 6;
    1025..1536: T0 := 4;
  else
    T0 := 2;
  end;
  R := nil;
  MPRealloc(R,C^.Size + 1);
  R^.Sign := 1;
  repeat
    RawMulByInt(C,i,R);
    MPAddInt(R,1);
    Inc(i,2);
  until MPMillerRabin(R,T0);

  MPDealloc(C);
  C := MPCopy(R);
  MPSubInt(C,2);
  P0 := nil;
  MPExpMod(S,C,R,P0);
  MPMul(P0,S);
  MPShl(P0,1);
  MPSubInt(P0,1);

  MPDealloc(C);
  C := MPCopy(R);
  MPMul(C,S);

  if Assigned(Params) and Assigned(Params^.PMin) then
    J0 := MPCopy(Params^.PMin)
  else begin
    J0 := nil;
    MPRealloc(J0,1);
    J0^.Data[0] := 1;
    J0^.Sign := 1;
    MPShl(J0,BitSize - 1);
  end;
  JMin := nil;
  MPDiv(J0,C,JMin);
  MPAddInt(JMin,1);
  if Assigned(Params) and Assigned(Params^.PUBound) then
    J0 := MPCopy(Params^.PUBound)
  else begin
    MPRealloc(J0,1);
    J0^.Data[0] := 1;
    J0^.Sign := 1;
    MPShl(J0,BitSize);
  end;
  JMax := nil;
  MPDiv(J0,C,JMax);
  MPAddInt(JMax,1);

  D := nil;
  repeat
    MPRandomBound(J0,JMin,JMax);
    j := 0;
    repeat
      MPRealloc(P,J0^.Size);
      P^.Sign := 1;
      MPMul2(C,J0,P);
      MPAdd(P,P0);
      i := MPMSB(P);
      MPAddInt(J0,2);
      if Assigned(Params) and Assigned(Params^.F) then begin
        P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] and not 1;
        MPBinaryGCD(P,Params^.F,D);
        Found := MPMSB(D) = 1;
        if Found then
          Found := MPMillerRabin(P,T);
      end else
        Found := MPMillerRabin(P,T);
      Inc(j);
    until (i <> BitSize) or (j = 256) or Found;
  until (i = BitSize) and Found;
  MPRealloc(P,(BitSize + 31) shr 5);
  MPDealloc(S);
  MPDealloc(C);
  MPDealloc(R);
  MPDealloc(P0);
  MPDealloc(J0);
  MPDealloc(JMax);
  MPDealloc(JMin);
  MPDealloc(D);
end;

procedure MPStrongPrime(var P: PMPInteger; BitSize: Cardinal; Params: PStrongPrimeParams = nil);
var
  T, R, S, U, V, A, RS, M, B, K: PMPInteger;
  LT, LR, LS: Cardinal;
begin
  if Assigned(Params) then begin
    LT := Params^.LT;
    LR := Params^.LR;
    LS := Params^.LS;
  end else begin
    LT := BitSize div 3;
    LR := 2*BitSize div 5;
    LS := 2*BitSize div 5;
  end;
  T := nil;
  MPPrime(T,LT,paStandard);
  R := nil;
  MPPrimeCongruenceCondition(R,LR,nil,T);
  S := nil;
  MPPrime(S,LS,paStandard);

  U := nil;
  MPInvMod(S,R,U);
  V := nil;
  MPInvMod(R,S,V);

  A := nil;
  MPMul2(S,U,A);
  MPMul(V,R);
  MPSub(A,V);
  RS := nil;
  MPMul2(R,S,RS);
  MPMod(A,RS);
  if Assigned(Params) and (Params.Mod8 < 8) then begin
    B := IntToMPInt(Params.Mod8);
    M := IntToMPInt(8);
    K := nil;
    MPInvMod(M,RS,K);
    MPMul(K,M);
    MPMul(A,K);

    MPMul(B,B);
    MPMul(B,RS);

    MPAdd(A,B);

    MPMul(RS,M);

    MPMod(A,RS);

    MPDealloc(B);
    MPDealloc(K);
    MPDealloc(M);
  end;
  MPPrimeCongruenceCondition(P,BitSize,A,RS,Pointer(Params));

  MPDealloc(RS);
  MPDealloc(A);
  MPDealloc(V);
  MPDealloc(U);
  MPDealloc(S);
  MPDealloc(R);
  MPDealloc(T);
end;

procedure MPPrimeFast(var P: PMPInteger; BitSize: Cardinal; Params: PPrimeParams; T: Integer);
const
  MaxDelta = 512;
var
  I, J, ModCount, Pr, Res, K: LongWord;
  Mods: array of LongWord;
  Delta, PrevDelta: LongWord;
  OK, Done, Check: Boolean;
  NextSet: set of 0..MaxDelta shr 1 - 1;
  D: PMPInteger;

  function InRange: Boolean;
  begin
    if Assigned(Params) and Assigned(Params.PMin) and Assigned(Params.PUBound) then
      Result := (MPCmpOffset(P,Params.PMin) >= 0) and (MPCmpOffset(P,Params.PUBound) < 0)
    else
      Result := MPMSB(P) = LongInt(BitSize);
  end;

begin
  if BitSize > 1024 then begin
    ModCount := (BitSize div 32);
    ModCount := 2* ModCount * ModCount;
    if ModCount > SPrimeCount then
      ModCount := SPrimeCount;
  end else
    ModCount := 2048;
  SetLength(Mods,ModCount);
  repeat
    if Assigned(Params) and Assigned(Params.PMin) and Assigned(Params.PUBound) then
      MPRandomBound(P,Params.PMin,Params.PUBound)
    else
      MPRandom(P,BitSize);
    P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 1;
    for I := 0 to ModCount - 1 do begin
      Pr := Primes[I];
      Mods[I] := Pr - MPModByInt(P,Pr);
    end;
    K := 0;
    repeat
      NextSet := [0..MaxDelta shr 1 - 1];
      for I := 0 to 95 do begin
        Pr := Primes[I];
        for J := 1 to MaxDelta shr 1 do
          if (J*2 + (Pr - Mods[I])) mod Pr = 0 then
            Exclude(NextSet,J-1);
      end;
      if NextSet <> [] then begin
        for I := 96 to ModCount - 1 do begin
          Res := Mods[I];
          Assert(Res > 0);
          if (Res <= MaxDelta) and not Odd(Res) then
            Exclude(NextSet,(Res shr 1) - 1);
          if NextSet = [] then Break;
        end;
      end;
      PrevDelta := 0;
      OK := False;
      Done := False;
      if NextSet <> [] then begin
        for J := 0 to MaxDelta shr 1 - 1 do
          if J in NextSet then begin
            Delta := (J + 1)*2;
            MPAddInt(P,Delta - PrevDelta);
            PrevDelta := Delta;
            Check := True;
            if Assigned(Params) and Assigned(Params.F) then begin
              P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] and not 1;
              if Params^.Mod8 < 8 then
                Check := (P^.Data[P^.Size - 1] and 8) = Params^.Mod8;
              if Check then begin
                D := nil;
                MPBinaryGCD(P,Params^.F,D);
                Check := MPMSB(D) = 1;
                MPDealloc(D);
              end;
              P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 1;
            end;
            if Check then begin
//              Assert(MPTrialDiv(P,ModCount));
              OK := {MPTrialDiv(P,2048) and} MPFastMillerRabin(P,T);
              Done := OK or not InRange;
              if Done then Break;
            end else
              OK := False;
          end;
      end;
      Inc(K);
      Done := Done or (K = $1000);
      if not Done then begin
        if PrevDelta < MaxDelta then
          MPAddInt(P,MaxDelta - PrevDelta);
        for I := 0 to ModCount - 1 do begin
          Res := Mods[I] - MaxDelta;
          if Res <= 0 then begin
            Pr := Primes[I];
            repeat
              Inc(Res,Pr);
            until Res > 0;
          end;
          Mods[I] := Res;
        end;
      end;
    until Done;
  until OK and InRange;
end;

procedure MPPrime(var P: PMPInteger; BitSize: Cardinal; Algo: TPrimeAlgo; Params: Pointer = nil);
var
  Q, S, C, R, U, Dummy, D, SMin, SMax
 {$IFDEF FASTRND}, MPSeed{$ENDIF}: PMPInteger;
  T: Integer;
  PrParams: PPrimeParams;
  SubGroupParams: PSubGroupParams;
  i, l: Cardinal;
  Found: Boolean;
{$IFDEF SHA1}
  X: PMPInteger;
  DSAParams: PDSAParams;
  H: TSHA1;
  j: Cardinal;
  Seed, Seed1: string;
{$ENDIF SHA1}

 {$IFDEF FASTRND}
  procedure FastRandom(Seed: PMPInteger; var X: PMPInteger; BitSize: Cardinal);
  var
    H: TSHA1;
    I, Size: Cardinal;
    Mask: LongWord;
    D: string;
  begin
    Size := (BitSize + 32*5-1) shr 5;
    MPRealloc(X,Size);
    H := TSHA1.Create(Seed^.Data[0],Seed^.Size*4);
    try
      for I := 0 to (Size div 5) - 1 do begin
        D := H.Digest;
        H.Done(@X^.Data[I*5]);
        H.SetUpContext(Pointer(D));
        H.HashData(X^.Data[I*5],20);
      end;
    finally
      H.Free;
    end;
    MPInc(Seed);
    Size := (BitSize + 31) shr 5;
    for I := 0 to 3 do begin
      if X^.Size - I = Size then
        Break;
      X^.Data[I] := 0;
    end;
    if BitSize and 31 > 0 then begin
      Mask := (1 shl (BitSize and 31)) - 1;
      X^.Data[0] := X^.Data[0] and Mask;
    end;
    MPRealloc(X,Size);
    X^.Data[0] := X^.Data[0] or (1 shl ((BitSize-1) and 31));
  end;
  {$ENDIF}

begin
  case BitSize of
    1..163: T := 34;
    164..256: T := 32;
    257..512: T := 16;
    513..768: T := 8;
    769..1024: T := 6;
    1025..1536: T := 4;
    1537..2048: T := 3;
  else
    T := 2;
  end;

  if Algo = paStandard then begin
    if Assigned(Params) then begin
      D := nil;
      PrParams := Params;
      if Assigned(PrParams^.PMin) and Assigned(PrParams^.PUBound) then begin
        repeat
          MPRandomBound(P,PrParams^.PMin,PrParams^.PUBound);
          if Assigned(PrParams^.F) then begin
            P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] and not 1;
            if PrParams^.Mod8 < 8 then
              if (P^.Data[P^.Size - 1] and 7) <> PrParams^.Mod8 then Continue;
            MPBinaryGCD(P,PrParams^.F,D);
            if MPMSB(D) <> 1 then Continue;
          end;
          P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 1;
        until MPMillerRabin(P,T);
      end else
        repeat
          MPRandom(P,BitSize);
          if Assigned(PrParams^.F) then begin
            P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] and not 1;
            if PrParams^.Mod8 < 8 then
              if (P^.Data[P^.Size - 1] and 7) <> PrParams^.Mod8 then Continue;
            MPBinaryGCD(P,PrParams^.F,D);
            if MPMSB(D) <> 1 then Continue;
          end;
          P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 1;
        until MPMillerRabin(P,T);
      MPDealloc(D);
    end else begin
    {$IFDEF FASTRND}
      MPSeed := nil;
      MPRandom(MPSeed,20);
      repeat
        FastRandom(MPSeed,P,BitSize);
        P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 1;
      until MPMillerRabin(P,T);
      MPDealloc(MPSeed);
    {$ELSE}
      repeat
        MPRandom(P,BitSize);
        P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 1;
      until MPMillerRabin(P,T);
    {$ENDIF}
    end;

  end else if Algo = paStandardFast then begin
    MPPrimeFast(P,BitSize,Params,T);

  end else if Algo = paSafePrime then begin
    Dummy := nil;
    R := nil;
    Q := nil;
    try
      repeat
        repeat
          repeat
            MPRandom(P,BitSize);
            P^.Data[P^.Size - 1] := P^.Data[P^.Size - 1] or 3;
          until MPTrialDiv(P,-2);
          MPDealloc(Q);
          Q := MPCopy(P);
          MPShr(Q,1);
          if MPTrialDiv(P,1024) then begin
            MPPow2Mod(Q,P,R);
            Found := (MPMSB(R) > 1);
          end else
            Found := False;
        until Found;
      until MPMillerRabin(Q,T) and MPMillerRabin(P,T);
    finally
      MPDealloc(Dummy);
      MPDealloc(Q);
      MPDealloc(R);
    end;

  end else if Algo = paSubGroup then begin
    SubGroupParams := Params;
    if Assigned(SubGroupParams) then begin
      l := SubGroupParams^.l;
      if Assigned(SubGroupParams^.PUBound) then
        BitSize := MPMSB(SubGroupParams^.PUBound);
    end else
      l := 0;
    if l*2 > BitSize then
      raise Exception.Create('The sub group size must not exceed 2^(BitSize/2)');
    S := nil;
    Q := nil;
    R := nil;
    C := nil;
    U := nil;
    SMin := nil;
    SMax := nil;
    try
      repeat
        if Assigned(SubGroupParams) and Assigned(SubGroupParams^.q) then
          Q := SubGroupParams^.q
        else if l > 96 then
          MPPrime(Q,l,paStandardFast)
        else                         
          MPPrime(Q,l,paStandard);
        if BitSize > l*3 then begin
          if l > 96 then
            MPPrime(R,l,paStandardFast)
          else if BitSize > l*3 then
            MPPrime(R,l,paStandard);
          MPMul2(Q,R,C);
        end else
          MPCopy2(Q,C);
        if BitSize - l*3 > l then begin
          if BitSize - l*3 > l*3 then
            MPPrime(U,l*3,paStandardFast)
          else if BitSize - l*3 > 96 then
            MPPrime(U,BitSize - l*3,paStandardFast)
          else if BitSize - l*3 > l then
            MPPrime(U,BitSize - l*3,paStandard);
          MPMul(C,U);
        end;
        if Assigned(SubGroupParams) and Assigned(SubGroupParams^.PMin) then
          MPCopy2(SubGroupParams^.PMin,S)
        else begin
          MPRealloc(S,1);
          S^.Sign := 1;
          S^.Data[0] := 1;
          MPShl(S,BitSize-2);
        end;
        MPDiv(S,C,SMin);
        MPAddInt(SMin,1);
        if Assigned(SubGroupParams) and Assigned(SubGroupParams^.PUBound) then
          MPCopy2(SubGroupParams^.PUBound,S)
        else begin
          MPRealloc(S,1);
          S^.Sign := 1;
          S^.Data[0] := 1;
          MPShl(S,BitSize-1);
        end;
        MPDiv(S,C,SMax);
        MPAddInt(SMax,1);
        i := 0;
        repeat
          MPRandomBound(S,SMin,SMax);
          MPShl(S,1);
          MPMul2(S,C,P);
          MPRealloc(P,(BitSize+31) shr 5);
          MPAddInt(P,1);
          Found := MPTrialDiv(P,2048) and MPFastMillerRabin(P,T);
          Inc(i);
        until Found or (i = 4096);
        MPMul2(Q,Q,R);
        MPCopy2(P,S);
        MPDiv(S,R,U);
        Found := MPMSB(S) > 1;
      until Found;
      if Assigned(SubGroupParams) then begin
        SubGroupParams^.q := Q;
        MPCopy2(P,R);
        MPDiv(R,Q,S);
        SubGroupParams^.c := S;
        Q := nil;
        S := nil;
      end;
    finally
      MPDealloc(S);
      MPDealloc(U);
      MPDealloc(R);
      MPDealloc(C);
      MPDealloc(Q);
      MPDealloc(SMin);
      MPDealloc(SMax);
    end;

  end else if Algo = paDSAPrime then begin
{$IFDEF SHA1}
    DSAParams := Params;
    if Assigned(DSAParams) then
      l := DSAParams^.l
    else
      l := 0;
    S := nil;
    Q := nil;
    X := nil;
    MPRealloc(Q,5);
    MPRealloc(X,5);
    try
      repeat
        repeat
          MPRandom(S,512);
          Seed := MPIntToBase256(S,64);
          if Assigned(DSAParams) then begin
            DSAParams^.Seed := Seed;
            UniqueString(DSAParams^.Seed);
          end;
          H := TSHA1.Create(Seed[1],64);
          try
            H.Done(@X^.Data);
          finally
            H.Free;
          end;
          for i := 0 to 4 do
            Q^.Data[i] := ByteSwap(X^.Data[i]);
          MPAddInt(S,1);
          Seed1 := MPIntToBase256(S,64);
          H := TSHA1.Create(Seed1[1],64);
          try
            H.Done(@X^.Data);
          finally
            H.Free;
          end;
          ProtectClear(Seed1[1],Length(Seed1));
          for i := 0 to 4 do
            Q^.Data[i] := Q^.Data[i] xor ByteSwap(X^.Data[i]);
          Q^.Data[0] := Q^.Data[0] or $80000000;
          Q^.Data[4] := Q^.Data[4] or $1;
          Q^.Sign := 1;
        until MPMillerRabin(Q,18);
        j := 2;
        i := 0;
        repeat
          Base256ToMPInt(S,Seed,512);
          MPDSARandom(P,S,j,l);
          MPDealloc(S);
          S := MPCopy(Q);
          MPShl(S,1);
          Dummy := nil;
          C := MPCopy(P);
          try
            MPDiv(C,S,Dummy);
            MPSubInt(C,1);
            MPSub(P,C);
          finally
            MPDealloc(Dummy);
            MPDealloc(C);
          end;
          Found := MPMillerRabin(P,5);
          Inc(i);
        until Found or (i = 4096);
        ProtectClear(Seed[1],Length(Seed));
      until Found;
      if Assigned(DSAParams) then begin
        DSAParams^.q := Q;
        Q := nil;
      end;
    finally
      MPDealloc(S);
      MPDealloc(Q);
      MPDealloc(X);
    end;    
{$ELSE  SHA1}
    raise Exception.Create('The implementation made an attempt to use a feature that requires SHA-1');
{$ENDIF SHA1}
  end else if Algo = paGordonStrongPrime then begin
    MPGordonStrongPrime(P,BitSize,Params);
  end else if Algo = paStrongPrime then begin
    MPStrongPrime(P,BitSize,Params);
  end else if Algo = paMaurerPrime then begin
    MPMaurerPrime(P,BitSize,Params);
  end;
end;

function IntToMPInt(Value: Integer): PMPInteger;
begin
  Result := nil;
  MPRealloc(Result,1);
  if Value < 0 then begin
    Result^.Sign := -1;
    Result^.Data[0] := -Value;
  end else if Value > 0 then begin
    Result^.Sign := 1;
    Result^.Data[0] := Value;
  end;
end;

function IntegersToMPInt(const Value: array of LongWord;
                         Negative: Boolean): PMPInteger;
var
  I: Integer;
begin
  Result := nil;
  MPRealloc(Result,High(Value) - Low(Value) + 1);
  for I := Low(Value) to High(Value) do
    Result^.Data[I - Low(Value)] := Value[I];
  if Negative then
    Result^.Sign := -1
  else
    Result^.Sign := 1;
end;

function Base16ToMPInt(var A: PMPInteger; const HexStr: string): Boolean;
var
  I, L: Integer;
  Size, Sign: LongInt;
begin
  Result := False;
  if (HexStr = '') or (HexStr = '-') then Exit;
  if HexStr = '0' then begin
    Result := True;
    Size := 1;
    Sign := 0;
  end else if HexStr[1] = '-' then begin
    for I := 2 to Length(HexStr) do begin
      Result := HexStr[I] in ['0'..'9','a'..'f','A'..'F'];
      if not Result then Exit;
    end;
    Size := (Length(HexStr) + 6) shr 3;
    Sign := -1;
  end else begin
    for I := 1 to Length(HexStr) do begin
      Result := HexStr[I] in ['0'..'9','a'..'f','A'..'F'];
      if not Result then Exit;
    end;
    Size := (Length(HexStr) + 7) shr 3;
    Sign := 1;
  end;
  MPRealloc(A,Size);
  A^.Sign := Sign;
  L := Length(HexStr);
  for I := 1 to Size-1 do begin
    A^.Data[Size - I] := StrToInt('$' + Copy(HexStr,L-7,8));
    Dec(L,8);
  end;
  if Sign = -1 then
    A^.Data[0] := StrToInt('$' + Copy(HexStr,2,L-1))
  else
    A^.Data[0] := StrToInt('$' + Copy(HexStr,1,L));
end;

function BaseBToMPInt(var A: PMPInteger; B: Cardinal; const Digits, Str: string): Boolean;
var
  I, P, BC: Integer;
  T: PMPInteger;
begin
  if (B > 255) or (B > Cardinal(Length(Digits))) then
    raise Exception.Create('Not enough digits for base conversion.');
  if Pos('-',Digits) > 0 then
    raise Exception.Create('Minus sign "-" cannot be specified as a digit.');
  MPRealloc(A,1);
  A^.Data[0] := 0;
  if Length(Str) = 0 then begin
    A^.Sign := 0;
    Result := False;
    Exit;
  end;
  I := 1;
  if Str[1] = '-' then begin
    A^.Sign := -1;
    if Length(Str) = 1 then begin
      Result := False;
      Exit;
    end;
    I := 2;
  end else
    A^.Sign := 1;
  repeat
    P := Pos(Str[I],Digits) - 1;
    if P < 0 then begin
      Result := False;
      Exit;
    end else if Cardinal(P) >= B then begin
      Result := True;
      Exit;
    end;
    T := nil;
    MPRealloc(T,A^.Size + 1);
    RawMulByInt(A,B,T);
    MPAddInt(T,P);
    BC := MPMSB(T);
    MPRealloc(T,(BC + 31) shr 5);
    MPDealloc(A);
    A := T;
    T := nil;
    Inc(I);
  until I > Length(Str);
  Result := True;
end;

function Base10ToMPInt(var A: PMPInteger; const Str: string): Boolean;
begin
  Result := BaseBToMPInt(A,10,'0123456789',Str);
end;

procedure Base256ToMPInt(var A: PMPInteger; const Str: string); overload;
begin
  Base256ToMPInt(A,Str,0);
end;

procedure Base256ToMPInt(var A: PMPInteger; const Str: string; MaxBitSize: Cardinal); overload;
begin
  {IEEE P1363 - Octet String to Integer Conversion Primitive (OS2IP)}
  {IMPORTANT: P1363 specifies that OS2IP should take the desired max size
   of the integer as a parameter, and output an error if the octet string
   is too long. Base256ToMPInt does NOT perform this check if MaxBitSize = 0.}
   Base256ToMPInt(A,Pointer(Str)^,Length(Str),MaxBitSize);
end;        

procedure TwosComplement(var Buf; Count: Cardinal);
asm
  mov ECX,EDX
  xor EDX,EDX
  mov DL,1
@@1:
  not byte ptr [EAX+ECX-1]
  add byte ptr [EAX+ECX-1],DL
  adc DH,0
  mov DL,DH
  xor DH,DH
  loop @@1
end;

procedure Base256ToMPInt(var A: PMPInteger; const Buf; Count: Cardinal; MaxBitSize: Cardinal); overload;
var
  Offset: Byte;
  P: PChar;
  I: Integer;
  Mask: LongWord;
begin
  Offset := Count and 3;
  if Offset <> 0 then begin
    Offset := 4 - Offset;
    if MaxBitSize = 0 then
      MaxBitSize := Count * 8;
  end;
  MPRealloc(A,(Count + 3) shr 2);
  P := @A^.Data;
  Move(Buf,(P + Offset)^,Count);
  if Byte((P + Offset)^) and $80 > 0 then begin
    TwosComplement((P + Offset)^,Count);
    A.Sign := -1;
  end else
    A^.Sign := 1;
  if Count > 0 then begin
    for I := 0 to A^.Size - 1 do
      A^.Data[I] := ByteSwap(A^.Data[I]);
    if MaxBitSize > 0 then begin
      I := 0;
      while Cardinal((MPMSB(A) + 31) shr 5) > (MaxBitSize + 31) shr 5 do begin
        A^.Data[I] := 0;
        Inc(I);
      end;
      if Cardinal(MPMSB(A)) > MaxBitSize then begin
        Mask := (1 shl (MaxBitSize and $1F)) - 1;
        A^.Data[I] := A^.Data[I] and Mask;
      end;
    end;
  end;
end;     

procedure Base256ToUMPInt(var A: PMPInteger; const Buf; Count: Cardinal; MaxBitSize: Cardinal);
var
  Offset: Byte;
  P: PChar;
  I: Integer;
  Mask: LongWord;
begin
  if Count = 0 then begin
    MPDealloc(A);
    A := nil;
    Exit;
  end;
  Offset := Count and 3;
  if Offset <> 0 then Offset := 4 - Offset;
  MPRealloc(A,(Count + 3) shr 2);
  P := @A^.Data;
  FillChar(P^,Offset,0);
  Move(Buf,(P + Offset)^,Count);
  if Count > 0 then begin
    A^.Sign := 1;
    for I := 0 to A^.Size - 1 do
      A^.Data[I] := ByteSwap(A^.Data[I]);
    if MaxBitSize > 0 then begin
      I := 0;
      while Cardinal((MPMSB(A) + 31) shr 5) > (MaxBitSize + 31) shr 5 do begin
        A^.Data[I] := 0;
        Inc(I);
      end;
      if Cardinal(MPMSB(A)) > MaxBitSize then begin
        Mask := (1 shl (MaxBitSize and $1F)) - 1;
        A^.Data[I] := A^.Data[I] and Mask;
      end;
    end;
  end;
end;

function MPIntToBase16(A: PMPInteger): string;
type
  TDigitArray = array [0..15] of Char;
const
  Digits: TDigitArray = '0123456789ABCDEF';
var
  I, J, K: Cardinal;
  V: Longword;
begin
  if A = nil then
    Result := ''
  else if (A^.Sign = 0) or (A^.Size = 0) then
    Result := '0'
  else begin
    if A^.Sign = -1 then
      Result := StringOfChar('0',(MPMSB(A) + 7) shr 2)
    else
      Result := StringOfChar('0',(MPMSB(A) + 3) shr 2);
    J := Length(Result);
    if J = 0 then
      Result := '0'
    else begin
      for I := 1 to A^.Size do begin
        V := A^.Data[A^.Size - I];
        for K := 0 to 7 do begin
          if J = 0 then Break;
          Result[J] := Digits[V and $F];
          V := V shr 4;
          Dec(J);
        end;
        if J = 0 then Break;
      end;
      if A^.Sign = -1 then
        Result[1] := '-';
    end;
  end;
end;

function MPIntToBaseB(A: PMPInteger; B: Cardinal; const Digits: string): string;
var
  T: PMPInteger;
  P: LongWord;
  Size, I: Integer;
begin
  if (B > 255) or (B > Cardinal(Length(Digits))) then
    raise Exception.Create('Not enough digits for base conversion.');
  if Pos('-',Digits) > 0 then
    raise Exception.Create('Minus sign "-" cannot be specified as a digit.');
  Size := MPMSB(A);                    
  if Size = 0 then
    Size := 1
  else if A^.Sign = -1 then
    Size := Ceil(Size / (ln(B)/ln(2))) + 1
  else
    Size := Ceil(Size / (ln(B)/ln(2)));
  Result := StringOfChar(Digits[1],Size);
  Result[Size] := Digits[1];
  I := Size;
  T := MPCopy(A);
  while MPMSB(T) > 0 do begin
    P := MPDivByInt2(T,B,T);
    Result[I] := Digits[P + 1];
    Dec(I);
  end;
  MPDealloc(T);
  if A^.Sign = -1 then begin
    if I > 1 then
      Delete(Result,1,I-1);
    Result[I] := '-';
  end else if I = Size then
    Result := Digits[1]
  else if I > 0 then
    Delete(Result,1,I);
end;

function MPIntToBase10(A: PMPInteger): string;
begin
  Result := MPIntToBaseB(A,10,'0123456789');
end;

function MPIntToBase256(A: PMPInteger): string; overload;
begin
  Result := MPIntToBase256(A,0);
end;

function MPIntToBase256(A: PMPInteger; FixedOutputLen: Cardinal): string; overload;
var
  MSB, Size: Cardinal;
begin
  if A = nil then begin
    Result := '';
    Exit;
  end;
  {IEEE P1363 - Integer to Octet String Conversion Primitive (I2OSP)}
  {Result[1] := Most significant octet.}
  {NOTE: If FixedOutputLen = 0 then Result will be set to the minimum length
   that fits the entire integer.}
  MSB := MPMSB(A);
  Size := (MSB + 7) shr 3;
  if FixedOutputLen > 0 then begin
    if Size > FixedOutputLen then
      raise Exception.Create('MPIntToBase256: Integer too large for FixedOutputLen');
    Size := FixedOutputLen;
  end else if (Size shl 3) = MSB then
    Inc(Size);
  if (A^.Size = 0) or (A^.Sign = 0) then begin
    Result := StringOfChar(#0,Size);
  end else begin
    Result := StringOfChar(#0,Size);
    MPIntToBase256(A,Result[1],Size);
    if FixedOutputLen = 0 then
      while (Result <> '') and (Result[1] = #0) and
            (Cardinal(Length(Result)) > Size) do
        Delete(Result,1,1);
  end;
  if Result = '' then
    Result := #0;
end;

procedure MPIntToBase256(A: PMPInteger; var Buf; Count: Cardinal); overload;
var
  I, J: Integer;
  MSB, Size: Cardinal;
  LW: LongWord;
  P: PChar;
begin
  P := @Buf;
  MSB := MPMSB(A);
  Size := (MSB + 7) shr 3;
  if Size*8 = MSB then
    Inc(Size);
  if Count >= Size then
    ProtectClear(Buf,Count);
  if (A^.Size > 0) and (A^.Sign <> 0) then begin
    J := Count - 4;
    I := A^.Size - 1;
    while (J >= 0) and (I >= 0) do begin
      LW := ByteSwap(A^.Data[I]);
      Move(LW,P[J],4);
      Dec(I);
      Dec(J,4);
    end;
    if (J < 0) and (J > -4) and (I >= 0) then begin
      LW := ByteSwap(A^.Data[I]);
      Move(Ptr(LongInt(@LW) - J)^,P[0],4 + J);
    end;
    FillChar(LW,4,0);
    if A.Sign < 0 then
      TwosComplement(Buf,Count);
  end;
end;           

procedure UMPIntToBase256(A: PMPInteger; var Buf; Count: Cardinal);
var
  I, J: Integer;
  MSB, Size: Cardinal;
  LW: LongWord;
  P: PChar;
begin
  P := @Buf;
  MSB := MPMSB(A);
  Size := (MSB + 7) shr 3;
  if Count > Size then
    ProtectClear(Buf,Count);
  if (A^.Size > 0) and (A^.Sign <> 0) then begin
    J := Count - 4;
    I := A^.Size - 1;
    while (J >= 0) and (I >= 0) do begin
      LW := ByteSwap(A^.Data[I]);
      Move(LW,P[J],4);
      Dec(I);
      Dec(J,4);
    end;
    if (J < 0) and (J > -4) and (I >= 0) then begin
      LW := ByteSwap(A^.Data[I]);
      Move(Ptr(LongInt(@LW) - J)^,P[0],4 + J);
    end;
  end;
end;
              
{$IFDEF INITIALIZATION}
initialization
{$ENDIF}
{$IFDEF INIT_SECTIONS}
  Pool := TMPPool.Create(PoolSize,PoolBlockSize);
{$ENDIF}              
{$IFDEF FINI_SECTIONS}
finalization
  Pool.Free;
{$ENDIF}
end.
