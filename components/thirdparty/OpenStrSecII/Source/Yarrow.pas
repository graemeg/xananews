{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Yarrow Unit                                       }
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
unit Yarrow;

interface

uses
  SecUtils,
{$IFDEF RIJNDAEL}
  SsRijndael,
{$ELSE  RIJNDAEL}
{$IFDEF TWOFISH}
  SsTwoFish,
{$ELSE  TWOFISH}
{$IFDEF DES}
  SsDes,
{$ELSE  DES}
{$IFDEF BLOWFISH}
  SsBlowFish,
{$ELSE  BLOWFISH}
{$ENDIF BLOWFISH}
{$ENDIF DES}
{$ENDIF TWOFISH}
{$ENDIF RIJDNAEL}
  Classes, SyncObjs;

const
  MinimumAbsoluteReseedIterations = 4;
{$IFDEF SHA1}
  DigestLWLen = 5;
{$ELSE  SHA1}
{$IFDEF RIPEMD160}
  DigestLWLen = 5;
{$ELSE  RIPEMD160}
{$IFDEF SHA256}
  DigestLWLen = 8;
{$ELSE  SHA256}
{$IFDEF SHA512}
  DigestLWLen = 16;
{$ELSE  SHA512}
//  Compiler error: Ran out of acceptable algorithms
{$ENDIF SHA512}
{$ENDIF SHA256}
{$ENDIF RIPEMD160}
{$ENDIF SHA1}
var
  SlowReseedTicks: Integer = 1000;

type
  TGenerator = class
  private
    FLock: TCriticalSection;
    FCipher: TBlockCipher;
    FCipherClass: TBlockCipherClass;
    FGateCounter: Cardinal;
    FGateThreshold: Cardinal;
    FOldKey: Pointer;
    FOldKeySize: Cardinal;
    procedure InternalGenerate(var Buf; Count: Integer);
    procedure Rekey(const AKey; Count: Integer);
    procedure SetGateThreshold(const Value: Cardinal);
  public
    constructor Create(ACipherClass: TBlockCipherClass);
    destructor Destroy; override;
    procedure Generate(var Buf; Count: Integer);
    procedure GenerateSeed(var Buf; Count: Integer);
    procedure Lock;
    procedure Unlock;
    property GateThreshold: Cardinal read FGateThreshold write SetGateThreshold;
  end;

  TYarrow = class;

  PEntropyCounters = ^TEntropyCounters;
  TEntropyCounters = array [0..MaxListSize-1] of Cardinal;

  TMilliseconds = Cardinal;

  TReseedEvent = procedure (Sender: TObject; Slow: Boolean) of object;

  TEntropy = class
  private
    FFastPool: THash;
    FFastPoolCounter: Cardinal;
    FFastEntropyCounters: PEntropyCounters;
    FSlowPool: THash;
    FSlowPoolCounter: Cardinal;
    FSlowEntropyCounters: PEntropyCounters;
    FNextFast: Boolean;
    FOwner: TYarrow;
    FThreads: TThreadList;
    FSourceCount: Cardinal;
    FSlowReseedTime: TMilliseconds;
    FFastReseedTime: TMilliseconds;
    FOnReseeding: TReseedEvent;
    FOnReseeded: TReseedEvent;
    procedure FastReseed;
    procedure SlowReseed(Ticks: Cardinal);
    procedure SetSourceCount(const Value: Cardinal);
    procedure SetFastReseedTime(const Value: TMilliseconds);
    procedure SetSlowReseedTime(const Value: TMilliseconds);
    procedure SetOnReseeded(const Value: TReseedEvent);
    procedure SetOnReseeding(const Value: TReseedEvent);
  protected
    procedure NotifyThreads; virtual;
    procedure Reseed(Ticks: Cardinal; Slow: Boolean); virtual;
    procedure ReseedThreadDone(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TYarrow);
    destructor Destroy; override;
    procedure Accumulate(); overload;
    procedure Accumulate(const Buf; Count: Integer); overload;
    procedure Accumulate(const Buf; Count: Integer; SrcID: Cardinal); overload;
    procedure Accumulate(const Buf; Count: Integer; SrcID, EntrEst: Cardinal); overload;
    procedure ForceSlowReseed(const Buf; Count: Integer; Ticks: Cardinal);
    property FastReseedTime: TMilliseconds read FFastReseedTime write SetFastReseedTime;
    property SlowReseedTime: TMilliseconds read FSlowReseedTime write SetSlowReseedTime;
    property SourceCount: Cardinal read FSourceCount write SetSourceCount;
    property OnReseeding: TReseedEvent read FOnReseeding write SetOnReseeding;
    property OnReseeded: TReseedEvent read FOnReseeded write SetOnReseeded;
  end;

  TBlockingEntropy = class(TEntropy)
  protected
    procedure NotifyThreads; override;
    procedure Reseed(Ticks: Cardinal; Slow: Boolean); override;
    procedure ReseedThreadDone(Sender: TObject); override;
  end;

  TYarrow = class
  private
    FEntropy: TEntropy;
    FGenerator: TGenerator;
    FSeedFile: TFileStream;
    FSeedFilePath: string;
    FOnReseeding: TReseedEvent;
    FOnReseeded: TReseedEvent;
    FReseeded: Boolean;
    procedure DoReseeded(Sender: TObject; Slow: Boolean);
    procedure DoReseeding(Sender: TObject; Slow: Boolean);
    procedure SetSeedFilePath(const Value: string);
    procedure SetOnReseeded(const Value: TReseedEvent);
    procedure SetOnReseeding(const Value: TReseedEvent);
  protected
    property Reseeded: Boolean read FReseeded;
  public
    constructor Create(Blocking: Boolean = False);
    destructor Destroy; override;
    property Entropy: TEntropy read FEntropy;
    property Generator: TGenerator read FGenerator;
    property SeedFilePath: string read FSeedFilePath write SetSeedFilePath;
    property OnReseeding: TReseedEvent read FOnReseeding write SetOnReseeding;
    property OnReseeded: TReseedEvent read FOnReseeded write SetOnReseeded;
  end;

  TRawRandom = procedure(var X; MaxBitSize: Cardinal);

var
  RawRandom: TRawRandom;

implementation

uses
  SysUtils, TlsUtils,
{$IFDEF MSWINDOWS}
  Windows
{$ENDIF}
{$IFDEF LINUX}
  Libc
{$ENDIF};

const
{$IFDEF SHA1}
  GHashClass: THashClass = TSHA1;
{$ELSE  SHA1}
{$IFDEF RIPEMD160}
  GHashClass: THashClass = TRipeMD160;
{$ELSE  RIPEMD160}
{$IFDEF SHA256}
  GHashClass: THashClass = TSHA256;
{$ELSE  SHA256}
{$IFDEF SHA512}
  GHashClass: THashClass = TSHA512;
{$ELSE  SHA512}
//  Compiler error: Ran out of acceptable algorithms
  ---
{$ENDIF SHA512}
{$ENDIF SHA256}
{$ENDIF RIPEMD160}
{$ENDIF SHA1}
{$IFDEF CTR}
{$IFDEF RIJNDAEL}
  GBlockCipherClass: TBlockCipherClass = TRijndael_CTR;
{$ELSE  RIJNDAEL}
{$IFDEF TWOFISH}
  GBlockCipherClass: TBlockCipherClass = TTwoFish_CTR;
{$ELSE  TWOFISH}
{$IFDEF DES}
  GBlockCipherClass: TBlockCipherClass = T3DES_CTR;
{$ELSE  DES}
{$IFDEF BLOWFISH}
  GBlockCipherClass: TBlockCipherClass = TBlowFish_CTR;
{$ELSE  BLOWFISH}
{$ENDIF BLOWFISH}
{$ENDIF DES}
{$ENDIF TWOFISH}
{$ENDIF RIJNDAEL}
{$ELSE  CTR}
//  Compiler error: Yarrow requires CTR mode to be defined
  ---
{$ENDIF CTR}

{ TGenerator }

constructor TGenerator.Create(ACipherClass: TBlockCipherClass);
begin
  if ACipherClass = nil then
    FCipherClass := GBlockCipherClass
  else if ACipherClass.Mode = cmCTR then
    FCipherClass := ACipherClass
  else
    raise Exception.Create('TGenerator.Create: Cipher mode must be cmCTR.');
  FGateCounter := 0;
  FGateThreshold := 10;
  FLock := TCriticalSection.Create;
end;

destructor TGenerator.Destroy;
begin
  FLock.Free;
  ProtectClear(FOldKey^,FOldKeySize);
  FreeMem(FOldKey);
  FCipher.Free;
  inherited;
end;

procedure TGenerator.Generate(var Buf; Count: Integer);
begin
  Lock;
  try
    InternalGenerate(Buf,Count);
  finally
    Unlock;
  end;
end;

procedure TGenerator.GenerateSeed(var Buf; Count: Integer);
var
  IV: string;
begin
  Lock;
  try
    SetLength(IV,FCipher.BlockSize * FCipher.BlockVectorSize);
    try
      FCipher.Encrypt(Pointer(IV)^,Length(IV));
      FCipher.SetVectorBuf(Pointer(IV)^,Length(IV));
      InternalGenerate(Buf,Count);
    finally
      ProtectClear(Pointer(IV)^,Length(IV));
    end;
  finally
    Unlock;
  end;
end;

procedure TGenerator.InternalGenerate(var Buf; Count: Integer);
var
  K, B: Pointer;
begin
  if FOldKey = nil then
    raise Exception.Create('Yarrow has not been reseeded');
  B := @Buf;
  FillChar(Buf,Count,0);
  while Count > 0 do begin
    if Count >= Integer(FGateThreshold - FGateCounter) * FCipher.BlockSize then begin
      FCipher.Encrypt(B^,Integer(FGateThreshold - FGateCounter) * FCipher.BlockSize);
      Dec(Count,Integer(FGateThreshold - FGateCounter) * FCipher.BlockSize);
      Inc(LongInt(B),Integer(FGateThreshold - FGateCounter) * FCipher.BlockSize);

      GetMem(K,FCipher.MinKeySize);
      try
        FillChar(K^,FCipher.MinKeySize,0);
        FCipher.Encrypt(K^,FCipher.MinKeySize);
        ReKey(K^,FCipher.MinKeySize);
      finally
        ProtectClear(K^,FCipher.MinKeySize);
        FreeMem(K);
      end;
      FGateCounter := 0;
    end else begin
      FCipher.Encrypt(B^,Count);
      Inc(FGateCounter,(Count + FCipher.BlockSize - 1) div FCipher.BlockSize);
      if FGateCounter = FGateThreshold then begin
        GetMem(K,FCipher.MinKeySize);
        try
          FillChar(K^,FCipher.MinKeySize,0);
          FCipher.Encrypt(K^,FCipher.MinKeySize);
          ReKey(K^,FCipher.MinKeySize);
        finally
          ProtectClear(K^,FCipher.MinKeySize);
          FreeMem(K);
        end;
        FGateCounter := 0;
      end;
      Count := 0;
    end;
  end;
end;

procedure TGenerator.Lock;
begin
  FLock.Acquire;
end;

procedure TGenerator.Rekey(const AKey; Count: Integer);
var
  IV: string;
begin
  if FCipher = nil then
    FCipher := FCipherClass.Create(AKey,Count,0)
  else begin
    IV := FCipher.IVector;
    try
      FCipher.SetUp(AKey,Count,0);
      FCipher.IVector := IV;
    finally
      ProtectClear(Pointer(IV)^,Length(IV));
    end;
  end;
  if Assigned(FOldKey) then begin
    if Count <> Integer(FOldKeySize) then begin
      ProtectClear(FOldKey^,FOldKeySize);
      ReallocMem(FOldKey,Count);
      FOldKeySize := Count;
    end;
  end else if Count > 0 then begin
    GetMem(FOldKey,Count);
    FOldKeySize := Count;
  end;
  Move(AKey,FOldKey^,Count);
end;

procedure TGenerator.SetGateThreshold(const Value: Cardinal);
begin
  FGateThreshold := Value;
end;

procedure TGenerator.Unlock;
begin
  FLock.Release;
end;

type              
  TReseedObject = class
  private
    FDigests: packed array [0..DigestLWLen*2] of LongWord;
    FTicks: Cardinal;
    FSlow: Boolean;
    FOnTerminate: TNotifyEvent;
    procedure SetOnTerminate(const Value: TNotifyEvent);
  protected
    procedure BeginExecute(var H: THash; var Iter, Start: Cardinal);
    procedure EndExecute(H: THash);
    procedure Execute;
    procedure Step(H: THash; var Iter: Cardinal);
  public
    property OnTerminate: TNotifyEvent read FOnTerminate write SetOnTerminate;
  end;

  TReseedThread = class(TThread)
  private
    FDigests: packed array [0..DigestLWLen*2] of LongWord;
    FTicks: Cardinal;
    FSlow: Boolean;
  protected
    procedure Execute; override;
  end;

{ TReseedThread }

procedure TReseedThread.Execute;
var
  Start, Iter: Cardinal;
  H: THash;
  Obj: TReseedObject;
begin
  Obj := TReseedObject.Create;
  try
    Move(FDigests,Obj.FDigests,SizeOf(FDigests));
    Obj.FTicks := FTicks;
    Obj.BeginExecute(H,Iter,Start);
    try
      repeat
        Obj.Step(H,Iter);
      until Terminated or (Iter >= MinimumAbsoluteReseedIterations) or
                          (GetTickCount - Start > FTicks);
    finally
      Obj.EndExecute(H);
    end;
    Move(Obj.FDigests,FDigests,SizeOf(FDigests));
  finally
    Obj.Free;
    Terminate;
  end;
end;

{ TEntropy }

procedure TEntropy.Accumulate;
begin
  Accumulate(Self,0,0,0);
end;

procedure TEntropy.Accumulate(const Buf; Count: Integer);
begin
  if Count > 0 then
    Accumulate(Buf,Count,0,1)
  else
    Accumulate(Buf,Count,0,0);
end;

procedure TEntropy.Accumulate(const Buf; Count: Integer; SrcID: Cardinal);
begin
  if Count > 0 then
    Accumulate(Buf,Count,SrcID,1)
  else
    Accumulate(Buf,Count,SrcID,0)
end;

procedure TEntropy.Accumulate(const Buf; Count: Integer; SrcID,
  EntrEst: Cardinal);
var
  Ticks: Int64;
begin
  if SrcID >= SourceCount then
    raise Exception.Create(Format('TEntropy.Accumulate: SrcID %d is too large',[SrcID]));
  if Int64(EntrEst) > Count*8 then
    raise Exception.Create(Format('TEntropy.Accumulate: EntrEst %d is larger than the bit size %d of Buf',[SrcID,Count*8]));
  if FNextFast then begin
    if Count > 0 then
      FFastPool.HashData(Buf,Count);
    QueryPerformanceCounter(Ticks);
    FFastPool.HashData(Ticks,SizeOf(Ticks));
    Inc(FFastPoolCounter);
    if EntrEst > 0 then
      Inc(FFastEntropyCounters^[SrcID],EntrEst);
    FastReseed;
  end else begin
    if Count > 0 then
      FSlowPool.HashData(Buf,Count);
    QueryPerformanceCounter(Ticks);
    FSlowPool.HashData(Ticks,SizeOf(Ticks));
    Inc(FSlowPoolCounter);
    if EntrEst > 0 then
      Inc(FSlowEntropyCounters^[SrcID],EntrEst);
  end;
  FNextFast := not FNextFast;
end;

constructor TEntropy.Create(AOwner: TYarrow);
begin
  FOwner := AOwner;
  FFastPool := GHashClass.Create(Self,0);
  FSlowPool := GHashClass.Create(Self,0);
  FThreads := TThreadList.Create;
  SourceCount := 1;
  FFastReseedTime := 1;
  FSlowReseedTime := 500;
end;

destructor TEntropy.Destroy;
begin
  NotifyThreads;
  FThreads.Free;
  FFastPool.Free;
  FSlowPool.Free;
  FreeMem(FSlowEntropyCounters);
  FreeMem(FFastEntropyCounters);
  inherited;
end;

procedure TEntropy.FastReseed;
var
  OK, OK1, OK2: Boolean;
  I: Integer;
begin
  OK := (FFastPoolCounter > 100);
  OK1 := (FSlowPoolCounter > 160);
  OK2 := False;
  if not OK then begin
    for I := 0 to FSourceCount - 1 do begin
      OK := FFastEntropyCounters^[I] > 100;
      OK2 := OK1 and (FSlowEntropyCounters^[I] > 160);
      OK1 := OK1 or (FSlowEntropyCounters^[I] > 160);
      if OK or (OK1 and OK2) then Break;
    end;
  end;
  if OK1 and OK2 then
    SlowReseed(FSlowReseedTime)
  else if OK then begin
    Reseed(FFastReseedTime,False);
    ProtectClear(FFastEntropyCounters^,FSourceCount*4);
    FFastPoolCounter := 0;
  end;
end;

procedure TEntropy.ForceSlowReseed(const Buf; Count: Integer;
  Ticks: Cardinal);
begin
  FSlowPool.HashData(Buf,Count);
  SlowReseed(Ticks);
end;

procedure TEntropy.NotifyThreads;
var
  List: TList;
  I: Integer;
  Thrd: TThread;
begin
  List := FThreads.LockList;
  try
    for I := List.Count - 1 downto 0 do begin
      Thrd := TThread(List[I]);
      with Thrd do begin
        Suspend;
        OnTerminate := nil;
        {$IFDEF WIN32}
        Priority := tpNormal;
        {$ENDIF}
        FreeOnTerminate := True;
        Terminate;
        Resume;
      end;
      List.Remove(Thrd);
      Sleep(250);
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TEntropy.Reseed(Ticks: Cardinal; Slow: Boolean);
var
  Thrd: TReseedThread;
begin
  if Assigned(FOnReseeding) then
    FOnReseeding(Self,Slow);

  Thrd := TReseedThread.Create(True);
  Thrd.FreeOnTerminate := True;  //GT
  Thrd.FSlow := Slow;
  FFastPool.Done(@Thrd.FDigests);
  Move(Thrd.FDigests,Thrd.FDigests[DigestLWLen],DigestLWLen*SizeOf(LongWord));
  Thrd.FTicks := Ticks;
  {Usually several reseed threads are active simultaneously. The threads do
   not have to finish in the same order they where fired. All entropy collected
   by Yarrow is present in the key, and the present key is hashed together with
   the result of each thread when a new key is generated.
  }
  FThreads.Add(Thrd);
  {The priority of the reseed threads are set to tpIdle.
   Pro:
     + Performance will not be crippled.
     + It will normally be harder to determine how many iterations are actually
       done.
     + The probability of out-of-order reseed thread termination will be higher
       which might be regarded as a crude secondary entropy source.
   Con:
     - A trivial form of attack would be to start CPU intensive processes when a
       thread is fired, thereby reducing the number of iterations. The constant
       MinimumAbsoluteReseedIterations will make sure that a fixed number of
       iterations are done regardless of the state of the system.
  }
  {$IFDEF MSWINDOWS}
  Thrd.Priority := tpIdle;
  {$ENDIF}
  {$IFDEF LINUX}
  Thrd.Policy := SCHED_OTHER;
  Thrd.Priority := 0;
  {$ENDIF}
  Thrd.OnTerminate := ReseedThreadDone;
  Thrd.Resume;
end;

procedure TEntropy.ReseedThreadDone(Sender: TObject);
var
  Thrd: TReseedThread absolute Sender;
  List: TList;
  H: THash;
  Seed: string;
begin
  if FThreads = nil then Exit;
  List := FThreads.LockList;
  try
    List.Remove(Sender);
    H := GHashClass.Create(Thrd.FDigests,SizeOf(Thrd.FDigests));
    try
      FOwner.FGenerator.Lock;
      try
        if Assigned(FOwner.FGenerator.FOldKey) then
          H.HashData(FOwner.FGenerator.FOldKey^,FOwner.FGenerator.FOldKeySize);
        H.Done(@Thrd.FDigests);
        FOwner.FGenerator.Rekey(Thrd.FDigests,16);
      finally
        FOwner.FGenerator.Unlock;
      end;
      ProtectClear(Thrd.FDigests,SizeOf(Thrd.FDigests));
    finally
      H.Free;
    end;
    if Assigned(FOwner.FSeedFile) then begin
      SetLength(Seed,32);
      try
        FOwner.FGenerator.GenerateSeed(Seed[1],32);
        FOwner.FSeedFile.Seek(0,soFromBeginning);
        FOwner.FSeedFile.Write(Seed[1],32);
      finally
        ProtectClear(Seed[1],32);
      end;
    end;
  finally
    FThreads.UnlockList;
  end;

  if Assigned(FOnReseeded) then
    FOnReseeded(Self,Thrd.FSlow);

//  Thrd.FreeOnTerminate := True;
end;

procedure TEntropy.SetFastReseedTime(const Value: TMilliseconds);
begin
  FFastReseedTime := Value;
end;

procedure TEntropy.SetOnReseeded(const Value: TReseedEvent);
begin
  FOnReseeded := Value;
end;

procedure TEntropy.SetOnReseeding(const Value: TReseedEvent);
begin
  FOnReseeding := Value;
end;

procedure TEntropy.SetSlowReseedTime(const Value: TMilliseconds);
begin
  FSlowReseedTime := Value;
end;

procedure TEntropy.SetSourceCount(const Value: Cardinal);
begin
  if FSourceCount <> Value then begin
    if Value = 0 then begin
      FreeMem(FSlowEntropyCounters);
      FreeMem(FFastEntropyCounters);
      FSlowEntropyCounters := nil;
      FFastEntropyCounters := nil;
    end else if FSourceCount = 0 then begin
      GetMem(FSlowEntropyCounters,Value*4);
      GetMem(FFastEntropyCounters,Value*4);
    end else begin
      ReallocMem(FSlowEntropyCounters,Value*4);
      ReallocMem(FFastEntropyCounters,Value*4);
    end;
    FSourceCount := Value;
  end;
end;

procedure TEntropy.SlowReseed(Ticks: Cardinal);
var
  Digest: packed array [0..4] of LongWord;
begin
  FSlowPool.Done(@Digest);
  FFastPool.HashData(Digest,SizeOf(Digest));
  ProtectClear(Digest,SizeOf(Digest));
  Reseed(Ticks,True);
  ProtectClear(FFastEntropyCounters^,FSourceCount*4);
  ProtectClear(FSlowEntropyCounters^,FSourceCount*4);
  FFastPoolCounter := 0;
  FSlowPoolCounter := 0;
end;

{ TYarrow }

const
  DefKey = '0123456789abcdef';

constructor TYarrow.Create;
begin
  if Blocking then
    FEntropy := TBlockingEntropy.Create(Self)
  else
    FEntropy := TEntropy.Create(Self);
  FEntropy.OnReseeding := DoReseeding;
  FEntropy.OnReseeded := DoReseeded;
  FGenerator := TGenerator.Create(GBlockCipherClass);
  FGenerator.Rekey(DefKey,16);
end;

destructor TYarrow.Destroy;
begin
  FEntropy.Free;
  FGenerator.Free;
  FSeedFile.Free;
  inherited;
end;

procedure TYarrow.DoReseeded(Sender: TObject; Slow: Boolean);
begin
  if Assigned(FOnReseeded) then
    FOnReseeded(Self,Slow);
end;

procedure TYarrow.DoReseeding(Sender: TObject; Slow: Boolean);
begin
  if Assigned(FOnReseeding) then
    FOnReseeding(Self,Slow);
end;

procedure TYarrow.SetOnReseeded(const Value: TReseedEvent);
begin
  FOnReseeded := Value;
end;

procedure TYarrow.SetOnReseeding(const Value: TReseedEvent);
begin
  FOnReseeding := Value;
end;

procedure TYarrow.SetSeedFilePath(const Value: string);
var
  S: string;
begin
  FSeedFilePath := Value;
  {$IFNDEF D5UP}
  FSeedFile.Free;
  FSeedFile := nil;
  {$ELSE}
  FreeAndNil(FSeedFile); // IMPORTANT: The next step might fail...
  {$ENDIF}
  if (Value <> '') and not FileExists(Value) then
    FSeedFile := TFileStream.Create(Value,fmCreate or fmOpenReadWrite or fmShareExclusive)
  else if Value <> '' then begin
    FSeedFile := TFileStream.Create(Value,fmOpenReadWrite or fmShareExclusive);
    if FSeedFile.Size >= 32 then begin
      SetLength(S,32);
      try
        FSeedFile.Read(S[1],32);
        FEntropy.ForceSlowReseed(S[1],32,SlowReseedTicks);
      finally
        ProtectClear(S[1],32);
      end;
    end;
  end;
end;

{ TReseedObject }

procedure TReseedObject.BeginExecute;
begin                       
  Iter := 0;
  Start := GetTickCount;
  H := GHashClass.Create(nil^,0);
end;

procedure TReseedObject.EndExecute;
begin
  H.Free;
end;

procedure TReseedObject.Execute;
var
  H: THash;
  Iter, Start: Cardinal;
begin
  BeginExecute(H,Iter,Start);
  try
    repeat
      Step(H,Iter);
    until (Iter >= MinimumAbsoluteReseedIterations) and
          (GetTickCount - Start > FTicks);
  finally
    EndExecute(H);
  end;
end;

procedure TReseedObject.SetOnTerminate(const Value: TNotifyEvent);
begin
  FOnTerminate := Value;
end;

procedure TReseedObject.Step;
var
  PerfCount: Int64;
begin
  H.SetUp;
  QueryPerformanceCounter(PerfCount);
  H.HashData(PerfCount,8);
  H.HashData(FDigests,SizeOf(FDigests));
  H.Done(@FDigests);
  Inc(FDigests[DigestLWLen*2]);
  H.SetUp;
  QueryPerformanceCounter(PerfCount);
  H.HashData(PerfCount,8);
  H.HashData(FDigests,SizeOf(FDigests));
  H.Done(@FDigests);
  Inc(FDigests[DigestLWLen*2]);
  H.SetUp;
  QueryPerformanceCounter(PerfCount);
  H.HashData(PerfCount,8);
  H.HashData(FDigests,SizeOf(FDigests));
  H.Done(@FDigests);
  Inc(FDigests[DigestLWLen*2]);
  H.SetUp;
  QueryPerformanceCounter(PerfCount);
  H.HashData(PerfCount,8);
  H.HashData(FDigests,SizeOf(FDigests));
  H.Done(@FDigests);
  Inc(FDigests[DigestLWLen*2]);
  H.SetUp;
  QueryPerformanceCounter(PerfCount);
  H.HashData(PerfCount,8);
  H.HashData(FDigests,SizeOf(FDigests));
  H.Done(@FDigests);
  Inc(FDigests[DigestLWLen*2]);
  if FTicks > 1 then
    Sleep(1)
  else
    Sleep(0);
  Inc(Iter);
end;

{ TBlockingEntropy }

procedure TBlockingEntropy.NotifyThreads;
var
  List: TList;
  I: Integer;
  Thrd: TReseedObject;
begin
  List := FThreads.LockList;
  try
    for I := List.Count - 1 downto 0 do begin
      Thrd := TReseedObject(List[I]);
      Thrd.OnTerminate := nil;
      List.Remove(Thrd);
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TBlockingEntropy.Reseed(Ticks: Cardinal; Slow: Boolean);
var
  Thrd: TReseedObject;
begin
  if Assigned(FOnReseeding) then
    FOnReseeding(Self,Slow);

  Thrd := TReseedObject.Create;
  Thrd.FSlow := Slow;
  FFastPool.Done(@Thrd.FDigests);
  Move(Thrd.FDigests,Thrd.FDigests[DigestLWLen],DigestLWLen*SizeOf(LongWord));
  Thrd.FTicks := Ticks;
  {Usually several reseed threads are active simultaneously. The threads do
   not have to finish in the same order they where fired. All entropy collected
   by Yarrow is present in the key, and the present key is hashed together with
   the result of each thread when a new key is generated.
  }
  FThreads.Add(Thrd);
  Thrd.OnTerminate := ReseedThreadDone;
  Thrd.Execute;
  if Assigned(Thrd.FOnTerminate) then
    Thrd.OnTerminate(Thrd)
  else
    Thrd.Free;
end;

procedure TBlockingEntropy.ReseedThreadDone(Sender: TObject);
var
  Thrd: TReseedObject absolute Sender;
  List: TList;
  H: THash;
  Seed: string;
begin
  List := FThreads.LockList;
  try
    List.Remove(Sender);
    H := GHashClass.Create(Thrd.FDigests,SizeOf(Thrd.FDigests));
    try
      FOwner.FGenerator.Lock;
      try
        if Assigned(FOwner.FGenerator.FOldKey) then
          H.HashData(FOwner.FGenerator.FOldKey^,FOwner.FGenerator.FOldKeySize);
        H.Done(@Thrd.FDigests);
        FOwner.FGenerator.Rekey(Thrd.FDigests,16);
      finally
        FOwner.FGenerator.Unlock;
      end;
      ProtectClear(Thrd.FDigests,SizeOf(Thrd.FDigests));
    finally
      H.Free;
    end;
    if Assigned(FOwner.FSeedFile) then begin
      SetLength(Seed,32);
      try
        FOwner.FGenerator.GenerateSeed(Seed[1],32);
        FOwner.FSeedFile.Seek(0,soFromBeginning);
        FOwner.FSeedFile.Write(Seed[1],32);
      finally
        ProtectClear(Seed[1],32);
      end;
    end;
  finally
    FThreads.UnlockList;
  end;

  if Assigned(FOnReseeded) then
    FOnReseeded(Self,Thrd.FSlow);

  Thrd.Free;
end;

end.
