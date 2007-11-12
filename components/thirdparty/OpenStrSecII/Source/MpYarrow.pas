{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPYarrow Unit                                     }
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
unit MpYarrow;

interface

uses
  Classes, SecUtils, Yarrow, MpArithTypes;

type
  TMPYarrow = class(TComponent)
  private
    FOnReseeding: TReseedEvent;
    FOnReseeded: TReseedEvent;
    FSeedFilePath: string;
    procedure DoReseeded(Slow: Boolean);
    procedure DoReseeding(Slow: Boolean);
    function GetFastReseedTime: TMilliseconds;
    function GetGateThreshold: Cardinal;
    function GetSeedFilePath: string;
    function GetSlowReseedTime: TMilliseconds;
    function GetSourceCount: Cardinal;
    procedure SetFastReseedTime(const Value: TMilliseconds);
    procedure SetGateThreshold(const Value: Cardinal);
    procedure SetSeedFilePath(const Value: string);
    procedure SetSlowReseedTime(const Value: TMilliseconds);
    procedure SetSourceCount(const Value: Cardinal);
    procedure SetOnReseeded(const Value: TReseedEvent);
    procedure SetOnReseeding(const Value: TReseedEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Accumulate(); overload;
    procedure Accumulate(const Buf; Count: Integer); overload;
    procedure Accumulate(const Buf; Count: Integer; SrcID: Cardinal); overload;
    procedure Accumulate(const Buf; Count: Integer; SrcID, EntrEst: Cardinal); overload;
    procedure ForceSlowReseed(const Buf; Count: Integer; Ticks: Cardinal);
    procedure Generate(var Buf; Count: Integer);
  published
    property FastReseedTime: TMilliseconds read GetFastReseedTime write SetFastReseedTime;
    property GateThreshold: Cardinal read GetGateThreshold write SetGateThreshold;
    property SeedFilePath: string read GetSeedFilePath write SetSeedFilePath;
    property SlowReseedTime: TMilliseconds read GetSlowReseedTime write SetSlowReseedTime;
    property SourceCount: Cardinal read GetSourceCount write SetSourceCount;    
    property OnReseeding: TReseedEvent read FOnReseeding write SetOnReseeding;
    property OnReseeded: TReseedEvent read FOnReseeded write SetOnReseeded;
  end;

function YarrowHasReseeded: Boolean;

procedure MPRawRandom(var X: PMPInteger; MaxBitSize: Cardinal);
procedure RawRandom(var X; MaxBitSize: Cardinal);
procedure RawRandomIntf(var X: ISecretKey; MaxBitSize: Cardinal);

{$IFNDEF INIT_SECTIONS}
procedure CreateGlobalYarrow(Blocking: Boolean = False);
{$ENDIF}
{$IFNDEF FINI_SECTIONS}
procedure FreeGlobalYarrow;
{$ENDIF}

implementation

uses
  SysUtils, MpArith,
  {$IFDEF MSWINDOWS}
  Windows
  {$ENDIF}
  {$IFDEF LINUX}
  Libc
  {$ENDIF};

type
  TGlobalYarrow = class(TYarrow)
  private
    FComps: TList;
    procedure AddComp(AComp: TMPYarrow);
    procedure DoReseeded(Sender: TObject; Slow: Boolean);
    procedure DoReseeding(Sender: TObject; Slow: Boolean);
    procedure RemoveComp(AComp: TMPYarrow);
  public
    constructor Create(Blocking: Boolean = False);
    destructor Destroy; override;
  end;

var
  GlobalReseeded: Boolean = False;
  GlobalYarrow: TGlobalYarrow;
                                    
function YarrowHasReseeded: Boolean;
begin
  Result := GlobalReseeded;
end;

procedure MPRawRandom(var X: PMPInteger; MaxBitSize: Cardinal);
var
  Size: Cardinal;
  BC, Mask: LongWord;
begin
  if not GlobalReseeded then
    raise Exception.Create('Yarrow has not reseeded yet');

  if MaxBitSize = 0 then
    raise Exception.Create('HighBit must be non-zero.');

  if GlobalYarrow = nil then
    raise Exception.Create('Must call CreateGlobalYarrow.');

  Size := (MaxBitSize + 31) shr 5;
  MPRealloc(X,Size);

  GlobalYarrow.Generator.Generate(X^.Data,Size*4);
  X^.Sign := 1;

  if (MaxBitSize and $1F) > 0 then begin
    BC := 1 shl (((MaxBitSize - 1) and $1F) + 1);
    Mask := BC - 1;
    X^.Data[0] := X^.Data[0] and Mask;
  end;
end;

procedure RawRandom(var X; MaxBitSize: Cardinal);
var
  Size: Cardinal;
  BC, Mask: LongWord;
begin          
  if not GlobalReseeded then
    raise Exception.Create('Yarrow has not reseeded yet');

  if MaxBitSize = 0 then
    raise Exception.Create('HighBit must be non-zero.');

  if GlobalYarrow = nil then
    raise Exception.Create('Must call CreateGlobalYarrow.');

  Size := (MaxBitSize + 7) shr 3;

  GlobalYarrow.Generator.Generate(X,Size);

  if (MaxBitSize and $7) > 0 then begin
    BC := 1 shl (((MaxBitSize - 1) and $7) + 1);
    Mask := BC - 1;
    Byte((@X)^) := Byte((@X)^) and Mask;
  end;
end;

procedure RawRandomIntf(var X: ISecretKey; MaxBitSize: Cardinal);
begin
  if X = nil then X := TSecretKey.Create('');
  X.SetLength((MaxBitSize + 7) shr 3);
  RawRandom(X.Key^,MaxBitSize);
end;

{ TMPYarrow }

procedure TMPYarrow.Accumulate;
begin
  GlobalYarrow.Entropy.Accumulate;
end;

procedure TMPYarrow.Accumulate(const Buf; Count: Integer; SrcID,
  EntrEst: Cardinal);
begin
  GlobalYarrow.Entropy.Accumulate(Buf,Count,SrcID,EntrEst);
end;

procedure TMPYarrow.Accumulate(const Buf; Count: Integer);
begin
  GlobalYarrow.Entropy.Accumulate(Buf,Count);
end;

procedure TMPYarrow.Accumulate(const Buf; Count: Integer; SrcID: Cardinal);
begin
  GlobalYarrow.Entropy.Accumulate(Buf,Count,SrcID);
end;

constructor TMPYarrow.Create(AOwner: TComponent);
begin
  if GlobalYarrow = nil  then
    raise Exception.Create('Must call CreateGlobalYarrow.');
  inherited;
  GlobalYarrow.AddComp(Self);
end;

destructor TMPYarrow.Destroy;
begin
  if Assigned(GlobalYarrow) then
    GlobalYarrow.RemoveComp(Self);
  inherited;
end;

procedure TMPYarrow.DoReseeded(Slow: Boolean);
begin
  if Assigned(FOnReseeded) then
    FOnReseeded(Self,Slow);
end;

procedure TMPYarrow.DoReseeding(Slow: Boolean);
begin
  if Assigned(FOnReseeding) then
    FOnReseeding(Self,Slow);
end;

procedure TMPYarrow.ForceSlowReseed(const Buf; Count: Integer;
  Ticks: Cardinal);
begin
  GlobalYarrow.Entropy.ForceSlowReseed(Buf,Count,Ticks);
end;

procedure TMPYarrow.Generate(var Buf; Count: Integer);
begin
  GlobalYarrow.Generator.Generate(Buf,Count);
end;

function TMPYarrow.GetFastReseedTime: TMilliseconds;
begin
  Result := GlobalYarrow.Entropy.FastReseedTime;
end;

function TMPYarrow.GetGateThreshold: Cardinal;
begin
  Result := GlobalYarrow.Generator.GateThreshold;
end;

function TMPYarrow.GetSeedFilePath: string;
begin
  if csDesigning in ComponentState then
    Result := FSeedFilePath
  else
    Result := GlobalYarrow.SeedFilePath;
end;

function TMPYarrow.GetSlowReseedTime: TMilliseconds;
begin
  Result := GlobalYarrow.Entropy.SlowReseedTime;
end;

function TMPYarrow.GetSourceCount: Cardinal;
begin
  Result := GlobalYarrow.Entropy.SourceCount;
end;

procedure TMPYarrow.SetFastReseedTime(const Value: TMilliseconds);
begin
  GlobalYarrow.Entropy.FastReseedTime := Value;
end;

procedure TMPYarrow.SetGateThreshold(const Value: Cardinal);
begin
  GlobalYarrow.Generator.GateThreshold := Value;
end;

procedure TMPYarrow.SetOnReseeded(const Value: TReseedEvent);
begin
  FOnReseeded := Value;
end;

procedure TMPYarrow.SetOnReseeding(const Value: TReseedEvent);
begin
  FOnReseeding := Value;
end;

procedure TMPYarrow.SetSeedFilePath(const Value: string);
begin
  if csDesigning in ComponentState then
    FSeedFilePath := Value
  else
    GlobalYarrow.SeedFilePath := Value;
end;

procedure TMPYarrow.SetSlowReseedTime(const Value: TMilliseconds);
begin
  GlobalYarrow.Entropy.SlowReseedTime := Value;
end;

procedure TMPYarrow.SetSourceCount(const Value: Cardinal);
begin
  GlobalYarrow.Entropy.SourceCount := Value;
end;

{ TGlobalYarrow }

procedure TGlobalYarrow.AddComp(AComp: TMPYarrow);
begin
  if Assigned(FComps) then
    FComps.Add(AComp);
end;

constructor TGlobalYarrow.Create;
begin
  FComps := TList.Create;
  inherited;
  Entropy.OnReseeding := DoReseeding;
  Entropy.OnReseeded := DoReseeded;
end;

destructor TGlobalYarrow.Destroy;
begin
  inherited;
  FComps.Free;
  FComps := nil;
end;

procedure TGlobalYarrow.DoReseeded(Sender: TObject; Slow: Boolean);
var
  I: Integer;
begin
  GlobalReseeded := True;
  if Assigned(FComps) then
    for I := 0 to FComps.Count - 1 do
      TMPYarrow(FComps[I]).DoReseeded(Slow);
end;

procedure TGlobalYarrow.DoReseeding(Sender: TObject; Slow: Boolean);
var
  I: Integer;
begin
  if Assigned(FComps) then
    for I := 0 to FComps.Count - 1 do
      TMPYarrow(FComps[I]).DoReseeding(Slow);
end;

procedure TGlobalYarrow.RemoveComp(AComp: TMPYarrow);
begin
  if Assigned(FComps) then
    FComps.Remove(AComp);
end;

procedure CreateGlobalYarrow(Blocking: Boolean);
begin
  GlobalYarrow := TGlobalYarrow.Create(Blocking);
  Yarrow.RawRandom := MPYarrow.RawRandom;
end;

procedure FreeGlobalYarrow;
var
  GY: TGlobalYarrow;
begin
  GY := GlobalYarrow;
  GlobalYarrow := nil;
  GY.Free;
end;
              
{$IFDEF INITIALIZATION}
initialization
{$ENDIF}
{$IFDEF INIT_SECTIONS}
  CreateGlobalYarrow({$IFDEF LINUX}True{$ENDIF}{$IFDEF MSWINDOWS}False{$ENDIF});
  {$IFDEF MSWINDOWS}
    GlobalYarrow.Entropy.ForceSlowReseed(nil^,0,1000);
  {$ENDIF}
  {$IFDEF LINUX}
    GlobalYarrow.Entropy.ForceSlowReseed(nil^,0,100);
  {$ENDIF}
{$ENDIF}
{$IFDEF FINI_SECTIONS}
finalization
  FreeGlobalYarrow;
{$ENDIF}
end.
