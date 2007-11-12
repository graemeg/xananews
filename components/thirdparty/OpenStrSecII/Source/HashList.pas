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
}
{$I ver.inc}
unit HashList;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  Classes;

type
  THashList = class(TObject)
  private
    FCapacity: Integer;
    FLists: array [0..255] of TList;
    function GetCount: Integer;
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
  protected
    function FindList(Item: Pointer; out IndexOffset: Integer): TList;
    function Get(Index: Integer): Pointer;
    function GetList(var Index: Integer): TList;
    procedure Put(Index: Integer; Item: Pointer);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    function Remove(Item: Pointer): Integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
  end;

  TThreadHashList = class
  private
    FList: THashList;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    function  LockList: THashList;
    procedure UnlockList;
  end;

implementation

{ THashList }

function THashList.Add(Item: Pointer): Integer;
var
  List: TList;
begin
  List := FindList(Item,Result);
  Result := Result + List.Add(Item);
  FCapacity := 0;
end;

procedure THashList.Clear;
var
  I: Integer;
begin
  for I := 0 to 255 do
    FLists[I].Clear;
end;

constructor THashList.Create;
var
  I: Integer;
begin
  for I := 0 to 255 do
    FLists[I] := TList.Create;
end;

procedure THashList.Delete(Index: Integer);
var
  List: TList;
begin
  List := GetList(Index);
  if Assigned(List) then
    List.Delete(Index);
end;

destructor THashList.Destroy;
var
  I: Integer;
begin
  for I := 0 to 255 do
    FLists[I].Free;
  inherited;
end;

function THashList.FindList(Item: Pointer;
  out IndexOffset: Integer): TList;
var
  Hash: Byte;
begin
  Hash := LongInt(Item) xor
          (LongInt(Item) shr 8) xor
          (LongInt(Item) shr 16) xor
          (LongInt(Item) shr 24);
  Result := FLists[Hash];
  IndexOffset := 0;
  while Hash > 0 do begin
    Dec(Hash);
    IndexOffset := IndexOffset + FLists[Hash].Count;
  end;
end;

function THashList.First: Pointer;
begin
  Result := Items[0];
end;

function THashList.Get(Index: Integer): Pointer;
var
  List: TList;
begin
  List := GetList(Index);
  if Assigned(List) then
    Result := List[Index]
  else
    Result := nil;
end;

function THashList.GetCapacity: Integer;
var
  I: Integer;
begin
  Result := FCapacity;
  if Result = 0 then
    for I := 0 to 255 do
      Result := Result + FLists[I].Capacity;
end;

function THashList.GetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 255 do
    Result := Result + FLists[I].Count;
end;

function THashList.GetList(var Index: Integer): TList;
var
  I: Integer;
begin
  for I := 0 to 255 do begin
    Result := FLists[I];
    if Index < Result.Count then Exit;
    Index := Index - Result.Count;
  end;
  Result := nil;
end;

function THashList.IndexOf(Item: Pointer): Integer;
var
  List: TList;
  Offs: Integer;
begin
  List := FindList(Item,Offs);
  Result := List.IndexOf(Item);
  if Result > -1 then
    Result := Result + Offs;
end;

type
  THack = class(TList);

procedure THashList.Put(Index: Integer; Item: Pointer);
var
  List: TList;
begin
  List := GetList(Index);
  if Assigned(List) then
    THack(List).Put(Index,Item);
  FCapacity := 0;
end;

function THashList.Remove(Item: Pointer): Integer;
var
  List: TList;
  Offs: Integer;
begin
  List := FindList(Item,Offs);
  Result := List.Remove(Item);
  if Result > -1 then
    Result := Result + Offs;
end;

procedure THashList.SetCapacity(const Value: Integer);
var
  I: Integer;
  dCap: Integer;
begin
  FCapacity := GetCapacity;
  if Value > FCapacity then begin
    dCap := ((Value - FCapacity) shr 8);
    for I := 0 to 255 do
      FLists[I].Capacity := FLists[I].Capacity + dCap;
    Inc(FCapacity,dCap);
  end else begin
    FCapacity := 0;
    dCap := Value shr 8;
    for I := 0 to 255 do
      if dCap >= FLists[I].Count then begin
        FLists[I].Capacity := dCap;
        Inc(FCapacity,dCap);
      end else
        Inc(FCapacity,FLists[I].Capacity);
  end;
end;

{ TThreadHashList }

constructor TThreadHashList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FList := THashList.Create;
end;

destructor TThreadHashList.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;
end;

function TThreadHashList.LockList: THashList;
begin
  EnterCriticalSection(FLock);
  Result := FList;
end;

procedure TThreadHashList.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

end.
