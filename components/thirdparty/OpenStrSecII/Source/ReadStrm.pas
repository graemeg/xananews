{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     ReadStrm Unit                                     }
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
unit ReadStrm;

interface

uses
  Classes, SysUtils;

resourcestring
  SReadOnly = 'Stream is read only';
  SFixedSize = 'Cannot write past end of buffer';
type
  EReadOnly = class(Exception);

  TReadStream = class(TCustomMemoryStream)
  public
    constructor Create(const Buf; Count: Integer);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TWriteStream = class(TReadStream)
  private
    FCapacity: Longint;
    procedure SetCapacity(const Value: Longint);
  protected
    property Capacity: Longint read FCapacity write SetCapacity;
  public
    constructor Create(var Buf; Count: Integer);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TSecureMemoryStream = class(TMemoryStream)
  private
    FRefCount: Integer;
    FInvalid: Boolean;
  protected
{$IFDEF D7UP}
    function GetSize: Int64; override;
{$ENDIF D7UP}
    function Realloc(var NewCapacity: Longint): Pointer; override;
  public
    procedure AddRef;
    procedure Invalidate;
    procedure LoadFromFile(const FileName: string);
    procedure Release;
    procedure SaveToFile(const FileName: string);
{$IFDEF D6UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
{$ENDIF D6UP}
    procedure SetSize(NewSize: Longint); override;
    property Capacity;
    property Invalid: Boolean read FInvalid;
    property RefCount: Integer read FRefCount;
  end;

implementation

uses
  SecUtils, {$IFDEF D6UP}RTLConsts{$ELSE}Consts{$ENDIF},
  {$IFDEF MSWINDOWS}
  Windows
  {$ENDIF}
  {$IFDEF LINUX}
  Libc
  {$ENDIF};

{ TReadStream }

constructor TReadStream.Create(const Buf; Count: Integer);
begin
  SetPointer(@Buf,Count);
  Position := 0;
end;

function TReadStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise EReadOnly.Create(SReadOnly);
end;

{ TSecureMemoryStream }

const
  MemoryDelta = $2000; { Must be a power of 2 }

procedure TSecureMemoryStream.AddRef;
begin
  Inc(FRefCount);
end;

{$IFDEF D7UP}
function TSecureMemoryStream.GetSize: Int64;
var
  Pos: LongInt;
begin
  Pos := Seek(0, soFromCurrent);
  Result := Seek(0, soFromEnd);
  Seek(Pos, soFromBeginning);
end;
{$ENDIF D7UP}

procedure TSecureMemoryStream.Invalidate;
begin
  FInvalid := True;
  if FRefCount = 0 then
    Free;
end;

procedure TSecureMemoryStream.LoadFromFile(const FileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    SetSize(FS.Size);
    FS.Read(Memory^,Size);
  finally
    FS.Free;
  end;
end;

function TSecureMemoryStream.Realloc(var NewCapacity: Integer): Pointer;
begin
  if NewCapacity > 0 then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1)
  else
    NewCapacity := 0;
  Result := Memory;
  if NewCapacity <> Capacity then begin
    if NewCapacity = 0 then begin
      Result := nil;
    end else begin
      {$IFDEF MSWINDOWS}
      Result := GlobalAllocPtr(GMEM_FIXED or GMEM_FIXED, NewCapacity);
      {$ELSE}
      GetMem(Result,NewCapacity);
      {$ENDIF}
      {$IFDEF D5UP}
      if Result = nil then raise EStreamError.CreateRes(@SMemoryStreamError);
      {$ELSE}
      if Result = nil then raise EStreamError.Create(SMemoryStreamError);
      {$ENDIF}
      if Assigned(Memory) and (Size > 0) then begin
        if NewCapacity > Size then
          System.Move(Memory^,Result^,Size)
        else
          System.Move(Memory^,Result^,NewCapacity);
      end;
    end;
    if Assigned(Memory) then begin
      ProtectClear(Memory^,Size);
      {$IFDEF MSWINDOWS}
      GlobalFreePtr(Memory);
      {$ELSE}
      FreeMem(Memory);
      {$ENDIF}
    end;
  end;
end;

procedure TSecureMemoryStream.Release;
begin
  Dec(FRefCount);
  if FRefCount <= 0 then
    Free;
end;

procedure TSecureMemoryStream.SaveToFile(const FileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName,fmCreate);
  try
    FS.Write(Memory^,Size);
  finally
    FS.Free;
  end;
end;

{$IFDEF D6UP}
function TSecureMemoryStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  if (Offset < Low(LongInt)) or (Offset > High(LongInt)) then
    Result := inherited Seek(Offset,Origin)
  else
    Result := Seek(LongInt(Offset),Ord(Origin));
end;
{$ENDIF D6UP}

procedure TSecureMemoryStream.SetSize(NewSize: Integer);
var
  OldPosition: Longint;
begin
  if NewSize < 0 then NewSize := 0;
  OldPosition := Position;
  if NewSize > Capacity then Capacity := NewSize;
  SetPointer(Memory,NewSize);
  if OldPosition > NewSize then Seek(0, soFromEnd);
end;

{ TWriteStream }

constructor TWriteStream.Create(var Buf; Count: Integer);
begin
  inherited Create(Buf,0);
  SetCapacity(Count);
end;

procedure TWriteStream.SetCapacity(const Value: Longint);
begin
  FCapacity := Value;
end;

function TWriteStream.Write(const Buffer; Count: Integer): Longint;
var
  OldPos, Pos: Longint;
begin
  Result := 0;
  OldPos := Position;
  if (OldPos >= 0) and (Count >= 0) then begin
    Pos := OldPos + Count;
    if Pos > 0 then begin
      if Pos > Size then begin
        if Pos > FCapacity then
          raise EReadOnly.Create(SFixedSize);
        SetPointer(Memory,Pos);
      end;
      System.Move(Buffer, Pointer(Longint(Memory) + OldPos)^, Count);
      Position := Pos;
      Result := Count;
    end;
  end;
end;

end.
