{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     TLSUtils Unit                                     }
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
unit TlsUtils;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows
  {$ENDIF}
  {$IFDEF LINUX}
  Libc
  {$ENDIF}
  ;

type
  TRandomString = packed array [0..31] of Char;

  TGMTUnixTime = LongWord;

  TRandom = packed record
    case Byte of
      0: (gmt_unix_time: TGMTUnixTime;
          random_bytes:  packed array [0..27] of Char);
      1: (AsString:      TRandomString);
  end;

{$IFDEF MSWINDOWS}
procedure UnixTimeToSystemTime(UnixTime: TGMTUnixTime; var SystemTime: TSystemTime);
function SystemTimeToUnixTime(const SystemTime: TSystemTime): TGMTUnixTime;
{$ENDIF}
{$IFDEF LINUX}                                      
function GetTickCount: Cardinal;
procedure QueryPerformanceCounter(var Value: Int64);
{$ENDIF LINUX}
function UnixTime: TGMTUnixTime;

function ByteSwap(Value: LongWord): Longword; overload;
function ByteSwap(Value: Int64): Int64; overload;

procedure TLS_NewRandom(var Random: TRandom);

implementation

uses
  {$IFDEF LINUX}DateUtils, SysUtils, {$ENDIF}
  Yarrow;

{$IFDEF MSWINDOWS}
const
  ZTime: Int64 = $019DB1DED53E8000;

procedure UnixTimeToSystemTime(UnixTime: TGMTUnixTime; var SystemTime: TSystemTime);
var
  FT: TFileTime;
  IT: Int64 absolute FT;
begin
  IT := (UnixTime * 10000000) + ZTime;
  FileTimeToSystemTime(FT,SystemTime);
end;

function SystemTimeToUnixTime(const SystemTime: TSystemTime): TGMTUnixTime;
var
  FT: TFileTime;
  IT: Int64 absolute FT;
begin
  SystemTimeToFileTime(SystemTime,FT);
  IT := IT - ZTime;
  Result := IT div 10000000;
end;

function UnixTime: TGMTUnixTime;
var
  SystemTime: TSystemTime;
begin
  GetSystemTime(SystemTime);
  Result := SystemTimeToUnixTime(SystemTime);
end;
{$ENDIF}

{$IFDEF LINUX}
function GetTickCount: Cardinal;
var
  tv: timeval;
begin
  gettimeofday(tv, nil);
  Result := (Int64(tv.tv_sec) shl 10) +
            tv.tv_usec shr 10;
end;

procedure QueryPerformanceCounter(var Value: Int64);
begin
  Value := clock;
end;

function UnixTime: TGMTUnixTime;
begin
  Result := DateTimeToUnix(Now);
end;
{$ENDIF}

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

procedure TLS_NewRandom(var Random: TRandom);
begin
  Random.gmt_unix_time := ByteSwap(UnixTime);
  RawRandom(Random.random_bytes,28*8);
end;

end.
