(*======================================================================*
 | unitStreamTextReader                                                 |
 |                                                                      |
 | Various classes for reading (and writing) text from streams:         |
 |                                                                      |
 |   TStreamTextReader is a fairly fast, buffered CRLF or LF separated  |
 |   line reader                                                        |
 |                                                                      |
 |   TStreamWideTextReader is a fairly fast, buffered CRLF or LF        |
 |   separated line reader for wide-text files                          |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2005  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 10.0     08/03/2006  CPWW  BDS 2006 release version                  |
 *======================================================================*)
unit unitStreamTextReader;

interface

uses
  Windows, Classes, SysUtils, XnClasses;

type
  TStreamTextReader = class
  private
    fStream: TStream;
    fBuffer: PAnsiChar;

    fbufPos: Int64;
    fBufSize: Int64;
    fBlockSize: Int64;

    procedure GetChunk;
    function GetPosition: Int64;
    procedure SetPosition(const Value: Int64);
    procedure SetStream(const Value: TStream);

  public
    constructor Create(AStream: TStream; blockSize: Integer = 1024);
    destructor Destroy; override;
    function GetChar: AnsiChar;
    function ReadLn(var st: MessageString; continuationChar: AnsiChar = #0): Boolean; overload;
    function ReadLn(ms: TMemoryStream; continuationChar: AnsiChar = #0): Boolean; overload;
    procedure ReadChunk(var chunk; offset, length: Integer);
    property Position: Int64 read GetPosition write SetPosition;
    function Search(const st: MessageString): Integer;
    property Stream: TStream read fStream write SetStream;
  end;

  TMappedFile = class
  private
    fSize: Int64;
    fMemory: PAnsiChar;
    fFileHandle, fMappingHandle: THandle;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    property Size: Int64 read fSize;
    property Memory: PAnsiChar read fMemory;
  end;

  TMappedFileStream = class(TStream)
  private
    fPosition: Int64;
    fSize: Int64;
    fMemory: PAnsiChar;
    fFileHandle, fMappingHandle: THandle;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TTextFileReader = class(TMappedFile)
  private
    fPosition: Int64;
  public
    function ReadLn(var st: MessageString): Boolean;
  end;

  TBufferedStreamWriter = class
  private
    fStream: TStream;
    fBuffer: PAnsiChar;
    fBufPos, fBufSize: Int64;
    function GetPosition: Int64;
  public
    constructor Create(const AStream: TStream);
    destructor Destroy; override;
    procedure Write(const data; dataLen: Integer);
    procedure FlushBuffer;
    property Position: Int64 read GetPosition;
  end;

  TBufferedFileWriter = class(TBufferedStreamWriter)
  private
    fStream: TFileStream;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
  end;

  TTextFileWriter = class(TBufferedFileWriter)
  public
    procedure Write(const st: string);  overload;
    procedure WriteLn(const st: string);  overload;
    procedure Write(const st: RawByteString); overload;
    procedure WriteLn(const st: RawByteString);  overload;
  end;

  TTextStreamWriter = class(TBufferedStreamWriter)
  public
    procedure Write(const st: string);  overload;
    procedure WriteLn(const st: string);  overload;
    procedure Write(const st: RawByteString); overload;
    procedure WriteLn(const st: RawByteString);  overload;
  end;

implementation

uses
  unitSearchString;

(*----------------------------------------------------------------------*
 | StrLScan                                                             |
 |                                                                      |
 | Search for a character in the first 'len' bytes of the specified     |
 | block of memory                                                      |
 |                                                                      |
 | Parameters:                                                          |
 |   const Str: PAnsiChar; ch: char; len: DWORD                         |
 |                                                                      |
 | The function returns PAnsiChar                                       |
 *----------------------------------------------------------------------*)
function StrLScan(const Str: PAnsiChar; ch: Char; len: DWORD): PAnsiChar;
asm
        // EAX = Str
        // DL = char
        // ECX = len

        PUSH    EDI
        MOV     EDI, EAX
        MOV     AL, DL
        REPNE   SCASB
        JNE     @1              // Fail
        MOV     EAX, EDI
        DEC     EAX
        POP     EDI
        RET

@1:     XOR     EAX, EAX
        POP     EDI
end;

{ TStreamTextReader }

constructor TStreamTextReader.Create(AStream: TStream; blockSize: Integer = 1024);
begin
  fStream := AStream;
  fBlockSize := blockSize;
  GetMem(fBuffer, fBlockSize + 1);
  fBuffer[blockSize] := #0
end;

destructor TStreamTextReader.Destroy;
begin
  FreeMem(fBuffer);
  inherited Destroy;
end;

function TStreamTextReader.GetChar: AnsiChar;
begin
  GetChunk;
  if fBufPos < fBufSize then
  begin
    Result := fBuffer[fBufPos];
    Inc(fBufPos);
  end
  else
    Result := #0;
end;

procedure TStreamTextReader.GetChunk;
begin
  if fBufPos = fBufSize then
  begin
    fBufPos := 0;
    fBufSize := fBlockSize;
    if fBufSize > fStream.Size - fStream.Position then
      fBufSize := fStream.Size - fStream.Position;

    fBufSize := fStream.Read(fBuffer^, fBufSize);
    if fBufSize < fBlockSize then
      fBuffer[fBufSize] := #10;
  end;
end;

function TStreamTextReader.GetPosition: Int64;
begin
  Result := fStream.Position - fBufSize + fBufPos;
end;

procedure TStreamTextReader.ReadChunk(var chunk; offset, length: Integer);
begin
  Position := offset;
  fStream.Read(chunk, length)
end;

function TStreamTextReader.ReadLn(var st: MessageString; continuationChar: AnsiChar = #0): Boolean;
var
  L, lineStartPos: Integer;
  pch, pch1: PAnsiChar;
  st1: MessageString;
  cont, scont: Boolean;
begin
  lineStartPos := Position;
  cont := True;
  scont := false;
  st := '';
  while cont do
  begin
    GetChunk;
    if fBufPos = fBufSize then
      Break;

    pch := fBuffer;
    Inc(pch, fBufPos);
    pch1 := StrScan(pch, #10);

    if pch1 <> nil then
    begin
      L := pch1 - pch;
      Inc(fBufPos, L + 1);
      if fBufPos > fBufSize then
        fBufPos := fBufSize;
      if L > 0 then
      begin
        repeat
          Dec(pch1);
          if pch1^ = #13 then
            Dec(L)
          else
            Break;
        until pch1 = pch;
        cont := pch1^ = continuationChar;
      end
      else
        cont := scont;
      SetLength(st1, L);
      if L > 0 then
        Move(pch^, PAnsiChar(st1)^, L);
    end
    else
    begin
      st1 := pch;
      L := Length(st1);

      while (L > 0) and (st1[L] = #13) do
        Dec(L);

      if L < Length(st1) then
        SetLength(st1, L);

      scont := (L > 0) and (st1[L] = continuationChar);

      fBufPos := fBufSize;
    end;

    st := st + st1;
  end;

  if cont then
  begin
    Position := lineStartPos;
    Result := False;
  end
  else
    Result := True;
end;

function TStreamTextReader.ReadLn(ms: TMemoryStream; continuationChar: AnsiChar = #0): Boolean;
var
  L, lineStartPos: Integer;
  pch, pch1: PAnsiChar;
  st1: MessageString;
  cont, scont: Boolean;
begin
  lineStartPos := Position;
  cont := True;
  scont := false;

  ms.Clear;
  while cont do
  begin
    GetChunk;
    if fBufPos = fBufSize then
      Break;

    pch := fBuffer;
    Inc(pch, fBufPos);
    pch1 := StrScan(pch, #10);

    if pch1 <> nil then
    begin
      L := pch1 - pch;
      Inc(fBufPos, L + 1);
      if fBufPos > fBufSize then
        fBufPos := fBufSize;
      if L > 0 then
      begin
        repeat
          Dec(pch1);
          if pch1^ = #13 then
            Dec(L)
          else
            Break;
        until pch1 = pch;
        cont := pch1^ = continuationChar;
      end
      else
        cont := scont;

      ms.Write(pch^, L);
    end
    else
    begin
      st1 := pch;
      L := Length(st1);

      while (L > 0) and (st1[L] = #13) do
        Dec(L);

      if L < Length(st1) then
        SetLength(st1, L);

      scont := (L > 0) and (st1[L] = continuationChar);

      ms.Write(pch^, L);

      fBufPos := fBufSize;
    end;
  end;

  ms.Seek(0, soBeginning);

  if cont then
  begin
    Position := lineStartPos;
    Result := False;
  end
  else
    Result := True;
end;

function TStreamTextReader.Search(const st: MessageString): Integer;
var
  p, p1: PAnsiChar;
begin
  Result := -1;
  if Length(st) <> 0 then
  repeat
    GetChunk;
    if fBufSize < Length(st) then
      Exit;

    p := fBuffer;
    Inc(p, fBufPos);

    p1 := StrPos(p, PAnsiChar(AnsiString(st)));

    if p1 <> nil then
    begin
      Result := (fStream.Position - fBufSize) + (p1 - fBuffer);
      fBufPos := (p1 - fBuffer) + Length(st);
      Break
    end;

    fStream.Position := fStream.Position - (Length(st) - 1);
    fBufPos := 0;
    fBufSize := 0;
  until False;
end;

procedure TStreamTextReader.SetPosition(const Value: Int64);
begin
  if Value <> Position then
  begin
    fBufSize := 0;
    fBufPos := 0;
    fStream.Position := Value;
  end
end;

procedure TStreamTextReader.SetStream(const Value: TStream);
begin
  fStream := Value;
  fBufPos := 0;
  fBufSize := 0;
end;

{ TMappedFile }

constructor TMappedFile.Create(const AFileName: string);
begin
  fFileHandle := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if fFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  fSize := GetFileSize(fFileHandle, nil);
  if fSize <> 0 then
  begin
    fMappingHandle := CreateFileMapping(fFileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if fMappingHandle = 0 then
      RaiseLastOSError;

    fMemory := MapViewOfFile(fMappingHandle, FILE_MAP_READ, 0, 0, 0);
    if fMemory = nil then
      RaiseLastOSError;
  end;
end;

destructor TMappedFile.Destroy;
begin
  if fMemory <> nil then
    UnmapViewOfFile(fMemory);

  if fMappingHandle <> 0 then
    CloseHandle(fMappingHandle);

  if fFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(fFileHandle);

  inherited Destroy;
end;

{ TTextFileReader }

function TTextFileReader.ReadLn(var st: MessageString): Boolean;
var
  p, p1: PAnsiChar;
  L: Integer;
begin
  L := fSize - fPosition;
  if L > 0 then
  begin
    Result := True;
    p1 := fMemory + fPosition;
    p := StrLScan(p1, #10, L);

    if p <> nil then
    begin
      L := p - p1;
      Inc(fPosition, L + 1);

      while (L > 0) and (p1[L - 1] = #13) do
        Dec(L);
    end
    else
      Inc(fPosition, L);

    SetString(st, p1, L);
  end
  else
    Result := False;
end;

{ TTextFileWriter }

procedure TTextFileWriter.Write(const st: string);
var
  EncodedString: MessageString;
begin
  if st <> '' then
  begin
    EncodedString := MessageString(st);
    inherited Write(EncodedString[1], Length(EncodedString));
  end;
end;

procedure TTextFileWriter.WriteLn(const st: string);
begin
  Write(st + #13#10);
end;

procedure TTextFileWriter.Write(const st: RawByteString);
begin
  if st <> '' then
    inherited Write(st[1], Length(st));
end;

procedure TTextFileWriter.WriteLn(const st: RawByteString);
begin
  Write(st + #13#10);
end;

{ TBufferedFileWriter }

constructor TBufferedFileWriter.Create(const AFileName: string);
begin
  fStream := TFileStream.Create(AFileName, fmCreate);
  inherited Create(fStream);
end;

destructor TBufferedFileWriter.Destroy;
begin
  inherited Destroy;  // Note: needs to be first to flush the buffers!
  FreeAndnil(fStream);
end;

{ TMappedFileStream }

function TMappedFileStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create('TMappedFileStream is read only');
end;

constructor TMappedFileStream.Create(const AFileName: string);
begin
  fFileHandle := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if fFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  fSize := GetFileSize(fFileHandle, nil);
  if fSize <> 0 then
  begin
    fMappingHandle := CreateFileMapping(fFileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if fMappingHandle = 0 then
      RaiseLastOSError;

    fMemory := MapViewOfFile(fMappingHandle, FILE_MAP_READ, 0, 0, 0);
    if fMemory = nil then
      RaiseLastOSError;
  end
end;

function TMappedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: fPosition := Offset;
    soEnd:       fPosition := fSize - Offset;
    soCurrent:   Inc(fPosition, Offset);
  end;

  if fPosition > fSize then
    fPosition := fSize;

  Result := fPosition;
end;

function TMappedFileStream.Read(var Buffer; Count: Integer): Longint;
begin
  if Count <= (fSize - fPosition) then
    Result := Count
  else
    Result := fSize - fPosition;

  if Result < 0 then
    Result := 0;

  if Result > 0 then
  begin
    Move(fMemory[fPosition], Buffer, Result);
    Inc(fPosition, Result);
  end;
end;

destructor TMappedFileStream.Destroy;
begin
  if fMemory <> nil then
    UnmapViewOfFile(fMemory);

  if fMappingHandle <> 0 then
    CloseHandle(fMappingHandle);

  if fFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(fFileHandle);

  inherited Destroy;
end;

{ TBufferedStreamWriter }

constructor TBufferedStreamWriter.Create(const AStream: TStream);
begin
  fBufSize := 65536;
  GetMem(fBuffer, fBufSize);
  fStream := AStream;
end;

destructor TBufferedStreamWriter.Destroy;
begin
  FlushBuffer;
  FreeMem(fBuffer);

  inherited Destroy;
end;

procedure TBufferedStreamWriter.FlushBuffer;
begin
  fStream.Write(fBuffer^, fBufPos);
  fBufPos := 0;
end;

function TBufferedStreamWriter.GetPosition: Int64;
begin
  Result := fStream.Position + fBufPos;
end;

procedure TBufferedStreamWriter.Write(const data; dataLen: Integer);
var
  stPos, chunkLen: Integer;
  p :PAnsiChar;
begin
  stPos := 0;
  while stPos < dataLen do
  begin
    chunkLen := fBufSize - fBufPos;
    if chunkLen = 0 then
    begin
      FlushBuffer;
      chunkLen := fBufSize - fBufPos;
    end;

    if chunkLen > dataLen - stPos then
      chunkLen := dataLen - stPos;

    p := PAnsiChar(@data);
    Inc(p, stPos);
    Move(p^, (fBuffer + fBufPos)^, chunkLen);
    Inc(stPos, chunkLen);
    Inc(fBufPos, chunkLen);
  end;
end;

{ TTextStreamWriter }

procedure TTextStreamWriter.Write(const st: string);
var
  EncodedString: MessageString;
begin
  if st <> '' then
  begin
    EncodedString := MessageString(st);
    inherited Write(EncodedString[1], Length(EncodedString));
  end;
end;

procedure TTextStreamWriter.WriteLn(const st: string);
begin
  Write(st + #13#10);
end;

procedure TTextStreamWriter.Write(const st: RawByteString);
begin
  if st <> '' then
    inherited Write(st[1], Length(st));
end;

procedure TTextStreamWriter.WriteLn(const st: RawByteString);
begin
  Write(st + #13#10);
end;

end.
