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

uses Windows, Classes, SysUtils;

type

TStreamTextReader = class
private
  fStream : TStream;
  fBuffer : PAnsiChar;

  fbufPos : Integer;
  fBufSize : Integer;
  fBlockSize : Integer;

  procedure GetChunk;
  function GetPosition: Integer;
  procedure SetPosition(const Value: Integer);
  procedure SetStream(const Value: TStream);

public
  constructor Create (AStream : TStream; blockSize : Integer = 1024);
  destructor Destroy; override;
  function GetChar : AnsiChar;
  function ReadLn (var st : string; continuationChar : AnsiChar = #0) : boolean;
  procedure ReadChunk (var chunk; offset, length : Integer);
  property Position : Integer read GetPosition write SetPosition;
  function Search (const st : string) : Integer;
  property Stream : TStream read fStream write SetStream;
end;

TStreamWideTextReader = class
private
  fStream : TStream;
  fBuffer : PWideChar;

  fbufPos : Integer;
  fBufSize : Integer;
  fBlockSize : Integer;

  procedure GetChunk;
  function GetPosition: Integer;
  procedure SetPosition(const Value: Integer);
  procedure SetStream(const Value: TStream);

public
  constructor Create (AStream : TStream; blockSize : Integer = 1024);
  destructor Destroy; override;
  function GetChar : WideChar;
  function ReadLn (var st : WideString; continuationChar : WideChar = #0) : boolean;
  property Position : Integer read GetPosition write SetPosition;
  property Stream : TStream read fStream write SetStream;
end;

TMappedFile = class
private
  fSize : Integer;
  fMemory : PAnsiChar;
  fFileHandle, fMappingHandle : THandle;
public
  constructor Create (const AFileName : string);
  destructor Destroy; override;

  property Size : Integer read fSize;
  property Memory : PAnsiChar read fMemory;
end;

TMappedFileStream = class (TStream)
private
  fPosition : Integer;
  fSize : Integer;
  fMemory : PAnsiChar;
  fFileHandle, fMappingHandle : THandle;
public
  constructor Create (const AFileName : string);
  destructor Destroy; override;

  function Read(var Buffer; Count: Longint): Longint; override;
  function Write(const Buffer; Count: Longint): Longint; override;
  function Seek(Offset: Longint; Origin: Word): Longint; override;
end;

TTextFileReader = class (TMappedFile)
private
  fPosition : Integer;
public
  function ReadLn (var st : string) : boolean;
end;

TBufferedStreamWriter = class
private
  fStream : TStream;
  fBuffer : PAnsiChar;
  fBufPos, fBufSize : Integer;
  function GetPosition: Integer;
public
  constructor Create (const AStream : TStream);
  destructor Destroy; override;
  procedure Write (const data; dataLen : Integer);
  procedure FlushBuffer;
  property Position : Integer read GetPosition;
end;

TBufferedFileWriter = class (TBufferedStreamWriter)
private
  fStream : TFileStream;
public
  constructor Create (const AFileName : string);
  destructor Destroy; override;
end;

TTextFileWriter = class (TBufferedFileWriter)
public
  procedure Write(const st : string);
  procedure WriteLn(const st : string);
end;

TTextStreamWriter = class (TBufferedStreamWriter)
public
  procedure Write(const st : string);
  procedure WriteLn(const st : string);
end;


implementation

uses unitSearchString;

(*----------------------------------------------------------------------*
 | StrLScan                                                             |
 |                                                                      |
 | Search for a character in the first 'len' bytes of the specified     |
 | block of memory                                                      |
 |                                                                      |
 | Parameters:                                                          |
 |   const Str: PAnsiChar; ch : char; len : DWORD                       |
 |                                                                      |
 | The function returns PAnsiChar                                       |
 *----------------------------------------------------------------------*)
function StrLScan(const Str: PAnsiChar; ch : Char; len : DWORD): PAnsiChar;
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

constructor TStreamTextReader.Create(AStream: TStream; blockSize: Integer);
begin
  fStream := AStream;
  fBlockSize := blockSize;
  GetMem (fBuffer, fBlockSize + 1);
  fBuffer [blockSize] := #0
end;

destructor TStreamTextReader.Destroy;
begin
  FreeMem (fBuffer);
  inherited;
end;

function TStreamTextReader.GetChar: AnsiChar;
begin
  GetChunk;
  if fBufPos < fBufSize then
  begin
    result := fBuffer [fBufPos];
    Inc (fBufPos)
  end
  else
    result := #0
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
      fBuffer [fBufSize] := #10
  end;
end;

function TStreamTextReader.GetPosition: Integer;
begin
  result := fStream.Position - fBufSize + fBufPos;
end;

procedure TStreamTextReader.ReadChunk(var chunk; offset, length: Integer);
begin
  Position := offset;
  fStream.Read(chunk, length)
end;

function TStreamTextReader.ReadLn(var st: string; continuationChar: AnsiChar): boolean;
var
  L, lineStartPos : Integer;
  pch, pch1 : PAnsiChar;
  st1 : UTF8String;
  cont, scont : boolean;
begin
  lineStartPos := Position;
  cont := True;
  scont := false;
  st := '';
  while cont do
  begin
    GetChunk;
    if fBufPos = fBufSize then
      break;

    pch := fBuffer;
    Inc (pch, fBufPos);
    pch1 := StrScan (pch, #10);

    if pch1 <> nil then
    begin
      L := pch1 - pch;
      Inc (fBufPos, L + 1);
      if fBufPos > fBufSize then
        fBufPos := fBufSize;
      if L > 0 then
      begin
        repeat
          Dec (pch1);
          if pch1^ = #13 then
            Dec (L)
          else
            break
        until pch1 = pch;
        cont := pch1^ = continuationChar;
      end
      else
        cont := scont;
      SetLength (st1, L);
      if L > 0 then
        Move (pch^, PAnsiChar(st1)^, L);
    end
    else
    begin
      st1 := pch;
      L := Length (st1);

      while (L > 0) and (st1 [L] = #13) do
        Dec (L);

      if L < Length (st1) then
        SetLength (st1, L);

      scont := (L > 0) and (st1[L] = continuationChar);

      fBufPos := fBufSize
    end;

// TODO: check UTF8 decoding
//    st := st + st1
    st := st + UTF8ToString(st1);
  end;

  if cont then
  begin
    Position := lineStartPos;
    result := False
  end
  else
    result := True;
end;

function TStreamTextReader.Search(const st: string): Integer;
var
  p, p1 : PAnsiChar;
begin
  result := -1;
  if Length (st) <> 0 then
  repeat
    GetChunk;
    if fBufSize < Length (st) then
      Exit;

    p := fBuffer;
    Inc (p, fBufPos);

// TODO: searching
    p1 := StrPos (p, PAnsiChar (AnsiString(st)));

    if p1 <> Nil then
    begin
      result := (fStream.Position - fBufSize) + Integer (p1) - Integer (fBuffer);
      fBufPos := Integer (p1) - Integer (fBuffer) + Length (st);
      break
    end;

    fStream.Position := fStream.Position - (Length (st) - 1);
    fBufPos := 0;
    fBufSize := 0;
  until False
end;

procedure TStreamTextReader.SetPosition(const Value: Integer);
begin
  if Value <> Position then
  begin
    fBufSize := 0;
    fBufPos := 0;
    fStream.Position := Value
  end
end;

procedure TStreamTextReader.SetStream(const Value: TStream);
begin
  fStream := Value;
  fBufPos := 0;
  fBufSize := 0
end;

{ TMappedFile }

constructor TMappedFile.Create(const AFileName: string);
begin
  fFileHandle := CreateFile (PChar (AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if fFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  fSize := GetFileSize (fFileHandle, nil);
  if fSize <> 0 then
  begin
    fMappingHandle := CreateFileMapping (fFileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if fMappingHandle = 0 then
      RaiseLastOSError;

    fMemory := MapViewOfFile (fMappingHandle, FILE_MAP_READ, 0, 0, 0);
    if fMemory = Nil then
      RaiseLastOSError;
  end
end;

destructor TMappedFile.Destroy;
begin
  if fMemory <> Nil then
    UnmapViewOfFile (fMemory);

  if fMappingHandle <> 0 then
    CloseHandle (fMappingHandle);

  if fFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle (fFileHandle);

  inherited;
end;

{ TTextFileReader }

function TTextFileReader.ReadLn(var st: string): boolean;
var
  p, p1: PAnsiChar;
  L: Integer;
  S: UTF8String;
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

// TODO: check UTF8 decoding
//    SetString(st, p1, L);
    SetString(S, p1, L);
    st := UTF8ToString(S);
  end
  else
    Result := False;
end;

{ TTextFileWriter }

procedure TTextFileWriter.Write(const st: string);
var
  EncodedString: UTF8String;
begin
// TODO: check UTF8 decoding
  EncodedString := UTF8Encode(st);
  inherited Write(EncodedString[1], Length(EncodedString));
end;

procedure TTextFileWriter.WriteLn(const st: string);
begin
  Write(st + #13#10);
end;

{ TStreamWideTextReader }

constructor TStreamWideTextReader.Create(AStream: TStream;
  blockSize: Integer);
begin
  fStream := AStream;
  fBlockSize := blockSize;
  GetMem (fBuffer, (fBlockSize + 1) * sizeof (WideChar));
  fBuffer [blockSize] := #0;
  Position := 0;
end;

destructor TStreamWideTextReader.Destroy;
begin
  FreeMem (fBuffer);
  inherited;
end;

function TStreamWideTextReader.GetChar: WideChar;
begin
  GetChunk;
  if fBufPos < fBufSize then
  begin
    result := fBuffer [fBufPos];
    Inc (fBufPos)
  end
  else
    result := #0
end;

procedure TStreamWideTextReader.GetChunk;
var
  ps : Integer;
begin
  if fBufPos = fBufSize then
  begin
    ps := (fStream.Size - fStream.Position) div sizeof (WideChar);
    fBufPos := 0;
    fBufSize := fBlockSize;
    if fBufSize > ps then
      fBufSize := ps;

    fBufSize := fStream.Read(fBuffer^, fBufSize * sizeof (WideChar)) div sizeof (WideChar);
    if fBufSize < fBlockSize then
      fBuffer [fBufSize] := #10
  end;
end;

function TStreamWideTextReader.GetPosition: Integer;
begin
  result := ((fStream.Position - 2) div sizeof (WideChar)) - fBufSize + fBufPos;
end;

function TStreamWideTextReader.ReadLn(var st: WideString;
  continuationChar: WideChar): boolean;
var
  l, lineStartPos : Integer;
  pch, pch1 : PWideChar;
  st1 : WideString;
  cont, scont : boolean;
begin
  lineStartPos := Position;
  cont := True;
  scont := false;
  st := '';
  while cont do
  begin
    GetChunk;
    if fBufPos = fBufSize then
      break;

    pch := fBuffer;
    Inc (pch, fBufPos);
    pch1 := WideStrScan (pch, #10);

    if pch1 <> nil then
    begin
      l := (Integer (pch1) - Integer (pch)) div sizeof (WideChar);
      Inc (fBufPos, l + 1);
      if fBufPos > fBufSize then
        fBufPos := fBufSize;
      if l > 0 then
      begin
        repeat
          Dec (pch1);
          if pch1^ = #13 then
            Dec (l)
          else
            break
        until pch1 = pch;
        cont := pch1^ = continuationChar
      end
      else
        cont := scont;
      SetLength (st1, l);
      if l > 0 then
        Move (pch^, PWideChar (st1)^, l * sizeof (WideChar));
    end
    else
    begin
      st1 := pch;
      l := Length (st1);

      while (l > 0) and (st1 [l] = #13) do
        Dec (l);

      if l < Length (st1) then
        SetLength (st1, l);

      scont := (l > 0) and (st1 [l] = continuationChar);

      fBufPos := fBufSize
    end;

    st := st + st1
  end;

  if cont then
  begin
    Position := lineStartPos;
    result := False
  end
  else
    result := True;
end;

procedure TStreamWideTextReader.SetPosition(const Value: Integer);
begin
  if Value <> Position then
  begin
    fBufSize := 0;
    fBufPos := 0;
    fStream.Position := Value * sizeof (WideChar) + 2;
  end
end;

procedure TStreamWideTextReader.SetStream(const Value: TStream);
begin
  fStream := Value;
  fBufPos := 0;
  fBufSize := 0;
  Position := 0;
end;

{ TBufferedFileWriter }

constructor TBufferedFileWriter.Create(const AFileName: string);
begin
  fStream := TFileStream.Create(AFileName, fmCreate);
  inherited Create (fStream);
end;

destructor TBufferedFileWriter.Destroy;
begin
  inherited;
  FreeAndNil (fStream);
end;

{ TMappedFileStream }

function TMappedFileStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create ('TMappedFileStream is read only');
end;

constructor TMappedFileStream.Create(const AFileName: string);
begin
  fFileHandle := CreateFile (PChar (AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if fFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  fSize := GetFileSize (fFileHandle, nil);
  if fSize <> 0 then
  begin
    fMappingHandle := CreateFileMapping (fFileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if fMappingHandle = 0 then
      RaiseLastOSError;

    fMemory := MapViewOfFile (fMappingHandle, FILE_MAP_READ, 0, 0, 0);
    if fMemory = Nil then
      RaiseLastOSError;
  end
end;

function TMappedFileStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning : fPosition := Offset;
    soFromEnd : fPosition := fSize - Offset;
    soFromCurrent : Inc (fPosition, Offset)
  end;

  if fPosition > fSize then
    fPosition := fSize;

  result := fPosition
end;

function TMappedFileStream.Read(var Buffer; Count: Integer): Longint;
begin
  if Count <= (fSize - fPosition) then
    result := Count
  else
    result := fSize - fPosition;

  if result < 0 then
    result := 0;

  if result > 0 then
  begin
    Move (fMemory [fPosition], Buffer, result);
    Inc (fPosition, result)
  end
end;

destructor TMappedFileStream.Destroy;
begin
  if fMemory <> Nil then
    UnmapViewOfFile (fMemory);

  if fMappingHandle <> 0 then
    CloseHandle (fMappingHandle);

  if fFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle (fFileHandle);

  inherited
end;

{ TBufferedStreamWriter }

constructor TBufferedStreamWriter.Create(const AStream: TStream);
begin
  fBufSize := 65536;
  GetMem (fBuffer, fBufSize);
  fStream := AStream;
end;

destructor TBufferedStreamWriter.Destroy;
begin
  FlushBuffer;
  FreeMem (fBuffer);

  inherited;
end;

procedure TBufferedStreamWriter.FlushBuffer;
begin
  fStream.Write(fBuffer^, fBufPos);
  fBufPos := 0
end;

function TBufferedStreamWriter.GetPosition: Integer;
begin
  result := fStream.Position + fBufPos;
end;

procedure TBufferedStreamWriter.Write(const data; dataLen: Integer);
var
  stPos, chunkLen : Integer;
  p :PAnsiChar;
begin
  stPos := 0;
  while stPos < dataLen do
  begin
    chunkLen := fBufSize - fBufPos;
    if chunkLen = 0 then
    begin
      FlushBuffer;
      chunkLen := fBufSize - fBufPos
    end;

    if chunkLen > dataLen - stPos then
      chunkLen := dataLen - stPos;

    p := PAnsiChar (@data);
    Inc (p, stPos);
    Move (p^, (fBuffer + fBufPos)^, chunkLen);
    Inc (stPos, chunkLen);
    Inc (fBufPos, chunkLen)
  end
end;

{ TTextStreamWriter }

procedure TTextStreamWriter.Write(const st: string);
var
  EncodedString: UTF8String;
begin
// TODO: check UTF8 decoding
  EncodedString := UTF8Encode(st);
  inherited Write(EncodedString[1], Length(EncodedString));
end;

procedure TTextStreamWriter.WriteLn(const st: string);
begin
  Write(st + #13#10)
end;

end.
