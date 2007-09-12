(*======================================================================*
 | unitVirtualMemory                                                    |
 |                                                                      |
 | TVirtualMemoryStream class.  A memory stream that allows you to      |
 | reserve (but not commit) a big chunk of memory, then commit it       |
 | later as required - up to the reserved size.                         |
 |                                                                      |
 | This can be used when it's vital that the location of the memory     |
 | in a memory stream never changes.  eg. MIDI streams etc.             |
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
unit unitVirtualMemory;

interface

uses Windows, Classes, SysUtils;

type
TVirtualMemoryStream = class (TCustomMemoryStream)
private
  fReserved : Integer;
  fChunkSize : Integer;
protected
  procedure SetSize (NewSize : Integer); override;
public
  constructor Create (AReserved, AInitialSize : Integer);
  destructor Destroy; override;
  function Write(const Buffer; Count: Longint): Longint; override;
  property Reserved : Integer read fReserved;
  property ChunkSize : Integer read fChunkSize write fChunkSize;
end;

EVirtualMemory = class (Exception);

implementation

constructor TVirtualMemoryStream.Create (AReserved, AInitialSize : Integer);
begin
  fReserved := AReserved;
  fChunkSize := 1024;
  SetPointer (VirtualAlloc (Nil, AReserved, MEM_RESERVE, PAGE_READWRITE), AInitialSize);
  if AInitialSize > 0 then
    VirtualAlloc (Memory, AInitialSize, MEM_COMMIT, PAGE_READWRITE);
end;

destructor TVirtualMemoryStream.Destroy;
begin
  VirtualFree (Memory, 0, MEM_RELEASE);
  inherited;
end;

procedure TVirtualMemoryStream.SetSize (NewSize : Integer);
var
  oldSize : Integer;
  commitSize : Integer;
begin
  oldSize := Size;
  if NewSize <> oldSize then
    if NewSize <= Reserved then
    begin
      if NewSize > oldSize then          // Grow the buffer
      begin
        commitSize := NewSize - oldSize;
        if commitSize < ChunkSize then
          commitSize := ChunkSize;
        if commitSize + oldSize > Reserved then
           commitSize := Reserved - oldSize;
        NewSize := oldSize + commitSize;

        VirtualAlloc (PChar (memory) + oldSize, commitSize, MEM_COMMIT, PAGE_READWRITE)
      end
      else                           // Shrink the buffer (lop off the end)
        VirtualFree (PChar (Memory) + NewSize, oldSize - NewSize, MEM_DECOMMIT);
      SetPointer (Memory, NewSize);
    end
    else raise EVirtualMemory.Create ('Size exceeds capacity');
end;

function TVirtualMemoryStream.Write(const Buffer; Count: Longint): Longint;
var
  pos : Integer;
begin
  pos := Seek (0, soFromCurrent);
  if pos + count > Size then
    Size := pos + count;
  Move (buffer, PChar (Integer (memory) + pos)^, count);
  Seek (count, soFromCurrent);
  result := Count
end;

end.
