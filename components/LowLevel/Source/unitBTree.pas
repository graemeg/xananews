(*======================================================================*
 | unitBTree                                                            |
 |                                                                      |
 | Cached, variable length key B-Tree index unit.  Public classes are:  |
 |                                                                      |
 | TBTree               Associate an integer with a variable length     |
 |                      MessageString                                   |
 |                                                                      |
 | TBTreeIterator       Iterate through a TBTree                        |
 |                                                                      |
 | TDataTree            Associate a variable length MessageString       |
 |                      with an integer                                 |
 |                                                                      |
 | TDataTreeIterator    Iterate through a TDataTree                     |
 |                                                                      |
 | TIndexTree           Sparse array of integers (ie. associate an      |
 |                      integer with an integer)                        |
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
 | Copyright © Colin Wilson 2003  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      05/03/2003  CPWW  Original                                  |
 | 1.1      21/03/2003  CPWW  All working!                              |
 | 10.0     08/03/2006  CPWW  BDS 2006 release version                  |
 *======================================================================*)

unit unitBTree;

interface

uses
  Windows, Classes, SysUtils, ConTnrs, unitObjectCache, XnClasses;

type
  //----------------------------------------------------------------
  // Header for B-Tree page (in index file)
  TPageHeader = packed record
    Flags: LongInt;
    KeysOnPage: LongInt;
    PrevPage: LongInt;          // -1 = leaf
    PrevPageHeight: LongInt;
  end;

  //----------------------------------------------------------------
  // Header for B-Tree node (in index file)
  TNodeHeader = packed record
    NextPage: LongInt;
    NextPageHeight: LongInt;
    KeyLen: Word;
  end;
  PNodeHeader = ^TNodeHeader;

const
  flgDirty = 1;                 // Page flags

  fflgCaseSensitive = 1;        // File flags
  fflgDupAccept = 2;
  fflgDupError = 4;
  fflgDupReplace = 8;
  fflgDupFlags = 14;

  PAGE_SIZE = 4096;
  PAGE_DATA_SIZE = PAGE_SIZE - SizeOf(TPageHeader);

  MAX_KEY_LEN = (PAGE_DATA_SIZE div 2) - SizeOf(TNodeHeader);

  NO_CACHED_PAGES = 64;

type
  //----------------------------------------------------------------
  // Index file header
  TFileInfo = packed record
    id: array[0..7] of AnsiChar;
    Flags: LongInt;
    PageCount: LongInt;
    RootPage: LongInt;
    RecordCount: LongInt;
    FirstDeletedPage: LongInt;
                                  // Let the user use the otherwise unused
                                  // space in the file header.
    ExtraDataSize: Word;
    ExtraData: array[0..PAGE_SIZE - 5 * SizeOf(LongInt) - 8 - 1 - SizeOf(Word)] of Byte;
  end;

  //----------------------------------------------------------------
  // B-Tree page (in index file)
  TPageRec = packed record
    Header: TPageHeader;
    Data: array[0..PAGE_DATA_SIZE - 1] of Byte; // The node data
  end;

  //----------------------------------------------------------------
  // B-Tree node (in memory)
  TNode = record
    key: MessageString;
    NextPage: LongInt;
    NextPageHeight: LongInt;
  end;

  TRawBTree = class;

  //----------------------------------------------------------------
  // B-Tree page (in memory)
  TPage = class
  private
    fFlags: Integer;
    fPrevPage: Integer;
    fPrevPageHeight: Integer;
    fNodes: array of TNode;
    fNodeCount: Integer;

    fOwner: TRawBTree;
    fIdx: Integer;
    fTotalDataLen: Integer;
    function GetFlags(bits: Integer): Boolean;
    procedure SetFlags(bits: Integer; const Value: Boolean);

    function FindNode(const st: MessageString; var idx: Integer): Boolean;
    procedure InsertNode(idx: Integer; const node: TNode);
    function GetNode(idx: Integer): TNode;
    function GetHeight: Integer;
  public
    constructor Create(AOwner: TRawBTree; AIdx: Integer);
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    property Flags[bits: Integer]: Boolean read GetFlags write SetFlags;
    property Height: Integer read GetHeight;
    property Idx: Integer read fIDX;
    property NodeCount: Integer read fNodeCount;
    property Node[idx: Integer]: TNode read GetNode;
    property Owner: TRawBTree read fOwner;
    property PrevPage: Integer read fPrevPage;
    property PrevPageHeight: Integer read fPrevPageHeight;
  end;

  //----------------------------------------------------------------
  // Page cache stores TPage objects.  CanRemove is overridden to save
  // a page on disk befor it's removed
  TPageCache = class(TObjectCache)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  TRawBTreeForEachProc = procedure(const key: MessageString; param: Integer; var continue: Boolean) of object;

  TBTreeDuplicates = (dupIgnore, dupAccept, dupError, dupReplace);

  //----------------------------------------------------------------
  // TRawBTree object
  TRawBTree = class
  private
    fFileName: string;
    f: TFileStream;
    fFileInfo: TFileInfo;
    fPageCache: TPageCache;
    fUpdateCount: Integer;
    fDelIDx, fDelPIdx: Integer;  // Used during delete.
    fOK: Boolean;

    procedure Open;
    procedure Close;
    procedure Flush(clearPageCache: Boolean);
    procedure SaveFileInfo;

    function GetPage(pageNo: Integer): TPage;
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure CacheCheckProc(obj: TObject; idx, param: Integer; var continue: Boolean);
    procedure CacheSaveProc(obj: TObject; idx, param: Integer; var continue: Boolean);
    function CreateNewPage: TPage;
    procedure DeleteOldPage(page: TPage);

    function PutKeyOnPage(pg: TPage; idx: Integer; const memNode: TNode): TNode;
    function PutKeyInTree(pg: TPage; const key: MessageString): TNode;
    function DeleteKeyFromTree(pg: TPage; const key: MessageString): Integer;
    function DeleteKeyFromPage(page: TPage; idx: Integer): Integer;
    function GetRecordCount: Integer;
    function GetRootPage: TPage;
    procedure SetDuplicates(const Value: TBTreeDuplicates);
    function GetDuplicates: TBTreeDuplicates;
    function GetExtraData: MessageString;
    procedure ResetNodeHeight(pg: TPage; idx: Integer);
    procedure SetExtraData(Value: MessageString);
    function GetIndexOfKey(var key: MessageString): Integer;
  protected
    function GetKey(idx: Integer): MessageString;
    function CompareKeys(const k1, k2: MessageString): Integer; virtual;

    function AddKey(const key: MessageString): Boolean;
    function DeleteKey(const key: MessageString): Boolean;
    procedure ForEach(proc: TRawBTreeForEachProc; param: Integer);
    function Find(key: MessageString; var fKey: MessageString): Boolean;

    property Key[idx: Integer]: MessageString read GetKey;
  public
    constructor Create(const AFileName: string); virtual;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;


    property ExtraData: MessageString read GetExtraData write SetExtraData;

    property RecordCount: Integer read GetRecordCount;

    // ------ May not always be public, but handy for diagnostics
    property RootPage: TPage read GetRootPage;
    property Page[pageNo: Integer]: TPage read GetPage;

    property FileName: string read fFileName;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property Duplicates: TBTreeDuplicates read GetDuplicates write SetDuplicates;
  end;

  //----------------------------------------------------------------
  // TIteratorNode.  An element on the iterator's stack
  TIteratorNode = class
  private
    fPageNo: Integer;
    fKeyIdx: Integer;
  public
    constructor Create(APageNo: Integer; AKeyIdx: Integer);

    property PageNo: Integer read fPageNo;
    property KeyIdx: Integer read fKeyIdx;
  end;

  //----------------------------------------------------------------
  // TRawBTreeIterator.  Class for iterating through the BTree
  TRawBTreeIterator = class
  private
    fBTree: TRawBTree;
    fStack: TObjectStack;
    procedure ClearPageStack;
    function IntToBin(i: Integer): MessageString;
  public
    constructor Create(ABTree: TRawBTree);
    destructor Destroy; override;

    function First(var key: MessageString): Boolean;
    function Last(var key: MessageString): Boolean;
    function Next(var key: MessageString): Boolean;
    function Prev(var key: MessageString): Boolean;
    function Find(var key: MessageString): Boolean;

    property BTree: TRawBTree read fBTree;
  end;

  TBTreeForEachProc = procedure(const key: MessageString; dataRec: Integer; var continue: Boolean) of object;

  //----------------------------------------------------------------
  // TBTree class.  Associate an integer with a MessageString
  //
  // eg.   The integer could contain an offset into a file of
  //       'user details' records, and the MessageString could contain the
  //       user id.
  //
  //       In the XanaNewz server, this is used both to associate a
  //       message-id with the offset into the messages data file, and to
  //       associate a message-id with a group's article no.

  TBTree = class(TRawBTree)
  private
    function IntToBin(i: Integer): MessageString;
    function ExtractDataRec(const key: MessageString): Integer;
    function InternalGetKey(idx: Integer): MessageString;
    function GetDataRec(const key: MessageString): Integer;
    procedure SetDataRec(const key: MessageString; const Value: Integer);
  protected
    function CompareKeys(const k1, k2: MessageString): Integer; override;
  public
    function AddKey(const key: MessageString; DataRec: Integer): Boolean;
    function DeleteKey(const key: MessageString): Boolean;
    procedure ForEach(proc: TBTreeForEachProc);
    function Find(key: MessageString; var dataRec: Integer): Boolean;

    function GetKey(idx: Integer; var dataRec: Integer): MessageString;
    function GetIndexOfKey(var key: MessageString; var dataRec: Integer): Integer;

    property Key[idx: Integer]: MessageString read InternalGetKey;
    property DataRec[const key: MessageString]: Integer read GetDataRec write SetDataRec;
  end;

  TDataTreeForEachProc = procedure(n: Integer; const st: MessageString; var continue: Boolean) of object;

  //----------------------------------------------------------------
  // TDataTree class.  Associate a MessageString with an integer.
  //
  // This effectively gives a sparse array of MessageStrings.
  //
  // eg.   In the XanaNewz server, this is used to associate a group's
  //       article no with a message id.
  TDataTree = class(TRawBTree)
  private
    function IntToBin(i: Integer): MessageString;
    function BinToInt(const st: MessageString): Integer;
    function GetValue(n: Integer): MessageString;
  protected
    function CompareKeys(const k1, k2: MessageString): Integer; override;
  public
    function AddKey(n: Integer; const st: MessageString): Boolean;
    function DeleteKey(n: Integer): Boolean;
    procedure ForEach(proc: TDataTreeForEachProc);
    function Find(n: Integer; var st: MessageString): Boolean;
    function GetKey(idx: Integer; var dataRec: Integer): MessageString;
    property Value[n: Integer]: MessageString read GetValue; default;
  end;

  TBTreeIterator = class(TRawBTreeIterator)
  private
    procedure SplitKey(var key: MessageString; var dataRec: Integer);
  public
    constructor Create(ABTree: TBTree);
    function First(var key: MessageString; var dataRec: Integer): Boolean;
    function Last(var key: MessageString; var dataRec: Integer): Boolean;
    function Next(var key: MessageString; var dataRec: Integer): Boolean;
    function Prev(var key: MessageString; var dataRec: Integer): Boolean;
    function Find(var key: MessageString; var dataRec: Integer): Boolean;
  end;

  TDataTreeIterator = class(TRawBTreeIterator)
  private
    procedure SplitKey(var n: Integer; var key: MessageString);
  public
    constructor Create(ADataTree: TDataTree);
    function First(var n: Integer; var st: MessageString): Boolean;
    function Last(var n: Integer; var st: MessageString): Boolean;
    function Next(var n: Integer; var st: MessageString): Boolean;
    function Prev(var n: Integer; var st: MessageString): Boolean;
    function Find(n: Integer; var st: MessageString): Boolean;
  end;

  TIndexTreeForEachProc = procedure(i: Integer; var continue: Boolean) of object;
  //----------------------------------------------------------------
  // TIndexTree class.  Associate an integer with an integer.
  //
  // This effectively gives a sparse array of integers.
  TIndexTree = class(TRawBTree)
  private
    fBinBuffer: MessageString;
    procedure IntToBinBuffer(i: Integer);
    function BinToInt(const st: MessageString): Integer;
    function GetValue(n: Integer): Integer;
    function GetIndexOf(i: Integer): Integer;
  protected
    function CompareKeys(const k1, k2: MessageString): Integer; override;
  public
    constructor Create(const AFileName: string); override;
    function AddKey(i: Integer): Boolean;
    function DeleteKey(i: Integer): Boolean;
    procedure ForEach(proc: TIndexTreeForEachProc);
    function Find(i: Integer): Boolean;

    function Delete(n: Integer): Boolean;
    property Value[n: Integer]: Integer read GetValue; default;
    property IndexOf[i: Integer]: Integer read GetIndexOf;
  end;

  EBTree = class(Exception);

implementation

resourcestring
  rstMustBeEmpty  = 'The BTree must be empty to perform this operation';
  rstKeyTooLong   = 'Key Too Long';
  rstDuplicateKey = 'Duplicate key %s';
  rstIndexExceedsBounds = 'Index exceeds bounds';

// CompareStr() from Pierre le Riche as found at the FastCode project.
function CompareStr(const S1, S2: MessageString): Integer;
asm
  cmp eax, edx
  je @SameString
  {Is either of the strings perhaps nil?}
  test eax, edx
  jz @PossibleNilString
  {Compare the first four characters (there has to be a trailing #0). In random
   string compares this can save a lot of CPU time.}
@BothNonNil:
  {Compare the first character}
  movzx ecx, byte ptr [edx]
  cmp cl, [eax]
  je @FirstCharacterSame
  {First character differs}
  movzx eax, byte ptr [eax]
  sub eax, ecx
  ret
@FirstCharacterSame:
  {Save ebx}
  push ebx
  {Set ebx = length(S1)}
  mov ebx, [eax - 4]
  xor ecx, ecx
  {Set ebx = length(S1) - length(S2)}
  sub ebx, [edx - 4]
  {Save the length difference on the stack}
  push ebx
  {Set ecx = 0 if length(S1) < length(S2), $ffffffff otherwise}
  adc ecx, -1
  {Set ecx = - min(length(S1), length(S2))}
  and ecx, ebx
  sub ecx, [eax - 4]
  {Adjust the pointers to be negative based}
  sub eax, ecx
  sub edx, ecx
@CompareLoop:
  mov ebx, [eax + ecx]
  xor ebx, [edx + ecx]
  jnz @Mismatch
  add ecx, 4
  js @CompareLoop
  {All characters match - return the difference in length}
@MatchUpToLength:
  pop eax
  pop ebx
  ret
@Mismatch:
  bsf ebx, ebx
  shr ebx, 3
  add ecx, ebx
  jns @MatchUpToLength
  movzx eax, byte ptr [eax + ecx]
  movzx edx, byte ptr [edx + ecx]
  sub eax, edx
  pop ebx
  pop ebx
  ret
  {It is the same string}
@SameString:
  xor eax, eax
  ret
  {Good possibility that at least one of the strings are nil}
@PossibleNilString:
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothNonNil
  {Return first string length: second string is nil}
  mov eax, [eax - 4]
  ret
@FirstStringNil:
  {Return 0 - length(S2): first string is nil}
  sub eax, [edx - 4]
end;


// CompareText from Aleksandr Sharahov as found at the FastCode project.
function CompareText(const S1, S2: MessageString): Integer;
asm
        test  eax, eax       // if S1 = nil then return(False)
        jz    @nil1
        test  edx, edx       // if S2 = nil then return(False)
        jz    @nil2
        mov   ecx, [eax-4]
        cmp   ecx, [edx-4]
        je    @lenok         // if Length(S1) <> Length(S2) then return(False)

@nil2:  xor   eax, eax
@nil1:  ret

@lenok: push  edi
        push  ebx
        xor   ebx, ebx
        mov   edi, eax
        sub   ebx, ecx

        sub   edi, ebx
        sub   edx, ebx

@loop:  mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]
        cmp   eax, ecx
        jne   @byte0
@same:  add   ebx, 4
        jl    @loop

@len:   mov   al, 1
        pop   ebx
        pop   edi
        ret

@loop2: mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]
        cmp   eax, ecx
        je    @same

@byte0: cmp   al, cl
        je    @byte1

        and   eax, $FF
        and   ecx, $FF
        sub   eax, 'a'
        sub   ecx, 'a'
        cmp   al, 'z'-'a'
        ja    @up0a
        sub   eax, 'a'-'A'
@up0a:  cmp   cl, 'z'-'a'
        ja    @up0c
        sub   ecx, 'a'-'A'
@up0c:  sub   eax, ecx
        jnz   @done

        mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]

@byte1: cmp   ah, ch
        je    @byte2

        and   eax, $FF00
        and   ecx, $FF00
        sub   eax, 'a'*256
        sub   ecx, 'a'*256
        cmp   ah, 'z'-'a'
        ja    @up1a
        sub   eax, ('a'-'A')*256
@up1a:  cmp   ch, 'z'-'a'
        ja    @up1c
        sub   ecx, ('a'-'A')*256
@up1c:  sub   eax, ecx
        jnz   @done

        mov   eax, [ebx+edi]
        mov   ecx, [ebx+edx]

@byte2: add   ebx, 2
        jnl   @len2
        shr   eax, 16
        shr   ecx, 16
        cmp   al, cl
        je    @byte3

        and   eax, $FF
        and   ecx, $FF
        sub   eax, 'a'
        sub   ecx, 'a'
        cmp   al, 'z'-'a'
        ja    @up2a
        sub   eax, 'a'-'A'
@up2a:  cmp   cl, 'z'-'a'
        ja    @up2c
        sub   ecx, 'a'-'A'
@up2c:  sub   eax, ecx
        jnz   @done

        movzx eax, word ptr [ebx+edi]
        movzx ecx, word ptr [ebx+edx]

@byte3: cmp   ah, ch
        je    @byte4

        and   eax, $FF00
        and   ecx, $FF00
        sub   eax, 'a'*256
        sub   ecx, 'a'*256
        cmp   ah, 'z'-'a'
        ja    @up3a
        sub   eax, ('a'-'A')*256
@up3a:  cmp   ch, 'z'-'a'
        ja    @up3c
        sub   ecx, ('a'-'A')*256
@up3c:  sub   eax, ecx
        jnz   @done

@byte4: add   ebx, 2
        jl    @loop2

@len2:  mov   al, 1
        pop   ebx
        pop   edi
        ret

@done:  pop   ebx
        pop   edi
end;

{ TRawBTree }

(*----------------------------------------------------------------------*
 | function TRawBTree.AddKey                                            |
 |                                                                      |
 | Add a key to the index                                               |
 |                                                                      |
 | Parameters:                                                          |
 |   key: MessageString;               The key to add                   |
 *----------------------------------------------------------------------*)
function TRawBTree.AddKey(const key: MessageString): Boolean;
var
  passout: TNode;
  newPage0: TPage;
begin
  if Length(key) > MAX_KEY_LEN then
    raise EBTree.Create(rstKeyTooLong);

  BeginUpdate;
  try
    fOK := True;
    passout := PutKeyInTree(RootPage, key);

    if passout.NextPage <> -1 then
    begin
      newPage0 := CreateNewPage;
      newPage0.fPrevPage := fFileInfo.RootPage;
      fFileInfo.RootPage := newPage0.fIdx;
      PutKeyOnPage(newPage0, 0, passout);

      ResetNodeHeight(newPage0, -1);
      ResetNodeHeight(newPage0, 0);
    end;
    if fOK then
      Inc(fFileInfo.RecordCount);
  finally
    EndUpdate;
  end;
  Result := fOK;
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.BeginUpdate                                      |
 |                                                                      |
 | Start updating.  Must be matched with EndUpdate                      |
 *----------------------------------------------------------------------*)
procedure TRawBTree.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.CacheCheckProc                                   |
 |                                                                      |
 | Callback for cache ForEach function - to find whether a page is      |
 | already in the cache.                                                |
 *----------------------------------------------------------------------*)
procedure TRawBTree.CacheCheckProc(obj: TObject; idx, param: Integer;
  var continue: Boolean);
begin
  continue := TPage(obj).fIdx <> param;
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.CacheSaveProc                                    |
 |                                                                      |
 | Callback for cache ForEach function - called to save each (dirty)    |
 | page when flushing the cache                                         |
 *----------------------------------------------------------------------*)
procedure TRawBTree.CacheSaveProc(obj: TObject; idx, param: Integer;
  var continue: Boolean);
begin
  TPage(obj).Save;
  continue := True;
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.Close                                            |
 |                                                                      |
 | Close the BTree                                                      |
 *----------------------------------------------------------------------*)
procedure TRawBTree.Close;
begin
  Flush(True);         // Flush and clear cached pages
  FreeAndNil(f);       // Close the index file
end;

function TRawBTree.CompareKeys(const k1, k2: MessageString): Integer;
begin
  if CaseSensitive then
    Result := CompareStr(k1, k2)
  else
    Result := CompareText(k1, k2);
end;

(*----------------------------------------------------------------------*
 | constructor TRawBTree.Create                                         |
 |                                                                      |
 | constructor for TRawBTree                                            |
 |                                                                      |
 | Parameters:                                                          |
 |   const AFileName: MessageString    The index file name                     |
 *----------------------------------------------------------------------*)
constructor TRawBTree.Create(const AFileName: string);
begin
  fFileName := AFileName;
  fPageCache := TPageCache.Create(NO_CACHED_PAGES, True);
  Open
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.CreateNewPage                                     |
 |                                                                      |
 | Create a new page and cache it.                                      |
 |                                                                      |
 | The function returns the new TPage                                   |
 *----------------------------------------------------------------------*)
function TRawBTree.CreateNewPage: TPage;
begin
  Result := TPage.Create(self, fFileInfo.PageCount);
  if fFileInfo.FirstDeletedPage <> -1 then
  begin
    Result.fIdx := fFileInfo.FirstDeletedPage;
    Result.Load;
    fFileInfo.FirstDeletedPage := Result.PrevPage;
    Result.fPrevPage := -1;
  end;
  fPageCache.Add(Result);
  Result.Flags[flgDirty] := True;
  Inc(fFileInfo.PageCount);
end;

function TRawBTree.DeleteKey(const key: MessageString): Boolean;
var
  page0: TPage;
begin
  fDelPIdx := -1;
  BeginUpdate;
  try
    fOK := True;
    DeleteKeyFromTree(RootPage, key);
    if fOK then
    begin
      page0 := RootPage;
      if page0.NodeCount = 0 then
      begin
        if page0.PrevPage <> -1 then
        begin
          fFileInfo.RootPage := page0.PrevPage;
          ResetNodeHeight(RootPage, -1);
          DeleteOldPage(page0);
        end;
      end;

      if fOK then
        Dec(fFileInfo.RecordCount);
    end
  finally
    EndUpdate;
  end;

  Result := fOK;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.DeleteKeyFromPage                                 |
 |                                                                      |
 | Delete a key from a page.                                            |
 |                                                                      |
 | Parameters:                                                          |
 |   page: TPage;               The page to delete from                 |
 |   idx: Integer               The node index on the page              |
 |                                                                      |
 | The function returns the number of nodes remaining on the page       |
 *----------------------------------------------------------------------*)
function TRawBTree.DeleteKeyFromPage(page: TPage; idx: Integer): Integer;
var
  dl: Integer;
begin
  dl := SizeOf(TNodeHeader) + Length(page.fNodes[idx].key);
  Finalize(page.fNodes[idx]);
  FillChar(page.fNodes[idx], SizeOf(TNode), 0);

  if idx < page.fNodeCount - 1 then
  begin
    Move(page.fNodes[idx + 1], page.fNodes[idx], (page.fNodeCount - idx - 1) * SizeOf(TNode));
    FillChar(page.fNodes[page.fNodeCount - 1], SizeOf(TNode), 0);
  end;

  Dec(page.fTotalDataLen, dl);
  Dec(page.fNodeCount);
  page.Flags[flgDirty] := True;
  Result := page.fNodeCount;
end;

function TRawBTree.DeleteKeyFromTree(pg: TPage; const key: MessageString): Integer;
var
  idx, pidx, nidx, tidx, mp: Integer;
  tn: TNode;
  tp, br: TPage;
  found: Boolean;
begin
  pidx := pg.Idx;
  found := pg.FindNode(key, idx);
  if found then
  begin
    fDelPidx := pg.Idx;
    fDelIdx := idx;
    nidx := idx + 1;
  end
  else
    nidx := idx;

  if pg.PrevPage <> -1 then
  begin
    if nidx = 0 then
      tp := Page[pg.PrevPage]
    else
      tp := Page[pg.Node[nidx - 1].NextPage];

    tidx := tp.Idx;

    if DeleteKeyFromTree(tp, key) = 0 then
    begin       // The pg we deleted from is now empty.  Some adjustments must
                // be made...
      tp := Page[tidx];        // Make sure these still exist in the cache
      pg := Page[pidx];

      if tp.Idx = pg.PrevPage then
      begin
        idx := 0;
        br := Page[pg.Node[0].NextPage];
        mp := 0;
      end
      else
      begin
        idx := nidx - 1;
        if idx = 0 then
          br := Page[pg.PrevPage]
        else
          br := Page[pg.Node[idx - 1].NextPage];
        mp := br.NodeCount;
      end;

      // Here:
      // tp is the empty pg
      // br is the non-empty pg to borrow or merge with
      // pg.fNodes[idx] is the node on this pg to rotate around
      // mp is the insertion pos on br.
      // if mp=0 then we're rotating left to the PrevPage - otherwise we're
      // rotating right to pg.fNodes[idx]'s Next pg

      if br.NodeCount = 1 then  // merge with neighbour
      begin
        if mp = 0 then          // PageLeft is empty.  Move current key into it's
        begin                   // right branch
          pg.fNodes[idx].NextPage := br.fPrevPage;
          br.fPrevPage := tp.PrevPage;
          pg.fPrevPage := br.Idx;
        end
        else                    // Current key's pg right is empty.  Move current
                                // key to it's left sibling's children
          pg.fNodes[idx].NextPage := tp.PrevPage;

        PutKeyOnPage(br, mp, pg.Node[idx]);
        ResetNodeHeight(br, mp);
        ResetNodeHeight(br, -1);
        DeleteKeyFromPage(pg, idx);
        DeleteOldPage(tp);
      end
      else                      // Borrow from neighbour
      begin
        PutKeyOnPage(tp, 0, pg.Node[idx]);
        if mp = 0 then
        begin
          tp.fNodes[0].NextPage := br.PrevPage;
          br.fPrevPage := br.Node[mp].NextPage;
          br.fPrevPageHeight := br.Node[mp].NextPageHeight;
        end
        else
        begin
          Dec(mp);
          tp.fNodes[0].NextPage := tp.PrevPage;
          tp.fPrevPage := br.Node[mp].NextPage;
        end;
        pg.fNodes[idx].key := br.Node[mp].key;
        DeleteKeyFromPage(br, mp);

        ResetNodeHeight(tp, -1);
        ResetNodeHeight(tp, 0)
      end;
      pg.Flags[flgDirty] := True;
      br.Flags[flgDirty] := True;

      if idx < pg.NodeCount then
        ResetNodeHeight(pg, idx);
      if idx > 0 then
        ResetNodeHeight(pg, idx - 1);
      ResetNodeHeight(pg, -1);
    end
    else
    begin
      pg := Page[pidx];
      ResetNodeHeight(pg, nidx - 1);
    end
  end
  else
  begin
    if found then
    begin
      if fDelPIdx <> -1 then
      begin
        if fDelPIdx <> pidx then
        begin       // Move node to delete to the leaves
          tp := Page[fDelPIdx];
          tn := pg.Node[idx];
          pg.fNodes[idx].key := tp.Node[fDelIdx].key;
          tp.fNodes[fDelIdx].key := tn.Key;
          tp.Flags[flgDirty] := True;
        end;

        DeleteKeyFromPage(pg, idx);
      end;
    end
    else
      fOK := False;
  end;

  Result := pg.NodeCount;
end;

procedure TRawBTree.DeleteOldPage(page: TPage);
begin
  page.fPrevPage := fFileInfo.FirstDeletedPage;
  fFileInfo.FirstDeletedPage := page.fIdx;
  page.Flags[flgDirty] := True;
  page.fNodeCount := 0;
  SetLength(page.fNodes, 0);
  fPageCache.Remove(page);
end;

(*----------------------------------------------------------------------*
 | destructor TRawBTree.Destroy                                         |
 |                                                                      |
 | Destructor for TRawBTree.                                            |
 *----------------------------------------------------------------------*)
destructor TRawBTree.Destroy;
begin
  Close;
  fPageCache.Free;
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.EndUpdate                                        |
 |                                                                      |
 | Pair to BeginUpdate.  If updated have finished, flush the cached     |
 | pages.                                                               |
 *----------------------------------------------------------------------*)
procedure TRawBTree.EndUpdate;
begin
  if fUpdateCount > 0 then
  begin
    Dec(fUpdateCount);
    if fUpdateCount = 0 then
      Flush(False);
  end;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.Find                                              |
 |                                                                      |
 | Find a key                                                           |
 |                                                                      |
 | Parameters:                                                          |
 |   key: MessageString;               The key to find                         |
 |   var DataRec: Integer       Returns the associated data rec         |
 |                                                                      |
 | The function returns True if the key was found.                      |
 *----------------------------------------------------------------------*)
function TRawBTree.Find(key: MessageString; var fKey: MessageString): Boolean;
var
  pg: TPage;
  idx: Integer;
begin
  pg := RootPage;
  while pg <> nil do
  begin
    if pg.FindNode(key, idx) then
    begin                               // Found it!
      fKey := pg.Node[idx].key;
      Break;
    end;

    if idx = 0 then
      idx := pg.PrevPage                      // Search pg left
    else
      idx := pg.node[idx-1].NextPage;        // Search node right

    if idx > -1 then
      pg := Page[idx]
    else
      pg := nil;
  end;

  Result := pg <> nil;
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.Flush                                            |
 |                                                                      |
 | Flush the cached pages, and optionally clear the cache.              |
 *----------------------------------------------------------------------*)
procedure TRawBTree.Flush(clearPageCache: Boolean);
begin
  if fUpdateCount > 0 then Exit;

  if clearPageCache then
    fPageCache.Clear
  else
    fPageCache.ForEach(CacheSaveProc, 0);

  SaveFileInfo;
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.ForEach                                          |
 |                                                                      |
 | Call 'proc' for each key in the tree in key order.                   |
 |                                                                      |
 | Parameters:                                                          |
 |   proc: TBTreeForEachProc            The procedure to call for each  |
 |                                      key.                            |
 *----------------------------------------------------------------------*)
procedure TRawBTree.ForEach(proc: TRawBTreeForEachProc; param: Integer);
var
  continue: Boolean;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach(pageNo: Integer);
  var
    i: Integer;
    node: TNode;
    pg: TPage;

  begin
    if not Continue then Exit;

    pg := Page[pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach(pg.PrevPage);
      pg := Page[pageNo];              // Original 'pg' may no have been
                                        // deleted from the cache.  So make sure
                                        // it's reloaded
    end;

    for i := 0 to pg.NodeCount - 1 do
      if continue then
      begin
                                        // Call the callback for the node
        node := pg.Node[i];
        Proc(node.key, param, continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach(node.NextPage);
          pg := Page[pageNo];
        end;
      end;
  end;

begin
  continue := True;
  DoForEach(fFileInfo.RootPage);
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetCaseSensitive                                  |
 |                                                                      |
 | 'Get' method for CaseSensitive property                              |
 *----------------------------------------------------------------------*)
function TRawBTree.GetCaseSensitive: Boolean;
begin
  Result := (fFileInfo.Flags and fflgCaseSensitive) <> 0;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetDuplicates                                     |
 |                                                                      |
 | 'Get' method for Duplicates property.                                |
 *----------------------------------------------------------------------*)
function TRawBTree.GetDuplicates: TBTreeDuplicates;
begin
  if (fFileInfo.Flags and fflgDupAccept) <> 0 then
    Result := dupAccept
  else
    if (fFileInfo.Flags and fflgDupError) <> 0 then
      Result := dupError
    else
      if (fFileInfo.Flags and fflgDupReplace) <> 0 then
        Result := dupReplace
      else
        Result := dupIgnore;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetExtraData                                      |
 |                                                                      |
 | 'Get' method for ExtraData property                                  |
 *----------------------------------------------------------------------*)
function TRawBTree.GetExtraData: MessageString;
begin
  SetLength(Result, fFileInfo.ExtraDataSize);
  Move(fFileInfo.ExtraData[0], Result[1], fFileInfo.ExtraDataSize);
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetPage                                           |
 |                                                                      |
 | 'Get' method for Page property.  Return Page[pageno]                 |
 *----------------------------------------------------------------------*)
function TRawBTree.GetIndexOfKey(var key: MessageString): Integer;
var
  pg: TPage;
  i, idx: Integer;
begin
  pg := RootPage;
  Result := 0;
  while pg <> nil do
  begin
    if pg.FindNode(key, idx) then
    begin                               // Found it!
      key := pg.Node[idx].key;
      Inc(Result, pg.PrevPageHeight);
      Inc(Result, idx);
      for i := 0 to idx - 2 do
        Inc(Result, pg.Node[i].NextPageHeight);
      Break;
    end;

    if idx = 0 then
      idx := pg.PrevPage                      // Search pg left
    else
    begin
      Inc(Result, pg.PrevPageHeight);

      Inc(Result, idx);
      for i := 0 to idx - 2 do
        Inc(Result, pg.Node[i].NextPageHeight);
      idx := pg.node[idx-1].NextPage;        // Search node right
    end;

    if idx > -1 then
      pg := Page[idx]
    else
      pg := nil;
  end;

  if pg = nil then
    Result := -1;
end;

function TRawBTree.GetKey(idx: Integer): MessageString;

  function gk(root: TPage; idx: Integer): MessageString;
  var
    i: Integer;
  begin
    if root.PrevPage = -1 then
      Result := root.Node[idx].key
    else
    begin
      if idx < root.PrevPageHeight then
        Result := gk(Page[root.PrevPage], idx)
      else
      begin
        Dec(idx, root.PrevPageHeight);

        i := 0;
        while (i < root.NodeCount) and (idx > 0) do
        begin
          if idx <= root.Node[i].NextPageHeight then
          begin
            Result := gk(Page[root.Node[i].NextPage], idx - 1);
            Break;
          end;

          Dec(idx, 1 + root.Node[i].NextPageHeight);
          Inc(i);
        end;

        if idx = 0 then
          Result := root.Node[i].key;
      end;
    end;
  end;
begin
  if idx >= RecordCount then
    raise EBTree.Create(rstIndexExceedsBounds);
  Result := gk(RootPage, idx);
end;

function TRawBTree.GetPage(pageNo: Integer): TPage;
var
  idx: Integer;
begin
  idx := fPageCache.ForEachIdx(CacheCheckProc, pageNo);

  if idx >= 0 then
  begin
    Result := TPage(fPageCache[idx]);
    fPageCache.BringToFront(idx);
  end
  else
  begin
    Result := TPage.Create(self, pageNo);
    Result.Load;
    fPageCache.Add(Result);
  end;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetRecordCount                                    |
 |                                                                      |
 | 'Get' method for RecordCount property                                |
 *----------------------------------------------------------------------*)
function TRawBTree.GetRecordCount: Integer;
begin
  Result := fFileInfo.RecordCount;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetRootPage                                       |
 |                                                                      |
 | 'Get' method for RootPage property                                   |
 *----------------------------------------------------------------------*)
function TRawBTree.GetRootPage: TPage;
begin
  Result := Page[fFileInfo.RootPage];
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.Open                                             |
 |                                                                      |
 | Open the BTree                                                       |
 *----------------------------------------------------------------------*)
procedure TRawBTree.Open;
var
  page0: TPage;
  id: MessageString;
begin
  if not FileExists(FileName) then
  begin                                 // New file
    f := TFileStream.Create(FileName, fmCreate);
    try
                                        // Create file info page and empty root page
      id := 'BTWoozle';
      Move(id[1], fFileInfo.id[0], 8);
      page0 := TPage.Create(self, 0);
      try
        page0.Flags[flgDirty] := True;
        page0.Save;
      finally
        page0.Free
      end;
      fFileInfo.PageCount := 1;
      fFileInfo.FirstDeletedPage := -1;
      fFileInfo.Flags := fflgCaseSensitive;
      SaveFileInfo;
    finally
      FreeAndNil(f);                    // Close newly created file
    end;
  end;

                                        // Open existing file
  f := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
                                        // Read file info page.
  f.Read(fFileInfo, SizeOf(TFileInfo));
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.PutKeyInTree                                      |
 |                                                                      |
 | Put key in the tree at page.  This may return a node which should be |
 | put in the previous page - or form the new root.  If the returned    |
 | node.NextPage is -1 then no node was returned.                       |
 |                                                                      |
 | Parameters:                                                          |
 |   page: TPage;                       Root page of tree               |
 |   const key: MessageString;          The key to add                  |
 |   DataRec: Integer                   DataRec associated with the key |
 |                                                                      |
 | The function returns a new root node if retval.NextPage <> -1        |
 *----------------------------------------------------------------------*)
function TRawBTree.PutKeyInTree(pg: TPage; const key: MessageString): TNode;
var
  pidx, idx: Integer;
begin
  pidx := pg.Idx;
  if pg.FindNode(key, idx) then
    case Duplicates of
      dupIgnore, dupReplace:
        begin
          fOK := False;

          if (Duplicates = dupReplace) and (key <> pg.fNodes[idx].key) then
          begin
            pg.fNodes[idx].key := key;
            pg.Flags[flgDirty] := True
          end;
          Result.NextPage := -1;
          Exit;
        end;
      dupAccept: ;
      dupError : raise EBTree.Create(Format(rstDuplicateKey, [key]));
    end;

  if pg.PrevPage = -1 then             // We're at the leaves.
  begin
    Result.key := key;
    Result.NextPage := -1;
    Result.NextPageHeight := 0;
    Result := PutKeyOnPage(pg, idx, Result);
  end
  else
  begin                                // Not at leaves - put key in pg left
    if idx > 0 then                    // or node right
    begin
      Result := PutKeyInTree(Page[pg.Node[idx - 1].NextPage], key);
      ResetNodeHeight(pg, idx - 1);
    end
    else
    begin
      Result := PutKeyInTree(Page[pg.PrevPage], key);
      ResetNodeHeight(pg, -1);
    end;

                                       // If a node was passed out by inserting to
                                       // the child, put it on this pg.
    if Result.NextPage <> -1 then
    begin
      pg := Page[pidx];                // Just possible that the original pg is
                                       // no longer in the cache.  Make sure it is.
      Result := PutKeyOnPage(pg, idx, Result);
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.PutKeyOnPage                                      |
 |                                                                      |
 | Put a node on the page.                                              |
 |                                                                      |
 | If the page is full, split the page.                                 |
 | The original page contains the lower nodes - up to the midpoint.     |
 | The function returns the midpoint node, pointing to a new page       |
 | containing the upper nodes.                                          |
 |                                                                      |
 | If the page is not full, the node is put on the page, and the        |
 | function returns a node with NextPage = -1 - indicating that no      |
 | split took place.                                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   page: TPage;               The page to put the node on.            |
 |   idx: Integer;              The position on the page to put the     |
 |                              node.                                   |
 |   const memNode: TNode       The node to put.                        |
 |                                                                      |
 | The function returns a TNode.  If retval.NextPage is -1 then         |
 | retval is meaningless - otherwise it contains a node pointing to a   |
 | page of higher nodes to be inserted in a lower page.                 |
 *----------------------------------------------------------------------*)
function TRawBTree.PutKeyOnPage(pg: TPage;  idx: Integer; const memNode: TNode): TNode;
var
  i, iidx, ts: Integer;
  newPage: TPage;
begin
  if pg.fTotalDataLen + SizeOf(TNodeHeader) + Length(memNode.key) > PAGE_DATA_SIZE then
  begin                 // Key doesn't fit on the pg.
                        // Split the pg into pg & newPage, and
                        // passout the midpoint key, pointing to the new pg.
    ts := 0;
    iidx := 0;          // Find mid point - iidx
    while (ts <= PAGE_DATA_SIZE div 2) and (iidx < pg.NodeCount) do
    begin
      Inc(ts, Length(pg.Node[iidx].key) + SizeOf(TNodeHeader));
      Inc(iidx);
    end;

    Dec(iidx);
                        // Passout mid point key
    if idx = iidx then
    begin
      Result := memNode;
      ts := iidx;
    end
    else
    begin
      if idx > iidx then
        ts := iidx + 1
      else
      begin
        ts := iidx;
        Dec(iidx);
      end;
      Result := pg.Node[iidx];
    end;

    newPage := CreateNewPage;
    newPage.fPrevPage := Result.NextPage;
    Result.NextPage := newPage.fIdx;

                        // Move keys above midpoint to new pg.

    newPage.fNodeCount := pg.NodeCount - ts;
    if Length(newPage.fNodes) < newPage.fNodeCount then
      SetLength(newPage.fNodes, newPage.fNodeCount);

    for i := ts to pg.NodeCount - 1 do
    begin
      newPage.fNodes[i - ts] := pg.Node[i];
//      newPage.InsertNode(i - ts, pg.Node[i]);
      Inc(newPage.fTotalDataLen, SizeOf(TNodeHeader) + Length(pg.Node[i].key));
    end;

                        // Truncate current pg

    pg.fNodeCount := iidx;

    pg.Flags[flgDirty] := True;
                        // Recalc TotalDataLen for truncated pg
    pg.fTotalDataLen := 0;
    for i := 0 to iidx - 1 do
      Inc(pg.fTotalDataLen, SizeOf(TNodeHeader) + Length(pg.Node[i].key));

    if ts <> iidx then
      if idx <= iidx then    // Put the new key on the old or new pg.
        PutKeyOnPage(pg, idx, memNode)
      else
        PutKeyOnPage(newPage, idx - pg.NodeCount - 1, memNode);

    ResetNodeHeight(newPage, -1);
    Result.NextPageHeight := newPage.Height;
  end
  else                       // pg not full - just add the node
  begin
    pg.InsertNode(idx, memNode);
    Inc(pg.fTotalDataLen, SizeOf(TNodeHeader) + Length(memNode.key));
    pg.Flags[flgDirty] := True;
    Result.NextPage := -1;   // Don't pass out anything.
  end
end;

procedure TRawBTree.ResetNodeHeight(pg: TPage; idx: Integer);
var
  node: TNode;
  pint: PInteger;
  val: Integer;
begin
  if idx = -1 then
  begin
    PInt := @pg.fPrevPageHeight;
    if pg.PrevPage = -1 then
      val := 0
    else
      val := Page[pg.PrevPage].Height;
  end
  else
  begin
    if idx >= pg.NodeCount then
      Exit;
    PInt := @pg.fNodes[idx].NextPageHeight;
    node := pg.Node[idx];
    if node.NextPage = -1 then
      val := 0
    else
      val := Page[node.NextPage].Height;
  end;

  if PInt^ <> val then
  begin
    PInt^ := val;
    pg.Flags[flgDirty] := True;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.SaveFileInfo                                     |
 |                                                                      |
 | Save the FileInfo record at the start of the index file.             |
 *----------------------------------------------------------------------*)
procedure TRawBTree.SaveFileInfo;
begin
  if fUpdateCount > 0 then Exit;
  if Assigned(f) then
  begin
    f.Seek(0, soFromBeginning);
    f.Write(fFileInfo, SizeOf(fFileInfo));
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.SetCaseSensitive                                 |
 |                                                                      |
 | 'Set' method for CaseSensitive property.  The btree must be empty    |
 | to call this.                                                        |
 *----------------------------------------------------------------------*)
procedure TRawBTree.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> CaseSensitive then
  begin
    if Assigned(f) then
      if RootPage.NodeCount > 0 then
        raise EBTree.Create(rstMustBeEmpty);

    if Value then
      fFileInfo.Flags := fFileInfo.Flags or fflgCaseSensitive
    else
      fFileInfo.Flags := fFileInfo.Flags and (not fflgCaseSensitive);

    if Assigned(f) then
      SaveFileInfo;
  end;
end;

(*----------------------------------------------------------------------*
 | TRawBTree.SetDuplicates                                              |
 |                                                                      |
 | 'Set' method for Duplicates property.  The b-tree must be empty to   |
 | call this.                                                           |
 *----------------------------------------------------------------------*)
procedure TRawBTree.SetDuplicates(const Value: TBTreeDuplicates);
begin
  if Value <> Duplicates then
  begin
    if Assigned(f) then
      if RootPage.NodeCount > 0 then
        raise EBTree.Create(rstMustBeEmpty);

    fFileInfo.Flags := fFileInfo.Flags and not fflgDupFlags;

    case Value of
      dupAccept: fFileInfo.Flags := fFileInfo.Flags or fflgDupAccept;
      dupError: fFileInfo.Flags := fFileInfo.Flags or fflgDupError;
      dupReplace: fFileInfo.Flags := fFileInfo.Flags or fflgDupReplace;
    end;

    if Assigned(f) then
      SaveFileInfo;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.SetExtraData                                     |
 |                                                                      |
 | 'Set' method for ExtraData property                                  |
 *----------------------------------------------------------------------*)
procedure TRawBTree.SetExtraData(Value: MessageString);
begin
  if Length(value) > SizeOf(fFileInfo.ExtraData) then
    SetLength(value, SizeOf(fFileInfo.ExtraData));

  fFileInfo.ExtraDataSize := Length(Value);
  if Length(Value) > 0 then
    Move(Value[1], fFileInfo.ExtraData[0], Length(value));

  SaveFileInfo;
end;

{ TPage }

(*----------------------------------------------------------------------*
 | constructor TPage.Create                                             |
 |                                                                      |
 | Constructor for TPage                                                |
 *----------------------------------------------------------------------*)
constructor TPage.Create(AOwner: TRawBTree; AIdx: Integer);
begin
  fOwner := AOwner;
  fPrevPage := -1;
  fIdx := AIdx;
  SetLength(fNodes, 384);
end;

(*----------------------------------------------------------------------*
 | destructor TPage.Destroy                                             |
 |                                                                      |
 | Destructor for TPage                                                 |
 *----------------------------------------------------------------------*)
destructor TPage.Destroy;
begin
  Save;

  Finalize(fNodes);             // Free memory used by the fNodes dynamic array
  inherited;
end;

(*----------------------------------------------------------------------*
 | function TPage.FindNode                                              |
 |                                                                      |
 | Find a node on the page that matches the key.  If no match is found  |
 | return False and set idx to the position where the key should be     |
 | inserted.                                                            |
 |                                                                      |
 | Parameters:                                                          |
 |   const st: MessageString;   The key to find                         |
 |  var idx: Integer            If the key was found, returns the       |
 |                              index.  If it wasn't found, returns the |
 |                              insertion position.                     |
 |                                                                      |
 | The function returns True if the key was found.                      |
 *----------------------------------------------------------------------*)
function TPage.FindNode(const st: MessageString; var idx: Integer): Boolean;

//------------------------------------------------------
// Binary search the page
  function bsearch(s, e: Integer): Boolean;
  var
    cmp: Integer;
  begin
    if e >= s then
    begin
      idx := s + (e - s) div 2;

      cmp := Owner.CompareKeys(st, Node[idx].key);

      if cmp < 0 then
        Result := bsearch(s, idx - 1)
      else
        if cmp > 0 then
          Result := bsearch(idx + 1, e)
        else
          Result := True;
    end
    else
      Result := False;
  end;

begin
  idx := 0;
  Result := bsearch(0, NodeCount - 1);

  if (not Result) and (idx < NodeCount) then
  // Adjust 'idx' so that it contains the correct insertion point
    if Owner.CompareKeys(Node[idx].key, st) < 0 then
      Inc(idx);
end;

(*----------------------------------------------------------------------*
 | function TPage.GetFlags                                              |
 |                                                                      |
 | 'Get' method for Flags property                                      |
 *----------------------------------------------------------------------*)
function TPage.GetFlags(bits: Integer): Boolean;
begin
  Result := (fFlags and bits) = bits;
end;

(*----------------------------------------------------------------------*
 | function TPage.GetNode                                               |
 |                                                                      |
 | 'Get' method for the 'Node' property                                 |
 *----------------------------------------------------------------------*)
function TPage.GetHeight: Integer;
var
  i: Integer;
begin
  Result := NodeCount + PrevPageHeight;
  for i := 0 to NodeCount - 1 do
    Inc(Result, Node[i].NextPageHeight);
end;

function TPage.GetNode(idx: Integer): TNode;
begin
  if idx = -1 then
    idx := NodeCount - 1;
  Result := fNodes[idx];
end;

(*----------------------------------------------------------------------*
 | procedure TPage.InsertNode                                           |
 |                                                                      |
 | Insert a node on the page.                                           |
 |                                                                      |
 | Parameters:                                                          |
 |   idx: Integer;              Where to insert the node                |
 |   const node: TNode          The node to insert.                     |
 *----------------------------------------------------------------------*)
procedure TPage.InsertNode(idx: Integer; const node: TNode);
begin
  if idx > NodeCount then               // Reality check
    raise EBTree.Create('Error in Insert Node');

                                        // Grow the dynamic array if there's
                                        // no room
  if NodeCount = Length(fNodes) then
    SetLength(fNodes, Length(fNodes) + 64);

  if idx < NodeCount then               // If we're inserting within the existing
                                        // nodes, create a space by moving nodes.
  begin
    Move(fNodes[idx], fNodes[idx + 1], (NodeCount - idx) * SizeOf(TNode));
                                        // Very important to clear the space -
                                        // otherwise stings will go mad.
    FillChar(fNodes[idx], SizeOf(TNode), 0);
  end;

  fNodes[idx] := node;
  Inc(fNodeCount);
end;

(*----------------------------------------------------------------------*
 | procedure TPage.Load                                                 |
 |                                                                      |
 | Load a page.                                                         |
 *----------------------------------------------------------------------*)
procedure TPage.Load;
var
  pr: TPageRec;
  i: Integer;
  p: PAnsiChar;
  pnh: PNodeHeader;
  st: MessageString;
begin
        // Read the page
  Owner.f.Seek(SizeOf(TFileInfo) + fIdx * SizeOf(TPageRec), soFromBeginning);
  Owner.f.Read(pr, SizeOf(pr));

        // Get headeer info
  self.fFlags := pr.Header.Flags;
  self.fPrevPage := pr.Header.PrevPage;
  self.fPrevPageHeight := pr.Header.PrevPageHeight;

  p := @pr.Data[0];
  if pr.Header.KeysOnPage > Length(fNodes) then
    SetLength(fNodes, pr.Header.KeysOnPage);

  fNodeCount := 0;
        // Decode the nodes.
  for i := 0 to Integer(pr.Header.KeysOnPage) - 1 do
  begin
    pnh := PNodeHeader(p);
    Inc(p, SizeOf(TNodeHeader));
    Inc(fTotalDataLen, SizeOf(TNodeHeader) + pnh^.KeyLen);

    SetString(st, p, pnh^.KeyLen);
    Inc(p, pnh^.KeyLen);

    fNodes[fNodeCount].key := st;
    fNodes[fNodeCount].NextPage := pnh^.NextPage;
    fNodes[fNodeCount].NextPageHeight := pnh^.NextPageHeight;

    Inc(fNodeCount);
  end;

  Flags[flgDirty] := False;
end;

(*----------------------------------------------------------------------*
 | procedure TPage.Save                                                 |
 |                                                                      |
 | Save a page                                                          |
 *----------------------------------------------------------------------*)
procedure TPage.Save;
var
  pr: TPageRec;
  i: Integer;
  p: PAnsiChar;
  pnh: PNodeHeader;
  st: MessageString;
  nd: TNode;
begin
  if not Flags[flgDirty] then Exit;    // No need to save

  pr.Header.Flags := fFlags;            // Set the header info.
  pr.Header.KeysOnPage := NodeCount;
  pr.Header.PrevPage := PrevPage;
  pr.Header.PrevPageHeight := PrevPageHeight;
  FillChar(pr.Data, SizeOf(pr.Data), 0);

  p := @pr.Data[0];
                                        // Encode the nodes
  for i := 0 to NodeCount - 1 do
  begin
    nd := Node[i];
    pnh := PNodeHeader(p);
    pnh^.NextPage := nd.NextPage;
    pnh^.NextPageHeight := nd.NextPageHeight;
    st := nd.key;
    pnh^.KeyLen := Length(st);

    Inc(p, SizeOf(TNodeHeader));
    Move(st[1], p^, pnh^.KeyLen);
    Inc(p, pnh^.KeyLen);
  end;
                                        // Write the data
  Owner.f.Seek(SizeOf(TFileInfo) + fIdx * SizeOf(TPageRec), soFromBeginning);
  Owner.f.Write(pr, SizeOf(pr));

  Flags[flgDirty] := False;
end;

(*----------------------------------------------------------------------*
 | procedure TPage.SetFlags                                             |
 |                                                                      |
 | 'Set' method for flags property                                      |
 *----------------------------------------------------------------------*)
procedure TPage.SetFlags(bits: Integer; const Value: Boolean);
begin
  if Value then
    fFlags := fFlags or bits
  else
    fFlags := fFlags and not bits;
end;

{ TPageCache }

procedure TPageCache.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if not Reordering and (TObject(Ptr) is TPage) then
    if Action = lnDeleted then
      TPage(Ptr).Save;
  inherited;
end;

{ TRawBTreeIterator }

(*----------------------------------------------------------------------*
 | procedure TRawBTreeIterator.ClearPageStack                           |
 |                                                                      |
 | Clear the iterator stack.                                            |
 *----------------------------------------------------------------------*)
procedure TRawBTreeIterator.ClearPageStack;
begin
  while fStack.Count > 0 do
    fStack.Pop.Free;
end;

(*----------------------------------------------------------------------*
 | constructor TRawBTreeIterator.Create                                 |
 |                                                                      |
 | Constructor for TRawBTreeIterator                                    |
 *----------------------------------------------------------------------*)
constructor TRawBTreeIterator.Create(ABTree: TRawBTree);
begin
  fBTree := ABTree;
  fStack := TObjectStack.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TRawBTreeIterator.Destroy                                 |
 |                                                                      |
 | Destructor for TRawBTreeIterator                                     |
 *----------------------------------------------------------------------*)
destructor TRawBTreeIterator.Destroy;
begin
  ClearPageStack;
  fStack.Free;

  inherited;
end;

function TRawBTreeIterator.Find(var key: MessageString): Boolean;
var
  pageNo, idx: Integer;
  pg: TPage;
  found: Boolean;
begin
  ClearPageStack;
  pageNo := BTree.fFileInfo.RootPage;
  repeat
    pg := BTree.Page[pageNo];
    found := pg.FindNode(key, idx);
    fStack.Push(TIteratorNode.Create(pageNo, idx));
    if not found then
      if idx = 0 then
        pageNo := pg.PrevPage
      else
        pageNo := pg.Node[idx - 1].NextPage;
  until found or (pageNo = -1);

  Result := found;
  if found then
    key := pg.Node[idx].key;
end;

(*----------------------------------------------------------------------*
 | function TRawBTreeIterator.First                                     |
 |                                                                      |
 | Iterate to the first node in the index and return it's data          |
 |                                                                      |
 | Parameters:                                                          |
 |   var key: MessageString;    Returns the key of the first node       |
 |   var value: Integer         Returns the DataRec associated with the |
 |                              key                                     |
 |                                                                      |
 | The function returns True if there was a first node.                 |
 *----------------------------------------------------------------------*)
function TRawBTreeIterator.First(var key: MessageString): Boolean;
var
  pageNo: Integer;
  pg: TPage;
begin
  ClearPageStack;
  pageNo := BTree.fFileInfo.RootPage;
  repeat
    pg := BTree.Page[pageNo];

    if (pg.PrevPage = -1) and (pg.NodeCount = 0) then
      Break;    // Empty tree containing empty root pg.

    fStack.Push(TIteratorNode.Create(pageNo, 0));
    pageNo := pg.PrevPage
  until pageNo = -1;

  if pg.NodeCount > 0 then
  begin
    key := pg.Node[0].key;
    Result := True
  end
  else
    Result := False;
end;

function TRawBTreeIterator.IntToBin(i: Integer): MessageString;
begin
  SetLength(Result, SizeOf(Integer));
  Move(i, Result[1], SizeOf(Integer));
end;

(*----------------------------------------------------------------------*
 | TRawBTreeIterator.Last
 |                                                                      |
 | Iterate to the last node in the index and return it's data           |
 |                                                                      |
 | Parameters:                                                          |
 |   var key: MessageString;           Returns the key of the last node        |
 |   var value: Integer         Returns the DataRec associated with the |
 |                              key                                     |
 |                                                                      |
 | The function returns True if there was a last node.                  |
 *----------------------------------------------------------------------*)
function TRawBTreeIterator.Last(var key: MessageString): Boolean;
var
  pageNo: Integer;
  pg: TPage;
begin
  ClearPageStack;
  pageNo := BTree.fFileInfo.RootPage;
  repeat
    pg := BTree.Page[pageNo];

    if (pg.PrevPage = -1) and (pg.NodeCount = 0) then
      Break;

    fStack.Push(TIteratorNode.Create(pageNo, pg.NodeCount - 1));
    pageNo := pg.Node[-1].NextPage;
  until pg.PrevPage = -1;

  if pg.NodeCount > 0 then
  begin
    key := pg.Node[-1].key;
    Result := True;
  end
  else
    Result := False;
end;

(*----------------------------------------------------------------------*
 | function TRawBTreeIterator.Next                                      |
 |                                                                      |
 | Iterate to the next node in the index and return it's data.          |
 |                                                                      |
 | Parameters:                                                          |
 |   var key: MessageString;           Returns the key of the next node        |
 |   var value: Integer         Returns the DataRec associated with the |
 |                              key                                     |
 |                                                                      |
 | The function returns True if there was a next node.                  |
 *----------------------------------------------------------------------*)
function TRawBTreeIterator.Next(var key: MessageString): Boolean;
var
  pg: TPage;
  node: TIteratorNode;
  tmp, tmp1: Integer;
begin
  pg := nil;
  node := nil;
  if fStack.Count > 0 then
  begin
    node := TIteratorNode(fStack.Pop);         // Pop previously returned node
    pg := BTree.Page[node.PageNo];
                                                // Are there children?
    if pg.Node[node.fKeyIdx].NextPage <> -1 then
    begin
      fStack.Push(node);
                                                // Goto children...
      pg := BTree.Page[pg.Node[node.fKeyIdx].NextPage];
      node := TIteratorNode.Create(pg.fIdx, 0);
                                                // ... then Left, Left, Left!
      while pg.PrevPage <> -1 do
      begin
        fStack.Push(node);
        pg := BTree.Page[pg.PrevPage];
        node := TIteratorNode.Create(pg.fIdx, 0);
      end;

      node.fPageNo := pg.Idx;
    end                                         // Can we go right ?
    else
      if node.fKeyIdx < pg.NodeCount - 1 then
        Inc(node.fKeyIdx)
      else
      repeat                                    // No children - and can't go right
        tmp := node.PageNo;
        FreeAndNil(node);
        if fStack.Count > 0 then
        begin                                   // Pop a lower node.
          node := TIteratorNode(fStack.Pop);
          pg := BTree.Page[node.PageNo];

          if (node.fKeyIdx = 0) then
            tmp1 := pg.PrevPage
          else
            tmp1 := pg.Node[node.fKeyIdx - 1].NextPage;

          if tmp <> tmp1 then                   // If we we came from the node's children...
            Inc(node.fKeyIdx);                 // .. go right, or cycle round to pop another
                                                // pg.
          if node.fKeyIdx < pg.NodeCount then
            Break;
        end
        else
          Break;
      until False;
  end;

  if Assigned(node) then                        // Did we find a next node?
  begin
    fStack.Push(node);                          // Push it, for next 'next' or 'prev'
    key := pg.Node[node.fKeyIdx].key;
    Result := True;
  end
  else
    Result := False;
end;

(*----------------------------------------------------------------------*
 | function TRawBTreeIterator.Next                                      |
 |                                                                      |
 | Iterate to the previous node in the index and return it's data.      |
 |                                                                      |
 | Parameters:                                                          |
 |   var key: MessageString;           Returns the key of the previous node    |
 |   var value: Integer         Returns the DataRec associated with the |
 |                              key                                     |
 |                                                                      |
 | The function returns True if there was a previous node.              |
 *----------------------------------------------------------------------*)
function TRawBTreeIterator.Prev(var key: MessageString): Boolean;
begin
  Result := False;              // Not yet implemented!
end;

{ TIteratorNode }

(*----------------------------------------------------------------------*
 | constructor TIteratorNode.Create                                     |
 |                                                                      |
 | Create an iterator (stack) node.                                     |
 *----------------------------------------------------------------------*)
constructor TIteratorNode.Create(APageNo, AKeyIdx: Integer);
begin
  fPageNo := APageNo;
  fKeyIdx := AKeyIdx;
end;

{ TBTree }

function TBTree.AddKey(const key: MessageString; DataRec: Integer): Boolean;
var
  S: MessageString;
begin
  S := IntToBin(DataRec);
  Result := inherited AddKey(key + S);
end;

function TBTree.CompareKeys(const k1, k2: MessageString): Integer;
begin
  if CaseSensitive then
    Result := CompareStr(Copy(k1, 1, Length(k1) - SizeOf(Integer)), Copy(k2, 1, Length(k2) - SizeOf(Integer)))
  else
    Result := CompareText(Copy(k1, 1, Length(k1) - SizeOf(Integer)), Copy(k2, 1, Length(k2) - SizeOf(Integer)));
end;

function TBTree.DeleteKey(const key: MessageString): Boolean;
begin
  Result := inherited DeleteKey(key + IntToBin(0));
end;

function TBTree.ExtractDataRec(const key: MessageString): Integer;
begin
  Move((PAnsiChar(key) + Length(key) - 4)^, Result, SizeOf(Result));
end;

function TBTree.Find(key: MessageString; var dataRec: Integer): Boolean;
var
  k: MessageString;
begin
  Result := inherited Find(key + IntToBin(0), k);
  if Result then
    dataRec := ExtractDataRec(k);
end;

procedure TBTree.ForEach(proc: TBTreeForEachProc);
var
  continue: Boolean;
  k: MessageString;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach(pageNo: Integer);
  var
    i: Integer;
    node: TNode;
    pg: TPage;

  begin
    if not Continue then Exit;

    pg := Page[pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach(pg.PrevPage);
      pg := Page[pageNo];               // Original 'pg' may no have been
                                        // deleted from the cache.  So make sure
                                        // it's reloaded
    end;

    for i := 0 to pg.NodeCount - 1 do
      if continue then
      begin
                                        // Call the callback for the node
        node := pg.Node[i];
        k := Copy(node.key, 1, Length(node.key) - 4);

        Proc(k, ExtractDataRec(node.key), continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach(node.NextPage);
          pg := Page[pageNo];
        end;
      end;
  end;

begin
  continue := True;
  DoForEach(fFileInfo.RootPage)
end;

function TBTree.GetDataRec(const key: MessageString): Integer;
begin
  if not Find(key, Result) then
    Result := -1;
end;

function TBTree.GetIndexOfKey(var key: MessageString;
  var dataRec: Integer): Integer;
var
  k: MessageString;
begin
  k := key + IntToBin(0);
  Result := inherited GetIndexOfKey(k);
  if Result > -1 then
  begin
    dataRec := ExtractDataRec(k);
    key := Copy(k, 1, Length(k) - SizeOf(Integer));
  end;
end;

function TBTree.GetKey(idx: Integer; var dataRec: Integer): MessageString;
begin
  Result := inherited GetKey(idx);
  if Result <> '' then
  begin
    dataRec := ExtractDataRec(Result);
    SetLength(Result, Length(Result) - SizeOf(Integer));
  end
  else
    dataRec := -1
end;

function TBTree.InternalGetKey(idx: Integer): MessageString;
var
  dr: Integer;
begin
  Result := GetKey(idx, dr)
end;

function TBTree.IntToBin(i: Integer): MessageString;
begin
  SetLength(Result, SizeOf(Integer));
  Move(i, Result[1], SizeOf(Integer));
end;

procedure TBTree.SetDataRec(const key: MessageString; const Value: Integer);
var
  pg: TPage;
  idx: Integer;
  k: MessageString;
begin
  pg := RootPage;
  k := key + IntToBin(0);

  BeginUpdate;
  try
    while Assigned(pg) do
    begin
      if pg.FindNode(k, idx) then
      begin
        pg.fNodes[idx].key := key + IntToBin(Value);
        pg.Flags[flgDirty] := True;
        Break
      end;

      if pg.PrevPage <> -1 then
      begin
        if idx = 0 then
          pg := Page[pg.PrevPage]
        else
          pg := Page[pg.Node[idx - 1].NextPage];
      end
      else
        pg := nil;
    end
  finally
    EndUpdate;
  end;
end;

{ TBTreeIterator }

constructor TBTreeIterator.Create(ABTree: TBTree);
begin
  inherited Create(ABTree);
end;

function TBTreeIterator.Find(var key: MessageString;
  var dataRec: Integer): Boolean;
var
  k: MessageString;
begin
  k := key + IntToBin(0);

  Result := inherited Find(k);
  if Result then
  begin
    key := k;
    SplitKey(key, dataRec);
  end;
end;

function TBTreeIterator.First(var key: MessageString; var dataRec: Integer): Boolean;
begin
  Result := inherited First(key);
  if Result then
    SplitKey(key, dataRec);
end;

function TBTreeIterator.Last(var key: MessageString;
  var dataRec: Integer): Boolean;
begin
  Result := inherited Last(key);
  if Result then
    SplitKey(key, dataRec);
end;

function TBTreeIterator.Next(var key: MessageString;
  var dataRec: Integer): Boolean;
begin
  Result := inherited Next(key);
  if Result then
    SplitKey(key, dataRec);
end;

function TBTreeIterator.Prev(var key: MessageString;
  var dataRec: Integer): Boolean;
begin
  Result := inherited Prev(key);
  if Result then
    SplitKey(key, dataRec);
end;

procedure TBTreeIterator.SplitKey(var key: MessageString; var dataRec: Integer);
begin
  dataRec := TBTree(BTree).ExtractDataRec(key);
  key := Copy(key, 1, Length(key) - SizeOf(Integer));
end;

{ TDataTree }

function TDataTree.AddKey(n: Integer; const st: MessageString): Boolean;
begin
  Result := inherited AddKey(IntToBin(n) + st);
end;

function TDataTree.BinToInt(const st: MessageString): Integer;
begin
  Move(st[1], Result, SizeOf(Integer));
end;

function TDataTree.CompareKeys(const k1, k2: MessageString): Integer;
begin
  Result := BinToInt(k1) - BinToInt(k2);
end;

function TDataTree.DeleteKey(n: Integer): Boolean;
begin
  Result := inherited DeleteKey(IntToBin(n));
end;

function TDataTree.Find(n: Integer; var st: MessageString): Boolean;
var
  k: MessageString;
begin
  Result := inherited Find(IntToBin(n), k);
  if Result then
    st := Copy(k, 5, MaxInt);
end;

procedure TDataTree.ForEach(proc: TDataTreeForEachProc);
var
  continue: Boolean;
  k: MessageString;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach(pageNo: Integer);
  var
    i: Integer;
    node: TNode;
    pg: TPage;

  begin
    if not Continue then Exit;

    pg := Page[pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach(pg.PrevPage);
      pg := Page[pageNo];               // Original 'pg' may no have been
                                        // deleted from the cache.  So make sure
                                        // it's reloaded
    end;

    for i := 0 to pg.NodeCount - 1 do
      if continue then
      begin
                                        // Call the callback for the node
        node := pg.Node[i];
        k := node.key;
        Proc(BinToInt(k), Copy(k, SizeOf(Integer) + 1, MaxInt), continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach(node.NextPage);
          pg := Page[pageNo];
        end;
      end;
  end;

begin
  continue := True;
  DoForEach(fFileInfo.RootPage);
end;

function TDataTree.GetKey(idx: Integer; var DataRec: Integer): MessageString;
begin
  Result := inherited GetKey(idx);
  DataRec := BinToInt(Result);
  Result := Copy(Result, SizeOf(Integer) + 1, MaxInt);;
end;

function TDataTree.GetValue(n: Integer): MessageString;
begin
  if not Find(n, Result) then
    raise EBTree.Create('Value not found');
end;

function TDataTree.IntToBin(i: Integer): MessageString;
begin
  SetLength(Result, SizeOf(Integer));
  Move(i, Result[1], SizeOf(Integer));
end;

{ TDataTreeIterator }

constructor TDataTreeIterator.Create(ADataTree: TDataTree);
begin
  inherited Create(ADataTree);
end;

function TDataTreeIterator.Find(n: Integer; var st: MessageString): Boolean;
var
  k: MessageString;
begin
  SetLength(k, SizeOf(Integer));
  Move(n, k[1], SizeOf(Integer));
  Result := inherited Find(k);
  if Result then
  begin
    st := k;
    SplitKey(n, st);
  end;
end;

function TDataTreeIterator.First(var n: Integer; var st: MessageString): Boolean;
begin
  Result := inherited First(st);
  if Result then
    SplitKey(n, st);
end;

function TDataTreeIterator.Last(var n: Integer; var st: MessageString): Boolean;
begin
  Result := inherited Last(st);
  if Result then
    SplitKey(n, st);
end;

function TDataTreeIterator.Next(var n: Integer; var st: MessageString): Boolean;
begin
  Result := inherited Next(st);
  if Result then
    SplitKey(n, st);
end;

function TDataTreeIterator.Prev(var n: Integer; var st: MessageString): Boolean;
begin
  Result := inherited Prev(st);
  if Result then
    SplitKey(n, st);
end;

procedure TDataTreeIterator.SplitKey(var n: Integer; var key: MessageString);
begin
  Move(key[1], n, SizeOf(Integer));
  key := Copy(key, SizeOf(Integer) + 1, MaxInt);
end;

{ TIndexTree }

function TIndexTree.AddKey(i: Integer): Boolean;
begin
  IntToBinBuffer(i);
  Result := inherited AddKey(fBinBuffer);
end;

function TIndexTree.BinToInt(const st: MessageString): Integer;
begin
  Move(st[1], Result, SizeOf(Integer));
end;

function TIndexTree.CompareKeys(const k1, k2: MessageString): Integer;
begin
  Result := BinToInt(k1) - BinToInt(k2);
end;

constructor TIndexTree.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  SetLength(fBinBuffer, 4); //  fBinBuffer := #0#0#0#0;
end;

function TIndexTree.Delete(n: Integer): Boolean;
var
  i: Integer;
begin
  i := BinToInt(GetKey(n));
  Result := DeleteKey(i);
end;

function TIndexTree.DeleteKey(i: Integer): Boolean;
begin
  IntToBinBuffer(i);
  Result := inherited DeleteKey(fBinBuffer);
end;

function TIndexTree.Find(i: Integer): Boolean;
var
  fKey: MessageString;
begin
  IntToBinBuffer(i);
  Result := inherited Find(fBinBuffer, fKey);
end;

procedure TIndexTree.ForEach(proc: TIndexTreeForEachProc);
var
  continue: Boolean;
  k: MessageString;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach(pageNo: Integer);
  var
    i: Integer;
    node: TNode;
    pg: TPage;

  begin
    if not Continue then Exit;

    pg := Page[pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach(pg.PrevPage);
      pg := Page[pageNo];               // Original 'pg' may no have been
                                        // deleted from the cache.  So make sure
                                        // it's reloaded
    end;

    for i := 0 to pg.NodeCount - 1 do
      if continue then
      begin
                                        // Call the callback for the node
        node := pg.Node[i];
        k := node.key;
        Proc(BinToInt(k), continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach(node.NextPage);
          pg := Page[pageNo];
        end;
      end;
  end;

begin
  continue := True;
  DoForEach(fFileInfo.RootPage);
end;

function TIndexTree.GetIndexOf(i: Integer): Integer;
begin
  IntToBinBuffer(i);
  Result := GetIndexOfKey(fBinBuffer);
end;

function TIndexTree.GetValue(n: Integer): Integer;
begin
  Result := BinToInt(GetKey(n));
end;

procedure TIndexTree.IntToBinBuffer(i: Integer);
begin
  Move(i, fBinBuffer[1], SizeOf(Integer));
end;

end.
