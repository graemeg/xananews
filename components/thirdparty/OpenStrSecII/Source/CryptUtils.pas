{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     CryptUtils Unit                                   }
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
{$WEAKPACKAGEUNIT ON}
{$I ver.inc}
unit CryptUtils;

interface

uses
  Classes, SecUtils, CryptGlobal;

type
  TDigestStream = class(TStream)
  private
    FHash: THash;
    FDataStream: TStream;
    procedure SetDataStream(const Value: TStream);
    procedure SetHash(const Value: THash);
    function GetDigest: string;
  public
    constructor Create(ADataStream: TStream; AHash: THash);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Done(ADigest: Pointer);
    property DataStream: TStream read FDataStream write SetDataStream;
    property Digest: string read GetDigest;
    property Hash: THash read FHash write SetHash;
  end;

  TEncryptStream = class(TStream)
  private
    FBlock: Pointer;
    FBlockCount: Integer;
    FBlockSize: Integer;
    FCipher: TCipher;
    FDataStream: TStream;
    procedure SetCipher(const Value: TCipher);
    procedure SetDataStream(const Value: TStream);
  public
    constructor Create(ADataStream: TStream; ACipher: TCipher);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Done: Integer;
    property Cipher: TCipher read FCipher write SetCipher;
    property DataStream: TStream read FDataStream write SetDataStream;
  end;

  TDecryptStream = class(TStream)
  private
    FBlock: PByteArray;
    FBlockCount: Integer;
    FBlockSize: Integer;
    FCipher: TCipher;
    FDataStream: TStream;
    FOK: Boolean;
    FDecBuffer: array [0..255] of Byte;
    FDecLen: Integer;
    procedure SetCipher(const Value: TCipher);
    procedure SetDataStream(const Value: TStream);
  public
    constructor Create(ADataStream: TStream; ACipher: TCipher);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Done: Integer; virtual;
    property Cipher: TCipher read FCipher write SetCipher;
    property DataStream: TStream read FDataStream write SetDataStream;
    property OK: Boolean read FOK;
  end;

  // TSeekableCipherStream is a wrapper for any seekable read/write data stream
  // containing cipher text. For optimal performance, read and write whole
  // blocks at a time and use a CTR mode cipher.
  // IMPORTANT: This class will ONLY provide confidentiality. It will NOT
  // guarantee the integrity or authenticity of your data. This is the price
  // of making it seekable.
  TSeekMode = (smCBC,           // cmCBC
               smCFB,           // cmCFB
               smCTR,           // cmCTR
               smOFB,           // cmOFB
               smReadAll,       // cmABC, cmPCFB, cmPipedPCFB
               smByPass);       // no encryption

  TSeekableCipherStream = class(TStream)
  private
    FCipher: TCipher;
    FDataStream: TStream;
    FDataStart: Integer;
    FDataSize: Integer;
    FBlockSize: Integer;
    FPosition: LongInt;
    FSize: LongInt;
    FIV: ISecretKey;
    FCipherClone: TCipher;
    FSeekMode: TSeekMode;
    FPaddingNeeded: Boolean;
  protected
    {$IFDEF D7UP}
    function GetSize: Int64; override;
    {$ENDIF}
    procedure SetSize(NewSize: Longint); override;                          
    function SetPositionCBC(APosition: Integer; AGetSize: Boolean): Integer;
    function SetPositionCFB(APosition: Integer; AGetSize: Boolean): Integer;
    function SetPositionCTR(APosition: Integer; AGetSize: Boolean): Integer;
    function SetPositionOFB(APosition: Integer; AGetSize: Boolean): Integer;
    function SetPositionReadAll(APosition: Integer; AGetSize: Boolean): Integer;
    function WriteToEnd(const Buffer; Count: Longint): Longint;
    function WriteToEndPadding(const Buffer; Count: Longint): Longint;
    function WriteToEndReWrite(const Buffer; Count: Longint): Longint;
  public
    constructor Create(ADataStream: TStream; ACipher: TCipher);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property SeekMode: TSeekMode read FSeekMode;
  end;


  // NOTE: Use the global variables in CryptGlobal to set the defaults for these
  // routines.

  // EncryptStream will authenticate, compress and encrypt InStrm using the
  // provided key material. The complete cipher text will be written to
  // OutStream.
procedure EncryptStream(const Password: string; InStrm, OutStrm: TStream); overload;
  // DecryptStream will decrypt, decompress and verify InStrm using the provided
  // key material. The plain text will be written to OutStream. The function
  // returns True if the plain text is verified OK.
function DecryptStream(const Password: string; InStrm, OutStrm: TStream): Boolean; overload;

  // The EncryptStream and DecryptStream routines below should be used if
  //   * The HMAC of the plain text has to be kept in a secondary location
  //     (e.g. as the index in a hash table).
  //   * ContentKey and HMacKey aren't derived from a password.
procedure EncryptStream(const ContentKey, HMacKey: string;
                        InStrm, OutStrm: TStream;
                        var HMac: string); overload;
function DecryptStream(const ContentKey, HMacKey: string;
                       InStrm, OutStrm: TStream;
                       const HMac: string = ''): Boolean; overload;

procedure EncryptStream(const ContentKey; CKLen: Integer;
                        const HMacKey; HMKLen: Integer;
                        InStrm, OutStrm: TStream;
                        HMac: Pointer = nil); overload;
function DecryptStream(const ContentKey; CKLen: Integer;
                       const HMacKey; HMKLen: Integer;
                       InStrm, OutStrm: TStream;
                       const HMac: string = ''): Boolean; overload;

  // EncryptString and DecryptString are equivalent to the first syntax of
  // EncryptStream and DecryptStream.
function EncryptString(const Password: string; Src: string): string;
function DecryptString(const Password: string; Src: string; var Dst: string): Boolean;

  // HMacString is a wrapper for the HMac procedure in SecUtils and might be
  // used of the Key, Msg and Mac are all string variables. It will use
  // DefaultHash and the size of the Mac (the length of the result) equals
  // DefaultKeySize.
function HMacString(const Key, Msg: string): string;
  // HMacFile calculates the HMac of the file FileName. The file is left
  // unmodified.
function HMacFile(const Key, FileName: string): string;

procedure CompressStream(ASource, ADest: TStream);
procedure DecompressStream(ASource, ADest: TStream);

implementation

uses
{$IFDEF LINUX}
  ZLib,
{$ENDIF}
  SysUtils, MpYarrow;

{$IFDEF MSWINDOWS}
type
  TAlloc = function (AppData: Pointer; Items, Size: Integer): Pointer; register;
  TFree = procedure (AppData, Block: Pointer); register;

  TZStreamRec = packed record
    next_in: PChar;       // next input byte
    avail_in: Integer;    // number of bytes available at next_in
    total_in: Integer;    // total nb of input bytes read so far

    next_out: PChar;      // next output byte should be put here
    avail_out: Integer;   // remaining free space at next_out
    total_out: Integer;   // total nb of bytes output so far

    msg: PChar;           // last error message, NULL if no error
    internal: Pointer;    // not visible by applications

    zalloc: TAlloc;       // used to allocate the internal state
    zfree: TFree;         // used to free the internal state
    AppData: Pointer;     // private data object passed to zalloc and zfree

    data_type: Integer;   //  best guess about the data type: ascii or binary
    adler: Integer;       // adler32 value of the uncompressed data
    reserved: Integer;    // reserved for future use
  end;

  TCustomZlibStream = class(TStream)
  private
    FStrm: TStream;
    FZRec: TZStreamRec;
    FBuffer: array [Word] of Char;
  protected
    constructor Create(Strm: TStream);
  end;
  TCompressionLevel = (clNone, clFastest, clDefault, clMax);

  TCompressionStream = class(TCustomZlibStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(CompressionLevel: TCompressionLevel; Dest: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property CompressionRate: Single read GetCompressionRate;
  end;

  TDecompressionStream = class(TCustomZlibStream)
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

const
  zlib_Version = '1.0.4';

type
  EZlibError = class(Exception);
  ECompressionError = class(EZlibError);
  EDecompressionError = class(EZlibError);

resourcestring
 sTargetBufferTooSmall = 'ZLib error: target buffer may be too small';
 sInvalidStreamOp = 'Invalid stream operation';

const
  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  Z_NO_COMPRESSION       =   0;
  Z_BEST_SPEED           =   1;
  Z_BEST_COMPRESSION     =   9;
  Z_DEFAULT_COMPRESSION  = (-1);

  Z_FILTERED            = 1;
  Z_HUFFMAN_ONLY        = 2;
  Z_DEFAULT_STRATEGY    = 0;

  Z_BINARY   = 0;
  Z_ASCII    = 1;
  Z_UNKNOWN  = 2;

  Z_DEFLATED = 8;

procedure _tr_init; external;
procedure _tr_tally; external;
procedure _tr_flush_block; external;
procedure _tr_align; external;
procedure _tr_stored_block; external;
procedure adler32; external;
procedure inflate_blocks_new; external;
procedure inflate_blocks; external;
procedure inflate_blocks_reset; external;
procedure inflate_blocks_free; external;
procedure inflate_set_dictionary; external;
procedure inflate_trees_bits; external;
procedure inflate_trees_dynamic; external;
procedure inflate_trees_fixed; external;
procedure inflate_trees_free; external;
procedure inflate_codes_new; external;
procedure inflate_codes; external;
procedure inflate_codes_free; external;
procedure _inflate_mask; external;
procedure inflate_flush; external;
procedure inflate_fast; external;

procedure _memset(P: Pointer; B: Byte; count: Integer); cdecl;
begin
  FillChar(P^, count, B);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^, dest^, count);
end;

// deflate compresses data
function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer; external;
function deflate(var strm: TZStreamRec; flush: Integer): Integer; external;
function deflateEnd(var strm: TZStreamRec): Integer; external;

// inflate decompresses data
function inflateInit_(var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer; external;
function inflate(var strm: TZStreamRec; flush: Integer): Integer; external;
function inflateEnd(var strm: TZStreamRec): Integer; external;
function inflateReset(var strm: TZStreamRec): Integer; external;


function zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer; register;
begin
//  GetMem(Result, Items*Size);
  Result := AllocMem(Items * Size);
end;

procedure zlibFreeMem(AppData, Block: Pointer); register;
begin
  FreeMem(Block);
end;

{$L obj\deflate.obj}
{$L obj\inflate.obj}
{$L obj\inftrees.obj}
{$L obj\trees.obj}
{$L obj\adler32.obj}
{$L obj\infblock.obj}
{$L obj\infcodes.obj}
{$L obj\infutil.obj}
{$L obj\inffast.obj}

function CCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise ECompressionError.Create('error'); //!!
end;

function DCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EDecompressionError.Create('error');  //!!
end;
   
// TCustomZlibStream

constructor TCustomZLibStream.Create(Strm: TStream);
begin
  inherited Create;
  FStrm := Strm;
  FZRec.zalloc := zlibAllocMem;
  FZRec.zfree := zlibFreeMem;
end;


// TCompressionStream

constructor TCompressionStream.Create(CompressionLevel: TCompressionLevel;
  Dest: TStream);
const
  Levels: array [TCompressionLevel] of ShortInt =
    (Z_NO_COMPRESSION, Z_BEST_SPEED, Z_DEFAULT_COMPRESSION, Z_BEST_COMPRESSION);
begin
  inherited Create(Dest);
  FZRec.next_out := FBuffer;
  FZRec.avail_out := sizeof(FBuffer);
  CCheck(deflateInit_(FZRec, Levels[CompressionLevel], zlib_version, sizeof(FZRec)));
end;

destructor TCompressionStream.Destroy;
begin
  FZRec.next_in := nil;
  FZRec.avail_in := 0;
  try
    while (CCheck(deflate(FZRec, Z_FINISH)) <> Z_STREAM_END) and
          (FZRec.avail_out = 0) do begin
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer));
      FZRec.next_out := FBuffer;
      FZRec.avail_out := sizeof(FBuffer);
    end;
    if FZRec.avail_out < sizeof(FBuffer) then
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer) - FZRec.avail_out);
  finally
    deflateEnd(FZRec);
  end;
  inherited Destroy;
end;

function TCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  FZRec.next_in := @Buffer;
  FZRec.avail_in := Count;
  while (FZRec.avail_in > 0) do begin
    CCheck(deflate(FZRec, 0));
    if FZRec.avail_out = 0 then begin
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer));
      FZRec.next_out := FBuffer;
      FZRec.avail_out := sizeof(FBuffer);
    end;
  end;
  Result := Count;
end;

function TCompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;

function TCompressionStream.GetCompressionRate: Single;
begin
  if FZRec.total_in = 0 then
    Result := 0
  else
    Result := (1.0 - (FZRec.total_out / FZRec.total_in)) * 100.0;
end;


// TDecompressionStream

constructor TDecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);
  FZRec.next_in := FBuffer;
  FZRec.avail_in := 0;
  DCheck(inflateInit_(FZRec, zlib_version, sizeof(FZRec)));
end;

destructor TDecompressionStream.Destroy;
begin
  FStrm.Seek(-FZRec.avail_in, 1);
  inflateEnd(FZRec);
  inherited Destroy;
end;

function TDecompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  FZRec.next_out := @Buffer;
  FZRec.avail_out := Count;
  while (FZRec.avail_out > 0) do begin
    if FZRec.avail_in = 0 then begin
      FZRec.avail_in := FStrm.Read(FBuffer, sizeof(FBuffer));
      FZRec.next_in := @FBuffer;
    end;
    if FZRec.avail_in = 0 then Break;
    CCheck(inflate(FZRec, 0));
  end;
  Result := Count - FZRec.avail_out;
end;

function TDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0; // Never called
end;
{$ENDIF}


procedure EncryptStream(const ContentKey; CKLen: Integer;
                        const HMacKey; HMKLen: Integer;
                        InStrm, OutStrm: TStream;
                        HMac: Pointer);
var
  KeyBuf: PChar;
  Buf: array [0..$FFFF] of Byte;
  H: THash;
  C: TCipher;
  KSize, VSize, MaxCount, Count: Integer;
  Cmpr: TCompressionStream;
  Encr: TEncryptStream;
begin
  if DefaultCipher.InheritsFrom(TBlockCipher) then
    VSize := TBlockCipherClass(DefaultCipher).BlockVectorSize
  else
    VSize := 0;
  KSize := DefaultKeySize + VSize * DefaultCipher.BlockSize;
  GetMem(KeyBuf,KSize);
  try
    if KSize <> CKLen then
      WeakPasswordToPrivKey256(ContentKey,CKLen,
                               1,
                               KeyBuf[0],KSize)
    else
      Move(ContentKey,KeyBuf[0],KSize);
    H := DefaultHash.Create(nil^,0);
    try
      if DefaultCipher.InheritsFrom(TBlockCipher) then
        C := DefaultCipher.Create(KeyBuf[0],KSize,VSize)
      else
        C := DefaultCipher.Create(KeyBuf[0],KSize,0);
      try
        HMacBegin(HMacKey,HMKLen,H);
        Encr := TEncryptStream.Create(OutStrm,C);
        try
          Cmpr := TCompressionStream.Create(clMax,Encr);
          try
            MaxCount := SizeOf(Buf) - 2;
            repeat
              Count := InStrm.Read(Buf[2],MaxCount);
              H.HashData(Buf[2],Count);
              Buf[0] := Count shr 8;
              Buf[1] := Count and $FF;
              Cmpr.Write(Buf,Count + 2);
            until Count = 0;
            HMacEnd(HMacKey,HMKLen,H,Buf,DefaultKeySize);
            Cmpr.Write(Buf,DefaultKeySize);
            if Assigned(HMac) then
              Move(Buf,HMac^,DefaultKeySize);
          finally
            Cmpr.Free;
          end;
          Encr.Done;
        finally
          Encr.Free;
        end;
      finally
        C.Free;
      end;
    finally
      H.Free;
    end;
  finally
    ProtectClear(KeyBuf^,KSize);
    FreeMem(KeyBuf);
  end;
end;

function DecryptStream(const ContentKey; CKLen: Integer;
                       const HMacKey; HMKLen: Integer;
                       InStrm, OutStrm: TStream;
                       const HMac: string): Boolean;
var
  KeyBuf: PChar;
  Buf: array [0..$FFFF] of Byte;
  H: THash;
  C: TCipher;
  KSize, VSize, Count, I, OrgPos: Integer;
  Decr: TDecryptStream;
  Dcmp: TDecompressionStream;
begin
  Result := False;
  if DefaultCipher.InheritsFrom(TBlockCipher) then
    VSize := TBlockCipherClass(DefaultCipher).BlockVectorSize
  else
    VSize := 0;
  KSize := DefaultKeySize + VSize * DefaultCipher.BlockSize;
  GetMem(KeyBuf,KSize);
  try
    if KSize <> CKLen then
      WeakPasswordToPrivKey256(ContentKey,CKLen,
                               1,
                               KeyBuf[0],KSize)
    else
      Move(ContentKey,KeyBuf[0],KSize);
    H := DefaultHash.Create(nil^,0);
    try
      if DefaultCipher.InheritsFrom(TBlockCipher) then
        C := DefaultCipher.Create(KeyBuf[0],KSize,VSize)
      else
        C := DefaultCipher.Create(KeyBuf[0],KSize,0);
      try
        HMacBegin(HMacKey,HMKLen,H);

        OrgPos := InStrm.Position;

        Decr := TDecryptStream.Create(InStrm,C);
        try
          Dcmp := TDecompressionStream.Create(Decr);
          try
            repeat
              if Dcmp.Read(Buf,2) < 2 then
                raise Exception.Create('Unexpected end of stream');
              Count := Buf[0] shl 8 + Buf[1];
              if Count > 0 then begin
                if Dcmp.Read(Buf[2],Count) <> Count then
                  raise Exception.Create('Unexpected end of stream');
                H.HashData(Buf[2],Count);
                OutStrm.Write(Buf[2],Count);
              end;
            until Count = 0;
            Dcmp.Read(Buf,DefaultKeySize);
            I := DefaultKeySize;
            HMacEnd(HMacKey,HMKLen,H,Buf[I],DefaultKeySize);
            Result := CompareMem(@Buf,@Buf[I],DefaultKeySize);
            if Result and (HMac <> '') then begin
              Result := DefaultKeySize = Length(HMac);
              if Result then
                Result := CompareMem(@Buf,Pointer(HMac),DefaultKeySize);
            end;
          finally
            Dcmp.Free;
            ProtectClear(Buf,SizeOf(Buf));
          end;
        finally
          Decr.Free;
        end;
        I := InStrm.Position - OrgPos;
        I := C.BlockSize - (I mod C.BlockSize);
        InStrm.Seek(I,soFromCurrent);
      finally
        C.Free;
      end;
    finally
      H.Free;
    end;    
  finally
    ProtectClear(KeyBuf^,KSize);
    FreeMem(KeyBuf);
  end;
end;

procedure EncryptStream(const ContentKey, HMacKey: string;
                        InStrm, OutStrm: TStream; var HMac: string);
begin
  SetLength(HMac,DefaultKeySize);
  EncryptStream(Pointer(ContentKey)^,Length(ContentKey),
                Pointer(HMacKey)^,Length(HMacKey),
                InStrm,OutStrm,
                Pointer(HMac));
end;

function DecryptStream(const ContentKey, HMacKey: string;
                       InStrm, OutStrm: TStream;
                       const HMac: string): Boolean;
begin
  Result := DecryptStream(Pointer(ContentKey)^,Length(ContentKey),
                          Pointer(HMacKey)^,Length(HMacKey),
                          InStrm,OutStrm,
                          HMac);
end;

procedure EncryptStream(const Password: string; InStrm, OutStrm: TStream);
var
  KeyBuf: PChar;
  Buf: array [0..$FFFF] of Byte;
  KSize, VSize: Integer;
begin
  if DefaultCipher.InheritsFrom(TBlockCipher) then
    VSize := TBlockCipherClass(DefaultCipher).BlockVectorSize
  else
    VSize := 0;
  KSize := DefaultKeySize*2 + VSize * DefaultCipher.BlockSize;
  GetMem(KeyBuf,KSize + DefaultKeySize);
  try
    RawRandom(Buf,8*DefaultKeySize);
    OutStrm.Write(Buf,DefaultKeySize);
    WeakPasswordToPrivKey256(Pointer(Password)^,Length(Password),
                             Buf,DefaultKeySize,
                             DefaultKDFIter,
                             KeyBuf[KSize],DefaultKeySize);
    WeakPasswordToPrivKey256(KeyBuf[KSize],DefaultKeySize,
                             1,
                             KeyBuf[0],KSize);
    if DefaultCipher.InheritsFrom(TBlockCipher) then
      EncryptStream(KeyBuf[DefaultKeySize],KSize - DefaultKeySize,
                    KeyBuf[0],DefaultKeySize,InStrm,OutStrm)
    else
      EncryptStream(KeyBuf[DefaultKeySize],DefaultKeySize,
                    KeyBuf[0],DefaultKeySize,InStrm,OutStrm)
  finally
    ProtectClear(KeyBuf^,KSize + DefaultKeySize);
    FreeMem(KeyBuf);
  end;
end;

function DecryptStream(const Password: string; InStrm, OutStrm: TStream): Boolean;
var
  KeyBuf: PChar;
  Buf: array [0..$FFFF] of Byte;
  KSize, VSize: Integer;
begin
  if DefaultCipher.InheritsFrom(TBlockCipher) then
    VSize := TBlockCipherClass(DefaultCipher).BlockVectorSize
  else
    VSize := 0;
  KSize := DefaultKeySize*2 + VSize * DefaultCipher.BlockSize;
  GetMem(KeyBuf,KSize + DefaultKeySize);
  try
    InStrm.Read(Buf,DefaultKeySize);
    WeakPasswordToPrivKey256(Pointer(Password)^,Length(Password),
                             Buf,DefaultKeySize,
                             DefaultKDFIter,
                             KeyBuf[KSize],DefaultKeySize);
    WeakPasswordToPrivKey256(KeyBuf[KSize],DefaultKeySize,
                             1,
                             KeyBuf[0],KSize);
    if DefaultCipher.InheritsFrom(TBlockCipher) then
      Result := DecryptStream(KeyBuf[DefaultKeySize],KSize - DefaultKeySize,
                              KeyBuf[0],DefaultKeySize,
                              InStrm,OutStrm)
    else
      Result := DecryptStream(KeyBuf[DefaultKeySize],DefaultKeySize,
                              KeyBuf[0],DefaultKeySize,
                              InStrm,OutStrm);
  finally
    ProtectClear(KeyBuf^,KSize + DefaultKeySize);
    FreeMem(KeyBuf);
  end;
end;


function EncryptString(const Password: string; Src: string): string;
var
  InStrm, OutStrm: TStringStream;
begin
  InStrm := TStringStream.Create(Src);
  try
    OutStrm := TStringStream.Create('');
    try
      EncryptStream(Password,InStrm,OutStrm);
      Result := OutStrm.DataString;
    finally
      OutStrm.Free;
    end;
  finally
    InStrm.Free;
  end;
end;

function DecryptString(const Password: string; Src: string; var Dst: string): Boolean;
var
  InStrm, OutStrm: TStringStream;
begin
  InStrm := TStringStream.Create(Src);
  try
    OutStrm := TStringStream.Create('');
    try
      Result := DecryptStream(Password,InStrm,OutStrm);
      Dst := OutStrm.DataString;
    finally
      OutStrm.Free;
    end;
  finally
    InStrm.Free;
  end;
end;

function HMacString(const Key, Msg: string): string;
begin
  SetLength(Result,DefaultKeySize);
  HMac(Pointer(Key)^,Length(Key),
       Pointer(Msg)^,Length(Msg),
       DefaultHash.Algorithm,
       Pointer(Result)^,Length(Result));
end;

function HMacFile(const Key, FileName: string): string;
var
  FS: TFileStream;
  H: THash;
  Buf: array [0..$FFFF] of Byte;
  BufLen: Integer;
begin
  if FileExists(FileName) then begin
    FS := TFileStream.Create(FileName,fmOpenRead);
    try
      H := DefaultHash.Create(nil^,0);
      try
        HMacBegin(Pointer(Key)^,Length(Key),H);
        repeat
          BufLen := FS.Read(Buf,SizeOf(Buf));
          if BufLen > 0 then
            H.HashData(Buf,BufLen);
        until BufLen = 0;
        SetLength(Result,DefaultKeySize);
        HMacEnd(Pointer(Key)^,Length(Key),H,Pointer(Result)^,Length(Result));
      finally
        H.Free;
      end;
    finally
      FS.Free;
    end;
  end else
    Result := '(Empty)';
end;

procedure DecompressStream(ASource, ADest: TStream);
var
  Dcmp: TDecompressionStream;
  Buffer: array [0..65535] of Byte;
  Len: Integer;
begin
  Dcmp := TDecompressionStream.Create(ASource);
  try
    repeat
      Len := Dcmp.Read(Buffer,SizeOf(Buffer));
      if Len > 0 then
        ADest.Write(Buffer,Len);
    until Len = 0;
  finally
    Dcmp.Free;
  end;
end;

procedure CompressStream(ASource, ADest: TStream);
var
  Cmpr: TCompressionStream;
  Buffer: array [0..65535] of Byte;
  Len: Integer;
begin
  Cmpr := TCompressionStream.Create(clMax,ADest);
  try
    repeat
      Len := ASource.Read(Buffer,SizeOf(Buffer));
      if Len > 0 then
        Cmpr.Write(Buffer,Len);
    until Len = 0;
  finally
    Cmpr.Free;
  end;
end;

type
  TBlockCipherHack = class(TBlockCipher);

{ TSeekableCipherStream }

constructor TSeekableCipherStream.Create(ADataStream: TStream;
  ACipher: TCipher);
var
  CC: TCipherClass;
begin
  inherited Create;
  FCipher := ACipher;
  FSeekMode := smByPass;
  if Assigned(ACipher) then begin
    case ACipher.Mode of
      cmCBC:
        FSeekMode := smCBC;
      cmCFB:
        FSeekMode := smCFB;
      cmCTR:
        FSeekMode := smCTR;
      cmOFB:
        FSeekMode := smOFB;
      cmABC, cmPCFB, cmPipedPCFB:
        FSeekMode := smReadAll;
    else
      raise Exception.Create('TSeekableCipherStream.Create: Cipher mode not supported');
    end;

    if ACipher.Mode in [cmCFB,cmOFB,cmCTR,cmPCFB] then begin
      if (ACipher is TBlockCipher) then begin
        FBlockSize := TBlockCipherHack(ACipher).ModeRatio;
        if FBlockSize = 0 then
          FBlockSize := ACipher.BlockSize;
        TBlockCipherHack(ACipher).ModeRatio := FBlockSize;
      end else
        FBlockSize := ACipher.BlockSize;
    end else
      FBlockSize := ACipher.BlockSize;

    if (ACipher is TBlockCipher) then begin
      FIV := TSecretKey.Create('');
      FIV.SetLength(ACipher.BlockSize * TBlockCipherHack(ACipher).BlockVectorSize);
      ACipher.GetVectorBuf(FIV.Key^,FIV.KeyLen);
    end else begin
      CC := TCipherClass(ACipher.ClassType);
      FCipherClone := CC.CreateClone(ACipher);
    end;

    FPaddingNeeded := (ACipher.Mode in [cmCBC,cmABC,cmPCFB,cmPipedPCFB]) and
                      (FBlockSize > 1);
  end;
  FDataStream := ADataStream;
  FDataStart := ADataStream.Position;
  FDataSize := ADataStream.Size - FDataStart;
  FSize := FDataSize;
  FPosition := 0;
end;

destructor TSeekableCipherStream.Destroy;
begin
  FCipherClone.Free;
  inherited;
end;

{$IFDEF D7UP}
function TSeekableCipherStream.GetSize: Int64;
begin
  Result := FSize;
  if (Result = FDataSize) and FPaddingNeeded then
    Result := inherited GetSize;
end;
{$ENDIF}

function TSeekableCipherStream.Read(var Buffer; Count: Integer): Longint;
var
  P: PChar;
  Len: Integer;
  LPosition: Integer;
  LBuffer: array [0..47] of Byte;
begin
  P := @Buffer;
  LPosition := FPosition;
  Result := 0;

  // Don't read data that isn't there:
  Len := FSize - LPosition;
  if Count > Len then
    Count := Len;

  // If Count > 0 then read into Buffer:
  if Count > 0 then begin
    if FPaddingNeeded and (LPosition mod FBlockSize > 0) then begin
      FDataStream.Read(LBuffer,FBlockSize);
      if Assigned(FCipher) then
        FCipher.Decrypt(LBuffer,FBlockSize);
      Len := FBlockSize - (LPosition mod FBlockSize);
      if Len > Count then
        Len := Count;
      Move(LBuffer[LPosition mod FBlockSize],P^,Len);
      Inc(LPosition,Len);
      Inc(P,Len);
      Dec(Count,Len);
      Result := Len;
      FPosition := LPosition;
    end;
    if (FSize > LPosition) and (Count > 0) then begin
      Len := Count;
      if FPaddingNeeded then begin
        Len := Len - (Len mod FBlockSize);
        Assert(FPosition = LPosition,
               'Internal error in TSeekableCipherStream: FSize is miscalculated');
      end;
      FDataStream.Position := LPosition + FDataStart;
      if Len > 0 then begin
        Assert(FDataStream.Read(P^,Len) = Len,
               'TSeekableCipherStream.Read: Failure reading from data stream');
        if Assigned(FCipher) then
          FCipher.Decrypt(P^,Len);
        Inc(P,Len);
        Inc(Result,Len);
        Dec(Count,Len);
        Inc(LPosition,Len);
      end;
      if (LPosition < FSize) and FPaddingNeeded then begin
        Assert(FDataStream.Read(LBuffer,FBlockSize) = FBlockSize,
               'TSeekableCipherStream.Read: Failure reading from data stream');
        FDataStream.Seek(-FBlockSize,soFromCurrent);
        FCipher.GetVectorBuf(LBuffer[FBlockSize],FCipher.BlockSize*TBlockCipher(FCipher).BlockVectorSize);
        FCipher.Decrypt(LBuffer,FBlockSize);
        FCipher.SetVectorBuf(LBuffer[FBlockSize],FCipher.BlockSize*TBlockCipher(FCipher).BlockVectorSize);
        if LPosition >= FSize - FBlockSize then begin
          Len := FBlockSize - LBuffer[FBlockSize-1];
          FSize := LPosition + Len;
        end else
          Len := FBlockSize;

        if Count > Len then
          Count := Len;
        if Count > 0 then
          Move(LBuffer,P^,Count);
        Inc(LPosition,Count);
        Inc(Result,Count);
      end;
    end;
    FPosition := LPosition;
  end;
end;

function TSeekableCipherStream.Seek(Offset: Integer;
  Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning:
      Result := Offset;
    soFromCurrent:
      Result := FPosition + Offset;
    soFromEnd:
      Result := FSize + Offset;
  else
    Result := 0;
    Assert(False,'TSeekableCipherStream.Seek: Unknown kind of origin');
  end;
  if Result < 0 then
    Result := 0;
  if Result <> FPosition then begin
    case FSeekMode of
      smByPass:
        Result := FDataStream.Seek(Result + FDataStart,soFromBeginning);
      smCTR:
        Result := SetPositionCTR(Result,(Origin = soFromEnd) and (Offset = 0));
      smOFB:
        Result := SetPositionOFB(Result,(Origin = soFromEnd) and (Offset = 0));
      smReadAll:
        Result := SetPositionReadAll(Result,(Origin = soFromEnd) and (Offset = 0));
      smCBC:
        Result := SetPositionCBC(Result,(Origin = soFromEnd) and (Offset = 0));
      smCFB:
        Result := SetPositionCFB(Result,(Origin = soFromEnd) and (Offset = 0));
    end;
  end;
  FPosition := Result;
end;

function TSeekableCipherStream.SetPositionCBC(APosition: Integer;
  AGetSize: Boolean): Integer;
var
  LDataPos, LPosition, Len: Integer;
  LBuffer: array [0..$FFFF] of Byte;
begin
  // The internal state of FCipher and FDataStream are positioned at
  // the block boundary at or immediately before FPosition.
  if (FSize = FDataSize) and AGetSize then begin
    APosition := APosition - FBlockSize;
    if APosition < 0 then begin
      Result := 0;
      Exit;
    end;
  end;
  Result := APosition;
  LDataPos := APosition - (APosition mod FBlockSize);
  if LDataPos = 0 then
    FCipher.SetVectorBuf(FIV.Key^,FIV.KeyLen)
  else if LDataPos <= FSize then begin
    FDataStream.Position := FDataStart + LDataPos - FBlockSize;
    FDataStream.Read(LBuffer[FBlockSize],FBlockSize);
    FCipher.SetVectorBuf(LBuffer[FBlockSize],FBlockSize);
    TBlockCipherHack(FCipher).FFBIndex := 0;
    if (FSize = FDataSize) and AGetSize then begin
      FDataStream.Read(LBuffer,FBlockSize);
      FCipher.Decrypt(LBuffer,FBlockSize);
      if (LBuffer[FBlockSize-1] <= FBlockSize) and (LBuffer[FBlockSize-1] > 0) then
        FSize := FSize - LBuffer[FBlockSize-1];
      Result := FSize;
      FCipher.SetVectorBuf(LBuffer[FBlockSize],FBlockSize);
      FDataStream.Seek(-FBlockSize,soFromCurrent);
    end;
  end else begin
    LPosition := Seek(0,soFromEnd);
    FillChar(LBuffer,SizeOf(LBuffer),0);
    while LPosition < Result do begin
      Len := SizeOf(LBuffer);
      if Len + LPosition > Result then
        Len := Result - LPosition;
      Inc(LPosition,Write(LBuffer,Len));
    end;
  end;
end;

function TSeekableCipherStream.SetPositionCFB(APosition: Integer;
  AGetSize: Boolean): Integer;
var
  LDataPos, LPosition, Len: Integer;
  LBuffer: array [0..$FFFF] of Byte;
begin
  // The internal state of FCipher and FDataStream are positioned at
  // the block boundary at or immediately before FPosition.
  if (FSize = FDataSize) and AGetSize then begin
    APosition := APosition - FBlockSize;
    if APosition < 0 then begin
      Result := 0;
      Exit;
    end;
  end;
  Result := APosition;
  LDataPos := APosition - (APosition mod FBlockSize);
  if LDataPos = 0 then begin
    FCipher.SetVectorBuf(FIV.Key^,FIV.KeyLen);    
    TBlockCipherHack(FCipher).FFBIndex := 0;
  end else if LDataPos <= FSize then begin
    FDataStream.Position := FDataStart + LDataPos - FBlockSize;
    FDataStream.Read(LBuffer[FBlockSize],FBlockSize);
    FCipher.SetVectorBuf(LBuffer[FBlockSize],FBlockSize);
    if (FSize = FDataSize) and AGetSize then begin
      TBlockCipherHack(FCipher).FFBIndex := 0;
      FDataStream.Read(LBuffer,FBlockSize);
      FDataStream.Seek(-FBlockSize,soFromCurrent);
      FCipher.Decrypt(LBuffer[FBlockSize*2],FBlockSize);
      if (LBuffer[FBlockSize-1] <= FBlockSize) and (LBuffer[FBlockSize-1] > 0) then
        FSize := FSize - LBuffer[FBlockSize-1];
      Result := FSize;
      FCipher.SetVectorBuf(LBuffer[FBlockSize],FBlockSize);
    end;
    TBlockCipherHack(FCipher).FFBIndex := 0;
    FDataStream.Read(LBuffer,APosition - LDataPos);
    FCipher.Decrypt(LBuffer,APosition - LDataPos);
  end else begin
    LPosition := Seek(0,soFromEnd);
    FillChar(LBuffer,SizeOf(LBuffer),0);
    while LPosition < Result do begin
      Len := SizeOf(LBuffer);
      if Len + LPosition > Result then
        Len := Result - LPosition;
      Inc(LPosition,Write(LBuffer,Len));
    end;
  end;
end;

function TSeekableCipherStream.SetPositionCTR(APosition: Integer;
  AGetSize: Boolean): Integer;
var
  LBuffer: array [0..$FFFF] of Byte;
  LPosition, Len: Integer;
begin
  // First, advance the Counter, at most to the current FSize:
  Assert(Assigned(FIV),'TSeekableCipherStream.Seek: Internal error');
  Len := FIV.KeyLen;
  LPosition := FSize;
  if APosition < LPosition then
    LPosition := APosition;
  SecUtils.IncBlockByInt(LBuffer,FIV.Key^,Len shr 2,LPosition div FBlockSize);
  FCipher.SetVectorBuf(LBuffer,Len);

  // Second, advance the FBIndex, at most to the current FSize:
  Len := LPosition mod FBlockSize;
  FDataStream.Seek(FDataStart + LPosition,soFromBeginning);
  TBlockCipherHack(FCipher).FFBIndex := 0;
  if Len > 0 then begin
    FCipher.Decrypt(LBuffer,Len); // Whatever, could be Encrypt as well
    Assert((Len = LPosition mod FBlockSize) or AGetSize,
           'TSeekableCipherStream.Seek: Internal error');
  end;

  // Third, set size if necessary:
  if LPosition < APosition then begin
    while LPosition < APosition do begin
      Len := APosition - LPosition;
      if Len > SizeOf(LBuffer) then
        Len := SizeOf(LBuffer);
      // Here we could just set FDataStream.Size and advance FBIndex and thereby
      // fill the stream with garbage, but let's do it properly and pad the
      // stream with binary zero:
      FillChar(LBuffer,Len,0);
      FCipher.Encrypt(LBuffer,Len);
      FDataStream.Write(LBuffer,Len);
      Inc(LPosition,Len);
    end;
    FSize := LPosition;
    FDataSize := LPosition;
  end;

  // Lastly, assign current position to Result and FPosition:
  Result := LPosition;
  FPosition := LPosition;
end;

function TSeekableCipherStream.SetPositionOFB(APosition: Integer;
  AGetSize: Boolean): Integer;
begin
  Result := SetPositionReadAll(APosition,AGetSize);
end;

function TSeekableCipherStream.SetPositionReadAll(APosition: Integer;
  AGetSize: Boolean): Integer;
var
  CC: TCipherClass;
  LBuffer: array [0..$FFFF] of Byte;
  LPosition, Len, LReadLen: Integer;
begin
  if Assigned(FIV) then begin
    FCipher.SetVectorBuf(FIV.Key^,FIV.KeyLen);
    TBlockCipherHack(FCipher).FFBIndex := 0;
  end else begin
    Assert(Assigned(FCipherClone),'TSeekableCipherStream.Seek: Internal error');
    CC := TCipherClass(FCipher.ClassType);
    FCipher.Free;
    FCipher := CC.CreateClone(FCipherClone);
  end;
  FDataStream.Position := FDataStart;
  LPosition := 0;
  Result := APosition;
  FPosition := 0;
  while (LPosition < FSize) and (LPosition < Result) do begin
    Len := SizeOf(LBuffer);
    if Len + LPosition > Result then
      Len := Result - LPosition;
    if Len + LPosition > FSize then
      Len := FSize - LPosition;
    LReadLen := Read(LBuffer,Len);
    Inc(LPosition,LReadLen);
    if LReadLen < Len then
      Result := LPosition;
  end;
  Assert(LPosition = FPosition,'TSeekableCipherStream.Seek: Internal error');
  if LPosition < Result then begin
    Assert(LPosition = FSize,'TSeekableCipherStream.Seek: Internal error');
    FillChar(LBuffer,SizeOf(LBuffer),0);
    while LPosition < Result do begin
      Len := SizeOf(LBuffer);
      if Len + LPosition > Result then
        Len := Result - LPosition;
      Inc(LPosition,Write(LBuffer,Len));
    end;
    Assert(LPosition = FSize,'TSeekableCipherStream.Seek: Internal error');
  end;
  Assert(LPosition = FPosition,'TSeekableCipherStream.Seek: Internal error');
  Result := LPosition;
end;

procedure TSeekableCipherStream.SetSize(NewSize: Integer);
var
  OldPos: Integer;
begin
  OldPos := FPosition;
  Seek(NewSize,soFromBeginning);
  Seek(OldPos,soFromBeginning);
end;

function TSeekableCipherStream.Write(const Buffer;
  Count: Integer): Longint;
begin
  if Count > 0 then begin
    case FSeekMode of
      smByPass:
        Result := FDataStream.Write(Buffer,Count);
      smCTR, smOFB:
        Result := WriteToEnd(Buffer,Count);
    else
      if Count + FPosition > FDataSize - FBlockSize then begin
        if FPaddingNeeded then
          Result := WriteToEndPadding(Buffer,Count)
        else
          Result := WriteToEnd(Buffer,Count);
      end else
        Result := WriteToEndReWrite(Buffer,Count);
    end;
  end else
    Result := 0;
end;

function TSeekableCipherStream.WriteToEnd(const Buffer;
  Count: Integer): Longint;
var
  P: PChar;
  LBuffer: array [0..$FFFF] of Byte;
  Len: Integer;
begin
  P := @Buffer;
  Result := 0;
  if Count > 0 then begin
    Result := Count;
    while Count > 0 do begin
      Len := Count;
      if Len > SizeOf(LBuffer) then
        Len := SizeOf(LBuffer);
      Move(P^,LBuffer,Len);
      FCipher.Encrypt(LBuffer,Len);
      Assert(FDataStream.Write(LBuffer,Len) = Len,'TSeekableCipherStream.WriteToEnd');
      Inc(P,Len);
      Dec(Count,Len);
    end;
    Inc(FPosition,Result);
    if FPosition > FSize then
      FSize := FPosition;
    if FSize >= FDataSize then
      FDataSize := FDataStream.Size - FDataStart;
  end;
end;

function TSeekableCipherStream.WriteToEndPadding(const Buffer;
  Count: Integer): Longint;
var
  P: PChar;
  LBuffer: array [0..$FFFF] of Byte;
  Len, I: Integer;
  Pad: Byte;
begin
  P := @Buffer;
  Result := 0;
  if Count > 0 then begin
    Result := Count;

    if FPosition mod FBlockSize > 0 then begin
      FDataStream.Read(LBuffer,FBlockSize);
      FCipher.GetVectorBuf(LBuffer[FBlockSize],FCipher.BlockSize * TBlockCipher(FCipher).BlockVectorSize);
      FCipher.Decrypt(LBuffer,FBlockSize);
      FCipher.SetVectorBuf(LBuffer[FBlockSize],FCipher.BlockSize * TBlockCipher(FCipher).BlockVectorSize);
      Len := FBlockSize - (FPosition mod FBlockSize);
      if Len > Count then
        Len := Count;
      Move(P^,LBuffer[FBlockSize - Len],Len);
      Inc(P,Len);
      FCipher.Encrypt(LBuffer,FBlockSize);
      FDataStream.Seek(-FBlockSize,soFromCurrent);
      FDataStream.Write(LBuffer,FBlockSize);
      Dec(Count,Len);
    end;
    while Count >= FBlockSize do begin
      Len := Count;
      if Len > SizeOf(LBuffer) then
        Len := SizeOf(LBuffer);
      if Len mod FBlockSize > 0 then
        Dec(Len,Len mod FBlockSize);
      Move(P^,LBuffer,Len);
      FCipher.Encrypt(LBuffer,Len);
      Assert(FDataStream.Write(LBuffer,Len) = Len,'TSeekableCipherStream.WriteToEnd');
      Inc(P,Len);
      Dec(Count,Len);
    end;
    FPosition := FPosition + Result;
    FSize := FPosition;
    Len := FBlockSize;
    Pad := Len - Count;
    for I := Count to FBlockSize - 1 do
      LBuffer[I] := Pad;
    Move(P^,LBuffer,Count);
    FCipher.GetVectorBuf(LBuffer[FBlockSize],FCipher.BlockSize * TBlockCipher(FCipher).BlockVectorSize);
    FCipher.Encrypt(LBuffer,FBlockSize);
    FCipher.SetVectorBuf(LBuffer[FBlockSize],FCipher.BlockSize * TBlockCipher(FCipher).BlockVectorSize);
    FDataStream.Write(LBuffer,FBlockSize);
    FDataSize := FDataStream.Seek(-FBlockSize,soFromCurrent) + FBlockSize;
  end;
end;

function TSeekableCipherStream.WriteToEndReWrite(const Buffer;
  Count: Integer): Longint;
var
  P: PChar;
  LPos: Integer;
  CC: TCipherClass;
  LReadClone: TCipher;
  LWriteClone: TCipher;

  procedure ReadWrite(AReadCipher, AWriteCipher: TCipher;
                      Buf: PChar; Count: Integer);
  var
    LBuffer: array [0..$FFFF] of Byte;
    Len: Integer;
  begin
    while Count > 0 do begin
      Len := Count;
      if Len > SizeOf(LBuffer) then
        Len := SizeOf(LBuffer);
      if Len mod FBlockSize > 0 then
        Dec(Len,Len mod FBlockSize);
      Assert(FDataStream.Read(LBuffer,Len) = Len,'TSeekableCipherStream.WriteToEndReWrite');
      AReadCipher.Decrypt(LBuffer,Len);
      if Assigned(Buf) then begin
        Move(Buf^,LBuffer,Len);
        Inc(Buf,Len);
      end;
      AWriteCipher.Encrypt(LBuffer,Len);
      FDataStream.Seek(-Len,soFromCurrent);
      Assert(FDataStream.Write(LBuffer,Len) = Len,'TSeekableCipherStream.WriteToEndReWrite');
      Dec(Count,Len);
    end;
  end;

  procedure ReadWritePadding(AReadCipher, AWriteCipher: TCipher;
                             Buf: PChar; Count: Integer);
  var
    LBuffer: array [0..$FFFF] of Byte;
    Len: Integer;
  begin
    if FPosition mod FBlockSize > 0 then begin
      FDataStream.Read(LBuffer,FBlockSize);
      AReadCipher.Decrypt(LBuffer,FBlockSize);
      Len := FBlockSize - (FPosition mod FBlockSize);
      if Len > Count then
        Len := Count;
      if Assigned(Buf) then begin
        Move(Buf^,LBuffer[FBlockSize - Len],Len);
        Inc(Buf,Len);
      end;
      AWriteCipher.Encrypt(LBuffer,FBlockSize);
      FDataStream.Seek(-FBlockSize,soFromCurrent);
      FDataStream.Write(LBuffer,FBlockSize);
      Dec(Count,Len);
    end;
    ReadWrite(AReadCipher,AWriteCipher,Buf,Count - (Count mod FBlockSize));
    Count := Count mod FBlockSize;
    if Count > 0 then begin
      FDataStream.Read(LBuffer,FBlockSize);
      AReadCipher.Decrypt(LBuffer,FBlockSize);
      Len := FBlockSize - (FPosition mod FBlockSize);
      if Len > Count then
        Len := Count;
      if Assigned(Buf) then
        Move(Buf^,LBuffer,Len);
      AWriteCipher.Encrypt(LBuffer,FBlockSize);
      FDataStream.Seek(-FBlockSize,soFromCurrent);
      FDataStream.Write(LBuffer,FBlockSize);
      FDataStream.Seek(-FBlockSize,soFromCurrent);
    end;
  end;

begin
  P := @Buffer;
  Result := 0;
  if Count > 0 then begin
    Result := Count;
    CC := TCipherClass(FCipher.ClassType);
    LReadClone := CC.CreateClone(FCipher);
    try
      if FPaddingNeeded then
        ReadWritePadding(LReadClone,FCipher,P,Count)
      else
        ReadWrite(LReadClone,FCipher,P,Count);
      Inc(FPosition,Result);
      LWriteClone := CC.CreateClone(FCipher);
      try
        LPos := FPosition;
        if FPaddingNeeded then
          ReadWritePadding(LReadClone,LWriteClone,nil,FDataSize - LPos)
        else
          ReadWrite(LReadClone,LWriteClone,nil,FDataSize - LPos);
      finally
        LWriteClone.Free;
      end;
    finally
      LReadClone.Free;
    end;
  end;
end;

{ TDigestStream }

constructor TDigestStream.Create(ADataStream: TStream; AHash: THash);
begin
  inherited Create;
  FDataStream := ADataStream;
  FHash := AHash;
end;

procedure TDigestStream.Done(ADigest: Pointer);
begin
  if Assigned(FHash) then
    FHash.Done(ADigest);
end;

function TDigestStream.GetDigest: string;
begin
  if Assigned(FHash) then
    Result := FHash.Digest
  else
    Result := '';
end;

function TDigestStream.Read(var Buffer; Count: Integer): Longint;
begin
  if Assigned(FDataStream) then begin
    Result := FDataStream.Read(Buffer,Count);
    if Assigned(FHash) then
      FHash.HashData(Buffer,Result);
  end else
    Result := 0;
end;

function TDigestStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if Assigned(FDataStream) then
    Result := FDataStream.Seek(Offset,Origin)
  else
    Result := 0;
end;

procedure TDigestStream.SetDataStream(const Value: TStream);
begin
  FDataStream := Value;
end;

procedure TDigestStream.SetHash(const Value: THash);
begin
  FHash := Value;
end;

function TDigestStream.Write(const Buffer; Count: Integer): Longint;
begin
  if Assigned(FDataStream) then begin  
    Result := FDataStream.Write(Buffer,Count);
    if Assigned(FHash) then
      FHash.HashData(Buffer,Result);
  end else
    Result := 0;
end;

{ TEncryptStream }

constructor TEncryptStream.Create(ADataStream: TStream; ACipher: TCipher);
begin
  inherited Create;
  FCipher := ACipher;
  if ACipher is TBlockCipher then begin
    FBlockSize := ACipher.BlockSize;
    GetMem(FBlock,FBlockSize);
  end;
  FDataStream := ADataStream;
end;

destructor TEncryptStream.Destroy;
begin
  ProtectClear(FBlock^,FBlockSize);
  FreeMem(FBlock);
  FBlock := nil;
  inherited;
end;

function TEncryptStream.Done: Integer;
var
  P: PChar;
  I, PadSize: Integer;
begin
  if Assigned(FBlock) then begin
    PadSize := FBlockSize - FBlockCount;
    P := FBlock;
    for I := FBlockSize - 1 downto FBlockCount do
      P[I] := Char(PadSize);
    FCipher.Encrypt(FBlock^,FBlockSize);
    FDataStream.Write(FBlock^,FBlockSize);
    Result := PadSize;
    FBlockCount := 0;
  end else
    Result := 0;
end;

function TEncryptStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

function TEncryptStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if Assigned(FDataStream) then
    Result := FDataStream.Seek(Offset,Origin)
  else
    Result := 0;
end;

procedure TEncryptStream.SetCipher(const Value: TCipher);
begin
  FCipher := Value;
end;

procedure TEncryptStream.SetDataStream(const Value: TStream);
begin
  FDataStream := Value;
end;

function TEncryptStream.Write(const Buffer; Count: Integer): Longint;
var
  P: PChar;
  InternalBuffer: array [0..1023] of Byte;
  InBuffer: Integer;
begin
  if Assigned(FDataStream) then begin
    if Assigned(FCipher) then begin
      Result := 0;
      P := @Buffer;
      Count := Count + FBlockCount;
      while Count > 0 do begin
        if FBlockCount > 0 then
          Move(FBlock^,InternalBuffer,FBlockCount);
        if Count >= 1024 then begin
          Move(P^,InternalBuffer[FBlockCount],1024 - FBlockCount);
          P := P + 1024 - FBlockCount;
          Result := Result + 1024 - FBlockCount;
          InBuffer := 1024;
          Count := Count - 1024;
        end else begin
          Move(P^,InternalBuffer[FBlockCount],Count - FBlockCount);
          P := P + Count - FBlockCount;
          Result := Result + Count - FBlockCount;
          InBuffer := Count;
          Count := 0;
        end;
        if FBlockSize > 0 then begin
          FBlockCount := InBuffer mod FBlockSize;
          InBuffer := InBuffer - FBlockCount;
          Count := Count + FBlockCount;
          Move(InternalBuffer[InBuffer],FBlock^,FBlockCount);
        end else
          FBlockCount := 0;
        if InBuffer = 0 then
          Break;
        FCipher.Encrypt(InternalBuffer,InBuffer);
        FDataStream.Write(InternalBuffer,InBuffer);
      end;
    end else
      Result := FDataStream.Write(Buffer,Count);
  end else
    Result := 0;
end;

{ TDecryptStream }

constructor TDecryptStream.Create(ADataStream: TStream; ACipher: TCipher);
begin    
  inherited Create;
  FCipher := ACipher;
  if ACipher is TBlockCipher then begin
    FBlockSize := ACipher.BlockSize;
    GetMem(FBlock,FBlockSize);
  end;
  FDataStream := ADataStream;
end;

destructor TDecryptStream.Destroy;
begin
  inherited;
  ProtectClear(FBlock^,FBlockCount);
  FreeMem(FBlock);
  ProtectClear(FDecBuffer,FDecLen);
end;

function TDecryptStream.Done: Integer;
var
  I: Integer;
begin
  Result := 0;
  FOK := FDecLen <= FBlockSize;
  if FOK and (FDecLen > 0) then begin
    Result := FDecBuffer[FDecLen - 1];
    if Result > FBlockSize then
      FOK := False
    else if Result <> FDecLen then
      FOK := False
    else
      for I := 0 to Result - 2 do
        if FDecBuffer[I] <> Result then
          FOK := False;
  end;
end;

function TDecryptStream.Read(var Buffer; Count: Integer): Longint;
var
  P: PChar;
  InBuffer, PLen, DLen: Integer;
begin
  if Assigned(FDataStream) then begin
    if Assigned(FCipher) then begin
      Result := 0;
      InBuffer := 0;
      P := @Buffer;
      if FDecLen > 0 then begin
        if FDecLen > Count then begin
          Move(FDecBuffer,Buffer,Count);
          Move(FDecBuffer[Count],FDecBuffer,FDecLen - Count);
          FDecLen := FDecLen - Count;
          Result := Count;
          Count := 0;
        end else begin
          Move(FDecBuffer,Buffer,FDecLen);
          P := P + FDecLen;
          Result := FDecLen;
          Count := Count - FDecLen;
          FDecLen := 0;
        end;
      end else if FBlockCount > 0 then begin
        Move(FBlock^,P^,FBlockCount);
        InBuffer := FBlockCount;
        P := P + InBuffer;
        FBlockCount := 0;
      end;
      if Count > 0 then begin
        InBuffer := InBuffer + FDataStream.Read(P^,Count);
        Result := Result + InBuffer;
        DLen := InBuffer mod FBlockSize;
        PLen := InBuffer - DLen;
        if PLen > 0 then FCipher.Decrypt(P^,PLen);
        Move(P[PLen],FBlock^,DLen);
        FBlockCount := FDataStream.Read(FBlock^[DLen],FBlockSize - DLen) + DLen;
        if FBlockCount = FBlockSize then begin
          FCipher.DecryptToPtr(FBlock,@FDecBuffer,FBlockCount);
          Move(FDecBuffer,P[PLen],DLen);
          FBlockCount := 0;
          FDecLen := FBlockSize - DLen;
          Move(FDecBuffer[DLen],FDecBuffer,FDecLen);
        end else
          Result := Result - DLen;
      end;
    end else
      Result := FDataStream.Read(Buffer,Count);
  end else
    Result := 0;
end;

function TDecryptStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if Assigned(FDataStream) then begin
    Result := FDataStream.Seek(Offset,Origin);
  end else
    Result := 0;
end;

procedure TDecryptStream.SetCipher(const Value: TCipher);
begin
  FCipher := Value;
end;

procedure TDecryptStream.SetDataStream(const Value: TStream);
begin
  FDataStream := Value;
end;

function TDecryptStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

end.
