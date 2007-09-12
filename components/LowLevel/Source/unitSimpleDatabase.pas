unit unitSimpleDatabase;

interface

uses Windows, Classes, SysUtils, unitBTree, SyncObjs;

type

TCustomSimpleDatabase = class;

TSimpleDatabaseFieldType = (sdfInteger, sdfBoolean, sdfString);
TSimpleDatabaseField = class (TCollectionItem)
private
  fName: string;
  fFieldType : TSimpleDatabaseFieldType;
  function GetInternalSize : Integer;
public
published
  property Name : string read fName write fName;
  property FieldType : TSimpleDatabaseFieldType read fFieldType write fFieldType;
end;

TSimpleDatabaseFields = class (TOwnedCollection)
private
  function GetFields(index: Integer): TSimpleDatabaseField;
public
  property Fields [index : Integer] : TSimpleDatabaseField read GetFields;
end;

TSimpleDatabaseRecord = class
private
  fOwner : TCustomSimpleDatabase;
  fData : Pointer;
  function GetFieldAsString(const field: string): String;
  procedure SetFieldAsString(const field, Value: String);

  function GetFieldData (const field : string; var tp : TSimpleDatabaseFieldType) : pointer;

  function GetFieldAsBoolean(const field: string): Boolean;
  procedure SetFieldAsBoolean(const field: string; const Value: Boolean);
  function GetFieldAsInteger(const field: string): Integer;
  procedure SetFieldAsInteger(const field: string; const Value: Integer);

  function GetInternalFieldSize : Integer;
  procedure SaveToStream (stream : TStream);
  procedure LoadFromStream (stream : TStream);
protected
  function GetKey : string;
public
  constructor Create (AOwner : TCustomSimpleDatabase);
  destructor Destroy; override;

  property Owner : TCustomSimpleDatabase read fOwner;

  property FieldAsInteger [const field : string] : Integer read GetFieldAsInteger write SetFieldAsInteger;
  property FieldAsBoolean [const field : string] : Boolean read GetFieldAsBoolean write SetFieldAsBoolean;
  property FieldAsString  [const field : string] : String  read GetFieldAsString  write SetFieldAsString;
end;

TCustomSimpleDatabase = class (TComponent)
private
  fIndex : TBTree;
  fFields : TSimpleDatabaseFields;
  fDataFile : TFileStream;
  fIndexField: string;
  fLock : TCriticalSection;
  function GetRawCount: Integer;
public
  constructor Create (AOwner : TComponent); override;
  destructor Destroy; override;

  procedure Open (const Path, Name : string);
  procedure Close (leaveLocked : boolean);
  procedure Reindex;

  procedure Add (rec : TSimpleDatabaseRecord);
  procedure Delete (rec : TSimpleDatabaseRecord);
  procedure Replace (rec : TSimpleDatabaseRecord);
  function Find (rec : TSimpleDatabaseRecord) : boolean;
  function LockGetCount : Integer;
  procedure Unlock;
  procedure Get (idx : Integer; rec : TSimpleDatabaseRecord);

  property Fields : TSimpleDatabaseFields read fFields write fFields;
  property IndexField : string read fIndexField write fIndexField;
  property RawCount : Integer read GetRawCount;
end;

TSimpleDatabase = class (TCustomSimpleDatabase)
published
  property Fields;
end;

ESimpleDatabase = class (Exception);

implementation

resourcestring
  rstFieldNotFound = 'Field %s not found';
  rstTypeMismatch = 'Type mismatch';
  rstKeyNotFound = 'Key not found';

{ TCustomSimpleDatabase }

procedure TCustomSimpleDatabase.Add(rec: TSimpleDatabaseRecord);
var
  idx : string;
  offset : Integer;
begin
  fLock.Enter;
  try
    if fDataFile = Nil then Exit;
    idx := rec.GetKey;
    offset := fDataFile.Seek(0, soFromEnd);
    fIndex.AddKey(idx, offset);
    try
      rec.SaveToStream(fDataFile);
    except
      try
        fIndex.DeleteKey(idx)
      except

      end;
      raise
    end
  finally
    fLock.Leave
  end
end;

procedure TCustomSimpleDatabase.Close (leaveLocked : boolean);
begin
  fLock.Enter;
  try
    FreeAndNil (fIndex);
    FreeAndNil (fDataFile)
  finally
    if not leaveLocked then
      fLock.Leave
  end
end;

constructor TCustomSimpleDatabase.Create(AOwner: TComponent);
begin
  inherited;

  fLock := TCriticalSection.Create;
  fFields := TSimpleDatabaseFields.Create(self, TSimpleDatabaseField);
end;

procedure TCustomSimpleDatabase.Delete(rec: TSimpleDatabaseRecord);
var
  idx : string;
  offset : Integer;
begin
  fLock.Enter;
  try
    if fDataFile = Nil then Exit;
    idx := rec.GetKey;
    if fIndex.Find(idx, offset) then
    begin
      if fIndex.DeleteKey (idx) then
      begin
        fDataFile.Seek(offset, soFromBeginning);
        fDataFile.Write('XnDbZ' [1], 5)
      end
    end
    else
      raise ESimpleDatabase.Create(rstKeyNotFound);
  finally
    fLock.Leave
  end
end;

destructor TCustomSimpleDatabase.Destroy;
begin
  fFields.Free;

  Close (False);
  fLock.Free;
  inherited;
end;

function TCustomSimpleDatabase.Find(rec: TSimpleDatabaseRecord): boolean;
var
  idx : string;
  offset : Integer;
begin
  fLock.Enter;
  try
    result := False;
    if fDataFile = Nil then Exit;
    idx := rec.GetKey;
    if fIndex.Find(idx, offset) then
    begin
      fDataFile.Seek(offset, soFromBeginning);
      rec.LoadFromStream(fDataFile);
      result := True
    end
  finally
    fLock.Leave
  end
end;

procedure TCustomSimpleDatabase.Get(idx: Integer; rec: TSimpleDatabaseRecord);
var
  offset : Integer;
begin
  fLock.Enter;
  try
    if fDataFile = Nil then Exit;
    fIndex.GetKey(idx, offset);
    if offset <> -1 then
    begin
      fDataFile.Seek(offset, soFromBeginning);
      rec.LoadFromStream(fDataFile);
    end
  finally
    fLock.Leave
  end
end;

function TCustomSimpleDatabase.GetRawCount: Integer;
begin
  result := fIndex.RecordCount
end;

function TCustomSimpleDatabase.LockGetCount: Integer;
begin
  fLock.Enter;
  try
    result := RawCount
  except
    fLock.Leave;
    raise
  end
end;

procedure TCustomSimpleDatabase.Open(const Path, Name: string);
var
  needsReindex : boolean;
begin
  Close (True);
  try
    if not FileExists (Path + Name + '.dat') then
    begin
      DeleteFile (Path + Name + '.idx');
      fDataFile := TFileStream.Create(Path + Name + '.dat', fmCreate);
      FreeAndNil (fDataFile);
    end;

    fDataFile := TFileStream.Create(Path + Name + '.dat', fmOpenReadWrite or fmShareDenyWrite);
    needsReindex := not FileExists (Path + Name + '.idx');
    fIndex := TBTree.Create(Path + Name + '.idx');
    fIndex.Duplicates := dupError;
    if needsReindex then
      Reindex
  finally
    fLock.Leave
  end
end;

procedure TCustomSimpleDatabase.Reindex;
begin
  fLock.Enter;
  try
    if fDataFile = Nil then Exit;
  finally
    fLock.Leave
  end
end;

procedure TCustomSimpleDatabase.Replace(rec: TSimpleDatabaseRecord);
var
  idx : string;
  offset, newOffset : Integer;
begin
  fLock.Enter;
  try
    if fDataFile = Nil then Exit;
    idx := rec.GetKey;
    if fIndex.Find(idx, offset) then
    begin
      newOffset := fDataFile.Seek(0, soFromEnd);
      rec.SaveToStream(fDataFile);

      fIndex.DataRec [idx] := newOffset;

      fDataFile.Seek(offset, soFromBeginning);
      fDataFile.Write('XnDbZ' [1], 5);
    end
    else
      raise ESimpleDatabase.Create(rstKeyNotFound);
  finally
    fLock.Leave
  end
end;

procedure TCustomSimpleDatabase.Unlock;
begin
  fLock.Leave
end;

{ TSimpleDatabaseField }

{ TSimpleDatabaseFields }

function TSimpleDatabaseFields.GetFields(index: Integer): TSimpleDatabaseField;
begin
  result := TSimpleDatabaseField (Items [index]);
end;

{ TSimpleDatabaseField }

{ TSimpleDatabaseRecord }

constructor TSimpleDatabaseRecord.Create(AOwner: TCustomSimpleDatabase);
var
  size : Integer;
begin
  fOwner := AOwner;
  size := GetInternalFieldSize;
  GetMem (fData, size);
  FillChar (fData^, size, 0);
end;

destructor TSimpleDatabaseRecord.Destroy;
var
  i, offset : Integer;
  f : TSimpleDatabaseField;
  p : PInteger;
begin
  offset := 0;
  for i := 0 to Owner.Fields.Count - 1 do
  begin
    f := Owner.Fields.Fields [i];
    if f.FieldType = sdfString then
    begin
      p := PInteger (Integer (fData) + offset);
      Inc (p);
      FreeMem (PChar (p^));
    end;

    Inc (offset, f.GetInternalSize);
  end;
    
  FreeMem (fData);

  inherited;
end;

function TSimpleDatabaseRecord.GetFieldAsBoolean(const field: string): Boolean;
var
  tp : TSimpleDatabaseFieldType;
  p : PBoolean;
begin
  p := PBoolean(GetFieldData (field, tp));

  if tp <> sdfBoolean then
    raise ESimpleDatabase.Create(rstTypeMismatch);

  result := p^
end;

function TSimpleDatabaseRecord.GetFieldAsInteger(const field: string): Integer;
var
  tp : TSimpleDatabaseFieldType;
  p : PInteger;
begin
  p := PInteger (GetFieldData (field, tp));

  if tp <> sdfInteger then
    raise ESimpleDatabase.Create(rstTypeMismatch);

  result := p^
end;

function TSimpleDatabaseRecord.GetFieldAsString(const field: string): String;
var
  tp : TSimpleDatabaseFieldType;
  p : PInteger;
  len : Integer;
begin
  p := PInteger (GetFieldData (field, tp));

  case tp of
    sdfInteger : result := IntToStr (p^);
    sdfBoolean : result := BoolToStr (PBoolean (p)^);
    sdfString :
      begin
        len := p^;
        SetLength (result, len);
        if p^ > 0 then
        begin
          Inc (p);
          Move (PChar (p^)^, result [1], len);
        end
      end
  end
end;

function TSimpleDatabaseRecord.GetFieldData(const field: string; var tp : TSimpleDatabaseFieldType): pointer;
var
  i, offset : Integer;
  f : TSimpleDatabaseField;
begin
  offset := 0;
  result := Nil;
  for i := 0 to Owner.Fields.Count - 1 do
  begin
    f := Owner.Fields.Fields [i];
    if SameText (f.Name, field) then
    begin
      result := Pointer (Integer (fData) + offset);
      tp := f.FieldType;
      break
    end
    else
      Inc (Offset, f.GetInternalSize);
  end;
  if result = Nil then
    Raise ESimpleDatabase.CreateFmt (rstFieldNotFound, [field]);
end;

function TSimpleDatabaseRecord.GetInternalFieldSize: Integer;
var
  i : Integer;
begin
  result := 0;
  for i := 0 to Owner.Fields.Count - 1 do
    Inc (result, Owner.Fields.Fields [i].GetInternalSize)
end;

function TSimpleDatabaseRecord.GetKey : string;
var
  tp : TSimpleDatabaseFieldType;
  p : PInteger;
  len : Integer;
begin
  p := PInteger (GetFieldData (Owner.fIndexField, tp));

  case tp of
    sdfInteger : result := IntToHex (p^, 8);
    sdfBoolean : if PBoolean (p)^ then
                   result := 'T'
                 else
                   result := 'F';
    sdfString   :
      begin
        len := p^;
        SetLength (result, len);
        if p^ > 0 then
        begin
          Inc (p);
          Move (PChar (p^)^, result [1], len);
        end
      end
  end
end;

procedure TSimpleDatabaseRecord.LoadFromStream(stream: TStream);
var
  mkr : string;
  i, offset, len : Integer;
  f : TSimpleDatabaseField;
  p : PInteger;
  strdata : PChar;
begin
  SetLength (mkr, 5);
  stream.Read(mkr [1], 5);
  if Copy (mkr, 1, 4) = 'XnDb' then
  begin
    offset := 0;
    for i := 0 to Owner.Fields.Count - 1 do
    begin
      f := owner.Fields.Fields [i];
      p := PInteger (Integer (fData) + Offset);
      case f.FieldType of
        sdfInteger : Stream.Read(p^, sizeof (Integer));
        sdfBoolean : Stream.Read(PBoolean (p)^, sizeof (Boolean));
        sdfString  :
          begin
            Stream.Read(len, sizeof (len));
            p^ := len;
            Inc (p);
            GetMem (strData, len);
            p^ := Integer (strData);
            stream.Read(strData^, len);
          end;
      end;
      Inc (Offset, f.GetInternalSize);
    end;
  end
end;

procedure TSimpleDatabaseRecord.SaveToStream(stream: TStream);
var
  i, offset, len : Integer;
  f : TSimpleDatabaseField;
  p : PInteger;
  strdata : PChar;
begin
  stream.Write('XnDb0' [1], 5);

  offset := 0;
  for i := 0 to Owner.Fields.Count - 1 do
  begin
    f := owner.Fields.Fields [i];
    p := PInteger (Integer (fData) + Offset);
    case f.FieldType of
      sdfInteger : Stream.Write(p^, sizeof (Integer));
      sdfBoolean : Stream.Write(PBoolean (p)^, sizeof (Boolean));
      sdfString  :
        begin
          len := p^;
          Inc (p);
          strdata := PChar (p^);
          stream.Write(len, sizeof (len));
          stream.Write(strdata^, len);
        end;
    end;
    Inc (Offset, f.GetInternalSize);
  end;
end;

procedure TSimpleDatabaseRecord.SetFieldAsBoolean(const field: string;
  const Value: Boolean);
var
  tp : TSimpleDatabaseFieldType;
  p : PBoolean;
begin
  p := PBoolean(GetFieldData (field, tp));

  if tp <> sdfBoolean then
    raise ESimpleDatabase.Create(rstTypeMismatch);

  p^ := Value
end;

procedure TSimpleDatabaseRecord.SetFieldAsInteger(const field: string;
  const Value: Integer);
var
  tp : TSimpleDatabaseFieldType;
  p : PInteger;
begin
  p := PInteger (GetFieldData (field, tp));

  if tp <> sdfInteger then
    raise ESimpleDatabase.Create(rstTypeMismatch);

  p^ := Value
end;

procedure TSimpleDatabaseRecord.SetFieldAsString(const field, Value: String);
var
  tp : TSimpleDatabaseFieldType;
  p : PInteger;
  len : Integer;
  pc : PChar;
begin
  p := PInteger (GetFieldData (field, tp));

  if tp <> sdfString then
    raise ESimpleDatabase.Create(rstTypeMismatch);

  len := Length (Value);
  p^ := len;
  pc := Nil;
  Inc (p);
  ReallocMem (pc, len);
  p^ := Integer (pc);
  Move (Value [1], pc^, len);
end;

{ TSimpleDatabaseField }

function TSimpleDatabaseField.GetInternalSize: Integer;
begin
  result := 0;
  case FieldType of
    sdfInteger : result := SizeOf (Integer);
    sdfBoolean : result := SizeOf (Boolean);
    sdfString  : result := SizeOf (Integer) + SizeOf (pointer);
    else
      Assert (False, 'Invalid field type');
  end
end;

end.
