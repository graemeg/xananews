(*======================================================================*
 | unitObjectCache                                                      |
 |                                                                      |
 | Object caching & association classes:                                |
 |                                                                      |
 | TObjectCache                 Implements a flexible cache of objects  |
 | TClassAssociations           Associates pairs of classes             |
 | TClassStringAssociations     Associates a string/class pairs         |
 | TObjectProcessor             Process a list of objects in a          |
 |                              background thread.                      |
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
 | Copyright © Colin Wilson 2003  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      10/12/2003  CPWW  Original                                  |
 *======================================================================*)


unit unitObjectCache;

interface

uses Windows, Classes, SysUtils, ConTnrs, SyncObjs;

type
  TObjectCacheProc = procedure(obj: TObject; idx, param: Integer; var continue: Boolean) of object;

  TObjectCache = class;

  TObjectCacheEnumerator = class
  private
    fObjectCache: TObjectCache;
    fIdx: Integer;
  public
    constructor Create(AObjectCache: TObjectCache);
    function MoveNext: Boolean;
    function GetCurrent: TObject;
    property Current: TObject read GetCurrent;
  end;

  //---------------------------------------------------------------
  TObjectCache = class(TObjectList)
  private
    FMaxDepth: Integer;
    FReOrdering: Boolean;
    function GetMaxDepth: Integer;
    procedure SetMaxDepth(const Value: Integer);
    function IndexOf(AObject: TObject): Integer;
  protected
    function CanRemove(AObject: TObject): Boolean; virtual;
    function Matches(ObjA, ObjB: TObject): Boolean; virtual;
  public
    constructor Create(ACapacity: Integer; OwnsObjects: Boolean); overload;
    destructor Destroy; override;
    procedure Add(AObject: TObject);
    procedure BringToFront(idx: Integer);
    procedure Clear; override;
    function Extract(AObject: TObject): TObject;
    function ForEach(proc: TObjectCacheProc; param: Integer): TObject;
    function ForEachIdx(proc: TObjectCacheProc; param: Integer): Integer;
    function GetEnumerator: TObjectCacheEnumerator;
    function Remove(AObject: TObject): Integer;

    property MaxDepth: Integer read GetMaxDepth write SetMaxDepth;
    property ReOrdering: Boolean read FReOrdering;
  end;

  //---------------------------------------------------------------
  TClassAssociation = class
  private
    fClassA, fClassB: TClass;
  public
    constructor Create(AClassA, AClassB: TClass);
    property ClassA: TClass read fClassA;
    property ClassB: TClass read fClassB;
  end;

  //---------------------------------------------------------------
  TClassAssociations = class
  private
    fAssociations: TObjectList;
    function GetCount: Integer;
    function GetAssociation(idx: Integer): TClassAssociation;
    function GetIndexOf(classA, classB: TClass): Integer;
    function GetIndexOfClassA(classA: TClass): Integer;
    function GetIndexOfClassB(classB: TClass): Integer;
  protected
    property Association[idx: Integer]: TClassAssociation read GetAssociation;
    property Count: Integer read GetCount;
    property IndexOf[classA, classB: TClass]: Integer read GetIndexOf;
    property IndexOfClassA[classA: TClass]: Integer read GetIndexOfClassA;
    property IndexOfClassB[classB: TClass]: Integer read GetIndexOfClassB;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Associate(classA, classB: TClass);
    procedure DisAssociate(classA, classB: TClass);

    function FindClassBFor(classA: TClass): TClass;
    function FindClassAFor(classB: TClass): TClass;
  end;

  //---------------------------------------------------------------
  TClassStringAssociations = class
  private
    fAssociations: TStringList;
    function GetIndexOf(const st: string; cls: TClass): Integer;
    function GetCount: Integer;
    function GetString(idx: Integer): string;
    function GetClass(idx: Integer): TClass;
  protected
    property IndexOf[const st: string; cls: TClass]: Integer read GetIndexOf;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Associate(const st: string; cls: TClass);
    procedure DisAssociate(const st: string; cls: TClass);

    function FindStringFor(cls: TClass): string;
    function FindClassFor(const st: string): TClass;

    property Count: Integer read GetCount;
    property Strings[idx: Integer]: string read GetString;
    property Classes[idx: Integer]: TClass read GetClass;
  end;

  TObjectProcessorState = (opsIdle, opsBusy);
  //---------------------------------------------------------------
  TObjectProcessor = class(TThread)
  private
    fSync: TCriticalSection;
    fSignal: TEvent;
    fObjects: TObjectList;
    fState: TObjectProcessorState;
    procedure SetOwnsObjects(const Value: Boolean);
    function GetOwnsObjects: Boolean;
    function GetCount: Integer;
  protected
    procedure Execute; override;

    procedure Reset(obj: TObject); virtual;
    procedure Process(obj: TObject); virtual;
    procedure ObjectsProcessed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Terminate;
    procedure AddObjectToQueue(obj: TObject);

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
    property State: TObjectProcessorState read fState;
    property Count: Integer read GetCount;
  end;

  TLog = class
  private
    fLock: TCriticalSection;
    fStrings: TStrings;
    fLocked: Boolean;
    fInLock: Boolean;
    fCapacity: Integer;
    procedure Init;
    procedure Lock;
    procedure LimitCapacity;
    function GetStrings(idx: Integer): string;
    procedure SetCapacity(const Value: Integer);
  public
    destructor Destroy; override;
    function LockGetCount: Integer;
    procedure Add(const st: string);
    procedure Clear;
    procedure Unlock;

    property Capacity: Integer read fCapacity write SetCapacity;
    property Strings[idx: Integer]: string read GetStrings;
  end;

implementation

{ TObjectCache }

(*----------------------------------------------------------------------*
 | TObjectCache.Add                                                     |
 |                                                                      |
 | Add an object to the cache.                                          |
 |                                                                      |
 | Note that the cache capacity will automatically be increased if      |
 | it is full, and no objects can be removed (see the CanRemove method) |
 |                                                                      |
 | Parameters:                                                          |
 |   AObject: TObject           The object to add                       |
 *----------------------------------------------------------------------*)
procedure TObjectCache.Add(AObject: TObject);
var
  idx, c: Integer;
  b: Boolean;
begin
  idx := IndexOf(AObject);
  if idx = 0 then       // Already in the cache at the front
    Exit;

  if idx = -1 then
  begin                 // Not already in cache.  Add it.
    b := False;
    c := Count;
    while c >= FMaxDepth do
    begin               // There's not room.  Remove old objects (if we're allowed)
      repeat            // Try to get back to the original capacity if it's been
        Dec(c);         // exceeded.
        if CanRemove(Items[c]) then
        begin
          Delete(C);
          b := True
        end
      until b or (c = 0);
    end;

    if b then   // Shrink the cache if it's bulged.
      if Capacity > FMaxDepth then
        if Count < FMaxDepth then
            Capacity := FMaxDepth;

    if Capacity = Count then // Bulge the cache
      Capacity := Capacity + 1;

    Insert(0, AObject);
  end
  else                  // The object was already in the cache.  So bring it to the front
    BringToFront(idx);
end;

(*----------------------------------------------------------------------*
 | procedure TObjectCache.BringToFront                                  |
 |                                                                      |
 | Bring object 'idx' to the front of the cache.                        |
 |                                                                      |
 | Parameters:                                                          |
 |   idx: Integer       // Index of the object to bring to the front.   |
 *----------------------------------------------------------------------*)
procedure TObjectCache.BringToFront(idx: Integer);
var
  b: Boolean;
  obj: TObject;
begin
  if idx > 0 then
  begin
    FReOrdering := True;
    obj := Items[idx];
    b := OwnsObjects;
    OwnsObjects := False;       // Temporarily turn off 'owns objects' so we
    try                         // can delete and reinsert safely.
      Delete(idx);
      Insert(0, obj);
    finally
      OwnsObjects := b;
      FReOrdering := False;
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | TObjectCache.CanRemove                                               |
 |                                                                      |
 | Override this to prevent objects from being removed from the cache   |
 | - maybe because another reference to the object still exists.        |
 |                                                                      |
 | Parameters:                                                          |
 |   AObject: TObject           The object to test                      |
 |                                                                      |
 | The function returns True if the object can be safely removed.       |
 *----------------------------------------------------------------------*)
function TObjectCache.CanRemove(AObject: TObject): Boolean;
begin
  Result := True
end;

(*----------------------------------------------------------------------*
 | procedure TObjectCache.Clear                                         |
 |                                                                      |
 | Clear the cache - or as much of it as can safely be cleared.         |
 *----------------------------------------------------------------------*)
procedure TObjectCache.Clear;
var
  I: Integer;
  Temp: TObject;
begin
  I := 0;
  while I < Count do
  begin
    Temp := Items[I];
    if CanRemove(Temp) then
      Delete(I)
    else
      Inc(I);
  end;
end;

constructor TObjectCache.Create(ACapacity: Integer; OwnsObjects: Boolean);
begin
  inherited Create(OwnsObjects);
  FMaxDepth := ACapacity;
  Capacity := ACapacity;
end;

destructor TObjectCache.Destroy;
begin
  SetCount(0);
  SetCapacity(0);
  inherited Destroy;
end;

function TObjectCache.Extract(AObject: TObject): TObject;
begin
  if CanRemove(AObject) then
    Result := inherited Extract(AObject)
  else
    Result := nil;
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.ForEach                                        |
 |                                                                      |
 | Call 'proc' for each object in the cache                             |
 |                                                                      |
 | Parameters:                                                          |
 |   proc: TObjectCacheProc     procedure to call                       |
 |   param: Integer             User parameter to pass to the procedure |
 |                                                                      |
 | The function returns the object that caused 'proc' to return with    |
 | 'continue=False'.  You can use this eg. to search the cache.         |
 *----------------------------------------------------------------------*)
function TObjectCache.ForEach(proc: TObjectCacheProc; param: Integer): TObject;
var
  i: Integer;
  continue: Boolean;
begin
  i := 0;
  continue := True;

  while continue and (i < Count) do
  begin
    proc(Items[i], i, param, continue);
    if continue then
      Inc(i);
  end;

  if not continue then
    Result := Items[i]
  else
    Result := nil;
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.ForEachIdx                                     |
 |                                                                      |
 | Call 'proc' for each object in the cache.  nb. this differs from     |
 | ForEach only in the return value.                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   proc: TObjectCacheProc     procedure to call                       |
 |   param: Integer             User parameter to pass to the procedure |
 |                                                                      |
 | The function returns the index of the object that caused 'proc'      |
 | to return with 'continue=False'.  You can use this eg. to search the |
 | cache.                                                               |
 *----------------------------------------------------------------------*)
function TObjectCache.ForEachIdx(proc: TObjectCacheProc; param: Integer): Integer;
var
  i: Integer;
  continue: Boolean;
begin
  i := 0;
  continue := True;

  while continue and (i < Count) do
  begin
    proc(Items[i], i, param, continue);
    if continue then
      Inc(i);
  end;

  if not continue then
    Result := i
  else
    Result := -1;
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.GetEnumerator                                  |
 |                                                                      |
 | Create and return a TObjecTCacheEnumerator to support                |
 | for..in in Delphi 2005                                               |
 *----------------------------------------------------------------------*)
function TObjectCache.GetEnumerator: TObjectCacheEnumerator;
begin
  Result := TObjectCacheEnumerator.Create(Self);
end;

function TObjectCache.GetMaxDepth: Integer;
begin
  Result := FMaxDepth
end;

function TObjectCache.IndexOf(AObject: TObject): Integer;
var
  i, c: Integer;
begin
  Result := -1;
  c := Count;
  for i := 0 to c - 1 do
    if Matches(Items[i], AObject) then
    begin
      Result := i;
      Break;
    end;
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.Matches                                        |
 |                                                                      |
 | Return 'True' if ObjA matches ObB.  Override this to provide more    |
 | complicated matching of objects.                                     |
 *----------------------------------------------------------------------*)
function TObjectCache.Matches(ObjA, ObjB: TObject): Boolean;
begin
  Result := (ObjA = ObjB);
end;

function TObjectCache.Remove(AObject: TObject): Integer;
begin
  Result := -1;
  if (Count > 0) and CanRemove(AObject) then
  begin
    Result := IndexOf(AObject);
    if Result >= 0 then
      Delete(Result);
  end;
end;

procedure TObjectCache.SetMaxDepth(const Value: Integer);
begin
  if Value <> FMaxDepth then
  begin
    while Count > Value do
      Delete(Count - 1);
    Capacity := Value;
    FMaxDepth := Value
  end;
end;


{ TClassAssociations }

// TClassAssociations allows you to associate a class with another class.
// eg. you could associate a TGraphicsForm with TGraphic, etc.
//
// TClassAssociations support inheritance, so as TIcon derives from
// TGraphic, then TGraphicsForm will be returned for TIcon - unless a
// separate TIconForm is registered for TIcon.

(*----------------------------------------------------------------------*
 | TClassAssociations.Associate                                         |
 |                                                                      |
 | Associate ClassA with ClassB                                         |
 |                                                                      |
 | Parameters:                                                          |
 |   classA, classB: TClass             The classes to associate        |
 *----------------------------------------------------------------------*)
procedure TClassAssociations.Associate(classA, classB: TClass);
var
  i: Integer;
begin
  i := IndexOf[classA, classB];

  if i = -1 then
    fAssociations.Insert(0, TClassAssociation.Create(classA, classB));
end;

(*----------------------------------------------------------------------*
 | constructor TClassAssociations.Create                                 |
 *----------------------------------------------------------------------*)
constructor TClassAssociations.Create;
begin
  fAssociations := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TClassAssociations.Destroy                                |
 *----------------------------------------------------------------------*)
destructor TClassAssociations.Destroy;
begin
  fAssociations.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TClassAssociations.DisAssociate                            |
 |                                                                      |
 | Remove the association between classA and classB                     |
 *----------------------------------------------------------------------*)
procedure TClassAssociations.DisAssociate(classA, classB: TClass);
var
  idx: Integer;
begin
  idx := IndexOf[classA, classB];
  if idx >= 0 then
    fAssociations.Delete(idx);
end;

(*----------------------------------------------------------------------*
 | TClassAssociations.FindClassAFor                                     |
 |                                                                      |
 | eg. FindClassAFor (TGraphicsForm) will return TGraphic.  Note that   |
 | this way round there is no inheritance.  There's either an           |
 | association or there's not.                                          |
 |                                                                      |
 | Parameters:                                                          |
 |   classB: TClass             The ClassB to find.                     |
 |                                                                      |
 | The function returns the classA that matches classB                  |
 *----------------------------------------------------------------------*)
function TClassAssociations.FindClassAFor(classB: TClass): TClass;
var
  idx: Integer;
begin
  idx := IndexOfClassB[classB];
  if idx >= 0 then
    Result := Association[idx].ClassA
  else
    Result := nil
end;

(*----------------------------------------------------------------------*
 | function TClassAssociations.FindClassBFor                            |
 |                                                                      |
 | eg. FindClassAFor (TGraphic) will return TGraphicForm.               |
 |                                                                      |
 | nb. this supports inheritance, so as TIcon derives from TGraphic,    |
 | TGraphicsForm will be returned for TIcon - unless a separate         |
 | TIconForm is registered for TIcon.                                   |
 |                                                                      |
 | Parameters:                                                          |
 |   classA: TClass              The ClassA to find                     |
 |                                                                      |
 | The function returns the classB that matches classA.  If no match is |
 | found, classA's Ancestor classes are searched too.                   |
 *----------------------------------------------------------------------*)
function TClassAssociations.FindClassBFor(classA: TClass): TClass;
var
  idx: Integer;
begin
  idx := IndexOfClassA[classA];
  if idx >= 0 then
    Result := Association[idx].ClassB
  else
    Result := nil
end;

(*----------------------------------------------------------------------*
 | function TClassAssociations.GetAssociation                           |
 |                                                                      |
 | 'Get' method for Association property                                |
 *----------------------------------------------------------------------*)
function TClassAssociations.GetAssociation(
  idx: Integer): TClassAssociation;
begin
  Result := TClassAssociation(fAssociations[idx]);
end;

(*----------------------------------------------------------------------*
 | function TClassAssociations.GetCount                                 |
 |                                                                      |
 | 'Get' method for Count property                                      |
 *----------------------------------------------------------------------*)
function TClassAssociations.GetCount: Integer;
begin
  Result := fAssociations.Count;
end;

function TClassAssociations.GetIndexOf(classA, classB: TClass): Integer;
var
  i: Integer;
  ass: TClassAssociation;
begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    ass := Association[i];
    if (ass.ClassA = classA) and (ass.ClassB = classB) then
    begin
      Result := i;
      break
    end
  end
end;

function TClassAssociations.GetIndexOfClassA(classA: TClass): Integer;
var
  i: Integer;
  ass: TClassAssociation;
begin
  Result := -1;

  while (Result = -1) and Assigned(classA) do
  begin
    for i := 0 to Count - 1 do
    begin
      ass := Association[i];
      if ass.ClassA = classA then
      begin
        Result := i;
        break
      end
    end;

    if Result = -1 then
      classA := classA.ClassParent
  end
end;

function TClassAssociations.GetIndexOfClassB(classB: TClass): Integer;
var
  i: Integer;
  ass: TClassAssociation;
begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    ass := Association[i];
    if ass.ClassB = classB then
    begin
      Result := i;
      break
    end
  end
end;

{ TClassAssociation }

constructor TClassAssociation.Create(AClassA, AClassB: TClass);
begin
  fClassA := AClassA;
  fClassB := AClassB
end;

{ TClassStringAssociations }

procedure TClassStringAssociations.Associate(const st: string; cls: TClass);
var
  i: Integer;
begin
  i := IndexOf[st, cls];

  if i = -1 then
    fAssociations.InsertObject(0, st, TObject(cls))
end;

constructor TClassStringAssociations.Create;
begin
  fAssociations := TStringList.Create;
end;

destructor TClassStringAssociations.Destroy;
begin
  fAssociations.Free;

  inherited;
end;

procedure TClassStringAssociations.DisAssociate(const st: string;
  cls: TClass);
var
  idx: Integer;
begin
  idx := IndexOf[st, cls];
  if idx >= 0 then
    fAssociations.Delete(idx);
end;

function TClassStringAssociations.FindClassFor(const st: string): TClass;
var
  idx: Integer;
begin
  idx := fAssociations.IndexOf(st);
  if idx >= 0 then
    Result := TClass(fAssociations.Objects[idx])
  else
    Result := nil
end;

function TClassStringAssociations.FindStringFor(cls: TClass): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    if TClass(fAssociations.Objects[i]) = cls then
    begin
      Result := fAssociations[i];
      break
    end
end;

function TClassStringAssociations.GetClass(idx: Integer): TClass;
begin
  Result := TClass(fAssociations.Objects[idx])
end;

function TClassStringAssociations.GetCount: Integer;
begin
  Result := fAssociations.Count
end;

function TClassStringAssociations.GetIndexOf(const st: string;
  cls: TClass): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if (fAssociations.Objects[i] = TObject(cls)) and SameText(st, fAssociations[i]) then
    begin
      Result := i;
      break
    end
end;

function TClassStringAssociations.GetString(idx: Integer): string;
begin
  Result := fAssociations[idx];
end;

{ TObjectProcessor }

procedure TObjectProcessor.AddObjectToQueue(obj: TObject);
begin
  fSync.Enter;
  try
    fObjects.Add(obj);
    if fState = opsIdle then
      fSignal.SetEvent;
  finally
    fSync.Leave
  end
end;

procedure TObjectProcessor.Clear;
var
  i: Integer;
begin
  fSync.Enter;
  try
    for i := 0 to fObjects.Count - 1 do
      Reset(fObjects[i]);
    fObjects.Clear;
  finally
    fSync.Leave
  end
end;

constructor TObjectProcessor.Create;
begin
  fSync := TCriticalSection.Create;
  fSignal := TEvent.Create(nil, false, false, '');
  fObjects := TObjectList.Create;
  fObjects.OwnsObjects := False;

  inherited Create(false);
end;

destructor TObjectProcessor.Destroy;
begin
  fSync.Free;
  fSignal.Free;

  inherited;
end;

procedure TObjectProcessor.Execute;
begin
  while not Terminated do
  begin
    try
      if fObjects.Count = 0 then
        fSignal.WaitFor(INFINITE);

      fState := opsBusy;
      try
        while not Terminated and (fObjects.Count > 0) do
        begin
          fSync.Enter;
          try
            if fObjects.Count > 0 then
            begin
              Process(fObjects[0]);
              fObjects.Delete(0)
            end
          finally
            fSync.Leave
          end;
        end;
        if not Terminated then
          ObjectsProcessed;
      finally
        fState := opsIdle
      end
    except
      try
        Clear
      except
      end
    end
  end
end;

function TObjectProcessor.GetCount: Integer;
begin
  Result := fObjects.Count
end;

function TObjectProcessor.GetOwnsObjects: Boolean;
begin
  Result := fObjects.OwnsObjects
end;

procedure TObjectProcessor.ObjectsProcessed;
begin
// Stub - called when a batch of objects has been processed
end;

procedure TObjectProcessor.Process(obj: TObject);
begin
// Stub - called to process each object
end;

procedure TObjectProcessor.Reset(obj: TObject);
begin
// Stub - called when un-processed objects are removed from the queue (by Clear)
end;

procedure TObjectProcessor.SetOwnsObjects(const Value: Boolean);
begin
  fObjects.OwnsObjects := Value
end;

procedure TObjectProcessor.Terminate;
begin
  Clear;
  inherited Terminate;
  fSignal.SetEvent;
  WaitFor
end;

{ TLog }

procedure TLog.Add(const st: string);
begin
  Lock;
  try
    fStrings.Add(st);
    LimitCapacity
  finally
    Unlock
  end
end;

procedure TLog.Clear;
begin
  Lock;
  try
    fStrings.Clear
  finally
    Unlock
  end
end;

destructor TLog.Destroy;
begin
  fLock.Free;
  fStrings.Free;

  inherited;
end;

function TLog.GetStrings(idx: Integer): string;
begin
  if not fLocked then
    raise Exception.Create('Must call LockGetCount');
  Result := fStrings[idx];
end;

procedure TLog.Init;
begin
  if not Assigned(fStrings) then
    fStrings := TStringList.Create;

  if not Assigned(fLock) then
    fLock := TCriticalSection.Create;
end;

procedure TLog.LimitCapacity;
var
  needLock: Boolean;
begin
  if fCapacity < 1 then
    Exit;

  needLock := not fInLock;

  if needLock then
    Lock;

  try
    while fStrings.Count > fCapacity do
      fStrings.Delete(0);
  finally
    if needLock then
      Unlock
  end
end;

procedure TLog.Lock;
begin
  Init;
  fLock.Enter;
  fInLock := True;
end;

function TLog.LockGetCount: Integer;
begin
  Lock;
  fLocked := True;
  Result := fStrings.Count
end;

procedure TLog.SetCapacity(const Value: Integer);
begin
  if Value <> fCapacity then
  begin
    fCapacity := Value;
    LimitCapacity
  end
end;

procedure TLog.Unlock;
begin
  fLock.Leave;
  fLocked := False;
  fInLock := False;
end;

{ TObjectCacheEnumerator }

constructor TObjectCacheEnumerator.Create(AObjectCache: TObjectCache);
begin
  fIdx := -1;
  fObjectCache := AObjectCache;
end;

function TObjectCacheEnumerator.GetCurrent: TObject;
begin
  Result := fObjectCache[fIdx];
end;

function TObjectCacheEnumerator.MoveNext: Boolean;
begin
  Result := fIDx < fObjectCache.Count - 1;
  if Result then
    Inc(fIdx);
end;

end.
