{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SecStr Unit                                       }
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
unit SecStr;

interface

uses
  Classes;

type
  TSecureStrings = class;
  TStringListSortCompare = function(List: TSecureStrings; Index1, Index2: Integer): Integer;

  TSecureStrings = class(TStrings)
  private
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
    procedure InsertItem(Index: Integer; const S: string);
    procedure SetSorted(Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

implementation

uses
  Windows, SecUtils, {$IFDEF D6UP}RTLConsts{$ELSE}Consts{$ENDIF}, SysUtils;

{ TSecureStrings }

procedure ClearStrings(List: PStringItemList; Count: Integer);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do begin
    if List^[I].FString <> '' then
      ProtectClear(List^[I].FString[1],Length(List^[I].FString));
    List^[I].FString := '';
  end;
end;

destructor TSecureStrings.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  if FCount <> 0 then ClearStrings(FList, FCount);
  inherited Destroy;
  FCount := 0;
  SetCapacity(0);
end;

function TSecureStrings.Add(const S: string): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        {$IFDEF D5UP}
        dupError: Error(@SDuplicateString, 0);
        {$ELSE}
        dupError: Error(SDuplicateString, 0);
        {$ENDIF}
      end;
  InsertItem(Result, S);
end;

procedure TSecureStrings.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSecureStrings.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TSecureStrings.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    ClearStrings(FList, FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TSecureStrings.Delete(Index: Integer);
begin
  {$IFDEF D5UP}
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  {$ELSE}
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  {$ENDIF}
  Changing;
  ClearStrings(@FList^[Index],1);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TStringItem));
  Changed;
end;

procedure TSecureStrings.Exchange(Index1, Index2: Integer);
begin
  {$IFDEF D5UP}
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  {$ELSE}
  if (Index1 < 0) or (Index1 >= FCount) then Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(SListIndexError, Index2);
  {$ENDIF}
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TSecureStrings.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TSecureStrings.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := AnsiCompareText(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TSecureStrings.Get(Index: Integer): string;
begin
  {$IFDEF D5UP}
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  {$ELSE}
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  {$ENDIF}
  Result := FList^[Index].FString;
end;

function TSecureStrings.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TSecureStrings.GetCount: Integer;
begin
  Result := FCount;
end;

function TSecureStrings.GetObject(Index: Integer): TObject;
begin
  {$IFDEF D5UP}
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  {$ELSE}
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  {$ENDIF}
  Result := FList^[Index].FObject;
end;

procedure TSecureStrings.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TSecureStrings.IndexOf(const S: string): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TSecureStrings.Insert(Index: Integer; const S: string);
begin
  {$IFDEF D5UP}
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  {$ELSE}
  if Sorted then Error(SSortedListError, 0);                             
  if (Index < 0) or (Index > FCount) then Error(SListIndexError, Index);
  {$ENDIF}
  InsertItem(Index, S);
end;

procedure TSecureStrings.InsertItem(Index: Integer; const S: string);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TSecureStrings.Put(Index: Integer; const S: string);
begin
  {$IFDEF D5UP}
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  {$ELSE}
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  {$ENDIF}
  Changing;
  ProtectClear(FList^[Index].FString[1], Length(FList^[Index].FString));
  FList^[Index].FString := S;
  Changed;
end;

procedure TSecureStrings.PutObject(Index: Integer; AObject: TObject);
begin
  {$IFDEF D5UP}
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  {$ELSE}
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);             
  {$ENDIF}
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TSecureStrings.QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TSecureStrings.SetCapacity(NewCapacity: Integer);
var
  NewList: PStringItemList;
begin
  if NewCapacity > 0 then
    GetMem(NewList, NewCapacity * SizeOf(TStringItem))
  else
    NewList := nil;
  if FCapacity > 0 then begin
    if NewCapacity > 0 then
      System.Move(FList^, NewList^, FCapacity * SizeOf(TStringItem));
    ProtectClear(FList^, FCapacity * SizeOf(TStringItem));
    FreeMem(FList);
  end;
  FList := NewList;
  FCapacity := NewCapacity;
end;

procedure TSecureStrings.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TSecureStrings.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListAnsiCompare(List: TSecureStrings; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(List.FList^[Index1].FString,
                            List.FList^[Index2].FString);
end;

procedure TSecureStrings.Sort;
begin
  CustomSort(StringListAnsiCompare);
end;

procedure TSecureStrings.CustomSort(Compare: TStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

end.
