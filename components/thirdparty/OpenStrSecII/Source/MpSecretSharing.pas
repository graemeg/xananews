{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPSecretSharing Unit                              }
{     Implementation of Shamir Secret Sharing           }
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
unit MpSecretSharing;

interface

uses
  Classes, MpArithTypes;

type
  IMpIntegerCollectionItem = interface
  ['{0748E2AE-55B6-49FB-9608-A58D2B05196C}']
    function GetData: PMPInteger;
    function GetId: Integer;        
    function GetLargeId: PMPInteger;
    procedure SetData(const Value: PMPInteger);
    procedure SetId(const Value: Integer);        
    procedure SetLargeId(const Value: PMPInteger);
    procedure Delete;
    procedure GetDataExp(AGenerator, AFieldModulus: PMPInteger; var AData: PMPInteger);
    property Data: PMPInteger read GetData write SetData;
    property Id: Integer read GetId write SetId;
    property LargeId: PMPInteger read GetLargeId write SetLargeId;
  end;

  TMpIntegerCollection = class;

  TMpIntegerCollectionItem = class(TInterfacedObject,IMpIntegerCollectionItem)
  private
    FCollection: TMpIntegerCollection;
    FData: PMPInteger;
    FLargeId: PMPInteger;
    function GetData: PMPInteger;
    function GetId: Integer;
    function GetLargeId: PMPInteger;
    procedure SetData(const Value: PMPInteger);
    procedure SetId(const Value: Integer);
    procedure SetLargeId(const Value: PMPInteger);
  public
    constructor Create(ACollection: TMpIntegerCollection);
    destructor Destroy; override;
    procedure Delete;
    procedure GetDataExp(AGenerator, AFieldModulus: PMPInteger; var AData: PMPInteger);
    property Data: PMPInteger read GetData write SetData;
    property Id: Integer read GetId write SetId;                  
    property LargeId: PMPInteger read GetLargeId write SetLargeId;
  end;

  IMpIntegerCollection = interface
  ['{8FBAD902-B01B-45C6-BBAF-8FDD6BEA4455}']
    function GetFieldModulus: PMPInteger;
    function GetGroupModulus: PMPInteger;
    function GetItemCount: Integer;
    function GetItems(Index: Integer): IMpIntegerCollectionItem;
    procedure FreeItems;
    function Add: IMpIntegerCollectionItem;
    procedure Assign(ASource: IMpIntegerCollection);
    procedure Clear;
    procedure SetFieldModulus(AValue: PMPInteger);
    procedure SetGroupModulus(AValue: PMPInteger);
    property FieldModulus: PMPInteger read GetFieldModulus;
    property GroupModulus: PMPInteger read GetGroupModulus;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: IMpIntegerCollectionItem read GetItems;
  end;

  TMpIntegerCollection = class(TInterfacedObject,IMpIntegerCollection)
  private
    FItems: TInterfaceList;
    FFieldModulus: PMPInteger;
    FGroupModulus: PMPInteger;
    function GetFieldModulus: PMPInteger;
    function GetGroupModulus: PMPInteger;
    function GetItemCount: Integer;
    function GetItems(Index: Integer): IMpIntegerCollectionItem;
  protected
    procedure FreeItems;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: IMpIntegerCollectionItem;
    procedure Assign(ASource: IMpIntegerCollection); virtual;
    procedure Clear; virtual;
    procedure SetFieldModulus(AValue: PMPInteger);
    procedure SetGroupModulus(AValue: PMPInteger);
    property FieldModulus: PMPInteger read GetFieldModulus;
    property GroupModulus: PMPInteger read GetGroupModulus;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: IMpIntegerCollectionItem read GetItems;
  end;

  IMpPolynomial = interface(IMpIntegerCollection)
  ['{F3CB10E9-C9EA-4088-A3B0-F537390791BD}']
    procedure AssignExp(AGenerator: PMPInteger; ASourcePolynomial: IMpPolynomial);
    procedure AssignExpTo(AGenerator: PMPInteger; APolynomialExp: IMpPolynomial);
    procedure CreatePolynomial(ADegree: Integer;
                               ATerm0, AFieldModulus: PMPInteger);
    procedure Evaluate(Index: Integer; var AValue: PMPInteger); overload;
    procedure Evaluate(Index: PMPInteger; var AValue: PMPInteger); overload;
    procedure EvaluateExp(Index: Integer; var AValue: PMPInteger); overload;
    procedure EvaluateExp(Index: PMPInteger; var AValue: PMPInteger); overload;
  end;

  TMpPolynomial = class(TMpIntegerCollection,IMpPolynomial)
  public
    procedure AssignExp(AGenerator: PMPInteger; ASourcePolynomial: IMpPolynomial);
    procedure AssignExpTo(AGenerator: PMPInteger; APolynomialExp: IMpPolynomial);
    procedure CreatePolynomial(ADegree: Integer;
                               ATerm0, AFieldModulus: PMPInteger);
    procedure Evaluate(Index: Integer; var AValue: PMPInteger); overload;
    procedure Evaluate(Index: PMPInteger; var AValue: PMPInteger); overload;
    procedure EvaluateExp(Index: Integer; var AValue: PMPInteger); overload;
    procedure EvaluateExp(Index: PMPInteger; var AValue: PMPInteger); overload;
  end;

  IMpSecretSharing = interface(IMpIntegerCollection)
  ['{562B426F-4416-4158-9B73-6D7ED979BF60}']
    function GetThreshold: Integer;
    procedure SetThreshold(const Value: Integer);
    { Call CreateSharing to create a Shamir secret sharing of ASecret.
      The Items array will be filled with the AShareCount shares. Each
      share will be Items[i].Data := P(Items[i].Id), where P is a random
      polynomial of degree AThreshold in the field defined by AFieldModulus.
      To recreate ASecret one will need at least AThreshold + 1 shares. }
    function CreateSharing(AShareCount, AThreshold: Integer;
                           ASecret, AFieldModulus: PMPInteger;
                           AGenerator: PMPInteger = nil;
                           APolynomialExp: IMpPolynomial = nil;
                           AIds: IMpIntegerCollection = nil): Boolean;
    { Call CreateZeroSharing to create a Shamir sharing of the value zero.
      Zero sharings are used in different protocols for masking actual shares.
      If <a1,..,an> is a sharing of a and <z1,..,zn> is a sharing of zero,
      then <a1+z1,..,an+zn> will be another sharing of a. }
    function CreateZeroSharing(AShareCount, AThreshold: Integer;
                               AFieldModulus: PMPInteger;
                               AGenerator: PMPInteger = nil;
                               APolynomialExp: IMpPolynomial = nil;
                               AIds: IMpIntegerCollection = nil): Boolean;
    procedure DeleteId(AId: Integer);
    procedure DeleteIndex(AIndex: Integer);
    function FindId(AId: Integer): Integer;
    { Call RecreateSecret to recreate a shared secret from at least
      AThreshold + 1 shares present in the Items array. }
    function RecreateSecret(var ASecret: PMPInteger): Boolean;
    { Call RecreateSecretExp to recreate the discrete exponential value of a
      shared secret from at least AThreshold+1 expontential shares present in
      the Items array. If <a1,..,an> is a sharing of a in the field
      AGroupModulus, and Items contains the values <g^a1,..,g^an> in the field
      AFieldModulus, then g^a mod AFieldModulus will be returned as ASecret. }
    function RecreateSecretExp(var ASecret: PMPInteger): Boolean;
    property Threshold: Integer read GetThreshold write SetThreshold;
  end;

  TMpSecretSharing = class(TMpIntegerCollection,IMpSecretSharing)
  private
    FThreshold: Integer;
    function GetThreshold: Integer;
    procedure SetThreshold(const Value: Integer);
  public
    procedure Assign(ASource: IMpIntegerCollection); override;
    function CreateSharing(AShareCount, AThreshold: Integer;
                           ASecret, AFieldModulus: PMPInteger;
                           AGenerator: PMPInteger = nil;
                           APolynomialExp: IMpPolynomial = nil;
                           AIds: IMpIntegerCollection = nil): Boolean;
    function CreateZeroSharing(AShareCount, AThreshold: Integer;
                               AFieldModulus: PMPInteger;
                               AGenerator: PMPInteger = nil;
                               APolynomialExp: IMpPolynomial = nil;
                               AIds: IMpIntegerCollection = nil): Boolean;
    procedure DeleteId(AId: Integer);
    procedure DeleteIndex(AIndex: Integer);
    function FindId(AId: Integer): Integer;
    function RecreateSecret(var ASecret: PMPInteger): Boolean;
    function RecreateSecretExp(var ASecret: PMPInteger): Boolean;
    property Threshold: Integer read GetThreshold write SetThreshold;
  end;



implementation

uses
{$IFNDEF D5UP}
  MpX509,
{$ENDIF}
  SysUtils, MpArith, MpYarrow, MpKaratsuba;

{ TMpIntegerCollectionItem }

constructor TMpIntegerCollectionItem.Create(
  ACollection: TMpIntegerCollection);
begin
  FCollection := ACollection;
  if Assigned(ACollection) then
    ACollection.FItems.Add(Self);
end;

procedure TMpIntegerCollectionItem.Delete;
begin
  if Assigned(FCollection) then
    FCollection.FItems.Remove(Self);
  FCollection := nil;
end;

destructor TMpIntegerCollectionItem.Destroy;
begin
  MpDealloc(FData);
  FData := nil;    
  MpDealloc(FLargeId);
  FLargeId := nil;
  Delete;
  FCollection := nil;
  inherited;
end;

function TMpIntegerCollectionItem.GetData: PMPInteger;
begin
  Result := FData;
end;

procedure TMpIntegerCollectionItem.GetDataExp(AGenerator,
  AFieldModulus: PMPInteger; var AData: PMPInteger);
begin
  MpKaratsubaExpMod(AGenerator,Data,AFieldModulus,AData);
end;

function TMpIntegerCollectionItem.GetId: Integer;
begin
  if MPMSB(FLargeId) < 32 then begin
    Result := FLargeId.Data[FLargeId.Size - 1];
    if FLargeId.Sign < 0 then
      Result := -Result;
  end else
    raise Exception.Create('Value too large');
end;

function TMpIntegerCollectionItem.GetLargeId: PMPInteger;
begin
  Result := FLargeId;
end;

procedure TMpIntegerCollectionItem.SetData(const Value: PMPInteger);
begin
  MpCopy2(Value,FData);
end;

procedure TMpIntegerCollectionItem.SetId(const Value: Integer);
begin
  MpDealloc(FLargeId);
  FLargeId := IntToMpInt(Value);
end;

procedure TMpIntegerCollectionItem.SetLargeId(const Value: PMPInteger);
begin
  MpCopy2(Value,FLargeId);
end;

{ TMpIntegerCollection }

function TMpIntegerCollection.Add: IMpIntegerCollectionItem;
begin
  Result := TMpIntegerCollectionItem.Create(Self);
end;

procedure TMpIntegerCollection.Assign(ASource: IMpIntegerCollection);
var
  I: Integer;
begin
  if not Assigned(ASource.FieldModulus) then
    raise Exception.Create('Cannot assign a TMpIntegerCollection without a FieldModulus');
  Clear;
  MpCopy2(ASource.FieldModulus,FFieldModulus);
  if Assigned(ASource.GroupModulus) then
    MpCopy2(ASource.GroupModulus,FGroupModulus);
  for I := 0 to ASource.ItemCount - 1 do
    with TMpIntegerCollectionItem.Create(Self) do begin
      MpCopy2(ASource.Items[I].Data,FData);
      SetLargeId(ASource.Items[I].LargeId);
    end;
end;

procedure TMpIntegerCollection.Clear;
begin
  FreeItems;
  MpDealloc(FFieldModulus);
  FFieldModulus := nil;
  MpDealloc(FGroupModulus);
  FGroupModulus := nil;
end;

constructor TMpIntegerCollection.Create;
begin
  FItems := TInterfaceList.Create;
end;

destructor TMpIntegerCollection.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

procedure TMpIntegerCollection.FreeItems;
var
  I: Integer;
begin
  for I := ItemCount - 1 downto 0 do
    Items[I].Delete;
  FItems.Clear;
end;

function TMpIntegerCollection.GetFieldModulus: PMPInteger;
begin
  Result := FFieldModulus;
end;

function TMpIntegerCollection.GetGroupModulus: PMPInteger;
begin
  Result := FGroupModulus;
end;

function TMpIntegerCollection.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TMpIntegerCollection.GetItems(Index: Integer): IMpIntegerCollectionItem;
begin
  Result := nil;
  Supports(FItems[Index],IMpIntegerCollectionItem,Result);
end;

procedure TMpIntegerCollection.SetFieldModulus(AValue: PMPInteger);
begin
  MpCopy2(AValue,FFieldModulus);
  if FGroupModulus = nil then
    MpDec2(AValue,FGroupModulus);
end;

procedure TMpIntegerCollection.SetGroupModulus(AValue: PMPInteger);
begin
  MpCopy2(AValue,FGroupModulus);
end;

{ TMpSecretSharing }

procedure TMpSecretSharing.Assign(ASource: IMpIntegerCollection);
var
  Intf: IMpSecretSharing;
begin
  Intf := ASource as IMpSecretSharing;
  inherited Assign(Intf);
  FThreshold := Intf.Threshold;
end;

function TMpSecretSharing.CreateSharing(AShareCount, AThreshold: Integer;
  ASecret, AFieldModulus, AGenerator: PMPInteger;
  APolynomialExp: IMpPolynomial; AIds: IMpIntegerCollection): Boolean;
var
  I:Integer;
  Item: TMpIntegerCollectionItem;
  P: IMpPolynomial;
begin
  Result := AShareCount > AThreshold;
  Assert(Result,
    'The number of shares must be greater than the degree of the polynomial.');
  Result := (AIds = nil) or (AIds.ItemCount = AShareCount);
  Assert(Result,
    'The number of shares must be identical to the number of Ids.');
  FreeItems;
  if Result then begin
    P := TMpPolynomial.Create;
    try
      P.CreatePolynomial(AThreshold,ASecret,AFieldModulus);
      for I := 1 to AShareCount do begin
        Item := TMpIntegerCollectionItem.Create(Self);
        if Assigned(AIds) then begin
          P.Evaluate(AIds.Items[I-1].LargeId,Item.FData);
          Item.LargeId := AIds.Items[I-1].LargeId;
        end else begin
          P.Evaluate(I,Item.FData);
          Item.Id := I;
        end;
      end;
      if Assigned(AGenerator) and Assigned(APolynomialExp) then
        P.AssignExpTo(AGenerator,APolynomialExp);
    finally
      P := nil;
    end;
    MpCopy2(AFieldModulus,FFieldModulus);
    FThreshold := AThreshold;
  end;
end;

function TMpSecretSharing.CreateZeroSharing(AShareCount,
  AThreshold: Integer; AFieldModulus, AGenerator: PMPInteger;
  APolynomialExp: IMpPolynomial; AIds: IMpIntegerCollection): Boolean;
var
  Z: PMPInteger;
begin
  Z := IntToMPInt(0);
  try
    Result := CreateSharing(AShareCount,AThreshold,Z,AFieldModulus,AGenerator,APolynomialExp,AIds);
  finally
    MpDealloc(Z);
  end;
end;

procedure TMpSecretSharing.DeleteId(AId: Integer);
var
  I: Integer;
begin
  I := FindId(AId);
  if I >= 0 then
    FItems.Delete(I);
end;

procedure TMpSecretSharing.DeleteIndex(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

function TMpSecretSharing.FindId(AId: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := 0;
  while (I < ItemCount) and (Result < 0) do begin
    if AId = Items[I].Id then
      Result := I;
    Inc(I);
  end;
end;

function TMpSecretSharing.GetThreshold: Integer;
begin
  Result := FThreshold;
end;

function TMpSecretSharing.RecreateSecret(var ASecret: PMPInteger): Boolean;
var
  I, J, Count: Integer;
  K, D, S: PMPInteger;
  Item0, Item1: IMpIntegerCollectionItem;
begin
  Count := Threshold + 1;
  Assert(Assigned(FFieldModulus));
  Result := Count <= ItemCount;
  Assert(Result,'Not enough shares to recreate value');
  MPDealloc(ASecret);
  ASecret := IntToMPInt(0);
  if Result then begin
    for I := 0 to Count - 1 do begin
      Item0 := Items[I];
      D := IntToMPInt(1);
      K := IntToMPInt(1);
      S := nil;
      try
        for J := 0 to Count - 1 do begin
          if I <> J then begin
            Item1 := Items[J];
            MPSub2(Item0.LargeId,Item1.LargeId,S);
            MpMulMod(D,S,FFieldModulus);
            MPSub2(FFieldModulus,Item1.LargeId,S);
            MpMulMod(K,S,FFieldModulus);
          end;
        end;
        MpDivMod(K,D,FFieldModulus);
        MpMulMod(K,Item0.Data,FFieldModulus);
        MpAdd(ASecret,K);
      finally
        MPDealloc(D);
        MPDealloc(K);
        MPDealloc(S);
      end;
    end;
    MpMod(ASecret,FFieldModulus);
  end;
end;

function TMpSecretSharing.RecreateSecretExp(
  var ASecret: PMPInteger): Boolean;
var
  I, J, Count: Integer;
  K, D, R, S: PMPInteger;
  Item0, Item1: IMpIntegerCollectionItem;
begin
  Count := Threshold + 1;
  Assert(Assigned(FFieldModulus));
  Assert(Assigned(FGroupModulus));
  Result := Count <= ItemCount;
  Assert(Result,'Not enough shares to recreate value');
  MPDealloc(ASecret);
  ASecret := IntToMPInt(1);
  if Result then begin
    for I := 0 to Count - 1 do begin
      Item0 := Items[I];
      D := IntToMPInt(1);
      K := IntToMPInt(1);
      R := nil;
      S := nil;
      try
        for J := 0 to Count - 1 do begin
          if I <> J then begin
            Item1 := Items[J];
            MPSub2(Item0.LargeId,Item1.LargeId,S);
            MpMulMod(D,S,FGroupModulus);
            MPSub2(FGroupModulus,Item1.LargeId,S);
            MPMulMod(K,S,FGroupModulus);
          end;
        end;
        MpDivMod(K,D,FGroupModulus);
        MpKaratsubaExpMod(Item0.Data,K,FFieldModulus,R);
        MpMulMod(ASecret,R,FFieldModulus);
      finally
        MPDealloc(D);
        MPDealloc(K);
        MPDealloc(R);
        MPDealloc(S);
      end;
    end;
  end;
end;

procedure TMpSecretSharing.SetThreshold(const Value: Integer);
begin
  FThreshold := Value;
end;

{ TMpPolynomial }

procedure TMpPolynomial.AssignExp(AGenerator: PMPInteger;
  ASourcePolynomial: IMpPolynomial);
var
  I: Integer;
begin
  if FieldModulus = nil then
    raise Exception.Create('TMpPolynomial.AssignExpTo: No modulus for destination field');
  FreeItems;
  SetGroupModulus(ASourcePolynomial.FieldModulus);
  for I := 0 to ItemCount - 1 do
    with TMpIntegerCollectionItem.Create(Self) do begin
      Items[I].GetDataExp(AGenerator,FieldModulus,FData);
      SetLargeId(Items[I].LargeId);
    end;
end;

procedure TMpPolynomial.AssignExpTo(AGenerator: PMPInteger;
  APolynomialExp: IMpPolynomial);
begin
  APolynomialExp.AssignExp(AGenerator,Self);
end;

procedure TMpPolynomial.CreatePolynomial(ADegree: Integer;
  ATerm0, AFieldModulus: PMPInteger);
var
  I: Integer;
  Item: TMpIntegerCollectionItem;
begin
  FreeItems;
  Item := TMpIntegerCollectionItem.Create(Self);
  Item.FData := MpCopy(ATerm0);
  Item.Id := 0;
  for I := 1 to ADegree do begin
    Item := TMpIntegerCollectionItem.Create(Self);
    MpRandomBound(Item.FData,nil,AFieldModulus);
    Item.Id := I;
  end;
  MpCopy2(AFieldModulus,FFieldModulus);
end;

procedure TMpPolynomial.Evaluate(Index: Integer; var AValue: PMPInteger);
var
  I: Integer;
begin
  MpDealloc(AValue);
  AValue := IntToMpInt(0);
  for I := ItemCount - 1 downto 0 do begin
    MpMulByInt(AValue,Index);
    MpAdd(AValue,Items[I].Data);
  end;
  MpMod(AValue,FFieldModulus);
end;

procedure TMpPolynomial.EvaluateExp(Index: Integer; var AValue: PMPInteger);
var
  I: Integer;
  Idx: PMPInteger;
begin
  MpDealloc(AValue);
  AValue := IntToMpInt(1);
  Idx := IntToMPInt(Index);
  try
    for I := ItemCount - 1 downto 0 do begin
      MpKaratsubaExpMod(AValue,Idx,FFieldModulus,AValue);
      MpMulMod(AValue,Items[I].Data,FFieldModulus);
    end;
  finally
    MPDealloc(Idx);
  end;
end;

procedure TMpPolynomial.Evaluate(Index: PMPInteger;
  var AValue: PMPInteger);
var
  I: Integer;
begin
  MpDealloc(AValue);
  AValue := IntToMpInt(0);
  for I := ItemCount - 1 downto 0 do begin
    MpMulMod(AValue,Index,FFieldModulus);
    MpAdd(AValue,Items[I].Data);
  end;
  MpMod(AValue,FFieldModulus);
end;

procedure TMpPolynomial.EvaluateExp(Index: PMPInteger;
  var AValue: PMPInteger);
var
  I: Integer;
begin
  MpDealloc(AValue);
  AValue := IntToMpInt(1);
  for I := ItemCount - 1 downto 0 do begin
    MpKaratsubaExpMod(AValue,Index,FFieldModulus,AValue);
    MpMulMod(AValue,Items[I].Data,FFieldModulus);
  end;
end;

end.
