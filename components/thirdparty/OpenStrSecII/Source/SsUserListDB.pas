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
}
unit SsUserListDB;

interface

uses
  SysUtils, Classes, SyncObjs, DB, Pkix_Cert, StreamSecII, SsUserList;

type
  TFieldName = type string;

  TSsDBUserList = class(TSsCustomUserList,IUserList)
  private
    FLock: TCriticalSection;
    FDataLink: TDataLink;
    FGivenNameField: TField;
    FLocalityField: TField;
    FStateOrProvinceField: TField;
    FGenerationQualifierField: TField;
    FOrganizationField: TField;
    FInitialsField: TField;
    FURIField: TField;
    FDnsNameField: TField;
    FRfc822NameField: TField;
    FCommonNameField: TField;
    FDNQualifierField: TField;
    FOrganizationalUnitField: TField;
    FCountryField: TField;
    FSurnameField: TField;
    FIPAddressField: TField;
    FRegTokenField: TField;
    
    FGivenNameFld: string;
    FLocalityFld: string;
    FStateOrProvinceFld: string;
    FGenerationQualifierFld: string;
    FOrganizationFld: string;
    FInitialsFld: string;
    FURIFld: string;
    FDnsNameFld: string;
    FRfc822NameFld: string;
    FCommonNameFld: string;
    FDNQualifierFld: string;
    FOrganizationalUnitFld: string;
    FCountryFld: string;
    FSurnameFld: string;
    FIPAddressFld: string;
    FRegTokenFld: string;
    procedure SetDataSource(const Value: TDataSource);
    function GetDataSource: TDataSource;
    procedure SetCommonNameField(const Value: TFieldName);
    procedure SetCountryField(const Value: TFieldName);
    procedure SetDNQualifierField(const Value: TFieldName);
    procedure SetDnsNameField(const Value: TFieldName);
    procedure SetGenerationQualifierField(const Value: TFieldName);
    procedure SetGivenNameField(const Value: TFieldName);
    procedure SetInitialsField(const Value: TFieldName);
    procedure SetIPAddressField(const Value: TFieldName);
    procedure SetLocalityField(const Value: TFieldName);
    procedure SetOrganizationalUnitField(const Value: TFieldName);
    procedure SetOrganizationField(const Value: TFieldName);
    procedure SetRfc822NameField(const Value: TFieldName);
    procedure SetStateOrProvinceField(const Value: TFieldName);
    procedure SetSurnameField(const Value: TFieldName);
    procedure SetURIField(const Value: TFieldName);
    function GetCommonNameField: TFieldName;
    function GetCountryField: TFieldName;
    function GetDNQualifierField: TFieldName;
    function GetDnsNameField: TFieldName;
    function GetGenerationQualifierField: TFieldName;
    function GetGivenNameField: TFieldName;
    function GetInitialsField: TFieldName;
    function GetIPAddressField: TFieldName;
    function GetLocalityField: TFieldName;
    function GetOrganizationalUnitField: TFieldName;
    function GetOrganizationField: TFieldName;
    function GetRfc822NameField: TFieldName;
    function GetStateOrProvinceField: TFieldName;
    function GetSurnameField: TFieldName;
    function GetURIField: TFieldName;
    function GetRegTokenField: TFieldName;
    procedure SetRegTokenField(const Value: TFieldName);
  protected
    procedure DoVerifyUser(ASubjectName: TRdnSequence;
                           ASubjectAltName: TGeneralNames;
                           const ARegToken: WideString;
                           var AVerified: Boolean); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property CommonNameField: TFieldName read GetCommonNameField write SetCommonNameField;
    property GivenNameField: TFieldName read GetGivenNameField write SetGivenNameField;
    property SurnameField: TFieldName read GetSurnameField write SetSurnameField;
    property InitialsField: TFieldName read GetInitialsField write SetInitialsField;
    property GenerationQualifierField: TFieldName read GetGenerationQualifierField write SetGenerationQualifierField;
    property LocalityField: TFieldName read GetLocalityField write SetLocalityField;
    property StateOrProvinceField: TFieldName read GetStateOrProvinceField write SetStateOrProvinceField;
    property CountryField: TFieldName read GetCountryField write SetCountryField;
    property OrganizationalUnitField: TFieldName read GetOrganizationalUnitField write SetOrganizationalUnitField;
    property OrganizationField: TFieldName read GetOrganizationField write SetOrganizationField;
    property DNQualifierField: TFieldName read GetDNQualifierField write SetDNQualifierField;
    property Rfc822NameField: TFieldName read GetRfc822NameField write SetRfc822NameField;
    property URIField: TFieldName read GetURIField write SetURIField;
    property DnsNameField: TFieldName read GetDnsNameField write SetDnsNameField;
    property IPAddressField: TFieldName read GetIPAddressField write SetIPAddressField;
    property RegTokenField: TFieldName read GetRegTokenField write SetRegTokenField;
  end;

implementation

{ TSsDBUserList }

constructor TSsDBUserList.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TDataLink.Create;
  FLock := TCriticalSection.Create;
end;

destructor TSsDBUserList.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FLock.Free;
  inherited;
end;

procedure TSsDBUserList.DoVerifyUser(ASubjectName: TRdnSequence;
  ASubjectAltName: TGeneralNames; const ARegToken: WideString;
  var AVerified: Boolean);
var
  Found: Boolean;
begin
  AVerified := False;
  if not Assigned(FRegTokenField) then
    raise Exception.Create('A RegTokenField must be assigned');
  FLock.Acquire;
  try
    DataSource.DataSet.First;
    while not DataSource.DataSet.Eof do begin
      Found := True;
      if Assigned(FCommonNameField) then
        Found := Found and (FCommonNameField.AsString = ASubjectName.CommonName);
      if Assigned(FCountryField) then
        Found := Found and (FCountryField.AsString = ASubjectName.Country);
      if Assigned(FDNQualifierField) then
        Found := Found and (FDNQualifierField.AsString = ASubjectName.DnQualifier);
      if Assigned(FDnsNameField) then
        Found := Found and (FDnsNameField.AsString = ASubjectAltName.DnsName);
      if Assigned(FGenerationQualifierField) then
        Found := Found and (FGenerationQualifierField.AsString = ASubjectName.GenerationQualifier);
      if Assigned(FGenerationQualifierField) then
        Found := Found and (FGenerationQualifierField.AsString = ASubjectName.GenerationQualifier);
      if Assigned(FGivenNameField) then
        Found := Found and (FGivenNameField.AsString = ASubjectName.GivenName);
      if Assigned(FInitialsField) then
        Found := Found and (FInitialsField.AsString = ASubjectName.Initials);
      if Assigned(FIPAddressField) then
        Found := Found and (FIPAddressField.AsString = ASubjectAltName.IPAddress);
      if Assigned(FLocalityField) then
        Found := Found and (FLocalityField.AsString = ASubjectName.LocalityName);
      if Assigned(FOrganizationalUnitField) then
        Found := Found and (FOrganizationalUnitField.AsString = ASubjectName.OrganizationalUnitName);
      if Assigned(FOrganizationField) then
        Found := Found and (FOrganizationField.AsString = ASubjectName.OrganizationName);
      if Assigned(FRfc822NameField) then
        Found := Found and (FRfc822NameField.AsString = ASubjectAltName.Rfc822Name);
      if Assigned(FStateOrProvinceField) then
        Found := Found and (FStateOrProvinceField.AsString = ASubjectName.StateOrProvinceName);
      if Assigned(FSurnameField) then
        Found := Found and (FSurnameField.AsString = ASubjectName.Surname);
      if Assigned(FURIField) then
        Found := Found and (FURIField.AsString = ASubjectAltName.URI);
      if Found then begin
        AVerified := ARegToken = FRegTokenField.AsString;
        Break;
      end;
      DataSource.DataSet.Next;
    end;
  finally
    FLock.Release;
  end;
end;

function TSsDBUserList.GetCommonNameField: TFieldName;
begin
  if Assigned(FCommonNameField) then
    Result := FCommonNameField.FieldName;
end;

function TSsDBUserList.GetCountryField: TFieldName;
begin
  if Assigned(FCountryField) then
    Result := FCountryField.FieldName;
end;

function TSsDBUserList.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TSsDBUserList.GetDNQualifierField: TFieldName;
begin
  if Assigned(FDNQualifierField) then
    Result := FDNQualifierField.FieldName;
end;

function TSsDBUserList.GetDnsNameField: TFieldName;
begin
  if Assigned(FDnsNameField) then
    Result := FDnsNameField.FieldName;
end;

function TSsDBUserList.GetGenerationQualifierField: TFieldName;
begin
  if Assigned(FGenerationQualifierField) then
    Result := FGenerationQualifierField.FieldName;
end;

function TSsDBUserList.GetGivenNameField: TFieldName;
begin
  if Assigned(FGivenNameField) then
    Result := FGivenNameField.FieldName;
end;

function TSsDBUserList.GetInitialsField: TFieldName;
begin
  if Assigned(FInitialsField) then
    Result := FInitialsField.FieldName;
end;

function TSsDBUserList.GetIPAddressField: TFieldName;
begin
  if Assigned(FIPAddressField) then
    Result := FIPAddressField.FieldName;
end;

function TSsDBUserList.GetLocalityField: TFieldName;
begin
  if Assigned(FLocalityField) then
    Result := FLocalityField.FieldName;
end;

function TSsDBUserList.GetOrganizationalUnitField: TFieldName;
begin
  if Assigned(FOrganizationalUnitField) then
    Result := FOrganizationalUnitField.FieldName;
end;

function TSsDBUserList.GetOrganizationField: TFieldName;
begin
  if Assigned(FOrganizationField) then
    Result := FOrganizationField.FieldName;
end;

function TSsDBUserList.GetRegTokenField: TFieldName;
begin
  if Assigned(FRegTokenField) then
    Result := FRegTokenField.FieldName;
end;

function TSsDBUserList.GetRfc822NameField: TFieldName;
begin
  if Assigned(FRfc822NameField) then
    Result := FRfc822NameField.FieldName;
end;

function TSsDBUserList.GetStateOrProvinceField: TFieldName;
begin
  if Assigned(FStateOrProvinceField) then
    Result := FStateOrProvinceField.FieldName;
end;

function TSsDBUserList.GetSurnameField: TFieldName;
begin
  if Assigned(FSurnameField) then
    Result := FSurnameField.FieldName;
end;

function TSsDBUserList.GetURIField: TFieldName;
begin
  if Assigned(FURIField) then
    Result := FURIField.FieldName;
end;

procedure TSsDBUserList.Loaded;
begin
  inherited;
  CommonNameField := FCommonNameFld;
  CountryField := FCountryFld;
  DnsNameField := FDnsNameFld;
  DNQualifierField := FDNQualifierFld;
  GenerationQualifierField := FGenerationQualifierFld;
  GivenNameField := FGivenNameFld;
  InitialsField := FInitialsFld;
  IPAddressField := FIPAddressFld;
  LocalityField := FLocalityFld;
  OrganizationalUnitField := FOrganizationalUnitFld;
  OrganizationField := FOrganizationFld;
  RegTokenField := FRegTokenFld;
  Rfc822NameField := FRfc822NameFld;
  StateOrProvinceField := FStateOrProvinceFld;
  SurnameField := FSurnameFld;
  URIField := FURIFld;
end;

procedure TSsDBUserList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and
     Assigned(FDataLink) and
     (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TSsDBUserList.SetCommonNameField(const Value: TFieldName);
begin
  if csLoading in ComponentState then
    FCommonNameFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FCommonNameField := DataSource.DataSet.FieldByName(Value)
  else
    FCommonNameField := nil;
end;

procedure TSsDBUserList.SetCountryField(const Value: TFieldName);
begin
  if csLoading in ComponentState then
    FCountryFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FCountryField := DataSource.DataSet.FieldByName(Value)
  else
    FCountryField := nil;
end;

procedure TSsDBUserList.SetDataSource(const Value: TDataSource);
begin
  if Value <> DataSource then begin
    FGivenNameField := nil;
    FLocalityField := nil;
    FStateOrProvinceField := nil;
    FGenerationQualifierField := nil;
    FOrganizationField := nil;
    FInitialsField := nil;
    FURIField := nil;
    FDnsNameField := nil;
    FRfc822NameField := nil;
    FCommonNameField := nil;
    FDNQualifierField := nil;
    FOrganizationalUnitField := nil;
    FCountryField := nil;
    FSurnameField := nil;
    FIPAddressField := nil;
  end;
  FDataLink.DataSource := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

procedure TSsDBUserList.SetDNQualifierField(const Value: TFieldName);
begin
  if csLoading in ComponentState then
    FDNQualifierFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FDNQualifierField := DataSource.DataSet.FieldByName(Value)
  else
    FDNQualifierField := nil;
end;

procedure TSsDBUserList.SetDnsNameField(const Value: TFieldName);
begin
  if csLoading in ComponentState then
    FDnsNameFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FDnsNameField := DataSource.DataSet.FieldByName(Value)
  else
    FDnsNameField := nil;
end;

procedure TSsDBUserList.SetGenerationQualifierField(const Value: TFieldName);
begin
  if csLoading in ComponentState then
    FGenerationQualifierFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FGenerationQualifierField := DataSource.DataSet.FieldByName(Value)
  else
    FGenerationQualifierField := nil;
end;

procedure TSsDBUserList.SetGivenNameField(const Value: TFieldName);
begin                               
  if csLoading in ComponentState then
    FGivenNameFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FGivenNameField := DataSource.DataSet.FieldByName(Value)
  else
    FGivenNameField := nil;
end;

procedure TSsDBUserList.SetInitialsField(const Value: TFieldName);
begin         
  if csLoading in ComponentState then
    FInitialsFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FInitialsField := DataSource.DataSet.FieldByName(Value)
  else
    FInitialsField := nil;
end;

procedure TSsDBUserList.SetIPAddressField(const Value: TFieldName);
begin        
  if csLoading in ComponentState then
    FIPAddressFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FIPAddressField := DataSource.DataSet.FieldByName(Value)
  else
    FIPAddressField := nil;
end;

procedure TSsDBUserList.SetLocalityField(const Value: TFieldName);
begin         
  if csLoading in ComponentState then
    FLocalityFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FLocalityField := DataSource.DataSet.FieldByName(Value)
  else
    FLocalityField := nil;
end;

procedure TSsDBUserList.SetOrganizationalUnitField(const Value: TFieldName);
begin        
  if csLoading in ComponentState then
    FOrganizationalUnitFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FOrganizationalUnitField := DataSource.DataSet.FieldByName(Value)
  else
    FOrganizationalUnitField := nil;
end;

procedure TSsDBUserList.SetOrganizationField(const Value: TFieldName);
begin                  
  if csLoading in ComponentState then
    FOrganizationFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FOrganizationField := DataSource.DataSet.FieldByName(Value)
  else
    FOrganizationField := nil;
end;

procedure TSsDBUserList.SetRegTokenField(const Value: TFieldName);
begin            
  if csLoading in ComponentState then
    FRegTokenFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FRegTokenField := DataSource.DataSet.FieldByName(Value)
  else
    FRegTokenField := nil;
end;

procedure TSsDBUserList.SetRfc822NameField(const Value: TFieldName);
begin        
  if csLoading in ComponentState then
    FRfc822NameFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FRfc822NameField := DataSource.DataSet.FieldByName(Value)
  else
    FRfc822NameField := nil;
end;

procedure TSsDBUserList.SetStateOrProvinceField(const Value: TFieldName);
begin          
  if csLoading in ComponentState then
    FStateOrProvinceFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FStateOrProvinceField := DataSource.DataSet.FieldByName(Value)
  else
    FStateOrProvinceField := nil;
end;

procedure TSsDBUserList.SetSurnameField(const Value: TFieldName);
begin               
  if csLoading in ComponentState then
    FSurnameFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FSurnameField := DataSource.DataSet.FieldByName(Value)
  else
    FSurnameField := nil;
end;

procedure TSsDBUserList.SetURIField(const Value: TFieldName);
begin       
  if csLoading in ComponentState then
    FURIFld := Value
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and (Value <> '') then
    FURIField := DataSource.DataSet.FieldByName(Value)
  else
    FURIField := nil;
end;

end.
