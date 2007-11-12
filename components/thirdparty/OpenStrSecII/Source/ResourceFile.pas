{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     ResourceFile Unit                                 }
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
unit ResourceFile;

interface

uses
  {$IFDEF GUI_APP}
  {$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc, QGraphics, QControls, QForms, QDialogs,
  {$ENDIF}
  {$ENDIF}
  SysUtils, Classes;

type
  TLoadNotifyInfo = class
  private
    FOnLoad: TNotifyEvent;
    FTarget: TObject;
    procedure SetOnLoad(const Value: TNotifyEvent);
    procedure SetTarget(const Value: TObject);
  public
    constructor Create(ATarget: TObject; AOnLoad: TNotifyEvent);
    property Target: TObject read FTarget write SetTarget;
    property OnLoad: TNotifyEvent read FOnLoad write SetOnLoad;
  end;

  TResourceFile = class(TComponent)
  private
    FDataStream: TStream;
    FDecompStream: TStream;
    FFileName: TFileName;
    FLoadTime: TDateTime;
    FLoadNotifies: TList;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure SetFileName(const Value: TFileName);
    procedure SetLoadTime(const Value: TDateTime);
    function GetDataStream: TStream;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DestroyLoadNotifies;
    procedure Loaded; override;
    procedure LoadNotify;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddLoadNotification(ATarget: TObject; AOnLoad: TNotifyEvent);
    procedure RemoveLoadNotification(ATarget: TObject; AOnLoad: TNotifyEvent);
    procedure CheckCreateDataStream;
    function DecompStream: TStream;
    property DataStream: TStream read GetDataStream;
  published
    property FileName: TFileName read FFileName write SetFileName;
    property LoadTime: TDateTime read FLoadTime write SetLoadTime stored False;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('StreamSec', [TResourceFile]);
end;

{ TResourceFile }

procedure TResourceFile.AddLoadNotification(ATarget: TObject;
  AOnLoad: TNotifyEvent);
begin
  FLoadNotifies.Add(TLoadNotifyInfo.Create(ATarget,AOnLoad));
end;

procedure TResourceFile.CheckCreateDataStream;
begin
  if FDataStream = nil then
    FDataStream := TMemoryStream.Create;
end;

constructor TResourceFile.Create(AOwner: TComponent);
begin
  inherited;
  FLoadNotifies := TList.Create;
end;

function TResourceFile.DecompStream: TStream;
begin
  if not Assigned(FDecompStream) then begin
    FDecompStream := TMemoryStream.Create;
    FDataStream.Position := 0;
    FDecompStream.CopyFrom(FDataStream,0);
//    LZDecompress(FDataStream,FDecompStream);
  end;
  FDecompStream.Position := 0;
  Result := FDecompStream;
end;

procedure TResourceFile.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Image',ReadData,WriteData,Assigned(FDataStream));
end;

destructor TResourceFile.Destroy;
begin
  inherited;
  DestroyLoadNotifies;
  FDataStream.Free;
end;

procedure TResourceFile.DestroyLoadNotifies;
var
  I: Integer;
begin
  if Assigned(FLoadNotifies) then begin
    for I := 0 to FLoadNotifies.Count - 1 do
      TObject(FLoadNotifies[I]).Free;
    FLoadNotifies.Free;
    FLoadNotifies := nil;
  end;
end;

function TResourceFile.GetDataStream: TStream;
begin
  Result := FDataStream;
  if Assigned(Result) then Result.Position := 0;
end;

procedure TResourceFile.Loaded;
begin
  inherited;
  SetFileName(FFileName);
end;

procedure TResourceFile.LoadNotify;
var
  I: Integer;
  Info: TLoadNotifyInfo;
begin
  // Implementation note: Must loop in reverse order, since the target may
  // call RemoveLoadNotification
  for I := FLoadNotifies.Count - 1 downto 0 do begin
    Info := FLoadNotifies[I];
    Info.OnLoad(Self);
  end;
end;

procedure TResourceFile.ReadData(Stream: TStream);
var
  Size: Integer;
begin
  if (FFileName <> '') and (csDesigning in ComponentState) then Exit;
  Size := Stream.Size - Stream.Position;
  if FDataStream = nil then
    FDataStream := TMemoryStream.Create;
  FDataStream.Size := 0;
  FDataStream.CopyFrom(Stream,Size);
  FDataStream.Position := 0;
end;

procedure TResourceFile.RemoveLoadNotification(ATarget: TObject;
  AOnLoad: TNotifyEvent);
var
  I: Integer;
  Info: TLoadNotifyInfo;
begin
  if Assigned(FLoadNotifies) then
    for I := FLoadNotifies.Count - 1 downto 0 do begin
      Info := FLoadNotifies[I];
      if (Info.Target = ATarget) and
         (TMethod(AOnLoad).Code = TMethod(Info.OnLoad).Code) and
         (TMethod(AOnLoad).Data = TMethod(Info.OnLoad).Data) then begin
        FLoadNotifies.Delete(I);
        Info.Free;
        Break;
      end;
    end;
end;

procedure TResourceFile.SetFileName(const Value: TFileName);
var
  MS: TMemoryStream;
begin
  FFileName := Value;
  if Value = '' then
    Exit;
  if FDataStream = nil then FDataStream := TMemoryStream.Create;
  if FileExists(Value) and (csDesigning in ComponentState) and
     not (csLoading in ComponentState) then begin
    MS := TMemoryStream.Create;
    try
      MS.LoadFromFile(Value);
      MS.Position := 0;
      FDataStream.Size := 0;
      FDataStream.CopyFrom(MS,0);
//      LZCompress(MS,FDataStream);
      FDecompStream.Free;
      FDecompStream := nil;
    finally
      MS.Free;
    end;
    FLoadTime := Now;       
    LoadNotify;
  end else
    FFileName := '';
  FDataStream.Position := 0;
end;

procedure TResourceFile.SetLoadTime(const Value: TDateTime);
begin
  if csLoading in ComponentState then
    FLoadTime := Value;
end;

procedure TResourceFile.WriteData(Stream: TStream);
begin
  FDataStream.Position := 0;
  Stream.CopyFrom(FDataStream,0);
end;

{ TLoadNotifyInfo }

constructor TLoadNotifyInfo.Create(ATarget: TObject;
  AOnLoad: TNotifyEvent);
begin
  FTarget := ATarget;
  FOnLoad := AOnLoad;
end;

procedure TLoadNotifyInfo.SetOnLoad(const Value: TNotifyEvent);
begin
  FOnLoad := Value;
end;

procedure TLoadNotifyInfo.SetTarget(const Value: TObject);
begin
  FTarget := Value;
end;

end.
 
