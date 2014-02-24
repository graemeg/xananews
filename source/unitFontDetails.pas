unit unitFontDetails;

interface

uses
  Windows, Classes, SysUtils, Forms, Graphics, ConTnrs;

type
  TFontDetails = class
  private
    fName: string;
    fSizes: TList;
    fFixed: Boolean;
    fTrueType: Boolean;
    function GetSize(idx: Integer): Integer;
    function GetSizeCount: Integer;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    property Name: string read fName;
    property Fixed: Boolean read fFixed;
    property TrueType: Boolean read fTrueType;

    property SizeCount: Integer read GetSizeCount;
    property Size[idx: Integer]: Integer read GetSize;
  end;

var
  gFontDetails: TStringList = nil;

procedure EnumerateFonts;
function FindFontDetails(const fontName: string): TFontDetails;
function IsFontFixed(const fontName: string): Boolean;
function FindMatchingFixedFont(const fontName: string): string;
procedure FreeFontDetails;

implementation

uses
  unitSearchString;

var
  gStandardSizes: array[0..17] of Integer = (6, 7, 8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 26, 28, 36, 48, 72);

function XRound(r1: Extended): Integer;
begin
  if Frac(r1) >=  0.5 then
    Result := Trunc(r1 + 0.5)
  else
    Result := Trunc(r1);
end;

function FindFontDetails(const fontName: string): TFontDetails;
var
  idx: Integer;
begin
  if gFontDetails = nil then
    EnumerateFonts;
  idx := gFontDetails.IndexOf(fontName);
  if idx >= 0 then
    Result := TFontDetails(gFontDetails.Objects[idx])
  else
    Result := nil;
end;

function IsFontFixed(const fontName: string): Boolean;
var
  details: TFontDetails;
begin
  details := FindFontDetails(fontName);
  if details <> nil then
    Result := details.Fixed
  else
    Result := False;
end;

function FindMatchingFixedFont(const fontName: string): string;
var
  st, stub, stub1: string;
  i: Integer;
begin
  if IsFontFixed(fontName) then
    Result := fontName
  else
  begin
    st := fontName;
    stub := SplitString(' ', st);
    Result := '';

    if stub <> '' then
      for i := 0 to gFontDetails.Count - 1 do
      begin
        st := gFontDetails[i];
        stub1 := SplitString(' ', st);
        if (stub1 = stub) and TFontDetails(gFontDetails.Objects[i]).Fixed then
        begin
          Result := gFontDetails[i];
          Break;
        end;
      end;

    if (Result = '') and IsFontFixed('Courier New') then
      Result := 'Courier New';

    if Result = '' then
      for i := 0 to gFontDetails.Count - 1 do
        if TFontDetails(gFontDetails.Objects[i]).Fixed then
        begin
          Result := gFontDetails[i];
          Break;
        end;
  end;
end;

function EnumFontSizesProc(const lpelfe: TEnumLogFontEx;
  const lpntme: TNewTextMetricExW; FontType: DWORD; param: LPARAM): Integer; stdcall;
var
  details: TFontDetails;
  pix, pts: Integer;
begin
  details := TFontDetails(param);
  pix := lpntme.ntmTm.tmHeight - lpntme.ntmTm.tmInternalLeading;// lpelfe.elfLogFont.lfHeight;
  pts := XRound((pix * 72) / Screen.PixelsPerInch);
  if details.fSizes.IndexOf(Pointer(pts)) = -1 then
    details.fSizes.Add(Pointer(pts));
  Result := 1;
end;

function CompareSizes(p1, p2: Pointer): Integer;
begin
  Result := Integer(p1) - Integer(p2);
end;

function EnumFontFamiliesProc(const lpelfe: TEnumLogFontEx;
  const lpntme: TNewTextMetricExW; FontType: DWORD; param: LPARAM): Integer; stdcall;
var
  details: TFontDetails;
  lf: TLogFont;
  fontName: string;
  idx: Integer;
begin
  Result := 1;
  fontName := lpelfe.elfLogFont.lfFaceName;

  if lpelfe.elfLogFont.lfCharset = SYMBOL_CHARSET then
    Exit;

  if (Length(fontName) = 0) or not (fontName[1] in ['A'..'Z', 'a'..'z', '0'..'9']) then
    Exit;

  if not gFontDetails.Find(fontName, idx) then
  begin
    details := TFontDetails.Create(fontName);
    gFontDetails.AddObject(fontName, details);

    details.fTrueType := FontType = TRUETYPE_FONTTYPE;
    if FontType = TRUETYPE_FONTTYPE then
      details.fFixed := (lpntme.ntmTm.tmPitchAndFamily and 1) = 0
    else
      details.fFixed := (PTextMetric(@lpntme.ntmTm)^.tmPitchAndFamily and 1) = 0;

    if lpelfe.elfLogFont.lfOutPrecision = OUT_STRING_PRECIS then
    begin
      details.fSizes := TList.Create;
      FillChar(lf, SizeOf(lf), 0);
      lf.lfCharSet := DEFAULT_CHARSET;
      lstrcpyn(lf.lfFaceName, PChar(details.fName), SizeOf(lf.lfFaceName) div SizeOf(Char));
      EnumFontFamiliesEx(HDC(param), lf, @EnumFontSizesProc, LPARAM(details), 0);
      details.fSizes.Sort(CompareSizes);
    end;
  end;
end;

function CompareFontDetails(p1, p2: Pointer): Integer;
var
  d1, d2: TFontDetails;
begin
  d1 := TFontDetails(p1);
  d2 := TFontDetails(p2);

  Result := CompareText(d1.Name, d2.Name);
end;

procedure EnumerateFonts;
var
  dc: hdc;
  lf: TLogFont;
begin
  if Assigned(gFontDetails) then Exit;

  gFontDetails := TStringList.Create;
  gFontDetails.Duplicates := dupIgnore;
  gFontDetails.Sorted := True;
  dc := GetDC(0);
  try
    FillChar(lf, SizeOf(lf), 0);
    lf.lfCharSet := DEFAULT_CHARSET;

    EnumFontFamiliesEx(dc, lf, @EnumFontFamiliesProc, LPARAM(dc), 0);
  finally
    ReleaseDC(0, dc);
  end;
end;

{ TFontDetails }

constructor TFontDetails.Create(const AName: string);
begin
  fName := AName;
end;

destructor TFontDetails.Destroy;
begin
  FreeAndNil(fSizes);
  inherited Destroy;
end;

function TFontDetails.GetSize(idx: Integer): Integer;
begin
  if Assigned(fSizes) then
    Result := Integer(fSizes[idx])
  else
    Result := gStandardSizes[idx];
end;

function TFontDetails.GetSizeCount: Integer;
begin
  if Assigned(fSizes) then
    Result := fSizes.Count
  else
    Result := High(gStandardSizes) + 1;
end;

procedure FreeFontDetails;
var
  i: Integer;
begin
  if not Assigned(gFontDetails) then Exit;

  for i := 0 to gFontDetails.Count - 1 do
    gFontDetails.Objects[i].Free;
  FreeAndNil(gFontDetails);
end;

initialization
finalization
   FreeFontDetails;
end.
