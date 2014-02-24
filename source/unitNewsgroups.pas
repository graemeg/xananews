unit unitNewsgroups;

interface

uses
  Classes;

type
  // Specialized class to help searching for existing groupnames inside the
  // groups stringlist where each line contains:
  //   group last first p
  // where <group> is the name of the newsgroup, <last> is the number of
  // the last known article currently in that newsgroup, <first> is the
  // number of the first article currently in the newsgroup, and <p> is
  // either 'y' or 'n' indicating whether posting to this newsgroup is
  // allowed ('y') or prohibited ('n').

  TNewsgroupsStringList = class(TStringList)
  public
    function Find(const S: string; var Index: Integer): Boolean; override;
    function IndexOf(const S: string): Integer; override;
  end;

implementation

uses
  unitSearchString;

{ TNewsgroupsStringList }

function TNewsgroupsStringList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
  st: string;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    st := Get(I);
    C := CompareStrings(SplitString(' ', st), S);
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

function TNewsgroupsStringList.IndexOf(const S: string): Integer;
var
  st: string;
begin
  if not Sorted then
  begin
    for Result := 0 to Count - 1 do
    begin
      st := Get(Result);
      if CompareStrings(SplitString(' ', st), S) = 0 then
        Exit;
    end;
    Result := -1;
  end
  else
    if not Find(S, Result) then
      Result := -1;
end;


end.
