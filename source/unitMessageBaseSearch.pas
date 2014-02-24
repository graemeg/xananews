unit unitMessageBaseSearch;

interface

uses
  Windows, Classes, SysUtils, ConTnrs, unitNNTPServices, unitNNTPFilters;

function Search(from: TArticleBase; containers: TObjectList; filters: TNNTPFilters): TArticleBase;

implementation

function Search(from: TArticleBase; containers: TObjectList; filters: TNNTPFilters): TArticleBase;
var
  ctnr: TArticleContainer;
  ctnrIdx: Integer;
  art: TArticleBase;
begin
  Result := nil;
  if not Assigned(filters) then Exit;

  if Assigned(from) then
  begin
    ctnr := from.Owner;
    ctnrIdx := containers.IndexOf(ctnr);
  end
  else
  begin
    ctnrIdx := 0;
    if Assigned(containers) and (containers.Count > 0) then
      ctnr := TArticleContainer(containers[ctnrIdx])
    else
      ctnr := nil;
  end;

  if ctnr = nil then Exit;

  art := from;
  if art = nil then art := ctnr.FirstArticle else art := art.Next;

  while ctnr <> nil do
  begin
    while art <> nil do
      if Filters.MatchesAll(art) then
        Break
      else
        art := art.Next;

    if art <> nil then
      Break;

    Inc(ctnrIdx);
    if (containers <> nil) and (ctnrIdx < containers.Count) then
    begin
      ctnr := TArticleContainer(containers[ctnrIdx]);
      art := ctnr.FirstArticle;
    end
    else
      ctnr := nil;
  end;

  Result := art;
end;

end.
