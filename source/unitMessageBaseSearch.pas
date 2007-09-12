unit unitMessageBaseSearch;

interface

uses Windows, Classes, SysUtils, ConTnrs, unitNNTPServices, unitNNTPFilters;

function Search (from : TArticleBase; containers : TObjectList; filters : TNNTPFilters) : TArticleBase;

implementation

(*----------------------------------------------------------------------*
 | function Search
 *----------------------------------------------------------------------*)
function Search (from : TArticleBase; containers : TObjectList; filters : TNNTPFilters) : TArticleBase;
var
  ctnr : TArticleContainer;
  ctnrIdx : Integer;
  art : TArticleBase;
begin
  result := Nil;
  if not Assigned (filters) then Exit;

  if Assigned (from) then
  begin
    ctnr := from.Owner;
    ctnrIdx := containers.IndexOf(ctnr);
  end
  else
  begin
    ctnrIdx := 0;
    if Assigned (containers) and (containers.Count > 0) then
      ctnr := TArticleContainer (containers [ctnrIdx])
    else
      ctnr := Nil
  end;

  if ctnr = Nil then Exit;

  art := from;
  if art = Nil then art := ctnr.FirstArticle else art := art.Next;

  while ctnr <> Nil do
  begin
    while art <> Nil do
      if Filters.MatchesAll (art) then
        break
      else
        art := art.Next;

    if art <> Nil then
      break;

    Inc (ctnrIdx);
    if (containers <> Nil) and (ctnrIdx < containers.Count) then
    begin
      ctnr := TArticleContainer (containers [ctnrIdx]);
      art := ctnr.FirstArticle
    end
    else
      ctnr := Nil
  end;

  result := art
end;


end.
