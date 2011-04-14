unit cmpCWSpellChecker;

interface

uses
  SysUtils, Classes, Controls, Forms, cmpSpellChecker, cmpCWRichEdit;

type
  TCWSpellChecker = class(TSpellChecker)
  private
    fEXRichEdit: TCustomExRichEdit;
  public
    destructor Destroy; override;
    function CheckAndShowModal(PopupParent: TCustomForm; SkipFirstLine: Boolean): Integer;
  published
    property ExRichEdit: TCustomExRichEdit read fEXRichEdit write fEXRichEdit;
  end;

implementation

uses SpellCheckerForm;

{ TCWSpellChecker }

function TCWSpellChecker.CheckAndShowModal(PopupParent: TCustomForm;
  SkipFirstLine: Boolean): Integer;
var
  ss, se: Integer;
  txt: WideString;
  suggestions: TStrings;
begin
  Result := mrOK;
  if not Assigned(ExRichEdit) then Exit;
  if Assigned(fmSpellChecker) then Exit;

  txt := ExRichEdit.Text;

  suggestions := TStringList.Create;
  try
    if not Check(txt, 1, ss, se, suggestions, SkipFirstLine) then
    begin
      fmSpellChecker := TfmSpellChecker.Create(nil);
      fmSpellChecker.QuoteChars := QuoteChars;
      fmSpellChecker.Initialize(self, ss, se, suggestions);
      fmSpellChecker.PopupParent := PopupParent;
      fmSpellChecker.PopupMode := pmExplicit;
      Result := fmSpellChecker.ShowModal;
    end;
  finally
    suggestions.Free;
  end;
end;

destructor TCWSpellChecker.Destroy;
begin
  if Assigned(fmSpellChecker) then
    fmSpellChecker.Close;

  inherited Destroy;
end;

end.
