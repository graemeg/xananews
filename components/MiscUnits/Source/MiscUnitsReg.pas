unit MiscUnitsReg;

interface

procedure Register;

implementation

{$R MiscUnitsReg.dcr}

uses Classes,
     cmpRunOnce,
     cmpStandardSystemMenu,
     cmpPersistentPosition,
     cmpNTAboutBox,
     cmpHyperlinkButton,
     cmpExSplitter,
     cmpMessageDisplay,
     cmpThemedScrollBox,
     cmpCWRichEdit,
     cmpNewsRichEdit,
     cmpRuler,
     cmpSpellChecker,
     cmpCWSpellChecker,
     cmpExWebBrowser,
     cmpFileCopier,
     cmpSplitterPanel;

procedure Register;
begin
  RegisterComponents ('Colin Wilson''s Components', [
    TRunOnce,
    TStandardSystemMenu,
    TPersistentPosition,
    TNTAboutBox,
    THyperlinkButton,
    TExSplitter,
    TMessageDisplay,
    TThemedScrollBox,
    TExRichEdit,
    TNewsRichEdit,
    TXNRuler,
    TSpellChecker,
    TCWSpellChecker,
    TExWebBrowser,
    TFileCopier,
    TSplitterPanel
  ]);
end;

end.
