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
     cmpMRUList,
     cmpThemedScrollBox,
     cmpCWRichEdit,
     cmpNewsRichEdit,
     cmpRuler,
     cmpHexDump,
     cmpSizingPageControl,
     cmpSpellChecker,
     cmpCWSpellChecker,
     cmpExWebBrowser,
     cmpTexturedPanel,
     cmpColorSelector,
     cmpPersistentOptions,
     cmpRegistryPersistentOptions,
     cmpIniFilePersistentOptions,
     cmpXMLPersistentOptions,
     cmpFileCopier,
     cmpSplitterPanel,
     cmpTextDisplay,
     cmpUCtrls,
     CustomAlignPanel,
     cmpSuDoku,
     cmpExToolbar,
     cmpUserDatabase,
     ExCoolBar,
     cmpExplorerTree,
     cmpCountryComboBox;

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
    TMRUList,
    TThemedScrollBox,
    TExRichEdit,
    TNewsRichEdit,
    TXNRuler,
    THexDump,
    TSizingPageControl,
    TSpellChecker,
    TCWSpellChecker,
    TExWebBrowser,
    TTexturedPanel,
    TColorSelector,
    TRegistryPersistentOptions,
    TIniFilePersistentOptions,
    TXMLPersistentOptions,
    TUniPersistentOptions,
    TFileCopier,
    TSplitterPanel,
    TTextDisplay,
    TUEdit,
    TUComboBox,
    TCustomAlignPanel,
    TSuDoku,
    TExToolbar,
    TUserDatabase,
    TExCoolBar,
    TExplorerTree,
    TCountryComboBox
  ]);
end;

end.
