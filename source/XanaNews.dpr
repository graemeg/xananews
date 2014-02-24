program XanaNews;

{$SetPEFlags $21}  // $01 = IMAGE_FILE_RELOCS_STRIPPED
                   // $20 = IMAGE_FILE_LARGE_ADDRESS_AWARE

{$if CompilerVersion >= 21.0} // 21.0 = Delphi 2010
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$ifend}

{$I IdCompilerDefines.inc}

uses
  FastMM4,
{$ifdef madExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
{$endif}
  Forms,
  Windows,
  SysUtils,
  SyncObjs,
  IdGlobal,
  IdStack,
  IdThread,
  IdThreadSafe,
  HTMLHelpViewer,
  NewsGlobals in 'NewsGlobals.pas',
  MainForm in 'MainForm.pas' {fmMain},
  SplashForm in 'SplashForm.pas' {fmSplash},
  AccountsDialog in 'AccountsDialog.pas' {dlgAccounts},
  NewsgroupsDialog in 'NewsgroupsDialog.pas' {dlgNewsgroups},
  MessagebaseManagementDialog in 'MessagebaseManagementDialog.pas' {dlgMessagebaseManagement},
  PostMessageForm in 'PostMessageForm.pas' {fmPostMessage},
  FilterDialog in 'FilterDialog.pas' {dlgDeleteMessages},
  MessagesDialog in 'MessagesDialog.pas' {dlgGetMessages},
  AddFilterDialog in 'AddFilterDialog.pas' {dlgAddFilter},
  BatchesDialog in 'BatchesDialog.pas' {dlgBatches},
  BatchDialog in 'BatchDialog.pas' {dlgBatch},
  ReplyByMailForm in 'ReplyByMailForm.pas' {fmReplyByMail},
  AdvancedHeadersDialog in 'AdvancedHeadersDialog.pas' {dlgAdvancedHeaders},
  NewsgroupStatisticsForm in 'NewsgroupStatisticsForm.pas' {fmNewsgroupStatistics},
  AttachmentsDialog in 'AttachmentsDialog.pas' {dlgAttachments},
  ServerAdminCreateGroupDialog in 'ServerAdminCreateGroupDialog.pas' {dlgServerAdminCreateGroup},
  ServerAdminRemoveGroupDialog in 'ServerAdminRemoveGroupDialog.pas' {dlgServerAdminRemoveGroup},
  TestPerformanceDialog in 'TestPerformanceDialog.pas' {dlgTestPerformance},
  FilterDetailsDialog in 'FilterDetailsDialog.pas' {dlgFilterDetails},
  MailAccountsDialog in 'MailAccountsDialog.pas' {dlgMailAccounts},
  unitCheckVersion in 'unitCheckVersion.pas',
  NewVersionDialog in 'NewVersionDialog.pas' {fmNewVersionNotification},
  PostToGroupsForm in 'PostToGroupsForm.pas' {fmPostToGroups},
  unitXanaExporter in 'unitXanaExporter.pas',
  ExportSettingsFrame in 'ExportSettingsFrame.pas' {fmeExportSettings: TFrame},
  BozoDetailsDialog in 'BozoDetailsDialog.pas' {dlgBozoDetails},
  unitBookmarks in 'unitBookmarks.pas',
  unitCIDMimeHandler in 'unitCIDMimeHandler.pas',
  unitLog in 'unitLog.pas',
  CancelArticleDialog in 'CancelArticleDialog.pas' {dlgCancelArticles},
  MoveMessagebaseDialog in 'MoveMessagebaseDialog.pas' {dlgMoveMessagebase},
  IdentityDialog in 'IdentityDialog.pas' {dlgIdentity},
  IdentitiesDialog in 'IdentitiesDialog.pas' {dlgIdentities},
  NewUserWizardForm in 'NewUserWizardForm.pas' {frmNewUserWizard},
  CombineDecodeDialog in 'CombineDecodeDialog.pas' {dlgCombineDecode},
  CheckCrosspostDialog in 'CheckCrosspostDialog.pas' {dlgCheckCrosspost},
  unitGetMessages1 in 'unitGetMessages1.pas' {dlgGetMessages1},
  unitNNTPServices in 'unitNNTPServices.pas',
  PropertyBaseForm in 'PropertyBaseForm.pas' {fmPropertyBase},
  OptionsForm in 'OptionsForm.pas' {fmOptions},
  PropertyPageForm in 'PropertyPageForm.pas' {fmPropertyPage},
  PropertyPageGeneralForm in 'PropertyPageGeneralForm.pas' {fmPropertyPageGeneral},
  PropertyPageEnterKeyForm in 'PropertyPageEnterKeyForm.pas' {fmPropertyPageEnterKey},
  PropertyPageConnectionForm in 'PropertyPageConnectionForm.pas' {fmPropertyPageConnection},
  PropertyPageGroupsTreeForm in 'PropertyPageGroupsTreeForm.pas' {fmPropertyPageGroupsTree},
  PropertyPageColorFontForm in 'PropertyPageColorFontForm.pas' {fmPropertyPageColorFont},
  PropertyPageFontForm in 'PropertyPageFontForm.pas' {fmPropertyPageFont},
  unitFontDetails in 'unitFontDetails.pas',
  PropertyPageMessageTreeActionsForm in 'PropertyPageMessageTreeActionsForm.pas' {fmPropertyPageMessageTreeActions},
  PropertyPageMessageTreeDisplayForm in 'PropertyPageMessageTreeDisplayForm.pas' {fmPropertyPageMessageTreeDisplay},
  PropertyPageMessagePaneForm in 'PropertyPageMessagePaneForm.pas' {fmPropertyPageMessagePane},
  PropertyPageCustomHeadersForm in 'PropertyPageCustomHeadersForm.pas' {fmPropertyPageCustomHeaders},
  PropertyPageWin98Form in 'PropertyPageWin98Form.pas' {fmPropertyPageWin98},
  PropertyPageKeywordsForm in 'PropertyPageKeywordsForm.pas' {fmPropertyPageKeywords},
  PropertyPagePosting in 'PropertyPagePosting.pas' {fmPropertyPagePosting},
  AccountForm in 'AccountForm.pas' {fmAccount},
  PropertyPageAccountGeneralForm in 'PropertyPageAccountGeneralForm.pas' {fmPropertyPageAccountGeneral},
  PropertyPageAccountServerForm in 'PropertyPageAccountServerForm.pas' {fmPropertyPageAccountServer},
  PropertyPageAccountAdvancedServerForm in 'PropertyPageAccountAdvancedServerForm.pas' {fmPropertyPageAccountAdvancedServer},
  PropertyPageDefaultsForm in 'PropertyPageDefaultsForm.pas' {fmPropertyPageDefaults},
  PropertyPageExtraPostingForm in 'PropertyPageExtraPostingForm.pas' {fmPropertyPageExtraPosting},
  PropertyPageQuotingForm in 'PropertyPageQuotingForm.pas' {fmPropertyPageQuoting},
  PropertyPageFiltersForm in 'PropertyPageFiltersForm.pas' {fmPropertyPageFilters},
  PropertyPagePreferencesForm in 'PropertyPagePreferencesForm.pas' {fmPropertyPagePreferences},
  PropertyPagePostingServersForm in 'PropertyPagePostingServersForm.pas' {fmPropertyPagePostingServers},
  PropertyPageBozoForm in 'PropertyPageBozoForm.pas' {fmPropertyPageBozos},
  PropertyPageShortcutsForm in 'PropertyPageShortcutsForm.pas' {fmPropertyPageShortcuts},
  MailAccountForm in 'MailAccountForm.pas' {fmMailAccount},
  PropertyPageMailAccountGeneralForm in 'PropertyPageMailAccountGeneralForm.pas' {fmPropertyPageMailAccountGeneral},
  PropertyPageMailAccountPreferencesForm in 'PropertyPageMailAccountPreferencesForm.pas' {fmPropertyPageMailAccountPreferences},
  PropertyPageMailAccountServerForm in 'PropertyPageMailAccountServerForm.pas' {fmPropertyPageMailAccountServer},
  PropertyPageMailAccountConnectionForm in 'PropertyPageMailAccountConnectionForm.pas' {fmPropertyPageMailAccountConnection},
  NewsgroupForm in 'NewsgroupForm.pas' {fmNewsgroup},
  PropertyPageNewsgroupGeneralForm in 'PropertyPageNewsgroupGeneralForm.pas' {fmPropertyPageNewsgroupGeneral},
  PropertyPageSortingForm in 'PropertyPageSortingForm.pas' {fmPropertyPageSorting},
  PostFrame in 'PostFrame.pas' {fmePost: TFrame},
  unitMessages in 'unitMessages.pas',
  unitSettings in 'unitSettings.pas',
  unitIdentities in 'unitIdentities.pas',
  unitNNTPFilters in 'unitNNTPFilters.pas',
  unitNewsReaderOptions in 'unitNewsReaderOptions.pas',
  unitSavedArticles in 'unitSavedArticles.pas',
  unitMailServices in 'unitMailServices.pas',
  unitNNTPThreadManager in 'unitNNTPThreadManager.pas',
  AddAccountWizard in 'AddAccountWizard.pas' {fmAddAccountWizard},
  FindOnInternetDialog in 'FindOnInternetDialog.pas' {dlgFindMessageOnInternet},
  SearchDialog in 'SearchDialog.pas' {dlgSearch},
  unitRFC2646Coder in 'unitRFC2646Coder.pas',
  ReadLnDelayDialog in 'ReadLnDelayDialog.pas' {dlgReadLnDelay},
  unitMessageBaseSearch in 'unitMessageBaseSearch.pas',
  unitNewsgroups in 'unitNewsgroups.pas',
  unitNewsThread in 'unitNewsThread.pas',
  unitNNTPThreads in 'unitNNTPThreads.pas',
  cmpMessageScrollBox in 'cmpMessageScrollBox.pas',
  ExportDialog in 'ExportDialog.pas',
  IdNNTPX in 'IdNNTPX.pas',
  unitArticleHash in 'unitArticleHash.pas',
  unitBatches in 'unitBatches.pas',
  unitCRC32 in 'unitCRC32.pas',
  unitMessageMime in 'unitMessageMime.pas',
  unitMessageNNTPBinary in 'unitMessageNNTPBinary.pas',
  unitMessageYEncodedBinary in 'unitMessageYEncodedBinary.pas',
  unitNewUserWizard in 'unitNewUserWizard.pas',
  XnCoderQuotedPrintable in 'XnCoderQuotedPrintable.pas',
  XnCoderUUE in 'XnCoderUUE.pas',
  unitDefaultNewsreader in 'unitDefaultNewsreader.pas',
  XnXFace in 'XnXFace.pas';

{$R *.res}
{$R I.RES}

procedure ShowSplash;
var
  i: Integer;
  noSplash: Boolean;
begin
  noSplash := False;
  for i := 1 to ParamCount do
    if CompareText(ParamStr(i), '-ns') = 0 then
      noSplash := True
    else
      if CompareText(Copy(ParamStr(i), 1, 12), '-XMLSettings') = 0 then
        NewsGlobals.UseXMLSettings(Copy(ParamStr(i), 13, MaxInt));

  if not noSplash then
  begin
    fmSplash := TfmSplash.Create(Application);
    fmSplash.Show;
    fmSplash.Update;
  end;
end;

{$ifdef REGISTER_EXPECTED_MEMORY_LEAK}
type
  TIdThreadSafeIntegerAccess = class(TIdThreadSafeInteger);
  TCriticalSectionAcceess = class(TCriticalSection);
{$endif}
begin
//  OutputDebugString('SAMPLING OFF');
  if CheckSetAsDefaultNewsreader then
    Exit;

  if TfmMain.CheckRunOnce then
  begin
    Application.Initialize;
    Application.MainFormOnTaskBar := True;
    Application.UpdateMetricSettings := False;

    // Ignore expected/documented Indy leaks
    {$ifdef madExcept}
      {$ifdef LeakChecking}
        {$ifndef FREE_ON_FINAL}
          {$ifdef REGISTER_EXPECTED_MEMORY_LEAK}
            ThisIsNoLeak(GThreadCount);
            ThisIsNoLeak(TIdThreadSafeIntegerAccess(GThreadCount).FCriticalSection);
            ThisIsNoLeak(@TCriticalSectionAcceess(TIdThreadSafeIntegerAccess(GThreadCount).FCriticalSection).FSection);

            {$ifndef DOTNET}
              // Hmm, this is not possible it is a "global" in the implementation section.
              //ThisIsNoLeak(GStackCriticalSection);
              //ThisIsNoLeak(TIdThreadSafeIntegerAccess(GStackCriticalSection).FCriticalSection);
              //ThisIsNoLeak(@TCriticalSectionAcceess(TIdThreadSafeIntegerAccess(GStackCriticalSection).FCriticalSection).FSection);
            {$endif}
          {$endif}
        {$endif}
      {$endif}
    {$endif}

    SetThreadLocale(GetUserDefaultLCID);
    GetFormatSettings;

    ShowSplash;
    Application.Title := 'XanaNews';
    Application.HelpFile := 'XanaNews.chm';
    Application.CreateForm(TfmMain, fmMain);
    Application.Run;
  end;
end.
