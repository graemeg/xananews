(*======================================================================*
 | MainForm unit for XanaNews Newsreader                                |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright (c) Colin Wilson 2002-2005  All Rights Reserved            |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/01/2002  CPWW  Original                                  |
 | 1.17.2.5 8/2/2005    Q     Author short name functionality           |
 | 1.17.6.0 6/9/2005    DN    Dave Nottage Auto Mark as Read mods       |
 | 1.18.1.0 11/12/2005  CPWW  BDS 2006 version                          |
 *======================================================================*)

unit MainForm;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ActnList, Types,
{$if CompilerVersion >= 24.0} // 24.0 = Delphi XE3
  System.Actions,
{$ifend}
  cmpPersistentPosition, cmpStandardSystemMenu, ImgList, StdActns,
  cmpNTAboutBox, ComCtrls, ExtCtrls, VirtualTrees, ConTnrs, unitNNTPServices,
  Menus, AppEvnts, ExtDlgs, cmpMessageScrollBox, cmpExSplitter,
  unitMessages, PostMessageForm, ReplyByMailForm, unitNewsReaderOptions,
  Buttons, SearchDialog,
  ShellApi, NewsGlobals, CommCtrl,
  ActiveX, unitNewsThread, DateUtils,
  cmpThemedScrollBox, unitSavedArticles, StdCtrls, MMSystem, unitBatches, unitSettings,
  ExportDialog,
{$ifdef madExcept}
  madExcept,
{$endif}
  unitBookmarks, cmpSplitterPanel, unitNewsStringsDisplayObject,
  unitGetMessages1, unitMailServices, Tabs, ButtonGroup, CategoryButtons,
  unitExSettings, XnClasses, XnRawByteStrings, XnCaptionedDockTree, DockingUtils;

type

//=======================================================================
// Iterators used for 'ForEachSelectedArticle' 'ForEachSelectedGroup'
// 'ForEachArticleInSelectedThread'
  TArticleIteratorProc = procedure(article: TArticleBase; param: LPARAM; multiSelect: Boolean) of object;
  TFolderArticleIteratorProc = procedure(article: TFolderArticle; param: LPARAM) of object;
  TGroupIteratorProc = function(group: TSubscribedGroup; param: LPARAM): Boolean of object;
  TArticleFolderIteratorProc = function(folder: TArticleFolder; param: LPARAM): Boolean of object;

//=======================================================================
// Control flags for colun auto-resizing
  TColumnHeaderValue = (chAutoResize, chResizing, chColumnResizing);
  TColumnHeaderStatus = set of TColumnHeaderValue;

  TGetMessagesParams = record     // Used for getting messages for multiple groups
    fromArticle: Int64;           // .. passed as a parameter to ForEachSelectedGroup
    batchRef: Integer;
    useDefaultGroupSettings: Boolean;
                                  // If this flags is set, get the settings below from
                                  // the groups default action

    fHeadersOnly: Boolean;
    fMessageCount: Int64;
    fActionType: TBatchActionType;
    fManagementType: TBatchManagementType;
    fManagementOption: TBatchManagementOption;
    fManagementCount: Integer;
    fSince: TDateTime;
  end;
  PGetMessagesParams = ^TGetMessagesParams;

//=======================================================================
// Main Form class
  TfmMain = class(TForm)
    ilMain: TImageList;
    NTAboutBox: TNTAboutBox;
    StatusBar: TStatusBar;
    pnlRight: TPanel;
    pnlLeft: TPanel;
    vstSubscribed: TVirtualStringTree;
    ImageList1: TImageList;
    pomMessage: TPopupMenu;
    pomMessageSaveAttachment: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    pomArticles: TPopupMenu;
    pomArticlesGetArticle: TMenuItem;
    pomArticlesGetThread: TMenuItem;
    N1: TMenuItem;
    pomArticlesReplytoArticle: TMenuItem;
    Timer1: TTimer;
    pomArticlesPostNewArticle: TMenuItem;
    N2: TMenuItem;
    pomArticlesGetNewMessageHeaders: TMenuItem;
    pomArticlesDeleteMessages: TMenuItem;
    pnlMessage: TPanel;
    pomGroups: TPopupMenu;
    pomGroupsDeleteMessages: TMenuItem;
    N3: TMenuItem;
    pomGroupsRefreshGroupList: TMenuItem;
    pomGroupsShowNewsgroupList: TMenuItem;
    N4: TMenuItem;
    pomGroupsGetMessages: TMenuItem;
    pomGroupsUnsubscribe: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    FindDialog1: TFindDialog;
    pomMessageFindinMessage: TMenuItem;
    N5: TMenuItem;
    pomGroupsSearchMessages: TMenuItem;
    pomArticlesSearchMessages: TMenuItem;
    pomGroupsPostNewArticle: TMenuItem;
    alMain: TActionList;
    actAccountShowNewsgroupList: TAction;
    actAccountRefreshGroupList: TAction;
    actArticleGetMessageBody: TAction;
    actArticleReplyToMessage: TAction;
    actArticlePostNewMessage: TAction;
    actArticleGetThread: TAction;
    actArticleMarkMessageAsRead: TAction;
    actArticleMarkThreadAsRead: TAction;
    actArticleDeleteThread: TAction;
    actEditCopy: TEditCopy;
    actEditDelete: TAction;
    actFileExit: TFileExit;
    actHelpAbout: TAction;
    actHelpContents: THelpContents;
    actHelpTopicSearch: THelpTopicSearch;
    actMessageSaveAttachment: TAction;
    actNewsgroupDeleteMessages: TAction;
    actNewsgroupGetMessages: TAction;
    actNewsgroupUnsubscribe: TAction;
    actSearchFindInMessage: TAction;
    actSearchSearchMessages: TAction;
    actToolsAccounts: TAction;
    actToolsReconnect: TAction;
    actToolsOptions: TAction;
    actToolsBatches: TAction;
    actViewHeadersNone: TAction;
    actViewHeadersShort: TAction;
    actViewHeadersFull: TAction;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditDelete: TMenuItem;
    mnuView: TMenuItem;
    mnuViewHeaders: TMenuItem;
    mnuViewHeadersNone: TMenuItem;
    mnuViewHeadersShort: TMenuItem;
    mnuViewHeadersFull: TMenuItem;
    mnuSearch: TMenuItem;
    mnuSearchSearchMessages: TMenuItem;
    mnuSearchFindInMessage: TMenuItem;
    N6: TMenuItem;
    mnuFileSaveAttachment: TMenuItem;
    mnuAccount: TMenuItem;
    mnuAccountShowNewsgroupList: TMenuItem;
    mnuAccountRefreshGroupList: TMenuItem;
    mnuNewsgroup: TMenuItem;
    mnuNewsgroupGetMessages: TMenuItem;
    mnuNewsgroupDeleteMessages: TMenuItem;
    N8: TMenuItem;
    mnuNewsgroupUnsubscribe: TMenuItem;
    mnuMessage: TMenuItem;
    mnuMessageGetMessageBody: TMenuItem;
    mnuMessageMarkMessageAsRead: TMenuItem;
    N9: TMenuItem;
    mnuMessageGetThread: TMenuItem;
    mnuMessageDeleteThread: TMenuItem;
    mnuMessageMarkThreadsasRead: TMenuItem;
    N10: TMenuItem;
    mnuMessageReplytoMessage: TMenuItem;
    mnuMessagePostNewMessage: TMenuItem;
    mnuTools: TMenuItem;
    mnuToolsBatches: TMenuItem;
    N11: TMenuItem;
    mnuToolsAccounts: TMenuItem;
    mnuToolsOptions: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpContents: TMenuItem;
    mnuHelpopicSearch: TMenuItem;
    N12: TMenuItem;
    mnuHelpAbout: TMenuItem;
    PersistentPosition: TPersistentPosition;
    MarkMessageAsRead1: TMenuItem;
    MarkThreadsasRead1: TMenuItem;
    DeleteThread1: TMenuItem;
    N13: TMenuItem;
    tbMenu: TToolBar;
    mnuBtnFile: TToolButton;
    mnuBtnEdit: TToolButton;
    mnuBtnView: TToolButton;
    mnuBtnSearch: TToolButton;
    mnuBtnAccount: TToolButton;
    mnuBtnNewsgroup: TToolButton;
    mnuBtnMessages: TToolButton;
    mnuBtnTools: TToolButton;
    mnuBtnHelp: TToolButton;
    tbMain: TToolBar;
    btnReconnect: TToolButton;
    tbs0: TToolButton;
    btnGetMessages: TToolButton;
    tbs2: TToolButton;
    btnPostNew: TToolButton;
    btnPostReply: TToolButton;
    tbs3: TToolButton;
    btnFind: TToolButton;
    btnSearch: TToolButton;
    tbs4: TToolButton;
    btnBozoAuthor: TToolButton;
    actArticleExpandThread: TAction;
    actArticleExpandAllThreads: TAction;
    ExpandThread1: TMenuItem;
    ExpandAllThreads1: TMenuItem;
    ExpandThread2: TMenuItem;
    ExpandAllThreads2: TMenuItem;
    actSearchFindNextUnreadMessageToMe: TAction;
    FindNextUnreadMessageToMe1: TMenuItem;
    actAccountProperties: TAction;
    AccountProperties1: TMenuItem;
    EditSelectAll1: TEditSelectAll;
    SelectAll1: TMenuItem;
    Copy1: TMenuItem;
    N14: TMenuItem;
    SelectAll2: TMenuItem;
    actViewMessagesNormal: TAction;
    actViewMessagesRawText: TAction;
    actViewMessagesRawMessages: TAction;
    mnuViewMessages: TMenuItem;
    Normal1: TMenuItem;
    RawTextParts1: TMenuItem;
    Raw1: TMenuItem;
    FindNextUnreadMessageToMe2: TMenuItem;
    N15: TMenuItem;
    FindNextUnreadMessageToMe3: TMenuItem;
    N16: TMenuItem;
    actArticleReplyByMail: TAction;
    ReplyByMail1: TMenuItem;
    ReplyByMail2: TMenuItem;
    btnMailReply: TToolButton;
    actToolsDisconnectAll: TAction;
    N17: TMenuItem;
    DisconnectAll1: TMenuItem;
    actToolsLoadTestMessage: TAction;
    est1: TMenuItem;
    LoadTestMessage1: TMenuItem;
    OpenDialog1: TOpenDialog;
    pnlQueuedRequests: TPanel;
    vstQueuedRequests: TVirtualStringTree;
    spltQueuedRequests: TExSplitter;
    pnlQueuedRequestsHeader: TPanel;
    spPauseRequests: TSpeedButton;
    actNewsgroupMarkAllMessagesAsRead: TAction;
    MarkAllMessagesAsRead1: TMenuItem;
    MarkAllMessagesAsRead2: TMenuItem;
    actArticleCollapseThread: TAction;
    actArticleCollapseAllThreads: TAction;
    CollapsThread1: TMenuItem;
    CollapseAllThreads1: TMenuItem;
    CollapsThread2: TMenuItem;
    CollapseAllThreads2: TMenuItem;
    actToolsForensicMode: TAction;
    ForensicMode1: TMenuItem;
    MarkAllMessagesAsRead3: TMenuItem;
    actArticleNextUnread: TAction;
    actArticleGotoPrevious: TAction;
    btnPrevious: TToolButton;
    btnNext: TToolButton;
    tbs1: TToolButton;
    actToolsNewsgroupStatistics: TAction;
    NewsgroupStatistics1: TMenuItem;
    pomTrayMenu: TPopupMenu;
    Exit1: TMenuItem;
    actTrayOpen: TAction;
    actTrayExit: TAction;
    N18: TMenuItem;
    Open1: TMenuItem;
    pomQueuedRequests: TPopupMenu;
    actQRDelete: TAction;
    actQREdit: TAction;
    EditQueuedPost1: TMenuItem;
    DeleteRequest1: TMenuItem;
    actQRPause: TAction;
    N19: TMenuItem;
    Pause1: TMenuItem;
    TrayIcon1: TTrayIcon;
    StandardSystemMenu1: TStandardSystemMenu;
    actArticleFlag: TAction;
    FlagInterestingMessages1: TMenuItem;
    FlagInterestingMessages2: TMenuItem;
    actSearchFindFlagged: TAction;
    FindNextInterestingMessage1: TMenuItem;
    ProgressBar1: TProgressBar;
    N7: TMenuItem;
    AccountProperties2: TMenuItem;
    actAccountAdd: TAction;
    actAccountAdd1: TMenuItem;
    AddNewAccount1: TMenuItem;
    actAccountRemove: TAction;
    RemoveAccount1: TMenuItem;
    RemoveAccount2: TMenuItem;
    actToolsMessagebaseManagement: TAction;
    MessagebaseManagement1: TMenuItem;
    N20: TMenuItem;
    mnuServerAdministration: TMenuItem;
    actToolsAdminCreateGroup: TAction;
    actToolsAdminRemoveGroup: TAction;
    CreateNewsgroup1: TMenuItem;
    RemoveGroup1: TMenuItem;
    actToolsAdminXanaNewzLoggingOn: TAction;
    actToolsAdminXanaNewzGetLog: TAction;
    actToolsAdminXanaNewzLoggingOff: TAction;
    XanaNewz1: TMenuItem;
    N21: TMenuItem;
    urnOffLogging1: TMenuItem;
    urnOfnLogging1: TMenuItem;
    N22: TMenuItem;
    GetLog1: TMenuItem;
    actArticleCancel: TAction;
    CancelMessage1: TMenuItem;
    CancelMessage2: TMenuItem;
    actMessageDelete: TAction;
    DeleteMessage1: TMenuItem;
    DeleteMessage2: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    actNewsgroupProperties: TAction;
    N26: TMenuItem;
    Properties1: TMenuItem;
    N27: TMenuItem;
    NewgroupProperties1: TMenuItem;
    actNewsgroupGetMessagesDefault: TAction;
    actToolsPurgeDeletedMessages: TAction;
    PurgeDeletedMessagesNow1: TMenuItem;
    QuickGetMessages1: TMenuItem;
    QuickGetMessages2: TMenuItem;
    QuickGetMessages3: TMenuItem;
    PurgeDeletedMessagesNow3: TMenuItem;
    N28: TMenuItem;
    N29: TMenuItem;
    actSearchFindKeyword1: TAction;
    actSearchFindKeyword2: TAction;
    actSearchFindKeyword3: TAction;
    actSearchFindKeyword4: TAction;
    actSearchFindKeyword5: TAction;
    actSearchFindKeyword6: TAction;
    actSearchFindKeyword7: TAction;
    actSearchFindKeyword8: TAction;
    FindMessageWithKeyword11: TMenuItem;
    FindMessageWithKeyword21: TMenuItem;
    FindMessageWithKeyword31: TMenuItem;
    FindMessageWithKeyword41: TMenuItem;
    FindMessageWithKeyword51: TMenuItem;
    FindMessageWithKeyword61: TMenuItem;
    FindMessageWithKeyword71: TMenuItem;
    FindMessageWithKeyword81: TMenuItem;
    N30: TMenuItem;
    ReplytoMessage1: TMenuItem;
    actToolsDecodePerformance: TAction;
    estDecodeMessagePerformance1: TMenuItem;
    actGetEverything: TAction;
    btnQuickGet: TToolButton;
    N31: TMenuItem;
    actFileNewFolder: TAction;
    NewFolder1: TMenuItem;
    N32: TMenuItem;
    N33: TMenuItem;
    pomFolders: TPopupMenu;
    NewArchivedMessagesFolder1: TMenuItem;
    actFolderRename: TAction;
    actFolderDelete: TAction;
    RenameArchivedMessagesFolder1: TMenuItem;
    DeleteArchivedMessagesFolder1: TMenuItem;
    RenameArchivedMessagesFolder2: TMenuItem;
    DeleteArchivedMessagesFolder2: TMenuItem;
    actFolderClear: TAction;
    ClearArchivedMessagesFolder1: TMenuItem;
    ClearArchivedMessagesFolder2: TMenuItem;
    actFolderReloadMessages: TAction;
    Reloadmessagesfromfolder1: TMenuItem;
    Reloadmessagesfromfolder2: TMenuItem;
    actFolderReloadAllMessages: TAction;
    N34: TMenuItem;
    Reloadallmessagesselectedfromfolder1: TMenuItem;
    N35: TMenuItem;
    Reloadallmessagesselectedfromfolder2: TMenuItem;
    N36: TMenuItem;
    actQRClear: TAction;
    Clearallrequests1: TMenuItem;
    N37: TMenuItem;
    RefreshGroupList1: TMenuItem;
    Headers1: TMenuItem;
    None1: TMenuItem;
    Short1: TMenuItem;
    Full1: TMenuItem;
    Messages1: TMenuItem;
    Normal2: TMenuItem;
    Raw2: TMenuItem;
    RawTextParts2: TMenuItem;
    actROT13: TAction;
    DecodeROT13Text1: TMenuItem;
    actToolsIdentities: TAction;
    Identities1: TMenuItem;
    actViewHeadersCustom: TAction;
    Custom1: TMenuItem;
    Custom2: TMenuItem;
    pnlMessageHeader: TPanel;
    pnlDetailsBar: TPanel;
    pnlCharset: TPanel;
    cbCharset: TComboBox;
    actToolsFlickerTest: TAction;
    estFlicker1: TMenuItem;
    actEditCopyLink: TAction;
    CopyLinktoClipboard1: TMenuItem;
    actEditSelectThread: TAction;
    actEditSelectSubthread: TAction;
    N38: TMenuItem;
    N39: TMenuItem;
    SelectThread1: TMenuItem;
    SelectSubThread1: TMenuItem;
    actFileExportSelected: TAction;
    actFileImportArticles: TAction;
    ExportSelectedArticles1: TMenuItem;
    ImportArticles1: TMenuItem;
    N40: TMenuItem;
    dlgSaveArticle: TSaveDialog;
    dlgImportArticles: TOpenDialog;
    actFilePrinterSetup: TAction;
    actFilePrint: TAction;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintDialog1: TPrintDialog;
    N41: TMenuItem;
    SetupPrinter1: TMenuItem;
    Print1: TMenuItem;
    actViewMessagesImagesOnly: TAction;
    ImagesOnly1: TMenuItem;
    ImagesOnly2: TMenuItem;
    Print2: TMenuItem;
    Print3: TMenuItem;
    actViewGroupMultipart: TAction;
    N42: TMenuItem;
    GroupMultipartMessages1: TMenuItem;
    actToolsMailAccounts: TAction;
    MailAccounts1: TMenuItem;
    actToolsToggleLogging: TAction;
    Logging1: TMenuItem;
    actViewHideReadMessages: TAction;
    HideReadMessages1: TMenuItem;
    actToolsAudiblePerformanceCues: TAction;
    AudiblePerformanceCues1: TMenuItem;
    actNewsgroupSaveAllAttachments: TAction;
    SaveAllAttachments1: TMenuItem;
    actFileExportCompressed: TAction;
    ExportCompressedMessages1: TMenuItem;
    dlgImportCompressed: TOpenDialog;
    actFileImportCompressed: TAction;
    ImportCompressedMessages1: TMenuItem;
    actArticleChangeSubject: TAction;
    actToolsTestCrash: TAction;
    CrashXanaNews1: TMenuItem;
    actToolsRunSelectedBatch: TAction;
    actMessageAddToBozoBin: TAction;
    tbs5: TToolButton;
    pnlArticles: TPanel;
    vstArticles: TVirtualStringTree;
    spltBookmark: TExSplitter;
    spltArticles: TExSplitter;
    actViewShowBookmarkPane: TAction;
    N43: TMenuItem;
    ShowBookmarkPane1: TMenuItem;
    actReverseSelectedText: TAction;
    ReverseSelectedText1: TMenuItem;
    pnlBookmark: TPanel;
    vstBookmark: TVirtualStringTree;
    tbBookmark: TToolBar;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    actBookmarksAdd: TAction;
    actBookmarksRemove: TAction;
    actBookmarksCreate: TAction;
    actBookmarksDelete: TAction;
    mnuBtnBookmarks: TToolButton;
    mnuBookmarks: TMenuItem;
    AddArticletoBookmarkSet1: TMenuItem;
    RemoveArticlefromBookmarkSet1: TMenuItem;
    N44: TMenuItem;
    AddArticletoBookmarkSet2: TMenuItem;
    DeleteBookmarkSet1: TMenuItem;
    cbBookmark: TComboBox;
    Bevel1: TBevel;
    actBookmarksClearAll: TAction;
    ClearAllBookmarks1: TMenuItem;
    actSearchFindNextReplyToMe: TAction;
    FindNextUnreadReplytoMe1: TMenuItem;
    FindNextUnreadReplytoMe2: TMenuItem;
    FindNextInterestingMessage2: TMenuItem;
    FindNextUnreadReplytoMe3: TMenuItem;
    ChangeSubject1: TMenuItem;
    actAccountExpandAll: TAction;
    actAccountCollapseAll: TAction;
    N45: TMenuItem;
    ExpandAllAccounts1: TMenuItem;
    CollapseAllAccounts1: TMenuItem;
    N46: TMenuItem;
    ExpandAllAccounts2: TMenuItem;
    CollapseAllAccounts2: TMenuItem;
    actToolsSendOutbasket: TAction;
    SendOutbasket1: TMenuItem;
    SendOutbasket2: TMenuItem;
    btnPrint: TToolButton;
    SplitterPanel1: TSplitterPanel;
    SplitterPanel2: TSplitterPanel;
    actViewSubscribedGroupsPane: TAction;
    ShowSubscribedGroupsPane1: TMenuItem;
    actFileMoveMessagebase: TAction;
    N47: TMenuItem;
    MoveMessagebase1: TMenuItem;
    actMessageCopyXFace: TAction;
    CopyXFacetoClipboard1: TMenuItem;
    actArticleIgnoreBranch: TAction;
    IgnoreBranch1: TMenuItem;
    IgnoreBranch2: TMenuItem;
    actArticleRetrieveParentMessages: TAction;
    RetrieveParentMessages1: TMenuItem;
    RetrieveParentMessages2: TMenuItem;
    actFolderReindex: TAction;
    ReIndexArticleFolder1: TMenuItem;
    ReIndexArticleFolder2: TMenuItem;
    mnuCopyURLToClipboard: TMenuItem;
    actFolderFindMessage: TAction;
    FindFolderMessageinMessageBase1: TMenuItem;
    FindFolderMessageinMessageBase2: TMenuItem;
    actToolsResetHighWaterMark: TAction;
    ResetHighWaterMark1: TMenuItem;
    actToolsTestMarkAllUnread: TAction;
    MarkAllArticlesasUnread1: TMenuItem;
    actArticleCombineDecode: TAction;
    CombineDecodeSelectedMessages1: TMenuItem;
    CombineDecodeSelectedMessages2: TMenuItem;
    SplitterPanel3: TSplitterPanel;
    GotoSelectedURL1: TMenuItem;
    actArticleGoToNext: TAction;
    actArticleGoToNextDontMark: TAction;
    actViewShowSecrets: TAction;
    ShowSecretAccountsandNewsgroups1: TMenuItem;
    actSearchFindFlaggedUnread: TAction;
    actSearchFindFlaggedInNewThread: TAction;
    N23: TMenuItem;
    FindNextInterestingMessage3: TMenuItem;
    FindNextInterestingMessageinAnotherThread1: TMenuItem;
    actSearchFindAnyKeyword: TAction;
    FindMessageWithAnyKeyword1: TMenuItem;
    spFixedFont: TSpeedButton;
    actMessageToggleFixedFont: TAction;
    actArticleSaveHTML: TAction;
    actArticleCopyHTML: TAction;
    dlgSaveHTML: TSaveDialog;
    CopytoclipboardasHTML1: TMenuItem;
    N48: TMenuItem;
    N49: TMenuItem;
    SaveArticleasHTML1: TMenuItem;
    actMessageExecuteAttachment: TAction;
    SaveExecuteDialog1: TSaveDialog;
    actArticleIgnore: TAction;
    IgnoreUnignoreSelectedArticles1: TMenuItem;
    actViewHideIgnoredMessages: TAction;
    HideIgnoredMessages1: TMenuItem;
    IgnoreUnignoreSelectedArticles2: TMenuItem;
    actViewFindOnInternet: TAction;
    actViewFindOnInternet1: TMenuItem;
    FindMessageonInternet1: TMenuItem;
    actArticleMarkBranchAsRead: TAction;
    MarkBranchasReadUnread1: TMenuItem;
    MarkBranchasReadUnread2: TMenuItem;
    Message1: TMenuItem;
    Branch1: TMenuItem;
    hread1: TMenuItem;
    N50: TMenuItem;
    N52: TMenuItem;
    Message2: TMenuItem;
    Branch2: TMenuItem;
    hread2: TMenuItem;
    N53: TMenuItem;
    AddUsertoBozoBin1: TMenuItem;
    AddUsertoBozoBin2: TMenuItem;
    actArticleMarkMessageAsInteresting: TAction;
    MarkUnmarkMessageasInteresting1: TMenuItem;
    MarkUnmarkMessageasInteresting2: TMenuItem;
    actToolsTestReadlnDelay: TAction;
    SlowInternetConnection1: TMenuItem;
    actSearchFindNoReplies: TAction;
    actSearchFindUnreadNoReplies: TAction;
    FindNextUnreadMessagewithNoReplies1: TMenuItem;
    FindNextMessageithNoReplies1: TMenuItem;
    actArticleMarkThreadAsInteresting: TAction;
    actArticleMarkThreadAsInteresting1: TMenuItem;
    MarkUnmarkThreadasInteresting1: TMenuItem;
    actViewFindTextOnInternet: TAction;
    FindTextonInternet1: TMenuItem;
    actnewsgroupMakeDormant: TAction;
    MakeDormant1: TMenuItem;
    MakeDormant2: TMenuItem;
    actViewAutofitImages: TAction;
    AutofitImages1: TMenuItem;
    ilDisabled: TImageList;
    pnlBatchBar: TPanel;
    cbBatches: TComboBox;
    cbMain: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    pnlSearchBar: TPanel;
    actViewShowSearchBar: TAction;
    cbSearchBarTarget: TComboBox;
    cbSearchBarOp: TComboBox;
    edSearchBarText: TEdit;
    rbSearchbarSearch: TRadioButton;
    rbBookmark: TRadioButton;
    rbFilter: TRadioButton;
    btnGo: TButton;
    ilMainLarge: TImageList;
    ImageList3_NotUsedYet: TImageList;
    ImageList4_NotUsedYet: TImageList;
    actToolsSearchbarGo: TAction;
    mnuViewToolbars: TMenuItem;
    ShowSearchBar2: TMenuItem;
    actViewShowBatchBar: TAction;
    actViewShowToolbar: TAction;
    actViewToolbarSmallImages: TAction;
    ilDisabledLarge: TImageList;
    N51: TMenuItem;
    SmallImages1: TMenuItem;
    ShowSearchBar1: TMenuItem;
    ShowToolBar1: TMenuItem;
    actViewToolbarCaptions: TAction;
    actViewToolbarCaptions1: TMenuItem;
    N54: TMenuItem;
    actViewHideMessagesNotToMe: TAction;
    HideMessagesNotToMe1: TMenuItem;
    N55: TMenuItem;
    pomSortGroupsByName: TMenuItem;
    actAccountSortGroupsByName: TAction;
    spGoToWebForum: TSpeedButton;
    actArticleIgnoreThread: TAction;
    IgnoreUnignoreThread1: TMenuItem;
    IgnoreUnignoreThread2: TMenuItem;
    procedure ApplicationEvents1Activate(Sender: TObject);
    procedure ApplicationEvents1Deactivate(Sender: TObject);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    function ApplicationEvents1Help(Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean;
    procedure ApplicationEvents1Hint(Sender: TObject);
    procedure FindDialog1Close(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GotoSelectedURL1Click(Sender: TObject);
    procedure MessageScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MessageScrollBox1DoubleClick(Sender: TObject);
    procedure PersistentPositionGetSettingsClass(Owner: TObject; var SettingsClass: TExSettingsClass);
    procedure PersistentPositionGetSettingsFile(Owner: TObject; var fileName: string);
    procedure SplitterPanel1DockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    procedure SplitterPanel1DockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SplitterPanel2DockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SplitterPanel3DockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure StatusBarClick(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure actAccountAddExecute(Sender: TObject);
    procedure actAccountCollapseAllExecute(Sender: TObject);
    procedure actAccountExpandAllExecute(Sender: TObject);
    procedure actAccountPropertiesExecute(Sender: TObject);
    procedure actAccountRefreshGroupListExecute(Sender: TObject);
    procedure actAccountRemoveExecute(Sender: TObject);
    procedure actAccountShowNewsgroupListExecute(Sender: TObject);
    procedure actAccountSortGroupsByNameExecute(Sender: TObject);

    procedure actArticleCancelExecute(Sender: TObject);
    procedure actArticleChangeSubjectExecute(Sender: TObject);
    procedure actArticleCombineDecodeExecute(Sender: TObject);
    procedure actArticleCollapseAllThreadsExecute(Sender: TObject);
    procedure actArticleCollapseThreadExecute(Sender: TObject);
    procedure actArticleCopyHTMLExecute(Sender: TObject);
    procedure actArticleDeleteArticleExecute(Sender: TObject);
    procedure actArticleDeleteThreadExecute(Sender: TObject);
    procedure actArticleExpandAllThreadsExecute(Sender: TObject);
    procedure actArticleExpandThreadExecute(Sender: TObject);
    procedure actArticleFlagExecute(Sender: TObject);
    procedure actArticleGetMessageBodyExecute(Sender: TObject);
    procedure actArticleGetThreadExecute(Sender: TObject);
    procedure actArticleGoToNextDontMarkExecute(Sender: TObject);
    procedure actArticleGoToNextExecute(Sender: TObject);
    procedure actArticleGotoPreviousExecute(Sender: TObject);
    procedure actArticleIgnoreBranchExecute(Sender: TObject);
    procedure actArticleIgnoreExecute(Sender: TObject);
    procedure actArticleIgnoreThreadExecute(Sender: TObject);
    procedure actArticleMarkAsReadExecute(Sender: TObject);
    procedure actArticleMarkBranchAsReadExecute(Sender: TObject);
    procedure actArticleMarkMessageAsInterestingExecute(Sender: TObject);
    procedure actArticleMarkThreadAsInterestingExecute(Sender: TObject);
    procedure actArticleMarkThreadReadExecute(Sender: TObject);
    procedure actArticleNextUnreadExecute(Sender: TObject);
    procedure actArticlePostNewArticleExecute(Sender: TObject);
    procedure actArticleReplyByMailExecute(Sender: TObject);
    procedure actArticleReplyToArticleExecute(Sender: TObject);
    procedure actArticleRetrieveParentMessagesExecute(Sender: TObject);
    procedure actArticleSaveHTMLExecute(Sender: TObject);

    procedure actBatchBatchesExecute(Sender: TObject);

    procedure actBookmarksAddExecute(Sender: TObject);
    procedure actBookmarksClearAllExecute(Sender: TObject);
    procedure actBookmarksCreateExecute(Sender: TObject);
    procedure actBookmarksDeleteExecute(Sender: TObject);
    procedure actBookmarksRemoveExecute(Sender: TObject);

    procedure actEditCopyLinkExecute(Sender: TObject);
    procedure actEditSelectThreadExecute(Sender: TObject);
    procedure actEditSelectSubthreadExecute(Sender: TObject);

    procedure actGetEverythingExecute(Sender: TObject);

    procedure actFileExportCompressedExecute(Sender: TObject);
    procedure actFileExportSelectedExecute(Sender: TObject);
    procedure actFileImportArticlesExecute(Sender: TObject);
    procedure actFileImportCompressedExecute(Sender: TObject);
    procedure actFileMoveMessagebaseExecute(Sender: TObject);
    procedure actFileNewFolderExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFilePrinterSetupExecute(Sender: TObject);

    procedure actFolderClearExecute(Sender: TObject);
    procedure actFolderDeleteExecute(Sender: TObject);
    procedure actFolderFindMessageExecute(Sender: TObject);
    procedure actFolderReindexExecute(Sender: TObject);
    procedure actFolderReloadAllMessagesExecute(Sender: TObject);
    procedure actFolderReloadMessagesExecute(Sender: TObject);
    procedure actFolderRenameExecute(Sender: TObject);

    procedure actHelpAboutExecute(Sender: TObject);

    procedure actMessageAddToBozoBinExecute(Sender: TObject);
    procedure actMessageCopyXFaceExecute(Sender: TObject);
    procedure actMessageDeleteExecute(Sender: TObject);
    procedure actMessageExecuteAttachmentExecute(Sender: TObject);
    procedure actMessageSaveAttachmentExecute(Sender: TObject);
    procedure actMessageToggleFixedFontExecute(Sender: TObject);

    procedure actNewsgroupDeleteMessagesExecute(Sender: TObject);
    procedure actNewsgroupGetMessagesDefaultExecute(Sender: TObject);
    procedure actNewsgroupGetMessagesExecute(Sender: TObject);
    procedure actNewsgroupMarkAllMessagesAsReadExecute(Sender: TObject);
    procedure actNewsgroupMakeDormantExecute(Sender: TObject);
    procedure actNewsgroupPropertiesExecute(Sender: TObject);
    procedure actNewsgroupSaveAllAttachmentsExecute(Sender: TObject);
    procedure actNewsgroupUnsubscribeExecute(Sender: TObject);

    procedure actReverseSelectedTextExecute(Sender: TObject);
    procedure actROT13Execute(Sender: TObject);

    procedure actSearchFindAnyKeywordExecute(Sender: TObject);
    procedure actSearchFindExecute(Sender: TObject);
    procedure actSearchFindFlaggedExecute(Sender: TObject);
    procedure actSearchFindFlaggedInNewThreadExecute(Sender: TObject);
    procedure actSearchFindFlaggedUnreadExecute(Sender: TObject);
    procedure actSearchFindKeywordNExecute(Sender: TObject);
    procedure actSearchFindMessageExecute(Sender: TObject);
    procedure actSearchFindNextReplyToMeExecute(Sender: TObject);
    procedure actSearchFindNextUnreadMessageToMeExecute(Sender: TObject);
    procedure actSearchFindNoRepliesExecute(Sender: TObject);
    procedure actSearchFindUnreadNoRepliesExecute(Sender: TObject);

    procedure actToolsAccountsExecute(Sender: TObject);
    procedure actToolsAdminCreateGroupExecute(Sender: TObject);
    procedure actToolsAdminRemoveGroupExecute(Sender: TObject);
    procedure actToolsAudiblePerformanceCuesExecute(Sender: TObject);
    procedure actToolsDecodePerformanceExecute(Sender: TObject);
    procedure actToolsDisconnectAllExecute(Sender: TObject);
    procedure actToolsForensicModeExecute(Sender: TObject);
    procedure actToolsFlickerTestExecute(Sender: TObject);
    procedure actToolsIdentitiesExecute(Sender: TObject);
    procedure actToolsLoadTestMessageExecute(Sender: TObject);
    procedure actToolsMailAccountsExecute(Sender: TObject);
    procedure actToolsMessagebaseManagementExecute(Sender: TObject);
    procedure actToolsNewsgroupStatisticsExecute(Sender: TObject);
    procedure actToolsOptionsExecute(Sender: TObject);
    procedure actToolsPurgeDeletedMessagesExecute(Sender: TObject);
    procedure actToolsReconnectExecute(Sender: TObject);
    procedure actToolsRunSelectedBatchExecute(Sender: TObject);
    procedure actToolsSearchbarGoExecute(Sender: TObject);
    procedure actToolsResetHighWaterMarkExecute(Sender: TObject);
    procedure actToolsSendOutbasketExecute(Sender: TObject);
    procedure actToolsTestCrashExecute(Sender: TObject);
    procedure actToolsTestMarkAllUnreadExecute(Sender: TObject);
    procedure actToolsTestReadlnDelayExecute(Sender: TObject);
    procedure actToolsToggleLoggingExecute(Sender: TObject);

    procedure actQRClearExecute(Sender: TObject);
    procedure actQRDeleteExecute(Sender: TObject);
    procedure actQREditExecute(Sender: TObject);
    procedure actQRPauseExecute(Sender: TObject);

    procedure actTrayExitExecute(Sender: TObject);
    procedure actTrayOpenExecute(Sender: TObject);

    procedure actViewAutofitImagesExecute(Sender: TObject);
    procedure actViewFindOnInternetExecute(Sender: TObject);
    procedure actViewFindTextOnInternetExecute(Sender: TObject);
    procedure actViewGroupMultipartExecute(Sender: TObject);
    procedure actViewHeadersCustomExecute(Sender: TObject);
    procedure actViewHeadersFullExecute(Sender: TObject);
    procedure actViewHeadersNoneExecute(Sender: TObject);
    procedure actViewHeadersShortExecute(Sender: TObject);
    procedure actViewHideIgnoredMessagesExecute(Sender: TObject);
    procedure actViewHideMessagesNotToMeExecute(Sender: TObject);
    procedure actViewHideReadMessagesExecute(Sender: TObject);
    procedure actViewMessagesImagesOnlyExecute(Sender: TObject);
    procedure actViewMessagesNormalExecute(Sender: TObject);
    procedure actViewMessagesRawMessagesExecute(Sender: TObject);
    procedure actViewMessagesRawTextExecute(Sender: TObject);
    procedure actViewShowBatchBarExecute(Sender: TObject);
    procedure actViewShowBookmarkPaneExecute(Sender: TObject);
    procedure actViewShowSecretsExecute(Sender: TObject);
    procedure actViewShowSearchBarExecute(Sender: TObject);
    procedure actViewShowToolbarExecute(Sender: TObject);
    procedure actViewSubscribedGroupsPaneExecute(Sender: TObject);
    procedure actViewToolbarCaptionsExecute(Sender: TObject);
    procedure actViewToolbarSmallImagesExecute(Sender: TObject);

    procedure cbBatchesSelect(Sender: TObject);
    procedure cbBookmarkChange(Sender: TObject);
    procedure cbCharsetChange(Sender: TObject);
    procedure cbSearchBarTargetChange(Sender: TObject);

    procedure edSearchBarTextExit(Sender: TObject);
    procedure edSearchBarTextEnter(Sender: TObject);
    procedure edSearchBarTextKeyPress(Sender: TObject; var Key: Char);

    procedure mnuCopyURLToClipboardClick(Sender: TObject);
    procedure mnuViewHeadersClick(Sender: TObject);
    procedure mnuViewMessagesClick(Sender: TObject);
    procedure mnuToolsClick(Sender: TObject);
    procedure mnuViewClick(Sender: TObject);

    procedure pnlLeftStartDock(Sender: TObject; var DragObject: TDragDockObject);

    procedure pomGroupsPopup(Sender: TObject);
    procedure pomMessagePopup(Sender: TObject);
    procedure pomQueuedRequestsPopup(Sender: TObject);

    procedure spFixedFontClick(Sender: TObject);
    procedure spGoToWebForumClick(Sender: TObject);
    procedure spPauseRequestsClick(Sender: TObject);

    procedure tbMainCustomized(Sender: TObject);
    procedure tbMainCustomizeReset(Sender: TObject);

    procedure vstArticlesAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure vstArticlesAfterItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
    procedure vstArticlesAfterItemPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
    procedure vstArticlesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstArticlesClick(Sender: TObject);
    procedure vstArticlesColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure vstArticlesDblClick(Sender: TObject);
    procedure vstArticlesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstArticlesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
//    procedure vstArticlesHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstArticlesHeaderMouseUp(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstArticlesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer);
    procedure vstArticlesGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure vstArticlesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string);
    procedure vstArticlesHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure vstArticlesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstArticlesInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vstArticlesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstArticlesPaintText(Sender: TBaseVirtualTree; const Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure vstArticlesResize(Sender: TObject);

    procedure vstBookmarkColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure vstBookmarkDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vstBookmarkDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstBookmarkFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstBookmarkGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstBookmarkHeaderMouseUp(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstBookmarkInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstBookmarkResize(Sender: TObject);

    procedure vstQueuedRequestsDblClick(Sender: TObject);
    procedure vstQueuedRequestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstQueuedRequestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstQueuedRequestsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstQueuedRequestsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

    procedure vstSubscribedAfterItemPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
    procedure vstSubscribedCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstSubscribedDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vstSubscribedDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstSubscribedEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstSubscribedEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstSubscribedExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstSubscribedDblClick(Sender: TObject);
    procedure vstSubscribedFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstSubscribedGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure vstSubscribedGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer);
    procedure vstSubscribedGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string);
    procedure vstSubscribedInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstSubscribedInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vstSubscribedKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstSubscribedNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure vstSubscribedPaintText(Sender: TBaseVirtualTree; const Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure pnlLeftEndDock(Sender, Target: TObject; X, Y: Integer);
  private
    fHeaderSortCol: Integer;
    fURL: string;
    fEditNameAllowed: Boolean;
    fLastFindSucceeded: Boolean;
    fColumnHeaderStatus: TColumnHeaderStatus;
    fBookmarkHeaderStatus: TColumnHeaderStatus;
    fOutstandingRequestCount: Integer;
    fOutstandingGetterCount: Integer;
    fOutstandingActiveGetterCount: Integer;
    fTestMessage: TmvMessage;
    fForensicMode: Boolean;
    fPrevArticleStack: TArticleStack;
    fNextArticleStack: TArticleStack;
    fPrevArticle: TArticleBase;
    fCanClose: Boolean;
    fEndSession: Boolean;
    fTrayed: Boolean;
    fHadUnread: Boolean;
    fSuppressSound: Boolean;
    fBatchRef: Integer;
    fTickCount: Integer;
    fSnapToFirstUnreadMessageAfterTray: Boolean;
    fWasMaximized: Boolean;
    fIteratorFailed: Boolean;
    fNonDormantIcon: TIcon;
    fInterestingIcon: TIcon;
    fGoofyIcon: TIcon;
    fInCollapse: Boolean;
    fModelessWindowList: TList;
    fPurgingGroupName: string;
    fPurgingAccountName: string;
    fPurgingMessageID: RawByteString;
    fPurgingFolder: Boolean;
    fReloadedList: TList;
    fFolderArticleHeader: TAnsiStrings;
    fDeferredCombineList: TObjectList;
    fSm: string;
    fSmMax: Word;
    fSmPos: word;
    fCaptureUpdatePanel: Boolean;
    fPanelTextRect: TRect;
    fCoUninitialize: Boolean;
    fLastFocusedArticleContainer: TArticleContainer;
    fLastFocusedAccount: TNNTPAccount;
    fUpdatingBookmark: Boolean;
    fRetrySetMsgFlag: Boolean;
    fPanelLeftDragOffset: TPoint;
    fPanelLeftWidth: Integer;
    fPanelLeftHeight: Integer;
    fPreForensicThreadOrder: TThreadOrder;
    fPreForensicSortOrder: TThreadSortOrder;
    fPreForensicSortDirection: TThreadSortDirection;
    fAutoMarkTicks: Integer;
    fExpandNode: PVirtualNode;
    fMedal: TImage;
    fDeservesMedals: string;
    fFindDialogShowing: Boolean;
    fInSearchBarText: Boolean;
    fRenamingFolder: Boolean;
    fClicked: Boolean;
    fNxStack: Boolean;
    fAutoGetMessages: Boolean;
//    fOldMonitorWindowProc: TWndMethod;
    fDontMarkOnLeave: Boolean;
    fOptionsFormActive: Boolean;

    procedure DisplayBookmarks(visible: Boolean);
    procedure PopulateSearchBarOpCombo;

    procedure CheckScheduledBatches;
    procedure CheckDeferredCombineList(article: TArticleBase);

    function GetFocusedAccount: TNNTPAccount;
    function GetFocusedGroup: TSubscribedGroup;
    function GetFocusedArticleFolder: TArticleFolder;
    function GetFocusedQAccount: TNNTPAccount;
    function GetFocusedQGroup: TArticleContainer;
    function GetFocusedFolderArticle: TFolderArticle;

    procedure DoOnArticlesChanged(Sender: TObject; group: TArticleContainer);
    procedure DoOnArticleChanged(Sender: TObject; article: TArticleBase);
    procedure DoOnClearArticle(Sender: TObject; article: TArticleBase);
    procedure DoOnStartArticle(Sender: TObject; article: TArticleBase);
    procedure DoOnArticleFailed(Sender: TObject; article: TArticleBase);
    procedure DoOnNewsgroupsChanged(Sender: TObject; account: TNNTPAccount);
    procedure DoOnNotifyError(Sender: TObject; const error: string);
    procedure DoOnNewGroups(Sender: TObject; account: TNNTPAccount);

    procedure DisplayArticleBody(article: TArticleBase);
    procedure RunBatch(batch: TNNTPBatch);

    procedure SearchMessageFormDestroy(Sender: TObject);
    procedure ModelessWindowFormDestroy(Sender: TObject);
    procedure ModelessWindowFormActivate(Sender: TObject);
    procedure ModelessWindowFormDeactivate(Sender: TObject);

    procedure InitArticlesRootNodeCount(ctnr: TArticleContainer);
    procedure Refresh_vstArticles(article: TArticleBase = nil);
    procedure Refresh_vstSubscribed;
    procedure Reinit_vstSubscribed(refresh: Boolean = True);
    procedure Refresh_vstQueuedRequests;

    procedure ResizeArticleHeader;
    procedure ResizeBookmarkHeader;

    function ForEachSelectedArticle(proc: TArticleIteratorProc; param: LPARAM = 0; expandedToo: Boolean = True): Integer;
    function ForEachSelectedBranch(proc: TArticleIteratorProc; param: LPARAM = 0; startAtRoot: Boolean = False): Integer;
    function ForEachArticleInThread(thread: TArticleBase; proc: TArticleIteratorProc; param: LPARAM = 0): Integer;
    function ForEachArticleInBranch(var branch: TArticleBase; proc: TArticleIteratorProc; param: LPARAM = 0): Integer;
    function ForEachSelectedGroup(proc: TGroupIteratorProc; purge: Boolean; param: LPARAM = 0): Integer;
    function ForEachGroupInSelectedAccount(proc: TGroupIteratorProc; purge: Boolean; param: LPARAM = 0): Integer;
    function ForEachSubscribedGroup(proc: TGroupIteratorProc; purge: Boolean; param: LPARAM = 0): Integer;
    function ForEachSelectedFolderArticle(proc: TFolderArticleIteratorProc; param: LPARAM = 0): Integer;
    function ForEachSelectedArticleFolder(proc: TArticleFolderIteratorProc; param: LPARAM = 0): Integer;

    procedure DoDeleteArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoMarkAsCancelledArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoCheckFromMe(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    function DoDeleteArticles(group: TSubscribedGroup; param: LPARAM): Boolean;
    procedure DoMarkAsReadArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoFlagArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoIgnoreArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoCancelArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    function DoCancelGroup(group: TSubscribedGroup; param: LPARAM): Boolean;
    function DoMarkArticlesAsRead(group: TSubscribedGroup; param: LPARAM): Boolean;
    procedure DoGetArticleBody(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoGetArticleThread(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    function DoDeleteThreads(group: TSubscribedGroup; param: LPARAM): Boolean;
    function DoMarkThreadsAsRead(group: TSubscribedGroup; param: LPARAM): Boolean;
    procedure DoCheckArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoCheckArticleRead(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoCheckArticleFlagged(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoCheckArticleIgnored(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoMoveToSelectedFolder(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoMoveToBookmark(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoReloadFolderArticle(article: TFolderArticle; param: LPARAM);
    function DoGetMessages(group: TSubscribedGroup; params: LPARAM): Boolean;
    procedure DoSaveArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoSaveAttachments(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    procedure DoOnExporterProgress(Sender: TObject; Pos, max: Integer; const group: string);
    procedure DoAddArticleToList(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
    function DoAddGroupToList(group: TSubscribedGroup; param: LPARAM): Boolean;
    function DoAddFolderToList(folder: TArticleFolder; param: LPARAM): Boolean;

    function NextArticle(options: TNextArticleOptions; firstArticle: TArticleBase; skipThisThread: Boolean = False): Boolean;
    function GetNodeAccount(node: PVirtualNode): TNNTPAccount;
    function GetNodeArticle(node: PVirtualNode): TArticleBase;
    function GetNodeArticleFolder(node: PVirtualNode): TArticleFolder;
    function GetNodeArticleContainer(node: PVirtualNode): TArticleContainer;
    function GetNodeSubscribedGroup(node: PVirtualNode): TSubscribedGroup;
    function GetNodeObject(node: PVirtualNode): TObject;

    function GetArticleNode(article: TArticleBase): PVirtualNode;
    function GetArticleContainerNode(ctnr: TArticleContainer): PVirtualNode;
    function GetAccountNode(acct: TNNTPAccount): PVirtualNode;
    procedure GoToURL(url: string);

    function IsThreaded(ctnr: TArticleContainer): Boolean;
    procedure FullExpandThreads(ctnr: TArticleContainer; node: PVirtualNode);

    procedure CentralizeDisplay;
    procedure EnableShortcuts(enable: Boolean);
    procedure SetControlOptions(initfonts: Boolean);
    procedure SaveArticleHeaderColumns;
    procedure SaveBookmarkHeaderColumns;
    procedure ApplyControlOptions;
    function GetFirstvstSubscribedSelectedNode: PVirtualNode;
    procedure SelectArticleNode(Node: PVirtualNode);
    function Unsubscribe(group: TSubscribedGroup; param: LPARAM): Boolean;
    function MakeDormant(group: TSubscribedGroup; param: LPARAM): Boolean;

    procedure WmSetup(var Msg: TMessage); message WM_SETUP;
    procedure WmUnsubscribe(var Msg: TMessage); message WM_UNSUBSCRIBE;
    procedure WmGroupsChanging(var Msg: TMessage); message WM_GROUPSCHANGING;
    procedure WmGroupsChanged(var Msg: TMessage); message WM_GROUPSCHANGED;
    procedure WmAutoExpand(var Msg: TMessage); message WM_AUTOEXPAND;
    procedure WmQueryEndSession(var Msg: TMessage); message WM_QUERYENDSESSION;
    procedure WmCopyData(var msg: TwmCopYData); message WM_COPYDATA;
    procedure WmStatusMessage(var Msg: TMessage); message WM_STATUS;
    procedure WmRetrySetMsg(var msg: TMessage); message WM_RETRYSETMSG;
    procedure WmSpecialKey(var msg: TMessage); message WM_SPECIALKEY;
    procedure WmNameThread(var msg: TMessage); message WM_NAMETHREAD;
    procedure WmGetConnected(var msg: TMessage); message WM_GETCONNECTED;
    procedure WmShowNewsgroupList(var msg: TMessage); message WM_SHOWNEWSGROUPLIST;
    procedure WmApplyChanges(var msg: TMessage); message WM_APPLYCHANGES;
    procedure WmSize(var msg: TMessage); message WM_SIZE;
    procedure WmSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WmFirstTime(var Msg: TMessage); message WM_FIRSTTIME;

    procedure SaveArticleHeaderPositions;
    procedure SaveAttachment(mp: TmvMessagePart; const fileName: string; multipart: Boolean);
    procedure SaveMultipartAttachment(const fileName: string; articles: TList);
    procedure FocusArticleContainer(ctnr: TArticleContainer);
    procedure SyncContainerTree(ctnr: TArticleContainer);
    function GetQuoteText(article: TArticleBase): string;
    function CheckSaveOutboxMessages: Boolean;
    procedure SaveOutstandingPostings;
    procedure LoadUnpostedMessages;
    function GetQNodeGetter(node: PVirtualNode; var idx: Integer): TTCPGetter;
    function GetFocusedObject: TObject;
    procedure BatchToParams(batch: TBatchAction; var params: TGetMessagesParams);
    function CreateModelessWindow(cls: TComponentClass): TForm;
    procedure GoToArticle(article: TArticleBase);
    procedure OnArticleFound(article: TArticleBase; bookmark: Boolean; var continue: Boolean);
    procedure MoveSelectedArticlesToFolder(fldr: TArticleFolder; deleteSrc, branches: Boolean);
    procedure MoveSelectedArticlesToBookmark(bookmark: TBookmark);

    procedure UpdateStatusBar(const msg: string; prMin, prMax, prPos: Integer);

    procedure PurgeCtnr(ctnr: TArticleObjectContainer; all, reset, archive: Boolean);
    procedure BeginPurge(ctnr: TArticleContainer);
    procedure EndPurge;
    procedure FixupReloadedGroups;
    procedure ShowNewsgroupList(account: TNNTPAccount; newGroupsOnly: Boolean);
    procedure SelectBranch(node: PVirtualNode);
    procedure FillBatchComboBox(selectBatch: Integer);
    function CreateBookmark: TBookmark;
    procedure HandleSpecialKey(key: word; shift: TShiftState);
    procedure StartEverything;
    procedure StopEverything;
    function IsInMultipartMode: Boolean;
    procedure SetAttachmentsDirectory(fileName: string);
    procedure GoToNextArticle(markread: Boolean);
    procedure DoSaveAttachment(const title: string; var fileName: string; var mp: TmvMessagePart);
    procedure GetSingleArticle(grp: TSubscribedGroup; articleNo: Int64);

    procedure LoadToolbarLayout;
    procedure SaveToolbarLayout;
  protected
    procedure ResizeSearchBar;
    procedure UpdateActions; override;
    procedure WndProc(var Message: TMessage); override;
  public
    fDisableShortcutCount: Integer;
    fSearchMessageForm: TdlgSearch;
    fBookmarkSet: TBookmarkSet;
    fCurrentBookmark: TBookmark;
    MessageScrollBox1: TMessageScrollBox;
    procedure SetCurrentBookmark(bookmark: TBookmark; dontMakeVisible: Boolean = False);
    procedure PopulateBookmarkCombo;
    function GetFocusedArticle: TArticleBase;
    procedure MouseWheelHandler(var Message: TMessage); override;
    function GetAttachmentsDirectory: string;

    class function CheckRunOnce: Boolean;
  end;

type
  PDWORD_PTR = ^DWORD_PTR;

function SendMessageTimeout(hWnd: HWND; Msg: UINT; wParam: WPARAM;
  lParam: LPARAM; fuFlags, uTimeout: UINT; lpdwResult: PDWORD_PTR): LRESULT;
  stdcall; external user32 name 'SendMessageTimeoutW';

var
  fmMain: TfmMain;
  gUniqueMessage: DWORD;

implementation

{$R *.dfm}

uses
  {$if CompilerVersion >= 24.0} // 24.0 = Delphi XE3
    System.UITypes,
  {$ifend}
  ClipBrd, Printers, StrUtils,
  AccountsDialog, NewsgroupsDialog, FilterDialog, MessagesDialog, unitNNTPFilters,
  IdException, SplashForm, unitNNTPThreadManager, BatchesDialog, NewsgroupStatisticsForm,
  unitCharsetMap, MessagebaseManagementDialog, ServerAdminCreateGroupDialog, ServerAdminRemoveGroupDialog,
  TestPerformanceDialog, IdGlobal, IdStack, unitStreamTextReader, IdentitiesDialog,
  unitSearchString, MailAccountsDialog, cmpSpellChecker, unitLog, FileCtrl, Registry,
  unitXanaExporter, unitCIDMIMEHandler, CancelArticleDialog, unitIdentities, cmpNewsRichEdit,
  MoveMessagebaseDialog, IdCoder, IdCoderUUE, IdCoderMIME, CombineDecodeDialog,
  OptionsForm, AccountForm, NewsgroupForm, unitFontDetails, AddAccountWizard,
  FindOnInternetDialog, ReadlnDelayDialog, IdURI, GraphUtil, unitMessageBaseSearch,
  XnCoderUUE;

type
  TfnIterator = function(proc: TGroupIteratorProc; purge: Boolean; param: LPARAM = 0): Integer of object;

  TDeferredCombineSet = class
  private
    fArticles: TList;
    fFileName: string;
  public
    constructor Create(const AFileName: string; articleList: TList);
    destructor Destroy; override;
  end;

const
 NewLine = #13#10;

var
  gMutex: THandle = 0; // Used by 'run once' detection

function EnumWindowsProc(hwnd: HWND; param: LPARAM): BOOL; stdcall;
var
  msgResult: DWORD;
  cds: TCopyDataStruct;
  str: string;
begin
  if (SendMessageTimeout(hwnd, param, 0, 0, SMTO_BLOCK or SMTO_ABORTIFHUNG, 1000, @msgResult) <> 0) and (msgResult = $F00B00) then
  begin
    if ParamCount >= 1 then
    begin
      str := ParamStr(1);
      cds.dwData := 1;
      cds.cbData := Succ(Length(str)) * SizeOf(Char);
      cds.lpData := PChar(str);
      SendMessage(hwnd, WM_COPYDATA, HWND_DESKTOP, LPARAM(@cds))
    end;
    Result := False;
  end
  else
    Result := True;
end;


{ TfmMain }

procedure TfmMain.actAccountAddExecute(Sender: TObject);
var
  dlg: TfmAddAccountWizard;
  account: TNNTPAccount;
begin
  dlg := TfmAddAccountWizard.Create(nil);
  try
    if dlg.ShowModal = mrOK then
    begin
      account := TNNTPAccount.Create(NNTPAccounts);

      account.AccountName := Trim(dlg.edAccountName.Text);
      account.NNTPServerSettings.ServerName := dlg.edServerName.Text;
      account.NNTPServerSettings.ServerLogonRequired := dlg.cbLogonRequired.Checked;
      if dlg.cbLogonrequired.Checked then
      begin
        account.NNTPServerSettings.ServerAccountName := dlg.edUserName.Text;
        account.NNTPServerSettings.ServerPassword := dlg.edPassword.Text
      end;
      SendMessage(Handle, WM_GROUPSCHANGING, 0, 0);
      try
        NNTPAccounts.Add(account);
        NNTPAccounts.SaveToRegistry;
      finally
        SendMessage(Handle, WM_GROUPSCHANGED, 0, 0);
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfmMain.actAccountPropertiesExecute(Sender: TObject);
var
  fm: TfmAccount;
  account: TNNTPAccount;
begin
  account := GetFocusedAccount;

  if Assigned(account) then
  begin
    fm := TfmAccount.CreateInit(nil, account);
    try
      if fm.ShowModal = mrOK then
      begin
        NNTPAccounts.SaveToRegistry;
        Reinit_vstSubscribed;
      end;
    finally
      fm.Free;
    end;
  end;
end;

procedure TfmMain.actAccountRefreshGroupListExecute(Sender: TObject);
var
  Account: TNNTPAccount;
begin
  Account := GetFocusedAccount;

  if Assigned(Account) then
  begin
    ThreadManager.GetNewsgroups(Account);
    vstSubscribed.Invalidate;   // Show the 'red earth activity' icon in the
                                // subscribed group list
  end;
end;

procedure TfmMain.actAccountRemoveExecute(Sender: TObject);
var
  account: TNNTPAccount;
  art: TArticleBase;
begin
  art := GetFocusedArticle;
  account := GetFocusedAccount;
  if Assigned(account) then
  begin
    if MessageBox(handle,
                  PChar(Format(rstDeleteMessage, [account.AccountName])),
                  PChar(Application.Title), MB_YESNO or MB_DEFBUTTON2) = ID_YES then
    begin
      if Assigned(art) and (TSubscribedGroup(art.Owner).Owner = account) then
        vstArticles.RootNodeCount := 0;
      SendMessage(Handle, WM_GROUPSCHANGING, 0, 0);
      try
        NNTPAccounts.Delete(account);
        NNTPAccounts.SaveToRegistry;
      finally
        SendMessage(Handle, WM_GROUPSCHANGED, 0, 0);
      end;
    end;
  end;
end;

procedure TfmMain.actAccountShowNewsgroupListExecute(Sender: TObject);
var
  account: TNNTPAccount;
begin
  Account := GetFocusedAccount;
  if Assigned(account) then
    ShowNewsgroupList(account, False);
end;

procedure TfmMain.actAccountSortGroupsByNameExecute(Sender: TObject);
var
  account: TNNTPAccount;
begin
  account := GetFocusedAccount;
  if Assigned(account) then
    account.SortGroupsByName := not account.SortGroupsByName;
end;


procedure TfmMain.actArticleCancelExecute(Sender: TObject);
var
  article: TArticleBase;
  grp: TSubscribedGroup;
  dlg: TdlgCancelArticles;
begin
  article := GetFocusedArticle;
  if Assigned(article) and (article is TArticle) then
  begin
    if (vstArticles.SelectedCount > 1) and not XNOptions.MagicUser then
    begin
      MessageBox(Handle, 'This version of XanaNews does not allow you to cancel multiply selected messages', 'XanaNews', MB_OK);
      Exit;
    end;

    if (vstArticles.SelectedCount = 1) and (article.IsCancelled) then
    begin
      article.IsCancelled := False;
      Exit;
    end;
    grp := TArticle(article).SubscribedGroup;
    dlg := TdlgCancelArticles.Create(nil);
    try
      ForEachSelectedArticle(DoCheckFromMe, 0, False);
      dlg.Group := grp;
      dlg.NotFromMe := fIteratorFailed;
      dlg.MultiSelect := vstArticles.SelectedCount > 1;
      if dlg.ShowModal = MRYES then
        ForEachSelectedArticle(DoMarkAsCancelledArticle, LPARAM(PChar(dlg.mmoReason.Text)), False)
    finally
      dlg.Free;
    end;
  end;
end;

procedure TfmMain.actArticleCollapseAllThreadsExecute(Sender: TObject);
var
  art: TArticleBase;
  node: PVirtualNode;
begin
  art := GetFocusedArticle;
  if Assigned(art) then
  begin
    fInCollapse := True;    // Cleared in the timer handler.
    while art.Parent <> nil do
      art := art.Parent;
    vstArticles.FullCollapse;
    node := GetArticleNode(art);
    SelectArticleNode(node);
    vstArticles.FullCollapse(node);
  end;
end;

procedure TfmMain.actArticleCollapseThreadExecute(Sender: TObject);
var
  article: TArticleBase;
  node: PVirtualNode;
begin
  article := GetFocusedArticle;
  if Assigned(article) then
  begin
    fInCollapse := True;
    while Assigned(article.Parent) do
      article := article.Parent;
    vstArticles.BeginUpdate;
    try
      node := GetArticleNode(article);
      vstArticles.FullCollapse(node);
    finally
      vstArticles.EndUpdate;
    end;
    node := GetArticleNode(article);
    SelectArticleNode(node);
    vstArticles.FullCollapse(node);
  end;
end;

procedure TfmMain.actArticleDeleteArticleExecute(Sender: TObject);
begin
  // Action handler for Article/Delete Article.  Delete or undelete the
  // selected article(s).  If more than one is selected, they all set to
  // same delete/undelete state as the first one.
  if vstQueuedRequests.Focused then
    actQRDeleteExecute(nil)
  else
    if not vstSubscribed.Focused then
      actMessageDelete.Execute
    else
      actNewsgroupUnsubscribe.Execute;
end;

procedure TfmMain.actArticleDeleteThreadExecute(Sender: TObject);
var
  delete: Boolean;
  article: TArticleBase;
begin
  article := GetNodeArticle(vstArticles.GetFirstSelected);
  delete := not article.IsDeleted;

  if ForEachArticleInThread(article, DoDeleteArticle, LPARAM(delete)) = 1 then
    NextArticle([naUnreadOnly], GetFocusedArticle);
                        // If a single message was deleted then
                        // go to the next message.
  vstArticles.Invalidate;
end;

procedure TfmMain.actArticleExpandAllThreadsExecute(Sender: TObject);
var
  art: TArticleBase;
  ctnr: TArticleContainer;
begin
  art := GetFocusedArticle;
  if Assigned(art) then
    ctnr := art.Owner
  else
    ctnr := fLastFocusedArticleContainer;

  if Assigned(ctnr) then
  begin
    FullExpandThreads(ctnr, nil);
    GoToArticle(art);
  end;
end;

procedure TfmMain.actArticleExpandThreadExecute(Sender: TObject);
var
  article: TArticleBase;
  node: PVirtualNode;
begin
  article := GetFocusedArticle;
  if Assigned(article) then
  begin
    while Assigned(article.Parent) do  // Find the thread root
      article := article.Parent;
    vstArticles.BeginUpdate;
    try
      node := GetArticleNode(article);
      FullExpandThreads(article.Owner, node);
    finally
      vstArticles.EndUpdate;
    end;
  end;
end;

procedure TfmMain.actArticleFlagExecute(Sender: TObject);
var
  article: TArticleBase;
begin
  article := GetFocusedArticle;
  if Assigned(article) then
    ForEachArticleInBranch(article, DoFlagArticle, LPARAM(not article.IsInteresting));
  vstArticles.Invalidate;
end;

procedure TfmMain.actArticleGetMessageBodyExecute(Sender: TObject);
begin
  // Get the selected articles from the news server.
  // Note: when a node is expanded it will also download all children.
  ForEachSelectedArticle(DoGetArticleBody);
  vstArticles.Invalidate;
end;

procedure TfmMain.actArticleGetThreadExecute(Sender: TObject);
begin
  // Get the whole thread for the selected articles.                                                   |
  ForEachSelectedBranch(DoGetArticleThread, 0, True);
  vstArticles.Invalidate;
end;

procedure TfmMain.actArticleGotoPreviousExecute(Sender: TObject);
var
  article: TArticleBase;
begin
  // Go to the previous article - off the Previous Article Stack
  article := GetFocusedArticle;
  if Assigned(article) then
    fNextArticleStack.Push(article);
  article := fPrevArticleStack.Pop;
  if Assigned(article) then
  begin
    fPrevArticle := nil;
    fNxStack := True;
    GoToArticle(article);
  end;
end;

procedure TfmMain.actArticleMarkAsReadExecute(Sender: TObject);
var
  article: TArticleBase;
begin
  article := GetFocusedArticle;
  if Assigned(article) then
  begin
    ForEachSelectedArticle(DoMarkAsReadArticle, LPARAM(not article.IsRead));
    vstArticles.Invalidate;
    Refresh_vstSubscribed;

    // If the user requested the article to be marked as unread then stop
    // the auto mark as read mechanism for the moment.
    if not article.IsRead then
      fAutoMarkTicks := MaxInt;
  end;
end;

procedure TfmMain.actArticleMarkThreadReadExecute(Sender: TObject);
var
  mark: Boolean;
  article: TArticleBase;
begin
  article := GetNodeArticle(vstArticles.GetFirstSelected);

  ForEachArticleInThread(article, DoCheckArticleRead, 0);
  mark := fIteratorFailed;

  ForEachArticleInThread(article, DoMarkAsReadArticle, LPARAM(mark));
  vstArticles.Invalidate;
  Refresh_vstSubscribed;
end;

procedure TfmMain.actArticleNextUnreadExecute(Sender: TObject);
begin
  GotoNextArticle(True);
end;

procedure TfmMain.actArticlePostNewArticleExecute(Sender: TObject);
var
  Account: TNNTPAccount;
  Group, grp: TSubscribedGroup;
  frm: TfmPostMessage;
  Groups: TList;
  i: Integer;
  gn: string;
begin
  Account := GetFocusedAccount;

  if Assigned(account) then   // Remember - you post to an account, not a group!
  begin
    frm := TfmPostMessage(CreateModelessWindow(TfmPostMessage));
    try
      Group := GetFocusedGroup;
      if Assigned(Group) then
      begin
        if vstSubscribed.SelectedCount > 1 then
        begin
          Groups := TList.Create;
          try
            ForEachSelectedGroup(DoAddGroupToList, False, LPARAM(Groups));

            gn := '';
            for i := 0 to Groups.Count - 1 do
            begin
              grp := TSubscribedGroup(Groups[i]);
              if grp.Owner = group.Owner then
                gn := gn + ',' + grp.Name;
            end;
            Delete(gn, 1, 1);
            frm.GroupName := gn;
          finally
            Groups.Free;
          end;
        end
        else
          frm.GroupName := group.Name;
        frm.Account := group.Owner;
        frm.NNTPSettings := group.NNTPSettings;
        frm.DefaultPostingSettings := group.PostingSettings;
      end
      else
      begin
        frm.GroupName := '';
        frm.NNTPSettings := account.NNTPSettings;
        frm.DefaultPostingSettings := account.PostingSettings;
        frm.Account := Account;
      end;

      frm.ReplyToArticle := nil;
      frm.Show;
    except
      frm.Free;
      raise;
    end
  end
end;

procedure TfmMain.actArticleReplyByMailExecute(Sender: TObject);
var
  Art: TArticleBase;
  frm: TfmReplyByMail;
begin
  Art := GetFocusedArticle;

  if Assigned(art) then
  begin
    frm := TfmReplyByMail(CreateModelessWindow(TfmReplyByMail));
    try
      frm.ReplyToArticle := Art;
      frm.ArticleContainer := art.Owner;
      frm.InitialText := GetQuoteText(Art);
      frm.Show;
    except
      frm.Free;
      raise;
    end;
  end;
end;

procedure TfmMain.actArticleReplyToArticleExecute(Sender: TObject);
var
  Article: TArticleBase;
  frm: TfmPostMessage;
  group: TSubscribedGroup;
  mr: Integer;
begin
  Article := GetFocusedArticle;

  if Assigned(article) and (article is TArticle) then
  begin
    group := TSubscribedGroup(article.Owner);

    if SameText(article.Header['Followup-To'], 'poster') then
    begin
      mr := MessageBox(handle, 'The author of this message has indicated that they want to receive follow-ups by E-Mail.  '#13#10#13#10'Do you want to send your reply by E-Mail?', 'XanaNews', MB_YESNOCANCEL or MB_ICONQUESTION);
      if (mr = IDYES) or (mr = IDCANCEL) then
      begin
        if mr = IDYES then
          actArticleReplyByMailexecute(Sender);
        Exit;
      end;
    end;

    frm := TfmPostMessage(CreateModelessWindow(TfmPostMessage));
    try
      frm.ReplyToArticle := Article;
      frm.GroupName := group.Name;
      frm.NNTPSettings := group.NNTPSettings;
      frm.DefaultPostingSettings := group.PostingSettings;
      frm.Account := group.Owner;

      frm.Font.Charset := CodePageToCharset(article.CodePage);
      // Insert selected text in current message as quote
      frm.InitialText := GetQuoteText(article);

      vstArticles.SetFocus;

      if not article.IsRead then
      begin
        article.IsRead := True;
        vstArticles.Invalidate;
        Refresh_vstSubscribed;
      end;

      frm.Show;
    except
      frm.Free;
      raise;
    end;
  end;
end;

procedure TfmMain.actBatchBatchesExecute(Sender: TObject);
var
  dlg: TdlgBatches;
  batch: TNNTPBatch;
begin
  Application.CreateForm(TdlgBatches, dlg);
  try
    if dlg.ShowModal = mrOK then
    begin
      batch := dlg.Batch;
      FillBatchComboBox(NNTPAccounts.IndexOfBatch(batch));
      RunBatch(batch);
    end
    else
      FillBatchComboBox(-1);
  finally
    dlg.Free;
  end;
end;

procedure TfmMain.actFileNewFolderExecute(Sender: TObject);
begin
  gArticleFolders.AddNew;
  Refresh_vstSubscribed;
end;

procedure TfmMain.actFolderClearExecute(Sender: TObject);
var
  fldr: TArticleFolder;
begin
  fldr := GetFocusedArticleFolder;
  if Assigned(fldr) then
  begin
    SendMessage(handle, WM_GROUPSCHANGING, 0, 0);
    try
      vstArticles.RootNodeCount := 0;
      DisplayArticleBody(nil);
      fldr.Clear;
    finally
      SendMessage(handle, WM_GROUPSCHANGED, 0, 0);
    end;
  end;
end;

procedure TfmMain.actFolderDeleteExecute(Sender: TObject);
var
  fldr: TArticleFolder;
  node: PVirtualNode;
begin
  node := vstSubscribed.FocusedNode;
  fldr := GetNodeArticleFolder(node);
  if Assigned(fldr) and not (fldr is TPurgedMessages) and not (fldr is TSentMessages) and
    (MessageBox(Handle, PChar(Format('Are you sure you want to delete folder %s?', [fldr.Name])), PChar(Application.Title), MB_YESNO or MB_DEFBUTTON2) = ID_YES) then
  begin
    BeginPurge(fldr);
    SendMessage(handle, WM_GROUPSCHANGING, 0, 0);
    try
      gArticleFolders.DeleteFolder(fldr);
      fLastFocusedArticleContainer := nil;
      fPrevArticle := nil;
    finally
      SendMessage(handle, WM_GROUPSCHANGED, 0, 0);
      EndPurge;
    end;
  end;
end;

procedure TfmMain.actFolderRenameExecute(Sender: TObject);
var
  fldr: TArticleFolder;
  node: PVirtualNode;
begin
  node := vstSubscribed.FocusedNode;
  fldr := GetNodeArticleFolder(node);
  if Assigned(fldr) then
  begin
    fEditNameAllowed := True;
    try
      fRenamingFolder := True;
      EnableShortcuts(False);
      vstSubscribed.EditNode(node, -1)
    finally
      fEditNameAllowed := False;
    end;
  end;
end;

procedure TfmMain.actGetEverythingExecute(Sender: TObject);
var
  i, j: Integer;
  grp: TSubscribedGroup;
  act: TNNTPAccount;
  params: TGetMessagesParams;
begin
  // 'Get Everything (lightning bolt button)
  for i := 0 to NNTPAccounts.Count - 1 do
  begin
    act := NNTPAccounts.Items[i];

    for j := 0 to act.SubscribedGroupCount - 1 do
    begin
      FillChar(params, SizeOf(params), 0);
      grp := act.SubscribedGroups[j];
      BatchToParams(grp.NNTPSettings.DefaultAction, params);
      DoGetMessages(grp, LPARAM(@params));
    end;
  end;
end;

procedure TfmMain.actHelpAboutExecute(Sender: TObject);
begin
  NTAboutBox.ThanksTo := fDeservesMedals;
  NTAboutBox.Execute;
end;

procedure TfmMain.actViewHideMessagesNotToMeExecute(Sender: TObject);
var
  art: TArticleBase;
  artno: Int64;
begin
  if Assigned(fLastFocusedArticleContainer) then
  begin
    art := GetFocusedArticle;
    if Assigned(art) then
      artno := art.ArticleNo
    else
      artNo := -1;

    fLastFocusedArticleContainer.HideMessagesNotToMe := not fLastFocusedArticleContainer.HideMessagesNotToMe;
    InitArticlesRootNodeCount(fLastFocusedArticleContainer);
    Refresh_vstArticles;

    if artNo <> -1 then
      GoToArticle(fLastFocusedArticleContainer.FindArticleNo(artNo));
  end;
end;

procedure TfmMain.actMessageDeleteExecute(Sender: TObject);
var
  article: TArticleBase;
  delete: Boolean;
begin
  article := GetNodeArticle(vstArticles.GetFirstSelected);
  if Assigned(article) then
  begin
    delete := not article.IsDeleted;
    ForEachSelectedArticle(DoFlagArticle, 0);  // They're actively deleting it
                                               // Therefore it's no longer interesting.

    if ForEachSelectedArticle(DoDeleteArticle, LPARAM(delete)) = 1 then
      NextArticle([], GetFocusedArticle);
                            // If a single message was deleted then
                            // go to the next message.

    vstArticles.Invalidate;
  end;
end;

procedure TfmMain.DoSaveAttachment(const title: string; var fileName: string; var mp: TmvMessagePart);
var
  gr: TGraphic;
  ext: string;
  gc: TGraphicClass;
begin
  mp := MessageScrollBox1.GetFocusedAttachment;

  if Assigned(mp) then
  begin
    gr := mp.Graphic;
    if Assigned(gr) then
    begin
      if mp.HasRawData then     // We don't know how to decode the attachment as a picture
      begin                     // Maybe it's something else - an .exe or whatever.  Let 'em save it
        ext := ExtractFileExt(mp.FileName);
        if Copy(ext, 1, 1) = '.' then
          Delete(ext, 1, 1);

        SaveExecuteDialog1.Title := title;
        SaveExecuteDialog1.DefaultExt := ext;
        SaveExecuteDialog1.Filter := 'All files (*.*)|*.*';
        SaveExecuteDialog1.FilterIndex := 1;

        SaveExecuteDialog1.InitialDir := GetAttachmentsDirectory;
        SaveExecuteDialog1.FileName := mp.FileName;
        SaveExecuteDialog1.FileName := StringReplace(SaveExecuteDialog1.FileName, '/', '-', [rfReplaceAll]);
        SaveExecuteDialog1.FileName := StringReplace(SaveExecuteDialog1.FileName, '\', '-', [rfReplaceAll]);
        if SaveExecuteDialog1.Execute then
        begin
          fileName := SaveExecuteDialog1.FileName;
          SaveAttachment(mp, fileName, IsInMultipartMode);
          SetAttachmentsDirectory(fileName);
        end
        else
          mp := nil;
      end
      else
      begin                     // Message part is a picture
        gc := TGraphicClass(gr.ClassType);
        SavePictureDialog1.InitialDir := GetAttachmentsDirectory;
        SavePictureDialog1.Filter := GraphicFilter(gc);
        SavePictureDialog1.DefaultExt := GraphicExtension(gc);
        SavePictureDialog1.FileName := FixFileNameString(mp.FileName);
        if SavePictureDialog1.Execute then
        begin
          fileName := SavePictureDialog1.FileName;
          SaveAttachment(mp, fileName, IsInMultipartMode);
          SetAttachmentsDirectory(fileName);
        end
        else
          mp := nil;
      end;
    end;
  end;
end;

procedure TfmMain.actMessageSaveAttachmentExecute(Sender: TObject);
var
  fn: string;
  mp: TmvMessagePart;
begin
  DoSaveAttachment('Save Attachment', fn, mp);
end;

procedure TfmMain.actNewsgroupDeleteMessagesExecute(Sender: TObject);
var
  dlg: TdlgDeleteMessages;
  newsgroup: TSubscribedGroup;
  account: TNNTPAccount;
begin
  newsgroup := GetFocusedGroup;
  account := GetFocusedAccount;

  if Assigned(newsgroup) or Assigned(account) then          // Show delete dialog
  begin
    Application.CreateForm(TdlgDeleteMessages, dlg);
    try
      if dlg.ShowModal = mrOK then      // Delete messages that match filter
      begin
        if Assigned(dlg.Filter) then   // from all selected groups.
          if Assigned(newsgroup) then
            ForEachSelectedGroup(DoDeleteArticles, True, LPARAM(dlg.Filter))
          else
            ForEachGroupInSelectedAccount(DoDeleteArticles, True, LPARAM(dlg.Filter));
        vstSubscribed.Invalidate;
        Refresh_vstArticles;
      end;
    finally
      dlg.Free;
    end;
  end;
end;

procedure TfmMain.actNewsgroupGetMessagesDefaultExecute(Sender: TObject);
var
  grp: TSubscribedGroup;
  act: TNNTPAccount;
  params: TGetMessagesParams;
begin
  // 'quick get messages'
  grp := GetFocusedGroup;
  act := GetFocusedAccount;
  FillChar(params, SizeOf(params), 0);
  params.useDefaultGroupSettings := True;

  if Assigned(grp) then
    ForEachSelectedGroup(DoGetMessages, False, LPARAM(@Params))
  else
    if Assigned(act) then
      ForEachGroupInSelectedAccount(DoGetMessages, False, LPARAM(@params));
end;

procedure TfmMain.actNewsgroupGetMessagesExecute(Sender: TObject);
var
  dlg: TdlgGetMessages1;
  group: TSubscribedGroup;
  account: TNNTPAccount;
  params: TGetMessagesParams;
  act: TBatchAction;
  tart: Int64;
begin
  // Get messages from the selected groups
  account := GetFocusedAccount;
  group := GetFocusedGroup;
  FillChar(params, SizeOf(params), 0);

  if Assigned(account) then
  begin
    Application.CreateForm(TdlgGetMessages1, dlg);
    try
      if vstSubscribed.SelectedCount < 2 then
        dlg.group := group;

      dlg.Account := Account;

      if dlg.ShowModal = mrOK then
      begin
        act := TBatchAction.Create;
        try
          dlg.Action := act;
          dlg.UpdateAct;
          BatchToParams(act, params)
        finally
          act.Free;
        end;

        if dlg.rbAddFrom.Checked then
        begin
          params.fromArticle := StrToInt64Def(dlg.edAddFrom.Text, 1);
          tart := StrToInt64Def(dlg.edAddTo.Text, 0);
          params.fMessageCount := tart - params.fromArticle + 1;
          if params.fMessageCount <= 0 then
            params.fMessageCount := -1;
          params.fActionType := batAll;
        end;

        if params.fMessageCount >= 0 then
        begin
          if Assigned(group) then
            ForEachSelectedGroup(DoGetMessages, False, LPARAM(@params))
          else
            ForEachGroupInSelectedAccount(DoGetMessages, False, LPARAM(@params));
        end;
        Refresh_vstSubscribed;
      end;
    finally
      dlg.Free;
    end;
  end;
end;

procedure TfmMain.actNewsgroupMarkAllMessagesAsReadExecute(Sender: TObject);
var
  newsgroup: TSubscribedGroup;
begin
  newsgroup := GetFocusedGroup;

  if Assigned(newsgroup) then
  begin
    if vstSubscribed.SelectedCount < 2 then
      DoMarkArticlesAsRead(newsgroup, 0)
    else
      ForEachSelectedGroup(DoMarkArticlesAsRead, False, 0);
    vstArticles.Invalidate;
    Refresh_vstSubscribed;
  end
  else
    if GetFocusedAccount <> nil then
    begin
      ForEachGroupInSelectedAccount(DoMarkArticlesAsRead, False, 0);
      vstArticles.Invalidate;
      Refresh_vstSubscribed;
    end;
end;

procedure TfmMain.actNewsgroupPropertiesExecute(Sender: TObject);
var
  grp: TSubscribedGroup;
  dlg: TfmNewsgroup;
  art: TArticleBase;
  artNo: Int64;
begin
  grp := GetFocusedGroup;
  if Assigned(grp) then
  begin
    dlg := TfmNewsgroup.CreateInit(nil, grp);
    try
      art := GetFocusedArticle;
      if Assigned(art) then
        artno := art.ArticleNo
      else
        artNo := -1;

      if dlg.ShowModal = mrOK then
      begin
        NNTPAccounts.SaveToRegistry(grp.Owner);

        if (not NNTPAccounts.ShowSecrets) and grp.Secret then
        begin
          Refresh_vstSubscribed;
          fPrevArticle := nil;
          FocusArticleContainer(nil);
        end
        else
        begin
          FocusArticleContainer(fLastFocusedArticleContainer);

          InitArticlesRootNodeCount(fLastFocusedArticleContainer);
          Refresh_vstArticles;
          Refresh_vstSubscribed;

          if artNo <> -1 then
            GoToArticle(fLastFocusedArticleContainer.FindArticleNo(artNo));
        end;
      end;
    finally
      dlg.Free;
    end;
  end;
end;

procedure TfmMain.actNewsgroupUnsubscribeExecute(Sender: TObject);
var
  I: Integer;
  st: string;
  c: Integer;
  grp: TSubscribedGroup;
  groups: TList;
  many: Boolean;
begin
  c := vstSubscribed.SelectedCount;
  grp := GetFocusedGroup;

  if (c > 0) and Assigned(grp) then   // 1.17.6.6
  begin
    st := IfThen(c = 1, grp.Name, rstSelectedGroups);

    if MessageDlg(Format(rstConfirmUnsubscribe, [st]), mtConfirmation, [mbYes, mbNo], 0) = idYes then
    begin
      groups := TList.Create;
      try
        ForEachSelectedGroup(DoAddGroupToList, False, LPARAM(groups));
        for i := 0 to groups.Count - 1 do
        begin
          if TSubscribedGroup(Groups[i]) = FLastFocusedArticleContainer then
          begin
            vstArticles.RootNodeCount := 0;
            DisplayArticleBody(nil);
            FLastFocusedArticleContainer := nil;
            Break;
          end;
        end;
        many := groups.Count > 1;
      finally
        groups.Free;
      end;

      try
        if many then
          ForEachSelectedGroup(Unsubscribe, False, 0)
        else
          ForEachSelectedGroup(Unsubscribe, False, 1);
      finally
        if many then
          NNTPAccounts.SaveToRegistry(nil);
      end;

      vstSubscribed.ClearSelection;
      fPrevArticle := nil;
    end;

    refresh_vstSubscribed;
    refresh_vstArticles;
  end;
end;

procedure TfmMain.actQRDeleteExecute(Sender: TObject);
var
  getter: TTCPGetter;
  idx, n: Integer;
  node, nd1: PVirtualNode;
begin
  // Delete a getter from the Queued Requests panel
  node := vstQueuedRequests.GetFirstSelected;
  n := 0;
  if Assigned(node) then
    repeat
      nd1 := vstQueuedRequests.GetNextSelected(node);
      getter := GetQNodeGetter(node, idx);

      if idx = -1 then
      begin
        n := 0;
        getter.Clear;
      end
      else
      begin
        Dec(idx, n);
        if idx = 0 then
          getter.Paused := True;
        getter.DeleteRequest(idx);
        Inc(n);
      end;
      node := nd1;
    until node = nil;

  fOutstandingRequestCount := 0;
  fOutstandingGetterCount := 0;
  fOutstandingActiveGetterCount := 0;
  vstQueuedRequests.RootNodeCount := 0;
  Refresh_vstQueuedRequests;
  vstArticles.Invalidate;
end;

procedure TfmMain.actQREditExecute(Sender: TObject);
var
  getter: TTCPGetter;
  poster: TPoster;
  idx: Integer;
  frm: TfmPostMessage;
  efrm: TfmReplyByMail;
  requests: TObjectList;
begin
  // Edit a 'post message' queued request.
  getter := GetQNodeGetter(vstQueuedRequests.FocusedNode, idx);

  if idx <> -1 then             // Must be a 'post message' request
  begin
    if getter is TPoster then
    begin
      poster := TPoster(getter);

      if not poster.UseOutbasket then
        poster.Paused := True;    // Pause it so that it doesn't suddenly get sent
                                  // while we're editing it!
                                  // Make sure that it still exists.  Now it's
                                  // paused it ain't going anywhere!

      Application.CreateForm(TfmPostMessage, frm);
      try
        frm.OnDestroy := ModelessWindowFormDestroy;
        frm.OnActivate := ModelessWindowFormActivate;
        frm.OnDeactivate := ModelessWindowFormDeactivate;
        frm.Account := poster.Account;
        requests := poster.LockList;
        try
          frm.PosterRequest := TPosterRequest(requests[idx]);
        finally
          poster.UnlockList;
        end;
        frm.Show;
        fModelessWindowList.Add(frm);
      except
        frm.Free;
        raise;
      end;
    end
    else
      if Getter is TEmailer then
      begin
        getter.Paused := True;
        Application.CreateForm(TfmReplyByMail, efrm);
        try
          efrm.OnDestroy := ModelessWindowFormDestroy;
          efrm.OnActivate := ModelessWindowFormActivate;
          efrm.OnDeactivate := ModelessWindowFormDeactivate;
          requests := TEmailer(getter).LockList;
          try
            efrm.EMailerRequest := TEmailerRequest(requests[idx]);
          finally
            TEmailer(getter).UnlockList;
          end;
          efrm.Show;
          fModelessWindowList.Add(efrm);
        except
          efrm.Free;
          raise;
        end;
      end;
  end;
end;

procedure TfmMain.actQRPauseExecute(Sender: TObject);
var
  getter: TTCPGetter;
  idx: Integer;
begin
  getter := GetQNodeGetter(vstQueuedRequests.FocusedNode, idx);
  if Assigned(getter) then
  begin
    getter.Paused := not Getter.Paused;
    Refresh_vstQueuedRequests;
  end;
end;

procedure TfmMain.actSearchFindExecute(Sender: TObject);
begin
  fFindDialogShowing := True;
  EnableShortcuts(False);
  FindDialog1.Execute;
end;

procedure TfmMain.actSearchFindFlaggedExecute(Sender: TObject);
begin
  fDontMarkOnLeave := True;
  try
    NextArticle([naInterestingOnly, naCanLeaveGroup, naCircularAccounts], GetFocusedArticle);
  finally
    fDontMarkOnLeave := False;
  end;
end;

procedure TfmMain.actSearchFindKeywordNExecute(Sender: TObject);
var
  kw: TNextArticleOption;

  function FindKeywordNo(const nm: string): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 1 to Length(nm) do
      if nm[i] in ['1'..'9'] then
      begin
        Result := Ord(nm[i]) - Ord('1');
        Break;
      end;
  end;

begin
  if not (Sender is TAction) then Exit;
  kw := naKeyword0;
  Inc(kw, FindKeywordNo(TAction(Sender).Name));
  fDontMarkOnLeave := True;
  try
    NextArticle([kw, naCanLeaveGroup, naCircularAccounts], GetFocusedArticle);
  finally
    fDontMarkOnLeave := False;
  end;
end;

procedure TfmMain.actSearchFindMessageExecute(Sender: TObject);
begin
  if Assigned(fSearchMessageForm) then // Dialog may already exist (it's modeless)
  begin
    if fSearchMessageForm.WindowState = wsMinimized then
      fSearchMessageForm.WindowState := wsNormal;
    fSearchMessageForm.BringToFront;
    Exit;
  end;

  fSearchMessageForm := TdlgSearch(CreateModelessWindow(TdlgSearch));
  fSearchMessageForm.OnDestroy := SearchMessageFormDestroy;

  ForEachSelectedGroup(DoAddGroupToList, False, LPARAM(fSearchMessageForm.Groups));

  if fSearchMessageForm.Groups.Count = 0 then
    ForEachGroupInSelectedAccount(DoAddGroupToList, False, LPARAM(fSearchMessageForm.Groups));

  if fSearchmessageForm.Groups.Count = 0 then
    ForEachSelectedArticleFolder(DoAddFolderToList, LPARAM(fSearchMessageForm.Groups));

  if fSearchMessageForm.Groups.Count > 0 then
  begin
    fSearchMessageForm.Article := GetFocusedArticle;
    fSearchMessageForm.OnArticleFound := OnArticleFound;
    fSearchMessageForm.Show;
  end
  else
    FreeAndNil(fSearchMessageForm);
end;

procedure TfmMain.actSearchFindNextUnreadMessageToMeExecute(Sender: TObject);
begin
  fDontMarkOnLeave := True;
  try
    NextArticle([naUnreadOnly, naToMeOnly, naCanWrap, naCanLeaveGroup, naCircularAccounts], GetFocusedArticle)
  finally
    fDontMarkOnLeave := False;
  end;
end;

procedure TfmMain.actToolsAccountsExecute(Sender: TObject);
var
  dlg: TdlgAccounts;
  Node: PVirtualNode;
begin
  Application.CreateForm(TdlgAccounts, dlg);
  try
    dlg.FirstAccount := NNTPAccounts.Count = 0;
    dlg.ShowModal;
    NNTPAccounts.SaveToRegistry;
    Reinit_vstSubscribed(False);

    if dlg.FirstAccount then    // If it's the first account created then
    begin                       // download the group list automatically -
                                // to help newbies.
      Node := GetAccountNode(NNTPAccounts.Items[0]);
      if Assigned(node) then
      begin
        vstSubscribed.FocusedNode := Node;
        vstSubscribed.Selected[Node] := True;
        actAccountRefreshGroupListExecute(Self);
        vstSubscribed.Invalidate;
      end;
    end
    else
      Refresh_vstSubscribed;
  finally
    dlg.Free;
  end;
end;

procedure TfmMain.actToolsAdminCreateGroupExecute(Sender: TObject);
var
  dlg: TdlgServerAdminCreateGroup;
  account: TNNTPAccount;
  header: TStrings;
  msg: TStrings;
  subj: string;
  i: Integer;
begin
  // Action handler for 'Create Group'  Create a new newsgroup on the server
  account := GetFocusedAccount;
  if Assigned(account) then
  begin
    dlg := TdlgServerAdminCreateGroup.Create(nil);
    try
      dlg.edApproved.Text := Account.NNTPSettings.Identity.EMailAddress;
      if dlg.ShowModal = mrOK then
      begin
        msg := nil;
        header := TStringList.Create;
        try
          msg := TStringList.Create;
          subj := 'newgroup ' + dlg.edGroupName.Text;
          if dlg.cbModerated.Checked then
            subj := subj + ' moderated';

          header.Values['From'] := '"' + Account.NNTPSettings.Identity.UserName + '" <' + Account.NNTPSettings.Identity.EMailAddress + '>';
          header.Values['Subject'] := 'cmsg ' + subj;
          header.Values['Control'] := subj;
          header.Values['Approved'] := dlg.edApproved.Text;
          header.Values['Newsgroups'] := dlg.edGroupName.Text + ',' + 'alt.config';

          msg.Add('For your newsgroups file');
          msg.Add(dlg.edGroupName.Text + ' ' + dlg.edDescription.Text);
          if dlg.cbModerated.Checked then
          begin
            msg.Add('');
            msg.Add('Moderator submission address:' + dlg.edModeratorSubmissionAddress.Text);
            msg.Add('Moderator contact address:' + dlg.edModeratorContactAddress.Text);
          end;

          if dlg.mmoCharter.Lines.Count > 0 then
          begin
            msg.Add('');
            for i := 0 to dlg.mmoCharter.Lines.Count - 1 do
              msg.Add(dlg.mmoCharter.Lines[i]);
          end;

          ThreadManager.PostMessage(Account, header.Text, msg.Text, nil, CP_USASCII, tpNNTP);
        finally
          header.Free;
          msg.Free;
        end;
      end;
    finally
      dlg.Free;
    end;
  end;
end;

procedure TfmMain.actToolsAdminRemoveGroupExecute(Sender: TObject);
var
  dlg: TdlgServerAdminRemoveGroup;
  account: TNNTPAccount;
  header, msg: TStrings;
  subj: string;
  i: Integer;
begin
  account := GetFocusedAccount;
  if Assigned(account) then
  begin
    dlg := TdlgServerAdminRemoveGroup.Create(nil);
    try
      dlg.edApproved.Text := Account.NNTPSettings.Identity.EMailAddress;
      if dlg.ShowModal = mrOK then
      begin
        msg := nil;
        header := TStringList.Create;
        try
          msg := TStringList.Create;
          subj := 'rmgroup ' + dlg.edGroupName.Text;

          header.Values['From'] := '"' + Account.NNTPSettings.Identity.UserName + '" <' + Account.NNTPSettings.Identity.EMailAddress + '>';
          header.Values['Subject'] := 'cmsg ' + subj;
          header.Values['Control'] := subj;
          header.Values['Approved'] := dlg.edApproved.Text;
          header.Values['Newsgroups'] := dlg.edGroupName.Text + ',' + 'alt.config';

          msg.Add('Please remove from your newsgroups file');
          msg.Add(dlg.edGroupName.Text);

          if dlg.mmoReason.Lines.Count > 0 then
          begin
            msg.Add('');

            for i := 0 to dlg.mmoReason.Lines.Count - 1 do
              msg.Add(dlg.mmoReason.Lines[i]);
          end;

          ThreadManager.PostMessage(Account, header.Text, msg.Text, nil, CP_USASCII, tpNNTP);
        finally
          header.Free;
          msg.Free;
        end;
      end;
    finally
      dlg.Free;
    end;
  end;
end;

procedure TfmMain.actToolsDecodePerformanceExecute(Sender: TObject);
var
  dlg: TdlgTestPerformance;
begin
  if GetFocusedArticle = nil then Exit;
  dlg := TdlgTestPerformance.Create(nil);
  try
    dlg.Article := GetFocusedArticle;
    dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;

procedure TfmMain.actToolsDisconnectAllExecute(Sender: TObject);
begin
  ThreadManager.DisconnectAll(True);

  fOutstandingRequestCount := 0;
  fOutstandingGetterCount := 0;
  fOutstandingActiveGetterCount := 0;
  vstQueuedRequests.RootNodeCount := 0;

  ThreadManager.ClearGetters;
end;

procedure TfmMain.actToolsForensicModeExecute(Sender: TObject);
var
  ctnr: TArticleContainer;
  oldCursor: TCursor;
  art: TArticleBase;
begin
  fForensicMode := not fForensicMode;

  if fForensicMode then
    vstArticles.Header.Columns[2].Text := rstPostingHost
  else
    vstArticles.Header.Columns[2].Text := rstSubject;
  actToolsForensicMode.Checked := fForensicMode;

  art := GetFocusedArticle;
  if Assigned(art) and (art.ArticleNo = 0) then
    art := nil;

  ctnr := fLastFocusedArticleContainer;
  if not (ctnr is TSubscribedGroup) then
    ctnr := nil;

  if Assigned(ctnr) then
  begin
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      if fForensicMode then
      begin
        fPreForensicThreadOrder := ctnr.ThreadOrder;
        fPreForensicSortDirection := ctnr.ThreadSortDirection;
        fPreForensicSortOrder := ctnr.ThreadSortOrder;

        ctnr.ThreadOrder := toChronological;
        ctnr.ThreadSortDirection := sdAscending;
        ctnr.ThreadSortOrder := soPostingHost;
      end
      else
      begin
        ctnr.ThreadOrder := fPreForensicThreadOrder;
        ctnr.ThreadSortDirection := fPreForensicSortDirection;
        ctnr.ThreadSortOrder := fPreForensicSortOrder;
      end
    finally
      Screen.Cursor := OldCursor;
    end;

    InitArticlesRootNodeCount(ctnr);
    Refresh_vstArticles;
    GoToArticle(art);
  end;
end;

procedure TfmMain.actToolsLoadTestMessageExecute(Sender: TObject);
var
  s: TStream;
  reader: TStreamTextReader;
  raw: RawByteString;
begin
  if Opendialog1.Execute then
  begin
    if not Assigned(fTestMessage) then
      fTestMessage := TmvMessage.Create(nil);

    reader := nil;
    s := TFileStream.Create(OpenDialog1.FileName, fmOpenRead or fmShareDenyNone);
    try
      reader := TStreamTextReader.Create(s);
      fTestMessage.Header.Clear;
      while reader.ReadLn(raw, ';') and (raw <> '') do
        fTestMessage.Header.Add(raw);

      FixHeaders(fTestMessage.Header);

      s.Seek(reader.Position, soBeginning);
      fTestMessage.RawData.Clear;
      fTestMessage.RawData.CopyFrom(s, s.Size - s.Position);
      pnlDetailsBar.Caption := '  Test Message ' + OpenDialog1.FileName;
      MessageScrollBox1.Msg := nil;
      MessageScrollBox1.Msg := fTestMessage;
    finally
      reader.Free;
      s.Free;
    end;
  end;
end;

procedure TfmMain.actToolsMessagebaseManagementExecute(Sender: TObject);
var
  dlg: TdlgMessagebaseManagement;
  fn: TfnIterator;
  pc: TGroupIteratorProc;
  filter: TNNTPFilter;
  dt: TDateTime;
  settings: TDisplaySettings;
  acc: TNNTPAccount;
  grp: TSubscribedGroup;
begin
  acc := GetFocusedAccount;
  grp := GetFocusedGroup;
  dlg := TdlgMessagebaseManagement.Create(nil);
  try
    if acc = nil then
    begin
      settings := NNTPAccounts.DisplaySettings;
      dlg.rbSelectedGroups.Enabled := False;
      dlg.rbAllGroupsInSelectedAccount.Enabled := False;
      dlg.rbAllGroups.Checked := True;
    end
    else
      if grp = nil then
      begin
        settings := acc.DisplaySettings;
        dlg.rbSelectedGroups.Enabled := False;
        dlg.rbAllGroupsInSelectedAccount.Checked := True;
      end
      else
        settings := grp.DisplaySettings;

    dlg.rgManagementAction.ItemIndex := settings.MessagebaseManagementAction;

    if settings.MessagebaseManagementDefault then
      dlg.rbMoreThanAMonth.Checked := True
    else
      dlg.rbMoreThanAWeek.Checked := True;

    if dlg.ShowModal = mrOK then
    begin
      fn := nil;
      pc := nil;
      dt := Trunc(Now);
      if dlg.rbMoreThanAWeek.Checked then
        dt := IncWeek(dt, -1)
      else
        if dlg.rbMoreThanAMonth.Checked then
          dt := IncMonth(dt, -1)
        else
        begin
          dt := Trunc(dlg.DatePicker.DateTime);
          if dlg.TimePicker.Checked then
            dt := dt + Frac(dlg.TimePicker.DateTime);
        end;

      filter := TNNTPFilter.Create('', ftDate, opLess, dt, False, False, False);
      try
        case dlg.rgManagementAction.ItemIndex of
          0: pc := DoMarkArticlesAsRead;
          1: pc := DoDeleteArticles;
          2: pc := DoMarkThreadsAsRead;
          3: pc := DoDeleteThreads;
        end;

        if dlg.rbAllGroups.Checked then
          fn := ForEachSubscribedGroup
        else
          if dlg.rbAllGroupsInSelectedAccount.Checked then
            fn := ForEachGroupInSelectedAccount
          else
            if dlg.rbSelectedGroups.Checked then
              fn := ForEachSelectedGroup;

        if Assigned(fn) and Assigned(pc) then
          fn(pc, dlg.rgManagementAction.ItemIndex in [1, 3], LPARAM(filter));

        settings.MessagebaseManagementAction := dlg.rgManagementAction.ItemIndex;
        if not dlg.rbOlderThan.Checked then
          settings.MessagebaseManagementDefault := dlg.rbMoreThanAMonth.Checked;
        NNTPAccounts.SaveToRegistry;

      finally
        filter.Free;
      end;

      Refresh_vstArticles;
      Refresh_vstSubscribed;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfmMain.actToolsNewsgroupStatisticsExecute(Sender: TObject);
var
  dlg: TfmNewsgroupStatistics;
  grp: TSubscribedGroup;
begin
  grp := GetFocusedGroup;

  if Assigned(grp) then
  begin
    dlg := TfmNewsgroupStatistics.Create(nil);
    try
      dlg.Group := grp;
      dlg.ShowModal;
    finally
      dlg.Free;
    end;
  end;
end;

procedure TfmMain.actToolsOptionsExecute(Sender: TObject);
var
  fm: TfmOptions;
  ctnr: TArticleContainer;
begin
  SetControlOptions(False);
  ctnr := fLastFocusedArticleContainer;
  if ctnr <> nil then
    if (not NNTPAccounts.ShowSecrets) and (ctnr.Secret or ((ctnr is TSubscribedGroup) and (TSubscribedGroup(ctnr).Owner.Secret))) then
      ctnr := nil;

  fOptionsFormActive := True;
  try
    fm := TfmOptions.Create(Self);
    try
      if fm.ShowModal = mrOK then
      begin
        XNOptions.Save;
        if fDisableShortcutCount = 0 then
          XNOptions.SaveKeyboardShortcuts;
        NNTPAccounts.SaveToRegistry;

        vstSubscribed.RootNodeCount := 0;
        Reinit_vstSubscribed(True);
        ApplyControlOptions;

        fPrevArticle := nil;
        FocusArticleContainer(ctnr);
        SyncContainerTree(ctnr);
      end
      else
        SetControlOptions(False);
    finally
      fm.Free;
    end;
  finally
    fOptionsFormActive := False;
  end;
end;

procedure TfmMain.actToolsPurgeDeletedMessagesExecute(Sender: TObject);
var
  obj: TObject;
  ctnr: TArticleObjectContainer;
begin
  obj := GetNodeObject(vstSubscribed.FocusedNode);
  if obj is TArticleObjectContainer then
  begin
    ctnr := TArticleObjectContainer(obj);
    PurgeCtnr(ctnr, False, False, True)
  end
  else
    if obj is TNNTPAccount then
    begin
      BeginPurge(nil);
      try
        ForEachGroupInSelectedAccount(nil, True)
      finally
        EndPurge;
      end;
    end
    else
    begin
      if obj is TArticleContainer then
        BeginPurge(TArticleContainer(obj))
      else
        BeginPurge(nil);
      try
        NNTPAccounts.PurgeOldArticles;
      finally
        EndPurge;
      end;
    end;
end;

procedure TfmMain.actToolsReconnectExecute(Sender: TObject);
var
  obj: TObject;
begin
  // Handle Restart/Cancel button.
  obj := GetFocusedObject;

  case ThreadManager.ThreadManagerState[obj] of
    tmPending: ThreadManager.JogThreads;
    tmBusy:
      if obj is TArticle then
      begin
        ForEachSelectedArticle(DoCancelArticle);
        vstArticles.Invalidate;
      end
      else
      begin
        if obj is TSubscribedGroup then
          ForEachSelectedGroup(DoCancelGroup, False)
        else
          if obj is TNNTPAccount then
            ThreadManager.Cancel(TNNTPAccount(obj).NNTPServerSettings, nil, nil);
        vstSubscribed.Invalidate;
      end;
  end;
end;

procedure TfmMain.actTrayExitExecute(Sender: TObject);
begin
  // They clicked 'Exit' on the tray icon menu.  Set a flag so that we definately close.
  fCanClose := True;
  Close;
end;

procedure TfmMain.actTrayOpenExecute(Sender: TObject);
var
  i: Integer;
  f: TForm;
  article: TArticle;
begin
  if IsIconic(Handle) then
    ShowWindow(Handle, SW_RESTORE)
  else
    if fTrayed then
    begin
      ShowWindow(Handle, SW_SHOW);
      if fWasMaximized then
        ShowWindow(Handle, SW_SHOWMAXIMIZED)
      else
        ShowWindow(Handle, SW_SHOW);
      Application.ProcessMessages;

      for i := 0 to fModelessWindowList.Count - 1 do
      begin
        f := TForm(fModelessWindowList[i]);
        ShowWindow(f.Handle, SW_SHOW);
      end;

      if fSnapToFirstUnreadMessageAfterTray then
      begin
        article := NNTPAccounts.FirstArticle;
        NextArticle([naUnreadOnly, naCanLeaveGroup, naCircularAccounts], article);
      end;
      fTrayed := False;
    end
    else
      SetForegroundWindow(Handle);
end;

procedure TfmMain.actViewFindTextOnInternetExecute(Sender: TObject);
var
  art: TArticleBase;
  txt, url: string;
  wtxt: string;
begin
  art := GetFocusedArticle;
  if not Assigned(art) then Exit;

  MessageScrollBox1.GetSelectedText(wtxt);
  txt := Trim(wtxt);

  url := StringReplace(XNOptions.TextInternetURLStub, '%qtext%', txt, []);
  url := TidURI.URLEncode(url);
  ShellExecute(Handle, 'open', PChar(url), nil, nil, SW_SHOW);
end;

procedure TfmMain.actViewHeadersFullExecute(Sender: TObject);
begin
  XNOptions.ShowHeader := shFull;
  MessageScrollBox1.ShowHeader := shFull;
end;

procedure TfmMain.actViewHeadersNoneExecute(Sender: TObject);
begin
  XNOptions.ShowHeader := shNone;
  MessageScrollBox1.ShowHeader := shNone;
end;

procedure TfmMain.actViewHeadersShortExecute(Sender: TObject);
begin
  XNOptions.ShowHeader := shShort;
  MessageScrollBox1.ShowHeader := shShort;
end;

procedure TfmMain.actViewMessagesNormalExecute(Sender: TObject);
begin
  XNOptions.ViewMode := vmNormal;
  MessageScrollBox1.ImagesOnly := False;
  MessageScrollBox1.RawMessage := False;
  MessageScrollBox1.RawMode := False;
  MessageScrollBox1.Refresh(True, True);
end;

procedure TfmMain.actViewMessagesRawMessagesExecute(Sender: TObject);
begin
  XNOptions.ViewMode := vmRaw;
  MessageScrollBox1.ImagesOnly := False;
  MessageScrollBox1.RawMode := True;
  MessageScrollBox1.RawMessage := True;
  MessageScrollBox1.Refresh(True, True);
end;

procedure TfmMain.actViewMessagesRawTextExecute(Sender: TObject);
begin
  XNOptions.ViewMode := vmRawText;
  MessageScrollBox1.ImagesOnly := False;
  MessageScrollBox1.RawMessage := False;
  MessageScrollBox1.RawMode := True;
  MessageScrollBox1.Refresh(True, True);
end;

procedure TfmMain.ApplicationEvents1Activate(Sender: TObject);
var
  i: Integer;
  newState, oldState: Boolean;
  btn: TToolButton;
begin
  // Enable the toolbar buttons if their actions are enabled
  for i := 0 to tbMenu.ButtonCount - 1 do
  begin
    btn := tbMenu.Buttons[i];
    oldState := btn.Enabled;
    if btn.Action is TAction then
      newState := TAction(btn.Action).Enabled
    else
      newState := True;
    if newState <> oldState then
      btn.Enabled := newState;
  end;

  for i := 0 to tbMain.ButtonCount - 1 do
  begin
    btn := tbMain.Buttons[i];
    oldState := btn.Enabled;
    if btn.Action is TAction then
      newState := TAction(btn.Action).Enabled
    else
      newState := True;
    if newState <> oldState then
      btn.Enabled := newState;
  end;
end;

procedure TfmMain.ApplicationEvents1Deactivate(Sender: TObject);
var
  i: Integer;
begin
  // Disable toolbar buttons if the application is deactivated.  OE does this...
  for i := 0 to tbMenu.ButtonCount - 1 do
    tbMenu.Buttons[i].Enabled := False;
  for i := 0 to tbMain.ButtonCount - 1 do
    tbMain.Buttons[i].Enabled := False;
end;

procedure TfmMain.ApplicationEvents1Exception(Sender: TObject; E: Exception);
begin
  LogMessage(E.Message, True);
  if E is EidSocketError then
    if (EidSocketError(E).LastError = 10054) or (EidSocketError(E).LastError = 0) then
      MessageBeep($FFFF)       // WSACONNRESET.  Not really an error, but
                               // squeak anyway.
    else
      Application.ShowException(E)
  else
    Application.ShowException(E);
end;

function TfmMain.ApplicationEvents1Help(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
  // Work round bug in help system                                        |
  CallHelp := not (command = HELP_FINDER);
  if not CallHelp then
    PostMessage(Application.Handle, CM_INVOKEHELP, HELP_CONTENTS, 0);
  Result := True;
end;

procedure TfmMain.ApplicationEvents1Hint(Sender: TObject);
begin
  if Monitor.Handle = 0 then ApplicationEvents1.CancelDispatch;
end;

type
  TCustomCracker = class(TCustomControl)
  public
    property Canvas;
  end;

procedure TfmMain.ResizeSearchBar;
begin
  with rbSearchBarSearch do
    Width := TCustomCracker(pnlSearchBar).Canvas.TextWidth(Caption) + 20;
  with rbBookmark do
  begin
    Left := rbSearchBarSearch.Left + rbSearchBarSearch.Width + 8;
    Width := TCustomCracker(pnlSearchBar).Canvas.TextWidth(Caption) + 20;
  end;
  with rbFilter do
  begin
    Left := rbBookmark.Left + rbBookmark.Width + 8;
    Width := TCustomCracker(pnlSearchBar).Canvas.TextWidth(Caption) + 20;
  end;
  with btnGo do
    Left := rbFilter.Left + rbFilter.Width + 8;
end;

procedure TfmMain.ApplyControlOptions;
var
  i: Integer;
begin
  pnlLeft.Width := XNOptions.PanelLeft;
  pnlArticles.Height := XNOptions.ArticlesHeight;
  pnlQueuedRequests.Height := XNOptions.QueuedRequestsHeight;
  pnlBookmark.Height := XNOptions.BookmarkHeight;

  tbMenu.SetBounds(XNOptions.MenuToolbarLeft, XNOptions.MenuToolbarTop, tbMenu.Width, tbMenu.Height);
  tbMain.SetBounds(XNOptions.MainToolbarLeft, XNOptions.MainToolbarTop, tbMain.Width, tbMain.Height);

  Self.Color := XNOptions.Appearance[apMainForm].ApplyFontAndGetColor(Self.Font);
  cbMain.Color := XNOptions.Appearance[apToolBar].ApplyFontAndGetColor(cbMain.Font);
  pnlDetailsBar.Color := XNOptions.Appearance[apMessageDetailsPanel].ApplyFontAndGetColor(pnlDetailsBar.Font);
  tbMenu.Color := XNOptions.Appearance[apMenu].ApplyFontAndGetColor(tbMenu.Font);
  Screen.MenuFont := tbMenu.Font;
  ResizeSearchBar;

  vstArticles.Color := XNOptions.Appearance[apMessageHeaders].ApplyFontAndGetColor(vstArticles.Font);
  vstBookmark.Color := XNOptions.Appearance[apMessageHeaders].ApplyFontAndGetColor(vstArticles.Font);
  vstArticles.Header.MainColumn := XNOptions.TreeColumn;
  vstSubscribed.Color := XNOptions.Appearance[apSubscribedGroups].ApplyFontAndGetColor(vstSubscribed.Font);
  vstQueuedRequests.Color := XNOptions.Appearance[apSubscribedGroups].ApplyFontAndGetColor(vstQueuedRequests.Font);
  MessageScrollBox1.Color := XNOptions.Appearance[apMessagePane].ApplyFontAndGetColor(MessageScrollBox1.Font);
  MessageScrollBox1.ShowHeader := XNOptions.ShowHeader;
  MessageScrollBox1.Refresh(False, True);
  TrayIcon1.Visible := XNOptions.ShowInSystemTray;

  vstSubscribed.ShowHint := XNOptions.ShowTooltips;
  vstArticles.ShowHint := XNOptions.ShowTooltips;

  for i := 0 to vstArticles.Header.Columns.Count - 1 do
  begin
    vstArticles.Header.Columns[i].Position := XNOptions.ArticlesColumnPositions[i];
    if XNOptions.HideColumn[i] then
      vstArticles.Header.Columns[i].Options := vstArticles.Header.Columns[i].Options - [coVisible]
    else
      vstArticles.Header.Columns[i].Options := vstArticles.Header.Columns[i].Options + [coVisible];
  end;

  pnlDetailsBar.Visible := XNOptions.ShowDetailsBar;

  if Assigned(fLastFocusedArticleContainer) then
  begin
    if fLastFocusedArticleContainer.HideReadMessages <> XNOptions.HideReadMessages then
      actViewHideReadMessagesExecute(Self);

    if fLastFocusedArticleContainer.HideIgnoredMessages <> XNOptions.HideIgnoredMessages then
      actViewHideIgnoredMessagesExecute(Self);

    if fLastFocusedArticleContainer.HideMessagesNotToMe then
      actViewHideMessagesNotToMeExecute(Self);
  end;

  if XNOptions.UseVistaExplorerTheme then
  begin
     vstArticles.TreeOptions.PaintOptions := vstSubscribed.TreeOptions.PaintOptions + [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
     vstBookmark.TreeOptions.PaintOptions := vstSubscribed.TreeOptions.PaintOptions + [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
     vstSubscribed.TreeOptions.PaintOptions := vstSubscribed.TreeOptions.PaintOptions + [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
     vstQueuedRequests.TreeOptions.PaintOptions := vstSubscribed.TreeOptions.PaintOptions + [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
  end
  else
  begin
     vstArticles.TreeOptions.PaintOptions := vstSubscribed.TreeOptions.PaintOptions - [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
     vstBookmark.TreeOptions.PaintOptions := vstSubscribed.TreeOptions.PaintOptions - [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
     vstSubscribed.TreeOptions.PaintOptions := vstSubscribed.TreeOptions.PaintOptions - [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
     vstQueuedRequests.TreeOptions.PaintOptions := vstSubscribed.TreeOptions.PaintOptions - [toHotTrack, toUseExplorerTheme, toHideTreeLinesIfThemed];
  end;

  vstSubscribed.DefaultNodeHeight := Abs(vstSubscribed.Font.Height) + 7;
  vstArticles.DefaultNodeHeight := Abs(vstArticles.Font.Height) + 7;

  Refresh_vstArticles;
  ResizeArticleHeader;
  ResizeBookmarkHeader;
  if XNOptions.AutoExpandGroupTree then
    vstSubscribed.FullExpand;
  MessageScrollBox1.AutoFit := XNOptions.AutofitImages;

  InitISpell(XNOptions.ISpellDirectory);
end;

procedure TfmMain.BatchToParams(batch: TBatchAction; var params: TGetMessagesParams);
begin
  // Get the 'TGetMessageParams' for <batch>.
  if batch <> nil then
  begin
    params.fHeadersOnly := batch.HeadersOnly;
    params.fMessageCount := batch.MessageCount;
    params.fActionType := batch.ActionType;
    params.fManagementType := batch.ManagementType;
    params.fManagementOption := batch.ManagementOption;
    params.fManagementCount := batch.ManagementCount;
    params.fSince := batch.Since;
  end
  else
  begin
    FillChar(params, SizeOf(params), 0);
    params.fActionType := batAllNew;
  end;
end;

procedure TfmMain.CentralizeDisplay;
var
  bm: Integer;
  s, s1, n: PVirtualNode;
  r: TRect;
begin
  // Ensure that the articles scroll box is suitably centralized and  expanded.                                                            |
  vstArticles.BeginUpdate;
  try
    bm := vstArticles.ClientHeight div 2;

    s := nil;
    s1 := nil;
    n := vstArticles.FocusedNode;

    r := vstArticles.GetDisplayRect(n, 0, False);

    if r.Top > bm then
    begin
      while Assigned(n) and (bm > n.NodeHeight) do
      begin
        s1 := n;
        if XNOptions.AutoExpandThread then
          vstArticles.Expanded[n] := True;
        bm := bm - n.NodeHeight;
        s := vstArticles.GetNext(n);
        n := s;
      end;

      if s = nil then
        s := s1;

      if s <> nil then
        vstArticles.ScrollIntoView(s, False);
    end
  finally
    vstArticles.EndUpdate;
  end;
end;

class function TfmMain.CheckRunOnce: Boolean;
var
  fName: string;
begin
  // Return False if another instance of XanaNews is running.
  fName := UpperCase(ExtractFileName(ParamStr(0)));
  gMutex := CreateMutex(nil, True, PChar(fName));
  Result := GetLastError = 0;
  gUniqueMessage := RegisterWindowMessage(PChar(fName));
  if not Result then
  begin
    EnumWindows(@EnumWindowsProc, gUniqueMessage);
    TerminateProcess(GetCurrentProcess, 1);
  end
end;

function TfmMain.CheckSaveOutboxMessages: Boolean;
var
  i: Integer;
  getter: TTCPGetter;
  somethingToSave: Boolean;
  getters: TObjectList;
begin
  // If there are unsent outgoing message, display a confirmation box,
  // and save the outgoing messages for later if requested.
  Result := True;
  somethingToSave := False;     // First check that there are no open
                                // 'Post Message' windows
  for i := 0 to fModelessWindowList.Count - 1 do
    if not (TForm(fModelessWindowList[i]) is TdlgSearch) then
    begin
      somethingToSave := True;
      Break;
    end;

  if somethingToSave then
    if fEndSession then
      Result := True
    else
      case MessageBox(handle, PChar(rstOutstandingEditors), PChar(Application.Title), MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2) of
        ID_NO: Result := False;
      end;

  if Result then                // The check that there are no queued posts.
  begin
    somethingToSave := False;
    getters := ThreadManager.LockGetterList;
    try
      for i := 0 to getters.Count - 1 do
      begin
        getter := TTCPGetter(getters[i]);

        if ((Getter is TPoster) or (Getter is TEmailer)) and (Getter.OutstandingRequestCount > 0) then
        begin
          somethingToSave := True;
          Break;
        end
      end
    finally
      ThreadManager.UnlockGetterList
    end;

    if somethingToSave then
      SaveOutstandingPostings;
  end;
end;

procedure TfmMain.CheckScheduledBatches;
var
  i, j, k: Integer;
  batch: TNNTPBatch;
  getter: TArticlesGetter;
  request: TArticlesGetterRequest;
  busy: Boolean;
  requests, getters: TObjectList;
begin
  for i := 0 to NNTPAccounts.BatchesCount - 1 do
  begin
    batch := NNTPAccounts.Batches[i];

    if batch.LastBatchRef = 0 then
    begin
      if ((batch.RunEvery <> 0) and batch.Scheduled) or (batch.RunAtStart and not batch.HasRunThisSession) then
      begin
        if batch.LastRun = 0 then
          batch.LastRun := Now;

        if (batch.RunAtStart and not batch.HasRunThisSession) then
          RunBatch(batch)
        else
          if MinutesBetween(Now, batch.LastRun) >= batch.RunEvery then
            RunBatch(batch);
      end;
    end
    else
    begin
      if SecondsBetween(Now, batch.LastRun) >= 2 then
      begin
        busy := False;
        j := 0;
        getters := ThreadManager.LockGetterList;
        try
          while not busy and (j < getters.Count) do
          begin
            if getters[j] is TArticlesGetter then
            begin
              getter := TArticlesGetter(getters[j]);
              if getter.State = tsBusy then
              begin
                k := 0;
                requests := getter.LockList;
                try
                  while not busy and (k < requests.Count) do
                  begin
                    request := TArticlesGetterRequest(requests[k]);
                    if request.BatchRef = batch.LastBatchRef then
                      busy := True;
                    Inc(k);
                  end;
                finally
                  getter.UnlockList;
                end;
              end;
            end;
            Inc(j);
          end
        finally
          ThreadManager.UnlockGetterList
        end;
        if not busy then
          batch.LastBatchRef := 0
        else
          batch.LastRun := Now;
      end;
    end;

    batch.HasRunThisSession := True;
  end;
end;

function TfmMain.CreateModelessWindow(cls: TComponentClass): TForm;
begin
  Application.CreateForm(cls, Result);
  Result.OnActivate := ModelessWindowFormActivate;
  Result.OnDeactivate := ModelessWindowFormDeactivate;
  Result.OnDestroy := ModelessWindowFormDestroy;
  fModelessWindowList.Add(Result);
end;

procedure TfmMain.DisplayArticleBody(article: TArticleBase);
var
  idx: Integer;
  S: string;
  st: string;
  sub: string;
begin
  if article <> fPrevArticle then
  begin
    if fPrevArticle <> nil then
      fPrevArticleStack.Push(fPrevArticle);
    fPrevArticle := article;
    if article <> nil then
    begin
      if not fNXStack then
        fNextArticleStack.Clear
      else
        fNXStack := False;
    end;
  end;

  spGoToWebForum.Visible := False;

  if Assigned(article) then
  begin
    S := article.Header['X-Post-url'];
    if S <> '' then
    begin
      spGoToWebForum.Hint := 'Look up this message on the web forum|' + S;
      spGoToWebForum.Visible := True;
    end;

    if Assigned(article.Msg) and Assigned(article.Owner) then
    begin
      article.Msg.TruncateFrom := article.Owner.DisplaySettings.TruncateFrom;
      article.Msg.StrictSigSeparator := XNOptions.StrictSigSep;
    end;

    // Note: Read CodePage, required for updating the CodePage of the message
    //       in case there is nothing defined in the post.
    idx := cbCharset.Items.IndexOf(CodePagetoCharsetName(article.CodePage));
    cbCharset.ItemIndex := idx;

    MessageScrollBox1.Msg := article.Msg;
    sub := article.Subject;
    st := Format('  Message %d from %s.  %s', [article.ArticleNo, article.FromName, sub]);
    pnlDetailsBar.Caption := StringReplace(st, '&', '&&', [rfReplaceAll]);
  end
  else
  begin
    MessageScrollBox1.Msg := nil;
    pnlDetailsBar.Caption := '';
  end
end;

procedure TfmMain.DisplayBookmarks(visible: Boolean);
begin
  pnlBookmark.Visible := visible;
  actViewShowBookmarkPane.Checked := visible;
  if visible then
  begin
    if pnlBookmark.Height > pnlRight.Height div 2 then
      pnlBookmark.Height := pnlRight.Height div 2;
    if pnlBookmark.Height + 120 > pnlArticles.Height then
      pnlArticles.Height := pnlBookmark.Height + 120;

    spltBookMark.Visible := visible;
    ResizeBookmarkHeader;
  end
  else
    spltBookMark.Visible := visible;
  XNOptions.ShowBookmark := visible;
end;

procedure TfmMain.DoCancelArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
begin
  // 'For each' handler.  Cancel activities for the selected article.
  ThreadManager.Cancel(article.Owner.ServerSettings, TSubscribedGroup(article.Owner), article);
end;

function TfmMain.DoCancelGroup(group: TSubscribedGroup; param: LPARAM): Boolean;
begin
  // 'For each' handler.  Cancel activities for the selected group.
  ThreadManager.Cancel(group.Owner.NNTPServerSettings, group, nil);
  Result := False;
end;

procedure TfmMain.DoCheckArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
var
  filter: TNNTPFilter;
begin
  // 'For each' handler.  Check article matches the filter.
  filter := TNNTPFilter(param);
  if Assigned(filter) and not filter.Matches(article) then
    fIteratorFailed := True;
end;

procedure TfmMain.DoDeleteArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
var
  delete: Boolean;
begin
  // ForEachSelectedArticle callback.  Delete/Undelete the article.
  delete := Boolean(param);
  article.IsDeleted := delete;
  if delete and not article.IsInteresting then // Mark deleted articles as read.
    article.IsRead := True;
end;

function TfmMain.DoDeleteArticles(group: TSubscribedGroup; param: LPARAM): Boolean;
var
  i: Integer;
  article: TArticle;
  filter: TNNTPFilter;
begin
  // ForEachSelectedGroup callback.  Delete articles that match the  delete parameters.                                                   |
  filter := TNNTPFilter(param);

  group.BeginLock;
  try
    for i := 0 to group.LoadGetArticleCount - 1 do
    begin
      article := group.Articles[i];
      if not article.IsInteresting and (not Assigned(filter) or filter.Matches(article)) then
      begin
        article.IsDeleted := True;
        article.RawMarkAsRead;
      end;
    end;
  finally
    group.EndLock;
  end;

  Result := False;
end;

function TfmMain.DoDeleteThreads(group: TSubscribedGroup; param: LPARAM): Boolean;
var
  rootArticle: TArticle;
  i: Integer;
begin
  if group.ThreadOrder = toThreaded then
    for i := 0 to Group.ThreadCount - 1 do
    begin
      rootArticle := TArticle(group.Threads[i]);
      ForEachArticleInThread(rootArticle, DoCheckArticle, param);
      if not fIteratorFailed then
        ForEachArticleInThread(rootArticle, DoDeleteArticle, 1);
    end;
  fIteratorFailed := False;
  Result := False;
end;

procedure TfmMain.DoFlagArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
begin
  article.IsInteresting := Boolean(param);
end;

procedure TfmMain.DoGetArticleBody(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
var
  art: TArticle;
begin
  if not multiSelect or not article.HasMsg then
    if article is TArticle then
    begin
      art := TArticle(article);
      ThreadManager.GetArticleBody(art.Account, art.SubscribedGroup, art);
    end;
end;

procedure TfmMain.DoGetArticleThread(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
var
  art: TArticle;
begin
  // Get's body for article's entire thread.
  if article is TArticle then
  begin
    art := TArticle(article);
    ThreadManager.GetArticleThreadBodies(art.Account, art.SubscribedGroup, art);
  end;
end;

function TfmMain.DoGetMessages(group: TSubscribedGroup; params: LPARAM): Boolean;
var
  p: TGetMessagesParams;
  doIt: Boolean;
  lart, fart, acount: Int64;

  filter: TNNTPFilter;
  dt: TDateTime;
begin
  Result := False;
  p := PGetMessagesParams(params)^;
  if ThreadManager.GettingArticleList(group, p.fManagementOption = bmoNone) then Exit;

  if p.useDefaultGroupSettings then
  begin
    BatchToParams(group.NNTPSettings.DefaultAction, p);
    group.ActionPerformedThisSession := True;
  end;

  lart := -2;
  if p.fManagementOption <> bmoNone then
  begin
    filter := nil;
    try
      if p.fManagementOption in [bmoWeek, bmoDay, bmoMonth] then
      begin
        dt := Trunc(Now);
        case p.fManagementOption of
          bmoWeek : dt := IncWeek(dt, -p.fManagementCount);
          bmoMonth: dt := IncMonth(dt, -p.fManagementCount);
          bmoDay  : dt := IncDay(dt, -p.fManagementCount);
        end;
        filter := TNNTPFilter.Create('', ftDate, opLess, dt, False, False, False);
      end;

      if p.fManagementType = bmtDelete then
      begin
        if ThreadManager.StopArticleDownloads(group) then
          ClearSynchronizedMethods;
        lart := group.LastArticle + 1;
        if p.fManagementOption = bmoAll then
          PurgeCtnr(group, True, (p.fromArticle = 0) and (p.fActionType <> batAllNew), False)
        else
        begin
          DoDeleteArticles(group, LPARAM(filter));
          PurgeCtnr(group, False, False, True);
        end;
        if p.fActionType = batAll then
        begin
          lart := -2;
          p.fActionType := batAllNew;
        end;
      end
      else
        DoMarkArticlesAsRead(group, LPARAM(filter));

      // When something goes wrong during retreiving messages and this batch is
      // restarted don't delete or mark as read the messages again.
      // Especially when deleting message first, it will run forever otherwise.
      p.fManagementOption := bmoNone;
    finally
      filter.Free;
    end;
  end;

  if p.fActionType <> batNone then
  begin
    acount := 0;
    fart := 0;
    doit := True;
    if p.fromArticle > 0 then
    begin
      fart := p.fromArticle;
      acount := p.fMessageCount;
    end
    else
      case p.fActionType of
        batAll   : fart := 0;
        batAllNew: fart := lart;
        batNextN : begin
                     fart := lart;
                     acount := p.fMessageCount;
                   end;
        batLastN : begin
                     fart := 0;
                     acount := p.fMessageCount;
                     if acount < 1 then
                       doit := False;
                   end;
        batSince : fart := -3;
      end;

    if doit then
      ThreadManager.GetArticles(group.Owner, group, fart, acount, not p.fHeadersOnly, p.batchRef, p.fSince);
  end;
end;

function TfmMain.DoMarkArticlesAsRead(group: TSubscribedGroup; param: LPARAM): Boolean;
var
  article: TArticle;
  i: Integer;
  filter: TNNTPFilter;
  read, changed: Boolean;
begin
  if param = 1 then
  begin
    read := False;
    filter := nil;
  end
  else
  begin
    read := True;
    filter := TNNTPFilter(param);
  end;

  changed := False;
  if (not read) or (group.UnreadArticleCount > 0) then
  begin
    group.BeginLock;
    try
      for i := 0 to group.LoadGetArticleCount - 1 do
      begin
        article := group.Articles[i];
        if not Assigned(filter) or filter.Matches(article) then
          if article.IsRead <> read then
          begin
            article.IsRead := read;
            changed := True;
          end;
      end;
    finally
      group.EndLock;
    end;
  end;

  if changed and not read then
    fSuppressSound := True;
  Result := False;
end;

procedure TfmMain.DoMarkAsReadArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
var
  mark: Boolean;
begin
  mark := Boolean(param);
  article.IsRead := mark;

  if not mark then fSuppressSound := True;
end;

function TfmMain.DoMarkThreadsAsRead(group: TSubscribedGroup; param: LPARAM): Boolean;
var
  rootArticle: TArticleBase;
  i: Integer;
begin
  if group.ThreadOrder = toThreaded then
    for i := 0 to Group.ThreadCount - 1 do
    begin
      rootArticle := group.Threads[i];
      ForEachArticleInThread(rootArticle, DoCheckArticle, param);
      if not fIteratorFailed then
        ForEachArticleInThread(rootArticle, DoMarkAsReadArticle, 1);
    end;
  fIteratorFailed := False;
  Result := False;
end;

procedure TfmMain.DoMoveToSelectedFolder(article: TArticleBase;
  param: LPARAM; multiSelect: Boolean);
var
  fldr: TArticleFolder;
begin
  fldr := TArticleFolder(param);
  fldr.AddArticle(article);
end;

procedure TfmMain.DoOnArticleChanged(Sender: TObject; article: TArticleBase);
var
  idx: Integer;
begin
  // Called when each full article has been downloaded.
  if article = GetFocusedArticle then
  begin
    if Assigned(article.Msg) then
    begin
      article.Msg.DefaultCodePage := article.Owner.DisplaySettings.DefaultCodepage;
      article.Msg.TruncateFrom := article.Owner.DisplaySettings.TruncateFrom;
      article.Msg.StrictSigSeparator := XNOptions.StrictSigSep;
      if XNOptions.AutoMarkSeconds = 0 then
        TArticle(article).IsRead := True;
    end;
    MessageScrollBox1.Refresh(MessageScrollBox1.ShowHeader <> shNone, MessageScrollBox1.AutoFit);
    idx := cbCharset.Items.IndexOf(CodePagetoCharsetName(article.CodePage));
    cbCharset.ItemIndex := idx;
  end;
  vstArticles.Invalidate;
  CheckDeferredCombineList(article);
end;

procedure TfmMain.DoOnArticleFailed(Sender: TObject; article: TArticleBase);
begin
  if article = GetFocusedArticle then
    DisplayArticleBody(nil);
  vstArticles.Invalidate;
end;

procedure TfmMain.DoOnArticlesChanged(Sender: TObject; group: TArticleContainer);
var
  art: TArticleBase;
  x, y: Integer;
begin
  // Called when the complete list of articles has been received.
  if fLastFocusedArticleContainer = group then
  begin
    x := MessageScrollBox1.HorzScrollBar.Position;
    y := MessageScrollBox1.VertScrollBar.Position;
    art := GetFocusedArticle;
    DisplayArticleBody(nil);
    InitArticlesRootNodeCount(group);
    Refresh_vstArticles;
    GoToArticle(art);
    if Assigned(art) then
    begin
      MessageScrollBox1.HorzScrollBar.Position := x;
      MessageScrollBox1.VertScrollBar.Position := y;
    end;
  end;
  Refresh_vstSubscribed;
end;

procedure TfmMain.DoOnClearArticle(Sender: TObject; article: TArticleBase);
begin
  // Called before re-downloading an article
  if article = GetFocusedArticle then
    DisplayArticleBody(nil);
  vstArticles.Invalidate;
end;

procedure TfmMain.DoOnNewsgroupsChanged(Sender: TObject; account: TNNTPAccount);
begin
  // Called when the list of newsgroups has been downloaded.
  vstSubscribed.Invalidate;     // Change red earth icon back to blue

  if (account = GetFocusedAccount) and (account.SubscribedGroupCount = 0) then
    PostMessage(Handle, WM_SHOWNEWSGROUPLIST, 0, LPARAM(account));
end;

procedure TfmMain.DoOnNotifyError(Sender: TObject; const error: string);
begin
  // Called when an exception has occured in the  thread manager.
  vstArticles.Invalidate;
end;

procedure TfmMain.DoOnStartArticle(Sender: TObject; article: TArticleBase);
begin
  if article = GetFocusedArticle then
    DisplayArticleBody(article);
  vstArticles.Invalidate;
end;

procedure TfmMain.EnableShortcuts(enable: Boolean);
var
  i: Integer;
  action: TCustomAction;
begin
  // Enable/Disable main menu shortcuts.  Disable shortcuts before
  // displaying modeless forms - otherwise the shortcuts trigger instead
  // of (eg.) typing space into edit box.
  if not enable then         // Only disable on first call
  begin
    Inc(fDisableShortcutCount);
    if fDisableShortcutCount > 1 then
      Exit;
  end
  else                       // Only enable on last call
  begin
    Dec(fDisableShortcutCount);
    if fDisableShortcutCount < 0 then
      fDisableShortcutCount := 0;
    if fDisableShortcutCount <> 0 then
      Exit;
  end;

  for i := 0 to alMain.ActionCount - 1 do
  begin
    action := TCustomAction(alMain.Actions[i]);

    if enable then   // Re-enable shortcuts by restoring action shortcuts from action tags.
    begin
      action.ShortCut := action.Tag;
      action.Tag := 0;
    end
    else
    begin             // Disable shortcut by setting the action shortcut  to 0
                      // Save the old shortcut in 'Tag' so it can be restored.
      action.Tag := action.Shortcut;
      action.Shortcut := 0;
    end;
  end;
end;

procedure TfmMain.FindDialog1Find(Sender: TObject);
var
  st: TStringSearchOptions;
begin
  st := [];

  if frMatchCase in FindDialog1.Options then
    st := st + [soMatchCase];

  if frWholeWord in FindDialog1.Options then
    st := st + [soWholeWord];

  if frDown in FindDialog1.Options then
    st := st + [soDown];

  fLastFindSucceeded := MessageScrollBox1.FindText(FindDialog1.FindText, not fLastFindSucceeded, st);
end;

function TfmMain.ForEachArticleInBranch(var branch: TArticleBase;
  proc: TArticleIteratorProc; param: LPARAM): Integer;
var
  p, p1: TArticleBase;
begin
  Result := 1;
  proc(branch, param, True);
  p1 := branch;
  fIteratorFailed := False;

  p := branch.Child;
  while not fIteratorFailed and (p <> nil) do
  begin
    Inc(Result);
    proc(p, param, True);
    p1 := p;
    if fIteratorFailed then Break;

    if Assigned(p.Child) then
      p := p.Child
    else
      if Assigned(p.sibling) then
        p := p.Sibling
      else
      begin
        p := p.parent;
        while Assigned(p) do
        begin
          if p = branch then
          begin
            p := nil;
            Break;
          end;

          if Assigned(p.Sibling) then
          begin
            p := p.Sibling;
            Break;
          end
          else
            p := p.Parent;
        end;
      end;
  end;
  branch := p1;
end;

function TfmMain.ForEachArticleInThread(thread: TArticleBase; proc: TArticleIteratorProc;
  param: LPARAM): Integer;
var
  article: TArticleBase;
begin
  Result := 0;
  article := thread;
  if article = nil then
    Exit;

  while article.Parent <> nil do        // Start at thread root
    article := article.Parent;

  fIteratorFailed := False;
  while not fIteratorFailed and Assigned(article) do
  begin
    Inc(Result);
    proc(article, param, True);
    article := article.Next;
    if (not Assigned(article)) or (not Assigned(article.Parent)) then
      Break;
  end;
end;

function TfmMain.ForEachGroupInSelectedAccount(proc: TGroupIteratorProc; purge: Boolean;
  param: LPARAM): Integer;
var
  account: TNNTPAccount;
  group: TSubscribedGroup;
  i: Integer;
  unsubscribed: Boolean;
begin
  Result := 0;
  account := GetFocusedAccount;
  if Assigned(account) then
  begin
    fIteratorFailed := False;

    for i := 0 to account.SubscribedGroupCount - 1 do
    begin
      group := account.SubscribedGroups[i];
      unsubscribed := False;
      group.BeginLock;
      try
        if Assigned(proc) then
          unsubscribed := proc(group, param);
        if purge then
          PurgeCtnr(group, False, False, True);
      finally
        if not unsubscribed then
          group.EndLock;
      end;
      Inc(Result);
      if fIteratorFailed then
        Break;
    end;
  end;
end;

function TfmMain.ForEachSelectedArticle(proc: TArticleIteratorProc; param: LPARAM = 0;
  expandedToo: Boolean = True): Integer;
var
  node, n1: PVirtualNode;
  article: TArticleBase;
  count: Integer;
  multiSelect: Boolean;
  child: PVirtualNode;

  procedure DoItToBranch(node: PVirtualNode);
  begin
    while Assigned(node) do
    begin
      article := GetNodeArticle(node);
      if Assigned(article) then
      begin
        Inc(count);
        proc(article, param, multiSelect);

        DoItToBranch(vstArticles.GetFirstChild(node));

        node := vstArticles.GetNextSibling(node);
      end;
    end;
  end;

begin
  node := vstArticles.GetFirstSelected;
  if vstArticles.SelectedCount = 1 then
  begin
    article := GetNodeArticle(node);
    if Assigned(article) and article.Owner.GroupMultipartMessages then
      multiSelect := expandedToo and (vsHasChildren in node^.States)
    else
      multiSelect := expandedToo and not (vsExpanded in node^.States) and (vsHasChildren in node^.States);
  end
  else
    multiSelect := True;

  count := 0;
  fIteratorFailed := False;
  while not fIteratorFailed and Assigned(node) do
  begin
    article := GetNodeArticle(node);
    if Assigned(article) then
    begin
      Inc(count);
      proc(article, param, multiSelect);

      if expandedToo and multiSelect and not fIteratorFailed then
      begin
        child := vstArticles.GetFirstChild(node);
        if Assigned(child) then
        begin
          DoItToBranch(child);
          n1 := vstArticles.GetNextSibling(node);
          if Assigned(n1) then
          begin
            node := n1;
            if not (vsSelected in node^.States) then
              node := vstArticles.GetNextSelected(node)
          end
          else
            node := vstArticles.GetNextSelected(node)
        end
        else
          node := vstArticles.GetNextSelected(node)
      end
      else
        node := vstArticles.GetNextSelected(node)
    end
    else
      node := vstArticles.GetNextSelected(node)
  end;
  Result := count;
end;

function TfmMain.ForEachSelectedGroup(proc: TGroupIteratorProc; purge: Boolean;
  param: LPARAM): Integer;
var
  node: PVirtualNode;
  group: TSubscribedGroup;
  groups: TList;
  i: Integer;
  unsubscribed: Boolean;
begin
 // Runs the number of affected articles.
 // nb. the order is quite important, so don't use vstSubscribed.FirstSelectedNode/GetNextSelectedNode

  groups := nil;
  Result := 0;

  try
    node := GetFirstvstSubscribedSelectedNode;
    if Assigned(node) then
    begin
      groups := TList.Create;

      while Assigned(node) do
      begin
        group := GetNodeSubscribedGroup(node);
        if Assigned(group) then
          groups.Add(group);

        repeat
          node := vstSubscribed.GetNext(node);
        until not Assigned(Node) or (vsSelected in Node.States);
      end;
    end;

    fIteratorFailed := False;

    if Assigned(groups) then
    begin
      for i := 0 to groups.Count - 1 do
      begin
        group := TSubscribedGroup(groups[i]);
        unsubscribed := False;
        group.BeginLock;
        try
          Inc(Result);
          if Assigned(proc) then
            unsubscribed := proc(group, param);
          if purge then
            PurgeCtnr(group, False, False, True);
          if fIteratorFailed then
            Break;
        finally
          if not unsubscribed then
            group.EndLock;
        end;
      end;
    end;
  finally
    groups.Free;
  end;
end;

function TfmMain.ForEachSubscribedGroup(proc: TGroupIteratorProc; purge: Boolean;
  param: LPARAM): Integer;
var
  account: TNNTPAccount;
  group: TSubscribedGroup;
  j, i: Integer;
  unsubscribed: Boolean;
begin
  Result := 0;
  fIteratorFailed := False;
  for j := 0 to NNTPAccounts.Count - 1 do
  begin
    account := NNTPAccounts.Items[j];
    for i := 0 to account.SubscribedGroupCount - 1 do
    begin
      group := account.SubscribedGroups[i];
      unsubscribed := False;
      group.BeginLock;
      try
        unsubscribed := proc(group, param);
        if purge then
          PurgeCtnr(group, False, False, True);
      finally
        if not unsubscribed then
          group.EndLock;
      end;
      Inc(Result);
      if fIteratorFailed then
        Break;
    end;

    if fIteratorFailed then
      Break;
  end;
end;

procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i, j: Integer;
  f: TForm;
begin
  // If there's a tray icon and the fCanClose flag isn't set, hide the
  // application to the system tray, rather than closing it.
  CanClose := fCanClose or not TrayIcon1.Visible;
  if CanClose then
  begin
    CanClose := CheckSaveOutboxMessages;

    if Assigned(XNOptions) then
      SetControlOptions(False);
    TrayIcon1.Visible := False;

    if Assigned(XNOptions) then
    begin
      XNOptions.Save;
      if fDisableShortcutCount > 0 then
      begin
        fDisableShortcutCount := 1;
        EnableShortcuts(True);
      end;
      XNOptions.SaveKeyboardShortcuts;
      SaveToolbarLayout;
      NNTPAccounts.SaveToRegistry;
    end;
  end
  else
  begin         // Minimize to tray
    fWasMaximized := Tag <> 0;

                // Hide modeless windows
    for i := 0 to fModelessWindowList.Count - 1 do
    begin
      f := fModelessWindowList[i];
      ShowWindow(f.Handle, SW_HIDE);
    end;

                // Hide the main form & app windw
    ShowWindow(Handle, SW_HIDE);
//    ShowWindow(Application.Handle, SW_HIDE);
    fTrayed := True;

    fSnapToFirstUnreadMessageAfterTray := True;

                // If there aren't any unread messages set a flag
                // so that it goes to the first unread message when
                // it's restored.
    for i := 0 to NNTPAccounts.Count - 1 do
      for j := 0 to NNTPAccounts.Items[i].SubscribedGroupCount - 1 do
        if NNTPAccounts.Items[i].SubscribedGroups[j].UnreadArticleCount > 0 then
        begin
          fSnapToFirstUnreadMessageAfterTray := False;
          Break;
        end;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  pmForm: TfmPostMessage;
  s: string;
begin
  if CoInitialize(nil) = S_OK then
    fCoUninitialize := True;

  vstSubscribed.NodeDataSize := SizeOf(Pointer);
  vstQueuedRequests.NodeDataSize := SizeOf(Pointer);
  vstArticles.NodeDataSize := SizeOf(Pointer);
  vstBookmark.NodeDataSize := SizeOf(Pointer);

  InitializeCIDMIMEHandler;
  Randomize;                    // Random signatures in unitIdentitied

  fPrevArticleStack := TArticleStack.Create(10);
  fNextArticleStack := TArticleStack.Create(10);
  XNOptions.AppKey := PersistentPosition.ApplicationKey;
  fReloadedList := TList.Create;
  fDeferredCombineList := TObjectList.Create;

  MessageScrollBox1 := TMessageScrollBox.Create(Self);
  MessageScrollBox1.Parent := pnlMessage;
  MessageScrollBox1.Align := alClient;
  MessageScrollBox1.PopupMenu := pomMessage;
  MessageScrollBox1.HelpKeyword := 'msbMessage';
  MessageScrollBox1.BevelInner := bvNone;
  MessageScrollBox1.BevelOuter := bvNone;
  MessageScrollBox1.HorzScrollBar.Tracking := True;
  MessageScrollBox1.VertScrollBar.Tracking := True;
  MessageScrollBox1.Color := clWindow;
  MessageScrollBox1.Font.Name := 'Lucida Console';
  MessageScrollBox1.Font.Charset := ANSI_CHARSET;
  MessageScrollBox1.Font.Color := clWindowText;
  MessageScrollBox1.OnMouseWheel := MessageScrollBox1MouseWheel;
  MessageScrollBox1.OnDblClick := MessageScrollBox1DoubleClick;
  MessageScrollBox1.TabOrder := 0;
  MessageScrollBox1.AutoFit := True;

  tbMenu.Font := Screen.MenuFont;
  SetControlOptions(True);        // Save default options
  XNOptions.BookmarkHeight := pnlArticles.Height div 2;

  Application.CreateForm(TfmPostMessage, pmForm);
  try
    pmForm.PersistentPosition1.Enabled := False;
    XNOptions.Appearance[apMessageEditor].Init(pmForm.fmePost1.mmoMessage.Font, pmForm.fmePost1.mmoMessage.Color);
  finally
    pmForm.Free;
  end;

  XNOptions.Load;                 // Load options registry settings
  ApplyControlOptions;
  PostMessage(Handle, WM_FIRSTTIME, 0, 0);

  NNTPAccounts := TNNTPAccounts.Create;
  MailAccounts := TMailAccounts.Create;
  gArticleFolders := TArticleFolders.Create;

  {$ifdef CPUX64}
    s := ' (x64; Portable ISpell)';
  {$else}
    s := ' (x86; Portable ISpell)';
  {$endif}
  ThreadManager := TNNTPThreadManager.Create(Application.Title + '/' + ProductVersion + s);
  AllFilters.Load;
  PersistentPosition.Enabled := False;
  NNTPAccounts.LoadFromRegistry;
  PersistentPosition.Enabled := True;
  MailAccounts.LoadFromRegistry;

                                // Connect up the ThreadManager event handlers

  ThreadManager.OnArticlesChanged := DoOnArticlesChanged;
  ThreadManager.OnArticleChanged := DoOnArticleChanged;
  ThreadManager.OnArticleFailed := DoOnArticleFailed;
  ThreadManager.OnStartArticle := DoOnStartArticle;
  ThreadManager.OnClearArticle := DoOnClearArticle;
  ThreadManager.OnNewsgroupsChanged := DoOnNewsgroupsChanged;
  ThreadManager.OnNotifyError := DoOnNotifyError;
  ThreadManager.OnNotifyNewGroups := DoOnNewGroups;

  TrayIcon1.Hint := 'XanaNews v ' + ProductVersion;

  fNonDormantIcon := TIcon.Create;
  ImageList1.GetIcon(17, fNonDormantIcon);

  fInterestingIcon := TIcon.Create;
  fGoofyIcon := TIcon.Create;
  ImageList1.GetIcon(16, fInterestingIcon);
  ImageList1.GetIcon(29, fGoofyIcon);
  fModelessWindowList := TList.Create;
  Timer1.Enabled := True;
end;

procedure FreeDefaultActions;
begin
  FreeAndNil(gDefaultActions);
end;

procedure TfmMain.FormDestroy(Sender: TObject);

  procedure LogOutstandingGetters;
  var
    i: Integer;
    getter: TTCPGetter;
    getters: TObjectList;
    st: string;
  begin
    try
      getters := ThreadManager.LockGetterList;
      try
        for i := 0 to getters.Count - 1 do
        begin
          getter := TTCPGetter(getters[i]);
          st := 'Getter "' + getter.GetterRootText + '" ';
          case getter.State of
            tsDormant: st := st + 'Idle';
            tsPending: st := st + 'Pending';
            tsBusy   : st := st + 'Busy';
            tsDone   : st := st + 'Done'
          end;
          LogMessage(st);
        end;
      finally
        ThreadManager.UnlockGetterList;
      end;
    except
      on e: exception do
        LogMessage(e.Message + ' in outstanding getters');
    end;
  end;

begin
  LogMessage('FormDestroy Entered');
  LogOutstandingGetters;
  Timer1.Enabled := False;
  gAppTerminating := True;
  ThreadManager.DisconnectAll(True);
  LogMessage('ThreadManager.DisconnectAll Done');
  DontUnloadCache.Clear;

  if NNTPAccounts.fNewUserFlag then Exit;

  if fLastFocusedArticleContainer <> nil then
  begin
    if (not fDontMarkOnLeave) and fLastFocusedArticleContainer.MarkOnLeave then
      actNewsgroupMarkAllMessagesAsRead.Execute;
    fLastFocusedArticleContainer.LeaveGroup(False);
  end;

  NNTPAccounts.UnloadOldContainers(nil);

  ClearSynchronizedMethods;

  LogMessage('Focused Group Saved');

  NNTPAccounts.PurgeOldArticles;
  Timer1.Enabled := False;
  LogMessage('Purge Complete');

  NNTPAccounts.SaveDirtyArticles; // (heh heh!)
  LogMessage('Dirty Articles Saved');

  FreeAndNil(NNTPAccounts);
  FreeAndNil(MailAccounts);
  FreeAndNil(gArticleFolders);
  LogMessage('Accounts & Folders Free''d');

  FreeAndNil(fCurrentBookmark);
  FreeAndNil(fTestMessage);
  FreeAndNil(fPrevArticleStack);
  FreeAndNil(fNextArticleStack);
  FreeAndNil(fModelessWindowList);
  fFolderArticleHeader.Free;
  FreeDefaultActions;
  fReloadedList.Free;
  fDeferredCombineList.Free;
  fInterestingIcon.Free;
  fGoofyIcon.Free;
  fNonDormantIcon.Free;
  LogMessage('UI elements free''d');


  FreeAndNil(ThreadManager);
  LogMessage('Thread manager free''d');

  if gMutex <> 0 then
    CloseHandle(gMutex);

  FreeFontDetails;
  FreeAndNil(fBookmarkSet);

  FreeCIDMimeHandler;

  if fCoUninitialize then
    CoUninitialize;
end;

procedure TfmMain.FormResize(Sender: TObject);
var
  sbPanelSize: Integer;
  i, n, w: Integer;
begin
  n := StatusBar.Panels.Count;

  if StatusBar.Panels[n - 1].Text = '' then
  begin
    w := 0;
    StatusBar.Panels[n - 1].Bevel := pbNone
  end
  else
  begin
    StatusBar.Panels[n - 1].Bevel := pbLowered;
    w := StatusBar.Canvas.TextWidth(StatusBar.Panels[n - 1].Text) + 16;
    if Assigned(fMedal) and fMedal.Visible then
      w := w + fMedal.Width;
  end;

  sbPanelSize := ((ClientWidth - 16) - w) div (StatusBar.Panels.Count - 1);
  StatusBar.Panels[n - 1].Width := w;

  for i := 0 to StatusBar.Panels.Count - 2 do
    StatusBar.Panels[i].Width := sbPanelSize;

  ProgressBar1.Top := StatusBar.Top + 3;
  ProgressBar1.Left := StatusBar.Panels[0].Width + 3;
  ProgressBar1.Height := StatusBar.Height - 4;

  if Assigned(fMedal) and fMedal.Visible then
  begin
    ProgressBar1.Width := StatusBar.Panels[1].Width - 12 - fMedal.Width;
    fMedal.Left := StatusBar.Left + StatusBar.Width - fMedal.Width - 20;
  end
  else
    ProgressBar1.Width := StatusBar.Panels[1].Width - 4;

  Toolbar1.Left := cbBatches.Left + cbBatches.Width + 8;

  cbMain.Realign;
end;

procedure SaveDefaultActions;
var
  i: Integer;
begin
  FreeAndNil(gDefaultActions);
  gDefaultActions := TObjectList.Create;
  for i := 0 to fmMain.alMain.ActionCount - 1 do
    gDefaultActions.Add(TActionDefault.Create(TCustomAction(fmMain.alMain.Actions[i])));
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  Caption := Application.Title + ' ' + ProductVersion;
  SaveDefaultActions;
  vstSubscribed.SetFocus;
  LoadToolbarLayout;

{QEC}
  if tbMain.Customizable then
     tbMain.Hint := 'Dbl-Click here to Configure Buttons.' + NewLine +
                    'Neither the Configure Dialog nor the' + NewLine +
                    'Shift-Dragging of Buttons work correctly.'
  else
     tbMain.Hint := 'ToolBar is NOT configurable.';

  fPanelLeftWidth := pnlLeft.Width;
  fPanelLeftHeight := pnlLeft.Height;
  if XNOptions.PanelLeftHeight > 0 then
    pnlLeft.UndockHeight := XNOptions.PanelLeftHeight;

  case XNOptions.PanelLeftSplitter of
    0: pnlLeft.ManualDock(SplitterPanel1);
    1: pnlLeft.ManualDock(SplitterPanel2);
    2: pnlLeft.ManualFloat(Rect(XNOptions.PanelLeftLeft, XNOptions.PanelLeftTop, XNOptions.PanelLeftLeft + XNOptions.PanelLeft, XNOptions.PanelLeftTop + XNOptions.PanelLeftHeight))
  end;

  fMedal := TImage.Create(Self);
  fMedal.Parent := StatusBar;
  if not LoadGifResource('MEDAL', fMedal) then
    FreeAndNil(fMedal);

  if Assigned(fMedal) then
  begin
    fMedal.AutoSize := True;
    fMedal.Top := (StatusBar.Height - fMedal.Height) div 2;
    fMedal.Left := StatusBar.Left + StatusBar.Width - fMedal.Width - 20;
    fMedal.Transparent := True;
    fMedal.Visible := XNOptions.DeservesMedal;
  end;

  pnlRight.Align := alClient;
  if pnlLeft.HostDockSite is TCustomForm then
    TForm(pnlLeft.HostDockSite).ActiveControl := vstSubscribed
  else
    ActiveControl := vstSubscribed;
  PostMessage(Handle, WM_SETUP, 0, 0);
end;

function TfmMain.GetArticleContainerNode(ctnr: TArticleContainer): PVirtualNode;
var
  ct: TArticleContainer;
begin
  if not Assigned(ctnr) then
    Result := nil
  else
  begin
    Result := vstSubscribed.GetFirst;
    repeat
      ct := GetNodeArticleContainer(Result);
      if ct = ctnr then
        Exit;
      Result := vstSubscribed.GetNext(Result);
    until Result = nil;
  end;
end;

function TfmMain.GetArticleNode(article: TArticleBase): PVirtualNode;
var
  art: TArticleBase;
  idx: Integer;
begin
  if not Assigned(article) then
    Result := nil
  else
  begin
    if not IsThreaded(article.Owner) then
    begin
      idx := article.Index;

      Result := vstArticles.GetFirst;
      while (idx > 0) and Assigned(Result) do
      begin
        Result := vstArticles.GetNextNoInit(Result);
        Dec(idx);
      end;
    end
    else
    begin
      vstArticles.BeginUpdate;
      try
        Result := vstArticles.GetFirst;
        while Result <> nil do
        begin
          art := GetNodeArticle(Result);
          if article.Equals(art) then
            Exit;
          Result := vstArticles.GetNext(Result);
        end;
      finally
        vstArticles.EndUpdate;
      end;
    end;
  end;
end;

function TfmMain.GetFirstvstSubscribedSelectedNode: PVirtualNode;
begin
  Result := vstSubscribed.GetFirst;

  while Assigned(Result) and not (vsSelected in Result.States) do
    Result := vstSubscribed.GetNext(Result);
end;

function TfmMain.GetFocusedAccount: TNNTPAccount;
begin
  if Assigned(fLastFocusedArticleContainer) and (fLastFocusedArticleContainer is TSubscribedGroup) then
    Result := TSubscribedGroup(fLastFocusedArticleContainer).Owner
  else
    Result := GetNodeAccount(vstSubscribed.FocusedNode);
end;

function TfmMain.GetFocusedArticle: TArticleBase;
begin
  Result := GetNodeArticle(vstArticles.FocusedNode);
end;

function TfmMain.GetFocusedArticleFolder: TArticleFolder;
begin
  if Assigned(fLastFocusedArticleContainer) and (fLastFocusedArticleContainer is TArticleFolder) then
    Result := TArticleFolder(fLastFocusedArticleContainer)
  else
    Result := nil;
end;

function TfmMain.GetFocusedGroup: TSubscribedGroup;
begin
  if Assigned(fLastFocusedArticleContainer) and (fLastFocusedArticleContainer is TSubscribedGroup) then
    Result := TSubscribedGroup(fLastFocusedArticleContainer)
  else
    Result := nil;
end;

function TfmMain.GetFocusedObject: TObject;
begin
  Result := nil;
  if vstQueuedRequests.Focused then
  begin
    Result := GetFocusedQGroup;
    if not Assigned(Result) then
      Result := GetFocusedQAccount;
  end
  else
    if vstArticles.Focused then
      Result := GetFocusedArticle
    else
      if vstSubscribed.Focused then
      begin
        Result := GetFocusedGroup;
        if not Assigned(Result) then
          Result := GetFocusedAccount;
      end;
end;

function TfmMain.GetFocusedQAccount: TNNTPAccount;
var
  getter: TTCPGetter;
  idx: Integer;
begin
  // Get the account that's focused in the Queued Requests tree.
  Result := nil;
  getter := GetQNodeGetter(vstQueuedRequests.FocusedNode, idx);
  if Assigned(getter) and (getter is TNewsGetter) then
    Result := TNewsGetter(getter).Account;
end;

function TfmMain.GetFocusedQGroup: TArticleContainer;
var
  getter: TTCPGetter;
  idx: Integer;
begin
  // Get the group that's focused in the Queued Requests tree.
  Result := nil;
  getter := GetQNodeGetter(vstQueuedRequests.FocusedNode, idx);
  if Assigned(getter) then
    if getter is TArticlesGetter then
      if idx >= 0 then
        Result := TArticlesGetter(getter).CurrentGroup
      else
        Result := nil
    else
      if getter is TArticleGetter then
        Result := TArticleGetter(getter).CurrentGroup;
end;

function TfmMain.GetNodeAccount(node: PVirtualNode): TNNTPAccount;
var
  data: TObject;
begin
  // Get the account the specified vstSubscribed node.
  data := GetNodeObject(node);
  if data is TSubscribedGroup then
    Result := TSubscribedGroup(data).Owner
  else
    if data is TNNTPAccount then
      Result := TNNTPAccount(data)
    else
      Result := nil;
end;

function TfmMain.GetNodeArticle(node: PVirtualNode): TArticleBase;
var
  data: PObject;
begin
  // Get the article for the specified vstArticles node.
  Result := nil;
  if Assigned(node) then
  begin
    try
      if fLastFocusedArticleContainer is TArticleFolder then
        Result := fLastFocusedArticleContainer.ArticleBase[node^.Index]
      else
      begin
        data := PObject(vstArticles.GetNodeData(node));

        if Assigned(data) and Assigned(data^) and (data^ is TArticleBase) then
          Result := TArticleBase(data^);
      end
    except
      Result := nil;
    end
  end
end;

function TfmMain.GetNodeArticleFolder(node: PVirtualNode): TArticleFolder;
var
  ctnr: TArticleContainer;
begin
  // Get the article folder for the specified vstSubscribed node.
  ctnr := GetNodeArticleContainer(node);
  if Assigned(ctnr) and (ctnr is TArticleFolder) then
    Result := TArticleFolder(ctnr)
  else
    Result := nil;
end;

function TfmMain.GetNodeSubscribedGroup(node: PVirtualNode): TSubscribedGroup;
var
  ctnr: TArticleContainer;
begin
  // Get the subscribed group for the specified vstSubscribed node.
  ctnr := GetNodeArticleContainer(node);
  if Assigned(ctnr) and (ctnr is TSubscribedGroup) then
    Result := TSubscribedGroup(ctnr)
  else
    Result := nil;
end;

function TfmMain.GetQNodeGetter(node: PVirtualNode; var idx: Integer): TTCPGetter;
var
  data: PObject;
  i1: Integer;
begin
  // Return the 'getter' and index for the specified Queued Request node.
  Result := nil;
  idx := -1;
  if Assigned(node) then
  begin
    data := PObject(vstQueuedRequests.GetNodeData(node));
    if Assigned(data) then
      if Assigned(data^) then
      begin
        if Integer(node^.Index) < ThreadManager.LockGetterList.Count then
        try
          Result := data^ as TTCPGetter;
        finally
          ThreadManager.UnlockGetterList;
        end;
      end
      else
      begin
        idx := node.Index;
        Result := GetQNodeGetter(node^.Parent, i1);
      end;
  end;
end;

function TfmMain.GetQuoteText(article: TArticleBase): string;
var
  s: TStringList;
  st: string;
  sx: string;
  quoteLineMarker: string;
  wrap, trimSig: Boolean;
  postingSettings: TPostingSettings;

  function ExpandQuoteHeader(const hdr: string): string;
  begin
    Result := StringReplace(hdr, '%author-short%', ShortName(article.FromName), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%author%', article.FromName, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%date%', DateToStr(article.Date), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%messageid%', article.MessageId, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%time%', TimeToStr(article.Date), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%mail%', article.FromEmail, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%group%', article.Owner.Name, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%forename%', ForeName(article.FromName), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%newline%', #13#10, [rfReplaceAll, rfIgnoreCase]);
  end;

begin
  // Get default text when replying to a message.  Include the default
  // header and footers, and the selected lines in the message.
  Result := '';
  if Assigned(article) then
  begin
    postingSettings := article.Owner.PostingSettings;
    quoteLineMarker := postingSettings.QuoteLineMarker;

    trimSig := False;
    s := TStringList.Create;
    try
      st := '';
      MessageScrollBox1.GetSelectedText(st);

      if (st = '') and XNOptions.QuoteSelectedText then
      begin
        MessageScrollBox1.GetText(st);
        trimSig := True;
      end;

      if st <> '' then
      begin
        s.Text := st;

        if trimSig and (XNOptions.ShowHeader <> shNone) then
        begin
          while s.Count > 0 do
          begin
            sx := s[0];
            s.Delete(0);
            if sx = '' then
              Break;
          end;
        end;

        wrap := (postingSettings.TextPartStyle <> tpQuotedPrintable) and (postingSettings.TextPartStyle <> tpFlowed) and (postingSettings.MaxPostLineLength <> 0);

        FixQuotes(s, wrap, postingSettings.MaxPostLineLength, quoteLineMarker, trimSig, XNOptions.StrictSigSep);

        if postingSettings.QuoteHeader <> '' then
          s.Insert(0, ExpandQuoteHeader(postingSettings.QuoteHeader));

        if (postingSettings.QuoteFooter <> '') then
          s.Add(ExpandQuoteHeader(postingSettings.QuoteFooter));
      end;

      if postingSettings.QuoteSalutation <> '' then
        s.Insert(0, ExpandQuoteHeader(postingSettings.QuoteSalutation));

      Result := s.Text;
    finally
      s.Free;
    end;
  end;
end;

procedure TfmMain.GoToArticle(article: TArticleBase);
var
  node: PVirtualNode;
begin
  if not Assigned(article) then
  begin
    DisplayArticleBody(nil);
    Exit;
  end;

  if article.Owner <> fLastFocusedArticleContainer then
  begin
    FocusArticleContainer(article.Owner);
    SyncContainerTree(article.Owner);
  end;
  node := GetArticleNode(article);
  if Assigned(node) then
    SelectArticleNode(node);
end;

function CompareIdx(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := LPARAM(List.Objects[Index1]) - LPARAM(List.Objects[Index2]);
end;

procedure TfmMain.LoadToolbarLayout;
var
  reg: TExSettings;
  sl: TStringList;
  st, s: string;
  i, idx: Integer;
  ctrl: TControl;
  band: TCoolBand;

procedure ArrangetbMainButtons; // QEC20100124
  type
    PosRec = record
      CN: string;  // Control Name
      CL: longInt; // Control Left Value
      CX: longInt; // Component Index
      CE: boolean; // Control Enabled Value
      CV: boolean; // Control Visible Value
      CS: TToolButtonStyle; // Control Style ('B' is Button, 'S' is Separator)
    end;
  var
    st, s: string;
    i, J, BC, CC, Val: Integer;
    ctrlArray: array of PosRec;
    Swapped: boolean;
    Temp: PosRec;

  begin
    if not tbMain.Customizable then
      Exit;

    reg.Section := 'Position\Toolbar';
    if not reg.HasSection('tbMainButtons') then
      Exit;

    reg.Section := reg.Section + '\tbMainButtons';
    if not(reg.HasValue('ButtonCount') and reg.HasValue('ControlCount') and reg.HasValue
        ('LeftValues-Ctrls') and reg.HasValue('Enabled-Btns') and reg.HasValue('Visible-Btns')
        and reg.HasValue('Style-Btns') and reg.HasValue('BtnsConfiguration')) then
      Exit;

    st := reg.StringValue['ButtonCount'];
    BC := StrToInt(st);

    st := reg.StringValue['ControlCount'];
    CC := StrToInt(st);

    SetLength(ctrlArray, BC);
    st := reg.StringValue['Names-Btns'];
    for i := 0 to BC - 1 do
    begin
      s := SplitString(',', st);
      ctrlArray[i].CN := s;
    end;

    st := reg.StringValue['LeftValues-Btns'];
    for i := 0 to BC - 1 do
    begin
      Val := StrToInt(SplitString(',', st));
      ctrlArray[i].CL := Val;
    end;

    st := reg.StringValue['Index-BtnComp'];
    for i := 0 to BC - 1 do
    begin
      Val := StrToInt(SplitString(',', st));
      ctrlArray[i].CX := Val;
    end;

    st := reg.StringValue['Enabled-Btns'];
    for i := 0 to BC - 1 do
    begin
      s := SplitString(',', st);
      ctrlArray[i].CE := (s = 'E');
    end;

    st := reg.StringValue['Visible-Btns'];
    for i := 0 to BC - 1 do
    begin
      s := SplitString(',', st);
      ctrlArray[i].CV := (s = 'V');
    end;

    st := reg.StringValue['Style-Btns'];
    for i := 0 to BC - 1 do
    begin
      s := SplitString(',', st);
      if s = 'B' then
        ctrlArray[i].CS := tbsButton
      else if s = 'S' then
        ctrlArray[i].CS := tbsSeparator;
    end;

    // =========================================================================
    // Bubble sort ctrlArray by LeftValues
    Swapped := True; // Required for entry to while loop
    while Swapped do
    begin
      Swapped := False;
      for i := 0 to High(ctrlArray) - 1 do
      begin
        if ctrlArray[i].CL < ctrlArray[i + 1].CL then // < is Descending
        begin                                         // > is Ascending
          Temp := ctrlArray[i];
          ctrlArray[i] := ctrlArray[i + 1];
          ctrlArray[i + 1] := Temp;
          Swapped := True;
        end; // if
      end; // for
    end; // while
    // =========================================================================

    tbMain.Enabled := False;
    try
      for i := 0 to BC - 1 do
      begin
        for J := 0 to CC - 1 do
        begin
          if tbMain.Controls[J].name = ctrlArray[i].CN then
          begin
            tbMain.Controls[J].Left := ctrlArray[i].CL;
            tbMain.Controls[J].Visible := tbMain.Buttons[J].Visible;
            tbMain.Controls[J].Enabled := tbMain.Buttons[i].Enabled;
          end;
        end;
      end;
    finally
      tbMain.Enabled := True;
    end;
    tbMain.Update;
    tbMain.Invalidate;
  end;

begin
  if tbMain.Customizable then
  begin
    tbMain.CustomizeKeyName := 'Software\Woozle\XanaNews\Position\ToolBar\tbMainButtons';
    tbMain.CustomizeValueName := 'BtnsConfiguration';
  end;

  sl := nil;
  reg := CreateExSettings;
  try
    reg.Section := 'Position';
    if not reg.HasSection('Toolbar') then
    begin
      pnlSearchBar.Visible := False;
      pnlBatchBar.Visible := False;
      actViewShowSearchBar.Checked := pnlSearchBar.Visible;
      actViewShowBatchBar.Checked := pnlBatchBar.Visible;
      Exit;
    end;

    reg.Section := reg.Section + '\Toolbar';
    tbMain.ShowCaptions := reg.BooleanValue['ShowCaptions'];
    actViewToolbarCaptions.Checked := tbMain.ShowCaptions;

    if reg.BooleanValue['SmallImages'] then
    begin
      tbMain.Images := ilMain;
      tbMain.DisabledImages := ilDisabled;
      actViewToolbarSmallImages.Checked := True;
    end
    else
    begin
      tbMain.Images := ilMainLarge;
      tbMain.DisabledImages := ilDisabledLarge;
      actViewToolbarSmallImages.Checked := False;
    end;

    tbMain.ButtonWidth := 0;
    tbMain.ButtonHeight := 0;

    if reg.HasSection('Bands') then
    begin
      reg.Section := reg.Section + '\Bands';
      sl := TStringList.Create;
      reg.GetValueNames(sl);
      for i := 0 to sl.Count - 1 do
      begin
        s := sl[i];
        st := reg.StringValue[s];
        idx := StrToIntDef(SplitString(',', st), 0);
        sl.Objects[i] := TObject(idx);
        sl[i] := s + ',' + st;
      end;

      sl.CustomSort(CompareIdx);

      for i := 0 to sl.Count - 1 do
      begin
        st := sl[i];
        s := SplitString(',', st);
        ctrl := cbMain.FindChildControl(s);
        sl.Objects[i] := ctrl;
        sl[i] := st;
      end;

      // Workaround for D2009 to get the same behaviour as with the pre-D2009 versions.
      // First create the bands and then in a separate run adjust the bands.
      // Has to do with the changed behaviour of when Update is called in-side
      // the constructor of TCoolBand.
      cbMain.Bands.Clear;
      for i := 0 to sl.Count - 1 do
        cbMain.Bands.Add;

      cbMain.Bands.BeginUpdate;
      try
        for i := 0 to sl.Count - 1 do
        begin
          st := sl[i];
          band := cbMain.Bands[i];
          band.Control := TWinControl(sl.Objects[i]);
          band.Width := StrToIntDef(SplitString(',', st), 100);
          band.Break := StrToIntDef(SplitString(',', st), 1) <> 0;
          sl[i] := st;
        end;
      finally
        cbMain.Bands.EndUpdate;
      end;

      for i := sl.Count - 1 downto 0 do
      begin
        ctrl := TControl(sl.Objects[i]);
{QEC}
        if (ctrl.name = 'tbMain') then
          ArrangetbMainButtons;
        ctrl.Visible := StrToIntDef(sl[i], 1) <> 0;
      end;
    end
    else
    begin
      pnlSearchBar.Visible := False;
      pnlBatchBar.Visible := False;
    end;

    actViewShowSearchBar.Checked := pnlSearchBar.Visible;
    actViewShowBatchBar.Checked := pnlBatchBar.Visible;
    actViewShowToolbar.Checked := tbMain.Visible;

  finally
    reg.Free;
    sl.Free;
  end;
end;

procedure TfmMain.LoadUnpostedMessages;
var
  f: TFileStream;
  reader: TStreamTextReader;
  fileName: string;
  st: string;
  accountName, MTo, MCC, MBCC, MSubject, MReplyTo: string;
  i, noHdrs, noMsgLines, noAttachments, codepage: Integer;
  h: TAnsiStringList;
  m: TStringList;
  account: TNNTPAccount;
  mailAccount: TMailAccount;
  ctnr: TServerAccount;
  attachments: TObjectList;
  raw: RawByteString;
  rok: Boolean;
begin
   // Load unposted messages that were saved the last time XanaNews was run.
  try
    fileName := gMessageBaseRoot + '\UnsentMessages.dat';

    if FileExists(fileName) then
    begin
      reader := nil;
      h := nil;
      m := nil;
      f := TFileStream.Create(fileName, fmOpenRead);
      try
        reader := TStreamTextReader.Create(f);
        h := TAnsiStringList.Create;
        m := TStringList.Create;
        rok := reader.ReadLn(raw);
        while rok do
        begin
          st := string(raw);
          accountName := SplitString(#9, st);
          noHdrs := StrToIntDef(SplitString(#9, st), -1);
          noMsgLines := StrToIntDef(SplitString(#9, st), -1);
          noAttachments := StrToIntDef(SplitString(#9, st), -1);
          codepage := StrToIntDef(st, -1);

          if (noMsgLines = -1) or (noHdrs = -1) or (noMsgLines = -1) or (noAttachments = -1) then
            rok := reader.ReadLn(raw)
          else
          begin
            st := Copy(accountName, 1, 2);
            if st = '&&' then
            begin
              st := Copy(accountName, 3, MaxInt);
              accountName := SplitString(':', st);
              ctnr := NNTPAccounts.FindArticleContainer(accountName, st);

              if Assigned(ctnr) then
              begin
                m.Clear;
                h.Clear;

                if ctnr is TMailAccount then
                  mailAccount := TMailAccount(ctnr)
                else
                  if ctnr is TSubscribedGroup then
                    if TSubscribedGroup(ctnr).Owner.MailAccountName = 'MAPI' then
                      mailAccount := nil
                    else
                      mailAccount := MailAccounts.FindMailAccount(TSubscribedGroup(ctnr).Owner.MailAccountName)
                  else
                    mailAccount := nil;


                rok := reader.ReadLn(raw);
                MTo := AnsiStringToWideString(raw, codepage);

                if rok then
                begin
                  rok := reader.ReadLn(raw);
                  MCC := AnsiStringToWideString(raw, codepage);
                end;

                if rok then
                begin
                  rok := reader.ReadLn(raw);
                  MBCC := AnsiStringToWideString(raw, codepage);
                end;

                if rok then
                begin
                  rok := reader.ReadLn(raw);
                  MSubject := AnsiStringToWideString(raw, codepage);
                end;

                if rok then
                begin
                  rok := reader.ReadLn(raw);
                  MReplyTo := AnsiStringToWideString(raw, codepage);
                end;

                i := noMsgLines;

                while rok and (i > 0) do
                begin
                  rok := reader.ReadLn(raw);
                  if rok then m.Add(AnsiStringToWideString(raw, codepage));
                  Dec(i);
                end;

                i := noAttachments;
                if i > 0 then
                begin
                  attachments := TObjectList.Create;
                  while rok and (i > 0) do
                  begin
                    rok := reader.ReadLn(raw);
                    if rok then
                    try
                      attachments.Add(TAttachment.Create(string(raw)));
                    except // They might have deleted the file - ignore attachment
                    end;
                    Dec(i);
                  end
                end
                else
                  attachments := nil;

                if rok and Assigned(mailAccount) then
                begin
                  ThreadManager.SendSMTPMail(ctnr,
                    mailAccount.ServerSettings as TSMTPServerSettings,
                    MTo, MCC, MBCC, MSubject, MReplyTo, m.Text,
                    attachments, codePage, mailAccount.PostingSettings.DelayPosting);
                  rok := Reader.ReadLn(raw);
                end;
              end;
            end
            else
            begin
              account := nil;
              for i := 0 to NNTPAccounts.Count - 1 do
                if NNTPAccounts.Items[i].AccountName = accountName then
                begin
                  account := NNTPAccounts.Items[i];
                  Break;
                end;

              if Assigned(account) then
              begin
                h.Clear;
                m.Clear;

                i := noHdrs;
                rok := reader.ReadLn(raw);
                while rok and (i > 0) do
                begin
                  if h.Count > 0 then
                    while rok and (Copy(raw, 1, 1) = ' ') do
                    begin
                      h[h.Count - 1] := h[h.Count - 1] + #13#10 + raw;
                      rok := reader.ReadLn(raw);
                    end;
                  h.Add(raw);
                  rok := reader.ReadLn(raw);
                  Dec(i);
                end;

                if h.Count > 0 then
                  while rok and (Copy(raw, 1, 1) = ' ') do
                  begin
                    h[h.Count - 1] := h[h.Count - 1] + #13#10 + raw;
                    rok := reader.ReadLn(raw);
                  end;

                i := noMsgLines;
                while rok and (i > 0) do
                begin
                  st := AnsiStringToWideString(raw, codepage);
                  m.Add(st);
                  Dec(i);
                  rok := reader.ReadLn(raw);
                end;

                i := noAttachments;
                if i > 0 then
                begin
                  attachments := TObjectList.Create;
                  while rok and (i > 0) do
                  begin
                    try
                      attachments.Add(TAttachment.Create(string(raw)));
                    except // They might have deleted the file - ignore attachment
                    end;
                    Dec(i);
                    rok := reader.ReadLn(raw);
                  end;
                end
                else
                  attachments := nil;

                raw := WideStringToAnsiString(m.Text, codepage);
                ThreadManager.PostMessage(account, h, raw, attachments, codepage, tpNNTP);
              end;
            end;
          end;
        end;
      finally
        reader.Free;
        f.Free;
        h.Free;
        m.Free;
      end;
      DeleteFile(fileName);
    end;
  except
    // Ah well...
    DeleteFile(fileName);
  end;
end;

procedure TfmMain.MessageScrollBox1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  with MessageScrollBox1, VertScrollBar do
    Position := Position - (WheelDelta div 120) * Mouse.WheelScrollLines * LineHeight;
  Handled := True;
end;

procedure TfmMain.mnuViewHeadersClick(Sender: TObject);
begin
  case XNOptions.ShowHeader of
    shNone:   actViewHeadersNone.Checked := True;
    shShort:  actViewHeadersShort.Checked := True;
    shFull:   actViewHeadersFull.Checked := True;
    shCustom: actViewHeadersCustom.Checked := True;
  end;
end;

procedure TfmMain.mnuViewMessagesClick(Sender: TObject);
begin
  case XNOptions.ViewMode of
    vmNormal:  actViewMessagesNormal.Checked := True;
    vmRawText: actViewMessagesRawText.Checked := True;
    vmRaw:     actViewMessagesRawMessages.Checked := True;
    vmImages:  actViewMessagesImagesOnly.Checked := True;
  end;
end;

procedure TfmMain.ModelessWindowFormActivate(Sender: TObject);
begin
  EnableShortcuts(False);
end;

procedure TfmMain.ModelessWindowFormDeactivate(Sender: TObject);
begin
  EnableShortcuts(True);
end;

procedure TfmMain.ModelessWindowFormDestroy(Sender: TObject);
var
  idx: Integer;
begin
  idx := fModelessWindowList.IndexOf(Sender);
  if idx >= 0 then
    fModelessWindowList.Delete(idx);
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.MoveSelectedArticlesToFolder                       |
 |                                                                      |
 | Move or copy selected articles or branches to a folder.              |
 |                                                                      |
 | Parameters:                                                          |
 |   fldr: TArticleFolder       // The folder to move them to           |
 |   deleteSrc: Boolean         // Move or Copy flag                    |
 |   branches: Boolean          // articles or branches flag            |
 *----------------------------------------------------------------------*)
procedure TfmMain.MoveSelectedArticlesToFolder(fldr: TArticleFolder;
  deleteSrc, branches: Boolean);
var
  node: PVirtualNode;
  article: TArticleBase;
  ctnr: TArticleContainer;
  msgID: RawByteString;
begin
  node := vstArticles.GetFirstSelected;
  article := GetNodeArticle(node);

  if Assigned(article) and Assigned(fldr) then
  begin
    ctnr := article.Owner;

    if deleteSrc then
    begin
      msgID := article.UniqueID;

      NNTPAccounts.PurgeOldArticles;
      InitArticlesRootNodeCount(ctnr);
                                        // The 'purge' may have deleted the article
                                        // if it was marked as delete - so make sure
                                        // it still exists!
      article := TArticle(ctnr.FindUniqueID(msgID));
      node := GetArticleNode(article);
    end;

    fldr.BeginAdd;
    try
      while Assigned(node) do
      begin
        article := GetNodeArticle(node);
        if Assigned(article) then
        begin
          if deleteSrc then
            if branches then
            begin
              ForEachArticleInBranch(article, DoDeleteArticle, LPARAM(True));
              node := GetArticleNode(article);
            end
            else
            begin
              article.IsDeleted := True;
              if ctnr is TArticleFolder then
                fldr.AddArticle(article);
            end
          else
            if branches then
            begin
              ForEachArticleInBranch(article, DoMoveToSelectedFolder, LPARAM(fldr));
              node := GetArticleNode(article);
            end
            else
              DoMoveToSelectedFolder(article, LPARAM(fldr), True);
        end;

        if Assigned(node) then
          node := vstArticles.GetNextSelected(node);
      end;

      if deleteSrc then
      begin
        BeginPurge(ctnr);
        if ctnr is TArticleObjectContainer then
          TArticleObjectContainer(ctnr).PurgeArticles(False, False, fldr.Name)
        else
          if ctnr is TArticleFolder then
            TArticleFolder(ctnr).RemoveDeletedMessages;
      end

    finally
      fldr.EndAdd;
      if deleteSrc then
        EndPurge;
    end;
  end;
end;

function TfmMain.NextArticle(Options: TNextArticleOptions; firstArticle: TArticleBase; skipThisThread: Boolean): Boolean;
var
  article: TArticleBase;
  grp: TArticleContainer;
  act: TNNTPAccount;
begin
  // Scan for the next article that matches the options.  If
  // 'firstArticle' is set, the scan starts from this article.  Otherwise
  // the scan starts from the next article after the current focused one.
  if firstArticle = nil then
    Options := Options + [naTempFirstArticle];

  if (firstArticle <> nil) and skipThisThread then
  begin
    Options := Options + [naTempFirstArticle];
    grp := firstArticle.Owner;
    if grp is TSubscribedGroup then
      act := TSubscribedGroup(grp).Owner
    else
      act := nil;
    while firstArticle.Parent <> nil do        // Start at thread root
      firstArticle := firstArticle.Parent;
    firstArticle := firstArticle.Sibling;
    if firstArticle = nil then
    begin
      grp := grp.Next;
      if grp = nil then
      begin
        act := act.Next;
        if Assigned(act) and (act.SubscribedGroupCount > 0) then
          grp := act.SubscribedGroups[0];
      end;
    end;
    Options := Options + [naIncludeFirstArticle];
  end
  else
    grp := GetFocusedGroup;

  if (firstArticle = nil) and (naCanLeaveGroup in Options) then
  begin
    if grp = nil then
      firstArticle := NNTPAccounts.FirstArticle;
    while Assigned(grp) and not Assigned(firstArticle) do
    begin
      firstArticle := grp.FirstArticle;
      if firstArticle = nil then
        grp := grp.Next;
    end;
    Options := Options + [naIncludeFirstArticle];
  end;

  if firstArticle = nil then
  begin
    Result := False;
    Exit;
  end;

  article := NNTPAccounts.NextArticle(options, firstArticle);
  Result := Assigned(article);

  if Result then
    GoToArticle(article);

  if naMarkAsRead in Options then   // Do this whether or not it jumped!
  begin
    vstArticles.Invalidate;
    Refresh_vstSubscribed;
  end;
end;

procedure TfmMain.pomGroupsPopup(Sender: TObject);
var
  account: TNNTPAccount;
begin
  account := GetFocusedAccount;
  actAccountSortGroupsByName.Enabled := Assigned(account);
  actAccountSortGroupsByName.Checked := Assigned(account) and account.SortGroupsByName;
end;

procedure TfmMain.pomMessagePopup(Sender: TObject);
begin
  if MessageScrollBox1.URLText <> '' then
  begin
    fURL := MessageScrollBox1.URLText;
    mnuCopyURLToClipboard.Visible := True;
  end
  else
  begin
    mnuCopyURLToClipboard.Visible := False;
  end;
  mnuViewHeadersClick(Sender);
  mnuViewMessagesClick(Sender);
end;

procedure TfmMain.pomQueuedRequestsPopup(Sender: TObject);
var
  getter: TTCPGetter;
  idx: Integer;
begin
  getter := GetQNodeGetter(vstQueuedRequests.FocusedNode, idx);
  if Assigned(getter) then
    actQRPause.Checked := getter.Paused
  else
    actQRPause.Enabled := False;
end;

procedure TfmMain.PurgeCtnr(ctnr: TArticleObjectContainer; all, reset, archive: Boolean);
var
  binFolder: string;
begin
  // Purge deleted messages from a group.
  if archive then
    binFolder := ctnr.DisplaySettings.PurgeFolder
  else
    binFolder := '';
  BeginPurge(ctnr);
  try
    ctnr.PurgeArticles(all, reset, binFolder);
  finally
    EndPurge;
  end;
end;

procedure TfmMain.Refresh_vstArticles(article: TArticleBase);
var
  node: PVirtualNode;
begin
  vstArticles.BeginUpdate;
  try
    node := nil;
    if Assigned(article) then
      node := GetArticleNode(article);
    if (node = nil) and (fLastFocusedArticleContainer is TArticleFolder) then
      vstArticles.Invalidate
    else
      vstArticles.ReinitNode(node, node = nil);
  finally
    vstArticles.EndUpdate;
  end;
end;

procedure TfmMain.Refresh_vstQueuedRequests;
begin
  vstQueuedRequests.BeginUpdate;
  try
    vstQueuedRequests.ReinitNode(nil, True)
  finally
    vstQueuedRequests.EndUpdate
  end
end;

procedure TfmMain.Refresh_vstSubscribed;
begin
  vstSubscribed.BeginUpdate;
  try
    vstSubscribed.ReinitNode(nil, True)
  finally
    vstSubscribed.EndUpdate;
  end;
end;

procedure TfmMain.ResizeArticleHeader;
var
  i, w, cw, tot: Integer;
begin
  if csDestroying in ComponentState then Exit;
  Include(fColumnHeaderStatus, chAutoResize);
  try
    tot := 0;
    with vstArticles.Header do
    begin
      cw := vstArticles.ClientWidth;
      for i := 0 to Columns.Count - 2 do
      begin
        w := XNOptions.ArticlesColumnPCs[i] * cw div 100;
        Columns[i].Width := w;
        if coVisible in Columns[i].Options then
          Inc(tot, w);
      end;

      Columns[Columns.Count - 1].Width := cw - tot;
    end;
  finally
    Exclude(fColumnHeaderStatus, chAutoResize);
  end;
end;

procedure TfmMain.ResizeBookmarkHeader;
var
  i, w, cw, tot: Integer;
begin
  if csDestroying in ComponentState then Exit;
  if not pnlBookmark.Visible then Exit;
  Include(fBookmarkHeaderStatus, chAutoResize);
  try
    tot := 0;
    with vstBookmark.Header do
    begin
      cw := vstBookmark.ClientWidth;
      for i := 0 to Columns.Count - 2 do
      begin
        w := XNOptions.BookmarkColumnPCs[i] * cw div 100;
        Columns[i].Width := w;
        if coVisible in Columns[i].Options then
          Inc(tot, w);
      end;

      Columns[Columns.Count - 1].Width := cw - tot;
    end
  finally
    Exclude(fBookmarkHeaderStatus, chAutoResize);
  end;
end;

procedure TfmMain.RunBatch(batch: TNNTPBatch);
var
  i, ac, gr: Integer;
  params: TGetMessagesParams;
  Action: TBatchAction;
  group: TSubscribedGroup;
  account: TNNTPAccount;
begin
  Inc(fBatchRef);
  batch.LastBatchRef := fBatchRef;
  batch.LastRun := Now;
  for i := 0 to batch.ActionCount - 1 do
  begin
    Action := batch.Actions[i];

    for ac := 0 to NNTPAccounts.Count - 1 do
    begin
      account := NNTPAccounts.Items[ac];

      for gr := 0 to account.SubscribedGroupCount - 1 do
      begin
        group := account.SubscribedGroups[gr];

        if (group.Name = Action.GroupName) and (account.AccountName = Action.AccountName) then
        begin
          FillChar(params, SizeOf(params), 0);
          BatchToParams(action, params);
          params.batchRef := fBatchRef;
          DoGetMessages(group, LPARAM(@params));
        end;
      end;
    end;
  end;
end;

procedure TfmMain.SaveArticleHeaderColumns;
var
  i: Integer;
  cw: Integer;
begin
  cw := vstArticles.ClientWidth;
  with vstArticles.Header do
    for i := 0 to Columns.Count - 2 do
      XNOptions.ArticlesColumnPCs[i] := (Columns[i].Width * 100 + cw div 2) div cw;
end;

procedure TfmMain.SaveArticleHeaderPositions;
var
  i: Integer;
begin
  with vstArticles.Header do
    for i := 0 to Columns.Count - 1 do
      XNOptions.ArticlesColumnPositions[i] := Columns[i].Position;
end;

procedure TfmMain.SaveOutstandingPostings;
var
  f: TFileStream;
  writer: TTextStreamWriter;
  i, j, k, attachCount: Integer;
  st: string;
  getter: TTCPGetter;
  posterRequest: TPosterRequest;
  mailerRequest: TEMailerRequest;
  h: TAnsiStrings;
  m: TAnsiStrings;
  requests, getters: TObjectList;
begin
  m := nil;
  f := TFileStream.Create(gMessageBaseRoot + '\UnsentMessages.dat', fmCreate);
  try
    writer := TTextStreamWriter.Create(f);
    try
      m := TAnsiStringList.Create;
      getters := ThreadManager.LockGetterList;
      try
        for i := 0 to getters.Count - 1 do
        begin
          getter := TTCPGetter(getters[i]);

          if Getter is TPoster then
          begin
            requests := TPoster(getter).LockList;
            try
              for j := 0 to requests.Count - 1 do
              begin
                posterRequest := TPosterRequest(requests[j]);
                if Assigned(posterRequest.Attachments) then
                  attachCount := posterRequest.Attachments.Count
                else
                  attachCount := 0;

                m.Text := posterRequest.Msg;
                h := posterRequest.Hdr;

                st := TPoster(getter).Account.AccountName + #9 +
                  IntToStr(h.Count) + #9 +
                  IntToStr(m.Count) + #9 +
                  IntToStr(attachCount) + #9 +
                  IntToStr(posterRequest.Codepage);

                writer.WriteLn(st);

                for k := 0 to h.Count - 1 do
                  writer.WriteLn(h[k]);

                for k := 0 to m.Count - 1 do
                  writer.WriteLn(m[k]);

                for k := 0 to attachCount - 1 do
                  writer.WriteLn(TAttachment(posterRequest.Attachments[k]).PathName);
              end;
            finally
              TPoster(getter).UnlockList;
            end;
          end
          else
            if Getter is TEMailer then
            begin
              requests := TEMailer(getter).LockList;
              try
                for j := 0 to requests.Count - 1 do
                try
                  mailerRequest := TEMailerRequest(requests[j]);
                  if Assigned(mailerRequest.Attachments) then
                    attachCount := mailerRequest.Attachments.Count
                  else
                    attachCount := 0;

                  m.Text := RawByteString(mailerRequest.Msg);
                  if mailerRequest.ArticleContainer is TSubscribedGroup then
                    st := '&&' + TSubscribedGroup(mailerRequest.ArticleContainer).Owner.AccountName + ':'
                  else
                    st := '&&' + 'Mail:';

                  st := st + mailerRequest.ArticleContainer.Name + #9 +
                        IntToStr(5 {h.Count}) + #9 +
                        IntToStr(m.Count) + #9 +
                        IntToStr(attachCount) + #9 +
                        IntToStr(mailerRequest.Codepage);

                  writer.WriteLn(st);

                  writer.WriteLn(WideStringToAnsiString(mailerRequest.MTo, mailerRequest.Codepage));
                  writer.WriteLn(WideStringToAnsiString(mailerRequest.MCC, mailerRequest.Codepage));
                  writer.WriteLn(WideStringToAnsiString(mailerRequest.MBCC, mailerRequest.Codepage));
                  writer.WriteLn(WideStringToAnsiString(mailerRequest.MSubject, mailerRequest.Codepage));
                  writer.WriteLn(WideStringToAnsiString(mailerRequest.MReplyTo, mailerRequest.Codepage));

                  for k := 0 to m.Count - 1 do
                    writer.WriteLn(m[k]);

                  for k := 0 to attachCount - 1 do
                    writer.WriteLn(TAttachment(mailerRequest.Attachments[k]).PathName);
                except
                end;
              finally
                TEMailer(getter).UnlockList;
              end;
            end;
        end;
      finally
        ThreadManager.UnlockGetterList;
      end;
    finally
      writer.Free;
    end;
  finally
    f.Free;
    m.Free;
  end;
end;

procedure TfmMain.SaveToolbarLayout;
var
  reg: TExSettings;
  i: Integer;
  ctrl: TWinControl;
  st: string;
begin
  reg := CreateExSettings;
  try
    reg.Section := 'Position\Toolbar';

    reg.BooleanValue['ShowCaptions'] := actViewToolbarCaptions.Checked;
    reg.BooleanValue['SmallImages'] := actViewToolbarSmallImages.Checked;

    reg.Section := reg.Section + '\Bands';

    for i := 0 to cbMain.Bands.Count - 1 do
    begin
      ctrl := cbMain.Bands[i].Control;

      with cbMain.Bands[i] do
        st := Format('%d,%d,%d,%d', [i, Width, Ord(Break), Ord(Control.Visible)]);

      reg.StringValue[ctrl.Name] := st;
    end;

    // QEC20100120-13:31 - Added tbMainButtons LeftValues Save
    // 'Software\Woozle\XanaNews\Position\ToolBar\tbMainButtons'
    if not tbMain.Customizable then
      Exit;

    reg.Section := 'Position\ToolBar\tbMainButtons';
    st := Format('%d', [tbMain.ControlCount]);
    reg.StringValue['ControlCount'] := st;

    st := Format('%d', [tbMain.ButtonCount]);
    reg.StringValue['ButtonCount'] := st;

    // LeftValues =======================================
    st := '';
    for I := 0 to tbMain.ButtonCount - 1 do
    begin
      ctrl := TWinControl(tbMain.Buttons[I]);
      st := st + Format('%d', [ctrl.Left]);
      if (I < (tbMain.ButtonCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['LeftValues-Btns'] := st;

    st := '';
    for I := 0 to tbMain.ControlCount - 1 do
    begin
      ctrl := TWinControl(tbMain.Controls[I]);
      st := st + Format('%d', [ctrl.Left]);
      if (I < (tbMain.ControlCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['LeftValues-Ctrls'] := st;

    // Names ============================================
    st := '';
    for I := 0 to tbMain.ButtonCount - 1 do
    begin
      st := st + tbMain.Buttons[I].name;
      if (I < (tbMain.ButtonCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['Names-Btns'] := st;

    st := '';
    for I := 0 to tbMain.ButtonCount - 1 do
    begin
      st := st + tbMain.Buttons[I].Caption;
      if (I < (tbMain.ButtonCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['Names-Capt'] := st;

    st := '';
    for I := 0 to tbMain.ControlCount - 1 do
    begin
      st := st + tbMain.Controls[I].name;
      if (I < (tbMain.ControlCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['Names-Ctrls'] := st;

    // Style ============================================
    st := '';
    for I := 0 to tbMain.ButtonCount - 1 do
    begin
      if tbMain.Buttons[I].Style = tbsButton then
        st := st + 'B'
      else if tbMain.Buttons[I].Style = tbsSeparator then
        st := st + 'S';
      if (I < (tbMain.ButtonCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['Style-Btns'] := st;

    // Enable ===========================================
    st := '';
    for I := 0 to tbMain.ButtonCount - 1 do
    begin
      if tbMain.Buttons[I].Enabled then
        st := st + 'E'
      else
        st := st + 'D';
      if (I < (tbMain.ButtonCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['Enabled-Btns'] := st;

    st := '';
    for I := 0 to tbMain.ControlCount - 1 do
    begin
      if tbMain.Controls[I].Enabled then
        st := st + 'E'
      else
        st := st + 'D';
      if (I < (tbMain.ControlCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['Enabled-Ctrls'] := st;

    // Visible ==========================================
    st := '';
    for I := 0 to tbMain.ButtonCount - 1 do
    begin
      if tbMain.Buttons[I].Visible then
        st := st + 'V'
      else
        st := st + 'I';
      if (I < (tbMain.ButtonCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['Visible-Btns'] := st;

    st := '';
    for I := 0 to tbMain.ControlCount - 1 do
    begin
      if tbMain.Controls[I].Visible then
        st := st + 'V'
      else
        st := st + 'I';
      if (I < (tbMain.ControlCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['Visible-Ctrls'] := st;

    // Index ============================================
    st := '';
    for I := 0 to tbMain.ButtonCount - 1 do
    begin
      st := st + IntToStr(tbMain.Buttons[I].ComponentIndex);
      if (I < (tbMain.ButtonCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['Index-BtnComp'] := st;

    st := '';
    for I := 0 to tbMain.ControlCount - 1 do
    begin
      st := st + IntToStr(tbMain.Controls[I].ComponentIndex);
      if (I < (tbMain.ControlCount - 1)) then
        st := st + ',';
    end;
    reg.StringValue['Index-CtlComp'] := st;

    // ==================================================

  finally
    reg.Free;
  end;
end;

procedure TfmMain.SearchMessageFormDestroy(Sender: TObject);
begin
  ModelessWindowFormDestroy(Sender);
  fSearchMessageForm := nil;
end;

procedure TfmMain.SelectArticleNode(Node: PVirtualNode);
begin
  if Assigned(Node) then
  begin
    vstArticles.BeginUpdate;
    try
      vstArticles.ClearSelection;
      vstArticles.FocusedNode := node;
      vstArticles.Selected[node] := True;

      vstArticles.ScrollIntoView(node, False);

      if XNOptions.AutoExpandThread and not fInCollapse then
      begin
        while node^.Parent <> vstArticles.RootNode do
          node := node^.Parent;
        FullExpandThreads(GetNodeArticle(vstArticles.FocusedNode).Owner, node);
      end;

    finally
      vstArticles.EndUpdate;
      if XNOptions.AutoCentralizeMessage then
        CentralizeDisplay;
    end;
  end;
end;

procedure TfmMain.SetControlOptions(initfonts: Boolean);
begin
  // Set control layout, based on 'XNOptions' registry entries.
  XNOptions.PanelLeft := pnlLeft.Width;
  if pnlLeft.Floating then
  begin
    XNOptions.PanelLeftSplitter := 2;
    if Assigned(pnlLeft.Parent) then
    begin
      XNOptions.PanelLeftLeft := pnlLeft.Parent.Left;
      XNOptions.PanelLeftTop := pnlLeft.Parent.Top;
      XNOptions.PanelLeftHeight := pnlLeft.Parent.Height;
      XNOptions.PanelLeft := pnlLeft.Parent.Width;
    end;
  end
  else
    if pnlLeft.Parent = SplitterPanel2 then
      XNOptions.PanelLeftSplitter := 1
    else
      XNOptions.PanelLeftSplitter := 0;

  XNOptions.ArticlesHeight := spltArticles.ResizeControlSize;

  XNOptions.QueuedRequestsHeight := spltQueuedRequests.ResizeControlSize;
  XNOptions.ShowInSystemTray := TrayIcon1.Visible;
  XNOptions.BookmarkHeight := spltBookmark.ResizeControlSize;

  XNOptions.MainToolbarLeft := tbMain.Left;
  XNOptions.MenuToolbarLeft := tbMenu.Left;
  XNOptions.MainToolbarTop := tbMain.Top;
  XNOptions.MenuToolbarTop := tbMenu.Top;

  SaveArticleHeaderColumns;
  SaveArticleHeaderPositions;

  if initfonts then
  begin
    XNOptions.Appearance[apMessageHeaders].Init(vstArticles.Font, vstArticles.Color);
    if MessageScrollBox1.FixedFont = '' then
      XNOptions.Appearance[apMessagePane].Init(MessageScrollBox1.Font, MessageScrollBox1.Color);
    XNOptions.Appearance[apSubscribedGroups].Init(vstSubscribed.Font, vstSubscribed.Color);
    XNOptions.Appearance[apMainForm].Init(Self.Font, Self.Color);
    XNOptions.Appearance[apToolBar].Init(cbMain.Font, cbMain.Color);
    XNOptions.Appearance[apMessageDetailsPanel].Init(pnlDetailsBar.Font, pnlDetailsbar.Color);
    XNOptions.Appearance[apMenu].Init(tbMenu.Font, tbMenu.Color);
  end;
end;

procedure TfmMain.spPauseRequestsClick(Sender: TObject);
begin
  ThreadManager.AllThreadsPaused := spPauseRequests.Down;
end;

procedure TfmMain.Timer1Timer(Sender: TObject);
var
  ct, cta, cto: DWORD;
  hasUnread: Boolean;
  i, j: Integer;
  ico: HICON;
  icon: TIcon;
  article: TArticleBase;
  soundfile: string;
  lv: string;
  sl: TStringList;

  function VersionCompare(v1, v2: string): Boolean;
  var
    v1hh, v1h, v1l: string;
    v2hh, v2h, v2l: string;

    l1hh, l1h, l1l, l1ll: Integer;
    l2hh, l2h, l2l, l2ll: Integer;
    i: Integer;
  begin
    v1hh := SplitString('.', v1); v1h := SplitString('.', v1); v1l := SplitString('.', v1);
    v2hh := SplitString('.', v2); v2h := SplitString('.', v2); v2l := SplitString('.', v2);

    l1hh := StrToIntDef(v1hh, 0); l1h := StrToIntDef(v1h, 0); l1l := StrtoIntDef(v1l, 0); l1ll := StrToIntDef(v1, 0);
    l2hh := StrToIntDef(v2hh, 0); l2h := StrToIntDef(v2h, 0); l2l := StrtoIntDef(v2l, 0); l2ll := StrToIntDef(v2, 0);

    i := l1hh - l2hh;
    if i = 0 then
    begin
      i := l1h - l2h;
      if i = 0 then
      begin
        i := l1l - l2l;
        if i = 0 then
          i := l1ll - l2ll;
      end;
    end;

    Result := i > 0;
  end;

begin
  if gAppTerminating then
    Exit;

  // If a timer was just disabled, it still possible that this event fires, because
  // KillTimer does not remove WM_TIMER messages already posted to the message queue.
  if not (Sender as TTimer).Enabled then
    Exit;

  if not Assigned(ThreadManager) then
    Exit;

  if (GetKeyState(VK_LBUTTON) and $8000) <> 0 then
    Exit;

  actEditCopy.Enabled := True;
  fInCollapse := False;
  MessageScrollBox1.URLText := '';

  Inc(fTickCount);

(*
  if fAutoMarkTicks < MaxInt then
    Inc(fAutoMarkTicks);

  if XNOptions.AutoMarkAsRead and (XNOptions.AutoMarkSeconds > 0) then
  begin
    if (fAutoMarkTicks < MaxInt) and (fAutoMarkTicks > XNOptions.AutoMarkSeconds) then
    begin
      article := GetFocusedArticle;
      if Assigned(article) then
        if Assigned(article.Msg) and not article.IsRead then
        begin
          article.IsRead := True;
          vstArticles.Invalidate;
          Refresh_vstSubscribed;
        end;
      fAutoMarkTicks := MaxInt
    end
  end;
*)

(* Dave Nottage Mod - http://blogs.teamb.com/davenottage *)
  article := GetFocusedArticle;
  if Assigned(article) then
  begin
    // Only increase the tick count if the message has downloaded, or
    // the article isn't on the server
    if (Assigned(article.Msg) or article.IsNotOnServer) and (fAutoMarkTicks < MaxInt) then
      Inc(fAutoMarkTicks);
    if XNOptions.AutoMarkAsRead and (XNOptions.AutoMarkSeconds > 0) then
    begin
      if (fAutoMarkTicks < MaxInt) and (fAutoMarkTicks > XNOptions.AutoMarkSeconds) then
      begin
        if (Assigned(article.Msg) or article.IsNotOnServer) and not article.IsRead then
        begin
          article.IsRead := True;
          vstArticles.Invalidate;
          Refresh_vstSubscribed;
        end;
        fAutoMarkTicks := MaxInt;
      end;
    end;
  end;
(* End Dave Nottage Mod *)

  if fTickCount < 4 then
    Exit;

  if not fOptionsFormActive then
  begin
    lv := GetLatestVersion;
    if (lv <> '~') and (lv <> '') then
    begin
      if VersionCompare(lv, ProductVersion) then
      begin
        StatusBar.Panels[StatusBar.Panels.Count - 1].Text := Format(rstVersionAvailable, [lv]);
        NewVersion := lv;
        FormResize(nil);
      end;
      SetLatestVersion('~');
    end;

    lv := GetDeserveMedals;
    if (lv <> '~') and (lv <> '') then
    begin
      fDeservesMedals := lv;
      sl := TStringList.Create;
      try
        sl.CaseSensitive := False;
        sl.Text := lv;
        sl.Sort;

        XNOptions.DeservesMedal := sl.Find(NNTPAccounts.Identities.DefaultIdentity.UserName, i);

        if Assigned(fMedal) then
          fMedal.Visible := XNOptions.DeservesMedal;
        XNOptions.Save;
      finally
        sl.Free;
      end;
      SetDeserveMedals('~');
    end;
  end;

  ct := ThreadManager.LockGetterList.Count;
  try
    cto := ThreadManager.QueuedRequestCount;

    if NNTPAccounts.HideDormantConnections then
      cta := ThreadManager.ActiveGetterCount
    else
      cta := ct;
  finally
    ThreadManager.UnlockGetterList;
  end;

  if (ct <> DWORD(fOutstandingGetterCount)) or
     (cto <> DWORD(fOutstandingRequestCount)) or
     (cta <> DWORD(fOutstandingActiveGetterCount)) then
  begin
    fOutstandingRequestCount := cto;
    fOutstandingGetterCount := ct;
    fOutstandingActiveGetterCount := cta;

    if NNTPAccounts.HideDormantConnections then
      vstQueuedRequests.RootNodeCount := cta
    else
      vstQueuedRequests.RootNodeCount := ct;
    Refresh_vstQueuedRequests;
  end;

  if Assigned(MessageScrollBox1.Msg) and
    MessageScrollBox1.Msg.Updating then
      MessageScrollBox1.Refresh(False, False);

  if (fTickCount mod 8) = 0 then
  begin
    Refresh_vstSubscribed;
    Refresh_vstQueuedRequests;
    NNTPAccounts.RemoveOldBozos;
  end;

  if Assigned(fLastFocusedArticleContainer) then
    InitArticlesRootNodeCount(fLastFocusedArticleContainer);

  if XNOptions.ShowInSystemTray then
  begin
    hasUnread := False;

    soundFile := '';
    for i := 0 to NNTPAccounts.Count - 1 do
    begin
      for j := 0 to NNTPAccounts.Items[i].SubscribedGroupCount - 1 do
        if NNTPAccounts.Items[i].SubscribedGroups[j].UnreadReplyCount <> 0 then
        begin
          hasUnread := True;
          soundFile := NNTPAccounts.Items[i].SubscribedGroups[j].DisplaySettings.SoundFile;
          if (soundFile <> '') and FileExists(soundFile) then
            Break;
        end;
      if hasUnread then
        Break;
    end;

    if fHadUnread <> hasUnread then
    begin
      fHadUnread := hasUnread;

      if hasUnread then
      begin
        if (not fSuppressSound) and FileExists(soundFile) then
          PlaySound(PChar(soundFile), 0, SND_FILENAME or SND_ASYNC);
        ico := LoadImage(hInstance, 'YELLOWPAPER', IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
      end
      else
        ico := LoadImage(hInstance, 'MAINICON', IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);

      Icon := TIcon.Create;
      try
        Icon.Width := 16;
        Icon.Height := 16;
        Icon.Handle := ico;
        TrayIcon1.Icon := icon;
      finally
        Icon.Free;
      end;
      DestroyIcon(ico);
    end;
  end;

  fSuppressSound := False;
  CheckScheduledBatches;

  if fRetrySetMsgFlag then
  begin
    fRetrySetMsgFlag := False;
    article := GetFocusedArticle;
    DisplayArticleBody(article);
  end;

  if (fModelessWindowList.Count = 0) and not fFindDialogShowing and not fRenamingFolder and not fInSearchBarText and (fDisableShortcutCount > 0) then
  begin
    fDisableShortcutCount := 1;
    EnableShortcuts(True);
  end;
end;

procedure TfmMain.TrayIcon1DblClick(Sender: TObject);
begin
  actTrayOpen.Execute;
end;

function TfmMain.Unsubscribe(group: TSubscribedGroup; param: LPARAM): Boolean;
begin
  BeginPurge(group);
  SendMessage(handle, WM_GROUPSCHANGING, 0, 0);
  try
    ThreadManager.Cancel(group.Owner.NNTPServerSettings, group, nil);

    if param = 1 then
      group.Owner.UnsubscribeTo(group.Name, True)
    else
      group.Owner.UnsubscribeTo(group.Name, False);
    Result := True;
  finally
    SendMessage(handle, WM_GROUPSCHANGED, 0, 0);
    EndPurge;
  end;
end;

procedure TfmMain.UpdateActions;
var
  hasSelAccount, hasSelGroup, hasSelCount, hasSelFolder, hasKeyPhrase, selFolderEmpty, hasFocusedArticle: Boolean;
  isNNTPArticle: Boolean;
  SelectedAccount: TNNTPAccount;
  SelectedGroup: TSubscribedGroup;
  SelectedFolder: TArticleFolder;
  FocusedArticle: TArticleBase;
  st: TThreadManagerState;
  selCount: Integer;
  obj: TObject;
  QGroup: TServerAccount;
  QGetter: TTCPGetter;
  QIdx: Integer;
  prMin, prMax, prPos: Integer;
  sbText: string;
  s: string;
begin
  // Enable/Disable actions depending on state.
  if gAppTerminating then Exit;
  SelectedAccount := GetFocusedAccount; hasSelAccount := Assigned(SelectedAccount);
  SelectedGroup := GetFocusedGroup;     hasSelGroup   := Assigned(SelectedGroup);
  SelectedFolder := GetFocusedArticleFolder; hasSelFolder := Assigned(SelectedFolder);

  FocusedArticle := GetFocusedArticle;
  isNNTPArticle := FocusedArticle is TArticle;
  selCount := vstArticles.SelectedCount;
  if Assigned(fTestMessage) and (fTestMessage = MessageScrollBox1.Msg) then
    selCount := 1;
  hasSelCount := selCount > 0;

  hasKeyPhrase := hasSelAccount and (SelectedAccount.ScanKeyPhrases);

  if hasSelFolder then
    selFolderEmpty := SelectedFolder.IsEmpty
  else
    selFolderEmpty := True;

  hasFocusedArticle := Assigned(FocusedArticle);

  actFolderRename.Enabled := hasSelFolder;
  actFolderReindex.Enabled := hasSelFolder;
  actFolderDelete.Enabled := hasSelFolder and not (SelectedFolder is TPurgedMessages) and not (SelectedFolder is TSentMessages);
  actFolderClear.Enabled := hasSelFolder and not selFolderEmpty;
  actFolderReloadMessages.Enabled := hasSelFolder and not selFolderEmpty;
  actFolderReloadAllMessages.Enabled := hasSelFolder and not selFolderEmpty;
  actFolderFindMessage.Enabled := FocusedArticle is TFolderArticle;

  actFilePrint.Enabled := hasSelCount;

  actAccountShowNewsgroupList.Enabled := hasSelAccount;
  actAccountRefreshGroupList.Enabled := hasSelAccount;
  actAccountProperties.Enabled := hasSelAccount;
  actAccountRemove.Enabled := hasSelAccount;
  actAccountAdd.Enabled := True;
  actAccountSortGroupsByName.Enabled := hasSelAccount;
  actAccountSortGroupsByName.Checked := hasSelAccount and SelectedAccount.SortGroupsByName;

  actNewsgroupGetMessagesDefault.Enabled := hasSelGroup or hasSelAccount;
  actNewsgroupDeleteMessages.Enabled := hasSelGroup or HasSelAccount;
  actNewsgroupGetMessages.Enabled := hasSelGroup or HasSelAccount;
  actNewsgroupUnsubscribe.Enabled := hasSelGroup;
  actNewsgroupMakeDormant.Enabled := hasSelGroup;
  actnewsgroupMarkAllMessagesAsRead.Enabled := hasSelGroup or hasSelAccount;

  actSearchSearchMessages.Enabled := hasSelGroup or hasSelAccount or hasSelFolder;
  actSearchFindNextUnreadMessageToMe.Enabled := True;
  actSearchFindNextReplyToMe.Enabled := True;
  actSearchFindFlagged.Enabled := True;
  actSearchFindFlaggedInNewThread.Enabled := hasFocusedArticle and FocusedArticle.IsInteresting;

  actArticleSaveHTML.Enabled := hasFocusedArticle;
  actArticleCopyHTML.Enabled := hasFocusedArticle;
  actViewFindOnInternet.Enabled := hasFocusedArticle;
  actViewFindTextOnInternet.Enabled := hasFocusedArticle;

  actEditSelectThread.Enabled := hasFocusedArticle;
  actEditSelectSubThread.Enabled := hasFocusedarticle;
  actFileExportSelected.Enabled := hasSelCount;
  actFileImportArticles.Enabled := True;

  actMessageAddToBozoBin.Enabled := hasFocusedArticle;

  actMessageSaveAttachment.Enabled := selCount > 0;

  actEditCopyLink.Enabled := selCount = 1;
  actArticleGetMessageBody.Enabled := hasSelCount;
  actArticleGetThread.Enabled := hasSelCount;
  actArticleDeleteThread.Enabled := hasSelCount;
  actArticleMarkMessageAsRead.Enabled := hasSelCount;
  actArticleMarkThreadAsRead.Enabled := hasSelCount;
  actArticleMarkThreadAsInteresting.Enabled := hasSelCount;
  actArticleMarkBranchAsRead.Enabled := hasSelCount;
  actArticleFlag.Enabled := hasSelCount;
  actArticleExpandThread.Enabled := hasFocusedArticle;
  actArticleExpandAllThreads.Enabled := hasSelGroup;
  actArticleCollapseThread.Enabled := hasFocusedArticle;
  actArticleCollapseAllThreads.Enabled := hasSelGroup;
  actArticleNextUnread.Enabled := hasSelAccount;
  actArticleGotoPrevious.Enabled := not fPrevArticleStack.IsEmpty;
  actMessageDelete.Enabled := hasSelCount;
  actNewsgroupProperties.Enabled := hasSelGroup;
  actViewGroupMultipart.Enabled := hasSelGroup;
  actArticleReplyToMessage.Enabled := hasFocusedArticle and isNNTPArticle;
  actArticleReplyByMail.Enabled := hasFocusedArticle;
  actArticlePostNewMessage.Enabled := Assigned(SelectedAccount);


  actEditDelete.Enabled := hasSelCount or vstSubscribed.Focused;
  actArticleMarkMessageAsRead.Enabled := hasSelCount;

  obj := GetFocusedObject;

  st := ThreadManager.ThreadManagerState[obj];
  if st = tmBusy then
  begin
    actToolsReconnect.Enabled := True;
    actToolsReconnect.ImageIndex := 10;   // Red button
  end
  else
    if st = tmPending then
    begin
      actToolsReconnect.Enabled := True;
      actToolsReconnect.ImageIndex := 9;  // Green Button
    end
    else
    begin
      actToolsReconnect.ImageIndex := 8;  // 50 Gray Button
      actToolsReconnect.Enabled := False;
    end;

  prMin := 0;
  prMax := 0;
  prPos := 0;
  sbText := '';

  if fSM = '' then
  try
    if vstQueuedRequests.Focused then
    begin
      QGetter := GetQNodeGetter(vstQueuedRequests.FocusedNode, QIdx);
      if Assigned(QGetter) then
      begin
        if QIdx >= 0 then
          QGroup := QGetter.Group[QIdx]
        else
          QGroup := nil;

        if Assigned(QGroup) then
        begin
          sbText := QGetter.StatusBarMessage[QGroup];
          QGetter.GetProgressNumbers(QGroup, prMin, prMax, prPos);
        end;

        actQRPause.Enabled := True;
      end
      else
        actQRPause.Enabled := False;

      actQRDelete.Enabled := QIdx <> -1;
      actQREdit.Enabled := (QIdx <> -1) and ((QGetter is TPoster) or (QGetter is TEmailer));
      if QIdx <> -1 then
        actEditDelete.Enabled := True;
    end
    else
    begin
      actQRPause.Enabled := False;
      actQRDelete.Enabled := False;
      actQREdit.Enabled := False;
      if Assigned(SelectedAccount) then
      begin
        if fSM = '' then
          sbText := ThreadManager.StatusBarMessage[SelectedAccount.NNTPServerSettings, SelectedGroup]
        else
          sbText := fSM;
        ThreadManager.GetProgressNumbers(SelectedAccount.NNTPServerSettings, SelectedGroup, prMin, prMax, prPos);
      end;
    end;
  except
  end
  else
  begin
    sbText := fSM;
    prMin := 0;
    prMax := fSMMax;
    prPos := fSmPos;
  end;

  actToolsSendOutbasket.Enabled := ThreadManager.NoOutbasketEnties > 0;

  UpdateStatusBar(sbText, prMin, prMax, prPos);

  actSearchFindInMessage.Enabled := Assigned(MessageScrollBox1.Msg) and not fFindDialogShowing;
  actMessageCopyXFace.Enabled := Assigned(MessageScrollBox1.Msg) and Assigned(MessageScrollBox1.Msg.XFace);

  actToolsOptions.Enabled := True;
  actToolsAccounts.Enabled := True;
  actHelpAbout.Enabled := True;
  actToolsBatches.Enabled := True;

  s := Application.Title {$ifdef CPUX64} + ' (x64)' {$endif};
  if Assigned(SelectedGroup) then
    Caption := Format('%s %s - %s', [s, ProductVersion, SelectedGroup.Name])
  else
    Caption := s + ' ' + ProductVersion;

  actToolsMessagebaseManagement.Enabled := True;
  actToolsAdminCreateGroup.Enabled := hasSelAccount;
  actToolsAdminRemoveGroup.Enabled := hasSelAccount;

  if hasFocusedArticle and (FocusedArticle is TArticle) then
    actArticleCancel.Enabled := True
  else
    actArticleCancel.Enabled := False;

  actSearchFindKeyword1.Enabled := hasKeyPhrase and (XNOptions.KeyPhrase[0] <> '');
  actSearchFindKeyword2.Enabled := hasKeyPhrase and (XNOptions.KeyPhrase[1] <> '');
  actSearchFindKeyword3.Enabled := hasKeyPhrase and (XNOptions.KeyPhrase[2] <> '');
  actSearchFindKeyword4.Enabled := hasKeyPhrase and (XNOptions.KeyPhrase[3] <> '');
  actSearchFindKeyword5.Enabled := hasKeyPhrase and (XNOptions.KeyPhrase[4] <> '');
  actSearchFindKeyword6.Enabled := hasKeyPhrase and (XNOptions.KeyPhrase[5] <> '');
  actSearchFindKeyword7.Enabled := hasKeyPhrase and (XNOptions.KeyPhrase[6] <> '');
  actSearchFindKeyword8.Enabled := hasKeyPhrase and (XNOptions.KeyPhrase[7] <> '');

  actSearchFindAnyKeyword.Enabled := hasKeyPhrase;

  actToolsDecodePerformance.Enabled := hasFocusedArticle;
  actQRClear.Enabled := fOutstandingRequestCount > 0;
  actROT13.Enabled := MessageScrollBox1.SelLength > 0;
  actReverseSelectedText.Enabled := actROT13.Enabled;

  actBookmarksAdd.Enabled := pnlBookmark.Visible and (selCount > 0);
  actBookmarksRemove.Enabled := pnlBookmark.Visible and Assigned(vstBookmark.FocusedNode);
  actBookmarksDelete.Enabled := pnlBookmark.Visible and Assigned(fCurrentBookmark);
  actBookmarksCreate.Enabled := pnlBookmark.Visible;
  actBookmarksClearAll.Enabled := pnlBookmark.Visible and (fBookmarkSet.BookmarkCount > 0);

  actViewShowSecrets.Checked := NNTPAccounts.ShowSecrets;
  actViewAutofitImages.Checked := MessageScrollBox1.AutoFit;

  if fNextArticleStack.IsEmpty then
    actArticleGotoNext.ImageIndex := 24
  else
    actArticleGotoNext.ImageIndex := 35;

  spFixedFont.Visible := not IsFontFixed(XNOptions.Appearance[apMessagePane].FontName);

  actArticleMarkMessageAsInteresting.Enabled := hasFocusedArticle;
  actArticleIgnore.Enabled := hasFocusedArticle;
  actArticleIgnoreBranch.Enabled := hasFocusedArticle;
  actArticleIgnoreThread.Enabled := hasSelCount;
  actArticleRetrieveParentMessages.Enabled := hasFocusedArticle;
  actArticleCombineDecode.Enabled := (selCount > 1) or
    ((selCount = 1) and isNNTPArticle and FocusedArticle.Owner.GroupMultiPartMessages and
     (FocusedArticle.MultipartFlags <> mfNotMultipart));
end;

procedure TfmMain.vstArticlesAfterItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
var
  article: TArticleBase;
begin
  // Draw the odd/even background colour.
  article := GetNodeArticle(node);

  if Assigned(article) and article.IsOdd then
  begin
    TargetCanvas.Brush.Color := XNOptions.Appearance[apMessageHeaders].AlternateBGColor;
    TargetCanvas.FillRect(ItemRect);
  end;
end;

procedure TfmMain.vstArticlesAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
var
  article: TArticleBase;
  art: TArticle;
  r: TRect;
  rgn: HRGN;
  scan: Boolean;
begin
  // Draw extended flags - eg. Keywords blobs, etc.
  article := GetNodeArticle(node);
  if not Assigned(article) then Exit;

  if article is TArticle then
  begin
    art := TArticle(article);
    scan := art.Account.ScanKeyPhrases;
  end
  else
  begin
    art := nil;
    scan := False;
  end;

  if Assigned(article) and (coVisible in vstArticles.Header.Columns[0].Options) and (not article.IsDormant or article.IsInteresting or scan or article.IsFromBozo) then
  begin
    r := ItemRect;
    r.Left := vstArticles.Header.Columns[0].Left;
    r.Right := r.Left + vstArticles.Header.Columns[0].Width;
    rgn := CreateRectRgnIndirect(r);
    if SelectClipRgn(TargetCanvas.Handle, rgn) <> ERROR then
    try
      if article.IsInteresting then
        TargetCanvas.Draw(r.Left + 16 + 16, ((r.Bottom - r.Top) - ImageList1.Height) div 2, fInterestingIcon)
      else
        if article.IsFromBozo then
          TargetCanvas.Draw(r.Left + 16 + 16, ((r.Bottom - r.Top) - ImageList1.Height) div 2, fGoofyIcon);

      if scan then
        case art.KeyPhraseNo of
          -1: ;
        else
          TargetCanvas.Brush.Color := XNOptions.KeywordColors[art.KeyPhraseNo];
          r.Left := r.Left + 23;
          r.Right := r.Left + 9;
          r.Top := r.Top + 5;
          r.Bottom := r.Top + 9;
          TargetCanvas.Ellipse(r);
        end;

    finally
      SelectClipRgn(TargetCanvas.Handle, 0);
      DeleteObject(rgn);
    end;
  end;
end;

procedure TfmMain.vstArticlesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  article: TArticleBase;
begin
  if node = nil then Exit;
  vstArticles.ScrollIntoView(Node, False);
  article := GetFocusedArticle;

  if not Assigned(article) then
    article := GetNodeArticle(vstArticles.FocusedNode);

  if Assigned(article) and (Article is TArticle) and XNOptions.AutoMarkAsRead then
    if XNOptions.AutoMarkSeconds = 0 then
      if Assigned(article.Msg) then
        TArticle(article).IsRead := True;

  fAutoMarkTicks := 0;

  DisplayArticleBody(article);

  if XNOptions.AutoExpandThread then
    PostMessage(Handle, WM_AUTOEXPAND, 0, 0);
end;

procedure TfmMain.vstArticlesClick(Sender: TObject);
var
  article: TArticleBase;
  n: PVirtualNode;
begin
  // Perform 'auto-download-on-click'
  fClicked := True;
  if XNOptions.AutoDownloadOnClick then
  begin
    Article := GetFocusedArticle;
    if Assigned(article) and not article.HasMsg then
    begin
      n := vstArticles.FocusedNode;
      if XNOptions.AutoExpandThread and not vstArticles.Expanded[n] and Assigned(vstArticles.GetFirstChild(n)) then
      begin
        DoGetArticleBody(article, 0, False);
        vstArticles.Invalidate;
      end
      else
        actArticleGetMessageBodyExecute(nil);
    end;
  end;
end;

procedure TfmMain.vstArticlesColumnResize(Sender: TVTHeader;
  Column: TColumnIndex);
begin
  if fColumnHeaderStatus = [] then
    Include(fColumnHeaderStatus, chColumnResizing);
end;

procedure TfmMain.vstArticlesDblClick(Sender: TObject);
begin
  DisplayArticleBody(nil);      // Immediately clear the message body, rather
                                // than wait for the thread manager to eventually do it.
  actArticleGetMessageBody.Execute;
end;

procedure TfmMain.vstArticlesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var Index: Integer);
var
  article, p: TArticleBase;
  group: TSubscribedGroup;
begin
  Index := -1;

  if kind = ikOverlay then
    Exit;

  article := GetNodeArticle(node);

  if Assigned(article) then
    if column = 0 then
    begin
      group := GetFocusedGroup;

      if Assigned(article) then
        if Article.IsDeleted then
          Index := 9 // Rubbish bin
        else
          if Article.IsCancelled then
            Index := 28
          else
            if Assigned(group) and ThreadManager.GettingArticle(group, article) then
              Index := 5 // Red msg
            else
              if Article.HasMsg then
              begin
                if article.MultipartFlags = mfNotMultipart then
                begin
                  if article.HasAttachment then
                    Index := 23 // Paper-clip
                  else
                    Index := 4; // Plain white message
                end
                else
                  if article.MultipartFlags = mfPartialMultipart then
                    Index := 13  // White munched message
                  else
                  begin
                    Index := 11; // Blue message (all parts might be available)

                    // TODO: It is using Childs and Siblings here, that will only
                    //       work correctly when GroupMultiPartMessages is True
                    //       and when not in threaded mode where Childs and
                    //       Siblings are used to display the threads.
                    //       - At the moment in threaded mode MultipartFlags is
                    //         always mfNotMultipart
                    //       - For multipart articles to always to work this
                    //         needs to be changed to a separate (linked) list.
                    p := TArticle(article.Child);
                    while p <> nil do
                    begin
                      if not p.HasMsg then
                      begin
                        Index := 12; // Blue munched message (Not all parts there)
                        Break;
                      end;
                      p := TArticle(p.Sibling);
                    end;
                  end;
              end
              else
                if article.MultipartFlags = mfPartialMultipart then
                  Index := 15    // Missing some parts (and no bodies are downloaded yet)
                else
                  if article.MultipartFlags = mfCompleteMultipart then
                    Index := 14  // All articles headers are there, but no bodies downloaded yet
                  else
                    if article.IsNotOnServer then
                      Index := 32;
    end
    else
      if (column = vstArticles.Header.MainColumn) and not XNOptions.HideFolderIcons then
      begin
        if Assigned(article.Child) then
          if vsExpanded in Node^.States then
            if article.IsDormant then
              Index := 18
            else
              Index := 7 // Open folder
          else
            if article.IsDormant then
              Index := 17
            else
              Index := 6 // Closed folder
          else
            if Assigned(article.Parent) then
              Index := 8 // Blank image
            else
              Index := 10; // Little arrow denotes thread root
      end;
end;

procedure TfmMain.vstArticlesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var Text: string);
var
  article: TArticleBase;
  st, stNumber: string;
  isRoot: Boolean;
begin
  article := GetNodeArticle(node);
  if article is TArticle then
  begin
    isRoot := not Assigned(Tarticle(article).Parent);
    if fForensicMode then
      stNumber := IntToStr(article.Index) + ' ' + IntToStr(Article.ArticleNo)
    else
      stNumber := IntToStr(article.ArticleNo);
  end
  else
    if article is TFolderArticle then with TFolderArticle(article) do
    begin
      stNumber := string(article.UniqueID); // OrigServer + ':' + ShortGroupName(OrigGroup) + ':' + IntToStr(ArticleNo); // + ' (' + IntToStr(Offset) + ')';
      isRoot := True;
    end
    else
      Exit;

  st := '';
  if Assigned(article) then
  begin
    case Column of
      0: st := '';
      1: st := stNumber;
      2: if not fForensicMode then
           if isRoot or not XNOptions.FirstLineAsSubject then
             st := article.Subject
           else
             st := Article.InterestingMessageLine
         else
           st := Article.PostingHost;
      3: st := Article.FromName;
      4: st := DateTimeToStr(Article.Date);
      5: st := IntToStr(Article.Lines)
    end;
    Text := st;
  end
end;

procedure TfmMain.vstArticlesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
//procedure TfmMain.vstArticlesHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
//  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ctnr: TArticleContainer;
  art: TArticleBase;
  oldCursor: TCursor;
  id: RawByteString;
  col1: Integer;

  procedure SetThreadSortOrder;
  begin
    if (ssCtrl in HitInfo.Shift) and (col1 = 4) then
      col1 := 6;
    if col1 = fHeaderSortCol then
      if ctnr.ThreadSortDirection = sdDescending then
        ctnr.ThreadSortDirection := sdAscending
      else
        ctnr.ThreadSortDirection := sdDescending
    else
    begin
      ctnr.ThreadSortDirection := sdAscending;

      case col1 of
        0, 1: ctnr.ThreadSortOrder := soMessageNo;
        2: if fForensicMode then
             ctnr.ThreadSortOrder := soPostingHost
           else
             ctnr.ThreadSortOrder := soSubject;
        3: ctnr.ThreadSortOrder := soAuthor;
        4: ctnr.ThreadSortOrder := soDate;
        5: ctnr.ThreadSortOrder := soLines;
        6: ctnr.ThreadSortOrder := soNewestMessage;
      end;
    end;
  end;

begin
  col1 := HitInfo.Column;
  art := GetFocusedArticle;
  if Assigned(art) and (art.ArticleNo = 0) then
    art := nil;
  if Assigned(art) then
    id := art.UniqueID;

  ctnr := fLastFocusedArticleContainer;
  if not Assigned(ctnr) then Exit;

  if ctnr is TArticleFolder then
  begin
    DisplayArticleBody(nil);
    SetThreadSortOrder;
  end
  else
  begin
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      if HitInfo.Column = 0 then
                                  // Column 0 (flags) clicked.  Switch between
                                  // chronological and threaded views.
        if ctnr.ThreadOrder = toChronological then
          ctnr.ThreadOrder := toThreaded
        else
          ctnr.ThreadOrder := toChronological
      else
        SetThreadSortOrder
    finally
      Screen.Cursor := oldCursor;
    end;
  end;

  if (HitInfo.Column <> 0) or (ctnr is TArticleFolder) then
  begin
    vstArticles.Header.SortColumn := HitInfo.Column;
    fHeaderSortCol := col1;
    if Assigned(ctnr) then
      vstArticles.Header.SortDirection := TSortDirection(ctnr.ThreadSortDirection)
    else
      vstArticles.Header.SortDirection := TSortDirection(0);
  end;

  InitArticlesRootNodeCount(ctnr);
  Refresh_vstArticles;
  if ctnr is TArticleFolder then
    art := ctnr.FindUniqueID(id);
  GoToArticle(art);
end;

procedure TfmMain.vstArticlesHeaderMouseUp(Sender: TVTHeader;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    if chColumnResizing in fColumnHeaderStatus then
    begin
      Exclude(fColumnHeaderStatus, chColumnResizing);
      SaveArticleHeaderColumns;
    end;
end;

procedure TfmMain.vstArticlesInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  article: TArticleBase;
  data: PObject;
begin
  ChildCount := 0;
  data := PObject(vstArticles.GetNodeData(node));
  if Assigned(data^) then
    // TODO: bug reports on the following line that indicates that data^ changed
    //       to zero in the meantime (or in one case no longer points to valid data)
    //       when TObject.InheritsFrom is called.
    //       Looks like this is a rare multi-threaded issue. Could it be that it
    //       concerned a VirtualTrees issue that has been fixed in the meantime?
    //       Not very likely, it sounds a little bit like false "hope" ;-)
    if data^ is TArticleBase then
    begin
      article := TArticleBase(data^);
      article := article.Child;           // Count child and it's siblings.
      while Assigned(article) do
      begin
        Inc(ChildCount);
        article := article.Sibling;
      end;
    end;
end;

procedure TfmMain.vstArticlesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  data, parentData: PObject;
  article, parentArticle: TArticleBase;
  i: Integer;
begin
  data := PObject(vstArticles.GetNodeData(node));

  if parentNode = nil then
  begin
    if fLastFocusedArticleContainer is TArticleFolder then
    begin
      InitialStates := []; // Article folders are always flat.
      article := nil;
    end
    else
    begin
      // NOTE: Following check on fLastFocusedArticleContainer is a work-around.
      // TODO: In the past there were more reports that seem to pin-point to a
      //       problem regarding the state of fLastFocusedArticleContainer. This
      //       variable does require some cleanup/redesign.
      //       Based on the rare bug reports it is most-likely a multithreaded issue.
      // TODO: Remove "work-around" when the problem with fLastFocusedArticleContainer
      //       is solved.
      if Assigned(fLastFocusedArticleContainer) then
      begin
        article := fLastFocusedArticleContainer.Threads[node^.Index];
        if Assigned(article.Child) then
          InitialStates := [ivsHasChildren]
        else
          InitialStates := [];
      end
      else
      begin
        InitialStates := [];
        article := nil;
      end;
    end;
  end
  else
  begin
    parentData := PObject(vstArticles.GetNodeData(ParentNode));
    parentArticle := TArticleBase(parentData^);
    article := parentArticle.Child;
    i := node^.index;

    while i > 0 do        // Get the i'th sibling of the parent articles child
    begin
      article := article.Sibling;
      if article = nil then
        Break;
      Dec(i);
    end;

    if Assigned(article.Child) then
      InitialStates := [ivsHasChildren]
    else
      InitialStates := [];
  end;

  data^ := article;
end;

(*----------------------------------------------------------------------*
 | TfmMain.vstArticlesMouseDown                                         |
 |                                                                      |
 | Mouse Down handler for vstArticles.  Adjust the handling for the     |
 | right button - if multiple articles are selected, dont change the    |
 | focus.  If a single article is selected, do change the focus (before |
 | popping up the context menu.                                         |
 *----------------------------------------------------------------------*)
procedure TfmMain.vstArticlesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  node: PVirtualNode;
  art: TArticleBase;
begin
  if Button = mbRight then
    if vstArticles.SelectedCount <= 1 then
    begin
      node := vstArticles.GetNodeAt(x, y);
      if Assigned(node) then
      begin
        art := GetNodeArticle(node);

        if art <> GetFocusedArticle then
        begin
          vstArticles.SetFocus;
          vstArticles.ClearSelection;
          vstArticles.Selected[node] := True;
          vstArticles.FocusedNode := node;
        end;
      end;
    end;
end;

const
  HLSMAX = 240;

function BoostContrast(fontColor: TColor; bkColor: TColor; defColor: TColor): TColor;
var
  c1, c2: TColor;
  h1, l1, s1: word;
  h2, l2, s2: word;
begin
  c1 := ColorToRGB(fontColor);
  c2 := ColorToRGB(bkColor);

  ColorRGBToHLS(c1, h1, l1, s1);
  ColorRGBToHLS(c2, h2, l2, s2);

  if Abs(l1 - l2) < 60 then
  begin
    if l1 < (HLSMAX div 2) then
    begin
      Inc(l1, 40);
      Result := ColorHLSToRGB(h1, l1, s1);
    end
    else
    begin
      Dec(l1, 40);
      Result := ColorHLSToRGB(h1, l1, s1);
    end;
  end
  else
    Result := fontColor;
end;

procedure TfmMain.vstArticlesPaintText(Sender: TBaseVirtualTree;
  const Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  article: TArticleBase;
  clr, bclr: TColor;
begin
  article := GetNodeArticle(node);
  clr := Canvas.Font.Color;
  bclr := Canvas.Brush.Color;
  if vsSelected in node.States then
    bclr := clHighlight;
  if Assigned(article) then
  begin
    if not (vsSelected in node^.States) or XNOptions.HighlightSelectedText then
    begin
      if fForensicMode then
      begin
        if (article.Flags and fgSpamSharesPostingHost) <> 0 then
          Canvas.Font.Color := clRed
        else
          if (article.Flags and (fgSpamNoXTrace or fgSpamNopostingHost)) <> 0 then
            Canvas.Font.Color := clMaroon
          else
            if (article.Flags and (fgSpamNoXTrace or fgSpamSharesName)) <> 0 then
              Canvas.Font.Color := clOlive;
      end
      else
      begin
        if article.IsInteresting and not (XNOptions.Appearance[apInterestingMessages].Equals(XNOptions.Appearance[apMessageHeaders])) then
          XNOptions.Appearance[apInterestingMessages].ApplyFontAndGetColor(Canvas.Font)
        else
          if article.IsIgnore then
            XNOptions.Appearance[apIgnoredMessages].ApplyFontAndGetColor(Canvas.Font)
          else
            if article.IsMine then
              if article.IsFromMe then
                XNOptions.Appearance[apMessagesFromMe].ApplyFontAndGetColor(Canvas.Font)
              else
                if article.IsReply then
                  XNOptions.Appearance[apReplies].ApplyFontAndGetColor(Canvas.Font)
                else
                  if Assigned(article.Parent) and article.IsXanaNews and not XNOptions.DontHighlightXanaNewsUsers then
                    XNOptions.Appearance[apXananewsMessages].ApplyFontAndGetColor(Canvas.Font)
                  else
                    XNOptions.Appearance[apMessagesToMe].ApplyFontAndGetColor(Canvas.Font)
            else
              if article.IsXanaNews and not XNOptions.DontHighlightXanaNewsUsers then
                XNOptions.Appearance[apXananewsMessages].ApplyFontAndGetColor(Canvas.Font)
              else
                if article.HasNoReplies and not (XNOptions.Appearance[apChildlessMessages].Equals(XNOptions.Appearance[apMessageHeaders])) then
                  XNOptions.Appearance[apChildlessMessages].ApplyFontAndGetColor(Canvas.Font)
                else
                  if article.IsDormant then
                    XNOptions.Appearance[apDormantMessages].ApplyFontAndGetColor(Canvas.Font);
      end;
    end;

    if not article.isRead then
      Canvas.Font.Style := XNOptions.UnreadFontStyle;
  end;

  if (vsSelected in Node.States) and XNOptions.HighlightSelectedText then
    Canvas.Font.Color := BoostContrast(Canvas.Font.Color, bclr, clr);
end;

procedure TfmMain.vstArticlesResize(Sender: TObject);
begin
  // Respond to articles tree resizing (Adjust the header widths)
  if fColumnHeaderStatus = [] then
  begin
    Include(fColumnHeaderStatus, chResizing);
    try
      ResizeArticleHeader
    finally
      Exclude(fColumnHeaderStatus, chResizing);
    end;
  end;
end;

procedure TfmMain.vstQueuedRequestsDblClick(Sender: TObject);
var
  QGetter: TTCPGetter;
  QIdx: Integer;
begin
  QGetter := GetQNodeGetter(vstQueuedRequests.FocusedNode, QIdx);
  if (QIdx <> -1) and ((QGetter is TPoster) or (QGetter is TEmailer)) then
    actQREditExecute(Sender);
end;

procedure TfmMain.vstQueuedRequestsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  data: PObject;
  getter: TTCPGetter;
begin
  ImageIndex := -1;

  if kind = ikOverlay then
    Exit;

  data := PObject(vstQueuedRequests.GetNodeData(node));

  if not Assigned(data) or not Assigned(data^) then Exit;

  getter := TTCPGetter(data^);
  try
    if (getter is TPoster) or (getter is TEmailer) then
    begin
      ImageIndex := 16;                 // Post icon
      if getter.State = tsPending then
        if getter.Paused then ImageIndex := 21; // Red
    end
    else
      case getter.State of
        tsDormant: if Getter.Connected then
                      ImageIndex := 36  // Green smudge
                    else
                      ImageIndex := 20; // No Image
        tsPending: if getter.Paused then ImageIndex := 21 else ImageIndex := 17; // Red
        tsBusy   : if getter.Paused then ImageIndex := 22 else ImageIndex := 18; // Green
        tsDone   : ImageIndex := 19;    // Wisp image
      end;
  except
  end;
end;

procedure TfmMain.vstQueuedRequestsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  data: PObject;
  getter: TTCPGetter;
  idx: Integer;
begin
  data := PObject(vstQueuedRequests.GetNodeData(node));

  if not Assigned(data) then Exit;

  if Assigned(data^) then
  begin
    getter := TTCPGetter(data^);
    try
      CellText := getter.GetterRootText
    except
    end;
  end
  else
  begin
    idx := node^.Index;
    node := node^.Parent;
    data := PObject(vstQueuedRequests.GetNodeData(node));

    if not Assigned(data) or not Assigned(data^) then Exit;
    getter := TTCPGetter(data^);
    try
      CellText := getter.OutstandingRequestText[idx];
    except
    end;
  end;
end;

procedure TfmMain.vstQueuedRequestsInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  data: PObject;
  getter: TTCPGetter;
begin
  data := PObject(vstQueuedRequests.GetNodeData(node));

  if not Assigned(data) or not Assigned(data^) then Exit;

  getter := TTCPGetter(data^);

  try
    ChildCount := Getter.OutstandingRequestCount;
    if Integer(ChildCount) < 0 then
      ChildCount := 0;
  except
    ChildCount := 0;
  end;
end;

procedure TfmMain.vstQueuedRequestsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  data: PObject;
  getter: TTCPGetter;
  getters: TObjectList;
  ct: Integer;
begin
  data := PObject(vstQueuedRequests.GetNodeData(node));

  if parentNode = nil then
  begin
    getters := ThreadManager.LockGetterList;
    try
      if NNTPAccounts.HideDormantConnections then
        ct := ThreadManager.ActiveGetterCount
      else
        ct := getters.Count;
      if node^.Index < DWORD(ct) then
      begin
        if NNTPAccounts.HideDormantConnections then
          getter := ThreadManager.ActiveGetters[node^.Index]
        else
          getter := TTCPGetter(getters[node^.Index]);
        if Assigned(getter) then
        begin
          data^ := getter;

          if getter.OutstandingRequestCount > 0 then
            InitialStates := InitialStates + [ivshasChildren];
        end;
      end
      else
        data^ := nil;
    finally
      ThreadManager.UnlockGetterList;
    end;
  end
  else
    data^ := nil;
end;

procedure TfmMain.vstSubscribedDblClick(Sender: TObject);
var
  node: PVirtualNode;
begin
  // Bring up the get messages dialog for the double-clicked group
  node := vstSubscribed.FocusedNode;

  if Assigned(node) then
    vstSubscribed.Expanded[node] := True;

  actNewsgroupGetMessages.Execute;
end;

procedure TfmMain.vstSubscribedDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  node, n1: PVirtualNode;
  fldr: TArticleFolder;
  src, dest: TNNTPAccount;
  srcG, destG: TSubscribedGroup;
  data: TObject;
begin
  node := vstSubscribed.GetNodeAt(pt.X, pt.y);
  if Source = vstSubscribed then
  begin
    data := GetNodeObject(vstSubscribed.FocusedNode);
    if data is TNNTPAccount then
    begin
      src := TNNTPAccount(data);
      data := GetNodeObject(node);
      if data is TNNTPAccount then
        dest := TNNTPAccount(data)
      else
        dest := nil;

      if Assigned(dest) then
      begin
        src.SortIdx := dest.SortIdx;
        Reinit_vstSubscribed(True);
        n1 := GetAccountNode(src);
        vstSubscribed.ClearSelection;
        vstSubscribed.FocusedNode := n1;
        vstSubscribed.Selected[n1] := True;
        NNTPAccounts.SaveToRegistry(nil);
      end;
    end
    else
      if data is TSubscribedGroup then
      begin
        srcG := TSubscribedGroup(data);
        data := GetNodeObject(node);
        if data is TSubscribedGroup then
          destG := TSubscribedGroup(data)
        else
          destG := nil;

        if Assigned(destG) then
        begin
          srcG.SortIdx := destG.SortIdx;
          Reinit_vstSubscribed(True);

          n1 := GetArticleContainerNode(srcG);

          vstSubscribed.ClearSelection;
          vstSubscribed.FocusedNode := n1;
          vstSubscribed.Selected[n1] := True;

          NNTPAccounts.SaveToRegistry(srcG.Owner);
        end;
      end;
  end
  else
  begin
    fldr := GetNodeArticleFolder(node);
    MoveSelectedArticlesToFolder(fldr, not (ssCtrl in Shift), ssAlt in Shift);
  end;
end;

procedure TfmMain.vstSubscribedDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  node: PVirtualNode;
  fldr: TArticleFolder;
  dest: TNNTPAccount;
  src: TNNTPAccount;
  srcG, destG: TSubscribedGroup;
  data: TObject;
begin
  node := vstSubscribed.GetNodeAt(pt.X, pt.y);
  if Source = vstSubscribed then
  begin
    data := GetNodeObject(vstSubscribed.FocusedNode);
    if data is TNNTPAccount then
    begin
      src := TNNTPAccount(data);
      data := GetNodeObject(node);
      if data is TNNTPAccount then
        dest := TNNTPAccount(data)
      else
        dest := nil;

      Accept := Assigned(dest) and Assigned(src) and (dest <> src) and (vstSubscribed.SelectedCount = 1)
    end
    else
      if data is TSubscribedGroup then
      begin
        srcG := TSubscribedGroup(data);
        data := GetNodeObject(node);
        if data is TSubscribedGroup then
          destG := TSubscribedGroup(data)
        else
          destG := nil;

        Accept := Assigned(destG) and Assigned(srcG) and (destG <> srcG) and (vstSubscribed.SelectedCount = 1) and (destG.Owner = srcG.Owner);
      end;
  end
  else
  begin
    fldr := GetNodeArticleFolder(node);

    Accept := Assigned(fldr) and (fldr <> GetFocusedArticleFolder) and not (fldr is TSentMessages);
  end;
end;

procedure TfmMain.vstSubscribedEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  fldr: TArticleFolder;
begin
  // Only allow editing of article folder names
  if fEditNameAllowed then
  begin
    fldr := GetNodeArticleFolder(node);
    Allowed := Assigned(fldr);
  end
  else
    Allowed := False;
end;

procedure TfmMain.vstSubscribedExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PObject;
begin
  Data := PObject(Sender.GetNodeData(Node));
  if Data^ is TNNTPAccount then
    TNNTPAccount(Data^).DisplaySettings.Expanded := True;
end;

procedure TfmMain.vstSubscribedFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  data: PObject;
begin
  if Node <> fExpandNode then
    fExpandNode := nil;

  pnlDetailsBar.Caption := '';
  data := PObject(vstSubscribed.GetNodeData(node));

  if Assigned(data) and Assigned(data^) then
  begin
    if data^ is TArticleContainer then
    begin
      if data^ is TArticleFolder then
        vstSubscribed.PopupMenu := pomFolders
      else
        vstSubscribed.PopupMenu := pomGroups;

      if fLastFocusedArticleContainer <> data^ then
      begin
        fAutoGetMessages := True;
        FocusArticleContainer(TArticleContainer(data^));
      end;
    end
    else
    begin
      FocusArticleContainer(nil);
      if data^ is TNNTPAccount then
        vstSubscribed.PopupMenu := pomGroups
      else
        vstSubscribed.PopupMenu := pomFolders;
    end
  end
  else
    FocusArticleContainer(nil);
end;

procedure TfmMain.vstSubscribedGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var Index: Integer);
var
  data: PObject;
begin
  index := -1;

  if kind = ikOverlay then
    Exit;

  data := PObject(vstSubscribed.GetNodeData(node));
  if Assigned(data) and Assigned(data^) then
    if data^ is TNNTPAccount then
      if ThreadManager.GettingNewsgroupList(TNNTPAccount(Data^)) then
        Index := 2 // Red world
      else
        if TNNTPAccount(Data^).HasNewGroups then
          index := 26
        else
          index := 0 // Blue world
      else
        if data^ is TSubscribedGroup then
          if ThreadManager.GettingArticleList(TSubscribedGroup(Data^)) then
            index := 3 // Red specs
          else
            if TSubscribedGroup(Data^).Loaded then
              Index := 27
            else
              index := 1 // Blue specs
          else
            if data^ is TArticleFolders then
              if vsExpanded in Node^.States then
                index := 20
              else
                index := 19
            else
              if data^ is TArticleFolder then
                if data^ is TPurgedMessages then
                  if TArticleFolder(data^).ArticleCount > 0 then
                    Index := 22
                  else
                    Index := 21
                else
                  if data^ is TSentMessages then
                    Index := 30
                  else
                    if vsExpanded in Node^.States then
                      index := 7
                    else
                      index := 6;
end;

procedure TfmMain.vstSubscribedGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var Text: string);
var
  data: PObject;
  ct, ct1: Integer;
  nm: string;
begin
  // Get the text for the subscribed group tree.  Return the account
  // name for level 1, or the group name for subsequent levels
  data := PObject(vstSubscribed.GetNodeData(node));

  if Assigned(data) and Assigned(data^) then
  begin
    if data^ is TNNTPAccount then
      text := TNNTPAccount(data^).AccountName
    else
      if data^ is TArticleContainer then
        with TArticleContainer(data^) do
        begin
          ct := UnreadArticleCount;
          ct1 := AdjustedArticleCount;

          if (data^ is TSubscribedGroup) and (TSubscribedGroup(data^).Nickname <> '') then
            nm := TSubscribedGroup(data^).Nickname
          else
            case XNOptions.TrimGroupNames of
              0: nm := Name;
              1: nm := FairlyShortGroupName(Name);
              2: nm := ShortGroupName(Name);
            end;

          if XNOptions.ShowMessageCount then
          begin
            if ct1 = 0 then
              text := nm
            else
              if (ct = 0) then
                text := nm + ' (' + IntToStr(ct1) + ')'
              else
                text := nm + ' (' + IntToStr(ct) + '/' + IntToStr(ct1) + ')';
          end
          else
            if ct = 0 then
              text := nm
            else
              text := nm + ' (' + IntToStr(ct) + ')';
        end
      else
        if data^ is TArticleFolders then
          text := rstArticleFolders
        else
          MessageBeep($FFFF);
  end
  else
    text := 'Loading...';
end;

procedure TfmMain.vstSubscribedInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  data: PObject;
begin
  data := PObject(vstSubscribed.GetNodeData(node));

  if not Assigned(data) or not Assigned(data^) then Exit;

  if data^ is TNNTPAccount then
    ChildCount := TNNTPAccount(data^).SubscribedGroupCount
  else
    if data^ is TArticleFolders then
      ChildCount := TArticleFolders(data^).Count;
end;

procedure TfmMain.vstSubscribedInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  data, parentData: PObject;
  account: TNNTPAccount;
  group: TSubscribedGroup;
  folders: TArticleFolders;
  folder: TArticleFolder;
  adjust: Integer;
begin
  data := PObject(vstSubscribed.GetNodeData(node));
  adjust := 1;
  if parentNode = nil then
  begin
    if node^.Index = 0 then
    begin
      data^ := gArticleFolders;
      if gArticleFolders.Count > 0 then
        InitialStates := InitialStates + [ivshasChildren];
    end
    else
    begin
      account := NNTPAccounts[Integer(node^.Index) - adjust];
      if Assigned(account) then
      begin
        data^ := account;

        if account.SubscribedGroupCount > 0 then
          InitialStates := InitialStates + [ivshasChildren];
        if not XNOptions.AutoExpandGroupTree and not XNOptions.AutoContractGroupTree then
        begin
          if account.DisplaySettings.Expanded then
            InitialStates := InitialStates + [ivsExpanded];
        end;
      end;
    end;
  end
  else
  begin
    parentData := PObject(vstSubscribed.GetNodeData(ParentNode));

    if parentData^ is TNNTPAccount then
    begin
      account := TNNTPAccount(parentData^);

      group := account.SubscribedGroups[Node^.Index];

      data^ := group;
    end
    else
      if parentData^ is TArticleFolders then
      begin
        folders := TArticleFolders(parentData^);
        folder := folders.Folder[Node^.Index];
        data^ := folder;
      end;
  end;
end;

procedure TfmMain.vstSubscribedKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    VK_RETURN:
      if XNOptions.EnterGetMessages then
        actNewsgroupGetMessages.Execute
      else
        vstArticles.SetFocus;
  end;
end;

procedure TfmMain.vstSubscribedNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  fldr: TArticleFolder;
begin
  fldr := GetNodeArticleFolder(node);
  if Assigned(fldr) then
  begin
    fldr.Name := NewText;
    Refresh_vstSubscribed;
  end;
end;

procedure TfmMain.vstSubscribedPaintText(Sender: TBaseVirtualTree;
  const Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  group: TSubscribedGroup;
  account: TNNTPAccount;
  i: Integer;
  bclr, clr: TColor;
begin
  group := GetNodeSubscribedGroup(node);
  bclr := Canvas.Brush.Color;
  if vsSelected in node.States then
    bclr := clHighlight;

  clr := Canvas.Font.Color;
  if Assigned(group) then
  begin
    if group.UnreadArticleCount > 0 then
    begin
      Canvas.Font.Style := XNOptions.UnreadNewsgroupsFontStyle;

      if not (vsSelected in node^.States) or XNOptions.HighlightSelectedText then
        if group.UnreadReplyCount > 0 then
          Canvas.Font.Color := XNOptions.Appearance[apReplies].FontColor
        else
          if group.UnreadArticleToMeCount > 0 then
            Canvas.Font.Color := XNOptions.Appearance[apMessagesToMe].FontColor;
    end;
    if gAudiblePerformanceCues then
      if group.Loaded then
        Canvas.Font.Color := clRed
      else
        Canvas.Font.Color := clBlack;
  end
  else
  begin
    account := GetNodeAccount(node);
    if Assigned(account) then
    begin
      for i := 0 to account.SubscribedGroupCount - 1 do
      begin
        group := account.SubscribedGroups[i];
        if group.UnreadArticleCount > 0 then
        begin
          Canvas.Font.Style := XNOptions.GroupsWithMessagesFontStyle;
          if (vsSelected in node^.States) and not XNOptions.HighlightSelectedText then
            Break;

          if group.UnreadReplyCount > 0 then
          begin
            Canvas.Font.Color := XNOptions.Appearance[apReplies].FontColor;
            Break
          end;

          if group.UnreadArticleToMeCount > 0 then
            Canvas.Font.Color := XNOptions.Appearance[apMessagesToMe].FontColor;
        end;
      end;
    end;
  end;
  if (vsSelected in Node.States) and XNOptions.HighlightSelectedText then
    Canvas.Font.Color := BoostContrast(Canvas.Font.Color, bclr, clr);
end;

procedure TfmMain.WmAutoExpand(var Msg: TMessage);
var
  node: PVirtualNode;
  fnode: TArticleBase;
begin
  if fInCollapse then Exit;
  node := vstArticles.FocusedNode;
  if Assigned(node) then
  begin
    vstArticles.BeginUpdate;
    try
      while node^.Parent <> vstArticles.RootNode do
        node := node^.Parent;

      // NOTE: I have received some bug reports (in earlier versions: r194, r269 mostly)
      //       on the following line:
      //         FullExpandThreads(GetNodeArticle(vstArticles.FocusedNode).Owner, node);
      //       that did indicate that the result of GetNodeArticle is nil.
      //       Looking at the code of GetNodeArticle the problem can't be about
      //       vstArticles.FocusedNode, it is more likely that fLastFocusedArticleContainer
      //       is in an invalid state and that the silent try...except is hiding
      //       the real problem.
      // TODO: In the past there were more reports that seem to pin-point to a
      //       problem regarding the state of fLastFocusedArticleContainer. This
      //       variable does require some cleanup/redesign.
      //       Based on the rare bug reports it is most-likely a multithreaded issue.
      // TODO: Remove "work-around" when the problem with fLastFocusedArticleContainer
      //       is solved.
      fnode := GetNodeArticle(vstArticles.FocusedNode);
      if Assigned(fnode) and Assigned(fnode.Owner) then
        FullExpandThreads(fnode.Owner, node);
    finally
      vstArticles.EndUpdate;
    end;
  end;
end;

procedure TfmMain.WmGroupsChanged(var Msg: TMessage);
begin
  Timer1.Enabled := True;
  fLastFocusedArticleContainer := nil;
  fPrevArticle := nil;
  fPrevArticleStack.Clear;
  fNextArticleStack.Clear;
  Reinit_vstSubscribed(False);
  vstSubscribed.ReinitNode(nil, True);
  vstSubscribed.EndUpdate;       // Started in 'WmGroupsChanging'
  Refresh_vstQueuedRequests;
end;

procedure TfmMain.WmGroupsChanging(var Msg: TMessage);
begin
  Timer1.Enabled := False;             // Prevent batches from starting.
  DisplayArticleBody(nil);

  fOutstandingRequestCount := 0;
  fOutstandingGetterCount := 0;
  fOutstandingActiveGetterCount := 0;
  vstQueuedRequests.RootNodeCount := 0;

  vstSubscribed.BeginUpdate;
end;

procedure TfmMain.WMSetup(var Msg: TMessage);
var
  Node: PVirtualNode;
  st, param: string;
  i: Integer;
begin
  GetCharsetNames(cbCharset.Items);
  cbCharSet.DropDownCount := (Screen.Height div 3) div cbCharSet.ItemHeight;
  Application.ProcessMessages;

  Reinit_vstSubscribed(False);
  try
    LoadRasEntries;
  except
  end;
  LoadUnpostedMessages;
  FreeAndNil(fmSplash);
  XNOptions.LoadKeyboardShortcuts;
  Application.ProcessMessages;

  param := '';
  for i := 1 to ParamCount do
  begin
    st := ParamStr(i);

    if (Length(st) > 0) and not (st[1] in ['/', '-']) then
    begin
      param := st;
      Break;
    end;
  end;

  if Param <> '' then
    GoToURL(Param)
  else
    if NNTPAccounts.Count = 0 then
      actToolsAccountsExecute(nil)
    else
    begin
      Node := GetAccountNode(NNTPAccounts.Items[0]);
      if Assigned(node) then
      begin
        vstSubscribed.FocusedNode := Node;
        vstSubscribed.Selected[Node] := True;
        if NNTPAccounts.fNewUserFlag then
        begin
          actAccountRefreshGroupListExecute(Self);
          vstSubscribed.Invalidate;
        end;
      end;
    end;

  FillBatchComboBox(0);

  fBookmarkSet := TBookmarkSet.Create;
  PopulateBookmarkCombo;
  if fBookmarkSet.BookmarkCount > 0 then
    SetCurrentBookmark(TBookmark.Create(fBookmarkSet.BookmarkName[0]), True);
  DisplayBookmarks(XNOptions.ShowBookmark);

  if XNOptions.AutoExpandGroupTree then
    vstSubscribed.FullExpand;
  PopulateSearchBarOpCombo;
end;

procedure TfmMain.WmUnsubscribe(var Msg: TMessage);
var
  I: Integer;
  list: TObjectList;
begin
  list := TObjectList(Msg.wParam);

  if Assigned(list) then
  begin
    for I := 0 to list.Count - 1 do
    begin
      if TSubscribedGroup(list[I]) = FLastFocusedArticleContainer then
      begin
        vstArticles.RootNodeCount := 0;
        DisplayArticleBody(nil);
        FLastFocusedArticleContainer := nil;
        Break;
      end;
    end;

    try
      for I := 0 to list.Count - 1 do
        Unsubscribe(TSubscribedGroup(list[I]), 0);
    finally
      NNTPAccounts.SaveToRegistry(nil);
    end;
  end;
  Refresh_vstSubscribed;
end;

var
  rmTaskbarCreated: DWord;

procedure TfmMain.WndProc(var Message: TMessage);
begin
  if rmTaskbarCreated = 0 then
    rmTaskbarCreated := RegisterWindowMessage('TaskbarCreated');

  // Monitor the message queue for the unique message we registered at
  // the beginning.  Part of the 'run once' functionality
  if Message.Msg = gUniqueMessage then
  begin
    actTrayOpenExecute(nil);
    Message.Result := $F00B00;
  end
  else if Message.Msg = rmTaskbarCreated then
  begin
    // Destroy and recreate trayicon when explorer restarts.
    try
      TrayIcon1.Free;
    except
      // it will fail because it no longer exists when the explorer restarts.
    end;
    TrayIcon1 := TTrayIcon.Create(Self);
    TrayIcon1.Visible := XNOptions.ShowInSystemTray;
    TrayIcon1.Hint := 'XanaNews v ' + ProductVersion;
    TrayIcon1.PopupMenu := pomTrayMenu;
    TrayIcon1.OnDblClick := TrayIcon1DblClick;
    fHadUnread := not fHadUnread;
  end
  else
    inherited;
end;

procedure TfmMain.WmQueryEndSession(var Msg: TMessage);
begin
  fEndSession := True;
  fCanClose := True;
  CheckSaveOutboxMessages;
  Msg.Result := 1;
end;

procedure TfmMain.BeginPurge(ctnr: TArticleContainer);
var
  article: TArticleBase;
begin
  fPurgingGroupName := '';
  fPurgingAccountName := '';
  fPurgingMessageID := '';
  if Assigned(ctnr) and (ctnr = fLastFocusedArticleContainer) then
  begin
    DisplayArticleBody(nil);
    fPurgingGroupName := ctnr.Name;
    fPurgingFolder := ctnr is TArticleFolder;

    fPurgingAccountName := '';
    if not fPurgingFolder then
      if ctnr is TArticleFolder then
        fPurgingAccountName := cFolders
      else
        if ctnr is TSubscribedGroup then
          fPurgingAccountName := TSubscribedGroup(ctnr).Owner.AccountName;

    article := GetNodeArticle(vstArticles.FocusedNode);
    if Assigned(article) then
      fPurgingMessageID := article.UniqueID;

    vstArticles.RootNodeCount := 0;
  end
end;

procedure TfmMain.EndPurge;
var
  article: TArticleBase;
  ctnr: TArticleContainer;
begin
  if fPurgingMessageID <> '' then
  begin
    fPrevArticleStack.Clear;
    fNextArticleStack.Clear;

    if fPurgingFolder then
      ctnr := gArticleFolders.FindFolder(fPurgingGroupName)
    else
      ctnr := NNTPAccounts.FindArticleContainer(fPurgingAccountName, fPurgingGroupName);
    if Assigned(ctnr) then
    begin
      fPrevArticle := nil;
      FocusArticleContainer(ctnr);
      article := ctnr.FindUniqueID(fPurgingMessageID);
      InitArticlesRootNodeCount(ctnr);
      Refresh_vstArticles;
      if Assigned(article) then
        GoToArticle(article);
    end;
  end;
  Refresh_vstSubscribed;
end;

function TfmMain.GetAccountNode(acct: TNNTPAccount): PVirtualNode;
var
  ac: TNNTPAccount;
begin
  if not Assigned(acct) then
    Result := nil
  else
  begin
    Result := vstSubscribed.GetFirst;
    repeat
      ac := GetNodeAccount(Result);
      if ac = acct then
        Exit;
      Result := vstSubscribed.GetNext(Result);
    until Result = nil;
  end;
end;

procedure TfmMain.actFolderReloadMessagesExecute(Sender: TObject);
begin
  fReloadedList.Clear;
  ForEachSelectedFolderArticle(DoReloadFolderArticle, 0);
  FixupReloadedGroups;
end;

function TfmMain.ForEachSelectedFolderArticle(
  proc: TFolderArticleIteratorProc; param: LPARAM): Integer;
var
  node: PVirtualNode;
  article: TArticleBase;
  all: Boolean;
begin
  all := Boolean(param);
  if all then
    node := vstArticles.GetFirst
  else
    node := vstArticles.GetFirstSelected;
  Result := 0;
  fIteratorFailed := False;
  while not fIteratorFailed and Assigned(node) do
  begin
    article := fLastFocusedArticleContainer.ArticleBase[node^.Index];
    if Assigned(article) and (article is TFolderArticle) then
    begin
      Inc(Result);
      proc(TFolderArticle(article), param);
    end;

    if all then
      node := vstArticles.GetNext(node)
    else
      node := vstArticles.GetNextSelected(node);
  end;
end;

procedure TfmMain.DoReloadFolderArticle(article: TFolderArticle;
  param: LPARAM);
var
  ctnr: TArticleContainer;
  grp: TSubscribedGroup;
  acct: string;
  art: TArticle;
  i: Integer;
  reloadGroup: Boolean;
begin
  acct := NNTPAccounts.GetServerAccountName(article.OrigServer, article.OrigGroup);

  ctnr := NNTPAccounts.FindArticleContainer(acct, article.OrigGroup);
  if Assigned(ctnr) and (ctnr is TSubscribedGroup) then
    grp := TSubscribedGroup(ctnr)
  else
    grp := nil;

  if Assigned(grp) then
  begin
    grp.LoadArticles;
    reloadGroup := False;
    for i := 0 to fReloadedList.Count - 1 do
      if fReloadedList[i] = grp then
      begin
        reloadGroup := True;
        Break;
      end;

    if not reloadGroup then
      fReloadedList.Add(grp);

    if not Assigned(fFolderArticleHeader) then
      fFolderArticleheader := TAnsiStringList.Create
    else
      fFolderArticleHeader.Clear;

    fFolderArticleHeader.Add('Message-ID:' + article.RawMessageId);
    fFolderArticleHeader.Add('Subject:' + article.RawSubject);
    fFolderArticleHeader.Add('From:' + article.RawFrom);
    fFolderArticleHeader.Add('Date:' + RawByteString(SafeDateTimeToInternetStr(article.Date, True)));
    fFolderArticleHeader.Add('Lines:' + RawIntToStr(article.Lines));
    if article.References <> '' then
      fFolderArticleHeader.Add('References:' + article.RawReferences);

    fFolderArticleHeader.AddAnsiStrings(article.Msg.Header);

    art := grp.AddArticle(article.ArticleNo, fFolderArticleHeader, article.Msg.RawData, False);
    if Assigned(art) then
      art.IsRead := True;
  end;
end;

procedure TfmMain.FocusArticleContainer(ctnr: TArticleContainer);
var
  grp: TSubscribedGroup;
  sortCol: Integer;
  article: TArticleBase;
  gm: Boolean;
  oldAcct: TNNTPAccount;
  n: PVirtualNode;
begin
  oldAcct := fLastFocusedAccount;
  gm := fAutoGetMessages;
  fAutoGetMessages := False;
//  fSearchMessageForm.Free;
  if Assigned(fPrevArticle) then
  begin
    fPrevArticleStack.Push(fPrevArticle);
    fPrevArticle := nil;
  end;

  DisplayArticleBody(nil);

  if Assigned(fLastFocusedArticleContainer) then
  begin
    if fLastFocusedArticleContainer is TSubscribedGroup then
    begin
      grp := TSubscribedGroup(fLastFocusedArticleContainer);
      oldAcct := grp.Owner;
      if (not fDontMarkOnLeave) and oldAcct.MarkOnLeave then
        actNewsgroupMarkAllMessagesAsRead.Execute;
    end;

    article := GetFocusedArticle;
    if Assigned(article) then
      fLastFocusedArticleContainer.CursorArticleID := article.UniqueID
    else
      fLastFocusedArticleContainer.CursorArticleID := '';

    vstArticles.Clear;
    fLastFocusedArticleContainer.LeaveGroup;
  end
  else
    vstArticles.Clear;

  fLastFocusedArticleContainer := ctnr;
  if Assigned(ctnr) then
  begin
    ctnr.fFocused := True;
    ctnr.HideReadMessages := XNOptions.HideReadMessages;
    ctnr.HideMessagesNotToMe := False;
    ctnr.HideIgnoredMessages := XNOptions.HideIgnoredMessages;
    InitArticlesRootNodeCount(ctnr);

    case ctnr.ThreadSortOrder of
      soMessageNo    : sortCol := 1;
      soSubject      : sortCol := 2;
      soPostingHost  : sortCol := 2;
      soAuthor       : sortCol := 3;
      soDate         : sortCol := 4;
      soLines        : sortCol := 5;
      soNewestMessage: sortCol := 6;
    else
      sortCol := -1;
    end;

    fHeaderSortCol := sortCol;
    if sortCol = 6 then
      sortCol := 4;

    vstArticles.Header.SortColumn := sortCol;

    if ctnr.ThreadSortDirection = sdAscending then
      vstArticles.Header.SortDirection := VirtualTrees.sdAscending
    else
      vstArticles.Header.SortDirection := VirtualTrees.sdDescending;
  end
  else
    vstArticles.RootNodeCount := 0;

  vstArticles.ClearSelection;

  if Assigned(ctnr) then
  begin
    if XNOptions.AutoExpandAll then
      FullExpandThreads(ctnr, nil);

    if ctnr.CursorArticleID <> '' then
      article := ctnr.FindUniqueID(ctnr.CursorArticleID)
    else
      article := nil;

    if Assigned(article) then
      GoToArticle(article)
    else
    begin
      if ctnr is TSubscribedGroup then
      begin
        article := ctnr.FirstArticle;
        if Assigned(article) and (article is TArticle) and TArticle(article).IsRead and (ctnr.UnreadArticleCount <> 0) then
          NextArticle([naUnreadOnly], TArticle(article))
        else
          GoToArticle(article);

        if gm then
          case TSubscribedGroup(ctnr).NNTPSettings.PerformDefaultAction of
            paAlways : actNewsgroupGetMessagesDefault.Execute;
            paSession: if not TSubscribedGroup(ctnr).ActionPerformedThisSession then
                          actNewsgroupGetMessagesDefault.Execute;
          end;
      end
      else
        GoToArticle(ctnr.ArticleBase[0]);
    end;
  end
  else
    pnlDetailsBar.Caption := '';

  fPrevArticle := nil;

  fLastFocusedAccount := GetFocusedAccount;

  if XNOptions.AutoContractGroupTree and (oldAcct <> nil) then
  begin
    if fLastFocusedAccount <> oldAcct then
    begin
      n := GetAccountNode(oldAcct);
      if Assigned(n) then
        vstSubscribed.FullCollapse(n);
    end;
  end;
end;

function TfmMain.GetNodeArticleContainer(
  node: PVirtualNode): TArticleContainer;
var
  data: PObject;
begin
  data := PObject(vstSubscribed.GetNodeData(node));
  if Assigned(data) and Assigned(data^) and (data^ is TArticleContainer) then
    Result := TArticleContainer(data^)
  else
    Result := nil;
end;

procedure TfmMain.actFolderReloadAllMessagesExecute(Sender: TObject);
begin
  fReloadedList.Clear;
  ForEachSelectedFolderArticle(DoReloadFolderArticle, 1);
  FixupReloadedGroups;
end;

procedure TfmMain.FixupReloadedGroups;
var
  i: Integer;
  grp: TSubscribedGroup;
begin
  for i := 0 to fReloadedList.Count - 1 do
  begin
    grp := TSubscribedGroup(fReloadedList[i]);
    if grp <> GetFocusedGroup then
    begin
      if grp.Loaded then
        grp.SaveArticles(False);
      grp.ResetSortFlags;
    end
    else
    begin
      grp.LeaveGroup(False);
      fPrevArticle := nil;
      FocusArticleContainer(grp);
    end;
  end;
end;

procedure TfmMain.DoOnNewGroups(Sender: TObject; account: TNNTPAccount);
begin
  account.HasNewGroups := True;
  Refresh_vstSubscribed;
end;

procedure TfmMain.ShowNewsgroupList(account: TNNTPAccount; newGroupsOnly: Boolean);
var
  dlg: TdlgNewsgroups;
begin
  if Assigned(account) then
  begin
    if ThreadManager.GettingNewsgroupList(account) then
    begin
      ShowMessage('XanaNews is currently retrieving the list of newsgroups for this account.  Please wait until it finishes');
      Exit;
    end;
    if (not FileExists(gMessageBaseRoot + '\' + FixFileNameString(account.AccountName) + '\newsgroups.dat')) then
    begin
      if MessageDlg(Format(rstIsNewAccount, [account.AccountName]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        actAccountRefreshGroupList.Execute;
      Exit;
    end;
    Application.CreateForm(TdlgNewsgroups, dlg);
    try
      dlg.cbNewGroupsOnly.Checked := newGroupsOnly;
      dlg.Account := account;
      dlg.ShowModal;

                        // The dialog may have subscribed or unsubscribed us to groups -
                        // including the currently selected one - so refresh *everything*

      Refresh_vstSubscribed;
    finally
      dlg.Free;
    end;
  end;
end;


procedure TfmMain.actQRClearExecute(Sender: TObject);
begin
  vstQueuedRequests.SelectAll(False);
  actQRDeleteExecute(nil);
end;

procedure TfmMain.actROT13Execute(Sender: TObject);
var
  st: string;
begin
  if MessageScrollBox1.GetSelectedText(st) then
    MessageScrollBox1.SetSelectedText(WideROT13(st));
end;

procedure TfmMain.WmCopyData(var msg: TwmCopYData);
var
  url: string;
begin
  if msg.CopyDataStruct.dwData = 1 then
  begin
    url := PChar(msg.CopyDataStruct.lpData);
    GotoURL(url);
  end;
end;

procedure TfmMain.WmFirstTime(var Msg: TMessage);
begin
  ResizeSearchBar;
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.GoToURL                                            |
 |                                                                      |
 | Go to a URL - which will either have been passed as a command line   |
 | parameter, or passed from another instance of XanaNews in a          |
 | WM_COPYDATA chunk.                                                   |
 |                                                                      |
 | Here's what the (draft) spec says (draft-gilman-news-url-02)         |
                                                                        |
 | Following the syntax conventions of [RFC URL SYNTAX], a news         |
 | URL has the form:                                                    |
 |                                                                      |
 | newsURL      =  scheme ":" [ news-server ] [ refbygroup | message ]  |
 | scheme       =  "news" | "nntp"                                      |
 | news-server  =  "//" server "/"                                      |
 | refbygroup   = group [ "/" messageno [ "-" messageno ] ]             |
 | message      = local-part "@" domain                                 |
 |                                                                      |
 | Messages take the form specified for the value of a Message-ID field |
 | in RFC 822 [1] or RFC 1036 [2], without the leading "<" or trailing  |
 | ">".                                                                 |
 |                                                                      |
 | Here are my extensions to the draft.                                 |
 |                                                                      |
 | 1.  We don't care what the 'scheme' is - or even if it's ommitted.   |
 | 2.  message = [group "/"] local-part "@" domain                      |
 | 3.  We don't care if the Message-ID has the leading "<" or trailing  |
 |     ">".                                                             |
 | 4.  The server in news-server can be server:port-no                  |
 |                                                                      |
 | Parameters:                                                          |
 |   url: string                The URL to go to.                       |
 *----------------------------------------------------------------------*)
procedure TfmMain.GoToURL(url: string);
var
  scheme, server, group: string;
  port: string;
  p, portNo, sslPort: Integer;
  articleNo: Int64;
  isMessageID: Boolean;
  article: TArticleBase;
  account: TNNTPAccount;
  grp: TSubscribedGroup;
  groupsChanged, gss: Boolean;
  node: PVirtualNode;
begin
  grp := nil;
  p := Pos('//', url);         // Extract the scheme if there's a '//' with a ':' before it.
  if (p = 0) or (Pos(':', url) < p) then
    scheme := ExtractString(':', url);

                                // If there's a '//' then get the server.
  if Copy(url, 1, 2) = '//' then
  begin
    url := Copy(url, 3, MaxInt);
    server := SplitString('/', url)
  end
  else
    server := '';

  gss := False;

                                // If the url contains '@ then it's a message ID
  isMessageID := Pos('@', url) > 0;

  if not isMessageID then       // The url is groupname/message no
    group := SplitString('/', url)
  else
                                // Allow group/message ID.  It's not in the spec
                                // but it could be useful.
    group := ExtractString('/', url);

  groupsChanged := False;
  try
    account := nil;
    if server <> '' then
    begin
                                // The server could be servername:port
      port := ExtractString(':', server);
      portNo := StrToIntDef(port, 119);
      if SameText(scheme, 'snews') then
        sslPort := 563
      else
        sslPort := -1;

                                // See if we've already got an account created
                                // for the server/port.
      account := NNTPAccounts.FindServerAccount(server, portNo, sslPort);

      if account = nil then     // Not an existing account.  Create one.
      begin
        account := TNNTPAccount.Create(NNTPAccounts);
        account.NNTPServerSettings.ServerName := server;
        if port = '' then
          account.AccountName := server
        else
          account.AccountName := server + ':' + port;
        account.NNTPServerSettings.ServerPort := portNo;
        SendMessage(Handle, WM_GROUPSCHANGING, 0, 0);
        gss := True;
        groupsChanged := True;
        NNTPAccounts.Add(account);
        NNTPAccounts.SaveToRegistry;
      end;
    end;

    if Assigned(account) then
    begin
                                // Find the group in the account's subscribed groups
                                //  if it was specified
      grp := account.FindSubscribedGroup(group);
      if (not Assigned(grp)) and (group <> '') then
      begin
                                // Group not found.  Subscribe to the group
        grp := account.SubscribeTo(group);
        if not groupsChanged then
        begin
          SendMessage(Handle, WM_GROUPSCHANGING, 0, 0);
          gss := True;
        end;
      end;
    end
    else                        // Do a global search for the group if the account
                                // wasn't specified.
      if group <> '' then
        grp := NNTPAccounts.FindArticleContainer('', group) as TSubscribedGroup;

  finally
    if gss then       // We created a new account or subscribed to a new group
                                // Update the GUI
      SendMessage(Handle, WM_GROUPSCHANGED, 0, 0);
  end;

  if Assigned(account) then
    server := account.AccountName
  else
    server := '';

  article := nil;
  articleNo := 0;
  if isMessageID then           // Message ID specified.  The spec says it won't
  begin                         // have the enclosing '<''>'.  Wanna bet!
    if Copy(url, 1, 1) <> '<' then
      url := '<' + url + '>';

    article := NNTPAccounts.FindMsgID(server, group, RawByteSTring(url));
  end
  else                          // Find the article no in the specified group
    if Assigned(grp) then
    begin
      articleNo := StrToInt64Def(url, 0);
      article := grp.FindArticleNo(articleNo);
    end;

  if Assigned(article) then
    GoToArticle(article)        // We found the article!!!  Jump to it.
  else
    if Assigned(grp) then       // We didn't find the article, but at least we
    begin                       // found the group.  Go to it.
      fPrevArticle := nil;
      FocusArticleContainer(grp);
      SyncContainerTree(grp);

      GetSingleArticle(grp, articleNo);
    end                         // We didn't find the article or group.  Maybe we
    else                        // found the account.
      if Assigned(account) then
      begin                     // Go to the account.
        node := GetAccountNode(account);
        vstSubscribed.ClearSelection;
        if Assigned(node) then
        begin
          vstSubscribed.FocusedNode := node;
          vstSubscribed.Selected[node] := True;
        end;
      end;
end;

procedure TfmMain.actToolsIdentitiesExecute(Sender: TObject);
var
  dlg: TdlgIdentities;
begin
  dlg := TdlgIdentities.Create(nil);
  try
    dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;

procedure TfmMain.actViewHeadersCustomExecute(Sender: TObject);
begin
  XNOptions.ShowHeader := shCustom;
  MessageScrollBox1.ShowHeader := shCustom;
end;

procedure TfmMain.cbCharsetChange(Sender: TObject);
var
  codePage: Integer;
  article: TArticleBase;
begin
  article := GetFocusedArticle;
  if Assigned(article) then
  begin
    codePage := CharsetNameToCodePage(cbCharset.Text);
    article.CodePage := codePage;
    vstArticles.Invalidate;
    MessageScrollBox1.Msg := nil;
    MessageScrollBox1.Msg := article.Msg;
  end;
end;

procedure TfmMain.cbSearchBarTargetChange(Sender: TObject);
begin
  PopulateSearchBarOpCombo;
end;

procedure MessageDelay(milliseconds: DWORD);
var
  start: DWORD;
  left: Integer;
  handle: DWORD;
begin
  handle := 0;
  start := GetTickCount;
  repeat
    left := Start + MilliSeconds - GetTickCount;
    if left > 0 then
    begin
      MsgWaitForMultipleObjects(0, handle, True, left, QS_ALLEVENTS);
      Application.ProcessMessages;
    end;
  until left <= 0;
end;

procedure TfmMain.actToolsFlickerTestExecute(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(MessageScrollBox1.Msg) then
  begin
    Windows.Beep(880, 10);
    for i := 1 to 10 do
    begin
      MessageScrollBox1.Refresh(False, True);
      MessageDelay(500);
    end;
    Windows.Beep(440, 10);
  end;
end;

procedure TfmMain.actEditCopyLinkExecute(Sender: TObject);
var
  article: TArticleBase;
begin
  article := GetFocusedArticle;

  if Assigned(article) and (article.ArticleNo <> 0) then
    Clipboard.AsText := 'nntp://' + article.Owner.ServerSettings.ServerName + '/' + article.Owner.Name + '/' + IntToStr(article.ArticleNo);
end;

procedure TfmMain.actEditSelectThreadExecute(Sender: TObject);
var
  art: TArticleBase;
begin
  art := GetFocusedArticle;
  if Assigned(art) then
  begin
    while Assigned(art.Parent) do
      art := art.Parent;

    SelectBranch(GetArticleNode(art));
  end
end;

procedure TfmMain.actEditSelectSubthreadExecute(Sender: TObject);
var
  art: TArticleBase;
begin
  art := GetFocusedArticle;
  SelectBranch(GetArticleNode(art));
end;

procedure TfmMain.SelectBranch(node: PVirtualNode);
begin
  if Assigned(node) then
  begin
    vstArticles.Selected[node] := True;
    node := node.FirstChild;
    while Assigned(node) do
    begin
      SelectBranch(node);
      node := node^.NextSibling;
    end;
  end;
end;

procedure TfmMain.actFileExportSelectedExecute(Sender: TObject);
var
  folder: TArticleFolder;
  f: TArticleContainer;
  st: string;
begin
  if vstArticles.GetFirstSelected <> nil then
  begin
    f := fLastFocusedArticleContainer;

    if Assigned(f) then
    begin
      st := f.Name + ' ' + DateToStr(Now);
      st := StringReplace(st, '/', '-', [rfReplaceAll]);
      st := StringReplace(st, '\', '-', [rfReplaceAll]);
      st := StringReplace(st, '.', '-', [rfReplaceAll]);
      st := st + '.txt';
      dlgSaveArticle.FileName := st;
    end;

    if dlgSaveArticle.Execute then
    begin
      folder := TArticleFolder.CreateFile(dlgSaveArticle.FileName, True, True);
      try
        folder.BeginAdd;
        try
          ForEachSelectedArticle(DoSaveArticle, LPARAM(folder))
        finally
          folder.EndAdd;
        end
      finally
        folder.Free;
      end;
    end;
  end;
end;

procedure TfmMain.DoSaveArticle(article: TArticleBase; param: LPARAM; multiSelect: Boolean);
var
  folder: TArticleFolder;
begin
  folder := TArticleFolder(param);
  folder.AddArticle(article);
end;

procedure TfmMain.actFileImportArticlesExecute(Sender: TObject);
var
  folder: TArticleFolder;
  article: TFolderArticle;
begin
  if dlgImportArticles.Execute then
  begin
    folder := TArticleFolder.CreateFile(dlgImportArticles.FileName, False, True);
    try
      fReloadedList.Clear;
      repeat
        article := folder.SequentialReadArticle;
        if Assigned(article) then
          DoReloadFolderArticle(article, 1)
      until not Assigned(article);
      FixupReloadedGroups;
    finally
      folder.Free;
    end;
  end;
end;

procedure TfmMain.actFilePrinterSetupExecute(Sender: TObject);
begin
  PrinterSetupDialog1.Execute
end;

procedure TfmMain.actFilePrintExecute(Sender: TObject);
begin
  if GetFocusedArticle <> nil then
    if PrintDialog1.Execute then
    begin
      printer.Copies := PrintDialog1.Copies;
      MessageScrollBox1.Print;
    end;
end;

procedure TfmMain.FindDialog1Close(Sender: TObject);
begin
  fFindDialogShowing := False;
  EnableShortcuts(True);
end;

procedure TfmMain.actViewMessagesImagesOnlyExecute(Sender: TObject);
begin
  XNOptions.ViewMode := vmImages;
  MessageScrollBox1.RawMessage := False;
  MessageScrollBox1.RawMode := False;
  MessageScrollBox1.ImagesOnly := True;
  MessageScrollBox1.Refresh(True, True);
end;

procedure TfmMain.WmStatusMessage(var Msg: TMessage);
begin
  fSM := PChar(Msg.WParam);
  if fSM <> '' then
  begin
    if Timer1.Enabled then                      // We've started a lengthy operation
      Application.ProcessMessages;              // so make sure the UI is refreshed.
    Timer1.Enabled := False;
    Screen.Cursor := crHourGlass;
    StatusBar.Panels[0].Text := fSM;
    StatusBar.Repaint;

    fSmPos := LOWORD(Msg.LParam);
    fSmMax := HIWORD(Msg.LParam);
  end
  else
  begin
    Screen.Cursor := crDefault;
    Timer1.Enabled := True;
    fSMMax := 0;
    fSMPos := 0;
  end;
  UpdateStatusBar(fSM, 0, fSmMax, fSmPos);
end;

procedure TfmMain.WMSysCommand(var Message: TWMSysCommand);
begin
  inherited;
end;

procedure TfmMain.actViewGroupMultipartExecute(Sender: TObject);
var
  art: TArticleBase;
  id: RawByteString;
  group: TSubscribedGroup;
  oldCursor: TCursor;
begin
  group := GetFocusedGroup;

  if Assigned(group) then
  begin
    art := GetFocusedArticle;
    if Assigned(art) and (art.ArticleNo > 0) then
      id := art.UniqueID;

    oldCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourglass;
      group.GroupMultipartMessages := True;
      vstArticles.Header.SortColumn := 2;
      vstArticles.Header.SortDirection := VirtualTrees.sdAscending;
    finally
      Screen.Cursor := oldCursor;
    end;

    InitArticlesRootNodeCount(group);
    Refresh_vstArticles;
    GoToArticle(group.FindUniqueID(id));
  end;
end;

procedure TfmMain.actToolsMailAccountsExecute(Sender: TObject);
var
  dlg: TdlgMailAccounts;
begin
  dlg := TdlgMailAccounts.Create(nil);
  try
    dlg.ShowModal;
    MailAccounts.SaveToRegistry;
  finally
    dlg.Free;
  end;
end;

procedure TfmMain.Reinit_vstSubscribed(refresh: Boolean = True);
var
  adjust: Integer;
begin
  adjust := 1;
  vstSubscribed.RootNodeCount := NNTPAccounts.Count + adjust;

  if Refresh then
    Refresh_vstSubscribed;
end;

function TfmMain.GetNodeObject(node: PVirtualNode): TObject;
var
  data: PObject;
begin
  Result := nil;
  if Assigned(node) then
  begin
    data := PObject(vstSubscribed.GetNodeData(node));

    if Assigned(data) and Assigned(data^) then
      Result := data^;
  end;
end;

procedure TfmMain.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  fPanelTextRect := Rect;
  InflateRect(fPanelTextRect, -4, 0);
  OffsetRect(fPanelTextRect, 0, 2);
  StatusBar.Canvas.Font.Color := clBlue;
  StatusBar.Canvas.Font.Style := StatusBar.Canvas.Font.Style + [fsUnderline];
  DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, fPanelTextRect, DT_LEFT or DT_VCENTER or DT_CALCRECT);
  DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, fPanelTextRect, DT_LEFT or DT_VCENTER);
end;

procedure TfmMain.StatusBarMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  pnlRect: TRect;
begin
  pnlRect := ClientRect;
  pnlRect.Left := pnlRect.Right - StatusBar.Panels[StatusBar.Panels.Count - 1].Width - 12;
  if PtInRect(fPanelTextRect, Point(x, y)) then
  begin
    if not fCaptureUpdatePanel then
    begin
      SetCapture(StatusBar.Handle);
      fCaptureUpdatePanel := True;
      Screen.Cursor := crHandPoint;
    end;
  end
  else
  begin
    if fCaptureUpdatePanel then
    begin
      ReleaseCapture;
      fCaptureUpdatePanel := False;
      Screen.Cursor := crArrow;
    end;
  end;
end;

procedure TfmMain.StatusBarClick(Sender: TObject);
begin
  if fCaptureUpdatePanel then
    ShellExecute(handle, 'open', 'http://xananews.techtips.com.br', nil, nil, SW_SHOW);
end;

procedure TfmMain.actToolsToggleLoggingExecute(Sender: TObject);
begin
  LogMessage('Logging disabled');
  gLogFlag := not gLogFlag;
  LogMessage('Logging enabled');
end;

procedure TfmMain.mnuToolsClick(Sender: TObject);
begin
  actToolsToggleLogging.Checked := gLogFlag;
  actToolsAudiblePerformanceCues.Checked := gAudiblePerformanceCues;
end;

procedure TfmMain.mnuViewClick(Sender: TObject);
begin
  if Assigned(fLastFocusedArticleContainer) then
  begin
    actViewHideReadMessages.Enabled := True;
    actViewHideReadMessages.Checked := fLastFocusedArticleContainer.HideReadMessages;
    actViewHideIgnoredMessages.Enabled := True;
    actViewHideIgnoredMessages.Checked := fLastFocusedArticleContainer.HideIgnoredMessages;
    actViewHideMessagesNotToMe.Enabled := True;
    actViewHideMessagesNotToMe.Checked := fLastFocusedArticleContainer.HideMessagesNotToMe;
  end
  else
  begin
    actViewHideReadMessages.Enabled := False;
    actViewHideReadMessages.Checked := XNOptions.HideReadMessages;
    actViewHideIgnoredMessages.Enabled := False;
    actViewHideIgnoredMessages.Checked := XNOptions.HideIgnoredMessages;
    actViewHideMessagesNotToMe.Enabled := False;
    actViewHideMessagesNotToMe.Checked := False;
  end;

  actViewShowBookmarkPane.Checked := pnlBookmark.Visible;
  actViewSubscribedGroupsPane.Checked := pnlLeft.Visible;
end;

procedure TfmMain.actViewHideReadMessagesExecute(Sender: TObject);
var
  art: TArticleBase;
  artno: Int64;
begin
  if Assigned(fLastFocusedArticleContainer) then
  begin
    art := GetFocusedArticle;
    if Assigned(art) then
      artno := art.ArticleNo
    else
      artNo := -1;

    fLastFocusedArticleContainer.HideReadMessages := not fLastFocusedArticleContainer.HideReadMessages;
    InitArticlesRootNodeCount(fLastFocusedArticleContainer);
    Refresh_vstArticles;

    if artNo <> -1 then
      GoToArticle(fLastFocusedArticleContainer.FindArticleNo(artNo));
  end;
end;

procedure TfmMain.actToolsAudiblePerformanceCuesExecute(Sender: TObject);
begin
  gAudiblePerformanceCues := not gAudiblePerformanceCues;
end;


function TfmMain.IsThreaded(ctnr: TArticleContainer): Boolean;
begin
  Result := (ctnr.ThreadOrder = toThreaded) or (ctnr.ThreadSortOrder = soSubject);
  Result := Result and not (ctnr is TArticleFolder);
end;

procedure TfmMain.FullExpandThreads(ctnr: TArticleContainer; node: PVirtualNode);
begin
  if IsThreaded(ctnr) then
    vstArticles.FullExpand(node);
end;

procedure TfmMain.PopulateSearchBarOpCombo;
var
  oldstrOp, strOp: Boolean;
begin
  oldStrOp := cbSearchBarOp.Items.Count = 2;
  strOp := True;
  case cbSearchBarTarget.ItemIndex of
    0: ; // Message
    1: ; // Subject
    2: ; // Author
    3: strOp := False; // Date
    4: strOp := False; // Lines
  end;

  if strOp <> oldStrOp then
  begin
    cbSearchBarOp.Items.BeginUpdate;
    try
      cbSearchBarOp.Items.Clear;
      if strOp then
      begin
        cbSearchBarOp.Items.Add('contains');
        cbSearchBarOp.Items.Add('Doesn''t contain');
      end
      else
      begin
        cbSearchBarOp.Items.Add('is less than');
        cbSearchBarOp.Items.Add('is greater or equal');
        cbSearchBarOp.Items.Add('is equal');
        cbSearchBarOp.Items.Add('is not equal');
      end;
      cbSearchBarOp.ItemIndex := 0;
    finally
      cbSearchBarOp.Items.EndUpdate;
    end;
  end;
end;

procedure TfmMain.actNewsgroupSaveAllAttachmentsExecute(Sender: TObject);
var
  root, dir: string;
begin
  dir := GetAttachmentsDirectory;
  root := '';
  if Copy(dir, Length(dir), 1) = '\' then
    Delete(dir, Length(dir), 1);
  if SelectDirectory('Save Files', root, dir) then
  begin
    dir := dir + '\';
    ForEachSelectedArticle(DoSaveAttachments, LPARAM(PChar(dir)));
    SetTempStatusMessage('', 0, 0);
    SetAttachmentsDirectory(dir + '*.*');
  end;
end;

procedure TfmMain.SaveAttachment(mp: TmvMessagePart; const fileName: string; multipart: Boolean);
var
  gr: TGraphic;
  articles: TList;
  art: TArticleBase;
  fs: TStream;
begin
  // GetMultipartData. Decode 'split' messages.
  // Combine all the raw data into a single message, then logically the
  // message part we want will be the *last* multipart.
  if mp.HasRawData or multipart then
  begin
    if multipart then
    begin
      articles := TList.Create;
      try
        art := TArticleBase(mp.Owner.Obj);
        articles.Add(art);

        art := art.Child;
        while Assigned(art) do
        begin
          articles.Add(art);
          art := art.Sibling;
        end;
        SaveMultipartAttachment(fileName, articles);
      finally
        articles.Free
      end;
    end
    else
    begin
      fs := TFileStream.Create(fileName, fmCreate or fmShareExclusive);
      try
        mp.GetData(fs);
      finally
        fs.Free;
      end;
    end;
  end
  else
  begin
    Timer1.Enabled := False;
    try
      gr := mp.Graphic;
      if Assigned(gr) then
        gr.SaveToFile(FileName);
    finally
      Timer1.Enabled := True;
    end;
  end;
end;

procedure TfmMain.DoSaveAttachments(article: TArticleBase; param: LPARAM;
  multiSelect: Boolean);
var
  mp: TmvMessagePart;
  i: Integer;
  dir: string;
  rootsOnly: Boolean;
begin
  if not Assigned(Article) or not Assigned(Article.Msg) then
    Exit;
  rootsOnly := (article.Owner.ThreadOrder = toChronological) and (article.Owner.ThreadSortOrder = soSubject);

  if rootsOnly and (Article.Parent <> nil) then
    Exit;

  dir := PChar(param);
  for i := 0 to article.Msg.MessageParts.Count - 1 do
  begin
    mp := article.Msg.MessageParts.Items[i];
    if not Assigned(mp.Body) and Assigned(mp.Graphic) then
      if article.MultipartFlags <> mfPartialMultipart then
      try
        SetTempStatusMessage('Saving attachment ' + mp.FileName, 0, 0);
        SaveAttachment(mp, dir + mp.FileName, IsInMultipartMode);
      except
      end;
  end;
end;

procedure TfmMain.edSearchBarTextEnter(Sender: TObject);
begin
  fInSearchbarText := True;
  EnableShortcuts(False);
end;

procedure TfmMain.edSearchBarTextExit(Sender: TObject);
begin
  EnableShortcuts(True);
  fInSearchbarText := False;
end;

procedure TfmMain.edSearchBarTextKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    btnGo.Click;
    Key := #0;
  end;
end;

procedure TfmMain.actFileExportCompressedExecute(Sender: TObject);
var
  exporter: TXanaExporter;
//  art: TArticleBase;
  dlg: TExportDialog;
begin
  dlg := TExportDialog.Create(nil);
  try
    dlg.Filter := 'XanaNews Compressed Files (*.xns)|*.xns';
    dlg.DefaultExt := 'XNS';
    if dlg.Execute then
    begin
      exporter := TXanaExporter.Create;
      try
        StopEverything;
        try
          exporter.OnProgress := DoOnExporterProgress;
//          art := GetFocusedArticle;
//          if Assigned(art) then
//          begin
//            art.Owner.LeaveGroup(False);
//            fLastFocusedArticleContainer := nil;
//            fPrevArticle := nil;
//          end;
          exporter.Export(dlg.FileName, dlg.Groups, dlg.ExportSettings);
//          GoToArticle(art)
          XNOptions.Reload;
          ApplyControlOptions;
        finally
          StartEverything;
        end;
      finally
        exporter.Free;
        SetTempStatusMessage('', 0, 0);
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfmMain.actFileImportCompressedExecute(Sender: TObject);
var
  exporter: TXanaExporter;
begin
  if dlgImportCompressed.Execute then
  begin
    exporter := TXanaExporter.Create;
    try
      StopEverything;
      try
        exporter.Import(dlgImportCompressed.FileName);
        XNOptions.Reload;
        ApplyControlOptions;
      finally
        StartEverything;
      end;
    finally
      exporter.Free;
    end;
  end;
end;

procedure TfmMain.DoOnExporterProgress(Sender: TObject; pos, max: Integer;
  const group: string);
var
  st: string;
begin
  if group = '' then
    st := 'Exporting Settings'
  else
    st := 'Exporting ' + group;

  SetTempStatusMessage(st, pos, max);
end;

procedure TfmMain.UpdateStatusBar(const msg: string; prMin, prMax,
  prPos: Integer);
begin
  StatusBar.Panels[0].Text := msg;

  if prMax < prMin then
    prMax := prMin;

  if prPos > prMax then
    prPos := prMax;

  if prPos < prMin then
    prPos := prMin;

  if prMax > prMin then
  begin
    ProgressBar1.Min := prMin;
    ProgressBar1.Max := prMax;
    ProgressBar1.Position := prPos;
    ProgressBar1.Visible := True;
  end
  else
  begin
    ProgressBar1.Visible := False;
    ProgressBar1.Min := 0;
    ProgressBar1.Position := 0;
    ProgressBar1.Max := 0;
  end;

//  StatusBar.Repaint
end;

procedure TfmMain.SyncContainerTree(ctnr: TArticleContainer);
var
  node: PVirtualNode;
begin
  node := GetArticleContainerNode(ctnr);
  vstSubscribed.ClearSelection;
  if Assigned(node) then
  begin
    vstSubscribed.FocusedNode := node;
    vstSubscribed.Selected[node] := True;
  end;
end;

procedure TfmMain.actArticleChangeSubjectExecute(Sender: TObject);
var
  art: TArticleBase;
  subj: string;
begin
  art := GetFocusedArticle;
  subj := art.Subject;

  if InputQuery('XanaNews', 'Subject', subj) then
    art.Subject := subj;
end;

procedure TfmMain.FillBatchComboBox(selectBatch: Integer);
var
  i: Integer;
begin
  if selectBatch = -1 then
    selectBatch := cbBatches.ItemIndex;

  cbBatches.Clear;
  cbBatches.Items.BeginUpdate;
  try
    for i := 0 to NNTPAccounts.BatchesCount - 1 do
      cbBatches.Items.Add(NNTPAccounts.Batches[i].BatchName);
  finally
    cbBatches.Items.EndUpdate;
  end;

  if selectBatch >= cbBatches.Items.Count then
    selectBatch := cbBatches.Items.Count - 1;

  if selectBatch >= 0 then
    cbBatches.ItemIndex := selectBatch;

  cbBatches.DropDownCount := (Screen.Height div 3) div cbBatches.ItemHeight;
end;

procedure TfmMain.actToolsTestCrashExecute(Sender: TObject);
begin
  if Windows.MessageBox(Handle, 'Do you want XanaNews to crash?', 'Crash XanaNews', MB_YESNO or MB_DEFBUTTON2 or MB_ICONSTOP) = IDYES then
  begin
    fLastFocusedArticleContainer := nil;
    MessageBeep(fLastFocusedArticleContainer.FirstArticle.CodePage);
  end;
end;

procedure TfmMain.actToolsRunSelectedBatchExecute(Sender: TObject);
begin
  if (cbBatches.ItemIndex >= 0) and (cbBatches.ItemIndex < NNTPAccounts.BatchesCount) then
    RunBatch(NNTPAccounts.Batches[cbBatches.ItemIndex]);
end;

procedure TfmMain.actMessageAddToBozoBinExecute(Sender: TObject);
var
  bart: TArticleBase;
  art: TArticle;
begin
  bart := GetFocusedArticle;
  if bart is TArticle then
    art := TArticle(bart)
  else
    art := nil;
  if Assigned(art) then
  begin
    if art.IsFromBozo then
      NNTPAccounts.RemoveBozoMatching(art)
    else
      NNTPAccounts.AddBozoMatching(art);
    art.Owner.ResetBozoFlags;
    vstArticles.Invalidate;
  end;
end;

procedure TfmMain.actViewShowBatchBarExecute(Sender: TObject);
begin
  actViewShowBatchbar.Checked := not actViewShowBatchbar.Checked;
  pnlBatchBar.Visible := actViewShowBatchbar.Checked
end;

procedure TfmMain.actViewShowBookmarkPaneExecute(Sender: TObject);
begin
  DisplayBookmarks(not pnlBookmark.Visible);
end;

procedure TfmMain.vstBookmarkDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
begin
  MoveSelectedArticlesToBookmark(fCurrentBookmark);
end;

procedure TfmMain.vstBookmarkDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfmMain.MoveSelectedArticlesToBookmark(bookmark: TBookmark);
var
  node: PVirtualNode;
  article: TArticleBase;
begin
  if not Assigned(bookmark) then
    bookmark := CreateBookmark;

  SetCurrentBookmark(bookmark);

  if Assigned(bookmark) then
  begin
    node := vstArticles.GetFirstSelected;
    article := GetNodeArticle(node);

    if Assigned(article) then
      ForEachSelectedArticle(DoMoveToBookmark, LPARAM(bookmark));

    vstBookmark.RootNodeCount := bookmark.MarkedArticleCount;
    vstBookmark.Invalidate;

    node := vstBookmark.GetLast;

    if Assigned(node) then
    begin
      vstBookmark.FocusedNode := node;
      vstBookmark.ClearSelection;
      vstBookmark.Selected[node] := True;
    end;
  end;
end;


procedure TfmMain.SetCurrentBookmark(bookmark: TBookmark; dontMakeVisible: Boolean);
begin
  if bookmark <> fCurrentBookmark then
  begin
    vstBookmark.Clear;
    FreeAndNil(fCurrentBookmark);

    if Assigned(bookmark) then
    begin
      if not dontMakeVisible then
      begin
        pnlBookmark.Visible := True;
        spltBookMark.Visible := True
      end;

      fCurrentBookmark := bookmark;
      vstBookmark.RootNodeCount := fCurrentBookmark.MarkedArticleCount;
      vstBookmark.Invalidate;
      cbBookmark.ItemIndex := cbBookmark.Items.IndexOf(bookmark.Name);
    end
    else
      cbBookmark.Text := '';
  end
  else
    if not dontMakeVisible and (pnlBookmark.Visible = False) then
    begin
      pnlBookmark.Visible := True;
      spltBookMark.Visible := True;
    end;
end;

procedure TfmMain.DoMoveToBookmark(article: TArticleBase; param: LPARAM;
  multiSelect: Boolean);
var
  bookmark: TBookmark;
begin
  bookmark := TBookmark(param);
  bookmark.AddArticle(article);
end;

procedure TfmMain.vstBookmarkInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  data: PObject;
begin
  data := PObject(vstBookmark.GetNodeData(node));
  if Assigned(fCurrentBookmark) and (node^.Index < DWORD(fCurrentBookmark.MarkedArticleCount)) then
    data^ := fCurrentBookmark.MarkedArticle[node^.index]
  else
    data^ := nil;
end;

procedure TfmMain.vstBookmarkGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  obj: PObject;
  ma: TMarkedArticle;
  st: string;
begin
  st := '';
  obj := PObject(vstBookmark.GetNodeData(node));
  if Assigned(obj) then
  begin
    ma := TMarkedArticle(obj^);
    if Assigned(ma) then
    begin
      case Column of
        0: st := ma.Account + ':' + ShortGroupName(ma.Group);
        1: st := ma.Subject;
        2: st := ma.From;
        3: st := DateTimeToStr(ma.Date);
        4: st := IntToStr(ma.Lines);
        5: st := DateTimeToStr(ma.BookmarkedDate);
      end;
    end;
  end;
  CellText := st;
end;

procedure TfmMain.vstBookmarkResize(Sender: TObject);
begin
  if fBookmarkHeaderStatus = [] then
  begin
    Include(fBookmarkHeaderStatus, chResizing);
    try
      ResizeBookmarkHeader;
    finally
      Exclude(fBookmarkHeaderStatus, chResizing);
    end;
  end;
end;

procedure TfmMain.actReverseSelectedTextExecute(Sender: TObject);
var
  st: string;
begin
  if MessageScrollBox1.GetSelectedText(st) then
    MessageScrollBox1.SetSelectedText(WideReverseString(st));
end;

function TfmMain.CreateBookmark: TBookmark;
var
  st: string;
begin
  st := fBookmarkSet.GetUniqueName;
  if InputQuery('Create Bookmark Set', 'Bookmark Set Name', st) then
    Result := fBookmarkSet.CreateBookmark(st)
  else
    Result := nil;
  PopulateBookmarkCombo;
end;

procedure TfmMain.actBookmarksAddExecute(Sender: TObject);
begin
  MoveSelectedArticlesToBookmark(fCurrentBookmark);
end;

procedure TfmMain.actBookmarksRemoveExecute(Sender: TObject);
var
  idx: Integer;
  node: PVirtualNode;
  l: TList;
begin
  node := vstBookmark.GetFirstSelected;
  if Assigned(node) then
  begin
    l := TList.Create;
    try
      while Assigned(node) do
      begin
        l.Add(Pointer(node.Index));
        node := vstBookmark.GetNextSelected(node)
      end;

      if l.Count > 0 then
      begin
        vstBookmark.BeginUpdate;
        try
          for idx := l.Count - 1 downto 0 do
            fCurrentBookmark.DeleteArticle(LPARAM(l[idx]));

          vstBookmark.RootNodeCount := fCurrentBookmark.MarkedArticleCount;
          vstBookmark.ReinitNode(nil, True);
        finally
          vstBookmark.EndUpdate;
        end;
      end;
    finally
      l.Free;
    end;
  end;
end;

procedure TfmMain.actBookmarksDeleteExecute(Sender: TObject);
var
  st: string;
begin
  if Assigned(fCurrentBookmark) then
  begin
    st := fCurrentBookmark.Name;
    SetCurrentBookmark(nil);
    fBookmarkSet.DeleteBookmarkSet(st);
    PopulateBookmarkCombo;
  end;
end;

procedure TfmMain.OnArticleFound(article: TArticleBase;
  bookmark: Boolean; var continue: Boolean);
var
  node: PVirtualNode;
  frDate: string;
  bmk: TBookmark;
begin
  if Bookmark then
  begin
    fUpdatingBookmark := True;
    try
      if not pnlBookmark.Visible then
        FreeAndNil(fCurrentBookmark);

      if fCurrentBookmark = nil then
      begin
        frDate := StringReplace(DateToStr(Now), '/', '-', [rfReplaceAll]);
        frDate := StringReplace(frDate, '.', '-', [rfReplaceAll]);

        bmk := fmMain.fBookmarkSet.CreateBookmark('Search Results ' + frDate);
        bmk.Clear;
        PopulateBookmarkCombo;
        SetCurrentBookmark(bmk);
      end;

      fCurrentBookmark.AddArticle(article);
      vstBookmark.RootNodeCount := fCurrentBookmark.MarkedArticleCount;
      vstBookmark.Invalidate;

      node := vstBookmark.GetLast;

      if Assigned(node) then
      begin
        vstBookmark.FocusedNode := node;
        vstBookmark.ClearSelection;
        vstBookmark.Selected[node] := True
      end;
      Application.ProcessMessages;
    finally
      fUpdatingBookmark := False;
    end;
  end
  else
  begin
    GoToArticle(article);
    continue := False;
  end;
end;

procedure TfmMain.PersistentPositionGetSettingsClass(Owner: TObject;
  var SettingsClass: TExSettingsClass);
begin
  SettingsClass := gExSettingsClass;
end;

procedure TfmMain.PersistentPositionGetSettingsFile(Owner: TObject;
  var fileName: string);
begin
  fileName := gExSettingsFile;
end;

procedure TfmMain.cbBookmarkChange(Sender: TObject);
var
  st: string;
begin
  st := cbBookmark.Text;
  if not Assigned(fCurrentBookmark) or (fCurrentBookmark.Name <> st) and (st <> '') then
    SetCurrentBookmark(fBookmarkSet.CreateBookmark(st));
end;

procedure TfmMain.PopulateBookmarkCombo;
var
  i: Integer;
  itm: string;
begin
  itm := cbBookmark.Text;
  cbBookmark.Items.BeginUpdate;
  try
    cbBookmark.Items.Clear;
    for i := 0 to fBookmarkSet.BookmarkCount - 1 do
      cbBookmark.Items.Add(fBookmarkSet.BookmarkName[i])
  finally
    cbBookmark.Items.EndUpdate;
  end;
  cbBookmark.Text := itm;
end;

procedure TfmMain.actBookmarksCreateExecute(Sender: TObject);
var
  bmk: TBookmark;
begin
  bmk := CreateBookmark;
  if Assigned(bmk) then
    SetCurrentBookmark(bmk);
end;

procedure TfmMain.actBookmarksClearAllExecute(Sender: TObject);
begin
  SetCurrentBookmark(nil);
  fBookmarkSet.DeleteAllBookmarkSets;
  PopulateBookmarkCombo;
end;

procedure TfmMain.actSearchFindNextReplyToMeExecute(Sender: TObject);
begin
  fDontMarkOnLeave := True;
  try
    NextArticle([naUnreadOnly, naReplyOnly, naCanWrap, naCanLeaveGroup, naCircularAccounts], GetFocusedArticle)
  finally
    fDontMarkOnLeave := False;
  end;
end;

procedure TfmMain.WmRetrySetMsg(var msg: TMessage);
begin
  fRetrySetMsgFlag := True;
end;

procedure TfmMain.DoMarkAsCancelledArticle(article: TArticleBase;
  param: LPARAM; multiSelect: Boolean);
var
  subj: string;
  msg: TStrings;
  header: TStrings;
  account: TNNTPAccount;
  grp: TSubscribedGroup;
  reason: string;
  mid: string;
  b: Boolean;
begin
  msg := nil;
  reason := PChar(param);
  header := TStringList.Create;
  try
    account := TArticle(article).Account;
    grp := TArticle(article).SubscribedGroup;
    msg := TStringList.Create;
    subj := 'cancel ' + article.MessageId;

    header.Values['From'] := '"' + grp.NNTPSettings.Identity.UserName + '" <' + grp.NNTPSettings.Identity.EMailAddress + '>';
    header.Values['Subject'] := 'cmsg ' + subj;
    if grp.NNTPSettings.GenerateDateHeaders then
      header.Values['Date'] := LocalDateTimeToGMT(Now);
    header.Values['Control'] := subj;
    header.Values['Newsgroups'] := grp.Name;
    if grp.NNTPSettings.GenerateApprovedHeaders then
      header.Values['Approved'] := grp.NNTPSettings.Identity.EMailAddress;
    header.Values['Sender'] := article.From;
    mid := article.MessageId;
    if Copy(mid, 1, 1) = '<' then
      mid := '<cancel.' + Copy(mid, 2, MaxInt);
    header.Values['Message-Id'] := mid;
    header.Values['X-Canceled-By'] := '"' + grp.NNTPSettings.Identity.UserName + '" <' + grp.NNTPSettings.Identity.EMailAddress + '>';

    b := fIteratorFailed;
    try
      DoCheckFromMe(article, 0, False);
      if fIteratorFailed then
        header.Values['Path'] := 'cyberspam';
    finally
      fIteratorFailed := b;
    end;

    msg.Add('This message was cancelled by ' + grp.NNTPSettings.Identity.UserName);
    msg.Add('');
    msg.Add('Original message header:');
    msg.Add('  From: ' + article.From);
    msg.Add('  Subject: ' + article.Subject);
    msg.Add('  Date: ' + SafeDateTimeToInternetStr(article.Date));
    msg.Add('  Newsgroups: ' + article.Header['Newsgroups']);
    msg.Add('  Message-ID: ' + article.MessageId);

    if reason <> '' then
      msg.Add(#13#10 + reason);

    ThreadManager.PostMessage(Account, header.Text, msg.Text, nil, CP_USASCII, tpNNTP);
    article.IsCancelled := True;
  finally
    header.Free;
    msg.Free;
  end;
end;

procedure TfmMain.DoCheckFromMe(article: TArticleBase; param: LPARAM;
  multiSelect: Boolean);
var
  identity: TIdentity;
begin
  identity := article.Owner.Identity;
  fIteratorFailed := (article.FromName <> Identity.UserName) or (article.FromEmail <> Identity.EMailAddress);
end;

procedure TfmMain.actAccountExpandAllExecute(Sender: TObject);
begin
  vstSubscribed.FullExpand;
end;

procedure TfmMain.actAccountCollapseAllExecute(Sender: TObject);
begin
  vstSubscribed.FullCollapse;
end;

procedure TfmMain.HandleSpecialKey(key: word; shift: TShiftState);
var
  article: TArticleBase;
  NextOptions: TNextArticleOptions;
  pageIncrement: Integer;
begin
  if fDisableShortcutCount <> 0 then Exit;

  case key of
    VK_SPACE:           // SPACEBAR.  Scroll down the messager view if possible
                        // otherwise, mark the message as read, and go to the next
                        // unread article is there is one.

                        // If the article body hasn't been loaded, do nothing
      if ssCtrl in Shift then
        actArticleGotoPrevious.Execute
      else
      begin
        article := GetFocusedArticle;
        pageIncrement := MessageScrollBox1.ClientHeight;
        if Assigned(article) then
          if (ActiveControl is TNewsRichEditX) and (MessageScrollBox1.VertScrollBar.Position < MessageScrollBox1.VertScrollBar.Range - PageIncrement) then
          else
          begin
            if not article.HasMsg or (MessageScrollBox1.VertScrollBar.Position + pageIncrement >= MessageScrollBox1.VertScrollBar.Range) then
              NextArticle([naMarkAsRead, naUnreadOnly, naCanWrap, naCanLeaveGroup, naCircularAccounts], GetFocusedArticle)
            else
            begin
              pageIncrement := pageIncrement - MessageScrollBox1.LineHeight;
              MessageScrollBox1.VertScrollBar.Position := MessageScrollBox1.VertScrollBar.Position + pageIncrement;
            end;
          end;
      end;


    VK_RETURN:          // RETURN.  If the article has been loaded, mark the message
                        // as read, and go to the next unread article.  If there isn't one,
                        // go to the next unread article in another group.

                        // If the article hasn't been loaded, load it.
      begin
        article := GetFocusedArticle;
        if Assigned(article) and (not article.HasMsg) and (article.ArticleNo <> 0) and (XNOptions.EnterLoadsMessage) then
          actArticleGetMessageBody.Execute
        else
        begin
          NextOptions := [naMarkAsRead, naUnreadOnly, naCanWrap, naCircularAccounts];
          if XNOptions.EnterGoToNextGroup then
            Include(NextOptions, naCanLeaveGroup);

          NextArticle(NextOptions, GetFocusedArticle);
        end;
      end;
  end;
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or ((KEY = VK_RETURN) and (ActiveControl <> vstSubscribed)) then
  begin
    PostMessage(Handle, WM_SPECIALKEY, Key, 0);
    Key := 0;
  end
  else
    if fDisableShortcutCount > 0 then
      if (Key <> VK_RIGHT) and (Key <> VK_LEFT) and (Key <> VK_HOME) and (Key <> VK_END) and (Key <> VK_DELETE) and (Key <> VK_INSERT) then
        Key := 0;
end;

procedure TfmMain.WmSpecialKey(var msg: TMessage);
begin
  HandleSpecialKey(msg.WParam, KeyDataToShiftState(Msg.WParam));
end;

procedure TfmMain.actToolsSearchbarGoExecute(Sender: TObject);

  function CreateSearchBarFilter(invertOp: Boolean): TNNTPFilter;
  var
    strOp: Boolean;
    column: TNNTPFilterColumn;
    op: TNNTPFilterOperator;
  begin
    column := ftMessageBody;
    case cbSearchBarTarget.ItemIndex of
      0: column := ftMessageBody;
      1: column := ftSubject;
      2: column := ftAuthor;
      3: column := ftDate;
      4: column := ftLines;
    end;
    strOp := column in [ftMessageBody, ftSubject, ftAuthor];

    op := opContains;
    if strOp then
      case cbSearchBarOp.ItemIndex of
        0: op := opContains;
        1: op := opDoesntContain;
      end
    else
      case cbSearchBarOp.ItemIndex of
        0: op := opLess;
        1: op := opGreater;
        2: op := opEqual;
        3: op := opNotEqual;
      end;

    if invertOp then
    case op of
      opContains: op := opDoesntContain;
      opDoesntContain: op := opContains;
      opLess: op := opGreater;
      opGreater: op := opLess;
      opEqual: op := opNotEqual;
      opNotEqual: op := opEqual;
    end;

    if strOp then
      Result := TNNTPFilter.Create('', column, op, edSearchBarText.Text, False, False, False)
    else
      if column = ftLines then
        Result := TNNTPFilter.Create('', column, op, StrToIntDef(edSearchBarText.Text, 0), False, False, False)
      else
        Result := TNNTPFilter.Create('', column, op, StrToDateDef(edSearchBarText.Text, Now), False, False, False);
  end;

  procedure SearchbarSearch(searchToBookmark: Boolean);
  var
    ctnrs: TObjectList;
    filters: TNNTPFilters;
    art: TArticleBase;
    cont: Boolean;
    max: Integer;
  begin
    filters := nil;
    ctnrs := TObjectList.Create;
    try
      ctnrs.OwnsObjects := False;
      ForEachSelectedGroup(DoAddGroupToList, False, LPARAM(ctnrs));
      if ctnrs.Count = 0 then
        ForEachGroupInSelectedAccount(DoAddGroupToList, False, LPARAM(ctnrs));

      filters := TNNTPFilters.Create(True);
      filters.AddObject('', CreateSearchBarFilter(False));

      cont := True;
      max := 200;
      art := GetFocusedArticle;
      while cont do
      begin
        art := unitMessageBaseSearch.Search(art, ctnrs, filters);
        if Assigned(art) then
          OnArticleFound(art, searchToBookmark, cont);

        Dec(max);
        if not searchToBookmark or (Max <= 0) or not Assigned(art) then
          Break;
      end;
    finally
      ctnrs.Free;
      filters.Free;
    end;
  end;

  procedure SetFilter;
  var
    ctnr: TArticleContainer;
    fltr: TNNTPFilter;
    art: TArticleBase;
  begin
    art := GetFocusedArticle;
    ctnr := fLastFocusedArticleContainer;
    if not Assigned(ctnr) then Exit;

    if edSearchBarText.Text <> '' then
    begin
      fltr := CreateSearchBarFilter(True);
      ctnr.DisplayFiltersCtnr.EnableFilter(fltr, True);
    end
    else
      fltr := nil;

    ctnr.DisplayFiltersCtnr.Filters.DeleteTemporaryFilters;

    if fltr <> nil then
      fltr.Temporary := True;  // Also caused it to be 'owned'

    fLastFocusedArticleContainer.ReSortArticles;
    InitArticlesRootNodeCount(ctnr);
    Refresh_vstArticles;
    GoToArticle(art);
  end;

begin
  if rbSearchbarSearch.Checked then
    SearchbarSearch(False)
  else
    if rbBookmark.Checked then
      SearchbarSearch(True)
    else
      if rbFilter.Checked then
        SetFilter;
end;

procedure TfmMain.actToolsSendOutbasketExecute(Sender: TObject);
begin
  ThreadManager.ResumeOutbasketEntries;
end;

procedure TfmMain.WmNameThread(var msg: TMessage);
begin
  {$ifdef madExcept}
//  NameThread(msg.WParam, UTF8Encode(PChar(msg.LParam))); // madExcept v3
    NameThread(msg.WParam, PChar(msg.LParam));             // madExcept v4
  {$else}
    {$if CompilerVersion >= 21.0} // 21.0 = Delphi 2010
      TThread.NameThreadForDebugging(UTF8Encode(PChar(msg.LParam)), msg.WParam);
    {$ifend}
  {$endif}
end;

procedure TfmMain.WmGetConnected(var msg: TMessage);
begin
  if Assigned(ThreadManager) then
    msg.Result := Integer(ThreadManager.CurrentConnectoid <> '')
  else
    msg.Result := 0;
end;

procedure TfmMain.SplitterPanel1DockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  if Source.Control = pnlLeft then
    Source.Control.Width := fPanelLeftWidth;
end;

procedure TfmMain.SplitterPanel1DockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := Source.Control is TPanel;
  if Accept then
  begin
    // Modify the DockRect to preview dock area.
    ARect.TopLeft := TControl(Sender).ClientToScreen(Point(0, 0));
    ARect.BottomRight := TControl(Sender).ClientToScreen(Point(Source.Control.Width, TControl(Sender).Height));
    Source.DockRect := ARect;
  end;
end;

procedure TfmMain.SplitterPanel2DockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := Source.Control is TPanel;
  if Accept then
  begin
    // Modify the DockRect to preview dock area.
    ARect.TopLeft := TControl(Sender).ClientToScreen(Point(-Source.Control.Width, 0));
    ARect.BottomRight := TControl(Sender).ClientToScreen(Point(0, TControl(Sender).Height));
    Source.DockRect := ARect;
  end;
end;

procedure TfmMain.pnlLeftEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  if (Sender is TPanel) and (Target is TCustomForm) then
  begin
    with TCustomForm(Target) do
    begin
      Left := X - fPanelLeftDragOffset.X;
      Top := Y {- fPanelLeftDragOffset.Y};
    end;
  end;
end;

procedure TfmMain.pnlLeftStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
var
  mp: TPoint;
begin
  GetCursorPos(mp);
  fPanelLeftDragOffset := pnlLeft.ScreenToClient(mp);
  fPanelLeftWidth := pnlLeft.Width;
  fPanelLeftHeight := pnlLeft.Height;
  pnlLeft.UndockWidth := pnlLeft.Width;

  DragObject:= TTransparentDragDockObject.Create(pnlLeft);
end;

procedure TfmMain.actViewSubscribedGroupsPaneExecute(Sender: TObject);
begin
  if pnlLeft.Visible then
  begin
    fPanelLeftHeight := pnlLeft.Height;
    fPanelLeftWidth := pnlLeft.Width;
  end;
  pnlLeft.Visible := not pnlLeft.Visible;

  if pnlLeft.Visible then
  begin
    pnlLeft.Width := fPanelLeftWidth;
    pnlLeft.Height := fPanelLeftHeight;
  end;
end;

procedure TfmMain.actViewToolbarCaptionsExecute(Sender: TObject);
begin
  actViewToolbarCaptions.Checked := not actViewToolbarCaptions.Checked;

  if actViewToolbarCaptions.Checked then
    tbMain.ShowCaptions := True
  else
  begin
    tbMain.ShowCaptions := False;
    tbMain.ButtonWidth := 0;
    tbMain.ButtonHeight := 0;
  end;
end;

procedure TfmMain.actViewToolbarSmallImagesExecute(Sender: TObject);
begin
  actViewToolbarSmallImages.Checked := not actViewToolbarSmallImages.Checked;

  if actViewToolbarSmallImages.Checked then
  begin
    tbMain.Images := ilMain;
    tbMain.DisabledImages := ilDisabled;
    tbMain.ButtonWidth := 0;
    tbMain.ButtonHeight := 0;
  end
  else
  begin
    tbMain.Images := ilMainLarge;
    tbMain.DisabledImages := ilDisabledLarge;
  end;
end;

procedure TfmMain.StopEverything;
begin
  Timer1.Enabled := False;

  ThreadManager.DisconnectAll(True);
  DontUnloadCache.Clear;

  if fLastFocusedArticleContainer <> nil then
  begin
    if (not fDontMarkOnLeave) and fLastFocusedArticleContainer.MarkOnLeave then
      actNewsgroupMarkAllMessagesAsRead.Execute;
    fLastFocusedArticleContainer.LeaveGroup(False);
  end;

  vstArticles.RootNodeCount := 0;
  vstSubscribed.RootNodeCount := 0;
  vstQueuedRequests.RootNodeCount := 0;

  fOutstandingRequestCount := 0;
  fOutstandingGetterCount := 0;
  fOutstandingActiveGetterCount := 0;

  NNTPAccounts.UnloadOldContainers(nil);

  NNTPAccounts.PurgeOldArticles;
  NNTPAccounts.SaveDirtyArticles; // (heh heh!)

  ThreadManager.ClearGetters;

  FreeAndNil(NNTPAccounts);
  FreeAndNil(MailAccounts);
  FreeAndNil(gArticleFolders);

  FreeAndNil(fBookmarkSet);
end;

procedure TfmMain.StartEverything;
begin
  gArticleFolders := TArticleFolders.Create;
  NNTPAccounts := TNNTPAccounts.Create;
  MailAccounts := TMailAccounts.Create;
  AllFilters.Load;
  PersistentPosition.Enabled := False;
  NNTPAccounts.LoadFromRegistry;
  PersistentPosition.Enabled := True;
  MailAccounts.LoadFromRegistry;
  PostMessage(Handle, WM_SETUP, 0, 0);
  Timer1.Enabled := True;
end;


procedure TfmMain.actFileMoveMessagebaseExecute(Sender: TObject);
var
  dlg: TdlgMoveMessagebase;
begin
  if ThreadManager.ActiveGetterCount > 0 then
  begin
    ShowMessage('Please wait until all queued requests have completed before moving the message base.');
    Exit;
  end;

  dlg := nil;
  StopEverything;
  try
    dlg := TdlgMoveMessagebase.Create(nil);
    dlg.ShowModal;
  finally
    try
      StartEverything;
    except
    end;
    dlg.Free;
  end;
end;

var
  gtc: Integer = 0;

procedure TfmMain.MouseWheelHandler(var Message: TMessage);
var
  pt: TPoint;
  tc: Integer;
begin
  pt := Mouse.CursorPos;
  MapWindowPoints(0, MessageScrollBox1.Handle, pt, 1);

  if ptInRect(MessageScrollBox1.BoundsRect, pt) then
  begin
    tc := GetTickCount;
    if Abs(tc - gtc) > 10 then
    begin
      gtc := tc;
      MessageScrollBox1.Perform(CM_MOUSEWHEEL, Message.WParam, Message.LParam);
    end;
  end
  else
    inherited;
end;

procedure TfmMain.actMessageCopyXFaceExecute(Sender: TObject);
var
  bmp: TBitmap;
begin
  if Assigned(MessageScrollBox1.Msg) then
  begin
    bmp := MessageScrollBox1.Msg.XFace;
    if Assigned(bmp) then
      Clipboard.Assign(bmp);
  end;
end;

procedure TfmMain.DoIgnoreArticle(article: TArticleBase; param: LPARAM;
  multiSelect: Boolean);
begin
  article.IsIgnore := Boolean(param);
end;

procedure TfmMain.WmShowNewsgroupList(var msg: TMessage);
var
  account: TNNTPAccount;
begin
  account := TNNTPAccount(msg.LParam);
  ShowNewsgroupList(account, False);
end;

procedure TfmMain.WmSize(var msg: TMessage);
begin
  inherited;
end;

procedure TfmMain.actArticleRetrieveParentMessagesExecute(Sender: TObject);
var
  article: TArticleBase;
begin
  article := GetFocusedArticle;
  if Assigned(article) and (article is TArticle) then
    ThreadManager.GetParentArticles(TArticle(article));
end;

procedure TfmMain.vstSubscribedEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  fRenamingFolder := False;
  EnableShortcuts(True);
end;

procedure TfmMain.actFolderReindexExecute(Sender: TObject);
var
  fldr: TArticleFolder;
  art: TArticleBase;
  id: RawByteString;
begin

  fldr := GetFocusedArticleFolder;
  if Assigned(fldr) then
  begin
    art := GetFocusedArticle;
    if Assigned(art) and (art.ArticleNo = 0) then
      art := nil;
    if Assigned(art) then
      id := art.UniqueID;
    DisplayArticleBody(nil);
    fldr.Reindex;
    InitArticlesRootNodeCount(fldr);
    Refresh_vstArticles;
    art := fldr.FindUniqueID(id);
    GoToArticle(art);
  end;
end;

procedure TfmMain.mnuCopyURLToClipboardClick(Sender: TObject);
begin
  if fURL <> '' then
    Clipboard.AsText := fURL;
end;

procedure TfmMain.actFolderFindMessageExecute(Sender: TObject);
var
  art: TFolderArticle;
  mart: TArticleBase;
  xref: string;
  st, serverName, accountName, groupName: string;
  msgID: RawByteString;
  ctnr: TArticleContainer;
begin
  mart := nil;
  art := GetFocusedFolderArticle;

  if Assigned(art) then
  begin
    xref := art.Header['X-Ref'];
    if art is TSentMessage then
    begin
      serverName := SplitString(' ', xref);
      st := art.Header['Newsgroups'];
      msgID := art.RawMessageId;
      groupName := SplitString(',', st);
      accountName := NNTPAccounts.GetServerAccountName(serverName, groupName);

      if (accountName <> '') and (groupName <> '') and (msgID <> '') then
      begin
        ctnr := NNTPAccounts.FindArticleContainer(accountName, groupName);
        if Assigned(ctnr) then
        begin
          mart := ctnr.FindMsgID(msgID);
          // Some servers do change message-ids, try finding it based on the X-Ref header line.
          if not Assigned(mart) then
            mart := ctnr.FindMyXRef(art.Header['X-Ref']);
        end;
      end;
    end
    else
      mart := NNTPAccounts.FindXRef(xref);
    GoToArticle(mart);
  end;
end;

function TfmMain.GetFocusedFolderArticle: TFolderArticle;
var
  art: TArticleBase;
begin
  art := GetFocusedArticle;
  if Assigned(art) and (art is TFolderArticle) then
    Result := TFolderArticle(art)
  else
    Result := nil;
end;

procedure TfmMain.actToolsResetHighWaterMarkExecute(Sender: TObject);
var
  grp: TSubscribedGroup;
begin
  grp := GetFocusedGroup;

  if Assigned(grp) then
  begin
    grp.HighWaterMark := 0;
    NNTPAccounts.SaveToRegistry(grp.Owner);
    ShowMessage('High Water Mark for ' + grp.Name + ' reset');
  end;
end;

procedure TfmMain.actToolsTestMarkAllUnreadExecute(Sender: TObject);
var
  newsgroup: TSubscribedGroup;
begin
  newsgroup := GetFocusedGroup;

  if Assigned(newsgroup) then          // Show delete dialog
  begin
    if vstSubscribed.SelectedCount < 2 then
      DoMarkArticlesAsRead(newsgroup, 1)
    else
      ForEachSelectedGroup(DoMarkArticlesAsRead, False, 1);
    vstArticles.Invalidate;
    Refresh_vstSubscribed;
  end
  else
    if GetFocusedAccount <> nil then
    begin
      ForEachGroupInSelectedAccount(DoMarkArticlesAsRead, False, 1);
      vstArticles.Invalidate;
      Refresh_vstSubscribed;
    end;
end;

procedure TfmMain.actArticleCombineDecodeExecute(Sender: TObject);
var
  dlg: TdlgCombineDecode;
  gotAllMessageParts: Boolean;
  i: Integer;
begin
  dlg := TdlgCombineDecode.Create(nil);
  try
    ForEachSelectedArticle(DoAddArticleToList, LPARAM(dlg.Articles));
    if dlg.ShowModal = mrOK then
    begin
      SetAttachmentsDirectory(dlg.edFileName.Text);
      gotAllMessageParts := dlg.Articles.Count > 0;
      for i := 0 to dlg.Articles.Count - 1 do
        if not TArticleBase(dlg.Articles[i]).HasMsg then
          gotAllMessageParts := False;

      if gotAllMessageParts then
        SaveMultipartAttachment(dlg.edFileName.Text, dlg.Articles)
      else
        if dlg.Articles.Count > 0 then
          fDeferredCombineList.Add(TDeferredCombineSet.Create(dlg.edFileName.Text, dlg.Articles));
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfmMain.DoAddArticleToList(article: TArticleBase; param: LPARAM;
  multiSelect: Boolean);
begin
  TList(param).Add(article);
end;

procedure TfmMain.GoToNextArticle(markread: Boolean);
var
  art: TArticleBase;
  grp: TSubscribedGroup;
  act: TNNTPAccount;
  idx: Integer;
  options: TNextArticleOptions;

  function IndexOfGroup(grp: TSubscribedGroup; acct: TNNTPAccount): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to act.SubscribedGroupCount - 1 do
      if act.SubscribedGroups[i] = grp then
      begin
        Result := i;
        Break;
      end;
  end;

  function IndexOfAct(act: TNNTPAccount): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to NNTPAccounts.Count - 1 do
      if NNTPAccounts.Items[i] = act then
      begin
        Result := i;
        Break;
      end;
  end;

begin
  art := GetFocusedArticle;

  if not Assigned(art) then
  begin
    grp := GetFocusedGroup;
    if not Assigned(grp) then
      act := GetFocusedAccount
    else
      act := grp.Owner;

    while Assigned(act) and not Assigned(art) do
    begin
      if not Assigned(grp) then
        if Assigned(act) and (act.SubscribedGroupCount > 0) then
          grp := act.SubscribedGroups[0];

      while Assigned(grp) and not Assigned(art) do
        if grp.LoadGetArticleCount > 0 then
          art := grp.Articles[0]
        else
        begin
          idx := IndexOfGroup(grp, act);
          if (idx >= 0) and (idx < act.SubscribedGroupCount - 1) then
            grp := act.SubscribedGroups[idx + 1]
          else
            grp := nil;
        end;

      if not Assigned(art) then
      begin
        idx := IndexOfAct(act);
        if (idx >= 0) and (idx < NNTPAccounts.Count - 1) then
          act := NNTPAccounts.Items[idx + 1]
        else
          act := nil;
      end;
    end;
  end;

  options := [naUnreadOnly, naCanLeaveGroup, naCircularAccounts];
  if markRead then
    options := options + [naMarkAsRead, naCanWrap];
  NextArticle(options, art);
end;

function TfmMain.ForEachSelectedArticleFolder(
  proc: TArticleFolderIteratorProc; param: LPARAM): Integer;
var
  node: PVirtualNode;
  folder: TArticleFolder;
  folders: TList;
  i: Integer;
begin
  // nb. the order is quite important, so don't use vstSubscribed.FirstSelectedNode/GetNextSelectedNode
  folders := nil;
  Result := 0;

  try
    node := GetFirstvstSubscribedSelectedNode;
    if Assigned(node) then
    begin
      folders := TList.Create;

      while Assigned(node) do
      begin
        folder := GetNodeArticleFolder(node);
        if Assigned(folder) then
          folders.Add(folder);

        repeat
          node := vstSubscribed.GetNext(node);
        until not Assigned(Node) or (vsSelected in Node.States);
      end;
    end;

    fIteratorFailed := False;

    if Assigned(folders) then
    begin
      for i := 0 to folders.Count - 1 do
      begin
        folder := TArticleFolder(folders[i]);
        if Assigned(proc) then
          proc(folder, param);
      end;
    end;
  finally
    folders.Free;
  end;
end;

function TfmMain.DoAddFolderToList(folder: TArticleFolder; param: LPARAM): Boolean;
var
  list: TList;
begin
  list := TList(param);
  list.Add(folder);
  Result := True;
end;

procedure TfmMain.DoCheckArticleRead(article: TArticleBase; param: LPARAM;
  multiSelect: Boolean);
begin
  if not article.IsRead then
    fIteratorFailed := True;
end;

procedure TfmMain.WmApplyChanges(var msg: TMessage);
var
  ctnr: TArticleContainer;
begin
  XNOptions.Save;
  NNTPAccounts.SaveToRegistry;

  ctnr := fLastFocusedArticleContainer;
  vstSubscribed.RootNodeCount := 0;
  Reinit_vstSubscribed(True);
  ApplyControlOptions;
  if ctnr <> nil then
    if (not NNTPAccounts.ShowSecrets) and (ctnr.Secret or ((ctnr is TSubscribedGroup) and (TSubscribedGroup(ctnr).Owner.Secret))) then
      ctnr := nil;
  fPrevArticle := nil;
  FocusArticleContainer(ctnr);
  SyncContainerTree(ctnr);
  Reinit_vstSubscribed;
end;

procedure TfmMain.MessageScrollBox1DoubleClick(Sender: TObject);
begin
  actMessageExecuteAttachment.Execute
end;

procedure TfmMain.SaveBookmarkHeaderColumns;
var
  i: Integer;
  cw: Integer;
begin
  if not pnlBookmark.Visible then Exit;
  cw := vstBookmark.ClientWidth;
  with vstBookmark.Header do
    for i := 0 to Columns.Count - 2 do
      XNOptions.BookmarkColumnPCs[i] := (Columns[i].Width * 100 + cw div 2) div cw;
end;

function TfmMain.ForEachSelectedBranch(proc: TArticleIteratorProc;
  param: LPARAM; startAtRoot: Boolean): Integer;
var
  node, next: PVirtualNode;
  article: TArticleBase;
  count: Integer;
  multiSelect: Boolean;
begin
  multiSelect := vstArticles.SelectedCount > 1;
  node := vstArticles.GetFirstSelected;
  count := 0;
  fIteratorFailed := False;
  while not fIteratorFailed and Assigned(node) do
  begin
    if startAtRoot then
      while Assigned(node.Parent) and (node.Parent <> vstArticles.RootNode) do
        node := node^.Parent;
    article := GetNodeArticle(node);
    if Assigned(article) then
    begin
      Inc(count);
      proc(article, param, multiSelect);

      next := vstArticles.GetNextSibling(node);
      while next = nil do
      begin
        node := node.Parent;
        if node = vstArticles.RootNode then
          Break;
        next := vstArticles.GetNextSibling(node);
      end;
      node := next;
      if node <> nil then
        if not vstArticles.Selected[node] then
          node := vstArticles.GetNextSelected(node);
    end;
  end;
  Result := count;
end;

procedure TfmMain.GetSingleArticle(grp: TSubscribedGroup; articleNo: Int64);
var
  params: TGetMessagesParams;
begin
  if articleNo = 0 then Exit;
  params.fromArticle := articleNo;
  params.batchRef := 0;
  params.useDefaultGroupSettings := False;
  params.fHeadersOnly := False;
  params.fMessageCount := 1;
  params.fActionType := batAll;
  params.fManagementType := bmtMarkAsRead;
  params.fManagementOption := bmoNone;
  params.fManagementCount := 0;
  DoGetMessages(grp, LPARAM(@params));
end;

procedure TfmMain.DoCheckArticleFlagged(article: TArticleBase; param: LPARAM;
  multiSelect: Boolean);
begin
  if not article.IsInteresting then
    fIteratorFailed := True;
end;

procedure TfmMain.DoCheckArticleIgnored(article: TArticleBase; param: LPARAM;
  multiSelect: Boolean);
begin
  if not article.IsIgnore then
    fIteratorFailed := True;
end;

function TfmMain.MakeDormant(group: TSubscribedGroup; param: LPARAM): Boolean;
var
  i: Integer;
  batch: TNNTPBatch;
  noAction: TBatchAction;
  batchChanged, batchesChanged: Boolean;
begin
  Result := True;
  batchesChanged := False;
  for i := 0 to NNTPAccounts.BatchesCount - 1 do
  begin
    batch := NNTPAccounts.Batches[i];

    repeat
      batchChanged := batch.RemoveAction(group.Owner.AccountName, group.Name);
      batchesChanged := batchesChanged or batchChanged;
    until not batchChanged;
  end;

  noAction := TBatchAction.Create;
  try
    noAction.ActionType := batNone;
    noAction.ManagementOption := bmoNone;

    group.NNTPSettings.DefaultAction := noAction;
    group.NNTPSettings.PerformDefaultAction := paNever;
    group.WriteSettings();
  finally
    noAction.Free;
  end;
end;

{ TDeferredCombineSet }

constructor TDeferredCombineSet.Create(const AFileName: string;
  articleList: TList);
var
  i: Integer;
  art: TArticleBase;
begin
  fArticles := TList.Create;
  fFileName := AFileName;
  fArticles.Assign(articleList);

  for i := 0 to fArticles.Count - 1 do
  begin
    art := TArticleBase(fArticles[i]);
    if not art.HasMsg then
      fmMain.DoGetArticleBody(art, 0, False);
  end;
end;

destructor TDeferredCombineSet.Destroy;
begin
  fArticles.Free;
  inherited;
end;

procedure TfmMain.CheckDeferredCombineList(article: TArticleBase);
var
  i, j: Integer;
  s: TDeferredCombineSet;
  gotAllArticles: Boolean;
begin
  i := 0;
  while i < fDeferredCombineList.Count do
  begin
    gotAllArticles := False;
    s := TDeferredCombineSet(fDeferredCombineList[i]);
    if article.Owner = TArticleBase(s.fArticles[0]).Owner then
    begin
      gotAllArticles := True;
      for j := 0 to s.fArticles.Count - 1 do
        if not TArticleBase(s.fArticles[j]).HasMsg then
          gotAllArticles := False;
    end;

    if gotAllArticles then
    begin
      SaveMultipartAttachment(s.fFileName, s.fArticles);
      fDeferredCombineList.Delete(i);
    end
    else
      Inc(i);
  end;
end;

procedure TfmMain.SaveMultipartAttachment(const fileName: string;
  articles: TList);
var
  f: TFileStream;
  mp: TmvMessagePart;
  art: TArticleBase;

  procedure GetMultipartData(f: TStream);
  var
    m: TmvMessage;
    c: Integer;
    mm: TmvMessagePart;
    artNo: Int64;

    function NextArticle: TArticleBase;
    begin
      if artNo < articles.Count then
      begin
        Result := TArticleBase(articles[artNo]);
        Inc(artNo);
      end
      else
        Result := nil;
      art := Result;
    end;

  begin
    artNo := 0;
    if mp.DecodeType = ttyEnc then
    begin
      mp.GetData(f);
      Inc(artNo);

      while NextArticle <> nil do
      begin
        if (art.Msg <> nil) then
          if art.Msg.MessageParts.Count = 0 then
            art.Msg.DecodeBody(f, ttyEnc) // TODO: this does nothing, should it have done something?!?!
          else
            art.Msg.MessageParts[0].GetData(f);
      end;
    end
    else
    begin
      m := TmvMessage.Create(nil);
      try
        m.Header.AddAnsiStrings(art.Msg.Header);
        m.AddData(art.Msg.RawData);
        Inc(artNo);

        while NextArticle <> nil do
        begin
          if art.Msg <> nil then
            m.AddData(art.Msg.RawData);
        end;

        c := m.MessageParts.Count;
        if c > 0 then
        begin
          while (c > 0) do
          begin
            mm := m.MessageParts[c - 1];
            f.Seek(0, soBeginning);
            mm.GetData(f);
            if f.Size > 4 then
              Break;
            Dec(c);
          end
        end
        else
          m.DecodeBody(f, mp.DecodeType);
      finally
        m.Free;
      end;
    end;
  end;

begin
  f := nil;
  Timer1.Enabled := False;
  try
    art := TArticleBase(articles[0]);
    if Assigned(art.Msg) and (art.Msg.MessageParts.Count > 0) then
    begin
      mp := art.Msg.MessageParts[0];
      f := TFileStream.Create(fileName, fmCreate, fmShareDenyWrite);

      GetMultipartData(f);
    end;
  finally
    f.Free;
    Timer1.Enabled := True;
  end;
end;

procedure TfmMain.InitArticlesRootNodeCount(ctnr: TArticleContainer);
var
  wasEnabled: Boolean;
begin
  wasEnabled := Timer1.Enabled;
  Timer1.Enabled := False;
  try
    vstArticles.RootNodeCount := ctnr.ThreadCount
  finally
    Timer1.Enabled := wasEnabled;
  end;
end;

function TfmMain.IsInMultipartMode: Boolean;
var
  ctnr: TArticleContainer;
begin
  Result := False;
  ctnr := fLastFocusedArticleContainer;
  if Assigned(ctnr) then
    Result := (ctnr.ThreadOrder = toChronological) and (ctnr.ThreadSortOrder = soSubject);
end;

function TfmMain.GetAttachmentsDirectory: string;
var
  reg: TExSettings;
begin
  reg := CreateExSettings;
  try
    reg.Section := 'Directories';
    Result := reg.StringValue['Attachments'];
  finally
    reg.Free;
  end;
  if (Result <> '') and not (Copy(Result, Length(Result), 1)[1] in [':', '\']) then
    Result := Result + '\';
end;

procedure TfmMain.SetAttachmentsDirectory(fileName: string);
var
  reg: TExSettings;
begin
  fileName := ExpandFilename(fileName);
  fileName := ExtractFilePath(fileName);

  reg := CreateExSettings;
  try
    reg.Section := 'Directories';
    reg.StringValue['Attachments'] := fileName;
  finally
    reg.Free;
  end;
end;

function TfmMain.DoAddGroupToList(group: TSubscribedGroup;
  param: LPARAM): Boolean;
var
  list: TList;
begin
  list := TList(param);
  list.Add(group);
  Result := False;
end;

procedure TfmMain.SplitterPanel3DockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  if Source.Control = pnlLeft then
    Source.Control.Height := fPanelLeftHeight;
end;

procedure TfmMain.GotoSelectedURL1Click(Sender: TObject);
var
  st: string;
  s: string;
  sl: TStringList;
  i: Integer;
begin
  if MessageScrollBox1.GetSelectedText(st) then
  begin
    sl := TStringList.Create;
    try
      sl.Text := st;
      s := '';
      for i := 0 to sl.Count - 1 do
        s := s + Trim(sl[i]);
      if s <> '' then
        ShellExecute(handle, 'open', PChar(s), nil, nil, SW_SHOW);
    finally
      sl.Free;
    end;
  end;
end;

procedure TfmMain.actArticleGoToNextExecute(Sender: TObject);
var
  article: TArticleBase;
begin
  article := fNextArticleStack.Pop;
  if Assigned(article) then
  begin
    fNxStack := True;
    GoToArticle(article);
  end
  else
    actArticleNextUnreadExecute(nil);
end;

procedure TfmMain.actArticleGoToNextDontMarkExecute(Sender: TObject);
begin
  GotoNextArticle(False);
end;

procedure TfmMain.actViewShowSearchBarExecute(Sender: TObject);
begin
  actViewShowSearchBar.Checked := not actViewShowSearchBar.Checked;
  pnlSearchBar.Visible := actViewShowSearchBar.Checked;
end;

procedure TfmMain.actViewShowSecretsExecute(Sender: TObject);
var
  ctnr: TArticleContainer;
begin
  NNTPAccounts.ShowSecrets := not NNTPAccounts.ShowSecrets;
  ctnr := fLastFocusedArticleContainer;
  vstSubscribed.RootNodeCount := 0;
  Reinit_vstSubscribed(True);

  if ctnr <> nil then
    if (not NNTPAccounts.ShowSecrets) and (ctnr.Secret or ((ctnr is TSubscribedGroup) and (TSubscribedGroup(ctnr).Owner.Secret))) then
      ctnr := nil;
  fPrevArticle := nil;
  FocusArticleContainer(ctnr);
  SyncContainerTree(ctnr);
end;


procedure TfmMain.actViewShowToolbarExecute(Sender: TObject);
begin
  actViewShowToolbar.Checked := not actViewShowToolbar.Checked;
  tbMain.Visible := actViewShowToolbar.Checked
end;

procedure TfmMain.vstArticlesGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  article: TArticleBase;
  st, stNumber: string;
  isRoot: Boolean;
//  cp, defCP: Integer;
//  s: string;
begin
  article := GetNodeArticle(node);
  if article is TArticle then
  begin
    isRoot := not Assigned(Tarticle(article).Parent);
    if fForensicMode then
      stNumber := IntToStr(article.Index) + ' ' + IntToStr(Article.ArticleNo)
    else
      stNumber := IntToStr(article.ArticleNo);
  end
  else
    if article is TFolderArticle then
      with TFolderArticle(article) do
      begin
        stNumber := string(article.UniqueID); // OrigServer + ':' + ShortGroupName(OrigGroup) + ':' + IntToStr(ArticleNo); // + ' (' + IntToStr(Offset) + ')';
        isRoot := True;
      end
      else
        Exit;

  st := '';
  if Assigned(article) then
  begin
    case Column of
      0: if Assigned(Article.Owner) and (Article.Owner.ThreadOrder = toThreaded) then
           st := Format('Messages in thread: Unread=%d Total=%d', [Article.UnreadMessagesInThread, Article.MessagesInThread])
         else
           st := '';
      1: st := stNumber;
      2: if not fForensicMode then
           if (isRoot) or (not XNOptions.FirstLineAsSubject) then
             st := article.Subject
           else
             st := Article.InterestingMessageLine
         else
           st := Article.PostingHost;
      3: st := '"' + Article.FromName + '" <' + Article.FromEmail + '>';
//      3: st := Article.From;
      4: begin
           st := Article.Header['X-XanaOrigDate'];
           if st = '' then
             st := SafeDateTimeToInternetStr(Article.Date, True);
         end;

      5: st := IntToStr(Article.Lines) + ' lines'
    end;

    HintText := st;
  end;
end;

procedure TfmMain.vstSubscribedGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  data: PObject;
  ct, ct1: Integer;
  nm: string;
begin
  data := PObject(vstSubscribed.GetNodeData(node));

  if Assigned(data) and Assigned(data^) then
  begin
    if data^ is TNNTPAccount then
      HintText := TNNTPAccount(data^).AccountName + ' (' + TNNTPAccount(data^).NNTPServerSettings.ServerName + ')'
    else
      if data^ is TArticleContainer then
        with TArticleContainer(data^) do
        begin
          ct := UnreadArticleCount;
          ct1 := AdjustedArticleCount;

          if (data^ is TSubscribedGroup) and (TSubscribedGroup(data^).Nickname <> '') then
            nm := TSubscribedGroup(data^).Nickname
          else
            nm := Name;

          if (ct = 0) then
            HintText := nm + ' (' + IntToStr(ct1) + ')'
          else
            HintText := nm + ' (' + IntToStr(ct) + '/' + IntToStr(ct1) + ')';
        end
      else
        if data^ is TArticleFolders then
          HintText := rstArticleFolders
        else
          MessageBeep($FFFF);
  end
  else
    HintText := 'Loading...'
end;

procedure TfmMain.actSearchFindFlaggedUnreadExecute(Sender: TObject);
begin
  fDontMarkOnLeave := True;
  try
    NextArticle([naInterestingOnly, naCanLeaveGroup, naUnreadOnly, naCircularAccounts], GetFocusedArticle);
  finally
    fDontmarkOnLeave := False;
  end;
end;

procedure TfmMain.actSearchFindFlaggedInNewThreadExecute(Sender: TObject);
begin
  fDontMarkOnLeave := True;
  try
    NextArticle([naInterestingOnly, naCanLeaveGroup, naCircularAccounts], GetFocusedArticle, True);
  finally
    fDontmarkOnLeave := False;
  end;
end;

procedure TfmMain.vstSubscribedAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
var
  data: PObject;
  grp: TSubscribedGroup;
  r: TRect;
  rgn: HRGN;
  ct: Integer;
begin
  data := PObject(vstSubscribed.GetNodeData(node));
  if (data^ is TSubscribedGroup) and (XNOptions.ShowInterestingMarkers <> 0) then
    grp := TSubscribedGroup(data^)
  else
    Exit;

  case XNOptions.ShowInterestingMarkers of
    1: ct := grp.UnreadInterestingArticleCount;
  else
    ct := grp.InterestingArticleCount;
  end;

  if ct > 0 then
  begin
    r := ItemRect;
    r.Left := 0;
    r.Right := vstSubscribed.ClientWidth;
    rgn := CreateRectRgnIndirect(r);
    if SelectClipRgn(TargetCanvas.Handle, rgn) <> ERROR then
    try
      TargetCanvas.Draw(r.Left + 16 + 16, ((r.Bottom - r.Top) - ImageList1.Height) div 2, fInterestingIcon)
    finally
      SelectClipRgn(TargetCanvas.Handle, 0);
      DeleteObject(rgn);
    end;
  end;
end;

procedure TfmMain.vstSubscribedCollapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PObject;
begin
  Data := PObject(Sender.GetNodeData(Node));
  if Data^ is TNNTPAccount then
    TNNTPAccount(Data^).DisplaySettings.Expanded := False;
end;

procedure TfmMain.actSearchFindAnyKeywordExecute(Sender: TObject);
begin
  fDontMarkOnLeave := True;
  try
    NextArticle([naAnyKeyword, naCanLeaveGroup, naCircularAccounts], GetFocusedArticle);
  finally
    fDontMarkOnLeave := False;
  end;
end;

procedure TfmMain.spFixedFontClick(Sender: TObject);
var
  fixedFont: string;
begin
  if spfixedFont.Down then
  begin
    fixedFont := FindMatchingFixedFont(MessageScrollBox1.Font.Name);
    MessageScrollBox1.FixedFont := fixedFont;
  end
  else
    MessageScrollBox1.FixedFont := '';
  MessageScrollBox1.Refresh(True, True);
end;

procedure TfmMain.spGoToWebForumClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  S := spGoToWebForum.Hint;
  I := Pos('|', S);
  if I > 0 then
    ShellExecute(handle, 'open', PChar(Copy(S, I + 1, MaxInt)), nil, nil, SW_SHOWNORMAL);
end;

procedure TfmMain.actMessageToggleFixedFontExecute(Sender: TObject);
begin
  spFixedFont.Down := not spFixedFont.Down;
  spFixedFontClick(nil);
end;

procedure TfmMain.actArticleCopyHTMLExecute(Sender: TObject);
var
  html, origHTML: string;
  cf, len: Integer;
  h: THandle;
  p: PChar;
begin
  // TODO: check if this still works.
  if MessageScrollBox1.GetHTML(html, True) then
  begin
    origHTML := html;
    cf := HTMLClipboardFormat;
    if cf = 0 then Exit;

    html :=
      'Version:0.9'#13#10 +
      'StartHTML:ssssssss'#13#10 +
      'EndHTML:eeeeeeee'#13#10 +
      'StartFragment:ffffffff'#13#10 +
      'EndFragment:gggggggg'#13#10 +
      '<html><body>'#13#10 +
      '<!--StartFragment -->'#13#10 +
      html + #13#10 +
      '<!--EndFragment-->'#13#10 +
      '</body>'#13#10 +
      '</html>';

    len := Length(HTML);
    html := StringReplace(html, 'ssssssss', Format('%08.8d', [Pos('<html>', html) - 1]), []);
    html := StringReplace(html, 'eeeeeeee', Format('%08.8d', [len]), []);
    html := StringReplace(html, 'ffffffff', Format('%08.8d', [Pos('<!--StartFrag', html) - 1]), []);
    html := StringReplace(html, 'gggggggg', Format('%08.8d', [Pos('<!--EndFrag', html) - 1]), []);

    h := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, (len + 4)  * SizeOf(Char));
    try
      p := GlobalLock(h);
      try
        lstrcpy(p, PChar(HTML));
      finally
        GlobalUnlock(h);
      end;
      Clipboard.Open;
      try
        //Copy in both CF_HTML and CF_TEXT format
        Clipboard.SetAsHandle(cf, h);  // That 'h' is now owned
                                       // by the clipboard

        html := '<html><body>'#13#10 +
          origHTML + #13#10 +
          '</body></html>';
        h := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, (Length(html) + 4) * SizeOf(Char));
        p := GlobalLock(h);
        try
          lstrcpy(p, PChar(html));
        finally
          GlobalUnlock(h);
        end;

        SetClipboardData(CF_TEXT, h);
      finally
        Clipboard.Close;
      end;
    except
      GlobalFree(h);
    end;
  end;
end;


procedure TfmMain.actArticleSaveHTMLExecute(Sender: TObject);
var
  s: TStringList;
  html: string;
begin
  // TODO: check if this still works.
  if MessageScrollBox1.GetHTML(html, False) and dlgSaveHTML.Execute then
  begin
    s := TStringList.Create;
    try
      s.Text := html;
      s.SaveToFile(dlgSaveHTML.FileName);
    finally
      s.Free;
    end;
  end;
end;

procedure TfmMain.actMessageExecuteAttachmentExecute(Sender: TObject);
var
  mp: TmvMessagePart;
  fName, dir: string;
begin
  DoSaveAttachment('Run Attachment', fName, mp);
  if mp <> nil then
  begin
    dir := ExtractFilePath(fName);
    fName := ExtractFileName(fName);
    ShellExecute(Handle, 'open', PChar(fName), nil, PChar(dir), SW_SHOWNORMAL);
  end;
end;

procedure TfmMain.vstBookmarkHeaderMouseUp(Sender: TVTHeader;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    if chColumnResizing in fBookmarkHeaderStatus then
    begin
      Exclude(fBookmarkHeaderStatus, chColumnResizing);
      SaveBookmarkHeaderColumns;
    end;
end;

procedure TfmMain.vstBookmarkColumnResize(Sender: TVTHeader;
  Column: TColumnIndex);
begin
  if fBookmarkHeaderStatus = [] then
    Include(fBookmarkHeaderStatus, chColumnResizing);
end;

procedure TfmMain.actArticleIgnoreExecute(Sender: TObject);
var
  param: LPARAM;
  art: TArticleBase;
begin
  art := GetFocusedArticle;
  if Assigned(art) then
  begin
    if art.IsIgnore then
      param := 0
    else
      param := 1;
    ForEachSelectedArticle(DoIgnoreArticle, param);
  end;
end;

procedure TfmMain.actArticleIgnoreBranchExecute(Sender: TObject);
var
  article: TArticleBase;
begin
  article := GetFocusedArticle;
  if Assigned(article) then
    ForEachArticleInBranch(article, DoIgnoreArticle, LPARAM(not article.IsIgnore));
  vstArticles.Invalidate;
end;

procedure TfmMain.actArticleIgnoreThreadExecute(Sender: TObject);
var
  mark: Boolean;
  article: TArticleBase;
begin
  article := GetNodeArticle(vstArticles.GetFirstSelected);

  ForEachArticleInThread(article, DoCheckArticleIgnored, 0);
  mark := fIteratorFailed;

  ForEachArticleInThread(article, DoIgnoreArticle, LPARAM(mark));
  vstArticles.Invalidate;
end;

procedure TfmMain.actViewHideIgnoredMessagesExecute(Sender: TObject);
var
  art: TArticleBase;
  artno: Int64;
begin
  if Assigned(fLastFocusedArticleContainer) then
  begin
    art := GetFocusedArticle;
    if Assigned(art) then
      artno := art.ArticleNo
    else
      artNo := -1;

    fLastFocusedArticleContainer.HideIgnoredMessages := not fLastFocusedArticleContainer.HideIgnoredMessages;
    InitArticlesRootNodeCount(fLastFocusedArticleContainer);
    Refresh_vstArticles;

    if artNo <> -1 then
      GoToArticle(fLastFocusedArticleContainer.FindArticleNo(artNo));
  end;
end;

procedure TfmMain.cbBatchesSelect(Sender: TObject);
begin
  actToolsRunSelectedBatch.Execute;
end;

procedure TfmMain.actViewAutofitImagesExecute(Sender: TObject);
begin
  MessageScrollBox1.AutoFit := not MessageScrollBox1.AutoFit;
  XNOptions.AutofitImages := MessageScrollBox1.AutoFit;
end;

procedure TfmMain.actViewFindOnInternetExecute(Sender: TObject);
var
  art: TArticleBase;
  dlg: TdlgFindMessageOnInternet;
  mid: string;
  wmid: string;
begin
  art := GetFocusedArticle;
  if not Assigned(art) then Exit;

  MessageScrollBox1.GetSelectedText(wmid);
  mid := wmid;

  // Selected text can be a 'news:' or 'nntp:' link, or simply a message ID surrounded by '<>'
  if SameText(Copy(mid, 1, 5), 'news:') or SameText(Copy(mid, 1, 5), 'nntp:') then
  begin
    mid := Copy(mid, 6, MaxInt);
    if Pos('/', mid) > 0 then
      mid := ''     // Can't lookup by server/group/message no
    else
      if Copy(mid, 1, 1) <> '<' then
        mid := '<' + mid + '>';
  end;

  if (mid <> '') and (Copy(mid, 1, 1) = '<') and (Copy(mid, Length(mid), 1) = '>') then
  else
    mid := art.MessageID;

  if (GetKeyState(VK_CONTROL) and $80000000) <> 0 then
  begin
    dlg := TdlgFindMessageOnInternet.Create(Self);
    try
      dlg.edMessageID.Text := mid;
      dlg.ShowModal;
    finally
      dlg.Free;
    end;
  end
  else
    GotoMessageOnInternet(Handle, XNOptions.SearchInternetURLStub, mid);
end;

procedure TfmMain.actArticleMarkBranchAsReadExecute(Sender: TObject);
var
  mark: Boolean;
  art, article: TArticleBase;
begin
  article := GetNodeArticle(vstArticles.GetFirstSelected);
  if not Assigned(article) then Exit;

  art := article;
  ForEachArticleInBranch(article, DoCheckArticleRead, 0);
  mark := fIteratorFailed;

  ForEachArticleInBranch(art, DoMarkAsReadArticle, LPARAM(mark));
  vstArticles.Invalidate;
  Refresh_vstSubscribed;
end;

procedure TfmMain.actArticleMarkMessageAsInterestingExecute(Sender: TObject);
var
  article: TArticleBase;
  flagIt: Boolean;
begin
  article := GetFocusedArticle;
  if Assigned(article) then
  begin
    flagIt := not article.IsInteresting;
    ForEachSelectedArticle(DoFlagArticle, LPARAM(flagIt));
  end;
  vstArticles.Invalidate;
end;

procedure TfmMain.actToolsTestReadlnDelayExecute(Sender: TObject);
var
  dlg: TdlgReadLnDelay;
begin
  dlg := TdlgReadLnDelay.Create(nil);
  try
    dlg.edReadLnDelay.Text := IntToStr(gReadLnDelay);
    if dlg.ShowModal = mrOK then
      gReadLnDelay := StrToIntDef(dlg.edReadLnDelay.Text, 0);
  finally
    dlg.Free;
  end;
end;

procedure TfmMain.actSearchFindNoRepliesExecute(Sender: TObject);
begin
  fDontMarkOnLeave := True;
  try
    NextArticle([naNoReplies, naCanLeaveGroup, naCircularAccounts], GetFocusedArticle);
  finally
    fDontMarkOnLeave := False;
  end;
end;

procedure TfmMain.actSearchFindUnreadNoRepliesExecute(Sender: TObject);
begin
  fDontMarkOnLeave := True;
  try
    NextArticle([naNoReplies, naCanLeaveGroup, naUnreadOnly, naCircularAccounts], GetFocusedArticle);
  finally
    fDontMarkOnLeave := False;
  end;
end;

procedure TfmMain.vstBookmarkFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  ma: TMarkedArticle;
  art: TArticleBase;
  obj: PObject;
  grp: TSubscribedGroup;
begin
  if fUpdatingBookmark then Exit;
  obj := PObject(vstBookmark.GetNodeData(node));
  if Assigned(obj) then
  begin
    ma := TMarkedArticle(obj^);
    if Assigned(ma) then
    begin
      art := NNTPAccounts.FindMsgID(ma.Account, ma.Group, ma.RawMessageID);
      if not Assigned(art) then
      begin
        grp := GetFocusedGroup;
        if Assigned(grp) then
          art := NNTPAccounts.FindMsgID(grp.Owner.AccountName, grp.Name, ma.RawMessageID);
        if not Assigned(art) then
          art := NNTPAccounts.FindMsgID('', '', ma.RawMessageID);
      end;
      GoToArticle(art);
    end;
  end;
end;

procedure TfmMain.vstArticlesAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
var
  idx: Integer;
begin
  if hpeSortGlyph in Elements then
    with PaintInfo do
      if ShowSortGlyph then
      begin
        if vstArticles.Header.SortDirection = VirtualTrees.sdAscending then
          idx := 34
        else
          idx := 33;
        ImageList1.Draw(TargetCanvas, SortGlyphPos.X, SortGlyphPos.Y, idx);
      end;
end;

procedure TfmMain.vstArticlesHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if fHeaderSortCol = 6 then
    Elements := Elements + [hpeSortGlyph];
end;

procedure TfmMain.actArticleMarkThreadAsInterestingExecute(Sender: TObject);
var
  mark: Boolean;
  article: TArticleBase;
begin
  article := GetNodeArticle(vstArticles.GetFirstSelected);

  ForEachArticleInThread(article, DoCheckArticleFlagged, 0);
  mark := fIteratorFailed;

  ForEachArticleInThread(article, DoFlagArticle, LPARAM(mark));
  vstArticles.Invalidate;
  Refresh_vstSubscribed;
end;

procedure TfmMain.actNewsgroupMakeDormantExecute(Sender: TObject);
var
  st: string;
  c: Integer;
begin
  c := vstSubscribed.SelectedCount;

  if c > 0 then
  begin
    st := IfThen(c = 1, GetFocusedGroup.Name, rstSelectedGroups);

    if MessageDlg(Format(rstConfirmMakeDormant, [st]), mtConfirmation, [mbYes, mbNo], 0) = idYes then
    begin
      ForEachSelectedGroup(MakeDormant, False);
      NNTPAccounts.SaveToRegistry;
      fPrevArticle := nil;
    end;

    refresh_vstSubscribed;
    refresh_vstArticles;
  end;
end;

procedure TfmMain.tbMainCustomized(Sender: TObject);
begin
  SaveToolbarLayout;
end;


procedure TfmMain.tbMainCustomizeReset(Sender: TObject);
begin
  SaveToolbarLayout;
end;


procedure TfmMain.vstArticlesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if fClicked then
  begin
    fNextArticleStack.Clear;
    fClicked := False;
  end;
end;

end.