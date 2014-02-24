(*======================================================================*
 | NewsGlobals unit for NewsReader3                                     |
 |                                                                      |
 | Global constants and utility functions for XanaNews                  |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/01/2002  CPWW  Original                                  |
 | 1.17.2.5 8/2/2005    Q     ShortName function                        |
 | 1.17.3.0 18/3/2005   CPWW  Fixed problem in Wrap Stings when         |
 |                            wrapping sig separator.                   |
 *======================================================================*)

unit NewsGlobals;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, Classes, SysUtils, Forms, Ras, StrUtils, ActnList, ConTnrs, Dialogs, SyncObjs,
  unitExSettings, unitExRegSettings, unitExXMLSettings, XnClasses, XnRawByteStrings;

const
  WM_SETUP = WM_APP + $400;
  WM_UNSUBSCRIBE = WM_APP + $401;
  WM_GROUPSCHANGING = WM_APP + $402;
  WM_GROUPSCHANGED = WM_APP + $403;
  WM_AUTOEXPAND = WM_APP + $404;
  WM_FOREGROUND = WM_APP + $405;
  WM_NEWUSERCANCELLED = WM_APP + $406;
  WM_NAMETHREAD = WM_APP + $407;
  WM_GETCONNECTED = WM_APP + $408;
  WM_SHOWNEWSGROUPLIST = WM_APP + $409;
  WM_RSSSTATECHANGE = WM_APP + $40A;
  WM_APPLYCHANGES = WM_APP + $40B;
  WM_STATUS = WM_APP + $410;
  WM_FIRSTTIME = WM_APP + $411;

  WM_POSTANDCLOSE = WM_APP + $420; // Posting window messages
  WM_SETCODEPAGE = WM_APP + $422;
  WM_RETRYSETMSG = WM_APP + $423;
  WM_SPECIALKEY = WM_APP + $424;
  WM_SETIDENTITY = WM_APP + $425;

  WM_DELAYEDRESIZE = WM_APP + $430;

  DefaultMaxLineLength = 72;

type
  TTransfer = (bit7, bit8, iso2022jp);
  CSET = set of AnsiChar;
  TTextPartStyle = (tpNNTP, tpMIME, tpQuotedPrintable, tpFlowed);
  TPostingStyle = (psBottom, psTop);


  TActionDefault = class
  private
    fAction: TCustomAction;
    fShortcut: TShortcut;
  public
    constructor Create(AAction: TCustomAction);
    property Action: TCustomAction read fAction;
    property Shortcut: TShortcut read fShortcut;
  end;

  TBozoAction = (baIgnore, baMarkAsRead, baIgnoreThread, baMarkAsReadThread, baDontDownload);

var
  gDefaultActions: TObjectList = nil;
  gReadLnDelay: Integer = 0;

resourcestring
  rstConfirmMakeDormant = 'Making a group dormant removes it from all batches and sets its default actions to ''do nothing'''#13#10#13#10'Are you sure you want to make %s dormant?';
  rstConfirmUnsubscribe = 'Are you sure you want to delete all messages for %s?';
  rstSelectedGroups = 'the selected groups';
  rstOutstandingMessages = 'There are unposted messages in the queue.  Do you want to save them so that they can be sent when you next run XanaNews?';
  rstOutstandingEditors = 'There are open message editors.  Do you want to discard their contents?';
  rstSubject = 'Subject';
  rstPostingHost = 'NNTP Posting Host';
  rstOK = 'OK';
  rstNext = 'Next';
  rstQueued = 'Queued';
  rstDialling = 'Dialling %s';
  rstConnectedTo = 'Connected to %s';
  rstConnectingTo = 'Connecting to %s';
  rstDone = 'Done';
  rstFrom = 'from';
  rstCloseConnection = 'Do you want to disconnect from the internet?';

  rstQueuedPending = 'Queued.  %s will go next';
  rstGettingArticles = 'Getting articles for %s';
  rstGettingFullArticle = 'Getting article %d.  (%d left)';
  rstQueuedBusy = 'Queued.  Waiting for %s';
  rstGetNewsgroupsFor = 'Get newsgroups for ';
  rstGettingNewsgroupsFor = 'Getting newsgroups for ';
  rstGetArticleListFor = 'Get article list for ';
  rstAboutToGetArticle = 'About to get article';
  rstGetArticleBodyFrom = 'Get article body from ';

  rstAnyAvailable = 'Any Available';
  rstDeleteMessage = 'Are you sure you want delete all account details and messages for %s?';
  rstSelectedAccounts = 'the selected accounts';

  rstArticleFolders = 'Archived Messages';
  rstPurgedMessages = 'Purged Messages';
  rstDefaultFolderName = '%s - Messages';
  rstIsNewAccount = '%s is a new account.  Do you want to download its newsgroups list now?';
  rstNewGroupsAvaliable = 'There are new newsgroups available on %s.  Do you want to review them now?';
  rstAdminOnly = 'You can only change the default newsreader if you are an Administrator or Power User';
  rstConfirmDefaultNewsreader = 'Settings for the previous default newsreader will be lost.'#13#10#13#10'Are you sure you want to make XanaNews your default newsreader?';
  rstDefaultIdentity = 'Default Identity';

  rstMail = 'Mail';
  rstRSSFeeds = 'RSS Feeds';
  rstDefaultMailAccount = 'Default E-Mail Program';
  rstThisAccount = 'This Account';
  rstVersionAvailable = 'Version %s available';

  rstNewsgroupsFor = 'Newsgroups for %s';
  rstNone = 'None';
  rstBadBookmarkFile = 'Bookmark file %s is invalid';

  rstMessageTree = 'Message Tree';
  rstMessagePane = 'Message Pane';
  rstMainForm = 'Main Form';
  rstColorsFonts = 'Colours & Fonts';
  rstColorsFontsHelp = 'Select an item in the tree below to adjust its colours and fonts.';
  rstColorFontMessageTreeHelp = 'Colour & Font settings for all Message Tree elements.  You can adjust these individually using the sections below.';
  rstColorFontMessagePaneHelp = 'Font settings for all Message Pane elements.  You can adjust these individually using the sections below.';
  rstColorFontMainFormHelp = 'Font settings for all Main Form elements.  You can adjust these individually using the sections below.';
  rstDefaultSettings = 'Default Settings';
  rstDefaultSettingsHelp = 'The default settings in this section can be overridden per account in ''Account Properties'' or per newsgroup in ''Newsgroup Properties''.';
  rstDerivedSettings = 'Derived Settings';
  rstDerivedMailSettingsHelp = 'The settings in this section are derived from those held in Tools/Options.  You can override them here for this particular mail account.';
  rstDerivedAccountSettingsHelp = 'The settings in this section are derived from those held in Tools/Options.  You can override them here for this particular account.';
  rstDerivedNewsgroupSettingsHelp = 'The settings in this section are derived from those held in Account Properties.  You can override them here for this particular newsgroup.';

  rstRegularMessages = 'Regular Messages';
  rstMessagesToMe = 'Messages To Me';
  rstMyMessages = 'My Messages';
  rstXanaNewsMessages = 'XanaNews Messages';
  rstDormantMessages = 'Dormant Messages';
  rstRepliesToMyMessages = 'Replies to my Messages';
  rstIgnoredMessages = 'Ignored Messages';
  rstChildlessMessages = 'Message Without Replies';
  rstInterestingMessages = 'Interesting Messages';
  rstRegularText = 'Regular Text';
  rstHeaderText = 'Header Text';
  rstSignatureText = 'Signatures Text';
  rstLevel1QuoteText = 'Level 1 Quotes Text';
  rstLevel2QuoteText = 'Level 2 Quotes Text';
  rstLevel3QuoteText = 'Level 3 Quotes Text';
  rstMessageEditor = 'Message Editor';
  rstNewsgroupTree = 'Newsgroup Tree';
  rstBasicElements = 'Basic elements';
  rstMenu = 'Menu';
  rstToolBar = 'Toolbar';
  rstMessageDetailsPanel = 'Message Details Panel';
  rstStandardHelp = 'Colour and Font Settings for %s';
  rstStandardHelp1 = 'Colour and Font Settings for the %s';
  rstStandardHelp2 = 'Font Settings for the %s';

type
  TThreadOrder = (toThreaded, toChronological);
  TThreadSortOrder = (soDate, soSubject, soAuthor, soLines, soMessageNo, soPostingHost, soNewestMessage);
  TThreadSortDirection = (sdAscending, sdDescending);
  PObject = ^TObject;

var
  RasEntries: array of TRasEntryName;
  gLatestVersion: string = '~';
  gDeserveMedals: string = '~';
  NewVersion: string = '~';
  gAppTerminating: Boolean = False;
  gLogFlag: Boolean = False;
  gAudiblePerformanceCues: Boolean = False;
  gProductVersion: string = '';
  gExSettingsClass: TExSettingsClass = TExRegSettings;
  gExSettingsFile: string = '';

procedure FixHeaders(hdrs: TAnsiStrings);
function ProductVersion: string;
procedure UseXMLSettings(const fn: string);

procedure WrapStrings(m: TStrings; maxLen: Integer; textPartStyle: TTextPartStyle; nest, strictSigSep: Boolean);
procedure FixQuotes(s: TStrings; wrap: Boolean; maxLineLen: Integer; const quoteLineMarker: string; trimSig, strictSigSep: Boolean);
procedure PurgeDirectory(const dirName: string);
function ShortGroupName(const groupName: string): string;
function FairlyShortGroupName(const groupName: string): string;
function WideROT13(const st: string): string;
function WideReverseString(const st: string): string;
procedure LoadRASEntries;
function HeaderCharset(h: string): string;
procedure DecodeFromEMail(const from: RawByteString; var fromName, fromEMail: string; defCP: Integer);
function DecodeHeader(const header: RawByteString; defCP: Integer = -1): string;
function DecodeSubject(const subject: RawByteString; defCP: Integer): string;
function GenerateMessageID(const product, stub, host: string): RawByteString;
function EncodeHeader(const header: RawByteString; codepage: Integer; from: Boolean; headerLength: Integer): RawByteString;
procedure SetTempStatusMessage(const msg: string; pos, max: word);
function SafeDateTimeToInternetStr(const Value: TDateTime; const AIsGMT: Boolean = False): string;
function FixedGMTToLocalDateTime(S: RawByteString): TDateTime;
function RawGMTToLocalDateTime(S: string): TDateTime;
procedure UpdateGlobalOffsetFromUTC;
function ComputerName: string;
procedure ClearSynchronizedMethods;
function FixFileNameString(const st: string): string;
procedure SetLatestVersion(const v: string);
procedure SetDeserveMedals(const v: string);
function GetLatestVersion: string;
function GetDeserveMedals: string;
function ShortName(const Name: string): string;
function HTMLClipboardFormat: Integer;
function ForeName(const name: string): string;
procedure AdjustFormConstraints(form: TForm);
//function AdjustForLargeFonts(x: Integer): Integer;
function CreateExSettings(cls: TExSettingsClass = nil): TExSettings;
function CreateChildSettings(parent: TExSettings; const section: string = ''): TExSettings;


implementation

uses
  unitSearchString, idCoder, idCoderMIME, idCoderQuotedPrintable, unitCharsetMap,
  IdGlobal, IdGlobalProtocols, unitExFileSettings, XnCoderQuotedPrintable, unitLog;

var
  gLatestVersionSync: TCriticalSection = nil;
  gHTMLClipboardFormat: Integer = 0;
  gOffsetFromUTC: TDateTime = 0.0;

(*----------------------------------------------------------------------*
 | FixHeaders                                                           |
 |                                                                      |
 | Nowhere is there more dodginess than in message headers!             |
 |                                                                      |
 | See RFC2822 for the full lowdown.                                    |
 |                                                                      |
 | This function.                                                       |
 |                                                                      |
 |   Replaces tabs with spaces.  Tabs are legal, but unusual.           |
 |   Replaces CR & LF with spaces.  They're legal but unusual.          |
 |   Merges split lines.                                                |
 |   Replaces runs of whitespace with a single space character          |
 |   RFC 850 says that 'references' should be <id>blank<id> - but       |
 |   people sometimes leave out the blank.                              |
 |                                                                      |
 | Parameters:                                                          |
 |   hdrs: TStrings                                                     |
 |                                                                      |
 | The function returns None                                            |
 |                                                                      |
 | ** TODO - Any text within brackets (including the brackets) is a     |
 |           comment.  Remove this                                      |
 *----------------------------------------------------------------------*)
procedure FixHeaders(hdrs: TAnsiStrings);
var
  i, j, p: Integer;
  s, s1: RawByteString;
  ch, lastch: AnsiChar;
  inQuote: Boolean;
begin
  i := 1;
  while i < hdrs.Count do              // Merge split lines (unfolding).
  begin
    s := hdrs[i];

    if Length(s) = 0 then
      hdrs.Delete(i)
    else
      if s[1] in [' ', #9] then        // If a line starts with a whitespace
      begin                            // then append it to the previous line.
        hdrs[i - 1] := hdrs[i - 1] + s;
        hdrs.Delete(i);
      end
      else
        Inc(i);
  end;

  for i := 0 to hdrs.Count - 1 do
  begin
    s := hdrs[i];

    SetLength(s1, Length(s));
    inQuote := False;
    lastch := #0;
    p := 1;

    for j := 1 to Length(s) do
    begin
      ch := s[j];

      if ch in [#9, #10, #13] then     // Replace these with spaces
        ch := ' ';

      if ch = '"' then
        inQuote := not inQuote;

      if ch <> ' ' then
        lastch := #0;
                                       // Delete multiple spaces - unless they're
                                       // in quotes.
      if inQuote or (lastch <> ' ') then
      begin
        s1[p] := ch;
        Inc(p);
      end;

      lastch := ch;
    end;

    SetLength(s1, p - 1);
    if RawSameText(Copy(s1, 1, 11), 'References:') then
      s1 := RawStringReplace(s1, '><', '> <', [rfReplaceAll])  // Fix dodgy references
    else if RawSameText(Copy(s1, 1, 5), 'Face:') then
    begin
      s1 := RawStringReplace(s1, ' ', '', [rfReplaceAll]);      // Remove folding spaces from Face headers
      s1 := RawStringReplace(s1,  #9, '', [rfReplaceAll]);
    end;

    hdrs[i] := RawTrim(s1);
  end;
end;

function ProductVersion: string;
var
  size, zero: DWORD;
  buffer, pBuffer: pointer;
  info: PVSFixedFileInfo;
begin
  if gProductVersion <> '' then
  begin
    Result := gProductVersion;
    Exit;
  end;

  Result := '';         // Get it from the versioninfo resource
  size := GetFileVersionInfoSize(PChar(ParamStr(0)), zero);
  if size > 0 then
  begin
    GetMem(buffer, size);
    try
      if not GetFileVersionInfo(PChar(paramStr(0)), zero, size, buffer) then
        RaiseLastOSError;

      if not VerQueryValue(buffer, '\', pBuffer, size) then
        RaiseLastOSError;

      info := PVSFixedFileInfo(pBuffer);

      Result := Format('%d.%d.%d.%d', [HiWord(info^.dwProductVersionMS),
                                       LoWord(info^.dwProductVersionMS),
                                       HiWord(info^.dwProductVersionLS),
                                       LoWord(info^.dwProductVersionLS)]);
    finally
      FreeMem(buffer);
    end;
  end;
  gProductVersion := Result;
end;

function IsSigSep(st: string; strct, quoted: Boolean): Boolean;
begin
  Result := False;
  if Length(st) >= 2 then
    if quoted then
    begin
      if (st[1] = '>') or (st[1] = '|') then
      begin
        Delete(st, 1, 1);
        if st[1] = ' ' then
          Delete(st, 1, 1);
        Result := IsSigSep(st, strct, False);
      end;
    end
    else
      Result := (st = '-- ') or (not strct and (st = '--'));
end;

(*----------------------------------------------------------------------*
 | procedure WrapStrings                                                |
 |                                                                      |
 | Wrap strings so that they're not longer than the specified maximum   |
 | length.                                                              |
 |                                                                      |
 | Parameters:                                                          |
 |   m: TStrings                // The strings to wrap                  |
 |   maxLen: Integer;           // The maximum length                   |
 |   quotedPrintable: Boolean;  // If the flag is set, convert the      |
 |                              // strings to 'quoted printable' format |
 |   nest: Boolean              // nest quoted text.  The quote level   |
 |                              // for each line must be in Objects [i] |
 |                                                                      |
 | The function returns None                                            |
 *----------------------------------------------------------------------*)
procedure WrapStrings(m: TStrings; maxLen: Integer; TextPartStyle: TTextPartStyle; nest, strictSigSep: Boolean);
var
  holdQuotedPrintable: Boolean;
  i, len, p, ml: Integer;
  s1, s2: string;
  ch: char;
  encoder: TXnEncoderQuotedPrintable;

  function IsURL(const S1: string): Boolean;
  begin
    Result := ContainsText(S1, '://') and not ContainsText(S1, ' ');
  end;

begin
  holdQuotedPrintable := False;
  i := 0;
  ml := maxLen;
  encoder := nil;
  try
    while i < m.Count do
    begin
      s1 := m[i];
      if TextPartStyle = tpFlowed then
      begin
        if s1 <> '-- ' then
          s1 := TrimRight(s1);
        m[i] := s1;
      end;

      if nest then
      begin
        ml := maxLen - 2 * (NativeInt(m.Objects[i]) + 1);
        if ml <= 0 then
        begin
          Inc(i);
          Continue;
        end;
      end;

      // Replace all equal symbols with '=3D' if quoted-printable
      if not holdQuotedPrintable and (TextPartStyle = tpQuotedPrintable) then
      begin
        if encoder = nil then
          encoder := TXnEncoderQuotedPrintable.Create(nil);
        s1 := encoder.Encode(s1);
        m[i] := s1;
      end;

      len := Length(s1);

      holdQuotedPrintable := False;
      if (len > ml) and not IsURL(s1) then
      begin
        p := ml;
        while (p > 0) and not (s1[p] in [#9, ' ']) do
          Dec(p);

        if p = 0 then
          if TextPartStyle = tpFlowed then
          begin
            Inc(i);
            Continue;
          end
          else
            p := ml;

        s2 := Copy(s1, 1, p);
        if TextPartStyle = tpQuotedPrintable then
          s2 := s2 + '='
        else
          if TextPartStyle = tpFlowed then
          begin
            if s2[Length(s2)] <> ' ' then
              s2 := s2 + ' ';
          end
          else
            s2 := TrimRight(s2);
        m[i] := s2;

        if not nest or (i + 1 = m.Count) or (m[i + 1] = '') or
           IsSigSep(m[i + 1], strictSigSep, False) or
           (m.Objects[i] <> m.Objects[i + 1]) then
        begin
          s2 := Copy(s1, p + 1, MaxInt);
          if textPartStyle <> tpQuotedPrintable then
            s2 := Trim(s2);
          m.InsertObject(i + 1, s2, m.Objects[i]);
        end
        else
        begin
          s1 := Trim(Copy(s1, p + 1, MaxInt));
          if s1 <> '' then
          begin
            ch := s1[Length(s1)];

            if ch in ['.', '?', '!', ':'] then
              s1 := s1 + ' ';

            s2 := m[i + 1];
            if (textPartStyle = tpQuotedPrintable) then
            begin
              if encoder = nil then
                encoder := TXnEncoderQuotedPrintable.Create(nil);
              s2 := encoder.Encode(s2);
            end;

            m[i + 1] := s1 + ' ' + s2;
          end;
        end;
        holdQuotedPrintable := True;
      end;
      Inc(i);
    end;
  finally
    encoder.Free;
  end;
end;

procedure FixQuotes(s: TStrings; wrap: Boolean; maxLineLen: Integer;
  const quoteLineMarker: string; trimSig, strictSigSep: Boolean);
var
  i, p: Integer;
  sigLine: Integer;
  st: string;
begin
  for i := 0 to s.Count - 1 do
  begin
    repeat
      st := quoteLineMarker;
      p := Pos(st, s[i]);
      if p = 0 then
      begin
        st := '>';
        p := Pos(st, s[i]);
        if p = 0 then
        begin
          st := '|';
          p := Pos(st, s[i]);
        end;
      end;
      if p = 1 then
      begin
        s.Objects[i] := TObject(NativeInt(s.Objects[i]) + 1);
        s[i] := Copy(s[i], Length(st) + 1, maxInt);
      end;
    until p <> 1;
  end;

  if wrap then
    WrapStrings(s, maxLineLen, tpNNTP, True, strictSigSep);

  if quoteLineMarker <> '' then
  begin
    for i := 0 to s.Count - 1 do
      for p := 0 to NativeInt(s.Objects[i]) do
        s[i] := quoteLineMarker + s[i];

    s.Insert(0, '');
  end;

  sigLine := -1;
  if trimSig then
    for i := 0 to s.Count - 1 do
      if IsSigSep(s[i], strictSigSep, True) then
      begin
        sigLine := i;
        Break;
      end;

  if sigLine >= 0 then
    while s.Count > sigLine do
      s.Delete(sigLine);

  while s.Count > 0 do
    if s[s.Count - 1] = quoteLineMarker then
      s.Delete(s.Count - 1)
    else
      Break;
end;

procedure PurgeDirectory(const dirName: string);
var
  f: TSearchRec;
  st: string;
  err: Integer;
begin
  // Delete a directory and it's sub-directories.
  st := dirName;
  UniqueString(st);
  if FindFirst(st + '\' + '*.*', faAnyFile, f) = 0 then
  try
    repeat
      if (f.Attr and (faReadOnly or faSysFile or faHidden)) <> 0 then
        FileSetAttr(st + '\' + f.Name, f.Attr and not (faReadOnly or faSysFile or faHidden));

      if (f.Attr and faDirectory) <> 0 then
      begin
        if (f.Name <> '.') and (f.name <> '..') then
          PurgeDirectory(st + '\' + f.Name)
      end
      else
        if not DeleteFile(st + '\' + f.Name) then
          RaiseLastOSError;
    until FindNext(f) <> 0;
  finally
    FindClose(f);
  end;

  if not RemoveDir(st) then
  begin
    err := GetLastError;
    if (err <> ERROR_FILE_NOT_FOUND) and (err <> ERROR_PATH_NOT_FOUND) then
      RaiseLastOSError;
  end;
end;

function ShortGroupName(const groupName: string): string;
var
  c: Integer;
begin
  c := Length(groupName);

  while (c > 0) and (groupName[c] <> '.') do
    Dec(c);

  if c = 0 then
    Result := groupName
  else
    Result := Copy(groupName, c, MaxInt);

  while c > 0 do
  begin
    repeat
      Dec(c);
    until (c = 0) or (groupName[c] = '.');

    if c = 0 then
      Result := groupName[c + 1] + Result
    else
      Result := Copy(groupName, c, 2) + Result;
  end;
end;

function FairlyShortGroupName(const groupName: string): string;
var
  c: Integer;
begin
  c := Length(groupName);

  while (c > 0) and (groupName[c] <> '.') do
    Dec(c);

  if c = 0 then
    Result := groupName
  else
    Result := ShortGroupName(Copy(groupName, 1, c - 1)) + Copy(groupName, c, MaxInt);
end;


function WideROT13(const st: string): string;
var
  i, l: Integer;
  ch: WideChar;
begin
  // Return a ROT-13 en/decryption of a string.  Call it again to de/encode the string.
  l := Length(st);
  SetLength(Result, l);

  for i := 1 to l do
  begin
    ch := st[i];
    if ((ch >= 'A') and (ch <= 'M')) or ((ch >= 'a') and (ch <= 'm')) then
      ch := WideChar(Ord(ch) + 13)
    else
      if ((ch >= 'N') and (ch <= 'Z')) or ((ch >= 'n') and (ch <= 'z')) then
        ch := WideChar(Ord(ch) - 13);
    Result[i] := ch;
  end;
end;

function WideReverseString(const st: string): string;
var
  i, l: Integer;
begin
  l := Length(st);
  SetLength(Result, l);
  for i := 1 to l do
    Result[i] := st[l - i + 1];
end;

procedure LoadRASEntries;
var
  ce, cb, rv: DWORD;
begin
  // Populate the RAS Entries array with available diallup connections
  try
    if InitRASLibrary then
    try
      ce := 10;
      repeat
        SetLength(RasEntries, ce);
        cb := ce * SizeOf(TRasEntryName);
        FillChar(RasEntries[0], cb, 0);
        RasEntries[0].dwSize := SizeOf(TRasEntryName);
        rv := RasEnumEntries(nil, nil, @RasEntries[0], cb, ce);

        case rv of
          ERROR_SUCCESS: SetLength(RasEntries, ce);
          ERROR_BUFFER_TOO_SMALL:;
        else
          SetLength(RasEntries, 0);
          SetLastError(rv);
          RaiseLastOSError;
        end;
      until rv <> ERROR_BUFFER_TOO_SMALL;
    finally
      FreeRASLibrary;
    end
    else
      SetLength(RasEntries, 0);
  except
    SetLength(RasEntries, 0);
  end;
end;

function HeaderCharset(h: string): string;
var
  p: Integer;
begin
  p := Pos('=?', h);
  if p > 0 then
  begin
    h := Copy(h, p + 2, MaxInt);
    Result := ExtractString('?', h);
  end
  else
    Result := ''
end;

function DequotedCString(const st: RawByteString): RawByteString;
var
  i, l: Integer;
begin
  Result := st;
  l := Length(Result);
  if (l > 0) and (st[1] = '"') then
  begin
    Delete(Result, 1, 1);
    Dec(l);
  end;

  if (l > 0) and (Result[l] = '"') then
  begin
    Delete(Result, l, 1);
    Dec(l);
  end;

  i := 0;
  while i < l do
  begin
    Inc(i);
    if Result[i] = '\' then
    begin
      Delete(Result, i, 1);
      Dec(l);
      if i <= l then
        case Result[i] of
          't': Result[i] := #9;
          'r', 'n': Result[i] := ' ';
        end;
    end;
  end;
end;

procedure DecodeFromEMail(const from: RawByteString; var fromName, fromEMail: string; defCP: Integer);
var
  p: Integer;
  rawFrom, rawEMail: RawByteString;
begin
  p := RawPos('<', from);

  if p > 0 then
  begin
    rawEMail := from;
    rawFrom := DequotedCString(RawSplitString('<', rawEMail));
    if (Length(rawEMail) > 0) and (rawEMail[Length(rawEMail)] = '>') then
      Delete(rawEMail, Length(rawEMail), 1);
  end
  else
  begin
    p := RawPos('(', from);
    if p > 0 then
    begin
      rawFrom := from;
      rawEMail := RawSplitString('(', rawFrom);
      if (Length(rawFrom) > 0) and (rawFrom[Length(rawFrom)] = ')') then
        Delete(rawFrom, Length(rawFrom), 1);
    end
    else
    begin
      rawFrom := from;
      rawEMail := '';
    end;
  end;

  fromName := DecodeHeader(rawFrom, defCP);
  fromEMail := string(rawEMail);

  if fromName = '' then
    fromName := string(from);
end;

{ TActionDefault }

constructor TActionDefault.Create(AAction: TCustomAction);
begin
  fAction := AAction;
  fShortcut := AAction.ShortCut;
end;

var
  gDecoderMIME: TidDecoderMIME = nil;
  gDecoderQuotedPrintable: TIdDecoderQuotedPrintable = nil;

// Searches Data for an RFC-2047 chunk, starting at cFrom.
// If a chunk is found, cFrom and cTo contain positions of the first
// and last character, respectively.
// Format:  "=?" charset "?" encoding "?" encoded-text "?="
// Example: =?UTF-8?Q?ascii?=
function FindChunk(const Data: RawByteString; var cFrom, cTo: integer): Boolean;
const
  ChunkChars = ['A'..'Z'] + ['a'..'z'] + ['0'..'9'] + ['=', '?', '-', '_'];
var
  State: Byte;
  i: Integer;
begin
  Result := False;
  i := cFrom;
  State := 0;
  while i <= Length(Data) do
  begin
    case State of
      0: if Data[i] = '=' then
         begin
           State := 1;
           cFrom := i;
         end;
      1: if Data[i] = '?' then
           State := 2
         else
           State := 0;
      2,3,4:
         // Fix: Some clients actually include spaces in chunks.
         if (State <> 4) and not (Data[i] in ChunkChars) then
           State := 0
         else
           if Data[i] = '?' then
             Inc(State);
      5: if Data[i] = '=' then
         begin
           cTo := i;
           Result := True;
           Exit;
         end
         else
           if Data[i] <> '?' then
             State := 4;
    end;
    Inc(i);
  end;
end;

procedure ControlsToSpaces(var Str: string);
var
  x: Integer;
begin
  for x := 1 to Length(Str) do
    if Ord(Str[x]) < 32 then
      Str[x] := ' ';
end;

// Decodes a RFC-2047 chunk into string.
// Expects a single, pre-stripped chunk as input ('ISO-8859-1?q?data')
function DecodeChunk(Input: RawByteString): string;
var
  data: string;
  enc: AnsiChar;
  L, x, cpnum: Integer;
  AttachSpace: Boolean;
  DecodedStream: TMemoryStream;
  EncodedStream: TMemoryStream;
  S: RawByteString;
begin
  Result := '';
  x := RawPos('?', Input);
  // Checks for encoding byte, '?', and at least one byte of data.
  if Length(Input) < x + 3 then
    Exit;

  // Encoding should be exactly one character
  if Input[x + 2] <> '?' then
    Exit;

  cpnum := MIMECharsetNameToCodePage(string(Copy(Input, 1, x - 1)));
  enc := Input[x + 1];
  data := string(Copy(Input, x + 3, maxint));

  DecodedStream := TMemoryStream.Create;
  try
    EncodedStream := TMemoryStream.Create;
    try
      if enc in ['b', 'B'] then
      begin
        AttachSpace := False;
        with TidDecoderMIME.Create(nil) do
        begin
          WriteStringToStream(EncodedStream, Data, IndyTextEncoding_ASCII);
          EncodedStream.Position := 0;
          try
            DecodeBegin(DecodedStream);
            Decode(EncodedStream);
            DecodeEnd;
          finally
            Free;
          end;
        end;
      end
      else
      begin
        // TidDecoderQuotedPrintable does not deal with underscore encoded spaces
        data := StringReplace(data, '_', ' ', [rfReplaceAll]);
        AttachSpace := data[Length(data)] = ' ';
        with TXnDecoderQuotedPrintable.Create(nil) do
        begin
          WriteStringToStream(EncodedStream, Data, IndyTextEncoding_ASCII);
          EncodedStream.Position := 0;
          try
            DecodeBegin(DecodedStream);
            Decode(EncodedStream);
            DecodeEnd;
          finally
            Free;
          end;
        end;
      end;
    finally
      EncodedStream.Free;
    end;

    L := DecodedStream.Size;
    if L > 0 then
    begin
      SetLength(S, L);
      Move(DecodedStream.Memory^, S[1], L);
      Result := AnsiStringToWideString(S, cpnum);
    end
    else
      Result := '';

    // Unfortunately, the Decoder will also trim trailing spaces
    if AttachSpace then
      Result := Result + ' ';
  finally
    DecodedStream.Free;
  end;
end;

function DecodeHeader(const header: RawByteString; defCP: Integer = -1): string;
const
  CP_UTF_16 = 1200;
var
  bt8: Boolean;
  I: Integer;
  chkStart, chkEnd, lastChkEnd: Integer;
  encoding: RawByteString;
  st: RawByteString;
  x: Integer;
begin
  // Some clients use 8 bit "ascii" in the header, if thats' the case use the
  // article codepage and if there is none, use the user's default setting.
  bt8 := False;
  for i := 1 to Length(header) do
    if not (header[i] in [' '..'~']) then
    begin
      bt8 := True;
      Break;
    end;

  if bt8 and (defCP <> -1) then
  begin
    Result := AnsiStringToWideString(header, defCP);
  end
  else
  begin
    Result := '';
    lastChkEnd := 0;
    chkStart := 1;

    st := header;
    // Remove folding spaces from multiple MIME encoded words.
    if FindChunk(st, chkStart, chkEnd) then
    begin
      encoding := Copy(st, chkStart + 2, chkEnd - chkStart - 3);
      x := RawPos('?', encoding);
      if (Length(encoding) >= (x + 3)) and (encoding[x + 2] = '?') and (encoding[x + 1] in ['q', 'Q']) then
      begin
        encoding := Copy(encoding, 1, x + 2);
        st := RawStringReplace(st, '?= =?' + encoding, '', [rfReplaceAll]);
      end;
    end;

    chkStart := 1;
    while FindChunk(st, chkStart, chkEnd) do
    begin
      Result := Result +
        string(Copy(st, lastChkEnd + 1, chkStart - lastChkEnd - 1)) +
        DecodeChunk(Copy(st, chkStart + 2, chkEnd - chkStart - 3));
      lastChkEnd := chkEnd;
      chkStart := chkEnd + 1;
      // If a chunk is followed by ' =?', ignore it
      if (Length(st) > chkStart + 8) and (st[chkStart] in [' ', #8]) and
         (st[chkStart+1] = '=') and (st[chkStart+2] = '?') then
        Inc(lastChkEnd);
    end;
    Result := Result + Copy(string(st), lastChkEnd + 1, MaxInt);
  end;
end;

function DecodeSubject(const subject: RawByteString; defCP: Integer): string;
begin
  Result := DecodeHeader(subject, defCP);
end;

var
  gmi: Integer = 0;

function GenerateMessageID(const product, stub, host: string): RawByteString;
var
  st, hs: string;

  function Enc36(i: Integer; padLen: Integer = 7): string;
  var
    i1: Integer;
  begin
    Result := '';
    while i > 0 do
    begin
      i1 := i mod 36;
      i := i div 36;
      if i1 < 10 then
        Result := Char(Ord('0') + i1) + Result
      else
        Result := Char(Ord('a') + i1 - 10) + Result;
    end;

    while Length(Result) < padLen do
      Result := '0' + Result;
  end;

begin
  st := product + Enc36(DateTimeToFileDate(now)) +
    Enc36(GetTickCount and MaxInt, 4) + Enc36(gmi, 3) + stub;
  Inc(gmi);
  if gmi >= 36 * 36 * 36 then
    gmi := 0;
  if SameText(host, 'LocalHost') then
    hs := 'xananews'
  else
    hs := host;
  Result := RawByteString('<' + st + '@' + hs + '>');
end;

const
  base64_tbl: array[0..63] of AnsiChar = (
    'A','B','C','D','E','F','G','H',      {Do not Localize}
    'I','J','K','L','M','N','O','P',      {Do not Localize}
    'Q','R','S','T','U','V','W','X',      {Do not Localize}
    'Y','Z','a','b','c','d','e','f',      {Do not Localize}
    'g','h','i','j','k','l','m','n',      {Do not Localize}
    'o','p','q','r','s','t','u','v',      {Do not Localize}
    'w','x','y','z','0','1','2','3',      {Do not Localize}
    '4','5','6','7','8','9','+','/');     {Do not Localize}

function EncodeHeader1(const Header: RawByteString; specials: CSET; HeaderEncoding: AnsiChar;
  TransferHeader: TTransfer; codePage, initialLength: Integer): RawByteString;
const
  SPACES: set of Char = [' ', #9, #10, #13, '''', '"'];    {Do not Localize}
var
  S, T: RawByteString;
  L, P, Q, R: Integer;
  B0, B1, B2: Integer;
  InEncode: Integer;
  NeedEncode: Boolean;
  csNeedEncode, csReqQuote: CSET;
  BeginEncode, EndEncode: RawByteString;
  ch: AnsiChar;
  mimeCharSet: RawByteString;
  extraBytes: Integer;

  procedure EncodeWord(P: Integer);
  const
    MaxEncLen = 75;
  var
    Q: Integer;
    EncLen: Integer;
    Enc1: RawByteString;
    ac: AnsiChar;
    first: Boolean;
  begin
    if headerEncoding = 'Q' then
      initialLength := initialLength + Length(T);
    first := True;
    if L < P then P := L + 1;
    Q := InEncode;
    InEncode := 0;
    EncLen := Length(BeginEncode) + 2;

    if headerEncoding = 'Q' then {quoted-printable} {Do not Localize}
    begin
      while Q < P do
      begin
        ac := S[Q];

        // Determine extra bytes needed to encode a *complete* multi-byte character.
        extraBytes := 0;
        if CodePage = CP_UTF8 then
        begin
          if (Byte(ac) and $F0) = $F0 then
            extraBytes := 9
          else if (Byte(ac) and $E0) = $E0 then
            extraBytes := 6
          else if (Byte(ac) and $C0) = $C0 then
            extraBytes := 3;
        end;

        if not (ac in csReqQuote) then
          Enc1 := ac
        else
        begin
          if ac = ' ' then {Do not Localize}
            Enc1 := '_'  {Do not Localize}
          else
            Enc1 := '=' + RawByteString(IntToHex(Ord(ac), 2)); {Do not Localize}
        end;

        if initialLength + EncLen + Length(Enc1) + extraBytes > MaxEncLen then
        begin
          initialLength := 0;
          if first then
            T := T + #13#10#9 + BeginEncode
          else
            T := T + EndEncode + #13#10#9 + BeginEncode;
          EncLen := Length(BeginEncode) + 2;
        end
        else
          if first then
            T := T + BeginEncode;
        first := False;

        T := T + Enc1;
        INC(EncLen, Length(Enc1));
        INC(Q);
      end;
    end
    else
    begin { base64 }
      while Q < P do
      begin
        if initialLength + EncLen + 4 > MaxEncLen then
        begin
          initialLength := 0;
          if first then
            T := T + #13#10#9 + BeginEncode
          else
            T := T + EndEncode + #13#10#9 + BeginEncode;
          EncLen := Length(BeginEncode) + 2;
        end
        else
          if first then
            T := T + BeginEncode;
        first := False;

        B0 := Ord(S[Q]);
        case P - Q of
          1: T := T + base64_tbl[B0 shr 2] + base64_tbl[B0 and $03 shl 4] + '=='; {Do not Localize}
          2: begin
               B1 := Ord(S[Q + 1]);
               T := T + base64_tbl[B0 shr 2] +
                 base64_tbl[B0 and $03 shl 4 + B1 shr 4] +
                 base64_tbl[B1 and $0F shl 2] + '='; {Do not Localize}
             end;
        else
          B1 := Ord(S[Q + 1]);
          B2 := Ord(S[Q + 2]);
          T := T + base64_tbl[B0 shr 2] +
            base64_tbl[B0 and $03 shl 4 + B1 shr 4] +
            base64_tbl[B1 and $0F shl 2 + B2 shr 6] +
            base64_tbl[B2 and $3F];
        end;
        INC(EncLen, 4);
        INC(Q, 3);
      end;
    end;
    T := T + EndEncode;
  end;

begin
  S := Header;
  mimeCharSet := RawCodePageToMIMECharsetName(codePage);
  headerEncoding := UpCase(headerEncoding);

  {Suggested by Andrew P.Rybin for easy 8bit support}
  if HeaderEncoding = '8' then
  begin
    Result := S;
    Exit;
  end;

  csNeedEncode := [#0..#31, #127..#255] + specials;
  csReqQuote := csNeedEncode + ['?', '=', '_', ' ']; {Do not Localize}
  BeginEncode := '=?' + mimeCharSet + '?' + headerEncoding + '?'; {Do not Localize}
  EndEncode := '?='; {Do not Localize}

  L := Length(S);
  P := 1;
  T := ''; {Do not Localize}
  InEncode := 0;
  while P <= L do
  begin
    Q := P;
    while (P <= L) and (S[P] in SPACES) do
      Inc(P);
    R := P;
    NeedEncode := False;

    // Find start of first word that needs encoding (in 'R')
    while (P <= L) do
    begin
      ch := s[P];
      if ch in SPACES then
        if (ch <> ' ') or not NeedEncode or (headerEncoding <> 'Q') then
          Break;

      if ch in csNeedEncode then
        NeedEncode := True;
      Inc(P);
    end;

    if NeedEncode then
    begin
      if InEncode = 0 then
      begin
        T := T + Copy(S, Q, R - Q);
        InEncode := R;
      end;
    end
    else
    begin
      if InEncode <> 0 then
        EncodeWord(Q);
      T := T + Copy(S, Q, P - Q);
    end;
  end;
  if InEncode <> 0 then
    EncodeWord(P);

  Result := T;
end;

function EncodeHeader(const header: RawByteString; codePage: Integer;
  from: Boolean; headerLength: Integer): RawByteString;
var
  raw: RawByteString;
  i: Integer;
  ansi: Boolean;
  coder: TXnEncoderQuotedPrintable;
  sl: TAnsiStringList;
begin
  ansi := True;
  if from then
  begin
    if header[1] <> '"' then
      raise Exception.Create('Invalid from');

    i := 2;
    raw := '';
    while (header[i] <> '"') and (i < Length(header)) do
    begin
      raw := raw + header[i];
      if not (header[i] in [' '..'~', #9, #10, #13]) then
        ansi := False;
      Inc(i);
    end;
  end
  else
  begin
    i := 1;
    while i < Length(header) do
      if not (header[i] in [' '..'~', #9, #10, #13]) then
      begin
        ansi := False;
        Break;
      end
      else
        Inc(i);
  end;

// TODO: Q or B-encoding (B-encoding seems to be more commonly accepted)
//       RFC says use Q when most of the text fits in normal ASCII.
// - Problem is that the current implementation of the B-encoding may
//   use folding within a multi-byte character, which is not allowed.
  if ansi or not from then
    Result := EncodeHeader1(header, [], 'Q', bit7, codePage, headerLength)
  else
  begin
    // It concerns a from name with non-ansi characters.
    coder := TXnEncoderQuotedPrintable.Create(nil);
    try
      sl := TAnsiStringList.Create;
      try
        sl.Text := raw;
        sl.LineBreak := '';
        coder.EncodeStrings(sl);
        raw := sl.Text;
      finally
        sl.Free;
      end;
      Result := '=?' + RawCodePageToMIMECharsetName(codePage) + '?Q?' + raw + '?= ' +
        RawTrim(Copy(header, i + 2, MaxInt));
    finally
      coder.Free;
    end;
  end;
end;

procedure SetTempStatusMessage(const msg: string; pos, max: word);
begin
  if (csDestroying in Application.MainForm.ComponentState) then Exit;
  SendMessage(Application.MainForm.Handle, WM_STATUS, WPARAM(PChar(msg)), MakeLParam(pos, max));
end;

function SafeDateTimeToInternetStr(const Value: TDateTime; const AIsGMT: Boolean = False): string;

  function FastLocalDateTimeToGMT(const Value: TDateTime; const AUseGMTStr: Boolean = False) : String;
  var
    wDay, wMonth, wYear: Word;
  begin
    // It's fast because it uses the global gOffsetFromUTC instead of asking windows for it each time.
    DecodeDate(Value, wYear, wMonth, wDay);
    Result := IndyFormat('%s, %d %s %d %s %s',    {do not localize}
                     [wdays[DayOfWeek(Value)], wDay, monthnames[wMonth],
                      wYear, FormatDateTime('HH":"nn":"ss', Value), {do not localize}
                      UTCOffsetToStr(gOffsetFromUTC, AUseGMTStr)]);
  end;
begin
  try
    Result := FastLocalDateTimeToGMT(Value, AIsGMT);
  except
    Result := 'Fri, 17 Nov 1961 08:00:00 GMT';
  end;
end;

// Expects a string as saved by Indy's function DateTimeToInternetStr().
// '%s, %.2d-%s-%.2d %s %s'
function FixedStrInternetToDateTime(var Value: RawByteString): TDateTime;
var
  P: PAnsiChar;
  Dt, Mo, Yr, Ho, Min, Sec: Word;

  function NextItem(var S: RawByteString): PAnsiChar;
  begin
    Result := P;
    if Result <> nil then
    begin
      while not (Result^ in [#0, ' ', ':']) do   {do not localize}
        Inc(Result);

      SetString(S, P, Result - P);
      if Result^ <> #0 then
        Inc(Result);
    end
    else
      S := '';
  end;

  function NextItemStr: RawByteString;
  begin
    P := NextItem(Result);
  end;

begin
  Result := 0.0;

  if Length(Value) = 0 then
    Exit;

  P := PAnsiChar(Value);

  try
    NextItemStr;                            // Day of Week, not used
    Dt  := RawStrToInt(NextItemStr);        // Day of Month
    Mo  := StrToMonth(string(NextItemStr)); // Month
    Yr  := RawStrToInt(NextItemStr);        // Year
    Ho  := RawStrToInt(NextItemStr);        // Hours
    Min := RawStrToInt(NextItemStr);        // Minutes
    Sec := RawStrToInt(NextItemStr);        // Seconds

    Value := P;                             // Remainder is GMT offset

    Result := EncodeDate(Yr, Mo, Dt) + EncodeTime(Ho, Min, Sec, 0);
  except
    Result := 0.0;
  end;
end;

function FixedGMTOffsetStrToDateTime(const S: RawByteString): TDateTime;
begin
  Result := 0.0;
  if Length(S) > 4 then
  begin
    if (s[1] = '-') or (s[1] = '+') then   {do not localize}
    begin
      try
        Result := EncodeTime(RawStrToInt(Copy(s, 2, 2)), RawStrToInt(Copy(s, 4, 2)), 0, 0);
        if s[1] = '-' then  {do not localize}
          Result := -Result;
      except
        Result := 0.0;
      end;
    end;
  end;
end;


// Expects a string as saved by Indy's function DateTimeToInternetStr().
// '%s, %.2d-%s-%.2d %s %s'
// Always returns date/time relative to GMT.
function FixedGMTToLocalDateTime(S: RawByteString): TDateTime;
var
  DateTimeOffset: TDateTime;
begin
  if s = '' then
    Result := 0
  else
  begin
    Result := FixedStrInternetToDateTime(S);
    if Length(S) < 5 then
      DateTimeOffset := 0.0
    else
      DateTimeOffset := FixedGMTOffsetStrToDateTime(S);
    // Apply GMT offset here
    if DateTimeOffset < 0.0 then
      Result := Result + Abs(DateTimeOffset)
    else
      Result := Result - DateTimeOffset;
    // Apply local offset
    Result := Result + gOffSetFromUTC;
  end;
end;

function RawStrInternetToDateTime(var Value: string; var VDateTime: TDateTime): Boolean;
var
  i: Integer;
  Dt, Mo, Yr, Ho, Min, Sec: Word;
  sYear, sTime, sDelim: string;
  //flags for if AM/PM marker found
  LAM, LPM : Boolean;

  procedure ParseDayOfMonth;
  begin
    Dt := IndyStrToInt(Fetch(Value, sDelim), 1);
    Value := TrimLeft(Value);
  end;

  procedure ParseMonth;
  begin
    Mo := StrToMonth(Fetch(Value, sDelim));
    Value := TrimLeft(Value);
  end;

begin
  Result := False;
  VDateTime := 0.0;

  LAM := False;
  LPM := False;

  Value := Trim(Value);
  if Length(Value) = 0 then begin
    Exit;
  end;

  try
    {Day of Week}
    if StrToDay(Copy(Value, 1, 3)) > 0 then begin
      //workaround in case a space is missing after the initial column
      if CharEquals(Value, 4, ',') and (not CharEquals(Value, 5, ' ')) then begin
        Insert(' ', Value, 5);
      end;
      Fetch(Value);
      Value := TrimLeft(Value);
    end;

    // Workaround for some buggy web servers which use '-' to separate the date parts.    {Do not Localize}
    if (IndyPos('-', Value) > 1) and (IndyPos('-', Value) < IndyPos(' ', Value)) then begin    {Do not Localize}
      sDelim := '-';    {Do not Localize}
    end else begin
      sDelim := ' ';    {Do not Localize}
    end;

    //workaround for improper dates such as 'Fri, Sep 7 2001'    {Do not Localize}
    //RFC 2822 states that they should be like 'Fri, 7 Sep 2001'    {Do not Localize}
    if StrToMonth(Fetch(Value, sDelim, False)) > 0 then begin
      {Month}
      ParseMonth;
      {Day of Month}
      ParseDayOfMonth;
    end else begin
      {Day of Month}
      ParseDayOfMonth;
      {Month}
      ParseMonth;
    end;

    {Year}
    // There is some strange date/time formats like
    // DayOfWeek Month DayOfMonth Time Year
    sYear := Fetch(Value);
    Yr := IndyStrToInt(sYear, High(Word));
    if Yr = High(Word) then begin // Is sTime valid Integer?
      sTime := sYear;
      sYear := Fetch(Value);
      Value := TrimRight(sTime + ' ' + Value);
      Yr := IndyStrToInt(sYear);
    end;

    // RLebeau: According to RFC 2822, Section 4.3:
    //
    // "Where a two or three digit year occurs in a date, the year is to be
    // interpreted as follows: If a two digit year is encountered whose
    // value is between 00 and 49, the year is interpreted by adding 2000,
    // ending up with a value between 2000 and 2049.  If a two digit year is
    // encountered with a value between 50 and 99, or any three digit year
    // is encountered, the year is interpreted by adding 1900."
    if Length(sYear) = 2 then begin
      if {(Yr >= 0) and} (Yr <= 49) then begin
        Inc(Yr, 2000);
      end
      else if (Yr >= 50) and (Yr <= 99) then begin
        Inc(Yr, 1900);
      end;
    end
    else if Length(sYear) = 3 then begin
      Inc(Yr, 1900);
    end;

    VDateTime := EncodeDate(Yr, Mo, Dt);
    // SG 26/9/00: Changed so that ANY time format is accepted
    if IndyPos('AM', Value) > 0 then begin{do not localize}
      LAM := True;
      Value := Fetch(Value, 'AM');  {do not localize}
    end
    else if IndyPos('PM', Value) > 0 then begin {do not localize}
      LPM := True;
      Value := Fetch(Value, 'PM');  {do not localize}
    end;

    // RLebeau 03/04/2009: some countries use dot instead of colon
    // for the time separator
    i := IndyPos('.', Value);       {do not localize}
    if i > 0 then begin
      sDelim := '.';                {do not localize}
    end else begin
      sDelim := ':';                {do not localize}
    end;
    i := IndyPos(sDelim, Value);
    if i > 0 then begin
      // Copy time string up until next space (before GMT offset)
      sTime := Fetch(Value, ' ');  {do not localize}
      {Hour}
      Ho  := IndyStrToInt( Fetch(sTime, sDelim), 0);
      {Minute}
      Min := IndyStrToInt( Fetch(sTime, sDelim), 0);
      {Second}
      Sec := IndyStrToInt( Fetch(sTime), 0);
      {AM/PM part if present}
      Value := TrimLeft(Value);
      if LAM then begin
        if Ho = 12 then begin
          Ho := 0;
        end;
      end
      else if LPM then begin
        //in the 12 hour format, afternoon is 12:00PM followed by 1:00PM
        //while midnight is written as 12:00 AM
        //Not exactly technically correct but pretty accurate
        if Ho < 12 then begin
          Inc(Ho, 12);
        end;
      end;
      {The date and time stamp returned}
      VDateTime := VDateTime + EncodeTime(Ho, Min, Sec, 0);
    end;
    Value := TrimLeft(Value);
    Result := True;
  except
    VDateTime := 0.0;
    Result := False;
  end;
end;

function RawGMTToLocalDateTime(S: string): TDateTime;
var
  DateTimeOffset: TDateTime;
begin
  if RawStrInternetToDateTime(S, Result) then begin
    DateTimeOffset := GmtOffsetStrToDateTime(S);
    {-Apply GMT offset here}
    if DateTimeOffset < 0.0 then begin
      Result := Result + Abs(DateTimeOffset);
    end else begin
      Result := Result - DateTimeOffset;
    end;
    // Apply local offset
    Result := Result + gOffsetFromUTC;
  end;
end;

var
  gComputerName: string = '';

function ComputerName: string;
var
  len: DWORD;
begin
  if gComputerName = '' then
  begin
    len := MAX_COMPUTERNAME_LENGTH + 1;
    SetLength(gComputerName, len);
    GetComputerName(PChar(gComputerName), len);
    SetLength(gComputerName, len)
  end;
  Result := gComputerName;
end;

procedure ClearSynchronizedMethods;
var
  retries: Integer;
begin
  retries := 0;
  while retries < 1000 do
  begin
    Sleep(10);
    if not CheckSynchronize then
      Break;
  end;
end;

function FixFileNameString(const st: string): string;
begin
  // Remove not-allowed characters from a string when it is used for a filename.
  Result := StringReplace(st, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, ':', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '\', '_', [rfReplaceAll]);
end;

procedure SetLatestVersion(const v: string);
begin
  if not Assigned(gLatestVersionSync) then
    gLatestVersionSync := TCriticalSection.Create;

  gLatestVersionSync.Enter;
  try
    gLatestVersion := v;
  finally
    gLatestVersionSync.Leave;
  end;
end;

procedure SetDeserveMedals(const v: string);
begin
  if not Assigned(gLatestVersionSync) then
    gLatestVersionSync := TCriticalSection.Create;

  gLatestVersionSync.Enter;
  try
    gDeserveMedals := v;
  finally
    gLatestVersionSync.Leave;
  end;
end;

function GetLatestVersion: string;
begin
  if not Assigned(gLatestVersionSync) then
    gLatestVersionSync := TCriticalSection.Create;

  gLatestVersionSync.Enter;
  try
    Result := gLatestVersion;
  finally
    gLatestVersionSync.Leave;
  end;
end;

function GetDeserveMedals: string;
begin
  if not Assigned(gLatestVersionSync) then
    gLatestVersionSync := TCriticalSection.Create;

  gLatestVersionSync.Enter;
  try
    Result := gDeserveMedals;
  finally
    gLatestVersionSync.Leave;
  end;
end;

// QEC-20040503-11:11  Added ShortName() function.
function ShortName(const Name: string): string;
type
  CharSetType = set of AnsiChar;
var
  I: integer;
  BreakChars: CharSetType;
  WorkStr: string;
begin
  BreakChars := [' ','.',',','@','/',';',':','<','>','[',']','\','|','{','}','*',
                 '''','=','+','-','_','(',')','&','^','%','$','#','!','`','~'];
  WorkStr := Name;
  for I := 1 to Length(WorkStr) do
  begin
    if WorkStr[I] = '"' then
    begin
      WorkStr[I] := ' ';
      Continue;
    end;
    if WorkStr[I] in BreakChars then
    begin
      Result := Trim(LeftStr(WorkStr, I - 1));
      Break;
    end;
    Result := Name;
  end;
end;

function HTMLClipboardFormat: Integer;
begin
  if gHTMLClipboardFormat = 0 then
    gHTMLClipboardFormat := RegisterClipboardFormat('HTML Format');
  Result := gHTMLClipboardFormat;
end;

function ForeName(const name: string): string;
begin
  Result := ShortName(name);
end;

function AdjustForLargeFonts(x: Integer): Integer;
begin
  Result := (x * Screen.PixelsPerInch) div 96;
end;

procedure AdjustFormConstraints(form: TForm);
begin
  if form.Scaled then
    with form.Constraints do
    begin
      MinWidth := AdjustForLargeFonts(MinWidth);
      MinHeight := AdjustForLargeFonts(MinHeight);
      MaxWidth := AdjustForLargeFonts(MaxWidth);
      MaxHeight := AdjustForLargeFonts(MaxHeight);
    end;
end;

function CreateExSettings(cls: TExSettingsClass): TExSettings;
begin
  if cls = nil then
    cls := gExSettingsClass;

  Result := cls.Create('Woozle', 'XanaNews');

  if Result is TExfileSettings then
    TExFileSettings(Result).CustomPath := gExSettingsFile;
end;

function CreateChildSettings(parent: TExSettings; const section: string = ''): TExSettings;
var
  tp: TExSettingsClass;
begin
  tp := TExSettingsClass(parent.ClassType);
  Result := tp.CreateChild(parent, section);
end;

procedure UseXMLSettings(const fn: string);
begin
  gExSettingsClass := TExXMLSettings;
  gExSettingsFile := fn;
  if gExSettingsFile <> '' then
    Delete(gExSettingsFile, 1, 1);
  gExSettingsFile := ExpandFileName(gExSettingsFile);
end;

procedure UpdateGlobalOffsetFromUTC;
begin
  gOffsetFromUTC := OffsetFromUTC;
end;

initialization
  UpdateGlobalOffsetFromUTC;
finalization
  FreeAndNil(gDecoderMIME);
  FreeAndNil(gDecoderQuotedPrintable);
  FreeAndNil(gLatestVersionSync);
end.
