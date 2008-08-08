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

uses Windows, Messages, Classes, SysUtils, Forms, Ras, StrUtils, ActnList, ConTnrs, Dialogs, SyncObjs,
     unitExSettings, unitExRegSettings, unitExXMLSettings;

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

  WM_POSTANDCLOSE = WM_APP + $420; // Posting window messages
  WM_ADJUSTWIDTH = WM_APP + $421;
  WM_SETCODEPAGE = WM_APP + $422;
  WM_RETRYSETMSG = WM_APP + $423;
  WM_SPECIALKEY = WM_APP + $424;
  WM_SETIDENTITY = WM_APP + $425;

  DefaultMaxLineLength = 72;

type
  TTransfer = (bit7, bit8, iso2022jp);
  CSET = set of Char;
  TTextPartStyle = (tpNNTP, tpMIME, tpQuotedPrintable, tpFlowed);
  TPostingStyle = (psBottom, psTop);


  TActionDefault = class
  private
    fAction: TCustomAction;
    fShortcut : TShortcut;
  public
    constructor Create (AAction : TCustomAction);
    property Action : TCustomAction read fAction;
    property Shortcut : TShortcut read fShortcut;
  end;

  TBozoAction = (baIgnore, baMarkAsRead, baIgnoreThread, baMarkAsReadThread, baDontDownload);

var
  gDefaultActions : TObjectList = Nil;
  gReadLnDelay : Integer = 0;

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
  rstColorsFonts = 'Colours & Fonts';
  rstColorsFontsHelp = 'Select an item in the tree below to adjust its colours and fonts.';
  rstColorFontMessageTreeHelp = 'Colour & Font settings for all Message Tree elements.  You can adjust these individually using the sections below.';
  rstColorFontMessagePaneHelp = 'Font settings for all Message Pane elements.  You can adjust these individually using the sections below.';
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
  rstnewsgroupTree = 'Newsgroup Tree';
  rstStandardHelp = 'Colour and Font Settings for %s';
  rstStandardHelp1 = 'Colour and Font Settings for the %s';

type

TThreadOrder = (toThreaded, toChronological);
TThreadSortOrder = (soDate, soSubject, soAuthor, soLines, soMessageNo, soPostingHost, soNewestMessage);
TThreadSortDirection = (sdAscending, sdDescending);
PObject = ^TObject;

var
  RasEntries : array of TRasEntryName;
  gLatestVersion : string = '~';
  gDeserveMedals : string = '~';
  NewVersion : string = '~';
  gAppTerminating : boolean = False;
  gLogFlag : boolean = False;
  gAudiblePerformanceCues : boolean = False;
  gProductVersion : string = '';
  gExSettingsClass : TExSettingsClass = TExRegSettings;
  gExSettingsFile : string = '';

procedure FixHeaders (hdrs : TStrings);
function ProductVersion : string;
procedure UseXMLSettings (const fn : string);


procedure WrapStrings (m : TStrings; maxLen : Integer; textPartStyle : TTextPartStyle; nest, strictSigSep : boolean);
procedure FixQuotes (s : TStrings; wrap : boolean; maxLineLen : Integer; const quoteLineMarker : string; trimSig, strictSigSep : boolean);
procedure PurgeDirectory (const dirName : string);
function ShortGroupName (const groupName : string) : string;
function FairlyShortGroupName (const groupName : string) : string;
function WideROT13 (const st : WideString) : WideString;
function WideReverseString (const st : WideString) : WideString;
procedure LoadRASEntries;
function HeaderCharset (h : string) : string;
procedure DecodeFromEMail (const from: string; var fromName, fromEMail: string; codePage: Integer = -1);
function DecodeHeader (const header : string; codePage : PInteger = Nil) : widestring;
function DecodeSubject (const subject: string; codePage: Integer): string;
function GenerateMessageID (const product, stub, host : string) : string;
function EncodeHeader (const header : string; codepage : Integer; from : boolean) : string;
procedure SetTempStatusMessage (const msg : string; pos, max : word);
function SafeDateTimeToInternetStr(const Value: TDateTime; const AIsGMT : Boolean = False) : String;
function ComputerName : string;
procedure ClearSynchronizedMethods;
function FixFileNameString (const st : string) : string;
procedure SetLatestVersion (const v : string);
procedure SetDeserveMedals (const v : string);
function GetLatestVersion : string;
function GetDeserveMedals : string;
function ShortName(const Name: string): string;
function HTMLClipboardFormat : Integer;
function ForeName (const name : string) : string;
procedure AdjustFormConstraints (form : TForm);
//function AdjustForLargeFonts (x : Integer) : Integer;
function CreateExSettings (cls : TExSettingsClass = Nil) : TExSettings;
function CreateChildSettings (parent : TExSettings; const section : string = '') : TExSettings;


implementation

uses unitSearchString, idCoder, idCoderMIME, idCoderQuotedPrintableX, unitCharsetMap, IdGlobal, unitExFileSettings;

var
  gLatestVersionSync : TCriticalSection = Nil;
  gHTMLClipboardFormat : Integer = 0;

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
 |   Replaces CR & LF with spaces.   They're legal but unusual.         |
 |   Merges split lines.                                                |
 |   Replaces runs of whitespace with a single space character          |
 |   RFC 850 says that 'references' should be <id>blank<id> - but       |
 |   people sometimes leave out the blank.                              |
 |                                                                      |
 | Parameters:                                                          |
 |   hdrs : TStrings                                                    |
 |                                                                      |
 | The function returns None                                            |
 |                                                                      |
 | ** TODO - Any text within brackets (including the brackets) is a     |
 |           comment.  Remove this                                      |
 *----------------------------------------------------------------------*)
procedure FixHeaders (hdrs : TStrings);
var
  i, j, p : Integer;
  s, s1 : string;
  ch, lastch : char;
  inQuote : Boolean;
begin
  i := 1;
  while i < hdrs.Count  do // Merge split lines.
  begin
    s := hdrs [i];
                           // If a line starts with whitespace then
                           // append it to the previous line.
    if (Length (s) = 0) or (s [1] in [' ', #9]) then
    begin
      hdrs [i - 1] := hdrs [i - 1] + Trim (s);
      hdrs.Delete(i);
    end
    else
      Inc (i)
  end;

  for i := 0 to hdrs.Count - 1 do
  begin
    s := hdrs [i];

    SetLength (s1, Length (s));
    inQuote := False;
    lastch := #0;
    p := 1;

    for j := 1 to Length (s) do
    begin
      ch := s [j];

      if ch in [#9, #10, #13] then      // Replace these with spaces
        ch := ' ';

      if ch = '"' then
        inQuote := not inQuote;

      if ch <> ' ' then
        lastch := #0;
                                        // Delete multiple spaces - unless they're
                                        // in quotes.
      if inQuote or (lastch <> ' ') then
      begin
        s1 [p] := ch;
        Inc (p)
      end;

      lastch := ch
    end;

    SetLength (s1, p - 1);
    if SameText (Copy (s1, 1, 11), 'References:') then
      s1 := StringReplace (s1, '><', '> <', [rfReplaceAll]);  // Fix dodgy references
    hdrs [i] := Trim (s1)
  end
end;

(*----------------------------------------------------------------------*
 | function ProductVersion : string                                     |
 |                                                                      |
 | Returns the product version - eg. 1.12.1.9                           |
 *----------------------------------------------------------------------*)
function ProductVersion : string;
var
  size, zero : DWORD;
  buffer, pBuffer: pointer;
  info : PVSFixedFileInfo;
begin
  if gProductVersion <> '' then
  begin
    Result := gProductVersion;
    Exit;
  end;
  result := '';         // Get it from the versioninfo resource
  size := GetFileVersionInfoSize (PChar (ParamStr (0)), zero);
  if size > 0 then
  begin
    GetMem (buffer, size);
    try
      if not GetFileVersionInfo (PChar (paramStr (0)), zero, size, buffer) then
        RaiseLastOSError;

      if not VerQueryValue (buffer, '\', pBuffer, size) then
        RaiseLastOSError;

      info := PVSFixedFileInfo (pBuffer);

      result := Format ('%d.%d.%d.%d', [HiWord (info^.dwProductVersionMS), LoWord (info^.dwProductVersionMS), HiWord (info^.dwProductVersionLS), LoWord (info^.dwProductVersionLS)])
    finally
      FreeMem (buffer)
    end
  end;
  gProductVersion := result
end;

function IsSigSep (st : string; strct, quoted : boolean) : boolean;
begin
  result := False;
  if Length (st) >= 2 then
    if quoted then
    begin
      if (st [1] = '>') or (st [1] = '|') then
      begin
        Delete (st, 1, 1);
        if st [1] = ' ' then
          Delete (st, 1, 1);
        result := IsSigSep (st, strct, false)
      end
    end
    else
      result := (st = '-- ') or ((not strct) and (st = '--'))
end;

(*----------------------------------------------------------------------*
 | procedure WrapStrings                                                |
 |                                                                      |
 | Wrap strings so that they're not longer than the specified maximum   |
 | length.                                                              |
 |                                                                      |
 | Parameters:                                                          |
 |   m : TStrings               // The strings to wrap                  |
 |   maxLen : Integer;          // The maximum length                   |
 |   quotedPrintable : boolean; // If the flag is set, convert the      |
 |                              // strings to 'quoted printable' format |
 |   nest : boolean             // nest quoted text.  The quote level   |
 |                              // for each line must be in Objects [i] |
 |                                                                      |
 | The function returns None
 *----------------------------------------------------------------------*)
procedure WrapStrings (m : TStrings; maxLen : Integer; TextPartStyle : TTextPartStyle; nest, strictSigSep : boolean);
var
  holdQuotedPrintable : boolean;
  i, len, p, ml : Integer;
  s1, s2 : string;
  ch : char;
  encoder : TidEncoderQuotedPrintable;

  function IsURL(const S1: string): Boolean;
  begin
    Result := ContainsText(S1, '://') and not ContainsText(S1, ' ');
  end;

begin
  holdQuotedPrintable := False;
  i := 0;
  ml := maxLen;
  encoder := Nil;
  try
    while i < m.Count do
    begin
      s1 := m [i];
      if TextPartStyle = tpFlowed then
      begin
        if s1 <> '-- ' then
          s1 := TrimRight (s1);
        m [i] := s1
      end;

      if nest then
      begin
        ml := maxLen - 2 * (Integer (m.Objects [i]) + 1);
        if ml <= 0 then
        begin
          Inc (i);
          continue
        end
      end;

      // Replace all equal symbols with '=3D' if quoted-printable
      if not holdQuotedPrintable and (TextPartStyle = tpQuotedPrintable) then
      begin
        if encoder = nil then
        begin
          encoder := TidEncoderQuotedPrintable.Create(nil);
          encoder.MaxLineLen := MaxInt
        end;
        s1 := encoder.Encode(s1);
//        s1 := StringReplace (s1, '=', '=3D', [rfReplaceAll]);
        m [i] := s1
      end;

      len := Length (s1);

      holdQuotedPrintable := False;
      if (len >ml) and not IsURL(s1) then
      begin
        p := ml;
        while (p > 0) and not (s1 [p] in [#9, ' ']) do
          Dec (p);

        if p = 0 then
          if TextPartStyle = tpFlowed then
          begin
            Inc (i);
            continue
          end
          else
            p := ml;

        s2 := Copy (s1, 1, p);
        if TextPartStyle = tpQuotedPrintable then
          s2 := s2 + '='
        else
          if TextPartStyle = tpFlowed then
          begin
            if s2 [Length (s2)] <> ' ' then
              s2 := s2 + ' '
          end
          else
            s2 := TrimRight (s2);
        m [i] := s2;

        if not nest or (i + 1 = m.Count) or (m [i + 1] = '') or IsSigSep (m [i + 1], strictSigSep, false) or (m.Objects [i] <> m.Objects [i + 1]) then
        begin
          s2 := Copy (s1, p + 1, MaxInt);
          if textPartStyle <> tpQuotedPrintable then
            s2 := Trim (s2);
          m.InsertObject(i + 1, s2, m.Objects [i])
        end
        else
        begin
          s1 := Trim (Copy (s1, p + 1, MaxInt));
          if s1 <> '' then
          begin
            ch := s1 [Length (s1)];

            if ch in ['.', '?', '!', ':'] then
              s1 := s1 + ' ';

            s2 := m [i + 1];
            if (textPartStyle = tpQuotedPrintable) then
            begin
              if encoder = nil then
              begin
                encoder := TidEncoderQuotedPrintable.Create(nil);
                encoder.MaxLineLen := MaxInt
              end;
              s2 := encoder.Encode (s2)
            end;

            m [i + 1] := s1 + ' ' + s2
          end
        end;
        holdQuotedPrintable := True;
      end;
      Inc (i)
    end
  finally
    encoder.Free
  end
end;

procedure FixQuotes (s : TStrings; wrap : boolean; maxLineLen : Integer; const quoteLineMarker : string; trimSig, strictSigSep : boolean);
var
  i, p : Integer;
  sigLine : Integer;
  st : string;
begin
  for i := 0 to s.Count - 1 do
  begin
    repeat
      st := quoteLineMarker;
      p := Pos (st, s [i]);
      if p = 0 then
      begin
        st := '>';
        p := Pos (st, s [i]);
        if p = 0 then
        begin
          st := '|';
          p := Pos (st, s [i])
        end
      end;
      if p = 1 then
      begin
        s.Objects [i] := TObject (Integer (s.Objects [i]) + 1);
        s [i] := Copy (s [i], Length (st) + 1, maxInt);
      end
    until p <> 1
  end;

  if wrap then
    WrapStrings (s, maxLineLen, tpNNTP, True, strictSigSep);

  if quoteLineMarker <> '' then
  begin
    for i := 0 to s.Count - 1 do
      for p := 0 to Integer (s.Objects [i]) do
        s [i] := quoteLineMarker + s [i];

    s.Insert (0, '')
  end;

  sigLine := -1;
  if trimSig then
    for i := 0 to s.Count - 1 do
      if IsSigSep (s [i], strictSigSep, true) then
      begin
        sigLine := i;
        break
      end;

  if sigLine >= 0 then
    while s.Count > sigLine do
      s.Delete(sigLine);

  while s.Count > 0 do
    if s [s.Count - 1] = quoteLineMarker then
      s.Delete (s.Count - 1)
    else
      Break
end;

(*----------------------------------------------------------------------*
 | procedure PurgeDirectory                                             |
 |                                                                      |
 | Delete a directory and it's sub-directories.                         |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   const dirName : string     The directory to purge.                 |
 *----------------------------------------------------------------------*)
procedure PurgeDirectory (const dirName : string);
var
  f : TSearchRec;
  st : string;
  err : Integer;
begin
  st := dirName;
  UniqueString (st);
  if FindFirst (st + '\' + '*.*', faAnyFile, f) = 0 then
  try
    repeat
      if (f.Attr and (faReadOnly or faSysFile or faHidden)) <> 0 then
        FileSetAttr (st + '\' + f.Name, f.Attr and not (faReadOnly or faSysFile or faHidden));

      if (f.Attr and faDirectory) <> 0 then
      begin
        if (f.Name <> '.') and (f.name <> '..') then
          PurgeDirectory (st + '\' + f.Name)
      end
      else
        if not DeleteFile (st + '\' + f.Name) then
          RaiseLastOSError
    until FindNext (f) <> 0;
  finally
    FindClose (f)
  end;
  if not RemoveDir (st) then
  begin
    err := GetLastError;
    if (err <> ERROR_FILE_NOT_FOUND) and (err <> ERROR_PATH_NOT_FOUND) then
      RaiseLastOSError
  end
end;

function ShortGroupName (const groupName : string) : string;
var
  c : Integer;
begin
  c := Length (groupName);

  while (c > 0) and (groupName [c] <> '.') do
    Dec (c);

  if c = 0 then
    result := groupName
  else
    result := Copy (groupName, c, MaxInt);

  while c > 0 do
  begin
    repeat
      Dec (c)
    until (c = 0) or (groupName [c] = '.');

    if c = 0 then
      result := groupName [c + 1] + result
    else
      result := Copy (groupName, c, 2) + result
  end
end;

function FairlyShortGroupName (const groupName : string) : string;
var
  c : Integer;
begin
  c := Length (groupName);

  while (c > 0) and (groupName [c] <> '.') do
    Dec (c);

  if c = 0 then
    result := groupName
  else
    result := ShortGroupName (Copy (groupName, 1, c - 1)) + Copy (groupName, c, MaxInt)
end;


(*----------------------------------------------------------------------*
 | function ROT13                                                       |
 |                                                                      |
 | Return a ROT-13 en/decryption of a string.  Call it again to         |
 | de/encode the string.                                                |
 *----------------------------------------------------------------------*)
function WideROT13 (const st : WideString) : WideString;
var
  i, l : Integer;
  ch : WideChar;
begin
  l := Length (st);
  SetLength (result, l);

  for i := 1 to l do
  begin
    ch := st [i];
    if ((ch >= 'A') and (ch <= 'M')) or ((ch >= 'a') and (ch <= 'm')) then
      ch := WideChar (Ord (ch) + 13)
    else
      if ((ch >= 'N') and (ch <= 'Z')) or ((ch >= 'n') and (ch <= 'z')) then
        ch := WideChar (Ord (ch) - 13);
    result [i] := ch
  end
end;

function WideReverseString (const st : WideString) : WideString;
var
  i, l : Integer;
begin
  l := Length (st);
  SetLength (result, l);
  for i := 1 to l do
    result [i] := st [l - i + 1]
end;

(*----------------------------------------------------------------------*
 | procedure LoadRASEntries                                             |
 |                                                                      |
 | Populate the RAS Entries array with available diallup connections    |
 *----------------------------------------------------------------------*)
procedure LoadRASEntries;
var
  ce, cb, rv : DWORD;

begin
  try
    if InitRASLibrary then
    try
      ce := 10;
      repeat
        SetLength (RasEntries, ce);
        cb := ce * SizeOf (TRasEntryName);
        FillChar (RasEntries [0], cb, 0);
        RasEntries [0].dwSize := sizeof (TRasEntryName);
        rv := RasEnumEntries (nil, nil, @RasEntries [0], cb, ce);

        case rv of
          0 : SetLength (RasEntries, ce);
          ERROR_BUFFER_TOO_SMALL :;
          else
          begin
            SetLength (RasEntries, 0);
            SetLastError (rv);
            RaiseLastOSError
          end
        end
      until rv <> ERROR_BUFFER_TOO_SMALL
    finally
      FreeRASLibrary
    end
    else
      SetLength (RasEntries, 0);
  except
    SetLength (RasEntries, 0);
  end
end;

function HeaderCharset (h : string) : string;
var
  p : Integer;
begin
  p := Pos ('=?', h);
  if p > 0 then
  begin
    h := Copy (h, p+2, MaxInt);
    result := ExtractString ('?', h)
  end
  else
    result := ''
end;

function DequotedCString (const st : string) : string;
var
  i, l : Integer;
begin
  result := st;
  l := Length (result);
  if (l > 0) and (st [1] = '"') then
  begin
    Delete (result, 1, 1);
    Dec (l)
  end;

  if (l > 0) and (result [l] = '"') then
  begin
    Delete (result, l, 1);
    Dec (l)
  end;

  i := 0;
  while i < l do
  begin
    Inc (i);

    case result [i] of
      '\' : begin
              Delete (result, i, 1);
              Dec (l);
              case result [i] of
                't' : result [i] := #9;
                'r', 'n' : result [i] := ' '
              end
            end;
    end
  end
end;

procedure DecodeFromEMail (const from: string; var fromName, fromEMail: string; codePage: Integer = -1);
var
  p : Integer;
begin
  p := Pos ('<', from);

  if p > 0 then
  begin
    fromEmail := from;
    fromName := DequotedCString (SplitString ('<', fromEmail));
    if (Length (fromEmail) > 0) and (fromEmail [Length (fromEmail)] = '>') then
      Delete (fromEmail, Length (fromEmail), 1)
  end
  else
  begin
    p := Pos ('(', from);
    if p > 0 then
    begin
      fromName := from;
      fromEmail := SplitString ('(', fromName);
      if (Length (fromName) > 0) and (fromName [Length (fromName)] = ')') then
        Delete (fromName, Length (fromName), 1);
    end
    else
    begin
      fromName := from;
      fromEmail := '';
    end
  end;


  if codePage = -1  then
    codePage := CP_ACP;
  fromName := WideStringToString(DecodeHeader(fromName), CodePage);

  if fromName = '' then
    fromName := from;

//  fromName := Trim (StringReplace (fromName, '\', '', [rfReplaceAll]));
end;

{ TActionDefault }

constructor TActionDefault.Create(AAction: TCustomAction);
begin
  fAction := AAction;
  fShortcut := AAction.ShortCut
end;

var
  gDecoderMIME : TidDecoderMIME = Nil;
  gDecoderQuotedPrintable : TidDecoderQuotedPrintable = Nil;

//Searches Data for an RFC-1522 chunk, starting at cFrom.
//If a chunk is found, cFrom and cTo contain positions of the first
//and last character, respectively.
function FindChunk(const Data: string; var cFrom, cTo: integer): boolean;
const
 ChunkChars = ['A'..'Z'] + ['a'..'z'] + ['0'..'9'] + ['=', '?', '-', '_'];
var
 State: byte;
 i: integer;
begin
  Result:=false;
  i:=cFrom;
  State:=0;
  while i<=Length(Data) do begin
    case State of
      0: if Data[i]='=' then begin
           State:=1;
           cFrom:=i;
         end;
      1: if Data[i]='?' then
           State:=2
         else
           State:=0;
      2,3,4:
         //FIX: Some clients actually include spaces in chunks.
         if (State<>4) and not (Data[i] in ChunkChars) then
           State:=0
         else if Data[i]='?' then
           inc(State);
      5: if Data[i]='=' then begin
           cTo:=i;
           Result:=true;
           EXIT;
         end else if Data[i]<>'?' then
           State:=4;
    end;
    inc(i);
  end;
end;

procedure ControlsToSpaces(var Str: widestring);
var x: integer;
begin
  for x:=1 to Length(Str) do
    if Ord(Str[x])<32 then Str[x]:=' ';
end;

//Decodes a RFC-1522 chunk into widestring.
//Expects a single, pre-stripped chunk as input ('ISO-8859-1?q?data')
function DecodeChunk(Input: string): Widestring;
var
  cp, data: string;
  enc: char;
  x, cpnum: integer;
  AttachSpace: boolean;
begin
  Result:='';
  x:=Pos('?', Input);
  //Checks for encoding byte, '?', and at least one byte of data.
  if Length(Input) < x+3 then EXIT;
  //Encoding should be exactly one character
  if Input[x+2] <> '?' then EXIT;

  cp:=LowerCase(Copy(Input, 1, x-1));
  cpnum:=MIMECharsetNameToCodePage(cp);
  enc:=Input[x+1];
  data:=Copy(Input, x+3, maxint);

  if enc in ['b', 'B'] then begin
    with TidDecoderMIME.Create(nil) do begin
      data:=DecodeString(data);
      Free;
    end;
  end
  else begin
    //TidDecoderQuotedPrintable does not deal with underscore encoded spaces
    data:=StringReplace(data, '_', ' ', [rfReplaceAll]);
    AttachSpace:=data[Length(data)]=' ';
    with TidDecoderQuotedPrintable.Create(nil) do begin
      data:=DecodeString(data);
      Free;
    end;
    //Unfortunately, the Decoder will also trim trailing spaces
    if AttachSpace then data:=data+' ';
  end;

  //Convert character data to widestring
  setlength(Result, Length(data));
  x:=MultiByteToWideChar(cpnum, 0, @data[1], Length(data),
      @Result[1], Length(Result));
  setlength(Result, x);
end;

function DecodeHeader(const header: string; codePage : PInteger = Nil) : widestring;
const CP_UTF_16 = 1200;
var
 chkStart, chkEnd, lastChkEnd: integer;
begin
  Result := '';
  lastChkEnd := 0;
  chkStart := 1;
  while FindChunk(Header, chkStart, chkEnd) do begin
    Result := Result +
              Copy(Header, lastChkEnd+1, chkStart-lastChkEnd-1) +
              DecodeChunk(Copy(Header, chkStart+2, chkEnd-chkStart-3));
    lastChkEnd := chkEnd;
    chkStart := chkEnd+1;
    //If a chunk is followed by ' =?', ignore it
    if (Length(Header)>chkStart+8) and
       (Header[chkStart] in [' ', #8]) and
       (Header[chkStart+1]='=') and
       (Header[chkStart+2]='?')
    then inc(lastChkEnd);
  end;
  Result := Result + Copy(Header, lastChkEnd+1, MaxInt);
  //Not exactly clean, but this parameter seems not to be used anyway.
  if Assigned(CodePage) then
    CodePage^ := CP_UTF_16;
end;


function DecodeSubject(const subject: string; codePage: Integer): string;
var
  cp: Integer;
begin
  cp := codePage;
  Result := WideStringToString(DecodeHeader(subject, @cp), codePage);
end;

var
  gmi : Integer = 0;

function GenerateMessageID (const product, stub, host : string) : string;
var
  st, hs : string;

function Enc36 (i : Integer; padLen : Integer = 7) : string;
var
  i1 : Integer;
begin
  result := '';
  while i > 0 do
  begin
    i1 := i mod 36;
    i := i div 36;
    if i1 < 10 then
      result := Char (Ord ('0') + i1) + result
    else
      result := Char (Ord ('a') + i1 - 10) + result
  end;

  while Length (result) < padLen do
    result := '0' + result
end;

begin
  st := product + Enc36 (DateTimeToFileDate (now)) + Enc36 (GetTickCount, 4) + Enc36(gmi, 3) + stub;
  Inc (gmi);
  if gmi >= 36*36*36 then
    gmi := 0;
  if SameText (host, 'LocalHost') then
    hs := 'xananews'
  else
    hs := host;
  result := '<' + st + '@' + hs + '>';
end;

const
  base64_tbl: array [0..63] of Char = (
    'A','B','C','D','E','F','G','H',     {Do not Localize}
    'I','J','K','L','M','N','O','P',      {Do not Localize}
    'Q','R','S','T','U','V','W','X',      {Do not Localize}
    'Y','Z','a','b','c','d','e','f',      {Do not Localize}
    'g','h','i','j','k','l','m','n',      {Do not Localize}
    'o','p','q','r','s','t','u','v',       {Do not Localize}
    'w','x','y','z','0','1','2','3',       {Do not Localize}
    '4','5','6','7','8','9','+','/');      {Do not Localize}

function EncodeHeader1(const Header: string; specials : CSET; HeaderEncoding: Char;
  TransferHeader: TTransfer; MimeCharSet: string): string;
const
  SPACES: set of Char = [' ', #9, #10, #13, '''', '"'];    {Do not Localize}

var
  S, T: string;
  L, P, Q, R: Integer;
  B0, B1, B2: Integer;
  InEncode: Integer;
  NeedEncode: Boolean;
  csNeedEncode, csReqQuote: CSET;
  BeginEncode, EndEncode: string;
  ch : char;

  procedure EncodeWord(P: Integer);
  const
    MaxEncLen = 75;
  var
    Q: Integer;
    EncLen: Integer;
    Enc1: string;
  begin
    T := T + BeginEncode;
    if L < P then P := L + 1;
    Q := InEncode;
    InEncode := 0;
    EncLen := Length(BeginEncode) + 2;

    if headerEncoding = 'Q' then  { quoted-printable }   {Do not Localize}
    begin
      while Q < P do
      begin
        if not (S[Q] in csReqQuote) then
        begin
          Enc1 := S[Q]
        end
        else
        begin
          if S[Q] = ' ' then  {Do not Localize}
            Enc1 := '_'   {Do not Localize}
          else
            Enc1 := '=' + IntToHex(Ord(S[Q]), 2);     {Do not Localize}
        end;
        if EncLen + Length(Enc1) > MaxEncLen then
        begin
          T := T + EndEncode + #13#10#9 + BeginEncode;
          EncLen := Length(BeginEncode) + 2;
        end;
        T := T + Enc1;
        INC(EncLen, Length(Enc1));
        INC(Q);
      end;
    end
    else
    begin { base64 }
      while Q < P do
      begin
        if EncLen + 4 > MaxEncLen then
        begin
          T := T + EndEncode + #13#10#9 + BeginEncode;
          EncLen := Length(BeginEncode) + 2;
        end;

        B0 := Ord(S[Q]);
        case P - Q of
        1: T := T + base64_tbl[B0 SHR 2] + base64_tbl[B0 AND $03 SHL 4] + '==';  {Do not Localize}
        2:
          begin
            B1 := Ord(S[Q + 1]);
            T := T             + base64_tbl[B0 SHR 2] +
              base64_tbl[B0 AND $03 SHL 4 + B1 SHR 4] +
              base64_tbl[B1 AND $0F SHL 2] + '=';  {Do not Localize}
          end;
        else
          B1 := Ord(S[Q + 1]);
          B2 := Ord(S[Q + 2]);
          T := T + base64_tbl[B0 SHR 2] +
            base64_tbl[B0 AND $03 SHL 4 + B1 SHR 4] +
            base64_tbl[B1 AND $0F SHL 2 + B2 SHR 6] +
            base64_tbl[B2 AND $3F];
        end;
        INC(EncLen, 4);
        INC(Q, 3);
      end;
    end;
    T := T + EndEncode;
  end;

begin
  S := Header;
  headerEncoding := UpCase (headerEncoding);

  {Suggested by Andrew P.Rybin for easy 8bit support}
  if HeaderEncoding='8' then begin //UpCase('8')='8'     {Do not Localize}
      Result:=S;
      EXIT;
  end;//if
  csNeedEncode := [#0..#31, #127..#255] + specials;
  csReqQuote := csNeedEncode + ['?', '=', '_', ' '];   {Do not Localize}
  BeginEncode := '=?' + MimeCharSet + '?' + HeaderEncoding + '?';    {Do not Localize}
  EndEncode := '?=';  {Do not Localize}

  L := Length(S);
  P := 1;
  T := '';  {Do not Localize}
  InEncode := 0;
  while P <= L do
  begin
    Q := P;
    while (P <= L) and (S[P] in SPACES) do
      INC(P);
    R := P;
    NeedEncode := False;

    // Find start of first word that needs encoding (in 'R')
    while (P <= L) do
    begin
      ch := s [P];
      if ch in SPACES then
        if (ch <> ' ') or not NeedEncode or (headerEncoding <> 'Q') then
          break;

      if ch in csNeedEncode then
        NeedEncode := True;
      INC(P);
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

function EncodeHeader (const header : string; codePage : Integer; from : boolean) : string;
var
  s1 : string;
  i : Integer;
  ansi : boolean;
  coder : TidEncoderQuotedPrintable;
begin
  ansi := True;
  if from then
  begin
    if header [1] <> '"' then
      Raise Exception.Create ('Invalid from');

    i := 2;
    s1 := '';
    while (header [i] <> '"') and (i < Length (header)) do
    begin
      s1 := s1 + header [i];
      if not (header [i] in [' '..'~', #9, #10, #13]) then
        ansi := False;
      Inc (i)
    end
  end
  else
  begin
    i := 1;
    while i < Length (header) do
      if not (header [i] in [' '..'~', #9, #10, #13]) then
      begin
        ansi := false;
        break
      end
      else
        Inc (i)
  end;

  if ansi or not from then
    result := EncodeHeader1 (header, [], 'Q', bit7, CodePageToMIMECharsetName (CodePage))
  else
  begin
    coder := TidEncoderQuotedPrintable.Create(nil);
    try
      coder.Atom := True;
      coder.LinePreamble := '=?' + CodePageToMIMECharsetName (CodePage) + '?Q?';
      coder.LinePostamble := '?=';
      (*
      coder.MaxLineLen := 75 - Length (coder.LinePreamble) - Length (coder.LinePostamble);
      result := coder.Encode (s1) + #13#10 + Trim (Copy (header,  i + 2, MaxInt))
      *)
      coder.MaxLineLen := MaxInt;
      result := coder.Encode(s1) + ' ' + Trim (Copy (header,  i + 2, MaxInt))

    finally
      coder.Free
    end
  end
end;

procedure SetTempStatusMessage (const msg : string; pos, max : word);
begin
  if (csDestroying in Application.MainForm.ComponentState) then Exit;
  SendMessage (Application.MainForm.Handle, WM_STATUS, Integer (PChar (msg)), MakeLParam (pos, max))
end;

function SafeDateTimeToInternetStr(const Value: TDateTime; const AIsGMT : Boolean = False) : String;
begin
  try
    Result := DateTimeToInternetStr (Value, AIsGMT)
  except
    result := 'Fri 17 Nov 1961 08:00:00 GMT'
  end
end;

var
  gComputerName : string = '';
function ComputerName : string;
var
  len : DWORD;
begin
  if gComputerName = '' then
  begin
    len := MAX_COMPUTERNAME_LENGTH + 1;
    SetLength (gComputerName, len);
    GetComputerName (PChar (gComputerName), len);
    SetLength (gComputerName, len)
  end;
  result := gComputerName
end;

procedure ClearSynchronizedMethods;
var
  retries : Integer;
begin
  retries := 0;
  while retries < 1000 do
  begin
    Sleep (10);
    if not CheckSynchronize then
      Break
  end
end;

(*----------------------------------------------------------------------*
 | function FixFileNameString                                           |
 |                                                                      |
 | Remove not-allowed characters from a string is too be used in a      |
 | filename                                                             |
 |                                                                      |
 | Parameters:                                                          |
 |   const st : string                  The string to fix               |
 |                                                                      |
 | The function returns the fixed string                                |
 *----------------------------------------------------------------------*)
function FixFileNameString (const st : string) : string;
begin
  result := StringReplace (st, '/', '_', [rfReplaceAll]);
  result := StringReplace (result, ':', '_', [rfReplaceAll]);
  result := StringReplace (result, '\', '_', [rfReplaceAll]);
end;

procedure SetLatestVersion (const v : string);
begin
  if not Assigned (gLatestVersionSync) then
    gLatestVersionSync := TCriticalSection.Create;

  gLatestVersionSync.Enter;
  try
    gLatestVersion := v;
  finally
    gLatestVersionSync.Leave
  end
end;

procedure SetDeserveMedals (const v : string);
begin
  if not Assigned (gLatestVersionSync) then
    gLatestVersionSync := TCriticalSection.Create;

  gLatestVersionSync.Enter;
  try
    gDeserveMedals := v;
  finally
    gLatestVersionSync.Leave
  end
end;

function GetLatestVersion : string;
begin
  if not Assigned (gLatestVersionSync) then
    gLatestVersionSync := TCriticalSection.Create;

  gLatestVersionSync.Enter;
  try
    result := gLatestVersion
  finally
    gLatestVersionSync.Leave
  end
end;

function GetDeserveMedals : string;
begin
  if not Assigned (gLatestVersionSync) then
    gLatestVersionSync := TCriticalSection.Create;

  gLatestVersionSync.Enter;
  try
    result := gDeserveMedals
  finally
    gLatestVersionSync.Leave
  end
end;

// QEC-20040503-11:11  Added ShortName() function.
function ShortName(const Name: string): string;
type
    CharSetType = set of AnsiChar;
var I: integer;
    BreakChars : CharSetType;
    WorkStr    : string;
begin
BreakChars := [' ','.',',','@','/',';',':','<','>','[',']','\','|','{','}','*',
               '''','=','+','-','_','(',')','&','^','%','$','#','!','`','~'];
WorkStr    := Name;
for I := 1 to length(WorkStr) do
    begin
    if (WorkStr[I] = '"') then
       begin
       WorkStr[I] := ' ';
       continue;
       end;
    if (WorkStr[I] in BreakChars) then
       begin
       Result := trim(leftStr(WorkStr, I - 1));
       break;
       end;
    Result := Name;
    end;
end;

function HTMLClipboardFormat : Integer;
begin
 if gHTMLClipboardFormat = 0 then
   gHTMLClipboardFormat := RegisterClipboardFormat ('HTML Format');
 result := gHTMLClipboardFormat;
end;

function ForeName (const name : string) : string;
begin
  result := ShortName (name)
end;

function AdjustForLargeFonts (x : Integer) : Integer;
begin
  result := (x * Screen.PixelsPerInch) div 96
end;

procedure AdjustFormConstraints (form : TForm);
begin
  if form.Scaled then with form.Constraints do
  begin
    MinWidth := AdjustForLargeFonts (MinWidth);
    MinHeight := AdjustForLargeFonts (MinHeight);
    MaxWidth := AdjustForLargeFonts (MaxWidth);
    MaxHeight := AdjustForLargeFonts (MaxHeight)
  end
end;

function CreateExSettings (cls : TExSettingsClass) : TExSettings;
begin
  if cls = Nil then
    cls := gExSettingsClass;

  result := cls.Create('Woozle', 'XanaNews');

  if result is TExfileSettings then
    TExFileSettings (result).CustomPath := gExSettingsFile;
end;

function CreateChildSettings (parent : TExSettings; const section : string = '') : TExSettings;
var
  tp: TExSettingsClass;
begin
  tp := TExSettingsClass (parent.ClassType);
  result := tp.CreateChild(parent, section);
end;

procedure UseXMLSettings (const fn : string);
begin
  gExSettingsClass := TExXMLSettings;
  gExSettingsFile := fn;
  if gExSettingsFile <> '' then
    Delete (gExSettingsFile, 1, 1);
  gExSettingsFile := ExpandFileName(gExSettingsFile);
end;

initialization
finalization
  FreeAndNil (gDecoderMIME);
  FreeAndNil (gDecoderQuotedPrintable);
  FreeAndNil (gLatestVersionSync);
end.
