unit unitXanaExporter;

interface

uses
  Windows, Classes, SysUtils, Forms, ZLib, unitEXSettings, unitNNTPServices,
  XnClasses;

type
  TOnProgress = procedure(sender: TObject; pos, max: Integer; const group: string) of object;

  TXanaExporter = class
  private
    fOnProgress: TOnProgress;
  public
    procedure Import(const FileName: string);
    procedure Export(const FileName: string; groups: TStrings; SettingsToo: Boolean);
    property OnProgress: TOnProgress read fOnProgress write fOnProgress;
  end;

implementation

uses
  StrUtils, NewsGlobals, unitSearchString;

type
  TLabelBlock = array[0..255] of AnsiChar;

procedure WriteLabelBlock(lab: string; size: Int64; strm: TStream);
var
  len: Integer;
  blck: TLabelBlock;
  raw: RawByteString;
begin
  lab := '~~XaNaFood~' + IntToHex(size, 16) + '~' + lab;
  FillChar(blck, SizeOf(blck), 0);
  len := Length(lab);
  if len > SizeOf(blck) then
    len := SizeOf(blck);
  raw := RawByteString(lab);
  Move(raw[1], blck, len);
  strm.Write(blck[0], SizeOf(blck));
end;

procedure ReadLabelBlock(var lab: string; var size: Int64; strm: TStream);
var
  blck: TLabelBlock;
  i: Integer;
  st: string;
begin
  strm.Read(blck[0], SizeOf(blck));
  st := string(blck);

  if Copy(st, 1, 11) <> '~~XaNaFood~' then
    raise Exception.Create('Invalid archive block');

  I := PosEx('~', st, 12); // Determine Size of size (Integer or Int64).
  size := StrToInt64('$' + Copy(st, 12, I-12));
  Inc(I);
  lab := Copy(st, I, SizeOf(blck) - I);
end;

procedure CopyFileToStream(const lab, fileName: string; stream: TStream);
var
  f: TFileStream;
begin
  if FileExists(fileName) then
  begin
    f := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
    try
      WriteLabelBlock(lab, f.Size, stream);
      stream.CopyFrom(f, 0);
    finally
      f.Free;
    end;
  end;
end;

procedure WriteRegistrySettingsToStream(const section: string; strm: TStream; excludeList: TStrings = nil);
var
  mstream: TMemoryStream;
  reg: TExSettings;
begin
  mstream := nil;
  reg := CreateExSettings;
  try
    mstream := TMemoryStream.Create;
    if reg.Open(True) then
    begin
      reg.ExportToRegStream(section, mstream, excludeList);
//      mstream.SaveToFile('c:\temp_' + FixFilenameString(Section) + '.reg'); 
      WriteLabelBlock('Settings', mstream.Size, strm);
      strm.CopyFrom(mstream, 0);
    end;
  finally
    reg.Free;
    mstream.Free;
  end;
end;

procedure CreateFileFromStream(const dir, filename: string; stream: TStream; size: Int64);
var
  f: TFileStream;
begin
  ForceDirectories(dir);
  f := TFileStream.Create(dir + '\' + fileName, fmCreate);
  try
    if size <> 0 then
      f.CopyFrom(stream, size);
  finally
    f.Free;
  end;
end;


{ TXanaExporter }

procedure TXanaExporter.Export(const FileName: string; groups: TStrings; SettingsToo: Boolean);
var
  Compressor: TCompressionStream;
  ostream: TStream;
  i: Integer;
  grp, account, lastAccount: string;
  excludeList: TStrings;
begin
  Application.ProcessMessages;
  Compressor := nil;
  excludeList := nil;
  ostream := TFileStream.Create(FileName, fmCreate);
  try
    excludeList := TStringList.Create;
    if Assigned(OnProgress) then
      OnProgress(Self, 0, 0, '');
    Compressor := TCompressionStream.Create(clMax, ostream);
    WriteLabelBlock('Start', 0, Compressor);

    if SettingsToo then
    begin
      if Assigned(groups) and (groups.Count > 0) then
        excludeList.Add('Accounts');
      WriteRegistrySettingsToStream('', Compressor, excludeList);
    end;

    lastAccount := '~';
    if Assigned(groups) then
      for i := 0 to groups.Count - 1 do
      begin
        grp := groups[i];
        account := SplitString(':', grp);

        if Assigned(OnProgress) then
          OnProgress(self, i, groups.Count, grp);

        if account <> lastAccount then
        begin
          if SettingsToo then
          begin
            excludeList.Clear;
            excludeList.Add('Subscribed Groups');
            WriteRegistrySettingsToStream('Accounts\' + account, Compressor, excludeList);
            CopyFileToStream('Newsgroups~' + account , gMessageBaseRoot + '\' + account + '\newsgroups.dat', Compressor)
          end;
          lastAccount := account;
        end;

        if SettingsToo then
          WriteRegistrySettingsToStream('Accounts\' + account + '\Subscribed Groups\' + grp, Compressor, nil);
        CopyFileToStream('Articles~' + account + '~' + grp, gMessageBaseRoot + '\' + account + '\' + grp + '\articles.dat', Compressor);
        CopyFileToStream('Messages~' + account + '~' + grp, gMessageBaseRoot + '\' + account + '\' + grp + '\messages.dat', Compressor);
      end;
    WriteLabelBlock('End', 0, Compressor);
  finally
    Compressor.Free;
    ostream.Free;
    excludeList.Free;
  end;
end;

procedure TXanaExporter.Import(const FileName: string);
var
  expander: TDecompressionStream;
  istream: TFileStream;
  mstream: TMemoryStream;
  st, lab, account: string;
  size: Int64;
  reg: TExSettings;
begin
  expander := nil;
  istream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    expander := TDecompressionStream.Create(istream);

    ReadLabelBlock(lab, size, expander);

    if (lab <> 'Start') or (size <> 0) then
      raise Exception.Create('Invalid start archive block');

    ReadLabelBlock(lab, size, expander);

    while lab <> 'End' do
    begin
      if lab = 'Settings' then
      begin
        reg := nil;
        mstream := TMemoryStream.Create;
        try
          reg := CreateExSettings;
          mStream.CopyFrom(expander, size);
          mStream.Seek(0, soBeginning);
          reg.ImportFromRegStream(mStream);
          reg.Section := '';
          st := ExpandFileName(reg.GetStringValue('Messagebase Directory', ''));
          if st <> '' then
            gMessagebaseRoot := st;
        finally
          mStream.Free;
          reg.Free;
        end;
      end
      else
      begin
        st := SplitString('~', lab);

        if st = 'Newsgroups' then
          CreateFileFromStream(gMessageBaseRoot + '\' + lab, 'newsgroups.dat', expander, size)
        else
          if st = 'Articles' then
          begin
            account := SplitString('~', lab);
            CreateFileFromStream(gMessageBaseRoot + '\' + account + '\' + lab, 'articles.dat', expander, size);
          end
          else
            if st = 'Messages' then
            begin
              account := SplitString('~', lab);
              CreateFileFromStream(gMessageBaseRoot + '\' + account + '\' + lab, 'messages.dat', expander, size);
            end;
      end;
      ReadLabelBlock(lab, size, expander);
    end;
  finally
    expander.Free;
    istream.Free;
  end;
end;

end.
