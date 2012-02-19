unit XnRawByteStrings;

interface

uses
  SysUtils;

function RawCompareStr(const S1, S2: RawByteString): Integer;
function RawCompareText(const S1, S2: RawByteString): Integer;
function RawFetch(var AInput: RawByteString; const ADelim: RawByteString = ' '; const ADelete: Boolean = True): RawByteString;
function RawIntToStr(Value: Integer): RawByteString; overload;
function RawIntToStr(Value: Int64): RawByteString; overload;
function RawLowerCase(S: RawByteString): RawByteString;
function RawPos(const substr, str: RawByteString): Integer;
function RawSameText(const S1, S2: RawByteString): Boolean;
function RawSplitString(const search: RawByteString; var s: RawByteString): RawByteString;
function RawStringOfChar(ch: AnsiChar; count: Integer): RawByteString;
function RawStringReplace(const S, OldPattern, NewPattern: RawByteString; Flags: TReplaceFlags): RawByteString;
function RawStrToInt(const S: RawByteString): Integer;
function RawStrToIntDef(const S: RawByteString; Default: Integer): Integer;
function RawStrToInt64(const S: RawByteString): Int64;
function RawStrToInt64Def(const S: RawByteString; const Default: Int64): Int64;
function RawTrim(const S: RawByteString): RawByteString;
function RawTrimRight(const S: RawByteString): RawByteString;


implementation

uses
  AnsiStrings, RTLConsts, SysConst;

procedure ConvertErrorFmt(ResString: PResStringRec; const Args: array of const); local;
begin
  raise EConvertError.CreateResFmt(ResString, Args);
end;

procedure StrToIntError(const S: RawByteString);
begin
  raise EConvertError.CreateResFmt(@SInvalidInteger, [S]);
end;


{$IFDEF CPUX64}
// TODO: see if this can/need be improved in the future
function RawCompareStr(const S1, S2: RawByteString): Integer;
begin
  Result := CompareStr(S1, S2);
end;
{$ELSE}
// CompareStr() from Pierre le Riche as found at the FastCode project.
function RawCompareStr(const S1, S2: RawByteString): Integer;
asm
  cmp eax, edx
  je @SameString
  {Is either of the strings perhaps nil?}
  test eax, edx
  jz @PossibleNilString
  {Compare the first four characters (there has to be a trailing #0). In random
   string compares this can save a lot of CPU time.}
@BothNonNil:
  {Compare the first character}
  movzx ecx, byte ptr [edx]
  cmp cl, [eax]
  je @FirstCharacterSame
  {First character differs}
  movzx eax, byte ptr [eax]
  sub eax, ecx
  ret
@FirstCharacterSame:
  {Save ebx}
  push ebx
  {Set ebx = length(S1)}
  mov ebx, [eax - 4]
  xor ecx, ecx
  {Set ebx = length(S1) - length(S2)}
  sub ebx, [edx - 4]
  {Save the length difference on the stack}
  push ebx
  {Set ecx = 0 if length(S1) < length(S2), $ffffffff otherwise}
  adc ecx, -1
  {Set ecx = - min(length(S1), length(S2))}
  and ecx, ebx
  sub ecx, [eax - 4]
  {Adjust the pointers to be negative based}
  sub eax, ecx
  sub edx, ecx
@CompareLoop:
  mov ebx, [eax + ecx]
  xor ebx, [edx + ecx]
  jnz @Mismatch
  add ecx, 4
  js @CompareLoop
  {All characters match - return the difference in length}
@MatchUpToLength:
  pop eax
  pop ebx
  ret
@Mismatch:
  bsf ebx, ebx
  shr ebx, 3
  add ecx, ebx
  jns @MatchUpToLength
  movzx eax, byte ptr [eax + ecx]
  movzx edx, byte ptr [edx + ecx]
  sub eax, edx
  pop ebx
  pop ebx
  ret
  {It is the same string}
@SameString:
  xor eax, eax
  ret
  {Good possibility that at least one of the strings are nil}
@PossibleNilString:
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothNonNil
  {Return first string length: second string is nil}
  mov eax, [eax - 4]
  ret
@FirstStringNil:
  {Return 0 - length(S2): first string is nil}
  sub eax, [edx - 4]
end;
{$ENDIF}


{$IFDEF CPUX64}
// TODO: see if this can/need be improved in the future
function RawCompareText(const S1, S2: RawByteString): Integer;
begin
  Result := CompareText(S1, S2);
end;
{$ELSE}
// CompareText_JOH_IA32_5 from John O'Harrow as found at the FastCode project.
function RawCompareText(const S1, S2: RawByteString): Integer;
asm
  cmp     eax, edx
  je      @@Same             {S1 = S2}
  test    eax, edx
  jnz     @@Compare
  test    eax, eax
  jz      @FirstNil          {S1 = NIL}
  test    edx, edx
  jnz     @@Compare          {S1 <> NIL and S2 <> NIL}
  mov     eax, [eax-4]       {S2 = NIL, Result = Length(S1)}
  ret
@@Same:
  xor     eax, eax
  ret
@FirstNil:
  sub     eax, [edx-4]       {S1 = NIL, Result = -Length(S2)}
  ret
@@Compare:
  push    ebx
  push    ebp
  push    edi
  push    esi
  mov     ebx, [eax-4]       {Length(S1)}
  sub     ebx, [edx-4]       {Default Result if All Compared Characters Match}
  push    ebx                {Save Default Result}
  sbb     ebp, ebp
  and     ebp, ebx
  add     ebp, [edx-4]       {Compare Length = Min(Length(S1),Length(S2))}
  add     eax, ebp           {End of S1}
  add     edx, ebp           {End of S2}
  neg     ebp                {Negate Compare Length}
@@MainLoop:                  {Compare 4 Characters per Loop}
  mov     ebx, [eax+ebp]
  mov     ecx, [edx+ebp]
  cmp     ebx, ecx
  je      @@Next
  mov     esi, ebx           {Convert 4 Chars in EBX into Uppercase}
  or      ebx, $80808080
  mov     edi, ebx
  sub     ebx, $7B7B7B7B
  xor     edi, esi
  or      ebx, $80808080
  sub     ebx, $66666666
  and     ebx, edi
  shr     ebx, 2
  xor     ebx, esi
  mov     esi, ecx           {Convert 4 Chars in ECX into Uppercase}
  or      ecx, $80808080
  mov     edi, ecx
  sub     ecx, $7B7B7B7B
  xor     edi, esi
  or      ecx, $80808080
  sub     ecx, $66666666
  and     ecx, edi
  shr     ecx, 2
  xor     ecx, esi
  cmp     ebx, ecx
  jne     @@CheckDiff
@@Next:
  add     ebp, 4
  jl      @@MainLoop         {Loop until all required Characters Compared}
  pop     eax                {Default Result}
  jmp     @@Done
@@CheckDiff:
  pop     eax                {Default Result}
@@DiffLoop:
  cmp     cl, bl
  jne     @@SetResult
  add     ebp, 1
  jz      @@Done             {Difference after Compare Length}
  shr     ecx, 8
  shr     ebx, 8
  jmp     @@DiffLoop
@@SetResult:
  movzx   eax, bl            {Set Result from Character Difference}
  and     ecx, $ff
  sub     eax, ecx
@@Done:
  pop     esi
  pop     edi
  pop     ebp
  pop     ebx
end;
{$ENDIF}


function RawFetch(var AInput: RawByteString; const ADelim: RawByteString = ' ';
  const ADelete: Boolean = True): RawByteString;
var
  LPos: Integer;
begin
  LPos := Pos(ADelim, AInput);
  if LPos = 0 then
  begin
    Result := AInput;
    if ADelete then begin
      AInput := '';    {Do not Localize}
    end;
  end
  else
  begin
    Result := Copy(AInput, 1, LPos - 1);
    if ADelete then
      AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
  end;
end;


{$IFDEF CPUX64}
// TODO: see if this can/need be improved in the future
function RawIntToStr(Value: Integer): RawByteString;
begin
  Result := IntToStr(Value);
end;
{$ELSE}
const
  TwoDigitLookup : packed array[0..99] of array[1..2] of AnsiChar =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

// IntToStr32_JOH_IA32_6 from John O'Harrow as found at the FastCode project.
function RawIntToStr(Value: Integer): RawByteString;
asm
  push   ebx
  push   edi
  push   esi
  mov    ebx, eax                {Value}
  sar    ebx, 31                 {0 for +ve Value or -1 for -ve Value}
  xor    eax, ebx
  sub    eax, ebx                {ABS(Value)}
  mov    esi, 10                 {Max Digits in Result}
  mov    edi, edx                {@Result}
  cmp    eax, 10
  sbb    esi, 0
  cmp    eax, 100
  sbb    esi, 0
  cmp    eax, 1000
  sbb    esi, 0
  cmp    eax, 10000
  sbb    esi, 0
  cmp    eax, 100000
  sbb    esi, 0
  cmp    eax, 1000000
  sbb    esi, 0
  cmp    eax, 10000000
  sbb    esi, 0
  cmp    eax, 100000000
  sbb    esi, 0
  cmp    eax, 1000000000
  sbb    esi, ebx                {Digits (Including Sign Character)}
  mov    ecx, [edx]              {Result}
  test   ecx, ecx
  je     @@NewStr                {Create New String for Result}
  cmp    dword ptr [ecx-8], 1
  jne    @@ChangeStr             {Reference Count <> 1}
  cmp    esi, [ecx-4]
  je     @@LengthOk              {Existing Length = Required Length}
  sub    ecx, 8                  {Allocation Address}
  push   eax                     {ABS(Value)}
  push   ecx
  mov    eax, esp
  lea    edx, [esi+9]            {New Allocation Size}
  call   system.@ReallocMem      {Reallocate Result String}
  pop    ecx
  pop    eax                     {ABS(Value)}
  add    ecx, 8                  {Result}
  mov    [ecx-4], esi            {Set New Length}
  mov    byte ptr [ecx+esi], 0   {Add Null Terminator}
  mov    [edi], ecx              {Set Result Address}
  jmp    @@LengthOk
@@ChangeStr:
  mov     edx, dword ptr [ecx-8]  {Reference Count}
  add     edx, 1
  jz      @@NewStr                {RefCount = -1 (String Constant)}
  lock    dec dword ptr [ecx-8]   {Decrement Existing Reference Count}
@@NewStr:
  push   eax                     {ABS(Value)}
  mov    eax, esi                {Length}
  call   system.@NewAnsiString
  mov    [edi], eax              {Set Result Address}
  mov    ecx, eax                {Result}
  pop    eax                     {ABS(Value)}
@@LengthOk:
  mov    byte ptr [ecx], '-'     {Store '-' Character (May be Overwritten)}
  add    esi, ebx                {Digits (Excluding Sign Character)}
  sub    ecx, ebx                {Destination of 1st Digit}
  sub    esi, 2                  {Digits (Excluding Sign Character) - 2}
  jle    @@FinalDigits           {1 or 2 Digit Value}
  cmp    esi, 8                  {10 Digit Value?}
  jne    @@SetResult             {Not a 10 Digit Value}
  sub    eax, 2000000000         {Digit 10 must be either '1' or '2'}
  mov    dl, '2'
  jnc    @@SetDigit10            {Digit 10 = '2'}
  mov    dl, '1'                 {Digit 10 = '1'}
  add    eax, 1000000000
@@SetDigit10:
  mov    [ecx], dl               {Save Digit 10}
  mov    esi, 7                  {9 Digits Remaining}
  add    ecx, 1                  {Destination of 2nd Digit}
@@SetResult:
  mov    edi, $28F5C29           {((2^32)+100-1)/100}
@@Loop:
  mov    ebx, eax                {Dividend}
  mul    edi                     {EDX = Dividend DIV 100}
  mov    eax, edx                {Set Next Dividend}
  imul   edx, -200               {-2 * (100 * Dividend DIV  100)}
  movzx  edx, word ptr [TwoDigitLookup+ebx*2+edx] {Dividend MOD 100 in ASCII}
  mov    [ecx+esi], dx
  sub    esi, 2
  jg     @@Loop                  {Loop Until 1 or 2 Digits Remaining}
@@FinalDigits:
  pop    esi
  pop    edi
  pop    ebx
  jnz    @@LastDigit
  movzx  eax, word ptr [TwoDigitLookup+eax*2]
  mov    [ecx], ax               {Save Final 2 Digits}
  ret
@@LastDigit:
  or     al , '0'                {Ascii Adjustment}
  mov    [ecx], al               {Save Final Digit}
end;
{$ENDIF}


function RawIntToStr(Value: Int64): RawByteString;
begin
  Result := RawByteString(IntToStr(Value));
end;


function RawLowerCase(S: RawByteString): RawByteString; inline;
begin
  Result := AnsiStrings.LowerCase(S);
end;


function RawPos(const substr, str: RawByteString): Integer; inline;
begin
  Result := Pos(substr, str);
end;


function RawSameText(const S1, S2: RawByteString): Boolean; inline;
begin
  Result := RawCompareText(S1, S2) = 0;
end;


function RawSplitString(const search: RawByteString; var s: RawByteString): RawByteString;
var
  p, l: Integer;
begin
  l := Length(search);
  p := Pos(search, s);
  if p > 0 then
  begin
    Result := RawTrim(Copy(s, 1, p - 1));
    s := RawTrim(Copy(s, p + l, MaxInt));
  end
  else
  begin
    Result := RawTrim(s);
    s := ''
  end;
end;


function RawStringOfChar(ch: AnsiChar; count: Integer): RawByteString; inline;
begin
  Result := StringOfChar(ch, count);
end;


function RawStringReplace(const S, OldPattern, NewPattern: RawByteString;
  Flags: TReplaceFlags): RawByteString; inline;
begin
  Result := AnsiStrings.StringReplace(S, OldPattern, NewPattern, Flags);
end;


{$IFDEF CPUX64}
// TODO: see if this can/need be improved in the future
function RawStrToInt(const S: RawByteString): Integer;
begin
  Result := StrToInt(S);
end;
{$ELSE}
// StrToInt32_JOH_IA32_7 from John O'Harrow as found at the FastCode project.
function RawStrToInt(const S: RawByteString): Integer;
asm
  test  eax, eax
  jz    @@Failed
  push  eax
  push  ebx
  push  edi
  push  esi
  mov   edx, eax            {String Pointer}
  xor   ebx, ebx            {Clear Sign Flag (top bit) and Valid Flag}
  xor   eax, eax            {Clear Result}
  mov   edi, '0'
  mov   esi, 9
@@Trim:                     {Strip Leading Spaces}
  movzx ecx, [edx]
  inc   edx
  cmp   cl, ' '
  je    @@Trim
  cmp   ecx, edi            {cl <= '0'?}
  jle   @@CheckFirstChar    {Yes, Check +, -, $, 0x, 0X}
  test  cl, not 'x'
  jz    @@CheckX            {May start with 'x' or 'X'}

@@Numeric:
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@TestValid         {Not '0'..'9'}
  mov   eax, ecx            {Result := Digit}

  movzx ecx, [edx]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+1]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+2]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+3]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+4]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+5]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+6]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+7]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

@@NumLoop:
  movzx ecx, [edx+8]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@NumDone           {Not '0'..'9'}
  cmp   eax, MaxInt/10
  ja    @@Error
  inc   edx
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}
  jmp   @@NumLoop

@@TestValid:
  test  bl, bl              {Got Valid Number?}
  jz    @@Error             {No, Error}
@@CheckDone:
  add   ecx, edi            {Last Character = Null Terminator?}
  jnz   @@Error             {No, Error}
  sar   ebx, 31             {Set Each Bit to Top Bit (Sign Flag)}
  xor   eax, ebx            {Negate Result if Necessary}
  sub   eax, ebx
  pop   esi
  pop   edi
  pop   ebx
  pop   ecx
  ret

@@NumDone:
  cmp   eax, $80000000
  jb    @@CheckDone         {No Overflow}
  jne   @@Error             {Overflow}
  test  ebx, ebx            {Sign Flag Set?}
  js    @@CheckDone         {Yes, Result is Valid (-MaxInt-1)}
@@Error:
  pop   esi
  pop   edi
  pop   ebx
  pop   eax
@@Failed:
  jmp   StrToIntError

@@CheckFirstChar:           {First Char <= '0'}
  cmp   cl, '$'
  je    @@Hex
  cmp   cl, '-'
  je    @@Minus
  cmp   cl, '+'
  je    @@Plus
  cmp   ecx, edi            {Starts with '0'?}
  jne   @@Error             {No, Error}
  movzx ecx, [edx]          {Character after '0'}
  mov   bl, 1               {Valid := True}
  inc   edx
  jmp   @@CheckX
@@Minus:
  mov   ebx, $80000000      {Set Sign Flag (Top Bit), Valid := False}
@@Plus:
  movzx ecx, [edx]          {Character after '+' or '-'}
  inc   edx
  cmp   cl, '$'
  je    @@Hex               {Starts with '+$' or '-$'}
  cmp   ecx, edi            {Starts with '+0' or '-0'?}
  jne   @@CheckAlpha        {No, May start with '+x', '-x', '+X' or '-X'}
  movzx ecx, [edx]          {Character after '+0' or '-0'}
  inc   ebx                 {Starts with '+0' or '-0', Valid := True}
  inc   edx
@@CheckAlpha:
  test  cl, not 'x'         {Could Char be 'x' or 'X'?}
  jnz   @@Numeric           {No, Assume Numeric}
@@CheckX:
  or    cl, $20             {'X' -> 'x'}
  cmp   cl, 'x'             {Char = 'X' or 'x'?}
  movzx ecx, [edx-1]        {Reload Character}
  jne   @@Numeric           {Does Not start with +/-('x', 'X', '0x' or '0X')}
  mov   bl, 0               {Reset Valid to False}
@@Hex:
  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex1              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@TestValid         {Check for Valid and Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex1:
  mov   eax, ecx            {Result = Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex2              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex2:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex3              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex3:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex4              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex4:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex5              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex5:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex6              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex6:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex7              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex7:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex8              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex8:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

@@HexLoop:
  movzx ecx, [edx]
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@CheckRange        {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@CheckRange:
  cmp   eax, MaxInt/8       {High(ULONG) div 16}
  ja    @@Error             {Overflow}
  shl   eax, 4
(*
  shl   eax, 4              //Using this instead of the above 3 lines wrongly
  jc    @@Error             //  passes validation with S='$200000000000000'
*)
  add   eax, ecx            {Result = Result * 16 + Digit}
  inc   edx
  jmp   @@HexLoop
end;
{$ENDIF}


// TODO: see if this can/need be improved in the future
function RawStrToIntDef(const S: RawByteString; Default: Integer): Integer;
var
  E: Integer;
begin
  Val(string(S), Result, E);
  if E <> 0 then Result := Default;
end;


// TODO: see if this can/need be improved in the future
function RawStrToInt64(const S: RawByteString): Int64;
var
  E: Integer;
begin
  Val(string(S), Result, E);
  if E <> 0 then
    ConvertErrorFmt(@SInvalidInteger, [S]);
end;


// TODO: see if this can/need be improved in the future
function RawStrToInt64Def(const S: RawByteString; const Default: Int64): Int64;
var
  E: Integer;
begin
  Val(string(S), Result, E);
  if E <> 0 then
    Result := Default;
end;


function RawTrim(const S: RawByteString): RawByteString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do
    Inc(I);

  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;


function RawTrimRight(const S: RawByteString): RawByteString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do
    Dec(I);
  Result := Copy(S, 1, I);
end;

end.
