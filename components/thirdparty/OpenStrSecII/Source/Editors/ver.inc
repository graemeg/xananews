{$IFDEF VER120}
  {$DEFINE D4}
  {$DEFINE D4UP}
  {$DEFINE MSWINDOWS}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE D5}
  {$DEFINE D4UP}
  {$DEFINE D5UP}     
  {$DEFINE MSWINDOWS}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE D6}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE D7}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE D7UP}       
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}             

{$IFNDEF D4UP}
  {$DEFINE D9}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE D7UP}
{$ENDIF}

{$IFDEF D4UP}
  {$DEFINE INT64}
  {$DEFINE LONGWORD}
{$ENDIF}

// Remove any appropriate conditional to reduce the footprint:
{$DEFINE CTR}       // Enables CTR mode
{$DEFINE ABC}       // Enables ABC mode
{$DEFINE ECB}       // Enables ECB mode
{$DEFINE CFB}       // Enables CFB mode
{$DEFINE OFB}       // Enables OFB mode
{$DEFINE CBC}       // Enables CBC mode           -- Used by TLS / SSL
{$DEFINE PCFB}      // Enables PCFB mode
{$DEFINE PIPEDPCFB} // Enables PipedPCFB mode
{$DEFINE AESWRAP}   // Enables AES-Wrap mode
{$DEFINE RIJNDAEL}  // Enables AES                -- Used by TLS
{$DEFINE TWOFISH}
{$DEFINE BLOWFISH}
{$DEFINE DES}       // Enabled DES and TripleDES  -- Used by TLS / SSL
{$DEFINE ARCFOUR}   // Enables ARC4               -- Used by TLS / SSL
{$DEFINE ARCTWO}    // Enables RC2                -- Used by CMS - Deprecated  
{$DEFINE SHA1}      // Enables SHA1               -- Used by TLS / SSL
{$DEFINE MD2}       // Enables MD2                -- Used by X.509
{$DEFINE MD5}       // Enables MD5                -- Used by TLS / SSL
{$DEFINE RIPEMD160} // Enables RipeMD160
{$DEFINE SHA256}    // Enables SHA-256
{$DEFINE SHA512}    // Enables SHA-384 & SHA-512
// WARNING: Do NOT alter these conditionals unless you are absolutely sure
// your application will NEVER use a particular cipher, cipher mode or hash.

{$IFDEF NO_CTR}       {$UNDEF CTR}       {$ENDIF}
{$IFDEF NO_ABC}       {$UNDEF ABC}       {$ENDIF}
{$IFDEF NO_ECB}       {$UNDEF ECB}       {$ENDIF}
{$IFDEF NO_CFB}       {$UNDEF CFB}       {$ENDIF}
{$IFDEF NO_OFB}       {$UNDEF OFB}       {$ENDIF}
{$IFDEF NO_CBC}       {$UNDEF CBC}       {$ENDIF}
{$IFDEF NO_PCFB}      {$UNDEF PCFB}      {$ENDIF}
{$IFDEF NO_PIPEDPCFB} {$UNDEF PIPEDPCFB} {$ENDIF}
{$IFDEF NO_AESWRAP}   {$UNDEF AESWRAP}   {$ENDIF}
{$IFDEF NO_RIJNDAEL}  {$UNDEF RIJNDAEL}  {$ENDIF}
{$IFDEF NO_TWOFISH}   {$UNDEF TWOFISH}   {$ENDIF}
{$IFDEF NO_BLOWFISH}  {$UNDEF BLOWFISH}  {$ENDIF}
{$IFDEF NO_DES}       {$UNDEF DES}       {$ENDIF}
{$IFDEF NO_ARCFOUR}   {$UNDEF ARCFOUR}   {$ENDIF}
{$IFDEF NO_ARCTWO}    {$UNDEF ARCTWO}    {$ENDIF}
{$IFDEF NO_SHA1}      {$UNDEF SHA1}      {$ENDIF}
{$IFDEF NO_MD2}       {$UNDEF MD2}       {$ENDIF}
{$IFDEF NO_MD5}       {$UNDEF MD5}       {$ENDIF}
{$IFDEF NO_RIPEMD160} {$UNDEF RIPEMD160} {$ENDIF}
{$IFDEF NO_SHA256}    {$UNDEF SHA256}    {$ENDIF}
{$IFDEF NO_SHA512}    {$UNDEF SHA512}    {$ENDIF}

{$DEFINE AES_BG} // Brian Gladman's assembler implemenentation of Rijndael.
{$IFDEF AES_BG}
{.$DEFINE USE_MMX}   // Use MMX registers for temporary storage
{.$DEFINE USE_EMMX}  // Use floating point operations
{$ENDIF}

{$DEFINE FreeManagedKeys} // Ifdef = Private keys are destroyed when the key
                          // ring is destroyed. Else = Private keys are
                          // destroyed when they go out of scope.

{$IFDEF NO_FreeManagedKeys} {$UNDEF FreeManagedKeys} {$ENDIF}

{.$DEFINE STATIC_PRIMES}  // Ifdef = SmallPrimes.Primes is linked in as a
                          // constant table. Otherwise (default) this table
                          // is created dynamically.
{$IFDEF NO_STATIC_PRIMES} {$UNDEF STATIC_PRIMES} {$ENDIF}

{.$DEFINE THIN_TASN1STRUCT}

{$IFDEF NO_THIN_TASN1STRUCT} {$UNDEF THIN_TASN1STRUCT} {$ENDIF}

{$DEFINE USE_INDY_HTTP}

{.$DEFINE UNIQUENAMES}                               
{$IFDEF NO_UNIQUENAMES} {$UNDEF UNIQUENAMES} {$ENDIF}
{$DEFINE INIT_SECTIONS}   // Ifndef = You must include StrSecInit and
                          // call the appropriate Init... methods.
                          // If StrSecII.InitMPYarrow(True) or alternatively
                          // MPYarrow.CreateGlobalYarrow(True) is called,
                          // TMPYarrow.Accumulate will not launch a reseed
                          // thread, but instead block execution.
                          // On shutdown you *may* call:
                          //   The Finalize routine in each of the used units
                          //    (if this routine is declared in these units),
                          //   ASN1.CollectGarbage;
                          //   MPArith.SetMPPoolSize(0);
                          //   MPYarrow.FreeGlobalYarrow;
{$IFDEF NO_INIT_SECTIONS} {$UNDEF INIT_SECTIONS} {$ENDIF}
{$DEFINE FINI_SECTIONS}   // It is currently *not* recommended to undefine this
                          // one.              
{$IFDEF NO_FINI_SECTIONS} {$UNDEF FINI_SECTIONS} {$ENDIF}

{$IFDEF INIT_SECTIONS}
{$DEFINE INITIALIZATION}
{$ENDIF}
{$IFDEF FINI_SECTIONS}
{$DEFINE INITIALIZATION}
{$ENDIF}

{$BOOLEVAL OFF}

{ A lot of if...then and while...do conditions will result in AVs if
  complete boolean evaluation is turned on.}

{$EXTENDEDSYNTAX ON}

{ Many units will not compile if extended syntax is turned off, mainly because I
  am very fond of the Result variable.}

{$LONGSTRINGS ON}

{ This is essential. Strings are type casted as pointers in numerous places in
  the code, and this will not work with the short string type.}

{$OPENSTRINGS ON}

{ It is likely that some StrSecII code will not compile or not function properly
  if this directive is turned off.}

{$OPTIMIZATION ON}

{ Optimization is recommended.}

{$OVERFLOWCHECKS OFF}

{ Overflow checks must be turned off, in particular to make some cryptographic
  pascal implementations function.}

{$RANGECHECKS OFF}

{ Turn range checks off if necessary, although this switch should already be
  turned off locallly where needed.}

{$STACKFRAMES OFF}

{ Essential. A lot of the asm code will not work at all with stack frames.}

{$TYPEDADDRESS OFF}

{ Essential. A lot of code will not compile with a typed @ operator.}
