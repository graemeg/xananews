{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}
  mov ESI,EDX
  xor ESI,EDI
  and ESI,EAX
  xor ESI,EDI
  add ECX,ESI
  add ECX,$8F1BBCDC
  add ECX,[EBP + 4]
  rol ECX,11
  add ECX,EBX
  rol EDI,10

  mov ESI,ECX
  xor ESI,EDX
  and ESI,EDI
  xor ESI,EDX
  add EBX,ESI
  add EBX,$8F1BBCDC
  add EBX,[EBP + 36]
  rol EBX,12
  add EBX,EAX
  rol EDX,10

  mov ESI,EBX
  xor ESI,ECX
  and ESI,EDX
  xor ESI,ECX
  add EAX,ESI
  add EAX,$8F1BBCDC
  add EAX,[EBP + 44]
  rol EAX,14
  add EAX,EDI
  rol ECX,10

  mov ESI,EAX
  xor ESI,EBX
  and ESI,ECX
  xor ESI,EBX
  add EDI,ESI
  add EDI,$8F1BBCDC
  add EDI,[EBP + 40]
  rol EDI,15
  add EDI,EDX
  rol EBX,10

  mov ESI,EDI
  xor ESI,EAX
  and ESI,EBX
  xor ESI,EAX
  add EDX,ESI
  add EDX,$8F1BBCDC
  add EDX,[EBP]
  rol EDX,14
  add EDX,ECX
  rol EAX,10

  mov ESI,EDX
  xor ESI,EDI
  and ESI,EAX
  xor ESI,EDI
  add ECX,ESI
  add ECX,$8F1BBCDC
  add ECX,[EBP + 32]
  rol ECX,15
  add ECX,EBX
  rol EDI,10

  mov ESI,ECX
  xor ESI,EDX
  and ESI,EDI
  xor ESI,EDX
  add EBX,ESI
  add EBX,$8F1BBCDC
  add EBX,[EBP + 48]
  rol EBX,9
  add EBX,EAX
  rol EDX,10

  mov ESI,EBX
  xor ESI,ECX
  and ESI,EDX
  xor ESI,ECX
  add EAX,ESI
  add EAX,$8F1BBCDC
  add EAX,[EBP + 16]
  rol EAX,8
  add EAX,EDI
  rol ECX,10

  mov ESI,EAX
  xor ESI,EBX
  and ESI,ECX
  xor ESI,EBX
  add EDI,ESI
  add EDI,$8F1BBCDC
  add EDI,[EBP + 52]
  rol EDI,9
  add EDI,EDX
  rol EBX,10

  mov ESI,EDI
  xor ESI,EAX
  and ESI,EBX
  xor ESI,EAX
  add EDX,ESI
  add EDX,$8F1BBCDC
  add EDX,[EBP + 12]
  rol EDX,14
  add EDX,ECX
  rol EAX,10

  mov ESI,EDX
  xor ESI,EDI
  and ESI,EAX
  xor ESI,EDI
  add ECX,ESI
  add ECX,$8F1BBCDC
  add ECX,[EBP + 28]
  rol ECX,5
  add ECX,EBX
  rol EDI,10

  mov ESI,ECX
  xor ESI,EDX
  and ESI,EDI
  xor ESI,EDX
  add EBX,ESI
  add EBX,$8F1BBCDC
  add EBX,[EBP + 60]
  rol EBX,6
  add EBX,EAX
  rol EDX,10

  mov ESI,EBX
  xor ESI,ECX
  and ESI,EDX
  xor ESI,ECX
  add EAX,ESI
  add EAX,$8F1BBCDC
  add EAX,[EBP + 56]
  rol EAX,8
  add EAX,EDI
  rol ECX,10

  mov ESI,EAX
  xor ESI,EBX
  and ESI,ECX
  xor ESI,EBX
  add EDI,ESI
  add EDI,$8F1BBCDC
  add EDI,[EBP + 20]
  rol EDI,6
  add EDI,EDX
  rol EBX,10

  mov ESI,EDI
  xor ESI,EAX
  and ESI,EBX
  xor ESI,EAX
  add EDX,ESI
  add EDX,$8F1BBCDC
  add EDX,[EBP + 24]
  rol EDX,5
  add EDX,ECX
  rol EAX,10

  mov ESI,EDX
  xor ESI,EDI
  and ESI,EAX
  xor ESI,EDI
  add ECX,ESI
  add ECX,$8F1BBCDC
  add ECX,[EBP + 8]
  rol ECX,12
  add ECX,EBX
  rol EDI,10

