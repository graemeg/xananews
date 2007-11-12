{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}
  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EBX
  add EDX,ESI
  add EDX,$6ED9EBA1
  add EDX,[EBP + 12]
  rol EDX,11
  add EDX,ECX
  rol EAX,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,EAX
  add ECX,ESI
  add ECX,$6ED9EBA1
  add ECX,[EBP + 40]
  rol ECX,13
  add ECX,EBX
  rol EDI,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EDI
  add EBX,ESI
  add EBX,$6ED9EBA1
  add EBX,[EBP + 56]
  rol EBX,6
  add EBX,EAX
  rol EDX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EDX
  add EAX,ESI
  add EAX,$6ED9EBA1
  add EAX,[EBP + 16]
  rol EAX,7
  add EAX,EDI
  rol ECX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,ECX
  add EDI,ESI
  add EDI,$6ED9EBA1
  add EDI,[EBP + 36]
  rol EDI,14
  add EDI,EDX
  rol EBX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EBX
  add EDX,ESI
  add EDX,$6ED9EBA1
  add EDX,[EBP + 60]
  rol EDX,9
  add EDX,ECX
  rol EAX,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,EAX
  add ECX,ESI
  add ECX,$6ED9EBA1
  add ECX,[EBP + 32]
  rol ECX,13
  add ECX,EBX
  rol EDI,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EDI
  add EBX,ESI
  add EBX,$6ED9EBA1
  add EBX,[EBP + 4]
  rol EBX,15
  add EBX,EAX
  rol EDX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EDX
  add EAX,ESI
  add EAX,$6ED9EBA1
  add EAX,[EBP + 8]
  rol EAX,14
  add EAX,EDI
  rol ECX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,ECX
  add EDI,ESI
  add EDI,$6ED9EBA1
  add EDI,[EBP + 28]
  rol EDI,8
  add EDI,EDX
  rol EBX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EBX
  add EDX,ESI
  add EDX,$6ED9EBA1
  add EDX,[EBP]
  rol EDX,13
  add EDX,ECX
  rol EAX,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,EAX
  add ECX,ESI
  add ECX,$6ED9EBA1
  add ECX,[EBP + 24]
  rol ECX,6
  add ECX,EBX
  rol EDI,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EDI
  add EBX,ESI
  add EBX,$6ED9EBA1
  add EBX,[EBP + 52]
  rol EBX,5
  add EBX,EAX
  rol EDX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EDX
  add EAX,ESI
  add EAX,$6ED9EBA1
  add EAX,[EBP + 44]
  rol EAX,12
  add EAX,EDI
  rol ECX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,ECX
  add EDI,ESI
  add EDI,$6ED9EBA1
  add EDI,[EBP + 20]
  rol EDI,7
  add EDI,EDX
  rol EBX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EBX
  add EDX,ESI
  add EDX,$6ED9EBA1
  add EDX,[EBP + 48]
  rol EDX,5
  add EDX,ECX
  rol EAX,10

