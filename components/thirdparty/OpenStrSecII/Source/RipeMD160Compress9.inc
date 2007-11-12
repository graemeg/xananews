{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}
  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  xor ESI,EAX
  add ECX,ESI
  add ECX,$7A6D76E9
  add ECX,[EBP + 32]
  rol ECX,15
  add ECX,EBX
  rol EDI,10

  mov ESI,EDI
  xor ESI,EDX
  and ESI,ECX
  xor ESI,EDI
  add EBX,ESI
  add EBX,$7A6D76E9
  add EBX,[EBP + 24]
  rol EBX,5
  add EBX,EAX
  rol EDX,10

  mov ESI,EDX
  xor ESI,ECX
  and ESI,EBX
  xor ESI,EDX
  add EAX,ESI
  add EAX,$7A6D76E9
  add EAX,[EBP + 16]
  rol EAX,8
  add EAX,EDI
  rol ECX,10

  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  xor ESI,ECX
  add EDI,ESI
  add EDI,$7A6D76E9
  add EDI,[EBP + 4]
  rol EDI,11
  add EDI,EDX
  rol EBX,10

  mov ESI,EBX
  xor ESI,EAX
  and ESI,EDI
  xor ESI,EBX
  add EDX,ESI
  add EDX,$7A6D76E9
  add EDX,[EBP + 12]
  rol EDX,14
  add EDX,ECX
  rol EAX,10

  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  xor ESI,EAX
  add ECX,ESI
  add ECX,$7A6D76E9
  add ECX,[EBP + 44]
  rol ECX,14
  add ECX,EBX
  rol EDI,10

  mov ESI,EDI
  xor ESI,EDX
  and ESI,ECX
  xor ESI,EDI
  add EBX,ESI
  add EBX,$7A6D76E9
  add EBX,[EBP + 60]
  rol EBX,6
  add EBX,EAX
  rol EDX,10

  mov ESI,EDX
  xor ESI,ECX
  and ESI,EBX
  xor ESI,EDX
  add EAX,ESI
  add EAX,$7A6D76E9
  add EAX,[EBP]
  rol EAX,14
  add EAX,EDI
  rol ECX,10

  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  xor ESI,ECX
  add EDI,ESI
  add EDI,$7A6D76E9
  add EDI,[EBP + 20]
  rol EDI,6
  add EDI,EDX
  rol EBX,10

  mov ESI,EBX
  xor ESI,EAX
  and ESI,EDI
  xor ESI,EBX
  add EDX,ESI
  add EDX,$7A6D76E9
  add EDX,[EBP + 48]
  rol EDX,9
  add EDX,ECX
  rol EAX,10

  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  xor ESI,EAX
  add ECX,ESI
  add ECX,$7A6D76E9
  add ECX,[EBP + 8]
  rol ECX,12
  add ECX,EBX
  rol EDI,10

  mov ESI,EDI
  xor ESI,EDX
  and ESI,ECX
  xor ESI,EDI
  add EBX,ESI
  add EBX,$7A6D76E9
  add EBX,[EBP + 52]
  rol EBX,9
  add EBX,EAX
  rol EDX,10

  mov ESI,EDX
  xor ESI,ECX
  and ESI,EBX
  xor ESI,EDX
  add EAX,ESI
  add EAX,$7A6D76E9
  add EAX,[EBP + 36]
  rol EAX,12
  add EAX,EDI
  rol ECX,10

  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  xor ESI,ECX
  add EDI,ESI
  add EDI,$7A6D76E9
  add EDI,[EBP + 28]
  rol EDI,5
  add EDI,EDX
  rol EBX,10

  mov ESI,EBX
  xor ESI,EAX
  and ESI,EDI
  xor ESI,EBX
  add EDX,ESI
  add EDX,$7A6D76E9
  add EDX,[EBP + 40]
  rol EDX,15
  add EDX,ECX
  rol EAX,10

  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  xor ESI,EAX
  add ECX,ESI
  add ECX,$7A6D76E9
  add ECX,[EBP + 56]
  rol ECX,8
  add ECX,EBX
  rol EDI,10


