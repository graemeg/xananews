{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}
  mov ESI,EAX
  xor ESI,EBX
  and ESI,ECX
  xor ESI,EBX
  add EDI,ESI
  add EDI,$5C4DD124
  add EDI,[EBP + 24]
  rol EDI,9
  add EDI,EDX
  rol EBX,10

  mov ESI,EDI
  xor ESI,EAX
  and ESI,EBX
  xor ESI,EAX
  add EDX,ESI
  add EDX,$5C4DD124
  add EDX,[EBP + 44]
  rol EDX,13
  add EDX,ECX
  rol EAX,10

  mov ESI,EDX
  xor ESI,EDI
  and ESI,EAX
  xor ESI,EDI
  add ECX,ESI
  add ECX,$5C4DD124
  add ECX,[EBP + 12]
  rol ECX,15
  add ECX,EBX
  rol EDI,10

  mov ESI,ECX
  xor ESI,EDX
  and ESI,EDI
  xor ESI,EDX
  add EBX,ESI
  add EBX,$5C4DD124
  add EBX,[EBP + 28]
  rol EBX,7
  add EBX,EAX
  rol EDX,10

  mov ESI,EBX
  xor ESI,ECX
  and ESI,EDX
  xor ESI,ECX
  add EAX,ESI
  add EAX,$5C4DD124
  add EAX,[EBP]
  rol EAX,12
  add EAX,EDI
  rol ECX,10

  mov ESI,EAX
  xor ESI,EBX
  and ESI,ECX
  xor ESI,EBX
  add EDI,ESI
  add EDI,$5C4DD124
  add EDI,[EBP + 52]
  rol EDI,8
  add EDI,EDX
  rol EBX,10

  mov ESI,EDI
  xor ESI,EAX
  and ESI,EBX
  xor ESI,EAX
  add EDX,ESI
  add EDX,$5C4DD124
  add EDX,[EBP + 20]
  rol EDX,9
  add EDX,ECX
  rol EAX,10

  mov ESI,EDX
  xor ESI,EDI
  and ESI,EAX
  xor ESI,EDI
  add ECX,ESI
  add ECX,$5C4DD124
  add ECX,[EBP + 40]
  rol ECX,11
  add ECX,EBX
  rol EDI,10

  mov ESI,ECX
  xor ESI,EDX
  and ESI,EDI
  xor ESI,EDX
  add EBX,ESI
  add EBX,$5C4DD124
  add EBX,[EBP + 56]
  rol EBX,7
  add EBX,EAX
  rol EDX,10

  mov ESI,EBX
  xor ESI,ECX
  and ESI,EDX
  xor ESI,ECX
  add EAX,ESI
  add EAX,$5C4DD124
  add EAX,[EBP + 60]
  rol EAX,7
  add EAX,EDI
  rol ECX,10

  mov ESI,EAX
  xor ESI,EBX
  and ESI,ECX
  xor ESI,EBX
  add EDI,ESI
  add EDI,$5C4DD124
  add EDI,[EBP + 32]
  rol EDI,12
  add EDI,EDX
  rol EBX,10

  mov ESI,EDI
  xor ESI,EAX
  and ESI,EBX
  xor ESI,EAX
  add EDX,ESI
  add EDX,$5C4DD124
  add EDX,[EBP + 48]
  rol EDX,7
  add EDX,ECX
  rol EAX,10

  mov ESI,EDX
  xor ESI,EDI
  and ESI,EAX
  xor ESI,EDI
  add ECX,ESI
  add ECX,$5C4DD124
  add ECX,[EBP + 16]
  rol ECX,6
  add ECX,EBX
  rol EDI,10

  mov ESI,ECX
  xor ESI,EDX
  and ESI,EDI
  xor ESI,EDX
  add EBX,ESI
  add EBX,$5C4DD124
  add EBX,[EBP + 36]
  rol EBX,15
  add EBX,EAX
  rol EDX,10

  mov ESI,EBX
  xor ESI,ECX
  and ESI,EDX
  xor ESI,ECX
  add EAX,ESI
  add EAX,$5C4DD124
  add EAX,[EBP + 4]
  rol EAX,13
  add EAX,EDI
  rol ECX,10

  mov ESI,EAX
  xor ESI,EBX
  and ESI,ECX
  xor ESI,EBX
  add EDI,ESI
  add EDI,$5C4DD124
  add EDI,[EBP + 8]
  rol EDI,11
  add EDI,EDX
  rol EBX,10


