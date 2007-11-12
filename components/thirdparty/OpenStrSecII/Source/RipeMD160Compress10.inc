{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}
  mov ESI,ECX
  xor ESI,EDX
  xor ESI,EDI
  add EBX,ESI
  add EBX,[EBP + 48]
  rol EBX,8
  add EBX,EAX
  rol EDX,10

  mov ESI,EBX
  xor ESI,ECX
  xor ESI,EDX
  add EAX,ESI
  add EAX,[EBP + 60]
  rol EAX,5
  add EAX,EDI
  rol ECX,10

  mov ESI,EAX
  xor ESI,EBX
  xor ESI,ECX
  add EDI,ESI
  add EDI,[EBP + 40]
  rol EDI,12
  add EDI,EDX
  rol EBX,10

  mov ESI,EDI
  xor ESI,EAX
  xor ESI,EBX
  add EDX,ESI
  add EDX,[EBP + 16]
  rol EDX,9
  add EDX,ECX
  rol EAX,10

  mov ESI,EDX
  xor ESI,EDI
  xor ESI,EAX
  add ECX,ESI
  add ECX,[EBP + 4]
  rol ECX,12
  add ECX,EBX
  rol EDI,10

  mov ESI,ECX
  xor ESI,EDX
  xor ESI,EDI
  add EBX,ESI
  add EBX,[EBP + 20]
  rol EBX,5
  add EBX,EAX
  rol EDX,10

  mov ESI,EBX
  xor ESI,ECX
  xor ESI,EDX
  add EAX,ESI
  add EAX,[EBP + 32]
  rol EAX,14
  add EAX,EDI
  rol ECX,10

  mov ESI,EAX
  xor ESI,EBX
  xor ESI,ECX
  add EDI,ESI
  add EDI,[EBP + 28]
  rol EDI,6
  add EDI,EDX
  rol EBX,10

  mov ESI,EDI
  xor ESI,EAX
  xor ESI,EBX
  add EDX,ESI
  add EDX,[EBP + 24]
  rol EDX,8
  add EDX,ECX
  rol EAX,10

  mov ESI,EDX
  xor ESI,EDI
  xor ESI,EAX
  add ECX,ESI
  add ECX,[EBP + 8]
  rol ECX,13
  add ECX,EBX
  rol EDI,10

  mov ESI,ECX
  xor ESI,EDX
  xor ESI,EDI
  add EBX,ESI
  add EBX,[EBP + 52]
  rol EBX,6
  add EBX,EAX
  rol EDX,10

  mov ESI,EBX
  xor ESI,ECX
  xor ESI,EDX
  add EAX,ESI
  add EAX,[EBP + 56]
  rol EAX,5
  add EAX,EDI
  rol ECX,10

  mov ESI,EAX
  xor ESI,EBX
  xor ESI,ECX
  add EDI,ESI
  add EDI,[EBP]
  rol EDI,15
  add EDI,EDX
  rol EBX,10

  mov ESI,EDI
  xor ESI,EAX
  xor ESI,EBX
  add EDX,ESI
  add EDX,[EBP + 12]
  rol EDX,13
  add EDX,ECX
  rol EAX,10

  mov ESI,EDX
  xor ESI,EDI
  xor ESI,EAX
  add ECX,ESI
  add ECX,[EBP + 36]
  rol ECX,11
  add ECX,EBX
  rol EDI,10

  mov ESI,ECX
  xor ESI,EDX
  xor ESI,EDI
  add EBX,ESI
  add EBX,[EBP + 44]
  rol EBX,11
  add EBX,EAX
  rol EDX,10


