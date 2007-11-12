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
  add EDX,$6D703EF3
  add EDX,[EBP + 60]
  rol EDX,9
  add EDX,ECX
  rol EAX,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,EAX
  add ECX,ESI
  add ECX,$6D703EF3
  add ECX,[EBP + 20]
  rol ECX,7
  add ECX,EBX
  rol EDI,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EDI
  add EBX,ESI
  add EBX,$6D703EF3
  add EBX,[EBP + 4]
  rol EBX,15
  add EBX,EAX
  rol EDX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EDX
  add EAX,ESI
  add EAX,$6D703EF3
  add EAX,[EBP + 12]
  rol EAX,11
  add EAX,EDI
  rol ECX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,ECX
  add EDI,ESI
  add EDI,$6D703EF3
  add EDI,[EBP + 28]
  rol EDI,8
  add EDI,EDX
  rol EBX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EBX
  add EDX,ESI
  add EDX,$6D703EF3
  add EDX,[EBP + 56]
  rol EDX,6
  add EDX,ECX
  rol EAX,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,EAX
  add ECX,ESI
  add ECX,$6D703EF3
  add ECX,[EBP + 24]
  rol ECX,6
  add ECX,EBX
  rol EDI,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EDI
  add EBX,ESI
  add EBX,$6D703EF3
  add EBX,[EBP + 36]
  rol EBX,14
  add EBX,EAX
  rol EDX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EDX
  add EAX,ESI
  add EAX,$6D703EF3
  add EAX,[EBP + 44]
  rol EAX,12
  add EAX,EDI
  rol ECX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,ECX
  add EDI,ESI
  add EDI,$6D703EF3
  add EDI,[EBP + 32]
  rol EDI,13
  add EDI,EDX
  rol EBX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EBX
  add EDX,ESI
  add EDX,$6D703EF3
  add EDX,[EBP + 48]
  rol EDX,5
  add EDX,ECX
  rol EAX,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,EAX
  add ECX,ESI
  add ECX,$6D703EF3
  add ECX,[EBP + 8]
  rol ECX,14
  add ECX,EBX
  rol EDI,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EDI
  add EBX,ESI
  add EBX,$6D703EF3
  add EBX,[EBP + 40]
  rol EBX,13
  add EBX,EAX
  rol EDX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EDX
  add EAX,ESI
  add EAX,$6D703EF3
  add EAX,[EBP]
  rol EAX,13
  add EAX,EDI
  rol ECX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,ECX
  add EDI,ESI
  add EDI,$6D703EF3
  add EDI,[EBP + 16]
  rol EDI,7
  add EDI,EDX
  rol EBX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EBX
  add EDX,ESI
  add EDX,$6D703EF3
  add EDX,[EBP + 52]
  rol EDX,5
  add EDX,ECX
  rol EAX,10


