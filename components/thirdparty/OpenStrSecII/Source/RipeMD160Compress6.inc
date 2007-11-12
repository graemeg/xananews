{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}
  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EBX
  add EAX,ESI
  add EAX,$50A28BE6
  add EAX,[EBP + 20]
  rol EAX,8
  add EAX,EDI
  rol ECX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EAX
  add EDI,ESI
  add EDI,$50A28BE6
  add EDI,[EBP + 56]
  rol EDI,9
  add EDI,EDX
  rol EBX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,EDI
  add EDX,ESI
  add EDX,$50A28BE6
  add EDX,[EBP + 28]
  rol EDX,9
  add EDX,ECX
  rol EAX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EDX
  add ECX,ESI
  add ECX,$50A28BE6
  add ECX,[EBP]
  rol ECX,11
  add ECX,EBX
  rol EDI,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,ECX
  add EBX,ESI
  add EBX,$50A28BE6
  add EBX,[EBP + 36]
  rol EBX,13
  add EBX,EAX
  rol EDX,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EBX
  add EAX,ESI
  add EAX,$50A28BE6
  add EAX,[EBP + 8]
  rol EAX,15
  add EAX,EDI
  rol ECX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EAX
  add EDI,ESI
  add EDI,$50A28BE6
  add EDI,[EBP + 44]
  rol EDI,15
  add EDI,EDX
  rol EBX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,EDI
  add EDX,ESI
  add EDX,$50A28BE6
  add EDX,[EBP + 16]
  rol EDX,5
  add EDX,ECX
  rol EAX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EDX
  add ECX,ESI
  add ECX,$50A28BE6
  add ECX,[EBP + 52]
  rol ECX,7
  add ECX,EBX
  rol EDI,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,ECX
  add EBX,ESI
  add EBX,$50A28BE6
  add EBX,[EBP + 24]
  rol EBX,7
  add EBX,EAX
  rol EDX,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EBX
  add EAX,ESI
  add EAX,$50A28BE6
  add EAX,[EBP + 60]
  rol EAX,8
  add EAX,EDI
  rol ECX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EAX
  add EDI,ESI
  add EDI,$50A28BE6
  add EDI,[EBP + 32]
  rol EDI,11
  add EDI,EDX
  rol EBX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,EDI
  add EDX,ESI
  add EDX,$50A28BE6
  add EDX,[EBP + 4]
  rol EDX,14
  add EDX,ECX
  rol EAX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EDX
  add ECX,ESI
  add ECX,$50A28BE6
  add ECX,[EBP + 40]
  rol ECX,14
  add ECX,EBX
  rol EDI,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,ECX
  add EBX,ESI
  add EBX,$50A28BE6
  add EBX,[EBP + 12]
  rol EBX,12
  add EBX,EAX
  rol EDX,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EBX
  add EAX,ESI
  add EAX,$50A28BE6
  add EAX,[EBP + 48]
  rol EAX,6
  add EAX,EDI
  rol ECX,10


