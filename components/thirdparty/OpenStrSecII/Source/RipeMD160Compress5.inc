{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}
  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,ECX
  add EBX,ESI
  add EBX,$A953FD4E
  add EBX,[EBP + 16]
  rol EBX,9
  add EBX,EAX
  rol EDX,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EBX
  add EAX,ESI
  add EAX,$A953FD4E
  add EAX,[EBP]
  rol EAX,15
  add EAX,EDI
  rol ECX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EAX
  add EDI,ESI
  add EDI,$A953FD4E
  add EDI,[EBP + 20]
  rol EDI,5
  add EDI,EDX
  rol EBX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,EDI
  add EDX,ESI
  add EDX,$A953FD4E
  add EDX,[EBP + 36]
  rol EDX,11
  add EDX,ECX
  rol EAX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EDX
  add ECX,ESI
  add ECX,$A953FD4E
  add ECX,[EBP + 28]
  rol ECX,6
  add ECX,EBX
  rol EDI,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,ECX
  add EBX,ESI
  add EBX,$A953FD4E
  add EBX,[EBP + 48]
  rol EBX,8
  add EBX,EAX
  rol EDX,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EBX
  add EAX,ESI
  add EAX,$A953FD4E
  add EAX,[EBP + 8]
  rol EAX,13
  add EAX,EDI
  rol ECX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EAX
  add EDI,ESI
  add EDI,$A953FD4E
  add EDI,[EBP + 40]
  rol EDI,12
  add EDI,EDX
  rol EBX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,EDI
  add EDX,ESI
  add EDX,$A953FD4E
  add EDX,[EBP + 56]
  rol EDX,5
  add EDX,ECX
  rol EAX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EDX
  add ECX,ESI
  add ECX,$A953FD4E
  add ECX,[EBP + 4]
  rol ECX,12
  add ECX,EBX
  rol EDI,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,ECX
  add EBX,ESI
  add EBX,$A953FD4E
  add EBX,[EBP + 12]
  rol EBX,13
  add EBX,EAX
  rol EDX,10

  mov ESI,EDX
  not ESI
  or  ESI,ECX
  xor ESI,EBX
  add EAX,ESI
  add EAX,$A953FD4E
  add EAX,[EBP + 32]
  rol EAX,14
  add EAX,EDI
  rol ECX,10

  mov ESI,ECX
  not ESI
  or  ESI,EBX
  xor ESI,EAX
  add EDI,ESI
  add EDI,$A953FD4E
  add EDI,[EBP + 44]
  rol EDI,11
  add EDI,EDX
  rol EBX,10

  mov ESI,EBX
  not ESI
  or  ESI,EAX
  xor ESI,EDI
  add EDX,ESI
  add EDX,$A953FD4E
  add EDX,[EBP + 24]
  rol EDX,8
  add EDX,ECX
  rol EAX,10

  mov ESI,EAX
  not ESI
  or  ESI,EDI
  xor ESI,EDX
  add ECX,ESI
  add ECX,$A953FD4E
  add ECX,[EBP + 60]
  rol ECX,5
  add ECX,EBX
  rol EDI,10

  mov ESI,EDI
  not ESI
  or  ESI,EDX
  xor ESI,ECX
  add EBX,ESI
  add EBX,$A953FD4E
  add EBX,[EBP + 52]
  rol EBX,6
  add EBX,EAX
  rol EDX,10

