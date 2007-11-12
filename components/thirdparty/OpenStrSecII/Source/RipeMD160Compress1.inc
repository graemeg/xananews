{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}
  add EAX,[EBP]
  mov ESI,EBX
  xor ESI,ECX
  xor ESI,EDX
  add EAX,ESI
  rol EAX,11
  add EAX,EDI
  rol ECX,10
             
  add EDI,[EBP + 4]
  mov ESI,EAX
  xor ESI,EBX
  xor ESI,ECX
  add EDI,ESI
  rol EDI,14
  add EDI,EDX
  rol EBX,10
             
  add EDX,[EBP + 8]
  mov ESI,EDI
  xor ESI,EAX
  xor ESI,EBX
  add EDX,ESI
  rol EDX,15
  add EDX,ECX
  rol EAX,10
             
  add ECX,[EBP + 12]
  mov ESI,EDX
  xor ESI,EDI
  xor ESI,EAX
  add ECX,ESI
  rol ECX,12
  add ECX,EBX
  rol EDI,10
             
  add EBX,[EBP + 16]
  mov ESI,ECX
  xor ESI,EDX
  xor ESI,EDI
  add EBX,ESI
  rol EBX,5
  add EBX,EAX
  rol EDX,10
             
  add EAX,[EBP + 20]
  mov ESI,EBX
  xor ESI,ECX
  xor ESI,EDX
  add EAX,ESI
  rol EAX,8
  add EAX,EDI
  rol ECX,10
             
  add EDI,[EBP + 24]
  mov ESI,EAX
  xor ESI,EBX
  xor ESI,ECX
  add EDI,ESI
  rol EDI,7
  add EDI,EDX
  rol EBX,10
             
  add EDX,[EBP + 28]
  mov ESI,EDI
  xor ESI,EAX
  xor ESI,EBX
  add EDX,ESI
  rol EDX,9
  add EDX,ECX
  rol EAX,10
             
  add ECX,[EBP + 32]
  mov ESI,EDX
  xor ESI,EDI
  xor ESI,EAX
  add ECX,ESI
  rol ECX,11
  add ECX,EBX
  rol EDI,10
             
  add EBX,[EBP + 36]
  mov ESI,ECX
  xor ESI,EDX
  xor ESI,EDI
  add EBX,ESI
  rol EBX,13
  add EBX,EAX
  rol EDX,10
             
  add EAX,[EBP + 40]
  mov ESI,EBX
  xor ESI,ECX
  xor ESI,EDX
  add EAX,ESI
  rol EAX,14
  add EAX,EDI
  rol ECX,10
             
  add EDI,[EBP + 44]
  mov ESI,EAX
  xor ESI,EBX
  xor ESI,ECX
  add EDI,ESI
  rol EDI,15
  add EDI,EDX
  rol EBX,10
             
  add EDX,[EBP + 48]
  mov ESI,EDI
  xor ESI,EAX
  xor ESI,EBX
  add EDX,ESI
  rol EDX,6
  add EDX,ECX
  rol EAX,10
             
  add ECX,[EBP + 52]
  mov ESI,EDX
  xor ESI,EDI
  xor ESI,EAX
  add ECX,ESI
  rol ECX,7
  add ECX,EBX
  rol EDI,10
             
  add EBX,[EBP + 56]
  mov ESI,ECX
  xor ESI,EDX
  xor ESI,EDI
  add EBX,ESI
  rol EBX,9
  add EBX,EAX
  rol EDX,10
             
  add EAX,[EBP + 60]
  mov ESI,EBX
  xor ESI,ECX
  xor ESI,EDX
  add EAX,ESI
  rol EAX,8
  add EAX,EDI
  rol ECX,10


