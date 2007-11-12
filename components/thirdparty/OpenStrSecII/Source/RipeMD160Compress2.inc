{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}
  add EDI,[EBP + 28]
  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  xor ESI,ECX
  lea EDI,[EDI + ESI + $5A827999]
  rol EDI,7
  add EDI,EDX
  rol EBX,10
                   
  add EDX,[EBP + 16]
  mov ESI,EBX
  xor ESI,EAX
  and ESI,EDI
  xor ESI,EBX
  lea EDX,[EDX + ESI + $5A827999]
  rol EDX,6
  add EDX,ECX
  rol EAX,10

  add ECX,[EBP + 52]
  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  xor ESI,EAX
  lea ECX,[ECX + ESI + $5A827999]
  rol ECX,8
  add ECX,EBX
  rol EDI,10

  add EBX,[EBP + 4]
  mov ESI,EDI
  xor ESI,EDX
  and ESI,ECX
  xor ESI,EDI
  lea EBX,[EBX + ESI + $5A827999]
  rol EBX,13
  add EBX,EAX
  rol EDX,10

  add EAX,[EBP + 40]
  mov ESI,EDX
  xor ESI,ECX
  and ESI,EBX
  xor ESI,EDX
  lea EAX,[EAX + ESI + $5A827999]
  rol EAX,11
  add EAX,EDI
  rol ECX,10

  add EDI,[EBP + 24]
  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  xor ESI,ECX
  lea EDI,[EDI + ESI + $5A827999]
  rol EDI,9
  add EDI,EDX
  rol EBX,10

  add EDX,[EBP + 60]
  mov ESI,EBX
  xor ESI,EAX
  and ESI,EDI
  xor ESI,EBX
  lea EDX,[EDX + ESI + $5A827999]
  rol EDX,7
  add EDX,ECX
  rol EAX,10

  add ECX,[EBP + 12]
  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  xor ESI,EAX
  lea ECX,[ECX + ESI + $5A827999]
  rol ECX,15
  add ECX,EBX
  rol EDI,10

  add EBX,[EBP + 48]
  mov ESI,EDI
  xor ESI,EDX
  and ESI,ECX
  xor ESI,EDI
  lea EBX,[EBX + ESI + $5A827999]
  rol EBX,7
  add EBX,EAX
  rol EDX,10

  add EAX,[EBP]
  mov ESI,EDX
  xor ESI,ECX
  and ESI,EBX
  xor ESI,EDX
  lea EAX,[EAX + ESI + $5A827999]
  rol EAX,12
  add EAX,EDI
  rol ECX,10

  add EDI,[EBP + 36]
  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  xor ESI,ECX
  lea EDI,[EDI + ESI + $5A827999]
  rol EDI,15
  add EDI,EDX
  rol EBX,10

  add EDX,[EBP + 20]
  mov ESI,EBX
  xor ESI,EAX
  and ESI,EDI
  xor ESI,EBX
  lea EDX,[EDX + ESI + $5A827999]
  rol EDX,9
  add EDX,ECX
  rol EAX,10

  add ECX,[EBP + 8]
  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  xor ESI,EAX
  lea ECX,[ECX + ESI + $5A827999]
  rol ECX,11
  add ECX,EBX
  rol EDI,10

  add EBX,[EBP + 56]
  mov ESI,EDI
  xor ESI,EDX
  and ESI,ECX
  xor ESI,EDI
  lea EBX,[EBX + ESI + $5A827999]
  rol EBX,7
  add EBX,EAX
  rol EDX,10

  add EAX,[EBP + 44]
  mov ESI,EDX
  xor ESI,ECX
  and ESI,EBX
  xor ESI,EDX
  lea EAX,[EAX + ESI + $5A827999]
  rol EAX,13
  add EAX,EDI
  rol ECX,10

  add EDI,[EBP + 32]
  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  xor ESI,ECX
  lea EDI,[EDI + ESI + $5A827999]
  rol EDI,12
  add EDI,EDX
  rol EBX,10

