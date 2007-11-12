{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 696]
  adc EDX,[ESP + 700]
  mov EBX,[ESP + 688]
  xor EBX,[ESP + 680]
  and EBX,ESI
  xor EBX,[ESP + 688]
  mov ECX,[ESP + 692]
  xor ECX,[ESP + 684]
  and ECX,EDI
  xor ECX,[ESP + 692]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 0]
  adc EDX,[ESP + 4]
  add EAX,$D728AE22
  adc EDX,$428A2F98

  add [ESP + 664],EAX
  adc [ESP + 668],EDX
  mov [ESP + 696],EAX
  mov [ESP + 700],EDX

  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 656]
  mov EBX,EBP
  xor EBX,[ESP + 648]
  and EBX,ESI
  and EBP,[ESP + 648]
  xor EBX,EBP
  mov EBP,[ESP + 660]
  mov ECX,EBP
  xor ECX,[ESP + 652]
  and ECX,EDI
  and EBP,[ESP + 652]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 696],EAX
  adc [ESP + 700],EDX


  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 688]
  adc EDX,[ESP + 692]
  mov EBX,[ESP + 680]
  xor EBX,[ESP + 672]
  and EBX,ESI
  xor EBX,[ESP + 680]
  mov ECX,[ESP + 684]
  xor ECX,[ESP + 676]
  and ECX,EDI
  xor ECX,[ESP + 684]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 8]
  adc EDX,[ESP + 12]
  add EAX,$23EF65CD
  adc EDX,$71374491

  add [ESP + 656],EAX
  adc [ESP + 660],EDX
  mov [ESP + 688],EAX
  mov [ESP + 692],EDX

  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 648]
  mov EBX,EBP
  xor EBX,[ESP + 640]
  and EBX,ESI
  and EBP,[ESP + 640]
  xor EBX,EBP
  mov EBP,[ESP + 652]
  mov ECX,EBP
  xor ECX,[ESP + 644]
  and ECX,EDI
  and EBP,[ESP + 644]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 688],EAX
  adc [ESP + 692],EDX


  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 680]
  adc EDX,[ESP + 684]
  mov EBX,[ESP + 672]
  xor EBX,[ESP + 664]
  and EBX,ESI
  xor EBX,[ESP + 672]
  mov ECX,[ESP + 676]
  xor ECX,[ESP + 668]
  and ECX,EDI
  xor ECX,[ESP + 676]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 16]
  adc EDX,[ESP + 20]
  add EAX,$EC4D3B2F
  adc EDX,$B5C0FBCF

  add [ESP + 648],EAX
  adc [ESP + 652],EDX
  mov [ESP + 680],EAX
  mov [ESP + 684],EDX

  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 640]
  mov EBX,EBP
  xor EBX,[ESP + 696]
  and EBX,ESI
  and EBP,[ESP + 696]
  xor EBX,EBP
  mov EBP,[ESP + 644]
  mov ECX,EBP
  xor ECX,[ESP + 700]
  and ECX,EDI
  and EBP,[ESP + 700]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 680],EAX
  adc [ESP + 684],EDX


  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 672]
  adc EDX,[ESP + 676]
  mov EBX,[ESP + 664]
  xor EBX,[ESP + 656]
  and EBX,ESI
  xor EBX,[ESP + 664]
  mov ECX,[ESP + 668]
  xor ECX,[ESP + 660]
  and ECX,EDI
  xor ECX,[ESP + 668]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 24]
  adc EDX,[ESP + 28]
  add EAX,$8189DBBC
  adc EDX,$E9B5DBA5

  add [ESP + 640],EAX
  adc [ESP + 644],EDX
  mov [ESP + 672],EAX
  mov [ESP + 676],EDX

  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 696]
  mov EBX,EBP
  xor EBX,[ESP + 688]
  and EBX,ESI
  and EBP,[ESP + 688]
  xor EBX,EBP
  mov EBP,[ESP + 700]
  mov ECX,EBP
  xor ECX,[ESP + 692]
  and ECX,EDI
  and EBP,[ESP + 692]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 672],EAX
  adc [ESP + 676],EDX


  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 664]
  adc EDX,[ESP + 668]
  mov EBX,[ESP + 656]
  xor EBX,[ESP + 648]
  and EBX,ESI
  xor EBX,[ESP + 656]
  mov ECX,[ESP + 660]
  xor ECX,[ESP + 652]
  and ECX,EDI
  xor ECX,[ESP + 660]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 32]
  adc EDX,[ESP + 36]
  add EAX,$F348B538
  adc EDX,$3956C25B

  add [ESP + 696],EAX
  adc [ESP + 700],EDX
  mov [ESP + 664],EAX
  mov [ESP + 668],EDX

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 688]
  mov EBX,EBP
  xor EBX,[ESP + 680]
  and EBX,ESI
  and EBP,[ESP + 680]
  xor EBX,EBP
  mov EBP,[ESP + 692]
  mov ECX,EBP
  xor ECX,[ESP + 684]
  and ECX,EDI
  and EBP,[ESP + 684]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 664],EAX
  adc [ESP + 668],EDX


  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 656]
  adc EDX,[ESP + 660]
  mov EBX,[ESP + 648]
  xor EBX,[ESP + 640]
  and EBX,ESI
  xor EBX,[ESP + 648]
  mov ECX,[ESP + 652]
  xor ECX,[ESP + 644]
  and ECX,EDI
  xor ECX,[ESP + 652]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 40]
  adc EDX,[ESP + 44]
  add EAX,$B605D019
  adc EDX,$59F111F1

  add [ESP + 688],EAX
  adc [ESP + 692],EDX
  mov [ESP + 656],EAX
  mov [ESP + 660],EDX

  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 680]
  mov EBX,EBP
  xor EBX,[ESP + 672]
  and EBX,ESI
  and EBP,[ESP + 672]
  xor EBX,EBP
  mov EBP,[ESP + 684]
  mov ECX,EBP
  xor ECX,[ESP + 676]
  and ECX,EDI
  and EBP,[ESP + 676]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 656],EAX
  adc [ESP + 660],EDX


  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 648]
  adc EDX,[ESP + 652]
  mov EBX,[ESP + 640]
  xor EBX,[ESP + 696]
  and EBX,ESI
  xor EBX,[ESP + 640]
  mov ECX,[ESP + 644]
  xor ECX,[ESP + 700]
  and ECX,EDI
  xor ECX,[ESP + 644]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 48]
  adc EDX,[ESP + 52]
  add EAX,$AF194F9B
  adc EDX,$923F82A4

  add [ESP + 680],EAX
  adc [ESP + 684],EDX
  mov [ESP + 648],EAX
  mov [ESP + 652],EDX

  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 672]
  mov EBX,EBP
  xor EBX,[ESP + 664]
  and EBX,ESI
  and EBP,[ESP + 664]
  xor EBX,EBP
  mov EBP,[ESP + 676]
  mov ECX,EBP
  xor ECX,[ESP + 668]
  and ECX,EDI
  and EBP,[ESP + 668]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 648],EAX
  adc [ESP + 652],EDX


  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 640]
  adc EDX,[ESP + 644]
  mov EBX,[ESP + 696]
  xor EBX,[ESP + 688]
  and EBX,ESI
  xor EBX,[ESP + 696]
  mov ECX,[ESP + 700]
  xor ECX,[ESP + 692]
  and ECX,EDI
  xor ECX,[ESP + 700]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 56]
  adc EDX,[ESP + 60]
  add EAX,$DA6D8118
  adc EDX,$AB1C5ED5

  add [ESP + 672],EAX
  adc [ESP + 676],EDX
  mov [ESP + 640],EAX
  mov [ESP + 644],EDX

  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 664]
  mov EBX,EBP
  xor EBX,[ESP + 656]
  and EBX,ESI
  and EBP,[ESP + 656]
  xor EBX,EBP
  mov EBP,[ESP + 668]
  mov ECX,EBP
  xor ECX,[ESP + 660]
  and ECX,EDI
  and EBP,[ESP + 660]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 640],EAX
  adc [ESP + 644],EDX


  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 696]
  adc EDX,[ESP + 700]
  mov EBX,[ESP + 688]
  xor EBX,[ESP + 680]
  and EBX,ESI
  xor EBX,[ESP + 688]
  mov ECX,[ESP + 692]
  xor ECX,[ESP + 684]
  and ECX,EDI
  xor ECX,[ESP + 692]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 64]
  adc EDX,[ESP + 68]
  add EAX,$A3030242
  adc EDX,$D807AA98

  add [ESP + 664],EAX
  adc [ESP + 668],EDX
  mov [ESP + 696],EAX
  mov [ESP + 700],EDX

  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 656]
  mov EBX,EBP
  xor EBX,[ESP + 648]
  and EBX,ESI
  and EBP,[ESP + 648]
  xor EBX,EBP
  mov EBP,[ESP + 660]
  mov ECX,EBP
  xor ECX,[ESP + 652]
  and ECX,EDI
  and EBP,[ESP + 652]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 696],EAX
  adc [ESP + 700],EDX


  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 688]
  adc EDX,[ESP + 692]
  mov EBX,[ESP + 680]
  xor EBX,[ESP + 672]
  and EBX,ESI
  xor EBX,[ESP + 680]
  mov ECX,[ESP + 684]
  xor ECX,[ESP + 676]
  and ECX,EDI
  xor ECX,[ESP + 684]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 72]
  adc EDX,[ESP + 76]
  add EAX,$45706FBE
  adc EDX,$12835B01

  add [ESP + 656],EAX
  adc [ESP + 660],EDX
  mov [ESP + 688],EAX
  mov [ESP + 692],EDX

  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 648]
  mov EBX,EBP
  xor EBX,[ESP + 640]
  and EBX,ESI
  and EBP,[ESP + 640]
  xor EBX,EBP
  mov EBP,[ESP + 652]
  mov ECX,EBP
  xor ECX,[ESP + 644]
  and ECX,EDI
  and EBP,[ESP + 644]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 688],EAX
  adc [ESP + 692],EDX


  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 680]
  adc EDX,[ESP + 684]
  mov EBX,[ESP + 672]
  xor EBX,[ESP + 664]
  and EBX,ESI
  xor EBX,[ESP + 672]
  mov ECX,[ESP + 676]
  xor ECX,[ESP + 668]
  and ECX,EDI
  xor ECX,[ESP + 676]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 80]
  adc EDX,[ESP + 84]
  add EAX,$4EE4B28C
  adc EDX,$243185BE

  add [ESP + 648],EAX
  adc [ESP + 652],EDX
  mov [ESP + 680],EAX
  mov [ESP + 684],EDX

  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 640]
  mov EBX,EBP
  xor EBX,[ESP + 696]
  and EBX,ESI
  and EBP,[ESP + 696]
  xor EBX,EBP
  mov EBP,[ESP + 644]
  mov ECX,EBP
  xor ECX,[ESP + 700]
  and ECX,EDI
  and EBP,[ESP + 700]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 680],EAX
  adc [ESP + 684],EDX


  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 672]
  adc EDX,[ESP + 676]
  mov EBX,[ESP + 664]
  xor EBX,[ESP + 656]
  and EBX,ESI
  xor EBX,[ESP + 664]
  mov ECX,[ESP + 668]
  xor ECX,[ESP + 660]
  and ECX,EDI
  xor ECX,[ESP + 668]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 88]
  adc EDX,[ESP + 92]
  add EAX,$D5FFB4E2
  adc EDX,$550C7DC3

  add [ESP + 640],EAX
  adc [ESP + 644],EDX
  mov [ESP + 672],EAX
  mov [ESP + 676],EDX

  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 696]
  mov EBX,EBP
  xor EBX,[ESP + 688]
  and EBX,ESI
  and EBP,[ESP + 688]
  xor EBX,EBP
  mov EBP,[ESP + 700]
  mov ECX,EBP
  xor ECX,[ESP + 692]
  and ECX,EDI
  and EBP,[ESP + 692]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 672],EAX
  adc [ESP + 676],EDX


  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 664]
  adc EDX,[ESP + 668]
  mov EBX,[ESP + 656]
  xor EBX,[ESP + 648]
  and EBX,ESI
  xor EBX,[ESP + 656]
  mov ECX,[ESP + 660]
  xor ECX,[ESP + 652]
  and ECX,EDI
  xor ECX,[ESP + 660]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 96]
  adc EDX,[ESP + 100]
  add EAX,$F27B896F
  adc EDX,$72BE5D74

  add [ESP + 696],EAX
  adc [ESP + 700],EDX
  mov [ESP + 664],EAX
  mov [ESP + 668],EDX

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 688]
  mov EBX,EBP
  xor EBX,[ESP + 680]
  and EBX,ESI
  and EBP,[ESP + 680]
  xor EBX,EBP
  mov EBP,[ESP + 692]
  mov ECX,EBP
  xor ECX,[ESP + 684]
  and ECX,EDI
  and EBP,[ESP + 684]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 664],EAX
  adc [ESP + 668],EDX


  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 656]
  adc EDX,[ESP + 660]
  mov EBX,[ESP + 648]
  xor EBX,[ESP + 640]
  and EBX,ESI
  xor EBX,[ESP + 648]
  mov ECX,[ESP + 652]
  xor ECX,[ESP + 644]
  and ECX,EDI
  xor ECX,[ESP + 652]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 104]
  adc EDX,[ESP + 108]
  add EAX,$3B1696B1
  adc EDX,$80DEB1FE

  add [ESP + 688],EAX
  adc [ESP + 692],EDX
  mov [ESP + 656],EAX
  mov [ESP + 660],EDX

  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 680]
  mov EBX,EBP
  xor EBX,[ESP + 672]
  and EBX,ESI
  and EBP,[ESP + 672]
  xor EBX,EBP
  mov EBP,[ESP + 684]
  mov ECX,EBP
  xor ECX,[ESP + 676]
  and ECX,EDI
  and EBP,[ESP + 676]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 656],EAX
  adc [ESP + 660],EDX


  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 648]
  adc EDX,[ESP + 652]
  mov EBX,[ESP + 640]
  xor EBX,[ESP + 696]
  and EBX,ESI
  xor EBX,[ESP + 640]
  mov ECX,[ESP + 644]
  xor ECX,[ESP + 700]
  and ECX,EDI
  xor ECX,[ESP + 644]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 112]
  adc EDX,[ESP + 116]
  add EAX,$25C71235
  adc EDX,$9BDC06A7

  add [ESP + 680],EAX
  adc [ESP + 684],EDX
  mov [ESP + 648],EAX
  mov [ESP + 652],EDX

  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 672]
  mov EBX,EBP
  xor EBX,[ESP + 664]
  and EBX,ESI
  and EBP,[ESP + 664]
  xor EBX,EBP
  mov EBP,[ESP + 676]
  mov ECX,EBP
  xor ECX,[ESP + 668]
  and ECX,EDI
  and EBP,[ESP + 668]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 648],EAX
  adc [ESP + 652],EDX


  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 640]
  adc EDX,[ESP + 644]
  mov EBX,[ESP + 696]
  xor EBX,[ESP + 688]
  and EBX,ESI
  xor EBX,[ESP + 696]
  mov ECX,[ESP + 700]
  xor ECX,[ESP + 692]
  and ECX,EDI
  xor ECX,[ESP + 700]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 120]
  adc EDX,[ESP + 124]
  add EAX,$CF692694
  adc EDX,$C19BF174

  add [ESP + 672],EAX
  adc [ESP + 676],EDX
  mov [ESP + 640],EAX
  mov [ESP + 644],EDX

  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 664]
  mov EBX,EBP
  xor EBX,[ESP + 656]
  and EBX,ESI
  and EBP,[ESP + 656]
  xor EBX,EBP
  mov EBP,[ESP + 668]
  mov ECX,EBP
  xor ECX,[ESP + 660]
  and ECX,EDI
  and EBP,[ESP + 660]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 640],EAX
  adc [ESP + 644],EDX


  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 696]
  adc EDX,[ESP + 700]
  mov EBX,[ESP + 688]
  xor EBX,[ESP + 680]
  and EBX,ESI
  xor EBX,[ESP + 688]
  mov ECX,[ESP + 692]
  xor ECX,[ESP + 684]
  and ECX,EDI
  xor ECX,[ESP + 692]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 128]
  adc EDX,[ESP + 132]
  add EAX,$9EF14AD2
  adc EDX,$E49B69C1

  add [ESP + 664],EAX
  adc [ESP + 668],EDX
  mov [ESP + 696],EAX
  mov [ESP + 700],EDX

  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 656]
  mov EBX,EBP
  xor EBX,[ESP + 648]
  and EBX,ESI
  and EBP,[ESP + 648]
  xor EBX,EBP
  mov EBP,[ESP + 660]
  mov ECX,EBP
  xor ECX,[ESP + 652]
  and ECX,EDI
  and EBP,[ESP + 652]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 696],EAX
  adc [ESP + 700],EDX


  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 688]
  adc EDX,[ESP + 692]
  mov EBX,[ESP + 680]
  xor EBX,[ESP + 672]
  and EBX,ESI
  xor EBX,[ESP + 680]
  mov ECX,[ESP + 684]
  xor ECX,[ESP + 676]
  and ECX,EDI
  xor ECX,[ESP + 684]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 136]
  adc EDX,[ESP + 140]
  add EAX,$384F25E3
  adc EDX,$EFBE4786

  add [ESP + 656],EAX
  adc [ESP + 660],EDX
  mov [ESP + 688],EAX
  mov [ESP + 692],EDX

  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 648]
  mov EBX,EBP
  xor EBX,[ESP + 640]
  and EBX,ESI
  and EBP,[ESP + 640]
  xor EBX,EBP
  mov EBP,[ESP + 652]
  mov ECX,EBP
  xor ECX,[ESP + 644]
  and ECX,EDI
  and EBP,[ESP + 644]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 688],EAX
  adc [ESP + 692],EDX


  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 680]
  adc EDX,[ESP + 684]
  mov EBX,[ESP + 672]
  xor EBX,[ESP + 664]
  and EBX,ESI
  xor EBX,[ESP + 672]
  mov ECX,[ESP + 676]
  xor ECX,[ESP + 668]
  and ECX,EDI
  xor ECX,[ESP + 676]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 144]
  adc EDX,[ESP + 148]
  add EAX,$8B8CD5B5
  adc EDX,$0FC19DC6

  add [ESP + 648],EAX
  adc [ESP + 652],EDX
  mov [ESP + 680],EAX
  mov [ESP + 684],EDX

  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 640]
  mov EBX,EBP
  xor EBX,[ESP + 696]
  and EBX,ESI
  and EBP,[ESP + 696]
  xor EBX,EBP
  mov EBP,[ESP + 644]
  mov ECX,EBP
  xor ECX,[ESP + 700]
  and ECX,EDI
  and EBP,[ESP + 700]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 680],EAX
  adc [ESP + 684],EDX


  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 672]
  adc EDX,[ESP + 676]
  mov EBX,[ESP + 664]
  xor EBX,[ESP + 656]
  and EBX,ESI
  xor EBX,[ESP + 664]
  mov ECX,[ESP + 668]
  xor ECX,[ESP + 660]
  and ECX,EDI
  xor ECX,[ESP + 668]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 152]
  adc EDX,[ESP + 156]
  add EAX,$77AC9C65
  adc EDX,$240CA1CC

  add [ESP + 640],EAX
  adc [ESP + 644],EDX
  mov [ESP + 672],EAX
  mov [ESP + 676],EDX

  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 696]
  mov EBX,EBP
  xor EBX,[ESP + 688]
  and EBX,ESI
  and EBP,[ESP + 688]
  xor EBX,EBP
  mov EBP,[ESP + 700]
  mov ECX,EBP
  xor ECX,[ESP + 692]
  and ECX,EDI
  and EBP,[ESP + 692]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 672],EAX
  adc [ESP + 676],EDX


  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 664]
  adc EDX,[ESP + 668]
  mov EBX,[ESP + 656]
  xor EBX,[ESP + 648]
  and EBX,ESI
  xor EBX,[ESP + 656]
  mov ECX,[ESP + 660]
  xor ECX,[ESP + 652]
  and ECX,EDI
  xor ECX,[ESP + 660]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 160]
  adc EDX,[ESP + 164]
  add EAX,$592B0275
  adc EDX,$2DE92C6F

  add [ESP + 696],EAX
  adc [ESP + 700],EDX
  mov [ESP + 664],EAX
  mov [ESP + 668],EDX

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 688]
  mov EBX,EBP
  xor EBX,[ESP + 680]
  and EBX,ESI
  and EBP,[ESP + 680]
  xor EBX,EBP
  mov EBP,[ESP + 692]
  mov ECX,EBP
  xor ECX,[ESP + 684]
  and ECX,EDI
  and EBP,[ESP + 684]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 664],EAX
  adc [ESP + 668],EDX


  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 656]
  adc EDX,[ESP + 660]
  mov EBX,[ESP + 648]
  xor EBX,[ESP + 640]
  and EBX,ESI
  xor EBX,[ESP + 648]
  mov ECX,[ESP + 652]
  xor ECX,[ESP + 644]
  and ECX,EDI
  xor ECX,[ESP + 652]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 168]
  adc EDX,[ESP + 172]
  add EAX,$6EA6E483
  adc EDX,$4A7484AA

  add [ESP + 688],EAX
  adc [ESP + 692],EDX
  mov [ESP + 656],EAX
  mov [ESP + 660],EDX

  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 680]
  mov EBX,EBP
  xor EBX,[ESP + 672]
  and EBX,ESI
  and EBP,[ESP + 672]
  xor EBX,EBP
  mov EBP,[ESP + 684]
  mov ECX,EBP
  xor ECX,[ESP + 676]
  and ECX,EDI
  and EBP,[ESP + 676]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 656],EAX
  adc [ESP + 660],EDX


  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 648]
  adc EDX,[ESP + 652]
  mov EBX,[ESP + 640]
  xor EBX,[ESP + 696]
  and EBX,ESI
  xor EBX,[ESP + 640]
  mov ECX,[ESP + 644]
  xor ECX,[ESP + 700]
  and ECX,EDI
  xor ECX,[ESP + 644]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 176]
  adc EDX,[ESP + 180]
  add EAX,$BD41FBD4
  adc EDX,$5CB0A9DC

  add [ESP + 680],EAX
  adc [ESP + 684],EDX
  mov [ESP + 648],EAX
  mov [ESP + 652],EDX

  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 672]
  mov EBX,EBP
  xor EBX,[ESP + 664]
  and EBX,ESI
  and EBP,[ESP + 664]
  xor EBX,EBP
  mov EBP,[ESP + 676]
  mov ECX,EBP
  xor ECX,[ESP + 668]
  and ECX,EDI
  and EBP,[ESP + 668]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 648],EAX
  adc [ESP + 652],EDX


  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 640]
  adc EDX,[ESP + 644]
  mov EBX,[ESP + 696]
  xor EBX,[ESP + 688]
  and EBX,ESI
  xor EBX,[ESP + 696]
  mov ECX,[ESP + 700]
  xor ECX,[ESP + 692]
  and ECX,EDI
  xor ECX,[ESP + 700]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 184]
  adc EDX,[ESP + 188]
  add EAX,$831153B5
  adc EDX,$76F988DA

  add [ESP + 672],EAX
  adc [ESP + 676],EDX
  mov [ESP + 640],EAX
  mov [ESP + 644],EDX

  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 664]
  mov EBX,EBP
  xor EBX,[ESP + 656]
  and EBX,ESI
  and EBP,[ESP + 656]
  xor EBX,EBP
  mov EBP,[ESP + 668]
  mov ECX,EBP
  xor ECX,[ESP + 660]
  and ECX,EDI
  and EBP,[ESP + 660]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 640],EAX
  adc [ESP + 644],EDX


  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 696]
  adc EDX,[ESP + 700]
  mov EBX,[ESP + 688]
  xor EBX,[ESP + 680]
  and EBX,ESI
  xor EBX,[ESP + 688]
  mov ECX,[ESP + 692]
  xor ECX,[ESP + 684]
  and ECX,EDI
  xor ECX,[ESP + 692]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 192]
  adc EDX,[ESP + 196]
  add EAX,$EE66DFAB
  adc EDX,$983E5152

  add [ESP + 664],EAX
  adc [ESP + 668],EDX
  mov [ESP + 696],EAX
  mov [ESP + 700],EDX

  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 656]
  mov EBX,EBP
  xor EBX,[ESP + 648]
  and EBX,ESI
  and EBP,[ESP + 648]
  xor EBX,EBP
  mov EBP,[ESP + 660]
  mov ECX,EBP
  xor ECX,[ESP + 652]
  and ECX,EDI
  and EBP,[ESP + 652]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 696],EAX
  adc [ESP + 700],EDX


  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 688]
  adc EDX,[ESP + 692]
  mov EBX,[ESP + 680]
  xor EBX,[ESP + 672]
  and EBX,ESI
  xor EBX,[ESP + 680]
  mov ECX,[ESP + 684]
  xor ECX,[ESP + 676]
  and ECX,EDI
  xor ECX,[ESP + 684]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 200]
  adc EDX,[ESP + 204]
  add EAX,$2DB43210
  adc EDX,$A831C66D

  add [ESP + 656],EAX
  adc [ESP + 660],EDX
  mov [ESP + 688],EAX
  mov [ESP + 692],EDX

  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 648]
  mov EBX,EBP
  xor EBX,[ESP + 640]
  and EBX,ESI
  and EBP,[ESP + 640]
  xor EBX,EBP
  mov EBP,[ESP + 652]
  mov ECX,EBP
  xor ECX,[ESP + 644]
  and ECX,EDI
  and EBP,[ESP + 644]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 688],EAX
  adc [ESP + 692],EDX


  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 680]
  adc EDX,[ESP + 684]
  mov EBX,[ESP + 672]
  xor EBX,[ESP + 664]
  and EBX,ESI
  xor EBX,[ESP + 672]
  mov ECX,[ESP + 676]
  xor ECX,[ESP + 668]
  and ECX,EDI
  xor ECX,[ESP + 676]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 208]
  adc EDX,[ESP + 212]
  add EAX,$98FB213F
  adc EDX,$B00327C8

  add [ESP + 648],EAX
  adc [ESP + 652],EDX
  mov [ESP + 680],EAX
  mov [ESP + 684],EDX

  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 640]
  mov EBX,EBP
  xor EBX,[ESP + 696]
  and EBX,ESI
  and EBP,[ESP + 696]
  xor EBX,EBP
  mov EBP,[ESP + 644]
  mov ECX,EBP
  xor ECX,[ESP + 700]
  and ECX,EDI
  and EBP,[ESP + 700]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 680],EAX
  adc [ESP + 684],EDX


  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 672]
  adc EDX,[ESP + 676]
  mov EBX,[ESP + 664]
  xor EBX,[ESP + 656]
  and EBX,ESI
  xor EBX,[ESP + 664]
  mov ECX,[ESP + 668]
  xor ECX,[ESP + 660]
  and ECX,EDI
  xor ECX,[ESP + 668]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 216]
  adc EDX,[ESP + 220]
  add EAX,$BEEF0EE4
  adc EDX,$BF597FC7

  add [ESP + 640],EAX
  adc [ESP + 644],EDX
  mov [ESP + 672],EAX
  mov [ESP + 676],EDX

  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 696]
  mov EBX,EBP
  xor EBX,[ESP + 688]
  and EBX,ESI
  and EBP,[ESP + 688]
  xor EBX,EBP
  mov EBP,[ESP + 700]
  mov ECX,EBP
  xor ECX,[ESP + 692]
  and ECX,EDI
  and EBP,[ESP + 692]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 672],EAX
  adc [ESP + 676],EDX


  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 664]
  adc EDX,[ESP + 668]
  mov EBX,[ESP + 656]
  xor EBX,[ESP + 648]
  and EBX,ESI
  xor EBX,[ESP + 656]
  mov ECX,[ESP + 660]
  xor ECX,[ESP + 652]
  and ECX,EDI
  xor ECX,[ESP + 660]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 224]
  adc EDX,[ESP + 228]
  add EAX,$3DA88FC2
  adc EDX,$C6E00BF3

  add [ESP + 696],EAX
  adc [ESP + 700],EDX
  mov [ESP + 664],EAX
  mov [ESP + 668],EDX

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 688]
  mov EBX,EBP
  xor EBX,[ESP + 680]
  and EBX,ESI
  and EBP,[ESP + 680]
  xor EBX,EBP
  mov EBP,[ESP + 692]
  mov ECX,EBP
  xor ECX,[ESP + 684]
  and ECX,EDI
  and EBP,[ESP + 684]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 664],EAX
  adc [ESP + 668],EDX


  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 656]
  adc EDX,[ESP + 660]
  mov EBX,[ESP + 648]
  xor EBX,[ESP + 640]
  and EBX,ESI
  xor EBX,[ESP + 648]
  mov ECX,[ESP + 652]
  xor ECX,[ESP + 644]
  and ECX,EDI
  xor ECX,[ESP + 652]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 232]
  adc EDX,[ESP + 236]
  add EAX,$930AA725
  adc EDX,$D5A79147

  add [ESP + 688],EAX
  adc [ESP + 692],EDX
  mov [ESP + 656],EAX
  mov [ESP + 660],EDX

  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 680]
  mov EBX,EBP
  xor EBX,[ESP + 672]
  and EBX,ESI
  and EBP,[ESP + 672]
  xor EBX,EBP
  mov EBP,[ESP + 684]
  mov ECX,EBP
  xor ECX,[ESP + 676]
  and ECX,EDI
  and EBP,[ESP + 676]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 656],EAX
  adc [ESP + 660],EDX


  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 648]
  adc EDX,[ESP + 652]
  mov EBX,[ESP + 640]
  xor EBX,[ESP + 696]
  and EBX,ESI
  xor EBX,[ESP + 640]
  mov ECX,[ESP + 644]
  xor ECX,[ESP + 700]
  and ECX,EDI
  xor ECX,[ESP + 644]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 240]
  adc EDX,[ESP + 244]
  add EAX,$E003826F
  adc EDX,$06CA6351

  add [ESP + 680],EAX
  adc [ESP + 684],EDX
  mov [ESP + 648],EAX
  mov [ESP + 652],EDX

  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 672]
  mov EBX,EBP
  xor EBX,[ESP + 664]
  and EBX,ESI
  and EBP,[ESP + 664]
  xor EBX,EBP
  mov EBP,[ESP + 676]
  mov ECX,EBP
  xor ECX,[ESP + 668]
  and ECX,EDI
  and EBP,[ESP + 668]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 648],EAX
  adc [ESP + 652],EDX


  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 640]
  adc EDX,[ESP + 644]
  mov EBX,[ESP + 696]
  xor EBX,[ESP + 688]
  and EBX,ESI
  xor EBX,[ESP + 696]
  mov ECX,[ESP + 700]
  xor ECX,[ESP + 692]
  and ECX,EDI
  xor ECX,[ESP + 700]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 248]
  adc EDX,[ESP + 252]
  add EAX,$0A0E6E70
  adc EDX,$14292967

  add [ESP + 672],EAX
  adc [ESP + 676],EDX
  mov [ESP + 640],EAX
  mov [ESP + 644],EDX

  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 664]
  mov EBX,EBP
  xor EBX,[ESP + 656]
  and EBX,ESI
  and EBP,[ESP + 656]
  xor EBX,EBP
  mov EBP,[ESP + 668]
  mov ECX,EBP
  xor ECX,[ESP + 660]
  and ECX,EDI
  and EBP,[ESP + 660]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 640],EAX
  adc [ESP + 644],EDX


  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 696]
  adc EDX,[ESP + 700]
  mov EBX,[ESP + 688]
  xor EBX,[ESP + 680]
  and EBX,ESI
  xor EBX,[ESP + 688]
  mov ECX,[ESP + 692]
  xor ECX,[ESP + 684]
  and ECX,EDI
  xor ECX,[ESP + 692]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 256]
  adc EDX,[ESP + 260]
  add EAX,$46D22FFC
  adc EDX,$27B70A85

  add [ESP + 664],EAX
  adc [ESP + 668],EDX
  mov [ESP + 696],EAX
  mov [ESP + 700],EDX

  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 656]
  mov EBX,EBP
  xor EBX,[ESP + 648]
  and EBX,ESI
  and EBP,[ESP + 648]
  xor EBX,EBP
  mov EBP,[ESP + 660]
  mov ECX,EBP
  xor ECX,[ESP + 652]
  and ECX,EDI
  and EBP,[ESP + 652]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 696],EAX
  adc [ESP + 700],EDX


  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 688]
  adc EDX,[ESP + 692]
  mov EBX,[ESP + 680]
  xor EBX,[ESP + 672]
  and EBX,ESI
  xor EBX,[ESP + 680]
  mov ECX,[ESP + 684]
  xor ECX,[ESP + 676]
  and ECX,EDI
  xor ECX,[ESP + 684]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 264]
  adc EDX,[ESP + 268]
  add EAX,$5C26C926
  adc EDX,$2E1B2138

  add [ESP + 656],EAX
  adc [ESP + 660],EDX
  mov [ESP + 688],EAX
  mov [ESP + 692],EDX

  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 648]
  mov EBX,EBP
  xor EBX,[ESP + 640]
  and EBX,ESI
  and EBP,[ESP + 640]
  xor EBX,EBP
  mov EBP,[ESP + 652]
  mov ECX,EBP
  xor ECX,[ESP + 644]
  and ECX,EDI
  and EBP,[ESP + 644]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 688],EAX
  adc [ESP + 692],EDX


  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 680]
  adc EDX,[ESP + 684]
  mov EBX,[ESP + 672]
  xor EBX,[ESP + 664]
  and EBX,ESI
  xor EBX,[ESP + 672]
  mov ECX,[ESP + 676]
  xor ECX,[ESP + 668]
  and ECX,EDI
  xor ECX,[ESP + 676]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 272]
  adc EDX,[ESP + 276]
  add EAX,$5AC42AED
  adc EDX,$4D2C6DFC

  add [ESP + 648],EAX
  adc [ESP + 652],EDX
  mov [ESP + 680],EAX
  mov [ESP + 684],EDX

  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 640]
  mov EBX,EBP
  xor EBX,[ESP + 696]
  and EBX,ESI
  and EBP,[ESP + 696]
  xor EBX,EBP
  mov EBP,[ESP + 644]
  mov ECX,EBP
  xor ECX,[ESP + 700]
  and ECX,EDI
  and EBP,[ESP + 700]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 680],EAX
  adc [ESP + 684],EDX


  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 672]
  adc EDX,[ESP + 676]
  mov EBX,[ESP + 664]
  xor EBX,[ESP + 656]
  and EBX,ESI
  xor EBX,[ESP + 664]
  mov ECX,[ESP + 668]
  xor ECX,[ESP + 660]
  and ECX,EDI
  xor ECX,[ESP + 668]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 280]
  adc EDX,[ESP + 284]
  add EAX,$9D95B3DF
  adc EDX,$53380D13

  add [ESP + 640],EAX
  adc [ESP + 644],EDX
  mov [ESP + 672],EAX
  mov [ESP + 676],EDX

  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 696]
  mov EBX,EBP
  xor EBX,[ESP + 688]
  and EBX,ESI
  and EBP,[ESP + 688]
  xor EBX,EBP
  mov EBP,[ESP + 700]
  mov ECX,EBP
  xor ECX,[ESP + 692]
  and ECX,EDI
  and EBP,[ESP + 692]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 672],EAX
  adc [ESP + 676],EDX


  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 664]
  adc EDX,[ESP + 668]
  mov EBX,[ESP + 656]
  xor EBX,[ESP + 648]
  and EBX,ESI
  xor EBX,[ESP + 656]
  mov ECX,[ESP + 660]
  xor ECX,[ESP + 652]
  and ECX,EDI
  xor ECX,[ESP + 660]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 288]
  adc EDX,[ESP + 292]
  add EAX,$8BAF63DE
  adc EDX,$650A7354

  add [ESP + 696],EAX
  adc [ESP + 700],EDX
  mov [ESP + 664],EAX
  mov [ESP + 668],EDX

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 688]
  mov EBX,EBP
  xor EBX,[ESP + 680]
  and EBX,ESI
  and EBP,[ESP + 680]
  xor EBX,EBP
  mov EBP,[ESP + 692]
  mov ECX,EBP
  xor ECX,[ESP + 684]
  and ECX,EDI
  and EBP,[ESP + 684]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 664],EAX
  adc [ESP + 668],EDX


  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 656]
  adc EDX,[ESP + 660]
  mov EBX,[ESP + 648]
  xor EBX,[ESP + 640]
  and EBX,ESI
  xor EBX,[ESP + 648]
  mov ECX,[ESP + 652]
  xor ECX,[ESP + 644]
  and ECX,EDI
  xor ECX,[ESP + 652]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 296]
  adc EDX,[ESP + 300]
  add EAX,$3C77B2A8
  adc EDX,$766A0ABB

  add [ESP + 688],EAX
  adc [ESP + 692],EDX
  mov [ESP + 656],EAX
  mov [ESP + 660],EDX

  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 680]
  mov EBX,EBP
  xor EBX,[ESP + 672]
  and EBX,ESI
  and EBP,[ESP + 672]
  xor EBX,EBP
  mov EBP,[ESP + 684]
  mov ECX,EBP
  xor ECX,[ESP + 676]
  and ECX,EDI
  and EBP,[ESP + 676]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 656],EAX
  adc [ESP + 660],EDX


  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 648]
  adc EDX,[ESP + 652]
  mov EBX,[ESP + 640]
  xor EBX,[ESP + 696]
  and EBX,ESI
  xor EBX,[ESP + 640]
  mov ECX,[ESP + 644]
  xor ECX,[ESP + 700]
  and ECX,EDI
  xor ECX,[ESP + 644]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 304]
  adc EDX,[ESP + 308]
  add EAX,$47EDAEE6
  adc EDX,$81C2C92E

  add [ESP + 680],EAX
  adc [ESP + 684],EDX
  mov [ESP + 648],EAX
  mov [ESP + 652],EDX

  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 672]
  mov EBX,EBP
  xor EBX,[ESP + 664]
  and EBX,ESI
  and EBP,[ESP + 664]
  xor EBX,EBP
  mov EBP,[ESP + 676]
  mov ECX,EBP
  xor ECX,[ESP + 668]
  and ECX,EDI
  and EBP,[ESP + 668]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 648],EAX
  adc [ESP + 652],EDX


  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 640]
  adc EDX,[ESP + 644]
  mov EBX,[ESP + 696]
  xor EBX,[ESP + 688]
  and EBX,ESI
  xor EBX,[ESP + 696]
  mov ECX,[ESP + 700]
  xor ECX,[ESP + 692]
  and ECX,EDI
  xor ECX,[ESP + 700]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 312]
  adc EDX,[ESP + 316]
  add EAX,$1482353B
  adc EDX,$92722C85

  add [ESP + 672],EAX
  adc [ESP + 676],EDX
  mov [ESP + 640],EAX
  mov [ESP + 644],EDX

  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 664]
  mov EBX,EBP
  xor EBX,[ESP + 656]
  and EBX,ESI
  and EBP,[ESP + 656]
  xor EBX,EBP
  mov EBP,[ESP + 668]
  mov ECX,EBP
  xor ECX,[ESP + 660]
  and ECX,EDI
  and EBP,[ESP + 660]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 640],EAX
  adc [ESP + 644],EDX


  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 696]
  adc EDX,[ESP + 700]
  mov EBX,[ESP + 688]
  xor EBX,[ESP + 680]
  and EBX,ESI
  xor EBX,[ESP + 688]
  mov ECX,[ESP + 692]
  xor ECX,[ESP + 684]
  and ECX,EDI
  xor ECX,[ESP + 692]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 320]
  adc EDX,[ESP + 324]
  add EAX,$4CF10364
  adc EDX,$A2BFE8A1

  add [ESP + 664],EAX
  adc [ESP + 668],EDX
  mov [ESP + 696],EAX
  mov [ESP + 700],EDX

  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 656]
  mov EBX,EBP
  xor EBX,[ESP + 648]
  and EBX,ESI
  and EBP,[ESP + 648]
  xor EBX,EBP
  mov EBP,[ESP + 660]
  mov ECX,EBP
  xor ECX,[ESP + 652]
  and ECX,EDI
  and EBP,[ESP + 652]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 696],EAX
  adc [ESP + 700],EDX


  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 688]
  adc EDX,[ESP + 692]
  mov EBX,[ESP + 680]
  xor EBX,[ESP + 672]
  and EBX,ESI
  xor EBX,[ESP + 680]
  mov ECX,[ESP + 684]
  xor ECX,[ESP + 676]
  and ECX,EDI
  xor ECX,[ESP + 684]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 328]
  adc EDX,[ESP + 332]
  add EAX,$BC423001
  adc EDX,$A81A664B

  add [ESP + 656],EAX
  adc [ESP + 660],EDX
  mov [ESP + 688],EAX
  mov [ESP + 692],EDX

  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 648]
  mov EBX,EBP
  xor EBX,[ESP + 640]
  and EBX,ESI
  and EBP,[ESP + 640]
  xor EBX,EBP
  mov EBP,[ESP + 652]
  mov ECX,EBP
  xor ECX,[ESP + 644]
  and ECX,EDI
  and EBP,[ESP + 644]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 688],EAX
  adc [ESP + 692],EDX


  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 680]
  adc EDX,[ESP + 684]
  mov EBX,[ESP + 672]
  xor EBX,[ESP + 664]
  and EBX,ESI
  xor EBX,[ESP + 672]
  mov ECX,[ESP + 676]
  xor ECX,[ESP + 668]
  and ECX,EDI
  xor ECX,[ESP + 676]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 336]
  adc EDX,[ESP + 340]
  add EAX,$D0F89791
  adc EDX,$C24B8B70

  add [ESP + 648],EAX
  adc [ESP + 652],EDX
  mov [ESP + 680],EAX
  mov [ESP + 684],EDX

  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 640]
  mov EBX,EBP
  xor EBX,[ESP + 696]
  and EBX,ESI
  and EBP,[ESP + 696]
  xor EBX,EBP
  mov EBP,[ESP + 644]
  mov ECX,EBP
  xor ECX,[ESP + 700]
  and ECX,EDI
  and EBP,[ESP + 700]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 680],EAX
  adc [ESP + 684],EDX


  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 672]
  adc EDX,[ESP + 676]
  mov EBX,[ESP + 664]
  xor EBX,[ESP + 656]
  and EBX,ESI
  xor EBX,[ESP + 664]
  mov ECX,[ESP + 668]
  xor ECX,[ESP + 660]
  and ECX,EDI
  xor ECX,[ESP + 668]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 344]
  adc EDX,[ESP + 348]
  add EAX,$0654BE30
  adc EDX,$C76C51A3

  add [ESP + 640],EAX
  adc [ESP + 644],EDX
  mov [ESP + 672],EAX
  mov [ESP + 676],EDX

  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 696]
  mov EBX,EBP
  xor EBX,[ESP + 688]
  and EBX,ESI
  and EBP,[ESP + 688]
  xor EBX,EBP
  mov EBP,[ESP + 700]
  mov ECX,EBP
  xor ECX,[ESP + 692]
  and ECX,EDI
  and EBP,[ESP + 692]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 672],EAX
  adc [ESP + 676],EDX


  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 664]
  adc EDX,[ESP + 668]
  mov EBX,[ESP + 656]
  xor EBX,[ESP + 648]
  and EBX,ESI
  xor EBX,[ESP + 656]
  mov ECX,[ESP + 660]
  xor ECX,[ESP + 652]
  and ECX,EDI
  xor ECX,[ESP + 660]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 352]
  adc EDX,[ESP + 356]
  add EAX,$D6EF5218
  adc EDX,$D192E819

  add [ESP + 696],EAX
  adc [ESP + 700],EDX
  mov [ESP + 664],EAX
  mov [ESP + 668],EDX

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 688]
  mov EBX,EBP
  xor EBX,[ESP + 680]
  and EBX,ESI
  and EBP,[ESP + 680]
  xor EBX,EBP
  mov EBP,[ESP + 692]
  mov ECX,EBP
  xor ECX,[ESP + 684]
  and ECX,EDI
  and EBP,[ESP + 684]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 664],EAX
  adc [ESP + 668],EDX


  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 656]
  adc EDX,[ESP + 660]
  mov EBX,[ESP + 648]
  xor EBX,[ESP + 640]
  and EBX,ESI
  xor EBX,[ESP + 648]
  mov ECX,[ESP + 652]
  xor ECX,[ESP + 644]
  and ECX,EDI
  xor ECX,[ESP + 652]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 360]
  adc EDX,[ESP + 364]
  add EAX,$5565A910
  adc EDX,$D6990624

  add [ESP + 688],EAX
  adc [ESP + 692],EDX
  mov [ESP + 656],EAX
  mov [ESP + 660],EDX

  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 680]
  mov EBX,EBP
  xor EBX,[ESP + 672]
  and EBX,ESI
  and EBP,[ESP + 672]
  xor EBX,EBP
  mov EBP,[ESP + 684]
  mov ECX,EBP
  xor ECX,[ESP + 676]
  and ECX,EDI
  and EBP,[ESP + 676]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 656],EAX
  adc [ESP + 660],EDX


  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 648]
  adc EDX,[ESP + 652]
  mov EBX,[ESP + 640]
  xor EBX,[ESP + 696]
  and EBX,ESI
  xor EBX,[ESP + 640]
  mov ECX,[ESP + 644]
  xor ECX,[ESP + 700]
  and ECX,EDI
  xor ECX,[ESP + 644]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 368]
  adc EDX,[ESP + 372]
  add EAX,$5771202A
  adc EDX,$F40E3585

  add [ESP + 680],EAX
  adc [ESP + 684],EDX
  mov [ESP + 648],EAX
  mov [ESP + 652],EDX

  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 672]
  mov EBX,EBP
  xor EBX,[ESP + 664]
  and EBX,ESI
  and EBP,[ESP + 664]
  xor EBX,EBP
  mov EBP,[ESP + 676]
  mov ECX,EBP
  xor ECX,[ESP + 668]
  and ECX,EDI
  and EBP,[ESP + 668]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 648],EAX
  adc [ESP + 652],EDX


  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 640]
  adc EDX,[ESP + 644]
  mov EBX,[ESP + 696]
  xor EBX,[ESP + 688]
  and EBX,ESI
  xor EBX,[ESP + 696]
  mov ECX,[ESP + 700]
  xor ECX,[ESP + 692]
  and ECX,EDI
  xor ECX,[ESP + 700]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 376]
  adc EDX,[ESP + 380]
  add EAX,$32BBD1B8
  adc EDX,$106AA070

  add [ESP + 672],EAX
  adc [ESP + 676],EDX
  mov [ESP + 640],EAX
  mov [ESP + 644],EDX

  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 664]
  mov EBX,EBP
  xor EBX,[ESP + 656]
  and EBX,ESI
  and EBP,[ESP + 656]
  xor EBX,EBP
  mov EBP,[ESP + 668]
  mov ECX,EBP
  xor ECX,[ESP + 660]
  and ECX,EDI
  and EBP,[ESP + 660]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 640],EAX
  adc [ESP + 644],EDX


  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 696]
  adc EDX,[ESP + 700]
  mov EBX,[ESP + 688]
  xor EBX,[ESP + 680]
  and EBX,ESI
  xor EBX,[ESP + 688]
  mov ECX,[ESP + 692]
  xor ECX,[ESP + 684]
  and ECX,EDI
  xor ECX,[ESP + 692]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 384]
  adc EDX,[ESP + 388]
  add EAX,$B8D2D0C8
  adc EDX,$19A4C116

  add [ESP + 664],EAX
  adc [ESP + 668],EDX
  mov [ESP + 696],EAX
  mov [ESP + 700],EDX

  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 656]
  mov EBX,EBP
  xor EBX,[ESP + 648]
  and EBX,ESI
  and EBP,[ESP + 648]
  xor EBX,EBP
  mov EBP,[ESP + 660]
  mov ECX,EBP
  xor ECX,[ESP + 652]
  and ECX,EDI
  and EBP,[ESP + 652]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 696],EAX
  adc [ESP + 700],EDX


  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 688]
  adc EDX,[ESP + 692]
  mov EBX,[ESP + 680]
  xor EBX,[ESP + 672]
  and EBX,ESI
  xor EBX,[ESP + 680]
  mov ECX,[ESP + 684]
  xor ECX,[ESP + 676]
  and ECX,EDI
  xor ECX,[ESP + 684]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 392]
  adc EDX,[ESP + 396]
  add EAX,$5141AB53
  adc EDX,$1E376C08

  add [ESP + 656],EAX
  adc [ESP + 660],EDX
  mov [ESP + 688],EAX
  mov [ESP + 692],EDX

  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 648]
  mov EBX,EBP
  xor EBX,[ESP + 640]
  and EBX,ESI
  and EBP,[ESP + 640]
  xor EBX,EBP
  mov EBP,[ESP + 652]
  mov ECX,EBP
  xor ECX,[ESP + 644]
  and ECX,EDI
  and EBP,[ESP + 644]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 688],EAX
  adc [ESP + 692],EDX


  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 680]
  adc EDX,[ESP + 684]
  mov EBX,[ESP + 672]
  xor EBX,[ESP + 664]
  and EBX,ESI
  xor EBX,[ESP + 672]
  mov ECX,[ESP + 676]
  xor ECX,[ESP + 668]
  and ECX,EDI
  xor ECX,[ESP + 676]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 400]
  adc EDX,[ESP + 404]
  add EAX,$DF8EEB99
  adc EDX,$2748774C

  add [ESP + 648],EAX
  adc [ESP + 652],EDX
  mov [ESP + 680],EAX
  mov [ESP + 684],EDX

  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 640]
  mov EBX,EBP
  xor EBX,[ESP + 696]
  and EBX,ESI
  and EBP,[ESP + 696]
  xor EBX,EBP
  mov EBP,[ESP + 644]
  mov ECX,EBP
  xor ECX,[ESP + 700]
  and ECX,EDI
  and EBP,[ESP + 700]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 680],EAX
  adc [ESP + 684],EDX


  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 672]
  adc EDX,[ESP + 676]
  mov EBX,[ESP + 664]
  xor EBX,[ESP + 656]
  and EBX,ESI
  xor EBX,[ESP + 664]
  mov ECX,[ESP + 668]
  xor ECX,[ESP + 660]
  and ECX,EDI
  xor ECX,[ESP + 668]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 408]
  adc EDX,[ESP + 412]
  add EAX,$E19B48A8
  adc EDX,$34B0BCB5

  add [ESP + 640],EAX
  adc [ESP + 644],EDX
  mov [ESP + 672],EAX
  mov [ESP + 676],EDX

  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 696]
  mov EBX,EBP
  xor EBX,[ESP + 688]
  and EBX,ESI
  and EBP,[ESP + 688]
  xor EBX,EBP
  mov EBP,[ESP + 700]
  mov ECX,EBP
  xor ECX,[ESP + 692]
  and ECX,EDI
  and EBP,[ESP + 692]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 672],EAX
  adc [ESP + 676],EDX


  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 664]
  adc EDX,[ESP + 668]
  mov EBX,[ESP + 656]
  xor EBX,[ESP + 648]
  and EBX,ESI
  xor EBX,[ESP + 656]
  mov ECX,[ESP + 660]
  xor ECX,[ESP + 652]
  and ECX,EDI
  xor ECX,[ESP + 660]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 416]
  adc EDX,[ESP + 420]
  add EAX,$C5C95A63
  adc EDX,$391C0CB3

  add [ESP + 696],EAX
  adc [ESP + 700],EDX
  mov [ESP + 664],EAX
  mov [ESP + 668],EDX

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 688]
  mov EBX,EBP
  xor EBX,[ESP + 680]
  and EBX,ESI
  and EBP,[ESP + 680]
  xor EBX,EBP
  mov EBP,[ESP + 692]
  mov ECX,EBP
  xor ECX,[ESP + 684]
  and ECX,EDI
  and EBP,[ESP + 684]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 664],EAX
  adc [ESP + 668],EDX


  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 656]
  adc EDX,[ESP + 660]
  mov EBX,[ESP + 648]
  xor EBX,[ESP + 640]
  and EBX,ESI
  xor EBX,[ESP + 648]
  mov ECX,[ESP + 652]
  xor ECX,[ESP + 644]
  and ECX,EDI
  xor ECX,[ESP + 652]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 424]
  adc EDX,[ESP + 428]
  add EAX,$E3418ACB
  adc EDX,$4ED8AA4A

  add [ESP + 688],EAX
  adc [ESP + 692],EDX
  mov [ESP + 656],EAX
  mov [ESP + 660],EDX

  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 680]
  mov EBX,EBP
  xor EBX,[ESP + 672]
  and EBX,ESI
  and EBP,[ESP + 672]
  xor EBX,EBP
  mov EBP,[ESP + 684]
  mov ECX,EBP
  xor ECX,[ESP + 676]
  and ECX,EDI
  and EBP,[ESP + 676]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 656],EAX
  adc [ESP + 660],EDX


  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 648]
  adc EDX,[ESP + 652]
  mov EBX,[ESP + 640]
  xor EBX,[ESP + 696]
  and EBX,ESI
  xor EBX,[ESP + 640]
  mov ECX,[ESP + 644]
  xor ECX,[ESP + 700]
  and ECX,EDI
  xor ECX,[ESP + 644]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 432]
  adc EDX,[ESP + 436]
  add EAX,$7763E373
  adc EDX,$5B9CCA4F

  add [ESP + 680],EAX
  adc [ESP + 684],EDX
  mov [ESP + 648],EAX
  mov [ESP + 652],EDX

  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 672]
  mov EBX,EBP
  xor EBX,[ESP + 664]
  and EBX,ESI
  and EBP,[ESP + 664]
  xor EBX,EBP
  mov EBP,[ESP + 676]
  mov ECX,EBP
  xor ECX,[ESP + 668]
  and ECX,EDI
  and EBP,[ESP + 668]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 648],EAX
  adc [ESP + 652],EDX


  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 640]
  adc EDX,[ESP + 644]
  mov EBX,[ESP + 696]
  xor EBX,[ESP + 688]
  and EBX,ESI
  xor EBX,[ESP + 696]
  mov ECX,[ESP + 700]
  xor ECX,[ESP + 692]
  and ECX,EDI
  xor ECX,[ESP + 700]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 440]
  adc EDX,[ESP + 444]
  add EAX,$D6B2B8A3
  adc EDX,$682E6FF3

  add [ESP + 672],EAX
  adc [ESP + 676],EDX
  mov [ESP + 640],EAX
  mov [ESP + 644],EDX

  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 664]
  mov EBX,EBP
  xor EBX,[ESP + 656]
  and EBX,ESI
  and EBP,[ESP + 656]
  xor EBX,EBP
  mov EBP,[ESP + 668]
  mov ECX,EBP
  xor ECX,[ESP + 660]
  and ECX,EDI
  and EBP,[ESP + 660]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 640],EAX
  adc [ESP + 644],EDX


  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 696]
  adc EDX,[ESP + 700]
  mov EBX,[ESP + 688]
  xor EBX,[ESP + 680]
  and EBX,ESI
  xor EBX,[ESP + 688]
  mov ECX,[ESP + 692]
  xor ECX,[ESP + 684]
  and ECX,EDI
  xor ECX,[ESP + 692]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 448]
  adc EDX,[ESP + 452]
  add EAX,$5DEFB2FC
  adc EDX,$748F82EE

  add [ESP + 664],EAX
  adc [ESP + 668],EDX
  mov [ESP + 696],EAX
  mov [ESP + 700],EDX

  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 656]
  mov EBX,EBP
  xor EBX,[ESP + 648]
  and EBX,ESI
  and EBP,[ESP + 648]
  xor EBX,EBP
  mov EBP,[ESP + 660]
  mov ECX,EBP
  xor ECX,[ESP + 652]
  and ECX,EDI
  and EBP,[ESP + 652]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 696],EAX
  adc [ESP + 700],EDX


  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 688]
  adc EDX,[ESP + 692]
  mov EBX,[ESP + 680]
  xor EBX,[ESP + 672]
  and EBX,ESI
  xor EBX,[ESP + 680]
  mov ECX,[ESP + 684]
  xor ECX,[ESP + 676]
  and ECX,EDI
  xor ECX,[ESP + 684]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 456]
  adc EDX,[ESP + 460]
  add EAX,$43172F60
  adc EDX,$78A5636F

  add [ESP + 656],EAX
  adc [ESP + 660],EDX
  mov [ESP + 688],EAX
  mov [ESP + 692],EDX

  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 648]
  mov EBX,EBP
  xor EBX,[ESP + 640]
  and EBX,ESI
  and EBP,[ESP + 640]
  xor EBX,EBP
  mov EBP,[ESP + 652]
  mov ECX,EBP
  xor ECX,[ESP + 644]
  and ECX,EDI
  and EBP,[ESP + 644]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 688],EAX
  adc [ESP + 692],EDX


  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 680]
  adc EDX,[ESP + 684]
  mov EBX,[ESP + 672]
  xor EBX,[ESP + 664]
  and EBX,ESI
  xor EBX,[ESP + 672]
  mov ECX,[ESP + 676]
  xor ECX,[ESP + 668]
  and ECX,EDI
  xor ECX,[ESP + 676]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 464]
  adc EDX,[ESP + 468]
  add EAX,$A1F0AB72
  adc EDX,$84C87814

  add [ESP + 648],EAX
  adc [ESP + 652],EDX
  mov [ESP + 680],EAX
  mov [ESP + 684],EDX

  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 640]
  mov EBX,EBP
  xor EBX,[ESP + 696]
  and EBX,ESI
  and EBP,[ESP + 696]
  xor EBX,EBP
  mov EBP,[ESP + 644]
  mov ECX,EBP
  xor ECX,[ESP + 700]
  and ECX,EDI
  and EBP,[ESP + 700]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 680],EAX
  adc [ESP + 684],EDX


  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 672]
  adc EDX,[ESP + 676]
  mov EBX,[ESP + 664]
  xor EBX,[ESP + 656]
  and EBX,ESI
  xor EBX,[ESP + 664]
  mov ECX,[ESP + 668]
  xor ECX,[ESP + 660]
  and ECX,EDI
  xor ECX,[ESP + 668]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 472]
  adc EDX,[ESP + 476]
  add EAX,$1A6439EC
  adc EDX,$8CC70208

  add [ESP + 640],EAX
  adc [ESP + 644],EDX
  mov [ESP + 672],EAX
  mov [ESP + 676],EDX

  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 696]
  mov EBX,EBP
  xor EBX,[ESP + 688]
  and EBX,ESI
  and EBP,[ESP + 688]
  xor EBX,EBP
  mov EBP,[ESP + 700]
  mov ECX,EBP
  xor ECX,[ESP + 692]
  and ECX,EDI
  and EBP,[ESP + 692]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 672],EAX
  adc [ESP + 676],EDX


  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 664]
  adc EDX,[ESP + 668]
  mov EBX,[ESP + 656]
  xor EBX,[ESP + 648]
  and EBX,ESI
  xor EBX,[ESP + 656]
  mov ECX,[ESP + 660]
  xor ECX,[ESP + 652]
  and ECX,EDI
  xor ECX,[ESP + 660]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 480]
  adc EDX,[ESP + 484]
  add EAX,$23631E28
  adc EDX,$90BEFFFA

  add [ESP + 696],EAX
  adc [ESP + 700],EDX
  mov [ESP + 664],EAX
  mov [ESP + 668],EDX

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 688]
  mov EBX,EBP
  xor EBX,[ESP + 680]
  and EBX,ESI
  and EBP,[ESP + 680]
  xor EBX,EBP
  mov EBP,[ESP + 692]
  mov ECX,EBP
  xor ECX,[ESP + 684]
  and ECX,EDI
  and EBP,[ESP + 684]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 664],EAX
  adc [ESP + 668],EDX


  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 656]
  adc EDX,[ESP + 660]
  mov EBX,[ESP + 648]
  xor EBX,[ESP + 640]
  and EBX,ESI
  xor EBX,[ESP + 648]
  mov ECX,[ESP + 652]
  xor ECX,[ESP + 644]
  and ECX,EDI
  xor ECX,[ESP + 652]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 488]
  adc EDX,[ESP + 492]
  add EAX,$DE82BDE9
  adc EDX,$A4506CEB

  add [ESP + 688],EAX
  adc [ESP + 692],EDX
  mov [ESP + 656],EAX
  mov [ESP + 660],EDX

  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 680]
  mov EBX,EBP
  xor EBX,[ESP + 672]
  and EBX,ESI
  and EBP,[ESP + 672]
  xor EBX,EBP
  mov EBP,[ESP + 684]
  mov ECX,EBP
  xor ECX,[ESP + 676]
  and ECX,EDI
  and EBP,[ESP + 676]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 656],EAX
  adc [ESP + 660],EDX


  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 648]
  adc EDX,[ESP + 652]
  mov EBX,[ESP + 640]
  xor EBX,[ESP + 696]
  and EBX,ESI
  xor EBX,[ESP + 640]
  mov ECX,[ESP + 644]
  xor ECX,[ESP + 700]
  and ECX,EDI
  xor ECX,[ESP + 644]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 496]
  adc EDX,[ESP + 500]
  add EAX,$B2C67915
  adc EDX,$BEF9A3F7

  add [ESP + 680],EAX
  adc [ESP + 684],EDX
  mov [ESP + 648],EAX
  mov [ESP + 652],EDX

  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 672]
  mov EBX,EBP
  xor EBX,[ESP + 664]
  and EBX,ESI
  and EBP,[ESP + 664]
  xor EBX,EBP
  mov EBP,[ESP + 676]
  mov ECX,EBP
  xor ECX,[ESP + 668]
  and ECX,EDI
  and EBP,[ESP + 668]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 648],EAX
  adc [ESP + 652],EDX


  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 640]
  adc EDX,[ESP + 644]
  mov EBX,[ESP + 696]
  xor EBX,[ESP + 688]
  and EBX,ESI
  xor EBX,[ESP + 696]
  mov ECX,[ESP + 700]
  xor ECX,[ESP + 692]
  and ECX,EDI
  xor ECX,[ESP + 700]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 504]
  adc EDX,[ESP + 508]
  add EAX,$E372532B
  adc EDX,$C67178F2

  add [ESP + 672],EAX
  adc [ESP + 676],EDX
  mov [ESP + 640],EAX
  mov [ESP + 644],EDX

  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 664]
  mov EBX,EBP
  xor EBX,[ESP + 656]
  and EBX,ESI
  and EBP,[ESP + 656]
  xor EBX,EBP
  mov EBP,[ESP + 668]
  mov ECX,EBP
  xor ECX,[ESP + 660]
  and ECX,EDI
  and EBP,[ESP + 660]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 640],EAX
  adc [ESP + 644],EDX


  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 696]
  adc EDX,[ESP + 700]
  mov EBX,[ESP + 688]
  xor EBX,[ESP + 680]
  and EBX,ESI
  xor EBX,[ESP + 688]
  mov ECX,[ESP + 692]
  xor ECX,[ESP + 684]
  and ECX,EDI
  xor ECX,[ESP + 692]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 512]
  adc EDX,[ESP + 516]
  add EAX,$EA26619C
  adc EDX,$CA273ECE

  add [ESP + 664],EAX
  adc [ESP + 668],EDX
  mov [ESP + 696],EAX
  mov [ESP + 700],EDX

  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 656]
  mov EBX,EBP
  xor EBX,[ESP + 648]
  and EBX,ESI
  and EBP,[ESP + 648]
  xor EBX,EBP
  mov EBP,[ESP + 660]
  mov ECX,EBP
  xor ECX,[ESP + 652]
  and ECX,EDI
  and EBP,[ESP + 652]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 696],EAX
  adc [ESP + 700],EDX


  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 688]
  adc EDX,[ESP + 692]
  mov EBX,[ESP + 680]
  xor EBX,[ESP + 672]
  and EBX,ESI
  xor EBX,[ESP + 680]
  mov ECX,[ESP + 684]
  xor ECX,[ESP + 676]
  and ECX,EDI
  xor ECX,[ESP + 684]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 520]
  adc EDX,[ESP + 524]
  add EAX,$21C0C207
  adc EDX,$D186B8C7

  add [ESP + 656],EAX
  adc [ESP + 660],EDX
  mov [ESP + 688],EAX
  mov [ESP + 692],EDX

  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 648]
  mov EBX,EBP
  xor EBX,[ESP + 640]
  and EBX,ESI
  and EBP,[ESP + 640]
  xor EBX,EBP
  mov EBP,[ESP + 652]
  mov ECX,EBP
  xor ECX,[ESP + 644]
  and ECX,EDI
  and EBP,[ESP + 644]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 688],EAX
  adc [ESP + 692],EDX


  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 680]
  adc EDX,[ESP + 684]
  mov EBX,[ESP + 672]
  xor EBX,[ESP + 664]
  and EBX,ESI
  xor EBX,[ESP + 672]
  mov ECX,[ESP + 676]
  xor ECX,[ESP + 668]
  and ECX,EDI
  xor ECX,[ESP + 676]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 528]
  adc EDX,[ESP + 532]
  add EAX,$CDE0EB1E
  adc EDX,$EADA7DD6

  add [ESP + 648],EAX
  adc [ESP + 652],EDX
  mov [ESP + 680],EAX
  mov [ESP + 684],EDX

  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 640]
  mov EBX,EBP
  xor EBX,[ESP + 696]
  and EBX,ESI
  and EBP,[ESP + 696]
  xor EBX,EBP
  mov EBP,[ESP + 644]
  mov ECX,EBP
  xor ECX,[ESP + 700]
  and ECX,EDI
  and EBP,[ESP + 700]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 680],EAX
  adc [ESP + 684],EDX


  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 672]
  adc EDX,[ESP + 676]
  mov EBX,[ESP + 664]
  xor EBX,[ESP + 656]
  and EBX,ESI
  xor EBX,[ESP + 664]
  mov ECX,[ESP + 668]
  xor ECX,[ESP + 660]
  and ECX,EDI
  xor ECX,[ESP + 668]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 536]
  adc EDX,[ESP + 540]
  add EAX,$EE6ED178
  adc EDX,$F57D4F7F

  add [ESP + 640],EAX
  adc [ESP + 644],EDX
  mov [ESP + 672],EAX
  mov [ESP + 676],EDX

  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 696]
  mov EBX,EBP
  xor EBX,[ESP + 688]
  and EBX,ESI
  and EBP,[ESP + 688]
  xor EBX,EBP
  mov EBP,[ESP + 700]
  mov ECX,EBP
  xor ECX,[ESP + 692]
  and ECX,EDI
  and EBP,[ESP + 692]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 672],EAX
  adc [ESP + 676],EDX


  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 664]
  adc EDX,[ESP + 668]
  mov EBX,[ESP + 656]
  xor EBX,[ESP + 648]
  and EBX,ESI
  xor EBX,[ESP + 656]
  mov ECX,[ESP + 660]
  xor ECX,[ESP + 652]
  and ECX,EDI
  xor ECX,[ESP + 660]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 544]
  adc EDX,[ESP + 548]
  add EAX,$72176FBA
  adc EDX,$06F067AA

  add [ESP + 696],EAX
  adc [ESP + 700],EDX
  mov [ESP + 664],EAX
  mov [ESP + 668],EDX

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 688]
  mov EBX,EBP
  xor EBX,[ESP + 680]
  and EBX,ESI
  and EBP,[ESP + 680]
  xor EBX,EBP
  mov EBP,[ESP + 692]
  mov ECX,EBP
  xor ECX,[ESP + 684]
  and ECX,EDI
  and EBP,[ESP + 684]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 664],EAX
  adc [ESP + 668],EDX


  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 656]
  adc EDX,[ESP + 660]
  mov EBX,[ESP + 648]
  xor EBX,[ESP + 640]
  and EBX,ESI
  xor EBX,[ESP + 648]
  mov ECX,[ESP + 652]
  xor ECX,[ESP + 644]
  and ECX,EDI
  xor ECX,[ESP + 652]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 552]
  adc EDX,[ESP + 556]
  add EAX,$A2C898A6
  adc EDX,$0A637DC5

  add [ESP + 688],EAX
  adc [ESP + 692],EDX
  mov [ESP + 656],EAX
  mov [ESP + 660],EDX

  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 680]
  mov EBX,EBP
  xor EBX,[ESP + 672]
  and EBX,ESI
  and EBP,[ESP + 672]
  xor EBX,EBP
  mov EBP,[ESP + 684]
  mov ECX,EBP
  xor ECX,[ESP + 676]
  and ECX,EDI
  and EBP,[ESP + 676]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 656],EAX
  adc [ESP + 660],EDX


  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 648]
  adc EDX,[ESP + 652]
  mov EBX,[ESP + 640]
  xor EBX,[ESP + 696]
  and EBX,ESI
  xor EBX,[ESP + 640]
  mov ECX,[ESP + 644]
  xor ECX,[ESP + 700]
  and ECX,EDI
  xor ECX,[ESP + 644]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 560]
  adc EDX,[ESP + 564]
  add EAX,$BEF90DAE
  adc EDX,$113F9804

  add [ESP + 680],EAX
  adc [ESP + 684],EDX
  mov [ESP + 648],EAX
  mov [ESP + 652],EDX

  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 672]
  mov EBX,EBP
  xor EBX,[ESP + 664]
  and EBX,ESI
  and EBP,[ESP + 664]
  xor EBX,EBP
  mov EBP,[ESP + 676]
  mov ECX,EBP
  xor ECX,[ESP + 668]
  and ECX,EDI
  and EBP,[ESP + 668]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 648],EAX
  adc [ESP + 652],EDX


  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 640]
  adc EDX,[ESP + 644]
  mov EBX,[ESP + 696]
  xor EBX,[ESP + 688]
  and EBX,ESI
  xor EBX,[ESP + 696]
  mov ECX,[ESP + 700]
  xor ECX,[ESP + 692]
  and ECX,EDI
  xor ECX,[ESP + 700]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 568]
  adc EDX,[ESP + 572]
  add EAX,$131C471B
  adc EDX,$1B710B35

  add [ESP + 672],EAX
  adc [ESP + 676],EDX
  mov [ESP + 640],EAX
  mov [ESP + 644],EDX

  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 664]
  mov EBX,EBP
  xor EBX,[ESP + 656]
  and EBX,ESI
  and EBP,[ESP + 656]
  xor EBX,EBP
  mov EBP,[ESP + 668]
  mov ECX,EBP
  xor ECX,[ESP + 660]
  and ECX,EDI
  and EBP,[ESP + 660]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 640],EAX
  adc [ESP + 644],EDX


  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 696]
  adc EDX,[ESP + 700]
  mov EBX,[ESP + 688]
  xor EBX,[ESP + 680]
  and EBX,ESI
  xor EBX,[ESP + 688]
  mov ECX,[ESP + 692]
  xor ECX,[ESP + 684]
  and ECX,EDI
  xor ECX,[ESP + 692]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 576]
  adc EDX,[ESP + 580]
  add EAX,$23047D84
  adc EDX,$28DB77F5

  add [ESP + 664],EAX
  adc [ESP + 668],EDX
  mov [ESP + 696],EAX
  mov [ESP + 700],EDX

  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 656]
  mov EBX,EBP
  xor EBX,[ESP + 648]
  and EBX,ESI
  and EBP,[ESP + 648]
  xor EBX,EBP
  mov EBP,[ESP + 660]
  mov ECX,EBP
  xor ECX,[ESP + 652]
  and ECX,EDI
  and EBP,[ESP + 652]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 696],EAX
  adc [ESP + 700],EDX


  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 688]
  adc EDX,[ESP + 692]
  mov EBX,[ESP + 680]
  xor EBX,[ESP + 672]
  and EBX,ESI
  xor EBX,[ESP + 680]
  mov ECX,[ESP + 684]
  xor ECX,[ESP + 676]
  and ECX,EDI
  xor ECX,[ESP + 684]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 584]
  adc EDX,[ESP + 588]
  add EAX,$40C72493
  adc EDX,$32CAAB7B

  add [ESP + 656],EAX
  adc [ESP + 660],EDX
  mov [ESP + 688],EAX
  mov [ESP + 692],EDX

  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 648]
  mov EBX,EBP
  xor EBX,[ESP + 640]
  and EBX,ESI
  and EBP,[ESP + 640]
  xor EBX,EBP
  mov EBP,[ESP + 652]
  mov ECX,EBP
  xor ECX,[ESP + 644]
  and ECX,EDI
  and EBP,[ESP + 644]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 688],EAX
  adc [ESP + 692],EDX


  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 680]
  adc EDX,[ESP + 684]
  mov EBX,[ESP + 672]
  xor EBX,[ESP + 664]
  and EBX,ESI
  xor EBX,[ESP + 672]
  mov ECX,[ESP + 676]
  xor ECX,[ESP + 668]
  and ECX,EDI
  xor ECX,[ESP + 676]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 592]
  adc EDX,[ESP + 596]
  add EAX,$15C9BEBC
  adc EDX,$3C9EBE0A

  add [ESP + 648],EAX
  adc [ESP + 652],EDX
  mov [ESP + 680],EAX
  mov [ESP + 684],EDX

  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 640]
  mov EBX,EBP
  xor EBX,[ESP + 696]
  and EBX,ESI
  and EBP,[ESP + 696]
  xor EBX,EBP
  mov EBP,[ESP + 644]
  mov ECX,EBP
  xor ECX,[ESP + 700]
  and ECX,EDI
  and EBP,[ESP + 700]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 680],EAX
  adc [ESP + 684],EDX


  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 672]
  adc EDX,[ESP + 676]
  mov EBX,[ESP + 664]
  xor EBX,[ESP + 656]
  and EBX,ESI
  xor EBX,[ESP + 664]
  mov ECX,[ESP + 668]
  xor ECX,[ESP + 660]
  and ECX,EDI
  xor ECX,[ESP + 668]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 600]
  adc EDX,[ESP + 604]
  add EAX,$9C100D4C
  adc EDX,$431D67C4

  add [ESP + 640],EAX
  adc [ESP + 644],EDX
  mov [ESP + 672],EAX
  mov [ESP + 676],EDX

  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 696]
  mov EBX,EBP
  xor EBX,[ESP + 688]
  and EBX,ESI
  and EBP,[ESP + 688]
  xor EBX,EBP
  mov EBP,[ESP + 700]
  mov ECX,EBP
  xor ECX,[ESP + 692]
  and ECX,EDI
  and EBP,[ESP + 692]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 672],EAX
  adc [ESP + 676],EDX


  mov ESI,[ESP + 640]
  mov EDI,[ESP + 644]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 664]
  adc EDX,[ESP + 668]
  mov EBX,[ESP + 656]
  xor EBX,[ESP + 648]
  and EBX,ESI
  xor EBX,[ESP + 656]
  mov ECX,[ESP + 660]
  xor ECX,[ESP + 652]
  and ECX,EDI
  xor ECX,[ESP + 660]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 608]
  adc EDX,[ESP + 612]
  add EAX,$CB3E42B6
  adc EDX,$4CC5D4BE

  add [ESP + 696],EAX
  adc [ESP + 700],EDX
  mov [ESP + 664],EAX
  mov [ESP + 668],EDX

  mov ESI,[ESP + 672]
  mov EDI,[ESP + 676]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 688]
  mov EBX,EBP
  xor EBX,[ESP + 680]
  and EBX,ESI
  and EBP,[ESP + 680]
  xor EBX,EBP
  mov EBP,[ESP + 692]
  mov ECX,EBP
  xor ECX,[ESP + 684]
  and ECX,EDI
  and EBP,[ESP + 684]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 664],EAX
  adc [ESP + 668],EDX


  mov ESI,[ESP + 696]
  mov EDI,[ESP + 700]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 656]
  adc EDX,[ESP + 660]
  mov EBX,[ESP + 648]
  xor EBX,[ESP + 640]
  and EBX,ESI
  xor EBX,[ESP + 648]
  mov ECX,[ESP + 652]
  xor ECX,[ESP + 644]
  and ECX,EDI
  xor ECX,[ESP + 652]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 616]
  adc EDX,[ESP + 620]
  add EAX,$FC657E2A
  adc EDX,$597F299C

  add [ESP + 688],EAX
  adc [ESP + 692],EDX
  mov [ESP + 656],EAX
  mov [ESP + 660],EDX

  mov ESI,[ESP + 664]
  mov EDI,[ESP + 668]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 680]
  mov EBX,EBP
  xor EBX,[ESP + 672]
  and EBX,ESI
  and EBP,[ESP + 672]
  xor EBX,EBP
  mov EBP,[ESP + 684]
  mov ECX,EBP
  xor ECX,[ESP + 676]
  and ECX,EDI
  and EBP,[ESP + 676]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 656],EAX
  adc [ESP + 660],EDX


  mov ESI,[ESP + 688]
  mov EDI,[ESP + 692]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 648]
  adc EDX,[ESP + 652]
  mov EBX,[ESP + 640]
  xor EBX,[ESP + 696]
  and EBX,ESI
  xor EBX,[ESP + 640]
  mov ECX,[ESP + 644]
  xor ECX,[ESP + 700]
  and ECX,EDI
  xor ECX,[ESP + 644]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 624]
  adc EDX,[ESP + 628]
  add EAX,$3AD6FAEC
  adc EDX,$5FCB6FAB

  add [ESP + 680],EAX
  adc [ESP + 684],EDX
  mov [ESP + 648],EAX
  mov [ESP + 652],EDX

  mov ESI,[ESP + 656]
  mov EDI,[ESP + 660]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 672]
  mov EBX,EBP
  xor EBX,[ESP + 664]
  and EBX,ESI
  and EBP,[ESP + 664]
  xor EBX,EBP
  mov EBP,[ESP + 676]
  mov ECX,EBP
  xor ECX,[ESP + 668]
  and ECX,EDI
  and EBP,[ESP + 668]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 648],EAX
  adc [ESP + 652],EDX


  mov ESI,[ESP + 680]
  mov EDI,[ESP + 684]
  mov EAX,ESI
  shrd EAX,EDI,14
  mov EDX,EDI
  shrd EDX,ESI,14
  mov EBX,ESI
  shrd EBX,EDI,18
  xor EAX,EBX
  mov EBX,EDI
  shrd EBX,ESI,18
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,23
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,23
  xor EDX,EBX
  add EAX,[ESP + 640]
  adc EDX,[ESP + 644]
  mov EBX,[ESP + 696]
  xor EBX,[ESP + 688]
  and EBX,ESI
  xor EBX,[ESP + 696]
  mov ECX,[ESP + 700]
  xor ECX,[ESP + 692]
  and ECX,EDI
  xor ECX,[ESP + 700]
  add EAX,EBX
  adc EDX,ECX
  add EAX,[ESP + 632]
  adc EDX,[ESP + 636]
  add EAX,$4A475817
  adc EDX,$6C44198C

  add [ESP + 672],EAX
  adc [ESP + 676],EDX
  mov [ESP + 640],EAX
  mov [ESP + 644],EDX

  mov ESI,[ESP + 648]
  mov EDI,[ESP + 652]
  mov EAX,ESI
  shrd EAX,EDI,28
  mov EDX,EDI
  shrd EDX,ESI,28
  mov EBX,ESI
  shld EBX,EDI,30
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,30
  xor EDX,EBX
  mov EBX,ESI
  shld EBX,EDI,25
  xor EAX,EBX
  mov EBX,EDI
  shld EBX,ESI,25
  xor EDX,EBX
  mov EBP,[ESP + 664]
  mov EBX,EBP
  xor EBX,[ESP + 656]
  and EBX,ESI
  and EBP,[ESP + 656]
  xor EBX,EBP
  mov EBP,[ESP + 668]
  mov ECX,EBP
  xor ECX,[ESP + 660]
  and ECX,EDI
  and EBP,[ESP + 660]
  xor ECX,EBP
  add EAX,EBX
  adc EDX,ECX

  add [ESP + 640],EAX
  adc [ESP + 644],EDX


