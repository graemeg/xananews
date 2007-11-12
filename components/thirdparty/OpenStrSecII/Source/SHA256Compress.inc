{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     Security Utilities Unit                           }
{                                                       }
{     Copyright (C) 2000-2002 StreamSec Handelsbolag    }
{                                                       }
{*******************************************************}

  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 284]
  mov EBX,[ESP + 280]
  xor EBX,[ESP + 276]
  and EBX,ESI
  xor EBX,[ESP + 280]
  add EAX,EBX
  add EAX,[ESP + 0]
  add EAX,$428A2F98

  add [ESP + 268],EAX
  mov [ESP + 284],EAX

  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 264]
  mov EBX,EBP
  xor EBX,[ESP + 260]
  and EBX,ESI
  and EBP,[ESP + 260]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 284],EAX


  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 280]
  mov EBX,[ESP + 276]
  xor EBX,[ESP + 272]
  and EBX,ESI
  xor EBX,[ESP + 276]
  add EAX,EBX
  add EAX,[ESP + 4]
  add EAX,$71374491

  add [ESP + 264],EAX
  mov [ESP + 280],EAX

  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 260]
  mov EBX,EBP
  xor EBX,[ESP + 256]
  and EBX,ESI
  and EBP,[ESP + 256]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 280],EAX


  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 276]
  mov EBX,[ESP + 272]
  xor EBX,[ESP + 268]
  and EBX,ESI
  xor EBX,[ESP + 272]
  add EAX,EBX
  add EAX,[ESP + 8]
  add EAX,$B5C0FBCF

  add [ESP + 260],EAX
  mov [ESP + 276],EAX

  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 256]
  mov EBX,EBP
  xor EBX,[ESP + 284]
  and EBX,ESI
  and EBP,[ESP + 284]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 276],EAX


  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 272]
  mov EBX,[ESP + 268]
  xor EBX,[ESP + 264]
  and EBX,ESI
  xor EBX,[ESP + 268]
  add EAX,EBX
  add EAX,[ESP + 12]
  add EAX,$E9B5DBA5

  add [ESP + 256],EAX
  mov [ESP + 272],EAX

  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 284]
  mov EBX,EBP
  xor EBX,[ESP + 280]
  and EBX,ESI
  and EBP,[ESP + 280]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 272],EAX


  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 268]
  mov EBX,[ESP + 264]
  xor EBX,[ESP + 260]
  and EBX,ESI
  xor EBX,[ESP + 264]
  add EAX,EBX
  add EAX,[ESP + 16]
  add EAX,$3956C25B

  add [ESP + 284],EAX
  mov [ESP + 268],EAX

  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 280]
  mov EBX,EBP
  xor EBX,[ESP + 276]
  and EBX,ESI
  and EBP,[ESP + 276]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 268],EAX


  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 264]
  mov EBX,[ESP + 260]
  xor EBX,[ESP + 256]
  and EBX,ESI
  xor EBX,[ESP + 260]
  add EAX,EBX
  add EAX,[ESP + 20]
  add EAX,$59F111F1

  add [ESP + 280],EAX
  mov [ESP + 264],EAX

  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 276]
  mov EBX,EBP
  xor EBX,[ESP + 272]
  and EBX,ESI
  and EBP,[ESP + 272]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 264],EAX


  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 260]
  mov EBX,[ESP + 256]
  xor EBX,[ESP + 284]
  and EBX,ESI
  xor EBX,[ESP + 256]
  add EAX,EBX
  add EAX,[ESP + 24]
  add EAX,$923F82A4

  add [ESP + 276],EAX
  mov [ESP + 260],EAX

  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 272]
  mov EBX,EBP
  xor EBX,[ESP + 268]
  and EBX,ESI
  and EBP,[ESP + 268]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 260],EAX


  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 256]
  mov EBX,[ESP + 284]
  xor EBX,[ESP + 280]
  and EBX,ESI
  xor EBX,[ESP + 284]
  add EAX,EBX
  add EAX,[ESP + 28]
  add EAX,$AB1C5ED5

  add [ESP + 272],EAX
  mov [ESP + 256],EAX

  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 268]
  mov EBX,EBP
  xor EBX,[ESP + 264]
  and EBX,ESI
  and EBP,[ESP + 264]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 256],EAX


  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 284]
  mov EBX,[ESP + 280]
  xor EBX,[ESP + 276]
  and EBX,ESI
  xor EBX,[ESP + 280]
  add EAX,EBX
  add EAX,[ESP + 32]
  add EAX,$D807AA98

  add [ESP + 268],EAX
  mov [ESP + 284],EAX

  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 264]
  mov EBX,EBP
  xor EBX,[ESP + 260]
  and EBX,ESI
  and EBP,[ESP + 260]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 284],EAX


  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 280]
  mov EBX,[ESP + 276]
  xor EBX,[ESP + 272]
  and EBX,ESI
  xor EBX,[ESP + 276]
  add EAX,EBX
  add EAX,[ESP + 36]
  add EAX,$12835B01

  add [ESP + 264],EAX
  mov [ESP + 280],EAX

  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 260]
  mov EBX,EBP
  xor EBX,[ESP + 256]
  and EBX,ESI
  and EBP,[ESP + 256]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 280],EAX


  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 276]
  mov EBX,[ESP + 272]
  xor EBX,[ESP + 268]
  and EBX,ESI
  xor EBX,[ESP + 272]
  add EAX,EBX
  add EAX,[ESP + 40]
  add EAX,$243185BE

  add [ESP + 260],EAX
  mov [ESP + 276],EAX

  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 256]
  mov EBX,EBP
  xor EBX,[ESP + 284]
  and EBX,ESI
  and EBP,[ESP + 284]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 276],EAX


  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 272]
  mov EBX,[ESP + 268]
  xor EBX,[ESP + 264]
  and EBX,ESI
  xor EBX,[ESP + 268]
  add EAX,EBX
  add EAX,[ESP + 44]
  add EAX,$550C7DC3

  add [ESP + 256],EAX
  mov [ESP + 272],EAX

  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 284]
  mov EBX,EBP
  xor EBX,[ESP + 280]
  and EBX,ESI
  and EBP,[ESP + 280]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 272],EAX


  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 268]
  mov EBX,[ESP + 264]
  xor EBX,[ESP + 260]
  and EBX,ESI
  xor EBX,[ESP + 264]
  add EAX,EBX
  add EAX,[ESP + 48]
  add EAX,$72BE5D74

  add [ESP + 284],EAX
  mov [ESP + 268],EAX

  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 280]
  mov EBX,EBP
  xor EBX,[ESP + 276]
  and EBX,ESI
  and EBP,[ESP + 276]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 268],EAX


  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 264]
  mov EBX,[ESP + 260]
  xor EBX,[ESP + 256]
  and EBX,ESI
  xor EBX,[ESP + 260]
  add EAX,EBX
  add EAX,[ESP + 52]
  add EAX,$80DEB1FE

  add [ESP + 280],EAX
  mov [ESP + 264],EAX

  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 276]
  mov EBX,EBP
  xor EBX,[ESP + 272]
  and EBX,ESI
  and EBP,[ESP + 272]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 264],EAX


  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 260]
  mov EBX,[ESP + 256]
  xor EBX,[ESP + 284]
  and EBX,ESI
  xor EBX,[ESP + 256]
  add EAX,EBX
  add EAX,[ESP + 56]
  add EAX,$9BDC06A7

  add [ESP + 276],EAX
  mov [ESP + 260],EAX

  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 272]
  mov EBX,EBP
  xor EBX,[ESP + 268]
  and EBX,ESI
  and EBP,[ESP + 268]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 260],EAX


  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 256]
  mov EBX,[ESP + 284]
  xor EBX,[ESP + 280]
  and EBX,ESI
  xor EBX,[ESP + 284]
  add EAX,EBX
  add EAX,[ESP + 60]
  add EAX,$C19BF174

  add [ESP + 272],EAX
  mov [ESP + 256],EAX

  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 268]
  mov EBX,EBP
  xor EBX,[ESP + 264]
  and EBX,ESI
  and EBP,[ESP + 264]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 256],EAX


  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 284]
  mov EBX,[ESP + 280]
  xor EBX,[ESP + 276]
  and EBX,ESI
  xor EBX,[ESP + 280]
  add EAX,EBX
  add EAX,[ESP + 64]
  add EAX,$E49B69C1

  add [ESP + 268],EAX
  mov [ESP + 284],EAX

  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 264]
  mov EBX,EBP
  xor EBX,[ESP + 260]
  and EBX,ESI
  and EBP,[ESP + 260]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 284],EAX


  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 280]
  mov EBX,[ESP + 276]
  xor EBX,[ESP + 272]
  and EBX,ESI
  xor EBX,[ESP + 276]
  add EAX,EBX
  add EAX,[ESP + 68]
  add EAX,$EFBE4786

  add [ESP + 264],EAX
  mov [ESP + 280],EAX

  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 260]
  mov EBX,EBP
  xor EBX,[ESP + 256]
  and EBX,ESI
  and EBP,[ESP + 256]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 280],EAX


  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 276]
  mov EBX,[ESP + 272]
  xor EBX,[ESP + 268]
  and EBX,ESI
  xor EBX,[ESP + 272]
  add EAX,EBX
  add EAX,[ESP + 72]
  add EAX,$0FC19DC6

  add [ESP + 260],EAX
  mov [ESP + 276],EAX

  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 256]
  mov EBX,EBP
  xor EBX,[ESP + 284]
  and EBX,ESI
  and EBP,[ESP + 284]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 276],EAX


  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 272]
  mov EBX,[ESP + 268]
  xor EBX,[ESP + 264]
  and EBX,ESI
  xor EBX,[ESP + 268]
  add EAX,EBX
  add EAX,[ESP + 76]
  add EAX,$240CA1CC

  add [ESP + 256],EAX
  mov [ESP + 272],EAX

  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 284]
  mov EBX,EBP
  xor EBX,[ESP + 280]
  and EBX,ESI
  and EBP,[ESP + 280]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 272],EAX


  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 268]
  mov EBX,[ESP + 264]
  xor EBX,[ESP + 260]
  and EBX,ESI
  xor EBX,[ESP + 264]
  add EAX,EBX
  add EAX,[ESP + 80]
  add EAX,$2DE92C6F

  add [ESP + 284],EAX
  mov [ESP + 268],EAX

  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 280]
  mov EBX,EBP
  xor EBX,[ESP + 276]
  and EBX,ESI
  and EBP,[ESP + 276]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 268],EAX


  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 264]
  mov EBX,[ESP + 260]
  xor EBX,[ESP + 256]
  and EBX,ESI
  xor EBX,[ESP + 260]
  add EAX,EBX
  add EAX,[ESP + 84]
  add EAX,$4A7484AA

  add [ESP + 280],EAX
  mov [ESP + 264],EAX

  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 276]
  mov EBX,EBP
  xor EBX,[ESP + 272]
  and EBX,ESI
  and EBP,[ESP + 272]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 264],EAX


  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 260]
  mov EBX,[ESP + 256]
  xor EBX,[ESP + 284]
  and EBX,ESI
  xor EBX,[ESP + 256]
  add EAX,EBX
  add EAX,[ESP + 88]
  add EAX,$5CB0A9DC

  add [ESP + 276],EAX
  mov [ESP + 260],EAX

  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 272]
  mov EBX,EBP
  xor EBX,[ESP + 268]
  and EBX,ESI
  and EBP,[ESP + 268]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 260],EAX


  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 256]
  mov EBX,[ESP + 284]
  xor EBX,[ESP + 280]
  and EBX,ESI
  xor EBX,[ESP + 284]
  add EAX,EBX
  add EAX,[ESP + 92]
  add EAX,$76F988DA

  add [ESP + 272],EAX
  mov [ESP + 256],EAX

  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 268]
  mov EBX,EBP
  xor EBX,[ESP + 264]
  and EBX,ESI
  and EBP,[ESP + 264]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 256],EAX


  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 284]
  mov EBX,[ESP + 280]
  xor EBX,[ESP + 276]
  and EBX,ESI
  xor EBX,[ESP + 280]
  add EAX,EBX
  add EAX,[ESP + 96]
  add EAX,$983E5152

  add [ESP + 268],EAX
  mov [ESP + 284],EAX

  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 264]
  mov EBX,EBP
  xor EBX,[ESP + 260]
  and EBX,ESI
  and EBP,[ESP + 260]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 284],EAX


  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 280]
  mov EBX,[ESP + 276]
  xor EBX,[ESP + 272]
  and EBX,ESI
  xor EBX,[ESP + 276]
  add EAX,EBX
  add EAX,[ESP + 100]
  add EAX,$A831C66D

  add [ESP + 264],EAX
  mov [ESP + 280],EAX

  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 260]
  mov EBX,EBP
  xor EBX,[ESP + 256]
  and EBX,ESI
  and EBP,[ESP + 256]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 280],EAX


  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 276]
  mov EBX,[ESP + 272]
  xor EBX,[ESP + 268]
  and EBX,ESI
  xor EBX,[ESP + 272]
  add EAX,EBX
  add EAX,[ESP + 104]
  add EAX,$B00327C8

  add [ESP + 260],EAX
  mov [ESP + 276],EAX

  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 256]
  mov EBX,EBP
  xor EBX,[ESP + 284]
  and EBX,ESI
  and EBP,[ESP + 284]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 276],EAX


  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 272]
  mov EBX,[ESP + 268]
  xor EBX,[ESP + 264]
  and EBX,ESI
  xor EBX,[ESP + 268]
  add EAX,EBX
  add EAX,[ESP + 108]
  add EAX,$BF597FC7

  add [ESP + 256],EAX
  mov [ESP + 272],EAX

  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 284]
  mov EBX,EBP
  xor EBX,[ESP + 280]
  and EBX,ESI
  and EBP,[ESP + 280]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 272],EAX


  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 268]
  mov EBX,[ESP + 264]
  xor EBX,[ESP + 260]
  and EBX,ESI
  xor EBX,[ESP + 264]
  add EAX,EBX
  add EAX,[ESP + 112]
  add EAX,$C6E00BF3

  add [ESP + 284],EAX
  mov [ESP + 268],EAX

  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 280]
  mov EBX,EBP
  xor EBX,[ESP + 276]
  and EBX,ESI
  and EBP,[ESP + 276]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 268],EAX


  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 264]
  mov EBX,[ESP + 260]
  xor EBX,[ESP + 256]
  and EBX,ESI
  xor EBX,[ESP + 260]
  add EAX,EBX
  add EAX,[ESP + 116]
  add EAX,$D5A79147

  add [ESP + 280],EAX
  mov [ESP + 264],EAX

  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 276]
  mov EBX,EBP
  xor EBX,[ESP + 272]
  and EBX,ESI
  and EBP,[ESP + 272]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 264],EAX


  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 260]
  mov EBX,[ESP + 256]
  xor EBX,[ESP + 284]
  and EBX,ESI
  xor EBX,[ESP + 256]
  add EAX,EBX
  add EAX,[ESP + 120]
  add EAX,$06CA6351

  add [ESP + 276],EAX
  mov [ESP + 260],EAX

  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 272]
  mov EBX,EBP
  xor EBX,[ESP + 268]
  and EBX,ESI
  and EBP,[ESP + 268]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 260],EAX


  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 256]
  mov EBX,[ESP + 284]
  xor EBX,[ESP + 280]
  and EBX,ESI
  xor EBX,[ESP + 284]
  add EAX,EBX
  add EAX,[ESP + 124]
  add EAX,$14292967

  add [ESP + 272],EAX
  mov [ESP + 256],EAX

  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 268]
  mov EBX,EBP
  xor EBX,[ESP + 264]
  and EBX,ESI
  and EBP,[ESP + 264]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 256],EAX


  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 284]
  mov EBX,[ESP + 280]
  xor EBX,[ESP + 276]
  and EBX,ESI
  xor EBX,[ESP + 280]
  add EAX,EBX
  add EAX,[ESP + 128]
  add EAX,$27B70A85

  add [ESP + 268],EAX
  mov [ESP + 284],EAX

  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 264]
  mov EBX,EBP
  xor EBX,[ESP + 260]
  and EBX,ESI
  and EBP,[ESP + 260]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 284],EAX


  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 280]
  mov EBX,[ESP + 276]
  xor EBX,[ESP + 272]
  and EBX,ESI
  xor EBX,[ESP + 276]
  add EAX,EBX
  add EAX,[ESP + 132]
  add EAX,$2E1B2138

  add [ESP + 264],EAX
  mov [ESP + 280],EAX

  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 260]
  mov EBX,EBP
  xor EBX,[ESP + 256]
  and EBX,ESI
  and EBP,[ESP + 256]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 280],EAX


  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 276]
  mov EBX,[ESP + 272]
  xor EBX,[ESP + 268]
  and EBX,ESI
  xor EBX,[ESP + 272]
  add EAX,EBX
  add EAX,[ESP + 136]
  add EAX,$4D2C6DFC

  add [ESP + 260],EAX
  mov [ESP + 276],EAX

  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 256]
  mov EBX,EBP
  xor EBX,[ESP + 284]
  and EBX,ESI
  and EBP,[ESP + 284]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 276],EAX


  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 272]
  mov EBX,[ESP + 268]
  xor EBX,[ESP + 264]
  and EBX,ESI
  xor EBX,[ESP + 268]
  add EAX,EBX
  add EAX,[ESP + 140]
  add EAX,$53380D13

  add [ESP + 256],EAX
  mov [ESP + 272],EAX

  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 284]
  mov EBX,EBP
  xor EBX,[ESP + 280]
  and EBX,ESI
  and EBP,[ESP + 280]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 272],EAX


  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 268]
  mov EBX,[ESP + 264]
  xor EBX,[ESP + 260]
  and EBX,ESI
  xor EBX,[ESP + 264]
  add EAX,EBX
  add EAX,[ESP + 144]
  add EAX,$650A7354

  add [ESP + 284],EAX
  mov [ESP + 268],EAX

  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 280]
  mov EBX,EBP
  xor EBX,[ESP + 276]
  and EBX,ESI
  and EBP,[ESP + 276]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 268],EAX


  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 264]
  mov EBX,[ESP + 260]
  xor EBX,[ESP + 256]
  and EBX,ESI
  xor EBX,[ESP + 260]
  add EAX,EBX
  add EAX,[ESP + 148]
  add EAX,$766A0ABB

  add [ESP + 280],EAX
  mov [ESP + 264],EAX

  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 276]
  mov EBX,EBP
  xor EBX,[ESP + 272]
  and EBX,ESI
  and EBP,[ESP + 272]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 264],EAX


  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 260]
  mov EBX,[ESP + 256]
  xor EBX,[ESP + 284]
  and EBX,ESI
  xor EBX,[ESP + 256]
  add EAX,EBX
  add EAX,[ESP + 152]
  add EAX,$81C2C92E

  add [ESP + 276],EAX
  mov [ESP + 260],EAX

  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 272]
  mov EBX,EBP
  xor EBX,[ESP + 268]
  and EBX,ESI
  and EBP,[ESP + 268]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 260],EAX


  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 256]
  mov EBX,[ESP + 284]
  xor EBX,[ESP + 280]
  and EBX,ESI
  xor EBX,[ESP + 284]
  add EAX,EBX
  add EAX,[ESP + 156]
  add EAX,$92722C85

  add [ESP + 272],EAX
  mov [ESP + 256],EAX

  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 268]
  mov EBX,EBP
  xor EBX,[ESP + 264]
  and EBX,ESI
  and EBP,[ESP + 264]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 256],EAX


  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 284]
  mov EBX,[ESP + 280]
  xor EBX,[ESP + 276]
  and EBX,ESI
  xor EBX,[ESP + 280]
  add EAX,EBX
  add EAX,[ESP + 160]
  add EAX,$A2BFE8A1

  add [ESP + 268],EAX
  mov [ESP + 284],EAX

  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 264]
  mov EBX,EBP
  xor EBX,[ESP + 260]
  and EBX,ESI
  and EBP,[ESP + 260]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 284],EAX


  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 280]
  mov EBX,[ESP + 276]
  xor EBX,[ESP + 272]
  and EBX,ESI
  xor EBX,[ESP + 276]
  add EAX,EBX
  add EAX,[ESP + 164]
  add EAX,$A81A664B

  add [ESP + 264],EAX
  mov [ESP + 280],EAX

  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 260]
  mov EBX,EBP
  xor EBX,[ESP + 256]
  and EBX,ESI
  and EBP,[ESP + 256]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 280],EAX


  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 276]
  mov EBX,[ESP + 272]
  xor EBX,[ESP + 268]
  and EBX,ESI
  xor EBX,[ESP + 272]
  add EAX,EBX
  add EAX,[ESP + 168]
  add EAX,$C24B8B70

  add [ESP + 260],EAX
  mov [ESP + 276],EAX

  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 256]
  mov EBX,EBP
  xor EBX,[ESP + 284]
  and EBX,ESI
  and EBP,[ESP + 284]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 276],EAX


  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 272]
  mov EBX,[ESP + 268]
  xor EBX,[ESP + 264]
  and EBX,ESI
  xor EBX,[ESP + 268]
  add EAX,EBX
  add EAX,[ESP + 172]
  add EAX,$C76C51A3

  add [ESP + 256],EAX
  mov [ESP + 272],EAX

  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 284]
  mov EBX,EBP
  xor EBX,[ESP + 280]
  and EBX,ESI
  and EBP,[ESP + 280]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 272],EAX


  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 268]
  mov EBX,[ESP + 264]
  xor EBX,[ESP + 260]
  and EBX,ESI
  xor EBX,[ESP + 264]
  add EAX,EBX
  add EAX,[ESP + 176]
  add EAX,$D192E819

  add [ESP + 284],EAX
  mov [ESP + 268],EAX

  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 280]
  mov EBX,EBP
  xor EBX,[ESP + 276]
  and EBX,ESI
  and EBP,[ESP + 276]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 268],EAX


  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 264]
  mov EBX,[ESP + 260]
  xor EBX,[ESP + 256]
  and EBX,ESI
  xor EBX,[ESP + 260]
  add EAX,EBX
  add EAX,[ESP + 180]
  add EAX,$D6990624

  add [ESP + 280],EAX
  mov [ESP + 264],EAX

  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 276]
  mov EBX,EBP
  xor EBX,[ESP + 272]
  and EBX,ESI
  and EBP,[ESP + 272]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 264],EAX


  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 260]
  mov EBX,[ESP + 256]
  xor EBX,[ESP + 284]
  and EBX,ESI
  xor EBX,[ESP + 256]
  add EAX,EBX
  add EAX,[ESP + 184]
  add EAX,$F40E3585

  add [ESP + 276],EAX
  mov [ESP + 260],EAX

  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 272]
  mov EBX,EBP
  xor EBX,[ESP + 268]
  and EBX,ESI
  and EBP,[ESP + 268]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 260],EAX


  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 256]
  mov EBX,[ESP + 284]
  xor EBX,[ESP + 280]
  and EBX,ESI
  xor EBX,[ESP + 284]
  add EAX,EBX
  add EAX,[ESP + 188]
  add EAX,$106AA070

  add [ESP + 272],EAX
  mov [ESP + 256],EAX

  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 268]
  mov EBX,EBP
  xor EBX,[ESP + 264]
  and EBX,ESI
  and EBP,[ESP + 264]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 256],EAX


  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 284]
  mov EBX,[ESP + 280]
  xor EBX,[ESP + 276]
  and EBX,ESI
  xor EBX,[ESP + 280]
  add EAX,EBX
  add EAX,[ESP + 192]
  add EAX,$19A4C116

  add [ESP + 268],EAX
  mov [ESP + 284],EAX

  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 264]
  mov EBX,EBP
  xor EBX,[ESP + 260]
  and EBX,ESI
  and EBP,[ESP + 260]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 284],EAX


  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 280]
  mov EBX,[ESP + 276]
  xor EBX,[ESP + 272]
  and EBX,ESI
  xor EBX,[ESP + 276]
  add EAX,EBX
  add EAX,[ESP + 196]
  add EAX,$1E376C08

  add [ESP + 264],EAX
  mov [ESP + 280],EAX

  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 260]
  mov EBX,EBP
  xor EBX,[ESP + 256]
  and EBX,ESI
  and EBP,[ESP + 256]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 280],EAX


  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 276]
  mov EBX,[ESP + 272]
  xor EBX,[ESP + 268]
  and EBX,ESI
  xor EBX,[ESP + 272]
  add EAX,EBX
  add EAX,[ESP + 200]
  add EAX,$2748774C

  add [ESP + 260],EAX
  mov [ESP + 276],EAX

  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 256]
  mov EBX,EBP
  xor EBX,[ESP + 284]
  and EBX,ESI
  and EBP,[ESP + 284]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 276],EAX


  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 272]
  mov EBX,[ESP + 268]
  xor EBX,[ESP + 264]
  and EBX,ESI
  xor EBX,[ESP + 268]
  add EAX,EBX
  add EAX,[ESP + 204]
  add EAX,$34B0BCB5

  add [ESP + 256],EAX
  mov [ESP + 272],EAX

  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 284]
  mov EBX,EBP
  xor EBX,[ESP + 280]
  and EBX,ESI
  and EBP,[ESP + 280]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 272],EAX


  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 268]
  mov EBX,[ESP + 264]
  xor EBX,[ESP + 260]
  and EBX,ESI
  xor EBX,[ESP + 264]
  add EAX,EBX
  add EAX,[ESP + 208]
  add EAX,$391C0CB3

  add [ESP + 284],EAX
  mov [ESP + 268],EAX

  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 280]
  mov EBX,EBP
  xor EBX,[ESP + 276]
  and EBX,ESI
  and EBP,[ESP + 276]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 268],EAX


  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 264]
  mov EBX,[ESP + 260]
  xor EBX,[ESP + 256]
  and EBX,ESI
  xor EBX,[ESP + 260]
  add EAX,EBX
  add EAX,[ESP + 212]
  add EAX,$4ED8AA4A

  add [ESP + 280],EAX
  mov [ESP + 264],EAX

  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 276]
  mov EBX,EBP
  xor EBX,[ESP + 272]
  and EBX,ESI
  and EBP,[ESP + 272]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 264],EAX


  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 260]
  mov EBX,[ESP + 256]
  xor EBX,[ESP + 284]
  and EBX,ESI
  xor EBX,[ESP + 256]
  add EAX,EBX
  add EAX,[ESP + 216]
  add EAX,$5B9CCA4F

  add [ESP + 276],EAX
  mov [ESP + 260],EAX

  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 272]
  mov EBX,EBP
  xor EBX,[ESP + 268]
  and EBX,ESI
  and EBP,[ESP + 268]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 260],EAX


  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 256]
  mov EBX,[ESP + 284]
  xor EBX,[ESP + 280]
  and EBX,ESI
  xor EBX,[ESP + 284]
  add EAX,EBX
  add EAX,[ESP + 220]
  add EAX,$682E6FF3

  add [ESP + 272],EAX
  mov [ESP + 256],EAX

  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 268]
  mov EBX,EBP
  xor EBX,[ESP + 264]
  and EBX,ESI
  and EBP,[ESP + 264]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 256],EAX


  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 284]
  mov EBX,[ESP + 280]
  xor EBX,[ESP + 276]
  and EBX,ESI
  xor EBX,[ESP + 280]
  add EAX,EBX
  add EAX,[ESP + 224]
  add EAX,$748F82EE

  add [ESP + 268],EAX
  mov [ESP + 284],EAX

  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 264]
  mov EBX,EBP
  xor EBX,[ESP + 260]
  and EBX,ESI
  and EBP,[ESP + 260]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 284],EAX


  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 280]
  mov EBX,[ESP + 276]
  xor EBX,[ESP + 272]
  and EBX,ESI
  xor EBX,[ESP + 276]
  add EAX,EBX
  add EAX,[ESP + 228]
  add EAX,$78A5636F

  add [ESP + 264],EAX
  mov [ESP + 280],EAX

  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 260]
  mov EBX,EBP
  xor EBX,[ESP + 256]
  and EBX,ESI
  and EBP,[ESP + 256]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 280],EAX


  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 276]
  mov EBX,[ESP + 272]
  xor EBX,[ESP + 268]
  and EBX,ESI
  xor EBX,[ESP + 272]
  add EAX,EBX
  add EAX,[ESP + 232]
  add EAX,$84C87814

  add [ESP + 260],EAX
  mov [ESP + 276],EAX

  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 256]
  mov EBX,EBP
  xor EBX,[ESP + 284]
  and EBX,ESI
  and EBP,[ESP + 284]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 276],EAX


  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 272]
  mov EBX,[ESP + 268]
  xor EBX,[ESP + 264]
  and EBX,ESI
  xor EBX,[ESP + 268]
  add EAX,EBX
  add EAX,[ESP + 236]
  add EAX,$8CC70208

  add [ESP + 256],EAX
  mov [ESP + 272],EAX

  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 284]
  mov EBX,EBP
  xor EBX,[ESP + 280]
  and EBX,ESI
  and EBP,[ESP + 280]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 272],EAX


  mov ESI,[ESP + 256]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 268]
  mov EBX,[ESP + 264]
  xor EBX,[ESP + 260]
  and EBX,ESI
  xor EBX,[ESP + 264]
  add EAX,EBX
  add EAX,[ESP + 240]
  add EAX,$90BEFFFA

  add [ESP + 284],EAX
  mov [ESP + 268],EAX

  mov ESI,[ESP + 272]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 280]
  mov EBX,EBP
  xor EBX,[ESP + 276]
  and EBX,ESI
  and EBP,[ESP + 276]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 268],EAX


  mov ESI,[ESP + 284]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 264]
  mov EBX,[ESP + 260]
  xor EBX,[ESP + 256]
  and EBX,ESI
  xor EBX,[ESP + 260]
  add EAX,EBX
  add EAX,[ESP + 244]
  add EAX,$A4506CEB

  add [ESP + 280],EAX
  mov [ESP + 264],EAX

  mov ESI,[ESP + 268]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 276]
  mov EBX,EBP
  xor EBX,[ESP + 272]
  and EBX,ESI
  and EBP,[ESP + 272]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 264],EAX


  mov ESI,[ESP + 280]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 260]
  mov EBX,[ESP + 256]
  xor EBX,[ESP + 284]
  and EBX,ESI
  xor EBX,[ESP + 256]
  add EAX,EBX
  add EAX,[ESP + 248]
  add EAX,$BEF9A3F7

  add [ESP + 276],EAX
  mov [ESP + 260],EAX

  mov ESI,[ESP + 264]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 272]
  mov EBX,EBP
  xor EBX,[ESP + 268]
  and EBX,ESI
  and EBP,[ESP + 268]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 260],EAX


  mov ESI,[ESP + 276]
  mov EDX,ESI
  ror EDX,6
  mov EAX,EDX
  ror EDX,5
  xor EAX,EDX
  ror EDX,14
  xor EAX,EDX
  add EAX,[ESP + 256]
  mov EBX,[ESP + 284]
  xor EBX,[ESP + 280]
  and EBX,ESI
  xor EBX,[ESP + 284]
  add EAX,EBX
  add EAX,[ESP + 252]
  add EAX,$C67178F2

  add [ESP + 272],EAX
  mov [ESP + 256],EAX

  mov ESI,[ESP + 260]
  mov EDX,ESI
  ror EDX,2
  mov EAX,EDX
  ror EDX,11
  xor EAX,EDX
  ror EDX,9
  xor EAX,EDX
  mov EBP,[ESP + 268]
  mov EBX,EBP
  xor EBX,[ESP + 264]
  and EBX,ESI
  and EBP,[ESP + 264]
  xor EBX,EBP
  add EAX,EBX

  add [ESP + 256],EAX


