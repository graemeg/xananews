{ The G function is:

   (D and B) or ((not D) and C)
Truth table:
    1  1  1 _1_    0  1   0  1
    0  0  1 _1_    1  0   1  1
    1  0  0 _0_    0  1   0  1
    0  0  0 _1_    1  1   1  1
    1  1  1 _1_    0  1   0  0
    0  0  1 _0_    1  0   0  0
    1  0  0 _0_    0  1   0  0
    0  0  0 _0_    1  0   0  0

or equivalently:

   (((C xor B) and D) xor C)
Truth table:
      1  0  1   0  1  _1_ 1
      1  0  1   0  0  _1_ 1
      1  1  0   1  1  _0_ 1
      1  1  0   0  0  _1_ 1
      0  1  1   1  1  _1_ 0
      0  1  1   0  0  _0_ 0
      0  0  0   0  1  _0_ 0
      0  0  0   0  0  _0_ 0
}

//  Inc(A, Buffer[ 1] + $F61E2562 + (C xor (D and (B xor C)))); A := A rol  5 + B;
  mov ESI,[EBP + 4]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea EAX,[EDI + EAX + $F61E2562]
  rol EAX,5
  add EAX,EBX
//  Inc(D, Buffer[ 6] + $C040B340 + (B xor (C and (A xor B)))); D := D rol  9 + A;
  mov ESI,[EBP + 24]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea EDX,[EDI + EDX + $C040B340]
  rol EDX,9
  add EDX,EAX
//  Inc(C, Buffer[11] + $265E5A51 + (A xor (B and (D xor A)))); C := C rol 14 + D;
  mov ESI,[EBP + 44]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea ECX,[EDI + ECX + $265E5A51]
  rol ECX,14
  add ECX,EDX
//  Inc(B, Buffer[ 0] + $E9B6C7AA + (D xor (A and (C xor D)))); B := B rol 20 + C;
  mov ESI,[EBP]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea EBX,[EDI + EBX + $E9B6C7AA]
  rol EBX,20
  add EBX,ECX
//  Inc(A, Buffer[ 5] + $D62F105D + (C xor (D and (B xor C)))); A := A rol  5 + B;
  mov ESI,[EBP + 20]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea EAX,[EDI + EAX + $D62F105D]
  rol EAX,5
  add EAX,EBX
//  Inc(D, Buffer[10] + $02441453 + (B xor (C and (A xor B)))); D := D rol  9 + A;
  mov ESI,[EBP + 40]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea EDX,[EDI + EDX + $02441453]
  rol EDX,9
  add EDX,EAX
//  Inc(C, Buffer[15] + $D8A1E681 + (A xor (B and (D xor A)))); C := C rol 14 + D;
  mov ESI,[EBP + 60]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea ECX,[EDI + ECX + $D8A1E681]
  rol ECX,14
  add ECX,EDX
//  Inc(B, Buffer[ 4] + $E7D3FBC8 + (D xor (A and (C xor D)))); B := B rol 20 + C;
  mov ESI,[EBP + 16]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea EBX,[EDI + EBX + $E7D3FBC8]
  rol EBX,20
  add EBX,ECX
//  Inc(A, Buffer[ 9] + $21E1CDE6 + (C xor (D and (B xor C)))); A := A rol  5 + B;
  mov ESI,[EBP + 36]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea EAX,[EDI + EAX + $21E1CDE6]
  rol EAX,5
  add EAX,EBX
//  Inc(D, Buffer[14] + $C33707D6 + (B xor (C and (A xor B)))); D := D rol  9 + A;
  mov ESI,[EBP + 56]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea EDX,[EDI + EDX + $C33707D6]
  rol EDX,9
  add EDX,EAX
//  Inc(C, Buffer[ 3] + $F4D50D87 + (A xor (B and (D xor A)))); C := C rol 14 + D;
  mov ESI,[EBP + 12]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea ECX,[EDI + ECX + $F4D50D87]
  rol ECX,14
  add ECX,EDX
//  Inc(B, Buffer[ 8] + $455A14ED + (D xor (A and (C xor D)))); B := B rol 20 + C;
  mov ESI,[EBP + 32]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea EBX,[EDI + EBX + $455A14ED]
  rol EBX,20
  add EBX,ECX
//  Inc(A, Buffer[13] + $A9E3E905 + (C xor (D and (B xor C)))); A := A rol  5 + B;
  mov ESI,[EBP + 52]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea EAX,[EDI + EAX + $A9E3E905]
  rol EAX,5
  add EAX,EBX
//  Inc(D, Buffer[ 2] + $FCEFA3F8 + (B xor (C and (A xor B)))); D := D rol  9 + A;
  mov ESI,[EBP + 8]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea EDX,[EDI + EDX + $FCEFA3F8]
  rol EDX,9
  add EDX,EAX
//  Inc(C, Buffer[ 7] + $676F02D9 + (A xor (B and (D xor A)))); C := C rol 14 + D;
  mov ESI,[EBP + 28]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea ECX,[EDI + ECX + $676F02D9]
  rol ECX,14
  add ECX,EDX
//  Inc(B, Buffer[12] + $8D2A4C8A + (D xor (A and (C xor D)))); B := B rol 20 + C;
  mov ESI,[EBP + 48]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea EBX,[EDI + EBX + $8D2A4C8A]
  rol EBX,20
  add EBX,ECX

