{ The F function is:

   (B and C) or ((not B) and D)
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

   (((D xor C) and B) xor D)
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

//  Inc(A, Buffer[ 0] + $D76AA478 + (D xor (B and (C xor D)))); A := A rol  7 + B;
  mov ESI,[EBP]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea EAX,[EAX + EDI + $D76AA478]
  ror EAX,25
  add EAX,EBX
//  Inc(D, Buffer[ 1] + $E8C7B756 + (C xor (A and (B xor C)))); D := D rol 12 + A;
  mov ESI,[EBP + 4]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea EDX,[EDX + EDI + $E8C7B756]
  ror EDX,20
  add EDX,EAX
//  Inc(C, Buffer[ 2] + $242070DB + (B xor (D and (A xor B)))); C := C rol 17 + D;
  mov ESI,[EBP + 8]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea ECX,[ECX + EDI + $242070DB]
  ror ECX,15
  add ECX,EDX
//  Inc(B, Buffer[ 3] + $C1BDCEEE + (A xor (C and (D xor A)))); B := B rol 22 + C;
  mov ESI,[EBP + 12]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea EBX,[EBX + EDI + $C1BDCEEE]
  ror EBX,10
  add EBX,ECX
//  Inc(A, Buffer[ 4] + $F57C0FAF + (D xor (B and (C xor D)))); A := A rol  7 + B;
  mov ESI,[EBP + 16]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea EAX,[EAX + EDI + $F57C0FAF]
  ror EAX,25
  add EAX,EBX
//  Inc(D, Buffer[ 5] + $4787C62A + (C xor (A and (B xor C)))); D := D rol 12 + A;
  mov ESI,[EBP + 20]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea EDX,[EDX + EDI + $4787C62A]
  ror EDX,20
  add EDX,EAX
//  Inc(C, Buffer[ 6] + $A8304613 + (B xor (D and (A xor B)))); C := C rol 17 + D;
  mov ESI,[EBP + 24]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea ECX,[ECX + EDI + $A8304613]
  ror ECX,15
  add ECX,EDX
//  Inc(B, Buffer[ 7] + $FD469501 + (A xor (C and (D xor A)))); B := B rol 22 + C;
  mov ESI,[EBP + 28]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea EBX,[EBX + EDI + $FD469501]
  ror EBX,10
  add EBX,ECX
//  Inc(A, Buffer[ 8] + $698098D8 + (D xor (B and (C xor D)))); A := A rol  7 + B;
  mov ESI,[EBP + 32]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea EAX,[EDI + EAX + $698098D8]
  ror EAX,25
  add EAX,EBX
//  Inc(D, Buffer[ 9] + $8B44F7AF + (C xor (A and (B xor C)))); D := D rol 12 + A;
  mov ESI,[EBP + 36]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea EDX,[EDI + EDX + $8B44F7AF]
  ror EDX,20
  add EDX,EAX
//  Inc(C, Buffer[10] + $FFFF5BB1 + (B xor (D and (A xor B)))); C := C rol 17 + D;
  mov ESI,[EBP + 40]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea ECX,[EDI + ECX + $FFFF5BB1]
  ror ECX,15
  add ECX,EDX
//  Inc(B, Buffer[11] + $895CD7BE + (A xor (C and (D xor A)))); B := B rol 22 + C;
  mov ESI,[EBP + 44]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea EBX,[EDI + EBX + $895CD7BE]
  ror EBX,10
  add EBX,ECX
//  Inc(A, Buffer[12] + $6B901122 + (D xor (B and (C xor D)))); A := A rol  7 + B;
  mov ESI,[EBP + 48]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea EAX,[EDI + EAX + $6B901122]
  ror EAX,25
  add EAX,EBX
//  Inc(D, Buffer[13] + $FD987193 + (C xor (A and (B xor C)))); D := D rol 12 + A;
  mov ESI,[EBP + 52]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea EDX,[EDI + EDX + $FD987193]
  ror EDX,20
  add EDX,EAX
//  Inc(C, Buffer[14] + $A679438E + (B xor (D and (A xor B)))); C := C rol 17 + D;
  mov ESI,[EBP + 56]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea ECX,[EDI + ECX + $A679438E]
  ror ECX,15
  add ECX,EDX
//  Inc(B, Buffer[15] + $49B40821 + (A xor (C and (D xor A)))); B := B rol 22 + C;
  mov ESI,[EBP + 60]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea EBX,[EDI + EBX + $49B40821]
  ror EBX,10
  add EBX,ECX

