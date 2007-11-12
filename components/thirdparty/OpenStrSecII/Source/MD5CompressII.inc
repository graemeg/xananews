//  Inc(A, Buffer[ 0] + $F4292244 + (C xor (B or not D))); A := A rol  6 + B;
  mov ESI,[EBP]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea EAX,[EAX + EDI + $F4292244]
  rol EAX,6
  add EAX,EBX
//  Inc(D, Buffer[ 7] + $432AFF97 + (B xor (A or not C))); D := D rol 10 + A;
  mov ESI,[EBP + 28]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea EDX,[EDX + EDI + $432AFF97]
  rol EDX,10
  add EDX,EAX
//  Inc(C, Buffer[14] + $AB9423A7 + (A xor (D or not B))); C := C rol 15 + D;
  mov ESI,[EBP + 56]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea ECX,[ECX + EDI + $AB9423A7]
  rol ECX,15
  add ECX,EDX
//  Inc(B, Buffer[ 5] + $FC93A039 + (D xor (C or not A))); B := B rol 21 + C;
  mov ESI,[EBP + 20]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea EBX,[EBX + EDI + $FC93A039]
  rol EBX,21
  add EBX,ECX
//  Inc(A, Buffer[12] + $655B59C3 + (C xor (B or not D))); A := A rol  6 + B;
  mov ESI,[EBP + 48]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea EAX,[EAX + EDI + $655B59C3]
  rol EAX,6
  add EAX,EBX
//  Inc(D, Buffer[ 3] + $8F0CCC92 + (B xor (A or not C))); D := D rol 10 + A;
  mov ESI,[EBP + 12]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea EDX,[EDX + EDI + $8F0CCC92]
  rol EDX,10
  add EDX,EAX
//  Inc(C, Buffer[10] + $FFEFF47D + (A xor (D or not B))); C := C rol 15 + D;
  mov ESI,[EBP + 40]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea ECX,[ECX + EDI + $FFEFF47D]
  rol ECX,15
  add ECX,EDX
//  Inc(B, Buffer[ 1] + $85845DD1 + (D xor (C or not A))); B := B rol 21 + C;
  mov ESI,[EBP + 4]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea EBX,[EBX + EDI + $85845DD1]
  rol EBX,21
  add EBX,ECX
//  Inc(A, Buffer[ 8] + $6FA87E4F + (C xor (B or not D))); A := A rol  6 + B;
  mov ESI,[EBP + 32]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea EAX,[EAX + EDI + $6FA87E4F]
  rol EAX,6
  add EAX,EBX
//  Inc(D, Buffer[15] + $FE2CE6E0 + (B xor (A or not C))); D := D rol 10 + A;
  mov ESI,[EBP + 60]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea EDX,[EDX + EDI + $FE2CE6E0]
  rol EDX,10
  add EDX,EAX
//  Inc(C, Buffer[ 6] + $A3014314 + (A xor (D or not B))); C := C rol 15 + D;
  mov ESI,[EBP + 24]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea ECX,[ECX + EDI + $A3014314]
  rol ECX,15
  add ECX,EDX
//  Inc(B, Buffer[13] + $4E0811A1 + (D xor (C or not A))); B := B rol 21 + C;
  mov ESI,[EBP + 52]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea EBX,[EBX + EDI + $4E0811A1]
  rol EBX,21
  add EBX,ECX
//  Inc(A, Buffer[ 4] + $F7537E82 + (C xor (B or not D))); A := A rol  6 + B;
  mov ESI,[EBP + 16]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea EAX,[EAX + EDI + $F7537E82]
  rol EAX,6
  add EAX,EBX
//  Inc(D, Buffer[11] + $BD3AF235 + (B xor (A or not C))); D := D rol 10 + A;
  mov ESI,[EBP + 44]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea EDX,[EDX + EDI + $BD3AF235]
  rol EDX,10
  add EDX,EAX
//  Inc(C, Buffer[ 2] + $2AD7D2BB + (A xor (D or not B))); C := C rol 15 + D;
  mov ESI,[EBP + 8]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea ECX,[ECX + EDI + $2AD7D2BB]
  rol ECX,15
  add ECX,EDX
//  Inc(B, Buffer[ 9] + $EB86D391 + (D xor (C or not A))); B := B rol 21 + C;
  mov ESI,[EBP + 36]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea EBX,[EBX + EDI + $EB86D391]
  rol EBX,21
  add EBX,ECX

