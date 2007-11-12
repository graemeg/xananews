//  Inc(A, Buffer[ 5] + $FFFA3942 + (B xor C xor D)); A := A rol  4 + B;
  mov ESI,[EBP + 20]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea EAX,[EAX + EDI + $FFFA3942]
  rol EAX,4
  add EAX,EBX
//  Inc(D, Buffer[ 8] + $8771F681 + (A xor B xor C)); D := D rol 11 + A;
  mov ESI,[EBP + 32]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea EDX,[EDX + EDI + $8771F681]
  rol EDX,11
  add EDX,EAX
//  Inc(C, Buffer[11] + $6D9D6122 + (D xor A xor B)); C := C rol 16 + D;
  mov ESI,[EBP + 44]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea ECX,[ECX + EDI + $6D9D6122]
  rol ECX,16
  add ECX,EDX
//  Inc(B, Buffer[14] + $FDE5380C + (C xor D xor A)); B := B rol 23 + C;
  mov ESI,[EBP + 56]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea EBX,[EBX + EDI + $FDE5380C]
  rol EBX,23
  add EBX,ECX
//  Inc(A, Buffer[ 1] + $A4BEEA44 + (B xor C xor D)); A := A rol  4 + B;
  mov ESI,[EBP + 4]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea EAX,[EAX + EDI + $A4BEEA44]
  rol EAX,4
  add EAX,EBX
//  Inc(D, Buffer[ 4] + $4BDECFA9 + (A xor B xor C)); D := D rol 11 + A;
  mov ESI,[EBP + 16]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea EDX,[EDX + EDI + $4BDECFA9]
  rol EDX,11
  add EDX,EAX
//  Inc(C, Buffer[ 7] + $F6BB4B60 + (D xor A xor B)); C := C rol 16 + D;
  mov ESI,[EBP + 28]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea ECX,[ECX + EDI + $F6BB4B60]
  rol ECX,16
  add ECX,EDX
//  Inc(B, Buffer[10] + $BEBFBC70 + (C xor D xor A)); B := B rol 23 + C;
  mov ESI,[EBP + 40]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea EBX,[EBX + EDI + $BEBFBC70]
  rol EBX,23
  add EBX,ECX
//  Inc(A, Buffer[13] + $289B7EC6 + (B xor C xor D)); A := A rol  4 + B;
  mov ESI,[EBP + 52]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea EAX,[EAX + EDI + $289B7EC6]
  rol EAX,4
  add EAX,EBX
//  Inc(D, Buffer[ 0] + $EAA127FA + (A xor B xor C)); D := D rol 11 + A;
  mov ESI,[EBP]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea EDX,[EDX + EDI + $EAA127FA]
  rol EDX,11
  add EDX,EAX
//  Inc(C, Buffer[ 3] + $D4EF3085 + (D xor A xor B)); C := C rol 16 + D;
  mov ESI,[EBP + 12]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea ECX,[ECX + EDI + $D4EF3085]
  rol ECX,16
  add ECX,EDX
//  Inc(B, Buffer[ 6] + $04881D05 + (C xor D xor A)); B := B rol 23 + C;
  mov ESI,[EBP + 24]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea EBX,[EBX + EDI + $04881D05]
  rol EBX,23
  add EBX,ECX
//  Inc(A, Buffer[ 9] + $D9D4D039 + (B xor C xor D)); A := A rol  4 + B;
  mov ESI,[EBP + 36]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea EAX,[EAX + EDI + $D9D4D039]
  rol EAX,4
  add EAX,EBX
//  Inc(D, Buffer[12] + $E6DB99E5 + (A xor B xor C)); D := D rol 11 + A;
  mov ESI,[EBP + 48]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea EDX,[EDX + EDI + $E6DB99E5]
  rol EDX,11
  add EDX,EAX
//  Inc(C, Buffer[15] + $1FA27CF8 + (D xor A xor B)); C := C rol 16 + D;
  mov ESI,[EBP + 60]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea ECX,[ECX + EDI + $1FA27CF8]
  rol ECX,16
  add ECX,EDX
//  Inc(B, Buffer[ 2] + $C4AC5665 + (C xor D xor A)); B := B rol 23 + C;
  mov ESI,[EBP + 8]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea EBX,[EBX + EDI + $C4AC5665]
  rol EBX,23
  add EBX,ECX

