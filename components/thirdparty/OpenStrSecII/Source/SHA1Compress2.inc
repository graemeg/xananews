// A: EAX
// B: EBX
// C: ECX
// D: EDX
// E: EDI
// W: [ESP]

//  Inc(E, (A rol 5) + (D xor B xor C) + W[20] + $6ED9EBA1); B := B rol 30;
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
  mov ESI,ECX
  xor ESI,EBX
  xor ESI,EDX
  add EDI,[ESP + 80]
  lea EDI,[ESI + EDI + $6ED9EBA1]
  ror EBX,2
//  Inc(D, (E rol 5) + (C xor A xor B) + W[21] + $6ED9EBA1); A := A rol 30;
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
  mov ESI,EBX
  xor ESI,EAX
  xor ESI,ECX
  add EDX,[ESP + 84]
  lea EDX,[ESI + EDX + $6ED9EBA1]
  ror EAX,2
//  Inc(C, (D rol 5) + (B xor E xor A) + W[22] + $6ED9EBA1); E := E rol 30;
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
  mov ESI,EAX
  xor ESI,EDI
  xor ESI,EBX
  add ECX,[ESP + 88]
  lea ECX,[ESI + ECX + $6ED9EBA1]
  ror EDI,2
//  Inc(B, (C rol 5) + (A xor D xor E) + W[23] + $6ED9EBA1); D := D rol 30;
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
  mov ESI,EDI
  xor ESI,EDX
  xor ESI,EAX
  add EBX,[ESP + 92]
  lea EBX,[ESI + EBX + $6ED9EBA1]
  ror EDX,2
//  Inc(A, (B rol 5) + (E xor C xor D) + W[24] + $6ED9EBA1); C := C rol 30;
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI
  mov ESI,EDX
  xor ESI,ECX
  xor ESI,EDI
  add EAX,[ESP + 96]
  lea EAX,[ESI + EAX + $6ED9EBA1]
  ror ECX,2
//  Inc(E, (A rol 5) + (D xor B xor C) + W[25] + $6ED9EBA1); B := B rol 30;
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
  mov ESI,ECX
  xor ESI,EBX
  xor ESI,EDX
  add EDI,[ESP + 100]
  lea EDI,[ESI + EDI + $6ED9EBA1]
  ror EBX,2
//  Inc(D, (E rol 5) + (C xor A xor B) + W[26] + $6ED9EBA1); A := A rol 30;
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
  mov ESI,EBX
  xor ESI,EAX
  xor ESI,ECX
  add EDX,[ESP + 104]
  lea EDX,[ESI + EDX + $6ED9EBA1]
  ror EAX,2
//  Inc(C, (D rol 5) + (B xor E xor A) + W[27] + $6ED9EBA1); E := E rol 30;
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
  mov ESI,EAX
  xor ESI,EDI
  xor ESI,EBX
  add ECX,[ESP + 108]
  lea ECX,[ESI + ECX + $6ED9EBA1]
  ror EDI,2
//  Inc(B, (C rol 5) + (A xor D xor E) + W[28] + $6ED9EBA1); D := D rol 30;
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
  mov ESI,EDI
  xor ESI,EDX
  xor ESI,EAX
  add EBX,[ESP + 112]
  lea EBX,[ESI + EBX + $6ED9EBA1]
  ror EDX,2
//  Inc(A, (B rol 5) + (E xor C xor D) + W[29] + $6ED9EBA1); C := C rol 30;
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI
  mov ESI,EDX
  xor ESI,ECX
  xor ESI,EDI
  add EAX,[ESP + 116]
  lea EAX,[ESI + EAX + $6ED9EBA1]
  ror ECX,2
//  Inc(E, (A rol 5) + (D xor B xor C) + W[30] + $6ED9EBA1); B := B rol 30;
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
  mov ESI,ECX
  xor ESI,EBX
  xor ESI,EDX
  add EDI,[ESP + 120]
  lea EDI,[ESI + EDI + $6ED9EBA1]
  ror EBX,2
//  Inc(D, (E rol 5) + (C xor A xor B) + W[31] + $6ED9EBA1); A := A rol 30;
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
  mov ESI,EBX
  xor ESI,EAX
  xor ESI,ECX
  add EDX,[ESP + 124]
  lea EDX,[ESI + EDX + $6ED9EBA1]
  ror EAX,2
//  Inc(C, (D rol 5) + (B xor E xor A) + W[32] + $6ED9EBA1); E := E rol 30;
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
  mov ESI,EAX
  xor ESI,EDI
  xor ESI,EBX
  add ECX,[ESP + 128]
  lea ECX,[ESI + ECX + $6ED9EBA1]
  ror EDI,2
//  Inc(B, (C rol 5) + (A xor D xor E) + W[33] + $6ED9EBA1); D := D rol 30;
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
  mov ESI,EDI
  xor ESI,EDX
  xor ESI,EAX
  add EBX,[ESP + 132]
  lea EBX,[ESI + EBX + $6ED9EBA1]
  ror EDX,2
//  Inc(A, (B rol 5) + (E xor C xor D) + W[34] + $6ED9EBA1); C := C rol 30;
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI
  mov ESI,EDX
  xor ESI,ECX
  xor ESI,EDI
  add EAX,[ESP + 136]
  lea EAX,[ESI + EAX + $6ED9EBA1]
  ror ECX,2
//  Inc(E, (A rol 5) + (D xor B xor C) + W[35] + $6ED9EBA1); B := B rol 30;
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
  mov ESI,ECX
  xor ESI,EBX
  xor ESI,EDX
  add EDI,[ESP + 140]
  lea EDI,[ESI + EDI + $6ED9EBA1]
  ror EBX,2
//  Inc(D, (E rol 5) + (C xor A xor B) + W[36] + $6ED9EBA1); A := A rol 30;
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
  mov ESI,EBX
  xor ESI,EAX
  xor ESI,ECX
  add EDX,[ESP + 144]
  lea EDX,[ESI + EDX + $6ED9EBA1]
  ror EAX,2
//  Inc(C, (D rol 5) + (B xor E xor A) + W[37] + $6ED9EBA1); E := E rol 30;
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
  mov ESI,EAX
  xor ESI,EDI
  xor ESI,EBX
  add ECX,[ESP + 148]
  lea ECX,[ESI + ECX + $6ED9EBA1]
  ror EDI,2
//  Inc(B, (C rol 5) + (A xor D xor E) + W[38] + $6ED9EBA1); D := D rol 30;
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
  mov ESI,EDI
  xor ESI,EDX
  xor ESI,EAX
  add EBX,[ESP + 152]
  lea EBX,[ESI + EBX + $6ED9EBA1]
  ror EDX,2
//  Inc(A, (B rol 5) + (E xor C xor D) + W[39] + $6ED9EBA1); C := C rol 30;
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI
  mov ESI,EDX
  xor ESI,ECX
  xor ESI,EDI
  add EAX,[ESP + 156]
  lea EAX,[ESI + EAX + $6ED9EBA1]
  ror ECX,2

