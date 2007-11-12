// A: EAX
// B: EBX
// C: ECX
// D: EDX
// E: EDI
// W: [ESP]

//  Inc(E, (A rol 5) + (D xor (B and (C xor D))) + W[ 0] + $5A827999); B := B rol 30;
  mov ESI,EDX
  xor ESI,ECX
  and ESI,EBX
  add EDI,[ESP]
  xor ESI,EDX
  ror EBX,2
  lea EDI,[EDI + ESI + $5A827999]
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
  //  Inc(D, (E rol 5) + (C xor (A and (B xor C))) + W[ 1] + $5A827999); A := A rol 30;
  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  add EDX,[ESP + 4]
  xor ESI,ECX
  ror EAX,2
  lea EDX,[EDX + ESI + $5A827999]                                                      
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
//  Inc(C, (D rol 5) + (B xor (E and (A xor B))) + W[ 2] + $5A827999); E := E rol 30;
  mov ESI,EBX
  xor ESI,EAX
  and ESI,EDI
  add ECX,[ESP + 8]
  xor ESI,EBX
  ror EDI,2
  lea ECX,[ECX + ESI + $5A827999]                                                    
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
//  Inc(B, (C rol 5) + (A xor (D and (E xor A))) + W[ 3] + $5A827999); D := D rol 30;
  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  add EBX,[ESP + 12]
  xor ESI,EAX
  ror EDX,2
  lea EBX,[EBX + ESI + $5A827999]                                                    
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
//  Inc(A, (B rol 5) + (E xor (C and (D xor E))) + W[ 4] + $5A827999); C := C rol 30;
  mov ESI,EDI
  xor ESI,EDX
  and ESI,ECX
  add EAX,[ESP + 16]
  xor ESI,EDI
  ror ECX,2
  lea EAX,[EAX + ESI + $5A827999]                                                    
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI
//  Inc(E, (A rol 5) + (D xor (B and (C xor D))) + W[ 5] + $5A827999); B := B rol 30;
  mov ESI,EDX
  xor ESI,ECX
  and ESI,EBX
  add EDI,[ESP + 20]
  xor ESI,EDX
  ror EBX,2
  lea EDI,[EDI + ESI + $5A827999]                                                    
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
//  Inc(D, (E rol 5) + (C xor (A and (B xor C))) + W[ 6] + $5A827999); A := A rol 30;
  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  add EDX,[ESP + 24]
  xor ESI,ECX
  ror EAX,2
  lea EDX,[ESI + EDX + $5A827999]                                                    
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
//  Inc(C, (D rol 5) + (B xor (E and (A xor B))) + W[ 7] + $5A827999); E := E rol 30;
  mov ESI,EBX
  xor ESI,EAX
  and ESI,EDI
  add ECX,[ESP + 28]
  xor ESI,EBX
  ror EDI,2
  lea ECX,[ESI + ECX + $5A827999]                                                    
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
//  Inc(B, (C rol 5) + (A xor (D and (E xor A))) + W[ 8] + $5A827999); D := D rol 30;
  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  add EBX,[ESP + 32]
  xor ESI,EAX
  ror EDX,2
  lea EBX,[ESI + EBX + $5A827999]                                                    
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
//  Inc(A, (B rol 5) + (E xor (C and (D xor E))) + W[ 9] + $5A827999); C := C rol 30;
  mov ESI,EDI
  xor ESI,EDX
  and ESI,ECX
  add EAX,[ESP + 36]
  xor ESI,EDI
  ror ECX,2
  lea EAX,[ESI + EAX + $5A827999]                                                    
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI
//  Inc(E, (A rol 5) + (D xor (B and (C xor D))) + W[10] + $5A827999); B := B rol 30;
  mov ESI,EDX
  xor ESI,ECX
  and ESI,EBX
  add EDI,[ESP + 40]
  xor ESI,EDX
  ror EBX,2
  lea EDI,[ESI + EDI + $5A827999]                                                    
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
//  Inc(D, (E rol 5) + (C xor (A and (B xor C))) + W[11] + $5A827999); A := A rol 30;
  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  add EDX,[ESP + 44]
  xor ESI,ECX
  ror EAX,2
  lea EDX,[ESI + EDX + $5A827999]                                                    
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
//  Inc(C, (D rol 5) + (B xor (E and (A xor B))) + W[12] + $5A827999); E := E rol 30;
  mov ESI,EBX
  xor ESI,EAX
  and ESI,EDI
  add ECX,[ESP + 48]
  xor ESI,EBX
  ror EDI,2
  lea ECX,[ESI + ECX + $5A827999]                                                    
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
//  Inc(B, (C rol 5) + (A xor (D and (E xor A))) + W[13] + $5A827999); D := D rol 30;
  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  add EBX,[ESP + 52]
  xor ESI,EAX
  ror EDX,2
  lea EBX,[ESI + EBX + $5A827999]                                                    
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
//  Inc(A, (B rol 5) + (E xor (C and (D xor E))) + W[14] + $5A827999); C := C rol 30;
  mov ESI,EDI
  xor ESI,EDX
  and ESI,ECX
  add EAX,[ESP + 56]
  xor ESI,EDI
  ror ECX,2
  lea EAX,[ESI + EAX + $5A827999]                                                    
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI
//  Inc(E, (A rol 5) + (D xor (B and (C xor D))) + W[15] + $5A827999); B := B rol 30;
  mov ESI,EDX
  xor ESI,ECX
  and ESI,EBX
  add EDI,[ESP + 60]
  xor ESI,EDX
  ror EBX,2
  lea EDI,[ESI + EDI + $5A827999]                                                    
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
//  Inc(D, (E rol 5) + (C xor (A and (B xor C))) + W[16] + $5A827999); A := A rol 30;
  mov ESI,ECX
  xor ESI,EBX
  and ESI,EAX
  add EDX,[ESP + 64]
  xor ESI,ECX
  ror EAX,2
  lea EDX,[ESI + EDX + $5A827999]                                                    
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
//  Inc(C, (D rol 5) + (B xor (E and (A xor B))) + W[17] + $5A827999); E := E rol 30;
  mov ESI,EBX
  xor ESI,EAX
  and ESI,EDI
  add ECX,[ESP + 68]
  xor ESI,EBX
  ror EDI,2
  lea ECX,[ESI + ECX + $5A827999]                                                    
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
//  Inc(B, (C rol 5) + (A xor (D and (E xor A))) + W[18] + $5A827999); D := D rol 30;
  mov ESI,EAX
  xor ESI,EDI
  and ESI,EDX
  add EBX,[ESP + 72]
  xor ESI,EAX
  ror EDX,2
  lea EBX,[ESI + EBX + $5A827999]                                                    
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
//  Inc(A, (B rol 5) + (E xor (C and (D xor E))) + W[19] + $5A827999); C := C rol 30;
  mov ESI,EDI
  xor ESI,EDX
  and ESI,ECX
  add EAX,[ESP + 76]
  xor ESI,EDI
  ror ECX,2
  lea EAX,[ESI + EAX + $5A827999]                                                    
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI

