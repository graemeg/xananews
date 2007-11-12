// A: EAX
// B: EBX
// C: ECX
// D: EDX
// E: EDI
// W: [ESP]

//  Inc(E, (A rol 5) + (D xor B xor C) + W[60] + $CA62C1D6); B := B rol 30;
  mov ESI,EAX
  rol ESI,5
  mov EBP,ECX
  add EDI,ESI
  xor EBP,EBX
  add EDI,[ESP + 240]
  xor EBP,EDX
  ror EBX,2
  lea EDI,[EBP + EDI - $359D3E2A]
//  Inc(D, (E rol 5) + (C xor A xor B) + W[61] + $CA62C1D6); A := A rol 30;
  mov ESI,EDI
  rol ESI,5
  mov EBP,EBX
  add EDX,ESI
  xor EBP,EAX
  add EDX,[ESP + 244]
  xor EBP,ECX
  ror EAX,2
  lea EDX,[EBP + EDX - $359D3E2A]
//  Inc(C, (D rol 5) + (B xor E xor A) + W[62] + $CA62C1D6); E := E rol 30;
  mov ESI,EDX
  rol ESI,5
  mov EBP,EAX
  add ECX,ESI
  xor EBP,EDI
  add ECX,[ESP + 248]
  xor EBP,EBX
  ror EDI,2
  lea ECX,[EBP + ECX - $359D3E2A]
//  Inc(B, (C rol 5) + (A xor D xor E) + W[63] + $CA62C1D6); D := D rol 30;
  mov ESI,ECX
  rol ESI,5
  mov EBP,EDI
  add EBX,ESI
  xor EBP,EDX
  add EBX,[ESP + 252]
  xor EBP,EAX
  ror EDX,2
  lea EBX,[EBP + EBX - $359D3E2A]
//  Inc(A, (B rol 5) + (E xor C xor D) + W[64] + $CA62C1D6); C := C rol 30;
  mov ESI,EBX
  rol ESI,5
  mov EBP,EDX
  add EAX,ESI
  xor EBP,ECX
  add EAX,[ESP + 256]
  xor EBP,EDI
  ror ECX,2
  lea EAX,[EBP + EAX - $359D3E2A]
//  Inc(E, (A rol 5) + (D xor B xor C) + W[65] + $CA62C1D6); B := B rol 30;
  mov ESI,EAX
  rol ESI,5
  mov EBP,ECX
  add EDI,ESI
  xor EBP,EBX
  add EDI,[ESP + 260]
  xor EBP,EDX
  ror EBX,2
  lea EDI,[EBP + EDI - $359D3E2A]
//  Inc(D, (E rol 5) + (C xor A xor B) + W[66] + $CA62C1D6); A := A rol 30;
  mov ESI,EDI
  rol ESI,5
  mov EBP,EBX
  add EDX,ESI
  xor EBP,EAX
  add EDX,[ESP + 264]
  xor EBP,ECX
  ror EAX,2
  lea EDX,[EBP + EDX - $359D3E2A]
//  Inc(C, (D rol 5) + (B xor E xor A) + W[67] + $CA62C1D6); E := E rol 30;
  mov ESI,EDX
  rol ESI,5
  mov EBP,EAX
  add ECX,ESI
  xor EBP,EDI
  add ECX,[ESP + 268]
  xor EBP,EBX
  ror EDI,2
  lea ECX,[EBP + ECX - $359D3E2A]
//  Inc(B, (C rol 5) + (A xor D xor E) + W[68] + $CA62C1D6); D := D rol 30;
  mov ESI,ECX
  rol ESI,5
  mov EBP,EDI
  add EBX,ESI
  xor EBP,EDX
  add EBX,[ESP + 272]
  xor EBP,EAX
  ror EDX,2
  lea EBX,[EBP + EBX - $359D3E2A]
//  Inc(A, (B rol 5) + (E xor C xor D) + W[69] + $CA62C1D6); C := C rol 30;
  mov ESI,EBX
  rol ESI,5
  mov EBP,EDX
  add EAX,ESI
  xor EBP,ECX
  add EAX,[ESP + 276]
  xor EBP,EDI
  ror ECX,2
  lea EAX,[EBP + EAX - $359D3E2A]
//  Inc(E, (A rol 5) + (D xor B xor C) + W[70] + $CA62C1D6); B := B rol 30;
  mov ESI,EAX
  rol ESI,5
  mov EBP,ECX
  add EDI,ESI
  xor EBP,EBX
  add EDI,[ESP + 280]
  xor EBP,EDX
  ror EBX,2
  lea EDI,[EBP + EDI - $359D3E2A]
//  Inc(D, (E rol 5) + (C xor A xor B) + W[71] + $CA62C1D6); A := A rol 30;
  mov ESI,EDI
  rol ESI,5
  mov EBP,EBX
  add EDX,ESI
  xor EBP,EAX
  add EDX,[ESP + 284]
  xor EBP,ECX
  ror EAX,2
  lea EDX,[EBP + EDX - $359D3E2A]
//  Inc(C, (D rol 5) + (B xor E xor A) + W[72] + $CA62C1D6); E := E rol 30;
  mov ESI,EDX
  rol ESI,5
  mov EBP,EAX
  add ECX,ESI
  xor EBP,EDI
  add ECX,[ESP + 288]
  xor EBP,EBX
  ror EDI,2
  lea ECX,[EBP + ECX - $359D3E2A]
//  Inc(B, (C rol 5) + (A xor D xor E) + W[73] + $CA62C1D6); D := D rol 30;
  mov ESI,ECX
  rol ESI,5
  mov EBP,EDI
  add EBX,ESI
  xor EBP,EDX
  add EBX,[ESP + 292]
  xor EBP,EAX
  ror EDX,2
  lea EBX,[EBP + EBX - $359D3E2A]
//  Inc(A, (B rol 5) + (E xor C xor D) + W[74] + $CA62C1D6); C := C rol 30;
  mov ESI,EBX
  rol ESI,5
  mov EBP,EDX
  add EAX,ESI
  xor EBP,ECX
  add EAX,[ESP + 296]
  xor EBP,EDI
  ror ECX,2
  lea EAX,[EBP + EAX - $359D3E2A]
//  Inc(E, (A rol 5) + (D xor B xor C) + W[75] + $CA62C1D6); B := B rol 30;
  mov ESI,EAX
  rol ESI,5
  mov EBP,ECX
  add EDI,ESI
  xor EBP,EBX
  add EDI,[ESP + 300]
  xor EBP,EDX
  ror EBX,2
  lea EDI,[EBP + EDI - $359D3E2A]
//  Inc(D, (E rol 5) + (C xor A xor B) + W[76] + $CA62C1D6); A := A rol 30;
  mov ESI,EDI
  rol ESI,5
  mov EBP,EBX
  add EDX,ESI
  xor EBP,EAX
  add EDX,[ESP + 304]
  xor EBP,ECX
  ror EAX,2
  lea EDX,[EBP + EDX - $359D3E2A]
//  Inc(C, (D rol 5) + (B xor E xor A) + W[77] + $CA62C1D6); E := E rol 30;
  mov ESI,EDX
  rol ESI,5
  mov EBP,EAX
  add ECX,ESI
  xor EBP,EDI
  add ECX,[ESP + 308]
  xor EBP,EBX
  ror EDI,2
  lea ECX,[EBP + ECX - $359D3E2A]
//  Inc(B, (C rol 5) + (A xor D xor E) + W[78] + $CA62C1D6); D := D rol 30;
  mov ESI,ECX
  rol ESI,5
  mov EBP,EDI
  add EBX,ESI
  xor EBP,EDX
  add EBX,[ESP + 312]
  xor EBP,EAX
  ror EDX,2
  lea EBX,[EBP + EBX - $359D3E2A]
//  Inc(A, (B rol 5) + (E xor C xor D) + W[79] + $CA62C1D6); C := C rol 30;
  mov ESI,EBX
  rol ESI,5
  mov EBP,EDX
  add EAX,ESI
  xor EBP,ECX
  add EAX,[ESP + 316]
  xor EBP,EDI
  ror ECX,2
  lea EAX,[EBP + EAX - $359D3E2A]

