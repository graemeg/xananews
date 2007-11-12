// A: EAX
// B: EBX
// C: ECX
// D: EDX
// E: EDI
// W: [ESP]

//  Inc(E, (A rol 5) + ((B and C) or (D and (B or C))) + W[40] + $8F1BBCDC); B := B rol 30;
  mov ESI,ECX
  add EDI,[ESP + 160]
  or  ESI,EBX
  and ESI,EDX
  mov EBP,ECX
  and EBP,EBX
  or  ESI,EBP
  ror EBX,2
  lea EDI,[ESI + EDI - $70E44324]
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
//  Inc(D, (E rol 5) + ((A and B) or (C and (A or B))) + W[41] + $8F1BBCDC); A := A rol 30;
  mov ESI,EBX
  add EDX,[ESP + 164]
  or  ESI,EAX
  and ESI,ECX
  mov EBP,EBX
  and EBP,EAX
  or  ESI,EBP
  ror EAX,2
  lea EDX,[ESI + EDX - $70E44324]
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
//  Inc(C, (D rol 5) + ((E and A) or (B and (E or A))) + W[42] + $8F1BBCDC); E := E rol 30;
  mov ESI,EAX
  add ECX,[ESP + 168]
  or  ESI,EDI
  and ESI,EBX
  mov EBP,EAX
  and EBP,EDI
  or  ESI,EBP
  ror EDI,2
  lea ECX,[ESI + ECX - $70E44324]
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
//  Inc(B, (C rol 5) + ((D and E) or (A and (D or E))) + W[43] + $8F1BBCDC); D := D rol 30;
  mov ESI,EDI
  add EBX,[ESP + 172]
  or  ESI,EDX
  and ESI,EAX
  mov EBP,EDI
  and EBP,EDX
  or  ESI,EBP
  ror EDX,2
  lea EBX,[ESI + EBX - $70E44324]
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
//  Inc(A, (B rol 5) + ((C and D) or (E and (C or D))) + W[44] + $8F1BBCDC); C := C rol 30;
  mov ESI,EDX
  add EAX,[ESP + 176]
  or  ESI,ECX
  and ESI,EDI
  mov EBP,EDX
  and EBP,ECX
  or  ESI,EBP
  ror ECX,2
  lea EAX,[ESI + EAX - $70E44324]
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI
//  Inc(E, (A rol 5) + ((B and C) or (D and (B or C))) + W[45] + $8F1BBCDC); B := B rol 30;
  mov ESI,ECX
  add EDI,[ESP + 180]
  or  ESI,EBX
  and ESI,EDX
  mov EBP,ECX
  and EBP,EBX
  or  ESI,EBP
  ror EBX,2
  lea EDI,[ESI + EDI - $70E44324]
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
//  Inc(D, (E rol 5) + ((A and B) or (C and (A or B))) + W[46] + $8F1BBCDC); A := A rol 30;
  mov ESI,EBX
  add EDX,[ESP + 184]
  or  ESI,EAX
  and ESI,ECX
  mov EBP,EBX
  and EBP,EAX
  or  ESI,EBP
  ror EAX,2
  lea EDX,[ESI + EDX - $70E44324]
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
//  Inc(C, (D rol 5) + ((E and A) or (B and (E or A))) + W[47] + $8F1BBCDC); E := E rol 30;
  mov ESI,EAX
  add ECX,[ESP + 188]
  or  ESI,EDI
  and ESI,EBX
  mov EBP,EAX
  and EBP,EDI
  or  ESI,EBP
  ror EDI,2
  lea ECX,[ESI + ECX - $70E44324]
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
//  Inc(B, (C rol 5) + ((D and E) or (A and (D or E))) + W[48] + $8F1BBCDC); D := D rol 30;
  mov ESI,EDI
  add EBX,[ESP + 192]
  or  ESI,EDX
  and ESI,EAX
  mov EBP,EDI
  and EBP,EDX
  or  ESI,EBP
  ror EDX,2
  lea EBX,[ESI + EBX - $70E44324]
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
//  Inc(A, (B rol 5) + ((C and D) or (E and (C or D))) + W[49] + $8F1BBCDC); C := C rol 30;
  mov ESI,EDX
  add EAX,[ESP + 196]
  or  ESI,ECX
  and ESI,EDI
  mov EBP,EDX
  and EBP,ECX
  or  ESI,EBP
  ror ECX,2
  lea EAX,[ESI + EAX - $70E44324]
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI
//  Inc(E, (A rol 5) + ((B and C) or (D and (B or C))) + W[50] + $8F1BBCDC); B := B rol 30;
  mov ESI,ECX
  add EDI,[ESP + 200]
  or  ESI,EBX
  and ESI,EDX
  mov EBP,ECX
  and EBP,EBX
  or  ESI,EBP
  ror EBX,2
  lea EDI,[ESI + EDI - $70E44324]
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
//  Inc(D, (E rol 5) + ((A and B) or (C and (A or B))) + W[51] + $8F1BBCDC); A := A rol 30;
  mov ESI,EBX
  add EDX,[ESP + 204]
  or  ESI,EAX
  and ESI,ECX
  mov EBP,EBX
  and EBP,EAX
  or  ESI,EBP
  ror EAX,2
  lea EDX,[ESI + EDX - $70E44324]
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
//  Inc(C, (D rol 5) + ((E and A) or (B and (E or A))) + W[52] + $8F1BBCDC); E := E rol 30;
  mov ESI,EAX
  add ECX,[ESP + 208]
  or  ESI,EDI
  and ESI,EBX
  mov EBP,EAX
  and EBP,EDI
  or  ESI,EBP
  ror EDI,2
  lea ECX,[ESI + ECX - $70E44324]
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
//  Inc(B, (C rol 5) + ((D and E) or (A and (D or E))) + W[53] + $8F1BBCDC); D := D rol 30;
  mov ESI,EDI
  add EBX,[ESP + 212]
  or  ESI,EDX
  and ESI,EAX
  mov EBP,EDI
  and EBP,EDX
  or  ESI,EBP
  ror EDX,2
  lea EBX,[ESI + EBX - $70E44324]
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
//  Inc(A, (B rol 5) + ((C and D) or (E and (C or D))) + W[54] + $8F1BBCDC); C := C rol 30;
  mov ESI,EDX
  add EAX,[ESP + 216]
  or  ESI,ECX
  and ESI,EDI
  mov EBP,EDX
  and EBP,ECX
  or  ESI,EBP
  ror ECX,2
  lea EAX,[ESI + EAX - $70E44324]
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI
//  Inc(E, (A rol 5) + ((B and C) or (D and (B or C))) + W[55] + $8F1BBCDC); B := B rol 30;
  mov ESI,ECX
  add EDI,[ESP + 220]
  or  ESI,EBX
  and ESI,EDX
  mov EBP,ECX
  and EBP,EBX
  or  ESI,EBP
  ror EBX,2
  lea EDI,[ESI + EDI - $70E44324]
  mov ESI,EAX
  rol ESI,5
  add EDI,ESI
//  Inc(D, (E rol 5) + ((A and B) or (C and (A or B))) + W[56] + $8F1BBCDC); A := A rol 30;
  mov ESI,EBX
  add EDX,[ESP + 224]
  or  ESI,EAX
  and ESI,ECX
  mov EBP,EBX
  and EBP,EAX
  or  ESI,EBP
  ror EAX,2
  lea EDX,[ESI + EDX - $70E44324]
  mov ESI,EDI
  rol ESI,5
  add EDX,ESI
//  Inc(C, (D rol 5) + ((E and A) or (B and (E or A))) + W[57] + $8F1BBCDC); E := E rol 30;
  mov ESI,EAX
  add ECX,[ESP + 228]
  or  ESI,EDI
  and ESI,EBX
  mov EBP,EAX
  and EBP,EDI
  or  ESI,EBP
  ror EDI,2
  lea ECX,[ESI + ECX - $70E44324]
  mov ESI,EDX
  rol ESI,5
  add ECX,ESI
//  Inc(B, (C rol 5) + ((D and E) or (A and (D or E))) + W[58] + $8F1BBCDC); D := D rol 30;
  mov ESI,EDI
  add EBX,[ESP + 232]
  or  ESI,EDX
  and ESI,EAX
  mov EBP,EDI
  and EBP,EDX
  or  ESI,EBP
  ror EDX,2
  lea EBX,[ESI + EBX - $70E44324]
  mov ESI,ECX
  rol ESI,5
  add EBX,ESI
//  Inc(A, (B rol 5) + ((C and D) or (E and (C or D))) + W[59] + $8F1BBCDC); C := C rol 30;
  mov ESI,EDX
  add EAX,[ESP + 236]
  or  ESI,ECX
  and ESI,EDI
  mov EBP,EDX
  and EBP,ECX
  or  ESI,EBP
  ror ECX,2
  lea EAX,[ESI + EAX - $70E44324]
  mov ESI,EBX
  rol ESI,5
  add EAX,ESI

