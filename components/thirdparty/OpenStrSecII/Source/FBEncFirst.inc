// EAX = Self (TBlockCipher desc.)
// EDX = Dst
// ECX = Src
// ESI = Key stream block
// EDI = Block len
// EBX = Count (Src/Dst len)

  push EBX
  mov EBP,dword [EAX].TBlockCipher.FFBIndex
  test EBP,EBP
  jz @@DoneFBEncFirst
  add EBX,EBP
  cmp EBX,EDI
  jl @@1
  mov EBX,EDI
@@1:
  sub EBX,EBP
  jz @@DoneFBEncFirst

  push EAX
  push ESI

  add ESI,EBX
  add ESI,EBP

  add EDX,EBX
  add ECX,EBX

  add EBP,EBX

  neg EBX
  add dword [ESP + 8],EBX

@@2:
  mov AL,byte [ESI + EBX]
  xor AL,byte [ECX + EBX]
  mov byte [EDX + EBX],AL
  mov byte [ESI + EBX],AL
  inc EBX
  jnz @@2

  pop ESI
  pop EAX

  cmp EBP,EDI
  jne @@3
  xor EBP,EBP
@@3:
  mov dword [EAX].TBlockCipher.FFBIndex,EBP
@@DoneFBEncFirst:
  pop EBX
