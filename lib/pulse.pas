unit pulse;

{$IF NOT (DEFINED(atmega328p) or DEFINED(arduinouno) or DEFINED(arduinonano) or DEFINED(fpc_mcu_atmega328p) or DEFINED(fpc_mcu_arduinouno) or DEFINED(fpc_mcu_arduinonano))}
 {$Fatal Invalid controller type, expected: atmega328p, arduinouno, or arduinonano}
{$ENDIF}

{$mode objfpc}

interface

//pulseInMilli: return value in milliseconds (routine by @Dzandaa)
function pulseInMilli(pout, State: UINT8; TimeOut: UInt32): UInt32;

//pulseIn: return value in microseconds
function pulseIn(pin: UInt8; state: UInt8; timeout: UInt32 = 1000000): UInt32;
function pulseInLong(pin: UInt8; state: UInt8; timeout: UInt32 = 1000000): UInt32;

implementation

uses
  defs, digital, timer;

//pulseInMilli routine by @Dzandaa
//return value in Milliseconds
function pulseInMilli(pout, State: UINT8; TimeOut: UInt32): UInt32;
var
  Count, Cs: UInt32;
begin
  Cs := Millis;
  while (DigitalRead(pout) <> State) do if (Millis - Cs) > Timeout then exit(0);
  Cs := Millis;
  While (DigitalRead(pout) = State) do if (Millis - Cs) > Timeout then exit(0);
  Result := Millis - Cs;
end;

function pulseInSimplAsm(PortPtr: PUint8; bit, stateMask: Uint8; maxloops: UInt32): UInt32; assembler; nostackframe;
label
  L18D8, L18F4, L1904, L1920, L1930, L194E, LEnd_1964;
asm
    //registers usage

    //portptr:   r24:r25=>r5:r6=>r30:r31 (to make place for result?)
    //bit:       r22:r23=>r7    (to make place for result?)
    //statemask: r20:r21
    //maxloops:  r16:r17:r18:19

    //result:    r22:r23:r24:r25

    //width:     r2:r21:r3:r4

    //main interation loop on L1930, cycles per instruction in []

    {18b4:	1f 93}    	  push	r17
    {18b6:	0f 93}     	  push	r16
    {18b8:	9f 92}     	  push	r9
    {18ba:	8f 92}     	  push	r8
    {18bc:	7f 92}     	  push	r7
    {18be:	6f 92}     	  push	r6
    {18c0:	5f 92}     	  push	r5
    {18c2:	4f 92}     	  push	r4
    {18c4:	3f 92}     	  push	r3
    {18c6:	2f 92}     	  push	r2
    {18c8:	58 2e}     	  mov	r5, r24
    {18ca:	69 2e}     	  mov	r6, r25
    {18cc:	76 2e}     	  mov	r7, r22
//   width:= 0;
    {18ce:	21 2c}       	mov	r2, r1   //width
    {18d0:	51 2d}       	mov	r21, r1  //width
    {18d2:	31 2c}       	mov	r3, r1   //width
    {18d4:	41 2c}       	mov	r4, r1   //width
   // wait for any previous pulse to end
//   while ((portPtr^ and bit) = stateMask) do
    {18d6:	0e c0}       	rjmp	L18F4     	//rjmp	.+28 0x18f4
//   begin
//     Dec(maxloops);
  L18D8:
    {18d8:	01 50}       	subi	r16, 0x01	// 1
    {18da:	11 09}       	sbc	r17, r1
    {18dc:	21 09}       	sbc	r18, r1
    {18de:	31 09}       	sbc	r19, r1
//     if (maxloops = 0) then
    {18e0:	01 15}       	cp	r16, r1
    {18e2:	11 05}       	cpc	r17, r1
    {18e4:	21 05}       	cpc	r18, r1
    {18e6:	31 05}       	cpc	r19, r1
    {18e8:	29 f4}       	brne	L18F4      //brne	.+10     	; 0x18f4
//       Exit(0);
    {18ea:	61 2d}       	mov	r22, r1
    {18ec:	71 2d}       	mov	r23, r1
    {18ee:	81 2d}       	mov	r24, r1
    {18f0:	91 2d}       	mov	r25, r1
    {18f2:	38 c0}       	rjmp	LEnd_1964 //rjmp	.+112    	; 0x1964
//var
//  width: UInt32;
//begin
//   width:= 0;
   // wait for any previous pulse to end
//   while ((portPtr^ and bit) = stateMask) do
  L18F4:
    {18f4:	e5 2d}       	mov	r30, r5
    {18f6:	f6 2d}       	mov	r31, r6
    {18f8:	90 80}       	ld	r9, Z
    {18fa:	87 2c}       	mov	r8, r7
    {18fc:	89 20}       	and	r8, r9
    {18fe:	84 16}       	cp	r8, r20
    {1900:	59 f3}       	breq L18D8 //breq	.-42     	; 0x18d8
//     if (maxloops = 0) then
//       Exit(0);
//   end;

   // wait for the pulse to start
//   while ((PortPtr^ and bit) <> stateMask) do
    {1902:	0e c0}       	rjmp L1920 //rjmp	.+28     	; 0x1920
//   begin
//     Dec(maxloops);
  L1904:
    {1904:	01 50}       	subi	r16, 0x01	// 1
    {1906:	11 09}       	sbc	r17, r1
    {1908:	21 09}       	sbc	r18, r1
    {190a:	31 09}       	sbc	r19, r1
//     if (maxloops = 0) then
    {190c:	01 15}       	cp	r16, r1
    {190e:	11 05}       	cpc	r17, r1
    {1910:	21 05}       	cpc	r18, r1
    {1912:	31 05}       	cpc	r19, r1
    {1914:	29 f4}       	brne L1920  //brne	.+10     	; 0x1920
//       Exit(0);
    {1916:	61 2d}       	mov	r22, r1
    {1918:	71 2d}       	mov	r23, r1
    {191a:	81 2d}       	mov	r24, r1
    {191c:	91 2d}       	mov	r25, r1
    {191e:	22 c0}       	rjmp LEnd_1964  //rjmp	.+68     	; 0x1964
//     if (maxloops = 0) then
//       Exit(0);
//   end;

   // wait for the pulse to start
//   while ((PortPtr^ and bit) <> stateMask) do
  L1920:
    {1920:	e5 2d}       	mov	r30, r5
    {1922:	f6 2d}       	mov	r31, r6
    {1924:	90 80}       	ld	r9, Z
    {1926:	87 2c}       	mov	r8, r7
    {1928:	89 20}       	and	r8, r9
    {192a:	84 16}       	cp	r8, r20
    {192c:	59 f7}       	brne L1904  //brne	.-42     	; 0x1904
//     if (maxloops = 0) then
//       Exit(0);
//   end;

   // wait for the pulse to stop
//   while ((PortPtr^ and bit) = stateMask) do
    {192e:	0f c0}       	rjmp L194E  //rjmp	.+30     	; 0x194e
//   begin
//     Inc(width);
  L1930:
    {1930:	a1 e0}       	ldi	r26, 0x01	// 1                                    [length = 1]
    {1932:	2a 0e}       	add	r2, r26   //width:=width+1                        [length = 1]
    {1934:	51 1d}       	adc	r21, r1   //width                                 [length = 1]
    {1936:	31 1c}       	adc	r3, r1    //width                                 [length = 1]
    {1938:	41 1c}       	adc	r4, r1    //width                                 [length = 1]
//     if (width = maxloops) then
    {193a:	20 16}       	cp	r2, r16   //(width = maxloops)                    [length = 1]
    {193c:	51 07}       	cpc	r21, r17  //                                      [length = 1]
    {193e:	32 06}       	cpc	r3, r18   //                                      [length = 1]
    {1940:	43 06}       	cpc	r4, r19   //                                      [length = 1]
    {1942:	29 f4}       	brne L194E //brne	.+10     	; 0x194e                  [length = 2 (1/2)] (jump if not equal: not branching/branchig)
//        Exit(0);
    {1944:	61 2d}       	mov	r22, r1
    {1946:	71 2d}       	mov	r23, r1
    {1948:	81 2d}       	mov	r24, r1
    {194a:	91 2d}       	mov	r25, r1
    {194c:	0b c0}       	rjmp LEnd_1964 //rjmp	.+22     	; 0x1964
//     if (maxloops = 0) then
//       Exit(0);
//   end;

   // wait for the pulse to stop
//   while ((PortPtr^ and bit) = stateMask) do
  L194E:
    {194e:	e5 2d}       	mov	r30, r5 //                                        [length = 1]
    {1950:	f6 2d}       	mov	r31, r6 //                                        [length = 1]
    {1952:	90 80}       	ld	r9, Z   //PortPtr                                 [length = 2]
    {1954:	87 2c}       	mov	r8, r7  //bit                                     [length = 1]
    {1956:	89 20}       	and	r8, r9  //bit and Portptr^                        [length = 1]
    {1958:	84 16}       	cp	r8, r20 //StateMask                               [length = 1]
    {195a:	51 f3}       	breq L1930  //breq	.-44     	; 0x1930                [length = 2 (1/2)] (jump if equal)
//   begin
//     Inc(width);
//     if (width = maxloops) then
//        Exit(0);
//   end;
//   Result:=width;
    {195c:	62 2d}       	mov	r22, r2
    {195e:	75 2f}       	mov	r23, r21
    {1960:	83 2d}       	mov	r24, r3
    {1962:	94 2d}       	mov	r25, r4
//end;
   LEnd_1964:
    {1964:	2f 90}       	pop	r2
    {1966:	3f 90}       	pop	r3
    {1968:	4f 90}       	pop	r4
    {196a:	5f 90}       	pop	r5
    {196c:	6f 90}       	pop	r6
    {196e:	7f 90}       	pop	r7
    {1970:	8f 90}       	pop	r8
    {1972:	9f 90}       	pop	r9
    {1974:	0f 91}       	pop	r16
    {1976:	1f 91}       	pop	r17
end;

//original arduino asm code, compiles thanks to path to aasmcpu.pas made by @ccrause (fix for #41174)
//commit a43f1bc1d0fcc3518412feb61019d124ff170f58
//https://gitlab.com/freepascal.org/fpc/source
{function pulseInSimplAsmOrg(PortPtr: PUint8; bit, stateMask: Uint8; maxloops: UInt32): UInt32; assembler; nostackframe;
label
  LFBB1,
  L2,L4,L13,L6,L7,
  LM0,LM1,LM2,LM3,LM4,LM5,L9,L10,LM6,LM7,LM8,LM9,LM10,LM11;
asm
LM0:
LFBB1:
    push r12   //   //  130 pushqi1/1 [length = 1]
    push r13   //   //  131 pushqi1/1 [length = 1]
    push r14   //   //  132 pushqi1/1 [length = 1]
    push r15   //   //  133 pushqi1/1 [length = 1]
    push r16   //   //  134 pushqi1/1 [length = 1]
    push r17   //   //  135 pushqi1/1 [length = 1]
// prologue: function
// frame size = 0
// stack size = 6
//.L__stack_usage = 6
    mov r30,r24  //  port, port   //  2 *movhi/1  [length = 2]
    mov r31,r25  //  port, port
//     unsigned long width = 0//
//     // wait for any previous pulse to end
//     while ((*port & bit) == stateMask)

LM1:
    rjmp L2   //   //  181 jump  [length = 1]
L4:
//         if (--maxloops == 0)
LM2:
    subi r16,1   //  maxloops,  //  17  addsi3/2  [length = 4]
    sbc r17, r1   //  maxloops
    sbc r18, r1   //  maxloops
    sbc r19, r1   //  maxloops
    breq L13  // ,   //  19  branch  [length = 1]
L2:
//         if (--maxloops == 0)
LM3:
    ld r25,Z   //  D.1554, *port_7(D)   //  22  movqi_insn/4  [length = 1]
    and r25,r22  //  D.1554, bit  //  24  andqi3/1  [length = 1]
    cp r25,r20   //  D.1554, stateMask  //  25  *cmpqi/2  [length = 1]
    breq L4   // ,   //  26  branch  [length = 1]
    rjmp L6   //   //  184 jump  [length = 1]
L7:
//             return 0
//
//    // wait for the pulse to start
//     while ((*port & bit) != stateMask)
//         if (--maxloops == 0)

LM4:
    subi r16,1   //  maxloops,  //  31  addsi3/2  [length = 4]
    sbc r17, r1   //  maxloops
    sbc r18, r1   //  maxloops
    sbc r19, r1   //  maxloops
    breq L13  // ,   //  33  branch  [length = 1]  <-- @ackarwow here error "conditional branch destination is out of range"
L6:
//         if (--maxloops == 0)
LM5:
    ld r25,Z   //  D.1554, *port_7(D)   //  41  movqi_insn/4  [length = 1]
    and r25,r22  //  D.1554, bit  //  43  andqi3/1  [length = 1]
    cpse r25,r20   //  D.1554, stateMask  //  44  enable_interrupt-3  [length = 1]
    rjmp L7   //
    mov r12, r1   //  width  //  7 *movsi/2  [length = 4]
    mov r13, r1   //  width
    mov r14, r1   //  width
    mov r15, r1   //  width
    rjmp L9   //   //  186 jump  [length = 1]
L10:
//             return 0//
//
//     // wait for the pulse to stop
//     while ((*port & bit) == stateMask)
//         if (++width == maxloops)
//
LM6:
    ldi r24,-1   // ,   //  50  addsi3/3  [length = 5]
    sub r12,r24  //  width,
    sbc r13,r24  //  width,
    sbc r14,r24  //  width,
    sbc r15,r24  //  width,
    cp r16,r12   //  maxloops, width  //  51  *cmpsi/2  [length = 4]
    cpc r17,r13  //  maxloops, width
    cpc r18,r14  //  maxloops, width
    cpc r19,r15  //  maxloops, width
    breq L13  // ,   //  52  branch  [length = 1]  <-- @ackarwow here error "conditional branch destination is out of range"
L9:
//         if (++width == maxloops)
LM7:
    ld r24,Z   //  D.1554, *port_7(D)   //  60  movqi_insn/4  [length = 1]
    and r24,r22  //  D.1554, bit  //  62  andqi3/1  [length = 1]
    cp r24,r20   //  D.1554, stateMask  //  63  *cmpqi/2  [length = 1]
    breq L10  // ,   //  64  branch  [length = 1]
//             return 0//
//
//     return width//
//
LM8:
    mov r22,r12  //  D.1553, width  //  108 movqi_insn/1  [length = 1]
    mov r23,r13  //  D.1553, width  //  109 movqi_insn/1  [length = 1]
    mov r24,r14  //  D.1553, width  //  110 movqi_insn/1  [length = 1]
    mov r25,r15  //  D.1553, width  //  111 movqi_insn/1  [length = 1]
// epilogue start
LM9:
    pop r17  //   //  171 popqi [length = 1]
    pop r16  //   //  172 popqi [length = 1]
    pop r15  //   //  173 popqi [length = 1]
    pop r14  //   //  174 popqi [length = 1]
    pop r13  //   //  175 popqi [length = 1]
    pop r12  //   //  176 popqi [length = 1]
//    ret  //  177 return_from_epilogue  [length = 1]
L13:
LM10:
    ldi r22,0  //  D.1553   //  120 movqi_insn/1  [length = 1]
    ldi r23,0  //  D.1553   //  121 movqi_insn/1  [length = 1]
    ldi r24,0  //  D.1553   //  122 movqi_insn/1  [length = 1]
    ldi r25,0  //  D.1553   //  123 movqi_insn/1  [length = 1]
// epilogue start
LM11:
    pop r17  //   //  138 popqi [length = 1]
    pop r16  //   //  139 popqi [length = 1]
    pop r15  //   //  140 popqi [length = 1]
    pop r14  //   //  141 popqi [length = 1]
    pop r13  //   //  142 popqi [length = 1]
    pop r12  //   //  143 popqi [length = 1]
//    ret  //  144 return_from_epilogue  [length = 1]
end;}

function pulseInSimpl(PortPtr: PUint8; bit, stateMask: Uint8; maxloops: UInt32): UInt32;
var
  width: UInt32;
begin
   width:= 0;
   // wait for any previous pulse to end
   while ((portPtr^ and bit) = stateMask) do
   begin
     Dec(maxloops);
     if (maxloops = 0) then
       Exit(0);
   end;

   // wait for the pulse to start
   while ((PortPtr^ and bit) <> stateMask) do
   begin
     Dec(maxloops);
     if (maxloops = 0) then
       Exit(0);
   end;

   // wait for the pulse to stop
   while ((PortPtr^ and bit) = stateMask) do
   begin
     Inc(width);
     if (width = maxloops) then
        Exit(0);
   end;
   Result:=width;
end;

{
 Measures the length (in microseconds) of a pulse on the pin; state is HIGH
 or LOW, the type of pulse to measure.  Works on pulses from 2-3 microseconds
 to 3 minutes in length, but must be called at least a few dozen microseconds
 before the start of the pulse.

 This function performs better with short pulses in noInterrupt() context
}
function pulseIn(pin: UInt8; state: UInt8; timeout: UInt32 = 1000000): UInt32;
const
  cycles = 20; {@ackarwow: counted clock cycles per iteration; 16 in Arduino sketch}
var
  bit, port, stateMask: UInt8;
  maxloops, width: UInt32;
begin
  // cache the port and bit of the pin in order to speed up the
  // pulse width measuring loop and achieve finer resolution.  calling
  // digitalRead() instead yields much coarser resolution.
  bit:= digitalPinToBitMask(pin);
  port:= digitalPinToPort(pin);
  if state>0 then
    stateMask:=bit
  else
    StateMask:=0;

  // convert the timeout from microseconds to a number of times through
  // the initial loop; it takes approximately 16 clock cycles per iteration
  maxloops:= microsecondsToClockCycles(timeout) div cycles;

  width:={pulseInSimpl} pulseInSimplAsm(portInputRegister(port), bit, stateMask, maxloops);

  // prevent clockCyclesToMicroseconds to return bogus values if countPulseASM timed out
  if (width>0) then
    Result:=clockCyclesToMicroseconds(width * cycles + cycles)
  else
    Result:=0;
end;

function pulseInLong(pin: UInt8; state: UInt8; timeout: UInt32 = 1000000): UInt32;
var
  bit, port, stateMask: UInt8;
  startMicros, start: UInt32;
begin
  // cache the port and bit of the pin in order to speed up the
  // pulse width measuring loop and achieve finer resolution.  calling
  // digitalRead() instead yields much coarser resolution.
  bit:= digitalPinToBitMask(pin);
  port:= digitalPinToPort(pin);
  if state>0 then
    stateMask:=bit
  else
    StateMask:=0;

  startMicros:= micros();

  // wait for any previous pulse to end
  while ((portInputRegister(port)^ and bit) = stateMask) do
  begin
    if (micros() - startMicros > timeout) then
      Exit(0);
  end;

  // wait for the pulse to start
  while ((portInputRegister(port)^ and bit) <> stateMask) do
  begin
    if (micros() - startMicros > timeout) then
      Exit(0);
  end;

  start:= micros();
  // wait for the pulse to stop
  while ((portInputRegister(port)^ and bit) = stateMask) do
  begin
    if (micros() - startMicros > timeout) then
      Exit(0);
  end;

  Result:=micros() - start;
end;

end.

