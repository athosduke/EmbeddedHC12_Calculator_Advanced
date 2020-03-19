***********************************************************
*
* Title:       Homework 7
*
* Rivision:    V4.5
*
* Date:        Oct 23 2019
*
* Programmer:  Songmeng Wang
*
* Company:    The Pennsylvania State University
*
* Algorithm:   Loops and conditional branches of CSM-12C128 board
*
* Register use:A accumulator:delay time counter
*             B accumulator:delay time counter
*             X register:delay loop counter
*             Y register:delay loop counter
*
* Memory use: RAM locations from $3000 for data
*                           from $3100 for instruction
*
* Input: Parameters hard coded in program
*
* Output: LED 1,2,3,4 at PORT 4,5,6,7
*
* Observation: This is the program that hold LED 2 on and rise/fall 
*             light level of LED 4 sequentially 
*
* Comments: This program is developed and simulated using Codeworrior 
*          development software
*
***********************************************************
;export symbols
              XDEF      Entry          ; export 'Entry' symbol
              ABSENTRY  Entry          ; for assembly entry point

;include derivative specific macros
;PORTB         EQU     $0001
;DDRB          EQU     $0003
;add more for the ones you need

SCISR1        EQU     $00cc            ; Serial port (SCI) Status Register 1
SCIDRL        EQU     $00cf            ; Serial port (SCI) Data Register

;following is for the TestTerm debugger simulation only
;SCISR1        EQU     $0203            ; Serial port (SCI) Status Register 1
;SCIDRL        EQU     $0204            ; Serial port (SCI) Data Register

CR            equ     $0d              ; carriage return, ASCII 'Return' key
LF            equ     $0a              ; line feed, ASCII 'next line' character

;variable/data section below
              ORG     $3000            ; RAMStart defined as $3000
                                       ; in MC9S12C128 chip ($3000 - $3FFF)
Count         DS.B    1
Count1        DS.B    1
Count2        DS.B    1
Countsig      DS.B    1
Buff          DS.B    8
Buff1         DS.B    3
Buffsig       DS.B    1
Buff2         DS.B    3
num1          DS.B    2
num2          DS.B    2
checkWrong    DS.B    1
result        DS.B    2
checkY        DS.B    2
checkoverflow DS.B    1
leadzero      DS.B    1
xstore        DS.B    2

; Each message ends with $00 (NULL ASCII character) for your program.
;
; There are 256 bytes from $3000 to $3100.  If you need more bytes for
; your messages, you can put more messages 'msg3' and 'msg4' at the end of 
; the program.
                                  
StackSP                                ; Stack space reserved from here to
                                       ; StackST

              ORG  $3100
;code section below
Entry
              LDS   #Entry             ; initialize the stack pointer

; add PORTB initialization code here
              ;
              ;LDAA       #%11110000    ; set PORTB bit 7,6,5,4 as output, 3,2,1,0 as input
              ;STAA       DDRB          ; LED 1,2,3,4 on PORTB bit 4,5,6,7
                                       ; DIP switch 1,2,3,4 on PORTB bit 0,1,2,3.
             ; LDAA       #%00110000    ; Turn off LED 1,2 at PORTB bit 4,5
              ;STAA       PORTB         ; Note: LED numbers and PORTB bit numbers are different

              ldx   #msg1              ; print the first message, 'Welcome!.... '
              jsr   printmsg
            
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar            ;   Cariage Return/Enter key
              ldaa  #LF                ; move the cursor to next line, Line Feed
              jsr   putchar
                        
initialize                  
              ldaa  #$20
              ldx   #Count
initialloop   clr   1,X+
              deca
              bne   initialloop

              ldy   #Buff              ; initialize Y 
              ldx   #Buff1             ; initialize X
              
              
looop         
              jsr   getchar            ; type writer - check the key board
              cmpa  #$00               ;  if nothing typed, keep checking
              beq   looop
                                       ;  otherwise - what is typed on key board
              jsr   putchar            ; is displayed on the terminal window
             
              staa  1,Y+               ; save char in buff, increment X
              inc   Count              ; increment Count
                            
              cmpa  #$2A
              beq   checksig           ; *
                                          
              cmpa  #$2B
              beq   checksig           ; +

              
              cmpa  #$2D
              beq   checksig           ; -

              
              cmpa  #$2F
              beq   checksig           ; /

              cmpa  #CR
              beq   enter              ; check enter
              
              cmpa  #$30
              blt   wrong              ; 0~9
              cmpa  #$39
              bgt   wrong
              
              ldab  Countsig
              cmpb  #$01               ;Countsig = 1?
              beq   secondnum
              
                          
              ldab  Count1
              cmpb  #$03               ;Count1 >= 3?
              bge   wrong
              bra   storeinBuff1              
                              
storeinBuff1  
              staa  1,X+                ; store num1 in Buff1
              inc   Count1              ; increment Count1
              bra   looop
              

secondnum     ldab  Count2
              cmpb  #$03                ;Count2 >=3?
              bge   wrong
              bra   storeinBuff2
                            

storeinBuff2  
              staa  1,X+                ; store num2 in Buff2
              inc   Count2              ; increment Count2
              bra   looop
                                          
checksig      ldab  Countsig
              cmpb  #$01                ; Countsig = 1?
              beq   wrong               

              ldab  Count1
              beq   wrong               ;Count1 = 0?
              ldx   #Buffsig
              staa  X                   ; store sig in BuffSig
              ldx   #Buff2              ; set x point to Buff2
              inc   Countsig
              bra   looop
              
wrong         jsr   getchar            ; type writer - check the key board
              cmpa  #$00               ;  if nothing typed, keep checking
              beq   wrong
                                       ;  otherwise - what is typed on key board
              jsr   putchar            ; is displayed on the terminal window
              
              cmpa  #CR
              bne   wrong              ; if Enter/Return key is pressed, move the             
              inc   checkWrong
              iny
              inc   Count
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF                ; cursor to next line
              jsr   putchar
              bra   printsaved

enter         
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF
              jsr   putchar
              ldaa  Count2
              cmpa  #$00
              bne   printsaved
              ldaa  Countsig
              cmpa  #$01
              bne   printsaved
              inc   checkWrong
              bra   printsaved
              
printsaved    dey                       
              dec   Count
              ldab  Count
deyloop       dey
              decb
              bne   deyloop
printloop     ldaa  1,Y+
              jsr   putchar             ;print on screen
              dec   Count
              bne   printloop
              ldaa  checkWrong
              cmpa  #$01
              bne   validinput
overflow     
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF
              jsr   putchar
              ldaa  checkoverflow
              cmpa  #$01
              beq   printoverflow
              ldx   #invalidmsg
              jsr   printmsg
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF
              jsr   putchar
              lbra   initialize

printoverflow ldx   #overflowmsg
              jsr   printmsg
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF
              jsr   putchar
              lbra  initialize              
              
validinput    jsr   getnum1
num1saved     jsr   getnum2
num2saved
              ldd   num2
              cmpa  #$00
              bne   continue
              cmpb  #$00
              bne   continue
              ldaa  Buffsig
              cmpa  #$00
              beq   onenum
              bra   overflow
              
         
continue      ldaa  Buffsig             
              
              cmpa  #$2A
              lbeq   multiply            ; *
                                        
              cmpa  #$2B
              lbeq   add                 ; +
              
              cmpa  #$2D
              lbeq   minus               ; -
              
              cmpa  #$2F
              lbeq   divide              ; /
              
onenum        ldd   num1
              std   result
                                                       
computedone   
              jsr   printfinal
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF
              jsr   putchar
              
              lbra   initialize

;subroutine section below

;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL           equ     $00
printmsg       psha                   ;Save registers
               pshx
printmsgloop   ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
               cmpa    #NULL
               beq     printmsgdone   ;end of strint yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsgloop

printmsgdone   pulx 
               pula
               rts
;***********end of printmsg********************


;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar        brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
               staa  SCIDRL                      ; send a character
               rts
;***************end of putchar*****************


;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************
getchar        brclr SCISR1,#%00100000,getchar7
               ldaa  SCIDRL
               rts
getchar7       clra
               rts
;****************end of getchar**************** 


;*****************getnum***********************
;*input: Count, Buff, X
;*output: number in num
;**********************************************
getnum1         
               ldaa   Count1
               ldx    #Buff1
num1loop       deca
               beq    num1lastdig     ;X now point to the last digit of num1
               inx
               bra    num1loop

num1lastdig    ldab   1,X-    ; load last char in B, decrement Y

               subb   #$30    ; subtract $30
               clra
               std    num1
               dec    Count1
               lbeq   num1saved
               
               ldab   1,X-
               
               subb   #$30
               ldaa   #$0A
               mul            ; multiply A and B, stored in A:B
               addd   num1    ; add last num
               std    num1    ; store in num1
               dec    Count1
               lbeq   num1saved
               
               ldab   1,X-               
               clra               
               subb   #$30
               ldy    #$0064    ; Y = 100
               emul           ; multiply D and Y, Y:D
               addd   num1
               std    num1    ; D to num1
               rts
               
;**********************************************   
               
 ;*****************getnum***********************
;*input: Count, Buff, X
;*output: number in num
;**********************************************
getnum2         
               ldaa   Count2
               ldx    #Buff2
num2loop       deca
               beq    num2lastdig     ;X now point to the last digit of num1
               inx
               bra    num2loop

num2lastdig    ldab   1,X-    ; load last char in B, decrement Y

               subb   #$30    ; subtract $30
               clra
               std    num2
               dec    Count2
               lbeq   num2saved
               
               ldab   1,X-
               
               subb   #$30
               ldaa   #$0A
               mul            ; multiply A and B, stored in A:B
               addd   num2    ; add last num
               std    num2    ; store in num1
               dec    Count2
               lbeq   num2saved
               
               ldab   1,X-
                              
               subb   #$30
               clra
               ldy    #$0064    ; Y = 100
               emul           ; multiply D and Y, Y:D
               addd   num2
               std    num2    ; D to num1
               rts
;**********************************************                 
 ;*****************add***********************
;*input: 
;*output: 
;**********************************************
add
               ldd    num1     ;num1 to A, num1+1 to B
               addd   num2     ;A:B + num2:num2+1, stored in A:B
               std    result
                                             
               lbra   computedone
;********************************************** 

 ;*****************minus***********************
;*input: 
;*output: 
;**********************************************
minus
               ldd    num1     ;num1 to A, num1+1 to B
               subd   num2     ;A:B + num2:num2+1, stored in A:B
               std    result
               lbra   computedone
;**********************************************

 ;*****************multiply***********************
;*input: 
;*output: 
;**********************************************
multiply
               ldy    num2     ;num2 to Y
               ldd    num1     ; num1 to D
               emul            ; D * Y, Y:D
               std    result   ; D to result
               sty    checkY   ; Y to checkY
               ldd    checkY   ; Y to A:B
               inc    checkoverflow
               cmpa   #$00
               lbne    overflow
               cmpb   #$00
               lbne    overflow ; check if Y = 0?
               lbra    computedone 
               
               
;**********************************************                 

 ;*****************divide***********************
;*input: 
;*output: 
;**********************************************
divide
               ldx    num2     ;num2 to X
               ldd    num1     ; num1 to D
               idiv            ; D / X => X, remainder => D
               stx    result   ; X to result
               lbra    computedone 
               
               
;**********************************************                 
                 
;*****************printfinal***********************
;*input: 
;*output: 
;**********************************************                
printfinal     
               ldaa   #$3D
               jsr    putchar                 ; print ="="
               ldaa   Buffsig
               cmpa   #$2D                    ; -
               lbeq   couldneg                ;jump for -
               
tenthousand    ldd    result                  ;result to D
               ldx    #$2710                   ; X = 10000
               idiv                           ; D / X => X, remainder => D
               std    result                  ; remainder => result
               stx    xstore                  ; X to D
               ldd    xstore
               cmpb   #$00                    ; if = 0, don't print zero
               beq    thousand
               tba                            ;b to a
               adda   #$30
               jsr    putchar                 ;printchar                           
               inc    leadzero
               
thousand       ldd    result                  ; result to D
               ldx    #$03E8                  ; X = 1000
               idiv                           ; D / X => X, remainder => D        
               std    result                  ; remainder to result
               stx    xstore
               ldd    xstore                  ; X to D
               cmpb   #$00
               beq    ifzerothousand
               tba
               adda   #$30                     ;print char
               jsr    putchar
               inc    leadzero
               
hundred        ldd    result                  ; result to D
               ldx    #$0064                  ; X = 100
               idiv                           ; D / X => X, remainder => D        
               std    result                  ; remainder to result
               stx    xstore
               ldd    xstore                  ; X to D
               cmpb   #$00
               beq    ifzerohundred
               tba
               adda   #$30                    ;print char
               jsr    putchar
               inc    leadzero
               
ten            ldd    result                  ; result to D
               ldx    #$000A                  ; X = 10
               idiv                           ; D / X => X, remainder => D        
               std    result                  ; remainder to result
               stx    xstore
               ldd    xstore                  ; X to D
               cmpb   #$00
               beq    ifzeroten
               tba
               adda   #$30                    ;print char
               jsr    putchar
               inc    leadzero               
               
one            ldd    result
               cmpb   #$00
               beq    ifonezero
               tba
               adda   #$30
               jsr    putchar
               rts
               
                              
ifzerothousand ldaa   leadzero
               cmpa   #$00
               ble    hundred                   ; leadzero > 0 ?
               ldaa   #$30                      ; print 0
               jsr    putchar
               bra    hundred
ifzerohundred  ldaa   leadzero
               cmpa   #$00
               ble    ten                       ; leadzero > 0 ?
               ldaa   #$30                      ; print 0
               jsr    putchar
               bra    ten
ifzeroten      ldaa   leadzero
               cmpa   #$00
               ble    one                   ; leadzero > 0 ?
               ldaa   #$30                      ; print 0
               jsr    putchar
               bra    one
ifonezero      ldaa   #$30
               jsr    putchar
               rts
      
couldneg       ldaa   result
               anda   #%10000000
               cmpa   #%10000000                 ; check first digit 1?
               lbne    tenthousand
               ldd    result
               cmpb   #$00
               beq    negboth
               negb                              ;two's complement of B
               coma                              ;one's complement of A
negatedone     std    result
               ldaa   #$2D
               jsr    putchar                     ;print -
               lbra    hundred
               
negboth        negb
               nega                              
               bra    negatedone
                       
               
               
;OPTIONAL
;more variable/data section below
; this is after the program code section
; of the RAM.  RAM ends at $3FFF
; in MC9S12C128 chip

msg1           DC.B    'Welcome!  Enter the number you want to calculate, maximum compute two three-digit-number, with one operation.', $00
invalidmsg     DC.B    'invalid input, error ', $00
overflowmsg    DC.B    'overflow error ', $00           
               
               
               
               
               
               
               
               
               
               
               
               
                               
end