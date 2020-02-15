
; Register allocation.
; FORTH requires some registers to be allocated for specific functions.

; IP - the Interpreter Pointer.
; IP always points to the address of the next subroutine to be executed.
; In our case, the IP is mapped onto DE.

; W - the Working register.
; W is used for holding the address of the subroutine when we execute it.
; W is mapped onto HL.

; Some macros.

; These macros are general-purpose.

; INC2.
; Increment twice the given 16-bit register pair.
    macro INC2, regpair
    inc     \regpair
    inc     \regpair
    endmacro

; DEC2.
; Decrement twice the given 16-bit register pair.
    macro DEC2, regpair
    dec     \regpair
    dec     \regpair
    endmacro

; These macros are directly FORTH-related.

; Pushing and Popping from the Return Stack.
; Push goes High-Low so Pop goes Low-High.
    macro   PUSHRSP
    inc     IX
    ld      (IX), D
    inc     IX
    ld      (IX), E
    endmacro

    macro   POPRSP
    ld      E, (IX)
    dec     IX
    ld      D, (IX)
    dec     IX
    endmacro

; NEXT.
; NEXT loads the address of the next subroutine to be executed,
; which is pointed to by IP, and jumps to it. It also advances the IP
; to the next codeword to be executed.
    macro   NEXT
    ld      A, (DE)     ; Load next address to be executed.
    ld      L, A
    inc     DE
    ld      A, (DE)
    ld      H, A
    inc     DE

    jp      (HL)        ; Execute the subroutine!
    endmacro

; Reset vector.
    org     $0000
    jp      start

; Program start.
    org     $0100
start:
    ; Set up the stacks. SP points to the Parameter Stack,
    ; which grows down from 0xFFFD.
    ; IX points to the Return Stack, which grows upwards from
    ; 0xF000.
    ;
    ; This means that we have 4094 bytes available to be shared
    ; between them.
    ; Having one grow up and the other down allows them to share the space
    ; more efficiently - space not used by one stack can be used
    ; by the other.
    ld      SP, $FFFD
    ld      IX, $F000

    ; Now initialize data in RAM.
    ; HE points to the load values in ROM,
    ; while DE points to the runtime location in RAM.
    ld      HL, DATA_LOAD_START
    ld      DE, DATA_START

    ; BC is the amount of data to transfer.
    ld      BC, DATA_SIZE

    ldir

    ld      DE, cold_start
    NEXT

cold_start:
    addr    QUIT
    addr    GOODBYE

; DOCOL - "Do Colon"
; This is Forth's interpreter.
; Its job is to save the 'old' IP value and set IP
; to the address of the next codeword to be executed.
DOCOL:
    PUSHRSP             ; Push IP onto the stack.

    ; At this point, if DOCOL has been called (and not just jumped to)
    ; the return address will be on the stack.
    ; Handily, this return address is also the address of the first
    ; parameter word of the colon definition! How neat!
    ;
    ; So, to set IP to the address of the first parameter word, we can
    ; just pop DE off the stack!
    pop     DE

    NEXT                ; Execute the first word of this definition!

; Let's set up the dictionary.

; A dictionary entry looks like this:

;   2       address of previous word.
;   1       length <n>.
;   <n>     name, up to 256 characters.
;   2       codeword
;   2 * n   pointer to codeword of subsequent words.

; A special assembler symbol to keep track of the last word we defined.
LINK set 0

; Let's write a macro to do this.
    macro   DEFWORD, name, length, label
name_\label:
    addr    LINK
LINK set name_\label
    byte    \length
    text    \name
\label:
    call    DOCOL
    endmacro

; Let's also write a macro to define a word in assembler.
    macro   DEFCODE, name, length, label
name_\label:
    addr    LINK
LINK set name_\label
    byte    \length
    text    \name
\label:
    endmacro

; Now we can actually define some primitives.
; These are some simple ones, defined in assembler.

; Stack manipulations.

    DEFCODE "DROP", 4, DROP
    pop     HL          ; Drop the top of the parameter stack.
    NEXT

    DEFCODE "SWAP", 4, SWAP
    pop     HL
    pop     BC
    push    HL
    push    BC
    NEXT

    DEFCODE "DUP", 3, DUP
    pop     HL
    push    HL
    push    HL
    NEXT

; Arithmetic words.

    DEFCODE "1+", 2, INCR
    pop     HL
    inc     HL
    push    HL
    NEXT

    DEFCODE "1-", 2, DECR
    pop     HL
    dec     HL
    push    HL
    NEXT

    DEFCODE "2+", 2, INCR2
    pop     HL
    INC2    HL
    push    HL
    NEXT

    DEFCODE "2-", 2, DECR2
    pop     HL
    DEC2    HL
    push    HL
    NEXT

    DEFCODE "+", 1, ADD
    pop     HL
    pop     BC
    adc     HL, BC
    push    HL
    NEXT

    DEFCODE "-", 1, SUB
    pop     HL
    pop     BC
    sbc     HL, BC
    push    HL
    NEXT

; Memory operations.

    DEFCODE "!", 1, STORE
    pop     IY              ; TOS is address to store at.
    pop     HL              ; Next is data to store.
    ld      (IY), L
    ld      (IY+1), H
    NEXT

    DEFCODE "@", 1, FETCH
    pop     IY
    ld      L, (IY)
    ld      H, (IY+1)
    push    HL
    NEXT

; Input/Output.

    ; KEY gets a character from the serial line.
    DEFCODE "KEY", 3, KEY
    call    _KEY            ; The key pressed is returned in A.
    ld      L, A            ; Put it in the lower half of HL.
    ld      H, 0
    push    HL              ; Push onto the stack.
    NEXT

; Simple subroutine to get a key from the serial line, returned in A.
_KEY:
    push    HL
    push    BC

    ld      HL, (var_KEY_HEAD)
    ld      BC, (var_KEY_TAIL)

    ; Do we have characters in the buffer?
    ; If so skip straight to reading them.
    ld      A, H
    cp      B
    jp      nz, _KEY_read_buffer
    ld      A, L
    cp      C
    jp      nz, _KEY_read_buffer

    ; Otherwise we fill the buffer with characters from the serial line
    ; until we hit a LF.

    call    _NEWLINE

    ld      HL, str_ZFORTH_prompt
    ld      B, 8
    call    _PRINT

    ; Reset head and tail.
    ld      HL, _mem_KEY
    ld      BC, _mem_KEY

    ; Wait for ready.
_KEY_not_ready:
    in      A, (1)          ; Polling loop until we can read a character.
    cp      1
    jp      nz, _KEY_not_ready

    in      A, (0)          ; Read a character.
    cp      $0d             ; Is this a newline?

    ; If so, we're done.
    jp      z, _KEY_fill_done

    ; Otherwise store the character in the buffer.

    out     (0), A          ; Echo the character to the user.
    ld      (BC), A
    inc     BC              ; Increment the tail pointer.

    jp      _KEY_not_ready

_KEY_fill_done:
    ld      A, ' '          ; Store a terminating space.
    out     (0), A
    ld      (BC), A
    inc     BC

    ; Store the updated tail pointer.
    ld      (var_KEY_TAIL), BC

_KEY_read_buffer:
    ld      A, (HL)         ; Get the character.

    ; Increment the head pointer and store.
    inc     HL
    ld      (var_KEY_HEAD), HL

    pop     BC
    pop     HL

    ret

    ; EMIT writes a character to the serial line.
    DEFCODE "EMIT", 4, EMIT
    pop     HL
    ld      A, L
    out     (0), A
    NEXT

    ; DOT ('.') prints the top of the stack.
    DEFCODE ".", 1, DOT
    pop     HL
    call    _DOT
    NEXT

_DOT:
    ; Print a '$', indicating that this is a hexadecimal number.
    ld      A, '$'
    out     (0), A

    ; Print top half of H.
    ld      A, H
    srl     A
    srl     A
    srl     A
    srl     A

    call    print_hex_single

    ; Print bottom half of H.
    ld      A, H
    and     A, $0f

    call    print_hex_single

    ; Print top half of L.
    ld      A, L
    srl     A
    srl     A
    srl     A
    srl     A

    call    print_hex_single

    ; Print bottom half of L.
    ld      A, L
    and     A, $0f

    call    print_hex_single

    ret

; Prints a single hexadecimal digit.
; Expects the value to print to be in A.
; Behaviour is undefined if the digit is above 15.
print_hex_single:
    ; Is the value <= 9?
    cp      10
    jp      m, print_hex_single_digit

    ; We didn't branch, so it's between 10 and 15.
    add     $37
    out     (0), A
    ret

print_hex_single_digit:
    add     A, $30
    out     (0), A
    ret

    ; PRINT writes a string to the serial line.
    ; The TOS is the address of the string to write,
    ; and NOS is the size.
    DEFCODE "PRINT", 5, PRINT
    pop     HL
    pop     BC
    ld      B, C
    call    _PRINT
    NEXT

_PRINT:
_PRINT_loop:
    ld      A, (HL)
    out     (0), A
    inc     HL
    djnz    _PRINT_loop

_PRINT_done:
    ret

    DEFCODE "CR", 2, CR
    call    _NEWLINE
    NEXT

_NEWLINE:
    ld      A, $0d
    out     (0), A
    ld      A, $0a
    out     (0), A
    ret

    DEFCODE "DUMPSTACK", 9, DUMPSTACK
    ld      (_mem_SCRATCH), SP
    ld      IY, (_mem_SCRATCH)

    ld      L, (IY)
    ld      H, (IY+1)

    call    _DOT
    call    _NEWLINE

    ld      L, (IY+2)
    ld      H, (IY+3)

    call    _DOT
    call    _NEWLINE

    ld      L, (IY+4)
    ld      H, (IY+5)

    call    _DOT
    call    _NEWLINE

    NEXT

    ; WORD gets a space-delimited word from the input stream.
    DEFCODE "WORD", 4, WORD
    call    _WORD           ; Call the WORD subroutine.
                            ; HL will hold the base address, and B the size.

    ld      C, B            ; Push size in lower half of BC.
    ld      B, 0
    push    BC

    push    HL              ; Push base address.
    NEXT

_WORD:
_WORD_skip:
    call    _KEY
    cp      ' '             ; Is this a space?
    jp      z, _WORD_skip   ; Loop until it's not.

    ld      HL, _mem_WORD   ; Load start address of buffer
                            ; into HL.
    
    ld      (HL), A         ; Store first character in the buffer.
    inc     HL

    ld      B, 1            ; Initialize B, which will hold the size.
                            ; We initialize to 1 because we've already consumed
                            ; one character of the word.

_WORD_get:
    call    _KEY
    cp      ' '             ; Is this a space?
    jp      z, _WORD_done   ; If so, we're done.

    ld      (HL), A         ; If not, store the character
    inc     HL              ; and move onto the next location.
    inc     B

    jp      _WORD_get       ; Repeat.

_WORD_done:
    ld      HL, _mem_WORD
    ret

    ; NUMBER parses a number out of the given string.
    DEFCODE "NUMBER", 6, NUMBER
    pop     BC              ; Size.
    pop     HL              ; Base address of string.
    ld      B, C

    call    _NUMBER

    push    HL              ; Returned value.
    NEXT

_NUMBER:
    push    DE              ; Save DE, BC so we don't trash them.
    push    BC

    ld      D, 0            ; Initialize to 0.
    ld      E, 0

    ; Make sure that B is not greater than 4.
    ld      A, B
    cp      5
    jp      m, _NUMBER_size_ok
    ld      B, 4

    jp      _NUMBER_loop_start

_NUMBER_size_ok:
_NUMBER_loop:
    ; We've read in a character, so we want to shift
    ; DE left four times to make space for the next character.
    ; Unfortunately the Z80 has no 16-bit shift so we have to implement
    ; it ourselves!

    ld      A, D

    ; Do the shift four times.
    repeat  4

    ; Shift A left.
    sla     A

    ; Shift E left. Top bit will be in carry flag.
    sla     E

    ; Add carry flag to A.
    adc     A, 0

    endrepeat

    ld      D, A

_NUMBER_loop_start
    ; Read in a character and convert into a number in the range
    ; 0-15.
    ld      A, (HL)
    inc     HL
    call    _NUMBER_get_digit

    cp      $ff
    jp      nz, _NUMBER_valid

    pop     BC
    pop     DE

    ld      A, $ff
    ret

_NUMBER_valid

    ; OR with E. Move result back into E.
    or      A, E
    ld      E, A

    djnz    _NUMBER_loop    ; Repeat if we've not read all the characters.

    ; At this point our number should be in DE.
    ; Move it into HL and restore DE.
    ld      H, D
    ld      L, E

    pop     BC
    pop     DE

    ld      A, $00
    ret

    ; Given a hexadecimal digit in A,
    ; return its value in A.
_NUMBER_get_digit:
    cp      $3a
    jp      m, _NUMBER_get_digit_1
    cp      $47
    jp      m, _NUMBER_get_digit_2

    cp      $61
    jp      m, _NUMBER_get_digit_invalid
    ; In range 'a'-'f'. Subtract $57.
    sub     A, $57
    ret

_NUMBER_get_digit_1:
    cp      $30
    jp      m, _NUMBER_get_digit_invalid
    ; In range '0'-'9'. Subtract $30.
    sub     A, $30
    ret

_NUMBER_get_digit_2:
    cp      $41
    jp      m, _NUMBER_get_digit_invalid
    ; In range 'A'-'F'. Subtract $37.
    sub     A, $37
    ret

_NUMBER_get_digit_invalid:
    ld      A, $ff
    ret

    DEFCODE "STRING", 6, STRING
    call    _STRING
    ld      C, B
    ld      B, 0
    push    BC
    push    HL
    NEXT

_STRING:
    ld      HL, _mem_STRING
    ld      B, 0
_STRING_start:
    call    _KEY
    cp      '"'
    jp      nz, _STRING_start

    ; Now the string has started.
_STRING_get:
    call    _KEY

    ; Is this the end of the string?
    cp      '"'
    jp      z, _STRING_done

    ; If not, store this character.
    ld      (HL), A
    inc     HL
    inc     B

    jp      _STRING_get

_STRING_done:
    ld      HL, _mem_STRING
    ret

; These are compilation-related words.
    DEFCODE ":", 1, COLON
    ; Put the interpreter into COMPILE mode.
    ld      HL, var_STATE
    set     0, (HL)

    ; Save DE because we're going to use it.
    push    DE

    ; Set up the dictionary link pointer.
    ld      HL, (var_HERE)
    ld      DE, (var_LATEST)

    ; Save the new link pointer.
    ld      (var_LATEST), HL

    ; Store the old link pointer in the first two bytes
    ; of this new word.
    ld      (HL), E
    inc     HL
    ld      (HL), D
    inc     HL

    ; Save HL because it'll be trashed by WORD.
    push    HL

    ; Get the word name and size.
    call    _WORD

    ; Restore the old value of HL, which points to HERE.
    pop     DE

    push    HL
    push    BC

    ; Store the size.
    ld      A, B
    ld      (DE), A
    inc     DE

    ; At this point, DE points to the location
    ; of the first character of the name, and HL
    ; points to the WORD buffer. B holds the size.
    ; Zero C and we can use a block transfer instruction.
    ld      C, B
    ld      B, 0
    ldir

    ; Now DE points to the codeword.
    ; For now just give a dummy codeword.
    ld      HL, _COLON_DUMMY
    ld      C, 19
    ldir

    ld      (var_HERE), DE

    pop     BC
    pop     HL
    call    _PRINT

    pop     DE

    NEXT

_COLON_DUMMY:
    ld      A, '!'
    out     (0), A
    out     (0), A
    out     (0), A
    ld      A, ' '
    out     (0), A
    NEXT

    DEFCODE "FIND", 4, FIND
    pop     BC              ; Size
    pop     HL              ; Start address.

    ld      B, C

    call    _FIND

    push    HL

    NEXT

_FIND:
    push    DE              ; Save DE so we don't trash it.
    push    HL              ; Save HL.
    push    BC              ; Save B.

    ; Load address of latest entry in dictionary.
    ld      IY, (var_LATEST)

_FIND_loop:
    push    IY
    pop     HL

    pop     BC
    push    BC

    ld      A, (IY+2)       ; Size is 2 bytes into the entry.
    cp      B               ; Compare this size to the one given in B.

    jp      z, _FIND_test   ; If they're equal, we've potentially found something.
    jp      _FIND_next      ; Otherwise skip onto the next one.

_FIND_test:
    pop     BC              ; Restore B and HL and save them again.
    pop     HL
    push    HL
    push    BC

    push    IY
    pop     DE
    INC2    DE              ; This is IY+3.
    inc     DE

_FIND_test_loop:
    ld      A, (DE)         ; Load the character of the name into A.
    ld      C, (HL)         ; Load the character of the test string into D.
    cp      C               ; Compare.

    jp      nz, _FIND_next

    inc     DE
    inc     HL

    djnz    _FIND_test_loop

    ; If we get here then we've tested each character of the name
    ; and found them to be the same.
    ; This means we've found the word!
    ;
    ; At this point DE should point to the codeword.
_FIND_found:
    ; First restore the stuff on the stack.
    pop     BC

    ; We don't want to actually restore HL,
    ; just increment the SP.
    ;
    ; Move the codeword into HL.
    pop     HL

    ld      H, D
    ld      L, E

    pop     DE
    
    ret

_FIND_next:
    ld      L, (IY)         ; Load address of next word in little-endian format.
    ld      H, (IY+1)

    ; Fail if the address is 0.
    ld      A, L
    cp      H
    jp      nz, _FIND_not_zero
    cp      0
    jp      nz, _FIND_not_zero

    jp      _FIND_not_found

_FIND_not_zero:

    push    HL
    pop     IY

    jp      _FIND_loop

_FIND_not_found:
    pop     BC
    pop     HL
    pop     DE

    ld      H, 0
    ld      L, 0
    ret

    DEFCODE "LIT", 3, LIT
    ; At this point DE points to the literal to be pushed.
    push    DE
    pop     IY

    ld      L, (IY)
    ld      H, (IY+1)

    push    HL

    ; Skip over the literal so we don't execute it!
    INC2    DE

    NEXT

    DEFCODE "BRANCH", 6, BRANCH
    ; At this point DE points 2 ahead of the codeword
    ; of BRANCH, handily to the branch offset.
    ;
    ; Move it into IY so we can load the branch offset.
    push    DE
    pop     IY

    ld      L, (IY)
    ld      H, (IY+1)

    ; Decrement DE twice because it previously pointed
    ; two bytes ahead of the BRANCH.
    DEC2    DE

    ; Now add the offset.
    adc     HL, DE
    ld      D, H
    ld      E, L

    ; Execute next word, now that we've updated DE.
    NEXT

    DEFCODE "EXIT", 4, EXIT
    POPRSP
    NEXT

    DEFCODE "INTERPRET", 9, INTERPRET
    ; Pop string address and size 
    pop     HL
    pop     BC
    ld      B, C

    ; Save HL because it will be overwritten by FIND.
    push    HL

    call    _FIND

    ld      A, L
    cp      0
    jp      nz, _INTERPRET_found
    ld      A, H
    cp      0
    jp      nz, _INTERPRET_found

_INTERPRET_number:
    ; Restore HL.
    pop     HL
    push    HL

    call    _NUMBER

    cp      $00
    jp      z, _INTERPRET_number_valid

    ; NUMBER has returned an error code, so this isn't
    ; a number but an undefined word.
    ld      A, '?'
    out     (0), A

    pop     HL
    call    _PRINT

    ld      A, ' '
    out     (0), A
    NEXT

_INTERPRET_number_valid:
    INC2    SP
    push    HL
    NEXT

_INTERPRET_found:
    ; We can discard the saved value of HL.
    INC2    SP

    ; Execute the found word!
    jp      (HL)

    DEFCODE "END", 3, END
    halt

    DEFWORD "GOODBYE", 7, GOODBYE
    addr    LIT
    addr    8
    addr    LIT
    addr    str_GOODBYE

    addr    PRINT
    addr    CR

    addr    END

    DEFWORD "QUIT", 4, QUIT
    ; Get a word and interpret it.
    addr    WORD
    addr    INTERPRET

    ; And repeat!
    addr    BRANCH
    addr    -4

; Start of readonly data.
str_ZFORTH_prompt:
    text    "ZFORTH> "
str_GOODBYE:
    text    "GOODBYE!"

; Start of data initializers.
DATA_LOAD_START:

load_LATEST:
    addr    LINK
load_KEY_HEAD:
    addr    _mem_KEY
load_KEY_TAIL:
    addr    _mem_KEY
load_STATE:
    byte    $00
load_HERE:
    addr    FREE_START

DATA_LOAD_END:

DATA_SIZE set DATA_LOAD_END - DATA_LOAD_START



; Start of RAM.
    org     $8000

; Initialized data.
DATA_START:

var_LATEST:
    blk     2
var_KEY_HEAD:
    blk     2
var_KEY_TAIL:
    blk     2
var_STATE:
    blk     1
var_HERE:
    blk     2

; Uninitialized data.
_mem_KEY:
    blk     256
_mem_KEY_END:
_mem_WORD:
    blk     256
_mem_SCRATCH:
    blk     256
_mem_STRING:
    blk     256

FREE_START: