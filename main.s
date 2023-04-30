    processor 6502
    ; DONE 0. Function to draw square
    ; DONE 1. Frame
    ; 2. I shape and rotation data
    ;   a. field memory representation
    ;   b. function to draw field on screen
    ;   c. shape memory representation
    ;   d. rotation memory representation
    ;   e. function to draw current shape at current location
    ; 3. Drop I to bottom
    ; 4. Shift
    ; 5. Rotate
    ; 6. Top out
    ; 7. Clear
    ; 8. Other shapes with random selection
    ; 9. Level up/speed up
    ; 10. Soft drop
    ; 11. Hard drop
    ;
    ; Field is 10x20

    ; TO RUN ON APPLE II:
    ; BLOAD A
    ; CALL 2051


    org $0803  ; default program load location.
    jmp start


; my constants
fieldbottom = #$9e
framebottom = fieldbottom + 1
fieldleft = #$5b   ; 91 - evenly divisible by 7 so it's byte aligned
frameleft = fieldleft - 1
; not a coincidence that this is the number of
; screen pixels per byte. Square-drawing code assumes this,
; so this cannot be changed without rewriting that code.
squaresize = #$07
fieldwidth = #$0A  ; 10 squares
fieldheight = #$14  ; 20 squares
fieldtop = framebottom - (squaresize * fieldheight) + 1
frametop = fieldtop - 1
fieldright = frameleft + (squaresize * fieldwidth) - 1
frameright = fieldright + 1
emptyrowleft = #$80  ; 1000 0000
emptyrowright = #$3f  ; 0011 1111


; my memory locations
; these temp vars are only to be used in leaf routines (with no JSRs)
temp1 equ $06
temp2 = $07
temp3 = $08
temp4 = $09
curshape = $eb
curshapex = $ec
curshapey = $ed
fieldptr = $ee
fieldptrhi = $ef
squaremask = #$3f  ; for turning on screen byte except rightmost pixel
bytemask = #$7f  ; for turning on an entire screen byte
fieldmapptrhi = #$80  ; fieldmap starts at $8000
fieldmapptrlo = #$00


initfieldmap
    lda #fieldmapptrlo
    sta fieldptr
    lda #fieldmapptrhi
    sta fieldptrhi
    ldx #fieldheight + 4  ; add 4 extra rows above top of screen
    ldy #$00
initfieldmaploopleft
    lda #emptyrowleft
    sta (fieldptr),y
    iny
    lda #emptyrowright
    sta (fieldptr),y
    iny
    dex
    bne initfieldmaploopleft
    rts


drawsquare
; draws square at x = x, y = y

; these numbers are in FIELD COORDINATES with origin at lower left

; the top row and rightmost column are not filled, to create a
; "grid" effect between squares.

; SCREEN COORDINATE CALCS:
; Squaresize = the number of horizontal dots per memory byte,
; and fieldleft is byte-aligned.
; Therefore, given a start-of-row byte, to
; to find the byte for the Xth square in that row, the calculation is:
; start-of-row address + byte offset of fieldleft
;                      + byte offset of (X * squaresize)
; = start-of-row address + (fieldleft / 7) + X
;
; Squaresize = 7. Therefore, to find the start-of-row byte for
; the Yth square, the calc is:
; yaddress(fieldbottom - (7 * Y))
; = yaddress(fieldbottom - ((8 - 1) * Y))
; = yaddress(fieldbottom - (8*Y - Y))
; = yaddress(fieldbottom - ((Y << 3) - Y))
;
; Calculate start-of-row byte for bottom row of square (smart version)
    tya
    asl
    asl
    asl
    sty temp1
    sec
    sbc temp1
    ; negate A
    eor #$ff
    ldy #$01
    sty temp1
    clc
    adc temp1
    clc
    adc #fieldbottom
    ; now A contains the y coordinate of the bottom
    ; row of the square. Store it so that we can
    ; decrement it later to draw the higher rows
    ; of the square
    sta temp2
    ; Next, calculate the offset from the start-of-row byte.
    lda #fieldleft / 7
    stx temp1
    clc
    adc temp1
    sta temp1
    lda #squaresize - 1
    sta temp3
    ; temp1 = offset to add to start-of-row byte
    ; temp2 = y coordinate of bottom row
    ; temp3 = row counter
    ldy temp1
squareloop
    lda temp2
    jsr yaddress
    lda #squaremask
    sta (gbas),y
    dec temp2
    dec temp3
    bne squareloop
    rts






;external routines
wait    equ $fb1e
hcolor  equ $f6f0
hgr     equ $f3e2
hplot   equ $f457
hposn   equ $f411
hlin    equ $f53a
shnum   equ $f730
draw    equ $f601
delay   equ $fca8


;memory locations
gbas equ $26
hpag equ $e6


start   jsr hgr
        ldx     #$03  ; white
        jsr     hcolor

        ; border
        ; bottom left
        ldx #frameleft
        ldy #$0
        lda #framebottom
        jsr hplot
        ; bottom right
        lda #frameright % #$0100
        ldx #frameright / #$0100
        ldy #framebottom
        jsr hlin
        ; top right
        lda #frameright % #$0100
        ldx #frameright / #$0100
        ldy #frametop
        jsr hlin
        ; top left
        lda #frameleft
        ldx #$0
        ldy #frametop
        jsr hlin
        ; bottom left
        lda #frameleft
        ldx #$0
        ldy #framebottom
        jsr hlin

        ; scale line (remove later)
        ldx #fieldleft
        ldy #$0
        lda (#framebottom + #frametop) / 2
        jsr hplot
        lda #fieldleft + #squaresize - 1
        ldx #$0
        ldy (#framebottom + #frametop) / 2
        jsr hlin

        ; example of coloring a single byte
        lda (#frametop + #framebottom) / 2 + 2
        jsr yaddress
        lda #bytemask  ; all dots on
        ldy #fieldleft / 7
        sta (gbas),y

        ; example of drawing squares 0,0 and 1,1
        ldx #$00
        ldy #$00
        jsr drawsquare
        ldx #$01
        ldy #$01
        jsr drawsquare

        ; initialize field map
        jsr initfieldmap
    rts

; utilities

; calculate base address for y-coord in A, store in gbas
yaddress
    pha
    and #$C0
    sta gbas
    lsr
    lsr
    ora gbas
    sta gbas
    pla
    sta gbas+1
    asl
    asl
    asl
    rol gbas+1
    asl
    rol gbas+1
    asl
    ror gbas
    ;
    lda gbas+1
    and #$1f
    ora hpag
    sta gbas+1
    rts
