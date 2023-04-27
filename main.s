    processor 6502
    ; 0. Function to draw square
    ; 1. Frame
    ; 2. I shape and rotation data
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
; so this cannot be change without rewriting that code.
squaresize = #$07
framewidth = #$0A  ; 10 squares
frameheight = #$14  ; 20 squares
fieldtop = framebottom - (squaresize * frameheight) + 1
frametop = fieldtop - 1
fieldright = frameleft + (squaresize * framewidth) - 1
frameright = fieldright + 1

drawsquare
; draws square at x = x, y = y
; these numbers are in FIELD COORDINATES with origin at lower left
; L = f


;external routines
wait    equ $fb1e
hcolor  equ $f6f0
hgr     equ $f3e2
hplot   equ $f457
hposn   equ $f411
hlin    equ $f53a
shnum   equ $f730
draw    equ $f601


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

        lda (#frametop + #framebottom) / 2 + 2
        jsr yaddress
        lda #$7F  ; all dots on
        ldy #fieldleft / 7
        sta (gbas),y

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
