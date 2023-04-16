`   processor 6502
    ORG $0803
    LDA #$CF
    JSR $FDED
    LDA #$CB
    JSR $FDED
    LDA #$8D
    JSR $FDED
    JSR $FBDD
    JSR $FBDD
    JSR $FBDD
    RTS

