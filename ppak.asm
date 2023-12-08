
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


; PINBALL CONSTRUCTION KIT
; LOW LEVEL


                .include "equates/gpak.equ"
                .include "equates/cdraw.equ"


; CONSTANTS

POLYGON         = 1
BPOLYGON        = 2
LIBOBJ          = 3

; GENERAL PARAM SPACE

PARAM           = $C0

; PURE TEMPS

TEMP            = PARAM+7
XTEMP           = TEMP+1
YTEMP           = XTEMP+1
TEMP2           = YTEMP+1

; LOCALS FOR DRAWBITS

BASE1           = $D0
BASE2           = BASE1+2
TEMPBITS        = BASE2+2
VERT            = TEMPBITS+2
HDIV8           = VERT+1
HMOD8           = HDIV8+1
WIDTH           = HMOD8+1
HEIGHT          = WIDTH+1

; LOCALS FOR RECTANGLES

LFTDIV8         = BASE2+3
LEFTMASK        = LFTDIV8+3
RTDIV8          = LEFTMASK+2
RIGHTMASK       = RTDIV8+3

; LOCALS FOR SCAN CONVERSION

X1              = $D0
Y1              = X1+1
X2              = Y1+1
Y2              = X2+1
DX              = Y2+1
DY              = DX+1
DXCOEFF         = DY+1
DXFRACT         = DXCOEFF+1
DXCODE          = DXFRACT+1
QUOT            = DXCODE+1
DVSR            = QUOT+2
DVDND           = DVSR+2
SCANLINE        = DVDND+2
NEXTSTART       = SCANLINE+1
STARTCOUNT      = NEXTSTART+1
FIRSTACTV       = STARTCOUNT+1
LASTLINK        = FIRSTACTV+1
WRAPPOINT       = LASTLINK+1
HCNT            = WRAPPOINT+1
HCNT2           = HCNT+1

; MAIN PROGRAM LOCALS, GLOBALS

CURSOR          = $80
cursorY         = CURSOR+2
cursorX_Div8    = cursorY+1
cursorX_Mod8    = cursorX_Div8+1
CURSORHEIGHT    = cursorX_Mod8+1
CURSORWIDTH     = CURSORHEIGHT+1
NEWCURSORXDIV8  = CURSORWIDTH+1
NEWCURSORXMOD8  = NEWCURSORXDIV8+1
NEWCURSORY      = NEWCURSORXMOD8+1
NEWITEM         = NEWCURSORY+1
lastItem        = NEWITEM+2

; OBJECT PROCESSING STUFF

OBJ             = lastItem+2
objNext         = OBJ+2
objCount        = objNext+1
PLYPTRX         = objCount+1
PLYPTRY         = PLYPTRX+2
objID           = PLYPTRY+2
FILLCOLOR       = objID+1
VRTXCOUNT       = FILLCOLOR+1
LBASE           = VRTXCOUNT+1
scanMode        = LBASE+2
MEMBTM          = scanMode+1
MIDBTM          = MEMBTM+2
MIDTOP          = MIDBTM+2
MIDY            = MIDTOP+2
COLOR           = MIDY+1

; TEMPORARY DATA STRUCTURE

ANEXT           = $400
ANVRTX          = ANEXT+8
AYFLG           = ANVRTX+8
AXCOEFF         = AYFLG+8
AXFRACT         = AXCOEFF+8
ADXCOEFF        = AXFRACT+8
ADXFRACT        = ADXCOEFF+8
ADXCODE         = ADXFRACT+8

NVRTX           = $440                ;USER AREA
NDXCOEFF        = NVRTX+64
NDXFRACT        = NDXCOEFF+64
NDXCODE         = NDXFRACT+64
DXBUFR          = NDXCODE+64

; PINBALL DATA STRUCTURE

PBDATA          = $4B1C
PBDX            = $7A40

; EXTERNAL CALLS AND ADDRESSES

LO              = MOD8+$100
HI              = LO+$C0
;DrawBits        = SetMode+$25
;XOffDraw        = DrawBits+$41
;MASKS           = XOffDraw+$9C
;HorzLine        = MASKS+$11
;VertLine        = HorzLine+$20
;FrameRectangle  = VertLine+$58
;Drawrectangle   = FrameRectangle+$28
;InRectangle     = Drawrectangle+$10
;GetButtons      = InRectangle+$2D
;InitCursor      = GetButtons+$B
;XDrawCursor     = InitCursor+$14
;UpdateCursor    = XDrawCursor+$7
;DoCursorX       = UpdateCursor+$1E
;GetCursorX      = DoCursorX+$17
;DoCursorY       = GetCursorX+$10
;WAIT            = DoCursorY+$3
;CursorInRectangle      = WAIT+$C
;DoMenu          = CursorInRectangle+$13
;SELECT          = DoMenu+$54
;INIT            = SELECT+$40
;MoveUp          = INIT+$35
;MoveDown        = MoveUp+$3B
;AddiYX          = MoveDown+$39
;AddYX           = AddiYX+$E
;SubtractiYX     = AddYX+$10
;SubtractYX      = SubtractiYX+$12
;CMPYX           = SubtractYX+$10


;--------------------------------------
;--------------------------------------
                * = $9920
;--------------------------------------

;======================================
; POLYGON SCAN CONVERSION,
; PINBALL DATA BASE CONSTRUCTION
;======================================
DrawDisplay     ldy PBDATA              ; GET OBJ COUNT
                sty objCount            ; INIT INTERNAL DB

                jsr GetObj

                lda OBJ
                sta MEMBTM
                sta MIDBTM

                lda OBJ+1
                sta MEMBTM+1
                sta MIDBTM+1

                lda #<PBDX
                sta MIDTOP
                lda #>PBDX
                sta MIDTOP+1

                lda #1
                sta MIDY

                lda #0
                ldy #192
_next1          sta PBDX-1,Y

                dey
                bne _next1

                jsr GetObj
_next2          jsr DrawObj

                inc objNext
                ldy objNext

                jsr GetObjNext

                ldy objNext
                cpy objCount
                bne _next2

                rts


;======================================
;
;======================================
GetObj          lda #<PBDATA+1
                sta OBJ
                lda #>PBDATA+1
                sta OBJ+1

                sty objNext

                ldy #0

                ;[fall-through]


;======================================
;
;======================================
GetObjNext      lda PBDATA,Y
                clc
                adc OBJ
                sta OBJ
                lda #0
                adc OBJ+1
                sta OBJ+1

                cpy objNext
                beq _1

                iny
                bne GetObjNext

_1              lda scanMode
                and #$80
                sta scanMode

                ldy #0
                lda (OBJ),Y             ; GET OBJECT ID
                sta objID

                cmp #<POLYGON
                bne _2

; GET A POLYGON: F,V,X'S,Y'S

_next1          iny
                lda (OBJ),Y
                sta FILLCOLOR

                iny
                lda (OBJ),Y
                sta VRTXCOUNT

                lda #3
                ldy #<OBJ
                ldx #<PLYPTRX
                jsr AddiYX

                lda VRTXCOUNT
                ldy #<PLYPTRX
                ldx #<PLYPTRY
                jmp AddiYX

_2              cmp #<BPOLYGON
                bne _3

                lda #$40
                ora scanMode
                sta scanMode
                bne _next1

_3              jsr _next1

                lda VRTXCOUNT
                ldy #<PLYPTRY
                ldx #<LBASE
                jmp AddiYX


;======================================
;
;======================================
DrawObj         lda objID
                cmp #<LIBOBJ+1
                bcs SCANOUT._XIT

                cmp #<LIBOBJ
                bne _XIT

                lda LBASE
                ldx LBASE+1
                jsr XOffDraw

_XIT            jmp SCANPOLY


;--------------------------------------
;
;--------------------------------------
SCANPOLY        bit scanMode
                bmi _1

                jsr GETMINY

                iny
                jsr MakeHole

_1              lda objID
                cmp #<LIBOBJ
                beq _2

                ldy FILLCOLOR
                bne _2

                jsr PolygonPoints
_2              jsr PROCESSPOLY

; THE ACTIVE LIST IS EMPTY, SO SKIP TO NEXT START

                ldx NEXTSTART
                bmi SCANOUT

                lda AYFLG,X             ; SKIP TO FIRST Y
                sta SCANLINE

                ;[fall-through]


;--------------------------------------
; ADD ALL EDGES STARTING AT SCANLINE
;--------------------------------------
ADDSTARTS       ldx NEXTSTART
                bmi SCANPLY2

_next1          lda AYFLG,X
                cmp SCANLINE
                bne ADDSTARTS9

; SWITCH FROM START RECORD TO ACTIVE RECORD

                ldy ANVRTX,X
                lda (PLYPTRY),Y
                sta AYFLG,X             ; CHANGE FROM START-Y TO END-Y

                lda ANEXT,X             ; SAVE THIS
                sta TEMP

; INSERT IN ACTIVE LIST (SORTED BY X)

                lda FIRSTACTV
                bmi _next4

_next2          tay
                lda AXCOEFF,X
                cmp AXCOEFF,Y
                bcc ADDSTARTS7
                beq ADDSTARTS6

_next3          sty YTEMP

                lda ANEXT,Y
                bpl _next2

                sta ANEXT,X             ; END OF LIST
                txa
                sta ANEXT,Y
                bpl ADDSTARTS8

_next4          sta ANEXT,X             ; HEAD OF LIST
                stx FIRSTACTV

                jmp ADDSTARTS8


;--------------------------------------
;
;--------------------------------------
SCANOUT         clc
_XIT            rts


;--------------------------------------
;
;--------------------------------------
ADDSTARTS6      lda ADXFRACT,X
                sec
                sbc ADXFRACT,Y

                lda ADXCOEFF,X
                sbc ADXCOEFF,Y
                bvc _1

                lda ADXCOEFF,X
_1              bpl ADDSTARTS._next3

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
ADDSTARTS7      tya                     ; AHEAD OF Y
                cpy FIRSTACTV
                beq ADDSTARTS._next4

                sta ANEXT,X

                ldy YTEMP
                txa
                sta ANEXT,Y

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
ADDSTARTS8      ldx TEMP
                bpl ADDSTARTS._next1

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
ADDSTARTS9      stx NEXTSTART

                ;[fall-through]


;--------------------------------------
; NOW SCAN CONVERT ACTIVE EDGES
;--------------------------------------
SCANPLY2        lda #0
                sta HCNT

                lda FIRSTACTV
                bmi SCANOUT

_next1          tax

; CHECK FOR EDGES WHICH CHAIN
; OR TERMINATE

                lda AYFLG,X
                cmp SCANLINE
                bne _3

                ldy ANVRTX,X            ; TERMINATE?
                lda NVRTX,Y
                bpl _2

                cpx FIRSTACTV           ; YES, REMOVE FROM
                beq _1                  ; ACTIVE LIST

                ldy XTEMP               ; MID-LIST
                lda ANEXT,X
                sta ANEXT,Y

                jmp _4

_1              lda ANEXT,X             ; HEAD-LIST
                sta FIRSTACTV
                bcs _4

_2              sta ANVRTX,X            ; NO, CHAIN

                lda NDXCOEFF,Y
                sta ADXCOEFF,X

                lda NDXFRACT,Y
                sta ADXFRACT,X

                lda NDXCODE,Y
                sta ADXCODE,X

                lda (PLYPTRX),Y
                sta AXCOEFF,X

                lda #0
                sta AXFRACT,X
                ldy ANVRTX,X
                lda (PLYPTRY),Y
                sta AYFLG,X

; NOW SCAN CONVERT THE EDGE

_3              lda AXFRACT,X
                clc
                adc ADXFRACT,X
                sta AXFRACT,X

                lda AXCOEFF,X
                sta TEMP

                adc ADXCOEFF,X
                sta AXCOEFF,X

                clc
                adc TEMP
                ror
                ldy HCNT
                sta (MIDBTM),Y

                iny
                lda objNext
                sta (MIDBTM),Y

                lda ADXCODE,X
                sta DXBUFR,Y

                iny
                sty HCNT

                stx XTEMP
                lda ANEXT,X
_4              bpl _next1

; GET READY FOR NEXT SCANLINE

                jsr DOSCAN

                inc SCANLINE

                jmp ADDSTARTS


;======================================
; TRAVERSE THE EDGE LIST OF
; A POLYGON TO SET UP SCAN
; CONVERSION
;======================================
PROCESSPOLY     ldy #0
                sty STARTCOUNT
                sty LASTLINK

                lda (PLYPTRY),Y
                sta Y1
                lda (PLYPTRX),Y
                sta X1

                iny
                sty WRAPPOINT

; MAKE SURE V(WRAP-1) DOESN'T
; GET CLASSIFIED AS A START

                sec
                php
_next1          plp
                jsr PROCESSVERTEX

                php

                lda (PLYPTRX),Y
                sta X1
                lda (PLYPTRY),Y
                sta Y1

                iny
                cpy WRAPPOINT
                beq _2

                cpy VRTXCOUNT
                bne _1

                ldy #0
_1              jmp _next1

; WRAP TO MAKE SURE V(N-1) WAS
; CLASSIFIED CORRECTLY

_2              plp
                jsr PROCESSVERTEX

                ;[fall-through]


;--------------------------------------
; FINALLY, SORT THE NEW STARTS
;--------------------------------------
SORTSTARTS      ldx #0
                stx NEXTSTART

                lda #$80
                sta ANEXT               ;END OF LIST MARKER
                sta FIRSTACTV

_next1          inx
                cpx STARTCOUNT
                beq _XIT

                lda NEXTSTART
_next2          tay
                lda AYFLG,X
                cmp AYFLG,Y
                bcs _2

                tya
                sta ANEXT,X

                cpy NEXTSTART
                beq _1

                ldy YTEMP
                txa
                sta ANEXT,Y

                jmp _next1

_1              stx NEXTSTART
                bcs _next1

_2              sty YTEMP

                lda ANEXT,Y
                bpl _next2

                sta ANEXT,X

                txa
                sta ANEXT,Y
                bpl _next1

_XIT            rts


;======================================
; PROCESS A VERTEX, BASED ON
; THE PREVIOUS 2 VERTICES
;======================================
PROCESSVERTEX   php

                lda (PLYPTRX),Y
                sta X2

                sec
                sbc X1
                sta DX

                php

                lda (PLYPTRY),Y
                sta Y2

; SKIP ANY HORIZONTAL EDGES
;  THAT FOLLOW

                sty TEMP
_next1          iny
                cpy VRTXCOUNT
                bcc _1

                ldy #0
_1              cmp (PLYPTRY),Y
                beq _next1

                dey
                bpl _2

                ldy VRTXCOUNT
                dey

_2              sec
                sbc Y1
                sta DY
                bcc _5

; V2.Y > V3.Y: CALCULATE DX,
;  RECALL V1--V2

                plp
                jsr DIVIDE

                plp
                bcc _3

; CHAIN: V1.Y > V2.Y > V3.Y

                ldx LASTLINK
                tya
                sta NVRTX,X

                lda DXCOEFF
                sta NDXCOEFF,X

                lda DXFRACT
                sta NDXFRACT,X

                lda DXCODE
                sta NDXCODE,X
                sty LASTLINK

                rts

; START: V1.Y < V2.Y > V3.Y

_3              ldx STARTCOUNT
                cpx #8
                bcc _4

                jmp ABORTSCAN

_4              sty YTEMP

                tya
                sta ANVRTX,X

                lda X1
                sta AXCOEFF,X

                lda DXCOEFF
                sta ADXCOEFF,X

                lda DXFRACT
                sta ADXFRACT,X

                lda DXCODE
                sta ADXCODE,X

                ldy LASTLINK
                lda NVRTX,Y
                sta ANVRTX+1,X

                lda (PLYPTRX),Y
                sta AXCOEFF+1,X

                lda NDXCOEFF,Y
                sta ADXCOEFF+1,X

                lda NDXFRACT,Y
                sta ADXFRACT+1,X

                lda NDXCODE,Y
                sta ADXCODE+1,X

                lda Y1
                sta AYFLG,X
                sta AYFLG+1,X

                lda #0
                sta AXFRACT,X
                sta AXFRACT+1,X

                inx
                inx
                stx STARTCOUNT

                ldy YTEMP
                sty LASTLINK

                sec
                rts

; V2.Y < V3.Y: CALCULATE -DX,
;  AND RECALL V1--V2

_5              eor #$FF
                adc #1
                sta DY

                clc
                lda DX
                eor #$FF
                adc #1
                sta DX

                plp
                bcs _6

                sec
                bcs _7

_6              clc

_7              jsr DIVIDE
                sty YTEMP

                ldy TEMP
                plp
                bcc _8

; TERMINAL: V1.Y > V2.Y < V3.Y

                ldx LASTLINK
                lda #$80
                sta NVRTX,X

; CHAIN: V1.Y < V2.Y < V3.Y

_8              lda LASTLINK
                sta NVRTX,Y

                lda DXCOEFF
                sta NDXCOEFF,Y

                lda DXFRACT
                sta NDXFRACT,Y

                lda DXCODE
                sta NDXCODE,Y

                sty LASTLINK

                ldy YTEMP
                clc
                rts


;======================================
;
;======================================
DIVIDE          lda DX
                bne _1

                sta DXCOEFF
                sta DXFRACT

                lda #8
                sta DXCODE

                rts

_1              php
                bcs _2

                eor #$FF
                adc #1

_2              tax
                lda DY
                beq _3

                sta DVSR+1
                stx DVDND+1

                jsr QDIV

_3              stx DXCOEFF
                sta DXFRACT

                cpx #16
                bcc _4

                lda #0
                beq _6

_4              cpx #1
                bcc _5

                lda DXCODESA,X          ; SLOPES < 1
                bne _6

_5              lsr
                lsr
                lsr
                lsr

                tax
                lda DXCODESB,X          ; SLOPES > 1
_6              sta DXCODE

                plp
                bcs _XIT

                lda #17                 ;C=0!
                sbc DXCODE
                sta DXCODE

                lda DXFRACT
                eor #$FF
                adc #0                  ;C=1!
                sta DXFRACT

                lda DXCOEFF
                eor #$FF
                adc #0
                sta DXCOEFF

_XIT            rts

;--------------------------------------

DXCODESA        .byte $08,$0C,$0C,$0D,$0D,$0E,$0E,$0E
                .byte $0E,$0F,$0F,$0F,$0F,$0F,$0F,$0F
DXCODESB        .byte $08,$09,$09,$0A,$0A,$0B,$0B,$0B
                .byte $0B,$0B,$0C,$0C,$0C,$0C,$0C,$0C


;======================================
; QUICK 16 X 8 BIT DIVIDE
;======================================
QDIV            lda #0                  ; ZERO QUOTIENT
                sta QUOT
                sta QUOT+1
                sta DVSR
                sta DVDND

                ldx #8
                lda DVSR+1              ; PRE-SCALE DIVISOR
_next1          inx
                asl
                bcs _1

                cmp DVDND+1
                bcc _next1

                clc
_1              ror
                sta DVSR+1

                dex
                bne _2

_next2          lda DVDND
                sbc DVSR
                sta DVDND
                lda DVDND+1
                sbc DVSR+1
                sta DVDND+1

_next3          rol QUOT
                rol QUOT+1

                dex
                bmi _3

                lsr DVSR+1
                ror DVSR

_2              lda DVDND+1
                cmp DVSR+1
                bcc _next3
                bne _next2

                lda DVDND
                cmp DVSR
                bcs _next2
                bcc _next3              ; [unc]

_3              lda QUOT
                ldx QUOT+1

                rts


;======================================
; SCAN/MERGE/DRAW ROUTINES
;======================================
DOSCAN          ldy SCANLINE
                lda LO,Y
                sta BASE1
                lda HI,Y
                sta BASE1+1

                ldy HCNT
                beq _2

                bit scanMode
                bvc _next1

                ldx #159
                bne _1

_next1          dey
                dey
                bmi _2

                lda (MIDBTM),Y
                tax
_1              dey
                dey

                lda (MIDBTM),Y
                sty YTEMP

                jsr DOBAR

                ldy YTEMP
                cpy #2
                bne _next1

; MUST BE B-POLY, FINISH UP

                dey
                dey
                lda (MIDBTM),Y
                tax

                lda #0
                jsr DOBAR

_2              bit scanMode
                bmi _XIT

                ldy HCNT
                beq _XIT

                tya
                ldx SCANLINE
                clc
                adc PBDX,X
                php

                sta PBDX,X

                lda MIDTOP
                sec
                sbc MIDBTM

                tax
                lda MIDTOP+1
                sbc MIDBTM+1
                beq _3

                ldx #$80                ; 128-32 > 32
_3              txa
                sbc HCNT
                cmp #32
                php

_next2          dey
                lda DXBUFR,Y
                asl
                asl
                asl
                asl
                ora DXBUFR-2,Y
                sta (MIDBTM),Y

                dey
                dey
                dey
                bne _next2

                lda HCNT
                ldy #<MIDBTM
                ldx #<MIDBTM
                jsr AddiYX

; PBDB IS IN NORMAL CONDITION

                plp
                bcc ABORTSCAN._ENTRY1

                plp
                bcs ABORTSCAN._ENTRY2

                ldx SCANLINE
                cpx #191
                bcs _XIT

                inc MIDY

                ldy PBDX+1,X
                beq _XIT

_next3          dey
                lda (MIDTOP),Y
                sta (MIDBTM),Y

                tya
                bne _next3

                lda PBDX+1,X
                sta TEMP

                ldx #<MIDBTM
                ldy #<MIDBTM
                jsr AddiYX

                lda TEMP
                ldx #<MIDTOP
                ldy #<MIDTOP
                jsr AddiYX

_XIT            rts


;--------------------------------------
;
;--------------------------------------
ABORTSCAN       pla
_ENTRY1         pla
_ENTRY2         pla
                pla
                sec
                rts


;======================================
; DRAW A COLOR BAR FROM A TO X
;======================================
DOBAR           stx TEMP
                cmp TEMP
                bcc _1

                tax
                lda TEMP
_1              tay

                lda DIV8,Y
                sta LFTDIV8

                lda MOD8,Y
                tay

                lda MASKS,Y
                eor #$FF
                sta LEFTMASK

                lda DIV8,X
                sta RTDIV8

                lda MOD8,X
                tax

                lda MASKS+1,X
                sta RIGHTMASK

                lda LEFTMASK
                ldy LFTDIV8
                cpy RTDIV8
                bne _2

                eor RIGHTMASK
                eor #$FF
                bne _3

_next1          lda #$FF
_2              and FILLCOLOR
                eor (BASE1),Y
                sta (BASE1),Y

                iny
                cpy RTDIV8
                bcc _next1

                lda RIGHTMASK

_3              and FILLCOLOR
                eor (BASE1),Y
                sta (BASE1),Y

                rts


;======================================
; ALIGN POLY SO FIRST EDGE HAS
;  NEGATIVE SLOPE
;======================================
AlignPolygon    lda #0
                sta YTEMP

_next1          ldy #0
                lda (PLYPTRY),Y
                iny
                cmp (PLYPTRY),Y
                bcc _XIT1

                inc YTEMP
                ldx YTEMP
                cpx VRTXCOUNT
                beq _XIT

                dey
                lda (PLYPTRY),Y
                pha
                lda (PLYPTRX),Y
                pha

                iny
_next2          lda (PLYPTRY),Y
                tax
                lda (PLYPTRX),Y
                dey
                sta (PLYPTRX),Y
                txa
                sta (PLYPTRY),Y

                iny
                iny
                cpy VRTXCOUNT
                bne _next2

                dey
                pla
                sta (PLYPTRX),Y
                pla
                sta (PLYPTRY),Y
                bne _next1

_XIT1           clc
_XIT            rts


;======================================
; FIND ABSOLUTE ADDRESS OF
;  SCANLINE
;======================================
GETSCAN         sty YTEMP
                cpy MIDY
                bcc _1
                bne _2

_1              lda MIDBTM
                sta TEMP
                lda MIDBTM+1
                sta TEMP+1

_next1          lda TEMP                ; GET ADDR OF Y
                sec
                sbc PBDX,Y
                sta TEMP

                lda TEMP+1
                sbc #0
                sta TEMP+1

                iny
                cpy MIDY
                bcc _next1
                beq _next1

_next2          ldy YTEMP
                rts

_2              lda MIDTOP
                sta TEMP
                lda MIDTOP+1
                sta TEMP+1

_next3          dey
                cpy MIDY
                beq _next2

                lda TEMP
                clc
                adc PBDX,Y
                sta TEMP
                lda TEMP+1
                adc #0
                sta TEMP+1
                bcc _next3


;======================================
;
;======================================
GETMINY         ldy #0
                lda (PLYPTRY),Y
                iny
_next1          cmp (PLYPTRY),Y
                bcc _1

                lda (PLYPTRY),Y
_1              iny
                cpy VRTXCOUNT
                bne _next1

                tay
                rts


;======================================
; PREPARE HOLE IN PB-TABLE
;======================================
MakeHole        jsr GETSCAN

                dey                     ; Y>0
                cpy MIDY
                sty MIDY
                bcs _1

                lda #<MIDBTM            ; MOVE CHUNK
                ldx #<TEMP
                ldy #<MIDTOP
                jsr MoveUp

                ldy #<TEMP
                ldx #<MIDBTM
                jsr SubtractYX          ; MIDBTM = DIFF

                ldy #<MIDBTM
                ldx #<MIDTOP
                jsr SubtractYX

                lda TEMP                ; TEMP -> NEW BTM
                sta MIDBTM
                lda TEMP+1
                sta MIDBTM+1

                rts

_1              lda #<TEMP
                ldx #<MIDTOP
                ldy #<MIDBTM
                jsr MoveDown            ; MOVE CHUNK

                ldx #<MIDTOP
                ldy #<TEMP
                jsr SubtractYX          ; MIDTOP = -DIFF

                ldx #<MIDBTM
                ldy #<MIDTOP
                jsr SubtractYX

                lda TEMP
                sta MIDTOP
                lda TEMP+1
                sta MIDTOP+1

                rts


;======================================
; WHICH POLYGON IS THE USER
;  SELECTING?
;======================================
SelectPolygon   ldy cursorY
                lda PBDX,Y
                beq _5

                sta HCNT

                jsr GETSCAN
                jsr GetCursorX
                sta YTEMP

                ldy HCNT
_next1          dey
                dey
                cmp (TEMP),Y
                beq _1
                bcs _3

_1              dey
                dey
                cmp (TEMP),Y
                bcc _4

                iny
                lda (TEMP),Y
                beq _2                  ; B-POLY

                sec
                rts

_2              iny
                lda YTEMP

_3              dey
                dey
_4              cpy #0
                bne _next1
_5              clc
                rts


;======================================
; DRAW A POLY'S VERTICES
;======================================
PolygonPoints   ldy #0
_next1          lda (PLYPTRY),Y
                sec
                sbc #1
                sta POINT+2

                lda (PLYPTRX),Y
                sec
                sbc #1

                tax
                lda DIV8,X
                sta POINT+3             ; XDIV8
                lda MOD8,X
                sta POINT+4

                sty YTEMP
                lda #<POINT
                ldx #>POINT
                jsr XOffDraw

                ldy YTEMP
                iny
                cpy VRTXCOUNT
                bne _next1

                rts

;--------------------------------------

POINT           .addr POINT+7
                .byte $00,$00,$00,$03,$01
                .byte $E0,$E0,$E0


;======================================
; REMOVE A POLY FROM THE DB
;======================================
RemovePolygon   jsr GETMINY
                jsr MakeHole

                ldx MIDY
_next1          inx
                cpx #191
                bcs _3
                lda PBDX,X
                beq _3

                lsr
                lsr
                sta HCNT

                ldy #0
                sty HCNT2

_next2          ldy #1
                lda (MIDTOP),Y
                cmp objNext
                beq _1

                sta (MIDBTM),Y          ; PRESERVE IT

                iny
                lda (MIDTOP),Y
                sta (MIDBTM),Y

                iny
                lda (MIDTOP),Y
                sta (MIDBTM),Y

                ldy #0
                lda (MIDTOP),Y
                sta (MIDBTM),Y

                lda MIDBTM              ; ADD 4 TO MIDBTM
                clc
                adc #4
                sta MIDBTM
                lda MIDBTM+1
                adc #0
                sta MIDBTM+1
                bne _2

_1              inc HCNT2

_2              lda MIDTOP
                clc
                adc #4
                sta MIDTOP
                lda MIDTOP+1
                adc #0
                sta MIDTOP+1

                dec HCNT
                bne _next2

                lda HCNT2
                beq _4

                asl
                asl
                sta TEMP

                lda PBDX,X
                sec
                sbc TEMP
                sta PBDX,X

                jmp _next1

_3              dex
_4              stx MIDY

                rts


;======================================
;
;======================================
GetBounds       ldy #0
                lda (PLYPTRY),Y
                sta PARAM
                sta PARAM+1

                lda (PLYPTRX),Y
                sta PARAM+2
                sta PARAM+3

                iny
_next1          lda (PLYPTRY),Y
                cmp PARAM
                bcs _1

                sta PARAM               ;MINY

_1              cmp PARAM+1
                bcc _2

                sta PARAM+1

_2              lda (PLYPTRX),Y
                cmp PARAM+2
                bcs _3

                sta PARAM+2             ;MINX

_3              cmp PARAM+3
                bcc _4

                sta PARAM+3

_4              iny
                cpy VRTXCOUNT
                bne _next1

                rts


;--------------------------------------
;--------------------------------------     HACK:

                .byte $B4,$03,$90,$02,$85
                .byte $03,$C8,$C4,$9B,$D0
                .byte $DF,$60,$D8,$A5,$1D
                .byte $E5,$1B,$85,$1D
