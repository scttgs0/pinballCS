
# SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
# SPDX-PackageOriginator: BudgeCo: Bill Budge
# SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
# SPDX-License-Identifier: MIT

# SPDX-FileCopyrightText: Copyright 2023 Scott Giese


mkdir -p obj/

# -------------------------------------

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/empty53.bin \
        data/empty53.inc

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/empty138.bin \
        data/empty138.inc

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/empty279_fill.bin \
        data/empty279_fill.inc

# -------------------------------------

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/atr_header.bin \
        data/atr_header.inc

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/BITMAPS.bin \
        data/BITMAPS.inc

# -------------------------------------

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/disk.bin \
        --list=obj/disk_a8.lst \
        --labels=obj/disk_a8.lbl \
        disk.asm

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/edit.bin \
        --list=obj/edit_a8.lst \
        --labels=obj/edit_a8.lbl \
        edit.asm

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/ppak.bin \
        --list=obj/ppak_a8.lst \
        --labels=obj/ppak_a8.lbl \
        ppak.asm

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/run.bin \
        --list=obj/run_a8.lst \
        --labels=obj/run_a8.lbl \
        run.asm

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/run2.bin \
        --list=obj/run2_a8.lst \
        --labels=obj/run2_a8.lbl \
        run2.asm

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/wire.bin \
        --list=obj/wire_a8.lst \
        --labels=obj/wire_a8.lbl \
        wire.asm


# -------------------------------------


cat     obj/atr_header.bin \
                        obj/empty138.bin \
                        obj/empty53.bin \
        obj/BITMAPS.bin \
        obj/run.bin \
        obj/ppak.bin \
        obj/edit.bin \
        obj/wire.bin \
        obj/disk.bin \
        obj/run2.bin \
        data/VTOC.dat \
        data/DEMO1.dat \
        data/DEMO2.dat \
        data/DEMO3.dat \
        data/DEMO4.dat \
        data/DEMO5.dat \
                        obj/empty279_fill.bin \
        >obj/pinballCS.atr
