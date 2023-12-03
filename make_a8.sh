
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
        -o obj/empty24.bin \
        data/empty24.inc

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/empty53.bin \
        data/empty53.inc

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

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/DLIST.bin \
        data/DLIST.inc

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/GPAK.bin \
        data/GPAK.inc

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/TITLE.bin \
        data/TITLE.inc

# -------------------------------------

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/boot2.bin \
        --list=obj/boot2_a8.lst \
        --labels=obj/boot2_a8.lbl \
        boot2.asm

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/cdraw.bin \
        --list=obj/cdraw_a8.lst \
        --labels=obj/cdraw_a8.lbl \
        cdraw.asm

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
        -o obj/platform_a8.bin \
        --list=obj/platform_a8.lst \
        --labels=obj/platform_a8.lbl \
        platform_a8.asm

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
        -o obj/swap.bin \
        --list=obj/swap_a8.lst \
        --labels=obj/swap_a8.lbl \
        swap.asm

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/tst.bin \
        --list=obj/tst_a8.lst \
        --labels=obj/tst_a8.lbl \
        tst.asm

64tass  --m65xx \
        --atari-xex \
        --nostart \
        -o obj/wire.bin \
        --list=obj/wire_a8.lst \
        --labels=obj/wire_a8.lbl \
        wire.asm


# -------------------------------------


cat     obj/atr_header.bin \
        obj/boot2.bin \
                        obj/empty24.bin \
        obj/GPAK.bin \
        obj/cdraw.bin \
        obj/TITLE.bin \
        obj/DLIST.bin \
        obj/platform_a8.bin \
        obj/swap.bin \
        obj/tst.bin \
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
        data/DEMO3a.dat \
        data/DEMO4.dat \
        data/DEMO5.dat \
        data/DEMO3b.dat \
                        obj/empty279_fill.bin \
        >obj/pinballCS.atr
