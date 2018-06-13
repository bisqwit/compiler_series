// common.inc
// ==========
//
// Copyright 2018, Marcus Rowe <undisbeliever@gmail.com>
// Distributed under The MIT License: https://opensource.org/licenses/MIT


#ifndef _COMMON_INC_
#define _COMMON_INC_ 1


#define a8  as
#define a16 al
#define i8  xs
#define i16 xl


// VRAM Map

#define BG1_MAP         0x0400
#define BG2_MAP         0x0800
#define BG3_MAP         0x0000
#define BG1_TILES       0x2000
#define BG2_TILES       0x2000
#define BG3_TILES       0x1000
#define OBJ_TILES       0x6000

#define VRAM_TEXT_MAP   BG3_MAP
#define VRAM_TEXT_TILES BG3_TILES

.text
.link page $80
.data
.link page $80
.zero
.link page $80
.bss
.link page $80

#endif // _COMMON_INC_

// vim: ft=asm:


