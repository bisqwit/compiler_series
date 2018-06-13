//
// SNES Register Defines.
//
// Names and flags taken from Anomie's SNES Documents
//
//
// Copyright 2014 - 2018, Marcus Rowe <undisbeliever@gmail.com>
// Distributed under The MIT License: https://opensource.org/licenses/MIT


#ifndef _REGISTERS_INC_
#define _REGISTERS_INC_ 1

#define REGISTER_DB $80

// Tile Map Format
//
// cccccccc
// vhopppcc
//
// cccccccccc = character of tile
// ppp        = palette
// o          = order
// h          = horizontal flip
// v          = vertical flip

// Flags and Masks
#define TILEMAP_CHAR_MASK    $03ff
#define TILEMAP_PALETTE_MASK $1c00
#define TILEMAP_ORDER_FLAG   $2000
#define TILEMAP_H_FLIP_FLAG  $4000
#define TILEMAP_V_FLIP_FLAG  $8000

// Flags and Masks (high byte only)
#define TILEMAP_HIGH_CHAR_MASK    $03
#define TILEMAP_HIGH_PALETTE_MASK $1c
#define TILEMAP_HIGH_ORDER_FLAG   $20
#define TILEMAP_HIGH_H_FLIP_FLAG  $40
#define TILEMAP_HIGH_V_FLIP_FLAG  $80

// Shifts
#define TILEMAP_HIGH_PALETTE_SHIFT  2
#define TILEMAP_PALETTE_SHIFT      10
#define TILEMAP_HIGH_ORDER_SHIFT    5
#define TILEMAP_ORDER_SHIFT        13



// OAM Table 0 Format
//
// xxxxxxxx
// yyyyyyyy
// cccccccc
// vhoopppN
//

// Flags and Masks
#define OAM_ATTR_TABLE_FLAG   $01
#define OAM_ATTR_PALETTE_MASK $0e
#define OAM_ATTR_ORDER_MASK   $30
#define OAM_ATTR_H_FLIP_FLAG  $40
#define OAM_ATTR_V_FLIP_FLAG  $80

#define OAM_CHARATTR_TABLE_FLAG   $0100
#define OAM_CHARATTR_PALETTE_MASK $0e00
#define OAM_CHARATTR_ORDER_MASK   $3000
#define OAM_CHARATTR_H_FLIP_FLAG  $4000
#define OAM_CHARATTR_V_FLIP_FLAG  $8000

// Shifts
#define OAM_ATTR_PALETTE_SHIFT   1
#define OAM_ATTR_ORDER_SHIFT     4

#define OAM_CHARATTR_PALETTE_SHIFT   9
#define OAM_CHARATTR_ORDER_SHIFT     12


// Screen Display
// (write, any)
//
// f---bbbb
//
// f    = Force Blank
// bbbb = Screen Brightness, 0 = Dull, 16 = max
#define INIDISP $2100

// Flags and Masks
#define INIDISP_FORCE           $80
#define INIDISP_BRIGHTNESS_MASK $0f



// Object Size and Character Address
// (write, v-blank)
//
// sssnnbbb
//
// sss = Object Size
// nn  = Name Select
// bbb = Name Base Address (addr>>14)
#define OBSEL $2101

// Masks
#define OBSEL_SIZE_MASK        $e0
#define OBSEL_NAME_MASK        $18
#define OBSEL_BASE_MASK        $07

// Shifts
#define OBSEL_SIZE_SHIFT 5
#define OBSEL_NAME_SHIFT 3

// Object Sizes.
#define OBSEL_SIZE_8_16        $00
#define OBSEL_SIZE_8_32        $20
#define OBSEL_SIZE_8_64        $40
#define OBSEL_SIZE_16_32       $60
#define OBSEL_SIZE_16_64       $80
#define OBSEL_SIZE_32_64       $a0
//; Undocumented
#define OBSEL_SIZE_16X32_32X64 $c0
//; Undocumented
#define OBSEL_SIZE_16X32_32X32 $e0

// Object Tile Address segments sizes.
// The VRAM address used is 16384 * `bbb`
#define OBSEL_BASE_WALIGN  8192
#define OBSEL_BASE_ALIGN  16384

#define OBSEL_VAL(addr, size) \
    ((addr / OBSEL_BASE_WALIGN) | (OBSEL_SIZE_##size))



// OAM Address and Priority
// (write, v-blank)
//
// p------b aaaaaaaa
//
// p        = Obj priority bit
// b        = Table access (0 = Low Table, 1 = High Table)
// aaaaaaaa = Address
#define OAMADDL $2102
#define OAMADDH $2103

// Flags and Masks
#define OAMADDL_ADDRESS_MASK  $ff
#define OAMADDH_TABLE_MASK    $01

// Sprite Priority Select
// When set, `OAMADDL` handles sprite priority.
#define OAMADDH_PRIORITY_FLAG $80

// Table Select
#define OAMADDH_TABLE_LOW     $00
#define OAMADDH_TABLE_HIGH    $01



// Word Access of OAMADDL/OAMADDH
// (write, word, v-blank)
#define OAMADD $2102

// Masks
#define OAMADD_TABLE_MASK       $0100
#define OAMADD_ADDRESS_MASK     $00ff

// Sprite Priority Select
// When set, `OAMADDL` handles sprite priority.
#define OAMADD_PRIORITY_FLAG    $8000

// Table Select
#define OAMADD_TABLE_LOW        $0000
#define OAMADD_TABLE_HIGH       $0100



// OAM Data Write
// (write, write twice, v-blank)
#define OAMDATA $2104



// BG Mode and Character Size
// (write, h-blank, v-blank)
//
// DCBAemmm
//
// A/B/C/D = Character size for BG1 - BG4 (1 = 16x16 tiles, 0 = 8x8 tiles)
// e       = Mode 1 BG3 Priority
// mmm     = BG Mode
#define BGMODE $2105

// Masks
#define BGMODE_SIZE_MASK          $f0
#define BGMODE_MODE_MASK          $07

// MODES
#define BGMODE_MODE0              $00
#define BGMODE_MODE1              $01
#define BGMODE_MODE1_BG3_PRIORITY $09
#define BGMODE_MODE2              $02
#define BGMODE_MODE3              $03
#define BGMODE_MODE4              $04
#define BGMODE_MODE5              $05
#define BGMODE_MODE6              $06
#define BGMODE_MODE7              $07

// BG Character Size
#define BGMODE_SIZE_BG1_8         $00
#define BGMODE_SIZE_BG1_16        $10
#define BGMODE_SIZE_BG2_8         $00
#define BGMODE_SIZE_BG2_16        $20
#define BGMODE_SIZE_BG3_8         $00
#define BGMODE_SIZE_BG3_16        $40
#define BGMODE_SIZE_BG4_8         $00
#define BGMODE_SIZE_BG4_16        $80



// Screen Pixelation
// (write, h-blank, v-blank)
//
// xxxxDCBA
//
// A/B/C/D = Affect BG1 - BG4
// xxxx    = pixel size (0 to 16)
#define MOSAIC $2106

// Masks
#define MOSAIC_PIXEL_MASK  $f0
#define MOSAIC_BG_MASK     $0f

// Left shift for pixel size
#define MOSAIC_PIXEL_SHIFT 4

// BGs to Affect
#define MOSAIC_BG1 $01
#define MOSAIC_BG2 $02
#define MOSAIC_BG3 $04
#define MOSAIC_BG4 $08



// BG Tilemap Address and Size
// (write, v-blank)
//
// aaaaaass
//
// aaaaaa = Tilemap base address in VRAM (Addr>>10)
// ss     = Tilemap size
#define BG1SC $2107
#define BG2SC $2108
#define BG3SC $2109
#define BG4SC $210A

// Masks
#define BGXSC_BASE_MASK     $fc
#define BGXSC_MAP_SIZE_MASK $03

// Shifts
#define BGXSC_BASE_SHIFT 2

// Sizes
#define BGXSC_SIZE_32X32 $00
#define BGXSC_SIZE_64X32 $01
#define BGXSC_SIZE_32X64 $02
#define BGXSC_SIZE_64X64 $03

// Address segments sizes.
// The VRAM address used is 2048 * `aaaaaa`
#define BGXSC_BASE_WALIGN 1024
#define BGXSC_BASE_ALIGN  2048


#define BGXSC_VAL(addr, size) \
    (((addr / BGXSC_BASE_WALIGN) << BGXSC_BASE_SHIFT) | BGXSC_SIZE_##size)



// BG1/2 Character Address
// (write, v-blank)
//
// bbbbaaaa
//
// aaaa - Base address for BG1 (Addr>>13)
// bbbb - Base address for BG2 (Addr>>13)
#define BG12NBA $210B

// Masks
#define BG12NBA_BG1_MASK $0f
#define BG12NBA_BG2_MASK $f0

// Shifts
#define BG12NBA_BG2_SHIFT 4

// Address segments sizes.
// The VRAM address used is 8192 * `aaaa`
#define BG12NBA_BASE_WALIGN 4096
#define BG12NBA_BASE_ALIGN  8192


#define BGXXNBA_VAL(addr, addr2) \
    ((addr / BG12NBA_BASE_WALIGN) | ((addr2 / BG12NBA_BASE_WALIGN) << 4))


// BG 3/4 Character Address
// (write, v-blank)
//
// bbbbaaaa
//
// aaaa - Base address for BG3 (Addr>>13)
// bbbb - Base address for BG4 (Addr>>13)
#define BG34NBA $210C

// Masks
#define BG34NBA_BG3_MASK $0f
#define BG34NBA_BG4_MASK $f0

// Shifts
#define BG34NBA_BG4_SHIFT 4

// Address segments sizes.
// The VRAM address used is 8192 * `aaaa`
#define BG34NBA_BASE_WALIGN 4096
#define BG34NBA_BASE_ALIGN  8192



// BG Horizontal Scroll
// (write, write twice, h-blank, v-blank)
//
// ------xx xxxxxxxx
//
// x = BG Horizontal offset
#define BG1HOFS $210D
#define BG2HOFS $210F
#define BG3HOFS $2111
#define BG4HOFS $2113

// Mode 7 BG Horizontal Scroll
// (write, write twice, h-blank, v-blank)
//
// ---mmmmm mmmmmmmm
//
// m = Mode 7 BG Offset (M7HOFS only)
#define M7HOFS $210D

// BG Vertical Scroll
// (write, write twice, h-blank, v-blank)
//
// ------yy yyyyyyyy
//
// y = BG Vertical offset
#define BG1VOFS $210E
#define BG2VOFS $2110
#define BG3VOFS $2112
#define BG4VOFS $2114

// Mode 7 BG Vertical Scroll
// (write, write twice, h-blank, v-blank)
//
// ---mmmmm mmmmmmmm
//
// m = Mode 7 BG1 Offset (M7VOFS only)
#define M7VOFS $210E



// Video Port Control
// (write, v-blank)
//
// i---mmii
//
// i  = Address increment mode
// ii = Address increment amount
// mm = Address remapping
//
// According to the manual only `ii` or `mm` should be set.
#define VMAIN $2115

// Masks
#define VMAIN_INCREMENT_MODE_MASK   $80
#define VMAIN_INCREMENT_AMOUNT_MASK $03
#define VMAIN_REMAP_MASK            $0c

// Increment after writing $2118 or reading $2139
#define VMAIN_INCREMENT_LOW  $00
// Increment after writing $2119 or reading $213A (recommended)
#define VMAIN_INCREMENT_HIGH $80

// Increment Amount
#define VMAIN_INCREMENT_1    $00
#define VMAIN_INCREMENT_32   $01
#define VMAIN_INCREMENT_128  $02

// Address remapping
#define VMAIN_REMAP_FOR_32  $04
#define VMAIN_REMAP_FOR_64  $08
#define VMAIN_REMAP_FOR_128 $0c



// VRAM Word Address
// (write, word, v-blank)
#define VMADD  $2116
#define VMADDL $2116
#define VMADDH $2117



// VRAM Data Write
// (write, word, v-blank)
#define VMDATA  $2118
#define VMDATAL $2118
#define VMDATAH $2119


// Mode 7 Select
// (write, v-blank)
//
// oo----vh
//
// oo = Out of screen area
//        00 - Screen Repeat
//        01 - Color 0
//        10 - Tile 0
// h  = horizontal flip
// v  = vertical flip
#define M7SEL $211A

// Out of screen area
#define M7SEL_OUTSIDE_REPEAT  $00
#define M7SEL_OUTSIDE_COLOR0  $80
#define M7SEL_OUTSIDE_TILE0   $c0

// Horizontal Flip
#define M7SEL_HORIZONTAL_FLIP $01

// Vertical flip
#define M7SEL_VERTICAL_FLIP   $02



// Mode 7 Matrix
// (write, write twice, 1:7:8 signed fixed point, h-blank, v-blank)
//
// The Matrix transformation is:
//    [ X ]   [ M7A  M7B ]   [ SX + M7HOFS - M7X ]   [ M7X ]
//    [ Y ] = [ M7C  M7D ] * [ SY + M7VOFS - M7Y ] + [ M7Y ]
//
// Where SX and SY are screen coordinates
#define M7A $211B
#define M7B $211C
#define M7C $211D
#define M7D $211E

// Mode 7 Center
// (write, write twice, 13 bit signed integer, h-blank, v-blank)
#define M7X $211F
#define M7Y $2120


// CGRAM Address
// (write, h-blank, v-blank)
#define CGADD $2121

// Starting color of the OAM palette
#define CGADD_OBJ_PALETTE 128


// CGRAM Data Write
// (write, write twice, h-blank, v-blank)
//
// -bbbbbgg gggrrrrr
#define CGDATA $2122

// Masks
#define CGDATA_BLUE_MASK  $7c00
#define CGDATA_GREEN_MASK $03e0
#define CGDATA_RED_MASK   $001f

// Shifts
#define CGDATA_BLUE_SHIFT  10
#define CGDATA_GREEN_SHIFT  5



// Window Mask Settings
// (write, h-blank, v-blank)
//
// For each nibble:
//   EIei
//
//   e = Enable Window 1
//   i = Window 1 In/Out (1 = Inside, 0 = Outside)
//   E = Enable Window 2
//   I = Window 2 In/Out (1 = Inside, 0 = Outside)
//
#define W12SEL  $2123
#define W34SEL  $2124
#define WOBJSEL $2125

// Enable
#define WSEL_WIN1_ENABLE   $2
#define WSEL_WIN2_ENABLE   $8

// Window In/Out
#define WSEL_WIN1_INSIDE   $0
#define WSEL_WIN1_OUTSIDE  $1
#define WSEL_WIN2_INSIDE   $0
#define WSEL_WIN2_OUTSIDE  $4

// Masks
#define W12SEL_BG1_MASK    $0f
#define W12SEL_BG2_MASK    $f0
#define W34SEL_BG3_MASK    $0f
#define W34SEL_BG4_MASK    $f0
#define WOBJSEL_OBJ_MASK   $0f
#define WOBJSEL_COLOR_MASK $f0

// Shifts
#define W12SEL_BG1_SHIFT    0
#define W12SEL_BG2_SHIFT    4
#define W34SEL_BG3_SHIFT    0
#define W34SEL_BG4_SHIFT    4
#define WOBJSEL_OBJ_SHIFT   0
#define WOBJSEL_COLOR_SHIFT 4


// Window 1 Left Position
// (write, h-blank, v-blank)
#define WH0 $2126

// Window 1 Right Position
// (write, h-blank, v-blank)
#define WH1 $2127

// Window 2 Left Position
// (write, h-blank, v-blank)
#define WH2 $2128

// Window 2 Right Position
// (write, h-blank, v-blank)
#define WH3 $2129



// Window mask logic for Backgrounds
// (write, h-blank, v-blank)
//
// 44332211
//
// For each bit pair:
//   00 = OR
//   01 = AND
//   10 = XOR
//   11 = XNOR
#define WBGLOG $212A

// Logic
#define WBGLOG_OR   $0
#define WBGLOG_AND  $1
#define WBGLOG_XOR  $2
#define WBGLOG_XNOR $3

// Masks
#define WBGLOG_BG1_MASK $03
#define WBGLOG_BG2_MASK $0c
#define WBGLOG_BG3_MASK $30
#define WBGLOG_BG4_MASK $c0

// Shifts
#define WBGLOG_BG1_SHIFT 0
#define WBGLOG_BG2_SHIFT 2
#define WBGLOG_BG3_SHIFT 4
#define WBGLOG_BG4_SHIFT 6



// Window mask logic for Objects and Color Window
// (write, h-blank, v-blank)
//
// ----ccoo
//
// For each bit pair:
//   00 = OR
//   01 = AND
//   10 = XOR
//   11 = XNOR
#define WOBJLOG $212B

// Logic
#define WOBJLOG_OR   $0
#define WOBJLOG_AND  $1
#define WOBJLOG_XOR  $2
#define WOBJLOG_XNOR $3

// Masks
#define WOBJLOG_BG1_MASK $03
#define WOBJLOG_BG2_MASK $0c
#define WOBJLOG_BG3_MASK $30
#define WOBJLOG_BG4_MASK $c0

// Shifts
#define WOBJLOG_OBJ_SHIFT   0
#define WOBJLOG_COLOR_SHIFT 2



// Main Screen Designation
// (write, h-blank, v-blank)
//
// ----o4321
//
// 1/2/3/4/o - enable BG1 - BG4 & Objects for the main screen
#define TM $212C

// layers to enable
#define TM_BG1 $01
#define TM_BG2 $02
#define TM_BG3 $04
#define TM_BG4 $08
#define TM_OBJ $10


// Sub Screen Designation
// (write, h-blank, v-blank)
//
// ----o4321
//
// 1/2/3/4/o - enable BG1 - BG4 & Objects for the sub screen
#define TS $212D

// layers to enable
#define TS_BG1 $01
#define TS_BG2 $02
#define TS_BG3 $04
#define TS_BG4 $08
#define TS_OAJ $10

// Window Mask Designation for the Main Screen
// (write, h-blank, v-blank)
//
// ----o4321
//
// 1/2/3/4/o - enable window masking for BG1-BG4 & Objects on the main screen
#define TMW $212E

// layers to enable
#define TMS_BG1 $01
#define TMS_BG2 $02
#define TMS_BG3 $04
#define TMS_BG4 $08
#define TMS_OAM $10

// Window Mask Designation for the Sub Screen
// (write, h-blank, v-blank)
//
// ----o4321
//
// 1/2/3/4/o - enable window masking for BG1 - BG4 & Objects for the sub screen
#define TSW $212F

// layers to enable
#define TSW_BG1 $01
#define TSW_BG2 $02
#define TSW_BG3 $04
#define TSW_BG4 $08
#define TSW_OAM $10



// Color Addition Select
// (write, h-blank, v-blank)
//
// ccmm--sd
//
// cc = clip color
//       00 - Never
//       01 - Outside color window
//       10 - Inside color window
//       11 - always
//
// mm = prevent color math
//       00 - Never
//       01 - Outside color window
//       10 - Inside color window
//       11 - always
//
// s  = Add subscreen enable
// d  = Direct color mode for 256 color BG
#define CGWSEL $2130

// Flags
#define CGWSEL_ADD_SUBSCREEN_FLAG $02
#define CGWSEL_DIRECT_COLOR_FLAG  $01

// Masks
#define CGWSEL_CLIP_MASK    $c0
#define CFWSEL_PREVENT_MASK $30

// Shifts
#define CGWSEL_CLIP_SHIFT    6
#define CGWSEL_PREVENT_SHIFT 4

// Clips
#define CGWSEL_CLIP_NEVER   $00
#define CGWSEL_CLIP_INSIDE  $40
#define GGWSEL_CLIP_OUTSIDE $80
#define CGWSEL_CLIP_ALWAYS  $c0

// Prevent Color Math
#define CGWSEL_PREVENT_NEVER   $00
#define CGWSEL_PREVENT_INSIDE  $10
#define GGWSEL_PREVENT_OUTSIDE $20
#define CGWSEL_PREVENT_ALWAYS  $30



// Color Math Designation
// (write, h-blank, v-blank)
//
// shbo4321
//
// s           = add/subtract colors (0 = add, 1 = subtract)
// h           = Half color math
// 4/3/2/1/o/b = Enable color math for BG1 - BG4, Objects and Backdrop
#define CGADSUB $2131

// Color Add/Subtract
#define CGADSUB_COLOR_ADD       $00
#define CGADSUB_COLOR_SUBRACT   $80

// Half Color Math
#define CGADSUB_HALF_COLOR      $40

// Enable
#define CGADSUB_ENABLE_BG1      $01
#define CGADSUB_ENABLE_BG2      $02
#define CGADSUB_ENABLE_BG3      $04
#define CGADSUB_ENABLE_BG4      $08
#define CGADSUB_ENABLE_OBJ      $10
#define CGADSUB_ENABLE_BACKDROP $20



// Fixed Color Data
// (write, h-blank, v-blank)
//
// bgrccccc
//
// b/g/r = color plane(s)
// ccccc = color intensity
#define COLDATA $2132

// Masks
#define COLDATA_PLANE_MASK       $e0
#define COLDATA_INTENSITY_MASK   $1f

// Planes
#define COLDATA_PLANE_BLUE_FLAG  $80
#define COLDATA_PLANE_GREEN_FLAG $40
#define COLDATA_PLANE_RED_FLAG   $20



// Screen Mode/ Video Select
// (write, h-blank, v-blank)
//
// se--poIi
//
// s = External Sync
// e = Enable Mode 7 EXTBG
// p = Enable Pseudo-hires mode
// o = Enable overscan
// I = Oject Interlace
// i = Screen Interlace
#define SETINI $2133

// Flags
#define SETINI_EXTSYNC          $80
#define SETINI_EXTBG            $40
#define SETINI_PSUEDO_HIRES     $08
#define SETINI_OVERSCAN         $04
#define SETINI_OBJ_INTERLACE    $02
#define SETINI_SCREEN_INTERLACE $01



// Multiplication Result
// (read, long, h-blank, v-blank)
//
// Two's compliment product of 16 bit `M7A` and the
// most recent byte written to `M7B`.
//
// Should not be read during Mode 7 rendering.
#define MPY  $2134
#define MPYL $2134
#define MPYM $2135
#define MPYH $2136



// Software Latch for H/V Counter
// (read, any)
//
// See `OPHCT` and `OPVCT`
#define SLHV $2137



// Data for OAM Read
// (read, read twice, h-blank, v-blank)
#define OAMDATAREAD $2138



// VRAM Data Read
// (read, word, v-blank)
#define VMDATAREAD  $2139
#define VMDATALREAD $2139
#define VMDATAHREAD $213A



// CGRAM Data Read
// (read, v-blank)
#define CGDATAREAD $213B



// Horizontal Scanline Location
// (read, read twice, any)
//
// --------x xxxxxxxx
//
// Notes:
//   * Latched on `SLHV` read or Pin 6 of Port 2 (bit 6 `WRIO`) falling trigger.
//   * Range 0 to 339
//   * Visible range: 22 - 277
//   * Bits 1-7 of high byte are open bus. Should mask bits 0-8.
//   * High/Low order reset on `STAT78` read.
#define OPHCT $213C

// Vertical Scanline Location
// (read, read twice, any)
//
// --------y yyyyyyyy
//
// Notes:
//   * Latched on `SLHV` read or Pin 6 of Port 2 (bit 6 `WRIO`) falling trigger.
//   * Range 0 to 261 NTSC (+1 when interlacing)
//   * Range 0 to 312 PAL (+1 when interlacing)
//   * Visible range 1 - 224, 1 - 239? on overscan.
//   * Bits 1-7 of high byte are open bus. Should mask bits 0-8.
//   * High/Low order reset on `STAT78` read.
#define OPVCT $213D



// PPU 5c77 Status Flag and Version
// (read, any)
//
// trm-vvvv
//
// t    = time over flag (more than 34 sprite tiles on a scanline)
// r    = Range overflow (more than 32 sprites on a scanline)
// m    = master/slave select. Unknown function
// vvvv = 5c77 chip number.
#define STAT77 $213E

// Masks
#define STAT77_VERSION_MASK $0f

// Flags
#define STAT77_TIME_FLAG    $80
#define STAT77_RANGE_FLAG   $40



// PPU 5c88 Status Flag and Version
// (read, any)
//
// fl-pvvvv
//
// f    = Interlace flag (toggled every v-blank)
// l    = External Latch Flag (set when PPU counters latched)
// p    = NTSC/PAL mode (1 = PAL, 0 = NTSC)
// vvvv = 5c78 chip number.
#define STAT78 $213F

// Masks
#define STAT78_VERSION_MASK   $0f
#define STAT78_PAL_MASK       $10

#define STAT78_PAL            $10
#define STAT78_NTSC           $00

// Flags
#define STAT78_INTERLACE_FLAG $80
#define STAT78_EXT_LATCH_FLAG $40



// APU I/O Register
// (read/write, any)
#define APUIO0 $2140
#define APUIO1 $2141
#define APUIO2 $2142
#define APUIO3 $2143



// WRAM Data Read/Write
// (read/write, any)
//
// Will not work with a DMA transfer from WRAM to WRAM.
#define WMDATA $2180

// WRAM Address
// (write, long, any)
//
// -------x xxxxxxxx xxxxxxxx
#define WMADD  $2181
#define WMADDL $2181
#define WMADDM $2182
#define WMADDH $2183



// NES-style Joypad Access Port 1
// (read/write, any)
//
// Read:  ------ca
// Write: -------l
//
// l = latch line of both ports
// a = data 1 line
// b = data 2 Line
#define JOYSER0 $4016

// Write
#define JOYSER0_LATCH $01

// Read
#define JOYSER0_DATA1 $01
#define JOUSER0_DATA2 $02



// NES-style Joypad Access Port 2
// (read, any)
//
// ---111db
//
// b = data 1 line
// d = data 2 line
// 1 = Always set
#define JOYSER1 $4017

// Read
#define JOYSER1_DATA1 $01
#define JOUSER1_DATA2 $02



// Interrupt Enable Flags
// (write, any)
//
// n-yx---a
//
// n  = Enable V-Blank (NMI)
// yx = Enable IRQ
//       00 - Disabled
//       01 - On V-Counter (`VTIME`)
//       10 - On H-Counter (`HTIME`)
//       11 - On V-Counter and H-Counter (`VTIME` and `HTIME`)
// a  = Auto read joypad
#define NMITIMEN $4200

// Flags
#define NMITIMEN_VBLANK_FLAG  $80
#define NMITIMEN_AUTOJOY_FLAG $01
#define NMITIMEN_IRQ_H_FLAG   $10
#define NMITIMEN_IRQ_V_FLAG   $20



// Programmable I/O Port
// (write, any)
//
// ab------
//
// a = Pin 6 of Port 1
// b = Pin 6 of Port 2
//
// H/V Counters (`OPHCT` and `OPVCT` ) will be latched on lowering
// trigger of Port 2 (Transition from 1 to 0).
#define WRIO $4201

// Masks
#define WRIO_PORT1 $80
#define WRIO_PORT2 $40



// Multiplicand A
// (write, any)
#define WRMPYA $4202

// Multiplicand B
// (write, any)
//
// After 8 CPU cycles `RDMPY` will contain the unsigned product of `WRMPYA` and `WRMPYB`.
#define WRMPYB $4203



// Dividend C
// (write, word, any)
#define WRDIV  $4204
#define WRDIVL $4204
#define WRDIVH $4205

// Divisor B
// (write, any)
//
// After 16 CPU cycles `RDDIV` will contain the unsigned quotient of `WRDIV` and `WRDIVB`.
// The remainder of the division is stored in `RDMPY`.
#define WRDIVB $4206



// H Timer
// (write, word, any)
//
// ------h hhhhhhhh
//
// hhhhhhhhh = horizontal counter target (Range 0 - 339)
//
// See `MNITIMEN` for details.
#define HTIME  $4207
#define HTIMEL $4207
#define HTIMEH $4208

// V Timer
// (write, word, any)
//
// ------v vvvvvvvv
//
// vvvvvvvvvv = vertical counter target (Range 0 - 261 NTSC and 0 - 311 PAL)
//
// See `MNITIMEN` for details.
#define VTIME  $4209
#define VTIMEL $4209
#define VTIMEH $420A



// DMA Enable
// (write, any)
//
// 76543210
//
// 7/6/5/4/3/2/1 = Enable DMA channel.
//
// When a DMA is activated the CPU will be paused until all DMAs are completed.
#define MDMAEN $420B

// Flags
#define MDMAEN_DMA0 $01
#define MDMAEN_DMA1 $02
#define MDMAEN_DMA2 $04
#define MDMAEN_DMA3 $08
#define MDMAEN_DMA4 $10
#define MDMAEN_DMA5 $20
#define MDMAEN_DMA6 $40
#define MDMAEN_DMA7 $80



// HDMA Enable
// (write, any)
//
// 76543210
//
// 7/6/5/4/3/2/1 = Enable HDMA channel.
#define HDMAEN $420C

// Flags
#define HDMAEN_DMA0 $01
#define HDMAEN_DMA1 $02
#define HDMAEN_DMA2 $04
#define HDMAEN_DMA3 $08
#define HDMAEN_DMA4 $10
#define HDMAEN_DMA5 $20
#define HDMAEN_DMA6 $40
#define HDMAEN_DMA7 $80



// ROM Access Speed
// (write, any)
//
// -------r
//
// r = FastROM select (1 = FastROM, 0 = SlowROM)
#define MEMSEL $420D

// Flags
#define MEMSEL_FASTROM $01
#define MEMSEL_SLOWROM $00



// NMI Flag and 5A22 Version
// (read, any)
//
// n---vvvv
//
// n    = MNI Flag. Set on start of V-Blank, cleaered on read
//        or at end of V-Blank.
// vvvv = 5A22 Chip version number
//
// This register should be read during the MNI Handler.
#define RDNMI $4210

// Flags
#define RDNMI_NMI_FLAG          $80
#define RDNMI_5A22_VERSION_MASK $0f



// IRQ Flag
// (read, any)
//
// i-------
//
// i = IRQ Flag. Set just after IRQ fires, cleared on read.
//
// This register should be read during the IRQ Handler.
#define TIMEUP $4211



// PPU Status
// (read, any)
//
// vh-----a
//
// v = V-Blank Flag. Set during V-Blank
// h = H-Blank Flag. Set during H-Blank
// a = Auto Joypad read status. Set while reading, cleared when completed.
#define HVBJOY $4212

// Flags
#define HVBJOY_VBLANK  $80
#define HVBJOY_HBLANK  $40
#define HVBJOY_AUTOJOY $01



// Programmable I/O Port (in-port)
//
// ab------
//
// a = Pin 6 of Port 1
// b = Pin 6 of Port 2
#define RDIO $4201

// Masks
#define RDIO_PORT1 $80
#define RDIO_PORT2 $40



// Quotient of Divide Result
// (read, word, any)
//
// Set 16 CPU cycles after `WRDIVB` write
#define RDDIV  $4214
#define RDDIVL $4214
#define RDDIVH $4215



// Multiplication Product of Divide Remainder
// (read, word, any)
//
// Set 16 CPU cycles after `WRDIVB` write or 8 CPU cycles after `WRMPYB` write.
#define RDMPY  $4216
#define RDMPYL $4216
#define RDMPYH $4217



// Autojoy Controller Ports
// (read, word, any)
//
// Should not be read when bit 0 of `HVJOY` set.
#define JOY1  $4218
#define JOY2  $421A
#define JOY3  $421C
#define JOY4  $421E

#define JOY1L $4218
#define JOY1H $4219
#define JOY2L $421A
#define JOY2H $421B
#define JOY3L $421C
#define JOY3H $421D
#define JOY4L $421E
#define JOY4H $421F

// Flags
#define JOY_B          $8000
#define JOY_Y          $4000
#define JOY_SELECT     $2000
#define JOY_START      $1000
#define JOY_UP         $0800
#define JOY_DOWN       $0400
#define JOY_LEFT       $0200
#define JOY_RIGHT      $0100
#define JOY_A          $0080
#define JOY_X          $0040
#define JOY_L          $0020
#define JOY_R          $0010

#define JOY_BUTTONS    $c0c0
#define JOY_DPAD       $0f00
#define JOY_SHOLDER    $0030
#define JOY_TYPE_MASK  $000f

#define JOYH_B          $80
#define JOYH_Y          $40
#define JOYH_SELECT     $20
#define JOYH_START      $10
#define JOYH_UP         $08
#define JOYH_DOWN       $04
#define JOYH_LEFT       $02
#define JOYH_RIGHT      $01
#define JOYL_A          $80
#define JOYL_X          $40
#define JOYL_L          $20
#define JOYL_R          $10
#define JOYL_TYPE_MASK	$0f

#define JOYH_DPAD       $0f

#define JOY_TYPE_CONTROLLER      $0
#define JOY_TYPE_MOUSE           $1
#define JOY_TYPE_KONAMI_JUSTIFER $e

// DMA Control For Channel 0
// (read, write, any)
//
// da-ifttt
//
// d    = Transfer direction (0 = CPU -> PPU, 1 = PPU -> CPU)
// a    = HDMA Addressing Mode (0 = absolute, 1 = indirect). HDMA Only
// i    = DMA Address Increment (0 = Increment, 1 = Decrement). DMA Only
// f    = DMA Fixed Transfer (1 = Address will not change). DMA Only.
// ttt  = Transfer Mode
//          000 - 1 register,  write once         ( r                )
//          001 - 2 registers, write once         ( r, r+1           )
//          010 - 1 register,  write twice        ( r, r             )
//          011 - 2 registers, write twice each   ( r, r,   r+1, r+1 )
//          100 - 4 registers, write once         ( r, r+1, r+2, r+3 )
//          101 - 2 register,  write twice alt  - undocumented
//          110 - 1 register,  write twice      - undocumented
//          111 - 2 registers, write twice each - undocumented
//
#define DMAP0 $4300
#define DMAP1 $4310
#define DMAP2 $4320
#define DMAP3 $4330
#define DMAP4 $4340
#define DMAP5 $4350
#define DMAP6 $4360
#define DMAP7 $4370

// Masks
#define DMAP_TRANSFER_MASK $07

// Transfer Direction
#define DMAP_DIRECTION_TO_PPU $00
#define DMAP_DIRECTION_TO_CPU $80

// HDMA Addressing
#define DMAP_ADDRESSING_ABSOLUTE $00
#define DMAP_ADDRESSING_INDIRECT $40

// DMA Address Increment
#define DMAP_ADDRESS_INCREMENT $00
#define DMAP_ADDRESS_DECREMENT $10
#define DMAP_ADDRESS_FIXED     $08

// Transfer Mode
#define DMAP_TRANSFER_1REG              $0
#define DMAP_TRANSFER_2REGS             $1
#define DMAP_TRANSFER_WRITE_TWICE       $2
#define DMAP_TRANSFER_READ_TWICE        $2
#define DMAP_TRANSFER_2REGS_WRITE_TWICE $3
#define DMAP_TRANSFER_4REGS             $4



// DMA Destination Register
// (read, write, any)
//
// Bus B Address (PPU Memory Address, maps to $00:21xx)
#define BBAD0 $4301
#define BBAD1 $4311
#define BBAD2 $4321
#define BBAD3 $4331
#define BBAD4 $4341
#define BBAD5 $4351
#define BBAD6 $4361
#define BBAD7 $4371



// DMA Source Address
// (read, write, long, any)
//
// Bus A Address (CPU Memory Address)
//
// DMA:
//  * Starting address for DMA Transfer.
//  * Address will be incremented/decremented
//  * Bank will not change during DMA Transfer.
//
// HDMA:
//  * Table address for HDMA Transfer.
#define A1T0  $4302
#define A1T1  $4312
#define A1T2  $4322
#define A1T3  $4332
#define A1T4  $4342
#define A1T5  $4352
#define A1T6  $4362
#define A1T7  $4372

#define A1T0L $4302
#define A1T0H $4303
#define A1B0  $4304
#define A1T1L $4312
#define A1T1H $4313
#define A1B1  $4314
#define A1T2L $4322
#define A1T2H $4323
#define A1B2  $4324
#define A1T3L $4332
#define A1T3H $4333
#define A1B3  $4334
#define A1T4L $4342
#define A1T4H $4343
#define A1B4  $4344
#define A1T5L $4352
#define A1T5H $4353
#define A1B5  $4354
#define A1T6L $4362
#define A1T6H $4363
#define A1B6  $4364
#define A1T7L $4372
#define A1T7H $4373
#define A1B7  $4374



// DMA Size / HDMA Indirect Address
// (read, write, word, any)
//
// DMA:
//  * 16 Bits
//  * Number of bytes to transfer
//  * Decremented during DMA.
//  * If 0 then $10000 bytes will be transfered.
//
// HDMA:
//  * Holding space for indirect addressing mode
//  * These registers should not be set by the CPU
#define DAS0  $4305
#define DAS1  $4315
#define DAS2  $4325
#define DAS3  $4335
#define DAS4  $4345
#define DAS5  $4355
#define DAS6  $4365
#define DAS7  $4375


#define DAS0L $4305
#define DAS0H $4306
#define DAS1L $4315
#define DAS1H $4316
#define DAS2L $4325
#define DAS2H $4326
#define DAS3L $4335
#define DAS3H $4336
#define DAS4L $4345
#define DAS4H $4346
#define DAS5L $4355
#define DAS5H $4356
#define DAS6L $4365
#define DAS6H $4366
#define DAS7L $4375
#define DAS7H $4376



// HDMA Indirect Address Bank
// (read, write, any)
//
// Used with HDMA only, combined with `DAS0` to form the effective address.
#define DAS0B $4307
#define DAS1B $4317
#define DAS2B $4327
#define DAS3B $4337
#define DAS4B $4347
#define DAS5B $4357
#define DAS6B $4367
#define DAS7B $4377



// HDMA Table Address
// (read, write, word, any)
//
// Used with HDMA only.
//
// Only modify this register if starting HDMA mid frame.
#define A2A0  $4308
#define A2A1  $4318
#define A2A2  $4328
#define A2A3  $4338
#define A2A4  $4348
#define A2A5  $4358
#define A2A6  $4368
#define A2A7  $4378

#define A2A0L $4308
#define A2A0H $4309
#define A2A1L $4318
#define A2A1H $4319
#define A2A2L $4328
#define A2A2H $4329
#define A2A3L $4338
#define A2A3H $4339
#define A2A4L $4348
#define A2A4H $4349
#define A2A5L $4358
#define A2A5H $4359
#define A2A6L $4368
#define A2A6H $4369
#define A2A7L $4378
#define A2A7H $4379



// HDMA Line Counter
// (read, write, any)
//
// rccccccc
//
// r       = Repeat Select (1 = HDMA every line, 0
// ccccccc = Line Count
//
// Used with HDMA only.
//
// Only modify this register if starting HDMA mid frame.
#define NLTR0 $430A
#define NLTR1 $431A
#define NLTR2 $432A
#define NLTR3 $433A
#define NLTR4 $434A
#define NLTR5 $435A
#define NLTR6 $436A
#define NLTR7 $437A

// Flags
#define NTLR_REPEAT_FLAG $80

// Masks
#define NTLR_COUNT_MASK  $7f

#endif // _REGISTERS_INC_

// vim: ft=asm:


