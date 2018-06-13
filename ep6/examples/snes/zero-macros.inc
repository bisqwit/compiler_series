// zero-macros.inc
// ===============
//
// Copyright 2018, Marcus Rowe <undisbeliever@gmail.com>
// Distributed under The MIT License: https://opensource.org/licenses/MIT


#ifndef _ZERO_MACROS_INC_
#define _ZERO_MACROS_INC_ 1

#define _2_ZEROS()  0, 0
#define _4_ZEROS()  0, 0, 0, 0
#define _8_ZEROS()  0, 0, 0, 0, 0, 0, 0, 0
#define _16_ZEROS() 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#define _32_ZEROS() 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

#define _64_ZEROS()    _32_ZEROS(),  _32_ZEROS()
#define _128_ZEROS()   _64_ZEROS(),  _64_ZEROS()
#define _256_ZEROS()  _128_ZEROS(), _128_ZEROS()
#define _512_ZEROS()  _256_ZEROS(), _256_ZEROS()
#define _1024_ZEROS() _512_ZEROS(), _512_ZEROS()

#define _2_FFs()  255, 255
#define _4_FFs()  255, 255, 255, 255
#define _8_FFs()  255, 255, 255, 255, 255, 255, 255, 255
#define _16_FFs() 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
#define _32_FFs() 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255

#define _64_FFs()    _32_FFs(),  _32_FFs()
#define _128_FFs()   _64_FFs(),  _64_FFs()
#define _256_FFs()  _128_FFs(), _128_FFs()
#define _512_FFs()  _256_FFs(), _256_FFs()
#define _1024_FFs() _512_FFs(), _512_FFs()

#endif // _ZERO_MACROS_INC_

// vim: ft=asm:

