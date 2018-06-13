#include <cstdio>
#include <cstring>
#include <vector>
#include <map>

/*
 * TODO: Change this program to create RLE expressions.
 * It now can _read_ them, but..
 */

namespace
{
    std::vector<unsigned char> ROM;

    void WriteByte(unsigned addr, unsigned char value)
    {
        ROM[addr]     = value;
    }

    void WriteShort(unsigned addr, unsigned short value)
    {
        unsigned oldvalue = ROM[addr] | (ROM[addr+1] << 8);
        std::fprintf(stderr, "Writing %04X @ %06X (was %04X)\n", value, addr, oldvalue);
        WriteByte(addr, value & 255);
        WriteByte(addr+1, value >> 8);
    }

    void WriteCheckSumPair(unsigned addr, unsigned sum1)
    {
        sum1 &= 0xFFFF;

        WriteShort(addr+2, sum1);
        WriteShort(addr+0, sum1 ^ 0xFFFF);
    }
}

int main(int argc, const char *const *argv)
{
    if(argc != 1+2)
    {
        std::fprintf(stderr, "fixchecksum: Fixes SNES patch checksum.\n"
               "Copyright (C) 2018 Bisqwit (http://iki.fi/bisqwit/)\n"
               "Usage: fixchecksum oldfile newfile\n");
        return -1;
    }
    const char *origfn = argv[1];
    const char *resfn = argv[2];

    FILE *original = std::fopen(origfn, "rb");
    if(!original) { if(!std::strcmp(origfn, "-")) original = stdin;
                    else std::perror(origfn); }

    if(!original)
        return -1;

    std::setbuf(original, NULL);

    while(!std::feof(original))
    {
        char tmp[4096];
        int c = std::fread(tmp, 1, sizeof tmp, original);
        if(c < 0 && std::ferror(original)) { std::perror("fread"); return -1; }
        if(!c) break;

        ROM.insert(ROM.end(), tmp, tmp+c);
    }
    std::fclose(original); original=NULL;

    unsigned CalculatedSize = (ROM.size() / 0x2000) * 0x2000;
    unsigned size = CalculatedSize;
    for(unsigned power2=0; ; ++power2)
        if(!(size >>= 1)) { size = 1 << power2; break; }
    unsigned remainder=CalculatedSize-size;
    unsigned offset = ROM.size() - CalculatedSize;

    /* First create dummy checksums, so that we won't calculate invalid values */
    WriteCheckSumPair(offset + 0xFFDC, 0);

    if(CalculatedSize >= 0x410000)
    {
        /* ExHiROM must have $008000 mirrored at $408000 */
        for(unsigned a=0x8000; a<=0xFFFF; ++a)
            WriteByte(offset+0x400000+a, ROM[offset+a]);
    }

    unsigned sum1 = 0;
    for(unsigned a=0; a<size; ++a) sum1 += ROM[offset+a];
    if(remainder)
    {
        unsigned sum2 = 0;
        for(unsigned a=0; a<remainder; ++a) sum2 += ROM[offset+size+a];
        // Multiply because of mirroring
        sum1 += sum2 * (size / remainder);
    }

    WriteCheckSumPair(offset + 0xFFDC, sum1);
    if(CalculatedSize >= 0x410000)
    {
        WriteCheckSumPair(offset + 0x40FFDC, sum1);
    }

    std::fprintf(stderr, "ROM size $%lX. Calculated $%X. Sum($%X..$%X, +$%X))=$%04X\n",
        (unsigned long) ROM.size(), CalculatedSize,
            offset, size, remainder, sum1);

    FILE *resultfile = std::fopen(resfn, "wb");
    if(!resultfile) { std::perror(resfn); return -1; }
    std::fwrite(&ROM[0], 1, ROM.size(), resultfile);
    std::fclose(resultfile);

    return 0;
}
