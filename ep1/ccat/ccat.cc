#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <string>

#define strnicmp strncasecmp
#define cdecl
#include "jsf.hh"

static const char* synfile = "ctcode2.jsf";

class Action: public JSF::Applier
{
public:
    std::vector<unsigned long> code;
    std::size_t position = 0;

    virtual int Get()
    {
        if(position >= code.size()) return -1;
        return code[position++];
    }
    virtual void Recolor(unsigned distance, unsigned n, unsigned long attr)
    {
        signed long p = position - (distance+1);
        for(; n-- && p>=0; --p)
        {
            code[p] = (code[p] & 0xFF) | (attr << 8);
        }
    }
    void Run()
    {
        JSF jsf;
        jsf.Parse(synfile);
        JSF::ApplyState state;
        jsf.ApplyInit(state);
        jsf.Apply(state, *this);
    }
    struct Attribute
    {
        unsigned char fg, bg;
        bool          extcolor:1, underline:1, dim:1, italic:1, bold:1, inverse:1, blink:1;

        void Split(unsigned attr)
        {
            underline = attr & 0x010000u;
            dim       = attr & 0x020000u;
            italic    = attr & 0x040000u;
            bold      = attr & 0x080000u;
            inverse   = attr & 0x100000u;
            blink     = attr & 0x200000u;
            extcolor  = attr & 0x800000u;
            if(extcolor)
                { bg = attr >> 8; fg = (attr & 0x7F) | ((attr & 0x400000u) ? 0x80u : 0x00u); }
            else
                { bg = (attr >> 4) & 0xF; fg = (attr & 0xF); }
        }
    };
    void Print()
    {
        std::string result;
        unsigned cur_attr = 7;
        for(auto c: code)
        {
            unsigned attr = c >> 8;
            if(char(c) == '\n')
                { result += "\33[m"; cur_attr = 7; }
            else if(cur_attr != attr)
            {
                const char* pfx = "\33[";
                #define add(n) do{ result += pfx; pfx = ";"; result += n; }while(0)

                Attribute cur; cur.Split(cur_attr);
                Attribute rep; rep.Split(attr);

                if(cur.underline && !rep.underline) add("24"); else if(rep.underline && !cur.underline) add("4");
                if(cur.dim && !rep.dim) add("22"); else if(rep.dim && !cur.dim) add("2");
                if(cur.italic && !rep.italic) add("23"); else if(rep.italic && !cur.italic) add("3");
                if(cur.bold && !rep.bold) add("21"); else if(rep.bold && !cur.bold) add("1");
                if(cur.inverse && !rep.inverse) add("27"); else if(rep.inverse && !cur.inverse) add("7");
                if(cur.blink && !rep.blink) add("25"); else if(rep.blink && !cur.blink) add("5");
                if(cur.fg != rep.fg)
                {
                    char Buf[16];
                    if(rep.extcolor) sprintf(Buf, "38;5;%d", rep.fg); else sprintf(Buf, "3%d", (rep.fg&2)|(4*(rep.fg&1))|((rep.fg&4)/4));
                    add(Buf);
                }
                if(cur.bg != rep.bg)
                {
                    char Buf[16];
                    if(rep.extcolor) sprintf(Buf, "48;5;%d", rep.bg); else sprintf(Buf, "4%d", (rep.bg&2)|(4*(rep.bg&1))|((rep.bg&4)/4));
                    add(Buf);
                }
                result += 'm';
                cur_attr = attr;
            }
            result += char(c);
        }
        fwrite(result.data(), 1, result.size(), stdout);
    }
};

#include <fstream>
int main(int argc, char** argv)
{
    std::string filename = argv[1] ? argv[1] : "";
    if(filename == "" || filename == "-") { filename = "/dev/stdin"; synfile = "c.jsf"; }
    std::ifstream f(filename);

    Action act;
    act.code.assign(std::istreambuf_iterator<char>(f), {});
    for(auto& c: act.code) c = (c & 0xFF) | 0x0700;
    act.Run();
    act.Print();
}

