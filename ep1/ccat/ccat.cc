#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <string>
#define strnicmp strncasecmp
#define register
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
        bool          extcolor:1, underline:1, dim:1, italic:1, bold:1, inverse:1, blink:1, dblunder:1, strikeout:1;

        void Split(unsigned attr)
        {
            underline = attr & 0x010000u;
            dim       = attr & 0x020000u;
            italic    = attr & 0x040000u;
            bold      = attr & 0x080000u;
            inverse   = attr & 0x100000u;
            blink     = attr & 0x200000u;
            extcolor  = attr & 0x800000u;
            dblunder  = false;
            strikeout = false;
            if(extcolor)
                { bg = attr >> 8; fg = (attr & 0x7F) | ((attr & 0x400000u) ? 0x80u : 0x00u); }
            else
                { bg = (attr >> 4) & 0x7; fg = (attr & 0xF); }
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
                std::string chosen_code;
                for(unsigned round=0; round<2; ++round)
                {
                    std::string code;
                    const char* pfx = "";
                    #define add(n) do{ code += pfx; pfx = ";"; code += n; }while(0)

                    Attribute cur; cur.Split(cur_attr);
                    Attribute rep; rep.Split(attr);

                    /*
                    SGR codes supported by Xterm

                    https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters

                        1:  Set bold
                        2:  Set dim
                        3:  Set italic
                        4:  Set underline
                        5:  Set blink
                        7:  Set inverse
                        8:  Set invisible
                        9:  Set strike-out
                        21: SET DOUBLE underline
                        22: Clear bold AND dim
                        23: Clear italic
                        24: Clear underline and DOUBLE underline
                        25: Clear blink
                        27: Clear inverse
                        29: Clear strike-out
                        30..37:     Set foreground attribute (0-7, from ANSI)
                        38;5;n:     Set foreground attribute (n, from xterm-256color)
                        38;2;r;g;b: Set foreground attribute (truecolor, 256*256*256)
                        39:         Set foreground attribute with default
                        40..47:     Set background attribute (0-7, from ANSI)
                        48;5;n:     Set background attribute (n, from xterm-256color)
                        48;2;r;g;b: Set background attribute (truecolor, 256*256*256)
                        49:         Set background attribute with default
                        90..97:     Set foreground attribute (0-7, from ANSI but bright & not bold), AIX extension
                        100:        Same as 39;49
                        101..107:   Set background attribute (0-7, from ANSI but bright & not bold), AIX extension
                        0:          Same as 22;23;24;25;27;29;39;49
                    */
                    if(round == 0) { add("0"); cur.Split(7); }

                    if((!rep.bold && cur.bold) || (!rep.dim && cur.dim))
                        { add("22"); cur.bold=false; cur.dim=false; }
                    if((!rep.underline && cur.underline) || (!rep.dblunder && cur.dblunder))
                        { add("24"); cur.underline=false; cur.dblunder=false; }
                    if(rep.bold && !cur.bold) add("1");
                    if(rep.dim && !cur.dim)   add("2");
                    if(rep.underline && !cur.underline) add("4");
                    if(rep.dblunder && !cur.dblunder) add("21");

                    if(cur.italic && !rep.italic)       add("23"); else if(rep.italic && !cur.italic) add("3");
                    if(cur.blink && !rep.blink)         add("25"); else if(rep.blink && !cur.blink) add("5");
                    if(cur.inverse && !rep.inverse)     add("27"); else if(rep.inverse && !cur.inverse) add("7");
                    if(cur.strikeout && !rep.strikeout) add("29"); else if(rep.strikeout && !cur.strikeout) add("9");

                    auto getcolor = [&](Attribute& attr, unsigned c) -> std::pair<bool,unsigned>
                    {
                        if(!attr.extcolor || c<16) return {false,c};
                        if(c == 16 || c == 232)    return {false,0};
                        if(c == 255 || c == 231)   return {false,15};
                        return {true,c};
                    };

                    if(auto c = getcolor(rep,rep.fg); c != getcolor(cur,cur.fg))
                    if(auto c2 = getcolor(rep,rep.bg); c2 != getcolor(cur,cur.bg))
                    if(c == std::pair{false,7u} && c2 == std::pair{false,0u})
                    {
                        add("100"); //39+49
                        cur.bg = 0; cur.fg = 7;
                    }

                    if(auto c = getcolor(rep,rep.fg); c != getcolor(cur,cur.fg))
                    {
                        char Buf[16];

                        if(c.first) sprintf(Buf, "38:5:%d", c.second);
                        else if(c.second&8)sprintf(Buf, "9%d", (c.second&2)|(4*(c.second&1))|((c.second&4)/4));
                        else               sprintf(Buf, "3%d", (c.second&2)|(4*(c.second&1))|((c.second&4)/4));
                        add(Buf);
                    }
                    if(auto c = getcolor(rep,rep.bg); c != getcolor(cur,cur.bg))
                    {
                        char Buf[16];
                        if(c.first) sprintf(Buf, "48:5:%d", c.second);
                        else if(c.second&8)sprintf(Buf, "10%d", (c.second&2)|(4*(c.second&1))|((c.second&4)/4));
                        else               sprintf(Buf, "4%d",  (c.second&2)|(4*(c.second&1))|((c.second&4)/4));
                        add(Buf);
                    }
                    if(!round || code.size() < chosen_code.size())
                        chosen_code = std::move(code);
                }
                if(chosen_code.size()==1 && chosen_code[0]=='0') chosen_code.clear();

                result += "\33[";
                result += std::move(chosen_code);
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

