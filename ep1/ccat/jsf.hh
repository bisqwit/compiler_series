/* Ad-hoc programming editor for DOSBox -- (C) 2011-03-08 Joel Yliluoma */
#include <string.h>
#include "vec_c.hh"

class JSF
{
public:
    JSF() : states(0)
    {
    }
    void Parse(const char* fn)
    {
        FILE* fp = fopen(fn, "rb");
        if(!fp) { perror(fn); return; }
        Parse(fp);
        fclose(fp);
    }
    void Parse(FILE* fp)
    {
        char Buf[512]={0};
        //fprintf(stdout, "Parsing syntax file... "); fflush(stdout);
        TabType colortable;
        char colors_sorted = 0;
        states = 0; // NOTE: THIS LEAKS MEMORY
        while(fgets(Buf, sizeof(Buf), fp))
        {
            cleanup(Buf);
            if(Buf[0] == '=')
            {
                colors_sorted = 0;
                ParseColorDeclaration(Buf+1, colortable);
            }
            else if(Buf[0] == ':')
            {
                if(!colors_sorted)
                {
                    /* Sort the color table when the first state is encountered */
                    sort(colortable);
                    colors_sorted = 1;
                }
                ParseStateStart(Buf+1, colortable);
            }
            else if(Buf[0] == ' ' || Buf[0] == '\t')
                ParseStateLine(Buf, fp);
        }
        //fprintf(stdout, "Binding... "); fflush(stdout);
        BindStates();

        for(unsigned n=0; n<colortable.size(); ++n) free(colortable[n].token);

        //fprintf(stdout, "Done\n"); fflush(stdout);
    }
    struct state;
    struct ApplyState
    {
        /* std::vector<unsigned char> */
        CharVecType buffer;
        int buffering;
        int recolor, markbegin, markend;
        int recolormark:1, noeat:1;
        unsigned char c;
        state* s;
    };
    void ApplyInit(ApplyState& state)
    {
        state.buffer.clear();
        state.buffering = state.recolor = state.noeat = 0;
        state.markbegin = state.markend = 0;
        state.c = '?';
        state.s = states;
    }
    struct Applier
    {
        virtual cdecl int Get(void) = 0;
        virtual cdecl void Recolor(register unsigned distance, register unsigned n, register unsigned long attr) = 0;
    };
    void Apply( ApplyState& state, Applier& app)
    {
        for(;;)
        {
            /*fprintf(stdout, "[State %s]", state.s->name);*/
            if(state.noeat)
            {
                state.noeat = 0;
                if(!state.recolor) state.recolor = 1;
            }
            else
            {
                int ch = app.Get();
                if(ch < 0) break;
                state.c       = ch;
                state.recolor += 1;
                ++state.markbegin;
                ++state.markend;
            }
            if(state.recolor)
            {
                app.Recolor(0, state.recolor, state.s->attr);
            }
            if(state.recolormark)
            {
                // markbegin & markend say how many characters AGO it was marked
                app.Recolor(state.markend+1, state.markbegin - state.markend, state.s->attr);
            }

            option *o = state.s->options[state.c];
            state.recolor     = o->recolor;
            state.recolormark = o->recolormark;
            state.noeat       = o->noeat;
            state.s           = o->state;
            if(o->strings)
            {
                const char* k = (const char*) &state.buffer[0];
                unsigned    n = state.buffer.size();
                struct state* ns = o->strings==1
                        ? findstate(o->stringtable, k, n)
                        : findstate_i(o->stringtable, k, n);
                /*fprintf(stdout, "Tried '%.*s' for %p (%s)\n",
                    n,k, ns, ns->name);*/
                if(ns)
                {
                    state.s = ns;
                    state.recolor = state.buffer.size()+1;
                }
                state.buffer.clear();
                state.buffering = 0;
            }
            else if(state.buffering && !state.noeat)
                state.buffer.push_back(state.c);
            if(o->buffer)
                { state.buffering = 1;
                  state.buffer.assign(&state.c, &state.c + 1); }
            if(o->mark)    { state.markbegin = 0; }
            if(o->markend) { state.markend   = 0; }
        }
    }
private:
    struct option;
    struct state
    {
        state*        next;
        char*         name;
        unsigned long attr;
        option* options[256];
    }* states;
    struct table_item
    {
        char*  token;
        union
        {
            struct state* state;
            char*  state_name;
        };

        inline void Construct() { token=0; state=0; }
        inline void Construct(const table_item& b) { token=b.token; state=b.state; }
        inline void Destruct() { }
        inline void swap(table_item& b)
        {
            register char* t;
            t = token;      token     =b.token;      b.token     =t;
            t = state_name; state_name=b.state_name; b.state_name=t;
        }
    };
    /* std::vector<table_item> without STL, for Borland C++ */
    #ifdef __GNUC__
    using TabType = std::vector<table_item>;
    #else
    #define UsePlacementNew
    #define T       table_item
    #define VecType TabType
    #include "vecbase.hh"
    #undef VecType
    #undef T
    #undef UsePlacementNew
    #endif

    struct option
    {
        TabType stringtable;
        union
        {
            struct state* state;
            char*  state_name;
        };
        unsigned char recolor;
        unsigned noeat:  1;
        unsigned buffer: 1;
        unsigned strings:2; // 0=no strings, 1=strings, 2=istrings
        unsigned name_mapped:1; // whether state(1) or state_name(0) is valid
        unsigned mark:1, markend:1, recolormark:1;
    };
    inline void ParseColorDeclaration(char* line, TabType& colortable)
    {
        while(*line==' '||*line=='\t') ++line;
        char* namebegin = line;
        while(*line && *line != ' ' && *line!='\t') ++line;
        char* nameend = line;
        unsigned char fg256 = 0;
        unsigned char bg256 = 0;
        unsigned char flags = 0x00; // underline=1 dim=2 italic=4 bold=8 inverse=16 blink=32
        for(;;)
        {
            while(*line==' '||*line=='\t') ++line;
            if(!*line) break;
            char* line_end = NULL;
            int attr = strtol(line, &line_end, 16);
            if(line_end >= line+2) // Two-digit hex?
            {
                line     = line_end;
                fg256    = attr & 0x0F;
                bg256    = (attr >> 4) & 0x0F;
                continue;
            }
            if(strncmp(line, "fg_", 3) == 0)
            {
                if(line[5] >= '0' && line[5] <= '5') fg256 = 16 + strtol(line+3, &line, 6);
                else                                 fg256 = 232 + strtol(line+3, &line, 10);
                continue;
            }
            if(strncmp(line, "bg_", 3) == 0)
            {
                if(line[5] >= '0' && line[5] <= '5') bg256 = 16 + strtol(line+3, &line, 6);
                else                                 bg256 = 232 + strtol(line+3, &line, 10);
                continue;
            }
            /* Words: black blue cyan green red yellow magenta white
             *        BLACK BLUE CYAN GREEN RED YELLOW MAGENTA WHITE
             *        bg_black bg_blue bg_cyan bg_green bg_red bg_yellow bg_magenta bg_white
             *        BG_BLACK BG_BLUE BG_CYAN BG_GREEN BG_RED BG_YELLOW BG_MAGENTA BG_WHITE
             *        underline dim italic bold inverse blink
             */
            unsigned short c=0, i=0;
            while(*line && *line != ' ' && *line != '\t') { c += 90u*(unsigned char)*line + i; i+=28; ++line; }
            unsigned char code = ((c + 22u) / 26u) % 46u;
            static const signed char actions[46] = { 10,30,2,1,31,19,29,23,13,36,15,25,7,3,28,-1,20,-1,24,9,16,27,-1,8,35,0,12,21,-1,5,11,17,22,33,32,34,4,14,-1,-1,6,26,-1,-1,18,37};
            /*if(code >= 0 && code <= 45)*/ code = actions[code - 0];
            switch(code >> 4) { case 0: fg256 = code&15; break;
                                case 1: bg256 = code&15; break;
                                default:flags |= 1u << (code&7); }
        }
        if(flags & 0x10) { unsigned tmp=fg256; fg256=bg256; bg256=tmp; flags &= ~0x10; } // inverse

        unsigned short attrlo = fg256;
        unsigned char  attrhi = flags & 0xF;
        if(fg256 < 16 && bg256 < 16)
        {
            // Create a 8-bit CGA/EGA/VGA attribute
            attrlo |= (bg256 << 4) | ((flags & 0x20) << 2) | (flags & 0x08);
        }
        else
        {
            // Create an extended attribute
            attrhi |= 0x80u | ((fg256 & 0x80) >> 1);
            attrlo |= (bg256 << 8u) | 0x80u;
        }
        unsigned long attr = attrlo | (((unsigned long)attrhi) << 16u);

        *nameend = '\0';
        table_item tmp;
        tmp.token = strdup(namebegin);
        if(!tmp.token) fprintf(stdout, "strdup: failed to allocate string for %s\n", namebegin);
        tmp.state = (struct state *) attr;
        colortable.push_back(tmp);
    }
    inline void ParseStateStart(char* line, const TabType& colortable)
    {
        while(*line==' '||*line=='\t') ++line;
        char* namebegin = line;
        while(*line && *line != ' ' && *line!='\t') ++line;
        char* nameend = line;
        while(*line==' '||*line=='\t') ++line;
        *nameend = '\0';
        struct state* s = new state;
        if(!s) fprintf(stdout, "failed to allocate new jsf state\n");
        memset(s, 0, sizeof(*s));
        s->name = strdup(namebegin);
        if(!s->name)
        {
            fprintf(stdout, "strdup: failed to allocate string for %s\n", namebegin);
            s->attr = 0x4A;
        }
        else
        {
            state* c = findstate(colortable, line);
            // The value in the table is a pointer type, but it actually is a color code (integer).
            if(!c)
            {
                fprintf(stdout,"Unknown color: '%s'\n", line);
                s->attr = 0x4A;
            }
            else
            {
                s->attr = (long) c;
            }
        }
        s->next = states;
        states = s;
    }
    inline void ParseStateLine(char* line, FILE* fp)
    {
        option* o = new option;
        if(!o) fprintf(stdout, "failed to allocate new jsf option\n");
        memset(o, 0, sizeof(*o));
        while(*line == ' ' || *line == '\t') ++line;
        if(*line == '*')
        {
            for(unsigned a=0; a<256; ++a)
                states->options[a] = o;
            ++line;
        }
        else if(*line == '"')
        {
            for(++line; *line != '\0' && *line != '"'; ++line)
            {
                if(*line == '\\')
                    switch(*++line)
                    {
                        case 't': *line = '\t'; break;
                        case 'n': *line = '\n'; break;
                        case 'v': *line = '\v'; break;
                        case 'b': *line = '\b'; break;
                    }
                unsigned char first = *line;
                if(line[1] == '-' && line[2] != '"')
                {
                    line += 2;
                    if(*line == '\\')
                        switch(*++line)
                        {
                            case 't': *line = '\t'; break;
                            case 'n': *line = '\n'; break;
                            case 'v': *line = '\v'; break;
                            case 'b': *line = '\b'; break;
                        }
                    do states->options[first] = o;
                    while(first++ != (unsigned char)*line);
                }
                else
                    states->options[first] = o;
            }
            if(*line == '"') ++line;
        }
        while(*line == ' ' || *line == '\t') ++line;
        char* namebegin = line;
        while(*line && *line != ' ' && *line!='\t') ++line;
        char* nameend   = line;
        while(*line == ' ' || *line == '\t') ++line;
        *nameend = '\0';
        o->state_name  = strdup(namebegin);
        if(!o->state_name) fprintf(stdout, "strdup: failed to allocate string for %s\n", namebegin);
        o->name_mapped = 0;
        /*fprintf(stdout, "'%s' for these: ", o->state_name);
        for(unsigned c=0; c<256; ++c)
            if(states->options[c] == o)
                fprintf(stdout, "%c", c);
        fprintf(stdout, "\n");*/

        while(*line != '\0')
        {
            char* opt_begin = line;
            while(*line && *line != ' ' && *line!='\t') ++line;
            char* opt_end   = line;
            while(*line == ' ' || *line == '\t') ++line;
            *opt_end = '\0';
            switch(*opt_begin)
            {
                case 'n':
                    if(strcmp(opt_begin+1, "oeat") == 0) { o->noeat = 1; break; }
                    goto ukw;
                case 'b':
                    if(strcmp(opt_begin+1, "uffer") == 0) { o->buffer = 1; break; }
                    goto ukw;
                case 'm':
                    if(strcmp(opt_begin+1, "arkend") == 0) { o->markend = 1; break; }
                    if(strcmp(opt_begin+1, "ark")    == 0) { o->mark    = 1; break; }
                    goto ukw;
                case 's':
                    if(strcmp(opt_begin+1, "trings") == 0) { o->strings = 1; break; }
                    goto ukw;
                case 'i':
                    if(strcmp(opt_begin+1, /*i*/"strings") == 0) { o->strings = 2; break; }
                    goto ukw;
                case 'r':
                    if(strcmp(opt_begin+1, "ecolormark") == 0) { o->recolormark = 1; break; }
                    if(strncmp(opt_begin+1, "ecolor=", 7) == 0)
                    {
                        int r = atoi(opt_begin+8);
                        if(r < 0) r = -r;
                        o->recolor = r;
                        break;
                    }
                    goto ukw;
                default: ukw:
                    fprintf(stdout,"Unknown keyword '%s' in '%s'\n", opt_begin, namebegin);
            }
        }
        if(o->strings)
        {
            for(;;)
            {
                char Buf[512]={0};
                if(!fgets(Buf, sizeof(Buf), fp)) break;
                cleanup(Buf);
                line = Buf;
                while(*line == ' ' || *line == '\t') ++line;
                if(strcmp(line, "done") == 0) break;
                if(*line == '"') ++line;

                char* key_begin = line = strdup(line);
                if(!key_begin) fprintf(stdout, "strdup: failed to allocate string for %s\n", line);
                while(*line != '"' && *line != '\0') ++line;
                char* key_end   = line;
                if(*line == '"') ++line;
                while(*line == ' ' || *line == '\t') ++line;
                *key_end++   = '\0';

                char* value_begin = line;
                while(*line != '\0') ++line;
                /*unsigned char* value_end   = (unsigned char*) line;
                *value_end++ = '\0';*/
                if(*key_begin && *value_begin)
                {
                    table_item item;
                    item.token      = key_begin;
                    item.state_name = value_begin;
                    //fprintf(stdout, "String-table push '%s' '%s'\n", key_begin,value_begin);
                    o->stringtable.push_back(item);
                }
            }
            sort(o->stringtable);
        }
    }
    // Removes comments and trailing space from the buffer
    void cleanup(char* Buf)
    {
        char quote=0, *begin = Buf, *end = strchr(Buf, '\0');
        for(; *begin; ++begin)
        {
            if(*begin == '#' && !quote)
                { end=begin; *begin='\0'; break; }
            if(*begin == '"') quote=!quote;
            else if(*begin == '\\') ++begin;
        }
        while(end > Buf &&
            (end[-1] == '\r'
          || end[-1] == '\n'
          || end[-1] == ' '
          || end[-1] == '\t')) --end;
        *end = '\0';
    }
    // Search given table for the given string.
    // Is used by BindStates() for finding states for binding,
    // but also used by Apply for searching a string table
    // (i.e. used when coloring reserved words).
    static state* findstate(const TabType& table, const char* s, register unsigned n=0)
    {
        if(!n) n = strlen(s);
        unsigned begin = 0, end = table.size();
        while(begin < end)
        {
            unsigned half = (end-begin) >> 1;
            const table_item& m = table[begin + half];
            register int c = strncmp(m.token, s, n);
            if(c == 0)
            {
                if(m.token[n] == '\0') return m.state;
                c = m.token[n];
            }
            if(c < 0) begin += half+1;
            else      end = begin+half;
        }
        return 0;
    }
    // Case-ignorant version
    static state* findstate_i(const TabType& table, const char* s, register unsigned n=0)
    {
        if(!n) n = strlen(s);
        unsigned begin = 0, end = table.size();
        while(begin < end)
        {
            unsigned half = (end-begin) >> 1;
            const table_item& m = table[begin + half];
            register int c = strnicmp(m.token, s, n);
            if(c == 0)
            {
                if(m.token[n] == '\0') return m.state;
                c = m.token[n];
            }
            if(c < 0) begin += half+1;
            else      end = begin+half;
        }
        return 0;
    }

    // Converted state-names into pointers to state structures for fast access
    void Remap(option*& o, TabType& state_cache, unsigned a, const char* statename)
    {
        if( ! o->name_mapped)
        {
            char* name = o->state_name;
            o->state = findstate( state_cache, name );
            if(!o->state)
            {
                fprintf(stdout, "Failed to find state called '%s' for index %u/256 in '%s'\n", name, a, statename);
            }
            o->name_mapped = 1;
            for(TabType::iterator e = o->stringtable.end(),
                t = o->stringtable.begin();
                t != e;
                ++t)
            {
                char* name2 = t->state_name;
                t->state = findstate( state_cache, name2 );
                if(!t->state)
                {
                    fprintf(stdout, "Failed to find state called '%s' for string table in target '%s' for '%s'\n", name2, name, statename);
                }
                // free(name2); - was not separately allocated
            }
            free(name);
        }
    }
    void BindStates()
    {
        TabType state_cache;
        {for(state* s = states; s; s = s->next)
        {
            table_item tmp;
            tmp.token = s->name;
            tmp.state = s;
            state_cache.push_back(tmp);
        }}
        sort(state_cache);

        // Translate state names to state pointers.
        for(;;)
        {
            for(unsigned a=0; a<256; ++a)
            {
            //tail:;
                option*& o = states->options[a];
                if(!o)
                {
                    fprintf(stdout, "In state '%s', character state %u/256 not specified\n", states->name, a);
                    continue;
                }
                Remap(o, state_cache, a, states->name);
                while(o->noeat && o->recolor <= 1 && !o->buffer && !o->strings && !o->mark && !o->markend && !o->recolormark)
                {
                    unsigned long orig_attr = states->attr;
                    unsigned long new_attr  = o->state->attr;
                    int had_recolor         = o->recolor > 0;

                    o = o->state->options[a];
                    Remap(o, state_cache, a, o->state->name);

                    if(o->state->options[a]->recolor < 1 && (had_recolor || new_attr != orig_attr))
                    {
                        o->state->options[a]->recolor = 1;
                    }
                }
            }
            if(!states->next) break;
            // Get the first-inserted state (last in chain) as starting-point.
            states = states->next;
        }
    }

    static int TableItemCompareForSort(const void * a, const void * b)
    {
        table_item * aa = (table_item *)a;
        table_item * bb = (table_item *)b;
        return strcmp(aa->token, bb->token);
    }
    static inline void sort(TabType& tab)
    {
        /*
        // Sort the table using insertion sort
        unsigned b = tab.size();
        for(unsigned i, j=1; j<b; ++j)
        {
            table_item k = tab[j];
            for(i=j; i>=1 && strcmp( k.token, tab[i-1].token ) > 0; ++i)
                tab[i] = tab[i-1];
            tab[i] = k;
        }
        */
        qsort(&tab[0], tab.size(), sizeof(tab[0]), TableItemCompareForSort);
    }
};
