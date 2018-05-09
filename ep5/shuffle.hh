#include <vector>
#include <set>
#include <cassert>
#include <algorithm>

template<typename U, typename F, typename T>
static void ProduceShuffle(std::vector<U>&& params, F&& func, T&& maketemp)
{
    // Reorder paramaters, for example         "fcall (..) (..) R3 R1"
    // must do "copy R0 R3" so that it becomes "fcall (..) (..) R0 R1"
    //
    // func(tgtreg, srcreg) = produce a move insn
    // maketemp(reg)        = produce an index of a clobberable register, with reg as a hint
    //
    while(std::find_if(params.begin(), params.end(), [&](U& p) { return p != (&p-&params[0]); }) != params.end())
    {
        std::set<U> used(params.begin(), params.end());

        // Check if one of the params can be moved straight away.
        bool changed = false;
        for(std::size_t a=0; a<params.size(); ++a)
            if(params[a] != a && used.find(a) == used.end()) // Ra is not used; what should be Ra is something else
            {
                U src = params[a];
                used.erase(params[a]); used.insert(a);
                func(a, src); // Move that something else into Ra
                //All references to what was params[a] can now be satisfied by a
                for(std::size_t b=0; b<params.size(); ++b)
                    if(params[b] == src && (a < src || b == a))
                        params[b] = a;
                changed = true;
            }

        // None of them could be moved straight away.
        if(!changed)
            for(std::size_t a=0; a<params.size(); ++a)
                if(params[a] != a && used.find(a) != used.end()) // Ra is used by something else
                {
                    //Figure out where it is used
                    for(std::size_t b=0; b<params.size(); ++b)
                        if(params[b] == a)                       // Rb should be Ra, but Ra is occupied
                        {
                            // Move it away. We don't need to care about clobbering,
                            // because we are restarting the function;
                            // only the registers passed as parameters matter.
                            U unused = maketemp(std::max(U(params.size()), U(*used.rbegin()+1u)));
                            func(unused, params[b]);             // Move Rb away, making Ra unoccupied
                            params[b] = unused;
                            changed = true;
                            break;
                        }
                    if(changed) break;
                }
        assert(changed == true);
    }
}
