/* Ad-hoc programming editor for DOSBox -- (C) 2011-03-08 Joel Yliluoma */
#ifndef vecBaseIncludes
# define vecBaseIncludes
# include <stdlib.h>
# include <malloc.h>
#endif

class VecType
{
public:
    //typedef unsigned char T;

    typedef T value_type;
    typedef T * iterator;
    typedef T * pointer;
    typedef T & reference;
    typedef T const * const_iterator;
    typedef T const * const_pointer;
    typedef T const & const_reference;
    typedef size_t size_type;

#ifdef UsePlacementNew
    #define Ttype T const&
#else
    #define Ttype T
#endif

public:
    VecType() : data(0),len(0),cap(0) { }
    ~VecType() { clear(); if(cap) deallocate(data,cap); }

    void Construct() { data=0; len=0; cap=0; }
    void Destruct()  { clear(); if(cap) deallocate(data,cap); }
    void Construct(const VecType& b)
    {
        data=0; len=b.len; cap=b.len;
        if(len)
        {
            data = allocate(len);
            if(!data) fprintf(stdout, "VecType: Failed to allocate %u bytes\n", (unsigned)len);
            copy_construct(&data[0], &b.data[0],len);
        }
    }

    VecType(size_type length) : data(0),len(0),cap(0)
    {
        resize(length);
    }
    VecType(size_type length, Ttype value) : data(0),len(0),cap(0)
    {
        resize(length, value);
    }
    VecType(T const* first, T const* last) : data(0),len(0),cap(0)
    {
        assign(first, last);
    }
    VecType(const VecType& b) : data(0), len(b.len), cap(b.len)
    {
        if(len)
        {
            data = allocate(len);
            if(!data) fprintf(stdout, "VecType: Failed to allocate %u bytes\n", (unsigned)len);
            copy_construct(&data[0], &b.data[0], len);
        }
    }
#ifdef __GXX_EXPERIMENTAL_CXX0X__
    VecType(VecType&& b): data(b.data), len(b.len), cap(b.cap)
    {
        b.data = 0;
        b.len  = 0;
        b.cap  = 0;
    }
#endif
    VecType& operator= (const VecType& b)
    {
        if(&b == this) return *this;
        if(len < b.len)
        {
            reserve(b.len);
            copy_assign(&data[0], &b.data[0], len);
            copy_construct(&data[len], &b.data[len], b.len-len);
        }
        else if(len > b.len)
        {
            destroy(&data[b.len], len-b.len);
            copy_assign(&data[0], &b.data[0], b.len);
        }
        len = b.len;
        return *this;
    }
#ifdef __GXX_EXPERIMENTAL_CXX0X__
    VecType& operator= (VecType&& b)
    {
        if(&b != this) swap(b);
        return *this;
    }
#endif

    void assign(T const* first,
                T const* last)
    {
        size_type newlen = (size_type) (last-first);
        if(cap < newlen)
        {
            destroy(&data[0], len);
            deallocate(data, cap);
            data = allocate(cap = newlen);
            if(!data) fprintf(stdout, "VecType: Failed to allocate %u bytes\n", (unsigned)newlen);
            copy_construct(&data[0], first, newlen);
        }
        else if(len < newlen)
        {
            copy_assign(&data[0], first, len);
            copy_construct(&data[len], first+len, newlen-len);
        }
        else // len >= newlen
        {
            copy_assign(&data[0], first, newlen);
            destroy(&data[newlen], len-newlen);
        }
        len = newlen;
    }

    void assign(size_type newlen, Ttype value)
    {
      #if 0
        clear();
        resize(newlen, value);
      #else
        if(cap < newlen)
        {
            destroy(&data[0], len);
            deallocate(data, cap);
            data = allocate(cap = newlen);
            if(!data) fprintf(stdout, "VecType: Failed to allocate %u bytes\n", (unsigned)newlen);
            construct(&data[0], newlen, value);
        }
        else if(len < newlen)
        {
            for(size_type a=0; a<len; ++a)
                data[a] = value;
            construct(&data[len], newlen-len, value);
        }
        else // len >= newlen
        {
            for(size_type a=0; a<newlen; ++a)
                data[a] = value;
            destroy(&data[newlen], len-newlen);
        }
        len = newlen;
      #endif
    }

public:
    reference operator[] (size_type ind) { return data[ind]; }
    const_reference operator[] (size_type ind) const { return data[ind]; }
    iterator begin() { return data; }
    iterator end() { return data+len; }
    reference front() { return *begin(); }
    reference back() { return (*this)[size()-1]; } //*rbegin(); }
    const_iterator begin() const { return data; }
    const_iterator end() const { return data+len; }
    const_reference front() const { return *begin(); }
    const_reference back() const { return (*this)[size()-1]; }//*rbegin(); }
    /*
    typedef std::reverse_iterator<iterator> reverse_iterator;
    typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
    reverse_iterator rbegin() { return reverse_iterator( end() ); }
    reverse_iterator rend()   { return reverse_iterator( begin() ); }
    const_reverse_iterator rbegin() const { return const_reverse_iterator( end() ); }
    const_reverse_iterator rend()   const { return const_reverse_iterator( begin() ); }
    */

    void push_back(Ttype value)
    {
        //insert(end(), value);
        if(len >= cap) reserve(cap ? cap*2 : default_size());
      #ifdef UsePlacementNew
        data[len++].Construct(value);
      #else
        /*new(&data[len++]) T ( value );*/ data[len++] = value;
      #endif
    }

    iterator insert(iterator pos, Ttype value)
    {
        size_type ins_pos = pos - begin();
        if(len < cap)
        {
            if(ins_pos == len)
            {
              #ifdef UsePlacementNew
                data[ins_pos].Construct(value);
              #else
                /*new(&data[ins_pos]) T( value );*/ data[ins_pos] = value;
              #endif
            }
            else
            {
                move_construct(&data[len], &data[len-1], 1);
                move_assign_backwards(&data[ins_pos+1], &data[ins_pos], len-ins_pos);
                data[ins_pos] = value;
            }
            ++len;
            return data+ins_pos;
        }
        size_type newcap = cap ? cap*2 : default_size();
        if(ins_pos == len)
        {
            reserve(newcap);
          #ifdef UsePlacementNew
            data[ins_pos].Construct(value);
          #else
            /*new(&data[ins_pos]) T( value );*/ data[ins_pos] = value;
          #endif
            ++len;
            return data+ins_pos;
        }
        T * newdata = allocate(newcap);
        if(!newdata) fprintf(stdout, "VecType: Failed to allocate %u bytes\n", (unsigned)newcap);
        move_construct(&newdata[0], &data[0], ins_pos);
      #ifdef UsePlacementNew
        newdata[ins_pos].Construct(value);
      #else
        /*new(&newdata[ins_pos]) T( value );*/ newdata[ins_pos] = value;
      #endif
        move_construct(&newdata[ins_pos+1], &data[ins_pos], len-ins_pos);
        destroy(&data[0], len);
        deallocate(data, cap);
        ++len;
        data = newdata;
        cap  = newcap;
        return data+ins_pos;
    }

    void insert(iterator pos,
                T const* first,
                T const* last)
    {
        size_type ins_pos = pos - begin();
        size_type count   = (size_type) (last-first);

        if(len+count <= cap)
        {
            if(ins_pos == len)
                copy_construct(&data[ins_pos], first, count);
            else
            {
                /*******
                 *      012345678
                 *           ^NM
                 *      ins_pos = 5, count = 2
                 *      len = 9, new_length = 11
                 *      tail_length = 4 ("5678")
                 *      tail_source_pos = 5
                 *      tail_target_pos = 7
                 *      num_tail_bytes_after_current_area = min(tail_length, count)
                 */
                //unsigned new_length = len + count;
                unsigned tail_length = len - ins_pos;
                unsigned tail_source_pos = ins_pos;
                unsigned tail_target_pos = ins_pos + count;
                unsigned num_tail_bytes_after_current_area =
                    count < tail_length ? count : tail_length;
                unsigned num_tail_bytes_to_movecopy =
                    tail_length - num_tail_bytes_after_current_area;
              #ifdef UsePlacementNew
                /*fprintf(stdout, "Input (%u):", count);
                {for(unsigned a=0; a<count; ++a) fprintf(stdout, "\n\t%p", &first[a][0]);}
                fprintf(stdout, "\n");
                fprintf(stdout, "Before (%u + %u):", len, cap);
                {for(unsigned a=0; a<len; ++a) fprintf(stdout, "\n\t%p", &data[a][0]);}
                {for(unsigned a=len; a<cap; ++a) fprintf(stdout, "\n\t[%p]", &data[a][0]);}
                fprintf(stdout, "\n");*/
              #else
                /*fprintf(stdout, "Input (%u): '%.*s'\n", count, count*sizeof(T), (const char*)first);
                fprintf(stdout, "Before (%u + %u):", len,cap);
                fprintf(stdout, "\n\t%u:\t'%.*s'\n\t%u:\t'%.*s'\n",
                    len, len*sizeof(T), (const char*)data,
                    (cap-len), (cap-len)*sizeof(T), (const char*)(data+len));*/
              #endif
                move_construct(&data[tail_target_pos + num_tail_bytes_to_movecopy],
                               &data[tail_source_pos+num_tail_bytes_to_movecopy],
                               num_tail_bytes_after_current_area);
              #ifdef UsePlacementNew
                /*fprintf(stdout, "After move_construct(%u,%u,%u):",
                    tail_target_pos + num_tail_bytes_to_movecopy,
                    tail_source_pos + num_tail_bytes_to_movecopy,
                    num_tail_bytes_after_current_area);
                {for(unsigned a=0; a<len; ++a) fprintf(stdout, "\n\t%p", &data[a][0]);}
                {for(unsigned a=len; a<cap; ++a) fprintf(stdout, "\n\t[%p]", &data[a][0]);}
                fprintf(stdout, "\n");*/
              #else
                /*fprintf(stdout, "After move_construct(%u,%u,%u):",
                    tail_target_pos + num_tail_bytes_to_movecopy,
                    tail_source_pos + num_tail_bytes_to_movecopy,
                    num_tail_bytes_after_current_area);
                fprintf(stdout, "\n\t%u:\t'%.*s'\n\t%u:\t'%.*s'\n",
                    len, len*sizeof(T), (const char*)data,
                    (cap-len), (cap-len)*sizeof(T), (const char*)(data+len));*/
              #endif
                if(num_tail_bytes_to_movecopy > 0)
                {
                    move_assign_backwards(
                        &data[ins_pos+count],
                        &data[ins_pos],
                        num_tail_bytes_to_movecopy);
                  #ifdef UsePlacementNew
                    /*fprintf(stdout, "After move_assign_backwards(%u,%u,%u):", ins_pos+count,ins_pos,num_tail_bytes_to_movecopy);
                    {for(unsigned a=0; a<len; ++a) fprintf(stdout, "\n\t%p", &data[a][0]);}
                    {for(unsigned a=len; a<cap; ++a) fprintf(stdout, "\n\t[%p]", &data[a][0]);}
                    fprintf(stdout, "\n");*/
                  #else
                    /*fprintf(stdout, "After move_assign_backwards(%u,%u,%u):", ins_pos+count,ins_pos,num_tail_bytes_to_movecopy);
                    fprintf(stdout, "\n\t%u:\t'%.*s'\n\t%u:\t'%.*s'\n",
                        len, len*sizeof(T), (const char*)data,
                        (cap-len), (cap-len)*sizeof(T), (const char*)(data+len));*/
                  #endif
                }
                unsigned num_piece_bytes_after_current_area =
                    (ins_pos + count) > len ? (ins_pos+count)-len : 0;
                unsigned num_piece_bytes_to_copy =
                    count - num_piece_bytes_after_current_area;
                if(num_piece_bytes_to_copy > 0)
                {
                    copy_assign(
                        &data[ins_pos],
                        first,
                        num_piece_bytes_to_copy);
                  #ifdef UsePlacementNew
                    /*fprintf(stdout, "After copy_assign(%u,%u):", ins_pos,num_piece_bytes_to_copy);
                    {for(unsigned a=0; a<len; ++a) fprintf(stdout, "\n\t%p", &data[a][0]);}
                    {for(unsigned a=len; a<cap; ++a) fprintf(stdout, "\n\t[%p]", &data[a][0]);}
                    fprintf(stdout, "\n");
                    fflush(stdout);*/
                  #else
                    /*fprintf(stdout, "After copy_assign(%u,%u):", ins_pos,num_piece_bytes_to_copy);
                    fprintf(stdout, "\n\t%u:\t'%.*s'\n\t%u:\t'%.*s'\n",
                        len, len*sizeof(T), (const char*)data,
                        (cap-len), (cap-len)*sizeof(T), (const char*)(data+len));
                    fflush(stdout);*/
                  #endif
                }
                if(num_piece_bytes_after_current_area > 0)
                {
                    copy_construct(
                        &data[len],
                        first+num_piece_bytes_to_copy,
                        num_piece_bytes_after_current_area);
                  #ifdef UsePlacementNew
                    /*fprintf(stdout, "After copy_construct(%u,%u):", len, num_piece_bytes_after_current_area);
                    {for(unsigned a=0; a<len; ++a) fprintf(stdout, "\n\t%p", &data[a][0]);}
                    {for(unsigned a=len; a<cap; ++a) fprintf(stdout, "\n\t[%p]", &data[a][0]);}
                    fprintf(stdout, "\n");
                    fflush(stdout);*/
                  #else
                    /*fprintf(stdout, "After copy_construct(%u,%u):", len, num_piece_bytes_after_current_area);
                    fprintf(stdout, "\n\t%u:\t'%.*s'\n\t%u:\t'%.*s'\n",
                        len, len*sizeof(T), (const char*)data,
                        (cap-len), (cap-len)*sizeof(T), (const char*)(data+len));
                    fflush(stdout);*/
                  #endif
                }
            }
            len += count;
            goto Done;
        }
      {
        size_type newcap = (cap+count)*2;
        /*if(ins_pos == len)
        {
            reserve(newcap);
            copy_construct(&data[ins_pos], first, count);
            len += count;
            goto Done;
        }*/
        T * newdata = allocate(newcap);
        if(!newdata) fprintf(stdout, "VecType: Failed to allocate %u bytes\n", (unsigned)newcap);
        move_construct(&newdata[0], &data[0], ins_pos);
        copy_construct(&newdata[ins_pos], first, count);
        move_construct(&newdata[ins_pos+count], &data[ins_pos], len-ins_pos);
        #ifndef UsePlacementNew
        /*fprintf(stdout, "Became '%.*s'\n", (len+count)*sizeof(T), (const char*)newdata);*/
        #endif
        destroy(&data[0], len);
        deallocate(data, cap);
        len += count;
        data = newdata;
        cap  = newcap;
      }
   Done:;
        #ifndef UsePlacementNew
        /*fprintf(stdout, "Indeed, became '%.*s'\n", len*sizeof(T), (const char*)data);*/
        #endif
    }

    void erase(iterator pos)
    {
        size_type del_pos = pos - begin();
        move_assign(&data[del_pos], &data[del_pos+1], len-del_pos-1);
        destroy(&data[--len], 1);
    }
    void erase(iterator first, iterator last)
    {
        size_type del_pos = first - begin();
        size_type count   = last - first;
        if(!count) return;
        move_assign(&data[del_pos], &data[del_pos+count], len-del_pos-count);
        destroy(&data[len-count], count);
        len -= count;
    }

    void reserve(size_type newcap)
    {
        if(cap < newcap)
        {
            T * newdata = allocate(newcap);
            if(!newdata) fprintf(stdout, "VecType: Failed to allocate %u bytes\n", (unsigned)newcap);
            move_construct(&newdata[0], &data[0], len);
            destroy(&data[0], len);
            deallocate(data, cap);
            data = newdata;
            cap  = newcap;
        }
    }

    void pop_back()
    {
        destroy(&data[--len], 1);
    }

    void resize(size_type newlen)
    {
        if(newlen < len)
        {
            destroy(&data[newlen], len-newlen);
            len = newlen;
        }
        else if(newlen == len)
            return;
        else if(newlen <= cap) // newlen > len, too.
        {
            construct(&data[len], newlen-len);
            len = newlen;
        }
        else // newlen > cap, and newlen > len.
        {
            // reallocation required
            size_type newcap = newlen;
            T * newdata = allocate(newcap);
            if(!newdata) fprintf(stdout, "VecType: Failed to allocate %u bytes\n", (unsigned)newcap);
            move_construct(&newdata[0], &data[0], len);
            destroy(&data[0], len);
            deallocate(data, cap);
            construct(&newdata[len], newlen-len);
            data = newdata;
            len  = newlen;
            cap  = newcap;
        }
    }

    void resize(size_type newlen, Ttype value)
    {
        if(newlen < len)
        {
            destroy(&data[newlen], len-newlen);
            len = newlen;
        }
        else if(newlen == len)
            return;
        else if(newlen <= cap) // newlen > len, too.
        {
            construct(&data[len], newlen-len, value);
            len = newlen;
        }
        else // newlen > cap, and newlen > len.
        {
            // reallocation required
            size_type newcap = newlen;
            T * newdata = allocate(newcap);
            if(!newdata) fprintf(stdout, "VecType: Failed to allocate %u bytes\n", (unsigned)newcap);
            move_construct(&newdata[0], &data[0], len);
            destroy(&data[0], len);
            deallocate(data, cap);
            construct(&newdata[len], newlen-len, value);
            data = newdata;
            len  = newlen;
            cap  = newcap;
        }
    }

    void swap(VecType& b)
    {
     /* std::swap(data, b.data);
        std::swap(len,  b.len);
        std::swap(cap,  b.cap);
        */
        {size_type l=b.len; b.len=len; len=l;}
        {size_type l=b.cap; b.cap=cap; cap=l;}
        {T * d = b.data; b.data=data; data=d;}
    }

    int empty() const { return len==0; }
    void clear()
    {
        if(!cap) return;
        destroy(&data[0], len);
        len = 0;
        deallocate(data, cap);
        data = 0;
        cap = 0;
    }
    size_type size()     const { return len; }
    size_type capacity() const { return cap; }

private:
    static void construct(T * target, size_type count)
    {
        for(size_type a=0; a<count; ++a)
        {
      #ifdef UsePlacementNew
            target[a].Construct();
      #else
            /*new(&target[a]) T();*/ target[a] = (T)0;
      #endif
        }
    }
    static void construct(T * target, size_type count, Ttype param)
    {
        for(size_type a=0; a<count; ++a)
      #ifdef UsePlacementNew
            target[a].Construct(param);
      #else
            /*new(&target[a]) T(param);*/ target[a] = param;
      #endif
    }
    static void destroy(T * target, size_type count)
    {
      #ifdef UsePlacementNew
        for(size_type a=count; a-- > 0; )
            /*target[a].~T();*/
            target[a].Destruct();
      #else
        target=target;
        count=count;
      #endif
    }
    static void move_assign(T * target, T * source, size_type count)
    {
#ifdef __GXX_EXPERIMENTAL_CXX0X__
        for(size_type a=0; a<count; ++a)
            target[a] = std::move(source[a]);
#else
  #ifdef UsePlacementNew
        for(size_type a=0; a<count; ++a)
            target[a].swap(source[a]);
  #else
        copy_assign(target, source, count);
  #endif
#endif
    }
    static void move_assign_backwards(T * target, T * source, size_type count)
    {
#ifdef __GXX_EXPERIMENTAL_CXX0X__
        for(size_type a=count; a-- > 0; )
            target[a] = std::move(source[a]);
#else
  #ifdef UsePlacementNew
        for(size_type a=count; a-- > 0; )
            target[a].swap(source[a]);
  #else
        copy_assign_backwards(target, source, count);
  #endif
#endif
    }
    static void move_construct(T * target, T * source, size_type count)
    {
#ifdef __GXX_EXPERIMENTAL_CXX0X__
        for(size_type a=0; a<count; ++a)
            new(&target[a]) T( std::move(source[a]) );
#else
  #ifdef UsePlacementNew
        for(size_type a=0; a<count; ++a)
        {
            target[a].Construct();
            target[a].swap(source[a]);
        }
  #else
        copy_construct(target, source, count);
  #endif
#endif
    }
    static T const*
            copy_assign(T * target, T const* source, size_type count)
    {
        for(size_type a=0; a<count; ++a)
            target[a] = *source++;
        return source;
    }
    static T const*
            copy_assign_backwards(T * target, T const* source, size_type count)
    {
        for(size_type a=count; a-- > 0; )
            target[a] = source[a];
        return source;
    }
    static T const*
            copy_construct(T * target, T const* source, size_type count)
    {
        for(size_type a=0; a<count; ++a)
          #ifdef UsePlacementNew
            target[a].Construct(*source++);
          #else
            /*new(&target[a]) T( *source++ );*/ target[a] = *source++;
          #endif
        return source;
    }

    static size_t default_size()
    {
        return 1; //sizeof(T) < 16 ? 2 : (sizeof(T) < 256 ? 2 : 1);
    }

private:
    static T * allocate(size_type n)
    {
        return (T *) malloc( n * sizeof(T) );
    }
    static void deallocate(T * p, size_type n)
    {
        free( (void*) p );
        n=n;
    }

private:
    T * data;
    size_type len, cap;

#undef Ttype
};
