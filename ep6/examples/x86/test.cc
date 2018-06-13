extern "C" {
    // stpcpyn
    unsigned long Length(const char* s);
    char*         stpcpyn(char* output, const char* s, unsigned long n);
    // booleans
    char*         strchr(const char* string, char ch);
    long          strcmp(const char* string1, const char* string2);
    char*         strpos(const char* haystack, const char* needle);
    // conjugator
    bool          find(char ch, const char* s);
    //unsigned long Length(const char* s);
    unsigned char LastCharN(const char* s, unsigned long n);
    bool          IsVowel(char ch);
    bool          IsVowelTypeChar(char ch);
    bool          IsBackVowelTypeChar(char ch);
    bool          IsFrontVowelTypeChar(char c);
    bool          IsOtherVowelTypeChar(char c);
    bool          IsNumber(char ch);
    bool          IsKPT(char ch);
    bool          IsFront(const char* s);
    bool          IsAbbrev(const char* s);
    bool          IsEs(const char* s);
    bool          IsDe(const char* s);
    bool          IsAlien(const char* s);
    bool          EndsWithVowel(const char* s);
    bool          DoubleHard(const char* s);
    bool          AkiEnd(const char* s);
    char*         stpcpy(char* output, const char* s);
    //char*         stpcpyn(char* output, const char* s, unsigned long n);
    char*         append(char* output, char ch);
    char*         SoftStem(char* output, const char* s);
    char*         PartitiveStem(char* output, const char* s);
    char*         EssiveStem(char* output, const char* s);
    char*         IllativeStem(char* output, const char* s);
    char*         Do_N(char* output, const char* s);
    char*         Do_A(char* output, const char* s);
    char*         Do_SSA(char* output, const char* s);
    char*         Do_STA(char* output, const char* s);
    char*         Do_LLA(char* output, const char* s);
    char*         Do_LTA(char* output, const char* s);
    char*         Do_TTA(char* output, const char* s);
    char*         Do_NA(char* output, const char* s);
    char*         Do_HUN(char* output, const char* s);
    char*         Do_LLE(char* output, const char* s);
    char*         Do_KSI(char* output, const char* s);
}

#include <initializer_list>
#include <stdio.h>
int main()
{
    char Buf[64]="quite lengthy string";
#if 1
    printf("Testing boolean functions:\n");
    printf(" strcmp(\"kissa\",\"koira\") = %ld\n", strcmp("kissa","koira"));
    printf(" strcmp(\"kissa\",\"kissani\") = %ld\n", strcmp("kissa","kissani"));
    printf(" strcmp(\"kissa\",\"kissa\") = %ld\n", strcmp("kissa","kissa"));
    printf(" strchr(\"hae kaupasta maitoa\",'k') = %s\n", strchr("hae kaupasta maitoa",'k'));
    printf(" strpos(\"hae kaupasta maitoa\",\"kau\") = %s\n", strpos("hae kaupasta maitoa","kau"));
    printf(" strpos(\"kau\",\"hae kaupasta maitoa\") = %s\n", strpos("kau","hae kaupasta maitoa"));

    printf("Testing conjugator:\n");
    for(auto s: {"Matti","Mikko","Magus","John","Show","XYZ","Crono","Marle","Lucca","Robo","R66","Cless","Chester","Ihminen","Matt"})
    {
        for(auto m: {Do_N,Do_A,Do_SSA,Do_STA,Do_LLA,Do_LTA,Do_TTA,Do_NA,Do_HUN,Do_LLE,Do_KSI})
        {
            m(Buf, s);
            printf("%-12s", Buf);
            fflush(stdout);
        }
        printf("\n");
    }
#else
    printf(" Length(\"kissa\") = %d\n", Length("kissa"));
    printf(" Length(\"koirani\") = %d\n", Length("koirani"));
    char* result = stpcpyn(Buf, "eksytys", 4);
    printf(" stpcpyn(%p:\"quite lengthy string\", \"eksytys\", 4) = result=(%p,%s) -- Buf ends up: %s\n",
        Buf,
        result,result,
        Buf);
#endif
}
