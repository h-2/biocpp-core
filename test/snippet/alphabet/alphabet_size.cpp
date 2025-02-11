#include <bio/alphabet/concept.hpp>
#include <bio/alphabet/adaptation/char.hpp>
#include <bio/alphabet/nucleotide/dna5.hpp>

int main()
{
    auto r2 = bio::alphabet_size<char>;            // calls bio::custom::alphabet_size(char{}); r2 == 256
    auto r3 = bio::alphabet_size<bio::dna5>;    // returns dna5::alphabet_size; r3 == 5
}
