#include <bio/alphabet/nucleotide/sam_dna16.hpp>
#include <bio/alphabet/fmt.hpp>

int main()
{
    using bio::operator""_sam_dna16;

    bio::sam_dna16 my_letter{'A'_sam_dna16};

    my_letter.assign_char('=');

    my_letter.assign_char('F'); // unknown characters are implicitly converted to N.
    fmt::print("{}\n", my_letter); // "N";
}
