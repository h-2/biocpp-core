#include <bio/alphabet/nucleotide/dna15.hpp>
#include <bio/meta/debug_stream.hpp>

int main()
{
    using bio::operator""_dna15;

    bio::dna15 my_letter{'A'_dna15};

    my_letter.assign_char('C');

    my_letter.assign_char('F'); // unknown characters are implicitly converted to N.
    if (my_letter.to_char() == 'N')
        bio::debug_stream << "yeah\n"; // "yeah";

}
