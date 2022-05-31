#include <bio/alphabet/nucleotide/rna5.hpp>
#include <bio/meta/debug_stream.hpp>
int main()
{
    using bio::operator""_rna5;

    bio::rna5 my_letter{'A'_rna5};

    my_letter.assign_char('C');
    
    my_letter.assign_char('F'); // unknown characters are implicitly converted to N.
    if (my_letter.to_char() == 'N')
        bio::debug_stream << "yeah\n"; // "yeah";
}
