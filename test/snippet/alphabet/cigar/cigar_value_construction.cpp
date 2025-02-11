#include <bio/alphabet/cigar/cigar.hpp>
#include <bio/alphabet/cigar/cigar_op.hpp>
#include <bio/alphabet/fmt.hpp>

int main()
{
    using bio::operator""_cigar_op;

    bio::cigar letter1{0};
    // creates 0M, as the cigar_op field is not provided.
    bio::cigar letter2{'M'_cigar_op};
    // creates 0M, as the integer field is not provided.

    if (letter1 == letter2)
        fmt::print("{}", "yeah\n"); // yeah

    bio::cigar letter3{10, 'I'_cigar_op};
    // creates 10I, as both fields are explicitly given.
}
