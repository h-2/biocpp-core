#include <sstream>

#include <bio/alphabet/nucleotide/dna5.hpp>
#include <bio/meta/debug_stream.hpp>

int main()
{
    using seqan3::operator""_dna5;

    std::ostringstream o;
    seqan3::debug_stream.set_underlying_stream(o);

    seqan3::debug_stream << "ACGT"_dna5;

    o.flush();
    seqan3::debug_stream << o.str(); // prints the string stream's buffer: "ACGT"
}
