#include <vector>

#include <bio/alphabet/nucleotide/dna4.hpp>
#include <bio/meta/debug_stream.hpp>
#include <bio/ranges/container/concatenated_sequences.hpp>

int main()
{
    using seqan3::operator""_dna4;

    seqan3::concatenated_sequences<seqan3::dna4_vector> concat1{"ACGT"_dna4, "GAGGA"_dna4};
    seqan3::debug_stream << concat1[0] << '\n'; // "ACGT"

    std::vector<seqan3::dna4_vector> concat2{"ACTA"_dna4, "AGGA"_dna4};

    concat1 = concat2;               // you can assign from other ranges

    concat2[0] = "ATTA"_dna4;        // this works for vector of vector
    concat1[0][1] = 'T'_dna4;        // and this works for concatenated_sequences
    seqan3::debug_stream << concat1[0] << '\n'; // "ATTA"

    // if you know that you will be adding ten vectors of length ten:
    std::vector<seqan3::dna4> vector_of_length10{"ACGTACGTAC"_dna4};

    concat1.reserve(10);
    concat1.concat_reserve(10 * vector_of_length10.size());
    while (concat1.size() < 10)
    {
        // ...
        concat1.push_back(vector_of_length10);
    }
}
