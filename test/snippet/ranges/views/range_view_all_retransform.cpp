#include <bio/alphabet/nucleotide/dna4.hpp>
#include <bio/ranges/views/complement.hpp>
#include <bio/ranges/views/to.hpp>
#include <ranges>

int main()
{
    using bio::operator""_dna4;

    bio::dna4_vector vec{"ACGGTC"_dna4};
    auto vec_view2 = bio::views::complement(vec);

    // re-convert to container
    bio::dna4_vector complemented = vec_view2 | bio::views::to<bio::dna4_vector>();
    assert(complemented == "TGCCAG"_dna4);

    // also possible in one step
    bio::dna4_vector reversed = vec | std::views::reverse | bio::views::to<bio::dna4_vector>();
    assert(reversed == "CTGGCA"_dna4);
}
