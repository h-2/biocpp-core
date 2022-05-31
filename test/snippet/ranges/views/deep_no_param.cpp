#include <vector>

#include <bio/alphabet/nucleotide/dna5.hpp>
#include <bio/ranges/views/deep.hpp>
#include <ranges>

namespace my
{
// You can create a permanent alias:
inline auto const deep_reverse = seqan3::views::deep{std::views::reverse};
}

int main()
{
    using seqan3::operator""_dna5;

    std::vector<seqan3::dna5_vector> foo{"AAATTT"_dna5, "CCCGGG"_dna5};

    auto r = foo | std::views::reverse;                     // == [ [C,C,C,G,G,G], [A,A,A,T,T,T] ]

    // These two are equivalent:
    auto e = foo | my::deep_reverse;                       // == [ [T,T,T,A,A,A], [G,G,G,C,C,C] ]
    auto d = foo | seqan3::views::deep{std::views::reverse}; // == [ [T,T,T,A,A,A], [G,G,G,C,C,C] ]
}
