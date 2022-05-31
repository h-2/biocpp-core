//! [example]
#include <iostream>

#include <bio/alphabet/nucleotide/dna4.hpp>
#include <bio/meta/debug_stream.hpp>
#include <bio/ranges/views/char_to.hpp>
#include <bio/ranges/views/translate_join.hpp>

using bio::operator""_dna4;

int main()
{
    // Input range needs to be two-dimensional
    std::vector<std::vector<bio::dna4> > vec{"ACGTACGTACGTA"_dna4, "TCGAGAGCTTTAGC"_dna4};

    // Translation with default parameters
    auto v1 = vec | bio::views::translate_join;
    bio::debug_stream << v1 << "\n"; // [TYVR,RTYV,VRT,YVRT,TYVR,RTY,SRAL,REL*,ESFS,AKAL,LKLS,*SSR]

    // Access the third forward frame (index_frame 2) of the second input sequence (index_seq 1)
    // Required frames per sequence s = 6
    // n = (index_seq * s) + j
    //   = 1 * 6 + 2
    //   = 8

    auto third_frame_second_seq = v1[1 * 6 + 2];
    bio::debug_stream << third_frame_second_seq << "\n"; // ESFS

    // Translation with custom translation frame
    auto v2 = vec | bio::views::translate_join(bio::translation_frames::FWD_FRAME_0);
    bio::debug_stream << v2 << "\n"; // [TYVR,SRAL]

    return 0;
}
//! [example]