#include <algorithm>
#include <set>
#include <tuple>
#include <vector>

//! [create]
#include <bio/alphabet/all.hpp> // for working with alphabets directly

using bio::operator""_dna4;

int main ()
{
    // Two objects of bio::dna4 alphabet constructed with a char literal.
    bio::dna4 ade = 'A'_dna4;
    bio::dna4 gua = 'G'_dna4;

    // Two additional objects assigned explicitly from char or rank.
    bio::dna4 cyt, thy;
    cyt.assign_char('C');
    thy.assign_rank(3);

    // Further code here...
//! [create]
    assert(cyt == 'C'_dna4);
    assert(thy == 'T'_dna4);

//! [rank]
    // Get the rank type of the alphabet (here uint8_t).
    using rank_type = bio::alphabet_rank_t<bio::dna4>;

    // Retrieve the numerical representation (rank) of the objects.
    rank_type rank_a = ade.to_rank();   // => 0
    rank_type rank_g = gua.to_rank();   // => 2
//! [rank]
    assert(rank_a == 0u);
    assert(rank_g == 2u);

//! [char]
    // Get the character type of the alphabet (here char).
    using char_type = bio::alphabet_char_t<bio::dna4>;

    // Retrieve the character representation.
    char_type char_a = ade.to_char();   // => 'A'
    char_type char_g = gua.to_char();   // => 'G'
//! [char]
    assert(char_a == 'A');
    assert(char_g == 'G');

//! [char_strict]
    // Assign from character with value check.
    bio::assign_char_strictly_to('C', cyt);

    // bio::assign_char_strictly_to('X', thy); // would throw bio::invalid_char_assignment
//! [char_strict]
    assert(cyt == 'C'_dna4);

//! [size]
    // Get the alphabet size as class member of the alphabet.
    uint8_t const size1 = bio::dna4::alphabet_size;        // => 4
//! [size]
    assert(size1 == 4u);

//! [compare]
    // Equality and comparison of seqan::dna4 symbols.
    bool eq  = (cyt == 'C'_dna4); // true
    bool neq = (thy != 'C'_dna4); // true
    bool geq = (cyt >= 'C'_dna4); // true
    bool gt  = (thy >  'C'_dna4); // true
    bool seq = (cyt <= 'C'_dna4); // true
    bool st  = (ade <  'C'_dna4); // true

    // Sort a vector of symbols.
    std::vector<bio::dna4> some_nucl{"GTA"_dna4};
    std::sort(some_nucl.begin(), some_nucl.end()); // some_nucl: "AGT"
//! [compare]
    assert(eq && neq && geq && gt && seq && st);
    assert(some_nucl == "AGT"_dna4);

//! [gapped]
    // Assign a gap symbol to a gapped RNA alphabet.
    bio::gapped<bio::rna5> sym = bio::gap{};                         // => -

    using bio::operator""_rna5;
    // Each bio::rna5 symbol is still valid.
    sym = 'U'_rna5;                                   // => U

    // The alphabet size is six (AUGCN-).
    uint8_t const size2 = bio::gapped<bio::rna5>::alphabet_size;   // => 6
//! [gapped]
    assert(size2 == 6u);

//! [containers]
    using bio::operator""_dna5;

    // Examples of different container types with SeqAn's alphabets.
    std::vector<bio::dna5> dna_sequence{"GATTANAG"_dna5};
    std::pair<bio::gapped<bio::dna4>, bio::gapped<bio::dna4>> alignment_column{bio::gap{}, thy};
    std::set<bio::dna4> pyrimidines{'C'_dna4, 'T'_dna4};
//! [containers]

    // Prevent -Wunused-variable warnings.
    (void)rank_a;
    (void)rank_g;
    (void)char_a;
    (void)char_g;
    (void)size1;
    (void)size2;
    (void)dna_sequence;
    (void)alignment_column;
    (void)pyrimidines;
    (void)eq;
    (void)neq;
    (void)geq;
    (void)gt;
    (void)seq;
    (void)st;

//! [closing]
    return 0;
}
//! [closing]
