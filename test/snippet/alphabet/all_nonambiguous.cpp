#include <bio/alphabet/nucleotide/dna4.hpp>
#include <bio/alphabet/fmt.hpp>

int main()
{
    bio::dna4 my_letter;
    bio::assign_rank_to(0, my_letter);               // assign an A via rank interface
    bio::assign_char_to('A', my_letter);             // assign an A via char interface

    std::cout << bio::to_char(my_letter);            // prints 'A'
    std::cout << (unsigned)bio::to_rank(my_letter);  // prints 0
    // we have to add the cast here, because uint8_t is also treated as a char type by default :(

    // Using the format library:
    fmt::print("{}", bio::to_char(my_letter));      // prints 'A'
    fmt::print("{}", my_letter);                    // prints 'A' (calls to_char() automatically!)
    fmt::print("{}", bio::to_rank(my_letter));      // prints 0   (casts uint8_t to unsigned automatically!)
}
