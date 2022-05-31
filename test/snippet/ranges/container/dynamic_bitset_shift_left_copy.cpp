#include <bio/meta/debug_stream.hpp>
#include <bio/ranges/container/dynamic_bitset.hpp>

int main()
{
    seqan3::dynamic_bitset const t1{"10001100"};

    seqan3::dynamic_bitset t2 = t1 << 3;
    seqan3::debug_stream << t2 << '\n'; // 0110'0000
}
