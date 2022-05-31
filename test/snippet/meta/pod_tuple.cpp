#include <bio/meta/pod_tuple.hpp>
#include <bio/meta/debug_stream.hpp>

int main()
{
    bio::pod_tuple<int, float> t{3, 4.7};
    static_assert(std::is_standard_layout_v<bio::pod_tuple<int, float>>);
    static_assert(std::is_trivial_v<bio::pod_tuple<int, float>>);

    // template parameters are automatically deduced:
    bio::pod_tuple t2{17, 3.7f, 19l};

    bio::debug_stream << std::get<0>(t2) << '\n'; // 17

    auto [ i, f, l ] = t2; // creates an int i with value 17, float f...
}