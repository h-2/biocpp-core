#include <bio/ranges/detail/random_access_iterator.hpp>

template <typename range_type>
class my_random_access_iterator : public bio::detail::random_access_iterator_base<range_type, bio::detail::random_access_iterator>
{
//...
};

int main() {}
