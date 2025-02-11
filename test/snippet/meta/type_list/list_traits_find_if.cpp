#include <bio/meta/type_list/traits.hpp>

int main()
{
    using list_t = bio::type_list<int, float, double>;

    // None of the types in list_t is a pointer so find_if returns -1. However, int and bool are both integral,
    // so find_if returns 0 for the first occurrence.
    static_assert(bio::list_traits::find_if<std::is_pointer, list_t> == -1);
    static_assert(bio::list_traits::find_if<std::is_integral, list_t> == 0);
}
