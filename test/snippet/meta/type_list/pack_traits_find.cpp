#include <bio/meta/type_list/traits.hpp>

int main()
{
    // Double is not in the pack so find returns -1. However, bool is in the pack so find will return 2.
    static_assert(bio::pack_traits::find<double, int, float, bool> == -1);
    static_assert(bio::pack_traits::find<bool, int, float, bool> == 2);
}
