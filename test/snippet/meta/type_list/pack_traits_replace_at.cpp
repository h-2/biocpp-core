#include <bio/meta/type_list/traits.hpp>

int main()
{
    // Replace the second element of the type pack with int.
    static_assert(std::same_as<bio::type_list<int, int, bool, double>,
                               bio::pack_traits::replace_at<int, 1, int, float, bool, double>>);
}
