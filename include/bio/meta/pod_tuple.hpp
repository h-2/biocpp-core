// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

/*!\file
 * \author Hannes Hauswedell <hannes.hauswedell AT decode.is>
 * \brief Provides bio::meta::pod_tuple
 */

#pragma once

#include <tuple>
#include <type_traits>

#include <bio/meta/concept/core_language.hpp>
#include <bio/meta/type_list/traits.hpp>
#include <bio/meta/type_traits/template_inspection.hpp>

namespace bio::meta::detail
{
//!\cond
//!\brief A function that call unqualified get<>. Not SFINAE-safe to prevent recursive concept instantiation.
template <size_t i>
constexpr decltype(auto) adl_get(auto && arg) noexcept(noexcept(get<i>(std::forward<decltype(arg)>(arg))))
{
    return get<i>(std::forward<decltype(arg)>(arg));
}

//!\brief A function that call unqualified get<>. Not SFINAE-safe to prevent recursive concept instantiation.
template <typename t>
constexpr decltype(auto) adl_get(auto && arg) noexcept(noexcept(get<t>(std::forward<decltype(arg)>(arg))))

{
    return get<t>(std::forward<decltype(arg)>(arg));
}
//!\endcond
} // namespace bio::meta::detail

namespace bio::meta
{

/*!\brief Tuple template that is an aggregate and a plain-old-datatype. [base-case, no elements]
 * \ingroup meta
 * \implements bio::meta::tuple_like
 * \tparam ...types 0-n types.
 *
 * This class behaves like std::tuple, but it is itself a [PODType](https://en.cppreference.com/w/cpp/concept/PODType)
 * while std::tuple is not (even if all contained types are POD).
 *
 * It (only) supports [aggregate initialization](https://en.cppreference.com/w/cpp/language/aggregate_initialization),
 * and requires "brace elision", so you need to initialise with `{}` and not with `()` even in modern compilers.
 *
 * You can use bio::meta::get or std::get and also
 * [structured bindings](https://en.cppreference.com/w/cpp/language/declarations#Structured_binding_declaration)
 * to access the elements in the tuple.
 * The first element is also always accessible as `.first` and the second element as `.second`. Other elements
 * are not acessible as data members.
 *
 * \include test/snippet/meta/pod_tuple.cpp
 *
 */
template <typename... types>
struct pod_tuple
{};

//!\brief Tuple template that is an aggregate and a plain-old-datatype. [1-element specialisation]
template <typename type1>
struct pod_tuple<type1>
{
    //!\brief The first element as member.
    type1 first;

    /*!\name Comparison operators
     * \{
     */
    //!\brief Element-wise comparison.
    friend bool operator==(pod_tuple const & lhs, pod_tuple const & rhs) noexcept { return lhs.first == rhs.first; }

    //!\brief Element-wise comparison.
    friend auto operator<=>(pod_tuple const & lhs, pod_tuple const & rhs) noexcept { return lhs.first <=> rhs.first; }

    //!\brief Element-wise comparison.
    template <weakly_equality_comparable_with<type1> type1_>
    friend bool operator==(pod_tuple const & lhs, pod_tuple<type1_> const & rhs) noexcept
    {
        return lhs.first == rhs.first;
    }

    //!\brief Element-wise comparison.
    template <weakly_ordered_with<type1> type1_>
    friend auto operator<=>(pod_tuple const & lhs, pod_tuple<type1_> const & rhs) noexcept
    {
        return lhs.first <=> rhs.first;
    }
    //!\}

    //!\brief Convert to a pod_tuple of compatible types.
    template <typename target_t>
        requires(std::convertible_to<type1, target_t> || decays_to<target_t, type1>)
    constexpr operator pod_tuple<target_t>() const
    {
        return pod_tuple<target_t>{first};
    }

    /*!\name Index-based get<>()
     * \{
     */
    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto & get(bio::meta::pod_tuple<type1> & t) noexcept
        requires(i == 0)
    {
        return t.first;
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto const & get(bio::meta::pod_tuple<type1> const & t) noexcept
        requires(i == 0)
    {
        return t.first;
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto && get(bio::meta::pod_tuple<type1> && t) noexcept
        requires(i == 0)
    {
        return std::move(t.first);
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto const && get(bio::meta::pod_tuple<type1> const && t) noexcept
        requires(i == 0)
    {
        return std::move(t.first);
    }
    //!\}

    /*!\name Type-based get<>()
     * \{
     */
    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::same_as<type1> type1_>
    friend constexpr auto & get(bio::meta::pod_tuple<type1> & t) noexcept
    {
        return t.first;
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::same_as<type1> type1_>
    friend constexpr auto const & get(bio::meta::pod_tuple<type1> const & t) noexcept
    {
        return t.first;
    }

    // extra overloads for temporaries required, because members of temporaries may only be returned as temporaries
    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::same_as<type1> type1_>
    friend constexpr auto && get(bio::meta::pod_tuple<type1> && t) noexcept
    {
        return std::move(t.first);
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::same_as<type1> type1_>
    friend constexpr auto const && get(bio::meta::pod_tuple<type1> const && t) noexcept
    {
        return std::move(t.first);
    }
    //!\}
};

//!\brief Tuple template that is an aggregate and a plain-old-datatype. [2-element specialisation]
template <typename type1, typename type2>
struct pod_tuple<type1, type2>
{
    //!\brief The first element as member.
    type1 first;

    //!\brief The second element as member.
    type2 second;

    /*!\name Comparison operators
     * \{
     */
    //!\brief Element-wise comparison.
    friend bool operator==(pod_tuple const & lhs, pod_tuple const & rhs) noexcept
    {
        return std::tie(lhs.first, lhs.second) == std::tie(rhs.first, rhs.second);
    }

    //!\brief Element-wise comparison.
    friend auto operator<=>(pod_tuple const & lhs, pod_tuple const & rhs) noexcept
    {
        return std::tie(lhs.first, lhs.second) <=> std::tie(rhs.first, rhs.second);
    }

    //!\brief Element-wise comparison.
    template <weakly_equality_comparable_with<type1> type1_, weakly_equality_comparable_with<type2> type2_>
    friend bool operator==(pod_tuple const & lhs, pod_tuple<type1_, type2_> const & rhs) noexcept
    {
        return std::tie(lhs.first, lhs.second) == std::tie(rhs.first, rhs.second);
    }

    //!\brief Element-wise comparison.
    template <weakly_ordered_with<type1> type1_, weakly_ordered_with<type2> type2_>
    friend auto operator<=>(pod_tuple const & lhs, pod_tuple<type1_, type2_> const & rhs) noexcept
    {
        return std::tie(lhs.first, lhs.second) <=> std::tie(rhs.first, rhs.second);
    }
    //!\}

    //!\brief Convert to a pod_tuple of compatible types.
    template <typename target1_t, typename target2_t>
        requires((std::convertible_to<type1, target1_t> || decays_to<target1_t, type1>) &&
                 (std::convertible_to<type2, target2_t> || decays_to<target2_t, type2>))
    constexpr operator pod_tuple<target1_t, target2_t>() const
    {
        return pod_tuple<target1_t, target2_t>{first, second};
    }

    /*!\name Index-based get<>()
     * \{
     */
    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto & get(bio::meta::pod_tuple<type1, type2> & t) noexcept
        requires(i <= 1)
    {
        if constexpr (i == 0)
            return t.first;
        else
            return t.second;
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto const & get(bio::meta::pod_tuple<type1, type2> const & t) noexcept
        requires(i <= 1)
    {
        if constexpr (i == 0)
            return t.first;
        else
            return t.second;
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto && get(bio::meta::pod_tuple<type1, type2> && t) noexcept
        requires(i <= 1)
    {
        return std::move(get<i>(t));
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto const && get(bio::meta::pod_tuple<type1, type2> const && t) noexcept
        requires(i <= 1)
    {
        return std::move(get<i>(t));
    }
    //!\}

    /*!\name Type-based get<>()
     * \{
     */
    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <typename type_>
    friend constexpr auto & get(bio::meta::pod_tuple<type1, type2> & t) noexcept
        requires(meta::detail::pack_traits::count<type_, type1, type2> == 1)
    {
        return get<meta::detail::pack_traits::find<type_, type1, type2>>(t);
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <typename type_>
    friend constexpr auto const & get(bio::meta::pod_tuple<type1, type2> const & t) noexcept
        requires(meta::detail::pack_traits::count<type_, type1, type2> == 1)
    {
        return get<meta::detail::pack_traits::find<type_, type1, type2>>(t);
    }

    // extra overloads for temporaries required, because members of temporaries may only be returned as temporaries
    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <typename type_>
    friend constexpr auto && get(bio::meta::pod_tuple<type1, type2> && t) noexcept
        requires(meta::detail::pack_traits::count<type_, type1, type2> == 1)
    {
        return std::move(get<type_>(t));
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <typename type_>
    friend constexpr auto const && get(bio::meta::pod_tuple<type1, type2> const && t) noexcept
        requires(meta::detail::pack_traits::count<type_, type1, type2> == 1)
    {
        return std::move(get<type_>(t));
    }
    //!\}
};

//!\brief Tuple template that is an aggregate and a plain-old-datatype. [n-element specialisation]
template <typename type1, typename type2, typename... types>
    requires(sizeof...(types) > 0)
struct pod_tuple<type1, type2, types...>
{
    //!\brief The first element as member.
    type1               first;
    //!\brief The second element as member.
    type2               second;
    //!\cond DEV
    //!\brief The rest of the elements defined as a "recursive member".
    pod_tuple<types...> _tail;
    //!\endcond

    /*!\name Comparison operators
     * \{
     */
    //!\brief Element-wise comparison.
    friend bool operator==(pod_tuple const & lhs, pod_tuple const & rhs) noexcept
    {
        return std::tie(lhs.first, lhs.second, lhs._tail) == std::tie(rhs.first, rhs.second, rhs._tail);
    }

    //!\brief Element-wise comparison.
    friend auto operator<=>(pod_tuple const & lhs, pod_tuple const & rhs) noexcept
    {
        return std::tie(lhs.first, lhs.second, lhs._tail) <=> std::tie(rhs.first, rhs.second, rhs._tail);
    }

    //!\brief Element-wise comparison.
    template <weakly_equality_comparable_with<type1> type1_,
              weakly_equality_comparable_with<type2> type2_,
              typename... types_>
        requires((sizeof...(types_) == sizeof...(types)) && (weakly_equality_comparable_with<types, types_> && ...))
    friend bool operator==(pod_tuple const & lhs, pod_tuple<type1_, type2_, types_...> const & rhs) noexcept
    {
        return std::tie(lhs.first, lhs.second, lhs._tail) == std::tie(rhs.first, rhs.second, rhs._tail);
    }

    //!\brief Element-wise comparison.
    template <weakly_equality_comparable_with<type1> type1_,
              weakly_equality_comparable_with<type2> type2_,
              typename... types_>
        requires((sizeof...(types_) == sizeof...(types)) && (weakly_equality_comparable_with<types, types_> && ...))
    friend auto operator<=>(pod_tuple const & lhs, pod_tuple<type1_, type2_, types_...> const & rhs) noexcept
    {
        return std::tie(lhs.first, lhs.second, lhs._tail) <=> std::tie(rhs.first, rhs.second, rhs._tail);
    }
    //!\}

    //!\brief Convert to a pod_tuple of compatible types.
    template <typename target1_t, typename target2_t, typename... target_ts>
        requires(sizeof...(target_ts) == sizeof...(types) &&
                 (std::convertible_to<type1, target1_t> || decays_to<target1_t, type1>) &&
                 (std::convertible_to<type2, target2_t> || decays_to<target2_t, type2>) &&
                 ((std::convertible_to<types, target_ts> || decays_to<types, target_ts>)&&...))
    constexpr operator pod_tuple<target1_t, target2_t, target_ts...>() const
    {
        return [this]<size_t... indexes>(std::index_sequence<indexes...>) {
            return pod_tuple<target1_t, target2_t, target_ts...>{std::get<indexes>(*this)...};
        }(std::make_index_sequence<sizeof...(types) + 2>{});
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto & get(bio::meta::pod_tuple<type1, type2, types...> & t) noexcept
        requires(i <= sizeof...(types) + 2)
    {
        if constexpr (i == 0)
            return t.first;
        else if constexpr (i == 1)
            return t.second;
        else
            return get<i - 2>(t._tail);
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto const & get(bio::meta::pod_tuple<type1, type2, types...> const & t) noexcept
        requires(i <= sizeof...(types) + 2)
    {
        if constexpr (i == 0)
            return t.first;
        else if constexpr (i == 1)
            return t.second;
        else
            return get<i - 2>(t._tail);
    }

    // extra overloads for temporaries required, because members of temporaries may only be returned as temporaries
    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto && get(bio::meta::pod_tuple<type1, type2, types...> && t) noexcept
        requires(i <= sizeof...(types) + 2)
    {
        return std::move(get<i>(t));
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <std::size_t i>
    friend constexpr auto const && get(bio::meta::pod_tuple<type1, type2, types...> const && t) noexcept
        requires(i <= sizeof...(types) + 2)
    {
        return std::move(get<i>(t));
    }

    /*!\name Type-based get<>()
     * \{
     */
    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <typename type_>
    friend constexpr auto & get(bio::meta::pod_tuple<type1, type2, types...> & t) noexcept
        requires(meta::detail::pack_traits::count<type_, type1, type2, types...> == 1)
    {
        return get<meta::detail::pack_traits::find<type_, type1, type2, types...>>(t);
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <typename type_>
    friend constexpr auto const & get(bio::meta::pod_tuple<type1, type2, types...> const & t) noexcept
        requires(meta::detail::pack_traits::count<type_, type1, type2, types...> == 1)
    {
        return get<meta::detail::pack_traits::find<type_, type1, type2, types...>>(t);
    }

    // extra overloads for temporaries required, because members of temporaries may only be returned as temporaries
    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <typename type_>
    friend constexpr auto && get(bio::meta::pod_tuple<type1, type2, types...> && t) noexcept
        requires(meta::detail::pack_traits::count<type_, type1, type2, types...> == 1)
    {
        return std::move(get<type_>(t));
    }

    //!\brief The same as [std::get](https://en.cppreference.com/w/cpp/utility/tuple/get) on an std::tuple.
    template <typename type_>
    friend constexpr auto const && get(bio::meta::pod_tuple<type1, type2, types...> const && t) noexcept
        requires(meta::detail::pack_traits::count<type_, type1, type2, types...> == 1)
    {
        return std::move(get<type_>(t));
    }
    //!\}
};

//!\brief User defined deduction guide enables easy use.
//!\relates pod_tuple
template <typename... types>
pod_tuple(types &&...) -> pod_tuple<types...>;

} // namespace bio::meta

namespace std
{

/*!\brief Obtains the type of the specified element.
 * \implements bio::meta::transformation_trait
 * \relates bio::meta::pod_tuple
 * \see [std::tuple_element](https://en.cppreference.com/w/cpp/utility/tuple/tuple_element)
 */
template <std::size_t i, template <typename...> typename t, typename... types>
    requires(i < sizeof...(types)) && std::is_base_of_v<bio::meta::pod_tuple<types...>, t<types...>>
struct tuple_element<i, t<types...>>
{
    //!\brief Element type.
    using type = bio::meta::detail::pack_traits::at<i, types...>;
};

/*!\brief Provides access to the number of elements in a tuple as a compile-time constant expression.
 * \implements bio::meta::unary_type_trait
 * \see std::tuple_size_v
 * \relates bio::meta::pod_tuple
 */
template <template <typename...> typename t, typename... types>
    requires std::is_base_of_v<bio::meta::pod_tuple<types...>, t<types...>>
struct tuple_size<t<types...>> : public std::integral_constant<std::size_t, sizeof...(types)>
{};

//!\cond
template <std::size_t i, typename pod_tuple>
constexpr decltype(auto) get(pod_tuple && t) noexcept
    requires(bio::meta::template_specialisation_of<std::remove_cvref_t<pod_tuple>, bio::meta::pod_tuple> &&
             i < std::tuple_size_v<std::decay_t<pod_tuple>>)
{
    return bio::meta::detail::adl_get<i>(std::forward<pod_tuple>(t));
}

template <typename type, typename pod_tuple>
constexpr decltype(auto) get(pod_tuple && t) noexcept
    requires(bio::meta::template_specialisation_of<std::remove_cvref_t<pod_tuple>, bio::meta::pod_tuple> &&
             bio::meta::list_traits::count<
               type,
               bio::meta::transfer_template_args_onto_t<std::remove_cvref_t<pod_tuple>, bio::meta::type_list>> == 1)
{
    return bio::meta::detail::adl_get<type>(std::forward<pod_tuple>(t));
}

template <class... Ts, class... Us>
    requires requires { typename bio::meta::pod_tuple<std::common_type_t<Ts, Us>...>; }
struct common_type<bio::meta::pod_tuple<Ts...>, bio::meta::pod_tuple<Us...>>
{
    using type = bio::meta::pod_tuple<std::common_type_t<Ts, Us>...>;
};

template <class... Ts, class... Us, template <class> class TQual, template <class> class UQual>
    requires requires { typename bio::meta::pod_tuple<std::common_reference_t<TQual<Ts>, UQual<Us>>...>; }
struct basic_common_reference<bio::meta::pod_tuple<Ts...>, bio::meta::pod_tuple<Us...>, TQual, UQual>
{
    using type = bio::meta::pod_tuple<std::common_reference_t<TQual<Ts>, UQual<Us>>...>;
};
//!\endcond

} // namespace std
