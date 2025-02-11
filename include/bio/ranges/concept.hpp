// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

/*!\file
 * \brief Additional non-standard concepts for ranges.
 * \author Hannes Hauswedell <hannes.hauswedell AT decode.is>
 */

#pragma once

#include <ranges>

#include <bio/alphabet/concept.hpp>

namespace bio
{

/*!\interface bio::const_iterable_range <>
 * \ingroup range
 * \extends std::input_range
 * \brief Specifies requirements of an input range type for which the `const` version of that type satisfies the
 * same strength input range concept as the non-const version.
 *
 * \details
 *
 * For a type `t` it usually holds that if `t` is a range, `t const` is also a range with similar properties, but
 * there are cases where this does not hold:
 *
 *   * a `const` range is usually not writable so std::ranges::output_range is lost; pure output ranges
 * (those that are not also input ranges) are therefore not `const`-iterable;
 *   * single-pass input ranges, like BioC++ files, are not `const`-iterable, because "single-pass-ness" implies that
 * there is something in the range that changes on every iterator increment (and `const` ranges can't change);
 *   * certain views store a state with their algorithm that also changes when `begin()` is called or an
 * iterator is incremented; these may be not be `const`-iterable, because the standard library
 * (and also BioC++) guarantees that it is safe to call `const`-qualified functions concurrently.
 */
//!\cond
template <typename type>
concept const_iterable_range =
    std::ranges::input_range<std::remove_const_t<type>> &&
    std::ranges::input_range<type const> &&
    (std::ranges::forward_range<std::remove_const_t<type>>       == std::ranges::forward_range<type const>) &&
    (std::ranges::bidirectional_range<std::remove_const_t<type>> == std::ranges::bidirectional_range<type const>) &&
    (std::ranges::random_access_range<std::remove_const_t<type>> == std::ranges::random_access_range<type const>);
//!\endcond

/*!\interface bio::pseudo_random_access_iterator <>
 * \ingroup range
 * \extends   std::forward_iterator
 * \brief     This concept checks if an iterator type models pseudo random access.
 *
 * \details
 *
 * A pseudo random access iterator refines the std::forward_iterator and fulfils in addition all syntactic requirements
 * of a regular std::random_access_iterator except that iterator category is weaker than
 * std::random_access_iterator_tag.
 * These iterators do allow jumping within the associated range or accessing an arbitrary element but cannot guarantee
 * constant time for these operations. Yet, the performance is sub-linear. Typical examples are range adaptors that
 * store additional information on the original sequence within a tree like data structure. Accessing a specific
 * position can be then achieved in sub-linear time. However, since pseudo-random access iterators can't guarantee
 * constant time for random access, the rule in the c++ standard is to mark them as bidirectional iterators.
 * This in turn has implications on some functions that operate on iterators. An example would be `std::distance`
 * that needs linear time for non-random access iterators (e.g. bidirectional iterators and accordingly
 * pseudo random access iterators), and constant time otherwise, although it could be computed in sub-linear time when
 * using the pseudo randomness.
 * The bio::views::enforce_random_access adaptor can redeclare a pseudo random access iterator as a random access
 * iterator (while preserving the caveat of needing more than constant time for random access).
 * A rule-of-thumb is that all operations are at least as fast as when using the non-redeclared random access iterators,
 * but be aware that runtime guarantees of some algorithms are higher than advertised due to the non-constant access
 * time.
 *
 * ### Concepts and doxygen
 *
 * The requirements for this concept are given as related functions and type traits.
 * Types that model this concept are shown as "implementing this interface".
 */
//!\cond
template <typename iterator_t>
concept pseudo_random_access_iterator =
    std::forward_iterator<iterator_t> &&
    !std::is_base_of_v<std::random_access_iterator_tag,
                       typename std::iterator_traits<iterator_t>::iterator_category> &&
    std::totally_ordered<iterator_t> &&
    std::sized_sentinel_for<iterator_t, iterator_t> &&
    requires (iterator_t i, iterator_t const j, std::iter_difference_t<iterator_t> const n)
{
    std::same_as<decltype( i += n ), iterator_t &>;
    std::same_as<decltype( j +  n ), iterator_t>;
    std::same_as<decltype( n +  j ), iterator_t>;
    std::same_as<decltype(   --i  ), iterator_t &>;
    std::same_as<decltype(   i--  ), iterator_t>;
    std::same_as<decltype( i -= n ), iterator_t &>;
    std::same_as<decltype( j -  n ), iterator_t>;
    std::same_as<decltype(  j[n]  ), std::iter_reference_t<iterator_t>>;
};
//!\endcond

/*!\interface bio::pseudo_random_access_range <>
 * \ingroup range
 * \extends   std::ranges::forward_range
 * \brief     This concept checks if a type models a pseudo random access range.
 *
 * \details
 *
 * A pseudo random access range is a forward range whose iterator type models bio::pseudo_random_access_iterator.
 *
 * ### Concepts and doxygen
 *
 * The requirements for this concept are given as related functions and type traits.
 * Types that model this concept are shown as "implementing this interface".
 */
//!\cond
template <typename rng_t>
concept pseudo_random_access_range =
    std::ranges::forward_range<rng_t> &&
    pseudo_random_access_iterator<std::ranges::iterator_t<rng_t>>;
//!\endcond

/*!\interface bio::sequence <>
 * \brief The generic concept for a sequence.
 * \ingroup range
 * \extends std::ranges::input_range
 *
 * We define a range over an bio::alphabet as a *sequence*.
 * A type models bio::sequence if it is at least an std::ranges::input_range
 * and its references type models bio::alphabet.
 *
 * ### Concepts and doxygen
 *
 * The requirements for this concept are given as related functions and type traits.
 * Types that model this concept are shown as "implementing this interface".
 */
//!\cond
template <typename rng_t>
concept sequence = std::ranges::input_range<rng_t> && alphabet<std::ranges::range_reference_t<rng_t>>;
//!\endcond

} // namespace bio
