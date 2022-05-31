// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/seqan/seqan3/blob/master/LICENSE.md
// -----------------------------------------------------------------------------------------------------

/*!\file
 * \author Joshua Kim <joshua.kim AT fu-berlin.de>
 * \brief Provides seqan3::views::interleave.
 */

#pragma once

#include <cmath>

#include <bio/meta/type_traits/pre.hpp>
#include <bio/meta/type_traits/transformation_trait_or.hpp>
#include <bio/ranges/detail/random_access_iterator.hpp>
#include <bio/ranges/views/detail.hpp>
#include <bio/ranges/views/persist.hpp>
#include <bio/ranges/views/type_reduce.hpp>
#include <concepts>
#include <ranges>

namespace seqan3::detail
{

// ============================================================================
//  view_interleave
// ============================================================================

/*!\brief The type returned by seqan3::views::interleave.
 * \tparam urng_t The type of the underlying range, must model std::ranges::random_access_range.
 * \implements std::ranges::view
 * \implements std::ranges::random_access_range
 * \implements std::ranges::sized_range
 * \ingroup views
 *
 * \details
 *
 * Note that most members of this class are generated by ranges::view_interface which is not yet documented here.
 */
template <std::ranges::random_access_range urng_t, std::ranges::random_access_range inserted_rng_t>
    //!\cond
    requires std::ranges::view<urng_t> && std::ranges::sized_range<urng_t> &&
             std::ranges::view<inserted_rng_t> && std::ranges::sized_range<inserted_rng_t> &&
             std::common_reference_with<std::ranges::range_reference_t<urng_t>,
                                        std::ranges::range_reference_t<inserted_rng_t>>
    //!\endcond
class view_interleave : public std::ranges::view_interface<view_interleave<urng_t, inserted_rng_t>>
{
private:
    //!\brief The underlying range.
    urng_t urange;
    //!\brief The step size for the insertion.
    size_t step_size{};
    //!\brief The range to be inserted into urange.
    inserted_rng_t inserted_range;

    /*!\name Associated types
     * \brief These associated types are needed in seqan3::detail::random_access_iterator.
     * \{
     */
    //!\brief This resolves to range_type::size_type as the underlying range is guaranteed to be sized.
    using size_type         = std::ranges::range_size_t<urng_t>;
    //!\brief The reference_type.
    using reference         = std::common_reference_t<std::ranges::range_reference_t<urng_t>,
                                                         std::ranges::range_reference_t<inserted_rng_t>>;
    //!\brief The const_reference type is equal to the reference type.
    using const_reference   = detail::transformation_trait_or_t<
                                std::common_reference<std::ranges::range_reference_t<urng_t const>,
                                                      std::ranges::range_reference_t<inserted_rng_t const>>, void>;
    //!\brief The value_type (which equals the reference_type with any references removed).
    using value_type        = std::ranges::range_value_t<urng_t>;
    //!\brief A signed integer type, usually std::ptrdiff_t.
    using difference_type   = std::ranges::range_difference_t<urng_t>;
    //!\brief The iterator type of this view (a random access iterator).
    using iterator          = detail::random_access_iterator<view_interleave>;
    //!\brief The const_iterator type is equal to the iterator type.
    using const_iterator    = detail::random_access_iterator<view_interleave const>;
    //!\}

    //!\brief Befriend the following class s.t. iterator and const_iterator can be defined for this type.
    template <typename parent_type, typename crtp_base>
    friend class detail::random_access_iterator_base;

public:
    /*!\name Constructors, destructor and assignment
     * \{
     */
    constexpr view_interleave()                                        noexcept = default; //!< Defaulted.
    constexpr view_interleave(view_interleave const & rhs)             noexcept = default; //!< Defaulted.
    constexpr view_interleave(view_interleave && rhs)                  noexcept = default; //!< Defaulted.
    constexpr view_interleave & operator=(view_interleave const & rhs) noexcept = default; //!< Defaulted.
    constexpr view_interleave & operator=(view_interleave && rhs)      noexcept = default; //!< Defaulted.
    ~view_interleave()                                                 noexcept = default; //!< Defaulted.

    /*!\brief Construct from a standard random_access_range urange and inserted_range.
     * \param[in] _urange         The underlying range.
     * \param[in] _step_size      The step size for the insertion.
     * \param[in] _inserted_range The range to be inserted.
     */
    view_interleave(urng_t && _urange, size_t const _step_size, inserted_rng_t && _inserted_range) :
        urange{_urange}, step_size{_step_size}, inserted_range{_inserted_range}
    {}

    /*!\brief Construct from a viewable_range urange and inserted_range by wrapping in a views::type_reduce.
     * \tparam    orng_t          A type that is the same as `urng_t` once wrapped in a view
     *                            (e.g. reference to container).
     * \tparam    oirng_t         A type that is the same as `inserted_rng_t` once wrapped in a view
     *                            (e.g. reference to container).
     * \param[in] _urange         The underlying range.
     * \param[in] _step_size      The step size for the insertion.
     * \param[in] _inserted_range The range to be inserted.
     */
    template <typename orng_t, typename oirng_t>
        //!\cond
        requires std::constructible_from<urng_t, decltype(views::type_reduce(std::declval<orng_t>()))> &&
                 std::constructible_from<inserted_rng_t, decltype(views::persist(std::declval<oirng_t>()))>
        //!\endcond
    view_interleave(orng_t && _urange, size_t const _step_size, oirng_t && _inserted_range) :
        view_interleave{views::type_reduce(std::forward<orng_t>(_urange)), _step_size,
                        views::persist(std::forward<oirng_t>(_inserted_range))}
    {}
    //!\}

    /*!\name Iterators
     * \{
     */
    /*!\brief Returns an iterator to the first element of the container.
     * \returns Iterator to the first element.
     *
     * If the container is empty, the returned iterator will be equal to end().
     *
     * ### Complexity
     *
     * Constant.
     *
     * ### Exceptions
     *
     * No-throw guarantee.
     */
    iterator begin() noexcept
    {
        return {*this, 0};
    }

    //!\overload
    const_iterator begin() const noexcept
    {
        return {*this, 0};
    }

    /*!\brief Returns an iterator to the element following the last element of the container.
     * \returns Iterator to the first element.
     *
     * This element acts as a placeholder; attempting to dereference it results in undefined behaviour.
     *
     * ### Complexity
     *
     * Constant.
     *
     * ### Exceptions
     *
     * No-throw guarantee.
     */
    iterator end() noexcept
    {
        return {*this, size()};
    }

    //!\overload
    const_iterator end() const noexcept
    {
        return {*this, size()};
    }
    //!\}

    /*!\brief Returns the number of elements in the view.
     * \returns The number of elements in the view.
     *
     * ### Complexity
     *
     * Constant.
     *
     */
    size_type size()
    {
        return std::ranges::size(urange) +
               ((std::floor(std::ranges::size(urange) / step_size) -
                (std::ranges::size(urange) % step_size == 0 ? 1 : 0)) *
                std::ranges::size(inserted_range));
    }

    //!\overload
    size_type size() const
    {
        return std::ranges::size(urange) +
               ((std::floor(std::ranges::size(urange) / step_size) -
                (std::ranges::size(urange) % step_size == 0 ? 1 : 0)) *
                std::ranges::size(inserted_range));
    }

    /*!\brief Return the i-th element.
     * \param i The element to retrieve.
     * \returns Either a writable proxy to the element or a copy (if called in const context).
     *
     * Accessing an element behind the last causes undefined behaviour. In debug mode an assertion checks the size of
     * the container.
     *
     * ### Complexity
     *
     * Constant.
     *
     * ### Exceptions
     *
     * No-throw guarantee.
     */
    reference operator[](size_type const i)
    {
        size_t combined_size = step_size + std::ranges::size(inserted_range);
        assert(i < size());
        if (i % (combined_size) < step_size)
            return urange[i - (std::floor(i/(combined_size)) * std::ranges::size(inserted_range))];
        else
            return inserted_range[(i % (combined_size)) - step_size];
    }

    //!\overload
    const_reference operator[](size_type const i) const
    {
        size_t combined_size = step_size + std::ranges::size(inserted_range);
        assert(i < size());
        if (i % (combined_size) < step_size)
            return urange[i - (std::floor(i/(combined_size)) * std::ranges::size(inserted_range))];
        else
            return inserted_range[(i % (combined_size)) - step_size];
    }
};

//!\brief Template argument type deduction guide for viewable_range inputs.
//!\relates seqan3::detail::view_interleave
template <std::ranges::random_access_range urng_t, std::ranges::random_access_range inserted_rng_t>
    //!\cond
    requires std::ranges::viewable_range<urng_t> && std::ranges::sized_range<urng_t> &&
             std::ranges::sized_range<inserted_rng_t> &&
             std::common_reference_with<std::ranges::range_reference_t<urng_t>,
                                        std::ranges::range_reference_t<inserted_rng_t>>
    //!\endcond
view_interleave(urng_t &&, size_t, inserted_rng_t &&)
    -> view_interleave<decltype(views::type_reduce(std::declval<urng_t>())),
                       decltype(views::persist(std::declval<inserted_rng_t>()))>;

// ============================================================================
//  interleave_fn (adaptor definition)
// ============================================================================

/*!\brief views::interleave's range adaptor object type (non-closure).
 */
struct interleave_fn
{
    //!\brief Store the argument and return a range adaptor closure object.
    template <std::ranges::forward_range inserted_rng_t, std::integral size_type>
    constexpr auto operator()(size_type const size, inserted_rng_t && i) const noexcept
    {
        return detail::adaptor_from_functor{*this, size, std::forward<inserted_rng_t>(i)};
    }

    /*!\brief            Call the view's constructor with the underlying view as argument.
     * \param[in] urange The input range to process. Must model std::ranges::forward_range and std::ranges::viewable_range.
     * \param[in] i      The inserted range to process. Must model std::ranges::forward_range.
     * \param[in] size   The step size for insertion into the input range.
     * \returns          A range of with the inserted range interleaved into the underlying range at the specified intervals.
     */
    template <std::ranges::range urng_t, std::ranges::range inserted_rng_t, std::integral size_type>
    constexpr auto operator()(urng_t && urange, size_type const size, inserted_rng_t && i) const noexcept
    {
        static_assert(std::ranges::random_access_range<urng_t>,
            "The underlying range parameter in views::interleave must model std::ranges::random_access_range.");
        static_assert(std::ranges::viewable_range<urng_t>,
            "The underlying range parameter in views::interleave must model std::ranges::viewable_range.");
        static_assert(std::ranges::sized_range<urng_t>,
            "The underlying range parameter in views::interleave must model std::ranges::sized_range.");

        static_assert(std::ranges::random_access_range<inserted_rng_t>,
            "The range to be inserted by views::interleave must model std::ranges::forward_range.");
        static_assert(std::ranges::sized_range<inserted_rng_t>,
            "The range to be inserted by views::interleave must model std::ranges::sized_range.");

        return detail::view_interleave{std::forward<urng_t>(urange),
                                       static_cast<size_t>(size),
                                       std::forward<inserted_rng_t>(i)};
    }
};

} // namespace seqan3::detail

// ============================================================================
//  views::interleave (adaptor instance definition)
// ============================================================================

namespace seqan3::views
{

/*!\name General purpose views
 * \{
 */

/*!\brief A view that interleaves a given range into another range at regular intervals.
 * \tparam urng_t The type of the range being processed.
 * \tparam inserted_rng_t The type of the range being inserted.
 * \param[in] urange The range being processed.
 * \param[in] inserted_range The range being inserted.
 * \param[in] step_size A value of size_type which indicates the interval to insert the inserted_range.
 * \returns A range with the second range inserted at regular intervals. See below for properties of said range.
 * \ingroup views
 *
 * \details
 *
 * \header_file{seqan3/range/views/interleave.hpp}
 *
 * This view can be used to insert one range into another range at regular intervals. It behaves essentially like
 * `| std::views::chunk(step_size) | views::join(inserted_range)` except that for input that models
 * std::ranges::random_access_range and std::ranges::sized_range a more efficient data structure is returned
 * (otherwise it returns exactly the above combination of views).
 *
 * ### View properties
 *
 * | Concepts and traits              | `urng_t` (underlying range type)      | `rrng_t` (returned range type)         |
 * |----------------------------------|:-------------------------------------:|:--------------------------------------:|
 * | std::ranges::input_range         | *required*                            | *preserved*                            |
 * | std::ranges::forward_range       | *required*                            | *preserved*                            |
 * | std::ranges::bidirectional_range | *required*                            | *preserved*                            |
 * | std::ranges::random_access_range | *required*                            | *preserved*                            |
 * | std::ranges::contiguous_range    |                                       | *lost*                                 |
 * |                                  |                                       |                                        |
 * | std::ranges::viewable_range      | *required*                            | *guaranteed*                           |
 * | std::ranges::view                |                                       | *guaranteed*                           |
 * | std::ranges::sized_range         | *required*                            | *preserved*                            |
 * | std::ranges::common_range        |                                       | *preserved*                            |
 * | std::ranges::output_range        |                                       | *preserved*                            |
 * | seqan3::const_iterable_range     |                                       | *preserved*                            |
 * |                                  |                                       |                                        |
 * | std::ranges::range_reference_t   |                                       | std::ranges::range_reference_t<urng_t> |
 *
 *
 * If above requirements are not met, this adaptor forwards to
 * `| seqan3::views::chunk(step_size) | std::views::join(inserted_range)`
 * which returns a view with the following properties:
 *
 * | Concepts and traits              | `urng_t` (underlying range type)      | `rrng_t` (returned range type)       |
 * |----------------------------------|:-------------------------------------:|:------------------------------------:|
 * | std::ranges::input_range         | *required*                            | *preserved*                          |
 * | std::ranges::forward_range       | *required*                            | *lost*                               |
 * | std::ranges::bidirectional_range |                                       | *lost*                               |
 * | std::ranges::random_access_range |                                       | *lost*                               |
 * | std::ranges::contiguous_range    |                                       | *lost*                               |
 * |                                  |                                       |                                      |
 * | std::ranges::viewable_range      | *required*                            | *guaranteed*                         |
 * | std::ranges::view                |                                       | *guaranteed*                         |
 * | std::ranges::sized_range         |                                       | *lost*                               |
 * | std::ranges::common_range        |                                       | *lost*                               |
 * | std::ranges::output_range        |                                       | *lost*                               |
 * | seqan3::const_iterable_range     |                                       | *lost*                               |
 * |                                  |                                       |                                      |
 * | std::ranges::range_reference_t   |                                       | std::ranges::range_value_t<urng_t>   |
 *
 * * `urng_t` is the type of the range modified by this view (input).
 * * `rrng_type` is the type of the range returned by this view.
 * * for more details, see \ref views.
 *
 * ### Example
 *
 * \include test/snippet/range/views/interleave.cpp
 * \hideinitializer
 */
inline constexpr auto interleave = detail::interleave_fn{};

//!\}

} // namespace seqan3::views
