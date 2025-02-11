// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

/*!\file
 * \brief Provides various transformation trait base templates and shortcuts.
 * \author Hannes Hauswedell <hannes.hauswedell AT decode.is>
 */

#pragma once

#include <bio/meta/platform.hpp>

namespace bio
{

/*!\addtogroup type_traits
 * \{
 */

// ----------------------------------------------------------------------------
// value_type
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
//!\brief This is helper structure to deprecate bio::value_type and will be removed before BioC++ 3.1.
template <typename t>
struct value_type;

//!\brief This is helper structure to deprecate bio::value_type_t and will be removed before BioC++ 3.1.
template <typename t>
using value_type_t = typename value_type<t>::type;
} // namespace bio::detail
#endif

/*!\brief Exposes the `value_type` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query.
 * \see bio::value_type_t
 *
 * \details
 *
 * This is a pure declaration, you need to create a *definition* for concrete types
 * or specialized or constrained templates.
 *
 * There is a shortcut for this transformation trait: bio::value_type_t
 *
 * \deprecated If `t` models std::ranges::range use std::ranges::range_value_t instead or
 *             if `t` models std::input_iterator use std::iter_value_t instead.
 */
template <typename t>
struct BIOCPP_DEPRECATED_310 value_type : detail::value_type<t>
{};

/*!\brief Shortcut for bio::value_type (transformation_trait shortcut).
 * \tparam t The type you wish to query.
 * \see bio::value_type
 * \deprecated If `t` models std::ranges::range use std::ranges::range_value_t instead or
 *             if `t` models std::input_iterator use std::iter_value_t instead.
 */
template <typename t>
using value_type_t BIOCPP_DEPRECATED_310 = typename detail::value_type_t<t>;

// see specialisation for iterators in core/type_traits/iterator.hpp
// see specialisation for ranges in core/type_traits/range.hpp

// ----------------------------------------------------------------------------
// reference
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
//!\brief This is helper structure to deprecate bio::reference and will be removed before BioC++ 3.1.
template <typename t>
struct reference;

//!\brief This is helper structure to deprecate bio::reference_t and will be removed before BioC++ 3.1.
template <typename t>
using reference_t = typename reference<t>::type;
} // namespace bio::detail
#endif

/*!\brief Exposes the `reference` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query.
 * \see bio::reference_t
 *
 * \details
 *
 * This is a pure declaration, you need to create a *definition* for concrete types
 * or specialized or constrained templates.
 *
 * There is a shortcut for this transformation trait: bio::reference_t
 *
 * \deprecated If `t` models std::ranges::range use std::ranges::range_reference_t instead or
 *             if `t` models std::input_iterator use std::iter_reference_t instead.
 */
template <typename t>
struct BIOCPP_DEPRECATED_310 reference : detail::reference<t>
{};

/*!\brief Shortcut for bio::reference (transformation_trait shortcut).
 * \tparam t The type you wish to query.
 * \see bio::reference
 * \deprecated If `t` models std::ranges::range use std::ranges::range_reference_t instead or
 *             if `t` models std::input_iterator use std::iter_reference_t instead.
 */
template <typename t>
using reference_t BIOCPP_DEPRECATED_310 = typename detail::reference_t<t>;

// see specialisation for iterators in core/type_traits/iterator.hpp
// see specialisation for ranges in core/type_traits/range.hpp

// ----------------------------------------------------------------------------
// rvalue_reference
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
//!\brief This is helper structure to deprecate bio::rvalue_reference and will be removed before BioC++ 3.1.
template <typename t>
struct rvalue_reference;

//!\brief This is helper structure to deprecate bio::rvalue_reference_t and will be removed before BioC++ 3.1.
template <typename t>
using rvalue_reference_t = typename rvalue_reference<t>::type;
} // namespace bio::detail
#endif

/*!\brief Exposes the `rvalue_reference` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query.
 * \see bio::rvalue_reference_t
 *
 * \details
 *
 * This is a pure declaration, you need to create a *definition* for concrete types
 * or specialized or constrained templates.
 *
 * There is a shortcut for this transformation trait: bio::rvalue_reference_t
 *
 * \deprecated If `t` models std::ranges::range use std::ranges::range_rvalue_reference_t instead or
 *             if `t` models std::input_iterator use std::iter_rvalue_reference_t instead.
 */
template <typename t>
struct BIOCPP_DEPRECATED_310 rvalue_reference : detail::rvalue_reference<t>
{};

/*!\brief Shortcut for bio::rvalue_reference (transformation_trait shortcut).
 * \tparam t The type you wish to query.
 * \see bio::rvalue_reference
 * \deprecated If `t` models std::ranges::range use std::ranges::range_rvalue_reference_t instead or
 *             if `t` models std::input_iterator use std::iter_rvalue_reference_t instead.
 */
template <typename t>
using rvalue_reference_t BIOCPP_DEPRECATED_310 = typename detail::rvalue_reference_t<t>;

// see specialisation for iterators in core/type_traits/iterator.hpp
// see specialisation for ranges in core/type_traits/range.hpp

// ----------------------------------------------------------------------------
// const_reference
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
//!\brief This is helper structure to deprecate bio::const_reference and will be removed before BioC++ 3.1.
template <typename t>
struct const_reference;

//!\brief This is helper structure to deprecate bio::const_reference_t and will be removed before BioC++ 3.1.
template <typename t>
using const_reference_t = typename const_reference<t>::type;
} // namespace bio::detail
#endif

/*!\brief Exposes the `const_reference` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query.
 * \see bio::const_reference_t
 *
 * \details
 *
 * This is a pure declaration, you need to create a *definition* for concrete types
 * or specialized or constrained templates.
 *
 * There is a shortcut for this transformation trait: bio::const_reference_t
 *
 * \attention This transformation trait is not overloaded for iterators by default, but it is for ranges.
 *
 * \deprecated If `t` models std::ranges::range use std::ranges::range_reference_t<t const> instead or
 *             if `t` models std::input_iterator use std::iter_reference_t<t const> instead.
 */
template <typename t>
struct BIOCPP_DEPRECATED_310 const_reference : detail::const_reference<t>
{};


/*!\brief Shortcut for bio::const_reference (transformation_trait shortcut).
 * \tparam t The type you wish to query.
 * \see bio::const_reference
 * \deprecated If `t` models std::ranges::range use std::ranges::range_reference_t<t const> instead or
 *             if `t` models std::input_iterator use std::iter_reference_t<t const> instead.
 */
template <typename t>
using const_reference_t BIOCPP_DEPRECATED_310 = typename detail::const_reference_t<t>;


// no specialisation for iterators
// see specialisation for ranges in core/type_traits/range.hpp

// ----------------------------------------------------------------------------
// difference_type
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
//!\brief This is helper structure to deprecate bio::difference_type and will be removed before BioC++ 3.1.
template <typename t>
struct difference_type;

//!\brief This is helper structure to deprecate bio::difference_type_t and will be removed before BioC++ 3.1.
template <typename t>
using difference_type_t = typename difference_type<t>::type;
} // namespace bio::detail
#endif

/*!\brief Exposes the `difference_type` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query.
 * \see bio::difference_type_t
 *
 * \details
 *
 * This is a pure declaration, you need to create a *definition* for concrete types
 * or specialized or constrained templates.
 *
 * There is a shortcut for this transformation trait: bio::difference_type_t
 *
 * \deprecated If `t` models std::ranges::range use std::ranges::range_difference_t instead or
 *             if `t` models std::input_iterator use std::iter_difference_t instead.
 */
template <typename t>
struct BIOCPP_DEPRECATED_310 difference_type : detail::difference_type<t>
{};


/*!\brief Shortcut for bio::difference_type (transformation_trait shortcut).
 * \tparam t The type you wish to query.
 * \see bio::difference_type
 * \deprecated If `t` models std::ranges::range use std::ranges::range_difference_t instead or
 *             if `t` models std::input_iterator use std::iter_difference_t instead.
 */
template <typename t>
using difference_type_t BIOCPP_DEPRECATED_310 = typename detail::difference_type_t<t>;


// see specialisation for iterators in core/type_traits/iterator.hpp
// see specialisation for ranges in core/type_traits/range.hpp

// ----------------------------------------------------------------------------
// size_type
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
//!\brief This is helper structure to deprecate bio::size_type and will be removed before BioC++ 3.1.
template <typename t>
struct size_type;

//!\brief This is helper structure to deprecate bio::size_type_t and will be removed before BioC++ 3.1.
template <typename t>
using size_type_t = typename size_type<t>::type;
} // namespace bio::detail
#endif

/*!\brief Exposes the `size_type` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query.
 * \see bio::size_type_t
 *
 * \details
 *
 * This is a pure declaration, you need to create a *definition* for concrete types
 * or specialized or constrained templates.
 *
 * There is a shortcut for this transformation trait: bio::size_type_t
 *
 * \deprecated If `t` models std::ranges::range use std::ranges::range_size_t instead.
 *             If `t` models std::input_iterator there is no alternative. Unlike std::ranges::range_size_t, the Standard
 *             has no std::iter_size_t.
 *
 */
template <typename t>
struct BIOCPP_DEPRECATED_310 size_type : detail::size_type<t>
{};

/*!\brief Shortcut for bio::size_type (transformation_trait shortcut).
 * \tparam t The type you wish to query.
 * \see bio::size_type
 * \deprecated If `t` models std::ranges::range use std::ranges::size_type_t instead.
 *             If `t` models std::input_iterator there is no alternative. Unlike std::ranges::range_size_t, the Standard
 *             has no std::iter_size_t.
 */
template <typename t>
using size_type_t BIOCPP_DEPRECATED_310 = typename detail::size_type_t<t>;

// see specialisation for iterators in core/type_traits/iterator.hpp
// see specialisation for ranges in core/type_traits/range.hpp

//!\}

} // namespace bio
