// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

/*!\file
 * \author Marcel Ehrhardt <marcel.ehrhardt AT fu-berlin.de>
 * \brief Provides bio::detail::transformation_trait_or.
 */

#pragma once

#include <type_traits>

#include <bio/meta/type_traits/concept.hpp>
#include <type_traits>

namespace bio::detail
{

/*!\brief This gives a fallback type if *type_t::type* is not defined.
 * \implements bio::transformation_trait
 * \ingroup type_traits
 * \tparam type_t    The type to use if *type_t::type* is defined.
 * \tparam default_t The type to use otherwise.
 * \see bio::detail::transformation_trait_or_t
 *
 * \details
 *
 * Gives *type_t* back if *T::type* is a member type, otherwise *struct{using type = default_t}*.
 *
 * \include test/snippet/meta/type_traits/transformation_trait_or.cpp
 *
 * \attention This might get removed if one of our used libraries offers the same
 * functionality.
 *
 * ###Helper types
 *   bio::detail::transformation_trait_or_t as a shorthand for *bio::detail::transformation_trait_or::type*
 */
template <typename type_t, typename default_t>
using transformation_trait_or = std::conditional_t<transformation_trait<type_t>,    // check if type_t::type exists
                                                   type_t,                          // if yes, return type_t
                                                   std::type_identity<default_t>>;  // else return default_t as trait

/*!\brief Helper type of bio::detail::transformation_trait_or (transformation_trait shortcut).
 * \ingroup type_traits
 * \see bio::detail::transformation_trait_or
 */
template <typename type_t, typename default_t>
using transformation_trait_or_t = typename transformation_trait_or<type_t, default_t>::type;

} // namespace bio::detail
